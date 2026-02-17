use askama::Template;
use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::{Html, IntoResponse, Response},
};
use std::sync::Arc;

use lab::experiment::{self, Experiment, ExperimentStatus};
use lab::git;
use super::{github, render, AppState};

#[derive(Debug, serde::Deserialize)]
pub struct ListParams {
    #[serde(default)]
    pub sort: Option<String>,
    #[serde(default)]
    pub order: Option<String>,
    #[serde(default)]
    pub tag: Option<String>,
    #[serde(default)]
    pub status: Option<String>,
    #[serde(default)]
    pub q: Option<String>,
}

#[derive(Debug, serde::Deserialize)]
pub struct CompareParams {
    pub ids: String,
}

// Template structs

#[derive(Template)]
#[template(path = "list.html")]
struct ListTemplate {
    experiments: Vec<ExperimentView>,
    params: ListParams,
    all_tags: Vec<String>,
}

#[derive(Template)]
#[template(path = "list_partial.html")]
struct ListPartialTemplate {
    experiments: Vec<ExperimentView>,
    params: ListParams,
}

#[derive(Template)]
#[template(path = "detail.html")]
struct DetailTemplate {
    exp: ExperimentView,
    notes_html: String,
    artifacts: Vec<ArtifactView>,
    snapshots: Vec<SnapshotView>,
}

#[derive(Template)]
#[template(path = "compare.html")]
struct CompareTemplate {
    experiments: Vec<ExperimentView>,
    rows: Vec<CompareRow>,
}

#[derive(Template)]
#[template(path = "partials/diff.html")]
struct DiffTemplate {
    diff_html: String,
}

// View models

#[derive(Clone)]
pub struct ExperimentView {
    pub id: String,
    pub date: String,
    pub name: String,
    pub status: String,
    pub tags: Vec<String>,
    pub snapshot_count: usize,
    pub artifact_count: usize,
    pub hypothesis: String,
    pub result: String,
}

pub struct ArtifactView {
    pub name: String,
    pub content: render::ArtifactContent,
}

pub struct SnapshotView {
    pub index: usize,
    pub commit: String,
    pub branch: String,
    pub dirty: bool,
    pub dirty_files: Vec<String>,
    pub github_url: Option<String>,
    pub has_prev: bool,
}

pub struct CompareRow {
    pub label: String,
    pub is_artifact: bool,
    pub values: Vec<String>,
}

// Handlers

pub async fn list_page(
    State(state): State<Arc<AppState>>,
    Query(params): Query<ListParams>,
) -> Response {
    match build_list_template(&state, params) {
        Ok(template) => Html(template.render().unwrap_or_default()).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response(),
    }
}

pub async fn list_partial(
    State(state): State<Arc<AppState>>,
    Query(params): Query<ListParams>,
) -> Response {
    match load_experiments(&state, &params) {
        Ok(experiments) => {
            let template = ListPartialTemplate { experiments, params };
            Html(template.render().unwrap_or_default()).into_response()
        }
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response(),
    }
}

pub async fn detail_page(
    State(state): State<Arc<AppState>>,
    Path(id): Path<String>,
) -> Response {
    match build_detail_template(&state, &id) {
        Ok(template) => Html(template.render().unwrap_or_default()).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response(),
    }
}

pub async fn compare_page(
    State(state): State<Arc<AppState>>,
    Query(params): Query<CompareParams>,
) -> Response {
    let template = build_compare_template(&state, &params.ids);
    Html(template.render().unwrap_or_default()).into_response()
}

pub async fn diff_partial(
    State(state): State<Arc<AppState>>,
    Path((id, range)): Path<(String, String)>,
) -> Response {
    match build_diff(&state, &id, &range) {
        Ok(template) => Html(template.render().unwrap_or_default()).into_response(),
        Err(e) => (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()).into_response(),
    }
}

pub async fn artifact(
    State(state): State<Arc<AppState>>,
    Path((id, name)): Path<(String, String)>,
) -> Response {
    let path = experiment::artifacts_dir(&state.lab_dir, &id).join(&name);
    match std::fs::read(&path) {
        Ok(content) => {
            let content_type = mime_type(&name);
            ([(axum::http::header::CONTENT_TYPE, content_type)], content).into_response()
        }
        Err(_) => StatusCode::NOT_FOUND.into_response(),
    }
}

// Helper functions

fn build_list_template(state: &AppState, params: ListParams) -> anyhow::Result<ListTemplate> {
    let experiments = load_experiments(state, &params)?;
    let all_tags = collect_all_tags(state)?;
    Ok(ListTemplate { experiments, params, all_tags })
}

fn load_experiments(state: &AppState, params: &ListParams) -> anyhow::Result<Vec<ExperimentView>> {
    let ids = experiment::list_all(&state.lab_dir)?;
    let mut experiments: Vec<ExperimentView> = ids
        .iter()
        .filter_map(|id| {
            experiment::load_meta(&state.lab_dir, id)
                .ok()
                .map(|exp| to_experiment_view(&exp, &state.lab_dir))
        })
        .collect();

    // Apply filters
    if let Some(ref tag) = params.tag {
        if !tag.is_empty() {
            experiments.retain(|e| e.tags.contains(tag));
        }
    }
    if let Some(ref status) = params.status {
        if !status.is_empty() {
            experiments.retain(|e| e.status.to_lowercase() == status.to_lowercase());
        }
    }
    if let Some(ref q) = params.q {
        if !q.is_empty() {
            let q_lower = q.to_lowercase();
            experiments.retain(|e| {
                e.id.to_lowercase().contains(&q_lower)
                    || e.name.to_lowercase().contains(&q_lower)
                    || e.hypothesis.to_lowercase().contains(&q_lower)
            });
        }
    }

    // Apply sorting
    let sort_field = params.sort.as_deref().unwrap_or("date");
    let order_desc = params.order.as_deref() != Some("asc");

    experiments.sort_by(|a, b| {
        let cmp = match sort_field {
            "name" => a.name.cmp(&b.name),
            "status" => a.status.cmp(&b.status),
            "snapshots" => a.snapshot_count.cmp(&b.snapshot_count),
            _ => a.date.cmp(&b.date),
        };
        if order_desc { cmp.reverse() } else { cmp }
    });

    Ok(experiments)
}

fn to_experiment_view(exp: &Experiment, lab_dir: &std::path::Path) -> ExperimentView {
    let short_id = exp.id.rsplit('/').next().unwrap_or(&exp.id).to_string();
    let date = exp.id.split('/').take(3).collect::<Vec<_>>().join("/");
    let name = short_id.split('-').skip(1).collect::<Vec<_>>().join("-");
    let name = if name.is_empty() { short_id.clone() } else { name };

    // Parse frontmatter from notes
    let notes_path = experiment::notes_path(lab_dir, &exp.id);
    let (tags, hypothesis, result) = parse_frontmatter(&notes_path);

    ExperimentView {
        id: exp.id.clone(),
        date,
        name,
        status: match exp.status {
            ExperimentStatus::Open => "open".to_string(),
            ExperimentStatus::Closed => "closed".to_string(),
        },
        tags,
        snapshot_count: exp.snapshots.len(),
        artifact_count: exp.artifacts.len(),
        hypothesis,
        result,
    }
}

fn parse_frontmatter(notes_path: &std::path::Path) -> (Vec<String>, String, String) {
    let content = std::fs::read_to_string(notes_path).unwrap_or_default();
    let mut tags = Vec::new();
    let mut hypothesis = String::new();
    let mut result = String::new();

    // Simple YAML frontmatter parsing
    if let Some(rest) = content.strip_prefix("---") {
        if let Some(end) = rest.find("---") {
            let frontmatter = &rest[..end];
            for line in frontmatter.lines() {
                let line = line.trim();
                if line.starts_with("tags:") {
                    // Parse tags array
                    let rest = line.strip_prefix("tags:").unwrap().trim();
                    if rest.starts_with('[') && rest.ends_with(']') {
                        let inner = &rest[1..rest.len() - 1];
                        tags = inner
                            .split(',')
                            .map(|s| s.trim().trim_matches('"').trim_matches('\'').to_string())
                            .filter(|s| !s.is_empty())
                            .collect();
                    }
                } else if line.starts_with("hypothesis:") {
                    hypothesis = line.strip_prefix("hypothesis:").unwrap().trim().to_string();
                } else if line.starts_with("result:") {
                    result = line.strip_prefix("result:").unwrap().trim().to_string();
                }
            }
        }
    }

    (tags, hypothesis, result)
}

fn collect_all_tags(state: &AppState) -> anyhow::Result<Vec<String>> {
    let ids = experiment::list_all(&state.lab_dir)?;
    let mut all_tags = std::collections::BTreeSet::new();

    for id in &ids {
        let notes_path = experiment::notes_path(&state.lab_dir, id);
        let (tags, _, _) = parse_frontmatter(&notes_path);
        all_tags.extend(tags);
    }

    Ok(all_tags.into_iter().collect())
}

fn build_detail_template(state: &AppState, id: &str) -> anyhow::Result<DetailTemplate> {
    let exp = experiment::load_meta(&state.lab_dir, id)?;
    let exp_view = to_experiment_view(&exp, &state.lab_dir);

    // Render notes markdown
    let notes_path = experiment::notes_path(&state.lab_dir, id);
    let notes_content = std::fs::read_to_string(&notes_path).unwrap_or_default();
    let notes_html = render::render_markdown(&notes_content);

    // Load artifacts
    let artifacts_dir = experiment::artifacts_dir(&state.lab_dir, id);
    let artifacts: Vec<ArtifactView> = exp
        .artifacts
        .iter()
        .map(|name| {
            let path = artifacts_dir.join(name);
            let content = std::fs::read(&path).unwrap_or_default();
            let rendered = render::render_artifact(name, &content, &state.syntax_set, &state.theme_set);
            ArtifactView {
                name: name.clone(),
                content: rendered,
            }
        })
        .collect();

    // Build snapshot views
    let snapshots: Vec<SnapshotView> = exp
        .snapshots
        .iter()
        .enumerate()
        .map(|(i, s)| {
            let github_url = state.github.as_ref().map(|gh| github::commit_url(gh, &s.commit));
            SnapshotView {
                index: i,
                commit: s.commit.clone(),
                branch: s.branch.clone(),
                dirty: s.dirty,
                dirty_files: s.dirty_files.clone(),
                github_url,
                has_prev: i > 0,
            }
        })
        .collect();

    Ok(DetailTemplate {
        exp: exp_view,
        notes_html,
        artifacts,
        snapshots,
    })
}

fn build_compare_template(state: &AppState, ids_str: &str) -> CompareTemplate {
    let ids: Vec<&str> = ids_str.split(',').collect();
    let mut experiments = Vec::new();
    let mut all_artifacts = std::collections::BTreeSet::new();

    for id in &ids {
        if let Ok(exp) = experiment::load_meta(&state.lab_dir, id) {
            let view = to_experiment_view(&exp, &state.lab_dir);
            all_artifacts.extend(exp.artifacts.iter().cloned());
            experiments.push(view);
        }
    }

    // Build comparison rows
    let mut rows = Vec::new();

    // Metadata rows
    for key in &["status", "hypothesis", "result"] {
        let values: Vec<String> = experiments
            .iter()
            .map(|e| match *key {
                "status" => e.status.clone(),
                "hypothesis" => e.hypothesis.clone(),
                "result" => e.result.clone(),
                _ => String::new(),
            })
            .collect();
        rows.push(CompareRow {
            label: (*key).to_string(),
            is_artifact: false,
            values,
        });
    }

    // Artifact rows
    for artifact_name in &all_artifacts {
        let values: Vec<String> = experiments
            .iter()
            .map(|e| {
                let artifacts_dir = experiment::artifacts_dir(&state.lab_dir, &e.id);
                if artifacts_dir.join(artifact_name).exists() {
                    format!("/artifact/{}/{}", e.id, artifact_name)
                } else {
                    String::new()
                }
            })
            .collect();
        rows.push(CompareRow {
            label: artifact_name.clone(),
            is_artifact: true,
            values,
        });
    }

    CompareTemplate { experiments, rows }
}

fn build_diff(state: &AppState, id: &str, range: &str) -> anyhow::Result<DiffTemplate> {
    let exp = experiment::load_meta(&state.lab_dir, id)?;

    // Parse range like "0..1" to get snapshot indices
    let parts: Vec<&str> = range.split("..").collect();
    if parts.len() != 2 {
        anyhow::bail!("Invalid range format");
    }

    let from_idx: usize = parts[0].parse()?;
    let to_idx: usize = parts[1].parse()?;

    let from_commit = exp.snapshots.get(from_idx).map(|s| &s.commit);
    let to_commit = exp.snapshots.get(to_idx).map(|s| &s.commit);

    let diff_output = match (from_commit, to_commit) {
        (Some(from), Some(to)) => git::get_diff(&state.source_repo, from, to)?,
        _ => String::from("No commits to compare"),
    };

    let diff_html = render::highlight_diff(&diff_output);

    Ok(DiffTemplate { diff_html })
}

fn mime_type(name: &str) -> &'static str {
    match name.rsplit('.').next() {
        Some("png") => "image/png",
        Some("jpg" | "jpeg") => "image/jpeg",
        Some("gif") => "image/gif",
        Some("svg") => "image/svg+xml",
        Some("json") => "application/json",
        Some("yaml" | "yml") => "text/yaml",
        Some("txt") => "text/plain",
        Some("html") => "text/html",
        Some("css") => "text/css",
        Some("js") => "application/javascript",
        _ => "application/octet-stream",
    }
}
