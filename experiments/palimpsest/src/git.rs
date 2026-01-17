use anyhow::{Context, Result};
use chrono::{DateTime, TimeZone, Utc};
use git2::{BlameOptions, DiffOptions, Repository};
use std::path::Path;

#[derive(Clone, Debug)]
pub struct Commit {
    pub hash: String,
    pub short_hash: String,
    pub message: String,
    pub author: String,
    pub date: DateTime<Utc>,
    pub file_path: String,
}

pub struct GitRepo {
    repo: Repository,
    workdir: std::path::PathBuf,
}

impl GitRepo {
    pub fn open(path: &Path) -> Result<Self> {
        let repo = Repository::discover(path).context("Failed to find git repository")?;
        let workdir = repo
            .workdir()
            .context("Repository has no working directory")?
            .to_path_buf();
        Ok(Self { repo, workdir })
    }

    pub fn relative_path(&self, abs_path: &Path) -> Result<std::path::PathBuf> {
        abs_path
            .strip_prefix(&self.workdir)
            .map(|p| p.to_path_buf())
            .context("File is not within the repository")
    }

    pub fn get_file_history(&self, file_path: &Path) -> Result<Vec<Commit>> {
        let rel_path = self.relative_path(file_path)?;

        let mut revwalk = self.repo.revwalk()?;
        revwalk.push_head()?;
        revwalk.set_sorting(git2::Sort::TIME)?;

        let mut commits = Vec::new();
        let mut current_path = rel_path;

        for oid in revwalk {
            let oid = oid?;
            let commit = self.repo.find_commit(oid)?;

            if let Some(new_path) = self.commit_touches_file(&commit, &current_path)? {
                let time = commit.time();
                let date = Utc
                    .timestamp_opt(time.seconds(), 0)
                    .single()
                    .unwrap_or_else(Utc::now);

                commits.push(Commit {
                    hash: oid.to_string(),
                    short_hash: oid.to_string()[..7].to_string(),
                    message: commit
                        .message()
                        .unwrap_or("")
                        .lines()
                        .next()
                        .unwrap_or("")
                        .to_string(),
                    author: commit.author().name().unwrap_or("Unknown").to_string(),
                    date,
                    file_path: new_path.to_string_lossy().to_string(),
                });

                if new_path != current_path {
                    current_path = new_path;
                }
            }
        }

        Ok(commits)
    }

    fn commit_touches_file(
        &self,
        commit: &git2::Commit,
        file_path: &Path,
    ) -> Result<Option<std::path::PathBuf>> {
        let tree = commit.tree()?;

        let parent_tree = if commit.parent_count() > 0 {
            Some(commit.parent(0)?.tree()?)
        } else {
            None
        };

        let mut diff_opts = DiffOptions::new();
        diff_opts.pathspec(file_path);

        let diff = self.repo.diff_tree_to_tree(
            parent_tree.as_ref(),
            Some(&tree),
            Some(&mut diff_opts),
        )?;

        if diff.deltas().len() > 0 {
            return Ok(Some(file_path.to_path_buf()));
        }

        // Check for renames by looking at the full diff
        let mut diff_opts = DiffOptions::new();
        let diff = self.repo.diff_tree_to_tree(
            parent_tree.as_ref(),
            Some(&tree),
            Some(&mut diff_opts),
        )?;

        let mut find_opts = git2::DiffFindOptions::new();
        find_opts.renames(true);
        find_opts.copies(true);

        let mut diff = diff;
        diff.find_similar(Some(&mut find_opts))?;

        for delta in diff.deltas() {
            if delta.status() == git2::Delta::Renamed {
                let new_file = delta.new_file().path();
                let old_file = delta.old_file().path();

                if new_file == Some(file_path) {
                    if let Some(old) = old_file {
                        return Ok(Some(old.to_path_buf()));
                    }
                }
            }
        }

        Ok(None)
    }

    pub fn get_file_content(&self, commit_hash: &str, file_path: &str) -> Result<String> {
        let oid = git2::Oid::from_str(commit_hash)?;
        let commit = self.repo.find_commit(oid)?;
        let tree = commit.tree()?;

        let entry = tree.get_path(Path::new(file_path))?;
        let blob = self.repo.find_blob(entry.id())?;

        let content = std::str::from_utf8(blob.content())
            .context("File content is not valid UTF-8")?
            .to_string();

        Ok(content)
    }

    /// Get the commit hash that introduced a specific line at a given commit
    pub fn blame_line(&self, commit_hash: &str, file_path: &str, line_num: usize) -> Result<String> {
        let oid = git2::Oid::from_str(commit_hash)?;

        let mut opts = BlameOptions::new();
        opts.newest_commit(oid);

        let blame = self.repo.blame_file(Path::new(file_path), Some(&mut opts))?;

        let hunk = blame
            .get_line(line_num)
            .context("Line not found in blame")?;

        Ok(hunk.final_commit_id().to_string())
    }
}

pub fn compute_diff(old: &str, new: &str) -> Vec<DiffLine> {
    use similar::{ChangeTag, TextDiff};

    let diff = TextDiff::from_lines(old, new);
    let mut lines = Vec::new();

    for change in diff.iter_all_changes() {
        let tag = match change.tag() {
            ChangeTag::Delete => DiffTag::Delete,
            ChangeTag::Insert => DiffTag::Insert,
            ChangeTag::Equal => DiffTag::Equal,
        };

        lines.push(DiffLine {
            tag,
            old_line: change.old_index().map(|i| i + 1),
            new_line: change.new_index().map(|i| i + 1),
        });
    }

    lines
}

#[derive(Clone, Debug, PartialEq)]
pub enum DiffTag {
    Delete,
    Insert,
    Equal,
}

#[derive(Clone, Debug)]
pub struct DiffLine {
    pub tag: DiffTag,
    pub old_line: Option<usize>,
    pub new_line: Option<usize>,
}

#[derive(Clone, Debug)]
pub struct SideBySideRow {
    pub left_line_num: Option<usize>,
    pub left_tag: Option<DiffTag>,
    pub right_line_num: Option<usize>,
    pub right_tag: Option<DiffTag>,
}

impl SideBySideRow {
    pub fn is_change(&self) -> bool {
        matches!(self.left_tag, Some(DiffTag::Delete))
            || matches!(self.right_tag, Some(DiffTag::Insert))
    }
}

pub fn build_side_by_side(diff_lines: &[DiffLine]) -> Vec<SideBySideRow> {
    let mut rows = Vec::new();
    let mut i = 0;

    while i < diff_lines.len() {
        match diff_lines[i].tag {
            DiffTag::Equal => {
                rows.push(SideBySideRow {
                    left_line_num: diff_lines[i].old_line,
                    left_tag: Some(DiffTag::Equal),
                    right_line_num: diff_lines[i].new_line,
                    right_tag: Some(DiffTag::Equal),
                });
                i += 1;
            }
            DiffTag::Delete => {
                // Collect consecutive deletes
                let mut deletes = Vec::new();
                while i < diff_lines.len() && diff_lines[i].tag == DiffTag::Delete {
                    deletes.push(&diff_lines[i]);
                    i += 1;
                }
                // Collect consecutive inserts
                let mut inserts = Vec::new();
                while i < diff_lines.len() && diff_lines[i].tag == DiffTag::Insert {
                    inserts.push(&diff_lines[i]);
                    i += 1;
                }
                // Pair them up
                let max_len = deletes.len().max(inserts.len());
                for j in 0..max_len {
                    let left = deletes.get(j);
                    let right = inserts.get(j);
                    rows.push(SideBySideRow {
                        left_line_num: left.and_then(|d| d.old_line),
                        left_tag: left.map(|_| DiffTag::Delete),
                        right_line_num: right.and_then(|d| d.new_line),
                        right_tag: right.map(|_| DiffTag::Insert),
                    });
                }
            }
            DiffTag::Insert => {
                // Standalone insert (no preceding delete)
                rows.push(SideBySideRow {
                    left_line_num: None,
                    left_tag: None,
                    right_line_num: diff_lines[i].new_line,
                    right_tag: Some(DiffTag::Insert),
                });
                i += 1;
            }
        }
    }

    rows
}
