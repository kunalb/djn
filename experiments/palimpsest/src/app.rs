use crate::git::{build_side_by_side, compute_diff, Commit, DiffLine, GitRepo, SideBySideRow};
use crate::highlight::Highlighter;
use anyhow::Result;
use std::collections::HashMap;
use std::path::PathBuf;

pub struct App {
    pub file_path: PathBuf,
    pub extension: String,
    pub commits: Vec<Commit>,
    pub current_index: usize,
    pub scroll_offset: usize,
    pub focused_line: usize,
    pub should_quit: bool,
    pub highlighter: Highlighter,
    content_cache: HashMap<String, String>,
    repo: GitRepo,
}

impl App {
    pub fn new(file_path: PathBuf) -> Result<Self> {
        let repo = GitRepo::open(&file_path)?;
        let commits = repo.get_file_history(&file_path)?;

        if commits.is_empty() {
            anyhow::bail!("No commits found for file: {}", file_path.display());
        }

        let extension = Highlighter::get_extension(&file_path);

        Ok(Self {
            file_path,
            extension,
            commits,
            current_index: 0,
            scroll_offset: 0,
            focused_line: 0,
            should_quit: false,
            highlighter: Highlighter::new(),
            content_cache: HashMap::new(),
            repo,
        })
    }

    pub fn current_commit(&self) -> &Commit {
        &self.commits[self.current_index]
    }

    pub fn go_back(&mut self) {
        if self.current_index < self.commits.len() - 1 {
            self.current_index += 1;
        }
    }

    pub fn go_forward(&mut self) {
        if self.current_index > 0 {
            self.current_index -= 1;
        }
    }

    pub fn page_down(&mut self, page_size: usize) {
        self.scroll_offset = self.scroll_offset.saturating_add(page_size);
    }

    pub fn page_up(&mut self, page_size: usize) {
        self.scroll_offset = self.scroll_offset.saturating_sub(page_size);
    }

    pub fn focus_down(&mut self, view_height: usize) {
        self.focused_line = self.focused_line.saturating_add(1);
        self.ensure_focused_visible(view_height);
    }

    pub fn focus_up(&mut self, view_height: usize) {
        self.focused_line = self.focused_line.saturating_sub(1);
        self.ensure_focused_visible(view_height);
    }

    fn ensure_focused_visible(&mut self, view_height: usize) {
        if self.focused_line < self.scroll_offset {
            self.scroll_offset = self.focused_line;
        } else if self.focused_line >= self.scroll_offset + view_height {
            self.scroll_offset = self.focused_line - view_height + 1;
        }
    }

    fn center_on_focused(&mut self, view_height: usize) {
        self.scroll_offset = self.focused_line.saturating_sub(view_height / 2);
    }

    pub fn jump_to_next_change(&mut self, view_height: usize) {
        if let Some(positions) = self.get_change_positions() {
            for &pos in &positions {
                if pos > self.focused_line {
                    self.focused_line = pos;
                    self.center_on_focused(view_height);
                    return;
                }
            }
            // Wrap to first change
            if let Some(&first) = positions.first() {
                self.focused_line = first;
                self.center_on_focused(view_height);
            }
        }
    }

    pub fn jump_to_prev_change(&mut self, view_height: usize) {
        if let Some(positions) = self.get_change_positions() {
            // Find the hunk that starts strictly before our current position
            // If we're at a hunk start, go to the previous one
            for &pos in positions.iter().rev() {
                if pos < self.focused_line && !self.is_same_hunk(pos, self.focused_line) {
                    self.focused_line = pos;
                    self.center_on_focused(view_height);
                    return;
                }
            }
            // Wrap to last change
            if let Some(&last) = positions.last() {
                self.focused_line = last;
                self.center_on_focused(view_height);
            }
        }
    }

    fn is_same_hunk(&mut self, hunk_start: usize, line: usize) -> bool {
        if let Some(rows) = self.get_side_by_side() {
            // Check if all rows between hunk_start and line are changes
            for i in hunk_start..=line {
                if i < rows.len() && !rows[i].is_change() {
                    return false;
                }
            }
            return hunk_start <= line;
        }
        false
    }

    fn get_change_positions(&mut self) -> Option<Vec<usize>> {
        let rows = self.get_side_by_side()?;
        let mut positions = Vec::new();
        let mut in_change_block = false;

        for (i, row) in rows.iter().enumerate() {
            let is_change = row.is_change();
            if is_change && !in_change_block {
                // Start of a new change block
                positions.push(i);
                in_change_block = true;
            } else if !is_change {
                in_change_block = false;
            }
        }

        if positions.is_empty() {
            None
        } else {
            Some(positions)
        }
    }

    pub fn get_side_by_side(&mut self) -> Option<Vec<SideBySideRow>> {
        let diff = self.get_diff_to_previous().ok()??;
        Some(build_side_by_side(&diff))
    }

    pub fn get_current_content(&mut self) -> Result<String> {
        let commit = &self.commits[self.current_index];
        self.get_content(&commit.hash.clone(), &commit.file_path.clone())
    }

    pub fn get_diff_to_previous(&mut self) -> Result<Option<Vec<DiffLine>>> {
        if self.current_index >= self.commits.len() - 1 {
            return Ok(None);
        }

        let current_hash = self.commits[self.current_index].hash.clone();
        let current_path = self.commits[self.current_index].file_path.clone();
        let previous_hash = self.commits[self.current_index + 1].hash.clone();
        let previous_path = self.commits[self.current_index + 1].file_path.clone();

        let current_content = self.get_content(&current_hash, &current_path)?;
        let previous_content = self.get_content(&previous_hash, &previous_path)?;

        Ok(Some(compute_diff(&previous_content, &current_content)))
    }

    fn get_content(&mut self, hash: &str, file_path: &str) -> Result<String> {
        let key = format!("{}:{}", hash, file_path);
        if let Some(content) = self.content_cache.get(&key) {
            return Ok(content.clone());
        }

        let content = self.repo.get_file_content(hash, file_path)?;
        self.content_cache.insert(key, content.clone());
        Ok(content)
    }

    pub fn total_commits(&self) -> usize {
        self.commits.len()
    }

    pub fn position_from_end(&self) -> usize {
        self.commits.len() - self.current_index
    }
}
