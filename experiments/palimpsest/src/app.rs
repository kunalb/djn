use crate::git::{build_side_by_side, compute_diff, Commit, DiffLine, GitRepo, SideBySideRow};
use crate::highlight::Highlighter;
use anyhow::{Context, Result};
use ratatui::style::Style;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

pub type HighlightedLine = Vec<(Style, String)>;

pub const CONTEXT_LINES: usize = 3;

/// Represents a row in the collapsed view
#[derive(Clone, Debug)]
pub enum CollapsedRow {
    /// A normal row from the side-by-side diff
    Normal(usize), // index into full rows
    /// A collapsed section indicator
    Collapsed {
        start_idx: usize,
        hidden_count: usize,
    },
}

pub struct App {
    pub file_path: PathBuf,
    pub extension: String,
    pub commits: Vec<Commit>,
    pub current_index: usize,
    pub scroll_offset: usize,
    pub focused_line: usize,
    pub should_quit: bool,
    pub collapsed_mode: bool,
    pub expanded_sections: HashSet<usize>, // start indices of expanded sections
    pub show_commit_details: bool,
    pub details_selected_file: usize,
    pub highlighter: Highlighter,
    content_cache: HashMap<String, String>,
    highlight_cache: HashMap<String, Vec<HighlightedLine>>,
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
            collapsed_mode: false,
            expanded_sections: HashSet::new(),
            show_commit_details: false,
            details_selected_file: 0,
            highlighter: Highlighter::new(),
            content_cache: HashMap::new(),
            highlight_cache: HashMap::new(),
            repo,
        })
    }

    pub fn get_highlighted_content(&mut self, hash: &str, file_path: &str) -> Result<Vec<HighlightedLine>> {
        let key = format!("{}:{}", hash, file_path);

        if let Some(highlighted) = self.highlight_cache.get(&key) {
            return Ok(highlighted.clone());
        }

        let content = self.get_content(hash, file_path)?;
        let highlighted: Vec<HighlightedLine> = content
            .lines()
            .map(|line| self.highlighter.highlight_line(line, &self.extension))
            .collect();

        self.highlight_cache.insert(key.clone(), highlighted.clone());
        Ok(highlighted)
    }

    pub fn current_commit(&self) -> &Commit {
        &self.commits[self.current_index]
    }

    pub fn go_back(&mut self) {
        if self.current_index < self.commits.len() - 1 {
            self.current_index += 1;
            self.expanded_sections.clear();
        }
    }

    pub fn go_forward(&mut self) {
        if self.current_index > 0 {
            self.current_index -= 1;
            self.expanded_sections.clear();
        }
    }

    pub fn toggle_collapsed_mode(&mut self) {
        self.collapsed_mode = !self.collapsed_mode;
        self.focused_line = 0;
        self.scroll_offset = 0;
        self.expanded_sections.clear();
    }

    /// Build the collapsed view with context lines around changes
    pub fn get_collapsed_rows(&mut self) -> Option<Vec<CollapsedRow>> {
        let rows = self.get_side_by_side()?;
        if rows.is_empty() {
            return Some(vec![]);
        }

        // Find all change indices
        let mut is_context: Vec<bool> = vec![false; rows.len()];

        for (i, row) in rows.iter().enumerate() {
            if row.is_change() {
                // Mark this line and surrounding context as visible
                let start = i.saturating_sub(CONTEXT_LINES);
                let end = (i + CONTEXT_LINES + 1).min(rows.len());
                for j in start..end {
                    is_context[j] = true;
                }
            }
        }

        // Check expanded sections
        for &start_idx in &self.expanded_sections {
            // Find the end of this collapsed section and mark it all visible
            let mut j = start_idx;
            while j < rows.len() && !is_context[j] {
                is_context[j] = true;
                j += 1;
            }
        }

        // Build collapsed rows
        let mut collapsed = Vec::new();
        let mut i = 0;

        while i < rows.len() {
            if is_context[i] {
                collapsed.push(CollapsedRow::Normal(i));
                i += 1;
            } else {
                // Start of a collapsed section
                let start = i;
                while i < rows.len() && !is_context[i] {
                    i += 1;
                }
                collapsed.push(CollapsedRow::Collapsed {
                    start_idx: start,
                    hidden_count: i - start,
                });
            }
        }

        Some(collapsed)
    }

    /// Expand the collapsed section at the current focused line
    pub fn expand_at_focus(&mut self) -> bool {
        if !self.collapsed_mode {
            return false;
        }

        if let Some(collapsed_rows) = self.get_collapsed_rows() {
            if let Some(row) = collapsed_rows.get(self.focused_line) {
                if let CollapsedRow::Collapsed { start_idx, .. } = row {
                    self.expanded_sections.insert(*start_idx);
                    return true;
                }
            }
        }
        false
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

    /// Get the line number in the current file for the focused row
    /// Returns (line_num, is_current_side) - line_num is 1-indexed
    /// is_current_side indicates if we should look at the current (right) or previous (left) version
    pub fn get_focused_line_num(&mut self) -> Option<(usize, bool)> {
        if let Some(rows) = self.get_side_by_side() {
            if let Some(row) = rows.get(self.focused_line) {
                // Prefer the right side (current version) if available
                if let Some(line_num) = row.right_line_num {
                    return Some((line_num, true));
                }
                // Fall back to left side (previous version)
                if let Some(line_num) = row.left_line_num {
                    return Some((line_num, false));
                }
            }
        }
        // For content view (oldest commit), use focused_line + 1
        Some((self.focused_line + 1, true))
    }

    /// Jump to the commit that introduced the currently focused line
    pub fn jump_to_line_origin(&mut self, view_height: usize) -> Result<bool> {
        let (line_num, is_current) = self.get_focused_line_num().context("No line focused")?;

        // Determine which commit/file to blame from
        let (hash, path) = if is_current || self.current_index >= self.commits.len() - 1 {
            let c = &self.commits[self.current_index];
            (c.hash.clone(), c.file_path.clone())
        } else {
            let c = &self.commits[self.current_index + 1];
            (c.hash.clone(), c.file_path.clone())
        };

        let origin_hash = self.repo.blame_line(&hash, &path, line_num)?;

        // Find this commit in our history
        if let Some(idx) = self.commits.iter().position(|c| c.hash == origin_hash) {
            self.current_index = idx;
            self.focused_line = 0;
            self.scroll_offset = 0;
            self.center_on_focused(view_height);
            return Ok(true);
        }

        Ok(false)
    }

    /// Jump to the commit before the one that introduced the currently focused line
    pub fn jump_to_before_line_origin(&mut self, view_height: usize) -> Result<bool> {
        let (line_num, is_current) = self.get_focused_line_num().context("No line focused")?;

        let (hash, path) = if is_current || self.current_index >= self.commits.len() - 1 {
            let c = &self.commits[self.current_index];
            (c.hash.clone(), c.file_path.clone())
        } else {
            let c = &self.commits[self.current_index + 1];
            (c.hash.clone(), c.file_path.clone())
        };

        let origin_hash = self.repo.blame_line(&hash, &path, line_num)?;

        // Find this commit in our history and go to the one after it (older)
        if let Some(idx) = self.commits.iter().position(|c| c.hash == origin_hash) {
            if idx + 1 < self.commits.len() {
                self.current_index = idx + 1;
                self.focused_line = 0;
                self.scroll_offset = 0;
                self.center_on_focused(view_height);
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub fn toggle_commit_details(&mut self) {
        self.show_commit_details = !self.show_commit_details;
        self.details_selected_file = 0;
    }

    pub fn get_commit_details(&self) -> Option<(String, Vec<String>)> {
        let commit = self.current_commit();
        let message = self.repo.get_full_message(&commit.hash).ok()?;
        let files = self.repo.get_commit_files(&commit.hash).ok()?;
        Some((message, files))
    }

    pub fn details_select_next(&mut self) {
        if let Some((_, files)) = self.get_commit_details() {
            if !files.is_empty() {
                self.details_selected_file = (self.details_selected_file + 1) % files.len();
            }
        }
    }

    pub fn details_select_prev(&mut self) {
        if let Some((_, files)) = self.get_commit_details() {
            if !files.is_empty() {
                self.details_selected_file = self.details_selected_file
                    .checked_sub(1)
                    .unwrap_or(files.len() - 1);
            }
        }
    }

    /// Open the selected file from commit details in a new palimpsest instance
    pub fn get_selected_file_path(&self) -> Option<PathBuf> {
        let (_, files) = self.get_commit_details()?;
        let selected = files.get(self.details_selected_file)?;
        // Convert relative path to absolute
        Some(self.repo.workdir().join(selected))
    }

    /// Switch to viewing a different file
    pub fn switch_to_file(&mut self, file_path: PathBuf) -> Result<()> {
        let commits = self.repo.get_file_history(&file_path)?;

        if commits.is_empty() {
            anyhow::bail!("No commits found for file: {}", file_path.display());
        }

        let extension = Highlighter::get_extension(&file_path);

        self.file_path = file_path;
        self.extension = extension;
        self.commits = commits;
        self.current_index = 0;
        self.scroll_offset = 0;
        self.focused_line = 0;
        self.collapsed_mode = false;
        self.expanded_sections.clear();
        self.show_commit_details = false;
        self.details_selected_file = 0;
        self.content_cache.clear();
        self.highlight_cache.clear();

        Ok(())
    }
}
