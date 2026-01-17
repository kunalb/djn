use crate::app::{App, CollapsedRow, HighlightedLine};
use crate::git::DiffTag;
use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Clear, Paragraph, Wrap},
    Frame,
};

// More visible diff colors
const DELETE_BG: Color = Color::Rgb(80, 20, 20);
const INSERT_BG: Color = Color::Rgb(20, 60, 20);

pub fn draw(frame: &mut Frame, app: &mut App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3),
            Constraint::Min(0),
            Constraint::Length(3),
        ])
        .split(frame.area());

    draw_header(frame, app, chunks[0]);
    draw_content(frame, app, chunks[1]);
    draw_footer(frame, app, chunks[2]);

    // Draw commit details popup if active
    if app.show_commit_details {
        draw_commit_details(frame, app);
    }
}

fn draw_header(frame: &mut Frame, app: &App, area: Rect) {
    let commit = app.current_commit();

    let header_text = format!(
        " {} | {} | {} | {}",
        commit.short_hash,
        commit.author,
        commit.date.format("%Y-%m-%d %H:%M"),
        commit.message
    );

    let header = Paragraph::new(header_text)
        .style(Style::default().fg(Color::White))
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(format!(" {} ", app.file_path.display()))
                .title_style(Style::default().add_modifier(Modifier::BOLD)),
        );

    frame.render_widget(header, area);
}

fn draw_content(frame: &mut Frame, app: &mut App, area: Rect) {
    let inner_height = area.height.saturating_sub(2) as usize;

    // Try to get diff view, fall back to content view
    match app.get_side_by_side() {
        Some(rows) => {
            if app.collapsed_mode {
                draw_collapsed_diff_view(frame, app, area, rows, inner_height);
            } else {
                draw_diff_view(frame, app, area, rows, inner_height);
            }
        }
        None => {
            draw_content_view(frame, app, area, inner_height);
        }
    }
}

fn apply_highlight_with_bg<'a>(
    highlighted: &HighlightedLine,
    base_bg: Option<Color>,
    is_focused: bool,
    max_width: usize,
) -> Vec<Span<'a>> {
    let mut spans = Vec::new();
    let mut total_len = 0;

    for (style, text) in highlighted {
        if total_len >= max_width {
            break;
        }

        let remaining = max_width - total_len;
        let display_text: String = text.chars().take(remaining).collect();
        total_len += display_text.len();

        let mut final_style = *style;
        if let Some(bg) = base_bg {
            final_style = final_style.bg(bg);
        }
        if is_focused {
            final_style = final_style.add_modifier(Modifier::BOLD);
        }
        spans.push(Span::styled(display_text, final_style));
    }

    spans
}

fn draw_collapsed_diff_view(
    frame: &mut Frame,
    app: &mut App,
    area: Rect,
    full_rows: Vec<crate::git::SideBySideRow>,
    height: usize,
) {
    let collapsed_rows = match app.get_collapsed_rows() {
        Some(rows) => rows,
        None => return,
    };

    // Get highlighted content for both versions
    let current_hash = app.commits[app.current_index].hash.clone();
    let current_path = app.commits[app.current_index].file_path.clone();
    let previous_hash = app.commits[app.current_index + 1].hash.clone();
    let previous_path = app.commits[app.current_index + 1].file_path.clone();

    let current_highlighted = app
        .get_highlighted_content(&current_hash, &current_path)
        .unwrap_or_default();
    let previous_highlighted = app
        .get_highlighted_content(&previous_hash, &previous_path)
        .unwrap_or_default();

    // Split area into two columns
    let columns = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);

    let half_width = columns[0].width.saturating_sub(2) as usize;
    let content_width = half_width.saturating_sub(7);

    let mut left_lines: Vec<Line> = Vec::new();
    let mut right_lines: Vec<Line> = Vec::new();

    for (idx, collapsed_row) in collapsed_rows.iter().enumerate() {
        let is_focused = idx == app.focused_line;
        let focus_indicator = if is_focused { "▶" } else { " " };

        match collapsed_row {
            CollapsedRow::Normal(row_idx) => {
                let row = &full_rows[*row_idx];

                // Left side
                let left_line_num = row
                    .left_line_num
                    .map(|n| format!("{:4}", n))
                    .unwrap_or_else(|| "    ".to_string());

                let left_bg = match row.left_tag {
                    Some(DiffTag::Delete) => Some(DELETE_BG),
                    _ => None,
                };

                let mut left_num_style = Style::default().fg(Color::DarkGray);
                if is_focused {
                    left_num_style = left_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
                }
                if row.left_tag == Some(DiffTag::Delete) {
                    left_num_style = left_num_style.bg(DELETE_BG);
                }

                // Diff sign for accessibility (- for deletions)
                let left_sign = match row.left_tag {
                    Some(DiffTag::Delete) => Span::styled("-", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
                    _ => Span::raw(" "),
                };

                let mut left_spans = vec![
                    Span::styled(focus_indicator, Style::default().fg(Color::Yellow)),
                    Span::styled(format!("{}", left_line_num), left_num_style),
                    left_sign,
                    Span::raw(" "),
                ];

                if let Some(line_num) = row.left_line_num {
                    if let Some(highlighted) = previous_highlighted.get(line_num - 1) {
                        left_spans.extend(apply_highlight_with_bg(highlighted, left_bg, is_focused, content_width));
                    }
                }

                if left_bg.is_some() {
                    left_spans.push(Span::styled(
                        " ".repeat(content_width),
                        Style::default().bg(left_bg.unwrap()),
                    ));
                }

                left_lines.push(Line::from(left_spans));

                // Right side
                let right_line_num = row
                    .right_line_num
                    .map(|n| format!("{:4}", n))
                    .unwrap_or_else(|| "    ".to_string());

                let right_bg = match row.right_tag {
                    Some(DiffTag::Insert) => Some(INSERT_BG),
                    _ => None,
                };

                let mut right_num_style = Style::default().fg(Color::DarkGray);
                if is_focused {
                    right_num_style = right_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
                }
                if row.right_tag == Some(DiffTag::Insert) {
                    right_num_style = right_num_style.bg(INSERT_BG);
                }

                // Diff sign for accessibility (+ for insertions)
                let right_sign = match row.right_tag {
                    Some(DiffTag::Insert) => Span::styled("+", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
                    _ => Span::raw(" "),
                };

                let mut right_spans = vec![
                    Span::styled(format!("{}", right_line_num), right_num_style),
                    right_sign,
                    Span::raw(" "),
                ];

                if let Some(line_num) = row.right_line_num {
                    if let Some(highlighted) = current_highlighted.get(line_num - 1) {
                        right_spans.extend(apply_highlight_with_bg(highlighted, right_bg, is_focused, content_width));
                    }
                }

                if right_bg.is_some() {
                    right_spans.push(Span::styled(
                        " ".repeat(content_width),
                        Style::default().bg(right_bg.unwrap()),
                    ));
                }

                right_lines.push(Line::from(right_spans));
            }
            CollapsedRow::Collapsed { hidden_count, .. } => {
                // Render collapsed section indicator
                let collapse_style = if is_focused {
                    Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD)
                } else {
                    Style::default().fg(Color::DarkGray)
                };

                let collapse_text = format!("··· {} lines hidden (Tab to expand) ···", hidden_count);

                let left_spans = vec![
                    Span::styled(focus_indicator, Style::default().fg(Color::Yellow)),
                    Span::styled(collapse_text.clone(), collapse_style),
                ];
                left_lines.push(Line::from(left_spans));

                let right_spans = vec![
                    Span::styled(collapse_text, collapse_style),
                ];
                right_lines.push(Line::from(right_spans));
            }
        }
    }

    // Apply scrolling
    let visible_left: Vec<Line> = left_lines
        .into_iter()
        .skip(app.scroll_offset)
        .take(height)
        .collect();

    let visible_right: Vec<Line> = right_lines
        .into_iter()
        .skip(app.scroll_offset)
        .take(height)
        .collect();

    let left_panel = Paragraph::new(visible_left).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Previous (collapsed) ")
            .title_style(Style::default().fg(Color::Red)),
    );

    let right_panel = Paragraph::new(visible_right).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Current (collapsed) ")
            .title_style(Style::default().fg(Color::Green)),
    );

    frame.render_widget(left_panel, columns[0]);
    frame.render_widget(right_panel, columns[1]);
}

fn draw_diff_view(
    frame: &mut Frame,
    app: &mut App,
    area: Rect,
    rows: Vec<crate::git::SideBySideRow>,
    height: usize,
) {
    // Get highlighted content for both versions
    let current_hash = app.commits[app.current_index].hash.clone();
    let current_path = app.commits[app.current_index].file_path.clone();
    let previous_hash = app.commits[app.current_index + 1].hash.clone();
    let previous_path = app.commits[app.current_index + 1].file_path.clone();

    let current_highlighted = app
        .get_highlighted_content(&current_hash, &current_path)
        .unwrap_or_default();
    let previous_highlighted = app
        .get_highlighted_content(&previous_hash, &previous_path)
        .unwrap_or_default();

    // Split area into two columns
    let columns = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);

    let half_width = columns[0].width.saturating_sub(2) as usize;
    let content_width = half_width.saturating_sub(7);

    let mut left_lines: Vec<Line> = Vec::new();
    let mut right_lines: Vec<Line> = Vec::new();

    for (idx, row) in rows.iter().enumerate() {
        let is_focused = idx == app.focused_line;
        let focus_indicator = if is_focused { "▶" } else { " " };

        // Left side
        let left_line_num = row
            .left_line_num
            .map(|n| format!("{:4}", n))
            .unwrap_or_else(|| "    ".to_string());

        let left_bg = match row.left_tag {
            Some(DiffTag::Delete) => Some(DELETE_BG),
            _ => None,
        };

        let mut left_num_style = Style::default().fg(Color::DarkGray);
        if is_focused {
            left_num_style = left_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
        }
        if row.left_tag == Some(DiffTag::Delete) {
            left_num_style = left_num_style.bg(DELETE_BG);
        }

        // Diff sign for accessibility (- for deletions)
        let left_sign = match row.left_tag {
            Some(DiffTag::Delete) => Span::styled("-", Style::default().fg(Color::Red).add_modifier(Modifier::BOLD)),
            _ => Span::raw(" "),
        };

        let mut left_spans = vec![
            Span::styled(focus_indicator, Style::default().fg(Color::Yellow)),
            Span::styled(format!("{}", left_line_num), left_num_style),
            left_sign,
            Span::raw(" "),
        ];

        if let Some(line_num) = row.left_line_num {
            // Get highlighted content for this line
            if let Some(highlighted) = previous_highlighted.get(line_num - 1) {
                left_spans.extend(apply_highlight_with_bg(highlighted, left_bg, is_focused, content_width));
            }
        }

        // Pad the line with background color if needed
        if left_bg.is_some() {
            left_spans.push(Span::styled(
                " ".repeat(content_width),
                Style::default().bg(left_bg.unwrap()),
            ));
        }

        left_lines.push(Line::from(left_spans));

        // Right side
        let right_line_num = row
            .right_line_num
            .map(|n| format!("{:4}", n))
            .unwrap_or_else(|| "    ".to_string());

        let right_bg = match row.right_tag {
            Some(DiffTag::Insert) => Some(INSERT_BG),
            _ => None,
        };

        let mut right_num_style = Style::default().fg(Color::DarkGray);
        if is_focused {
            right_num_style = right_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
        }
        if row.right_tag == Some(DiffTag::Insert) {
            right_num_style = right_num_style.bg(INSERT_BG);
        }

        // Diff sign for accessibility (+ for insertions)
        let right_sign = match row.right_tag {
            Some(DiffTag::Insert) => Span::styled("+", Style::default().fg(Color::Green).add_modifier(Modifier::BOLD)),
            _ => Span::raw(" "),
        };

        let mut right_spans = vec![
            Span::styled(format!("{}", right_line_num), right_num_style),
            right_sign,
            Span::raw(" "),
        ];

        if let Some(line_num) = row.right_line_num {
            if let Some(highlighted) = current_highlighted.get(line_num - 1) {
                right_spans.extend(apply_highlight_with_bg(highlighted, right_bg, is_focused, content_width));
            }
        }

        // Pad the line with background color if needed
        if right_bg.is_some() {
            right_spans.push(Span::styled(
                " ".repeat(content_width),
                Style::default().bg(right_bg.unwrap()),
            ));
        }

        right_lines.push(Line::from(right_spans));
    }

    // Apply scrolling
    let visible_left: Vec<Line> = left_lines
        .into_iter()
        .skip(app.scroll_offset)
        .take(height)
        .collect();

    let visible_right: Vec<Line> = right_lines
        .into_iter()
        .skip(app.scroll_offset)
        .take(height)
        .collect();

    let left_panel = Paragraph::new(visible_left).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Previous ")
            .title_style(Style::default().fg(Color::Red)),
    );

    let right_panel = Paragraph::new(visible_right).block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Current ")
            .title_style(Style::default().fg(Color::Green)),
    );

    frame.render_widget(left_panel, columns[0]);
    frame.render_widget(right_panel, columns[1]);
}

fn draw_content_view(frame: &mut Frame, app: &mut App, area: Rect, height: usize) {
    let hash = app.commits[app.current_index].hash.clone();
    let path = app.commits[app.current_index].file_path.clone();
    let highlighted = app
        .get_highlighted_content(&hash, &path)
        .unwrap_or_default();

    let lines: Vec<Line> = highlighted
        .iter()
        .enumerate()
        .map(|(i, line_spans)| {
            let is_focused = i == app.focused_line;
            let mut line_num_style = Style::default().fg(Color::DarkGray);

            if is_focused {
                line_num_style = line_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
            }

            let mut spans = vec![
                Span::styled(if is_focused { "▶" } else { " " }, Style::default().fg(Color::Yellow)),
                Span::styled(format!("{:4} ", i + 1), line_num_style),
            ];

            spans.extend(apply_highlight_with_bg(line_spans, None, is_focused, usize::MAX));

            Line::from(spans)
        })
        .skip(app.scroll_offset)
        .take(height)
        .collect();

    let paragraph = Paragraph::new(lines);

    let content = paragraph.block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Content (oldest commit) ")
            .title_style(Style::default().fg(Color::Cyan)),
    );

    frame.render_widget(content, area);
}

fn draw_footer(frame: &mut Frame, app: &App, area: Rect) {
    let position = format!(
        " Commit {} of {} ",
        app.position_from_end(),
        app.total_commits()
    );

    let nav_hint = if app.current_index == 0 {
        "← older"
    } else if app.current_index == app.commits.len() - 1 {
        "→ newer"
    } else {
        "← older | → newer"
    };

    let collapse_hint = if app.collapsed_mode {
        "c: expand all | Tab: expand"
    } else {
        "c: collapse"
    };

    let footer_text = format!(
        "{}| {} | j/k: focus | n/N: change | b/B: blame | i: info | {} | q: quit",
        position, nav_hint, collapse_hint
    );

    let footer = Paragraph::new(footer_text)
        .style(Style::default().fg(Color::DarkGray))
        .block(Block::default().borders(Borders::ALL));

    frame.render_widget(footer, area);
}

fn draw_commit_details(frame: &mut Frame, app: &App) {
    let (message, files) = match app.get_commit_details() {
        Some(details) => details,
        None => return,
    };

    let commit = app.current_commit();
    let area = frame.area();

    // Center popup, 80% width, 70% height
    let popup_width = (area.width as f32 * 0.8) as u16;
    let popup_height = (area.height as f32 * 0.7) as u16;
    let popup_x = (area.width - popup_width) / 2;
    let popup_y = (area.height - popup_height) / 2;

    let popup_area = Rect::new(popup_x, popup_y, popup_width, popup_height);

    // Clear the background
    frame.render_widget(Clear, popup_area);

    // Split popup into message area and files list
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(3), // Header
            Constraint::Min(5),    // Message
            Constraint::Length(files.len().min(10) as u16 + 2), // Files list
            Constraint::Length(2), // Help
        ])
        .split(popup_area);

    // Header with commit info
    let header = Paragraph::new(format!(
        " {} | {} | {}",
        commit.hash[..12].to_string(),
        commit.author,
        commit.date.format("%Y-%m-%d %H:%M:%S")
    ))
    .style(Style::default().fg(Color::White))
    .block(
        Block::default()
            .borders(Borders::ALL)
            .title(" Commit Details ")
            .title_style(Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)),
    );
    frame.render_widget(header, chunks[0]);

    // Full commit message
    let message_widget = Paragraph::new(message.trim())
        .wrap(Wrap { trim: false })
        .style(Style::default().fg(Color::White))
        .block(
            Block::default()
                .borders(Borders::ALL)
                .title(" Message ")
                .title_style(Style::default().fg(Color::Cyan)),
        );
    frame.render_widget(message_widget, chunks[1]);

    // Files list with selection
    let file_lines: Vec<Line> = files
        .iter()
        .enumerate()
        .map(|(i, file)| {
            let is_selected = i == app.details_selected_file;
            let prefix = if is_selected { "▶ " } else { "  " };
            let style = if is_selected {
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::White)
            };
            Line::from(Span::styled(format!("{}{}", prefix, file), style))
        })
        .collect();

    let files_widget = Paragraph::new(file_lines).block(
        Block::default()
            .borders(Borders::ALL)
            .title(format!(" Files Changed ({}) ", files.len()))
            .title_style(Style::default().fg(Color::Magenta)),
    );
    frame.render_widget(files_widget, chunks[2]);

    // Help text
    let help = Paragraph::new(" j/k: select file | Enter: open file | i/Esc: close")
        .style(Style::default().fg(Color::DarkGray))
        .block(Block::default().borders(Borders::TOP));
    frame.render_widget(help, chunks[3]);
}
