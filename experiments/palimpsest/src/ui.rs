use crate::app::App;
use crate::git::DiffTag;
use ratatui::{
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Paragraph},
    Frame,
};

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
            draw_diff_view(frame, app, area, rows, inner_height);
        }
        None => {
            draw_content_view(frame, app, area, inner_height);
        }
    }
}

fn highlight_line<'a>(app: &App, content: &str, base_bg: Option<Color>, is_focused: bool) -> Vec<Span<'a>> {
    let highlighted = app.highlighter.highlight_line(content, &app.extension);

    highlighted
        .into_iter()
        .map(|(style, text)| {
            let mut final_style = style;
            if let Some(bg) = base_bg {
                final_style = final_style.bg(bg);
            }
            if is_focused {
                final_style = final_style.add_modifier(Modifier::BOLD);
            }
            Span::styled(text, final_style)
        })
        .collect()
}

fn draw_diff_view(
    frame: &mut Frame,
    app: &App,
    area: Rect,
    rows: Vec<crate::git::SideBySideRow>,
    height: usize,
) {
    // Split area into two columns
    let columns = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(50), Constraint::Percentage(50)])
        .split(area);

    let half_width = columns[0].width.saturating_sub(2) as usize;

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
            Some(DiffTag::Delete) => Some(Color::Rgb(40, 0, 0)),
            _ => None,
        };

        let mut left_num_style = Style::default().fg(Color::DarkGray);
        if is_focused {
            left_num_style = left_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
        }

        let left_content = truncate(&row.left_content, half_width.saturating_sub(7));

        let mut left_spans = vec![
            Span::styled(focus_indicator, Style::default().fg(Color::Yellow)),
            Span::styled(format!("{} ", left_line_num), left_num_style),
        ];

        if row.left_tag.is_some() {
            left_spans.extend(highlight_line(app, &left_content, left_bg, is_focused));
        } else {
            // Empty side - just show background
            let style = if let Some(bg) = left_bg {
                Style::default().bg(bg)
            } else {
                Style::default().fg(Color::DarkGray)
            };
            left_spans.push(Span::styled(pad_to_width(&left_content, half_width.saturating_sub(7)), style));
        }

        left_lines.push(Line::from(left_spans));

        // Right side
        let right_line_num = row
            .right_line_num
            .map(|n| format!("{:4}", n))
            .unwrap_or_else(|| "    ".to_string());

        let right_bg = match row.right_tag {
            Some(DiffTag::Insert) => Some(Color::Rgb(0, 40, 0)),
            _ => None,
        };

        let mut right_num_style = Style::default().fg(Color::DarkGray);
        if is_focused {
            right_num_style = right_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
        }

        let right_content = truncate(&row.right_content, half_width.saturating_sub(6));

        let mut right_spans = vec![
            Span::styled(format!("{} ", right_line_num), right_num_style),
        ];

        if row.right_tag.is_some() {
            right_spans.extend(highlight_line(app, &right_content, right_bg, is_focused));
        } else {
            let style = if let Some(bg) = right_bg {
                Style::default().bg(bg)
            } else {
                Style::default().fg(Color::DarkGray)
            };
            right_spans.push(Span::styled(pad_to_width(&right_content, half_width.saturating_sub(6)), style));
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

fn truncate(s: &str, width: usize) -> String {
    let chars: Vec<char> = s.chars().collect();
    if chars.len() > width {
        chars[..width].iter().collect()
    } else {
        s.to_string()
    }
}

fn pad_to_width(s: &str, width: usize) -> String {
    format!("{:<width$}", s, width = width)
}

fn draw_content_view(frame: &mut Frame, app: &mut App, area: Rect, height: usize) {
    let content_result = app.get_current_content();

    let lines: Vec<Line> = match content_result {
        Ok(content) => {
            content
                .lines()
                .enumerate()
                .map(|(i, line)| {
                    let is_focused = i == app.focused_line;
                    let mut line_num_style = Style::default().fg(Color::DarkGray);

                    if is_focused {
                        line_num_style = line_num_style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
                    }

                    let mut spans = vec![
                        Span::styled(if is_focused { "▶" } else { " " }, Style::default().fg(Color::Yellow)),
                        Span::styled(format!("{:4} ", i + 1), line_num_style),
                    ];

                    spans.extend(highlight_line(app, line, None, is_focused));

                    Line::from(spans)
                })
                .skip(app.scroll_offset)
                .take(height)
                .collect()
        }
        Err(e) => vec![Line::from(Span::styled(
            format!("Error loading content: {}", e),
            Style::default().fg(Color::Red),
        ))],
    };

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

    let footer_text = format!(
        "{}| {} | j/k: focus | n/N: next/prev change | q: quit",
        position, nav_hint
    );

    let footer = Paragraph::new(footer_text)
        .style(Style::default().fg(Color::DarkGray))
        .block(Block::default().borders(Borders::ALL));

    frame.render_widget(footer, area);
}
