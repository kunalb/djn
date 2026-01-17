mod app;
mod git;
mod highlight;
mod ui;

use anyhow::Result;
use app::App;
use clap::Parser;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind, KeyModifiers, MouseEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::prelude::*;
use std::io;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "palimpsest")]
#[command(about = "TUI for exploring git history of individual files")]
struct Cli {
    /// Path to the file to explore
    file: PathBuf,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let file_path = cli.file.canonicalize().unwrap_or(cli.file);

    let mut app = App::new(file_path)?;

    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let result = run_app(&mut terminal, &mut app);

    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    if let Err(e) = result {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }

    Ok(())
}

fn run_app<B: Backend>(terminal: &mut Terminal<B>, app: &mut App) -> Result<()> {
    // Content area starts at row 3 (after header) and has 1 row border
    const CONTENT_START_ROW: u16 = 4;

    loop {
        terminal.draw(|f| ui::draw(f, app))?;

        let view_height = terminal.size()?.height as usize - 6;

        match event::read()? {
            Event::Key(key) => {
                if key.kind != KeyEventKind::Press {
                    continue;
                }

                match key.code {
                    KeyCode::Char('q') => {
                        app.should_quit = true;
                    }
                    KeyCode::Char('i') | KeyCode::Esc => {
                        if app.show_commit_details {
                            app.toggle_commit_details();
                        } else if key.code == KeyCode::Char('i') {
                            app.toggle_commit_details();
                        }
                    }
                    KeyCode::Left | KeyCode::Char('h') => {
                        app.go_back();
                    }
                    KeyCode::Right | KeyCode::Char('l') => {
                        app.go_forward();
                    }
                    KeyCode::Down | KeyCode::Char('j') => {
                        app.focus_down(view_height);
                    }
                    KeyCode::Up | KeyCode::Char('k') => {
                        app.focus_up(view_height);
                    }
                    KeyCode::Char('n') => {
                        if key.modifiers.contains(KeyModifiers::SHIFT) {
                            app.jump_to_prev_change(view_height);
                        } else {
                            app.jump_to_next_change(view_height);
                        }
                    }
                    KeyCode::Char('N') => {
                        app.jump_to_prev_change(view_height);
                    }
                    KeyCode::PageDown | KeyCode::Char(' ') => {
                        app.page_down(view_height);
                    }
                    KeyCode::PageUp => {
                        app.page_up(view_height);
                    }
                    KeyCode::Home | KeyCode::Char('g') => {
                        app.focused_line = 0;
                        app.scroll_offset = 0;
                    }
                    KeyCode::Char('G') => {
                        // Go to end - we'd need to know total lines, approximate for now
                        app.focused_line = usize::MAX / 2;
                    }
                    KeyCode::Char('b') => {
                        // Jump to commit that introduced this line (blame)
                        let _ = app.jump_to_line_origin(view_height);
                    }
                    KeyCode::Char('B') => {
                        // Jump to commit before the one that introduced this line
                        let _ = app.jump_to_before_line_origin(view_height);
                    }
                    KeyCode::Char('c') => {
                        // Toggle collapsed mode
                        app.toggle_collapsed_mode();
                    }
                    KeyCode::Tab => {
                        // Expand collapsed section at focus
                        app.expand_at_focus();
                    }
                    KeyCode::Char('[') => {
                        if app.show_commit_details {
                            app.details_select_prev();
                        }
                    }
                    KeyCode::Char(']') => {
                        if app.show_commit_details {
                            app.details_select_next();
                        }
                    }
                    KeyCode::Enter => {
                        if app.show_commit_details {
                            // Open selected file
                            if let Some(file_path) = app.get_selected_file_path() {
                                if file_path.exists() {
                                    // Switch to viewing this file
                                    let _ = app.switch_to_file(file_path);
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
            Event::Mouse(mouse) => {
                match mouse.kind {
                    MouseEventKind::ScrollUp => {
                        app.focus_up(view_height);
                        app.focus_up(view_height);
                        app.focus_up(view_height);
                    }
                    MouseEventKind::ScrollDown => {
                        app.focus_down(view_height);
                        app.focus_down(view_height);
                        app.focus_down(view_height);
                    }
                    MouseEventKind::Down(_) => {
                        // Click to set focus line
                        if mouse.row >= CONTENT_START_ROW {
                            let clicked_line = app.scroll_offset + (mouse.row - CONTENT_START_ROW) as usize;
                            app.focused_line = clicked_line;
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        if app.should_quit {
            return Ok(());
        }
    }
}
