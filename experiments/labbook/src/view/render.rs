use pulldown_cmark::{Parser, Options, html};
use std::fmt::Write;
use syntect::highlighting::ThemeSet;
use syntect::html::highlighted_html_for_string;
use syntect::parsing::SyntaxSet;

#[derive(Clone)]
pub enum ArtifactContent {
    Image { base64: String, mime: String },
    Code { html: String, language: String },
    Markdown { html: String },
    Text { content: String },
    Download { size: usize },
}

pub fn render_markdown(input: &str) -> String {
    // Skip frontmatter if present
    let content = if let Some(rest) = input.strip_prefix("---") {
        if let Some(end) = rest.find("---") {
            &rest[end + 3..]
        } else {
            input
        }
    } else {
        input
    };

    let options = Options::all();
    let parser = Parser::new_ext(content, options);
    let mut html_output = String::new();
    html::push_html(&mut html_output, parser);
    html_output
}

pub fn render_artifact(
    name: &str,
    content: &[u8],
    syntax_set: &SyntaxSet,
    theme_set: &ThemeSet,
) -> ArtifactContent {
    let ext = name.rsplit('.').next().unwrap_or("");

    match ext {
        "png" => ArtifactContent::Image {
            base64: base64::Engine::encode(&base64::engine::general_purpose::STANDARD, content),
            mime: "image/png".to_string(),
        },
        "jpg" | "jpeg" => ArtifactContent::Image {
            base64: base64::Engine::encode(&base64::engine::general_purpose::STANDARD, content),
            mime: "image/jpeg".to_string(),
        },
        "gif" => ArtifactContent::Image {
            base64: base64::Engine::encode(&base64::engine::general_purpose::STANDARD, content),
            mime: "image/gif".to_string(),
        },
        "svg" => ArtifactContent::Image {
            base64: base64::Engine::encode(&base64::engine::general_purpose::STANDARD, content),
            mime: "image/svg+xml".to_string(),
        },
        "yaml" | "yml" | "json" | "toml" | "py" | "rs" | "js" | "ts" | "sh" | "bash" => {
            if let Ok(text) = std::str::from_utf8(content) {
                let html = highlight_code(text, ext, syntax_set, theme_set);
                ArtifactContent::Code {
                    html,
                    language: ext.to_string(),
                }
            } else {
                ArtifactContent::Download { size: content.len() }
            }
        }
        "md" | "markdown" => {
            if let Ok(text) = std::str::from_utf8(content) {
                ArtifactContent::Markdown {
                    html: render_markdown(text),
                }
            } else {
                ArtifactContent::Download { size: content.len() }
            }
        }
        "txt" | "log" | "csv" => {
            if let Ok(text) = std::str::from_utf8(content) {
                ArtifactContent::Text {
                    content: text.to_string(),
                }
            } else {
                ArtifactContent::Download { size: content.len() }
            }
        }
        _ => ArtifactContent::Download { size: content.len() },
    }
}

pub fn highlight_code(
    code: &str,
    extension: &str,
    syntax_set: &SyntaxSet,
    theme_set: &ThemeSet,
) -> String {
    let syntax = syntax_set
        .find_syntax_by_extension(extension)
        .unwrap_or_else(|| syntax_set.find_syntax_plain_text());

    let theme = &theme_set.themes["base16-ocean.dark"];

    highlighted_html_for_string(code, syntax_set, syntax, theme).unwrap_or_else(|_| {
        format!("<pre><code>{}</code></pre>", html_escape(code))
    })
}

pub fn highlight_diff(diff: &str) -> String {
    let mut html = String::from("<pre class=\"diff\">");

    for line in diff.lines() {
        let class = if line.starts_with('+') && !line.starts_with("+++") {
            "add"
        } else if line.starts_with('-') && !line.starts_with("---") {
            "del"
        } else if line.starts_with("@@") {
            "hunk"
        } else if line.starts_with("diff ") || line.starts_with("index ") {
            "meta"
        } else {
            ""
        };

        if class.is_empty() {
            let _ = writeln!(html, "<span>{}</span>", html_escape(line));
        } else {
            let _ = writeln!(html, "<span class=\"{}\">{}</span>", class, html_escape(line));
        }
    }

    html.push_str("</pre>");
    html
}

fn html_escape(s: &str) -> String {
    s.replace('&', "&amp;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('"', "&quot;")
}
