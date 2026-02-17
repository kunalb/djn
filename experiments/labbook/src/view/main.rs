use anyhow::Result;
use axum::{
    routing::get,
    Router,
};
use clap::Parser;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::Arc;
use tower_http::services::ServeDir;

mod github;
mod render;
mod routes;

use lab::config;

#[derive(Parser)]
#[command(name = "lab-view")]
#[command(about = "Web interface for browsing lab experiments")]
struct Cli {
    /// Port to run the server on
    #[arg(short, long, default_value = "3000")]
    port: u16,

    /// Path to lab directory (defaults to configured `lab_dir`)
    #[arg(long)]
    lab_dir: Option<PathBuf>,

    /// Don't open browser automatically
    #[arg(long)]
    no_open: bool,
}

pub struct AppState {
    pub lab_dir: PathBuf,
    pub source_repo: PathBuf,
    pub github: Option<github::GitHubRepo>,
    pub syntax_set: syntect::parsing::SyntaxSet,
    pub theme_set: syntect::highlighting::ThemeSet,
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    // Load config
    let cfg = config::load()?;
    let source_repo = config::find_repo_root()?;
    let lab_dir = cli.lab_dir.unwrap_or_else(|| config::expand_path(&cfg.lab_dir));

    if !lab_dir.exists() {
        anyhow::bail!("Lab directory does not exist: {}", lab_dir.display());
    }

    // Detect GitHub remote
    let github = github::detect_github_remote(&source_repo);

    // Load syntax highlighting assets
    let syntax_set = syntect::parsing::SyntaxSet::load_defaults_newlines();
    let theme_set = syntect::highlighting::ThemeSet::load_defaults();

    let state = Arc::new(AppState {
        lab_dir,
        source_repo,
        github,
        syntax_set,
        theme_set,
    });

    // Build router
    let app = Router::new()
        .route("/", get(routes::list_page))
        .route("/list", get(routes::list_partial))
        .route("/exp/{id:.*}", get(routes::detail_page))
        .route("/compare", get(routes::compare_page))
        .route("/diff/{id:.*}/{range}", get(routes::diff_partial))
        .route("/artifact/{id:.*}/{name}", get(routes::artifact))
        .nest_service("/static", ServeDir::new("static"))
        .with_state(state);

    let addr = SocketAddr::from(([127, 0, 0, 1], cli.port));
    println!("Starting lab-view at http://{addr}");

    // Open browser
    if !cli.no_open {
        let url = format!("http://{addr}");
        if let Err(e) = open::that(&url) {
            eprintln!("Failed to open browser: {e}");
        }
    }

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
