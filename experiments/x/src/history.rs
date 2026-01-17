use rusqlite::{Connection, params};
use std::path::PathBuf;

/// Outcome of user interaction with a generated command
#[derive(Debug, Clone, Copy)]
pub enum Outcome {
    Executed,   // User accepted and ran the command
    Edited,     // User edited then ran the command
    Cancelled,  // User cancelled (n/no/q)
    Refined,    // User requested refinement
}

impl Outcome {
    pub fn as_str(&self) -> &'static str {
        match self {
            Outcome::Executed => "executed",
            Outcome::Edited => "edited",
            Outcome::Cancelled => "cancelled",
            Outcome::Refined => "refined",
        }
    }
}

/// A single command generation record
pub struct HistoryEntry {
    pub cwd: String,
    pub request: String,
    pub provider: String,
    pub model: Option<String>,
    pub duration_ms: u64,
    pub user_time_us: u64,
    pub system_time_us: u64,
    pub max_rss_kb: u64,
    pub command: String,
    pub outcome: Outcome,
    pub edited_command: Option<String>,
    pub session_id: String,
    pub refinement_of: Option<i64>,
    pub exit_code: Option<i32>,
}

/// SQLite-backed history tracker
pub struct History {
    db: Connection,
}

impl History {
    /// Open or create the history database
    pub fn open() -> Result<Self, String> {
        let path = Self::db_path()?;

        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create history directory: {}", e))?;
        }

        let db = Connection::open(&path)
            .map_err(|e| format!("Failed to open history database: {}", e))?;

        // Create table if it doesn't exist
        db.execute_batch(
            "CREATE TABLE IF NOT EXISTS generations (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                timestamp TEXT NOT NULL DEFAULT (datetime('now')),

                -- Context
                cwd TEXT NOT NULL,

                -- Request
                request TEXT NOT NULL,

                -- Provider/Model
                provider TEXT NOT NULL,
                model TEXT,

                -- Timing and resource usage
                duration_ms INTEGER NOT NULL,
                user_time_us INTEGER NOT NULL DEFAULT 0,
                system_time_us INTEGER NOT NULL DEFAULT 0,
                max_rss_kb INTEGER NOT NULL DEFAULT 0,

                -- Result
                command TEXT NOT NULL,

                -- User interaction
                outcome TEXT NOT NULL,
                edited_command TEXT,

                -- Refinement tracking
                session_id TEXT NOT NULL,
                refinement_of INTEGER,

                -- Execution result
                exit_code INTEGER
            );

            CREATE INDEX IF NOT EXISTS idx_timestamp ON generations(timestamp);
            CREATE INDEX IF NOT EXISTS idx_session ON generations(session_id);",
        )
        .map_err(|e| format!("Failed to initialize history schema: {}", e))?;

        Ok(History { db })
    }

    /// Record a command generation
    pub fn record(&self, entry: &HistoryEntry) -> Result<i64, String> {
        self.db
            .execute(
                "INSERT INTO generations (
                    cwd, request, provider, model, duration_ms,
                    user_time_us, system_time_us, max_rss_kb,
                    command, outcome, edited_command, session_id, refinement_of, exit_code
                ) VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8, ?9, ?10, ?11, ?12, ?13, ?14)",
                params![
                    entry.cwd,
                    entry.request,
                    entry.provider,
                    entry.model,
                    entry.duration_ms,
                    entry.user_time_us,
                    entry.system_time_us,
                    entry.max_rss_kb,
                    entry.command,
                    entry.outcome.as_str(),
                    entry.edited_command,
                    entry.session_id,
                    entry.refinement_of,
                    entry.exit_code,
                ],
            )
            .map_err(|e| format!("Failed to record history: {}", e))?;

        Ok(self.db.last_insert_rowid())
    }

    /// Get the database path following XDG conventions
    fn db_path() -> Result<PathBuf, String> {
        let data_dir = std::env::var("XDG_DATA_HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                dirs::home_dir()
                    .unwrap_or_else(|| PathBuf::from("."))
                    .join(".local/share")
            })
            .join("x");

        Ok(data_dir.join("history.db"))
    }
}
