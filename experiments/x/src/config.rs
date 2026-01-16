use serde::Deserialize;
use std::path::PathBuf;

#[derive(Debug, Deserialize)]
pub struct Config {
    #[serde(default = "default_provider")]
    pub default_provider: String,

    #[serde(default)]
    pub claude: ProviderConfig,

    #[serde(default)]
    pub openai: ProviderConfig,

    #[serde(default)]
    pub gemini: ProviderConfig,
}

#[derive(Debug, Deserialize, Default)]
pub struct ProviderConfig {
    #[serde(default)]
    pub model: Option<String>,
}

fn default_provider() -> String {
    "gemini".to_string()
}

impl Default for Config {
    fn default() -> Self {
        Config {
            default_provider: default_provider(),
            claude: ProviderConfig::default(),
            openai: ProviderConfig::default(),
            gemini: ProviderConfig::default(),
        }
    }
}

impl Config {
    pub fn load() -> Self {
        let config_path = Self::config_path();

        if config_path.exists() {
            if let Ok(content) = std::fs::read_to_string(&config_path) {
                if let Ok(config) = toml::from_str(&content) {
                    return config;
                }
            }
        }

        Config::default()
    }

    pub fn config_path() -> PathBuf {
        // Respect XDG_CONFIG_HOME, fall back to ~/.config
        std::env::var("XDG_CONFIG_HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                dirs::home_dir()
                    .unwrap_or_else(|| PathBuf::from("."))
                    .join(".config")
            })
            .join("x")
            .join("config.toml")
    }

    pub fn get_model(&self, provider: &str) -> Option<String> {
        match provider {
            "claude" => self.claude.model.clone(),
            "openai" => self.openai.model.clone(),
            "gemini" => self.gemini.model.clone(),
            _ => None,
        }
    }

    /// Open config in $EDITOR, creating template if needed
    pub fn edit() -> Result<(), String> {
        let path = Self::config_path();

        // Create parent directory if needed
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent)
                .map_err(|e| format!("Failed to create config directory: {}", e))?;
        }

        // Create template if file doesn't exist
        if !path.exists() {
            std::fs::write(&path, Self::template())
                .map_err(|e| format!("Failed to write config template: {}", e))?;
        }

        // Get editor from $EDITOR or $VISUAL, fall back to vi
        let editor = std::env::var("EDITOR")
            .or_else(|_| std::env::var("VISUAL"))
            .unwrap_or_else(|_| "vi".to_string());

        // Open editor
        std::process::Command::new(&editor)
            .arg(&path)
            .status()
            .map_err(|e| format!("Failed to open {}: {}", editor, e))?;

        Ok(())
    }

    fn template() -> &'static str {
        r#"# x configuration
# Default provider: gemini, claude, or codex
default_provider = "gemini"

[gemini]
# model = "gemini-2.5-flash"    # fast (default)
# model = "gemini-2.5-pro"      # powerful

[claude]
# model = "sonnet"              # alias for latest sonnet (default)
# model = "opus"                # alias for latest opus
# model = "haiku"               # alias for latest haiku (fast)

[openai]
# model = "gpt-5-codex"         # default
# model = "gpt-5.2-codex"       # latest, most advanced
# model = "gpt-5-codex-mini"    # faster
"#
    }
}
