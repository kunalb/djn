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

    fn config_path() -> PathBuf {
        dirs::config_dir()
            .unwrap_or_else(|| PathBuf::from("."))
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
}
