use std::path::Path;
use std::process::Command;

#[derive(Clone, Debug)]
pub struct GitHubRepo {
    pub owner: String,
    pub repo: String,
}

/// Detect GitHub remote from git repository
pub fn detect_github_remote(repo_path: &Path) -> Option<GitHubRepo> {
    let output = Command::new("git")
        .args(["remote", "get-url", "origin"])
        .current_dir(repo_path)
        .output()
        .ok()?;

    if !output.status.success() {
        return None;
    }

    let url = String::from_utf8_lossy(&output.stdout).trim().to_string();
    parse_github_url(&url)
}

/// Parse GitHub URL in various formats
fn parse_github_url(url: &str) -> Option<GitHubRepo> {
    // git@github.com:owner/repo.git
    if let Some(rest) = url.strip_prefix("git@github.com:") {
        let rest = rest.strip_suffix(".git").unwrap_or(rest);
        let parts: Vec<&str> = rest.splitn(2, '/').collect();
        if parts.len() == 2 {
            return Some(GitHubRepo {
                owner: parts[0].to_string(),
                repo: parts[1].to_string(),
            });
        }
    }

    // https://github.com/owner/repo.git
    // https://github.com/owner/repo
    if url.contains("github.com") {
        let url = url.strip_suffix(".git").unwrap_or(url);
        let parts: Vec<&str> = url.split('/').collect();
        if parts.len() >= 2 {
            let repo = parts[parts.len() - 1].to_string();
            let owner = parts[parts.len() - 2].to_string();
            if !owner.is_empty() && !repo.is_empty() {
                return Some(GitHubRepo { owner, repo });
            }
        }
    }

    None
}

/// Generate URL to view a commit on GitHub
pub fn commit_url(gh: &GitHubRepo, sha: &str) -> String {
    format!("https://github.com/{}/{}/commit/{}", gh.owner, gh.repo, sha)
}

/// Generate URL to compare two commits on GitHub
#[allow(dead_code)]
pub fn compare_url(gh: &GitHubRepo, from: &str, to: &str) -> String {
    format!(
        "https://github.com/{}/{}/compare/{}...{}",
        gh.owner, gh.repo, from, to
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_ssh_url() {
        let url = "git@github.com:user/repo.git";
        let result = parse_github_url(url);
        assert!(result.is_some());
        let gh = result.unwrap();
        assert_eq!(gh.owner, "user");
        assert_eq!(gh.repo, "repo");
    }

    #[test]
    fn test_parse_https_url() {
        let url = "https://github.com/user/repo.git";
        let result = parse_github_url(url);
        assert!(result.is_some());
        let gh = result.unwrap();
        assert_eq!(gh.owner, "user");
        assert_eq!(gh.repo, "repo");
    }

    #[test]
    fn test_parse_https_url_no_git() {
        let url = "https://github.com/user/repo";
        let result = parse_github_url(url);
        assert!(result.is_some());
        let gh = result.unwrap();
        assert_eq!(gh.owner, "user");
        assert_eq!(gh.repo, "repo");
    }

    #[test]
    fn test_commit_url() {
        let gh = GitHubRepo {
            owner: "user".to_string(),
            repo: "repo".to_string(),
        };
        let url = commit_url(&gh, "abc123");
        assert_eq!(url, "https://github.com/user/repo/commit/abc123");
    }
}
