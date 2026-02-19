use std::fmt;

/// A semantic version: major.minor.patch
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Version {
    pub major: u64,
    pub minor: u64,
    pub patch: u64,
}

impl Version {
    pub fn new(major: u64, minor: u64, patch: u64) -> Self {
        Self {
            major,
            minor,
            patch,
        }
    }

    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.strip_prefix('v').unwrap_or(s);
        let parts: Vec<&str> = s.split('.').collect();
        match parts.len() {
            1 => {
                let major = parts[0]
                    .parse()
                    .map_err(|_| format!("invalid version: {s}"))?;
                Ok(Self::new(major, 0, 0))
            }
            2 => {
                let major = parts[0]
                    .parse()
                    .map_err(|_| format!("invalid version: {s}"))?;
                let minor = parts[1]
                    .parse()
                    .map_err(|_| format!("invalid version: {s}"))?;
                Ok(Self::new(major, minor, 0))
            }
            3 => {
                let major = parts[0]
                    .parse()
                    .map_err(|_| format!("invalid version: {s}"))?;
                let minor = parts[1]
                    .parse()
                    .map_err(|_| format!("invalid version: {s}"))?;
                let patch = parts[2]
                    .parse()
                    .map_err(|_| format!("invalid version: {s}"))?;
                Ok(Self::new(major, minor, patch))
            }
            _ => Err(format!("invalid version: {s}")),
        }
    }
}

impl fmt::Display for Version {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl PartialOrd for Version {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.major
            .cmp(&other.major)
            .then(self.minor.cmp(&other.minor))
            .then(self.patch.cmp(&other.patch))
    }
}

/// A version constraint: ^1.2.3, ~1.2.3, =1.2.3, >=1.2.3, or *
#[derive(Debug, Clone)]
pub enum VersionConstraint {
    /// ^1.2.3 — compatible: >=1.2.3, <2.0.0
    Caret(Version),
    /// ~1.2.3 — patch-level: >=1.2.3, <1.3.0
    Tilde(Version),
    /// =1.2.3 — exact match
    Exact(Version),
    /// >=1.2.3
    Gte(Version),
    /// * — any version
    Any,
}

impl VersionConstraint {
    pub fn parse(s: &str) -> Result<Self, String> {
        let s = s.trim();
        if s == "*" {
            return Ok(Self::Any);
        }
        if let Some(rest) = s.strip_prefix('^') {
            return Ok(Self::Caret(Version::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('~') {
            return Ok(Self::Tilde(Version::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix(">=") {
            return Ok(Self::Gte(Version::parse(rest)?));
        }
        if let Some(rest) = s.strip_prefix('=') {
            return Ok(Self::Exact(Version::parse(rest)?));
        }
        // Bare version = caret
        Ok(Self::Caret(Version::parse(s)?))
    }

    pub fn matches(&self, v: &Version) -> bool {
        match self {
            Self::Any => true,
            Self::Exact(req) => v == req,
            Self::Gte(req) => v >= req,
            Self::Caret(req) => {
                if v < req {
                    return false;
                }
                if req.major == 0 {
                    if req.minor == 0 {
                        // ^0.0.x — exact patch
                        v.major == 0 && v.minor == 0 && v.patch == req.patch
                    } else {
                        // ^0.y.z — same minor
                        v.major == 0 && v.minor == req.minor
                    }
                } else {
                    // ^x.y.z — same major
                    v.major == req.major
                }
            }
            Self::Tilde(req) => {
                if v < req {
                    return false;
                }
                v.major == req.major && v.minor == req.minor
            }
        }
    }

    /// Return the minimum version that satisfies this constraint (for MVS).
    pub fn minimum(&self) -> Version {
        match self {
            Self::Any => Version::new(0, 0, 0),
            Self::Exact(v) | Self::Caret(v) | Self::Tilde(v) | Self::Gte(v) => v.clone(),
        }
    }
}

impl fmt::Display for VersionConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Any => write!(f, "*"),
            Self::Exact(v) => write!(f, "={v}"),
            Self::Caret(v) => write!(f, "^{v}"),
            Self::Tilde(v) => write!(f, "~{v}"),
            Self::Gte(v) => write!(f, ">={v}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_version() {
        assert_eq!(Version::parse("1.2.3").unwrap(), Version::new(1, 2, 3));
        assert_eq!(Version::parse("v1.2.3").unwrap(), Version::new(1, 2, 3));
        assert_eq!(Version::parse("0.1").unwrap(), Version::new(0, 1, 0));
        assert_eq!(Version::parse("2").unwrap(), Version::new(2, 0, 0));
    }

    #[test]
    fn caret_constraint() {
        let c = VersionConstraint::parse("^1.2.3").unwrap();
        assert!(c.matches(&Version::new(1, 2, 3)));
        assert!(c.matches(&Version::new(1, 3, 0)));
        assert!(c.matches(&Version::new(1, 99, 0)));
        assert!(!c.matches(&Version::new(2, 0, 0)));
        assert!(!c.matches(&Version::new(1, 2, 2)));
    }

    #[test]
    fn caret_zero_minor() {
        let c = VersionConstraint::parse("^0.2.0").unwrap();
        assert!(c.matches(&Version::new(0, 2, 0)));
        assert!(c.matches(&Version::new(0, 2, 5)));
        assert!(!c.matches(&Version::new(0, 3, 0)));
    }

    #[test]
    fn tilde_constraint() {
        let c = VersionConstraint::parse("~1.2.3").unwrap();
        assert!(c.matches(&Version::new(1, 2, 3)));
        assert!(c.matches(&Version::new(1, 2, 9)));
        assert!(!c.matches(&Version::new(1, 3, 0)));
    }

    #[test]
    fn exact_constraint() {
        let c = VersionConstraint::parse("=1.2.3").unwrap();
        assert!(c.matches(&Version::new(1, 2, 3)));
        assert!(!c.matches(&Version::new(1, 2, 4)));
    }

    #[test]
    fn any_constraint() {
        let c = VersionConstraint::parse("*").unwrap();
        assert!(c.matches(&Version::new(0, 0, 0)));
        assert!(c.matches(&Version::new(99, 99, 99)));
    }

    #[test]
    fn bare_version_is_caret() {
        let c = VersionConstraint::parse("1.2.3").unwrap();
        assert!(c.matches(&Version::new(1, 5, 0)));
        assert!(!c.matches(&Version::new(2, 0, 0)));
    }

    #[test]
    fn version_ordering() {
        assert!(Version::new(1, 0, 0) < Version::new(2, 0, 0));
        assert!(Version::new(1, 2, 0) < Version::new(1, 3, 0));
        assert!(Version::new(1, 2, 3) < Version::new(1, 2, 4));
    }
}
