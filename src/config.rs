// two numbers are roughly the same if they are within 5% of each other
pub const DEFAULT_JAM_KARET_NUM: f64 = 0.05;

// two strings are roughly the same if their levenshtein distance is less than 20% of length
pub const DEFAULT_JAM_KARET_STR: f64 = 0.2;

pub struct RuntimeConfig {
    pub jam_karet_num: f64,
    pub jam_karet_str: f64,
}

impl Default for RuntimeConfig {
    fn default() -> Self {
        Self {
            // set default values here, unless overridden via command-line
            jam_karet_num: DEFAULT_JAM_KARET_NUM,
            jam_karet_str: DEFAULT_JAM_KARET_STR,
        }
    }
}
