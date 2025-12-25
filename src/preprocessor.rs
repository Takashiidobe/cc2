/// Preprocessor for C source code
///
/// The preprocessor runs before tokenization and handles:
/// - #include directives
/// - #define macros
/// - Conditional compilation (#ifdef, #ifndef, #if, #else, #elif, #endif)
/// - #undef
/// - #pragma (ignored for now)
///
/// The preprocessor transforms source text into preprocessed text that
/// can then be tokenized and parsed.

use std::collections::HashMap;

/// Macro definition stored in the preprocessor symbol table
#[derive(Debug, Clone, PartialEq)]
pub enum MacroDef {
    /// Object-like macro: #define NAME value
    Object(String),
    /// Function-like macro: #define NAME(params) body
    Function {
        params: Vec<String>,
        body: String,
        is_variadic: bool,
    },
}

/// Preprocessor state
pub struct Preprocessor {
    /// Defined macros
    macros: HashMap<String, MacroDef>,
    /// Include paths (-I flags)
    include_paths: Vec<String>,
    /// Current file being processed (for error messages)
    #[allow(dead_code)]
    current_file: String,
}

impl Preprocessor {
    /// Create a new preprocessor with default settings
    pub fn new() -> Self {
        Preprocessor {
            macros: HashMap::new(),
            include_paths: vec![
                "/usr/include".to_string(),
                "/usr/local/include".to_string(),
            ],
            current_file: "<input>".to_string(),
        }
    }

    /// Add an include path
    pub fn add_include_path(&mut self, path: String) {
        self.include_paths.push(path);
    }

    /// Define a macro
    pub fn define_macro(&mut self, name: String, definition: MacroDef) {
        self.macros.insert(name, definition);
    }

    /// Undefine a macro
    pub fn undef_macro(&mut self, name: &str) {
        self.macros.remove(name);
    }

    /// Check if a macro is defined
    pub fn is_defined(&self, name: &str) -> bool {
        self.macros.contains_key(name)
    }

    /// Preprocess source text
    ///
    /// This is the main entry point for preprocessing. It processes all
    /// preprocessor directives and returns the preprocessed source text.
    pub fn preprocess(&mut self, source: &str) -> Result<String, String> {
        let mut output = String::new();
        let lines: Vec<&str> = source.lines().collect();
        let mut line_num = 0;

        while line_num < lines.len() {
            let line = lines[line_num];
            let trimmed = line.trim_start();

            if trimmed.starts_with('#') {
                // This is a preprocessor directive
                let (new_line_num, directive_output) =
                    self.process_directive(&lines, line_num)?;
                line_num = new_line_num;
                output.push_str(&directive_output);
            } else {
                // Regular line - perform macro expansion
                let expanded = self.expand_macros(line)?;
                output.push_str(&expanded);
                output.push('\n');
                line_num += 1;
            }
        }

        Ok(output)
    }

    /// Process a preprocessor directive starting at line_num
    ///
    /// Returns (next_line_num, output_text)
    fn process_directive(
        &mut self,
        lines: &[&str],
        line_num: usize,
    ) -> Result<(usize, String), String> {
        let line = lines[line_num];
        let trimmed = line.trim_start();

        if !trimmed.starts_with('#') {
            return Err(format!("Expected preprocessor directive at line {}", line_num + 1));
        }

        // Extract directive name (first word after #)
        let after_hash = trimmed[1..].trim_start();
        let directive_name = after_hash
            .split_whitespace()
            .next()
            .unwrap_or("");

        match directive_name {
            "include" => self.process_include(after_hash, line_num),
            "define" => self.process_define(after_hash, line_num),
            "undef" => self.process_undef(after_hash, line_num),
            "ifdef" => self.process_ifdef(lines, line_num, false),
            "ifndef" => self.process_ifdef(lines, line_num, true),
            "if" => self.process_if(lines, line_num),
            "endif" => Err(format!("Unexpected #endif at line {}", line_num + 1)),
            "else" => Err(format!("Unexpected #else at line {}", line_num + 1)),
            "elif" => Err(format!("Unexpected #elif at line {}", line_num + 1)),
            "pragma" => {
                // Ignore pragma directives for now
                Ok((line_num + 1, String::new()))
            }
            "" => {
                // Empty directive (just #) - ignore
                Ok((line_num + 1, String::new()))
            }
            _ => Err(format!(
                "Unknown preprocessor directive '{}' at line {}",
                directive_name,
                line_num + 1
            )),
        }
    }

    /// Process #include directive (stub for now)
    fn process_include(
        &mut self,
        _directive: &str,
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // TODO: Implement in cc2-9xt
        Ok((line_num + 1, String::new()))
    }

    /// Process #define directive (stub for now)
    fn process_define(
        &mut self,
        _directive: &str,
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // TODO: Implement in cc2-krg
        Ok((line_num + 1, String::new()))
    }

    /// Process #undef directive (stub for now)
    fn process_undef(
        &mut self,
        _directive: &str,
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // TODO: Implement in cc2-krg
        Ok((line_num + 1, String::new()))
    }

    /// Process #ifdef or #ifndef directive (stub for now)
    fn process_ifdef(
        &mut self,
        _lines: &[&str],
        line_num: usize,
        _is_ifndef: bool,
    ) -> Result<(usize, String), String> {
        // TODO: Implement in cc2-a3g
        Ok((line_num + 1, String::new()))
    }

    /// Process #if directive (stub for now)
    fn process_if(
        &mut self,
        _lines: &[&str],
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // TODO: Implement in cc2-1cl
        Ok((line_num + 1, String::new()))
    }

    /// Expand macros in a line of text (stub for now)
    fn expand_macros(&self, line: &str) -> Result<String, String> {
        // TODO: Implement macro expansion in cc2-krg
        // For now, just return the line unchanged
        Ok(line.to_string())
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_preprocessor_creation() {
        let pp = Preprocessor::new();
        assert_eq!(pp.macros.len(), 0);
        assert!(pp.include_paths.len() >= 2);
    }

    #[test]
    fn test_define_macro() {
        let mut pp = Preprocessor::new();
        pp.define_macro("FOO".to_string(), MacroDef::Object("42".to_string()));
        assert!(pp.is_defined("FOO"));
        assert!(!pp.is_defined("BAR"));
    }

    #[test]
    fn test_undef_macro() {
        let mut pp = Preprocessor::new();
        pp.define_macro("FOO".to_string(), MacroDef::Object("42".to_string()));
        assert!(pp.is_defined("FOO"));
        pp.undef_macro("FOO");
        assert!(!pp.is_defined("FOO"));
    }

    #[test]
    fn test_preprocess_no_directives() {
        let mut pp = Preprocessor::new();
        let source = "int main() {\n    return 42;\n}\n";
        let result = pp.preprocess(source).unwrap();
        assert_eq!(result, source);
    }

    #[test]
    fn test_preprocess_ignore_pragma() {
        let mut pp = Preprocessor::new();
        let source = "#pragma once\nint main() { return 0; }\n";
        let result = pp.preprocess(source).unwrap();
        assert_eq!(result, "int main() { return 0; }\n");
    }

    #[test]
    fn test_preprocess_empty_directive() {
        let mut pp = Preprocessor::new();
        let source = "#\nint x = 5;\n";
        let result = pp.preprocess(source).unwrap();
        assert_eq!(result, "int x = 5;\n");
    }
}
