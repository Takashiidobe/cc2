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

use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

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
    current_file: String,
    /// Stack of files being included (for cycle detection)
    include_stack: HashSet<PathBuf>,
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
            include_stack: HashSet::new(),
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

    /// Process #include directive
    fn process_include(
        &mut self,
        directive: &str,
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // Parse the include directive to extract filename and type
        let (filename, is_system) = self.parse_include_directive(directive, line_num)?;

        // Find the file in the include paths
        let file_path = self.find_include_file(&filename, is_system, line_num)?;

        // Check for circular includes
        let canonical_path = file_path
            .canonicalize()
            .map_err(|e| format!("Failed to canonicalize path '{}': {}", file_path.display(), e))?;

        if self.include_stack.contains(&canonical_path) {
            return Err(format!(
                "Circular include detected at line {}: {}",
                line_num + 1,
                canonical_path.display()
            ));
        }

        // Read the file
        let content = fs::read_to_string(&file_path).map_err(|e| {
            format!(
                "Failed to read include file '{}' at line {}: {}",
                file_path.display(),
                line_num + 1,
                e
            )
        })?;

        // Save current state
        let old_file = self.current_file.clone();
        self.current_file = file_path.display().to_string();
        self.include_stack.insert(canonical_path.clone());

        // Recursively preprocess the included file
        let preprocessed = self.preprocess(&content)?;

        // Restore state
        self.current_file = old_file;
        self.include_stack.remove(&canonical_path);

        Ok((line_num + 1, preprocessed))
    }

    /// Parse #include directive to extract filename and determine type
    fn parse_include_directive(
        &self,
        directive: &str,
        line_num: usize,
    ) -> Result<(String, bool), String> {
        // directive is the part after '#', like "include <stdio.h>" or "include \"foo.h\""
        let after_include = directive.trim_start();
        if !after_include.starts_with("include") {
            return Err(format!("Invalid include directive at line {}", line_num + 1));
        }

        let rest = after_include[7..].trim_start(); // Skip "include"

        // Check for angle brackets: #include <file.h>
        if rest.starts_with('<') {
            let end = rest.find('>').ok_or_else(|| {
                format!("Missing closing '>' in include directive at line {}", line_num + 1)
            })?;
            let filename = rest[1..end].to_string();
            return Ok((filename, true));
        }

        // Check for quotes: #include "file.h"
        if rest.starts_with('"') {
            let end = rest[1..].find('"').ok_or_else(|| {
                format!("Missing closing '\"' in include directive at line {}", line_num + 1)
            })?;
            let filename = rest[1..end + 1].to_string();
            return Ok((filename, false));
        }

        Err(format!(
            "Invalid include syntax at line {}: expected <file> or \"file\"",
            line_num + 1
        ))
    }

    /// Find an include file in the include paths
    fn find_include_file(
        &self,
        filename: &str,
        is_system: bool,
        line_num: usize,
    ) -> Result<PathBuf, String> {
        // For quoted includes, search current directory first
        if !is_system {
            // Try current directory or relative to current file
            let current_dir = if self.current_file == "<input>" {
                PathBuf::from(".")
            } else {
                PathBuf::from(&self.current_file)
                    .parent()
                    .unwrap_or(Path::new("."))
                    .to_path_buf()
            };

            let local_path = current_dir.join(filename);
            if local_path.exists() {
                return Ok(local_path);
            }
        }

        // Search in include paths
        for include_path in &self.include_paths {
            let full_path = PathBuf::from(include_path).join(filename);
            if full_path.exists() {
                return Ok(full_path);
            }
        }

        Err(format!(
            "Include file '{}' not found at line {}",
            filename,
            line_num + 1
        ))
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

    #[test]
    fn test_parse_include_quoted() {
        let pp = Preprocessor::new();
        let (filename, is_system) = pp.parse_include_directive("include \"foo.h\"", 0).unwrap();
        assert_eq!(filename, "foo.h");
        assert!(!is_system);
    }

    #[test]
    fn test_parse_include_angle_brackets() {
        let pp = Preprocessor::new();
        let (filename, is_system) = pp.parse_include_directive("include <stdio.h>", 0).unwrap();
        assert_eq!(filename, "stdio.h");
        assert!(is_system);
    }

    #[test]
    fn test_parse_include_missing_close_quote() {
        let pp = Preprocessor::new();
        let result = pp.parse_include_directive("include \"foo.h", 0);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_include_missing_close_angle() {
        let pp = Preprocessor::new();
        let result = pp.parse_include_directive("include <stdio.h", 0);
        assert!(result.is_err());
    }

    #[test]
    fn test_include_file_not_found() {
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        let mut pp = Preprocessor::new();
        pp.include_paths.clear();
        pp.include_paths.push(temp_path.to_str().unwrap().to_string());

        let source = "#include \"nonexistent.h\"\n";
        let result = pp.preprocess(source);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not found"));
    }

    #[test]
    fn test_include_basic() {
        use std::io::Write;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create a header file
        let header_path = temp_path.join("test.h");
        let mut header_file = fs::File::create(&header_path).unwrap();
        writeln!(header_file, "int foo = 42;").unwrap();

        let mut pp = Preprocessor::new();
        pp.include_paths.clear();
        pp.include_paths.push(temp_path.to_str().unwrap().to_string());

        let source = "#include \"test.h\"\nint main() {{ return 0; }}\n";
        let result = pp.preprocess(source).unwrap();
        assert!(result.contains("int foo = 42;"));
        assert!(result.contains("int main()"));
    }

    #[test]
    fn test_include_circular_detection() {
        use std::io::Write;
        use tempfile::TempDir;

        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();

        // Create a.h that includes b.h
        let a_path = temp_path.join("a.h");
        let mut a_file = fs::File::create(&a_path).unwrap();
        writeln!(a_file, "#include \"b.h\"").unwrap();

        // Create b.h that includes a.h (circular)
        let b_path = temp_path.join("b.h");
        let mut b_file = fs::File::create(&b_path).unwrap();
        writeln!(b_file, "#include \"a.h\"").unwrap();

        let mut pp = Preprocessor::new();
        pp.include_paths.clear();
        pp.include_paths.push(temp_path.to_str().unwrap().to_string());

        let source = "#include \"a.h\"\n";
        let result = pp.preprocess(source);
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Circular include"));
    }
}
