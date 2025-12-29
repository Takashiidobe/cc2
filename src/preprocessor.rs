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

/// Token types for preprocessor expression evaluation
#[derive(Debug, Clone, PartialEq)]
enum ExprToken {
    Number(i64),
    Identifier(String),
    Defined,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    And,
    Or,
    Xor,
    Tilde,
    Bang,
    AndAnd,
    OrOr,
    EqEq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    LShift,
    RShift,
    LParen,
    RParen,
}

/// Preprocessor state
pub struct Preprocessor {
    /// Defined macros
    pub(crate) macros: HashMap<String, MacroDef>,
    /// Include paths (-I flags)
    pub(crate) include_paths: Vec<String>,
    /// System include paths (-isystem flags)
    pub(crate) system_include_paths: Vec<String>,
    /// Current file being processed (for error messages)
    pub(crate) current_file: String,
    /// Current line number being processed (1-indexed for __LINE__)
    pub(crate) current_line: usize,
    /// Stack of files being included (for cycle detection)
    pub(crate) include_stack: HashSet<PathBuf>,
}

impl Preprocessor {
    /// Create a new preprocessor with default settings
    pub fn new() -> Self {
        let mut preprocessor = Preprocessor {
            macros: HashMap::new(),
            include_paths: Vec::new(),
            system_include_paths: vec![
                "/usr/include".to_string(),
                "/usr/local/include".to_string(),
            ],
            current_file: "<input>".to_string(),
            current_line: 1,
            include_stack: HashSet::new(),
        };
        preprocessor.init_predefined_macros();
        preprocessor
    }

    /// Create a new preprocessor without standard include paths
    pub fn new_no_std() -> Self {
        let mut preprocessor = Preprocessor {
            macros: HashMap::new(),
            include_paths: Vec::new(),
            system_include_paths: Vec::new(),
            current_file: "<input>".to_string(),
            current_line: 1,
            include_stack: HashSet::new(),
        };
        preprocessor.init_predefined_macros();
        preprocessor
    }

    /// Initialize predefined macros (__STDC__, __DATE__, __TIME__, etc.)
    fn init_predefined_macros(&mut self) {
        // __STDC__ = 1 (indicates C standard conformance)
        self.macros.insert(
            "__STDC__".to_string(),
            MacroDef::Object("1".to_string()),
        );

        // __STDC_VERSION__ = 201112L (C11)
        self.macros.insert(
            "__STDC_VERSION__".to_string(),
            MacroDef::Object("201112L".to_string()),
        );

        // __STDC_HOSTED__ = 1 (hosted implementation)
        self.macros.insert(
            "__STDC_HOSTED__".to_string(),
            MacroDef::Object("1".to_string()),
        );

        // __DATE__ and __TIME__ - compilation timestamp
        let now = std::time::SystemTime::now();
        if let Ok(duration) = now.duration_since(std::time::UNIX_EPOCH) {
            let secs = duration.as_secs();

            // Convert to date/time
            // This is a simple implementation - not handling timezones properly
            let days_since_epoch = secs / 86400;
            let seconds_today = secs % 86400;

            // Calculate date (simplified - assumes 365.25 days/year starting from 1970)
            let mut year = 1970;
            let mut remaining_days = days_since_epoch;
            loop {
                let days_in_year = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                    366
                } else {
                    365
                };
                if remaining_days < days_in_year {
                    break;
                }
                remaining_days -= days_in_year;
                year += 1;
            }

            // Calculate month and day
            let days_in_months = if year % 4 == 0 && (year % 100 != 0 || year % 400 == 0) {
                [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            } else {
                [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
            };

            let mut month = 0;
            let mut day = remaining_days + 1;
            for (m, &days) in days_in_months.iter().enumerate() {
                if day <= days {
                    month = m;
                    break;
                }
                day -= days;
            }

            let month_names = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                              "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
            let date_str = format!("\"{} {:2} {}\"", month_names[month], day, year);

            // __DATE__ = "Mmm dd yyyy"
            self.macros.insert(
                "__DATE__".to_string(),
                MacroDef::Object(date_str),
            );

            // Calculate time
            let hour = seconds_today / 3600;
            let minute = (seconds_today % 3600) / 60;
            let second = seconds_today % 60;
            let time_str = format!("\"{:02}:{:02}:{:02}\"", hour, minute, second);

            // __TIME__ = "hh:mm:ss"
            self.macros.insert(
                "__TIME__".to_string(),
                MacroDef::Object(time_str),
            );
        }
    }

    /// Add an include path (-I flag)
    /// These paths are searched for quoted includes after the current directory
    pub fn add_include_path(&mut self, path: String) {
        self.include_paths.push(path);
    }

    /// Add a system include path (-isystem flag)
    /// These paths are searched for angle-bracket includes
    pub fn add_system_include_path(&mut self, path: String) {
        self.system_include_paths.push(path);
    }

    /// Set the current file being processed (for resolving relative includes)
    pub fn set_current_file(&mut self, path: String) {
        self.current_file = path;
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

    /// Remove C and C++ comments from source text
    fn remove_comments(source: &str) -> String {
        let mut result = String::new();
        let chars: Vec<char> = source.chars().collect();
        let mut i = 0;

        while i < chars.len() {
            // Check for C++ style comment //
            if i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '/' {
                // Skip until end of line
                while i < chars.len() && chars[i] != '\n' {
                    i += 1;
                }
                // Keep the newline
                if i < chars.len() {
                    result.push(chars[i]);
                    i += 1;
                }
            }
            // Check for C style comment /*
            else if i + 1 < chars.len() && chars[i] == '/' && chars[i + 1] == '*' {
                // Replace comment with a space
                result.push(' ');
                i += 2;
                // Skip until we find */
                while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '/') {
                    // Preserve newlines in comments
                    if chars[i] == '\n' {
                        result.push('\n');
                    }
                    i += 1;
                }
                // Skip the closing */
                if i + 1 < chars.len() {
                    i += 2;
                }
            } else {
                result.push(chars[i]);
                i += 1;
            }
        }

        result
    }

    /// Preprocess source text
    ///
    /// This is the main entry point for preprocessing. It processes all
    /// preprocessor directives and returns the preprocessed source text.
    pub fn preprocess(&mut self, source: &str) -> Result<String, String> {
        // First, remove all comments
        let source = Self::remove_comments(source);

        let mut output = String::new();
        let lines: Vec<&str> = source.lines().collect();
        let mut line_num = 0;

        while line_num < lines.len() {
            let line = lines[line_num];
            let trimmed = line.trim_start();

            // Update current line for __LINE__ macro (1-indexed)
            self.current_line = line_num + 1;

            if trimmed.starts_with('#') {
                // This is a preprocessor directive
                let (new_line_num, directive_output) = self.process_directive(&lines, line_num)?;
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
            return Err(format!(
                "Expected preprocessor directive at line {}",
                line_num + 1
            ));
        }

        // Extract directive name (first word after #)
        let after_hash = trimmed[1..].trim_start();
        let directive_name = after_hash.split_whitespace().next().unwrap_or("");

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
        let canonical_path = file_path.canonicalize().map_err(|e| {
            format!(
                "Failed to canonicalize path '{}': {}",
                file_path.display(),
                e
            )
        })?;

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
    pub(crate) fn parse_include_directive(
        &self,
        directive: &str,
        line_num: usize,
    ) -> Result<(String, bool), String> {
        // directive is the part after '#', like "include <stdio.h>" or "include \"foo.h\""
        let after_include = directive.trim_start();
        if !after_include.starts_with("include") {
            return Err(format!(
                "Invalid include directive at line {}",
                line_num + 1
            ));
        }

        let rest = after_include[7..].trim_start(); // Skip "include"

        // Check for angle brackets: #include <file.h>
        if rest.starts_with('<') {
            let end = rest.find('>').ok_or_else(|| {
                format!(
                    "Missing closing '>' in include directive at line {}",
                    line_num + 1
                )
            })?;
            let filename = rest[1..end].to_string();
            return Ok((filename, true));
        }

        // Check for quotes: #include "file.h"
        if rest.starts_with('"') {
            let end = rest[1..].find('"').ok_or_else(|| {
                format!(
                    "Missing closing '\"' in include directive at line {}",
                    line_num + 1
                )
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

            // Search in user include paths (-I)
            for include_path in &self.include_paths {
                let full_path = PathBuf::from(include_path).join(filename);
                if full_path.exists() {
                    return Ok(full_path);
                }
            }
        }

        // Search in system include paths (-isystem and default system paths)
        // For angle-bracket includes, only search here
        // For quoted includes, search here as a fallback
        for include_path in &self.system_include_paths {
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

    /// Process #define directive
    fn process_define(
        &mut self,
        directive: &str,
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // directive is like "define NAME value" or "define NAME(params) body"
        let after_define = directive.trim_start();
        if !after_define.starts_with("define") {
            return Err(format!("Invalid define directive at line {}", line_num + 1));
        }

        let rest = after_define[6..].trim_start(); // Skip "define"

        if rest.is_empty() {
            return Err(format!(
                "Missing macro name in #define at line {}",
                line_num + 1
            ));
        }

        // Check if this is a function-like macro by looking for '(' immediately after name
        // Note: There must be NO space between name and '(' for function-like macros
        let (name, macro_def) = if let Some(paren_pos) = rest.find('(') {
            // Check if there's a space before the paren - if so, it's object-like
            let name_part = &rest[..paren_pos];
            if name_part.chars().last().is_some_and(|c| c.is_whitespace()) {
                // Space before paren - this is object-like macro
                self.parse_object_like_macro(rest, line_num)?
            } else {
                // No space - this is function-like macro
                self.parse_function_like_macro(rest, line_num)?
            }
        } else {
            // No parentheses - object-like macro
            self.parse_object_like_macro(rest, line_num)?
        };

        self.macros.insert(name, macro_def);
        Ok((line_num + 1, String::new()))
    }

    /// Parse object-like macro: #define NAME replacement
    fn parse_object_like_macro(
        &self,
        text: &str,
        line_num: usize,
    ) -> Result<(String, MacroDef), String> {
        let mut parts = text.splitn(2, |c: char| c.is_whitespace());
        let name = parts
            .next()
            .ok_or_else(|| format!("Missing macro name at line {}", line_num + 1))?
            .to_string();

        // Validate macro name
        if !Self::is_valid_identifier(&name) {
            return Err(format!(
                "Invalid macro name '{}' at line {}",
                name,
                line_num + 1
            ));
        }

        let replacement = parts.next().unwrap_or("").trim().to_string();

        Ok((name, MacroDef::Object(replacement)))
    }

    /// Parse function-like macro: #define NAME(params) body
    fn parse_function_like_macro(
        &self,
        text: &str,
        line_num: usize,
    ) -> Result<(String, MacroDef), String> {
        // Find the macro name (everything before '(')
        let paren_start = text.find('(').ok_or_else(|| {
            format!(
                "Expected '(' in function-like macro at line {}",
                line_num + 1
            )
        })?;
        let name = text[..paren_start].trim().to_string();

        // Validate macro name
        if !Self::is_valid_identifier(&name) {
            return Err(format!(
                "Invalid macro name '{}' at line {}",
                name,
                line_num + 1
            ));
        }

        // Find the closing parenthesis
        let paren_end = text.find(')').ok_or_else(|| {
            format!(
                "Missing ')' in function-like macro at line {}",
                line_num + 1
            )
        })?;

        // Extract parameter list
        let param_text = &text[paren_start + 1..paren_end];
        let mut params = Vec::new();
        let mut is_variadic = false;

        if !param_text.trim().is_empty() {
            for param in param_text.split(',') {
                let param = param.trim();
                if param == "..." {
                    is_variadic = true;
                } else if param.is_empty() {
                    return Err(format!("Empty parameter in macro at line {}", line_num + 1));
                } else if !Self::is_valid_identifier(param) {
                    return Err(format!(
                        "Invalid parameter name '{}' at line {}",
                        param,
                        line_num + 1
                    ));
                } else {
                    params.push(param.to_string());
                }
            }
        }

        // Extract the body (everything after ')')
        let body = text[paren_end + 1..].trim().to_string();

        Ok((
            name,
            MacroDef::Function {
                params,
                body,
                is_variadic,
            },
        ))
    }

    /// Check if a string is a valid C identifier
    pub(crate) fn is_valid_identifier(s: &str) -> bool {
        if s.is_empty() {
            return false;
        }

        let mut chars = s.chars();
        let first = chars.next().unwrap();

        // First character must be letter or underscore
        if !first.is_alphabetic() && first != '_' {
            return false;
        }

        // Remaining characters must be alphanumeric or underscore
        chars.all(|c| c.is_alphanumeric() || c == '_')
    }

    /// Process #undef directive
    fn process_undef(
        &mut self,
        directive: &str,
        line_num: usize,
    ) -> Result<(usize, String), String> {
        // directive is like "undef NAME"
        let after_undef = directive.trim_start();
        if !after_undef.starts_with("undef") {
            return Err(format!("Invalid undef directive at line {}", line_num + 1));
        }

        let rest = after_undef[5..].trim(); // Skip "undef"

        if rest.is_empty() {
            return Err(format!(
                "Missing macro name in #undef at line {}",
                line_num + 1
            ));
        }

        // Extract macro name (should be a single identifier)
        let name = rest.split_whitespace().next().unwrap().to_string();

        if !Self::is_valid_identifier(&name) {
            return Err(format!(
                "Invalid macro name '{}' in #undef at line {}",
                name,
                line_num + 1
            ));
        }

        self.macros.remove(&name);
        Ok((line_num + 1, String::new()))
    }

    /// Process #ifdef or #ifndef directive
    fn process_ifdef(
        &mut self,
        lines: &[&str],
        line_num: usize,
        is_ifndef: bool,
    ) -> Result<(usize, String), String> {
        let line = lines[line_num];
        let trimmed = line.trim_start();

        // Extract the directive and macro name
        let directive_name = if is_ifndef { "ifndef" } else { "ifdef" };
        let after_hash = trimmed[1..].trim_start(); // Skip '#'

        if !after_hash.starts_with(directive_name) {
            return Err(format!(
                "Expected #{} directive at line {}",
                directive_name,
                line_num + 1
            ));
        }

        let rest = after_hash[directive_name.len()..].trim();
        if rest.is_empty() {
            return Err(format!(
                "Missing macro name in #{} at line {}",
                directive_name,
                line_num + 1
            ));
        }

        // Extract macro name (first word)
        let macro_name = rest.split_whitespace().next().unwrap();

        if !Self::is_valid_identifier(macro_name) {
            return Err(format!(
                "Invalid macro name '{}' in #{} at line {}",
                macro_name,
                directive_name,
                line_num + 1
            ));
        }

        // Check the condition
        let is_defined = self.is_defined(macro_name);
        let condition = if is_ifndef { !is_defined } else { is_defined };

        // Find the matching #endif and process the block
        let (end_line, block_output) = if condition {
            // Condition is true - process the block normally
            self.process_if_block(lines, line_num + 1, true)?
        } else {
            // Condition is false - skip to #elif, #else, or #endif
            self.skip_to_else_or_endif(lines, line_num + 1)?
        };

        Ok((end_line, block_output))
    }

    /// Process a conditional block when condition is true
    fn process_conditional_block(
        &mut self,
        lines: &[&str],
        start_line: usize,
        _is_active: bool,
    ) -> Result<(usize, String), String> {
        let mut output = String::new();
        let mut current_line = start_line;

        while current_line < lines.len() {
            let line = lines[current_line];
            let trimmed = line.trim_start();

            if trimmed.starts_with('#') {
                let after_hash = trimmed[1..].trim_start();

                if after_hash.starts_with("endif") {
                    // This is our matching #endif
                    return Ok((current_line + 1, output));
                } else if after_hash.starts_with("ifdef") || after_hash.starts_with("ifndef") {
                    // Process the nested conditional - it will handle its own #endif
                    let (new_line, directive_output) =
                        self.process_directive(lines, current_line)?;
                    current_line = new_line;
                    output.push_str(&directive_output);
                } else {
                    // Other directive - process normally
                    let (new_line, directive_output) =
                        self.process_directive(lines, current_line)?;
                    current_line = new_line;
                    output.push_str(&directive_output);
                }
            } else {
                // Regular line - expand macros
                let expanded = self.expand_macros(line)?;
                output.push_str(&expanded);
                output.push('\n');
                current_line += 1;
            }
        }

        Err(format!(
            "Missing #endif for conditional starting at line {}",
            start_line
        ))
    }

    /// Skip a conditional block when condition is false
    fn skip_conditional_block(
        &mut self,
        lines: &[&str],
        start_line: usize,
    ) -> Result<(usize, String), String> {
        let mut current_line = start_line;
        let mut nesting_level = 0;

        while current_line < lines.len() {
            let line = lines[current_line];
            let trimmed = line.trim_start();

            if trimmed.starts_with('#') {
                let after_hash = trimmed[1..].trim_start();

                // Check for nested conditionals
                if after_hash.starts_with("ifdef")
                    || after_hash.starts_with("ifndef")
                    || after_hash.starts_with("if")
                {
                    nesting_level += 1;
                } else if after_hash.starts_with("endif") {
                    if nesting_level == 0 {
                        // This is our matching #endif - skip it and return
                        return Ok((current_line + 1, String::new()));
                    } else {
                        nesting_level -= 1;
                    }
                }
            }

            current_line += 1;
        }

        Err(format!(
            "Missing #endif for conditional starting at line {}",
            start_line
        ))
    }

    /// Process #if directive
    fn process_if(&mut self, lines: &[&str], line_num: usize) -> Result<(usize, String), String> {
        let line = lines[line_num];
        let trimmed = line.trim_start();

        // Extract the expression after #if
        let after_hash = trimmed[1..].trim_start(); // Skip '#'

        if !after_hash.starts_with("if") {
            return Err(format!("Expected #if directive at line {}", line_num + 1));
        }

        let rest = after_hash[2..].trim(); // Skip "if"
        if rest.is_empty() {
            return Err(format!(
                "Missing expression in #if at line {}",
                line_num + 1
            ));
        }

        // Evaluate the preprocessor expression
        let condition = self.eval_preprocessor_expr(rest)?;

        // Process the conditional block similar to ifdef
        let (end_line, block_output) = if condition != 0 {
            // Condition is true - process the block normally
            self.process_if_block(lines, line_num + 1, true)?
        } else {
            // Condition is false - skip to #elif, #else, or #endif
            self.skip_to_else_or_endif(lines, line_num + 1)?
        };

        Ok((end_line, block_output))
    }

    /// Process an #if block, handling #elif and #else
    fn process_if_block(
        &mut self,
        lines: &[&str],
        start_line: usize,
        is_active: bool,
    ) -> Result<(usize, String), String> {
        let mut output = String::new();
        let mut current_line = start_line;

        while current_line < lines.len() {
            let line = lines[current_line];
            let trimmed = line.trim_start();

            if trimmed.starts_with('#') {
                let after_hash = trimmed[1..].trim_start();

                if after_hash.starts_with("endif") {
                    // This is our matching #endif
                    return Ok((current_line + 1, output));
                } else if after_hash.starts_with("elif") {
                    if is_active {
                        // We already processed an active branch, skip the rest
                        let end_line = self.skip_remaining_elif_else(lines, current_line)?;
                        return Ok((end_line, output));
                    } else {
                        // Evaluate the #elif condition
                        let rest = after_hash[4..].trim(); // Skip "elif"
                        if rest.is_empty() {
                            return Err(format!(
                                "Missing expression in #elif at line {}",
                                current_line + 1
                            ));
                        }

                        let condition = self.eval_preprocessor_expr(rest)?;
                        if condition != 0 {
                            // This elif branch is true - process it
                            let (end_line, elif_output) =
                                self.process_if_block(lines, current_line + 1, true)?;
                            output.push_str(&elif_output);
                            return Ok((end_line, output));
                        } else {
                            // This elif is false - continue to next elif/else/endif
                            current_line += 1;
                            continue;
                        }
                    }
                } else if after_hash.starts_with("else") {
                    if is_active {
                        // We already processed an active branch, skip to endif
                        let end_line = self.skip_to_endif(lines, current_line + 1)?;
                        return Ok((end_line, output));
                    } else {
                        // Process the else branch
                        let (end_line, else_output) =
                            self.process_if_block(lines, current_line + 1, true)?;
                        output.push_str(&else_output);
                        return Ok((end_line, output));
                    }
                } else if after_hash.starts_with("ifdef")
                    || after_hash.starts_with("ifndef")
                    || after_hash.starts_with("if")
                {
                    // Nested conditional - process it if we're active
                    if is_active {
                        let (new_line, directive_output) =
                            self.process_directive(lines, current_line)?;
                        current_line = new_line;
                        output.push_str(&directive_output);
                    } else {
                        current_line += 1;
                    }
                } else if is_active {
                    // Other directive - process normally if active
                    let (new_line, directive_output) =
                        self.process_directive(lines, current_line)?;
                    current_line = new_line;
                    output.push_str(&directive_output);
                } else {
                    current_line += 1;
                }
            } else if is_active {
                // Regular line - expand macros if active
                let expanded = self.expand_macros(line)?;
                output.push_str(&expanded);
                output.push('\n');
                current_line += 1;
            } else {
                current_line += 1;
            }
        }

        Err(format!(
            "Missing #endif for conditional starting at line {}",
            start_line
        ))
    }

    /// Skip to the next #elif, #else, or #endif at the same nesting level
    fn skip_to_else_or_endif(
        &mut self,
        lines: &[&str],
        start_line: usize,
    ) -> Result<(usize, String), String> {
        let mut current_line = start_line;
        let mut nesting_level = 0;

        while current_line < lines.len() {
            let line = lines[current_line];
            let trimmed = line.trim_start();

            if trimmed.starts_with('#') {
                let after_hash = trimmed[1..].trim_start();

                if after_hash.starts_with("ifdef")
                    || after_hash.starts_with("ifndef")
                    || after_hash.starts_with("if")
                {
                    nesting_level += 1;
                } else if after_hash.starts_with("endif") {
                    if nesting_level == 0 {
                        // Found our matching #endif
                        return Ok((current_line + 1, String::new()));
                    } else {
                        nesting_level -= 1;
                    }
                } else if nesting_level == 0
                    && (after_hash.starts_with("elif") || after_hash.starts_with("else"))
                {
                    // Found #elif or #else at our level
                    if after_hash.starts_with("elif") {
                        // Evaluate the elif condition
                        let rest = after_hash[4..].trim();
                        if rest.is_empty() {
                            return Err(format!(
                                "Missing expression in #elif at line {}",
                                current_line + 1
                            ));
                        }

                        let condition = self.eval_preprocessor_expr(rest)?;
                        if condition != 0 {
                            // This elif is true - process its block
                            return self.process_if_block(lines, current_line + 1, true);
                        } else {
                            // This elif is false - continue searching
                            current_line += 1;
                            continue;
                        }
                    } else {
                        // Found #else - process its block
                        return self.process_if_block(lines, current_line + 1, true);
                    }
                }
            }

            current_line += 1;
        }

        Err(format!(
            "Missing #endif for conditional starting at line {}",
            start_line
        ))
    }

    /// Skip all remaining #elif and #else branches to reach #endif
    fn skip_remaining_elif_else(
        &mut self,
        lines: &[&str],
        start_line: usize,
    ) -> Result<usize, String> {
        let mut current_line = start_line;
        let mut nesting_level = 0;

        while current_line < lines.len() {
            let line = lines[current_line];
            let trimmed = line.trim_start();

            if trimmed.starts_with('#') {
                let after_hash = trimmed[1..].trim_start();

                if after_hash.starts_with("ifdef")
                    || after_hash.starts_with("ifndef")
                    || after_hash.starts_with("if")
                {
                    nesting_level += 1;
                } else if after_hash.starts_with("endif") {
                    if nesting_level == 0 {
                        return Ok(current_line + 1);
                    } else {
                        nesting_level -= 1;
                    }
                }
            }

            current_line += 1;
        }

        Err(format!(
            "Missing #endif for conditional starting at line {}",
            start_line
        ))
    }

    /// Skip to #endif, ignoring #elif and #else
    fn skip_to_endif(&mut self, lines: &[&str], start_line: usize) -> Result<usize, String> {
        self.skip_remaining_elif_else(lines, start_line)
    }

    /// Evaluate a preprocessor expression
    ///
    /// This evaluates constant integer expressions with operators like:
    /// - Arithmetic: +, -, *, /, %, unary +, unary -
    /// - Logical: &&, ||, !
    /// - Comparison: ==, !=, <, >, <=, >=
    /// - Bitwise: &, |, ^, ~, <<, >>
    /// - Special: defined(NAME) or defined NAME
    /// Expand macros in preprocessor expression while preserving defined() syntax
    fn expand_macros_in_expr(&self, expr: &str) -> Result<String, String> {
        let mut result = String::new();
        let mut remaining = expr;

        while !remaining.is_empty() {
            // Try to find the next identifier
            if let Some((before, ident, after)) = self.find_next_identifier(remaining) {
                result.push_str(before);

                // Check if this identifier is "defined"
                if ident == "defined" {
                    // This is the defined operator - preserve it and its argument
                    result.push_str("defined");

                    let after_trimmed = after.trim_start();
                    if after_trimmed.starts_with('(') {
                        // Find the closing paren
                        let mut depth = 0;
                        let mut end = 0;
                        for (i, ch) in after_trimmed.char_indices() {
                            if ch == '(' {
                                depth += 1;
                            } else if ch == ')' {
                                depth -= 1;
                                if depth == 0 {
                                    end = i + 1;
                                    break;
                                }
                            }
                        }
                        // Copy the defined(...) part without expansion
                        result.push_str(&after_trimmed[..end]);
                        remaining = &after_trimmed[end..];
                    } else {
                        // defined NAME (without parens)
                        // Find the next identifier after "defined"
                        if let Some((ws, name, rest)) = self.find_next_identifier(after) {
                            result.push_str(ws);
                            result.push_str(name);
                            remaining = rest;
                        } else {
                            remaining = after;
                        }
                    }
                } else if let Some(macro_def) = self.macros.get(ident) {
                    // This is a regular macro - expand it
                    match macro_def {
                        MacroDef::Object(replacement) => {
                            result.push_str(replacement);
                        }
                        MacroDef::Function { .. } => {
                            // Don't expand function macros in expressions for now
                            result.push_str(ident);
                        }
                    }
                    remaining = after;
                } else {
                    // Not a macro - keep as-is
                    result.push_str(ident);
                    remaining = after;
                }
            } else {
                // No more identifiers
                result.push_str(remaining);
                break;
            }
        }

        Ok(result)
    }

    fn eval_preprocessor_expr(&self, expr: &str) -> Result<i64, String> {
        // First expand macros in the expression (but preserve defined())
        let expanded_expr = self.expand_macros_in_expr(expr)?;
        let tokens = self.tokenize_expr(&expanded_expr)?;
        let mut pos = 0;
        let result = self.parse_logical_or(&tokens, &mut pos)?;

        if pos < tokens.len() {
            return Err(format!(
                "Unexpected token after expression: {:?}",
                tokens[pos]
            ));
        }

        Ok(result)
    }

    /// Tokenize a preprocessor expression
    fn tokenize_expr(&self, expr: &str) -> Result<Vec<ExprToken>, String> {
        let mut tokens = Vec::new();
        let mut chars = expr.chars().peekable();

        while let Some(&ch) = chars.peek() {
            match ch {
                ' ' | '\t' => {
                    chars.next();
                }
                '0'..='9' => {
                    let mut num_str = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_digit()
                            || ch == 'x'
                            || ch == 'X'
                            || ('a'..='f').contains(&ch)
                            || ('A'..='F').contains(&ch)
                        {
                            num_str.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    let value = if num_str.starts_with("0x") || num_str.starts_with("0X") {
                        i64::from_str_radix(&num_str[2..], 16)
                            .map_err(|_| format!("Invalid hex number: {}", num_str))?
                    } else {
                        num_str
                            .parse::<i64>()
                            .map_err(|_| format!("Invalid number: {}", num_str))?
                    };
                    tokens.push(ExprToken::Number(value));
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut ident = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_alphanumeric() || ch == '_' {
                            ident.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }

                    // Check for 'defined' operator
                    if ident == "defined" {
                        tokens.push(ExprToken::Defined);
                    } else {
                        // Preserve identifier for defined() or treat as 0
                        tokens.push(ExprToken::Identifier(ident));
                    }
                }
                '(' => {
                    tokens.push(ExprToken::LParen);
                    chars.next();
                }
                ')' => {
                    tokens.push(ExprToken::RParen);
                    chars.next();
                }
                '+' => {
                    tokens.push(ExprToken::Plus);
                    chars.next();
                }
                '-' => {
                    tokens.push(ExprToken::Minus);
                    chars.next();
                }
                '*' => {
                    tokens.push(ExprToken::Star);
                    chars.next();
                }
                '/' => {
                    tokens.push(ExprToken::Slash);
                    chars.next();
                }
                '%' => {
                    tokens.push(ExprToken::Percent);
                    chars.next();
                }
                '&' => {
                    chars.next();
                    if chars.peek() == Some(&'&') {
                        chars.next();
                        tokens.push(ExprToken::AndAnd);
                    } else {
                        tokens.push(ExprToken::And);
                    }
                }
                '|' => {
                    chars.next();
                    if chars.peek() == Some(&'|') {
                        chars.next();
                        tokens.push(ExprToken::OrOr);
                    } else {
                        tokens.push(ExprToken::Or);
                    }
                }
                '^' => {
                    tokens.push(ExprToken::Xor);
                    chars.next();
                }
                '~' => {
                    tokens.push(ExprToken::Tilde);
                    chars.next();
                }
                '!' => {
                    chars.next();
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        tokens.push(ExprToken::NotEq);
                    } else {
                        tokens.push(ExprToken::Bang);
                    }
                }
                '=' => {
                    chars.next();
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        tokens.push(ExprToken::EqEq);
                    } else {
                        return Err("Unexpected '=' in preprocessor expression".to_string());
                    }
                }
                '<' => {
                    chars.next();
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        tokens.push(ExprToken::LessEq);
                    } else if chars.peek() == Some(&'<') {
                        chars.next();
                        tokens.push(ExprToken::LShift);
                    } else {
                        tokens.push(ExprToken::Less);
                    }
                }
                '>' => {
                    chars.next();
                    if chars.peek() == Some(&'=') {
                        chars.next();
                        tokens.push(ExprToken::GreaterEq);
                    } else if chars.peek() == Some(&'>') {
                        chars.next();
                        tokens.push(ExprToken::RShift);
                    } else {
                        tokens.push(ExprToken::Greater);
                    }
                }
                _ => {
                    return Err(format!(
                        "Unexpected character in preprocessor expression: '{}'",
                        ch
                    ));
                }
            }
        }

        Ok(tokens)
    }

    // Expression parsing with operator precedence
    // Precedence (lowest to highest):
    // 1. ||
    // 2. &&
    // 3. |
    // 4. ^
    // 5. &
    // 6. == !=
    // 7. < <= > >=
    // 8. << >>
    // 9. + -
    // 10. * / %
    // 11. unary: + - ! ~
    // 12. defined()

    fn parse_logical_or(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_logical_and(tokens, pos)?;

        while *pos < tokens.len() {
            if let ExprToken::OrOr = tokens[*pos] {
                *pos += 1;
                let right = self.parse_logical_and(tokens, pos)?;
                left = if left != 0 || right != 0 { 1 } else { 0 };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_logical_and(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_bitwise_or(tokens, pos)?;

        while *pos < tokens.len() {
            if let ExprToken::AndAnd = tokens[*pos] {
                *pos += 1;
                let right = self.parse_bitwise_or(tokens, pos)?;
                left = if left != 0 && right != 0 { 1 } else { 0 };
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_bitwise_or(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_bitwise_xor(tokens, pos)?;

        while *pos < tokens.len() {
            if let ExprToken::Or = tokens[*pos] {
                *pos += 1;
                let right = self.parse_bitwise_xor(tokens, pos)?;
                left |= right;
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_bitwise_xor(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_bitwise_and(tokens, pos)?;

        while *pos < tokens.len() {
            if let ExprToken::Xor = tokens[*pos] {
                *pos += 1;
                let right = self.parse_bitwise_and(tokens, pos)?;
                left ^= right;
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_bitwise_and(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_equality(tokens, pos)?;

        while *pos < tokens.len() {
            if let ExprToken::And = tokens[*pos] {
                *pos += 1;
                let right = self.parse_equality(tokens, pos)?;
                left &= right;
            } else {
                break;
            }
        }

        Ok(left)
    }

    fn parse_equality(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_relational(tokens, pos)?;

        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::EqEq => {
                    *pos += 1;
                    let right = self.parse_relational(tokens, pos)?;
                    left = if left == right { 1 } else { 0 };
                }
                ExprToken::NotEq => {
                    *pos += 1;
                    let right = self.parse_relational(tokens, pos)?;
                    left = if left != right { 1 } else { 0 };
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_relational(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_shift(tokens, pos)?;

        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Less => {
                    *pos += 1;
                    let right = self.parse_shift(tokens, pos)?;
                    left = if left < right { 1 } else { 0 };
                }
                ExprToken::LessEq => {
                    *pos += 1;
                    let right = self.parse_shift(tokens, pos)?;
                    left = if left <= right { 1 } else { 0 };
                }
                ExprToken::Greater => {
                    *pos += 1;
                    let right = self.parse_shift(tokens, pos)?;
                    left = if left > right { 1 } else { 0 };
                }
                ExprToken::GreaterEq => {
                    *pos += 1;
                    let right = self.parse_shift(tokens, pos)?;
                    left = if left >= right { 1 } else { 0 };
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_shift(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_additive(tokens, pos)?;

        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::LShift => {
                    *pos += 1;
                    let right = self.parse_additive(tokens, pos)?;
                    left <<= right;
                }
                ExprToken::RShift => {
                    *pos += 1;
                    let right = self.parse_additive(tokens, pos)?;
                    left >>= right;
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_additive(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_multiplicative(tokens, pos)?;

        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Plus => {
                    *pos += 1;
                    let right = self.parse_multiplicative(tokens, pos)?;
                    left += right;
                }
                ExprToken::Minus => {
                    *pos += 1;
                    let right = self.parse_multiplicative(tokens, pos)?;
                    left -= right;
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_multiplicative(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        let mut left = self.parse_unary(tokens, pos)?;

        while *pos < tokens.len() {
            match tokens[*pos] {
                ExprToken::Star => {
                    *pos += 1;
                    let right = self.parse_unary(tokens, pos)?;
                    left *= right;
                }
                ExprToken::Slash => {
                    *pos += 1;
                    let right = self.parse_unary(tokens, pos)?;
                    if right == 0 {
                        return Err("Division by zero in preprocessor expression".to_string());
                    }
                    left /= right;
                }
                ExprToken::Percent => {
                    *pos += 1;
                    let right = self.parse_unary(tokens, pos)?;
                    if right == 0 {
                        return Err("Modulo by zero in preprocessor expression".to_string());
                    }
                    left %= right;
                }
                _ => break,
            }
        }

        Ok(left)
    }

    fn parse_unary(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        if *pos >= tokens.len() {
            return Err("Unexpected end of expression".to_string());
        }

        match &tokens[*pos] {
            ExprToken::Plus => {
                *pos += 1;
                self.parse_unary(tokens, pos)
            }
            ExprToken::Minus => {
                *pos += 1;
                let val = self.parse_unary(tokens, pos)?;
                Ok(-val)
            }
            ExprToken::Bang => {
                *pos += 1;
                let val = self.parse_unary(tokens, pos)?;
                Ok(if val == 0 { 1 } else { 0 })
            }
            ExprToken::Tilde => {
                *pos += 1;
                let val = self.parse_unary(tokens, pos)?;
                Ok(!val)
            }
            ExprToken::Defined => {
                *pos += 1;
                self.parse_defined(tokens, pos)
            }
            _ => self.parse_primary(tokens, pos),
        }
    }

    fn parse_defined(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        if *pos >= tokens.len() {
            return Err("Expected identifier or '(' after 'defined'".to_string());
        }

        // Check for defined(NAME) or defined NAME
        let has_paren = matches!(tokens[*pos], ExprToken::LParen);

        if has_paren {
            *pos += 1; // Skip '('
        }

        if *pos >= tokens.len() {
            return Err("Expected identifier in 'defined'".to_string());
        }

        // Get the identifier name
        let identifier = match &tokens[*pos] {
            ExprToken::Identifier(name) => name.clone(),
            _ => return Err("Expected identifier after 'defined'".to_string()),
        };

        *pos += 1;

        if has_paren {
            if *pos >= tokens.len() || !matches!(tokens[*pos], ExprToken::RParen) {
                return Err("Expected ')' after identifier in 'defined'".to_string());
            }
            *pos += 1;
        }

        // Check if the macro is defined
        let is_defined = self.is_defined(&identifier);
        Ok(if is_defined { 1 } else { 0 })
    }

    fn parse_primary(&self, tokens: &[ExprToken], pos: &mut usize) -> Result<i64, String> {
        if *pos >= tokens.len() {
            return Err("Unexpected end of expression".to_string());
        }

        match &tokens[*pos] {
            ExprToken::Number(n) => {
                let val = *n;
                *pos += 1;
                Ok(val)
            }
            ExprToken::Identifier(_) => {
                // Unknown identifier in preprocessor expression - treat as 0
                *pos += 1;
                Ok(0)
            }
            ExprToken::LParen => {
                *pos += 1;
                let val = self.parse_logical_or(tokens, pos)?;
                if *pos >= tokens.len() || !matches!(tokens[*pos], ExprToken::RParen) {
                    return Err("Expected ')' in expression".to_string());
                }
                *pos += 1;
                Ok(val)
            }
            _ => Err(format!(
                "Unexpected token in expression: {:?}",
                tokens[*pos]
            )),
        }
    }

    /// Expand macros in a line of text
    fn expand_macros(&self, line: &str) -> Result<String, String> {
        let mut result = String::new();
        let mut remaining = line;

        while !remaining.is_empty() {
            // Find the next identifier that could be a macro
            if let Some((before, ident, after)) = self.find_next_identifier(remaining) {
                // Add everything before the identifier
                result.push_str(before);

                // Handle special predefined macros __FILE__ and __LINE__
                if ident == "__FILE__" {
                    result.push_str(&format!("\"{}\"", self.current_file));
                    remaining = after;
                    continue;
                } else if ident == "__LINE__" {
                    result.push_str(&format!("{}", self.current_line));
                    remaining = after;
                    continue;
                }

                // Check if this identifier is a macro
                if let Some(macro_def) = self.macros.get(ident) {
                    match macro_def {
                        MacroDef::Object(replacement) => {
                            // Simple object-like macro - replace with its value
                            let expanded = self.expand_macros(replacement)?;
                            result.push_str(&expanded);
                        }
                        MacroDef::Function {
                            params,
                            body,
                            is_variadic,
                        } => {
                            // Function-like macro - need to parse arguments
                            let after_trimmed = after.trim_start();
                            if after_trimmed.starts_with('(') {
                                // This is a macro invocation
                                let (args, rest) = self.parse_macro_arguments(after_trimmed)?;

                                // Expand the macro with arguments
                                let expanded =
                                    self.expand_function_macro(params, body, *is_variadic, &args)?;
                                result.push_str(&expanded);
                                remaining = rest;
                                continue;
                            } else {
                                // Function-like macro without '(' - don't expand
                                result.push_str(ident);
                            }
                        }
                    }
                } else {
                    // Not a macro - keep the identifier as-is
                    result.push_str(ident);
                }

                remaining = after;
            } else {
                // No more identifiers - add the rest of the line
                result.push_str(remaining);
                break;
            }
        }

        Ok(result)
    }

    /// Find the next identifier in the text
    /// Returns (text_before, identifier, text_after) or None
    fn find_next_identifier<'a>(&self, text: &'a str) -> Option<(&'a str, &'a str, &'a str)> {
        let mut start_idx = None;
        let mut end_idx = None;

        for (i, ch) in text.char_indices() {
            if ch.is_alphabetic() || ch == '_' {
                if start_idx.is_none() {
                    start_idx = Some(i);
                }
            } else if start_idx.is_some() && end_idx.is_none() {
                if ch.is_alphanumeric() || ch == '_' {
                    // Continue identifier
                } else {
                    end_idx = Some(i);
                    break;
                }
            }
        }

        if let Some(start) = start_idx {
            let end = end_idx.unwrap_or(text.len());
            Some((&text[..start], &text[start..end], &text[end..]))
        } else {
            None
        }
    }

    /// Parse macro arguments from a function call
    /// Returns (arguments, remaining_text)
    fn parse_macro_arguments<'a>(&self, text: &'a str) -> Result<(Vec<String>, &'a str), String> {
        if !text.starts_with('(') {
            return Err("Expected '(' for macro arguments".to_string());
        }

        let mut args = Vec::new();
        let mut current_arg = String::new();
        let mut depth = 0;
        let mut chars = text.char_indices();
        let mut end_pos = 0;

        // Skip the opening '('
        chars.next();

        for (i, ch) in chars {
            match ch {
                '(' => {
                    depth += 1;
                    current_arg.push(ch);
                }
                ')' => {
                    if depth == 0 {
                        // End of arguments
                        if !current_arg.trim().is_empty() || !args.is_empty() {
                            args.push(current_arg.trim().to_string());
                        }
                        end_pos = i + 1;
                        break;
                    } else {
                        depth -= 1;
                        current_arg.push(ch);
                    }
                }
                ',' if depth == 0 => {
                    // Argument separator at top level
                    args.push(current_arg.trim().to_string());
                    current_arg.clear();
                }
                _ => {
                    current_arg.push(ch);
                }
            }
        }

        if end_pos == 0 {
            return Err("Unclosed macro argument list".to_string());
        }

        Ok((args, &text[end_pos..]))
    }

    /// Normalize whitespace in a string for stringification
    /// Collapses multiple whitespace chars to single space, trims edges
    fn normalize_whitespace(s: &str) -> String {
        let mut result = String::new();
        let mut prev_was_space = false;

        for ch in s.chars() {
            if ch.is_whitespace() {
                if !prev_was_space && !result.is_empty() {
                    result.push(' ');
                    prev_was_space = true;
                }
            } else {
                result.push(ch);
                prev_was_space = false;
            }
        }

        // Trim trailing space
        result.trim_end().to_string()
    }

    /// Expand a function-like macro with arguments
    fn expand_function_macro(
        &self,
        params: &[String],
        body: &str,
        is_variadic: bool,
        args: &[String],
    ) -> Result<String, String> {
        // Check argument count
        if is_variadic {
            if args.len() < params.len() {
                return Err(format!(
                    "Too few arguments for variadic macro: expected at least {}, got {}",
                    params.len(),
                    args.len()
                ));
            }
        } else if args.len() != params.len() {
            return Err(format!(
                "Wrong number of arguments for macro: expected {}, got {}",
                params.len(),
                args.len()
            ));
        }

        let mut result = body.to_string();

        // Handle __VA_ARGS__ for variadic macros
        if is_variadic && args.len() > params.len() {
            let variadic_args = args[params.len()..].join(", ");
            result = result.replace("__VA_ARGS__", &variadic_args);
        }

        // First pass: handle stringification (#param) - must be done before ## processing
        // BUT we need to be careful not to replace # that's part of ##
        for (i, param) in params.iter().enumerate() {
            let normalized = Self::normalize_whitespace(&args[i]);
            let stringified = format!("\"{}\"", normalized);
            let pattern = format!("#{}", param);

            // Replace only if it's not preceded by another #
            let mut new_result = String::new();
            let mut remaining = result.as_str();

            while let Some(pos) = remaining.find(&pattern) {
                // Check if there's a # before this position
                let is_token_paste = pos > 0 && remaining.as_bytes()[pos - 1] == b'#';

                new_result.push_str(&remaining[..pos]);
                if is_token_paste {
                    // This is ##param, not #param - don't stringify
                    new_result.push_str(&pattern);
                } else {
                    // This is #param - stringify it
                    new_result.push_str(&stringified);
                }
                remaining = &remaining[pos + pattern.len()..];
            }
            new_result.push_str(remaining);
            result = new_result;
        }

        // Second pass: handle token pasting (##)
        // Token pasting must be done before regular replacement
        result = self.process_token_pasting(&result, params, args);

        // Third pass: regular parameter replacement
        for (i, param) in params.iter().enumerate() {
            result = self.replace_parameter(&result, param, &args[i]);
        }

        // Recursively expand macros in the result
        self.expand_macros(&result)
    }

    /// Process token pasting operators (##) in macro body
    fn process_token_pasting(&self, body: &str, params: &[String], args: &[String]) -> String {
        let mut result = body.to_string();

        // Process ## operators
        // Look for patterns like "param1##param2" or "param1##literal" or "literal##param2"
        while let Some(paste_pos) = result.find("##") {
            // Find the token before ##
            let before_paste = &result[..paste_pos].trim_end();
            let left_token = self.get_last_token(before_paste);

            // Find the token after ##
            let after_paste = &result[paste_pos + 2..].trim_start();
            let right_token = self.get_first_token(after_paste);

            // Replace parameter names with their values
            let left_value = self.param_to_arg(left_token, params, args);
            let right_value = self.param_to_arg(right_token, params, args);

            // Concatenate the tokens
            let concatenated = format!("{}{}", left_value, right_value);

            // Build the new result
            let before_left = &result[..paste_pos - left_token.len()];
            let after_right = &result[paste_pos + 2 + right_token.len()..];
            result = format!(
                "{}{}{}",
                before_left.trim_end(),
                concatenated,
                after_right.trim_start()
            );
        }

        result
    }

    /// Get the last token from a string
    fn get_last_token<'a>(&self, text: &'a str) -> &'a str {
        let trimmed = text.trim_end();
        let mut start = trimmed.len();

        for (i, ch) in trimmed.char_indices().rev() {
            if ch.is_alphanumeric() || ch == '_' {
                start = i;
            } else {
                break;
            }
        }

        &trimmed[start..]
    }

    /// Get the first token from a string
    fn get_first_token<'a>(&self, text: &'a str) -> &'a str {
        let trimmed = text.trim_start();
        let mut end = 0;

        for (i, ch) in trimmed.char_indices() {
            if ch.is_alphanumeric() || ch == '_' {
                end = i + ch.len_utf8();
            } else {
                break;
            }
        }

        if end == 0 && !trimmed.is_empty() {
            // Single character that's not alphanumeric
            let first_char = trimmed.chars().next().unwrap();
            &trimmed[..first_char.len_utf8()]
        } else {
            &trimmed[..end]
        }
    }

    /// Convert parameter name to argument value, or return as-is if not a parameter
    fn param_to_arg(&self, token: &str, params: &[String], args: &[String]) -> String {
        for (i, param) in params.iter().enumerate() {
            if token == param {
                return args[i].clone();
            }
        }
        token.to_string()
    }

    /// Replace a parameter with its argument, respecting identifier boundaries
    fn replace_parameter(&self, text: &str, param: &str, arg: &str) -> String {
        let mut result = String::new();
        let mut remaining = text;

        while !remaining.is_empty() {
            if let Some((before, ident, after)) = self.find_next_identifier(remaining) {
                result.push_str(before);
                if ident == param {
                    result.push_str(arg);
                } else {
                    result.push_str(ident);
                }
                remaining = after;
            } else {
                result.push_str(remaining);
                break;
            }
        }

        result
    }
}

impl Default for Preprocessor {
    fn default() -> Self {
        Self::new()
    }
}
