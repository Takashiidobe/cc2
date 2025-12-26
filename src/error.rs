use crate::lexer::SourceLocation;
use colored::*;
use std::fmt;

/// A compile error with location information
#[derive(Debug, Clone)]
pub struct CompileError {
    pub location: Option<SourceLocation>,
    pub message: String,
}

impl CompileError {
    pub fn new(message: String, location: Option<SourceLocation>) -> Self {
        CompileError { location, message }
    }

    pub fn with_location(message: String, location: SourceLocation) -> Self {
        CompileError {
            location: Some(location),
            message,
        }
    }

    pub fn without_location(message: String) -> Self {
        CompileError {
            location: None,
            message,
        }
    }
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(loc) = self.location {
            write!(f, "{} at {}", self.message, loc)
        } else {
            write!(f, "{}", self.message)
        }
    }
}

impl From<String> for CompileError {
    fn from(message: String) -> Self {
        CompileError::without_location(message)
    }
}

/// Format an error message with source context
pub fn format_error(
    filename: &str,
    source: &str,
    location: SourceLocation,
    message: &str,
) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let line_idx = location.line - 1; // Convert to 0-based index

    if line_idx >= lines.len() {
        // Location is out of bounds, just return basic error
        return format!(
            "{}: {} at {}",
            "error".red().bold(),
            message,
            format!("{}:{}", filename, location).cyan()
        );
    }

    let line = lines[line_idx];
    let line_num_width = format!("{}", location.line).len().max(3);

    // Build the error message with context
    let mut output = String::new();

    // Error header
    output.push_str(&format!(
        "{}{} {}\n",
        "error".red().bold(),
        ":".bold(),
        message
    ));

    // Location info
    output.push_str(&format!(
        "  {} {}:{}:{}\n",
        "-->".cyan().bold(),
        filename,
        location.line,
        location.column
    ));

    output.push_str(&format!("{}\n", " ".repeat(line_num_width + 1)));

    // Show previous line for context if available
    if line_idx > 0 {
        output.push_str(&format!(
            "{:width$} {} {}\n",
            location.line - 1,
            "|".cyan().bold(),
            lines[line_idx - 1],
            width = line_num_width
        ));
    }

    // Show the error line
    output.push_str(&format!(
        "{:width$} {} {}\n",
        location.line,
        "|".cyan().bold(),
        line,
        width = line_num_width
    ));

    // Show the caret pointing to the error
    let caret_padding = line_num_width + 3 + (location.column - 1);
    output.push_str(&format!(
        "{}{}\n",
        " ".repeat(caret_padding),
        "^".red().bold()
    ));

    // Show next line for context if available
    if line_idx + 1 < lines.len() {
        output.push_str(&format!(
            "{:width$} {} {}\n",
            location.line + 1,
            "|".cyan().bold(),
            lines[line_idx + 1],
            width = line_num_width
        ));
    }

    output
}

/// Format a simple error without source context
pub fn format_simple_error(message: &str) -> String {
    format!("{}{} {}", "error".red().bold(), ":".bold(), message)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_error() {
        let source = "int main() {\n    return 42\n}\n";
        let location = SourceLocation::new(2, 14);
        let message = "Expected Semicolon, got CloseBrace";

        let formatted = format_error("test.c", source, location, message);

        // Just check that it contains the key components
        assert!(formatted.contains("error"));
        assert!(formatted.contains(message));
        assert!(formatted.contains("test.c:2:14"));
        assert!(formatted.contains("return 42"));
        assert!(formatted.contains("^"));
    }

    #[test]
    fn test_format_error_first_line() {
        let source = "int main {\n    return 0;\n}\n";
        let location = SourceLocation::new(1, 10);
        let message = "Expected OpenParen, got OpenBrace";

        let formatted = format_error("test.c", source, location, message);

        assert!(formatted.contains("int main {"));
        assert!(formatted.contains("^"));
    }
}
