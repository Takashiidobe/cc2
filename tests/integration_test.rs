use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

#[test]
fn test_help_flag() {
    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg("--help");
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("C compiler"));
}

#[test]
fn test_missing_input_file() {
    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg("nonexistent.c");
    cmd.assert()
        .failure()
        .stderr(predicate::str::contains("Error reading file"));
}

#[test]
fn test_compile_simple_return() {
    let test_file = "tests/fixtures/simple_return.c";
    fs::write(test_file, "int main() { return 42; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg(test_file);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Compiled"));

    let output_file = "tests/fixtures/simple_return.s";
    assert!(std::path::Path::new(output_file).exists());

    let asm = fs::read_to_string(output_file).unwrap();
    assert!(asm.contains("main:"));
    assert!(asm.contains("movq $42, %rax"));

    fs::remove_file(test_file).ok();
    fs::remove_file(output_file).ok();
}

#[test]
fn test_lex_only_flag() {
    let test_file = "tests/fixtures/lex_test.c";
    fs::write(test_file, "int main() { return 0; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg("--lex-only").arg(test_file);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("int"))
        .stdout(predicate::str::contains("main"));

    fs::remove_file(test_file).ok();
}

#[test]
fn test_parse_only_flag() {
    let test_file = "tests/fixtures/parse_test.c";
    fs::write(test_file, "int main() { return 0; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg("--parse-only").arg(test_file);
    cmd.assert()
        .success()
        .stdout(predicate::str::contains("Program"));

    fs::remove_file(test_file).ok();
}
