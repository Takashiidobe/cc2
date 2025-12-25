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

#[test]
fn test_compile_arithmetic_addition() {
    let test_file = "tests/fixtures/arithmetic_add.c";
    fs::write(test_file, "int main() { return 2 + 3; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg(test_file);
    cmd.assert().success();

    let output_file = "tests/fixtures/arithmetic_add.s";
    assert!(std::path::Path::new(output_file).exists());

    let asm = fs::read_to_string(output_file).unwrap();
    assert!(asm.contains("addq %rcx, %rax"));

    fs::remove_file(test_file).ok();
    fs::remove_file(output_file).ok();
}

#[test]
fn test_compile_arithmetic_precedence() {
    let test_file = "tests/fixtures/arithmetic_precedence.c";
    fs::write(test_file, "int main() { return 2 + 3 * 4; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg(test_file);
    cmd.assert().success();

    let output_file = "tests/fixtures/arithmetic_precedence.s";
    let asm = fs::read_to_string(output_file).unwrap();

    assert!(asm.contains("imulq"));
    assert!(asm.contains("addq"));

    fs::remove_file(test_file).ok();
    fs::remove_file(output_file).ok();
}

#[test]
fn test_compile_arithmetic_parentheses() {
    let test_file = "tests/fixtures/arithmetic_parens.c";
    fs::write(test_file, "int main() { return (2 + 3) * 4; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg(test_file);
    cmd.assert().success();

    let output_file = "tests/fixtures/arithmetic_parens.s";
    let asm = fs::read_to_string(output_file).unwrap();

    assert!(asm.contains("addq"));
    assert!(asm.contains("imulq"));

    fs::remove_file(test_file).ok();
    fs::remove_file(output_file).ok();
}

#[test]
fn test_compile_arithmetic_division() {
    let test_file = "tests/fixtures/arithmetic_div.c";
    fs::write(test_file, "int main() { return 20 / 4; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg(test_file);
    cmd.assert().success();

    let output_file = "tests/fixtures/arithmetic_div.s";
    let asm = fs::read_to_string(output_file).unwrap();

    assert!(asm.contains("idivq"));
    assert!(asm.contains("cqto"));

    fs::remove_file(test_file).ok();
    fs::remove_file(output_file).ok();
}

#[test]
fn test_compile_arithmetic_subtraction() {
    let test_file = "tests/fixtures/arithmetic_sub.c";
    fs::write(test_file, "int main() { return 10 - 3; }").unwrap();

    let mut cmd = Command::cargo_bin("cc2").unwrap();
    cmd.arg(test_file);
    cmd.assert().success();

    let output_file = "tests/fixtures/arithmetic_sub.s";
    let asm = fs::read_to_string(output_file).unwrap();

    assert!(asm.contains("subq"));

    fs::remove_file(test_file).ok();
    fs::remove_file(output_file).ok();
}
