use assert_cmd::cargo::CommandCargoExt;
use insta::assert_yaml_snapshot;
use insta_cmd::Command;
use serde::Serialize;
use std::{io, path::Path, process::Stdio};

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct ParseOutput {
    stdout: String,
    stderr: String,
    status: i32,
}

fn run_parse(path: &Path) -> datatest_stable::Result<()> {
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");

    let mut bin = Command::cargo_bin(env!("CARGO_PKG_NAME"))?;
    let output = bin
        .arg(path)
        .arg("--parse-only")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()?;

    let result = ParseOutput {
        stdout: String::from_utf8_lossy(&output.stdout).to_string(),
        stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        status: output.status.code().unwrap_or(-1),
    };

    if !output.status.success() {
        return Err(format!(
            "Parsing failed for {}: status={}\nstderr: {}",
            path.display(),
            result.status,
            result.stderr
        )
        .into());
    }

    assert_yaml_snapshot!(stem.to_string(), &result);

    Ok(())
}

datatest_stable::harness! {
    { test = run_parse, root = "./tests/files/", pattern = r#"^.*.c$"# },
}
