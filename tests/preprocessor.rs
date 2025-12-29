use insta::assert_yaml_snapshot;
use serde::Serialize;
use std::{
    io,
    path::Path,
    process::{Command as StdCommand, Output, Stdio},
};

fn compile(src: &Path, exe: &Path, include_dir: Option<&Path>) -> io::Result<Output> {
    let mut cmd = StdCommand::new("clang");
    cmd.arg("-o").arg(exe).arg(src);

    // Add include directory if provided
    if let Some(dir) = include_dir {
        cmd.arg("-I").arg(dir);
    }

    cmd.stdout(Stdio::piped()).stderr(Stdio::piped()).output()
}

fn run_exe(exe: &Path) -> io::Result<Output> {
    StdCommand::new(exe)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
}

#[derive(Serialize, Debug, Clone, PartialEq, Eq)]
struct RunLog {
    stdout: String,
    status: i32,
}

fn to_runlog(out: Output) -> RunLog {
    RunLog {
        stdout: String::from_utf8_lossy(&out.stdout).to_string(),
        status: out.status.code().unwrap_or(-1),
    }
}

fn ensure_success(tag: &str, path: &Path, out: &Output) {
    assert!(
        out.status.success(),
        "[{}] {} failed (status: {:?})\n--- stdout ---\n{}\n--- stderr ---\n{}",
        path.display(),
        tag,
        out.status.code(),
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
}

fn run_case(path: &Path) -> datatest_stable::Result<()> {
    let tmp = tempfile::tempdir()?;
    let stem = path.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
    let asm_path = tmp.path().join(format!("{stem}.S"));
    let exe_mine = tmp.path().join(format!("{stem}.mine"));
    let exe_ref = tmp.path().join(format!("{stem}.ref"));

    // Get the directory containing the test file (for include paths)
    let test_dir = path
        .parent()
        .expect("test file should have parent directory");

    // 1) Your compiler: codegen -> .S, then cc -> exe_mine
    let codegen_out = StdCommand::new(assert_cmd::cargo::cargo_bin!(env!("CARGO_PKG_NAME")))
        .arg(path)
        .arg("-o")
        .arg(&asm_path)
        .arg("-I")
        .arg(test_dir) // Add test directory to include path
        .stderr(Stdio::piped())
        .output()?;
    eprintln!(
        "[{}] codegen status: {:?}",
        path.display(),
        codegen_out.status.code()
    );
    ensure_success("codegen", path, &codegen_out);

    let compile_out_mine = compile(&asm_path, &exe_mine, None)?;
    eprintln!(
        "[{}] cc(asm) status: {:?}",
        path.display(),
        compile_out_mine.status.code()
    );
    ensure_success("cc(asm)", path, &compile_out_mine);

    let run_out_mine = run_exe(&exe_mine)?;
    let mine = to_runlog(run_out_mine);

    assert_yaml_snapshot!(stem.to_string(), &mine);

    let compile_out_ref = compile(path, &exe_ref, Some(test_dir))?;
    eprintln!(
        "[{}] cc(src) status: {:?}",
        path.display(),
        compile_out_ref.status.code()
    );
    ensure_success("cc(src)", path, &compile_out_ref);

    let run_out_ref = run_exe(&exe_ref)?;
    let reference = to_runlog(run_out_ref);

    if mine != reference {
        let mut msg = String::new();
        use std::fmt::Write;
        writeln!(&mut msg, "\n=== MISMATCH for {} ===", path.display()).ok();
        writeln!(&mut msg, "Mine:      {:?}", mine).ok();
        writeln!(&mut msg, "Reference: {:?}", reference).ok();
        panic!("{}", msg);
    }

    Ok(())
}

datatest_stable::harness! {
    { test = run_case, root = "./tests/files/preprocessor/", pattern = r#"^.*.c$"# },
}
