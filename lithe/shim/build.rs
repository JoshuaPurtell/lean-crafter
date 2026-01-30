use std::{env, fs, path::{Path, PathBuf}, process::Command};

fn read_toolchain(path: &Path) -> Option<String> {
    fs::read_to_string(path)
        .ok()
        .and_then(|s| s.lines().next().map(|l| l.trim().to_string()))
        .filter(|s| !s.is_empty())
}

fn toolchain_dir_name(toolchain: &str) -> String {
    toolchain.replace('/', "--").replace(':', "---")
}

fn lean_sysroot(repo_root: &Path, example_dir: &Path) -> PathBuf {
    if let Ok(root) = env::var("LEAN_SYSROOT") {
        return PathBuf::from(root);
    }
    if let Ok(root) = env::var("LEAN_ROOT") {
        return PathBuf::from(root);
    }

    let toolchain = read_toolchain(&example_dir.join("lean-toolchain"))
        .or_else(|| read_toolchain(&repo_root.join("lean-toolchain")))
        .expect("lean-toolchain not found");

    let elan_home = env::var("ELAN_HOME")
        .ok()
        .or_else(|| env::var("HOME").ok().map(|h| format!("{}/.elan", h)))
        .expect("ELAN_HOME or HOME not set");

    let dir_name = toolchain_dir_name(&toolchain);
    PathBuf::from(elan_home).join("toolchains").join(dir_name)
}

fn add_c_files(build: &mut cc::Build, dir: &Path, label: &str) {
    for entry in walkdir::WalkDir::new(dir) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("c") {
            build.file(path);
        }
    }
    println!("cargo:rerun-if-changed={}", dir.display());
    println!("cargo:warning=Including {} C files from {}", label, dir.display());
}

fn main() {
    let manifest_dir = PathBuf::from(env::var("CARGO_MANIFEST_DIR").unwrap());
    let repo_root = manifest_dir
        .parent()
        .and_then(|p| p.parent())
        .expect("repo root");

    let example_dir = repo_root.join("lithe");
    let example_ir = example_dir.join(".lake/build/ir");
    let sqlite_pkg = example_dir.join(".lake/packages/SQLite");
    let sqlite_ir = sqlite_pkg.join(".lake/build/ir");
    let sqlite_lib = sqlite_pkg.join(".lake/build/lib");

    let lithe_repo = repo_root.parent().expect("workspace root").join("lithe");
    let lithe_ir = lithe_repo.join(".lake/build/ir");

    let crafter_lean_dir = repo_root.join("src_lean4");
    let crafter_lean_ir = crafter_lean_dir.join(".lake/build/ir");

    let skip_lake = env::var("LITHE_SKIP_LAKE_BUILD").is_ok();
    if !skip_lake {
        let status = Command::new("lake")
            .arg("build")
            .current_dir(&example_dir)
            .status()
            .expect("failed to run lake build for crafter web");
        if !status.success() {
            panic!("lake build failed for crafter web");
        }
    } else if !example_ir.exists() {
        panic!("missing {} (set LITHE_SKIP_LAKE_BUILD only if C output exists)", example_ir.display());
    }

    let lean_root = lean_sysroot(&repo_root, &example_dir);
    if !lean_root.exists() {
        panic!("Lean sysroot not found at {}", lean_root.display());
    }
    println!("cargo:rustc-link-lib=dylib=leanshared");
    let lean_lib_dir = lean_root.join("lib/lean");
    println!("cargo:rustc-link-search=native={}", lean_lib_dir.display());
    if env::var("CARGO_CFG_TARGET_OS").unwrap_or_default() != "windows" {
        println!("cargo:rustc-link-arg=-Wl,-rpath,{}", lean_lib_dir.display());
    }

    let mut build = cc::Build::new();
    build.include(lean_root.join("include"));
    build.flag_if_supported("-std=c11");

    add_c_files(&mut build, &example_ir, "CrafterWeb");
    add_c_files(&mut build, &lithe_ir, "Lithe");
    add_c_files(&mut build, &crafter_lean_ir, "CrafterLean");
    if sqlite_ir.exists() {
        add_c_files(&mut build, &sqlite_ir, "SQLite");
    }

    build.file(manifest_dir.join("lean_shim.c"));

    build.compile("crafter_web_lean");

    if sqlite_lib.exists() {
        println!("cargo:rustc-link-search=native={}", sqlite_lib.display());
        println!("cargo:rustc-link-lib=static=leansqlite");
    }

    println!("cargo:rerun-if-changed={}", manifest_dir.join("lean_shim.c").display());
    for entry in walkdir::WalkDir::new(&example_dir) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("lean") {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }
    for entry in walkdir::WalkDir::new(&lithe_repo) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("lean") {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }
    for entry in walkdir::WalkDir::new(&crafter_lean_dir) {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("lean") {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }
    println!("cargo:rerun-if-changed={}", example_dir.join("lakefile.lean").display());
    println!("cargo:rerun-if-changed={}", lithe_repo.join("lakefile.lean").display());
    println!("cargo:rerun-if-changed={}", crafter_lean_dir.join("lakefile.toml").display());
    println!("cargo:rerun-if-changed={}", example_dir.join("lean-toolchain").display());
    println!("cargo:rerun-if-env-changed=LEAN_SYSROOT");
    println!("cargo:rerun-if-env-changed=LEAN_ROOT");
    println!("cargo:rerun-if-env-changed=ELAN_HOME");
    println!("cargo:rerun-if-env-changed=LITHE_SKIP_LAKE_BUILD");
}
