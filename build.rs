use std::process::Command;

fn rustc_version() -> String {
    let result = Command::new("rustc")
        .arg("--version")
        .output()
        .expect("failed to run rustc");

    return String::from_utf8(result.stdout).unwrap();
}

fn cargo_version() -> String {
    let result = Command::new("cargo")
        .arg("--version")
        .output()
        .expect("failed to run cargo");

    return String::from_utf8(result.stdout).unwrap();
}

fn main() {
   println!("cargo:rustc-env=RUSTC_VERSION={}", rustc_version());
   println!("cargo:rustc-env=CARGO_VERSION={}", cargo_version());
}
