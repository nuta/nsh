use std::process::Command;
use std::fs::read_dir;

fn look_for(ext: &str) -> String {
    read_dir("src/config/ui/build")
        .expect("failed to readdir build: have you run `yarn build' in the `src/config/ui' directory?")
        .map(|e| e.unwrap().path().strip_prefix("src/config/ui/build").unwrap().to_str().unwrap().to_owned())
        .filter(|path| path.ends_with(ext))
        .nth(0)
        .unwrap()
}

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
   println!("cargo:rustc-env=CONFIG_UI_JS_FILENAME={}", look_for(".js"));
   println!("cargo:rustc-env=CONFIG_UI_CSS_FILENAME={}", look_for(".css"));
   println!("cargo:rustc-env=CONFIG_UI_MAP_FILENAME={}", look_for(".map"));
}
