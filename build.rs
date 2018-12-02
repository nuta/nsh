use std::fs::read_dir;

fn look_for(ext: &str) -> String {
    read_dir("src/config/ui/build")
        .expect("failed to readdir build: have you run `yarn build' in the `src/config/ui' directory?")
        .map(|e| e.unwrap().path().strip_prefix("src/config/ui/build").unwrap().to_str().unwrap().to_owned())
        .filter(|path| path.ends_with(ext))
        .nth(0)
        .unwrap()
}

fn main() {
   println!("cargo:rustc-env=CONFIG_UI_JS_FILENAME={}", look_for(".js"));
   println!("cargo:rustc-env=CONFIG_UI_MAP_FILENAME={}", look_for(".map"));
}
