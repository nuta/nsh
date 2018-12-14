use std::collections::BTreeMap;
use rand::{self, Rng, thread_rng};
use iron::prelude::*;
use iron::status;
use router::Router;
use std::io::{Read, Write};

lazy_static! {
    static ref FILES: BTreeMap<String, (Mime, String)> = {
        let mut files = BTreeMap::new();

        macro_rules! add_file {
            ($name:expr, $content_type:expr) => {
                files.insert(
                    $name.to_owned(),
                    (
                        $content_type.parse::<Mime>().unwrap(),
                        include_str!(concat!("../../src/config/ui/build/", $name)).to_owned()
                    )
                );
            };
        };

        add_file!("index.html", "text/html");
        add_file!(env!("CONFIG_UI_JS_FILENAME"), "text/javascript");
        add_file!(env!("CONFIG_UI_CSS_FILENAME"), "text/css");
        add_file!(env!("CONFIG_UI_MAP_FILENAME"), "application/json");
        files
    };

    static ref ACCESS_TOKEN: String = {
        let mut rng = thread_rng();
        rng.sample_iter(&rand::distributions::Alphanumeric).take(64).collect()
    };
}

fn open_in_web_browser(url: String) {
    std::thread::spawn(move || {
        std::process::Command::new("python")
            .args(&["-m", "webbrowser", "-t", &url])
            .spawn()
            .expect("failed to open a web browser")
            .wait()
            .ok();
    });
}

fn serve_static_file(filename: &str) -> IronResult<Response> {
    if let Some((content_type, body)) = FILES.get(filename) {
        Ok(Response::with((content_type.to_owned(), status::Ok, body.clone())))
    } else {
        Ok(Response::with(status::NotFound))
    }
}

fn index(_req: &mut Request) -> IronResult<Response> {
    serve_static_file("index.html")
}

fn static_file(req: &mut Request) -> IronResult<Response> {
    let file = &req.extensions.get::<Router>().unwrap().find("file").unwrap_or("/");
    serve_static_file(file)
}

use params::{Params, Value};
use iron::mime::Mime;

/// `/api/load?access_token=<access_token>`
fn load(req: &mut Request) -> IronResult<Response> {
    let content_type = "application/json".parse::<Mime>().unwrap();

    let params = req.get_ref::<Params>().unwrap();
    match params.find(&["access_token"]) {
        Some(&Value::String(ref token)) if *token == *ACCESS_TOKEN => {
            let mut nshconfig_path = dirs::home_dir().unwrap();
            nshconfig_path.push(".nshconfig");

            let f = std::fs::OpenOptions::new().read(true).open(nshconfig_path);
            if let Ok(mut f) = f {
                let mut body = String::new();
                f.read_to_string(&mut body).expect("failed to read to ~/.nshconfig");
                Ok(Response::with((content_type, status::Ok, body)))
            } else {
                Ok(Response::with((content_type, status::Ok, "{}".to_owned())))
            }
        }
        _ => {
            Ok(Response::with(status::Forbidden))
        }
    }
}

/// `/api/save?access_token=<access_token>`
fn save(req: &mut Request) -> IronResult<Response> {
    let mut body = String::new();
    req.body.read_to_string(&mut body)
        .expect("failed to read body");
    let params = req.get_ref::<Params>().unwrap();

    match params.find(&["access_token"]) {
        Some(&Value::String(ref token)) if *token == *ACCESS_TOKEN => {
            let mut nshconfig_path = dirs::home_dir().unwrap();
            nshconfig_path.push(".nshconfig");


            let mut nshconfig_path = dirs::home_dir().unwrap();
            nshconfig_path.push(".nshconfig");

            let mut f = std::fs::File::create(nshconfig_path)
                .expect("failed to save ~/.nshconfig");
            f.write_all(body.as_bytes()).expect("failed to read to ~/.nshconfig");

            println!("saved to ~/.nshconfig");
            Ok(Response::with(status::Ok))
        }
        _ => {
            Ok(Response::with(status::Forbidden))
        }
    }
}

pub fn main() {
    let port = if cfg!(debug_assertions) {
        // access through ./dev-server.js
        27171
    } else {
        // access to this server directly
        7171
    };

    let mut router = Router::new();
    router.get("/", index, "index");
    router.get("/api/load", load, "load");
    router.post("/api/save", save, "save");
    router.get("/:file", static_file, "static_file");

    std::thread::spawn(move || {
        let url = format!("http://127.0.0.1:{}/?access_token={}", port, &*ACCESS_TOKEN);
        std::thread::sleep(std::time::Duration::from_millis(500));
        open_in_web_browser(url.clone());
        println!("Open {}", url);
        println!("Press Ctrl-C to quit.");
    });

    Iron::new(router).http("127.0.0.1:7171").unwrap();
}
