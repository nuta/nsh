use std::io::Read;
use std::path::PathBuf;
use std::collections::BTreeMap;
use rocket::response::content::{Content, Json};
use rocket::http::{ContentType, Status};
use rocket::config::{Config, Environment};
use rand::{self, Rng, thread_rng};
use rocket::Data;

lazy_static! {
    static ref FILES: BTreeMap<String, Content<String>> = {
        let mut files = BTreeMap::new();

        macro_rules! add_file {
            ($name:expr, $content_type:expr) => {
                files.insert(
                    $name.to_owned(),
                    Content(
                        $content_type,
                        include_str!(concat!("../../src/config/ui/build/", $name)).to_owned()
                    )
                );
            };
        };

        add_file!("index.html", ContentType::HTML);
        add_file!(env!("CONFIG_UI_JS_FILENAME"), ContentType::JavaScript);
        add_file!(env!("CONFIG_UI_MAP_FILENAME"), ContentType::JSON);
        files
    };

    static ref ACCESS_TOKEN: String = {
        let mut rng = thread_rng();
        rng.sample_iter(&rand::distributions::Alphanumeric).take(64).collect()
    };
}

#[post("/api/save?<access_token>", data = "<payload>")]
fn save(access_token: String, payload: Data) -> Status {
    if access_token != *ACCESS_TOKEN {
        return Status::BadRequest;
    }

    let mut nshconfig_path = dirs::home_dir().unwrap();
    nshconfig_path.push(".nshconfig");

    payload.stream_to_file(nshconfig_path)
        .expect("failed to save ~/.nshconfig");

    Status::Ok
}

#[get("/api/load?<access_token>")]
fn load(access_token: String) -> Result<Json<String>, Status> {
    if access_token != *ACCESS_TOKEN {
        return Err(Status::BadRequest);
    }

    let mut nshconfig_path = dirs::home_dir().unwrap();
    nshconfig_path.push(".nshconfig");

    let f = std::fs::OpenOptions::new().read(true).open(nshconfig_path);
    if let Ok(mut f) = f {
        let mut body = String::new();
        f.read_to_string(&mut body).expect("failed to read to ~/.nshconfig");
        Ok(Json(body))
    } else {
        Ok(Json("{}".to_owned()))
    }
}

#[get("/")]
fn index() -> Option<Content<String>> {
    FILES.get("index.html").cloned()
}

#[get("/api/<requested_path..>")]
fn static_files(requested_path: PathBuf) -> Option<Content<String>> {
    let path = requested_path.to_str()?.to_owned();
    FILES.get(&path).cloned()
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

pub fn main() {
    let env = if cfg!(debug_assertions) {
        Environment::Development
    } else {
        Environment::Production
    };

    let host = "127.0.0.1";
    let port = 7171;
    let config = Config::build(env)
        .address(host)
        .port(port)
        .finalize()
        .expect("failed to launch a web server");

    let url = if cfg!(debug_assertions) {
        format!("http://{}:{}/?access_token={}", host, 27171, &*ACCESS_TOKEN)
    } else {
        format!("http://{}:{}/?access_token={}", host, port, &*ACCESS_TOKEN)
    };

    open_in_web_browser(url.clone());
    println!("*** Open {} in your web browser!", url);

    rocket::custom(config).mount("/", routes![index, save, load, static_files]).launch();
}
