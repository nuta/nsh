const http = require("http");
const fs = require("fs");
const os = require("os");
const path = require("path");
const crypto = require("crypto");
const parse_url = require("url").parse;
const opn = require('opn');

function generate_access_token() {
    const buf = Buffer.alloc(32);
    return crypto.randomFillSync(buf).toString("hex");
}

function get_static(filename) {
    const filepath = path.resolve(__dirname, "build", path.basename(filename));
    if (!fs.existsSync(filepath)) {
        return [404, "", ""];
    }

    let type;
    switch (path.extname(filepath)) {
        case ".html": type = "text/html"; break;
        case ".js": type = "application/javascript"; break;
        case ".css": type = "text/css"; break;
        case ".map": type = "application/json"; break;
    }

    const data = fs.readFileSync(filepath);
    return [200, type, data];
}

function load_config() {
    const filepath = path.resolve(os.homedir(), ".nshrc");
    if (!fs.existsSync(filepath)) {
        return ""
    }

    return fs.readFileSync(filepath);
}

function save_config(body) {
    const filepath = path.resolve(os.homedir(), ".nshrc");
    fs.writeFileSync(filepath, body);
}

function save_history(body) {
    const filepath = path.resolve(os.homedir(), ".nsh_history");
    fs.writeFileSync(filepath, body);
}

function handle_request(url, body) {
    let pathname = url.pathname;
    switch (pathname) {
        case "/api/load_config":
            return [200, "text/plain", load_config()];
        case "/api/save_config":
            save_config(body);
            return [200, "text/plain", "{}"];
        case "/api/save_history":
            save_history(body);
            return [200, "application/json", "{}"];
        case "/":
            pathname = "/index.html";
            /* fallthrough */
        default:
            return get_static(pathname);
    }
}

const server = http.createServer((req, resp) => {
    const url = parse_url(req.url, true);

    console.log(`==> ${url.pathname}`);

    // Check the access token.
    if (url.pathname.startsWith("/api/") && url.query.access_token !== ACCESS_TOKEN) {
        console.log(403);
        resp.writeHead(403);
        resp.end();
        return;
    }

    if (req.method == "POST") {
        let payload = "";
        req.on("data", chunk => {
            payload += chunk;
        });

        req.on("end", () => {
            const [status, type, body] = handle_request(url, payload);
            resp.writeHead(status, { "Content-Type": type });
            resp.end(body);
        });
    } else {
        const [status, type, body] = handle_request(url, null);
        console.log(status);
        resp.writeHead(status, { "Content-Type": type });
        resp.end(body);
    }
});

const ACCESS_TOKEN = generate_access_token();

function main() {
    server.listen(7171);
    return ACCESS_TOKEN;
}

if (require.main==module) {
    main();
    const url = `http://localhost:7171/?access_token=${ACCESS_TOKEN}`;
    console.log(`nshrc-server: listening ${url}`);
    opn(url);
}

module.exports = { main };
