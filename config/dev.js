const Bundler = require('parcel-bundler');
const express = require('express');
const httpProxyMiddleware = require('http-proxy-middleware');
const { main } = require("./main");

const bundler = new Bundler('ui/index.html');
const app = express();
app.use("/api", httpProxyMiddleware({
    target: "http://127.0.0.1:7171",
    changeOrigin: true,
}));
app.use(bundler.middleware());

const access_token = main();
app.listen(7777);

console.log(`\n*** http://localhost:7777/?access_token=${access_token}`);
