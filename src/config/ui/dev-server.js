const Bundler = require('parcel-bundler');
const express = require('express');
const httpProxyMiddleware = require('http-proxy-middleware');

const bundler = new Bundler('src/index.html');
const app = express();
app.use("/api", httpProxyMiddleware({
    target: "http://localhost:7171",
    changeOrigin: true,
}));
app.use(bundler.middleware());

app.listen(27171);
