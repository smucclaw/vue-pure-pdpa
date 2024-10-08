const fs = require('fs');

// vue.config.js
const getPublicPath = () => {
  const baseUrl = process.env.BASE_URL;
  const isBaseUrlEmpty = !baseUrl || baseUrl === '';

  return isBaseUrlEmpty ? '/vue-pure-pdpa' : baseUrl;
};

module.exports = {
  publicPath: getPublicPath(),
  lintOnSave: false,
  // the HTTPS logic is farther down in this file
  chainWebpack: (config) => {
    config.module
      .rule('vue')
      .use('vue-loader')
      .tap(options => {
        options['compilerOptions'] = { isCustomElement: (e) => ["clippath"].includes(e) }
        return options
      })

  },
}

// On the shared cclaw server, when v8k brings up vue instances on
// port 8xxx, we choose to HTTPS-enable those instances. Why? For good
// form; since about 2011, people have shouted "HTTPS Everywhere!" and
// if they found us serving the web apps without HTTPS, even in
// development mode, those people would get Internet-angry.
//
// So we set CCLAW_HTTPS in gunicorn.conf.py, and also in
// /etc/zsh/zshenv to be on the safe side. Why? In case vue gets `npm
// run serve`d outside of the gunicorn context; maybe somebody is
// running a vue instance by hand just to debug, and they want to
// simulate conditions as similar as possible to the "live" system.

// However, on our local servers, when we are doing dev on our laptops
// or whatever, we don't have SSL certs for our localhosts, so we turn
// off the HTTPS bits and expect to hit http://localhost:8888 or
// whatever. If you're on your laptop, do not set this environment variable.
module.exports.devServer = {
  allowedHosts: "all",
  historyApiFallback: true,
}

if (process.env.CCLAW_HTTPS) {
  module.exports.devServer.server = {
     type: 'https',
     options: {
     key:  '/etc/letsencrypt/live/cclaw.legalese.com/privkey.pem',
     cert: '/etc/letsencrypt/live/cclaw.legalese.com/cert.pem',
     },
    }
}

const wsProxyHostname = process.env.WS_PROXY_HOSTNAME ? process.env.WS_PROXY_HOSTNAME : 'cclaw.legalese.com';
const wsProxyProtocol = process.env.WS_PROXY_PROTOCOL ? process.env.WS_PROXY_PROTOCOL : 'wss';

if (process.env.LEGALSS_PROXY_PORT) {
  module.exports.devServer.client = {
    webSocketURL: `${wsProxyProtocol}://${wsProxyHostname}/port/${process.env.LEGALSS_PROXY_PORT}/ws`,
  };
}
