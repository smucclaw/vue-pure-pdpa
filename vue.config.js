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
  chainWebpack: (config) => {
    // Purescript Loader
    config.module
      .rule('purescript')
      .test(/\.purs$/)
      .use('purs-loader')
      .loader('purs-loader')
      .options({
        spago: true,
      });
    config.module
      .rule('jison')
      .test(/\.jison$/)
      .use('jison-gho-loader')
      .loader('jison-gho-loader');
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

if (process.env.CCLAW_HTTPS) {
  module.exports.devServer = {
    server: {
     type: 'https',
     options: {
     key:  '/etc/letsencrypt/live/cclaw.legalese.com/privkey.pem',
     cert: '/etc/letsencrypt/live/cclaw.legalese.com/cert.pem',
     },
    },
    allowedHosts: "all",
    historyApiFallback: true,
  }
}

