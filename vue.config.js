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
