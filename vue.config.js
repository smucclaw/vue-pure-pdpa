// vue.config.js
module.exports = {
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
  },
};
