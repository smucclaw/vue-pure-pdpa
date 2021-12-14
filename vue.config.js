// vue.config.js
module.exports = {
  publicPath: process.env.NODE_ENV === 'production'
    ? '/vue-pure-pdpa/'
    : '/',
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
};
