// vue.config.js
const getPublicPath = () => {
  const baseUrl = process.env.BASE_URL;
  const isBaseUrlEmpty = !baseUrl || baseUrl === '';

  return isBaseUrlEmpty ? '/vue-pure-pdpa' : baseUrl;
};

module.exports = {
  publicPath: getPublicPath(),
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
