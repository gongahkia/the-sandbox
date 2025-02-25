module.exports = {
  chainWebpack: (config) => {
    config.module
      .rule('wasm')
      .test(/\.wasm$/)
      .use('wasm-loader')
      .loader('wasm-loader')
      .end();
  }
}