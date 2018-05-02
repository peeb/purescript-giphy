'use strict';

const path = require('path');

const webpack = require('webpack');

const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');

const isWatch = process.argv.some(arg => arg === '--watch');

const plugins =
  isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function (stats) {
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
;

module.exports = {
  devtool: 'cheap-module-inline-source-map',

  devServer: {
    contentBase: __dirname,
    port: 4008,
    stats: 'errors-only'
  },

  entry: './src/Main.purs',

  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [ 'src/**/*.purs' ],
              psc: 'psa',
              pscPackage: true,
              watch: isWebpackDevServer || isWatch
            }
          }
        ]
      },
    ]
  },

  resolve: {
    modules: [ 'node_modules' ],
    extensions: [ '.purs', '.js']
  },

  plugins: [
    new webpack.LoaderOptionsPlugin({
      debug: true
    })
  ].concat(plugins)
};
