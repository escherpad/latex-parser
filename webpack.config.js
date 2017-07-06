const webpack = require("webpack");
const path = require("path");

const libraryName = "latex-parser";

const plugins = [
  new webpack.LoaderOptionsPlugin({
    options: {
      tslint: {
        emitErrors: true,
        failOnHint: true
      }
    }
  })
];

const minifiedFileName = `${libraryName}.min.js`;

console.log(minifiedFileName);

const config = {
  entry: {
    umd: `${__dirname}/src/main.ts`
  },
  devtool: "source-map",
  output: {
    filename: minifiedFileName,
    path: `${__dirname}/`,
    libraryTarget: "umd",
    library: libraryName
  },
  module: {
    rules: [
      {
          enforce: 'pre',
          test: /\.tsx?$/,
          loader: 'tslint-loader',
          exclude: /node_modules/
      },
      {
        test: /\.tsx?$/,
        loader: "awesome-typescript-loader",
        options: {
          configFileName: "tsconfig.webpack.json",
          useBabel: true
        },
        exclude: /node_modules/
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.ts', '.jsx', '.tsx']
  },

  plugins: plugins
};

module.exports = config;