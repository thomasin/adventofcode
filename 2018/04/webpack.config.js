const path = require("path")

const elmSource = path.resolve(__dirname, 'src/elm-app')

module.exports = {  
  entry: "./src/index.js",

  mode: "development",

  output: {
      filename: "bundle.js",
      path: path.resolve(__dirname, "src/static")
  },

  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: [/node_modules/],
        loader: 'babel-loader'
      },
      {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: {
              loader: "elm-webpack-loader?cwd=" + elmSource,
              options: {
                debug: true
              }
          }
      }
    ]
  },

  resolve: {
      extensions: [".js", ".elm"]
  }
}
