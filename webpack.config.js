const path = require("path")

module.exports = (_env, args) => ({
  entry: path.resolve(__dirname, "index.js"),
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: /elm-stuff/,
        use: {
          loader: "elm-webpack-loader",
          options: {
            debug: args.mode === "development"
          }
        }
      },
      {
        test: /\.html$/,
        exclude: /node_modules/,
        use: {
          loader: "file-loader",
          options: {
            name: "[path][name].[ext]",
            context: ""
          }
        }
      }
    ]
  }
})
