const path = require("path")
const CopyWebpackPlugin = require("copy-webpack-plugin")
const UglifyJsPlugin = require("uglifyjs-webpack-plugin")

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
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: "babel-loader",
          options: {
            presets: ["@babel/preset-env"]
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
  },
  optimization: {
    minimizer: [
      new UglifyJsPlugin({
        uglifyOptions: {
          compress: {
            pure_funcs: [
              "F2",
              "F3",
              "F4",
              "F5",
              "F6",
              "F7",
              "F8",
              "F9",
              "A2",
              "A3",
              "A4",
              "A5",
              "A6",
              "A7",
              "A8",
              "A9"
            ],
            pure_getters: true,
            keep_fargs: false,
            unsafe_comps: true,
            // unsafe: true,
            passes: 3
          }
        }
      })
    ]
  },
  plugins: [new CopyWebpackPlugin([{ from: "./assets/audio/*.wav", to: "" }])]
})
