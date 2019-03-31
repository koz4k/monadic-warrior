import purs from "rollup-plugin-purs";

export default {
  input: "src/Main.purs",
  output: {
    file: "bundle.js",
    format: "cjs",
    sourcemap: true,
    name: 'Main',
  },
  plugins: [
    purs({
      optimizations: {
        removeDeadCode: false,
      }
    })
  ]
};
