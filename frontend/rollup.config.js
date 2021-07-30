import elm from 'rollup-plugin-elm';
import serve from 'rollup-plugin-serve';

const plugins = [ elm() ];
if (process.env.ROLLUP_WATCH) plugins.push(serve({
  contentBase: ['dist'],
  port: 8080
}));

export default {
  input: 'src/main.js',
  output: {
    file: 'dist/bundle.js',
    format: 'iife'
  },
  plugins: plugins
};