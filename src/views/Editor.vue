<template>
  <div class="rows">
    <div id="editor"></div>
    <button class="button is-primary is-light" v-on:click="eval_input">eval</button>
    <textarea v-model="this.$store.state.editor_str"></textarea>
  </div>
</template>

<script>
import { EditorState, basicSetup } from '@codemirror/basic-setup';
import { EditorView, keymap } from '@codemirror/view';
import { javascript } from '@codemirror/lang-javascript';
import { indentWithTab } from '@codemirror/commands';

let editor;
export default {
  mounted() {
    try {
      editor = new EditorView({
        state: EditorState.create({
          doc: `
console.log("hello world!");
const fib = (xs) => [...xs, xs[xs.length-1] + xs[xs.length -2]];
const repeat = (f, n, x) => n <= 0 ? x : repeat(f, n-1, f(x));
console.log(repeat(fib,10,[0,1]));`.trim(),
          extensions: [
            basicSetup,
            keymap.of([indentWithTab]),
            javascript()],
        }),
        parent: document.querySelector('#editor'),
      });
    } catch (e) {
      console.log(e);
    }
  },
  methods: {
    eval_input() {
      this.$store.state.editor_str = '';
      const console = {};
      console.log = (x) => { this.$store.state.editor_str += `${x}\n`; };
      /* eslint-disable no-eval */
      try {
        eval(editor.contentDOM.innerText);
      } catch (e) {
        this.$store.state.editor_str += `${e}`;
      }
      /* eslint-enable no-eval */
    },
  },
};
</script>

<style scoped lang="scss">
#editor {
  width: 100%;
  height: auto;
  text-align: left;
  overflow: scroll;
}
.rows {
  flex-grow: 1;
  display: grid;
  height: 100%;
  grid-template-columns: 100%;
  grid-template-rows: 70% 5% 25%
}
textarea {
  height: 100%;
  font-family: monospace;
  border: none;
  padding: 1em;
}
</style>
