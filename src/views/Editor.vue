<template lang='pug'>
div(class='rows')
  div(id='editor')
  button(class='button is-primary is-light' v-on:click='eval_input') eval
  textarea(v-model='editor_str')
</template>

<script>
import { EditorState, basicSetup } from '@codemirror/basic-setup';
import { EditorView, keymap } from '@codemirror/view';
import { javascript } from '@codemirror/lang-javascript';
import { indentWithTab } from '@codemirror/commands';
import { mapFields } from 'vuex-map-fields';

const initStr = `console.log("hello world!");
const fib = (xs) => [...xs, xs[xs.length-1] + xs[xs.length -2]];
const repeat = (f, n, x) => n <= 0 ? x : repeat(f, n-1, f(x));
console.log(repeat(fib,10,[0,1]));`;
let editor;

export default {
  computed: {
    ...mapFields(['editor_str']),
  },
  mounted() {
    try {
      editor = new EditorView({
        state: EditorState.create({
          doc: initStr,
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
      this.$store.commit('reset_editor_str');
      const console = {};
      console.log = (x) => { this.$store.commit('append_editor_str', `${x}\n`); };
      try {
        /* eslint-disable no-eval */
        eval(editor.contentDOM.innerText);
        /* eslint-enable no-eval */
      } catch (e) {
        this.$store.commit('append_editor_str', `\n${e}`);
      }
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
