<template>
  <div id="editor"></div>
</template>

<script>
import { EditorState, basicSetup } from '@codemirror/basic-setup';
import { EditorView, keymap } from '@codemirror/view';
import { javascript } from '@codemirror/lang-javascript';
import { indentWithTab } from '@codemirror/commands';

export default {
  mounted() {
    try {
      /* eslint-disable no-unused-vars */
      const editor = new EditorView({
        state: EditorState.create({
          doc: 'console.log("hello world!");',
          extensions: [
            basicSetup,
            keymap.of([indentWithTab]),
            javascript()],
        }),
        parent: document.querySelector('#editor'),
      });
      /* eslint-enable no-unused-vars */
    } catch (e) {
      console.log(e);
    }
  },
};
</script>

<style scoped lang="scss">
#editor {
  width: 500px;
  height: 500px;
  border: 1px solid black;
  text-align: left;
  overflow: scroll;
}
</style>
