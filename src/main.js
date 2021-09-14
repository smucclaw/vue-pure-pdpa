import { createApp } from 'vue';
import { Parser } from 'jison';
import App from './App.vue';
import router from './router';
import store from './store';

/* TEST JISON PARSER CODE */

require('@/assets/main.scss');

createApp(App).use(store).use(router).mount('#app');

const grammar = {
  lex: {
    rules: [
      ['\\s+', '/* skip whitespace */'],
      ['[a-f0-9]+', "return 'HEX';"],
    ],
  },

  bnf: {
    hex_strings: ['hex_strings HEX',
      'HEX'],
  },
};

const parser = new Parser(grammar);

// generate source, ready to be written to disk
parser.generate();

// you can also use the parser directly from memory

console.log(parser.parse('adfe34bc e82a'));
// returns true
