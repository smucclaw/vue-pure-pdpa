import { createApp } from 'vue';
import { parser } from './grammars/calc.jison';
import App from './App.vue';
import router from './router';
import store from './store';

require('@/assets/main.scss');

createApp(App).use(store).use(router).mount('#app');

// TEST JISON CODE
parser.parse('1 + 1');
// console.log(str);
