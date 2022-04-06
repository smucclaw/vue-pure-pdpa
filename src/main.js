import { createApp } from 'vue';
import FontAwesomeIcon from '@/utils/fontawesome';
import App from './App.vue';
import router from './router';
import store from './store';

require('@/assets/main.scss');

createApp(App)
  .component('font-awesome-icon', FontAwesomeIcon)
  .use(store)
  .use(router)
  .mount('#app');
