import { createRouter, createWebHistory } from 'vue-router';
import {
  faQuestionCircle,
  faProjectDiagram,
} from '@fortawesome/free-solid-svg-icons';
import store from '@/store/index.js';


const routes = [
  {
    path: '/',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4eng/questions',
  },
  {
    path: '/nl4eng',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4eng/questions',
    beforeEnter: (to, from, next) => {
      store.commit('updateLang', 'nl4eng');
      next();
    },
  },
  {
    path: '/nl4chi',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4chi/questions',
    beforeEnter: (to, from, next) => {
      store.commit('updateLang', 'nl4chi');
      next();
    },
  },
  {
    path: '/nl4may',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4may/questions',
    beforeEnter: (to, from, next) => {
      store.commit('updateLang', 'nl4may');
      next();
    },
  },
  {
    path: '/nl4eng',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4eng/questions',
    beforeEnter: (to, from, next) => {
      // Call your desired function here
      store.commit('updateLang', 'nl4eng');
      next();
    },
  },
];

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes,
});

router.beforeEach(async (to, from) => {
  const name = process.env.VUE_APP_BROWSER_NAME;
  const isEmpty = !name || name === '';

  window.document.title = isEmpty ? 'Dolora the Law Explorer' : name;
});

export default router;
