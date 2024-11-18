import { createRouter, createWebHistory } from 'vue-router';
import store from '@/store/index.js';

const hostMap = {
  "dev": "https://cclaw.legalese.com/port/8090/workdir",
  "prod": "https://prod.cclaw.legalese.com/port/8090/workdir",
  "local": "http://localhost:8090/workdir"
}

const routes = [
  {
    path: '/',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4eng/questions',
  },
  {
    path: '/index.html',
    component: () => import('@/views/Questions.vue'),
    alias: '/nl4eng/questions',
    beforeEnter: (to, from, next) => {
      const host = hostMap[to.query.hostCode] ?? `https://${to.query.hostCode}.dev.cclaw.legalese.com/workdir`;
      fetch(`${host}/${to.query.uuid}/${to.query.spreadsheetId}/${to.query.sheetId}/aajson/LATEST.json`)
        .then(response => response.json())
        .then(data => store.state.allInverviews = data);

      next();
    }
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
  history: createWebHistory(import.meta.env.BASE_URL),
  routes,
});

router.beforeEach(async (to, from) => {
  const name = import.meta.env.VUE_APP_BROWSER_NAME;
  const isEmpty = !name || name === '';

  window.document.title = isEmpty ? 'Dolora the Law Explorer' : name;
});

export default router;
