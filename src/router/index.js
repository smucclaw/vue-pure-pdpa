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
    alias: '/eng/questions',
  },
  {
    path: '/:lang',
    meta: {
      icon: faQuestionCircle,
    },
    children: [
      {
        path: '',
        name: 'LangQuestions',
        component: () => import('@/views/Questions.vue'),
      },
      {
        path: 'questions',
        name: 'LangQuestions',
        component: () => import('@/views/Questions.vue'),
      },
      {
        path: 'diagram',
        name: 'Diagram',
        component: () => import('@/views/Diagram.vue'),
      },
      {
        path: 'ladder',
        name: 'Ladder',
        component: () => import('@/views/Ladder.vue'),
      },
    ],
  },
];

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes,
});

router.beforeEach(() => {
  const name = process.env.VUE_APP_BROWSER_NAME;
  const isEmpty = !name || name === '';

  window.document.title = isEmpty ? 'Dolora the Law Explorer' : name;
});

export default router;
