import { createRouter, createWebHistory } from 'vue-router';
import {
  faQuestionCircle,
  faProjectDiagram,
} from '@fortawesome/free-solid-svg-icons';

const routes = [
  {
    path: '/',
    name: 'Questions',
    component: () => import('@/views/Questions.vue'),
    meta: {
      icon: faQuestionCircle,
    },
    children: [
      {
        path: ':lang/question',
        name: 'LangQuestions',
        component: () => import('@/views/Questions.vue'),
      },
    ],
  },
  {
    path: '/:lang/diagram',
    name: 'Diagram',
    component: () => import('@/views/Diagram.vue'),
    meta: {
      icon: faProjectDiagram,
    },
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
