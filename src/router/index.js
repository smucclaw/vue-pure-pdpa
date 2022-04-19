import { createRouter, createWebHistory } from 'vue-router';
import {
  faHome,
  faProjectDiagram,
} from '@fortawesome/free-solid-svg-icons';

const routes = [
  {
    path: '/',
    name: 'Home',
    component: () => import('@/views/Home.vue'),
    meta: {
      icon: faHome,
    },
  },
  {
    path: '/diagram',
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

export default router;
