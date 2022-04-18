import { createRouter, createWebHistory } from 'vue-router';

const rtHome = () => import('@/views/Home.vue');
const rtDiagram = () => import('@/views/Diagram.vue');

const routes = [
  {
    path: '/',
    name: 'Home',
    component: rtHome,
  },
  {
    path: '/diagram',
    name: 'Diagram',
    component: rtDiagram,
  },
];

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes,
});

export default router;
