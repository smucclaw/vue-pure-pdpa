import { createRouter, createWebHistory } from 'vue-router';

const rtHome = () => import('@/views/Home.vue');

const routes = [
  {
    path: '/',
    name: 'Home',
    component: rtHome,
  },
];

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes,
});

export default router;
