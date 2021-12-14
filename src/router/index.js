import { createRouter, createWebHistory } from 'vue-router';
import Home from '../views/Home.vue';
import Questions from '../views/Questions.vue';
import Editor from '../views/Editor.vue';

const routes = [
  {
    path: '/',
    name: 'Home',
    component: Home,
  },
  {
    path: '/about',
    name: 'About',
    // route level code-splitting
    // this generates a separate chunk (about.[hash].js) for this route
    // which is lazy-loaded when the route is visited.
    component() {
      return import(/* webpackChunkName: "about" */ '../views/About.vue');
    },
  },
  {
    path: '/questions',
    name: 'Questions',
    component: Questions,
  },
  {
    path: '/editor',
    name: 'Editor',
    component: Editor,
  },
];

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes,
});

export default router;
