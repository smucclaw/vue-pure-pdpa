import { createRouter, createWebHistory } from 'vue-router';
import Home from '../views/Home.vue';
import Editor from '../views/Editor.vue';
import MustSing from '../views/MustSing.vue';
import PDPA from '../views/PDPA.vue';
import D3 from '../views/D3.vue';

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
    path: '/editor',
    name: 'Editor',
    component: Editor,
  },
  {
    path: '/mustsing',
    name: 'MustSing',
    component: MustSing,
  },
  {
    path: '/d3',
    name: 'D3',
    component: D3,
  },
  {
    path: '/pdpa',
    name: 'PDPA',
    component: PDPA,
  },
];

const router = createRouter({
  history: createWebHistory(process.env.BASE_URL),
  routes,
});

export default router;
