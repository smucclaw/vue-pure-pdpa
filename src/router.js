import Vue from 'vue'
import VueRouter from 'vue-router'

const routes = [
  {
    path: '/',
    component: null,
    props: {},
  },
];

export default function createRouter() {
  Vue.use(VueRouter)

  return new VueRouter({
    routes,
  });
}