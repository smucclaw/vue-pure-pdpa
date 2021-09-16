<template lang='pug'>
div(class='home')
  img(alt='Vue logo' src='../assets/logo.png')
  HelloWorld(msg='Welcome to Your Vue.js App')
  h1(class='title') {{pur_str}}
  button(class='button is-large') Bulma button
</template>

<script>
// @ is an alias to /src
import { mapFields } from 'vuex-map-fields';
import HelloWorld from '@/components/HelloWorld.vue';

export default {
  name: 'Home',
  computed: {
    ...mapFields(['pur_str']),
  },
  components: {
    HelloWorld,
  },
};
</script>
