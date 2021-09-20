<template lang='pug'>
div(class='home')
  button(class='button is-large') Bulma button
  h1(class='title') pur_str: {{pur_str}}
  HelloWorld(v-bind:msg='anyallform')
</template>

<script>
// @ is an alias to /src
import { mapFields } from 'vuex-map-fields';
import HelloWorld from '@/components/HelloWorld.vue';

export default {
  name: 'Home',
  computed: {
    ...mapFields(['pur_str', 'anyallform']),
  },
  components: {
    HelloWorld,
  },
};
</script>
