<template lang='pug'>
div(class='home')
  HelloWorld(v-bind:msg='anyallform')
  h1(class='title') pur_str: {{pur_str}}
  button(class='button is-large') Bulma button
  AnyAllForm(v-bind:anyallform='anyallform')
</template>

<script>
// @ is an alias to /src
import { mapFields } from 'vuex-map-fields';
import HelloWorld from '@/components/HelloWorld.vue';
import AnyAllForm from '@/components/AnyAllForm.vue';

export default {
  name: 'Home',
  computed: {
    ...mapFields(['pur_str', 'anyallform']),
  },
  components: {
    HelloWorld,
    AnyAllForm,
  },
};
</script>
