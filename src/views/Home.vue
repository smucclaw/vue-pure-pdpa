<template lang='pug'>
.columns
  .column
    img(alt='Vue logo' src='../assets/logo.png')
    HelloWorld(msg='Welcome to Your Vue.js App')
    h1.title {{pur_str}}
    button.button.is-large Bulma button
    Question
</template>

<script>
// @ is an alias to /src
import { mapFields } from 'vuex-map-fields';
import HelloWorld from '@/components/HelloWorld.vue';
import Question from '@/components/Question.vue';

export default {
  name: 'Home',
  computed: {
    ...mapFields(['pur_str']),
  },
  components: {
    HelloWorld,
    Question,
  },
};
</script>
