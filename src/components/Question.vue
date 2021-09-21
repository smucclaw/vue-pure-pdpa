<template lang="pug">
.box.has-text-left-tablet
  h1.title {{ title }}
  .buttons
    Button(v-for='choice in choices', :text='choice')
</template>

<script>
import Button from '@/components/Button.vue';

export default {
  name: 'Question',
  props: {
    title: String,
    choices: Array,
  },
  components: {
    Button,
  },
};
</script>

<style lang="scss">
.question {
  justify-content: flex-start !important;
}
</style>
