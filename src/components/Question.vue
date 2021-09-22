<template lang="pug">
.box.has-text-left-tablet
  h1.title {{ title }}
  //-.buttons
    Button(
      v-for='choice in choices',
      :text='choice',
      @selected='getSelected'
    )
  .field
    Checkbox(
      v-for='choice in choices',
      :text='choice'
    )
</template>

<script>
import Button from '@/components/Button.vue';
import Checkbox from '@/components/form/Checkbox.vue';

export default {
  name: 'Question',
  props: {
    title: String,
    choices: Array,
  },
  components: {
    Button,
    Checkbox,
  },
  data() {
    return {
      isSelected: false,
    };
  },
  methods: {
    getSelected(e) {
      this.isSelected = !this.isSelected;
      console.log(e, this.isSelected);
    },
  },
  computed: {
  },
};
</script>

<style lang="scss">
</style>
