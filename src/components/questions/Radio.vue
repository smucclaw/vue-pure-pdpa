<template lang="pug">
label.is-check.mr-2(v-for='opt in btnOptions')
  input(type='checkbox')
  span {{ opt.name }}
</template>

<script>
export default {
  props: {
  },
  data() {
    return {
      // modelName: 'hello',
      btnOptions: [
        {
          name: 'Yes',
          value: true,
        },
        {
          name: 'No',
          value: false,
        },
        {
          name: 'Don\'t Know',
          value: undefined,
        },
      ],
    };
  },
  methods: {
    clicked(value) {
      this.$emit('input', value);
    },
  },
  computed: {
    displayChecked() {
      return 'is-primary';
    },
  },
};
</script>
