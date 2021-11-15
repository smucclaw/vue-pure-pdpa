<template lang="pug">
label.is-check.mr-2(
  v-for='(opt, index) in btnOptions',
  :key='index',
  :class='displayChecked',
)
  input(
    type='radio',
    v-model='checked',
    :name='questionName',
    :id='opt.value',
    :value='opt.value',
    @change='onChange'
  )
  span {{ opt.name }}
</template>

<script>
export default {
  props: {
    questionName: {
      type: String,
    },
  },
  data() {
    return {
      // modelName: 'hello',
      btnOptions: [
        {
          name: 'Yes',
          value: 'true',
        },
        {
          name: 'No',
          value: 'false',
        },
        {
          name: 'Don\'t Know',
          value: 'undefined',
        },
      ],
      checked: false,
    };
  },
  methods: {
    onChange() {
      this.$emit('input', this.checked);
    },
  },
  computed: {
    displayChecked() {
      return this.checked ? 'is-primary' : '';
    },
  },
};
</script>
