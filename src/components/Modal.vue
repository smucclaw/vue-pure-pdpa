<template lang="pug">
.modal(:class='showActive')
  .modal-background
  .modal-content
    slot
  button.modal-close.is-large(aria-label='close', @click='closeModal')
</template>

<script>
export default {
  name: 'Modal',
  props: {
    active: Boolean,
  },
  data() {
    return {
      isActive: this.active,
    };
  },
  methods: {
    closeModal() {
      this.isActive = false;
    },
  },
  computed: {
    showActive() {
      return this.isActive ? 'is-active' : '';
    },
  },
};
</script>
