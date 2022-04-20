<template>
  <div class="navbar-menu" :class="showNav">
    <div class="navbar-start" v-if="$slots.start">
      <slot name="start"></slot>
    </div>
    <div class="navbar-end" v-if="$slots.end">
      <slot name="end"></slot>
    </div>
  </div>
</template>

<script>
export default {
  props: {
    active: Boolean,
  },
  computed: {
    showNav() {
      return this.active ? 'is-active' : '';
    },
  },
};
</script>
