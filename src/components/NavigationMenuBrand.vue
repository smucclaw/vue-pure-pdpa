<template>
  <div class="navbar-brand">
    <slot></slot>
    <a
      class="navbar-burger"
      v-if="hasExtendedMenu"
      role="button"
      aria-label="menu"
      aria-expanded="false"
      @click="toggleNav"
      :class="showNav"
      >
      <span v-for="_ in 3" :key="_" aria-hidden="true"></span>
    </a>
  </div>
</template>

<script>
export default {
  props: {
    extendedMenu: Boolean,
  },
  emits: ['isExpanded'],
  data() {
    return {
      isNavActive: false,
      hasExtendedMenu: this.extendedMenu,
    };
  },
  computed: {
    showNav() {
      return this.isNavActive ? 'is-active' : '';
    },
  },
  methods: {
    toggleNav() {
      this.isNavActive = !this.isNavActive;
      this.$emit('isExpanded');
    },
  },
};
</script>
