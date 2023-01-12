<template>
  <nav class="navbar" :class="navClasses">
    <div class="container" :class="showFluidWidth">
      <div class="navbar-brand">
        <slot name="brand" />
        <a
          class="navbar-burger"
          v-if="hasExtendedMenu"
          role="button"
          aria-label="menu"
          aria-expanded="false"
          @click="toggleNav"
          :class="showNav"
        >
          <span v-for="_ in 3" :key="_" aria-hidden="true" />
        </a>
      </div>
      <div class="navbar-menu" :class="showNav">
        <div class="navbar-start" v-if="$slots.start">
          <slot name="start" />
        </div>
        <div class="navbar-end" v-if="$slots.end">
          <slot name="end" />
        </div>
      </div>
    </div>
  </nav>
  <!-- <div class="vertical-container">
    <p>aaa</p>
    <p>bbb</p>
    <p>ccc</p>
  </div> -->
</template>

<script>
export default {
  props: {
    navClasses: String,
    extendedMenu: Boolean,
    fluidWidth: Boolean,
  },
  data() {
    return {
      isActive: false,
      hasExtendedMenu: this.extendedMenu,
    };
  },
  computed: {
    showNav() {
      return this.isActive ? 'is-active' : '';
    },
    showFluidWidth() {
      return this.fluidWidth ? 'is-fluid' : '';
    },
  },
  methods: {
    toggleNav() {
      this.isActive = !this.isActive;
    },
  },
};
</script>
