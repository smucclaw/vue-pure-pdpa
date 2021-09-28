<template lang="pug">
nav.navbar.is-dark.mb-4
  .container
    .navbar-brand
      router-link.navbar-item(to='/') Dolores
      a.navbar-burger(
        role='button',
        aria-label='menu',
        aria-expanded='false',
        @click='toggleNav',
        :class='showNav'
      )
        span(aria-hidden='true')
        span(aria-hidden='true')
        span(aria-hidden='true')
    .navbar-menu(
      @click='toggleNav',
      :class='showNav'
    )
      .navbar-end
        router-link.navbar-item(to='/about') About
        router-link.navbar-item(to='/questions') Questions
        router-link.navbar-item(to='/editor') Editor
</template>

<script>
export default {
  data() {
    return {
      isNavActive: false,
    };
  },
  methods: {
    toggleNav() {
      this.isNavActive = !this.isNavActive;
    },
  },
  computed: {
    showNav() {
      return this.isNavActive ? 'is-active' : '';
    },
  },
};
</script>
