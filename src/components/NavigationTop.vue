<template lang="pug">
nav.navbar.is-dark.mb-4
  .container
    .navbar-brand
      .navbar-item {{ appName }}
      a.navbar-burger(
        role='button',
        aria-label='menu',
        aria-expanded='false',
        @click='toggleNav',
        :class='showNav'
      )
        span(
          v-for='_ in 3',
          aria-hidden='true'
        )
    .navbar-menu(
      @click='toggleNav',
      :class='showNav'
    )
      .navbar-start
        router-link.navbar-item(
          v-for='menu in menuItems',
          active-class='is-active',
          :key='menu.name',
          :to='menu.path'
        )
          FontAwesomeIcon.mr-2(:icon='menu.meta.icon')
          span {{ menu.name }}
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';

export default {
  components: {
    FontAwesomeIcon,
  },
  data() {
    return {
      isNavActive: false,
      appName: 'Dolores',
      menuItems: [],
    };
  },
  computed: {
    showNav() {
      return this.isNavActive ? 'is-active' : '';
    },
  },
  beforeMount() {
    this.menuItems = this.$router.options.routes;
  },
  methods: {
    toggleNav() {
      this.isNavActive = !this.isNavActive;
    },
  },
};
</script>
