<template lang="pug">
nav.navbar.is-dark.mb-4
  .container
    .navbar-brand
      router-link.navbar-item(to='/') {{ appName }}
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
          :to='menu.link'
        ) {{ menu.name }}
</template>

<script>
export default {
  data() {
    return {
      isNavActive: false,
      appName: 'Dolores',
      menuItems: [
        {
          name: 'Diagram',
          link: '/diagram',
        },
        /* {
          name: 'About',
          link: '/about',
        }, */
      ],
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
