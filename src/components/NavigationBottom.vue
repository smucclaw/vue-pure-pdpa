<template>
  <nav class="navbar is-fixed-bottom is-hidden-desktop is-dark">
    <div class="container">
      <NavigationMenuBrand>
        <router-link
          class="navbar-item is-expanded is-block has-text-centered"
          active-class="is-active"
          v-for="menu in navigationLinks"
          :key="menu.name"
          :to="menu.path">
          <FontAwesomeIcon :icon="menu.meta.icon" />
          <p class="is-size-7">{{ menu.name }}</p>
        </router-link>
      </NavigationMenuBrand>
    </div>
  </nav>
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import NavigationMenuBrand from '@/components/NavigationMenuBrand.vue';

export default {
  components: {
    FontAwesomeIcon,
    NavigationMenuBrand,
  },
  data() {
    return {
      isNavActive: false,
      navigationLinks: [],
    };
  },
  beforeMount() {
    this.navigationLinks = this.$router.options.routes;
  },
  methods: {
    toggleNav() {
      this.isNavActive = !this.isNavActive;
    },
  },
};
</script>
