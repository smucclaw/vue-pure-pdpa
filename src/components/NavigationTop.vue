<template>
  <nav class="navbar is-dark is-fixed-top">
    <div class="container is-fluid">
      <NavigationMenuBrand @is-expanded="toggleNav">
        <div class="navbar-item">{{ appName }}</div>
      </NavigationMenuBrand>
      <NavigationMenuMain :active="isNavActive">
        <template v-slot:end>
          <router-link
            class="navbar-item"
            active-class="is-active"
            v-for="menu in navigationLinks"
            :key="menu.name"
            :to="menu.path">
            <FontAwesomeIcon
              class="icon is-small mr-2"
              :icon="menu.meta.icon"
              />
            <span>{{ menu.name }}</span>
          </router-link>
        </template>
      </NavigationMenuMain>
    </div>
  </nav>
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import NavigationMenuBrand from '@/components/NavigationMenuBrand.vue';
import NavigationMenuMain from '@/components/NavigationMenuMain.vue';

export default {
  components: {
    FontAwesomeIcon,
    NavigationMenuBrand,
    NavigationMenuMain,
  },
  data() {
    return {
      isNavActive: false,
      appName: 'PDPA DBNO PoC -- draft',
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
