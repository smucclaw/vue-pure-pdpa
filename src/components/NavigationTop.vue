<template>
  <nav class="navbar is-dark mb-4">
    <div class="container">
      <NavigationMenuBrand extended-menu @is-expanded="toggleNav">
        <div class="navbar-item">{{ appName }}</div>
      </NavigationMenuBrand>
      <NavigationMenuMain :active="isNavActive">
        <template v-slot:start>
          <router-link
            class="navbar-item"
            active-class="is-active"
            v-for="menu in navigationLinks"
            :key="menu.name"
            :to="menu.path">
            <FontAwesomeIcon
              class="mr-2"
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
      appName: 'Dolores',
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
