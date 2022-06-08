<template>
  <NavigationBase
    navClasses="is-dark is-fixed-bottom is-hidden-desktop">
    <template v-slot:brand>
      <router-link
        class="navbar-item is-expanded is-block has-text-centered"
        active-class="is-active"
        v-for="menu in navigationLinks"
        :key="menu.name"
        :to="menu.path">
        <FontAwesomeIcon :icon="menu.meta.icon" />
        <p class="is-size-7">{{ menu.name }}</p>
      </router-link>
    </template>
  </NavigationBase>
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import NavigationBase from '@/components/NavigationBase.vue';

export default {
  components: {
    FontAwesomeIcon,
    NavigationBase,
  },
  data() {
    return {
      navigationLinks: [],
    };
  },
  beforeMount() {
    this.navigationLinks = this.$router.options.routes;
  },
};
</script>
