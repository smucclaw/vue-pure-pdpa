<template>
  <BaseNavigation
    navClasses="is-dark is-fixed-top"
    fluidWidth
    >
    <template v-slot:brand>
      <div class="navbar-item">{{ appName }}</div>
    </template>
    <template v-slot:end>
      <router-link
        class="navbar-item"
        active-class="is-active"
        v-for="menu in navigationLinks"
        :key="menu.name"
        :to="menu.path"
        >
        <FontAwesomeIcon
          class="icon is-small mr-2"
          :icon="menu.meta.icon"
          />
        <span>{{ menu.name }}</span>
      </router-link>
    </template>
  </BaseNavigation>
  <main class="container is-fluid mt-8">
    <slot></slot>
  </main>
  <BaseNavigation
    navClasses="is-dark is-fixed-bottom is-hidden-desktop">
    <template v-slot:brand>
      <router-link
        class="navbar-item is-expanded is-block has-text-centered"
        active-class="is-active"
        v-for="menu in navigationLinks"
        :key="menu.name"
        :to="menu.path"
        >
        <FontAwesomeIcon :icon="menu.meta.icon" />
        <p class="is-size-7">{{ menu.name }}</p>
      </router-link>
    </template>
  </BaseNavigation>
  <!-- <div class="vertical-container">
    <p>aaa</p>
    <p>bbb</p>
    <p>ccc</p>
  </div> -->
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import BaseNavigation from '@/components/BaseNavigation.vue';

export default {
  name: 'TheMain',
  components: {
    FontAwesomeIcon,
    BaseNavigation,
  },
  data() {
    return {
      navigationLinks: [],
    };
  },
  computed: {
    appName() {
      const name = process.env.VUE_APP_NAME;
      const isEmpty = !name || name === '';

      return isEmpty ? 'Dolora the Law Explorer' : name;
    },
  },
  beforeMount() {
    this.navigationLinks = this.$router.options.routes;
  },
};
</script>

<style lang="scss">
.mt-8 {
  margin-top: 4rem !important;
  margin-bottom: 5rem !important;
}
</style>
