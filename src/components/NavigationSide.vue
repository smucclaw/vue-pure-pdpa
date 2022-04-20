<template>
  <aside class="menu has-text-left">
    <ul class="menu-list">
      <router-link
        custom
        v-slot="{ href, navigate, isActive }"
        v-for="menu in navigationLinks"
        :key="menu.name"
        :to="menu.path">
        <li>
          <a
            class="is-flex is-align-items-center"
            :href="href"
            :class="isActiveClass(isActive)"
            @click="navigate"
            >
            <FontAwesomeIcon
              class="icon mr-2"
              :icon="menu.meta.icon"
              />
            <span>{{ menu.name }}</span>
          </a>
        </li>
      </router-link>
    </ul>
  </aside>
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';

export default {
  name: 'NavigationSide',
  components: {
    FontAwesomeIcon,
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
    isActiveClass(isActive) {
      return isActive ? 'is-active' : '';
    },
  },
};
</script>
