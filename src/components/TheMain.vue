<template>
  <BaseNavigation navClasses="is-dark is-fixed-top" fluidWidth>
    <template v-slot:brand>
      <div class="navbar-item">{{ appName }}</div>
    </template>
    <template v-slot:end>
      <span class="navbar-item" active-class="is-active">
        <button
          v-for="(lang, index) in langs"
          :key="index"
          @click="showLang(lang, 'LangQuestions')"
        >
          {{ langNames(lang) }}
        </button>
      </span>
    </template>
  </BaseNavigation>
  <main class="container is-fluid mt-8">
    <slot />
  </main>
  <BaseNavigation navClasses="is-dark is-fixed-bottom is-hidden-desktop">
    <template v-slot:brand>
      <router-link
        class="navbar-item is-expanded is-block has-text-centered"
        active-class="is-active"
        v-for="menu in navigationLinks"
        :key="menu.name"
        :to="menu.path"
      >
        <!-- <FontAwesomeIcon :icon="menu.meta.icon" /> -->
        <p class="is-size-7">{{ menu.name }}</p>
      </router-link>
    </template>
  </BaseNavigation>
</template>

<script>
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import BaseNavigation from "@/components/BaseNavigation.vue";

export default {
  name: "TheMain",
  components: {
    FontAwesomeIcon,
    BaseNavigation,
  },
  data() {
    return {
      navigationLinks: [],
      langs: this.$store.getters.langs,
      chosenLang: "",
      showOptions: new Array(this.$store.getters.langs.length).fill(false),
      selectedIndex: null,
    };
  },
  computed: {
    appName() {
      const name = import.meta.env.VUE_APP_NAME;
      const isEmpty = !name || name === "";

      return isEmpty ? "Dolora the Law Explorer" : name;
    },
  },
  methods: {
    langNames(l) {
      const fulllangs = {
        nl4chi: "Chinese",
        nl4eng: "English",
        nl4may: "Malay",
      };
      return fulllangs[l];
    },
    showLang(l, viz) {
      this.chosenLang = l;
      this.$store.commit("updateLang", this.chosenLang);
    },
  },
  beforeMount() {
    this.navigationLinks = this.$router.options.routes;
  },
};
</script>

<style>
.mt-8 {
  margin-top: 4rem !important;
  margin-bottom: 5rem !important;
}
</style>
