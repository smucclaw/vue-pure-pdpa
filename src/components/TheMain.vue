<template>
  <BaseNavigation navClasses="is-dark is-fixed-top" fluidWidth>
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
        <!-- <FontAwesomeIcon class="icon is-small mr-2" :icon="menu.meta.icon" /> -->
        <span>{{ menu.name }}</span>
      </router-link>
      <span class="navbar-item" active-class="is-active">
        <button v-for="(lang, index) in langs" :key="index" @click="toggleVizOptions(index)">
          {{ lang }}
          <span class="viz-options" v-show="selectedIndex === index">
            <button @click="showLang(lang, 'LangQuestions')">questions</button>
            <button @click="showLang(lang, 'Diagram')">diagram</button>
            <button @click="showLang(lang, 'Ladder')">ladder</button>
          </span>
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
      const name = process.env.VUE_APP_NAME;
      const isEmpty = !name || name === "";

      return isEmpty ? "Dolora the Law Explorer" : name;
    },
  },
  methods: {
    showLang(l, viz) {
      this.chosenLang = l;
      this.$store.commit("updateLang", this.chosenLang);
      console.log(this.chosenLang);
      this.$router.push({ name: viz, params: { lang: this.chosenLang } });
    },
    toggleVizOptions(index) {
      this.selectedIndex = index;
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
