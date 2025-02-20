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
        <p class="is-size-7">{{ menu.name }}</p>
      </router-link>
    </template>
  </BaseNavigation>
</template>

<script setup>
import { ref, computed, onBeforeMount } from 'vue'
import { useRouter, useStore } from 'vue-router'
import BaseNavigation from "@/components/BaseNavigation.vue"

const router = useRouter()
const store = useStore()

const navigationLinks = ref([])
const langs = store.getters.allLangs
const chosenLang = ref('')

const appName = computed(() => {
  const name = import.meta.env.VUE_APP_NAME
  const isEmpty = !name || name === ""
  return isEmpty ? "Dolora the Law Explorer" : name
})

const langNames = (l) => {
  const fulllangs = {
    nl4chi: "Chinese",
    nl4eng: "English",
    nl4may: "Malay",
  }
  return fulllangs[l]
}

const showLang = (l) => {
  chosenLang.value = l
  store.commit("updateLang", chosenLang.value)
}

onBeforeMount(() => {
  navigationLinks.value = router.options.routes
})
</script>

<style>
.mt-8 {
  margin-top: 4rem !important;
  margin-bottom: 5rem !important;
}
</style>
