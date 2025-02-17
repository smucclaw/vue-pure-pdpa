<template>
  <nav class="navbar" :class="navClasses">
    <div class="container" :class="showFluidWidth">
      <div class="navbar-brand">
        <slot name="brand" />
        <a
          class="navbar-burger"
          v-if="hasExtendedMenu"
          role="button"
          aria-label="menu"
          aria-expanded="false"
          @click="toggleNav"
          :class="showNav"
        >
          <span v-for="_ in 3" :key="_" aria-hidden="true" />
        </a>
      </div>
      <div class="navbar-menu" :class="showNav">
        <div class="navbar-start" v-if="$slots.start">
          <slot name="start" />
        </div>
        <div class="navbar-end" v-if="$slots.end">
          <slot name="end" />
        </div>
      </div>
    </div>
  </nav>
</template>

<script setup>
import { ref, computed } from 'vue';

const props = defineProps({
  navClasses: String,
  extendedMenu: Boolean,
  fluidWidth: Boolean,
});

const isActive = ref(false);
const hasExtendedMenu = ref(props.extendedMenu);

const showNav = computed(() => isActive.value ? "is-active" : "");
const showFluidWidth = computed(() => props.fluidWidth ? "is-fluid" : "");

const toggleNav = () => {
  isActive.value = !isActive.value;
};
</script>
