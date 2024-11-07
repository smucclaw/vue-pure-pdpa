<template>
  <div class="notification" :class="themeColor">
    <button class="delete" :class="isCloseButtonVisible" />
    <slot>Content</slot>
  </div>
</template>

<script setup>
import { computed } from "vue";

const props = defineProps({
  hasCloseButton: Boolean,
  themeColor: String,
  content: String,
})

const isCloseButtonVisible = computed(() => {
  return props.hasCloseButton ? '' : 'is-hidden';
})

</script>
