<template>
  <TheMain>
    <router-view />
  </TheMain>
</template>

<script>
import TheMain from '@/components/TheMain.vue';

export default {
  components: {
    TheMain,
  },
};
</script>

<style>
:root {
  --vertical-container-padding-top: 50px;
  --vertical-container-padding-right: 20px;
  --vertical-container-padding-bottom: 50px;
  --vertical-container-padding-left: 20px;
  --vertical-container-border-width: 5px;
  --vertical-container-border-color: white;
  --vertical-container-hover-bg-color: lightgray;
}

#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
}

.vertical-container {
  padding-top: var(--vertical-container-padding-top);
  padding-right: var(--vertical-container-padding-right);
  padding-bottom: var(--vertical-container-padding-bottom);
  padding-left: var(--vertical-container-padding-left);
  cursor: pointer;
  border: var(--vertical-container-border-width) solid var(--vertical-container-border-color);
}

.vertical-container:hover {
  background-color: var(--vertical-container-hover-bg-color);
}
</style>
