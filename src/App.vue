<template lang='pug'>
NavigationTop
main.container
  router-view
</template>

<script>
import NavigationTop from '@/components/NavigationTop.vue';

export default {
  components: {
    NavigationTop,
  },
};
</script>

<style lang="scss">
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  min-height: 100vh;
}
</style>
