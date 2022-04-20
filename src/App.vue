<template>
  <NavigationTop />
  <main class="container">
    <div class="columns">
      <div class="column is-one-quarter is-hidden-touch">
        <NavigationSide />
      </div>
      <div class="column">
        <router-view />
      </div>
    </div>
  </main>
  <NavigationBottom />
</template>

<script>
import NavigationTop from '@/components/NavigationTop.vue';
import NavigationSide from '@/components/NavigationSide.vue';
import NavigationBottom from '@/components/NavigationBottom.vue';

export default {
  components: {
    NavigationTop,
    NavigationSide,
    NavigationBottom,
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
