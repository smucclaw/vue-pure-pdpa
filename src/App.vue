<template>
  <NavigationTop />
  <main class="container is-fluid mt-8">
    <router-view />
    <!--<div class="columns">
      <NavigationSide class="column is-one-quarter is-hidden-touch" />
    </div>-->
  </main>
  <NavigationBottom />
</template>

<script>
import NavigationTop from '@/components/NavigationTop.vue';
// import NavigationSide from '@/components/NavigationSide.vue';
import NavigationBottom from '@/components/NavigationBottom.vue';

export default {
  components: {
    NavigationTop,
    // NavigationSide,
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
}

.mt-8 {
  margin-top: 4rem !important;
  margin-bottom: 5rem !important;
}
</style>
