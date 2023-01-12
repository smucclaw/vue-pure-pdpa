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

<style lang="scss">
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
}
.vertical-container {
  padding-top: 50px;
  padding-right: 20px;
  padding-bottom: 50px;
  padding-left: 20px;
  cursor: pointer;
  border: 5px solid white;
}
.vertical-container:hover {
  background-color: lightgray;
}
</style>
