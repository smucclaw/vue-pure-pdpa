<template>
  <TheMain>
    <router-view />
  </TheMain>
  <!-- <div class="vertical-container">
    <p>aaa</p>
    <p>bbb</p>
    <p>ccc</p>
  </div> -->
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
  // display: block;
  // display: flex;
  // flex-direction: column;
  // gap: 1rem;
  padding-top: 50px;
  padding-right: 20px;
  padding-bottom: 50px;
  padding-left: 20px;
  cursor: pointer;
  // border: 5px solid black;
  // margin: 20px 0px;
}
.vertical-container:hover {
  background-color: lightgray;
}
</style>
