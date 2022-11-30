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
  display: block;
  // display: flex;
  flex-direction: column;
  // gap: 1rem;
<<<<<<< HEAD
  padding-top: 50px;
  padding-bottom: 50px;
=======
  padding-left: 10px;
  padding-top: 50px;
  padding-bottom: 50px;
  border: 5px solid black;
  margin: 20px 0px;
>>>>>>> 60e95d6... implemented a test version of the multiple heads feature
}
// .vertical-container > * {
//   display: block;
// }
</style>
