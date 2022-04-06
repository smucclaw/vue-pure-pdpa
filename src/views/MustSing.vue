<!-- eslint-disable max-len -->
<template>
  <form>
    <h1 class="title">Must You Sing?</h1>
    <h1 v-if="qrootExample1.mark.value == 'true'" class="title">Yes!</h1>
    <h1 v-if="qrootExample1.mark.value == 'false'" class="title">No!</h1>
    <h1 v-if="qrootExample1.mark.value == 'undefined'" class="title">It depends...</h1>
    <Q v-bind:q='qrootExample1' v-bind:depth=0 />
  </form>
  <ul class="viz-container">
    <li class="viz-item"><HelloWorld v-bind:msg='qrootExample1' /></li>
    <li class="viz-item"><D3 v-bind:qroot='qrootExample1' /></li>
  </ul>
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Q from '@/components/Q.vue';
import HelloWorld from '@/components/HelloWorld.vue';
import D3 from '@/components/D3.vue';

export default {
  name: 'MustSing',
  props: {
  },
  computed: {
    ...mapFields(['marking', 'anyallform', 'formTitle']),
    ...mapGetters(['qrootExample1']),
  },
  components: {
    Q,
    HelloWorld,
    D3,
  },
};
</script>
<style scoped>
.viz-container {
  display: flex;
  flex-flow: row wrap;
  justify-content: space-around;
  padding: 0;
  margin: 0;
  list-style: none;
}

.viz-item {
  padding: 5px;
  width: 40vw;
  margin-top: 10px;
}
</style>
