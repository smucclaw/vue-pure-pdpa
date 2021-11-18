<!-- eslint-disable max-len -->
<template>
  <form>
    <h1 class="title">Must You Notify?</h1>
    <h1 v-if="qrootPDPA.mark.value == 'true'" class="title">Yes!</h1>
    <h1 v-if="qrootPDPA.mark.value == 'false'" class="title">No!</h1>
    <h1 v-if="qrootPDPA.mark.value == 'undefined'" class="title">It depends...</h1>
    <Q v-bind:q='qrootPDPA' v-bind:depth=0 />
  </form>
  <ul class="viz-container">
    <li class="viz-item"><HelloWorld v-bind:msg='qrootPDPA' /></li>
    <li class="viz-item"><D3 v-bind:qroot='qrootPDPA' /></li>
  </ul>
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Q from '@/components/Q.vue';
<<<<<<< HEAD
import HelloWorld from '@/components/HelloWorld.vue';
import D3 from '@/components/D3.vue';
=======
// import HelloWorld from '@/components/HelloWorld.vue';
>>>>>>> tweak publicPath, set PDPA to be the "home"

export default {
  name: 'PDPA',
  props: {
  },
  computed: {
    ...mapFields(['marking', 'anyallform', 'formTitle']),
    ...mapGetters(['qrootPDPA']),
  },
  components: {
    Q,
<<<<<<< HEAD
    HelloWorld,
    D3,
=======
    //    HelloWorld,
>>>>>>> tweak publicPath, set PDPA to be the "home"
  },
};
</script>
