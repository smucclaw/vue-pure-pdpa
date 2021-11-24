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
    <li class="viz-item"><D3 v-bind:qroot='reformatdata' /></li>
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
    reformatdata() {
      const viewChild = (this.qrootExample1.andOr.children.filter((child) => child.shouldView === 'View'))[0].andOr;
      const getObject = { ...viewChild };
      const newTree = this.qrootExample1;
      const newChildren = this.qrootExample1.andOr.children
        .filter((child) => child.shouldView === 'Ask');
      newChildren.map((leaf) => Object.assign(leaf.andOr, getObject));
      newTree.andOr.children = newChildren;
      return newTree;
    },
  },
  components: {
    Q,
    HelloWorld,
    D3,
  },
};
</script>
