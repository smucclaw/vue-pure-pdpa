<template>
  <form>
    <h1 class="title">Must You Sing?</h1>
    <h1 v-if="qroot.mark.value == 'true'" class="title">Yes!</h1>
    <h1 v-if="qroot.mark.value == 'false'" class="title">No!</h1>
    <h1 v-if="qroot.mark.value == 'undefined'" class="title">It depends...</h1>
    <Q v-bind:q='qroot' v-bind:depth=0 />
  </form>
  <HelloWorld v-bind:msg='qroot' />
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import Q from '@/components/Q.vue';
import HelloWorld from '@/components/HelloWorld.vue';

export default {
  name: 'MustSing',
  props: {
  },
  computed: {
    ...mapFields(['marking', 'anyallform', 'formTitle']),
    ...mapGetters(['qroot']),
  },
  components: {
    Q,
    HelloWorld,
  },
};
</script>
