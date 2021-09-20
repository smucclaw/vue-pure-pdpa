<template>
  <form>
    <h1 class="title">You Must Sing If</h1>
    <Q v-bind:q='anyallform' v-bind:depth=0 />
  </form>
  <HelloWorld v-bind:msg='anyallform' />
</template>

<script>
import { mapFields } from 'vuex-map-fields';
import Q from '@/components/Q.vue';
import HelloWorld from '@/components/HelloWorld.vue';

export default {
  name: 'MustSing',
  props: {
  },
  computed: {
    ...mapFields(['anyallform', 'formTitle']),
  },
  components: {
    Q,
    HelloWorld,
  },
};
</script>
