<template>
<div class="box has-text-left">
  <div class="level">
    <div class="level-right has-text-right is-one-third">
      <em>{{q.shouldView}}</em> {{ verdict }}
    </div>
    <div class="level-left has-text-left" v-if='q.prePost.pre'>{{ q.prePost.pre }}:</div>
  </div>
  <div v-if='q.andOr.contents'>{{ q.andOr.contents }}: {{ q.andOr.nl.en }}</div>
   <ul v-else>
     <li v-for="child in q.andOr.children" v-bind:key="child">
       <Q v-bind:q='child' v-bind:depth=depth1 />
     </li>
   </ul>
  </div>
</template>

<!--   <Q v-bind:q='anyallform' v-bind:depth='depth' /> -->

<script>
export default {
  name: 'AnyAllForm',
  props: {
    q: Object,
    depth: Number,
  },
  computed: {
    depth1() { return this.depth + 1; },
    verdict() {
      const str = this.q.mark.value === 'undefined' ? 'unknown' : this.q.mark.value;
      return this.q.mark.source === 'user' ? str.toUpperCase() : str;
    },
  },
};
</script>
