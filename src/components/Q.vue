<template>
  <!-- eslint-disable max-len -->
  <div class="box has-text-left" :class="{ 'has-background-white': isShouldAsk
                                         , 'has-background-grey-lighter': isShouldHide
                                         , 'has-text-grey-light': isShouldHide
                                         , 'has-background-warning-light': bgAny
                                         , 'has-background-danger-light': bgAll
                                         }" >
    <div class="has-text-right" v-if="q.mark.value != 'undefined'">{{ q.mark.value }}</div>
    <div class="has-text-left" v-if='q.prePost.pre'>{{ q.prePost.pre }}: {{ q.andOr.nl.en }}</div>
    <div v-if='q.andOr.contents'><label>{{ q.andOr.contents }}</label>: {{ q.andOr.nl.en }}
      <div class="level" v-if='editable'>
        <div class="level-item has-text-centered">
          <input type='radio' v-bind:name='q.andOr.contents' v-model='leaf' id="true" value="true" />
          <p>yes</p>
          <p v-if='defaultTrue'>(default)</p>
        </div>
        <div class="level-item has-text-centered">
          <input type='radio' v-bind:name='q.andOr.contents' v-model='leaf' id="undefined" value="undefined" />
          <p>don't know</p>
          <p v-if='defaultUndefined'>(default)</p>
        </div>
        <div class="level-item has-text-centered">
          <input type='radio' v-bind:name='q.andOr.contents' v-model='leaf' id="false" value="false" />
          <p>no</p>
          <p v-if='defaultFalse'>(default)</p>
        </div>
      </div>
    </div>
    <ul v-else>
      <li v-for="child in q.andOr.children" v-bind:key="child">
        <Q :q='child' :depth=depth1 :parentTag='q.andOr.tag' :parentView='q.shouldView' />
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
    modelValue: Object,
    parentTag: String,
    parentView: String,
  },
  computed: {
    depth1() { return this.depth + 1; },
    bgAny() {
      return this.q.andOr.tag === 'Any'
        || (this.q.shouldView === 'View'
            && this.q.andOr.tag === 'Leaf'
            && this.parentTag === 'Any'
            && this.q.mark.source === 'user'
            && this.q.mark.value === 'true');
    },
    bgAll() {
      return this.q.andOr.tag === 'All'
        || (this.q.shouldView === 'View'
            && this.q.andOr.tag === 'Leaf'
            && this.parentTag === 'All'
            && this.q.mark.source === 'user'
            && this.q.mark.value === 'true');
    },
    isShouldAsk() { return this.q.shouldView === 'Ask'; },
    isShouldHide() {
      return this.q.shouldView === 'Hide'
        || (this.q.andOr.tag === 'Leaf'
            && this.q.shouldView === 'View'
            && this.parentView === 'Hide');
    },
    isShouldView() { return this.q.shouldView === 'View'; },
    verdict() {
      const str = this.q.mark.value === 'undefined' ? 'unknown' : this.q.mark.value;
      return this.q.mark.source === 'user' ? str.toUpperCase() : str;
    },
    editable() { return this.q.shouldView === 'Ask' || this.q.shouldView === 'View'; },
    defaultTrue() { return this.q.mark.source === 'default' && this.q.mark.value === 'true'; },
    defaultFalse() { return this.q.mark.source === 'default' && this.q.mark.value === 'false'; },
    defaultUndefined() { return this.q.mark.source === 'default' && this.q.mark.value === 'undefined'; },

    leaf: {
      get() {
        if (this.q.mark.source === 'user') { return this.q.mark.value; }
        return 'none';
      },
      set(value) {
        console.log(`radio clicked for ${this.q.andOr.contents}: ${value} ... emitting updateMarkingField`);
        this.$store.commit('updateMarkingField', {
          mField: this.q.andOr.contents,
          vValue: { source: 'user', value },
        });
      },
    },

  },
};
</script>
