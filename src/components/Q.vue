<template>
  <!-- eslint-disable max-len -->
<div class="box has-text-left" :class="{ 'has-background-info-light': isShouldAsk, 'has-background-grey-light': isShouldHide }" >
  <div class="has-text-left" v-if='q.prePost.pre'>{{ q.prePost.pre }}:</div>
  <div v-if='q.andOr.contents'><label>{{ q.andOr.contents }}</label>: {{ q.andOr.nl.en }}
    <div class="level" v-if='editable'>
      <div class="level-item has-text-centered">
        <input type='radio' v-model='value' v-bind:value='radioValue' />
        <p>yes</p>
        <p v-if='defaultTrue'>(default)</p>
      </div>
      <div class="level-item has-text-centered">
        <input type='radio' v-model='value' v-bind:value='radioValue'/>
        <p>don't know</p>
        <p v-if='defaultUndefined'>(default)</p>
      </div>
      <div class="level-item has-text-centered">
        <input type='radio' v-model='value' v-bind:value='radioValue'/>
        <p>no</p>
        <p v-if='defaultFalse'>(default)</p>
      </div>
    </div>
  </div>
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
    modelValue: Object,
  },
  emits: ['update:modelValue'],
  computed: {
    depth1() { return this.depth + 1; },
    isShouldAsk() { return this.q.shouldView === 'Ask'; },
    isShouldHide() { return this.q.shouldView === 'Hide'; },
    verdict() {
      const str = this.q.mark.value === 'undefined' ? 'unknown' : this.q.mark.value;
      return this.q.mark.source === 'user' ? str.toUpperCase() : str;
    },
    editable() { return this.q.shouldView === 'Ask' || this.q.shouldView === 'View'; },
    defaultTrue() { return this.q.mark.source === 'default' && this.q.mark.value === 'true'; },
    defaultFalse() { return this.q.mark.source === 'default' && this.q.mark.value === 'false'; },
    defaultUndefined() { return this.q.mark.source === 'default' && this.q.mark.value === 'undefined'; },
    radioValue() {
      if (this.q.mark.source === 'user' && this.q.mark.value === 'true') return 'yes';
      if (this.q.mark.source === 'user' && this.q.mark.value === 'false') return 'no';
      if (this.q.mark.source === 'user') return 'unknown';
      return 'none';
    },
    value: {
      get() {
        return this.q.andOr.contents;
      },
      set(value) {
        this.$emit('update:modelValue', value);
      },
    },

  },
};
</script>
