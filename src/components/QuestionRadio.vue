<template>
  <div class="control">
    <label
      class="button is-radio-button px-2"
      v-for="(opt, index) in btnOptions"
      :key="index"
      :class="getColor(opt.color, opt.value, selectedValue)"
      :checked="opt.value === selectedValue"
      >
      <input
        type="radio"
        v-model="selectedValue"
        :id="opt.value"
        :value="opt.value"
        />
      <FontAwesomeIcon
        class="icon is-small m-0"
        :icon="opt.icon"
        />
     <span class="is-hidden-touch ml-1">{{ opt.name }}</span>
    </label>
  </div>
</template>

<script>
import { FontAwesomeIcon } from '@fortawesome/vue-fontawesome';
import {
  faCheck,
  faQuestion,
  faTimes,
} from '@fortawesome/free-solid-svg-icons';

export default {
  name: 'QuestionRadio',
  components: {
    FontAwesomeIcon,
  },
  props: {
    modelValue: String,
  },
  emits: ['update:modelValue'],
  data() {
    return {
      btnOptions: [
        {
          name: 'Yes',
          icon: faCheck,
          color: 'is-success',
          value: 'true',
        },
        {
          name: 'Don\'t Know',
          icon: faQuestion,
          color: 'is-info',
          value: 'undefined',
        },
        {
          name: 'No',
          icon: faTimes,
          color: 'is-danger',
          value: 'false',
        },
      ],
    };
  },
  computed: {
    selectedValue: {
      get() {
        return this.modelValue;
      },
      set(value) {
        this.$emit('update:modelValue', value);
      },
    },
  },
  methods: {
    getColor(currentColor, currentValue, selectedValue) {
      const isSelected = currentValue === selectedValue;
      const shouldShowActive = isSelected ? 'is-active' : 'is-outlined';
      return currentColor.concat(' ', shouldShowActive);
    },
  },
};
</script>

<style scoped>
.control > label {
  margin-right: 0.5rem !important;
}

.control > label:last-child {
  margin-right: 0 !important;
}
</style>
