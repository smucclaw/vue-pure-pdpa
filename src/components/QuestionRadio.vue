<template>
  <div class="control">
    <label
      class="button is-radio-button px-2"
      v-for="(opt, index) in btnOptions"
      :key="index"
      :class="getColor(opt.color, opt.value, selectedValue, disabled)"
      :checked="opt.value === selectedValue"
    >
      <input
        type="radio"
        v-model="selectedValue"
        :id="opt.value"
        :value="opt.value"
        :disabled="disabled"
      />
      <FontAwesomeIcon class="icon is-small m-0" :icon="opt.icon" />
      <span class="is-hidden-mobile ml-1">{{ opt.name }}</span>
    </label>
  </div>
</template>

<script>
import { FontAwesomeIcon } from "@fortawesome/vue-fontawesome";
import { faCheck, faQuestion, faTimes } from "@fortawesome/free-solid-svg-icons";

export default {
  name: "QuestionRadio",
  components: {
    FontAwesomeIcon,
  },
  // Max: added disabled
  props: {
    modelValue: String,
    disabled: Boolean,
  },
  emits: ["update:modelValue"],
  data() {
    return {
      btnOptions: [
        {
          name: "Yes",
          icon: faCheck,
          color: "is-success",
          value: "true",
        },
        {
          name: "No",
          icon: faTimes,
          color: "is-danger",
          value: "false",
        },
        {
          name: "Don't Know",
          icon: faQuestion,
          color: "is-dark",
          value: "undefined",
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
        this.$emit("update:modelValue", value);
      },
    },
  },
  methods: {
    getColor(currentColor, currentValue, selectedValue, disabled) {
      const isSelected = currentValue === selectedValue;
      const shouldShowActive = isSelected && !disabled ? "is-active" : "is-outlined";
      const disabledClass = disabled ? "is-disabled" : "";
      return currentColor.concat(" ", shouldShowActive, " ", disabledClass);
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
