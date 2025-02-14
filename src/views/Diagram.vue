<template>
  <TheQuestion v-slot="slotProps">
    <QuestionDiagram class="block" :question="slotProps.questions" />
  </TheQuestion>
</template>

<script setup>
import TheQuestion from '@/components/TheQuestion.vue';
import QuestionDiagram from '@/components/QuestionDiagram.vue';
</script>
