<template>
  <TheQuestion v-slot="slotProps">
    <QuestionDiagram class="block" :question="slotProps.questions" />
  </TheQuestion>
</template>

<script setup lang="ts">
import TheQuestion from '@/components/TheQuestion.vue';
import QuestionDiagram from '@/components/QuestionDiagram.vue';
</script>
