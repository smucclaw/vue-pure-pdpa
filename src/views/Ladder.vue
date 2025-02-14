<template>
  <TheQuestion v-slot="slotProps">
    <LadderDiagram :question="slotProps.questions" />
  </TheQuestion>
</template>

<script setup>
import TheQuestion from '@/components/TheQuestion.vue';
import LadderDiagram from '@/components/LadderDiagram.vue';
</script>
