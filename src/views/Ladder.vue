<template>
  <TheQuestion v-slot="slotProps">
    <LadderDiagram :question="slotProps.questions" />
  </TheQuestion>
</template>

<script setup lang="ts">
import TheQuestion from '@/components/TheQuestion.vue';
import LadderDiagram from '@/components/LadderDiagram.vue';
</script>
