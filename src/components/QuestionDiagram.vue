<template>
  <div class="card is-shadowless">
    <svg class="tree" id="tree">
      <!-- preserveAspectRatio="xMinYMin meet"> -->
      <clippath id="clip" />
    </svg>
  </div>
</template>

<script setup lang="ts">
import { ref, onMounted } from 'vue';
import * as d3 from 'd3';

const props = defineProps({
  question: Object,
});

const width = ref(window.innerWidth);

function init(data) {
  let nodes = d3.hierarchy(data, (d) => d.andOr.children);

  const margin = {
    top: 10,
    right: 15,
    bottom: 10,
    left: 100,
  };
  const widthValue = width.value - margin.left - margin.right;
  const height =
    nodes.children.length * (widthValue / nodes.children.length) - margin.top - margin.bottom;

  const treemap = d3.tree().size([widthValue, height]);

  nodes = treemap(nodes);
  let x0 = Infinity;
  let x1 = -x0;
  nodes.each((d) => {
    if (d.x > x1) x1 = d.x;
    if (d.x < x0) x0 = d.x;
  });

  const svg = d3
    .select('#tree')
    .attr('width', widthValue + margin.right + margin.left)
    .attr('height', height + margin.top + margin.bottom)
    .attr('viewBox', [0, 0, widthValue, height]);

  const g = svg
    .append('g')
    .attr('font-family', 'sans-serif')
    .attr('font-size', 10)
    .attr('transform', `translate(${nodes.x / (nodes.height + 1)},0)`);

  function zoomed({ transform }) {
    g.attr('transform', transform);
  }

  function dragStart() {
    d3.select(this).raise();
    g.attr('cursor', 'grab');
  }

  function dragDo(e) {
    const { x } = e;
    const { y } = e;
    d3.select(this).attr('x', x).attr('y', y);
  }

  function dragEnd() {
    g.attr('cursor', 'grab');
  }

  function drag() {
    svg.call(d3.drag().on('start', dragStart).on('drag', dragDo).on('end', dragEnd));
  }

  function zoom() {
    svg.call(
      d3
        .zoom()
        .extent([
          [0, 0],
          [widthValue, height],
        ])
        .scaleExtent([1, 8])
        .on('zoom', zoomed)
    );
  }

  zoom();
  drag();

  function chooseColors(d, tags, color) {
    for (let i = 0; i < tags.length; i += 1) {
      if (d.data.andOr.tag === tags[i]) {
        return color[i];
      }
    }
    return 'black';
  }

  const link = g
    .append('g')
    .attr('fill', 'none')
    .selectAll('path')
    .data(nodes.links())
    .join('path')
    .attr('stroke', (d) => chooseColors(d.source, ['All', 'Any'], ['red', 'blue']))
    .attr('stroke-width', 2)
    .attr(
      'd',
      d3
        .linkHorizontal()
        .x((d) => d.y)
        .y((d) => d.x)
    )
    .attr('id', (d, i) => `path${i + 1}`);

  link
    .append('textPath')
    .attr('x', (d) => (d.target.x - d.source.x) / 2)
    .attr('y', (d) => (d.target.y - d.source.y) / 2)
    .attr('fill', (d) => chooseColors(d.source, ['All', 'Any'], ['red', 'blue']))
    .style('text-anchor', 'middle')
    .attr('href', (d, i) => `#path${i + 1}`)
    .text((d) => d.source.data.andOr.tag);

  const node = g
    .append('g')
    .selectAll('g')
    .data(nodes.descendants())
    .join('g')
    .attr('transform', (d) => `translate(${d.y},${d.x})`);

  node
    .append('circle')
    .attr('fill', (d) => {
      if (d.parent) {
        return chooseColors(d.parent, ['All', 'Any'], ['red', 'blue']);
      }
      return 'black';
    })
    .attr('fill-opacity', 0.5)
    .attr('r', 10);

  function text(t) {
    if (['All', 'Any'].includes(t.data.andOr.tag)) {
      return t.data.andOr.tag;
    }
    return t.data.andOr.contents;
  }

  node
    .append('text')
    .attr('dy', '0.35em')
    .attr('y', (d) => (d.children ? -20 : 20))
    .style('text-anchor', 'middle')
    .text((d) => text(d));
}

onMounted(() => {
  init(props.question);
});
</script>

<style scoped>
.tree {
  overflow-x: scroll;
  width: 100%;
}
</style>
