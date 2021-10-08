<template>
  <!-- <form>
    <h1 v-if="qrootExample1.mark.value == 'true'" class="title">Yes!</h1>
    <h1 v-if="qrootExample1.mark.value == 'false'" class="title">No!</h1>
    <h1 v-if="qrootExample1.mark.value == 'undefined'" class="title">It depends...</h1>
  <Q v-bind:q='qrootExample1' v-bind:depth=0 />
  </form> -->
  <div>
    <svg id="tree" />
  </div>
  -------
  <!-- <HelloWorld v-bind:msg='qrootExample1' /> -->
</template>

<script>
import { mapGetters } from 'vuex';
import { mapFields } from 'vuex-map-fields';
import * as d3 from 'd3';
// import { tree } from 'vued3tree';
// import Q from '@/components/Q.vue';
// import HelloWorld from '@/components/HelloWorld.vue';

export default {
  name: 'D3',
  data() {
    return {
      width: 800,
      height: 600,
      i: 0,
    };
  },
  methods: {
    addObject(target, obj) {
      return Object.assign(target, obj);
    },
    reformatdata(data) {
      const viewChild = (data.andOr.children.filter((child) => child.shouldView === 'View'))[0].andOr;
      const getObject = { ...viewChild };
      const newTree = data;
      const newChildren = data.andOr.children
        .filter((child) => child.shouldView === 'Ask');
      newChildren.map((leaf) => this.addObject(leaf.andOr, getObject));
      newTree.andOr.children = newChildren;
      return newTree;
    },
    init(data) {
      console.log('reformat ', this.reformatdata(data));

      // const cluster = d3.cluster();
      // console.log('cluster ', cluster);

      let nodes = d3.hierarchy(data, (d) => d.andOr.children);

      // set the dimensions and margins of the diagram
      const margin = {
        top: 40, right: 90, bottom: 50, left: 90,
      };
      const width = 660 - margin.left - margin.right;
      const height = 500 - margin.top - margin.bottom;

      // declares a tree layout and assigns the size
      const treemap = d3.tree()
        .size([width, height]);

      // maps the node data to the tree layout
      nodes = treemap(nodes);

      // appends a 'group' element to 'svg'
      // moves the 'group' element to the top left margin
      const svg = d3.select('#tree')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom);
      const g = svg.append('g')
        .attr('transform',
          `translate(${margin.left},${margin.top})`);

      // adds the links between the nodes
      const link = g.selectAll('.link')
        .data(nodes.links())
        .enter();

      function chooseColors(d, tags, color) {
        for (let i = 0; i < tags.length; i += 1) {
          if (d.data.andOr.tag === tags[i]) {
            return color[i];
          }
        }
        return 'black';
      }

      link.append('line')
        .attr('class', 'path')
        .attr('x1', (d) => d.source.x)
        .attr('y1', (d) => d.source.y)
        .attr('x2', (d) => d.target.x)
        .attr('y2', (d) => d.target.y)
        // .attr('stroke', 'red')
        .attr('stroke', (d) => chooseColors(d.source, ['All', 'Any'], ['red', 'blue']))
        .attr('stroke-width', 2);

      // add text to the links
      link.append('text')
        .attr('x', (d) => (d.source.x + d.target.x) / 2)
        .attr('y', (d) => (d.source.y + d.target.y) / 2)
        .attr('fill', (d) => chooseColors(d.source, ['All', 'Any'], ['red', 'blue']))
        .style('text-anchor', 'middle')
        .text((d) => d.source.data.andOr.tag);

      // adds each node as a group
      const node = g.selectAll('.node')
        .data(nodes.descendants())
        .enter().append('g')
        .attr('class', (d) => `node${
          d.children ? ' node--internal' : ' node--leaf'}`)
        .attr('transform', (d) => `translate(${d.x},${d.y})`);

      // adds the circle to the node
      node.append('circle')
        .attr('r', 10);

      // adds the text to the node
      node.append('text')
        .attr('dy', '.35em')
        .attr('y', (d) => (d.children ? -20 : 20))
        .style('text-anchor', 'middle')
        .text((d) => d.data.andOr.contents);
    },
  },
  computed: {
    ...mapFields(['marking', 'anyallform', 'formTitle']),
    ...mapGetters(['qrootExample1']),
  },
  components: {
    // Q,
    // HelloWorld,
  },
  mounted() {
    this.init(this.qrootExample1);
  },
};
</script>
