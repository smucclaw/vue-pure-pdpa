/* eslint-disable no-use-before-define */
/* eslint-disable no-unused-vars */
/* eslint-disable no-unused-vars */
<template>
  <!-- <form>
    <h1 v-if="qrootExample1.mark.value == 'true'" class="title">Yes!</h1>
    <h1 v-if="qrootExample1.mark.value == 'false'" class="title">No!</h1>
    <h1 v-if="qrootExample1.mark.value == 'undefined'" class="title">It depends...</h1>
  <Q v-bind:q='qrootExample1' v-bind:depth=0 />
  </form> -->
  <div>
    <svg id="tree">
      <!-- <g>
         <g class="links"></g>
         <g class="nodes"></g>
     </g> -->
    </svg>
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
    init(x) {
      console.log(x);
      console.log('check ', x.andOr.tag);
      // const clusterx = d3.cluster();

      // sort nodes by height and names

      let nodes = d3.hierarchy(x, (d) => d.andOr.children);
      console.log('hierarchy ', nodes);

      // const stratifyx = d3.stratify(x)
      //   .parentId((d) => d.id.substring(0, d.id.lastIndexOf('.')));
      // console.log('stratify ', stratifyx);

      // set the dimensions and margins of the diagram
      const margin = {
        top: 40, right: 90, bottom: 50, left: 90,
      };
      const width = 660 - margin.left - margin.right;
      const height = 500 - margin.top - margin.bottom;

      // declares a tree layout and assigns the size
      const treemap = d3.tree()
        .size([width, height]);

      //  assigns the data to a hierarchy using parent-child relationships

      // maps the node data to the tree layout
      nodes = treemap(nodes);

      // append the svg obgect to the body of the page
      // appends a 'group' element to 'svg'
      // moves the 'group' element to the top left margin
      const svg = d3.select('#tree')
        .attr('width', width + margin.left + margin.right)
        .attr('height', height + margin.top + margin.bottom);
      const g = svg.append('g')
        .attr('transform',
          `translate(${margin.left},${margin.top})`);

      console.log('descendants ', nodes.descendants());
      // adds the links between the nodes
      // eslint-disable-next-line no-unused-vars
      const link = g.selectAll('.link')
        .data(nodes.links())
        .enter().append('line')
        .attr('class', 'link')
        .attr('x1', (d) => d.source.x)
        .attr('y1', (d) => d.source.y)
        .attr('x2', (d) => d.target.x)
        .attr('y2', (d) => d.target.y)
        .attr('stroke', 'darkgray')
        .attr('stroke-width', 2);

      // add text to the links
      link.append('text')
        .attr('dy', '.35em')
        .attr('y', (d) => (d.children ? -20 : 20))
        .style('text-anchor', 'middle')
        .text((d) => console.log('line ', d));

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
        .text((d) => {
          if (d.data.andOr.tag === 'Leaf') {
            return d.data.andOr.contents;
          }
          return d.data.andOr.tag;

          // return d.data.andOr.contents;
        });
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
