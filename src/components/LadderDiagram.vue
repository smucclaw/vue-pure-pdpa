<template>
<div id="ladderDiagram" style="font-size:1.5em;" ref="ladderHere"></div>
<div id="debuggery">
  <h2>debug: AnyAll source</h2>
  <pre>{{ JSON.stringify(asCircuit,null,2) }}</pre>
</div>
</template>

<script setup>
  import { ref, computed, onMounted, onUpdated } from 'vue'
  import { useStore } from 'vuex'
import { BoolVar, AllQuantifier, AnyQuantifier, LadderDiagram } from 'ladder-diagram';

const props = defineProps({question: Object})

const store = useStore()

const asCircuit = computed(() => {
  // console.log(`LadderDiagram: computing asCircuit`)
  return q2circuit(store.getters.questions)
})

// recursively transform a QoutJS object from the store.questions into a Circuit object from LadderDiagram.
function q2circuit(q) {
  if (q.andOr.tag == "Leaf") {
    let utf = (q.mark.value == "undefined" ? 'U' :
               q.mark.value == "true"      ? 'T' :
               q.mark.value == "false"     ? 'F' :
               null)
    return new BoolVar(q.andOr.contents, // [TODO] nl.en || contents
                       false, // [TODO] this should depend on SimplyNot
                       q.mark.source == "default" ? utf : null,
                       q.mark.source == "user"    ? utf : null,
                      )
  }

  return (new
          (q.andOr.tag == "All" ? AllQuantifier : AnyQuantifier)
          (q.andOr.children.map((c) => q2circuit(c))))
}


// mount the ladder diagram image into the template, using the ref to ladderHere
const ld = computed(() => new LadderDiagram( 1.5, asCircuit.value, "Sides" ) )
const ladderHere = ref()
onMounted(() => { // console.log(`LadderDiagram: onMounted: appending LD element`);
                  ladderHere.value.appendChild(ld.value.dom_diagram) })
// update the ladder diagram every time the store updates
onUpdated(() => { // console.log(`LadderDiagram: onUpdated: resetting LD element`);
                  ladderHere.value.removeChild(ladderHere.value.firstElementChild);
                  ladderHere.value.appendChild(ld.value.dom_diagram) })

</script>

<style>
@import '~ladder-diagram/css/ladder.css';
</style>

<!--
  
// [TODO] this will come from the store
const sampleData = ref(
  new AllQuantifier([
    new AnyQuantifier([
      new BoolVar(    "Left  Null",    false, null, null  ),
      new BoolVar(    "Left  Nothing", false, 'U',  null  ),
      new BoolVar(    "Left  True",    false, 'T',  null  ),
      new BoolVar(    "Left  False",   false, 'F',  null  ),
      new BoolVar("Not Left  Null",    true,  null, null  ),
      new BoolVar("Not Left  Nothing", true,  'U',  null  ),
      new BoolVar("Not Left  False",   true,  'F',  null  ),
      new BoolVar("Not Left  True",    true,  'T',  null  ),
    ]),
    new AnyQuantifier([
      new BoolVar(    "Right Null",    false, null, null  ),
      new BoolVar(    "Right Nothing", false, null, 'U'   ),
      new BoolVar(    "Right True",    false, null, 'T'   ),
      new BoolVar(    "Right False",   false, null, 'F'   ),
      new BoolVar("Not Right Null",    true,  null, null  ),
      new BoolVar("Not Right Nothing", true,  null, 'U'   ),
      new BoolVar("Not Right False",   true,  null, 'F'   ),
      new BoolVar("Not Right True",    true,  null, 'T'   ),
    ])
  ])
)

-->
  
