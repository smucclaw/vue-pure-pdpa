<template>
  <div id="ladderDiagram" style="font-size:1.5em;" ref="ladderHere" @ladderEvent="ladderEventHandler"></div>
  <div id="debuggery">
    <h2>debug: AnyAll source</h2>
    <pre>{{ JSON.stringify(asCircuit, null, 2) }}</pre>
  </div>
</template>

<script setup lang="ts">
import { ref, computed, onMounted, onUpdated } from 'vue';
import {interviewStore} from '@/store/index.js';
import { BoolVar, AllQuantifier, AnyQuantifier, LadderDiagram } from 'ladder-diagram';
import { storeToRefs } from 'pinia';
import { Ternary } from '@/model/Ternary';

const store = interviewStore();
const { getMarkingField } = storeToRefs(store)
const ladderHere = ref();

// when the user clicks on the form Yes/No/Don'tKnow, Vue natively
// handles that by updating the store and recomputing everything. But
// when the user clicks on a text node in the ladder-diagram, because the
// diagram isn't a proper Vue component but is HTML DOM generated by
// the LadderDiagram constructor, we have to do a bit of magic to toss
// events between the native HTML DOM onClick, and the Vue component
// event listener. And what does the Vue event listener do? It cycles
// the value of that particular element across Yes/No/Don'tKnow
// values, and writes the new value to the store as though the user
// had clicked in the form itself.

// so, one way to do it, if we follow the QuestionRadio example, is to define an emitter to update.
// but we're using Vuex, so let's try writing directly to the store, shall we?

function cycleUTF(currentState) {
  switch (currentState) {
    case Ternary.True:
      return Ternary.False;
    case Ternary.False:
      return Ternary.Unknown;
    default:
      return Ternary.True;
  }
}

function ladderEventHandler(e) {
  store.updateMarkingField(
    e.detail,
    cycleUTF(getMarkingField.value(e.detail))
  );
}

// recursively transform a QoutJS object from the store.questions into a
// Circuit object from LadderDiagram.
function q2circuit(q) {
  if (q.andOr.tag === 'Leaf') {
    const utf = (
      q.mark.value === 'undefined' ? 'U' :
      q.mark.value === 'true'      ? 'T' :
      q.mark.value === 'false'     ? 'F' :
      null
    );

    return new BoolVar(q.andOr.contents,
      false,
      q.mark.source === 'default' ? utf : null,
      q.mark.source === 'user'    ? utf : null,
    );
  }

  const Construct = (q.andOr.tag === 'All' ? AllQuantifier : AnyQuantifier);
  return new Construct(q.andOr.children.map((c) => q2circuit(c)));
}

// recursively add event lander to text nodes of the LadderDiagram object
function ladderdiagramInitEvents(ld) {
  const clickevent = (idx) => () => {
    ladderHere.value.dispatchEvent(
      new CustomEvent('ladderEvent', {
        bubbles: false,
        cancelable: true,
        // [TODO] replace this with id when we have that available.
        detail: ld.circuit.children[idx].text,
      }),
    );
  };
  switch (ld.graph_type) {
    case 'AllQuantifier':
      ld.dom_nodes.forEach((item, idx) => {
        const [domNode, diagramObject] = item;
        if (diagramObject) {
          ladderdiagramInitEvents(diagramObject);
        } else {
          domNode.addEventListener('click', clickevent(idx));
        }
      });
      break;
    case 'AnyQuantifier':
      ld.dom_nodes.forEach((item, idx) => {
        const [domNode, diagramObject] = item;
        if (diagramObject) {
          ladderdiagramInitEvents(diagramObject);
        } else {
          domNode.addEventListener('click', clickevent(idx));
        }
      });
      break;
    default:
      console.error(
        'ladderdiagramInitEvents: Invalid circuit type. '
        + 'Expected AllQuantifier | AnyQuantifier. Got '
        + `${ld.graph_type}`,
      );
  }
}

// mount the ladder diagram image into the template, using the ref to ladderHere
//
// https://vuejs.org/guide/essentials/computed.html#writable-computed says:
// don't make async requests or mutate the DOM inside a computed getter!
const asCircuit = computed(() => q2circuit(store.questions));
const ld = computed(() => new LadderDiagram(asCircuit.value));
onMounted(() => {
  ld.value.attach(ladderHere.value);
  ladderdiagramInitEvents(ld.value);
});
// update the ladder diagram every time the store updates
onUpdated(() => {
  ladderHere.value.removeChild(ladderHere.value.firstElementChild);
  ld.value.attach(ladderHere.value);
  ladderdiagramInitEvents(ld.value);
});

</script>

<style>
@import 'ladder-diagram/css/ladder.css';
</style>