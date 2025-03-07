import { Q, ShouldView } from "./Interview";
import { AllItem, AnyItem, Item, LeafItem, NotItem } from "./Item";
import { not3, Ternary } from "./Ternary";

export type Marking = Map<string, Ternary>;

/**
 * Paint a tree as View, Hide, or Ask, depending on the dispositivity of the current node and its children.
 */
export function relevant(marking: Marking, parentValue: Ternary, self: Item): Q {
  const selfValue = evaluate(marking, self);
  let initVis: ShouldView;

  if (parentValue !== Ternary.Unknown) {
    if (parentValue === selfValue) {
      initVis = ShouldView.View;
    } else {
      initVis = ShouldView.Hide;
    }
  } else if (evaluate(marking, self) !== Ternary.Unknown) {
    initVis = ShouldView.View;
  } else {
    initVis = ShouldView.Ask;
  }

  const paintedChildren = getChildren(self).map(child =>
    initVis !== ShouldView.Hide
      ? relevant(marking, selfValue, child)
      : ask2hide(relevant(marking, selfValue, child))
  );

  function makeQNode(itemNode: Item): Q {
    switch (itemNode.type) {
      case 'Leaf':
        return mkQ(initVis, 'Simply', itemNode.value, null, lookupMarking(itemNode.value, marking), []);
      case 'Not':
        return makeQNode(itemNode.value);
      case 'Any':
        return mkQ(ask2view(initVis), 'Or', null, itemNode.label, selfValue, paintedChildren);
      case 'All':
        return mkQ(ask2view(initVis), 'And', null, itemNode.label, selfValue, paintedChildren);
    }
  }

  // Convert to a QTree for output
  return makeQNode(self);
}

export function getChildren(item: Item): Array<Item> {
  if (item instanceof NotItem) {
    return [item.child];
  } else if (item instanceof AnyItem || item instanceof AllItem) {
    return item.children;
  }
  
  return [];
}

export function ask2hide(q: Q): Q {
  if (q.shouldView === ShouldView.Ask) {
    return { ...q, shouldView: ShouldView.Hide };
  }
  return q;
}

export function ask2view(view: ShouldView): ShouldView {
  if (view === ShouldView.Ask) {
    return ShouldView.View;
  }
  return view;
}

export function nlMapFn(word: string, nldict: Map<string, Map<string, string>>, nl: Map<string, Map<string, string>>): Map<string, string> {
  const langs = Array.from(nldict.keys());
  const result = new Map<string, string>();

  langs.forEach(lg => {
    const lgDict = nl.get(lg) || new Map<string, string>();
    const longtext = lgDict.get(word) || "";
    result.set(lg, longtext);
  });

  return result;
}

// Well, it depends on what values the children have, and that depends on whether we're assessing them in soft or hard mode.
export function evaluate(marking: Marking, item: Item): Ternary {
  switch (true) {
    case item instanceof LeafItem:
      return marking.get(item.value) || Ternary.Unknown;
    case item instanceof NotItem:
      return not3(evaluate(marking, item.child));
    case item instanceof AnyItem:
      return evaluateAny(item.children.map(child => evaluate(marking, child)));
    case item instanceof AllItem:
      return evaluateAll(item.children.map(child => evaluate(marking, child)));
    }
}

export function evaluateAny(items: Ternary[]): Ternary {
  if (items.includes(Ternary.True)) {
    return Ternary.True;
  } else if (items.every(item => item === Ternary.False)) {
    return Ternary.False;
  } else {
    return Ternary.Unknown;
  }
}

export function evaluateAll(items: Ternary[]): Ternary {
  if (items.every(item => item === Ternary.True)) {
    return Ternary.True;
  } else if (items.includes(Ternary.False)) {
    return Ternary.False;
  } else {
    return Ternary.Unknown;
  }
}

export function lookupMarking(node: string, marking: Marking): Ternary {
  return marking.get(node) || Ternary.Unknown;
}

// This function should be defined in your Types.ts
function mkQ(shouldView: ShouldView, nodeType: string, simpleValue: string | null,
  label: string | null, ternary: Ternary, children: Q[]): Q {
  return {
    shouldView,
    nodeType,
    simpleValue,
    label,
    ternary,
    children
  };
}