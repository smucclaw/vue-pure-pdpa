import { mkQ, Or, And, Q, ShouldView, Simply } from "./Interview";
import { not3, Ternary } from "./Ternary";
import { Item, createLeaf, createAll, createAny, createNot } from './Item';

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
        return mkQ(initVis, new Simply(itemNode.value), undefined, lookupMarking(itemNode.value, marking), []);
      case 'Not':
        return makeQNode(itemNode.child);
      case 'Any':
        return mkQ(ask2view(initVis), new Or(), itemNode.label, selfValue, paintedChildren);
      case 'All':
        return mkQ(ask2view(initVis), new And(), itemNode.label, selfValue, paintedChildren);
    }
  }

  // Convert to a QTree for output
  return makeQNode(self);
}

export function getChildren(item: Item): Item[] {
  switch (item.type) {
    case 'Leaf':
      return [];
    case 'Not':
      return getChildren(item.child);
    case 'Any':
    case 'All':
      return item.children;
  }
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
  switch (item.type) {
    case 'Leaf':
      return marking.get(item.value) || Ternary.Unknown;
    case 'Not':
      return not3(evaluate(marking, item.child));
    case 'Any':
      return evaluateAny(item.children.map(child => evaluate(marking, child)));
    case 'All':
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
