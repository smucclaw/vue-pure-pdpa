export type Item =
  | LeafItem
  | AllItem
  | AnyItem
  | NotItem;

export interface LeafItem {
  type: 'Leaf';
  value: string;
}

export interface AllItem {
  type: 'All';
  label: Label;
  children: Item[];
}

export interface AnyItem {
  type: 'Any';
  label: Label;
  children: Item[];
}

export interface NotItem {
  type: 'Not';
  child: Item;
}

export type Label =
  | { type: 'Pre'; pre: string }
  | { type: 'PrePost'; pre: string; post: string };

// Helper functions to create items
export const createLeaf = (value: string): LeafItem => ({
  type: 'Leaf',
  value
});

export const createAll = (label: Label, children: Item[]): AllItem => ({
  type: 'All',
  label,
  children
});

export const createAny = (label: Label, children: Item[]): AnyItem => ({
  type: 'Any',
  label,
  children
});

export const createNot = (child: Item): NotItem => ({
  type: 'Not',
  child
});

export function nnf(item: Item): Item {
  switch (item.type) {
    case 'Leaf':
      return item;
    case 'All':
      return createAll(
        item.label,
        item.children.map(child => nnf(child))
      );
    case 'Any':
      return createAny(
        item.label,
        item.children.map(child => nnf(child))
      );
    case 'Not':
      if (item.child.type === 'Not') {
        return nnf(item.child.child);
      } else if (item.child.type === 'All') {
        return createAny(
          item.child.label,
          item.child.children.map(p => nnf(createNot(p)))
        );
      } else if (item.child.type === 'Any') {
        return createAll(
          item.child.label,
          item.child.children.map(p => nnf(createNot(p)))
        );
      }
      return item;
  }
}

export function deserializeItem(json: any): Item {
  switch (Object.keys(json)[0]) {
    case 'Leaf':
      return createLeaf(json.Leaf);
    case 'All':
      const allJson = json.All;
      return createAll(
        deserializeLabel(allJson.label),
        allJson.children.map(deserializeItem)
      );
    case 'Any':
      const anyJson = json.Any;
      return createAny(
        deserializeLabel(anyJson.label),
        anyJson.children.map(deserializeItem)
      );
    case 'Not':
      return createNot(deserializeItem(json.Not));
    default:
      throw new Error(`Unknown item: ${json}`);
  }
}

function deserializeLabel(json: any): Label {
  const keys = Object.keys(json);
  if (keys.length === 1 && keys.includes('Pre')) {
    return { type: 'Pre', pre: json.Pre };
  } else if (keys.length === 2 && keys.includes('Pre') && keys.includes('Post')) {
    return { type: 'PrePost', pre: json.Pre, post: json.Post };
  } else {
    throw new Error(`Unknown label type: ${json.type}`);
  }
}

export interface LabelViewModel {
  Pre?: string;
  Post?: string;
}

export function encodePrePostArgo(label?: Label): LabelViewModel {
  if (!label) {
    return {};
  }

  switch (label.type) {
    case 'Pre':
      return { "Pre": label.pre };
    case 'PrePost':
      return { "Pre": label.pre, "Post": label.post };
    default:
      return {};
  }
}