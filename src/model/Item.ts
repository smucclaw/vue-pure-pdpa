export abstract class Item {
  abstract type: string;
  abstract nnf(): Item;
}

export class LeafItem extends Item {
  readonly type = 'Leaf';
  constructor(public value: string) {
    super();
    this.value = value;
  }
  nnf(): Item {
    return this;
  }
}

export class AllItem extends Item {
  readonly type = 'All';
  constructor(public label: Label, public children: Item[]) {
    super();
    this.children = children;
  }
  nnf(): Item {
    return new AllItem(
      this.label,
      this.children.map(p => p.nnf())
    );
  }
}

export class AnyItem extends Item {
  readonly type = 'Any';
  constructor(public label: Label, public children: Item[]) {
    super();
    this.children = children;
  }
  nnf(): Item {
    return new AnyItem(
      this.label,
      this.children.map(p => p.nnf())
    );
  }
}

export class NotItem extends Item {
  readonly type = 'Not';
  readonly child: Item;
  constructor(public item: Item) {
    super();
    this.child = item;
  }
  nnf(): Item {
    if (this.child instanceof NotItem) {
      return this.child.child.nnf();
    } else if (this.child instanceof AllItem) {
      return new AnyItem(
        this.child.label,
        this.child.children.map(p => new NotItem(p).nnf())
      );
    } else if (this.child instanceof AnyItem) {
      return new AllItem(
        this.child.label,
        this.child.children.map(p => new NotItem(p).nnf())
      );
    }
    return this;
  }
}

export abstract class Label {
  abstract type: string;
}

export class PreLabel extends Label {
  readonly type = 'Pre';
  constructor(public pre: string) {
    super();
  }
}

export class PrePostLabel extends Label {
  readonly type = 'PrePost';
  constructor(public pre: string, public post: string) {
    super();
  }
}

export function deserializeItem(json: any): Item {
  switch (Object.keys(json)[0]) {
    case 'Leaf':
      return new LeafItem(json.Leaf);
    case 'All':
      const allJson = json.All;
      return new AllItem(deserializeLabel(allJson.label), allJson.children.map(deserializeItem));
    case 'Any':
      const anyJson = json.Any;
      return new AnyItem(deserializeLabel(anyJson.label), anyJson.children.map(deserializeItem));
    case 'Not':
      return new NotItem(deserializeItem(json.Not));
    default:
      throw new Error(`Unknown item: ${json}`);
  }
}

function deserializeLabel(json: any): Label {
  const keys = Object.keys(json);
  if (keys.length === 1 && keys.includes('Pre')) {
    return new PreLabel(json.Pre);
  } else if (keys.length === 2 && keys.includes('Pre') && keys.includes('Post')) {
    return new PrePostLabel(json.Pre, json.Post);
  } else {
    throw new Error(`Unknown label type: ${json.type}`);
  }
}