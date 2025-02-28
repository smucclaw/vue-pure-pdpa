export abstract class Item {
  abstract type: string;
  abstract nnf(): Item;
}

export class LeafItem extends Item {
  readonly type = 'Leaf';
  constructor(public value: string) {
    super();
  }
  nnf(): Item {
    return this;
  }
}

export class AllItem extends Item {
  readonly type = 'All';
  constructor(public label: Label, public children: Item[]) {
    super();
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
  constructor(public item: Item) {
    super();
  }
  nnf(): Item {
    if (this.item instanceof NotItem) {
      return this.item.item.nnf();
    } else if (this.item instanceof AllItem) {
      return new AnyItem(
        this.item.label,
        this.item.children.map(p => new NotItem(p).nnf())
      );
    } else if (this.item instanceof AnyItem) {
      return new AllItem(
        this.item.label,
        this.item.children.map(p => new NotItem(p).nnf())
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