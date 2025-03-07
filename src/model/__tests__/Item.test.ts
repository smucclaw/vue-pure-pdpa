import { describe, it, expect } from 'vitest'
import { LeafItem, AllItem, AnyItem, NotItem, PreLabel, PrePostLabel, nnf, deserializeItem, Label, Item } from '../Item'

describe('Item model', () => {
  it('should handle double negation', () => {
    const leaf = new LeafItem('test')
    const doubleNot = new NotItem(new NotItem(leaf))
    expect(doubleNot.nnf()).toEqual(leaf)
  })

  it('should convert NOT(ALL) to ANY(NOT)', () => {
    const label = new PreLabel('test')
    const child = new LeafItem('child')
    const all = new AllItem(label, [child])
    const notAll = new NotItem(all)

    const result = notAll.nnf()
    expect(result).toBeInstanceOf(AnyItem)
    expect((result as AnyItem).children[0]).toBeInstanceOf(NotItem)
  })

  it('should convert NOT(ANY) to ALL(NOT)', () => {
    const label = new PrePostLabel('pre', 'post')
    const child = new LeafItem('child')
    const any = new AnyItem(label, [child])
    const notAny = new NotItem(any)

    const result = notAny.nnf()
    expect(result).toBeInstanceOf(AllItem)
    expect((result as AllItem).children[0]).toBeInstanceOf(NotItem)
  })

  it('should preserve ALL structure while applying nnf to children', () => {
    const label = new PreLabel('test')
    const child1 = new NotItem(new LeafItem('child1'))
    const child2 = new LeafItem('child2')
    const all = new AllItem(label, [child1, child2])

    const result = all.nnf()
    expect(result).toBeInstanceOf(AllItem)
    expect((result as AllItem).children.length).toBe(2)
  })

  it('should preserve ANY structure while applying nnf to children', () => {
    const label = new PreLabel('test')
    const child1 = new NotItem(new LeafItem('child1'))
    const child2 = new LeafItem('child2')
    const any = new AnyItem(label, [child1, child2])

    const result = any.nnf()
    expect(result).toBeInstanceOf(AnyItem)
    expect((result as AnyItem).children.length).toBe(2)
  })

  it('should leave LeafItem unchanged', () => {
    const leaf = new LeafItem('test')
    expect(leaf.nnf()).toEqual(leaf)
  })

  it('should deserialize LeafItem', () => {
    const json = {
      "Leaf": "does the person walk?"
    }
    const result = deserializeItem(json)
    expect(result).toBeInstanceOf(LeafItem)
    expect((result as LeafItem).value).toBe('does the person walk?')
  })

  it('should deserialize AllItem with children', () => {
    const json = {
      "All": {
        "children": [
          {
            "Leaf": "does the person eat?"
          },
          {
            "Leaf": "does the person drink?"
          }
        ],
        "label": {
          "Pre": "any of:"
        }
      }
    }
    const result = deserializeItem(json)
    expect(result).toBeInstanceOf(AllItem)
    expect((result as AllItem).label).toBeInstanceOf(PreLabel)
    expect((result as AllItem).children[0]).toBeInstanceOf(LeafItem)
  })

  it('should deserialize AnyItem with children', () => {
    const json = {
      "Any": {
        "children": [
          {
            "Not": {"Leaf": "does the person eat?"}
          },
          {
            "Leaf": "does the person drink?"
          }
        ],
        "label": {
          "Post": "of personal data",
          "Pre": "is there any unauthorised"
        }
      }
    }
    const result = deserializeItem(json)
    expect(result).toBeInstanceOf(AnyItem)
    expect((result as AnyItem).label).toBeInstanceOf(PrePostLabel)
    expect((result as AnyItem).children[0]).toBeInstanceOf(Item)
  })

  it('should deserialize NotItem', () => {
    const json = {
      "Not": {
            "Leaf": "does the person eat?"
      }
    }
    const result = deserializeItem(json)
    expect(result).toBeInstanceOf(NotItem)
    expect((result as NotItem).child).toBeInstanceOf(LeafItem)
  })
})