import { describe, it, expect } from 'vitest'
import {
  createLeaf,
  createAll,
  createAny,
  createNot,
  nnf,
  deserializeItem,
  Label,
} from '../Item'

describe('Item model', () => {
  it('should handle double negation', () => {
    const leaf = createLeaf('test')
    const doubleNot = createNot(createNot(leaf))
    expect(nnf(doubleNot)).toEqual(leaf)
  })

  it('should convert NOT(ALL) to ANY(NOT)', () => {
    const label: Label = { type: 'Pre', pre: 'test' }
    const child = createLeaf('child')
    const all = createAll(label, [child])
    const notAll = createNot(all)

    const result = nnf(notAll)
    expect(result.type).toBe('Any')
    expect((result as any).children[0].type).toBe('Not')
  })

  it('should convert NOT(ANY) to ALL(NOT)', () => {
    const label: Label = { type: 'PrePost', pre: 'pre', post: 'post' }
    const child = createLeaf('child')
    const any = createAny(label, [child])
    const notAny = createNot(any)

    const result = nnf(notAny)
    expect(result.type).toBe('All')
    expect((result as any).children[0].type).toBe('Not')
  })

  it('should preserve ALL structure while applying nnf to children', () => {
    const label: Label = { type: 'Pre', pre: 'test' }
    const child1 = createNot(createLeaf('child1'))
    const child2 = createLeaf('child2')
    const all = createAll(label, [child1, child2])

    const result = nnf(all)
    expect(result.type).toBe('All')
    expect(result.children.length).toBe(2)
  })

  it('should preserve ANY structure while applying nnf to children', () => {
    const label: Label = { type: 'Pre', pre: 'test' }
    const child1 = createNot(createLeaf('child1'))
    const child2 = createLeaf('child2')
    const any = createAny(label, [child1, child2])

    const result = nnf(any)
    expect(result.type).toBe('Any')
    expect(result.children.length).toBe(2)
  })

  it('should leave LeafItem unchanged', () => {
    const leaf = createLeaf('test')
    expect(nnf(leaf)).toEqual(leaf)
  })

  it('should deserialize LeafItem', () => {
    const json = {
      "Leaf": "does the person walk?"
    }
    const result = deserializeItem(json)
    expect(result.type).toBe('Leaf')
    expect(result.value).toBe('does the person walk?')
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
    expect(result.type).toBe('All')
    expect(result.label).toEqual({ type: 'Pre', pre: 'any of:' })
    expect(result.children[0].type).toBe('Leaf')
  })

  it('should deserialize AnyItem with children', () => {
    const json = {
      "Any": {
        "children": [
          {
            "Not": { "Leaf": "does the person eat?" }
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
    expect(result.type).toBe('Any')
    expect(result.label).toEqual({ type: 'PrePost', pre: 'is there any unauthorised', post: 'of personal data' })
    expect(result.children[0].type).toBe('Not')
  })

  it('should deserialize NotItem', () => {
    const json = {
      "Not": {
        "Leaf": "does the person eat?"
      }
    }
    const result = deserializeItem(json)
    expect(result.type).toBe('Not')
    expect(result.child.type).toBe('Leaf')
  })
})