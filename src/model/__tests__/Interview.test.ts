import { describe, it, expect } from 'vitest';
import { encodeJsonQ, mkQ, ShouldView, Q } from '../Interview';
import { Ternary } from '../Ternary';

describe('encodeJsonQ', () => {
  it('encodes a simple Q with Simply', () => {
    const q: Q = mkQ(ShouldView.View, { type: 'Simply', value: 'test' }, undefined, Ternary.True, []);
    const encoded = encodeJsonQ(q);
    expect(encoded).toEqual({
      shouldView: 'View',
      prePost: {},
      mark: { source: 'user', value: 'true' },
      andOr: {
        tag: 'Leaf',
        contents: 'test',
      }
    });
  });

  it('encodes a Q with And and children', () => {
    const childQ: Q = mkQ(ShouldView.Hide, { type: 'Simply', value: 'child' }, undefined, Ternary.False, []);
    const q: Q = mkQ(ShouldView.View, { type: 'And' }, { type: 'Pre', pre: "All of:" }, Ternary.True, [childQ]);
    const encoded = encodeJsonQ(q);
    expect(encoded).toEqual({
      shouldView: 'View',
      prePost: { "Pre": "All of:" },
      mark: { source: 'user', value: 'true' },
      andOr: {
        tag: 'All',
        children: [
          {
            shouldView: 'Hide',
            prePost: {},
            mark: { source: 'user', value: 'false' },
            andOr: {
              tag: 'Leaf',
              contents: 'child',
            }
          }
        ],
      }
    });
  });

  it('encodes a Q with Or and children', () => {
    const childQ1: Q = mkQ(ShouldView.Hide, { type: 'Simply', value: 'child1' }, undefined, Ternary.False, []);
    const childQ2: Q = mkQ(ShouldView.Ask, { type: 'Simply', value: 'child2' }, undefined, Ternary.True, []);
    const q: Q = mkQ(ShouldView.View, { type: 'Or' }, { type: 'PrePost', pre: "Any of:", post: "Tally" }, Ternary.True, [childQ1, childQ2]);
    const encoded = encodeJsonQ(q);
    expect(encoded).toEqual({
      shouldView: 'View',
      prePost: {"Post": "Tally", "Pre": "Any of:"},
      mark: { source: 'user', value: 'true' },
      andOr: {
        tag: 'Any',
        children: [
          {
            shouldView: 'Hide',
            prePost: {},
            mark: { source: 'user', value: 'false' },
            andOr: {
              tag: 'Leaf',
              contents: 'child1',
            }
          },
          {
            shouldView: 'Ask',
            prePost: {},
            mark: { source: 'user', value: 'true' },
            andOr: {
              tag: 'Leaf',
              contents: 'child2',
            }
          }
        ],
      }
    });
  });
});