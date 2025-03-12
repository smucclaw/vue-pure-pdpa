import { describe, it, expect } from 'vitest';
import { encodeJsonQ, mkQ, ShouldView, And, Or, Simply } from '../Interview';
import { Ternary } from '../Ternary';
import { PreLabel, PrePostLabel } from '../Item';

describe('encodeJsonQ', () => {
  it('encodes a simple Q with Simply', () => {
    const q = mkQ(ShouldView.View, new Simply('test'), undefined, Ternary.True, []);
    const encoded = encodeJsonQ(q);
    expect(encoded).toEqual({
      shouldView: 'View',
      prePost: {},
      mark: { source: 'user', value: 'true' },
      andOr: {
        tag: 'Leaf',
        contents: 'test',
        nl: {}
      }
    });
  });

  it('encodes a Q with And and children', () => {
    const childQ = mkQ(ShouldView.Hide, new Simply('child'), undefined, Ternary.False, []);
    const q = mkQ(ShouldView.View, new And(), new PreLabel("All of:"), Ternary.True, [childQ]);
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
              nl: {}
            }
          }
        ],
        nl: {}
      }
    });
  });

  it('encodes a Q with Or and children', () => {
    const childQ1 = mkQ(ShouldView.Hide, new Simply('child1'), undefined, Ternary.False, []);
    const childQ2 = mkQ(ShouldView.Ask, new Simply('child2'), undefined, Ternary.True, []);
    const q = mkQ(ShouldView.View, new Or(), new PrePostLabel("Any of:", "Tally"), Ternary.True, [childQ1, childQ2]);
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
              nl: {}
            }
          },
          {
            shouldView: 'Ask',
            prePost: {},
            mark: { source: 'user', value: 'true' },
            andOr: {
              tag: 'Leaf',
              contents: 'child2',
              nl: {}
            }
          }
        ],
        nl: {}
      }
    });
  });
});