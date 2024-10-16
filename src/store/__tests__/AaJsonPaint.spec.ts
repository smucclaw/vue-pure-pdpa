import { describe, it, expect } from 'vitest'
import * as AaJson from '../../assets/Interview.json';


describe('AaJson', () => {
  it('All Langs', () => {
    expect(
      AaJson.default.map(x => Object.keys(x)[0])
    ).toEqual(
      ["nl4chi", "nl4eng", "nl4may"]
    );
  });

})