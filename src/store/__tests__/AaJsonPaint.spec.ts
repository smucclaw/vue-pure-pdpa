import { describe, it, expect } from 'vitest'
import * as AaJson from '../../assets/Interview.json';
import { aaJsonLangs, getAaJsonLins } from '../index.js'

const linear = {
  "nl4chi": [
    {
      "Person": {
        "All": {
          "children": [
            {
              "Leaf": "人 走 吗？"
            },
            {
              "Any": {
                "children": [
                  {
                    "Leaf": "人 吃 吗？"
                  },
                  {
                    "Leaf": "人 喝 吗？"
                  }
                ],
                "label": {
                  "Pre": "any of:"
                }
              }
            }
          ],
          "label": {
            "Pre": "all of:"
          }
        }
      }
    }
  ],
  "nl4eng": [
    {
      "Person": {
        "All": {
          "children": [
            {
              "Leaf": "does the person walk?"
            },
            {
              "Any": {
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
          ],
          "label": {
            "Pre": "all of:"
          }
        }
      }
    }
  ],
  "nl4may": [
    {
      "Person": {
        "All": {
          "children": [
            {
              "Leaf": "adakah seseorang berjalan?"
            },
            {
              "Any": {
                "children": [
                  {
                    "Leaf": "adakah seseorang makan?"
                  },
                  {
                    "Leaf": "adakah seseorang minum?"
                  }
                ],
                "label": {
                  "Pre": "any of:"
                }
              }
            }
          ],
          "label": {
            "Pre": "all of:"
          }
        }
      }
    }
  ]
}

describe('AaJson', () => {
  it('All Langs', () => {
    expect(
      aaJsonLangs(AaJson.default)
    ).toEqual(
      ["nl4chi", "nl4eng", "nl4may"]
    );
  });

  it('All Lin', () => {
    expect(
      getAaJsonLins(AaJson.default)
    ).toEqual(
      linear
    );
  });

})