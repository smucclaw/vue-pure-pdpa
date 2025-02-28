import { describe, it, expect } from 'vitest'
import { Ternary, ternary2string, ternary2bool, ternaryFromString, not3 } from '../Ternary'

describe('Ternary', () => {
  describe('ternary2string', () => {
    it('should convert Ternary.True to "true"', () => {
      expect(ternary2string(Ternary.True)).toBe('true')
    })

    it('should convert Ternary.False to "false"', () => {
      expect(ternary2string(Ternary.False)).toBe('false')
    })

    it('should convert Ternary.Unknown to "undefined"', () => {
      expect(ternary2string(Ternary.Unknown)).toBe('undefined')
    })
  })

  describe('ternary2bool', () => {
    it('should convert Ternary.True to true', () => {
      expect(ternary2bool(Ternary.True)).toBe(true)
    })

    it('should convert Ternary.False to false', () => {
      expect(ternary2bool(Ternary.False)).toBe(false)
    })

    it('should convert Ternary.Unknown to undefined', () => {
      expect(ternary2bool(Ternary.Unknown)).toBe(undefined)
    })
  })

  describe('ternaryFromString', () => {
    it('should convert "true" to Ternary.True', () => {
      expect(ternaryFromString('true')).toBe(Ternary.True)
    })

    it('should convert "false" to Ternary.False', () => {
      expect(ternaryFromString('false')).toBe(Ternary.False)
    })

    it('should convert any other string to Ternary.Unknown', () => {
      expect(ternaryFromString('whatever')).toBe(Ternary.Unknown)
      expect(ternaryFromString('')).toBe(Ternary.Unknown)
    })
  })

  describe('not3', () => {
    it('should convert Ternary.True to Ternary.False', () => {
      expect(not3(Ternary.True)).toBe(Ternary.False)
    })

    it('should convert Ternary.False to Ternary.True', () => {
      expect(not3(Ternary.False)).toBe(Ternary.True)
    })

    it('should keep Ternary.Unknown as Ternary.Unknown', () => {
      expect(not3(Ternary.Unknown)).toBe(Ternary.Unknown)
    })
  })
})