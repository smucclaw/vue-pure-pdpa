import { describe, it, expect } from 'vitest'
import { userMark } from '../MarkDetails'
import { Ternary } from '../Ternary'

describe('MarkDetails', () => {
    it('should create user mark with true value', () => {
        const mark = userMark("true")
        expect(mark.source).toBe('user')
        expect(mark.value).toBe(Ternary.True)
    })

    it('should create user mark with false value', () => {
        const mark = userMark("false")
        expect(mark.source).toBe('user') 
        expect(mark.value).toBe(Ternary.False)
    })

    it('should create user mark with undefined value', () => {
        const mark = userMark("undefined")
        expect(mark.source).toBe('user') 
        expect(mark.value).toBe(Ternary.Unknown)
    })

})

