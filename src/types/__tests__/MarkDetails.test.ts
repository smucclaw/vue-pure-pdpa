import { describe, it, expect } from 'vitest'
import { userMark } from '../MarkDetails'

describe('MarkDetails', () => {
    it('should create user mark with true value', () => {
        const mark = userMark(true)
        expect(mark.source).toBe('user')
        expect(mark.value).toBe(true)
    })

    it('should create user mark with false value', () => {
        const mark = userMark(false)
        expect(mark.source).toBe('user') 
        expect(mark.value).toBe(false)
    })

})

