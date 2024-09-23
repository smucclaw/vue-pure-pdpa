module.exports = {
  preset: '@vue/cli-plugin-unit-jest/presets/typescript',
  transform: {
    '^.+\\.vue$': 'vue3-jest',
  },
  moduleNameMapper: {
    '^@/(.*)$': '<rootDir>/src/$1',
    '^@ps/(.*)$': '<rootDir>/anyall-purs/output/$1'
  }
};
