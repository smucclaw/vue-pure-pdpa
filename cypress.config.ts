import { defineConfig } from 'cypress'

export default defineConfig({
  fixturesFolder: 'tests/e2e/fixtures',
  screenshotsFolder: 'tests/e2e/screenshots',
  videosFolder: 'tests/e2e/videos',
  e2e: {
    supportFile: false,
    specPattern: 'tests/e2e/specs/**/*.cy.{js,jsx,ts,tsx}',
  },
})