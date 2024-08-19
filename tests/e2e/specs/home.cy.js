describe('My First Test', () => {
  it('Visits the app root url', () => {
    cy.visit('http://localhost:8080/vue-pure-pdpa/');
    cy.get("[data-test='response-message']").contains(
      'It depends...',
    );
  });

  it('Press <<does the person walk?>>', () => {
    cy.visit('http://localhost:8080/vue-pure-pdpa/');
    cy.get("[data-test='does the person walk?']").find('label').first().click();
    cy.get("[data-test='does the person eat?']").find('label').first().click();
    cy.get("[data-test='response-message']").contains(
      'Yes!',
    );
  });
});
