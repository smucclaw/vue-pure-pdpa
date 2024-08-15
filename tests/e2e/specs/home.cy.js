describe('My First Test', () => {
  it('Visits the app root url', () => {
    cy.visit('http://localhost:8080/vue-pure-pdpa/');
    cy.get("[data-test='response-message']").contains(
      "It depends..."
    )
  });
});
