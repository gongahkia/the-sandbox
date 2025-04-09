const searchInput = document.getElementById('search-input');
const searchButton = document.getElementById('search-button');
const resultsContainer = document.getElementById('results-container');
const bookDetailsContainer = document.getElementById('book-details-container');

searchButton.addEventListener('click', async () => {
  const query = searchInput.value;
  const results = await window.electronAPI.searchBooks(query);
  displayResults(results);
});

function displayResults(results) {
  resultsContainer.innerHTML = '';
  bookDetailsContainer.innerHTML = '';

  if (results.work) {
    results.work.forEach(book => {
      const bookElement = document.createElement('div');
      bookElement.classList.add('book-item');
      bookElement.innerHTML = `
        <h3>${book.titleweb}</h3>
        <p>Author: ${book.authorweb}</p>
        <button class="details-button" data-workid="${book.workid}">View Details</button>
      `;
      resultsContainer.appendChild(bookElement);
    });

    document.querySelectorAll('.details-button').forEach(button => {
      button.addEventListener('click', async (event) => {
        const workId = event.target.getAttribute('data-workid');
        const bookDetails = await window.electronAPI.getBookDetails(workId);
        displayBookDetails(bookDetails);
      });
    });
  } else {
    resultsContainer.innerHTML = '<p>No results found.</p>';
  }
}

function displayBookDetails(bookDetails) {
  bookDetailsContainer.innerHTML = `
    <h2>${bookDetails.titleweb}</h2>
    <p><strong>Author:</strong> ${bookDetails.authorweb}</p>
    <p><strong>On Sale Date:</strong> ${bookDetails.onsaledate}</p>
    <p><strong>ISBN:</strong> ${bookDetails.isbn}</p>
    <p><strong>Description:</strong> ${bookDetails.flapcopy || 'No description available.'}</p>
  `;
}