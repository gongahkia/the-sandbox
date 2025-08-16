import { initializeApp } from "firebase/app";
import { getDatabase, ref, push, serverTimestamp } from "firebase/database";

const firebaseConfig = {
  apiKey: "XXX",
  authDomain: "XXX",
  projectId: "XXX",
  storageBucket: "XXX",
  messagingSenderId: "XXX",
  appId: "XXX",
  databaseURL: "XXX",
};

// Initialize Firebase
const app = initializeApp(firebaseConfig);

// Get a reference to the database service
const database = getDatabase(app);

// Get the form element
const contactForm = document.getElementById('contactForm');

// Add submit event listener to the form
contactForm.addEventListener('submit', (e) => {
    e.preventDefault();

    // Get form values
    const name = document.getElementById('name').value;
    const email = document.getElementById('email').value;
    const message = document.getElementById('message').value;

    // Push data to Firebase
    push(ref(database, 'contacts'), {
        name: name,
        email: email,
        message: message,
        timestamp: serverTimestamp()
    }).then(() => {
        alert('Message sent successfully!');
        contactForm.reset();
    }).catch((error) => {
        alert('Error sending message: ' + error.message);
    });
});