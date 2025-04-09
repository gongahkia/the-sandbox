let stripe;
let cardElement;

async function initializeStripe() {
    const publishableKey = "pk_test_51Qt09NCAlKELFGdA1gRPAxAF1mKGUzsUC9w1XKs7hmru8gFe8UV3djZSHMh5pwyTtXM0x0rS8cegblFhn0scLgHZ00d2L02T7Z";
    stripe = Stripe(publishableKey);
    const elements = stripe.elements();
    cardElement = elements.create('card');
    cardElement.mount('#card-element');
}

initializeStripe();

const form = document.getElementById('payment-form');
const resultDiv = document.getElementById('payment-result');
const userIdInput = document.getElementById('user-id');

form.addEventListener('submit', async (event) => {
    event.preventDefault();
    if (!stripe) {
        resultDiv.textContent = "Stripe hasn't been initialized yet. Please try again.";
        return;
    }

    const userId = userIdInput.value;
    if (!userId) {
        resultDiv.textContent = "Please enter a user ID.";
        return;
    }

    try {
        const { token, error } = await stripe.createToken(cardElement);
        if (error) {
            throw new Error(error.message);
        }

        const response = await fetch('http://localhost:8000/create-payment', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                token: token.id,
                amount: 1000, // this is in cents btw
                user_id: userId
            }),
        });

        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(`HTTP error! status: ${response.status}, message: ${errorData.detail || 'Unknown error'}`);
        }

        const result = await response.json();
        resultDiv.textContent = result.message;
    } catch (e) {
        console.error("Payment processing error:", e);
        resultDiv.textContent = `An error occurred while processing your payment: ${e.message}`;
    }
});