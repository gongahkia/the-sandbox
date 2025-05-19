const express = require('express');
const { OpenPayments } = require('@interledger/open-payments');
require('dotenv').config();

const app = express();
app.use(express.json());
app.use(express.static('public'));

const op = new OpenPayments({
    walletAddress: process.env.WALLET_ADDRESS,
    privateKey: process.env.PRIVATE_KEY,
    authServer: process.env.AUTH_SERVER
});

app.post('/create-payment', async (req, res) => {
    try {
        const incomingPayment = await op.incomingPayments.create({
            amount: req.body.amount,
            assetCode: 'USD',
            expiresAt: new Date(Date.now() + 15 * 60 * 1000) 
        });
        
        res.json({
            url: incomingPayment.url,
            sharedSecret: incomingPayment.sharedSecret
        });
    } catch (error) {
        res.status(500).json({ error: error.message });
    }
});

app.post('/execute-payment', async (req, res) => {
    try {
        const payment = await op.outgoingPayments.create({
            incomingPaymentUrl: req.body.paymentUrl,
            amount: req.body.amount,
            assetCode: 'USD'
        });
        
        res.json({ success: true, id: payment.id });
    } catch (error) {
        res.status(500).json({ success: false, error: error.message });
    }
});

const PORT = process.env.PORT || 3000;
app.listen(PORT, () => console.log(`Server running on port ${PORT}`));