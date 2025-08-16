require('dotenv').config();
const express = require('express');
const axios = require('axios');
const app = express();
const PORT = 3000;

app.use(express.json());
app.use(express.static('../public'));

// Verify incoming requests from Telegram
app.post('/api', async (req, res) => {
  const { initData } = req.body;
  const botToken = process.env.BOT_TOKEN;
  
  try {
    const response = await axios.post(
      `https://api.telegram.org/bot${botToken}/getMe`
    );
    
    // Add your business logic here
    res.json({ status: 'success', data: response.data });
  } catch (error) {
    res.status(500).json({ error: 'API call failed' });
  }
});

app.listen(PORT, () => {
  console.log(`Server running on port ${PORT}`);
});