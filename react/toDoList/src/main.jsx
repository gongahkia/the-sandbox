import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App.jsx'

// renders everything within the root empty div of the index html file
ReactDOM.createRoot(document.getElementById('root')).render( 
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)