import React from 'react';
import { createRoot } from 'react-dom/client';
import AppLayout from './layouts/AppLayout';
import './styles/global.css'; // Global styles and theme variables

const rootEl = document.getElementById('root');
const root = createRoot(rootEl!);
root.render(<AppLayout />);