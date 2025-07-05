/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ['./index.html', './src/**/*.{js,ts,jsx,tsx}'],
  theme: {
    extend: {
      colors: {
        primary: '#6366f1',
        dark: '#18181b',
        light: '#f4f4f5',
      },
    },
  },
  plugins: [],
}