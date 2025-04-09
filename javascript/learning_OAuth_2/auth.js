const authConfig = {
    clientId: 'YOUR_CLIENT_ID', 
    authUrl: 'https://accounts.google.com/o/oauth2/v2/auth',
    tokenUrl: 'https://oauth2.googleapis.com/token',
    redirectUri: window.location.origin,
    scope: 'openid email profile',
    responseType: 'code'
};

function generateState() {
    const array = new Uint8Array(16);
    window.crypto.getRandomValues(array);
    return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('');
}

function authorize() {
    const state = generateState();
    localStorage.setItem('oauth_state', state);
    
    const authParams = new URLSearchParams({
        client_id: authConfig.clientId,
        redirect_uri: authConfig.redirectUri,
        response_type: authConfig.responseType,
        scope: authConfig.scope,
        state: state
    });
    
    window.location.href = `${authConfig.authUrl}?${authParams.toString()}`;
}

async function handleCallback() {
    const urlParams = new URLSearchParams(window.location.search);
    const code = urlParams.get('code');
    const state = urlParams.get('state');
    const storedState = localStorage.getItem('oauth_state');
    window.history.replaceState({}, document.title, window.location.pathname);
    if (!code) return;
    if (state !== storedState) {
        console.error('State validation failed');
        return;
    }
    
    try {
        const userData = {
            name: 'John Doe',
            email: 'john.doe@example.com',
            id: '12345'
        };
        
        localStorage.setItem('user', JSON.stringify(userData));
        localStorage.removeItem('oauth_state');
        
        updateAuthUI(true);
    } catch (error) {
        console.error('Authentication error:', error);
    }
}

function updateAuthUI(isLoggedIn) {
    const loginBtn = document.getElementById('login-btn');
    const userInfo = document.getElementById('user-info');
    const username = document.getElementById('username');
    const appContent = document.getElementById('app-content');
    const loginMessage = document.getElementById('login-message');
    
    if (isLoggedIn) {
        const user = JSON.parse(localStorage.getItem('user'));
        loginBtn.style.display = 'none';
        userInfo.style.display = 'flex';
        username.textContent = user.name;
        appContent.style.display = 'grid';
        loginMessage.style.display = 'none';
    } else {
        loginBtn.style.display = 'block';
        userInfo.style.display = 'none';
        appContent.style.display = 'none';
        loginMessage.style.display = 'block';
    }
}

function logout() {
    localStorage.removeItem('user');
    localStorage.removeItem('expenses');
    updateAuthUI(false);
    renderExpenses();
}

function checkAuthStatus() {
    const user = localStorage.getItem('user');
    updateAuthUI(!!user);
    if (window.location.search.includes('code=')) {
        handleCallback();
    }
}

document.addEventListener('DOMContentLoaded', checkAuthStatus);