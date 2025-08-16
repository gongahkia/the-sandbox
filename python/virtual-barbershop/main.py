from backend import run_barbershop
from frontend import create_app

if __name__ == '__main__':
    shop = run_barbershop()
    app = create_app(shop)
    app.run_server(debug=True)
