import os
import threading
from http.server import SimpleHTTPRequestHandler, HTTPServer
from fastapi import FastAPI, HTTPException, Depends, Request
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from fastapi.security import OAuth2PasswordBearer
from dotenv import load_dotenv
import stripe
from supabase import create_client, Client
import uvicorn
import json

load_dotenv()

app = FastAPI()
oauth2_scheme = OAuth2PasswordBearer(tokenUrl="token")

app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:8080", "*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

stripe.api_key = os.getenv("STRIPE_SECRET_KEY")
supabase: Client = create_client(os.getenv("SUPABASE_URL"), os.getenv("SUPABASE_KEY"))

class PaymentRequest(BaseModel):
    token: str
    amount: int
    user_id: str

@app.post("/create-payment")
async def create_payment(payment: PaymentRequest):
    try:
        charge = stripe.Charge.create(
            amount=payment.amount,
            currency="usd",
            source=payment.token,
            description="Example charge",
            metadata={"user_id": payment.user_id}
        )
        user = supabase.table("users").select("*").eq("id", str(payment.user_id)).execute()
        if not user.data:
            result = supabase.table("users").insert({
                "id": str(payment.user_id),
                "payment_amount": payment.amount / 100
            }).execute()
            message = "Payment successful and new user created"
        else:
            result = supabase.table("users").update({
                "payment_amount": supabase.table("users").select("payment_amount").eq("id", str(payment.user_id)).execute().data[0]['payment_amount'] + payment.amount / 100
            }).eq("id", str(payment.user_id)).execute()
            message = "Payment successful and existing user updated"
        if result.data:
            return {"message": message, "charge_id": charge.id}
        else:
            return {"message": "Payment successful but database operation failed", "charge_id": charge.id}
    except stripe.error.StripeError as e:
        raise HTTPException(status_code=400, detail=str(e))
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"An error occurred: {str(e)}")

@app.post("/webhook")
async def stripe_webhook(request: Request):
    payload = await request.body()
    sig_header = request.headers.get('Stripe-Signature')
    try:
        event = stripe.Webhook.construct_event(
            payload, sig_header, os.getenv('STRIPE_WEBHOOK_SECRET')
        )
        if event['type'] == 'charge.succeeded':
            charge = event['data']['object']
            user_id = charge['metadata']['user_id']
            amount = charge['amount']
            result = supabase.table("users").update({"payment_amount": amount}).eq("id", user_id).execute()
            if result.data:
                print(f"Updated payment for user {user_id}")
            else:
                print(f"Failed to update payment for user {user_id}")
        return {"status": "success"}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

class User(BaseModel):
    email: str
    password: str

@app.post("/signup")
async def signup(user: User):
    try:
        response = supabase.auth.sign_up({
            "email": user.email,
            "password": user.password
        })
        return {"message": "User created successfully", "user": response.user}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/login")
async def login(user: User):
    try:
        response = supabase.auth.sign_in_with_password({
            "email": user.email,
            "password": user.password
        })
        return {"message": "Login successful", "session": response.session}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

@app.post("/logout")
async def logout():
    try:
        supabase.auth.sign_out()
        return {"message": "Logout successful"}
    except Exception as e:
        raise HTTPException(status_code=400, detail=str(e))

def get_current_user(token: str = Depends(oauth2_scheme)):
    try:
        user = supabase.auth.get_user(token)
        return user
    except Exception:
        raise HTTPException(status_code=401, detail="Invalid authentication credentials")

@app.get("/protected")
async def protected_route(user: dict = Depends(get_current_user)):
    return {"message": "This is a protected route", "user": user}

def run_fastapi():
    uvicorn.run(app, host="0.0.0.0", port=8000)

def run_file_server():
    handler = SimpleHTTPRequestHandler
    httpd = HTTPServer(("", 8080), handler)
    print("Serving files on port 8080 at http://localhost:8080")
    httpd.serve_forever()

if __name__ == "__main__":
    fastapi_thread = threading.Thread(target=run_fastapi)
    fastapi_thread.start()
    run_file_server()