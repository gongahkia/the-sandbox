import threading
import time
import random
import ollama
from collections import deque

class Barber:
    def __init__(self, name, skill_level):
        self.name = name
        self.skill_level = skill_level
        self.is_available = True

class Customer:
    def __init__(self, name, desired_service):
        self.name = name
        self.desired_service = desired_service
        self.satisfaction = 0

class Barbershop:
    def __init__(self):
        self.barbers = [
            Barber("John", 0.8),
            Barber("Emma", 0.9),
            Barber("Mike", 0.7)
        ]
        self.waiting_room = deque(maxlen=10)
        self.mutex = threading.Lock()
        self.customer_waiting = threading.Semaphore(0)
        self.barber_available = threading.Semaphore(len(self.barbers))
        self.conversations = []
        self.total_customers = 0
        self.satisfied_customers = 0

    def add_customer(self, customer):
        with self.mutex:
            if len(self.waiting_room) < self.waiting_room.maxlen:
                self.waiting_room.append(customer)
                self.customer_waiting.release()
                return True
            return False

    def barber_work(self, barber):
        while True:
            self.customer_waiting.acquire()
            with self.mutex:
                if self.waiting_room:
                    customer = self.waiting_room.popleft()
                    barber.is_available = False
            
            service_time = self.perform_service(barber, customer)
            time.sleep(service_time)
            
            with self.mutex:
                barber.is_available = True
                self.total_customers += 1
                if customer.satisfaction > 0.7:
                    self.satisfied_customers += 1
            self.barber_available.release()

    def perform_service(self, barber, customer):
        conversation = self.generate_conversation(barber, customer)
        self.conversations.append(conversation)
        
        service_quality = barber.skill_level * random.uniform(0.8, 1.0)
        customer.satisfaction = service_quality
        
        return random.uniform(10, 30)  # Service time in minutes

    def generate_conversation(self, barber, customer):
        prompt = f"Generate a short conversation between barber {barber.name} and customer {customer.name} about {customer.desired_service}."
        response = ollama.generate(model="llama2", prompt=prompt)
        return f"{barber.name} & {customer.name}: {response['response'].strip()}"

def run_barbershop():
    shop = Barbershop()
    
    for barber in shop.barbers:
        barber_thread = threading.Thread(target=shop.barber_work, args=(barber,))
        barber_thread.daemon = True
        barber_thread.start()

    def add_customers():
        services = ["haircut", "shave", "beard trim", "hair coloring"]
        while True:
            time.sleep(random.uniform(1, 5))
            customer = Customer(f"Customer{random.randint(1, 1000)}", random.choice(services))
            if not shop.add_customer(customer):
                print("Waiting room full. Customer left.")

    customer_thread = threading.Thread(target=add_customers)
    customer_thread.daemon = True
    customer_thread.start()

    return shop
