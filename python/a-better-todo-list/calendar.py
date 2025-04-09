from datetime import date, datetime
from todo import *
import time
import os

# DISPLAY FOR CURRENT DATE AND TIME

temp_date = date.today().strftime("%d.%m.%y").split(".")
temp_date[1] = abs(int(temp_date[1]))
current_day, current_month, current_year = int(temp_date[0]), int(temp_date[1]), int(temp_date[2])
current_date = f"{temp_date[0]}.{temp_date[1]}.{temp_date[2]}"
current_time = datetime.now().strftime("%H:%M:%S")

################################

def calendar():

    # actual program

    print("\n\n\n\n\n|\t\t\tWelcome to Bullet Calendar V1.0\t\t\t|\n   ~For any feedback and pull requests, refer to @gongahkia on Github~\n\n\n\n\n")
    time.sleep(2)
    print("loading...")
    time.sleep(2)

    # LOAD DEADLINES FROM TODO LIST TO CALENDAR

    try:
        Task_holder = read_tasks()
        print("\n\n\nSystem Notif: Previous save loaded.")

    except:
        Task_holder = []
        print("\n\n\nSystem Notif: Previous save could not be found.")

    time.sleep(2)

######################

    # DEADLINES DUE TODAY, UPCOMING, OVERDUE

    due_today, due_this_month_upcoming, overdue = [], [], []

    for task in Task_holder:
        print(task)
        task_date = task[2].split(".")
        task_day, task_month, task_year = int(task_date[0]), int(task_date[1]), int(task_date[2])
        
        if task_year == current_year and task_month == current_month:
            if task_day == current_day:
                due_today.append(task)

            elif task_day > current_day:
                due_this_month_upcoming.append(task)

            elif task_day < current_day:
                overdue.append(task)
        elif task_month < current_month or task_year < current_year:
            overdue.append(task)


######################

    while True:

        os.system("clear")

        user_choice = input(f"\n\n\n|\t\t\tToday is {current_date}, {current_time}\t\t\t|\n|\t\t\t\t\t\t\t\t\t|\n|\t\t\tWhat will your focus today be?\t\t\t|\n\n\t\t\t{len(overdue)} [O]verdue tasks\n\t\t\t{len(due_today)} tasks due [T]oday\n\t\t\t{len(due_this_month_upcoming)} tasks [U]pcoming\n\t\t\tE[X]it calender view\n\nI want to... ")
        # (2) get a web api and let this quote of the day be on constant rotation

        if user_choice.lower() == "o":
            os.system("clear")
            print(see_tasks(overdue))
            done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")

        elif user_choice.lower() == "t":
            os.system("clear")
            print(see_tasks(due_today)) 
            done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
        
        elif user_choice.lower() == "u":
            os.system("clear")
            print(see_tasks(due_this_month_upcoming))
            done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
        
        elif user_choice.lower() == "x" or user_choice.lower() == "exit":
            break

######################
