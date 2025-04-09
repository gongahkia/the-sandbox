from todo import *
from datetime import date 
from calendar import calendar
import os
import time

current_date = date.today()
temp_date = (current_date.strftime("%d.%m.%y")).split(".")
temp_date[1] = abs(int(temp_date[1]))
formatted_current_date = f"{temp_date[0]}.{temp_date[1]}.{temp_date[2]}"

    #####################

    #ADD_TASK

def add_task_operation(): 

    os.system("clear")
    task_name = input("Task name: ")
    if len(task_name) > 25:
        print("Please shorten task name.")
        task_name = task_name[:22] + "..."
        time.sleep(2)

    os.system("clear")
    task_description = input("Task description: ")
    if len(task_description) > 25:
        print("Please shorten task description. ")
        task_description = task_description[:22] + "..."
        time.sleep(2)

    os.system("clear")
    task_deadline = input("Task deadline [DD.MM.YY]: ")
    date_components = task_deadline.split(".")
    if len(date_components) == 3:
        date_components[0], date_components[1], date_components[2] = int(date_components[0]), int(date_components[1]), int(date_components[2])
        if date_components[0] < 32 and date_components[0] > 0 and date_components[1] < 13 and date_components[1] > 0 and date_components[2] < 100 and date_components[2] > 22:
            pass
        else:
            print("Invalid date entered. Please enter a deadline in the future.")
            task_deadline = formatted_current_date
            time.sleep(2)
    else:
        print("Please enter a deadline according to the specified date format [DD.MM.YY].")
        task_deadline = formatted_current_date
        time.sleep(2)
            
    os.system("clear")
    task_urgency = input("Task urgency [(L)ow / (M)edium / (H)igh]: ")
    if task_urgency.lower() == "l":
        task_urgency = "Low"
    elif task_urgency.lower() == "m":
        task_urgency = "Medium"
    elif task_urgency.lower() == "h":
        task_urgency = "High"
    else:
        task_urgency = "Low"

    add_task(task_name, task_description, task_deadline, task_urgency, Task_holder)

    #####################

    #SEE_TASKS

def see_tasks_operation(): 
    os.system("clear")
    print(see_tasks(Task_holder))

    #####################

    #COMPLETE_TASKS

def complete_task_operation():
    completed_task = input("Enter name of completed task: ")
    complete_task(completed_task, Task_holder)

    #####################

    #DELETE_TASKS

def delete_task_operation():
    deleted_task = input("Enter name of task to be deleted: ")
    complete_task(deleted_task, Task_holder)

    #####################

    #EDIT_TASK_NAME

def edit_task_name_operation():
    old_task_name = input("\n\n\nEnter old task name: ")
    new_task_name = input("Enter new task name: ")
    if len(new_task_name) > 25:
        print("Please shorten task name.")
        new_task_name = new_task_name[:22] + "..."
        time.sleep(2)

    edit_task_name(old_task_name, new_task_name, Task_holder)

    #####################

    #EDIT_TASK_DESCRIPTION

def edit_task_description_operation():
    old_task_description = input("\n\n\nEnter old task description: ")
    new_task_description = input("Enter new task description: ")
    if len(new_task_description) > 25:
        print("Please shorten task description. ")
        new_task_description = new_task_description[:22] + "..."
        time.sleep(2)

    edit_task_description(old_task_description, new_task_description, Task_holder)

    #####################

    #EDIT_TASK_DEADLINE

def edit_task_deadline_operation():
    old_task_deadline = input("\n\n\nEnter old task deadline [DD.MM.YY]: ")    
    new_task_deadline = input("Enter new task deadline [DD.MM.YY]: ")
    new_date_components = new_task_deadline.split(".")
    if len(new_date_components) == 3:
        new_date_components[0], new_date_components[1], new_date_components[2] = int(new_date_components[0]), int(new_date_components[1]), int(new_date_components[2])
        if new_date_components[0] < 32 and new_date_components[0] > 0 and new_date_components[1] < 13 and new_date_components[1] > 0 and new_date_components[2] < 100 and new_date_components[2] > 22:
            pass
        else:
            print("Invalid date entered. Please enter a deadline in the future.")
            new_task_deadline = formatted_current_date
        time.sleep(2)
    else:
        print("Please enter a deadline according to the specified date format [DD.MM.YY].")
        new_task_deadline = formatted_current_date
        time.sleep(2)
    edit_task_deadline(old_task_deadline, new_task_deadline, Task_holder)

    #####################

    #EDIT_TASK_URGENCY

def edit_task_urgency_operation():
    old_task_urgency = input("\n\n\nEnter old task urgency level [(L)ow / (M)edium / (H)igh]: ")
    new_task_urgency = input("Enter new task urgency level [(L)ow / (M)edium / (H)igh]: ")
    if new_task_urgency.lower() == "l":
        new_task_urgency = "Low"
    elif new_task_urgency.lower() == "m":
        new_task_urgency = "Medium"
    elif new_task_urgency.lower() == "h":
        new_task_urgency = "High"
    else:
        new_task_urgency = "Low"
    edit_task_urgency(old_task_urgency, new_task_urgency, Task_holder)

    #####################

################################

# actual program

while True:
    os.system("clear")
    user_input = input("\n\n\n|\t\t\t[L]ist\t\t\t\t\t|\n|\t\t\t[C]alender\t\t\t\t|\n|\t\t\t[P]roject manager\t\t\t|\n|\t\t\tE[X]it the program\t\t\t|\n\nI want to... ")

    if user_input.lower() == "l":
        os.system("clear")
        print("\n\n\nloading...")
        time.sleep(2)
        os.system("clear")
# TODO LIST
        print("\n\n\n\n\n|\t\t\tWelcome to Bullet List V1.0\t\t\t|\n   ~For any feedback and pull requests, refer to @gongahkia on Github~\n\n\n\n\n")
        time.sleep(2)
        print("loading...")
        time.sleep(2)

# RELOAD PAST SAVE

        try:
            Task_holder = read_tasks()
            print("\n\n\nSystem Notif: Previous save loaded.")

        except:
            Task_holder = []
            print("\n\n\nSystem Notif: Previous save could not be found.")

# MAIN TODO BODY CALL

        time.sleep(2)

        while True:
            os.system("clear")
            user_decision = input("\n\n\n|\t\t\tWhat would you like to do?\t\t\t|\n\n\t\t\t[A]dd task\n\t\t\t[C]omplete task\n\t\t\t[D]elete task\n\t\t\t[E]dit task\n\t\t\t[V]iew tasks\n\t\t\tE[X]it\n\nI want to... ")

            if user_decision.lower() == "a":
                see_tasks_operation()
                done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
                add_task_operation()
                see_tasks_operation()
                done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")

            elif user_decision.lower() == "c":
                see_tasks_operation()
                complete_task_operation()
                see_tasks_operation()
                print("\n\n\nSystem Notif: Your task has been checked off.\n\n\n")
                done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")

            elif user_decision.lower() == "d":
                see_tasks_operation()
                delete_task_operation()
                see_tasks_operation()
                print("\n\n\nSystem Notif: The task has been deleted.\n\n\n")
                done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")

            elif user_decision.lower() == "e":
                see_tasks_operation()
                edit_which_component = input("\n\n\n\n|\t\t\tWhat would you like to edit?\t\t\t|\n\n\t\t\tTask [N]ame\n\t\t\tTask [D]escription\n\t\t\tTask D[E]adline\n\t\t\tTask [U]rgency\n\nI want to edit... ")
                if edit_which_component == "n":
                    os.system("clear")
                    see_tasks_operation()
                    edit_task_name_operation()
                    see_tasks_operation()
                    done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
                elif edit_which_component == "d":
                    os.system("clear")
                    see_tasks_operation()
                    edit_task_description_operation()
                    see_tasks_operation()
                    done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
                elif edit_which_component == "e":
                    os.system("clear")
                    see_tasks_operation()
                    edit_task_deadline_operation()
                    see_tasks_operation()
                    done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
                elif edit_which_component == "u":
                    os.system("clear")
                    see_tasks_operation()
                    edit_task_urgency_operation() 
                    see_tasks_operation()
                    done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
                    # (1) edit the edit_task_urgency function to take in old_task_urgency parameter as l/m/h instead of having to type out the entire word

            elif user_decision.lower() == "v":
                see_tasks_operation()
                done_viewing = input("\n\n\n|\t\t\tWhen done viewing, press [Enter]\t\t\t|")
            
            #EXIT CALL

            elif user_decision.lower() == "x" or user_decision.lower() == "exit":
                break
            
        os.system("clear")
        save_input = input("[S]ave?: ")
        if save_input.lower() == "s":

        # SAVE TASKS TO LOCAL FILE

            write_tasks(Task_holder)
            os.system("clear")
            print("System Notif: Tasks saved")

    elif user_input.lower() == "c":
        os.system("clear")
        print("\n\n\nloading...")
        time.sleep(2)
        os.system("clear")

        # CALENDAR FUNCTION
        calendar()
                    
    elif user_input.lower() == "p":
        os.system("clear")
        print("\n\n\nloading...")
        time.sleep(2)
        os.system("clear")
        # (2) move the project manager function (imported from another file) here once its done, style it the same way as the calendar function

    elif user_input.lower() == "x":
        os.system("clear")
        print("\n\n\nExiting...")
        time.sleep(2)
        os.system("clear")
        print("Thanks for using this program.\nSee you again soon!")
        break

