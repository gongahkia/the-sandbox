import pickle

# WORKING
def add_task(task_name="", task_description="", task_deadline="", task_urgency="low", Task_holder=[]):
    task_format = []
    task_format.extend([task_name, task_description, task_deadline, task_urgency])
    Task_holder.append(task_format)
    return Task_holder

# WORKING
def complete_task(task_name, Task_holder):
    for itervar in range(len(Task_holder)):
        if Task_holder[itervar][0] == task_name:
            del Task_holder[itervar]
            return Task_holder
    return f"{task_name} cannot be found. Please choose an existing task."

# WORKING
def edit_task_name(task_name, new_task_name, Task_holder):
    for itervar in range(len(Task_holder)):
        if Task_holder[itervar][0] == task_name:
            Task_holder[itervar][0] = new_task_name
            return Task_holder
    return f"{task_name} cannot be found. Please choose an existing task."

# WORKING
def edit_task_description(task_description, new_task_description, Task_holder):
    for itervar in range(len(Task_holder)):
        if Task_holder[itervar][1] == task_description:
            Task_holder[itervar][1] = new_task_description
            return Task_holder
    return "Task cannot be found. Please choose an existing task."

# WORKING
def edit_task_deadline(task_deadline, new_task_deadline, Task_holder):
    for itervar in range(len(Task_holder)):
        if Task_holder[itervar][2] == task_deadline:
            Task_holder[itervar][2] = new_task_deadline
            return Task_holder
    return "Task cannot be found. Please choose an existing task."

# WORKING    
def edit_task_urgency(task_urgency, new_task_urgency, Task_holder):
    for itervar in range(len(Task_holder)):
        if Task_holder[itervar][3] == task_urgency:
            Task_holder[itervar][3] = new_task_urgency
            return Task_holder
    return "Task cannot be found. Please choose an existing task."

# WORKING
def see_tasks(Task_holder):
    final_print = "\n"
    counter = 1
    for task in Task_holder:
        if counter < 10:
            number = " " + str(counter)
        else:
            number = str(counter)
        line_print = f"| {number} |"
        for component in task:
            num_spaces1, space1, space2,= 0, "", ""
            if len(component) % 2 == 0:
                #even
                num_spaces1 = int((30 - len(component))/2)
                for itervar in range(num_spaces1):
                    space1 += " "
                space2 = space1
            elif len(component) % 2 != 0:
                #odd
                num_spaces1= int((30 - len(component) - 1)/2)
                for itervar in range(num_spaces1):
                    space1 += " "
                space2 = space1 + " "
            line_print += space1 + component + space2 + "|"
        final_print += line_print + "\n"
        counter += 1
    return final_print

# WORKING
def write_tasks(Task_holder):
    fhand = open("tasks", "wb")
    pickle.dump(Task_holder, fhand)

# WORKING
def read_tasks():
    fhand = open("tasks", "rb")
    Task_holder = pickle.load(fhand)
    return Task_holder
