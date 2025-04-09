import json
import time

empty_dictionary={}

def enter_a_task():
    user_input=input('Type "done" to exit\nEnter a task: ')
    global empty_dictionary
    count=1
    while user_input!='done':
        empty_dictionary[count]=user_input
        count=count+1
        user_input=input('Enter a task: ')
    #print(empty_dictionary)

def amend_a_task():
    global empty_dictionary
    user_input2=input('Enter number of task you wish to amend: ')
    int_user_input2=int(user_input2)
    user_input3=input('Enter amended task: ')
    empty_dictionary[int_user_input2]=user_input3
    time.sleep(0.5)
    print('\nChange saved\n')
    for number,task in empty_dictionary.items():
        output=str(number)+':'+str(task)
        print(output)

def check_off_task():
    global empty_dictionary
    user_input4=input('Enter number of task you want to check off: ')
    int_user_input4=int(user_input4)
    del empty_dictionary[int_user_input4]
    #print(empty_dictionary)
    time.sleep(0.5)
    print('\nTask completed.\n')
    lonely_values=empty_dictionary.values()
    empty_dictionary={}
    count=1
    for value in lonely_values:
        empty_dictionary[count]=value
        count=count+1
    for number,task in empty_dictionary.items():
        output=str(number)+':'+str(task)
        print(output)

while True:
    user_input=input('\nChoose one of the following:\nadd task\nview tasks\ncheck off task\namend tasks\nsave tasks\nload tasks\n\n')
    if user_input=='add task':
        print('\nLoading...\n')
        time.sleep(0.5)
        enter_a_task()
        #print(empty_dictionary)

    elif user_input=='view tasks': 
        print('\nLoading...\n')
        time.sleep(0.5) 
        for number,task in empty_dictionary.items():
            output=str(number)+':'+str(task)
            print(output)

    elif user_input=='amend tasks':
        print('\nLoading...\n')
        time.sleep(0.5)
        for number,task in empty_dictionary.items():
            output=str(number)+':'+str(task)
            print(output)
        amend_a_task()    

    elif user_input=='check off task':
        print('\nLoading...\n')
        time.sleep(0.5)
        for number,task in empty_dictionary.items():
            output=str(number)+':'+str(task)
            print(output)
        check_off_task()

#json built-in function to save tasks
    elif user_input=='save tasks':
        fhand=open('user_dictionary1.json','w')
        json.dump(empty_dictionary,fhand,indent=6)
        print('\nLoading...\n')
        time.sleep(0.5)
        print('Your tasks have been saved.\n')
        time.sleep(0.5)
        print('Exiting...')
        fhand.close()
        break   

    elif user_input=='load tasks':
        user_input2=input('Enter name of file: ')
        fhand=open(user_input2,'r')
        #print(json.load(fhand))
        empty_dictionary=json.load(fhand)
        for number,task in empty_dictionary.items():
            output=str(number)+':'+str(task)
            print(output)
        fhand.close()








