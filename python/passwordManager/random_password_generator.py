import random
import json

global_dict={}

#to generate random password based on ASCII chart
#global to globalize the local variable 'string'
#try except to detect invalid inputs
def password_generator():
    global string
    try:
        no_characters=int(input('\nHow many characters do you want your password to be?: '))

    except:
        print('Invalid input. Please input an integer.')
        exit()

    string=''
    for iter_var in range(no_characters):
        character=chr(random.randint(33,126))
        string=string+character
    print(f'\nYour password is: {string}')

#to save usernames and passwords to a joint dictionary
#global to globalize the local variable 'string'
def purpose_username_password():
    global string
    global global_dict
    purpose=input('\nInput a keyword you want to associate this username/password with: ')
    username=input('\nInput your username: ')
    temp_list=[]
    temp_list.extend([username,string])
    global_dict[purpose]=temp_list
    print(f'\nThese are your details.\n\nPurpose: {purpose} | Username: {username} | Password: {string}')

#to save global_dict to json file
#global to globalize the local variable 'string'
def save_memory():
    global global_dict
    y_or_n=input('\nDo you want to save your data to a local file?\n[Y/N]\n')
    if y_or_n == 'Y' or y_or_n == 'y':
        fhand=open('local_save.json','w')
        json.dump(global_dict,fhand,indent=6)
        print('\nYour data has been saved.\n')
    else:
        print('\nOkay, data will not be saved.\n')
    
#to load saved json files and parse the data within them
#global to globalize the local variable 'string'
#warning message occurs if user tries to save current data over a past save file
def load_saves():
    global global_dict
    y_or_n=input('\nDo you want to load your past save?\n[Y/N]\n')
    if y_or_n == 'Y' or y_or_n == 'y':
        file_name=input('\nEnter the name of the local save file: \n')
        fhand=open(file_name,'r')
        global_dict=json.load(fhand)
    else:
        user_input=input('\nWe recommend loading your save file (if available. Otherwise, saved data this session will override past saves. Are you sure you do not want to load your past save? [Y/N]\n')
        if user_input == 'N' or user_input == 'n':
            file_name=input('\nOkay. Enter the name of the local save file: \n')
            fhand=open(file_name,'r')
            global_dict=json.load(fhand)
        else:
            print('\nOkay. Your past save data will not be loaded.\n')

#to parse data from global_dict
#global to globalize the local variable 'string'
def parse_json():
    global global_dict
    for purpose,username_password in global_dict.items():
        username=username_password[0]
        password=username_password[1]
        print(f'Purpose: {purpose} | Username: {username} | Password: {password} ')
    
#to delete data from global_dict
#global to globalize the local variable 'string'        
def delete_purp_username_password():
    global global_dict
    user_input=input('\nEnter the purpose of the entry you want to delete: \n')
    try:
        del global_dict[user_input]
    except:
        print('\nEntry not found.\n')
        exit()
    print(f'\nEntry with name {user_input} succesfully removed.')
    for purpose,username_password in global_dict.items():
        username=username_password[0]
        password=username_password[1]
        print(f'\nPurpose: {purpose} | Username: {username} | Password: {password} ')

#the actual program
print('\nWelcome to Random Password Generator V1.')

while True:
    user_input=input('\nChoose one of the following:\n\nGenerate a password\nSave username and password\nView save log\nAmend save log\nSave changes\nLoad past save\nExit\n\n')
    
    if user_input=='Generate a password' or user_input=='generate a password':
        password_generator()
        user_input2=input('\nDo you want to save this password with a username? [Y/N]\n')
        if user_input2 == 'Y' or user_input2 == 'y':
            purpose_username_password()
        else:
            print('\nOkay.\n')
    
    if user_input=='Save username and password' or user_input=='save username and password':
        try:
            purpose_username_password()
        except:
            print('\nPlease generate a password first.\n')
            continue
    
    if user_input=='View save log' or user_input=='view save log':
        parse_json()
    
    if user_input=='Amend save log' or user_input=='amend save log':
        delete_purp_username_password()
    
    if user_input=='Save changes' or user_input=='save changes':
        save_memory()
    
    if user_input=='Load past save' or user_input=='load past save':
        load_saves()
        parse_json()
    
    if user_input=='Exit' or user_input=='exit':
        user_input3=input('\nHave you saved your changes? [Y/N]\n')
        if user_input3=='Y' or user_input3 == 'y':
            print('\nOkay. See you next time!\n')
            break
        else:
            print('\nPhew. That was a close one. Go save your changes first.\n')
            continue



        


