class Category:
    
    balance = 0

    #__init__ run upon a class object being instantiated, use this to assign instance attributes to arguments passed into instance methods
    def __init__ (self, name):
        self.name = name
        self.ledger = []
        print(f'{self.name} is instantaited!')

    def deposit (self, amount, desc=''):
        self.amount = amount
        self.desc = desc
        self.ledger.append(f'"amount": {self.amount} , "description": {self.desc} ')
        return self.ledger
    
    def withdraw (self, amount, desc='', condition = False):
        self.amount = -amount
        self.desc = desc
        self.balance = self.get_balance()
        if int(amount) < self.balance:
            self.ledger.append(f'"amount": {self.amount} ,"description": {self.desc} ')
            return self.ledger
            condition = True
    
    def get_balance (self):
        templist = []
        for pair in self.ledger:
            amount = pair.split(',')
            number = (amount[0]).split(':')
            figure = int(number[1])
            templist.append(figure)
        self.templist = templist
        balance = sum(templist)
        self.balance = balance
        return self.balance

    def transfer (self, amount:int, catname, condition = False):
        self.catname = catname
        self.amount = -amount
        self.balance = self.get_balance()
        if int(amount) < self.balance:
            self.ledger.append(f'"amount": {self.amount} , "description": Transfer to {catname.name}')
            catname.ledger.append(f'"amount": {amount} , "description": Transfer from {self.name}')
            condition = True
            #print (f'{self.ledger}\n{catname.ledger}')
            return condition

    def check_funds (self, amount):
        self.balance = self.get_balance()
        if amount > self.balance:
            return False
        else:
            return True
        
    def __repr__ (self):
        
        #HEADER
        halve_length = (30 - len(self.name))
        #print(halve_length)
        if halve_length%2 == 0:
            #print ("I'm doing that!")
            half_length = halve_length/2
            loopnum = int(half_length)
            for itervar in range (loopnum):
                print ('*', end='')
            print (f'{self.name}', end='')
            for itervar in range (loopnum):
                print ('*', end='')
        else:
            #print ("I'm doing this!")
            half_length = halve_length/2
            init_num = str(half_length)
            loopnum = init_num[0]
            loopnum2 = int(loopnum) + 1
            for itervar in range (int(loopnum)):
                print ('*', end='')
            print (f'{self.name}', end='')
            for itervar in range (loopnum2):
                print ('*', end='')
        
        #BODY
        for pairs in self.ledger:
            amount = pairs.split(',')
            desc_components = (amount[1]).split(':')
            descr = (desc_components[1]).lstrip()
            amt_components = (amount[0]).split(':')
            amt = str(amt_components[1]).strip()

            if '.' not in amt:
                amt_len = len(amt) + 2
                amt = f'{amt}.00'
            else: 
                amt_len = len (amt)
            if len(descr)>23:
                descr = descr[0:22]
                descr_len = 22
            else:
                descr_len = len(descr)
            
            numspaces = 30-descr_len-amt_len
            spaces=''
            for itervar in range(numspaces-1):
                spaces+=' '
            print (f'\n{descr}{spaces}{amt}')
        
        #ENDING
        #: intoduces the format spec, 0 enables sign-aware zero-padding for numbers, .2 sets precision to 2, f displays the number as a fixed-point
        return (f'\nTotal: {self.balance:0.2f}')

#to round down any number by dividing it and retrieving the quotient without giving a remainder
def round_down (num:int):
    return num//10

def create_spend_chart (catlist:list):

    #note that catlist is a list type comprising of multiple category names (ie. 'self', referring to each instance of the Category class object that is instantiated within the Category class, and thus referring to category.ledger/ category.templist is the equivalent to referencing self.templist within the Category class)
    #calculating percentage of budget spent, assinging the category and percentage to a dictionary as a key-value pair
    percent_spent = {}
    string_storer = {}
    name_storer = {}
    for category in catlist:
        category.get_balance()
        sumtot = 0
        #print (category.templist)
        #print ('Now im iterating!')
        for itervar in category.templist:
            if itervar < 0:
                #print ('Im smaller than 0')
                sumtot = sumtot + itervar
        percent_spent [category.name] = -sumtot
    total = sum(percent_spent.values())
    count = 0
    for number in percent_spent.values():
        calculated_percentage = round_down(number/total * 100)*10
        #print(calculated_percentage)
        percent_spent [(catlist[count]).name] = calculated_percentage
        count = count + 1
    #print (percent_spent)

    #to caculate number of circles to be displayed per category
    tupleisland = percent_spent.items()
    for catnaem, basento in tupleisland:
        num_circles = basento/10
        percent_spent [catnaem] = num_circles
    #print (percent_spent)
    
    #create a dictionary storing key value pairs, keys are the rows 100 to 0, values are strings of relevant 0 spaces to print 
    row_number = 10
    while row_number >= 0:
        circles = ' '
        for catnaem, num_circles in percent_spent.items():
            #print (num_circles)
            if num_circles >= row_number:
                circles = circles + 'o  '
            else:
                circles = circles + '   '
        string_storer [row_number] = circles
        row_number = row_number - 1
    #print (string_storer)

    #the actual PRINTING of the histogram
        
    #header
    print ('Percentage spent by category')

    #body
    counter = 10
    while counter >= 0:
        print (f'{(str((counter * 10))).rjust(3)}|{string_storer[counter]}')
        counter = counter - 1

    #divider
    print ('    ', end = '')
    length = len(string_storer[0]) 
    for itervar in range(length):
        print ('-', end = '')
    
        
    def longest_name (liste:list):
        longest_len = 0
        for itervar in liste:
            if longest_len == 0:
                longest_len = len (itervar)
            if longest_len < len (itervar):
                longest_len = len (itervar)
        return longest_len

    #creating a list of purely names for easier reference
    danke_list = []
    #category names
    for caetnaem, num_circles in percent_spent.items():
        danke_list.append (caetnaem)
    counti = longest_name (danke_list)
    #print (danke_list)

    #create a dictionary storing key value pairs, keys are the rows of index 0 to greatest length of longest name, values are strings of relevant letters and spaces to print 
    for q in range (counti):
        #print (q)
        names = ''
        for name in danke_list:
            #print (name)
            try:
                names = names + f'{name[q]}  '
            except:
                names = names + f'   ' 
        name_storer [q] = names

    #this single print statement to print the single blank space exists to push the output of the first letter of all categories to the next line, to simplify the previous divider print statement
    print ('')
    #the actual printing of the category names
    for i in range (counti):
        print (f'     {name_storer[i]}')
    

    




        
    


    


