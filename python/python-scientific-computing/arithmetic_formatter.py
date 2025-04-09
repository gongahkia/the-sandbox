def arithmetic_arranger (user_list,condition=None):
    output=''
    firstrow_final=''
    secondrow_final=''
    linerow_final=''
    bottomrow_final=''
    #Note that type(user_list)=List

    #To check for first condition of too many inputs 
    if len(user_list)>5:
        print('Too many problems. Maximum is 5.')
        exit()
    
    #Splitting each string equation into its components 
    for equation in user_list:
        equation_1=equation.split()

        #if operator is Addition, embedded try and except statement to check second condition, input not numeric
        if '+' in equation_1:
            first_num=int(equation_1[0])
            second_num=int(equation_1[2])
            operator='+'
            try:
                final_answer=first_num+second_num
            except:
                print('Error: Numbers must only contain digits.')
                exit()

        #if operator is Subtraction, embedded try and except statement to check second condition, input not numeric
        elif '-' in equation_1:
            first_num=int(equation_1[0])
            second_num=int(equation_1[2])
            operator='-'
            try:
                final_answer=first_num-second_num
            except:
                print('Error: Numbers must only contain digits.')
                exit()

        #To check for third condition that operator is neither addition nor subtraction
        else:
            print('Error: Operator must be + or -.')
            exit()

        #To check for forth condition that any of the operands exceed four digits
        if len(equation_1[0])>4 or len(equation_1[2])>4:
            print('Error: Numbers cannot be more than four digits.')
            exit()
        
        secondrow_spaces=''
        firstrow_spaces=''
        bottomrow_spaces=''
        lines=''

        longer_num=0
        #longer_num is number with most characters in entire formatted calculation
        if len(equation_1[0])>len(equation_1[2]):
            longer_num=len(equation_1[0])
        if len(equation_1)==len(equation_1[2]):
            longer_num=len(equation_1[0])
        else:
            longer_num=len(equation_1[2])

        for j in range(longer_num+2):
            lines=lines+'-'
    
        #To determine the number of spaces per 'chunk' of calculation
        secondrow_num_spaces=len(lines)-len(equation_1[2])-1
        firstrow_num_spaces=len(lines)-len(equation_1[0])
        bottom_num_spaces=len(lines)-len(str(final_answer))
        
        for i in range(secondrow_num_spaces):
            secondrow_spaces=secondrow_spaces+' '
        for x in range(firstrow_num_spaces):
            firstrow_spaces=firstrow_spaces+' '
        for p in range(bottom_num_spaces):
            bottomrow_spaces=bottomrow_spaces+' '
        
        #To create each 'chunk' of calculation
        firstrow_output=firstrow_spaces+str(first_num)+'    '
        secondrow_output=operator+secondrow_spaces+str(second_num)+'    '
        linerow_output=lines+'    '
        bottomrow_output=bottomrow_spaces+str(final_answer)+'    '

        #To create the entire string for each line (all the chunks of calculation)
        firstrow_final=firstrow_final+firstrow_output
        secondrow_final=secondrow_final+secondrow_output
        linerow_final=linerow_final+linerow_output
        bottomrow_final=bottomrow_final+bottomrow_output
    
    #To check for the fifth condition, whether the second parameter is True or not
    if condition is True:
        output=f'{firstrow_final}\n{secondrow_final}\n{linerow_final}\n{bottomrow_final}'
        print(output)
    
    else:
        output=f'{firstrow_final}\n{secondrow_final}\n{linerow_final}'
        print(output)


        
        

    
            
        
            
