def add_time (starttime,duration,dayofweek=None):
    #type(starttime)=string eg. 3:00 PM
    #type(duration)=string eg. 3:10
    #optional, type(dayofweek)=string eg. Monday

    #Parse starttime into its components
    starttime_components=starttime.split(':')
    starttime_hours=int(starttime_components[0])
    starttime_minutes=int(((starttime_components[1]).split())[0])
    starttime_AMPM=((starttime_components[1]).split())[1]

    #Parse duration into its components 
    duration_components=duration.split(':')
    duration_hours=int(duration_components[0])
    duration_minutes=int(duration_components[1])

    #Parse dayofweek and assign a numerical value to it, accepting None type to anticipate lack of day input
    if dayofweek is None:
        dayofweek_counter=0
        pass

    #Yes, in the future,it would be way easier to achieve this reading with a dictionary
    else:
        dayofweeks=dayofweek.lower()
        if dayofweeks == 'monday':
            dayofweek_counter=1
        elif dayofweeks == 'tuesday':
            dayofweek_counter=2
        elif dayofweeks == 'wednesday':
            dayofweek_counter=3
        elif dayofweeks == 'thursday':
            dayofweek_counter=4
        elif dayofweeks == 'friday':
            dayofweek_counter=5
        elif dayofweeks == 'saturday':
            dayofweek_counter=6
        elif dayofweeks == 'sunday':
            dayofweek_counter=7
        else:
            print('Invalid day of the week')
            exit()

    #Calculate added time
    new_hours=starttime_hours+duration_hours
    new_minutes=starttime_minutes+duration_minutes
    #print(f'new minutes: {new_minutes}')
    #^Used during debugging

    owed_new_hours=0

    #AMPM counter is EVEN when AM, ODD for PM
    if starttime_AMPM == 'AM':
        ampm_counter=0
    else:
        ampm_counter=1
    
    #print(f'intial AM/PM counter: {ampm_counter}')
    #^Used during debugging

    #to account for minutes exceeding 59, add new hour to hour count
    while new_minutes>59:
        owed_new_hours+=1
        #print(owed_new_hours)
        new_minutes-=60
    
    #to convert single digit minute values into double digit values
    new_minutes=str(new_minutes)
    new_minutes=new_minutes.zfill(2)

    #to calculate new hours
    new_hours=new_hours+owed_new_hours

    #print(f'owed hours: {owed_new_hours}')
    #print(f'new hours: {new_hours}')
    #^Used during debugging

    #to account for hours exceeding 12 to account for the 12hr AMPM format of the display time, each hour over 12 to change 
    if new_hours>11:
        if new_hours==12:
            ampm_counter=ampm_counter+(new_hours//12)
            new_hours=12
        if new_hours>12:
            ampm_counter=ampm_counter+(new_hours//12)
            new_hours=new_hours-(12*(new_hours//12))
            if new_hours==0:
                new_hours=12

    #print(f'final AM/PM counter: {ampm_counter}')
    #^Used during debugging
    
    #to determine the new state of day 
    if (ampm_counter%2) == 0:
        new_ampm='AM'
    elif ampm_counter==1:
        new_ampm='PM'
    elif ampm_counter==0:
        new_ampm='AM'
    else:
        new_ampm='PM'
    

    owed_day_counter=0

    #print(f'initial owed day counter: {owed_day_counter}')
    #^Used during debugging

    #to calculate number of days passed, required for both the 'days later' statement as well as the new day statement
    while ampm_counter>1:
        owed_day_counter+=1
        ampm_counter=ampm_counter-2
        if ampm_counter==1:
            pass
    
    #print(f'final owed day counter: {owed_day_counter}')
    #^Used during debugging

    #to calculate new day of the week
    dayofweek_counter=dayofweek_counter+owed_day_counter

    #to keep range of values within 0 to 7
    while dayofweek_counter>7:
        dayofweek_counter-=7
    
    if dayofweek is None:
        new_day=''
    else:
        if dayofweek_counter == 0:
            new_day=', Sunday'
        elif dayofweek_counter == 1:
            new_day=', Monday'
        elif dayofweek_counter == 2:
            new_day=', Tuesday'
        elif dayofweek_counter == 3:
            new_day=', Wednesday'
        elif dayofweek_counter == 4:
            new_day=', Thursday'
        elif dayofweek_counter == 5:
            new_day=', Friday'
        elif dayofweek_counter == 6:
            new_day=', Saturday'
        else:
            new_day=''

    #to check for what has to be printed for 'next day/days later'
    if owed_day_counter>1:
        days_later=f', ({owed_day_counter} days later)'
    elif owed_day_counter==1:
        days_later=' (next day)'
    elif owed_day_counter==0:
        days_later=''
    
    #Notice I leave the writing of the ',' to each individual executable's string writing, instead of enforcing it within the final print statement
    print(f'Returns: {new_hours}:{new_minutes} {new_ampm}{new_day}{days_later}')
