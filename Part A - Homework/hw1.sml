fun is_older((y1 : int, m1 : int, d1 : int), (y2 : int, m2 : int, d2 : int)) = 
    if y1 < y2 then true 
    else 
        if y1 = y2 andalso m1 < m2 then true
    else 
        if y1 = y2 andalso m1 = m2 andalso d1 < d2 then true 
    else 
        false;


fun number_in_month (dateslist : (int * int * int) list, month : int) =
    if null (dateslist)
    then 0
    else 
        let
            val x = if ((#2 (hd dateslist)) = month) then 1 else 0;
        in
            x + number_in_month(tl dateslist,month)
        end 
    

fun number_in_months (dateslist : (int * int * int) list, monthslist : int list) =
    if null (monthslist)
    then 0
    else number_in_month(dateslist, hd monthslis)) + number_in_months(dateslist,tl monthslist)
        
fun dates_in_month (dateslist : (int * int * int) list, month : int) =
    if null (dateslist)
    then []
    else 
        let
            val date = hd dateslist
            val monthOfDate = #2 date
        in
            if monthOfDate = month 
            then date :: dates_in_month(tl dateslist,month)
            else dates_in_month(tl dateslist,month)

        end 

fun dates_in_months (dateslist : (int * int * int) list, monthslist : int list) =
    if null monthslist
    then []
    else 
        let
            val x = dates_in_month(dateslist, (hd monthslist))
        in
            x @ dates_in_months(dateslist,tl monthslist) 
        end 


fun get_nth (monthes : string list, n : int) =
    if n = 1
    then hd(monthes)
    else get_nth(tl monthes,n-1)
    

fun date_to_string(date : int * int * int) =
get_nth(["January", "February", "March", "April",
"May", "June", "July", "August", "September", "October", "November", "December"],#2 date) ^ " " ^Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)

fun number_before_reaching_sum(sumVar : int, il : int list) = 
    if sumVar - hd il < 0 orelse sumVar - hd il = 0 
    then 0 
    else 1 +  number_before_reaching_sum(sumVar - hd il , tl il)


fun what_month(day : int) = 
    number_before_reaching_sum(day,[31,28,31,30,31,30,31,31,30,31,30,31]) + 1


fun month_range(day1 : int , day2: int) =
    if day1 > day2
    then []
    else what_month(day1)::month_range(day1 + 1, day2)

fun oldest(dates : (int * int * int) list) =
    if null dates
    then NONE
    else 
        let val tl_ans = oldest(tl dates)
        in 
        if isSome tl_ans andalso is_older(valOf tl_ans,hd dates)
            then tl_ans
	        else SOME (hd dates)
        end
