fun is_older (dt1 : int * int * int, dt2 : int * int * int) =
    if (#1 dt1) < (#1 dt2)
    then true
    else if (#1 dt1) > (#1 dt2)
    then false
    else if (#2 dt1) < (#2 dt2)
    then true
    else if (#2 dt1) > (#2 dt2)
    then false
    else if (#3 dt1) < (#3 dt2)
    then true
    else false


fun number_in_month (dtl : (int * int * int) list, m : int) =
    if null dtl
    then 0
    else if (#2 (hd dtl)) = m
    then number_in_month(tl dtl, m) + 1
    else number_in_month(tl dtl, m)


fun number_in_months (dtl : (int * int * int) list, ml : int list) =
    if null ml
    then 0
    else number_in_month(dtl, hd ml) + number_in_months(dtl, tl ml)


fun dates_in_month (dtl : (int * int * int) list, m : int) =
    if null dtl
    then []
    else if #2 (hd dtl) = m
    then hd dtl :: dates_in_month(tl dtl, m)
    else dates_in_month(tl dtl, m)


fun dates_in_months (dtl : (int * int * int) list, ml : int list) =
    if null ml
    then []
    else dates_in_month(dtl, hd ml) @ dates_in_months(dtl, tl ml)


fun get_nth (sl : string list, n : int) =
    let  fun h(xs, i) =
	     if i = n
	     then hd xs
	     else h(tl xs, i + 1)
    in
	h(sl, 1)
    end


fun date_to_string (y : int, m : int, d : int) =
    let val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
	val ystr = Int.toString(y)
	val mstr = get_nth (months, m)
	val dstr = Int.toString(d)
    in
	mstr ^ " " ^ dstr ^ ", " ^ ystr
    end


fun number_before_reaching_sum (sum : int, nl: int list) =
    let fun sum_nl (xs : int list, s : int, n : int) =
	    if s >= sum
	    then n - 1
	    else sum_nl(tl xs, s + hd xs, n + 1)
    in
	sum_nl(nl, 0, 0)
    end


fun what_month (day : int) =
    let val c = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	number_before_reaching_sum(day, c)
    end


fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range(day1 + 1, day2)


fun oldest (dtl : (int * int * int) list) =
    if null dtl
    then NONE
    else
	let fun f (xs : (int * int * int) list, r : (int * int * int)) =
		if null xs
		then r
	        else if is_older(hd xs, r)
		then f (tl xs, hd xs)
		else f (tl xs, r)

	in
	    SOME(f(dtl, hd dtl))
	end



(* challenge problem 12 *)
(*suppose given list is not null, return true if give int is in given int list*)
fun int_in_list (n : int, il : int list) =
    let fun f (ol : int list) =
	    if null ol
	    then false
	    else if n = hd ol
	    then true
	    else f(tl ol)
    in
	f(il)
    end

fun remove_int_list_duplicates (il : int list) =
    (* ol : original list, nl: new list*)
    let fun f (ol : int list, nl : int list) =
	    if null ol
	    then nl
	    else if int_in_list(hd ol, nl)
	    then f(tl ol, nl)
	    else f(tl ol, nl @ [hd ol])
    in
	f(il, [])
    end

fun number_in_months_challenge (dtl : (int * int * int) list, ml : int list ) =
    let val ml_n = remove_int_list_duplicates (ml)
    in
	number_in_months(dtl, ml_n)
    end

fun dates_in_months_challenge (dtl : (int * int * int) list, ml : int list ) =
    let val ml_n = remove_int_list_duplicates (ml)
    in
	dates_in_months(dtl, ml_n)
    end

(* challenge problem 13 *)
fun is_leap_year (y : int) =
    if y mod 400 = 0 orelse (y mod 4 = 0 andalso y mod 100 <> 0)
    then true
    else false

fun get_nth_int (il : int list, n : int) =
    let  fun h(xs, i) =
	     if i = n
	     then hd xs
	     else h(tl xs, i + 1)
    in
	h(il, 1)
    end

fun reasonable_date (dt : int * int * int) =
    if (#1 dt) <= 0
    then false
    else if (#2 dt) > 12 orelse (#2 dt) < 1
    then false
    else if (#3 dt) <= 0
    then false
    else if let val oy = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    in
		not (is_leap_year(#1 dt)) andalso (#3 dt) > get_nth_int(oy, (#2 dt))
	    end
    then false
    else if let val oy = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	    in
		is_leap_year(#1 dt) andalso (#3 dt) > get_nth_int(oy, (#2 dt))
	    end
    then false
    else true
