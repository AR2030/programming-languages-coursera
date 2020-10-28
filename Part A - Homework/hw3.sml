(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end


(**** you can put all your code here ****)


val only_capitals = 
    List.filter (fn s => Char.isUpper (String.sub(s,0))) 

val longest_string1 = 
    List.foldl (fn (s,y)  => if String.size s > String.size y then s else y ) "" 

val longest_string2 = 
    List.foldl (fn (s,y)  => if String.size s >= String.size y then s else y ) "" 

fun longest_string_helper f = 
     List.foldl (fn (s,y) => if f(String.size s,String.size y)
				                 then s
								else y) ""

val longest_string3 = longest_string_helper (fn (x,y) => x > y)

val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

val longest_capitalized = 
   longest_string3 o only_capitals

val rev_string  = String.implode o rev o  String.explode  

fun first_answer f thelist =	
    case thelist of
		[] => raise NoAnswer
    	| x::thelist' => case f x of 
								(SOME y) => y
								|NONE => first_answer f thelist'

fun all_answers f thelist =	
 	let fun aux(thelist,acc) =
            case thelist of
                [] => SOME acc
              | x::thelist' =>	case f x of
								    SOME x => aux(thelist',acc @ x)
								   |NONE => NONE 
    in
       aux(thelist,[]) 
    end 



val count_wildcards = g (fn () => 1 ) (fn _ => 0)  
val count_wild_and_variable_lengths =  g (fn () => 1 ) String.size   
fun count_some_var (s,p) = g  (fn () => 0) (fn x => if s = x then 1 else 0) p

fun check_pat p =
	let
		 fun filter p acc  =  case p of
									  Variable x        => x :: acc 
									| TupleP ps         => List.foldl (fn (p,acc) => (filter p []) @ acc) [] ps
									| ConstructorP(_,pat) => filter pat acc
									| _                 => []

		 fun noRepeats slist = 
		 	case slist of
				[] => true
				|[x] => true
			    | s :: slist' => if List.exists (fn x => (s = x)) slist' 	
								 then false	
								 else 
								 noRepeats slist'
	in
	  noRepeats (filter p [])
	end

fun match(v,p) = 
	case p of
	 	  Wildcard  => SOME []
		 | Variable s => SOME [(s,v)]
		 | UnitP => (case v of
						 Unit => SOME []
						| _ => NONE)
		 | ConstP x => (case v of
						 Const i => if i = x then SOME [] else NONE
						| _ => NONE)
		 | TupleP plst => (case v of 
		 			Tuple vlst => if List.length vlst = List.length plst
					 			  then all_answers match(ListPair.zip (vlst, plst))
								  else NONE
			  		| _ => NONE)
		 | ConstructorP (s,patternP) =>  (case v of
										Constructor (vs, vval) => if s = vs then match (vval, patternP) else NONE 
										| _ => NONE)

fun first_match v plst  = 
	SOME(first_answer (fn p => match(v,p)) plst)
	handle NoAnswer => NONE


