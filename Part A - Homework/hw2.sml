(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

 fun all_except_option(s1 : string, slist : string list) =
    case slist of
       [] => NONE
     | s::slist' => if same_string(s,s1)
                    then SOME(slist')
                    else case all_except_option(s1,slist') of
                        NONE => NONE
                        |SOME y => SOME(s::y) 


fun get_substitutions1(substitutions : string list list , target : string) = 
    case substitutions of
     [] => []
     | hd :: substitutions' => case all_except_option(target,hd)  of
                                    NONE => get_substitutions1(substitutions',target)
                                    |SOME y =>  y @ get_substitutions1(substitutions',target) 


fun get_substitutions2(substitutions : string list list , target : string) =
    let
    fun aux(substitutions_left,acc) =
                case substitutions_left of 
                [] => acc
                |hd::substitutions_left' => case all_except_option(target,hd)  of
                                        NONE => aux(substitutions_left',acc)
                                        |SOME y => aux(substitutions_left',y@acc)
    in
        aux(substitutions,[])
    end



fun similar_names(substitutions : string list list , {first:string,middle:string,last:string}) =
  
let
   val first_names = first :: get_substitutions2(substitutions,first)
   fun subs(first_names : string list) =
    case first_names of
        [] => []
         |first_name :: first_names' => {first=first_name,middle=middle,last=last} :: subs(first_names')

in
    subs(first_names) 
     
end  




(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(suit : suit, rank : rank) =
case suit of
   Spades => Black
 | Clubs => Black
 | Diamonds => Red
 | Hearts => Red

fun card_value(suit: suit, rank: rank) = 
case rank of
   Ace => 11
 | Num x => x
 | _ => 10

fun remove_card(cs : card list , target_card : card, e) =
    case cs of
       [] => raise e
     | c::cs' => if c = target_card
                then cs'
                else c :: remove_card(cs', target_card, e) 

fun all_same_color(cs: card list) =
    let
      val First_color = case cs of
                   c :: cs' => card_color(c) 
                   | _ => Black (*any value*)

        fun helper(cs: card list, First_color: color) = 
        case cs of
            [] => true
            |c :: cs' => if card_color(c) = First_color 
                        then helper(cs',First_color)
                        else  false  
    in
      helper(cs,First_color)
    end

fun sum_cards(cs : card list) = 
  let
    fun aux(cs,acc) = 
    case cs of
       [] => acc
     | c :: cs' => aux(cs',acc+card_value(c))
  in
    aux(cs,0)
  end

fun score(cs : card list, goal : int) =
   let
     val sum = sum_cards(cs)
   in
     if sum > goal 
     then if all_same_color(cs)
          then  (3 * (sum-goal))div 2 
          else   3 * (sum-goal)
     else if all_same_color(cs)
          then (goal - sum) div 2
          else goal - sum

   end 
                 
fun officiate(cs : card list, ms: move list, goal : int) = 
let
  fun helper(held_cards : card list, move_list : move list,card_list_left : card list) = 
   case move_list of
    [] => score(held_cards,goal)
   | move :: move_list' => case move of
                Discard c => helper(remove_card(held_cards, c,IllegalMove), move_list',card_list_left)
               
                |  Draw => case card_list_left of
                            [] => score(held_cards,goal)
                            | c :: card_list_left' => if sum_cards(c::held_cards) > goal 
                                                 then score(c::held_cards,goal)
                                                 else helper(c::held_cards,move_list',card_list_left')

in
  helper([],ms,cs)
end
                   
