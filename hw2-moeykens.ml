(* CS225 Programming Languages: Homework 2 *)

(* Template, by ceskalka *)

(*
   NOTE: you should complete all functions it says COMPLETE ME. Curently these
   functions have dummy bodies that are not correct solutions.
*)

(* 
   dayname : int -> string
   in  : An int n in {0,..,6} specifying a day of the week (Monday is 0).
   out : The english name of the nth day of the week
*)
let dayname n = match n with 
    0 -> "Monday"
  | 1 -> "Tuesday" 
  | 2 -> "Wednesday"
  | 3 -> "Thursday"
  | 4 -> "Friday"
  | 5 -> "Saturday"
  | 6 -> "Sunday"


(*
  add_daynames : int -> int list -> (int * string) list
  in  : A month offset o, list of calendar dates l = [d1,...,dk]
  out : List [(d1,dn1),...,(dk,dnk)], where for all  i in {1,...,k},
        dni = dayname di given o
*)
let add_daynames o l = let days_in_week = 7 in 
      List.map (fun x -> (x, dayname ((x + (o - 1)) mod days_in_week))) l;;

(* 
   member : 'a -> 'a list -> bool
   in  : Value v, list l
   out : True iff v is in l
*)
let rec member v l = match l with 
    [] -> false
  | x::xs -> if x = v then true else member v xs ;;

(*
   exists : (’a -> bool) -> ’a list -> bool
   in : a predicate p on values v : ’a
   out : a function f : ’a list -> bool such
                   that f(l) is true iff p(x) for some x in l
*)
let rec exists p l = match l with 
    [] -> false
  | x::xs -> if p(x) then true else exists p xs ;;

(* measurement datatype *)
type emeasures = Meter of float | Liter of float | Centigrade of float
type ameasures = Feet of float | Gallon of float | Fahrenheit of float

(*
  conversion : emeasures -> ameasures
  in : english measurement x
  out : conversion of x into corresponding american
        measurement
*)
let conversion em = match em with 
    Meter x -> Feet (x *. 3.2808)
  | Liter x -> Gallon (x *. 0.2641729)
  | Centigrade x -> Fahrenheit ((x *. 1.8) +. 32.0)

(* tree datatype *)
type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

(*
  lookup : ('a * 'a -> bool) -> 'a -> 'a tree -> bool
  in : total order lt, element x, tree t possessing 
       BST property
  out : true iff x is in t
*)
let rec lookup lt x t = match t with 
    Leaf -> false
  | Node (tl, nx, tr) -> if nx = x then true 
      else (if lt(x, nx) then lookup lt x tl else lookup lt x tr) ;;
	
(*
  insert : ('a * 'a -> bool) -> 'a -> 'a tree -> 'a tree
  in : total order lt, element x, tree t possessing 
       BST property
  out : tree t' which is t with x inserted such that t' 
        possesses BST property 
*)
let rec insert lt x t = match t with 
    Leaf -> Node(Leaf, x, Leaf)
  | Node (tl, nx, tr) -> if lt(x, nx) then Node (insert lt x tl, nx, tr) 
      else Node (tl, nx, insert lt x tr) ;;
