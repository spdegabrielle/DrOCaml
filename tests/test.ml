fun n -> match n with
  | 1 -> 0
  | n -> n+1;;

if (2 = 3) or (3=3)
then "hello"
else "goodbye";;

2+3;;

((fun x y -> x + y)  2 3);;

((fun x y -> x * y) 2 3);;

let rec fixme1 x = match x with
  | [] -> 1
  | hd::tl ->
    let foo = -4
    and bar = 5
    in foo + bar * (fixme1 tl);;

let rec fixme2 x = match x with
  | [a] -> [a]
  | a::b::tail -> b::a::(fixme2 tail)
  | x -> x;;

let test n = match n with
  | x -> x
  | 0 -> 1;;

  
