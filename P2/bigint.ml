(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)


    (*!! https://ocaml.org/learn/tutorials/99problems.html*)
    let rec fold_until f acc n = function
        | [] -> (acc, [])
        | h :: t as l -> if n = 0 then (acc, l)
                         else fold_until f (f acc h) (n-1) t
  
    let slice list i k =
        let _, list = fold_until (fun _ _ -> []) [] i list in
        let taken, _ = fold_until (fun acc h -> h :: acc) [] (k - i + 1) list in
        List.rev taken;;

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []          -> []
            | [0]         -> []
            | car::cdr    ->
                    let cdr' = trimzeros' cdr
                    in match car, cdr' with
                        | 0, []       -> []
                        | car, cdr'   ->  car::cdr'
        in trimzeros' list

    (*Helper metyhods*)

    let flip_sign sign = match sign with
        | Pos -> Neg
        | Neg -> Pos


    let rec compareTo' list1 list2 = match (list1, list2) with
        | [], []                -> 0
        | list1, list2          -> begin
            if car list1 > car list2 then 1
            else if car list1 < car list2 then -1
            else compareTo' (cdr list1) (cdr list2)
        end


    (**Similar to Java method returns:
        -   1   if value1 > value2
        - (-1)  if value1 < value2
        -   0   if value1 = value2*)
    let compareTo value1 value2 = 
        let list_int1 = reverse (trimzeros value1)  
        in let list_int2 = reverse (trimzeros value2)
        in if List.length list_int1 > List.length list_int2
        then 1
        else if  List.length list_int1 < List.length list_int2
            then -1
            else compareTo' list_int1 list_int2;;


    let rec to_string_helper reversed_list = 
        let len = List.length reversed_list
        in if len < 71
        then (map string_of_int reversed_list)
        else begin
            let l1 = map string_of_int (slice reversed_list 0 68)
            in let l2 = slice reversed_list 69 len
                in (l1)@["\\"]@["\n"]@(to_string_helper l2)

        end

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (to_string_helper reversed))


    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

     let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
        | list1, [], false                 -> list1
        | [], list2, false                 -> list2
        | list1, [], true                 -> sub' list1 [1] false
        | car1::cdr1, car2::cdr2, false  -> begin
          if car1 > car2 then let subs = car1 - car2
          in subs :: sub' cdr1 cdr2 false
          else if car1 < car2 then let subs = (car1 + 10) - car2
          in subs :: sub' cdr1 cdr2 true
          else 0 :: sub' cdr1 cdr2 false
        end
        | car1::cdr1, car2::cdr2, true  ->  begin
          if (car1 - 1) > car2 then let subs = (car1 - 1) - car2
          in subs :: sub' cdr1 cdr2 false
          else if (car1 - 1) < car2 then let subs = (car1 -1 + 10) - car2
          in subs :: sub' cdr1 cdr2 true
          else 0 :: sub' cdr1 cdr2 false
        end 
        | _, _, _  -> [0];;


    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 <> neg2     (* Addition *)
        then Bigint (neg1, add' value1 value2 0)
        else let cmp = compareTo value1 value2 in
            if cmp = 1 then Bigint (neg1, sub' value1 value2 false)
            else if cmp = -1 then   (* Sign is flipped*)
                Bigint (flip_sign neg2, sub' value2 value1 false)
            else zero

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else sub (Bigint (neg1, value1)) (Bigint (flip_sign neg2, value2)) 
        (* Should become substarction, sign is flipped, sub will flip it back*)

   

    let mul = add

    let div = add

    let rem = add

    let pow = add

    

end

