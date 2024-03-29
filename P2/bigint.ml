(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(**
    Author: Cesar Neri       ID: 1513805      email:ceneri@ucsc.edu
*)

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


    (*****************Helper methods**********)

    (*!! https://ocaml.org/learn/tutorials/99problems.html*)
    let rec fold_until f acc n = function
        | [] -> (acc, [])
        | h :: t as l -> if n = 0 then (acc, l)
                         else fold_until f (f acc h) (n-1) t
  
    let slice list i k =
        let _, list = fold_until (fun _ _ -> []) [] i list in
        let taken, _ = fold_until (fun acc h -> h :: acc) [] 
            (k - i + 1) list in
        List.rev taken;;

    (*!! https://ocaml.org/learn/tutorials/99problems.html*)
    let rec insert_at x n = function
        | [] -> [x]
        | h :: t as l -> if n = 0 then x :: l 
            else h :: insert_at x (n-1) t;;

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

    (*Multiplys a list number by 10*)
    let rec timesTen list1 =
        insert_at 0 0 list1

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

    let flip_sign sign = match sign with
        | Pos -> Neg
        | Neg -> Pos

    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []                -> 0
        | list1, list2          -> begin
            if car list1 > car list2 then 1
            else if car list1 < car list2 then -1
            else cmp' (cdr list1) (cdr list2)
        end

    (**Similar to Java method returns:
        -   1   if value1 > value2
        - (-1)  if value1 < value2
        -   0   if value1 = value2*)
    let cmp value1 value2 = 
        let list_int1 = reverse (trimzeros value1)  
        in let list_int2 = reverse (trimzeros value2)
        in if List.length list_int1 > List.length list_int2
        then 1
        else if  List.length list_int1 < List.length list_int2
            then -1
            else cmp' list_int1 list_int2;;
    

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
          else if (car1 - 1) < car2 then let subs = 
            (car1 -1 + 10) - car2
          in subs :: sub' cdr1 cdr2 true
          else 0 :: sub' cdr1 cdr2 false
        end 
        | _, _, _  -> [0];;

    (* Multiplies a number by a single digit number *)
    let rec single_mul list1 digit carry = 
        match (list1, digit, carry) with
            | list1, 0, 0                       -> []
            | [], digit, 0                      -> []
            | [], digit, carry                  -> [carry]
            | car1::cdr1, digit, carry          -> begin
              let sum = (car1 * digit) + carry
              in sum mod radix :: single_mul cdr1 digit (sum / radix)
            end;;

    let rec mul' list1 list2  = match (list1, list2) with
        | list1, []                 -> []
        | [], list2                 -> []
        | car1::cdr1, car2::cdr2    -> begin
          let single = single_mul list1 car2 0
          in let rem = timesTen list1
          in add' single (mul' rem cdr2) 0
        end;;

    (*Multiplys a list number by 2*)
    let double_val list = 
        mul' list [2]

    let rec div_and_rem' (dividend, divisor, pow2) =
        if (cmp divisor dividend) = 1
        then [0], dividend
        else let quotient, remainder =
          div_and_rem' (dividend, double_val divisor, double_val pow2)
          in  if (cmp remainder divisor) = -1
          then quotient, remainder
          else (add' quotient pow2 0),
            (sub' remainder divisor false)

    (*Returns tuple for both quotient and the remainder *)
    let div_and_rem (dividend, divisor) = 
        div_and_rem' (dividend, divisor, [1])

    let div_helper value1 value2 =
        let quotient, remainder = div_and_rem (value1, value2)
        in quotient

    (*Same as div_helper but return remainder not quotient*)
    let rem_helper value1 value2 =
        let quotient, remainder = div_and_rem (value1, value2)
        in remainder

    let rec pow' value1 value2 counter = 
        if cmp value2 counter = -1 then [1]
        else if cmp value2 counter = 0 then value1
        else let curr = mul' value1 value1
        in let new_counter = add' counter [2] 0 
        in mul' curr (pow' value1 value2 new_counter)
    
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 <> neg2     (* Addition *)
        then Bigint (neg1, add' value1 value2 0)
        else let cmp = cmp value1 value2 in
            if cmp = 1 then Bigint (neg1, sub' value1 value2 false)
            else if cmp = -1 then   (* Sign is flipped*)
                Bigint (flip_sign neg2, sub' value2 value1 false)
            else zero

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else sub (Bigint (neg1, value1)) 
                 (Bigint (flip_sign neg2, value2)) 
        (* Should become substraction, sign is flipped
           sub will flip it back*)
 
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if value1 = [] || value2 = []
        then zero
        else if neg1 = neg2
        then Bigint (Pos, mul' value1 value2)
        else Bigint (Neg, mul' value1 value2)

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if value1 = [] 
        then zero
        else if value2 = []
        then failwith "ocamldc: divide by zero"
        else if neg1 = neg2
        then Bigint (Pos, div_helper value1 value2)
        else Bigint (Neg, div_helper value1 value2) 

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if value1 = [] 
        then zero
        else if value2 = []
        then failwith "ocamldc: remainder by zero"
        else if neg1 = neg2
        then Bigint (Pos, trimzeros( rem_helper value1 value2 ))
        else Bigint (Neg, trimzeros( rem_helper value1 value2 ))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if value1 = [] || neg2 = Neg
        then zero
        else if value2 = []
        then Bigint (Pos, [1])
        else  Bigint (Pos, pow' value1 value2 [1] )     
                (* Counter starts at 0*)
   
end
