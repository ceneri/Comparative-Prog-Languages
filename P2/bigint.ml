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

    

    (*let string_of_bigint_help' value_list count =
        let len = length value_list 
        in if count < 70
            then 
                   in  let return_str = strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
                            in add_newline return_str)*)


    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else zero       (* Should become substarction*)

    let sub = add     

    let mul = add

    let div = add

    let rem = add

    let pow = add

    (*Helper metyhods*)

    (**Similar to Java method returns:
        -   1   if value1 > value2
        - (-1)  if value1 < value2
        -   0   if value1 = value2

    
    let compareTo value1 value2 = *)

end

