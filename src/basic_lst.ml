(*
   Copyright (C) 2024 International Digital Economy Academy.
   This program is licensed under the MoonBit Public Source
   License as published by the International Digital Economy Academy,
   either version 1 of the License, or (at your option) any later
   version. This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the MoonBit
   Public Source License for more details. You should have received a
   copy of the MoonBit Public Source License along with this program. If
   not, see
   <https://www.moonbitlang.com/licenses/moonbit-public-source-license-v1>.
*)

(**
This file introduces more facilities than the standard library.

You might find there are some functions whose order of arguments are different
from OCaml standard library. Please avoid them, and replace them with the functions
in standard `List` module when possible. Thank you.
*)

module Arr = Basic_arr

let map l f = List.map f l

let has_string (l : string list) query = List.mem query l

let rec map_split xs f =
  match xs with
  | [] -> ([], [])
  | x :: xs ->
      let c, d = f x in
      let cs, ds = map_split xs f in
      (c :: cs, d :: ds)

let rec mapi_aux lst i f tail =
  match lst with
  | [] -> tail
  | a :: l ->
      let r = f i a in
      r :: mapi_aux l (i + 1) f tail

let mapi lst f = List.mapi f lst
let mapi_append lst f tail = mapi_aux lst 0 f tail

let rec last xs =
  match xs with
  | x :: [] -> x
  | _ :: tl -> last tl
  | [] -> invalid_arg __FUNCTION__

let rec map_append l1 l2 f =
  match l1 with
  | [] -> l2
  | a :: tl -> f a :: map_append tl l2 f

let fold_right l acc f = List.fold_right f l acc

let fold_right2 l r acc f = List.fold_right2 f l r acc

let map2 l r f = List.map2 f l r

let rec fold_left_with_offset l accu i f =
  match l with
  | [] -> accu
  | a :: l -> fold_left_with_offset l (f a accu i) (i + 1) f

let rec filter_map xs (f : 'a -> 'b option) =
  match xs with
  | [] -> []
  | y :: ys -> (
      match f y with None -> filter_map ys f | Some z -> z :: filter_map ys f)

let rec exclude (xs : 'a list) (p : 'a -> bool) : 'a list =
  match xs with
  | [] -> []
  | x :: xs -> if p x then exclude xs p else x :: exclude xs p

let rec exclude_with_val l p =
  match l with
  | [] -> None
  | a0 :: xs -> (
      if p a0 then Some (exclude xs p)
      else
        match xs with
        | [] -> None
        | a1 :: rest -> (
            if p a1 then Some (a0 :: exclude rest p)
            else
              match exclude_with_val rest p with
              | None -> None
              | Some rest -> Some (a0 :: a1 :: rest)))

let rec same_length xs ys =
  match (xs, ys) with
  | [], [] -> true
  | _ :: xs, _ :: ys -> same_length xs ys
  | _, _ -> false

let rec split_at_last_aux acc x =
  match x with
  | [] -> invalid_arg __FUNCTION__
  | x :: [] -> (List.rev acc, x)
  | y0 :: ys -> split_at_last_aux (y0 :: acc) ys

let split_at_last (x : 'a list) =
  match x with
  | [] -> invalid_arg __FUNCTION__
  | a0 :: [] -> ([], a0)
  | [ a0; a1 ] -> ([ a0 ], a1)
  | [ a0; a1; a2 ] -> ([ a0; a1 ], a2)
  | [ a0; a1; a2; a3 ] -> ([ a0; a1; a2 ], a3)
  | [ a0; a1; a2; a3; a4 ] -> ([ a0; a1; a2; a3 ], a4)
  | a0 :: a1 :: a2 :: a3 :: a4 :: rest ->
      let rev, last = split_at_last_aux [] rest in
      (a0 :: a1 :: a2 :: a3 :: a4 :: rev, last)

let filter_mapi xs f =
  let rec aux i xs =
    match xs with
    | [] -> []
    | y :: ys -> (
        match f y i with
        | None -> aux (i + 1) ys
        | Some z -> z :: aux (i + 1) ys)
  in
  aux 0 xs

let rec filter_map2 xs ys (f : 'a -> 'b -> 'c option) =
  match (xs, ys) with
  | [], [] -> []
  | u :: us, v :: vs -> (
      match f u v with
      | None -> filter_map2 us vs f
      | Some z -> z :: filter_map2 us vs f)
  | _ -> invalid_arg __FUNCTION__

let rec rev_map_append l1 l2 f =
  match l1 with [] -> l2 | a :: l -> rev_map_append l (f a :: l2) f

let rec flat_map_aux f acc append lx =
  match lx with
  | [] -> List.rev_append acc append
  | a0 :: rest ->
      let new_acc =
        match f a0 with
        | [] -> acc
        | a0 :: [] -> a0 :: acc
        | [ a0; a1 ] -> a1 :: a0 :: acc
        | a0 :: a1 :: a2 :: rest -> List.rev_append rest (a2 :: a1 :: a0 :: acc)
      in
      flat_map_aux f new_acc append rest

let concat_map lx f = flat_map_aux f [] [] lx
let flat_map_append lx ~init:append ~f = flat_map_aux f [] append lx

let rec flat_map_auxi f acc append lx index =
  match lx with
  | [] -> List.rev_append acc append
  | a0 :: rest ->
      let new_acc =
        match f index a0 with
        | [] -> acc
        | a0 :: [] -> a0 :: acc
        | [ a0; a1 ] -> a1 :: a0 :: acc
        | a0 :: a1 :: a2 :: rest -> List.rev_append rest (a2 :: a1 :: a0 :: acc)
      in
      flat_map_auxi f new_acc append rest (index + 1)

let concat_mapi lx f = flat_map_auxi f [] [] lx 0
let flat_mapi_append lx ~init:append ~f = flat_map_auxi f [] append lx 0


let rec group (eq : 'a -> 'a -> bool) lst =
  match lst with [] -> [] | x :: xs -> aux eq x (group eq xs)

and aux eq (x : 'a) (xss : 'a list list) : 'a list list =
  match xss with
  | [] -> [ [ x ] ]
  | (y0 :: _ as y) :: ys -> if eq x y0 then (x :: y) :: ys else y :: aux eq x ys
  | _ :: _ -> assert false

let stable_group lst eq = group eq lst |> List.rev

let rec drop h n =
  if n < 0 then invalid_arg __FUNCTION__
  else if n = 0 then h
  else match h with [] -> invalid_arg __FUNCTION__ | _ :: tl -> drop tl (n - 1)

let rec find_first x p =
  match x with [] -> None | x :: l -> if p x then Some x else find_first l p

let find_first_with_index x p =
  let rec loop i xs p =
    match xs with
    | [] -> None
    | a :: l -> if p a then Some (i, a) else loop (i + 1) l p
  in
  loop 0 x p

let rec find_first_not xs p =
  match xs with
  | [] -> None
  | a :: l -> if p a then find_first_not l p else Some a

let rec find_exn x p =
  match x with
  | [] -> invalid_arg __FUNCTION__
  | x :: l -> if p x then x else find_exn l p

let find_index_exn xs p =
  let rec loop i xs p =
    match xs with
    | [] -> invalid_arg __FUNCTION__
    | a :: l -> if p a then i else loop (i + 1) l p
  in
  loop 0 xs p

let rec rev_iter l f =
  match l with
  | [] -> ()
  | x1 :: [] -> f x1
  | x1 :: tail ->
      rev_iter tail f;
      f x1

let iter l f = List.iter f l

let iteri l f = List.iteri f l

let iter2 l1 l2 f = List.iter2 f l1 l2

let rec for_all lst p =
  match lst with [] -> true | a :: l -> p a && for_all l p

let rec for_all_snd lst p =
  match lst with [] -> true | (_, a) :: l -> p a && for_all_snd l p

let rec for_all2_no_exn l1 l2 p =
  match (l1, l2) with
  | [], [] -> true
  | a1 :: l1, a2 :: l2 -> p a1 a2 && for_all2_no_exn l1 l2 p
  | _, _ -> false

let rec find_opt xs p =
  match xs with
  | [] -> None
  | x :: l -> ( match p x with Some _ as v -> v | None -> find_opt l p)

let rec find_def xs p def =
  match xs with
  | [] -> def
  | x :: l -> ( match p x with Some v -> v | None -> find_def l p def)

let rec split_map l f =
  match l with
  | [] -> ([], [])
  | x1 :: [] ->
      let a0, b0 = f x1 in
      ([ a0 ], [ b0 ])
  | [ x1; x2 ] ->
      let a1, b1 = f x1 in
      let a2, b2 = f x2 in
      ([ a1; a2 ], [ b1; b2 ])
  | [ x1; x2; x3 ] ->
      let a1, b1 = f x1 in
      let a2, b2 = f x2 in
      let a3, b3 = f x3 in
      ([ a1; a2; a3 ], [ b1; b2; b3 ])
  | [ x1; x2; x3; x4 ] ->
      let a1, b1 = f x1 in
      let a2, b2 = f x2 in
      let a3, b3 = f x3 in
      let a4, b4 = f x4 in
      ([ a1; a2; a3; a4 ], [ b1; b2; b3; b4 ])
  | x1 :: x2 :: x3 :: x4 :: x5 :: tail ->
      let a1, b1 = f x1 in
      let a2, b2 = f x2 in
      let a3, b3 = f x3 in
      let a4, b4 = f x4 in
      let a5, b5 = f x5 in
      let ass, bss = split_map tail f in
      (a1 :: a2 :: a3 :: a4 :: a5 :: ass, b1 :: b2 :: b3 :: b4 :: b5 :: bss)

let rec split_map2 l r f =
  match (l, r) with
  | [], [] -> ([], [])
  | x1 :: [], y1 :: [] ->
      let a0, b0 = f x1 y1 in
      ([ a0 ], [ b0 ])
  | [ x1; x2 ], [ y1; y2 ] ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      ([ a1; a2 ], [ b1; b2 ])
  | [ x1; x2; x3 ], [ y1; y2; y3 ] ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      let a3, b3 = f x3 y3 in
      ([ a1; a2; a3 ], [ b1; b2; b3 ])
  | [ x1; x2; x3; x4 ], [ y1; y2; y3; y4 ] ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      let a3, b3 = f x3 y3 in
      let a4, b4 = f x4 y4 in
      ([ a1; a2; a3; a4 ], [ b1; b2; b3; b4 ])
  | x1 :: x2 :: x3 :: x4 :: x5 :: tailx, y1 :: y2 :: y3 :: y4 :: y5 :: taily ->
      let a1, b1 = f x1 y1 in
      let a2, b2 = f x2 y2 in
      let a3, b3 = f x3 y3 in
      let a4, b4 = f x4 y4 in
      let a5, b5 = f x5 y5 in
      let ass, bss = split_map2 tailx taily f in
      (a1 :: a2 :: a3 :: a4 :: a5 :: ass, b1 :: b2 :: b3 :: b4 :: b5 :: bss)
  | _, _ -> invalid_arg __FUNCTION__

(** In a list of key-value pairs, find the value associated with key `k`. *)
let rec assoc_by_opt lst comp k =
  match lst with
  | [] -> None
  | (k1, v1) :: rest -> if comp k1 k then Some v1 else assoc_by_opt rest comp k

let assoc_str lst str = assoc_by_opt lst String.equal str

let rec exists l p = match l with [] -> false | x :: xs -> p x || exists xs p

let rec exists_fst l p =
  match l with [] -> false | (a, _) :: l -> p a || exists_fst l p

let rec exists_snd l p =
  match l with [] -> false | (_, a) :: l -> p a || exists_snd l p

let rec concat_append (xss : 'a list list) (xs : 'a list) : 'a list =
  match xss with [] -> xs | l :: r -> List.append l (concat_append r xs)

let fold_left l init f = List.fold_left f init l

let reduce_from_left lst fn =
  match lst with
  | first :: rest -> fold_left rest first fn
  | _ -> invalid_arg __FUNCTION__

let rec fold_left2 l1 l2 accu f =
  match (l1, l2) with
  | [], [] -> accu
  | a1 :: l1, a2 :: l2 -> fold_left2 l1 l2 (f a1 a2 accu) f
  | _, _ -> invalid_arg __FUNCTION__

let singleton_exn xs = match xs with x :: [] -> x | _ -> assert false

let rec mem_string (xs : string list) (x : string) =
  match xs with [] -> false | a :: l -> a = x || mem_string l x

let rec mem_int (xs : int list) (x : int) =
  match xs with [] -> false | a :: l -> a = x || mem_int l x

let filter lst p = List.filter p lst

let rec check_duplicate (xs : string list) =
  match xs with
  | [] -> false
  | x :: rest -> Stdlib__List.exists (( = ) x) rest || check_duplicate rest

let rec check_duplicate_opt ~equal xs =
  match xs with
  | [] -> None
  | x :: rest ->
      if Stdlib.List.exists (equal x) rest then Some x
      else check_duplicate_opt ~equal rest

let stable_sort l cmp =
  match l with
  | [] -> []
  | x :: [] -> [ x ]
  | [ x1; x2 ] -> if cmp x1 x2 <= 0 then [ x1; x2 ] else [ x2; x1 ]
  | [ x1; x2; x3 ] ->
      if cmp x1 x2 <= 0 then
        if cmp x2 x3 <= 0 then [ x1; x2; x3 ]
        else if cmp x1 x3 <= 0 then [ x1; x3; x2 ]
        else [ x3; x1; x2 ]
      else if cmp x1 x3 <= 0 then [ x2; x1; x3 ]
      else if cmp x2 x3 <= 0 then [ x2; x3; x1 ]
      else [ x3; x2; x1 ]
  | l ->
      let arr = Array.of_list l in
      Array.stable_sort cmp arr;
      Array.to_list arr

let stable_sort_as_array l ~cmp =
  match l with
  | [] -> [||]
  | x :: [] -> [| x |]
  | [ x1; x2 ] -> if cmp x1 x2 <= 0 then [| x1; x2 |] else [| x2; x1 |]
  | [ x1; x2; x3 ] ->
      if cmp x1 x2 <= 0 then
        if cmp x2 x3 <= 0 then [| x1; x2; x3 |]
        else if cmp x1 x3 <= 0 then [| x1; x3; x2 |]
        else [| x3; x1; x2 |]
      else if cmp x1 x3 <= 0 then [| x2; x1; x3 |]
      else if cmp x2 x3 <= 0 then [| x2; x3; x1 |]
      else [| x3; x2; x1 |]
  | l ->
      let arr = Array.of_list l in
      Array.stable_sort cmp arr;
      arr

let rec unsafe_take n xs =
  match xs, n with
  | _, 0 -> []
  | [], _ -> invalid_arg __FUNCTION__
  | x :: tl, _ -> x :: unsafe_take (n - 1) tl

let take n xs =
  if n >= List.length xs then xs else if n <= 0 then [] else unsafe_take n xs

(** Accumulated sum, same as numpy.cumsum *)
let cumsum lst =
  let rec aux acc sum = function
    | [] -> List.rev acc
    | x :: xs -> aux ((sum + x) :: acc) (sum + x) xs
  in
  aux [] 0 lst