open Arrows.Arrow;;






(* Computation with addition *)
let addA {A : Arrow} f g = A.(f &&& g >>> arr (fun (x, y) -> x + y))


(* This is testing arrows for the function type *)

(* think about a b c as a computation that takes in 'b and returns 'c*)

let func_test1 (f : int -> int) (g : int -> int) (x : int) : int * int = (f &&& g) x;;
let func_test2 (f : int -> int) (g : int -> int) (x : int * int) : int * int = (f *** g) x;;


let () = 
    begin 
      let out = func_test1 (fun x -> x + 1) (fun x -> x + 2) 3 in
      assert (out = (4, 5));
      let out = func_test2 (fun x -> x + 1) (fun x -> x + 2) (1,2) in
      assert (out = (2, 4));
    end 

let z = ((fun x -> x + 1) &&& (fun x -> x + 10)) 30

let double_op = ((fun x -> x + 1) *** (fun x -> x + 10)) (30, 50)


let compose f g x = (f >>> g) x
(* performs f (g (x))*)
