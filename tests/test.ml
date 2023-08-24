open Arrows.Arrow;;






(* Computation with addition *)
let addA {A : Arrow} f g = A.(f &&& g >>> arr (fun (x, y) -> x + y))