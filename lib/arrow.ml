open Imp.Control;;

let swap (x, y) = (y, x)

module type Arrow = sig 
  type ('a, 'b) t

  val arr : ('b -> 'c) -> ('b, 'c) t
  val ( >>> ) : ('b, 'c) t -> ('c, 'd) t -> ('b, 'd) t
  val first : ('b, 'c) t -> (('b * 'd), ('c * 'd)) t
end 


let arr {A : Arrow} = A.arr
let ( >>> ) {A : Arrow} = A.( >>> )
let first {A : Arrow} = A.first

let second {A : Arrow} f = A.(arr swap >>> first f >>> arr swap)

let ( *** ) {A : Arrow} f g = A.(first f >>> second g)
let ( &&& ) {A : Arrow} f g = A.(arr (fun x -> (x, x)) >>> f *** g)

module Kleisli (M : Monad) = struct 
  type ('a, 'b) t = 'a -> 'b M.t 
end 


implicit module MonadArrow {M : Monad} : Arrow with type ('a, 'b) t = ('a, 'b) Kleisli(M).t 
  = struct 
    type ('a, 'b) t = ('a, 'b) Kleisli(M).t

    let arr f = fun x -> M.return (f x)

    let ( >>> ) f g = fun x -> M.bind (f x) g
    let first f = fun (x, y) -> M.bind (f x) (fun x' -> M.return (x', y))
end


type ('a, 'b) either = Left of 'a | Right of 'b

module type ArrowApply = sig 
  include Arrow
  val app : (((('b, 'c) t) * 'b), 'c) t
end

let app {A : ArrowApply} = A.app

module type ArrowChoice = sig 
  include Arrow
  val left : ('b, 'c) t -> (('b, 'd) either, ('c, 'd) either) t
end

let left {A : ArrowChoice} = A.left

let right {A : ArrowChoice} f = let mirror = fun x -> match x with Left x -> Right x | Right x -> Left x in
  A.(arr mirror >>> left f >>> arr mirror)

let ( +++ ) {A : ArrowChoice} f g = A.(left f >>> right g)

let ( ||| ) {A : ArrowChoice} f g = let untag = fun x -> match x with Left x -> x | Right x -> x in
                                          A.(arr untag >>> f +++ g)

let addA {A : Arrow} f g = A.(f &&& g >>> arr (fun (x, y) -> x + y))



