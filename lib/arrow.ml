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


type ('a, 'b) kleisli = {M : Monad} -> 'a -> 'b M.t

type ('a, 'b) either = Left of 'a | Right of 'b

module type ArrowApply = sig 
  include Arrow
  val app : (((('b, 'c) t) * 'b), 'c) t
end

module type ArrowChoice = sig 
  include Arrow
  val left : ('b, 'c) t -> (('b, 'd) either, ('c, 'd) either) t
end

implicit module MonadArrow {M : Monad} : Arrow with type ('a, 'b) t = ('a, 'b) kleisli = struct 
  type ('a, 'b) t = ('a, 'b) kleisli

  let arr f = fun x -> M.return (f x)

  let ( >>> ) f g = fun x -> M.bind (f x) g
  let first f = fun (x, y) -> M.bind (f x) (fun x' -> M.return (x', y))
end


