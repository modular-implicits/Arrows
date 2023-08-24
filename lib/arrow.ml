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

let returnA {A : Arrow} = A.arr (fun x -> x)
let ( ^>> ) {A : Arrow} f g = A.(arr f >>> g)
let ( >>^ ) {A : Arrow} f g = A.(f >>> arr g)

let ( <<< ) {A : Arrow} f g = A.(g >>> f)

let ( ^<< ) {A : Arrow} f g = A.(f <<< arr g)
let ( <<^ ) {A : Arrow} f g = A.(arr f <<< g)



(* Right I want to be able to have *)

(*
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
instance Monad m => Arrow (Kleisli m)

maybe this is equivalent but I can't tell
*)

module Kleisli (M: Monad) = struct 
  type ('a,'b) t = 'a -> 'b M.t 
end 

let runKleisli {M:Monad} : ('a,'b) Kleisli(M).t -> 'a -> 'b M.t =
  fun k -> k


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

(* Want to test this stuf out with monad definition for pair *)


module type ArrowZero = sig 
  include Arrow
  val zero : ('b, 'c) t
end

module type ArrowPlus = sig 
  include ArrowZero
  val ( <+> ) : ('b, 'c) t -> ('b, 'c) t -> ('b, 'c) t
end

module type ArrowLoop = sig 
  include Arrow
  val loop : (('b * 'd), ('c * 'd)) t -> ('b, 'c) t
end


implicit module ArrowFunction : sig
   include Arrow with type ('a, 'b) t = 'a -> 'b
   include ArrowChoice with type ('a, 'b) t := ('a, 'b) t
   include ArrowApply with type ('a, 'b) t := ('a, 'b) t
   include ArrowLoop with type ('a, 'b) t := ('a, 'b) t
  end = struct 
    type ('a, 'b) t = 'a -> 'b

  let arr f = f
  let ( >>> ) f g = fun x -> g (f x)
  let first f = fun (x, y) -> (f x, y)

  (* Arrow Choice *)
  let left f = fun x -> match x with Left x -> Left (f x) | Right x -> Right x

  (* Arrow Apply *)
  let app (f,x) = f x

  (* Arrow Loop - doesn't work here *)
  (* Probably the best that can be done here, its not quite as good as haskell but close*)
  let loop f b = let (c, _) = f (b, Obj.magic ()) in c
end

implicit module MonadArrow {M : Monad} : sig
  include Arrow with type ('a, 'b) t = ('a, 'b) Kleisli(M).t
  include ArrowChoice with type ('a, 'b) t := ('a, 'b) t
  include ArrowApply with type ('a, 'b) t := ('a, 'b) t

  end = struct 
  type ('a, 'b) t = ('a, 'b) Kleisli(M).t 
  let arr f = fun x -> M.return (f x) 
  let ( >>> ) f g = fun x -> M.bind (f x) g 
  let first f = fun (x, y) -> M.bind (f x) (fun x' -> M.return (x', y)) 

  let left (f : ('b, 'c) t) : (('b, 'd) either, ('c, 'd) either) t  = function
    | Left x -> M.bind (f x) (fun x' -> M.return (Left x'))
    | Right x -> M.return (Right x)

  let app (f, x) = f x
end

implicit module MonadPlusArrow {M : Monad_plus} : sig
  include Arrow with type ('a, 'b) t = ('a, 'b) Kleisli(M).t
  include ArrowZero with type ('a, 'b) t := ('a, 'b) t
  end = struct 
  type ('a, 'b) t = ('a, 'b) Kleisli(M).t 
  let arr f = fun x -> M.return (f x) 
  let ( >>> ) f g = fun x -> M.bind (f x) g 
  let first f = fun (x, y) -> M.bind (f x) (fun x' -> M.return (x', y)) 
  let zero = fun x -> M.mzero
end

(* Hopefully this works when Monad_fix merged!*)
(*
implicit module MonadFixArrow {M : Monad_fix} : sig 
  include Arrow with type ('a 'b) t = ('a, 'b) Kleisli(M).t
  include ArrowLoop with type ('a, 'b) t := ('a, 'b) t
end = struct 
  type ('a, 'b) t = ('a, 'b) Kleisli(M).t 
  let arr f = fun x -> M.return (f x) 
  let ( >>> ) f g = fun x -> M.bind (f x) g 
  let first f = fun (x, y) -> M.bind (f x) (fun x' -> M.return (x', y)) 
  let loop f b = let f' x y = f (x, snd y) in
          M.liftM fst (M.mfix (f' b))

end
*)