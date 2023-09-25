open Arrow;;
(* open [@@warning "-32"] List;; *)
open Imp.Any;;




type 's static_parser = SP of (bool) * ('s list)
type ('s, 'a, 'b) dynamic_parser = DP of (('a * 's list) -> ('b * 's list))
type ('s, 'a, 'b) parser = P of ('s static_parser) * (('s, 'a, 'b) dynamic_parser)

let spChar c = SP (false, [c])

exception Error

let dpCharA c = DP (fun input -> match input with 
                                    | (_,(_::xs)) -> (c, xs)
                                    | _ -> raise Error)


let runParser' p a l = match (p, l) with 
                        | ((P ((SP (emp, _)), (DP p))), []) -> if emp then (Some (p (a, []))) else None
                        | ((P ((SP (_, start)), (DP p))), (x::xs)) -> if (List.mem x start) then (Some (p (a, (x::xs)))) else None

implicit module ParseArrow {S : Any} : Arrow with type ('a, 'b) t = (S.t, 'a, 'b) parser = struct 
    
    type ('a, 'b) t = (S.t, 'a, 'b) parser

    let arr f = P (SP (true, []), (DP (fun (b, s) -> (f b, s))))

    let first (P (sp, (DP p))) = P (sp, 
                                  DP (fun ((b,d),s) -> 
                                        let (c, s') = p (b,s)
                                  in ((c,d),s')
                                        ))
    let ( >>> ) ((P ((SP (empty1, start1)), (DP p1))) : ('b, 'c) t) ((P ((SP (empty2, start2)), (DP p2))) : ('c, 'd) t) : ('b,'d) t =
        P ((SP ((empty1 && empty2),
              (if not empty1 then start1 else List.append start1 start2))),
          (DP (fun x -> p2(p1(x)))))
end 

