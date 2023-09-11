Arrows

OCaml Arrows library using modular implicits:

Some examples using arrows as functions:
```ocaml
let tuple = ((fun x -> x + 1) &&& (fun x -> x + 10)) 30

let double_op = ((fun x -> x + 1) *** (fun x -> x + 10)) (30, 50)

let compose f g x = (f >>> g) x (* performs f (g (x))*)

```


Some papers used:

http://www.staff.city.ac.uk/~ross/papers/notation.pdf
https://www.sciencedirect.com/science/article/pii/S0167642399000234

