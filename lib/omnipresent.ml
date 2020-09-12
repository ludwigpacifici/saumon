let ( >> ) f g x = g (f x)
let ( ||> ) (x, y) f = f x y
let ( <| ) f x = f x
