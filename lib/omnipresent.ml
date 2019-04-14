let ( >> ) f g x = g (f x)

let ( ||> ) (x, y) f = f x y
