(*
 * Zadanie domowe 1, czesc 1
 *  structure file
 *)
structure id283112 :> PART_ONE =
struct
  exception NotImplemented

  datatype 'a tree= Leaf of 'a | Node of 'a tree * 'a * 'a tree

  fun sum n = 
	if n = 1 then 1
	else sum(n-1) + n

  fun fac n = 
	if n = 0 then 1
	else fac(n-1) *n

  fun fib n = 
	if n = 1 then 1
	else if n =2 then 2 
	else fib(n-1) + fib(n-2)	

  fun gcd (n, m) =
	if m = n then m
	else if n>m then gcd(n-m, m)
	else gcd(n, m-n)

  fun max (l: int list)=
	case l of
	nil=>0
	| head::tail=>
		if tail=[] orelse head>max tail
		then head
		else max tail
	 
  fun sumTree (t: int tree)=
	case t of
	Leaf l =>l
	| Node (l, v, r) => v + sumTree(l) + sumTree(r)

  fun depth (t:'a tree) = 
	case t of
	Leaf l=>0
	| Node(l, _, p)=>
		1+ (fn(x,y)=> if x > y then x else y)(depth l, depth p);

  fun binSearch (t:int tree)(x: int) = (*szukanie elementu x w drzewie binarnym*)
	case t of
	Leaf l => l=x
	| Node(l, v, r)=>
		if v=x then true
		else if x<v then binSearch l x
		else binSearch r x

  fun preorder (t:'a tree)  = 
	case t of 
		Leaf l=> [l]
		| Node(l,v,r)=> [v] @ preorder l @ preorder r

  fun listAdd  [] (b: int list )= b
	| listAdd(a:int list) [] = a
	| listAdd(a:int list as ha :: ta)(b:int list as hb :: tb)=
		(ha + hb) :: listAdd ta tb

   fun insert (m:int) [] = [m]
    | insert (m:int) (l:int list as h::t) =
        if m < h then m :: l
        else h :: insert m t

  fun insort (l:int list) =
    let
      fun sort [] res = res
        | sort (h::t) res = sort t (insert h res)
    in
      sort l nil
    end


  fun compose f g  = (fn x=> g (f x))

  fun curry f x y  =  f(x, y) 

  fun uncurry f (x,y) = f x y

  fun multifun f n = 
    if n = 1 then (fn x => f x)
    else (fn x =>f ( (multifun f (n-1)) x ))

  fun ltake _ 0 = []
    |ltake [] _ = []
    |ltake (x::xs) n = x::(ltake xs (n-1))

  fun lall _ [] = true
    |lall f (x::xs)= if (f x) then (lall f xs) else false

  fun lmap _ [] = []
    |lmap f (x::xs)= (f x)::(lmap f xs)

  fun lrev [] = []
    |lrev (x::xs) = (lrev xs) @ [x]

  fun lzip ([],_) = []
    |lzip (_,[]) = []
    |lzip ((x::xs),(y::ys)) = (x,y)::(lzip (xs,ys))

  fun split [] = ([],[])
    |split [x] = ([x],[])
    |split (x1::x2::xs) = let val (x1s,x2s) = split xs
                        in ((x1::x1s),(x2::x2s)) end;
  fun cartprod _ [] =[]
    |cartprod [] _ = []
    |cartprod (x::xs) ys = (lmap (fn y=> (x,y)) ys) @ (cartprod xs ys)

end
