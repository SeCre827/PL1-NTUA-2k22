   
(* Merges a list of lists based on 0 index  --> (List.nth(k,0)) *)
fun mergeSort nil = nil
   | mergeSort [e] = [e]
   | mergeSort theList =
      let
         (* From the given list make a pair of lists
         * (x,y), where half the elements of the
         * original are in x and half are in y. *)
         fun halve nil = (nil, nil)
            | halve [a] = ([a], nil)
            | halve (a::b::cs) =
               let
                  val (x, y) = halve cs
               in
                  (a::x, b::y)
               end
         (* Merge two sorted lists of integers into
         * a single sorted list. *)
         fun merge (nil, ys) = ys
            | merge (xs, nil) = xs
            | merge (x::xs, y::ys) =
               if List.nth(x,0) < List.nth(y,0) then x :: merge(xs, y::ys)
                  else y :: merge(x::xs, ys)
         val (x, y) = halve theList
      in
         merge (mergeSort x, mergeSort y)
      end



(* Find the (greatest) parent of i.    *)
  (*       2       6   *)
  (*      / \     /    *)
  (*     1   4   5     *)     (*val b = [3, 2, 2, 1, 1, 5 ,6]; *)
  (*    /              *)   (*for 0,3,1 we get 2            *)
  (*   3               *)   (*for 5,6 we get 6              *)
  (*  /                *)
  (* 0                 *)
(* get 2 nodes and checks if they have the same parent *)
  fun find (ls, i) = 
      if Array.sub(ls, i) = i 
      then i
      else find(ls, Array.sub(ls, i))

(* parent rank are arrays *)
fun union parent rank p q =
   let 
     val i = find(parent, p)
     val j = find(parent,q)
   in
(* update (arr, i, x)  sets the i(th) element of the array arr to x *)
      if (Array.sub(rank, i) < Array.sub(rank, j))
         then (Array.update(parent, i, j); Array.update(rank, j , (Array.sub(rank,j) + Array.sub(rank,i))))
      else (Array.update(parent, j, i);Array.update(rank,i,(Array.sub(rank,i) + Array.sub(rank,j))))
   end


val a = [[4,2,2], [1,2,3], [3,2,4], [1,2,3]]
val parent = [3,3,2,3,4]
val rank = [1,2,1,2,2]
val p = 2
val q = 4
(* val a = Array.fromList parent;
val a = Array.fromList rank;
union a b p q; *)