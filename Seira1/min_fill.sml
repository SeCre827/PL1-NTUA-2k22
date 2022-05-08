fun min_fill file = 
  let
    fun parse file =
        let 
            fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
            
            val inStream = TextIO.openIn file
            val (n,m) = (readInt inStream, readInt inStream)
            
            fun readInts 0 acc = List.rev acc
                | readInts i acc = readInts (i - 3) ([(readInt inStream) -1, (readInt inStream) - 1, (readInt inStream) ] :: acc)

            val inputList = readInts (3*m) []
            val _ = TextIO.closeIn inStream
        in
            (n, m, inputList)
        end

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
                  if List.nth(x,2) < List.nth(y,2) then x :: merge(xs, y::ys)
                      else y :: merge(x::xs, ys)
            val (x, y) = halve theList
          in
            merge (mergeSort x, mergeSort y)
      end

    (* Find the (greatest) parent of i.    *)
      (*       2       6   *)
      (*      / \     /    *)
      (*     1   4   5     *)   (*val b = [3, 2, 2, 1, 1, 5 ,6]; *)
      (*    /              *)   (*for 0,3,1 we get 2            *)
      (*   3               *)   (*for 5,6 we get 6              *)
      (*  /                *)
      (* 0                 *)
    (* get 2 nodes and checks if they have the same parent *)
    fun find (ls, i) = 
      if Array.sub(ls, i) = i 
      then i
      else find(ls, Array.sub(ls, i))


    (* {parent, rank} are arrays *)
    fun union (parent, rank, p, q) =
      let 
        val i = find(parent, p)
        val j = find(parent,q)
      in
    (* update (arr, i, x)  sets the i(th) element of the array arr to x *)
          if (Array.sub(rank, i) < Array.sub(rank, j))
            then (Array.update(parent, i, j); Array.update(rank, j , (Array.sub(rank,j) + Array.sub(rank,i))))
          else (Array.update(parent, j, i);Array.update(rank,i,(Array.sub(rank,i) + Array.sub(rank,j))))
      end

    (* fun print1s(s) = ((print(s^"\n");true)) *)

    fun range (x, y, ls) =
        if x = y
        then ls
        else x::range(x+1, y, ls)

    val (Ncities, Nroads, edges) = parse file

    val parent = Array.fromList (range(0, Ncities, []));
    val rank   = Array.array(Ncities, 1);

    val sortedEdges = mergeSort edges; 


    (* Kruskal below *)
    fun kruskal (current, wei, []) = wei
      | kruskal (current, wei, h::t:int list list) = 
          let
            val x = List.nth(h,0);
            val y = List.nth(h,1);
            val w = List.nth(h,2);
            
            val xfind = find(parent, x);
            val yfind = find(parent, y);

            
            val w' = 
              if xfind <> yfind
              then (
                union(parent, rank, xfind, yfind); 
                let
                  val mx = 
                    if w > current
                    then w
                    else current;
                in 
                  mx
                end
              )
              else wei
          in
            kruskal(current, w', t)
          end

    val w = kruskal(0, 0, sortedEdges) 
  in
    print(Int.toString(w)^"\n")
  end
