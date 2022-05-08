(* read input to 2d list *)
fun min_fill file = 
  let 
    fun parse file =
        let 
            fun readInt input = Option.valOf (TextIO.scanStream (Int.scan StringCvt.DEC) input)
            
            val inStream = TextIO.openIn file
            val (n,m) = (readInt inStream, readInt inStream)
            
            fun readInts 0 acc = acc 
                | readInts i acc = rev (readInts (i - 1) (readInt inStream :: acc))

            fun readLines acc =
                case TextIO.inputLine inStream of
                    NONE => rev acc
                    | SOME line => readLines ((readInts 3 [])::acc)  (*3 elements per line*)

            val inputList = readLines []:int list list
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

    (* fun print1s(s) = ((print(s^"\n");true)) *)

    fun singleDigitArray ( 0, _) = []
      | singleDigitArray (sz, y) = y::singleDigitArray(sz-1, y)

    fun range (x, y, ls) =
        if x = y
        then ls
        else x::range(x+1, y, ls)

    val parseout = parse file
    val Ncities = #1 parseout
    val Nroads = #2 parseout
    val edges = #3 parseout 

    val listparent = range(0, Ncities, []);
    val listrank   = singleDigitArray(Ncities, 1);

    val parent = Array.fromList listparent;
    val rank   = Array.fromList listrank;

    val listsortedEdges = mergeSort edges;
    val sortedEdges = Array2.fromList sortedEdges;
    

    (* Kruskal below *)
    fun kruskal (index, w, edgelist:int list list) = 
      let
        val elem = #index edgelist;
        val x = #0 elem;
        val y = #1 elem;
        val w = #2 elem;
      in
        if index = Nroads
        then w
        else 

  in
    print(Int.toString(w)^"\n")
    

end

(* val a = Array.fromList parent;
val a = Array.fromList rank;
union a b p q; *)

