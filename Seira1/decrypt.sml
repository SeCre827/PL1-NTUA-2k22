local
   val letterProbs =[0.08167, 0.01492, 0.02782, 0.04253, 0.12702, 0.02228, 0.02015,
   0.06094, 0.06966, 0.00153, 0.00772, 0.04025, 0.02406, 0.06749,
   0.07507, 0.01929, 0.00095, 0.05987, 0.06327, 0.09056, 0.02758,
   0.00978, 0.02360, 0.00150, 0.01974, 0.00074 ];

   local
      (* Na ftiaksw mia sinartisi poy na kanei ton pinaka letter times 26 *)
      fun ascciFixer array1  =
         let 
            fun arrayToList arr = Array.foldr (op ::) [] arr
            (* fun that removes the indexes 26 to 31 that are not aplha *)
            fun nonAplharemover list = List.take (list, 26) @ List.drop (list, 32)
            fun listShrink list = 
            let
            fun listSparser [] list2 list3 n = (list2,  list3) (* split a list in 2 on index 26*)
               | listSparser (hd::tl) list2 list3 n=
               if (n <27) then listSparser tl (hd::list2) list3 (n+1)
               else if  (n> 26) then listSparser tl list2 (hd::list3) (n+1)
               else listSparser tl list2 list3 (n+1)
               (* Get two lists and add the elements with the same index  *)
            fun addLists ([], []) = []
               |addLists ((hd::tl), [])  = []
               |addLists ([], (hd::tl))   = []
               |addLists ((hd1::tl1), (hd2::tl2))  =(hd1+hd2) :: addLists (tl1, tl2)
            in   
               addLists (listSparser list [] [] 1)
            end
         in
            listShrink (nonAplharemover(arrayToList(array1))) 
         end

      val letterTimes = Array.array(58,0); (*0-25 -> A-Z,  32-57 -> a,z *)
      fun rotn [] n = [] 
      | rotn (hd::tl) n = 
      (*An einai alfarithmitiko kai An me to shift paei mikrotero tou  A h An einai alfarithmitiko kai An einai mikro pou ginetai kefalaio*)
         if (((Char.isAlpha hd) andalso ((ord hd - n) < 65)) orelse ((Char.isAlpha hd) andalso (Char.isLower hd) andalso ((ord hd - n) < 97))) 
         then (Array.update(letterTimes
                           ,ord hd - n +26 -65
                           ,Array.sub(letterTimes, ord hd - n +26 -65)+1);
               (Char.toString(chr(ord hd - n +26))) :: rotn tl n
               )
         else if ((Char.isAlpha hd)) (*An einai alfarithmitiko*)
         then (Array.update(letterTimes
                           ,ord hd - n -65
                           ,Array.sub(letterTimes, ord hd - n-65)+1);
               (Char.toString(chr(ord hd - n))) :: rotn tl n)      
         else ((Char.toString(hd)) :: rotn tl n)   (*Se periptwsi pou den einai aplharitmitiko*)
   in
      (* rot2n tester 0; *)
      (* Function that takes as input: List: A list of characters for the message n: The n in rotN encryption
         Output: the list after the rotN, a list[26] where list[i] the times the letter i was found after the decoding (a->0 to z -> 25) *)
      fun rot2n list n = (Array.modify (fn x => 0)  letterTimes; (*I entoli auth adeiazei ton pinaka*)
                        (rotn list n,  rev (ascciFixer(letterTimes)))) 
      (* Array.modify (fn x => 0)  letterTimes *)

   end

   fun findBiggestEntropy list_of_strings  = 
      let
         (* Get the letter probabilites for the language and the charprobabilities for this text and return the entropy *)
         fun findEntropy (charProbs:int list) =
         let
            fun intToReal l = List.map Real.fromInt l; 
            fun findEntropyHelper ([]:real list) [] sum = sum 
            | findEntropyHelper (h1::tl1) (h2::tl2) sum = findEntropyHelper tl1 tl2 ((h1 * h2) + sum)
            | findEntropyHelper [] (h2::tl2) sum =  sum
            | findEntropyHelper (h1::tl1) [] sum = sum
         in
            findEntropyHelper letterProbs (intToReal(charProbs)) 0.0
         end 
         fun findAllEntropies list n  =
            if (n <26) then (
                           (findEntropy   (#2(rot2n list n))) :: findAllEntropies list  (n+1)) 
            else []
         fun findIndexOfMax  ([]:real list) max index step = index
            |findIndexOfMax  (hd::tl) max index step = 
               if (hd > max)  then findIndexOfMax tl hd (index+ step) 1
               else findIndexOfMax tl max (index) (step+1)
      in
         findIndexOfMax (findAllEntropies list_of_strings  0) 0.0 0 0
      end


   fun decryptWord file  = 
      let 
         fun read file =
         let 
         val inStream = TextIO.openIn file
            fun helper(copt: char option) =
            case copt of
               NONE => (TextIO.closeIn inStream; [])
            | SOME(c) => (c :: helper(TextIO.input1 inStream))
         in
            helper(TextIO.input1 inStream)
         end
         val input = read file
      in 
         #1(rot2n input (findBiggestEntropy input ))
      end

   fun printList [] = print("\n")
   |printList (hd::tl) =
      if (hd = "\\n") then(print("\n"); (printList tl)) (*Checks for new line*)
      else (print(hd); printList tl)
in 
   fun decrypt fileName = 
   let 
      val k = printList (decryptWord fileName)
   in
      k
   end
end

