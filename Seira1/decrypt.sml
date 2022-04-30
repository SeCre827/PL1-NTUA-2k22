val tester = [#"A",#"A",#"A",#"A",#"A", #"B", #"b", #"c" ,#"a" ,#"1"]

val letterTimes = Array2.array(26,26,0);
val entropies =Array.array(26,0);

val l1 = [1,3,5,6,12]
val l2 = [2,5,0,152,1]
val l3 = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58]

local
   val letterTimes = Array.array(58,0); (*0-25 -> A-Z,  32-57 -> a,z *)
   (* Na ftiaksw mia sinartisi poy na kanei ton pinaka letter times 26 *)
   fun ascciFixer array1  =
      let 
         fun arrayToList arr = Array.foldr (op ::) [] arr
         (* fun that removes the indexes 26 to 31 that are not aplha *)
         fun nonAplharemover list = List.take (list, 26) @ List.drop (list, 32)
         fun listShrink list = 
         let
         fun listSparser [] list2 list3 n = (list2,  list3)
            | listSparser (hd::tl) list2 list3 n=
            if (n <27) then listSparser tl (hd::list2) list3 (n+1)
            else if  (n> 26) then listSparser tl list2 (hd::list3) (n+1)
            else listSparser tl list2 list3 (n+1)

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
   fun rot2n list n = (rotn list n,  rev (ascciFixer(letterTimes)))
end

(* Control.Print.printLength *)