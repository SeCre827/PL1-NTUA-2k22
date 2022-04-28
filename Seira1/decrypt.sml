val tester = [#"A", #"B", #"b", #"c" , #"1"]

(*  prepei kapws na prostethw kai se ena pinaka tis emfanisis *)
(* Function that gets a char list and a N and does ROTN encryption for the list *)
fun rotn [] n = [] 
   | rotn (hd::tl) n = 
      (*An einai alfarithmitiko kai An me to shift paei mikrotero tou  a*)
      if ((Char.isAlpha hd) andalso ((ord hd - n) < 65)) 
         then ((Char.toString(chr(ord hd - n +26))) :: rotn tl n)
      (*An einai alfarithmitiko kai An einai mikro pou ginetai kefalaio*)
      else if ((Char.isAlpha hd) andalso (Char.isLower hd) andalso ((ord hd - n) < 97))
         then ((Char.toString(chr(ord hd - n +26))) :: rotn tl n)
      (*An einai alfarithmitiko*)
      else if ((Char.isAlpha hd))
         then ((Char.toString(chr(ord hd - n))) :: rotn tl n)      
      (*Se periptwsi pou den einai aplharitmitiko*)
      else ((Char.toString(hd)) :: rotn tl n)
