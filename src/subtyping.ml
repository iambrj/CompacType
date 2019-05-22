type ty =
         TyRecord of (string * ty) list
    |    TyTop
    |    TyArr of ty * ty


type term =
         TmRecord of info * (string * term) list
    |    TmProj of info * term * string
    |    TmVar of info * int * int
    |    TmAbs of info * string * ty * term
    |    TmApp of info * term * term


let rec subtype tyS tyT =
   (=) tyS tyT ||
   match (tyS, tyT) with
               (TyRecord(fS), TyRecord(fT))    ->
                   List.for_all
                   (fun (li, tyTi)    ->
                       try let tySi = List.assoc li fS in
                           subtype tySi tyTi
                       with Not_found    ->    false)
               fT
           |    (_, TyTop)    ->
                   true
           |    (TyArr(tyS1, tyS2), TyArr(tyT1, tyT2))    ->
                   (subtype tyT1 tyS1) && (subtype tyS2 tyT2)
           |    (_, _)    ->
                   false
