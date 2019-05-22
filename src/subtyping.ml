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


let rec typeof ctx t =
    match t with
                TmRecord(fi, fields)    ->
                    let fieldtys =
                        List.map (fun (li, ti)    ->    (li, typeof ctx ti)    fields in
                    TyRecord(fieldtys)
           |    TmProj(fi, t1, l)   ->
                   (match (typeof ctx t1) with
                       TyRecord(fieldtys)    ->
                           (try List.assoc l fieldtys
                            with Not_found    ->    error fi ("label " ^ l ^ " not found"))
                   |    _    ->    error fi "Expected record type"
           |    TmVar(fi, i, _)    ->    getTypeFromContext fi ctx i
           |    TmAbs(fi, x, tyT1, t2)    ->
                   let ctx' = addbinding ctx x (VarBind(tyT1)) in
                   let tyT2 = typeof ctx' t2 in
                   TyArr(tyT1, tyT2)
           |    TmApp(fi, t1, t2)    ->
                   let tyT1 = typeof ctx t1 in
                   let tyT2 = typeof ctx t2 in
                   (match tyT1 with
                       TyArr(tyT11, tyT12)    ->
                           if subtype tyT2 tyT11 then tyT12
                           else error fi "parameter type mismatch"
                    |    _    ->    error fi "arrow type expected"
