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
