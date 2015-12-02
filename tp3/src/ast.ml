
type decl =
    | DeclInt  of string
    | DeclBool of string

type expr =
    | NodePlus of expr * expr
    | NodeInf  of expr * expr
    | NodeAnd  of expr * expr
    | LeafNum  of int
    | LeafId   of string

type bloc =
    | NodeRoot of decl list * inst list
    | Error
and inst =
    | NodeInst of string * expr
    | NodeBloc of bloc

