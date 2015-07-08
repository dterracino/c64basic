module Ast

type label = string
type linenumber = uint64
type identifier = string
type index = int
type Hashtable<'k,'v> = System.Collections.Generic.Dictionary<'k,'v>
/// arithmetic operation
type arithmetic = Add | Subtract | Multiply | Divide
/// comparison operaton
type comparison = Eq | Ne | Lt | Gt | Le | Ge
/// logical operation
type logical = And | Or
/// value
type value =
    | Bool of bool
    | Int of int
    | Double of double
    | String of string
    | Array of Hashtable<value,value>
/// expression
type expr =
    | Literal of value
    | Var of identifier
    | GetAt of location
    | Func of invoke
    | Neg of expr
    | Arithmetic of expr * arithmetic * expr
    | Comparison of expr * comparison * expr
    | Logical of expr * logical * expr
and location =
    | Location of identifier * expr list
and invoke =
    | Method of string * string * expr[]
    | PropertyGet of string * string
type assign =
    | Set of identifier * expr
/// instruction
type instruction =
    | Assign of assign
    | Let of assign
    | SetAt of location * expr
    | PropertySet of string * string * expr
    | Action of invoke
    | For of assign * expr * expr
    | Next
    | If of expr * linenumber
    | Goto of linenumber
    | Print of expr

type programline = linenumber * instruction 