module Interpreter

open Ast
open Parser

type Color = System.ConsoleColor

type TextWindow private () =
    static member WriteLine (o:obj) = System.Console.WriteLine(o)
    static member ForegroundColor
        with get () = System.Console.ForegroundColor.ToString()
        and set color =   
            let color = Color.Parse(typeof<Color>, color, true)
            System.Console.ForegroundColor <- color :?> Color
type Clock private () =
    static let now() = System.DateTime.Now
    static member Year = now().Year
    static member Month = now().Month
    static member Day = now().Day

type IMarker = interface end
let getLibraryType name = typeof<IMarker>.DeclaringType.GetNestedType(name) 

/// Converts value to obj
let fromObj (x:obj) =
    match x with
    | :? bool as x -> Bool x
    | :? int as x -> Int x
    | :? double as x -> Double x
    | :? string as x -> String x
    | null -> Int 0
    | x -> raise (new System.NotSupportedException(x.ToString()))
/// Converts value to obj
let toObj = function
    | Bool x -> box x
    | Int x -> box x
    | Double x -> box x
    | String x -> box x
    | Array x -> raise (new System.NotSupportedException(x.ToString()))
/// Converts value to int
let toInt = function
    | Bool x -> raise (new System.NotSupportedException())
    | Int x -> x
    | Double x -> int x
    | String x -> int x
    | Array x -> raise (new System.NotSupportedException(x.ToString()))
/// Converts value to bool
let toBool = function
    | Bool x -> x
    | _ -> raise (new System.NotSupportedException())
/// Converts value to double
let toDouble = function
    | Double x -> x
    | Int x -> double x
    | _ -> raise (new System.NotSupportedException())
/// Converts value to array
let toArray = function
    | Array x -> x
    | _ -> raise (new System.NotSupportedException())
/// Coerces a tuple of numeric values to double
let (|AsDoubles|_|) = function
    | Double l, Double r -> Some(l,r)
    | Int l, Double r -> Some(double l,r)
    | Double l, Int r -> Some(l,double r)
    | _, _ -> None
/// Compares values
let compare lhs rhs =
    match lhs, rhs with
    | Bool l, Bool r -> l.CompareTo(r)
    | Int l, Int r -> l.CompareTo(r)
    | AsDoubles (l,r) -> l.CompareTo(r)
    | String l, String r -> l.CompareTo(r)
    | _ -> raise (new System.NotSupportedException(sprintf "%A %A" lhs rhs))

open System.Collections.Generic

type VarLookup = Dictionary<identifier,value>

/// Evaluates expressions
let rec eval state (expr:expr) =
    let (vars:VarLookup) = state
    match expr with
    | Literal x -> x
    | Var identifier -> vars.[identifier]
    | GetAt(Location(identifier,[index])) -> 
        let array = vars.[identifier] |> toArray
        array.[eval state index]
    | GetAt(Location(identifier,xs)) -> raise (System.NotSupportedException())
    | Func(call) -> invoke state call
    | Neg x -> arithmetic (eval state x) Multiply (Int(-1))
    | Arithmetic(l,op,r) -> arithmetic (eval state l) op (eval state r)
    | Comparison(l,op,r) -> comparison (eval state l) op (eval state r)
    | Logical(l,op,r) -> logical (eval state l) op (eval state r)
and comparison lhs op rhs =
    let x = compare lhs rhs
    match op with
    | Eq -> x = 0   | Ne -> x <> 0
    | Lt -> x < 0   | Gt -> x > 0
    | Le -> x <= 0  | Ge -> x >= 0
    |> fromObj
and arithmetic lhs op rhs =
    match op, (lhs, rhs) with
    | Add, (Int l,Int r) -> Int(l + r)
    | Add, AsDoubles (l,r) -> Double(l + r)
    | Add, (String l, String r) -> String(l + r)
    | Subtract, (Int l,Int r) -> Int(l - r)
    | Subtract, AsDoubles (l,r) -> Double(l - r)
    | Multiply, (Int l,Int r) -> Int(l * r)
    | Multiply, AsDoubles (l,r) -> Double(l * r)
    | Divide, (Int l,Int r) -> Int(l - r)
    | Divide, AsDoubles (l,r) -> Double(l - r)
    | _ -> raise (System.NotImplementedException())
and logical lhs op rhs =
    match op, lhs, rhs with
    | And, Bool l, Bool r -> Bool(l && r)
    | Or, Bool l, Bool r -> Bool(l || r)
    | _, _, _ -> raise (System.NotSupportedException())
and invoke state invoke =
    match invoke with
    | Method(tn, name, args) ->
        let t = getLibraryType tn
        let mi = t.GetMethod(name)
        let args = args |> Array.map (eval state >> toObj)
        mi.Invoke(null, args) |> fromObj
    | PropertyGet(tn, name) ->
        let t = getLibraryType tn
        let pi = t.GetProperty(name)
        pi.GetValue(null) |> fromObj
    | Cos(x) ->
        let c = eval state x |> toDouble |> cos
        (Double(c))
    | Sin(x) ->
        let s = eval state x |> toDouble |> sin
        (Double(s))

/// Runs program
let run (program:programline[]) =
    /// Program index
    let pi = ref 0
    /// Variable lookup   
    let variables = VarLookup()
    /// For from EndFor lookup
    let forLoops = Dictionary<index, index * identifier * expr * expr>()
    /// Evaluates expression with variables
    let eval = eval variables
    /// Assigns variable with result of expression
    let assign (Set(identifier,expr)) = variables.[identifier] <- eval expr    
    /// Sets property with result of expression
    let propertySet(tn,pn,expr) =
        let t = getLibraryType tn
        let pi = t.GetProperty(pn)
        pi.SetValue(null, eval expr |> toObj)
    /// Print built-in
    let print expr =
        eval expr |> toObj |> System.Console.WriteLine
    /// Obtains an array for the specified identifier
    let obtainArray identifier =
        match variables.TryGetValue(identifier) with
        | true, Array(array) -> array
        | true, _ -> raise (System.NotSupportedException())
        | false, _ -> 
            let array = Hashtable<value,value>()
            variables.Add(identifier,Array(array))
            array
    /// Sets array value at index with result of expression
    let setAt(identifier,index,expr) =        
        let array = obtainArray identifier
        array.[eval index] <- eval expr
    /// Finds first index of instructions
    let findFirstIndex start (inc,dec) isMatch =
        let mutable i = start
        let mutable nest = 0
        while nest > 0 || isMatch (snd program.[i]) |> not do 
            if inc program.[i] then nest <- nest + 1
            if nest > 0 && dec program.[i] then nest <- nest - 1
            i <- i + 1
        i
    /// Finds index of instruction
    let findIndex start (inc,dec) instruction =
        findFirstIndex start (inc,dec) ((=) instruction)
    let findIndexByLine linenum =
        let i = Array.findIndex (fun (n,_) -> n = linenum) program
        i - 1
    let isFor = function (_, For(_,_,_)) -> true | _ -> false
    let isNext = function (_, Next) -> true | _ -> false 
    let isFalse _ = false
    /// Instruction step
    let step () =
        let linenum, instruction = program.[!pi]
        match instruction with
        | Assign(set) -> assign set
        | PropertySet(tn,pn,expr) -> propertySet(tn,pn,expr)           
        | SetAt(Location(identifier,[index]),expr) -> setAt(identifier,index,expr)
        | SetAt(_) -> raise (System.NotImplementedException())
        | Action(call) -> invoke variables call |> ignore
        | If(condition, linenum) ->
            if eval condition |> toBool then
                pi := findIndexByLine linenum
        | For((Set(identifier,expr) as from), target, step) ->
            assign from
            let index = findIndex (!pi+1) (isFor,isNext) Next
            forLoops.[index] <- (!pi, identifier, target, step)
            if toInt(variables.[identifier]) > toInt(eval target) 
            then pi := index
        | Next ->
            let start, identifier, target, step = forLoops.[!pi]
            let x = variables.[identifier]
            variables.[identifier] <- arithmetic x Add (eval step)
            if toInt(variables.[identifier]) <= toInt(eval target) 
            then pi := start
        | Goto(linenum) -> pi := findIndexByLine linenum
        | Print(expr) -> print expr
        | _ -> failwith "syntax error"
    while !pi < program.Length do step (); incr pi

let execute source =
    let ast = parse source
    run ast
