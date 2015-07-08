module Parser

open FParsec
open Ast

let plinenum = puint64

let pnumliteral: Parser<expr, unit> =
    let numberFormat = NumberLiteralOptions.AllowFraction
    numberLiteral numberFormat "number"
    |>> fun nl ->
            if nl.IsInteger then Literal(Int (int nl.String))
            else Literal(Double (float nl.String))

let ws = skipManySatisfy (fun c -> c = ' ' || c = '\t' || c='\r') // spaces
let str_ws s = pstring s .>> ws
let str_ws1 s = pstring s .>> spaces1

let pstringliteral = 
    between (pstring "\"") (pstring "\"") (manySatisfy (fun x -> x <> '"')) 
    |>> (fun s -> Literal(String(s)))

let pidentifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
let pidentifier_ws = pidentifier .>> ws
let pvar = pidentifier |>> (fun x -> Var(x))

let pinvoke, pinvokeimpl = createParserForwardedToRef ()
let pfunc = pinvoke |>> (fun x -> Func(x))

let plocation, plocationimpl = createParserForwardedToRef ()
let pgetat = plocation |>> (fun loc -> GetAt(loc))

let pvalue = 
    choice [
        pnumliteral; pstringliteral
        attempt pgetat <|> attempt pfunc <|> attempt pvar
    ]

type Assoc = Associativity

let oppa = new OperatorPrecedenceParser<expr,unit,unit>()
let parithmetic = oppa.ExpressionParser
let terma = (pvalue .>> ws) <|> between (str_ws "(") (str_ws ")") parithmetic
oppa.TermParser <- terma
oppa.AddOperator(InfixOperator("+", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Add, y)))
oppa.AddOperator(InfixOperator("-", ws, 1, Assoc.Left, fun x y -> Arithmetic(x, Subtract, y)))
oppa.AddOperator(InfixOperator("*", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Multiply, y)))
oppa.AddOperator(InfixOperator("/", ws, 2, Assoc.Left, fun x y -> Arithmetic(x, Divide, y)))
oppa.AddOperator(PrefixOperator("-", ws, 2, true, fun x -> Neg(x)))

let oppc = new OperatorPrecedenceParser<expr,unit,unit>()
let pcomparison = oppc.ExpressionParser
let termc = (parithmetic .>> ws) <|> between (str_ws "(") (str_ws ")") pcomparison
oppc.TermParser <- termc
oppc.AddOperator(InfixOperator("=", ws, 1, Assoc.Left, fun x y -> Comparison(x, Eq, y)))
oppc.AddOperator(InfixOperator("<>", ws, 1, Assoc.Left, fun x y -> Comparison(x, Ne, y)))
oppc.AddOperator(InfixOperator("<=", ws, 2, Assoc.Left, fun x y -> Comparison(x, Le, y)))
oppc.AddOperator(InfixOperator(">=", ws, 2, Assoc.Left, fun x y -> Comparison(x, Ge, y)))
oppc.AddOperator(InfixOperator("<", ws, 2, Assoc.Left, fun x y -> Comparison(x, Lt, y)))
oppc.AddOperator(InfixOperator(">", ws, 2, Assoc.Left, fun x y -> Comparison(x, Gt, y)))

let oppl = new OperatorPrecedenceParser<expr,unit,unit>()
let plogical = oppl.ExpressionParser
let terml = (pcomparison .>> ws) <|> between (str_ws "(") (str_ws ")") plogical
oppl.TermParser <- terml
oppl.AddOperator(InfixOperator("AND", ws, 1, Assoc.Left, fun x y -> Logical(x,And,y)))
oppl.AddOperator(InfixOperator("OR", ws, 1, Assoc.Left, fun x y -> Logical(x,Or,y)))

let pmember = pipe3 (pidentifier_ws) (pchar '.') (pidentifier_ws) (fun tn _ mn -> tn,mn) 
let ptuple = between (str_ws "(") (str_ws ")") (sepBy parithmetic (str_ws ","))
pinvokeimpl := 
    pipe2 pmember (opt ptuple)
        (fun (tn,mn) args -> 
        match args with
        | Some args -> Method(tn, mn, args |> List.toArray)
        | None -> PropertyGet(tn,mn)
        )

let paction = pinvoke |>> (fun x -> Action(x))
let pset = pipe3 pidentifier_ws (str_ws "=") parithmetic (fun id _ e -> Set(id, e))
let passign = pipe3 pidentifier_ws (str_ws "=") parithmetic (fun id _ e -> Assign(Set(id, e)))
let plet = pipe4 (str_ws1 "LET") pidentifier_ws (str_ws "=") parithmetic (fun _ id _ e -> Assign(Set(id, e)))
let ppropertyset = pipe3 pmember (str_ws "=") parithmetic (fun (tn,pn) _ e -> PropertySet(tn,pn,e))

let pindex = str_ws "[" >>. parithmetic .>> str_ws "]"
let pindices = many1 pindex
plocationimpl := pipe2 pidentifier_ws pindices (fun id xs -> Location(id,xs))
let psetat = pipe3 plocation (str_ws "=") parithmetic (fun loc _ e -> SetAt(loc, e))

let pfor =
    let pfrom = str_ws1 "FOR" >>. pset
    let pto = str_ws1 "TO" >>. parithmetic
    let pstep = str_ws1 "STEP" >>. parithmetic
    let toStep = function None -> Literal(Int(1)) | Some s -> s
    pipe3 pfrom pto (opt pstep) (fun f t s -> For(f, t, toStep s))
let pendfor = str_ws "NEXT" |>> (fun _ -> Next)

let pif = pipe4 (str_ws1 "IF") plogical (str_ws "THEN") plinenum (fun _ e _ n -> If(e, n))

let pgoto = str_ws1 "GOTO" >>. plinenum |>> (fun n -> Goto(n))

let pprint = str_ws1 "PRINT" >>. parithmetic |>> (fun e -> Print(e))

let pinstruct = 
    [
        pfor;pendfor
        pif                
        ppropertyset; passign; psetat; plet
        paction
        pgoto
        pprint
    ]
    |> List.map attempt
    |> choice

type Line = Blank | Instruction of programline
let pcomment = str_ws "REM" >>. skipManySatisfy (fun c -> c <> '\n') >>. pchar '\n'
let peol = pcomment <|> (pchar '\n')
let pinstruction = pipe4 plinenum ws pinstruct peol (fun n _ i _ -> Instruction(n, i))
let pblank = ws >>. peol |>> (fun _ -> Blank)
let plines = many (pinstruction <|> pblank) .>> eof
let parse (program:string) =    
    match run plines program with
    | Success(result, _, _)   -> 
        result 
        |> List.choose (function Instruction i -> Some i | Blank -> None) 
        |> List.toArray
    | Failure(errorMsg, e, s) -> failwith errorMsg
