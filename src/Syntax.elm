module Syntax exposing (..)
import Set
import Set exposing (Set)
import Dict
import Dict exposing (Dict)

import Parser
import Parser exposing (Parser, (|=), (|.))

type Name = Name String | HOLE | IName Int 

nameToString : Name -> String 
nameToString n = case n of 
    Name s -> s 
    IName i -> "_x" ++ String.fromInt i 
    HOLE -> "[_]"

-- | Encode a name as a set key 
encName : Name -> String 
encName e = case e of 
    Name s -> "_" ++ s 
    HOLE   -> ""
    IName i -> "$" ++ String.fromInt i 

type Exp = EVar Info Name | EAbs Info Name Exp | EApp Info Exp Exp 


type Special = ChurchNum Int | ChurchBool Bool | CDef Name 

type alias Info =
    { isNormal : Bool,
       --^ whether a subterm is a normal form or not
      isNeutral : Bool, 
       --^ whether a subterm is a neutral term or not  
      special : Maybe Special, 
       -- ^ Whether a subterm represents a special thing (a Church numeral, a Church Boolean, or a defined symbol)
      fvi : Set String 
    }

info : Exp -> Info 
info e = case e of 
    EVar i _ -> i 
    EApp i _ _ -> i 
    EAbs i _ _ -> i 

-- Smart constructors

var : Name -> Exp  
var x = 
    let 
        i = { isNormal = True, isNeutral = True, special = Nothing, fvi = Set.singleton (encName x) }
    in 
    EVar i x 

lam : Name -> Exp -> Exp 
lam x e = lamS x e Nothing 

lamS : Name -> Exp -> Maybe Special -> Exp 
lamS x e s = 
    let 
        ie = info e 
        i = { isNormal = ie.isNormal, isNeutral = False, special = s, fvi = Set.remove (encName x) ie.fvi } 
    in EAbs i x e 

app : Exp -> Exp -> Exp 
app e1 e2 = 
    let 
        b = (info e1).isNeutral && (info e2).isNormal 
        i = { isNormal = b, isNeutral = b, special = Nothing, fvi = Set.union (info e1).fvi (info e2).fvi }
    in EApp i e1 e2 

type alias Context = List Frame 
type Frame = AppLeft Exp | AppRight Exp | LamBody Name 

applyContext : Context -> Exp -> Exp 
applyContext cs e = List.foldl applyFrame e cs 

applyFrame : Frame -> Exp -> Exp 
applyFrame f e0 = case f of 
    AppLeft e -> app e0 e 
    AppRight e -> app e e0 
    LamBody x -> lam x e0 

-- C[ (\x -> e1) e2 ]     
type Redex = Redex (Name, Exp, Exp) Context 

type NormalOrRedex = NRNormal Exp | NRRedex Redex 

oneStep : Redex -> NormalOrRedex 
oneStep (Redex red k) = findRedexNo <| applyContext k (reduce red)

initReduction : List (Name, Exp) -> Exp -> NormalOrRedex  
initReduction decls exp = findRedexNo <| resolveDecls decls exp

findRedexNo : Exp -> NormalOrRedex 
findRedexNo = findRedexNoWithContext []

findRedexNoWithContext : Context -> Exp -> NormalOrRedex 
findRedexNoWithContext k e0 = 
    let i = info e0 
    in case e0 of 
        EVar _ x -> NRNormal (EVar i x)
        EAbs _ x e -> 
            if i.isNormal then 
                NRNormal (EAbs i x e)
            else 
                case findRedexNoWithContext (LamBody x :: k) e of 
                    NRNormal ee -> NRNormal (lamS x ee i.special)
                    NRRedex red -> NRRedex red 
        EApp _ e1 e2 -> 
            if i.isNormal then 
                NRNormal (app e1 e2)
            else 
                case findRedexBNWithContext (AppLeft e2 :: k) e1 of 
                    NRNormal (EAbs _ x e) ->
                        NRRedex (Redex (x, e, e2) k)
                    NRNormal e1_ ->
                        case findRedexNoWithContext (AppLeft e2 :: k) e1_ of
                            NRNormal e1__ ->
                                case findRedexNoWithContext (AppRight e1 :: k) e2 of
                                    NRNormal e2_ ->
                                        NRNormal (app e1__ e2_)
                                    NRRedex red -> NRRedex red
                            NRRedex red -> NRRedex red
                    NRRedex red -> NRRedex red

findRedexBNWithContext : Context -> Exp -> NormalOrRedex 
findRedexBNWithContext k e0 = case e0 of 
    EVar i x -> NRNormal (EVar i x) 
    EAbs i x e -> NRNormal (EAbs i x e)
    EApp _ e1 e2 -> 
        case findRedexBNWithContext (AppLeft e2 :: k) e1 of 
            NRNormal (EAbs _ xx ee) -> NRRedex <| Redex (xx, ee, e2) k 
            NRNormal e1_ -> NRNormal (app e1_ e2) 
            NRRedex red  -> NRRedex red 

reduce : (Name, Exp, Exp) -> Exp 
reduce (x, e1, e2) = substSafely x e2 e1 

fv : Exp -> Set String 
fv e = (info e).fvi 

substSafely : Name -> Exp -> Exp -> Exp 
substSafely x e2 e1 = substitute x e2 (rename (fv e2) e1) 

substitute : Name -> Exp -> Exp -> Exp 
substitute x eToSubst e0 = 
    if not (Set.member (encName x) (fv e0)) then e0 
    else case e0 of 
        EVar i y -> 
            if x == y then eToSubst 
            else           EVar i y -- cannot happen 
        EAbs i y e -> 
            if x == y then EAbs i y e 
            else 
              -- capturing cannot happen as we rename all bound variables in e beforehand
              lamS y (substitute x eToSubst e) i.special -- NB: special ones are closed
        EApp _ e1 e2 -> 
            app (substitute x eToSubst e1) (substitute x eToSubst e2)

rename : Set String -> Exp -> Exp 
rename = 
    let 
        go : Dict String Name -> Set String -> Exp -> Exp 
        go mp namesToAvoid e0 = case e0 of 
            EVar _ x -> var <| Maybe.withDefault x (Dict.get (encName x) mp)
            EAbs i x e -> 
                if Set.member (encName x) namesToAvoid then 
                    let y = pickName (Set.union namesToAvoid (fv e))
                    in lamS y (go (Dict.insert (encName x) y mp) (Set.insert (encName y) namesToAvoid) e) i.special 
                else 
                    lamS x (go mp (Set.insert (encName x) namesToAvoid) e) i.special 
            EApp _ e1 e2 -> 
                app (go mp namesToAvoid e1) (go mp namesToAvoid e2)
    in go Dict.empty 

pickName : Set String -> Name 
pickName s = 
    let go i = 
            if Set.member (encName <| IName i) s then go (i + 1) 
            else IName i 
        goodNames = List.map (Name << String.fromChar)
                       [ 'x', 'y', 'z', 'w', 's', 't', 'u', 'p', 'q', 
                         'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'k', 'm', 'n', 'r', 'o', 'v' ] 
        tryPickGoodName = List.foldr 
                (\c r () -> 
                    if Set.member (encName c) s then r ()
                    else                             Just c) 
                (\_ -> Nothing) goodNames ()
    in case tryPickGoodName of 
        Nothing -> go 1 
        Just x -> x 

resolveDecls : List (Name, Exp) -> Exp -> Exp
resolveDecls defs e0 =
   let 
    addSpec exp n = case exp of 
        EVar i x -> EVar { i | special = Just <| CDef n} x
        EAbs i x e -> EAbs {i | special = Just <| CDef n } x e 
        EApp i e1 e2 -> EApp {i | special = Just <| CDef n} e1 e2 
  in List.foldr
    ( \(n, ebody) e ->
        let e__ = addSpec ebody n
        in  substSafely n e__ e
    )
    e0
    defs

-- Parsing 

parseExp : String -> Result (List String) Exp 
parseExp str = 
    Result.mapError (List.map renderCollated << collateDeadEnd) <| Parser.run (withEnd <| withPrecedingSpace pTopExp) str

parseDecls : String -> Result (List String) (List (Name, Exp))
parseDecls str = 
    Result.mapError (List.map renderCollated << collateDeadEnd) 
    <| Parser.run (withEnd <| withPrecedingSpace pDecls) str

collateDeadEnd : List Parser.DeadEnd -> List (Int, Int, List Parser.Problem)
collateDeadEnd = 
    let group xs0 = case xs0 of 
            [] -> []
            (x::xs) -> groupAux x.row x.col [x.problem] xs 

        groupAux row col ps ys0 = 
            case ys0 of 
                [] -> [(row, col, ps)]
                (y::ys) -> 
                    if row == y.row && col == y.col then 
                        groupAux row col (y.problem :: ps) ys 
                    else 
                        (row, col, ps) :: group (y :: ys)
    in 
    List.sortWith (\x y -> compare (x.row, x.col) (y.row, y.col)) >> List.reverse >> group

renderCollated : (Int, Int, List Parser.Problem) -> String 
renderCollated (row, col, ps) = 
    let exs = expectings ps 
    in  (String.fromInt row ++ ":" ++ String.fromInt col) ++ " " ++ 
        (if List.isEmpty exs then "" else "expecting " ++ String.join ", " exs)
    -- String.join "\n" 
    -- <| (String.fromInt row ++ ":" ++ String.fromInt col) :: 
    --     List.map renderProblem ps 

expectings : List Parser.Problem -> List String 
expectings = List.concatMap extractExpect

extractExpect : Parser.Problem -> List String
extractExpect p = case p of 
    Parser.Expecting s -> [s]
    Parser.ExpectingInt -> ["<int>"]
    Parser.ExpectingVariable -> ["<variable>"]
    Parser.ExpectingSymbol s -> [s] 
    Parser.ExpectingKeyword s -> [s] 
    Parser.ExpectingEnd -> ["EOF"]
    _ -> []


renderProblem : Parser.Problem -> String 
renderProblem p = case p of 
    Parser.Expecting s -> "expecting " ++ s 
    Parser.ExpectingInt -> "expecting <int>" 
    Parser.ExpectingVariable -> "expecting <variable>"
    Parser.ExpectingSymbol s -> "expecting " ++ s 
    Parser.ExpectingKeyword s -> "expecting keyword " ++ s 
    Parser.ExpectingEnd -> "expending EOF"
    Parser.Problem s -> s 
    Parser.BadRepeat -> "Bad repeat"
    _                  -> "unknown reason"

withEnd : Parser a -> Parser a 
withEnd p = 
    Parser.succeed identity 
    |= p 
    |. Parser.end

withPrecedingSpace : Parser a -> Parser a 
withPrecedingSpace p = 
    Parser.succeed identity
    |. sp 
    |= p 

pDecls : Parser (List (Name, Exp)) 
pDecls = Parser.loop [] <| \acc -> 
    Parser.oneOf 
    [ Parser.succeed (\d -> Parser.Loop (d :: acc)) 
        |= pDecl 
        |. sp
    , Parser.succeed (Parser.Done (List.reverse acc)) 
    ]

pDecl : Parser (Name, Exp)
pDecl = 
    Parser.succeed (\a b -> (a,b)) 
    |= pName 
    |. sp 
    |. Parser.symbol "="
    |. sp 
    |= pTopExp
    |. Parser.symbol ";"
    |. sp 


-- taken from https://package.elm-lang.org/packages/elm/parser/latest/Parser#multiComment
sp : Parser ()
sp = 
    Parser.loop 0 <| ifProgress <|
    Parser.oneOf
      [ Parser.lineComment "--"
      , Parser.multiComment "{-" "-}" Parser.Nestable
      , Parser.spaces
      ]

ifProgress : Parser a -> Int -> Parser (Parser.Step Int ())
ifProgress parser offset =
  Parser.succeed identity
    |. parser
    |= Parser.getOffset
    |> Parser.map (\newOffset -> if offset == newOffset then Parser.Done () else Parser.Loop newOffset)
-- 

symbolLambda : Parser ()
symbolLambda = Parser.oneOf [ Parser.symbol "\\", Parser.symbol "λ", Parser.keyword "fun"] 
symbolArrow : Parser ()
symbolArrow = Parser.oneOf [ Parser.symbol "->", Parser.symbol "." , Parser.symbol "→" ] 

makeAbs : List Name -> Exp -> Exp
makeAbs names exp = List.foldr (\n e -> lam n e) exp names 

pTopExp : Parser Exp 
pTopExp = 
    Parser.oneOf 
    [ Parser.succeed makeAbs 
    |. symbolLambda
    |. sp 
    |= pNamesArrow 
    |. sp 
    |= Parser.lazy (\_ -> pTopExp) 
    , Parser.succeed identity
    |= pAppChain 
    |. sp 
    ]

pAppChain : Parser Exp 
pAppChain = 
    Parser.succeed (\f args -> List.foldl (\h a -> app a h) f args)
    |= pAtomicExp 
    |= pAtomicExpLoop 

pAtomicExpLoop : Parser (List Exp)
pAtomicExpLoop = 
    let
        p acc = Parser.oneOf 
                [ Parser.succeed (\e -> Parser.Loop (e :: acc))
                  |= pAtomicExp , 
                  Parser.succeed (Parser.Done (List.reverse acc))
                ]
    in Parser.loop [] p 

pName : Parser Name
pName = 
    Parser.succeed Name 
    |= Parser.variable
        { start = Char.isAlpha, inner = \c -> Char.isAlphaNum c || c == '_' || c == '\'', 
            reserved = Set.fromList [ "fun", "λ" ] }

pNamesArrow : Parser (List Name)
pNamesArrow = 
    let
        p acc = Parser.oneOf 
            [ Parser.succeed (\e -> Parser.Loop (e :: acc))
              |= pName 
              |. sp, 
              Parser.succeed (Parser.Done (List.reverse acc))
              |. symbolArrow 
              |. sp 
            ]
    in 
    Parser.succeed (::)
    |= pName 
    |. sp 
    |= Parser.loop [] p 
pAtomicExp : Parser Exp 
pAtomicExp = 
    Parser.oneOf [
        Parser.succeed identity
        |. Parser.symbol "(" 
        |. sp 
        |= Parser.lazy (\_ -> pTopExp)
        |. Parser.symbol ")"
        |. sp, 
        Parser.succeed makeChurchNumeral 
        |= Parser.int 
        |. sp , 
        Parser.succeed var 
        |= pName 
        |. sp ] 

makeChurchNumeral : Int -> Exp 
makeChurchNumeral i =
    let n0 = if i < 0 then 0 else i 
        go n = if n <= 0 then var (Name "z")
               else app (var (Name "s")) (go (n - 1))
    in lamS (Name "s") (lam (Name "z") (go n0)) (Just (ChurchNum n0))
    