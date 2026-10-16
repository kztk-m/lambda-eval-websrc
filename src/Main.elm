module Main exposing (..)

import Syntax exposing (..)
import Browser
import Time
import Html exposing (Html)
import Process
import Task 

import Html as H exposing (Html)
import Html.Events as H exposing (onClick, onInput, onSubmit)
import Html.Attributes as H exposing (class, type_, value, placeholder, disabled)
import Html.Lazy as H

type alias PPair a = 
    {
        ppOriginalText : String, 
        ppParsed : Result String a 
    }

type Msg = Tick | SettleD Int | SettleE Int | SetDeclText String | SetExpText String | Run | Pause | Reset | Start 

type alias Model = 
    {
        declText : String, 
        expText : String, 
        parsedDecl : Result (List String) (List (Name, Exp)), 
        parsedExp : Result (List String) Exp, 
        d_revision : Int, 
        e_revision : Int, 
        e_errorToDisplay : Maybe (List String),
        d_errorToDisplay :  Maybe (List String),
        doneBlocks : List (List Redex), 
        currentBlock : List Redex, 
        currentBlockSize : Int, 
        runState : RunState 
    }

type RunState = RSInit | RSRunning Redex | RSPaused Redex | RSFinished Exp 

main : Program () Model Msg 
main = Browser.element { init = \_ -> init, view = view, update = update, subscriptions = subscriptions } 

buttonInfo : Model -> (Bool , String, Msg)
buttonInfo m = case m.runState of 
    RSRunning _ -> (False, "Pause", Pause) -- You can pause
    RSPaused _  -> (False, "Resume", Run) -- You can resume     
    _ -> case (m.parsedDecl, m.parsedExp) of 
        (Ok _, Ok _) -> (False, "Reduce", Run) -- You can start 
        _  -> (True , "Reduce", Run) 

renderBlock : List Redex -> Html Msg
renderBlock = H.div [class "block"] << List.map renderLine

renderLine : Redex -> Html Msg 
renderLine = H.lazy (H.div [class "line"] << List.singleton << renderRedex)

renderBlocks : List (List Redex) -> List Redex -> Html Msg 
renderBlocks bs b = 
    H.div [class "blocks"]
    <| (List.map (H.lazy renderBlock) <| List.reverse bs)
       ++ [H.lazy renderBlock (List.reverse b)]

renderRedex : Redex -> Html msg
renderRedex (Redex (x, e1, e2) k) = 
    renderContext k <| \prec -> parenWhen (prec > 1) 
        [H.span [class "redex"] 
             [ H.span [class "fun"] [parenWhen True <| renderLam x (\p -> renderExp p e1)]
             , H.text " " 
             , H.span [class "arg"] [renderExp 2 e2] ]]
             
renderLam : Name -> (Int -> Html msg) -> List (Html msg)
renderLam x op = [ H.text "λ", H.text (nameToString x), H.text ".", H.text " " , op 0]

renderExp : Int -> Exp -> Html msg 
renderExp prec e0 = 
    case (info e0).special of 
        Just (ChurchNum n) -> H.span [class "cn"] [H.text (String.fromInt n)]
        Just (CDef n)      -> H.span [class "dn"] [H.text (nameToString n)]
        _ -> 
            case e0 of 
            EVar _ x -> H.text (nameToString x) 
            EAbs _ x e -> 
                parenWhen (prec > 0) <| renderLam x (\p -> renderExp p e)
            EApp _ e1 e2 ->
                parenWhen (prec > 1) [ renderExp 1 e1, H.text " " , renderExp 2 e2 ]


parenWhen : Bool -> List (Html msg) -> Html msg 
parenWhen b ts = 
    if b then 
        H.span [class "paren"] <| H.span [class "lp"] [H.text "("] :: ts ++ [H.span [class "rp"] [H.text ")"]]
    else 
        case ts of
            [t] -> t 
            _   -> H.span [] ts 

renderContext : Context -> (Int -> Html msg) -> Html msg
renderContext cs k = 
    let 
        go prec f op = case f of 
            AppLeft e2 -> parenWhen (prec > 1) [op 1, H.text " ", renderExp 2 e2]
            AppRight e1 -> parenWhen (prec > 1) [renderExp 1 e1, H.text " ", op 2]
            LamBody x -> parenWhen (prec > 0) <| renderLam x op 

        goFrames : Int -> Context -> (Int -> Html msg) -> Html msg 
        goFrames prec fs0 op = case fs0 of 
            [] -> op prec
            f :: fs -> goFrames prec fs <| \p -> go p f op 
                -- go prec f <| \p -> goFrames p fs op 
    in 
    goFrames 0 cs k 

isOk : Result e a -> Bool 
isOk x = case x of
    Ok _ -> True 
    Err _ -> False 

viewBool : Bool -> String
viewBool b = if b then "true" else "false"


view : Model -> Html Msg 
view m = 
    let 
        controlPane = 
            let (bdisabled , btext, msg) = buttonInfo m 
            in 
            H.div [class "control"]
            [
                H.label [ H.for "decls", class "field-label" ] [H.text "Definitions"], 
                H.div [class "d"]
                    [ H.textarea [ H.id "decls"
                                 , onInput SetDeclText
                                 , H.attribute "aria-invalid" <| viewBool <| not (isOk m.parsedDecl) 
                                 ] [ H.text m.declText ], 
                      d_errorPane ], 
                H.label [H.for "term", class "field-label" ] [H.text "Term"],
                Html.form [class "e", onSubmit Start ] [
                    H.input [ H.id "term", class "exp_input", type_ "text", value m.expText
                            , H.attribute "aria-invalid" <| viewBool <| not (isOk m.parsedExp || String.isEmpty m.expText)
                            , onInput SetExpText, placeholder "e.g., '(\\x. x x) (\\x.x)', 'add 1 2', 'sum 2' (quotes are not part of terms)" ] [], 
                    H.button [ type_ "button", onClick msg, class "run", disabled bdisabled] [ 
                        H.text btext
                    ], 
                    H.button [ type_ "button", onClick Reset, class "reset" ] [ H.text "Reset" ], 
                    e_errorPane
                ]
            ]
        d_errorPane = H.div [class "d_error"] <| case m.d_errorToDisplay of 
            Nothing -> []
            Just ss -> List.map (H.div [] << List.singleton << H.text) ss 
        e_errorPane  = H.div [class "e_error"] <| case m.e_errorToDisplay of 
            Nothing -> []
            Just ss -> List.map (H.div [] << List.singleton << H.text) ss 
        resultPane  = H.div [class "result"]
            [
                H.lazy2 renderBlocks m.doneBlocks m.currentBlock,
--                renderBlock <| List.reverse m.currentBlock, 
                H.div [class "frontier"] <|   case m.runState of 
                    RSRunning red -> List.singleton <| H.div [class "line"] <| [renderRedex red]
                    RSPaused red -> List.singleton <| H.div [class "line"] <| [renderRedex red]
                    RSFinished norm -> List.singleton <| H.div [class "line"] <| [renderExp 0 norm]
                    RSInit          -> []
            ]
    in H.div [class "app"] [ controlPane, resultPane ] 


capacity : number
capacity = 100 

extendBlock : Redex -> Model -> Model 
extendBlock red m = 
    if m.currentBlockSize + 1 > capacity then 
        { m | doneBlocks = List.reverse m.currentBlock :: m.doneBlocks, currentBlock = [red], currentBlockSize = 1} 
    else 
        { m | currentBlock = red :: m.currentBlock , currentBlockSize = m.currentBlockSize + 1 }

resetReductionHistory : Model -> Model 
resetReductionHistory m = { m | currentBlock = [], currentBlockSize = 0, doneBlocks = [] }


update : Msg -> Model -> (Model, Cmd Msg) 
update msg m = case msg of 
    Tick -> case m.runState of 
        RSRunning red -> case oneStep red of 
            NRNormal norm -> 
                (extendBlock red {m | runState = RSFinished norm } , Cmd.none)
            NRRedex red2 ->
                (extendBlock red {m | runState = RSRunning red2 } , Cmd.none) 
        _ -> (m, Cmd.none)
    SetDeclText s -> 
        let v = m.d_revision + 1 
        in ({m | declText = s, parsedDecl = parseDecls s, d_revision = v},  
            Process.sleep 300 |> Task.perform (\_ -> SettleD v))
    SetExpText s -> 
        let v = m.e_revision + 1 
        in ({m | expText = s, parsedExp = parseExp s, e_revision = v},  
            Process.sleep 300 |> Task.perform (\_ -> SettleE v))
    SettleD v -> 
        if m.d_revision == v then 
            let checkErr x = case x of 
                    Err s -> Just s 
                    Ok _  -> Nothing 
            in 
            ({ m | d_errorToDisplay = checkErr m.parsedDecl }, Cmd.none)
        else 
            (m, Cmd.none)
    SettleE v -> 
        if m.e_revision == v then 
            let checkErr x = case x of 
                    Err s -> Just s 
                    Ok _  -> Nothing 
            in 
            ({ m | e_errorToDisplay = checkErr m.parsedExp }, Cmd.none)
        else 
            (m, Cmd.none)
    Run -> 
        case m.runState of 
            -- When paused, it works as "resume"
            RSPaused red -> ({m | runState = RSRunning red}, Cmd.none)
            _ -> 
                case (m.parsedDecl, m.parsedExp) of 
                    (Ok decls, Ok exp) ->
                                case initReduction decls exp of 
                                    NRRedex red -> (resetReductionHistory {m | runState = RSRunning red}, Cmd.none)
                                    NRNormal norm -> (resetReductionHistory {m | runState = RSFinished norm }, Cmd.none)
                    _ -> (m, Cmd.none)
    Pause -> 
        case m.runState of
            RSRunning red -> ({m | runState = RSPaused red }, Cmd.none)
            _             -> (m, Cmd.none)
    Reset -> (resetReductionHistory {m | runState = RSInit }, Cmd.none)
    Start -> 
        let (m1, c1) = update Reset m 
            (m2, c2) = update Run   m1 
        in (m2, Cmd.batch [c1, c2]) 

init : (Model, Cmd Msg) 
init = (initModel , Cmd.none)

initDecls : String 
initDecls = 
    """-- name = term; 
-- Defined names can be used in the following definitions and the Term field.
-- Recursive definitions are not allowed.
K = \\x.\\y. x;
S = \\x y z. x z (y z);
I = \\x. x;
Omega = (\\x. x x) (\\x. x x);
Y = \\f. (\\x. f (x x)) (\\x. f (x x));
succ = \\n s z. s (n s z);
add  = \\n m. \\s z. n s (m s z);
mul  = \\n m. \\s z. n (m s) z;
true  = \\x y. x;
false = \\x y. y;
pair = \\x y k. k x y;
fst = \\p. p true;
snd = \\p. p false;
pred = \\n s z. fst (n (\\p. p (\\x y k. k y (s y))) (\\k. k z z));
sub  = \\n m. m pred n;
iszero = \\n. n (\\x. false) true;

sum   = Y (\\f n. iszero n 0 (add n (f (pred n))));
sum'  = \\n. n (\\p. p (\\x y k. k (succ x) (add (succ x) y))) (\\k. k 0 0) false;
sum'' = \\n s z. n (\\p. p (\\x y k. k (\\z. s (x z)) (s (x y)))) (\\k. k I z) false;"""

initModel : Model 
initModel = 
    let r = parseDecls initDecls
    in 
    { declText = initDecls 
    , expText = "" 
    , parsedDecl = r
    , parsedExp = Err [] 
    , d_errorToDisplay = case r of
                             Ok _ -> Nothing
                             Err ss -> Just ss
    , e_errorToDisplay = Nothing
    , doneBlocks = []
    , currentBlock = [] 
    , currentBlockSize = 0
    , d_revision = 0
    , e_revision = 0
    , runState = RSInit 
    }

subscriptions : Model -> Sub Msg 
subscriptions m = case m.runState of 
    RSRunning _ -> Time.every 30 (\_ -> Tick) 
    _           -> Sub.none 