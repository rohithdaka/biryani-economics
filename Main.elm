import Html exposing (..)
import Html.Events as HE exposing (..)
import Html.Attributes as HA exposing (..)
import Basics exposing (..)
import String exposing (..)
import StartApp.Simple as S exposing(start)
import Regex as R exposing(..)
import Result exposing (..)
import Maybe exposing (..)

port title : String
port title = "Biryani Economics"

type alias Model = 
    { savings: String
    , price: String
    , interest: Float
    , inflation: Float
    , numberOfYears: Float
    }


initialModel = 
    { savings = 100000 |> toString |> indianNumberFormat
    , price = 250 |> toString |> indianNumberFormat
    , interest = 4
    , numberOfYears = 1
    , inflation = 5.5
    }

type Action 
    = UpdateSavings String
    | UpdatePrice String
    | UpdateInterest String
    | UpdateYears String
    | UpdateInflation String

update: Action -> Model -> Model
update action model =
    case action of 
        UpdateSavings n -> {model | savings = (n |> indianNumberFormat)}
        UpdatePrice n -> {model | price = (n |> indianNumberFormat)}
        UpdateInterest n -> {model | interest = (n |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0)}
        UpdateYears n -> {model | numberOfYears = (n |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0)}
        UpdateInflation n -> {model | inflation = (n |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0)}


cleanseToNumeric: String -> String
cleanseToNumeric n = n |> R.replace R.All (R.regex "[^0-9]") (\_ -> "")

indianNumberFormat: String -> String
indianNumberFormat n =
    let 
        numeric = cleanseToNumeric n
    in 
        (rinf numeric)

-- Recursive Indian Number Formater
rinf: String -> String 
rinf numeric =
    let l = String.length numeric
    in
        if l < 4 then 
            numeric
        else if (l % 2 == 0) then
            (left 1 numeric) ++ "," ++ rinf (dropLeft 1 numeric)
        else
            (left 2 numeric) ++ "," ++ rinf (dropLeft 2 numeric)


initialBiryanis: Model -> Int  
initialBiryanis model =
    let s = model.savings |> cleanseToNumeric |> String.toInt|> Result.toMaybe |> Maybe.withDefault 0
        p = model.price |> cleanseToNumeric |> String.toInt|> Result.toMaybe |> Maybe.withDefault 100
    in 
        s//p

finalSavings: Model -> Int
finalSavings model =
    let s = model.savings |> cleanseToNumeric |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0 
    in s*(1 + model.interest / 1200 )^(12*model.numberOfYears) |> round

finalPrice: Model -> Int
finalPrice model =
    let p = model.price |> cleanseToNumeric |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0 
    in p* (1 + model.inflation / 1200 )^(12*model.numberOfYears) |> round

finalBiryanis: Model -> Int 
finalBiryanis model =
    (model |> finalSavings)//(model |> finalPrice)


concludingRemark: Model -> String
concludingRemark model =
    let f = (model |> finalBiryanis )
        i = (model |> initialBiryanis)
    in 
        if f < i then
            "You are poorer than before. Keep your money some place where you get more interest or demand your elected representatives to reduce inflation"
        else if f == i then
            "You managed to protect your money's value. Not bad."
        else if f > i && f < ( (Basics.toFloat i) * (1 + 9/1200)^(12*model.numberOfYears) |> round ) then
            "Good Job! You became richer by beating the inflation"
        else 
            "Excellent!! You either worked really hard or have resorted to crony rent seeking mechanism through politicians "

pluralString: (String, String) -> Int -> String
pluralString (a, b) n =
    if n == 1 then
        " " ++ a
    else 
        " " ++ b

view address model =
    body []
        [ h1 
            [] 
            [ text "Biryani Economics"
            ]
        , p []
            [ text "The essay is to demonstrate the effect of inflation on your ability to purchase biryanis."
            ]
        , p 
            [] 
            [ text "Suppose that you have ₹ "
            , input 
                [ type' "text"
                , value model.savings
                , size (String.length model.savings)
                , on "input" targetValue (Signal.message address << UpdateSavings)
                ]
                []
            , text " in your savings account." 
            , text " Biryani price is ₹ " 
            , input 
                [ type' "text"
                , value model.price
                , size (String.length model.price)
                , on "input" targetValue (Signal.message address << UpdatePrice)
                ]
                []
            , br [] []
            , text "You can buy "
            , text (model |> initialBiryanis |> toString |> indianNumberFormat) 
            , text (pluralString ("biryani", "biryanis") (model |> initialBiryanis) )
            , text " at present."
            ]
        , p
            []
            [ text "You would have ₹ " 
            , text (model |> finalSavings |> toString |> indianNumberFormat) 
            , text " after " 
            , input 
                [ type' "number"
                , value (toString model.numberOfYears) 
                , size 2
                , HA.max "999"
                , HA.min "0"
                , HA.step "1"
                , on "change" targetValue (Signal.message address << UpdateYears)
                ]
                []
            , text (pluralString ("year", "years") (Basics.round model.numberOfYears))
            , text " if savings bank interest rate is: "
            , text ((toString model.interest) ++ "%")
            , input 
                [ type' "range"
                , value (toString model.interest)
                , HA.max "10"
                , HA.min "0"
                , HA.step "0.01"
                , on "input" targetValue (Signal.message address << UpdateInterest)
                ]
                []
            ]
        , p 
            []
            [ text "However, when the inflation is " 
            , text ((toString model.inflation) ++ "%")
            , input 
                [ type' "range"
                , value (toString model.inflation)
                , HA.max "10"
                , HA.min "0"
                , HA.step "0.01"
                , on "input" targetValue (Signal.message address << UpdateInflation)
                ]
                []
            , text (", the price of biryani becomes ₹ " ++ (model |> finalPrice |> toString |> indianNumberFormat)) 
            ]
        , p 
            []
            [ text "So you could buy "
            , text (model |> finalBiryanis |> toString |> indianNumberFormat )
            , text (pluralString ("biryani", "biryanis") (model |> finalBiryanis) )
            , text " after "
            ,  input 
                [ type' "number"
                , value (toString model.numberOfYears) 
                , size 2
                , HA.max "999"
                , HA.min "0"
                , HA.step "1"
                , on "change" targetValue (Signal.message address << UpdateYears)
                ]
                []
            , text (pluralString ("year", "years") (Basics.round model.numberOfYears))
            ]
        , p 
            []
            [ text (model |> concludingRemark)]
        ]






main =
    S.start {model = initialModel, update =update, view =view}