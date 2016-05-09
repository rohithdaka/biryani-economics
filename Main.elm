import Html exposing (..)
import Html.Events as HE exposing (..)
import Html.Attributes as HA exposing (..)
import Basics exposing (..)
import String exposing (..)
import StartApp.Simple as S
import Regex as R
import Result 
import Maybe 

type alias Model = 
    { savings: String
    , price: String
    , interest: Float
    , numberOfYears: Float
    }


initialModel = 
    { savings = 10000 |> toString |> indianNumberFormat
    , price = 250 |> toString |> indianNumberFormat
    , interest = 5
    , numberOfYears = 1
    }

type Action 
    = UpdateSavings String
    | UpdatePrice String
    | UpdateInterest String
    | UpdateYears String

update: Action -> Model -> Model
update action model =
    case action of 
        UpdateSavings n -> {model | savings = (n |> indianNumberFormat)}
        UpdatePrice n -> {model | price = (n |> indianNumberFormat)}
        UpdateInterest n -> {model | interest = (n |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0)}
        UpdateYears n -> {model | numberOfYears = (n |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0)}


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


numberOfBiryanis: Model -> Int  
numberOfBiryanis model =
    let s = model.savings |> cleanseToNumeric |> String.toInt|> Result.toMaybe |> Maybe.withDefault 0
        p = model.price |> cleanseToNumeric |> String.toInt|> Result.toMaybe |> Maybe.withDefault 100
    in 
        s//p

finalSavings: Model -> Int
finalSavings model =
    let s = model.savings |> cleanseToNumeric |> String.toFloat|> Result.toMaybe |> Maybe.withDefault 0 
    in s*(1 + model.interest / 1200 )^(12*model.numberOfYears) |> round

view address model =
    div []
        [ h1 
            [] 
            [ text "Biryani Economics"
            ]
        , p []
            [ text "The goal of this interactive essay is to demonstrate the effect of inflation on your ability to purchase biryanis."
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
            ]
        , p 
            []
            [ text "Biryani price is ₹ " 
            , input 
                [ type' "text"
                , value model.price
                , size (String.length model.price)
                , on "input" targetValue (Signal.message address << UpdatePrice)
                ]
                []
            , text (". So you can buy " ++ (model |> numberOfBiryanis |> toString |> indianNumberFormat) ++ " biryanis right now.")
            ]
        , p
            []
            [ text "Now let us consider the interest rate of your savings account. "
            , input 
                [ type' "range"
                , value (toString model.interest)
                , HA.max "10"
                , HA.min "0"
                , HA.step "0.01"
                , on "input" targetValue (Signal.message address << UpdateInterest)
                ]
                []
            , text ((toString model.interest) ++ "%")
            ]
        , p 
            []
            [ text "This means, after " 
            , input 
                [ type' "number"
                , value (toString model.numberOfYears) 
                , HA.max "999"
                , HA.min "0"
                , HA.step "1"
                , on "change" targetValue (Signal.message address << UpdateYears)
                ]
                []
            , text (" year, you would have ₹ " ++ (model |> finalSavings |> toString |> indianNumberFormat) )
            ]
        ]





main =
    S.start {model = initialModel, update =update, view =view}