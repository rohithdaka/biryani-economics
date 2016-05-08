import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Basics exposing (..)
import String exposing (..)
import StartApp.Simple as S
import Regex as R
import Result 
import Maybe 
import List 

type alias Model = 
    { savings: String
    , price: String
    }


initialModel = 
    { savings = 100000 |> toString |> indianNumberFormat
    , price = 100 |> toString |> indianNumberFormat
    }

type Action 
    = UpdateSavings String
    | UpdatePrice String

update: Action -> Model -> Model
update action model =
    case action of 
        UpdateSavings n -> {model | savings = (n |> indianNumberFormat)}
        UpdatePrice n -> {model | price = (n |> indianNumberFormat)}


cleanseToNumeric: String -> String
cleanseToNumeric n = n |> R.replace R.All (R.regex "[^0-9.]") (\_ -> "")

indianNumberFormat: String -> String
indianNumberFormat n =
    let 
        numeric = cleanseToNumeric n
        validPart = numeric |> split "." |> List.take 2
        integerPart = validPart |> List.head |> Maybe.withDefault ""
        fractionPart = validPart |> List.drop 1 |> List.head |> Maybe.withDefault "00"
    in 
        (rinf integerPart)
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


numberOfBiryanis: Model -> Float  
numberOfBiryanis model =
    let s = model.savings |> cleanseToNumeric |> String.toFloat |> Result.toMaybe |> Maybe.withDefault 0
        p = model.price |> cleanseToNumeric |> String.toFloat |> Result.toMaybe |> Maybe.withDefault 100
    in 
        s/p


view address model =
    div []
        [ h1 
            [] 
            [text "Biryani Economics"]
        , p []
            [ text "The goal of this interactive essay is to demonstrate the effect of inflation on your ability to purchase biryanis."]
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
            [ text "Also if the price of each biryani is ₹ " 
            , input 
                [ type' "text"
                , value model.price
                , size (String.length model.price)
                , on "input" targetValue (Signal.message address << UpdatePrice)
                ]
                []
            , text (" then you can buy " ++ (model |> numberOfBiryanis |> toString |> indianNumberFormat) ++ " biryanis right now.")
            ]
        ]




main =
    S.start {model = initialModel, update =update, view =view}