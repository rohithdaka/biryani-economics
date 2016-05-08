import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Basics exposing (..)
import String exposing (..)
import StartApp.Simple as S
import Regex as R


type alias Model = 
    { savingsAmount: String
    , priceOfBiryani: String
    }


initialModel = 
    { savingsAmount = 1000000000 |> toString |> indianNumberFormat
    , priceOfBiryani = 1000 |> toString |> indianNumberFormat
    }

type Action 
    = UpdateSavings String
    | UpdatePrice String

update: Action -> Model -> Model
update action model =
    case action of 
        UpdateSavings n -> {model | savingsAmount = (n |> indianNumberFormat)}
        UpdatePrice n -> {model | priceOfBiryani = (n |> indianNumberFormat)}


indianNumberFormat: String -> String
indianNumberFormat n =
    let 
        cleansedNumericString = split "," n 
                                |> concat 
                                |> R.replace R.All (R.regex "[^0-9]") (\_ -> "")

        l = String.length cleansedNumericString
    in 
        if l < 4 then 
            cleansedNumericString
        else if (l % 2 == 0) then
            (left 1 cleansedNumericString) ++ "," ++ indianNumberFormat (dropLeft 1 cleansedNumericString)
        else
            (left 2 cleansedNumericString) ++ "," ++ indianNumberFormat (dropLeft 2 cleansedNumericString)


view address model =
    div []
        [ h1 
            [] 
            [text "Biryani Economics"]
        , p 
            [] 
            [ text "Suppose you have ₹ "
            , input 
                [ type' "text"
                , value model.savingsAmount
                , size (String.length model.savingsAmount)
                , on "input" targetValue (Signal.message address << UpdateSavings)
                ]
                []
            , text (" and the price of biryani is ₹ " 
                    ++ (model.priceOfBiryani |> indianNumberFormat))]
        ]




main =
    S.start {model = initialModel, update =update, view =view}