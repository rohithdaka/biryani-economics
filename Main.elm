import Html exposing (..)
import Html.Events exposing (onClick)
import Basics exposing (..)
import String exposing (..)



type alias Model = 
    { savingsAmount: Float
    , priceOfBiryani: Float
    , numberOfBiryanis: Float
    }

initialModel = 
    { savingsAmount = 10000000000000
    , priceOfBiryani = 100
    }

indianNumberFormat: String -> String
indianNumberFormat value =
    let 
        l = String.length value
    in 
        if l < 4 then 
            value
        else if (l % 2 == 0) then
            (left 1 value) ++ "," ++ indianNumberFormat (dropLeft 1 value)
        else
            (left 2 value) ++ "," ++ indianNumberFormat (dropLeft 2 value)


view model =
    div []
        [ h1 [] [text "Biryani Economics"]
        , p [] [ text ("Suppose you have ₹ " ++ (model.savingsAmount |> toString |> indianNumberFormat) ++ " and the price of biryani is ₹ " ++ (model.priceOfBiryani |> toString))]
        ]




main =
    view initialModel