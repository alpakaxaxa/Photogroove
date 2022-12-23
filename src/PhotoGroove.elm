module PhotoGroove exposing (main)

import Browser
import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


initialModel : { photos : List { url : String }, selectedUrl : String }
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix =
    "http://elm-in-action.com/"


update msg model =
    if msg.description == "ClickedPhoto" then
        { model | selectedUrl = msg.data }

    else
        model


view : { a | selectedUrl : String, photos : List { b | url : String } } -> Html.Html { description : String, data : String }
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map (viewThumbnail model.selectedUrl)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


viewThumbnail : String -> { a | url : String } -> Html.Html { description : String, data : String }
viewThumbnail selectedUrl thumb =
    img [ src (urlPrefix ++ thumb.url), classList [ ( "selected", selectedUrl == thumb.url ) ], onClick { description = "ClickedPhoto", data = thumb.url } ] []


main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }