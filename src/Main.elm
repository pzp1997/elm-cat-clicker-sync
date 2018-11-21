module Main exposing (main)

import Browser
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List.Selection as Selection exposing (Selection)
import Maybe.Extra as Maybe
import Url.Builder



-- MODEL


type alias Model =
    RemoteData Http.Error (Selection Cat)


init : ( Model, Cmd Msg )
init =
    ( Loading
    , httpRetrieveCats
    )


type alias Cat =
    { name : String
    , imgSrc : String
    , clicks : Int
    , id : Maybe Identifier
    }


type alias Identifier =
    String


type RemoteData e a
    = Loading
    | Failure e
    | Success a


mapRemoteData : (a -> b) -> RemoteData e a -> RemoteData e b
mapRemoteData f remoteData =
    case remoteData of
        Success data ->
            Success (f data)

        Loading ->
            Loading

        Failure err ->
            Failure err


remoteDataToMaybe : RemoteData e a -> Maybe a
remoteDataToMaybe remoteData =
    case remoteData of
        Success data ->
            Just data

        _ ->
            Nothing



-- UPDATE


type Msg
    = ClickCat
    | ResetClicks
    | ReloadCats
    | SwitchCat Cat
    | LoadCats (Result Http.Error (List Cat))
    | UploadedCat (Result Http.Error ())


incrementClicks : Cat -> Cat
incrementClicks cat =
    { cat | clicks = cat.clicks + 1 }


resetClicks : Cat -> Cat
resetClicks cat =
    { cat | clicks = 0 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickCat ->
            updateSelected incrementClicks model

        ResetClicks ->
            updateSelected resetClicks model

        ReloadCats ->
            ( model, httpRetrieveCats )

        SwitchCat chosenCat ->
            ( mapRemoteData (Selection.select chosenCat) model
            , Cmd.none
            )

        LoadCats (Ok cats) ->
            ( Success <|
                let
                    chooseCat =
                        remoteDataToMaybe model
                            |> Maybe.andThen Selection.selected
                            |> Maybe.orElse (List.head cats)
                            |> Maybe.map
                                (\cat ->
                                    Selection.selectBy (equalBy .name cat)
                                )
                in
                applyWhenJust (Selection.fromList cats) chooseCat
            , Cmd.none
            )

        LoadCats (Err httpError) ->
            ( Failure httpError, Cmd.none )

        UploadedCat (Ok _) ->
            ( model, Cmd.none )

        UploadedCat (Err httpError) ->
            ( Failure httpError, Cmd.none )


updateSelected : (Cat -> Cat) -> Model -> ( Model, Cmd Msg )
updateSelected catFunc model =
    let
        updatedModel =
            mapRemoteData
                (Selection.mapSelected
                    { selected = catFunc, rest = identity }
                )
                model
    in
    ( updatedModel
    , updatedModel
        |> remoteDataToMaybe
        |> Maybe.andThen Selection.selected
        |> Maybe.map httpUpdateCat
        |> Maybe.withDefault Cmd.none
    )


equalBy : (a -> b) -> a -> a -> Bool
equalBy func x y =
    func x == func y


-- VIEW


view : Model -> Element Msg
view model =
    case model of
        Success cats ->
            Element.column
                [ Element.centerX
                , Element.centerY
                , Element.spacing 30
                , Font.family [ Font.sansSerif ]
                ]
                [ Element.el
                    [ Element.centerX ]
                    (selectorView cats)
                , Element.el
                    [ Element.centerY
                    , Font.family
                        [ Font.typeface "Arial"
                        , Font.sansSerif
                        ]
                    ]
                  <|
                    renderWhenJust catView (Selection.selected cats)
                , Element.row [ Element.centerX, Element.spacing 160 ]
                    [ Input.button
                        [ Background.color (Element.rgb 0.0837 0.4361 0.4802)
                        , Border.rounded 4
                        , Element.centerX
                        , Element.padding 5
                        , Font.color (Element.rgb 1 1 1)
                        ]
                        { onPress = Just ReloadCats
                        , label = Element.text "Reload"
                        }
                    , Input.button
                        [ Background.color (Element.rgb 0.8 0 0)
                        , Border.rounded 4
                        , Element.centerX
                        , Element.padding 5
                        , Font.color (Element.rgb 1 1 1)
                        ]
                        { onPress = Just ResetClicks
                        , label = Element.text "Reset"
                        }
                    ]
                ]

        Loading ->
            Element.el
                [ Element.centerX, Element.centerY, Region.heading 1 ]
                (Element.text "Loading...")

        Failure httpError ->
            Element.column
                [ Element.centerY ]
                [ Element.el
                    [ Element.centerX, Region.heading 1 ]
                    (Element.text "HTTP Error :O")
                , Element.el
                    [ Element.centerX ]
                    (Element.text <| httpErrorToEnglish httpError)
                ]


renderWhenJust : (a -> Element msg) -> Maybe a -> Element msg
renderWhenJust viewFunc maybeData =
    case maybeData of
        Just data ->
            viewFunc data

        Nothing ->
            Element.none


applyWhenJust : a -> Maybe (a -> a) -> a
applyWhenJust data maybeFunc =
    case maybeFunc of
        Just func ->
            func data

        Nothing ->
            data


httpErrorToEnglish : Http.Error -> String
httpErrorToEnglish error =
    case error of
        Http.BadUrl url ->
            "I was expecting a valid URL, but I got the url: " ++ url

        Http.Timeout ->
            "It took too long to get a response from the server!"

        Http.NetworkError ->
            "Unable to make a connection. Is your network working?"

        Http.BadStatus code ->
            "The response gave me the error code: " ++ String.fromInt code

        Http.BadBody errorMessage ->
            "I failed because of the following error: " ++ errorMessage


selectorView : Selection Cat -> Element Msg
selectorView cats =
    Input.radioRow [ Element.spacingXY 50 0 ]
        { onChange = SwitchCat
        , options =
            List.map
                (\cat ->
                    Input.option cat (Element.text cat.name)
                )
                (Selection.toList cats)
        , selected = Selection.selected cats
        , label = Input.labelAbove [ Element.centerX ] (Element.text "Choose a cat!")
        }


catView : Cat -> Element Msg
catView cat =
    Element.column
        [ Element.spacing 5 ]
        [ Element.el
            [ Element.centerX, Region.heading 2 ]
            (Element.text cat.name)
        , Element.image
            [ Element.centerX
            , Element.height (Element.px 320)
            , Events.onClick ClickCat
            ]
            { src = cat.imgSrc, description = "Picture of " ++ cat.name }
        , Element.el
            [ Element.centerX, Region.heading 2 ]
            (Element.text <| String.fromInt cat.clicks ++ " clicks")
        ]



-- JSON


encodeCat : Cat -> Encode.Value
encodeCat cat =
    Encode.object
        [ ( "name", Encode.string cat.name )
        , ( "img", Encode.string cat.imgSrc )
        , ( "clicks", Encode.int cat.clicks )
        ]


decodeCat : Decoder Cat
decodeCat =
    Decode.map4 Cat
        (Decode.field "name" Decode.string)
        (Decode.field "img" Decode.string)
        (Decode.field "clicks" Decode.int)
        (Decode.succeed Nothing)


decodeCats : Decoder (List Cat)
decodeCats =
    Decode.map
        (List.map
            (\( id, cat ) ->
                { cat | id = Just id }
            )
        )
        (Decode.keyValuePairs decodeCat)



-- HTTP


databaseUrl : String -> String
databaseUrl doc =
    Url.Builder.crossOrigin "https://cat-clicker-74048.firebaseio.com"
        [ doc ++ ".json" ]
        [ Url.Builder.string
            "auth"
            "p3QPtRbLLJKHjtXdNzs4WkZiSxULEylsWt8T8M1W"
        ]


httpRetrieveCats : Cmd Msg
httpRetrieveCats =
    Http.get
        { url = databaseUrl "/cats"
        , expect = Http.expectJson LoadCats decodeCats
        }


httpUpdateCat : Cat -> Cmd Msg
httpUpdateCat cat =
    case cat.id of
        Just catId ->
            Http.request
                { method = "PUT"
                , headers = []
                , url = databaseUrl ("/cats/" ++ catId)
                , body = Http.jsonBody (encodeCat cat)
                , expect = Http.expectWhatever UploadedCat
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = always init
        , update = update
        , view =
            \model ->
                { title = "Cat Clicker"
                , body = [ Element.layout [] <| view model ]
                }
        , subscriptions = always Sub.none
        }
