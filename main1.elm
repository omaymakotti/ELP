module Main exposing (..)

import Browser
import Html exposing (Html, div, text, input, button, ul, li)
import Http
import Json.Decode as Json
import Random exposing (Generator, int, step, initialSeed, generate, map)
import List.Extra exposing (getAt)
import Maybe exposing (withDefault)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, value, placeholder)

import Json.Decode exposing (Decoder, list, field, string, map, map2)

viewMeaning : Meaning -> List (Html Msg)
viewMeaning meaning =
    [ div [] [ text ("Part of Speech: " ++ meaning.partOfSpeech) ]
    , ul [] (List.indexedMap viewDefinition meaning.definitions)
    ]

viewDefinition : Int -> String -> Html Msg
viewDefinition index definition =
    li [] [ text (definition) ]
type alias Meaning =
    { partOfSpeech : String
    , definitions : List String
    }

definitionDecoder : Decoder (List Meaning)
definitionDecoder =
    field "0" (field "meanings" (list meaningDecoder))

meaningDecoder : Decoder Meaning
meaningDecoder =
    map2 Meaning
        (field "partOfSpeech" string)
        (field "definitions" (list (field "definition" string)))

-- MODEL

type alias Model =
    { status : Status
    , words : List String
    , randomWord : String
    , definition : String
    , userInput : String
    , message : String
    ,showAnswer : Bool
    }


type Status
    = Loading
    | Success
    | Failure String


-- INIT

init : () -> (Model, Cmd Msg)
init _ =
    ( { status = Loading
      , words = []
      , randomWord = ""
      , definition = ""
      , userInput = ""
      , message = ""
      , showAnswer = False  -- Ajoutez le champ showAnswer avec une valeur par défaut
      }
    , Http.get
        { url = "/texte.txt"
        , expect = Http.expectString GotText
        }
    )


-- UPDATE

type Msg
    = GotText (Result Http.Error String)
    | GetRandomWord
    | GotRandomNumber Int
    | GotDefinition (Result Http.Error String)
    |CheckGuess  -- Message pour vérifier la devinette
    | UpdateInput String  -- Message pour mettre à jour la saisie utilisateur
    |ShowAnswer  
    


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotText (Ok fullText) ->
            let
                words = String.words fullText
            in
            ( { model | status = Success, words = words }
            , Random.generate GotRandomNumber (Random.int 0 (List.length words - 1))
            )

        GotText (Err err) ->
            ( { model | status = Failure (Debug.toString err) }, Cmd.none )

        GetRandomWord ->
            ( model, Cmd.none )

        GotRandomNumber index ->
            let
                word =
                    List.Extra.getAt index model.words
                        |> withDefault "default word"

                updatedModel =
                    { model | randomWord = word, words = List.append model.words [ word ] }
            in
            ( updatedModel
            , Http.get
                { url = "https://api.dictionaryapi.dev/api/v2/entries/en/" ++ word
                , expect = Http.expectString GotDefinition
                }
            )

        GotDefinition (Ok definition) ->
            ( { model | definition = definition }, Cmd.none )

        GotDefinition (Err err) ->
            ( { model | definition = "Error fetching definition: " ++ Debug.toString err }, Cmd.none )
        
        CheckGuess ->
            let
                isCorrectGuess =
                    String.toLower model.userInput == String.toLower model.randomWord
                newStatus =
                    if isCorrectGuess then Success else Failure "Incorrect guess"
                newMessage =
                    if isCorrectGuess then "Félicitations, le mot est juste !" else "Essayer autre fois."
            in
            ( { model | status = newStatus, message = newMessage }, Cmd.none )

        UpdateInput input ->
            ( { model | userInput = input }, Cmd.none )
        ShowAnswer ->
            ( { model | showAnswer = True }, Cmd.none )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- VIEW

view : Model -> Html Msg
view model =
    let
        containerAttrs = [ Html.Attributes.class "container" ]
        inputContainerAttrs = [ Html.Attributes.class "input-container" ]
        buttonContainerAttrs = [ Html.Attributes.class "button-container" ]
        messageContainerAttrs = [ Html.Attributes.class "message-container" ]
    in
        case model.status of
            Loading ->
                text "Chargement..."

            Success ->
                div []
                    [ 
                      div [] [ text model.definition ] -- Affichage de la définition
                    , div containerAttrs
                        [ if model.showAnswer then text model.randomWord else text "Cliquez sur 'Voir réponse' pour afficher la réponse"
                        , div inputContainerAttrs
                            [ input [ value model.userInput, placeholder "Entrez votre devinette", onInput UpdateInput ] []
                            ]
                        , div buttonContainerAttrs
                            [ button [ onClick CheckGuess ] [ text "Vérifier la devinette" ]
                            , button [ onClick ShowAnswer ] [ text "Voir réponse" ]
                            ]
                        , div messageContainerAttrs [ text model.message ]
                        ]
                    ]

            Failure err ->
                div containerAttrs [ text ("Essayer de nouveau. " ++ err) ]



    


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init ()
        , update = update
        , subscriptions = subscriptions
        , view = view
        }