module Main exposing (Model, init, main, update, view)

import Browser
import DateTimePicker exposing (DateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import JapaneseCalendar as JC


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { year : String
    , month : String
    , day : String
    , convertedString : Maybe String
    , errors : List String
    }


init : Model
init =
    { year = ""
    , month = ""
    , day = ""
    , convertedString = Nothing
    , errors = []
    }



-- UPDATE


type Msg
    = UpdateYear String
    | UpdateMonth String
    | UpdateDay String
    | ConvertToJapaneseCalendar


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateYear year ->
            { model | year = year }

        UpdateMonth month ->
            { model | month = month }

        UpdateDay day ->
            { model | day = day }

        ConvertToJapaneseCalendar ->
            let
                ymd =
                    Result.map3 JC.ymd
                        (String.toInt model.year |> Result.fromMaybe "year is not number!")
                        (String.toInt model.month |> Result.fromMaybe "month is not number!")
                        (String.toInt model.day |> Result.fromMaybe "day is not number!")

                resultConvertedString =
                    ymd
                        |> Result.mapError List.singleton
                        |> Result.andThen JC.fromYMD
                        |> Result.map JC.toString

                ( convertedString, errors ) =
                    case resultConvertedString of
                        Ok str ->
                            ( Just str, [] )

                        Err errs ->
                            ( Nothing, errs )
            in
            { model
                | convertedString = convertedString
                , errors = errors
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "number", value model.year, placeholder "year", onInput UpdateYear ] []
        , input [ type_ "number", value model.month, placeholder "month", onInput UpdateMonth ] []
        , input [ type_ "number", value model.day, placeholder "day", onInput UpdateDay ] []
        , button [ onClick ConvertToJapaneseCalendar ] [ text "変換" ]
        , div []
            [ ul [] <|
                List.map
                    (\error ->
                        li [] [ text error ]
                    )
                    model.errors
            ]
        , div []
            (model.convertedString
                |> Maybe.map (text >> List.singleton)
                |> Maybe.withDefault []
            )
        ]
