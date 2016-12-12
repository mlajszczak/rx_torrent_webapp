import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Debug exposing (..)
import Json.Decode as Json
import Json.Decode exposing ((:=))
import Task



main =
  App.program
    { init = (initState, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

initState = Model "" [] ""

type alias Torrent =
    { name: String }

type alias Model =
  { currentPath : String
  , torrents : List Torrent
  , error : String
  }



-- UPDATE


type Msg
  = ChangeCurrentPath String
  | AddTorrent String
  | AddTorrentFail Http.Error
  | AddTorrentSuccess (Result String Torrent)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let errorTransition = \error -> ({ model | error = error }, Cmd.none) in
    case msg of
      ChangeCurrentPath path ->
        ({ model | currentPath = path }, Cmd.none)

      AddTorrent torrentPath ->
        (model, addTorrent torrentPath)

      AddTorrentSuccess torrent ->
        case torrent of
          Ok torrent -> ({ model | torrents = torrent :: model.torrents }, Cmd.none)
          Err error -> errorTransition error

      AddTorrentFail error ->
        case error of
          Http.Timeout -> errorTransition "operation timeout"
          Http.NetworkError -> errorTransition "network error"
          Http.UnexpectedPayload payload -> errorTransition ("bad response: " ++ payload)
          Http.BadResponse _ response -> errorTransition ("bad response: " ++ response)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Enter torrent path", onInput ChangeCurrentPath ] []
    , button [ onClick (AddTorrent model.currentPath) ] [ text "Add torrent!" ]
    , div [] [ text model.error ]
    , div [] [ viewTorrents model.torrents ]
    ]



viewTorrents : List Torrent -> Html Msg
viewTorrents torrents =
  ul [] <| List.map (\torrent -> li [] [ text torrent.name ]) torrents



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- HTTP


addTorrent : String -> Cmd Msg
addTorrent torrentPath =
  let
    url =
        Http.url "http://localhost:50000" [("torrentPath", torrentPath)]
  in
    Task.perform AddTorrentFail AddTorrentSuccess (Http.get decodeTorrent url)


decodeTorrent : Json.Decoder (Result String Torrent)
decodeTorrent = Json.oneOf
    [ Json.map (Torrent >> Ok) (Json.at ["info", "name"] Json.string)
    , Json.map Err ("error" := Json.string)
    ]

