import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Task exposing (Task)
import Json.Decode as Decode
import Url exposing (..)
import Url.Builder


-- MODEL


type alias Model =
    {title : String
    , stories : List Story
    }

type alias Story = 
  { title : String
  , author : String
  , time : Int
  }


init : () -> ( Model, Cmd Msg )
init _=
    ( Model "Hacker News Reader" [] , Cmd.none ) --fetchCmd 



-- MESSAGES


type Msg
    = Fetch (Result Http.Error (List Story))


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        
        Fetch result ->
          case result of
            Ok stories ->
              ( { model | stories = stories }, Cmd.none )

            Err erro ->
              ( model, Cmd.none)

        



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeader model.title
        , ul [ class "list-container"] (List.map viewStory model.stories)
        ]

viewHeader : String -> Html Msg
viewHeader title =
    header [ class "main-header flex"]
        [ h1 [] [ text title]
        , ul [ class "main-nav flex"] (List.map viewNav [ "News", "Jobs", "Best Stories"])
        ]

viewNav : String -> Html Msg
viewNav link =
    li []
    [ a [ href link] [ text link ] ]
    

viewStory : Story -> Html Msg
viewStory story =
    li []
        [ h2 [] [ text story.title]
        , p [] [ text ("by: " ++ story.author) ]

        ]

-- HTTP

url : String
url =
    "https://hacker-news.firebaseio.com"

toApiUrl : String -> String
toApiUrl topic =
  Url.Builder.crossOrigin url ["v0", topic][]


toStoryUrl : String -> String
toStoryUrl id =
  Url.Builder.crossOrigin url ["v0", "item", id][]


decode : Decode.Decoder Story
decode =
    Decode.map3 Story
      (Decode.at ["title"] Decode.string)
      (Decode.at ["author"] Decode.string)
      (Decode.at ["time"] Decode.int)


fetchList : String -> Task Http.Error (List String)
fetchList tag =
    Http.get (toApiUrl tag) (Decode.list Decode.string)
        |> Http.toTask


fetchUserList : (List String) -> (List (Task Http.Error Story))
fetchUserList list =
    List.map toStoryUrl list
        |> List.map fetchTask


fetchTask : String -> Task Http.Error Story
fetchTask id =
    Http.get id decode
      |> Http.toTask


-- fetchCmd : Cmd Msg
-- fetchCmd =
--   fetchList
--     |> Task.attempt fetchUserList





-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

stylesheet =
    let
        tag = "link"
        attrs =
            [ attribute "rel"       "stylesheet"
            , attribute "property"  "stylesheet"
            , attribute "href"      "./style.css"
            ]
        children = []
    in 
        node tag attrs children
