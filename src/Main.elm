import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Task exposing (Task)
import Json.Decode as Decode
import Url exposing (..)
import Url.Builder exposing (..)
import Url.Parser exposing (..)


-- MODEL


type alias Model =
    {title : String
    , stories : List Story
    , loading : Bool
    , key : Nav.Key
    , route : Route
    }

type alias Story = 
  { title : String
  , author : String
  , time : Int
  , url : Maybe String
  , comments : Maybe (List Int)
  }

type alias NavElement = (String, String)

type Route
    = News
    | Top
    | Best
    | NotFound



initialModel : Route -> Nav.Key -> Model
initialModel route key =
    {title = "Hacker News Reader"
    ,stories = []
    , loading = False
    , key = key
    , route = route}
    


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flag u key =
    ( initialModel (parseUrl u) key, fetchList "newstories.json")  



-- MESSAGES


type Msg
    = GetStory (Result Http.Error (List Story))
    | GotList (Result Http.Error (List Int))
    | Fetch String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch str -> 
            ( model
            , fetchList str)

        GotList result ->
            case result of
                Ok res ->
                    ( { model | loading = True } 
                    , getStories 30 res)

                Err _->
                    (model, Cmd.none)

        GetStory result ->
            case result of
                Ok res ->
                    ({ model | stories = res, loading = False }
                    , Cmd.none
                    )
                Err er ->
                    (model, Cmd.none)

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal path ->
                    ( model, Nav.pushUrl model.key (Url.toString path) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged path ->
            let
                newRoute =
                    parseUrl path
            in
                ( { model | route = newRoute }, page model.route )

page : Route -> Cmd Msg
page route =
    case route of
        News ->
            fetchList "newstories.json"
        
        Best ->
            fetchList "beststories.json"

        Top -> 
            fetchList "topstories.json"
        
        NotFound ->
            fetchList "newstories.json"


-- VIEW


view : Model -> Browser.Document Msg
view model =
    {title = "Hacker News"
    , body = [ viewHeader model.title
        , if model.loading then viewLoading else viewStories model
        ]
    }
    

viewLoading : Html Msg
viewLoading =
    div [ style "width" "100vw"
        , style "height" "80vh"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"]
        [h1 [] [text "LOADING..."]]


viewStories : Model -> Html Msg
viewStories model =
    ul [ class "list-container"] (List.map viewStory model.stories)

viewHeader : String -> Html Msg
viewHeader title =
    header [ class "main-header flex"]
        [ h1 [] [ text title]
        , menu [ class "main-nav flex"] (List.map viewNav [ (newsPath, "News"), (topPath, "Top"), (bestPath, "Best")])
        ]

viewNav : NavElement -> Html Msg
viewNav link =
    li []
    [ a [ href (Tuple.first link)] [ text (Tuple.second link) ] ]
    

viewStory : Story -> Html Msg
viewStory story =
    li [ class "story"]
        [ a [href (maybeUrl story.url)] [ text story.title]
        , div []
        [span [] [ text ("by: " ++ story.author) ]
        , viewComments story.comments
        ]
        
        ]

viewComments : Maybe (List Int) -> Html Msg
viewComments arr =
    case arr of
        Just a -> span [style "margin-left" "10px"] [ text ((String.fromInt (List.length a)) ++ " comments")]

        Nothing -> span [] []

maybeUrl : Maybe String -> String
maybeUrl link =
    case link of
        Just a -> a

        Nothing -> "#"

-- HTTP

url : String
url =
    "https://hacker-news.firebaseio.com"

toApiUrl : String -> String
toApiUrl topic =
  Url.Builder.crossOrigin url ["v0", topic][]


toStoryUrl : String -> String
toStoryUrl id =
  Url.Builder.crossOrigin url ["v0", "item", (id ++ ".json")][]


decode : Decode.Decoder Story
decode =
    Decode.map5 Story
      (Decode.at ["title"] Decode.string)
      (Decode.at ["by"] Decode.string)
      (Decode.at ["time"] Decode.int)
      (Decode.maybe (Decode.at ["url"] Decode.string))
      (Decode.maybe (Decode.at ["kids"] (Decode.list Decode.int)))


fetchList : String -> Cmd Msg
fetchList tag =
    Http.get (toApiUrl tag) (Decode.list Decode.int)
        |> Http.toTask
        |> Task.attempt GotList



fetchStory : Int -> Task Http.Error Story
fetchStory id =
    Http.get (toStoryUrl (String.fromInt id)) decode
        |> Http.toTask


getStories : Int -> (List Int) -> Cmd Msg
getStories n list =
    List.take n list
        |> List.map fetchStory
        |> Task.sequence
        |> Task.attempt GetStory


-- PARSERS

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ Url.Parser.map News (Url.Parser.s "build")
        , Url.Parser.map News (Url.Parser.s "news")
        , Url.Parser.map Top (Url.Parser.s "top")
        , Url.Parser.map Best (Url.Parser.s "best")
        ]


parseUrl : Url -> Route
parseUrl path =
    case parse matchers path of
        Just route ->
            route

        Nothing ->
            NotFound


pathFor : Route -> String
pathFor route =
    case route of
        News ->
            "/news"

        Top ->
            "/top"
          
        Best ->
            "/best"

        NotFound ->
            "/error"


newsPath =
    pathFor News

topPath =
    pathFor Top

bestPath =
    pathFor Best



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

