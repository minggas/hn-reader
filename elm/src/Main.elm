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
    , route = News}
    


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ initialUrl key =
    ( initialModel (parseUrl initialUrl) key, fetchList "newstories.json")  



-- MESSAGES


type Msg
    = GetStory (Result Http.Error (List Story))
    | GetList (Result Http.Error (List Int))
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url




-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetList result ->
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
                ( { model | route = newRoute }, page newRoute )


-- VIEW


view : Model -> Browser.Document Msg
view model =
    {title = "Hacker News"
    , body = [ viewHeader model.title model.route
        , if model.loading then viewLoading else viewStories model
        , viewFooter
        ]    
    }
    

viewLoading : Html Msg
viewLoading =
    div [ style "width" "100%"
        , style "height" "88vh"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"]
        [div [class "spinner"][]]


viewStories : Model -> Html Msg
viewStories model =
    ul [ class "list-container"] (List.map viewStory model.stories)

viewHeader : String -> Route -> Html Msg
viewHeader title route =
    header [ class "main-header flex", style "height" "7vh"]
        [ h1 [style "font-size" "calc(2vw + 10px)"] [ text title]
        , menu [ class "main-nav flex"] <| viewNav route (newsPath, "News") :: viewMenu route
        ]
viewFooter : Html Msg
viewFooter =
    footer [style "text-align" "center"
           , style "padding" "0.5rem 2rem"
           , style "background-color" "#777"] 
        [ span [] [text "Develop by ", a [ href "https://minggas.com", target "_blank" ] [text "Minggas"]] ]

viewMenu : Route -> List (Html Msg)
viewMenu route =
    let
        linkTo =
            viewNav route
    in
        [linkTo (bestPath, "Best")
        ,linkTo (topPath, "Top")]
    

viewNav : Route -> NavElement -> Html Msg
viewNav r link =
    li []
    [ a [ href (Tuple.first link), classList [ ( "active", isActive r (Tuple.second link)) ]] [ text (Tuple.second link) ] ]


isActive : Route -> String -> Bool
isActive route text =
    case (route, text) of
        (News, "News") ->
            True
    
        (Best, "Best") ->
            True
        
        (Top, "Top") ->
            True

        _ ->
            False

viewStory : Story -> Html Msg
viewStory story =
    li [ class "story"]
        [ maybeUrl story.url story.title
        , div [ class "story-info"]
            <| span [] [ text ("by: " ++ story.author) ] :: viewCommentsCounter story.comments
        ]
        
        

viewCommentsCounter : Maybe (List Int) -> List (Html Msg)
viewCommentsCounter arr =
    case arr of
        Just a -> [span [style "margin-left" "10px"] [ text ((String.fromInt (List.length a)) ++ " comments")]]

        Nothing -> []

maybeUrl : Maybe String -> String -> Html Msg
maybeUrl link t =
    case link of
        Just el -> a [href el, class "story-title", target "_blank"] [ text t]

        Nothing -> span [class "story-title"] [text t]

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
        |> Task.attempt GetList



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
        [ Url.Parser.map News top
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
            "news"

        Top ->
            "top"
          
        Best ->
            "best"

        NotFound ->
            "error"


newsPath =
    pathFor News

topPath =
    pathFor Top

bestPath =
    pathFor Best

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

