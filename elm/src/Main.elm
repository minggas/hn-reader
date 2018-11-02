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
import Markdown exposing (..)

-- MODEL


type alias Model =
    {title : String
    , stories : List Story
    , comments : List Comment
    , loading : Bool
    , key : Nav.Key
    , route : Route
    }

type alias Story = 
  { title : String
  , id : Int
  , author : String
  , time : Int
  , score : Int
  , url : Maybe String
  , comments : Maybe (List Int)
  }

type alias Comment =
  { text : Maybe String
  , kids : Maybe (List Int)
  }

type alias NavElement = (String, String)

type alias StoryId =
    String

type Route
    = News
    | Top
    | Best
    | Comments StoryId
    | NotFound



initialModel : Route -> Nav.Key -> Model
initialModel route key =
    {title = "Hacker News Reader"
    ,stories = []
    , comments = []
    , loading = False
    , key = key
    , route = News}
    


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ initialUrl key =
    ( initialModel (parseUrl initialUrl) key, fetchList "newstories.json")  



-- MESSAGES


type Msg
    = GetStory (Result Http.Error (List Story))
    | GetComments (Result Http.Error (List Comment))
    | GetList (Result Http.Error (List Int))
    | GetCommentList (Result Http.Error (List Int))
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

        GetCommentList result ->
            case result of
                Ok res ->
                    ({ model | loading = True }
                    , getComments 30 res
                    )
                Err _ ->
                    (model, Cmd.none)

        GetComments result ->
            case result of
                Ok res ->
                    ({ model | comments = res, loading = False }
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
        , if model.loading then viewLoading else viewContent model
        ]
    }
    
viewContent : Model -> Html Msg
viewContent model =
    case model.route of
        Comments id ->
            viewComments model     
    
        News ->
            viewStories model

        Top ->
            viewStories model
        
        Best ->
            viewStories model
        
        _ -> 
            viewStories model
            


viewLoading : Html Msg
viewLoading =
    div [ style "width" "100%"
        , style "height" "80vh"
        , style "display" "flex"
        , style "align-items" "center"
        , style "justify-content" "center"]
        [div [class "spinner"][]]


viewStories : Model -> Html Msg
viewStories model =
    ul [ class "list-container"] (List.map viewStory model.stories)

viewComments : Model -> Html Msg
viewComments model =
    ul [ class "list-container"] (List.map viewComment model.comments)

viewHeader : String -> Route -> Html Msg
viewHeader title route =
    header [ class "main-header flex"]
        [ h1 [] [ text title]
        , menu [ class "main-nav flex"] <| viewNav route (newsPath, "News") :: viewMenu route
        ]

viewMenu : Route -> List (Html Msg)
viewMenu route =
    let
        linkTo =
            viewNav route
    in
        [linkTo (bestPath, "Best")
        ,linkTo (topPath, "Top")]
    

viewNav : Route -> NavElement -> Html Msg
viewNav route link =
    li []
    [ a [ href (Tuple.first link), classList [ ( "active", isActive route (Tuple.second link)) ]] [ text (Tuple.second link) ] ]


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
        [span [] [ text ((String.fromInt story.score) ++ "points")]
        , span [] [ text ("by: " ++ story.author) ]
        , commentsCounter story.comments story.id
        ]
        
        ]

viewComment : Comment -> Html Msg
viewComment comment =
    li [ class "comment"]
        <| Markdown.toHtml Nothing (maybeComment comment.text)

maybeComment : Maybe String -> String
maybeComment text =
    case text of
        Just a -> a
            
        Nothing -> ""

commentsCounter : Maybe (List Int) -> Int -> Html Msg
commentsCounter arr id =
    case arr of
        Just b -> a [style "margin-left" "10px", href (commentPath (String.fromInt id))] [ text ((String.fromInt (List.length b)) ++ " comments")]

        Nothing -> span [] []

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


toItemUrl : String -> String
toItemUrl id =
  Url.Builder.crossOrigin url ["v0", "item", (id ++ ".json")][]


decodeStory : Decode.Decoder Story
decodeStory =
    Decode.map7 Story
      (Decode.field "title" Decode.string)
      (Decode.field "id" Decode.int)
      (Decode.field "by" Decode.string)
      (Decode.field "time" Decode.int)
      (Decode.field "score" Decode.int)
      (Decode.maybe (Decode.field "url" Decode.string))
      (Decode.maybe (Decode.field "kids" (Decode.list Decode.int)))

decodeComment : Decode.Decoder Comment
decodeComment =
    Decode.map2 Comment
        (Decode.maybe (Decode.field "text" Decode.string))
        (Decode.maybe (Decode.field "kids" (Decode.list Decode.int)))



fetchList : String -> Cmd Msg
fetchList tag =
    Http.get (toApiUrl tag) (Decode.list Decode.int)
        |> Http.toTask
        |> Task.attempt GetList

fetchComments : String -> Cmd Msg
fetchComments id =
    Http.get (toItemUrl id) (Decode.field "kids" (Decode.list Decode.int))
        |> Http.toTask
        |> Task.attempt GetCommentList

fetchStory : Int -> Task Http.Error Story
fetchStory id =
    Http.get (toItemUrl (String.fromInt id)) decodeStory
        |> Http.toTask

fetchComment : Int -> Task Http.Error Comment
fetchComment id =
    Http.get (toItemUrl (String.fromInt id)) decodeComment
        |> Http.toTask

getStories : Int -> (List Int) -> Cmd Msg
getStories n list =
    List.take n list
        |> List.map fetchStory
        |> Task.sequence
        |> Task.attempt GetStory

getComments : Int -> (List Int) -> Cmd Msg
getComments n list =
    List.take n list
        |> List.map fetchComment
        |> Task.sequence
        |> Task.attempt GetComments


-- PARSERS

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ Url.Parser.map News top
        , Url.Parser.map Comments (Url.Parser.s "comment" </> Url.Parser.string)
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

        Comments id ->
            "/comment/" ++ id

        NotFound ->
            "/error"


newsPath =
    pathFor News

topPath =
    pathFor Top

bestPath =
    pathFor Best

commentPath id =
    pathFor (Comments id)

page : Route -> Cmd Msg
page route =
    case route of
        News ->
            fetchList "newstories.json"
        
        Best ->
            fetchList "beststories.json"

        Top -> 
            fetchList "topstories.json"

        Comments id ->
            fetchComments id
        
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

