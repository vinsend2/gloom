module Main exposing (main)

import Browser
import Html exposing (Html, a, button, div, h1, img, input, label, li, ol, p, span, text)
import Html.Attributes exposing (alt, checked, class, for, href, id, name, placeholder, src, target, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Random
import Random.List


seedFromInput : String -> Random.Seed
seedFromInput str =
    str
        |> String.toList
        |> List.map Char.toCode
        |> List.sum
        |> Random.initialSeed


type SeedInput
    = Empty
    | SeedValue String


type Set
    = OriginalSet
    | SatireSet
    | CombinedSet


type alias Model =
    { seedInput : SeedInput
    , setSelection : Set
    , showPlayer1Cards : Bool
    , showPlayer2Cards : Bool
    , showPlayer3Cards : Bool
    , showPlayer4Cards : Bool
          , showPlayer5Cards : Bool
                , showPlayer6Cards : Bool
                , showPlayer7Cards : Bool
                , showPlayer8Cards : Bool
                     , showPlayer9Cards : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seedInput = Empty
      , setSelection = CombinedSet
      , showPlayer1Cards = False
      , showPlayer2Cards = False
      , showPlayer3Cards = False
      , showPlayer4Cards = False
            , showPlayer5Cards = False
                , showPlayer6Cards = False
                , showPlayer7Cards = False
                , showPlayer8Cards = False
                     , showPlayer9Cards = False
      }
    , Cmd.none
    )


type Msg
    = HandleSeedInput String
    | HandleSetSelection Set Bool
    | ToggleShowPlayer1Cards
    | ToggleShowPlayer2Cards
    | ToggleShowPlayer3Cards
    | ToggleShowPlayer4Cards
    | ToggleShowPlayer5Cards
    | ToggleShowPlayer6Cards
    | ToggleShowPlayer7Cards
    | ToggleShowPlayer8Cards
    | ToggleShowPlayer9Cards
   
 
   
    
    | FillRandomSeed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSeedInput "" ->
            ( { model
                | seedInput = Empty
                , showPlayer1Cards = False
                , showPlayer2Cards = False
                , showPlayer3Cards = False
                , showPlayer4Cards = False
                 , showPlayer5Cards = False
                , showPlayer6Cards = False
                , showPlayer7Cards = False
                , showPlayer8Cards = False
                     , showPlayer9Cards = False
              }
            , Cmd.none
            )

        HandleSetSelection set _ ->
            ( { model
                | setSelection = set
                , showPlayer1Cards = False
                , showPlayer2Cards = False
                , showPlayer3Cards = False
                , showPlayer4Cards = False
                      , showPlayer5Cards = False
                , showPlayer6Cards = False
                , showPlayer7Cards = False
                , showPlayer8Cards = False
                     , showPlayer9Cards = False
              }
            , Cmd.none
            )

        HandleSeedInput str ->
            ( { model | seedInput = SeedValue str }, Cmd.none )

        ToggleShowPlayer1Cards ->
            ( { model | showPlayer1Cards = not model.showPlayer1Cards }, Cmd.none )

        ToggleShowPlayer2Cards ->
            ( { model | showPlayer2Cards = not model.showPlayer2Cards }, Cmd.none )

        ToggleShowPlayer3Cards ->
            ( { model | showPlayer3Cards = not model.showPlayer3Cards }, Cmd.none )

        ToggleShowPlayer4Cards ->
            ( { model | showPlayer4Cards = not model.showPlayer4Cards }, Cmd.none )

        ToggleShowPlayer5Cards ->
            ( { model | showPlayer5Cards = not model.showPlayer5Cards }, Cmd.none )

        ToggleShowPlayer6Cards ->
            ( { model | showPlayer6Cards = not model.showPlayer6Cards }, Cmd.none )

        ToggleShowPlayer7Cards ->
            ( { model | showPlayer7Cards = not model.showPlayer7Cards }, Cmd.none )

        ToggleShowPlayer8Cards ->
            ( { model | showPlayer8Cards = not model.showPlayer8Cards }, Cmd.none )

        ToggleShowPlayer9Cards ->
            ( { model | showPlayer9Cards = not model.showPlayer9Cards }, Cmd.none )

       

        FillRandomSeed ->
            let
                randomChar : Random.Generator Char
                randomChar =
                    Random.map (\num -> Char.fromCode num) (Random.int 97 122)

                randomString : Random.Generator String
                randomString =
                    Random.map String.fromList (Random.list 10 randomChar)
            in
            ( model, Random.generate HandleSeedInput randomString )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


renderCard : String -> Html Msg
renderCard imageUrl =
    div [ class "flip-card" ]
        [ div [ class "flip-card-inner" ]
            [ div [ class "flip-card-back" ] [ img [ class "card", src imageUrl ] [] ]
            , div [ class "flip-card-front" ] [ img [ class "card", src "battle-goal-back.jpg" ] [] ]
            ]
        ]


renderPlayer : Msg -> Bool -> Int -> Int -> Int -> Html Msg
renderPlayer handleClick showCards first second index =
    let
        imageUrl : Int -> String
        imageUrl number =
            String.fromInt number ++ ".png" 
    in
    div
        [ class "player"
        , class
            (if showCards then
                "flip"

             else
                ""
            )
        ]
        [ div [  ] [  ]
        
           
        , div [ class "cards-container", onClick handleClick  ]
            [ renderCard (imageUrl first)
            
            ]
        ]


renderPlayers : Model -> String -> List (Html Msg)
renderPlayers model seedValue =
    let
        ( min, max ) =
            case model.setSelection of
                OriginalSet ->
                    ( 55, 79 )

                SatireSet ->
                    ( 1, 54 )

                CombinedSet ->
                    ( 1, 78 )

        numbers : List Int
        numbers =
            seedValue
                |> seedFromInput
                |> Random.step (Random.List.shuffle (List.range min max))
                |> Tuple.first
                |> List.take 9
    in
    case numbers of
        [ first, second, third, fourth, fifth, sixth, seventh, eigth, nine ] ->
            [ renderPlayer ToggleShowPlayer1Cards model.showPlayer1Cards first second 1
            , renderPlayer ToggleShowPlayer2Cards model.showPlayer2Cards second fourth 2
            , renderPlayer ToggleShowPlayer3Cards model.showPlayer3Cards third sixth 3
            , renderPlayer ToggleShowPlayer4Cards model.showPlayer4Cards fourth eigth 4
              , renderPlayer ToggleShowPlayer5Cards model.showPlayer5Cards fifth eigth 5
                  , renderPlayer ToggleShowPlayer6Cards model.showPlayer6Cards sixth eigth 6
                      , renderPlayer ToggleShowPlayer7Cards model.showPlayer7Cards seventh eigth 7
                          , renderPlayer ToggleShowPlayer8Cards model.showPlayer8Cards eigth eigth 8
                              , renderPlayer ToggleShowPlayer9Cards model.showPlayer9Cards nine eigth 9
            ]

        _ ->
            [ div [] [ text "Something went wrong." ] ]


renderPlayersContainer : Model -> Html Msg
renderPlayersContainer model =
    case model.seedInput of
        Empty ->
            div [ class "players-container" ] []

        SeedValue seedValue ->
            div [ class "players-container" ] (renderPlayers model seedValue)


renderSeedInput : Model -> Html Msg
renderSeedInput model =
    let
        seedInputValue : String
        seedInputValue =
            case model.seedInput of
                Empty ->
                    ""

                SeedValue seedValue ->
                    seedValue
    in
    div [ class "seed-input" ]
        [ div []
            [ label [ for "seed-input" ] [ text "Seed Input" ]
            , input [ id "seed-input", onInput HandleSeedInput, placeholder "Enter seed", value seedInputValue ] [ text "" ]
            ]
        , div [] [ button [ onClick FillRandomSeed ] [ text "Generate Random Seed" ] ]
        ]


renderSetSelection : Model -> Html Msg
renderSetSelection model =
    let
        radioButton : String -> String -> Set -> Html Msg
        radioButton idString labelString set =
            div
                [ class "radio-button-container"
                , class
                    (if model.setSelection == set then
                        "selected"

                     else
                        ""
                    )
                ]
                [ input
                    [ type_ "radio"
                    , id idString
                    , name "set-selection"
                    , onCheck (HandleSetSelection set)
                    , checked (model.setSelection == set)
                    ]
                    []
                , label [ for idString ] [ text labelString ]
                ]
    in
    div [ class "set-selection" ]
        [ p [] [ text "Cards Used" ]
        , div [ class "radio-button-group" ]
            [ 
          radioButton "option-combined" "All" CombinedSet
            ]
        ]


renderTopSection : Html Msg
renderTopSection =
    div []
        [ h1 [] [ text "Battle Goals Generator"]
        
        , p [] [ text "Что мне мне делать помогите??!!11" ]
        , ol []
            [ li [] [ span [ class "emphasize" ] [ text "Придумайте" ], span [] [ text " " ] ]
   
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "top-section" ] [ renderTopSection ]
        , div [ class "main-section" ]
            [ renderSetSelection model
            , renderSeedInput model
            , renderPlayersContainer model
            ]
        , div [ class "source-link" ]
            [ a [ href "https://github.com/vinsend2/battle-goals", target "_blank" ]
                [ img [ alt "Github Mark", class "github-mark", src "GitHub-Mark-32px.png" ] []
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
