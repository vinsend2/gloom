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
    , remove1 : Bool
    , remove2 : Bool
    , remove3 : Bool
    , remove4 : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seedInput = Empty
      , setSelection = CombinedSet
      , showPlayer1Cards = False
      , showPlayer2Cards = False
      , showPlayer3Cards = False
      , showPlayer4Cards = False
      , remove1 = False
      , remove2 = False
      , remove3 = False
      , remove4 = False
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
    | ToggleShowPlayer1
    | ToggleShowPlayer2
    | ToggleShowPlayer3
    | ToggleShowPlayer4
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
                 , remove1 = False
                  , remove2 = False
                  , remove3 = False
                 , remove4 = False
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
                    , remove1 = False
                  , remove2 = False
                  , remove3 = False
                 , remove4 = False
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

        ToggleShowPlayer1 ->
            ( { model | remove1 = not model.remove1 }, Cmd.none )

        ToggleShowPlayer2 ->
            ( { model | remove2 = not model.remove2 }, Cmd.none )

        ToggleShowPlayer3 ->
            ( { model | remove3 = not model.remove3 }, Cmd.none )

        ToggleShowPlayer4 ->
            ( { model | remove4 = not model.remove4 }, Cmd.none )

            

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


renderPlayer : Msg -> Bool ->Bool ->Msg -> Int -> Int -> Int -> Html Msg
renderPlayer handleClick showCards remove removePlayer first second index =
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
            )   , class
            (if remove then
                "hide"

             else
                ""
            ) 

        ]
        [ div [ class "player-title" ] [ text ("Player " ++ String.fromInt index) ]
        , button [ class "toggle", onClick removePlayer  ]
            [ text
                (if showCards then
                    "Скрыть"

                 else
                    "Скрыть"
                )
            ]
        , div [ class "cards-container", onClick handleClick ]
            [ renderCard (imageUrl first)
            , renderCard (imageUrl second)
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
                |> List.take 8
    in
    case numbers of
        [ first, second, third, fourth, fifth, sixth, seventh, eigth ] ->
            [ renderPlayer ToggleShowPlayer1Cards model.showPlayer1Cards model.remove1 ToggleShowPlayer1 first second 1
            , renderPlayer ToggleShowPlayer2Cards model.showPlayer2Cards model.remove2 ToggleShowPlayer2 third fourth 2
            , renderPlayer ToggleShowPlayer3Cards model.showPlayer3Cards model.remove3 ToggleShowPlayer3 fifth sixth 3
            , renderPlayer ToggleShowPlayer4Cards model.showPlayer4Cards model.remove4 ToggleShowPlayer4 seventh eigth 4
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
        
            
        ]


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }
