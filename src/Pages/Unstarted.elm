module Pages.Unstarted exposing (..)

import Element exposing (Element, centerX, centerY, column, el, mouseOver, text)
import Element.Background as Background
import Element.Font as Font
import Game.Scorebook exposing (ScorebookData)
import Message exposing (Msg(..))
import RemoteData exposing (RemoteData(..))
import UI.Common exposing (standardButtonView)
import UI.Palette exposing (..)


view : ScorebookData -> Element Msg
view wordsData =
    let
        content =
            case wordsData of
                NotAsked ->
                    [ standardButtonView
                        { attrs =
                            [ Background.color atlantis
                            , mouseOver [ Background.color <| hoverColor atlantis ]
                            ]
                        , onPress = Just ManuallyDownloadWords
                        , text = "Initialize Game"
                        }
                    ]

                Loading ->
                    [ el
                        [ centerX
                        , centerY
                        , Font.color chiffon
                        , Font.letterSpacing 2
                        , Font.size 22
                        ]
                      <|
                        text "Loading words dictionary..."
                    ]

                Success _ ->
                    [ standardButtonView
                        { attrs =
                            [ Background.color atlantis
                            , mouseOver [ Background.color <| hoverColor atlantis ]
                            ]
                        , onPress = Just ShuffleBoard
                        , text = "Start Game!"
                        }
                    ]

                Failure _ ->
                    [ text "Failed to load words dictionary :("
                    , standardButtonView
                        { attrs =
                            [ Background.color atlantis
                            , mouseOver [ Background.color <| hoverColor atlantis ]
                            ]
                        , onPress = Just ManuallyDownloadWords
                        , text = "Re-initialize Game?"
                        }
                    ]
    in
    column [ centerX, centerY ] content
