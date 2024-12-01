module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)
import Time
import Util.Time


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config now posts =
    -- div [] []
    -- Debug.todo "postTable"
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ text "Score" ]
                , Html.th [] [ text "Title" ]
                , Html.th [] [ text "Type" ]
                , Html.th [] [ text "Posted" ]
                , Html.th [] [ text "Link" ]
                ]
            ]
        , Html.tbody []
            (posts
                |> filterPosts config
                |> List.map (\post ->
                    Html.tr []
                        [ Html.td [ Html.Attributes.class "post-score" ] 
                            [ text (String.fromInt post.score) ]
                        , Html.td [ Html.Attributes.class "post-title" ] 
                            [ text post.title ]
                        , Html.td [ Html.Attributes.class "post-type" ] 
                            [ text post.type_ ]
                        , Html.td [ Html.Attributes.class "post-time" ] 
                            [ text (Util.Time.formatTime Time.utc post.time ++ 
                                  " (" ++ 
                                  (Util.Time.durationBetween post.time now
                                    |> Maybe.map Util.Time.formatDuration
                                    |> Maybe.withDefault "") ++
                                  " ago)") 
                            ]
                        , Html.td [ Html.Attributes.class "post-url" ] 
                            [ case post.url of
                                Just url -> Html.a [ href url ] [ text "Link" ]
                                Nothing -> text "N/A"
                            ]
                        ]
                )
            )
        ]

{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    -- div [] []
    -- Debug.todo "postsConfigView"
    div []
        [ div []
            [ text "Posts per page: "
            , Html.select 
                [ Html.Attributes.id "select-posts-per-page"
                , Html.Events.onInput (\s -> 
                    String.toInt s 
                        |> Maybe.map (ChangePostsToShow >> ConfigChanged)
                        |> Maybe.withDefault (ConfigChanged (ChangePostsToShow config.postsToShow))
                  )
                ]
                [ Html.option 
                    [ Html.Attributes.value "10"
                    , Html.Attributes.selected (config.postsToShow == 10)
                    ] [ text "10" ]
                , Html.option 
                    [ Html.Attributes.value "25"
                    , Html.Attributes.selected (config.postsToShow == 25)
                    ] [ text "25" ]
                , Html.option 
                    [ Html.Attributes.value "50"
                    , Html.Attributes.selected (config.postsToShow == 50)
                    ] [ text "50" ]
                ]
            ]
        , div []
            [ text "Sort by: "
            , Html.select 
                [ Html.Attributes.id "select-sort-by"
                , Html.Events.onInput (\s -> 
                    sortFromString s
                        |> Maybe.map (ChangeSortBy >> ConfigChanged)
                        |> Maybe.withDefault (ConfigChanged (ChangeSortBy config.sortBy))
                  )
                ]
                (sortOptions |> List.map (\sort ->
                    Html.option 
                        [ Html.Attributes.value (sortToString sort)
                        , Html.Attributes.selected (config.sortBy == sort)
                        ] 
                        [ text (sortToString sort) ]
                ))
            ]
        , div []
            [ Html.input 
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.checked config.showJobs
                , Html.Events.onCheck (ChangeShowJobs >> ConfigChanged)
                ] []
            , text " Show job posts"
            ]
        , div []
            [ Html.input 
                [ Html.Attributes.type_ "checkbox"
                , Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.checked config.showTextOnly
                , Html.Events.onCheck (ChangeShowTextOnly >> ConfigChanged)
                ] []
            , text " Show text-only posts"
            ]
        ]