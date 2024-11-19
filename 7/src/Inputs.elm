module Inputs exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (checked, placeholder, style, type_, value)
import Html.Events exposing (..)
import List exposing (length)
import String exposing (length)
import Html.Attributes exposing (disabled, hidden)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type AccountType
    = User
    | Admin


accountTypes : List AccountType
accountTypes = [User, Admin]

accountTypeToString ty =
    case ty of
        User ->
            "User"

        Admin ->
            "Admin"


accountTypeFromString s =
    case String.toLower s of
        "user" ->
            Just User

        "admin" ->
            Just Admin

        _ ->
            Nothing


type alias Model =
    { accountType : AccountType
    , activateAccount : Bool
    , username : String
    , password : String
    , confirmPassword : String
    , emailAddress : Maybe String
    }



type Msg
    = SelectedValue String
    | UsernameChanged String
    | PasswordChanged String
    | ConfirmPasswordChanged String
    | SetActivateAccount Bool


init : () -> ( Model, Cmd Msg )
init _ =
    ( { accountType = User, activateAccount = False, username = "", password = "", confirmPassword="", emailAddress = Nothing }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedValue s ->
            ( { model | accountType = accountTypeFromString s |> Maybe.withDefault User }
            , Cmd.none
            )

        UsernameChanged username ->
            ( { model | username = username }
            , Cmd.none
            )

        PasswordChanged password ->
            ( { model | password = password }
            , Cmd.none
            )

        ConfirmPasswordChanged confirmPassword ->
            ( { model | confirmPassword = confirmPassword }
            , Cmd.none
            )

        SetActivateAccount activate ->
            ( { model | activateAccount = activate }
            , Cmd.none
            )




accountTypeView : Html Msg
accountTypeView =
    div []
        [ select [ Html.Events.onInput SelectedValue ]
            [ option [ value "User" ] [ text "User" ]
            , option [ value "Admin" ] [ text "Admin" ]
            ]
        ]




accountDetailsView : Model -> Html Msg
accountDetailsView { username, password, confirmPassword, accountType } =
    let
        inputAttrs ty p v msg =
            [ type_ ty, placeholder p, value v, onInput msg ]
        matchy = confirmPassword == password
        enoughCh = 
          if accountType == Admin then
            (String.length password) >= 12
          else
            (String.length password) >= 8
        onButton = not ((String.length username) > 0 && (String.length password) > 0 && matchy && enoughCh)
        
    in
    div []
        [ input (inputAttrs "text" "username" username UsernameChanged) []
        , input (inputAttrs "password" "password" password PasswordChanged) []
        , input (inputAttrs "password" "confirmPassword" confirmPassword ConfirmPasswordChanged) []
        , button [disabled onButton] [text "Create account"]
        , p
         [
            style "color"
                <|
                if password == "" then "black" else
                if not matchy then
                    "red"
                else
                    "green"
            , style "font-size" "50pt"
         ]
        [text "blabla"]
        , p [ style "color" "black", style "font-size" "50pt", hidden enoughCh]
         [
          text <|
            if accountType == User then "parola trebuie sa contina cel putin 8 caractere"
            else "parola trebuie sa contina cel putin 12 caractere"
          ]
          
        ]




activateAccountView : Bool -> Html Msg
activateAccountView yes =
    div []
        [ input [ type_ "checkbox", onCheck SetActivateAccount, checked yes ] []
        , text "Activate account?"
        ]



statusView : Model -> Html Msg
statusView model =
    div []
        [ p [] [ text "Account type: ", text <| accountTypeToString model.accountType ]
        , p [] [ text "Username: ", text model.username ]
        , p [] [ text "Password: ", text model.password ]
        , p [] [ text "Confirm password: ", text model.confirmPassword ]
        , p []
            [ text <|
                if model.activateAccount then
                    "Account will be created activated"

                else
                    "Account will be created suspended"
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ statusView model
        , accountTypeView
        , activateAccountView model.activateAccount
        , accountDetailsView model
        ]
