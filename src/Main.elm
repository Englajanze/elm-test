module Main exposing (main)

{-| TODO

    - [ ] Implement a visually pleasing design without sacrificing usability
    - [ ] Make it responsive and useable on all screen sizes
    - [ ] Implement the hero in a resposive fashion
            - Feel free to adjust the HTML DOM as needed
            - The image should always take the full width of the screen (or a container)
            - The image should always be 400px in height
            - The person in the photo should always be visible regardless of screen width
            - The title and text should be vertically and horizontally aligned center and never overflow the container
    - [ ] Fix Accessability
    - [X] Validate that password contains at least 1 capital, 1 digit and 1 special character.
    - [X] confirmPassword and password fields must match. If not, form should not be submittable.
            - [X] checka om password och confirmpassword är lika
                    - [X] läsa om hur man sammanlingnar saker i elm (==)
            - [X] knappen ska inte funka om dom är olika
                    - [X] se vart knappen är och hur den fungerar
                    - [X] if else 
                            - [X] vart vi ska skriva if else
                            - [X] condition password === confirmpassword
                            - [X] False = create account knappen ska inte funka
                            - [X] True = create account knappen ska funka
            - [X] en text som säger att lösenorden är olika
                    - [X] False = text visas "confirm password does not match password."
    - [ ] Make a show/hide button to display password in cleartext at will.
            - [ ] göra en knapp som visar både password och confirmpassword
            - [ ] 
    - [ ] Form should not submit unless all fields are valid.
    - [ ] Form should not submit unless terms and condition checkbox is "checked"
    - [ ] Add feedback that the app is loading when submit is clicked.
    - [ ] Refactor code to make it more maintainable and easier to understand.

-}

import Browser
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events as Events
import Process
import Task
import Regex


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = FieldGotInput Field
    | FormSubmitClicked
    | GotBackendResponse


type alias Model =
    { form : Form
    , view : View
    }


type View
    = FillForm
    | SendingForm
    | SubmitSuccess


type alias Flags =
    ()


type alias Form =
    { firstname : String
    , lastname : String
    , email : String
    , password : String
    , confirmPassword : String
    }


type Field
    = Firstname String
    | Lastname String
    | Email String
    | Password String
    | ConfirmPassword String


form_Empty : Form
form_Empty =
    { firstname = ""
    , lastname = ""
    , email = ""
    , password = ""
    , confirmPassword = ""
    }


form_Update : Field -> Form -> Form
form_Update field form =
    case field of
        Firstname str ->
            { form | firstname = str }

        Lastname str ->
            { form | lastname = str }

        Email str ->
            { form | email = str }

        Password str ->
            { form | password = str }

        ConfirmPassword str ->
            { form | confirmPassword = str }


initModel : Model
initModel =
    { form = form_Empty
    , view = FillForm
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FieldGotInput field ->
            ( { model | form = form_Update field model.form }
            , Cmd.none
            )

        FormSubmitClicked ->
            ( { model | view = SendingForm }
            , Process.sleep 1000
                |> Task.andThen (\_ -> Task.succeed GotBackendResponse)
                |> Task.perform identity
            )

        GotBackendResponse ->
            ( { model | view = SubmitSuccess }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    Html.div
        []
        [ Html.node "style" [] [ Html.text css ]
        , view_Hero
        , case model.view of
            FillForm ->
                view_Form model.form

            SendingForm ->
                view_Form model.form

            SubmitSuccess ->
                view_Success model
        ]


view_Hero : Html msg
view_Hero =
    Html.div
        [ HA.class "hero"
        ]
        [ Html.div
            [ HA.class "hero-text"
            ]
            [ Html.h1 [] [ Html.text "Create account!" ]
            , Html.p [] [ Html.text "Dolor eveniet mollitia omnis sequi obcaecati. Nobis sit nam iure sit earum. Dolorem natus dolore perspiciatis accusamus numquam maiores lorem!" ]
            ]
        ]


view_Form : Form -> Html Msg
view_Form form =
    Html.div
        []
        [ Html.div
            [ HA.class "form"
            ]
            [ Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Firstname"
                    ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.firstname
                        , Events.onInput (FieldGotInput << Firstname)
                        ]
                        []
                    ]
                ]
            , Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Lastname"
                    ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.lastname
                        , Events.onInput (FieldGotInput << Lastname)
                        ]
                        []
                    ]
                ]
            , Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Email"
                    ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.email
                        , Events.onInput (FieldGotInput << Email)
                        ]
                        []
                    , case getEmailError form.email of
                        Just error ->
                            Html.div
                                []
                                [ Html.text error
                                ]

                        Nothing ->
                            Html.text ""
                    ]
                ]
            , Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Password"
                    ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.password
                        , Events.onInput (FieldGotInput << Password)
                        , HA.type_ "password"
                        ]
                        []
                    ]
                , Html.input
                    [ HA.value form.confirmPassword
                    , HA.type_ "checkbox"
                    , HA.class "checkboxPasswords"
                    ]
                    []
                , Html.text "Show passwords"
                , case getPasswordError form.password of
                    Just error ->
                        Html.div
                            []
                            [ Html.text error
                            ]

                    Nothing ->
                        Html.text ""
                ]
            , Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Confirm Password"
                    ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.confirmPassword
                        , Events.onInput (FieldGotInput << ConfirmPassword)
                        , HA.type_ "password"
                        ]
                        []
                    ]
                , case getConfirmPasswordError form.password form.confirmPassword of
                    Just error ->
                        Html.div
                            []
                            [ Html.text error
                            ]

                    Nothing ->
                        Html.text ""
                ]
            , Html.div
                [ HA.class "checkbox-div"
                ]
                [ Html.input
                    [ HA.value form.confirmPassword
                    , HA.type_ "checkbox"
                    , HA.class "checkbox"
                    ]
                    []
                , Html.text "I agree to terms and conditions"
                ]
            , Html.div
                []
                [ Html.button
                    [ 
                    if (form.password == form.confirmPassword) then
                        Events.onClick FormSubmitClicked 
                    else 
                        HA.class "hej"
                        
                    ]
                    [ Html.text "Create Account"
                    ]
                ]
            ]
        , Html.div
            [ HA.class "unique-selling-points"
            ]
            [ Html.h2 [] [ Html.text "Lots of features" ]
            , Html.p [] [ Html.text "Get access to our full set of features by registering, including but not limited to:" ]
            , Html.ul
                []
                [ Html.li [] [ Html.text "Lorem ipsum" ]
                , Html.li [] [ Html.text "Dolor eveniet" ]
                , Html.li [] [ Html.text "Mollitia omnis sequi obcaecati" ]
                , Html.li [] [ Html.text "Nobis" ]
                , Html.li [] [ Html.text "Nam iure sit earum" ]
                , Html.li [] [ Html.text "Perspiciatis accusamus numquam" ]
                , Html.li [] [ Html.text "Obcaecati" ]
                , Html.li [] [ Html.text "Dolor omnis" ]
                ]
            ]
        ]


view_Success : Model -> Html msg
view_Success model =
    Html.div
        [ HA.class "registering-thanks"
        ]
        [ Html.text "Thank you for registering!"
        ]


getEmailError : String -> Maybe String
getEmailError email =
    if email == "" then
        Nothing

    else if not <| String.contains "@" email then
        Just "Invalid email"

    else if String.endsWith "example.com" email then
        Just "Email already registered"

    else
        Nothing

checkRegex : String -> Regex.Regex
checkRegex regex =
    Maybe.withDefault Regex.never (Regex.fromString regex)

getPasswordError : String -> Maybe String
getPasswordError password =
    if password == "" then
        Nothing

    else if String.length password < 6 then
        Just "Password must be at least 6 chars"
        
    else if not (Regex.contains (checkRegex "[A-Z ]+") password) then
        Just "Password must contain at least one capital letter"
        
    else if not (Regex.contains (checkRegex "[\\d]") password) then
        Just "Password must contain at least one number"
        
    else if not (Regex.contains (checkRegex "[\\W|_]") password) then
        Just "Password must contain at least one special caracter"

    else
        Nothing

getConfirmPasswordError : String -> String -> Maybe String
getConfirmPasswordError password confirmPassword =
    if confirmPassword == "" then
        Nothing
    else if not (password == confirmPassword) then
    Just "Confirm password does not match password"
    else
        Nothing
 

-- bilden ska alltid vara 400px hög height: 400px/div som är 400px hög
-- bilden ska alltid visa mannen ?
    -- background-position background-size?
-- texten på bilden ska vara i höger hörn
-- form 
    -- bara understa raden ska synas
    -- texten ska vara till vänster
    -- båge inom bilden som är vit, görs med en div?
-- hemsidan ska först visa lots of features, och knapp
-- när knappen klickas ska form visas.
css : String
css =
    """
.hero {
    background-image: url("https://i.picsum.photos/id/665/1600/400.jpg?hmac=_bqybN6OSwmLs5ZvnuUz2GMWU5ZnLaQCAXc5CHsmGYs");
    height: 400px;
    background-position: 12%;
}

.hero-text {
 
}

.form {
margin-top: 40px;
color: rgba(111, 143, 154, 1);
}

input {
border: none;
outline: none;
border-bottom: 2px solid rgba(111, 143, 154, 1);
margin-bottom: 15px;
width: 80%;
}

.checkbox {

}


button {
background-color: rgba(111, 143, 154, 1);
border: none;
padding: 20px;
}


"""
