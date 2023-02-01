module Main exposing (main)

{-| TODO

    - [X] Implement a visually pleasing design without sacrificing usability
    - [X] Göra en ny startsida med Lots of features texten
            - [X] skriva en ny funktion view_Landing
                    - [X] innehålla lots of fetures text
                    - [X] skapa en Create account knapp
            - [X] skriva en ny type View LandingPage
            - [X] lägga till LandingPage i view model
            - [X] byta initModel till LandingPage
            - [X] knappen ska leda till FillForm sidan
                    - [X] lägga till LandingPageClicked type Msg
                    - [X] lägga till LandingPageClicked i update case
                            - [X] { model | view = FillForm }
                    - [X] onClick LandingPageClicked på knappen

    - [ ] Make it responsive and useable on all screen sizes
    - [X] Implement the hero in a resposive fashion
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
                            - [X] vart jag ska skriva if else
                            - [X] condition password === confirmpassword
                            - [X] False = create account knappen ska inte funka
                            - [X] True = create account knappen ska funka
            - [X] en text som säger att lösenorden är olika
                    - [X] False = text visas "confirm password does not match password."
    - [ ] Make a show/hide button to display password in cleartext at will.
            - [ ] göra en knapp som visar både password och confirmpassword
    - [ ] Form should not submit unless all fields are valid.
    - [ ] Form should not submit unless terms and condition checkbox is "checked"

    - [X] Add feedback that the app is loading when submit is clicked.
            - [X] Se hur de andra sidorna som visas i olika stadier är uppbyggda. 
            - [X] Skriva fler punkter efter research. 
            - [X] Skapa en view_Loading funktion
            - [X] Skriva in meddelande som ska komma upp medans det laddar "Page is loading"
            - [X] Ändra view model Sending form till view_Loading
            
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

-- De olika funktionaliteter som websidan har
type Msg
    = FieldGotInput Field
    | LandingPageClicked
    | FormSubmitClicked
    | GotBackendResponse

-- state
type alias Model =
    { form : Form
    , view : View
    , showPassword : Bool
    }

type alias Form =
    { firstname : String
    , lastname : String
    , email : String
    , password : String
    , confirmPassword : String
    }

-- Olika sidor vi kan vara inne på i View
type View
    = LandingPage
    | FillForm
    | SendingForm
    | SubmitSuccess
    


type alias Flags =
    ()

type Field
    = Firstname String
    | Lastname String
    | Email String
    | Password String
    | ConfirmPassword String

-- {} = record
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

init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel
    , Cmd.none
    )

-- initial model, standard när man går in på sidan. Start sida
initModel : Model
initModel =
    { form = form_Empty
    , view = LandingPage
    , showPassword = False
    }

-- Subscriptions allow us to listen to external events 
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LandingPageClicked ->
            ( { model | view = FillForm }
            , Cmd.none
            )

        FieldGotInput field ->
            ( { model | form = form_Update field model.form }
            , Cmd.none
            )

        FormSubmitClicked ->
            ( { model | view = SendingForm }
            , Process.sleep 2000
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
        , view_Hero -- header (visas hela tiden)
        , case model.view of
            LandingPage ->
                view_Landing 
            
            FillForm ->
                view_Form model.form

            SendingForm ->
                view_Loading

            SubmitSuccess ->
                view_Success
        ]


view_Hero : Html msg
view_Hero =
    Html.div
        [ HA.class "hero" ]
        [ Html.div 
            [HA.class "hero-latin-text-div"]
            [Html.p [HA.class "hero-latin-text"] [ Html.text "Dolor eveniet mollitia omnis sequi obcaecati. Nobis sit nam iure sit earum. Dolorem natus dolore perspiciatis accusamus numquam maiores lorem!" ]] 
        , Html.div 
            [ HA.class "curve-div" ] []
        ]
        
        

view_Landing : Html Msg
view_Landing =
    Html.div
        [ HA.class "unique-selling-points" ]
        [ Html.h2 [HA.class "features-title"] [ Html.text "Lots of features" ]
        , Html.p [HA.class "features-text"] [ Html.text "Get access to our full set of features by registering, including but not limited to:" ]
        , Html.ul
            [HA.class "features-list"]
            [Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675181976/book_icon_ytsgiw.png"
                , HA.alt "Book icon"] []
                , Html.li 
                    [] 
                    [Html.text "Lorem ipsum"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/pen_icon_klms5f.png"
                , HA.alt "Pen icon"] []
                , Html.li 
                    [] 
                    [Html.text "Dolor eveniet"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/weight_icon_uedlgf.png"
                , HA.alt "Weight icon"] []
                , Html.li 
                    [] 
                    [Html.text "Mollitia omnis sequi obcaecati"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/mail_icon_snzgls.png"
                , HA.alt "Mail icon"] []
                , Html.li 
                    [] 
                    [Html.text "Nobis"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/house_icon_sgymgv.png"
                , HA.alt "House icon"] []
                , Html.li 
                    [] 
                    [Html.text "Nam iure sit earum"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/globe_icon_z4shkh.png"
                , HA.alt "Globe icon"] []
                , Html.li 
                    [] 
                    [Html.text "Perspiciatis accusamus numquam"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/people_icon_nerfvf.png"
                , HA.alt "People icon"] []
                , Html.li 
                    [] 
                    [Html.text "Obcaecati"]
                ]
            , Html.div 
                [HA.class "latin-list-element"] 
                [ Html.img [HA.src "https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675186426/wrench_icon_qymcfu.png"
                , HA.alt "Wrench icon"] []
                , Html.li 
                    [] 
                    [Html.text "Dolor omnis"]
                ]
            ]
        , Html.button
            [ Events.onClick LandingPageClicked ]
            [ Html.text "Create Account" ]
        ]
view_Form : Form -> Html Msg
view_Form form =

    Html.div
        [HA.class "form-center"]
        [Html.div
            [ HA.class "form" ]
            [Html.h1 [HA.class "create-account-text"] [ Html.text "Create account!" ]
        ,  Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Firstname" ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.firstname
                        , HA.class "input-feild"
                        , Events.onInput (FieldGotInput << Firstname)
                        ]
                        []
                    ]
                ]
            , Html.div
                []
                [ Html.div
                    [HA.class "label"]
                    [ Html.text "Lastname" ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.lastname , 
                        HA.class "input-feild"
                        , Events.onInput (FieldGotInput << Lastname) 
                        ]
                        []
                    ]
                ]
            , Html.div
                []
                [ Html.div
                    []
                    [ Html.text "Email" ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.email
                        , HA.class "input-feild"
                        , Events.onInput (FieldGotInput << Email)
                        ]
                        []
                    , case getEmailError form.email of
                        Just error ->
                            Html.div
                                [ HA.class "error-text" ]
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
                    [ Html.text "Password" ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.password
                        , HA.class "input-feild"
                        , Events.onInput (FieldGotInput << Password)
                        -- Not implimented: Show and hide password
                        {- , if True then
                            HA.type_ "password"
                        else
                            HA.type_ "text" -}
                        ]
                        []
                        -- Showpassword checkbox och text

                        {- if type=password -> type=text
                            Html.input
                                [ HA.value form.confirmPassword
                                , HA.type_ "checkbox"
                                , HA.class "checkboxPasswords"
                                , Events.onClick ToggleShowPassword
                                ]
                                []
                            , Html.text "Show passwords" -}

                       , case getPasswordError form.password of
                            Just error ->
                                Html.div
                                    [HA.class "error-text"]
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
                    [ Html.text "Confirm Password" ]
                , Html.div
                    []
                    [ Html.input
                        [ HA.value form.confirmPassword
                        , HA.class "input-feild"
                        , Events.onInput (FieldGotInput << ConfirmPassword)
                        , HA.type_ "password"
                        ]
                        []
                    ]
                , case getConfirmPasswordError form.password form.confirmPassword of
                    Just error ->
                        Html.div
                            [ HA.class "error-text" ]
                            [ Html.text error ]

                    Nothing ->
                        Html.text ""
                ]
            , Html.div
                [ HA.class "checkbox-div" ]
                [ Html.input
                    [ HA.value form.confirmPassword
                    , HA.type_ "checkbox"
                    , HA.class "checkbox-terms"
                    ]
                    []
                , Html.text "I agree to terms and conditions"
                ]
            , Html.div
                [ HA.class "create-button" ]
                [ Html.button
                    [ 
                    if (form.password == form.confirmPassword) then
                        Events.onClick FormSubmitClicked 
                    else 
                        HA.class "" -- TODO: find a fix
                    ]
                    [ Html.text "Create Account" ]
                ]
            ]
        ]

view_Loading : Html msg
view_Loading =
    Html.div 
    [HA.class "page-loading"]
    [ Html.div 
        [HA.class "loading-icon"]
        []
    ]


view_Success : Html msg
view_Success =
    Html.div 
    [HA.class "thanks-page"]
    [ Html.div 
        [HA.class "registration-thanks"]
        [ Html.text "Thank you for registering!" 
        , Html.img 
            [HA.src "https://static.vecteezy.com/system/resources/previews/002/743/514/original/green-check-mark-icon-in-a-circle-free-vector.jpg" 
            , HA.alt "Green checkmark"
            , HA.class "checkmark"]
            []
        ]
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
        Just "Password must be at least 6 characters"
        
    else if not (Regex.contains (checkRegex "[A-Z ]+") password) then
        Just "Password must contain at least one capital letter"
        
    else if not (Regex.contains (checkRegex "[\\d]") password) then
        Just "Password must contain at least one number"
        
    else if not (Regex.contains (checkRegex "[\\W|_]") password) then
        Just "Password must contain at least one special character"

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

-- [X] Body ska vara ut till kanterna
-- header
    -- create account stor text i hörnet
    -- latin texten på sidan
-- Startsidan
    -- texten ska vara i center
    -- roligare punkter (googla)
-- FORM 
    -- Class på alla textinputs
    -- knapp i mitten
    -- texten under vid ogiltigt lösenord ska vara svart 
    -- texten under ogiltigt lösenord ska vara mindre
    -- checkbox lösenord ska vara på sidan 
-- Loading page
    -- texten större 
    -- texten i mitten av skärmen
    -- bild på loading
-- thank you page
    -- texten större
    -- texten i mitten av skärmen
    -- icon ?
    -- div med box shadow
css : String
css =
    """
* {
    padding:0;  
}

/* Header */

.hero {
    background-image: url("https://res.cloudinary.com/dvo8kf1x1/image/upload/v1675239318/elm3_wyuksh.png");
    height: 400px;
    background-position: 12%;
    width: 100%;
    position: relative;
}

.curve-div {
    background-color: white;
    height: 100px;
    border-radius: 75%;
    position: absolute;
    bottom: -12%;
    width: 100%;
}

.create-account-text {
    text-align: center;
    color: black;
    margin-top: 0;
}

.hero-latin-text-div {
    width: 65%;
    height: 400px;
    position: absolute;
    right: 0;
    overflow: hidden;
}

.hero-latin-text {
    font-style: oblique;
    font-size: 42px;
    padding: 0 0 0 32px;
}


/* List landing page  */

.unique-selling-points {
    text-align: center;
    font-family: palatino;
}

.features-title {
    font-size: 110px;
    margin-bottom: 0;
    
}

.features-text {
    font-size: 28px;
    margin-top: 8px;
    padding: 0 100px;
}

.features-list {
    display: flex;
    flex-direction: row;
    flex-wrap: wrap;
    justify-content: center;
    list-style: none; 
    font-size: 40px;
}

.latin-list-element {
    width: 40%;
}

ul li {
    margin: 20px;
}

/* Form */

.form-center {
    display: flex;
    justify-content: center;
}

.form {
    margin-top: 60px;
    color: rgba(111, 143, 154, 1);
    font-size: 45px;
    width: 80%;
}


.input-feild {
    border: none;
    outline: none;
    border-bottom: 2px solid rgba(111, 143, 154, 1);
    margin-bottom: 40px;
    width: 100%; 
    font-size: 40px;
}

/* NOT FINISHED: Show password button*/

/* .checkbox-password-div {
    position: absolute;
    right: 0;
    text-align: right;
    font-size: 35px;
    color: black;
} */

.error-text {
    color: black;
    font-size: 30px;
    margin-bottom: 16px;
}

.checkbox-div {
    text-align: center;
}

.checkbox-terms {
    width: 30px;
    height: 30px;
    margin-right: 20px;
}

.create-button {
    text-align: center;
    margin-top: 15%;
}
button {
    font-size: 60px;
    font-weight: bold;
    flex: 1 1 auto;
    padding: 30px;
    text-align: center;
    text-transform: uppercase;
    transition: 0.5s;
    background-size: 200% auto;
    color: white;
    text-shadow: 0px 0px 10px rgba(0,0,0,0.2);
    box-shadow: 0 0 20px #eee;
    border: 4px solid rgba(111, 143, 154, 1);
    border-radius: 20px;
    background-image: linear-gradient(to right, rgba(111, 143, 154, 0.20) 0%, rgba(111, 143, 154, 0.60) 51%, rgba(111, 143, 154, 1) 100%);
    margin-bottom: 50px;
}

/* Loading page */

.page-loading {
    display: flex;
    justify-content: center;
}

.loading-icon {
    border: 16px solid #f3f3f3; 
    border-top: 16px solid #3498db; 
    border-radius: 50%;
    width: 120px;
    height: 120px;
    animation: spin 2s linear infinite;
}

@keyframes spin {
  0% { transform: rotate(0deg); }
  100% { transform: rotate(360deg); }
}

/* Thanks for registration */

.thanks-page {
    width: 100%;
    position: absolute;
    display: flex;
    justify-content: center;
}
.registration-thanks {
    margin: 50px;
    font-size: 70px;
    font-weight: bold;
    text-align: center;

}

.checkmark {
    width: 45%;
}
 

"""
