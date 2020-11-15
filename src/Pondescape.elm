
module Pondescape exposing (..)

import Browser
import Browser.Events exposing (onClick, onKeyPress)
import Html exposing (Html, div, text)
import Json.Decode as Decode
import String exposing (toInt)
import Url exposing (toString)
import String exposing (fromFloat,fromInt,toFloat)
import Api.Data exposing (TurnResult(..))
import Html exposing (nav)
import Http exposing (..)
import Json.Decode exposing (errorToString)

import PlaygroundAllExposed exposing (..)
import Api.Request.CircleEscapeController exposing (..)
import Api.Data exposing (..)
import Api
import Math exposing (..)

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model --

type alias Model = 
  { duckpos: (Float, Float)
  , duckplan: PolarCoordinate
  , catpos: Float
  , pondRadius: Float
  , sessionID: Int
  , state: ModelTurnResult
  , errorLog: List String
  }

type ModelTurnResult = WIN
                     | WINHYPOTHETICAL
                     | LOSS
                     | LOSSHYPOTHETICAL
                     | ONGOING



init : () -> (Model, Cmd Msg)
init _ = ({ duckpos = (0, 0)
          , duckplan = (0,tau/4)
          , catpos = tau * 3/4
          , pondRadius = 100
          , sessionID = -1
          , state = ONGOING
          , errorLog = []
          }
         , sendNewGameRequest)

sendNewGameRequest : Cmd Msg
sendNewGameRequest =
  let
    f : (Result Http.Error Pair -> Msg)
    f result = 
     case result of
       Result.Ok response -> 
         NewGameSession response.sessionID response.gamestate
       Err e_ -> 
         Error "sendNewGameRequest" <| errorToString e_    
         
    newGameRequest : Api.Request Api.Data.Pair
    newGameRequest = Api.Request.CircleEscapeController.startNewGameVanillaSession
    
  in
  Api.send f newGameRequest

--https://stackoverflow.com/a/56445634
errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus _ ->
            "Unknown error"
        BadBody errorMessage ->
            errorMessage


-- UPDATE

type Msg
  = CharacterKey Char
  | ControlKey String
  | UP 
  | DOWN 
  | RIGHT
  | LEFT
  | NewGameSession Int GameState
  | PlanResponse TurnResponse
  | StepResponse TurnResponse
  | Error String String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  let  
      (r, theta) = model.duckplan
      epsilon = 0.2
      thetaStep = if r > 1 then epsilon / r else epsilon
      angleStepRight = -thetaStep
      angleStepLeft = thetaStep

      moveTheta step = fModBy tau <| theta + step

      navigate : Model -> ( Model, Cmd Msg )
      navigate updatedModel = (model, sendPlanRequest updatedModel)
  in
    case msg of
        ControlKey "Enter" ->
            ( model, sendStepRequest model )

        PlanResponse step ->            
            ( {model | duckplan =  subtractPolar (polarCoordinatesToTuple step.duck) model.duckpos
                     , catpos  = step.cat.theta
                     , state = case step.state of
                        TurnResultWIN   -> WINHYPOTHETICAL
                        TurnResultLOOSE -> LOSSHYPOTHETICAL
                        _               -> ONGOING
                     }, Cmd.none )
        StepResponse step ->            
            ( {model | duckpos = polarCoordinatesToTuple step.duck
                     , duckplan = (r * 0.8, theta)
                     , catpos  = step.cat.theta
                     , state = case step.state of
                        TurnResultWIN   -> WIN
                        TurnResultLOOSE -> LOSS
                        _               -> ONGOING
                     }, Cmd.none )

        UP ->
            navigate { model | duckplan = (r + 1, theta)}
        DOWN ->
            navigate { model | duckplan = (clipAt0 <| r - 1, theta)}
        LEFT ->
            navigate { model | duckplan = (r, moveTheta angleStepLeft)}
        RIGHT ->
            navigate { model | duckplan = (r, moveTheta angleStepRight)}
        CharacterKey 'x' ->
            let
              model2 = {model | duckplan = Tuple.mapFirst ( \ _ -> 0) model.duckplan }
            in
            ( model2, Cmd.none )
        NewGameSession session gamestate ->
            ( {model | sessionID = session
                     , duckpos = (gamestate.duck.r, gamestate.duck.theta)
                     , duckplan = (0, tau/4)
                     , catpos = gamestate.cat.theta
                     , pondRadius = gamestate.radius
                     }
              , Cmd.none )
        Error source errmsg ->  ( { model | errorLog = model.errorLog ++ [source ++ "," ++ errmsg]}, Cmd.none )
        _ ->         
            ( model, Cmd.none )


sendPlanRequest : Model -> Cmd Msg
sendPlanRequest model = 
  let
    f : (Result Http.Error TurnResponse -> Msg)
    f result = 
     case result of
       Result.Ok response -> 
         PlanResponse response
       Err e_ -> 
         Error "sendPlanRequest" <| errorToString e_
    body : PairGenericPolarCoordinates
    body = ({ sessionID = model.sessionID
            , t = polarCoordinatesFromTuple model.duckplan
            })
  in
  Api.send f <| Api.Request.CircleEscapeController.planStepPolarSession body


sendStepRequest : Model -> Cmd Msg
sendStepRequest model = 
  let
    f : (Result Http.Error TurnResponse -> Msg)
    f result = 
     case result of
       Result.Ok response -> 
         StepResponse response
       Err e_ -> 
         Error "sendStepRequest" <| errorToString e_
         
    body : PairGenericPolarCoordinates
    body = ({ sessionID = model.sessionID
            , t = polarCoordinatesFromTuple model.duckplan
            })
  in
  Api.send f <| Api.Request.CircleEscapeController.makeStepPolarSession body

polarCoordinatesFromTuple : (Float, Float) -> PolarCoordinates
polarCoordinatesFromTuple (r,theta) = ({r=r, theta=theta})
polarCoordinatesToTuple : PolarCoordinates -> (Float, Float)
polarCoordinatesToTuple pc = (pc.r, pc.theta)

clipAt0 : Float -> Float
clipAt0 n = Basics.max n 0



keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( 'w', "" ) ->
            UP
        Just ( 'a', "" ) ->
            LEFT
        Just ( 's', "" ) ->
            DOWN
        Just ( 'd', "" ) ->
            RIGHT
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue


subscriptions : Model -> Sub Msg
subscriptions model =
    onKeyPress keyDecoder

-- VIEW
view : Model -> Html Msg
view model =
    let
        renderPlayground : Model -> Html Msg
        renderPlayground m = 
          let
            (x,y) = fromPolar m.duckpos
            (dx,dy) = m.duckplan
            (px,py) = add (x,y) <| fromPolar (dx,dy) 
            r = 100
            (cx, cy) = fromPolar (r, m.catpos)
           in
           render (toScreen 200 300)
                  [ circle blue r --pond
                  , circle green 4  |> move x y   -- duck
                  , circle orange 2 |> move px py -- duckplan
                  , polygon orange [(0,0),(px,py),(-1,-1)]
                  , circle black 2  |> move cx cy -- cat
                  , polygon black [(0,0),(cx,cy),(-1,-1)]
                  ]
        state: String
        state = case model.state of
                    WIN -> 
                        "Win! The duck has successfully escaped!"
                    WINHYPOTHETICAL -> 
                        "Hypothetical win! The duck would successfully escape!"
                    LOSS -> 
                        "Game over! The cat caught the duck."
                    LOSSHYPOTHETICAL -> 
                        "Hypothetical loss! The cat would catch the duck."
                    _ ->
                        "ongoing..."
        combineError: List String -> Html Msg
        combineError list = div [] (List.map (\s -> div [] [text s]) list )
                  
    in
    div [] [ div [] [ text "navigate with wasd, confirm with enter"]
           , div [] [ text state ]
           , div [] [ text <| fromPC model.duckpos]
           , div [] [ text <| fromPC model.duckplan]
           , renderPlayground model 
           , combineError model.errorLog
           ]


fromPC : (Float, Float) -> String
fromPC (r,t) = "(" ++ String.fromFloat r ++ "," ++ String.fromFloat t ++ ")"