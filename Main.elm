import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Random exposing (Seed)
import String exposing (toInt)

main : Program Never
main =
  let
    (randomNumber, seed) = nextRandomNumber (Random.initialSeed 1)
  in
    Html.App.beginnerProgram
      { model = { seed = seed, targetNumber = randomNumber, guess = Nothing }
      , view = view
      , update = update
      }

-- Model
type alias Model =
  { seed : Seed
  , targetNumber : Int
  , guess : Maybe Int
  }

-- Update
type Msg = Guess String | Reset

update : Msg -> Model -> Model
update msg model =
  case msg of
    Guess input ->
      case String.toInt input of
        Ok newGuess ->
          { model | guess = Just newGuess }
        _ ->
          model

    Reset ->
      let
        (newTargetNumber, newSeed) = nextRandomNumber model.seed
      in
        { seed = newSeed, targetNumber = newTargetNumber, guess = Nothing }

-- View
view : Model -> Html.Html Msg
view model =
  let
    (messageText, inputText) = case model.guess of
      Just newGuess ->
        (messageFromGuessAndTarget newGuess model.targetNumber, toString newGuess)
      Nothing ->
        ("Make a guess!", "")
  in
    div []
      [ div [] [ text messageText ]
      , div [] [ input [ placeholder "Enter number", value inputText, onInput Guess ] [] ]
      , div [] [ button [ onClick Reset ] [ text "Reset" ] ]
      ]

-- Helpers
messageFromGuessAndTarget : Int -> Int -> String
messageFromGuessAndTarget guess target =
  if guess == target then
    "You won! " ++ toString guess
  else if guess < target then
    "You guessed " ++ toString guess ++ ", but it was too low."
  else
    "You guessed " ++ toString guess ++ ", but it was too high."

nextRandomNumber : Seed -> (Int, Seed)
nextRandomNumber seed = Random.step (Random.int 0 100) seed
