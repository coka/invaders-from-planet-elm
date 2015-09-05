import Color
import Graphics.Element
import Graphics.Collage
import Keyboard
import Time


-- MODEL

type alias Game =
  { player : Player }


initialGame : Game
initialGame =
  { player = initialPlayer }


type alias Player =
  { position : Float }


initialPlayer : Player
initialPlayer =
  { position = 0.0 }


-- UPDATE

updateGame : Int -> Game -> Game
updateGame direction game =
  let
    player' = updatePlayer direction game.player
  in
    { game | player <- player' }


updatePlayer : Int -> Player -> Player
updatePlayer direction player =
  let
    position' = player.position + 4.0 * (toFloat direction)
  in
    { player | position <- position' }


-- VIEW

render : Game -> Graphics.Element.Element
render game =
  let
    playerPosition = (game.player.position, 0.0)
  in
    Graphics.Collage.collage 640 480
      [ Graphics.Collage.filled Color.black (Graphics.Collage.rect 640.0 480.0)
      , Graphics.Collage.filled Color.white (Graphics.Collage.rect 32.0 32.0)
          |> Graphics.Collage.move playerPosition
      ]


-- SIGNALS

main : Signal Graphics.Element.Element
main =
  Signal.map render gameState


gameState : Signal Game
gameState =
  Signal.foldp updateGame initialGame direction


direction : Signal Int
direction =
  Signal.map .x Keyboard.arrows
    |> Signal.sampleOn (Time.fps 60)
