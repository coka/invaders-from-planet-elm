import Color
import Graphics.Element
import Graphics.Collage
import Keyboard
import Time


-- MODEL

type alias Game =
  { player : Ship
  , alien : Ship
  , projectiles : List Projectile
  }


initialGame : Game
initialGame =
  { player = initialPlayer
  , alien = initialAlien
  , projectiles = []
  }


type alias Moveable a =
  { a |
      position : Vec2f,
      velocity: Vec2f
  }


type alias Ship =
  Moveable { isFiring : Bool }


initialPlayer : Ship
initialPlayer =
  { position = vec2f 0.0 -192.0
  , velocity = vec2f 0.0 0.0
  , isFiring = False
  }


initialAlien : Ship
initialAlien =
  { position = vec2f 0.0 192.0
  , velocity = vec2f 0.0 0.0
  , isFiring = False
  }


type alias Projectile =
  Moveable {}


fireProjectile : Float -> Projectile
fireProjectile origin =
  { position = vec2f origin -192.0
  , velocity = vec2f 0.0 12.0
  }


projectileOnScreen : Projectile -> Bool
projectileOnScreen projectile =
  projectile.position.y < 248.0


type alias Vec2f =
  { x : Float
  , y : Float
  }


vec2f : Float -> Float -> Vec2f
vec2f x' y' =
  { x = x'
  , y = y'
  }


add : Vec2f -> Vec2f -> Vec2f
add v1 v2 =
  { x = v1.x + v2.x
  , y = v1.y + v2.y
  }


toTuple : Vec2f -> (Float, Float)
toTuple vec =
  (vec.x, vec.y)


-- UPDATE

updateGame : Input -> Game -> Game
updateGame input game =
  let
    player' = updatePlayer input game.player
    projectiles' = updateProjectiles game.player game.projectiles
  in
    { game |
        player <- player',
        projectiles <- projectiles'
    }


updatePlayer : Input -> Ship -> Ship
updatePlayer input player =
  let
    position' = add player.position player.velocity
    velocity' = vec2f (4.0 * (toFloat input.x)) 0.0
    isFiring' = (input.y < 0)
  in
    { player |
        position <- position',
        velocity <- velocity',
        isFiring <- isFiring'
    }


updateProjectiles : Ship -> List Projectile -> List Projectile
updateProjectiles player projectiles =
  if player.isFiring && (List.isEmpty projectiles) then
    [ fireProjectile player.position.x ]
  else
    List.map updateProjectile projectiles
      |> List.filter projectileOnScreen


updateProjectile : Projectile -> Projectile
updateProjectile projectile =
  let
    position' = add projectile.position projectile.velocity
  in
    { projectile | position <- position' }


-- VIEW

render : Game -> Graphics.Element.Element
render game =
  Graphics.Collage.collage 640 480
    [ Graphics.Collage.filled Color.black (Graphics.Collage.rect 640.0 480.0)
    , Graphics.Collage.filled Color.white (Graphics.Collage.rect 32.0 32.0)
        |> Graphics.Collage.move (toTuple game.player.position)
    , Graphics.Collage.filled Color.green (Graphics.Collage.rect 32.0 32.0)
        |> Graphics.Collage.move (toTuple game.alien.position)
    , Graphics.Collage.group (List.map drawProjectile game.projectiles)
    ]


drawProjectile : Projectile -> Graphics.Collage.Form
drawProjectile projectile =
  Graphics.Collage.filled Color.white (Graphics.Collage.rect 4.0 16.0)
    |> Graphics.Collage.move (toTuple projectile.position)


-- SIGNALS

main : Signal Graphics.Element.Element
main =
  Signal.map render gameState


gameState : Signal Game
gameState =
  Signal.foldp updateGame initialGame input


type alias Input =
  { x : Int
  , y : Int
  }


input : Signal Input
input =
  Keyboard.arrows
    |> Signal.sampleOn (Time.fps 60)
