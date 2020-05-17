module Seija.Element (
  image,sprite,text,spriteB,sprite_,emptyElement,setParent,
  switchElement,listElement
) where

import Prelude

import Control.Monad.Reader (class MonadAsk, class MonadTrans, ask, runReaderT)
import Data.Array (index)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.MaybeEx (maybeToList)
import Data.Tuple.Nested ((/\))
import Data.Vec (vec2)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign.Object as O
import Partial.Unsafe (unsafePartial)
import Seija.App (class MonadApp, GameEnv, GameM(..), askWorld)
import Seija.Asset (getSpirteRectInfo, getTextureSizeWorld)
import Seija.Asset.Types (Font(..), SpriteSheet(..), Texture(..))
import Seija.Component as C
import Seija.FRP (Behavior(..), effectBehavior, unsafeBehaviorValue)
import Seija.Foreign as F
import Unsafe.Coerce (unsafeCoerce)


image::forall m.(MonadApp m) => Texture -> Array C.Prop -> Maybe F.Entity -> m F.Entity
image s2d@(Texture id) arr parent = do
    world <- askWorld
    liftEffect $ do
     e <- F.newEntity world
     _ <- F.addTransformByProp world e (C.buildProp arr C.Transform false)
     addMayParent world e parent
     let rectProp =  C.buildProp arr C.Rect2D false
     if (not $ O.member "size" rectProp)
       then do
        let vecSize = getTextureSizeWorld world s2d
        let newRectProp = O.insert "size" (C.propFromVector2f vecSize) rectProp
        F.addRect2DByProp world e newRectProp *> pure unit
      else  F.addRect2DByProp world e rectProp *> pure unit
     _ <- F.addImageRenderByProp world e id (C.buildProp arr C.ImageRender false)
     --set behavior
     C.setTransformBehaviorWorld world e arr
     C.setRect2dBehaviorWorld world e arr
     pure e

spriteB::forall m.(MonadApp m) => SpriteSheet -> Behavior String ->  Array C.Prop -> Maybe F.Entity -> m F.Entity
spriteB s2d b = sprite s2d (C.B b)

sprite_::forall m.(MonadApp m) => SpriteSheet -> String ->  Array C.Prop -> Maybe F.Entity -> m F.Entity
sprite_ s2d p = sprite s2d (C.P p)

sprite::forall m.(MonadApp m) => SpriteSheet -> C.POrB String -> Array C.Prop -> Maybe F.Entity -> m F.Entity
sprite s2d@(SpriteSheet id) spr arr parent = do
  let spriteName = C.valPOrB spr
  let spritePropArr = maybeToList $ C.propPOrB spr C.spriteNameB
  world <- askWorld
  liftEffect $ do
    e <- F.newEntity world
    _ <- F.addTransformByProp world e (C.buildProp arr C.Transform false)
    addMayParent world e parent
    let rectProp =  C.buildProp arr C.Rect2D false
    let isNoSize = not $ O.member "size" rectProp
    if (isNoSize && isSetDefault mayImageIntType) 
    then do
        let (_ /\ _ /\ w /\ h /\ unit) = getSpirteRectInfo world s2d spriteName
        let newRectProp = O.insert "size" (C.propFromVector2f $ vec2 w h) rectProp
        F.addRect2DByProp world e newRectProp *> pure unit
    else  F.addRect2DByProp world e rectProp *> pure unit
    F._addSpriteRenderByProp world e id spriteName spriteProp
    F._addTransparent world e
    --set behavior
    C.setTransformBehaviorWorld world e arr
    C.setRect2dBehaviorWorld world e arr
    C.setSpriteBehaviorWorld world e (arr <> spritePropArr)
    pure e
  where
    spriteProp = C.buildProp arr C.SpriteRender false
    mayImageIntType::Maybe Int
    mayImageIntType = do
      typeProp <- O.lookup "type" spriteProp
      (fistNumber::Number) <- index (unsafeCoerce typeProp) 0
      pure $ unsafePartial $ fromJust $ fromNumber fistNumber
    isSetDefault::Maybe Int -> Boolean
    isSetDefault Nothing = true
    isSetDefault (Just v) = C.isImageTypeDefSize v

text::forall m.(MonadApp m) =>  Font -> Array C.Prop -> Maybe F.Entity -> m F.Entity
text (Font id) arr parent = do
 world <- askWorld
 liftEffect $ do
    e <- F.newEntity world
    _ <- F.addTransformByProp world e (C.buildProp arr C.Transform false)
    let rectProp =  C.buildProp arr C.Rect2D false
    _ <- F.addRect2DByProp world e rectProp
    addMayParent world e parent
    F._addTransparent world e
    F._addTextRenderByProp world e id $ C.buildProp arr C.TextRender false
     --set behavior
    C.setTransformBehaviorWorld world e arr
    C.setRect2dBehaviorWorld world e arr
    C.setTextBehaviorWorld  world e arr
    pure e


addMayParent::F.World -> F.Entity -> Maybe F.Entity -> Effect Unit
addMayParent _ _ Nothing = pure unit
addMayParent world e (Just p) = F.setParent world e p

setParent::forall m.MonadApp m => F.Entity -> F.Entity -> m Unit
setParent e p = do
  world <- askWorld
  liftEffect $ F.setParent world e p

emptyElement::forall m.(MonadApp m) => Array C.Prop -> Maybe F.Entity -> m F.Entity
emptyElement props parent = do
   world <- askWorld
   liftEffect do
    e <- F.newEntity world
    _ <- F.addTransformByProp world e $ C.buildProp props C.Transform false
    _ <- F.addRect2DByProp world e $ C.buildProp props C.Rect2D false
    C.setTransformBehaviorWorld world e props
    C.setRect2dBehaviorWorld world e props
    addMayParent world e parent
    pure e


switchElement::forall r. F.Entity -> Behavior (GameM r Effect F.Entity) -> GameM r Effect Unit
switchElement parent bVal = do
  let createFn = unsafeBehaviorValue bVal
  eid <- createFn
  setParent eid parent
  env <- ask
  world <- askWorld
  
  effectBehavior bVal (\(GameM bReader) -> do
                        F.removeAllChildren world parent
                        eId <- runReaderT bReader env
                        F.setParent world eId parent
                        pure unit
                      )
  pure unit

listElement::forall a r. F.Entity -> Behavior (Array a) -> (a -> GameM r Effect F.Entity) ->  GameM r Effect Unit
listElement parent (Behavior bList) f = do
  (env::GameEnv r) <- ask
  world <- askWorld
  _ <- liftEffect $ F._listElement world parent bList (\val -> do
                                                    let (GameM reader) = f val
                                                    runReaderT reader env
                                                )
  pure unit