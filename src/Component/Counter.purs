module Component.Counter where

import Prelude

import CSS.Display (display, flex)
import CSS.Flexbox (AlignContentValue(..), JustifyContentValue(..), flexDirection, justifyContent, row)
import CSS.Geometry (width)
import CSS.Property (Value(..))
import CSS.Size (rem)
import CSS.String (fromString)
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE

type Input = Int
type Output = Int
type State = { count :: Int }

type Query :: forall k. k -> Type
type Query = Const Void

type Slots :: forall k. Row k
type Slots = ()

data Action
  = Initialize
  | Finalize
  | Decrement
  | Increment

class SpaceEvenly a where
  spaceEvenly :: a

instance spaceEvenlyValue :: SpaceEvenly Value where
  spaceEvenly = fromString "space-evenly"

instance spaceEvenlyAlignContentValue :: SpaceEvenly AlignContentValue where
  spaceEvenly = fromString "space-evenly"

instance spaceEvenlyJustifyContentValue :: SpaceEvenly JustifyContentValue where
  spaceEvenly = fromString "space-evenly"

component :: forall m
          . MonadAff m
          => H.Component Query Input Output m
component = H.mkComponent
  { initialState: \i -> { count: i }
  , render
  , eval: H.mkEval H.defaultEval {
      initialize = Just Initialize
    , finalize = Just Finalize
    , handleAction = handleAction
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
      Initialize -> log "Initialized"
      Finalize -> log "Finalized"
      Decrement -> H.modify_ \s -> s { count = s.count - 1}
      Increment -> H.modify_ \s -> s { count = s.count + 1}

    render :: State -> H.ComponentHTML Action Slots m
    render { count } = let onClick = HE.onClick <<< const in
      HH.div []
      [
        HH.div
        [ HC.style do
             display flex
             flexDirection row
             justifyContent spaceEvenly
             width (rem 0.6)
        ]
        [
          HH.button [ onClick Decrement ] [ HH.text "-"]
        , HH.text $ show count
        , HH.button [ onClick Increment ] [ HH.text "+"]
        ]
      ]
