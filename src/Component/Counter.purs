module Component.Counter where

import Prelude

import CSS.Common (center)
import CSS.Display (display, flex)
import CSS.Flexbox (AlignContentValue, JustifyContentValue, flexDirection, justifyContent, row)
import CSS.Geometry (paddingBottom, paddingLeft, paddingTop, width)
import CSS.Property (Value)
import CSS.Size (rem)
import CSS.String (fromString)
import Data.Array (range)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

type Input = Int
type Output = Int
type State = { count :: Int }

data Query a
  = SetCount Int a
  | GetCount (Int -> a)

type Slots = ( counter :: H.Slot Query Output Int )

type SlotNum = Int
type CounterValue = Int

data Action
  = Initialize
  | Finalize
  | Decrement
  | Increment
  | FromChild SlotNum CounterValue
  | RaiseParent
  | Received Int

class SpaceEvenly a where
  spaceEvenly :: a

instance spaceEvenlyValue :: SpaceEvenly Value where
  spaceEvenly = fromString "space-evenly"

instance spaceEvenlyAlignContentValue :: SpaceEvenly AlignContentValue where
  spaceEvenly = fromString "space-evenly"

instance spaceEvenlyJustifyContentValue :: SpaceEvenly JustifyContentValue where
  spaceEvenly = fromString "space-evenly"

_counter = Proxy :: Proxy "counter"

component :: forall m
          . MonadAff m
          => Int
          -> H.Component Query Input Output m
component numChildren = H.mkComponent
  { initialState: \i -> { count: i }
  , render
  , eval: H.mkEval H.defaultEval {
      initialize = Just Initialize
    , finalize = Just Finalize
    , handleAction = handleAction
    , handleQuery = handleQuery
    , receive = Just <<< Received
    }
  }
  where
    handleAction :: Action -> H.HalogenM State Action Slots Output m Unit
    handleAction = case _ of
      Initialize -> log "Initialized"
      Finalize -> log "Finalized"
      Decrement -> H.modify_ \s -> s { count = s.count - 1}
      Increment -> H.modify_ \s -> s { count = s.count + 1}
      FromChild slot c -> do
        void $ H.tell _counter slot $ SetCount 123
        count <- H.request _counter slot $ GetCount
        log $ "Received from Child " <> show (slot + 1) <> ": " <> show c
        log $ "Queried from Child " <> show (slot + 1) <> ": " <> show count
      RaiseParent -> H.get >>= \ { count } -> H.raise count
      Received c -> H.modify_ _ { count = c }

    render :: State -> H.ComponentHTML Action Slots m
    render { count } = let onClick = HE.onClick <<< const in
      HH.div []
      ([
        HH.div
        [ HC.style do
             display flex
             flexDirection row
             justifyContent spaceEvenly
             width (rem 4.0)
        ]
        [
          HH.button [ onClick Decrement ] [ HH.text "-" ]
        , HH.text $ show count
        , HH.button [ onClick Increment ] [ HH.text "+" ]
        ]
      , HH.div
        [ HC.style do
             display flex
             justifyContent center
             width (rem 6.0)
             paddingTop (rem 0.5)
        ]
        [ HH.button [ onClick RaiseParent ] [ HH.text "raise" ] ]
      ] <> children )
      where
        children = if numChildren == 0 then [] else
                     [ HH.div [
                         HC.style do
                            display flex
                            paddingLeft (rem 1.0)
                            paddingTop (rem 1.0)
                            paddingBottom (rem 1.0)
                         ] $ range 0 (numChildren - 1) <#> \n ->
                        HH.slot _counter n (component 0) (n + 1) (FromChild n) ]

    handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output m (Maybe a)
    handleQuery = case _ of
      SetCount c a -> H.modify_ _ { count = c } *> pure (Just a)
      GetCount reply -> H.get >>= \ { count } -> pure $ Just $ reply count
