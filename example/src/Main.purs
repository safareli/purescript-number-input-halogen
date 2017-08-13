module Main where

import Prelude

import NumberInput.Halogen.Component as NI
import NumberInput.Range (Range(..))
import Control.Monad.Eff (Eff)
import Data.Either.Nested as Either
import Data.Functor.Coproduct.Nested as Coproduct
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main ∷ Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body ← HA.awaitBody
  runUI example unit body

data Query a
  = HandleMsg NumInputIdx (NI.Message Number) a
  | Inc a

type State = Int

type NumInputIdx = Int
type ChildQuery = Coproduct.Coproduct1 (NI.Query Number)
type Slot = Either.Either1 NumInputIdx


cpNumInput ∷ CP.ChildPath (NI.Query Number) ChildQuery NumInputIdx Slot
cpNumInput = CP.cp1


type HTML m = H.ParentHTML Query ChildQuery Slot m
type DSL m = H.ParentDSL State Query ChildQuery Slot Void m


example ∷ ∀ m. H.Component HH.HTML Query Unit Void m
example = H.parentComponent
    { initialState: const 0
    , render
    , eval
    , receiver: const Nothing
    }

render ∷ ∀ m. State → HTML m
render count = HH.div_
  [ HH.h1_ [ HH.text "input 1" ]
  , HH.slot' cpNumInput 0 NI.input numProps (HE.input (HandleMsg 0))
  , HH.h1_ [ HH.text "input 2" ]
  , HH.slot' cpNumInput 1 NI.input numProps' (HE.input (HandleMsg 1))
  , HH.p_ [ HH.text $ show count ]
  , HH.button [ HE.onClick (HE.input_ Inc) ] [HH.text "inc"]
  ]

eval ∷ ∀ m. Query ~> DSL m
eval (HandleMsg _ _ next) = eval (Inc next)
eval (Inc next) = do
  count <- H.get
  H.put $ count + 1
  pure next

numProps' :: NI.Props Number
numProps' = numProps
  { range = MinMax 0.0 999.0
  , placeholder = "***"
  }

numProps :: NI.Props Number
numProps =
  { title: "title"
  , placeholder: "**"
  , hasNumberValue: NI.numberHasNumberInputValue
  , range: MinMax 0.0 99.0
  , root: [HH.ClassName "NumberInput"]
  , rootInvalid: [HH.ClassName "NumberInput--invalid"]
  , rootLength: const []
  }
