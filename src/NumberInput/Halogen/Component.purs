module NumberInput.Halogen.Component
  ( input
  , Query(..)
  , Message(..)
  , Props
  , Input
  , InputValue
  , mkInputValue
  , HasNumberInputValue
  , numberHasNumberInputValue
  , intHasNumberInputValue
  , boundedEnumHasNumberInputValue )
  where

import Prelude

import CSS as CSS
import Control.Alternative (class Alternative, empty)
import Control.Monad.Except (runExcept)
import Control.MonadPlus (guard)
import DOM.Event.Event (Event)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Foreign (readBoolean, readString, toForeign)
import Data.Foreign.Index (readProp)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number as N
import Data.String (Pattern(..), length, stripSuffix)
import Data.Tuple (Tuple(..), fst, snd)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Core (ClassName)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import NumberInput.Range (Range(..), isInRange, rangeMax, rangeMin)
import Unsafe.Reference (unsafeRefEq)


type State val =
  { value ∷ InputValue val
  , props ∷ Props val
  }

data Message val = NotifyChange (Input val)
type Input val = Maybe val
data Query val next
  = GetValue (Input val → next)
  | SetValue (Input val) next
  | Update (InputValue val) next
  | SetProps (Props val) next

type DSL val = H.ComponentDSL (State val) (Query val) (Message val)
type HTML val = H.ComponentHTML (Query val)

type Props val =
  { title ∷ String
  , placeholder ∷ String
  , hasNumberValue ∷ HasNumberInputValue val
  , range ∷ Range val
  , root ∷ Array ClassName
  , rootInvalid ∷ Array ClassName
  , rootLength ∷ Int → Array ClassName
  }


input ∷ ∀ val m
  . Ord val
  ⇒ H.Component HH.HTML (Query val) (Props val) (Message val) m
input = H.component
  { initialState: { value: emptyNumberInputValue, props: _}
  , render: render
  , eval: eval
  , receiver: HE.input SetProps
  }

eval ∷ ∀ val m . Eq val ⇒ Query val ~> DSL val m
eval (SetProps props next) = do
  state <- H.get
  unless (unsafeRefEq props state.props) do
    let strVal = snd state.value
    putAndNotify state
      { props: props
      , value: Tuple (strVal >>= props.hasNumberValue.fromString) strVal
      }
  pure next
eval (SetValue number next) = do
  state <- H.get
  H.put $ state{value = Tuple number (toMbString state.props.hasNumberValue number)}
  pure next
eval (GetValue next) =
  H.get <#> (_.value >>> fst >>> next)
eval (Update number next) = do
  state ← H.get
  putAndNotify state (state{value = number})
  pure next

putAndNotify :: ∀ m val . Eq val ⇒ State val → State val → DSL val m Unit
putAndNotify oldstate state = do
  H.put state
  let nextVal = fst state.value
  unless (fst oldstate.value == nextVal) $ H.raise (NotifyChange $ nextVal)

toMbString ∷ ∀ a. HasNumberInputValue a → Maybe a → Maybe String
toMbString { toValue } number = (Just $ maybe "" toValue number)


type InputValue a = Tuple (Maybe a) (Maybe String)

toString ∷ ∀ a. InputValue a → String
toString (Tuple _ mbStr) = fromMaybe "" mbStr

mkInputValue ∷ ∀ a. HasNumberInputValue a → a → InputValue a
mkInputValue { toValue } n = Tuple (Just n) (Just $ toValue n)

emptyNumberInputValue ∷ ∀ a. InputValue a
emptyNumberInputValue = Tuple Nothing (Just "")

isInvalid  ∷ ∀ a. InputValue a → Boolean
isInvalid (Tuple Nothing (Just "")) = false
isInvalid (Tuple Nothing (Just _)) = true
isInvalid (Tuple _ Nothing) = true
isInvalid _ = false

isEmpty  ∷ ∀ a. InputValue a → Boolean
isEmpty (Tuple _ (Just "")) = true
isEmpty _ = false

showNum ∷ Number → String
showNum 0.0 = "0"
showNum n = let str = show n
  in fromMaybe str (stripSuffix (Pattern ".0") str)

render ∷ ∀ val
  . Ord val
  ⇒ State val
  → HTML val
render {props, value} = HH.input $
  [ HP.type_ HP.InputNumber
  , HP.classes classes
  , HP.title props.title
  , HP.placeholder props.placeholder
  , HP.value valueStr
  , HE.onInput $ HE.input $
    inputValueFromEvent
    >>> parseValidInput
    >>> isInputInRange props.range
    >>> Update
  ]
  <> (toArray (rangeMin props.range) <#> props.hasNumberValue.toNumber >>> HP.min)
  <> (toArray (rangeMax props.range) <#> props.hasNumberValue.toNumber >>> HP.max)
  <> [styles]
  where
  toArray = maybe [] pure
  -- Number and String value must comute (`map toValue (fromString x) == Just x`)
  -- to avoid this issues:
  --  * if user types `-0` we will parse it as `0` or
  --  * if user types `001` we will parse it as `1` or
  --  * if user types `0.1111111111111111111111` we will parse it as `0.1111111111111111` or
  --  * if user types `1e1` we will parse it as `10`
  parseValidInput ∷ InputValue String → InputValue val
  parseValidInput = lmap $ (=<<) \str → do
    val ← props.hasNumberValue.fromString str
    guard (props.hasNumberValue.toValue val == str)
    pure val

  valueStr = toString value
  sizeClass = case props.range of
    MinMax minVal maxVal →
      props.rootLength (max
        (length $ props.hasNumberValue.toValue minVal)
        (length $ props.hasNumberValue.toValue maxVal)
      )
    _ → []
  classes = props.root
    <> sizeClass
    <> (guard (isInvalid value) *> props.rootInvalid)
  controlWidth = 0.75
  styles = HCSS.style do
    case props.range of
      MinMax _ _ → pure unit
      _ | isInvalid value → pure unit
      _ | isEmpty value → CSS.width $ CSS.em 2.25
      _ → CSS.width $ CSS.em (Int.toNumber (length valueStr) * 0.5 + 1.0 + controlWidth)


-- We need to validate if value is in range manually as for example,
-- if `min = 0`, user still can enter `-1` in chrome.
isInputInRange ∷ ∀ a. Ord a ⇒ Range a → InputValue a → InputValue a
isInputInRange range val = lmap (_ >>= boolToAltPredicate (isInRange range)) val

boolToAltPredicate ∷ ∀ a f. Alternative f ⇒ (a → Boolean) → a → f a
boolToAltPredicate f a =  if f a then pure a else empty

inputValueFromEvent ∷ Event → InputValue String
inputValueFromEvent event = let val = validValueFromEvent event
  in Tuple val val

asRight ∷ ∀ e a f. Alternative f ⇒ Either e a → f a
asRight = either (const empty) pure

validValueFromEvent ∷ Event → Maybe String
validValueFromEvent event = join $ asRight $ runExcept $ do
  target ← readProp "target" $ toForeign event
  validity ← readProp "validity" target
  badInput ← readProp "badInput" validity >>= readBoolean
  value ← readProp "value" target >>= readString
  pure (if badInput then Nothing else Just value)

type HasNumberInputValue a  =
  { fromString ∷ String → Maybe a
  , toValue ∷ a → String
  , toNumber ∷ a → Number
  }

numberHasNumberInputValue ∷ HasNumberInputValue Number
numberHasNumberInputValue =
  { fromString: N.fromString
  , toValue: showNum
  , toNumber: id
  }

intHasNumberInputValue ∷ HasNumberInputValue Int
intHasNumberInputValue =
  { fromString: numberHasNumberInputValue.fromString >=> Int.fromNumber
  , toValue: show
  , toNumber: Int.toNumber
  }

boundedEnumHasNumberInputValue ∷ ∀ a. BoundedEnum a ⇒ HasNumberInputValue a
boundedEnumHasNumberInputValue =
  { fromString: intHasNumberInputValue.fromString >=> toEnum
  , toValue: fromEnum >>> intHasNumberInputValue.toValue
  , toNumber: fromEnum >>> intHasNumberInputValue.toNumber
  }
