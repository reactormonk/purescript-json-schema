module Data.JSON.Schema
  ( Schema(..)
  , Property(..)
  , StringFormat(..)
  , Definition(..)
  , Reference(..)
  , Description(..)
  , WForeign(..)

  , class JsonSchema
  , definition
  , description
  , schemaPath

  , class RecordDefinition
  , recordJsonSchema

  , class IsType
  , typeOf
  , TypeOf

  , class RowToProperty
  , toPropertyArray

  , showJson
  , showYaml
  , readSchemas
  ) where

import Prelude

import Control.Alternative ((<|>))
import Control.Monad.Error.Class (throwError)
import Data.Array (filter, fromFoldable) as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Foldable (any, oneOf)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Foreign (F, Foreign, ForeignError(..))
import Foreign.Object (Object)
import Foreign.Object as FO
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil)
import Simple.JSON (class ReadForeign, class WriteForeign, readImpl, write, writeImpl, writeJSON)
import Type.Prelude (RLProxy(..))
import Type.Proxy (Proxy(..))


-- | The different schema definitions, strings, arrays, objects and so on
data Schema
  = Object (Array Property)
  | String StringFormat
  | Number
  | Boolean
  | Int
  | Array (Array Schema)
  | Enum (Array WForeign)
  | Reference String
  | OneOf (Array Schema)
  | Const WForeign
  | Definitions (Object Schema)

instance showSchema :: Show Schema where
  show (Object obj) = "(Object " <> show obj <> ")"
  show (String fmt) = "(String " <> show fmt <> ")"
  show Number = "Number"
  show Boolean = "Boolean"
  show Int = "Int"
  show (Array s) = "(Array " <> show s <> ")"
  show (Reference r) = "(Reference " <> r <> ")"
  show (OneOf a) = "(OneOf " <> show a <> ")"
  show (Const s) = "(Const " <> show s <> ")"
  show (Definitions d) = "(Definitions " <> show d <> ")"
  show (Enum e) = "(Enum " <> show e <> ")"

derive instance eqSchema :: Eq Schema

-- | The different string format
data StringFormat
  = None
  | Date
  | DateTime
  | Password
  | Byte
  | Binary

derive instance genericStringFormat :: Generic StringFormat _

instance showStringFormat :: Show StringFormat where
  show = genericShow
derive instance eqStringFormat :: Eq StringFormat

-- TODO typecheck enum
type Property = { required :: Boolean, name :: String, schema :: Schema }

newtype WForeign = WForeign Foreign
derive instance ntWForeign :: Newtype WForeign _
derive newtype instance readForeignWForeign :: ReadForeign WForeign
derive newtype instance writeForeignWForeign :: WriteForeign WForeign
instance eqWForeign :: Eq WForeign where
  eq a b = foreignEq (unwrap a) (unwrap b)
instance showWForeign :: Show WForeign where
  show = stringify <<< unwrap

foreign import foreignEq :: Foreign -> Foreign -> Boolean

newtype Definition :: forall k. k -> Type
newtype Definition a = Definition Schema

derive instance newtypeDefinition :: Newtype (Definition a) _

derive newtype instance showDefinition :: Show (Definition a)
derive newtype instance eqDefinition :: Eq (Definition a)

newtype Reference :: forall k. k -> Type
newtype Reference a = Ref String

derive instance newtypeReference :: Newtype (Reference a) _

newtype Description :: forall k. k -> Type
newtype Description a = Description String

derive instance newtypeDescription :: Newtype (Description a) _

class JsonSchema :: forall k. k -> Constraint
class JsonSchema a where
  definition :: Definition a
  description :: Maybe (Description a)
  schemaPath :: Reference a

class RecordDefinition :: forall k. k -> Constraint
class RecordDefinition a where
  recordJsonSchema :: Definition a

class IsType :: forall k. k -> Constraint
class IsType a where
  typeOf :: Proxy a -> TypeOf

data TypeOf = BuiltinType | RefType | ArrayType TypeOf

derive instance eqTypeOf :: Eq TypeOf

instance jsonSchemaString :: JsonSchema String where
  definition = Definition (String None)
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaInt :: JsonSchema Int where
  definition = Definition Int
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaNumber :: JsonSchema Number where
  definition = Definition Number
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaBoolean :: JsonSchema Boolean where
  definition = Definition Boolean
  description = Nothing
  schemaPath = Ref "<INVALID>"

instance jsonSchemaArray :: JsonSchema a => JsonSchema (Array a) where
  definition = Definition $ Array $ pure $ unwrap (definition :: Definition a)
  description = Nothing
  schemaPath = Ref $ unwrap (schemaPath :: Reference a)

instance basicTypeString :: IsType String where
  typeOf _ = BuiltinType

else instance basicTypeInt :: IsType Int where
  typeOf _ = BuiltinType

else instance basicTypeArray :: IsType a => IsType (Array a) where
  typeOf _ = ArrayType $ typeOf (Proxy :: Proxy a)

else instance basicTypeMaybe :: IsType a => IsType (Maybe a) where
  typeOf _ = typeOf (Proxy :: Proxy a)

else instance basicTypeOther :: IsType a where
  typeOf _ = RefType

-- | Write a `Definition a` to a JSON string
showJson :: ∀ a. Definition a -> String
showJson = writeJSON <<< writeImpl

-- | Write a `Definition a` to a YAML string
showYaml :: ∀ a. Definition a -> String
showYaml = writeYaml <<< writeImpl

foreign import writeYaml :: Foreign -> String

instance writeForeignDefinition :: WriteForeign (Definition a) where
  writeImpl (Definition s) = writeImpl s

instance writeForeignSchema :: WriteForeign Schema where
  writeImpl :: Schema -> Foreign
  writeImpl Int = write { "type": "integer" }
  writeImpl Number = write { "type": "number" }
  writeImpl Boolean = write { "type": "boolean" }
  writeImpl (Array s) = write { "type": "array", "items": s }
  writeImpl (Reference r) = write { "$ref": r }
  writeImpl (OneOf o) = write { "oneOf": o }
  writeImpl (Const s) = write { "const": s }
  writeImpl (Definitions d) = write { "definitions": d }
  writeImpl (Enum e) = write { "enum": e }
  writeImpl (String fmt) = case format fmt of
    Just f -> write { "type": "string", format: f }
    Nothing -> write { "type": "string" }
    where
      format None = Nothing
      format Date = Just "date"
      format DateTime = Just "date-time"
      format Password = Just "password"
      format Byte = Just "byte"
      format Binary = Just "binary"
  writeImpl (Object properties) =
    if any (_.required) properties
      then write { "type": "object", required: Array.filter (_.required) properties, properties }
      else write { "type": "object", properties }

class RowToProperty xs where
  toPropertyArray :: RLProxy xs -> Array Property

instance recordUnamedSchema ::
  ( RowToList fields fieldList
  , RowToProperty fieldList
  ) => RecordDefinition (Record fields) where
  recordJsonSchema = Definition $ Object $ toPropertyArray fieldListP
    where
      fieldListP = RLProxy :: RLProxy fieldList

instance writeDefinitionFieldsMaybeCons ::
  ( IsSymbol name
  , IsType head
  , JsonSchema head
  , RowToProperty tail
  , Row.Cons name (Maybe head) trash row
  ) => RowToProperty (Cons name (Maybe head) tail) where
  toPropertyArray _ = [{ required: false, name: (reflectSymbol nameP), schema }] <> toPropertyArray rest
    where
      schema = case typeOf (Proxy :: Proxy head) of
        BuiltinType -> unwrap (definition :: Definition head)
        RefType -> Reference $ unwrap (schemaPath :: Reference head)
        ArrayType tp ->
          if tp == BuiltinType
            then unwrap (definition :: Definition head)
            else Array $ pure $ Reference $ unwrap (schemaPath :: Reference head)
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

else instance rowToPropertyCons ::
  ( IsSymbol name
  , IsType head
  , JsonSchema head
  , RowToProperty tail
  , Row.Cons name head trash row
  ) => RowToProperty (Cons name head tail) where
  toPropertyArray _ = [{ required: true, name: (reflectSymbol nameP), schema }] <> toPropertyArray rest
    where
      schema = case typeOf (Proxy :: Proxy head) of
        BuiltinType -> unwrap (definition :: Definition head)
        RefType -> Reference $ unwrap (schemaPath :: Reference head)
        ArrayType tp ->
          if tp == BuiltinType
            then unwrap (definition :: Definition head)
            else Array $ pure $ Reference $ unwrap (schemaPath :: Reference head)
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

instance rowToPropertyNil :: RowToProperty Nil where
  toPropertyArray _ = []

readSchemas :: Foreign -> F ({ root :: Schema, more :: Object Schema })
readSchemas j = do
  raw :: JSONFullSchema <- readImpl j
  root :: Schema <- readImpl j
  pure { root, more: fromMaybe FO.empty raw."$defs" }

type JSONFullSchema =
  { "$id" :: String
  , "$defs" :: Maybe (Object Schema)
  , "$schema" :: String
  -- , description :: String
  -- , properties :: Object Schema
  -- , type :: String
  }

instance readForeignSchema :: ReadForeign Schema where
  readImpl j = do
    raw :: RawSchema <- readImpl j
    case raw.enum of
      Just enum -> pure $ Enum enum
      Nothing -> 
        case raw."type" of 
          Just "integer" -> pure Int
          Just "number" -> pure Number
          Just "boolean" -> pure Boolean
          Just "array" -> do
            case raw.items of
              Nothing ->
                throwError $ pure $ ErrorAtProperty "items" $ TypeMismatch "Array" "null"
              Just items -> do
                pure $ Array $ Array.fromFoldable $ unwrap $ items
          Just "string" -> do
            case raw.format of
              Nothing -> pure $ String None
              Just format -> pure $ String format
          Just "object" -> do
            let
              testObj = case raw.required of
                Just req -> FO.fromFoldable $ req <#> (\p -> Tuple p true)
                Nothing -> FO.empty
              isRequired name = FO.member name testObj
            case raw.properties of
              Just p -> do
                pure $ Object $ (FO.toUnfoldable p) <#> (\(Tuple name schema) ->
                  { name, schema, required: isRequired name })
              Nothing -> pure $ Object []
          Nothing ->
            fromMaybe
            (throwError $ pure $ TypeMismatch "Schema" $ stringify j)
            (oneOf [
              raw."$ref" <#> \r -> pure $ Reference r
            , raw.oneOf <#> \oneOf -> pure $ OneOf oneOf
            , raw.const <#> \const -> pure $ Const const
            , raw.definitions <#> \defs -> pure $ Definitions defs
            ])
          Just t -> throwError $ pure $ ErrorAtProperty "type" $ TypeMismatch "Type" t
    
foreign import stringify :: Foreign -> String

type RawSchema =
  { "type" :: Maybe String
  , required :: Maybe (Array String)
  , items :: Maybe (OneOrMore Schema)
  , format :: Maybe StringFormat
  , "$ref" :: Maybe String
  , properties :: Maybe (Object Schema)
  , enum :: Maybe (Array WForeign)
  , oneOf :: Maybe (Array Schema)
  , const :: Maybe WForeign
  , definitions :: Maybe (Object Schema)
  }

instance readForeignStringFormat :: ReadForeign StringFormat where
  readImpl j = do
    raw <- readImpl j
    case raw of
      "date" -> pure Date
      "date-time" -> pure DateTime
      "password" -> pure Password
      "byte" -> pure Byte
      "binary" -> pure Binary
      s -> throwError $ pure $ TypeMismatch "StringFormat" s

newtype OneOrMore a = OneOrMore (NonEmptyArray a)
derive instance oneOrMoreEq :: Eq a => Eq (OneOrMore a)
derive instance oneOrMoreGeneric :: Generic (OneOrMore a) _
derive instance oneOrMoreNewtype :: Newtype (OneOrMore a) _
instance oneOrMoreShow :: Show a => Show (OneOrMore a) where
  show = genericShow
instance oneOrMoreReadForeign :: (Show a, ReadForeign a) => ReadForeign (OneOrMore a) where
  readImpl j = do
      let
        ary :: _ (NonEmptyArray a) 
        ary = readImpl j
        a :: _ a
        a = readImpl j
      (OneOrMore <$> ary) <|> ((OneOrMore <<< pure) <$> a)
instance oneOrMoreWriteForeign :: WriteForeign a => WriteForeign (OneOrMore a) where
  writeImpl (OneOrMore a) = writeImpl a
  