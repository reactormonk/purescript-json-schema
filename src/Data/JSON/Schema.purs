module Data.JSON.Schema
  ( Schema(..)
  , Property(..)
  , StringFormat(..)
  , Definition(..)
  , Reference(..)
  , Description(..)

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
  ) where

import Prelude

import Data.Array (filter)
import Data.Foldable (any)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Foreign (Foreign)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Simple.JSON (class WriteForeign, write, writeImpl, writeJSON)
import Type.Prelude (RLProxy(..))
import Type.Proxy (Proxy(..))


-- | The different schema definitions, strings, arrays, objects and so on
data Schema
  = Object (Array Property)
  | String StringFormat
  | Number
  | Boolean
  | Int
  | Array Schema
  | Reference String

instance showSchema :: Show Schema where
  show (Object obj) = "(Object " <> show obj <> ")"
  show (String fmt) = "(String " <> show fmt <> ")"
  show Number = "Number"
  show Boolean = "Boolean"
  show Int = "Int"
  show (Array s) = "(Array " <> show s <> ")"
  show (Reference r) = "(Reference " <> r <> ")"

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

type Property = { required :: Boolean, name :: String, schema :: Schema }

newtype Definition a = Definition Schema

derive instance newtypeDefinition :: Newtype (Definition a) _

derive newtype instance showDefinition :: Show (Definition a)
derive newtype instance eqDefinition :: Eq (Definition a)

newtype Reference a = Ref String

derive instance newtypeReference :: Newtype (Reference a) _

newtype Description a = Description String

derive instance newtypeDescription :: Newtype (Description a) _

class JsonSchema a where
  definition :: Definition a
  description :: Maybe (Description a)
  schemaPath :: Reference a

class RecordDefinition a where
  recordJsonSchema :: Definition a

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
  definition = Definition $ Array $ unwrap (definition :: Definition a)
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
  writeImpl (Array s) = write { "type": "array", "items": writeImpl s }
  writeImpl (Reference r) = write { "$ref": r }
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
      then write { "type": "object", required: filter (_.required) properties, properties }
      else write { "type": "object", properties }
    where
      required { required: true, name: n, schema: _ } = [n]
      required _ = []

class RowToProperty (xs :: RowList) where
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
            else Array $ Reference $ unwrap (schemaPath :: Reference head)
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
            else Array $ Reference $ unwrap (schemaPath :: Reference head)
      nameP = SProxy :: SProxy name
      rest = RLProxy :: RLProxy tail

instance rowToPropertyNil :: RowToProperty Nil where
  toPropertyArray _ = []