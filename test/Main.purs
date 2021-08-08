module Test.Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Data.Either (isRight)
import Data.JSON.Schema (class JsonSchema, Definition(..), Reference, Schema(..), StringFormat(..), definition, recordJsonSchema, schemaPath)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.FS.Sync (readdir)
import Simple.JSON (readImpl, readJSON')
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

-- | This test aims to implement support for the following definition:
-- |
-- | ```
-- | User:
-- |   type: object
-- |   required: [name, parents]
-- |   properties:
-- |     name:
-- |       type: string
-- |     age:
-- |       type: integer
-- |     parents:
-- |       type: array
-- |       items:
-- |         '$ref': '#/definitions/Parent'
-- | ```
main :: Effect Unit
main = launchAff_ $ do
  files <- liftEffect $ readdir "test/TestSchemas"
  runSpec [consoleReporter] do
    describe "purescript-json-schema" do
      it "generate for newtyped String" do
        definition `shouldEqual` userNameDef

      it "generate for { username :: UserName }" do
        recordJsonSchema `shouldEqual` userWithUserName

      it "generate for { string :: String }" do
        recordJsonSchema `shouldEqual` recordWithString

      it "generate for { username :: Maybe UserName, address :: String }" do
        recordJsonSchema `shouldEqual` withMaybeUserNameDef

      it "generate for { arr :: Array String }" do
        recordJsonSchema `shouldEqual` recordWithArr

      it "generate for { parents :: Array Parent }" do
        recordJsonSchema `shouldEqual` parents

      it "generate for { ages :: Maybe (Array String) }" do
        recordJsonSchema `shouldEqual` recordWithMaybeArr

      it "generate for { name :: UserName, age :: Maybe Int, parents :: Array Parent }" do
        recordJsonSchema `shouldEqual` userDefinition

      files `for_` \file -> do
        it ("can read schema from " <> file) do
          text <- readTextFile UTF8 $ "test/TestSchemas/" <> file
          let schema = unwrap $ runExceptT $ do
                json <- readJSON' text
                readImpl json :: _ Schema
          schema `shouldSatisfy` isRight

    where
      userDefinition :: Definition { name :: UserName, age :: Maybe Int, parents :: Array Parent }
      userDefinition =
        Definition $ Object 
          [ { required: false, name: "age", schema: Int }
          , { required: true, name: "name", schema: Reference "#/definitions/UserName" }
          , { required: true, name: "parents", schema: Array $ pure $ Reference "#/definitions/Parent" }
          ]

      parents :: Definition { parents :: Array Parent }
      parents =
        Definition $ Object
          [ { required: true, name: "parents", schema: Array $ pure $ Reference "#/definitions/Parent" } ]

      recordWithString :: Definition { string :: String }
      recordWithString =
        Definition $ Object $ [ { required: true, name: "string", schema: String None } ]

      userWithUserName :: Definition { username :: UserName }
      userWithUserName =
        Definition $ Object $
        [ { required: true, name: "username", schema: Reference "#/definitions/UserName" } ]

      recordWithArr :: Definition { arr :: Array String }
      recordWithArr = Definition $ Object $ [ { required: true, name: "arr", schema: Array $ pure (String None) } ]

      recordWithMaybeArr :: Definition { arr :: Maybe (Array String) }
      recordWithMaybeArr = Definition $ Object $ [ { required: false, name: "arr", schema: Array $ pure (String None) } ]

      userNameDef :: Definition UserName
      userNameDef = Definition (String None)

      withMaybeUserNameDef :: Definition { username :: Maybe UserName, address :: String }
      withMaybeUserNameDef =
        Definition $ Object $
        [ { required: true, name: "address", schema: String None }
        , { required: false, name: "username", schema: Reference "#/definitions/UserName" }
        ]

newtype UserName = UserName String

instance jsonSchemaUserName :: JsonSchema UserName where
  schemaPath = wrap "#/definitions/UserName"
  definition = Definition $ String None
  description = Nothing

newtype Parent = Parent UserName

instance jsonSchemaParent :: JsonSchema Parent where
  schemaPath = wrap "#/definitions/Parent"
  definition = wrap $ Reference $ unwrap (schemaPath :: Reference UserName)
  description = Nothing
