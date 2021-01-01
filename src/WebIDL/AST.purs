module WebIDL.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data Type
  = TypeUnsignedInteger String
  | TypeFloat String
  | TypeString String

derive instance genericType :: Generic Type _
instance showType :: Show Type where show = genericShow

data IDLType
  = IDLTypeNamed String
  | IDLTypeNullable Type

derive instance genericIDLType :: Generic IDLType _
instance showIDLType :: Show IDLType where show = genericShow

data Node
  = NodeTypeDef
    { name :: String
    , idlType :: IDLType
    }

derive instance genericNode :: Generic Node _
instance showNode :: Show Node where show = genericShow
