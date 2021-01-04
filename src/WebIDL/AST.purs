module WebIDL.AST where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data IDLType
  = IDLTypeNamed String
  | IDLTypeNullable IDLType

derive instance eqIDLType :: Eq IDLType
derive instance genericIDLType :: Generic IDLType _
instance showIDLType :: Show IDLType where show t = genericShow t

data Node
  = NodeTypeDef
    { name :: String
    , idlType :: IDLType
    }

derive instance eqNode :: Eq Node
derive instance genericNode :: Generic Node _
instance showNode :: Show Node where show = genericShow
