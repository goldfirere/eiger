{-|
Description : JSON support for Eiger
Copyright   : Richard Eisenberg
License     : MIT
Maintainer  : rae@richarde.dev
Stability   : experimental

A challenge in specifying a JSON format for an Eiger database is that of keys.
That is, some entities need to refer to other entities, but we naturally don't
have proper keys in JSON. We thus use the JSON object format to allow us to
specify keys: the names of object components are used as keys in the JSON file.
These names are *not* stored in any way in the loaded database. Instead, they
are solely used to denote cross-references in the JSON format.

We thus use the following data format:

@
  entities ::= group_object  -- each entity has a name used for cross-references
           |   array         -- each entity is anonymous

  group_object ::= '{' group_entries '}'

  group_entries ::= {- empty -}
                |   group_entry ',' group_entries

  group_entry ::= json_string ':' entity  -- the string is the cross-reference key
              |   '"data"' ':' entities   -- this allows for anonymous entities mixed
                                          -- with named ones

  array ::= '[' entity_list ']'

  entity_list ::= {- empty -}
              |   entity ',' entity_list

  entity ::= '{' '"type"' ':' json_string ',' entity_entries '}'
    -- the first entry in an entity is named "type", and it includes
    -- the name of the Haskell type of the entity
    -- TODO: handle module prefixes?

  entity_entries ::= {- empty -}
                 |   entity_entry ',' entity_entries

  entity_entry ::= json_string ':' entity_value

  entity_value ::= untyped_entity -- this is used when the Haskell type for the field
                                  -- is a key, and there are no cross-references to
                                  -- the encoded object; the enclosing entity in the JSON
                                  -- source is the only referer.
               |   value
               |   'null'         -- this is treated identically to an omitted field

  untyped_entity ::= '{' entity_entries '}'

  value ::= json_string     -- corresponds to a Haskell field type with an IsString instance
        |   json_number     -- corresponds to a Haskell field type with a DSL.FromInt instance
                            -- if the value is integral, otherwise a RealFloat instance
        |   'true'
        |   'false'
        |   json_array   -- corresponds to a Haskell list;
                         -- elements are parsed according
                         -- to their FromJSON instances, unless the elements are Keys, which
                         -- get the special treatment they do elsewhere
        -- NB: no objects here

@

-}

module Eiger.JSON (
  module Eiger.JSON.Class,
  module Eiger.JSON.Read,
  module Eiger.JSON.Write,
  InstanceDatabase, mkInstanceDatabase, deriveJSONRecordInstances,
  ) where

import Eiger.JSON.Class
import Eiger.JSON.Read
import Eiger.JSON.Instance
import Eiger.JSON.Write