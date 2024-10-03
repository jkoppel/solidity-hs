module Solidity.TypeSpec (spec) where

import Data.Text (Text)
import Solidity
import Test.Hspec
import TestUtils (testParseMany)

ints :: [(Text, ElementaryTypeName)]
ints =
  [ ("int", SignedInteger Nothing),
    ("int8", SignedInteger $ Just 8),
    ("int16", SignedInteger $ Just 16),
    ("int24", SignedInteger $ Just 24),
    ("int32", SignedInteger $ Just 32),
    ("int40", SignedInteger $ Just 40),
    ("int48", SignedInteger $ Just 48),
    ("int56", SignedInteger $ Just 56),
    ("int64", SignedInteger $ Just 64),
    ("int72", SignedInteger $ Just 72),
    ("int80", SignedInteger $ Just 80),
    ("int88", SignedInteger $ Just 88),
    ("int96", SignedInteger $ Just 96),
    ("int104", SignedInteger $ Just 104),
    ("int112", SignedInteger $ Just 112),
    ("int120", SignedInteger $ Just 120),
    ("int128", SignedInteger $ Just 128),
    ("int136", SignedInteger $ Just 136),
    ("int144", SignedInteger $ Just 144),
    ("int152", SignedInteger $ Just 152),
    ("int160", SignedInteger $ Just 160),
    ("int168", SignedInteger $ Just 168),
    ("int176", SignedInteger $ Just 176),
    ("int184", SignedInteger $ Just 184),
    ("int192", SignedInteger $ Just 192),
    ("int200", SignedInteger $ Just 200),
    ("int208", SignedInteger $ Just 208),
    ("int216", SignedInteger $ Just 216),
    ("int224", SignedInteger $ Just 224),
    ("int232", SignedInteger $ Just 232),
    ("int240", SignedInteger $ Just 240),
    ("int248", SignedInteger $ Just 248),
    ("int256", SignedInteger $ Just 256)
  ]

uints :: [(Text, ElementaryTypeName)]
uints =
  [ ("uint", UnsignedInteger Nothing),
    ("uint8", UnsignedInteger $ Just 8),
    ("uint16", UnsignedInteger $ Just 16),
    ("uint24", UnsignedInteger $ Just 24),
    ("uint32", UnsignedInteger $ Just 32),
    ("uint40", UnsignedInteger $ Just 40),
    ("uint48", UnsignedInteger $ Just 48),
    ("uint56", UnsignedInteger $ Just 56),
    ("uint64", UnsignedInteger $ Just 64),
    ("uint72", UnsignedInteger $ Just 72),
    ("uint80", UnsignedInteger $ Just 80),
    ("uint88", UnsignedInteger $ Just 88),
    ("uint96", UnsignedInteger $ Just 96),
    ("uint104", UnsignedInteger $ Just 104),
    ("uint112", UnsignedInteger $ Just 112),
    ("uint120", UnsignedInteger $ Just 120),
    ("uint128", UnsignedInteger $ Just 128),
    ("uint136", UnsignedInteger $ Just 136),
    ("uint144", UnsignedInteger $ Just 144),
    ("uint152", UnsignedInteger $ Just 152),
    ("uint160", UnsignedInteger $ Just 160),
    ("uint168", UnsignedInteger $ Just 168),
    ("uint176", UnsignedInteger $ Just 176),
    ("uint184", UnsignedInteger $ Just 184),
    ("uint192", UnsignedInteger $ Just 192),
    ("uint200", UnsignedInteger $ Just 200),
    ("uint208", UnsignedInteger $ Just 208),
    ("uint216", UnsignedInteger $ Just 216),
    ("uint224", UnsignedInteger $ Just 224),
    ("uint232", UnsignedInteger $ Just 232),
    ("uint240", UnsignedInteger $ Just 240),
    ("uint248", UnsignedInteger $ Just 248),
    ("uint256", UnsignedInteger $ Just 256)
  ]

units :: [(Text, NumberUnit)]
units =
  [ ("wei", Wei),
    ("gwei", Gwei),
    ("ether", Ether),
    ("seconds", Seconds),
    ("minutes", Minutes),
    ("hours", Hours),
    ("days", Days),
    ("weeks", Weeks),
    ("years", Years)
  ]

bytes :: [(Text, ElementaryTypeName)]
bytes =
  [ ("bytes1", FixedBytes 1),
    ("bytes2", FixedBytes 2),
    ("bytes3", FixedBytes 3),
    ("bytes4", FixedBytes 4),
    ("bytes5", FixedBytes 5),
    ("bytes6", FixedBytes 6),
    ("bytes7", FixedBytes 7),
    ("bytes8", FixedBytes 8),
    ("bytes9", FixedBytes 9),
    ("bytes10", FixedBytes 10),
    ("bytes11", FixedBytes 11),
    ("bytes12", FixedBytes 12),
    ("bytes13", FixedBytes 13),
    ("bytes14", FixedBytes 14),
    ("bytes15", FixedBytes 15),
    ("bytes16", FixedBytes 16),
    ("bytes17", FixedBytes 17),
    ("bytes18", FixedBytes 18),
    ("bytes19", FixedBytes 19),
    ("bytes20", FixedBytes 20),
    ("bytes21", FixedBytes 21),
    ("bytes22", FixedBytes 22),
    ("bytes23", FixedBytes 23),
    ("bytes24", FixedBytes 24),
    ("bytes25", FixedBytes 25),
    ("bytes26", FixedBytes 26),
    ("bytes27", FixedBytes 27),
    ("bytes28", FixedBytes 28),
    ("bytes29", FixedBytes 29),
    ("bytes30", FixedBytes 30),
    ("bytes31", FixedBytes 31),
    ("bytes32", FixedBytes 32)
  ]

types :: [(Text, ElementaryTypeName)]
types =
  [ ("string", String),
    ("bool", Bool),
    ("address payable", AddressPayable),
    ("address", Address),
    ("fixed", Fixed),
    ("ufixed", Ufixed),
    ("bytes", Bytes)
  ]


identifierTypes :: [(Text, TypeName)]
identifierTypes =
  [ ("Foo", IdentifierType (IdentifierPath [Identifier "Foo"])),
    ("Foo[]", ArrayType (IdentifierType (IdentifierPath [Identifier "Foo"])) [ArrayTypeEmpty])
  ]

spec :: Spec
spec = parallel $ do
  testParseMany parseElementaryTypeName ints "signed integers"
  testParseMany parseElementaryTypeName uints "unsigned integers"
  testParseMany parseNumberUnit units "number units"
  testParseMany parseElementaryTypeName bytes "number units"
  testParseMany parseElementaryTypeName types "other types"
  testParseMany parseTypeName identifierTypes "identifier types"
