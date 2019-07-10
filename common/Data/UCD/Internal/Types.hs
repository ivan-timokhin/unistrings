module Data.UCD.Internal.Types
  ( BidiClass(..)
  , NameAliasType(..)
  ) where

data BidiClass
  = LeftToRight
  | RightToLeft
  | ArabicLetter
  | EuropeanNumber
  | EuropeanSeparator
  | EuropeanTerminator
  | ArabicNumber
  | CommonSeparator
  | NonSpacingMark
  | BoundaryNeutral
  | ParagraphSeparator
  | SegmentSeparator
  | WhiteSpace
  | OtherNeutral
  | LeftToRightEmbedding
  | LeftToRightOverride
  | RightToLeftEmbedding
  | RightToLeftOverride
  | PopDirectionalFormat
  | LeftToRightIsolate
  | RightToLeftIsolate
  | FirstStrongIsolate
  | PopDirectionalIsolate
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data NameAliasType
  = Correction
  | Control
  | Alternate
  | Figment
  | Abbreviation
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
