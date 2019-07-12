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
  = CorrectionAlias
  | ControlAlias
  | AlternateAlias
  | FigmentAlias
  | AbbreviationAlias
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
