module Router
	( Pathy(SegmentDir, SegmentFile, TokenDir, TokenFile, RootDir)
	, IsURI, fromURI, toURI
	) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import qualified Data.Path.Pathy as P

data Pathy = SegmentDir  (P.RelDir  P.Sandboxed)
		   | SegmentFile (P.RelFile P.Sandboxed)
		   | TokenDir    (P.RelDir  P.Sandboxed)
		   | TokenFile   (P.RelFile P.Sandboxed)
		   | RootDir

instance showPathy :: Show Pathy where
	show (SegmentDir  d) = "SegmentDir "  <> show d
	show (SegmentFile f) = "SegmentFile " <> show f
	show (TokenDir    d) = "TokenDir "    <> show d
	show (TokenFile   f) = "TokenFile "   <> show f
	show RootDir         = "RootDir"

class IsURI a where
	fromURI :: String -> Maybe a
	toURI   :: a      -> String

foreign import decodeURIComponent :: String -> String
foreign import encodeURIComponent :: String -> String

instance isURIString :: IsURI String where
	fromURI = decodeURIComponent >>> Just
	toURI   = id >>> encodeURIComponent

foreign import parseIntImpl :: forall a.
	(a -> Maybe a) -> -- Just constructor
	(a -> Maybe a) -> -- Nothing constructor
	String         ->
	Maybe Int

parseInt :: String -> Maybe Int
parseInt = parseIntImpl Just (const Nothing)

instance isURIInt :: IsURI Int where
	fromURI = decodeURIComponent >>> parseInt
	toURI   = show >>> encodeURIComponent
