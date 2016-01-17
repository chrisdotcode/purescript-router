module Router
	( IsURI, fromURI, toURI
	, Parser()
	, rootDir
	, dir
	, file
	, matchDir
	, matchFile
	, regexDir
	, regexFile
	, alphaDir
	, alphaFile
	, alphaNumDir
	, alphaNumFile
	, numDir
	, numFile
	, paramDir
	, paramFile
	, Method
		( OPTIONS
		, GET
		, HEAD
		, POST
		, PUT
		, PATCH
		, DELETE
		, TRACE
		, CONNECT
		, Custom
		)
	, methodFromString
	, Route(NotFound, Route)
	, IsRoute
	, route
	, makeRoute
	) where

import Prelude

import Data.Char         (toString)
import Data.Either       (either)
import Data.Maybe        (Maybe(Just, Nothing))
import Data.String       (toChar, toLower, toUpper)
import Data.String.Regex (Regex(), noFlags, regex)
import Data.Tuple        (Tuple(Tuple))
import Data.URI.Types    (HierarchicalPart(HierarchicalPart), URI(URI))
import qualified Data.Path.Pathy as P

(|>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c
(|>) = (>>>)

data Pathy = SegmentDir  (P.RelDir  P.Sandboxed)
		   | SegmentFile (P.RelFile P.Sandboxed)
		   | TokenDir    (P.RelDir  P.Sandboxed)
		   | TokenFile   (P.RelFile P.Sandboxed)
		   | RegexDir    Regex
		   | RegexFile   Regex
		   | RootDir

instance showPathy :: Show Pathy where
	show (SegmentDir  d) = "SegmentDir ("  <> show d <> ")"
	show (SegmentFile f) = "SegmentFile (" <> show f <> ")"
	show (TokenDir    d) = "TokenDir ("    <> show d <> ")"
	show (TokenFile   f) = "TokenFile ("   <> show f <> ")"
	show (RegexDir    d) = "RegexDir ("    <> show d <> ")"
	show (RegexFile   f) = "RegexFile ("   <> show f <> ")"
	show RootDir         = "RootDir"

class IsURI a where
	fromURI :: String -> Maybe a
	toURI   :: a      -> String

foreign import decodeURIComponent :: String -> String
foreign import encodeURIComponent :: String -> String

instance isURIChar :: IsURI Char where
	fromURI = toChar
	toURI   = toString |> encodeURIComponent

instance isURIString :: IsURI String where
	fromURI = decodeURIComponent |> Just
	toURI   = id |> encodeURIComponent

foreign import parseIntImpl :: forall a.
	(a -> Maybe a) -> -- Just constructor
	(a -> Maybe a) -> -- Nothing constructor
	String         ->
	Maybe Int

parseInt :: String -> Maybe Int
parseInt = parseIntImpl Just (const Nothing)

instance isURIInt :: IsURI Int where
	fromURI = decodeURIComponent |> parseInt
	toURI   = show |> encodeURIComponent

type Continuation a = forall b. (IsURI a) => (a -> b)

data Parser a = Failure
			  | Parser (Array Pathy) a
			  | Continuation (Array Pathy) a

rootDir :: Parser Unit
rootDir = Parser [RootDir] unit

dir :: String -> Parser Unit
dir d = Parser [SegmentDir $ P.dir d] unit

file :: String -> Parser Unit
file f = Parser [SegmentFile $ P.file f] unit

matchDir :: String -> Parser Unit
matchDir d = Parser [TokenDir $ P.dir d] unit

matchFile :: String -> Parser Unit
matchFile f = Parser [TokenFile $ P.file f] unit

regexDir :: Regex -> Parser Unit
regexDir d = Parser [RegexDir d] unit

regexFile :: Regex -> Parser Unit
regexFile f = Parser [RegexFile f] unit

alphaRegex :: Regex
alphaRegex = regex "^[a-z]+$" noFlags

alphaDir :: Parser Unit
alphaDir = regexDir alphaRegex

alphaFile :: Parser Unit
alphaFile = regexFile alphaRegex

alphaNumRegex :: Regex
alphaNumRegex = regex "^[a-z0-9]+$" noFlags

alphaNumDir :: Parser Unit
alphaNumDir = regexDir alphaNumRegex

alphaNumFile :: Parser Unit
alphaNumFile = regexFile alphaNumRegex

numRegex :: Regex
numRegex = regex "^[0-9]+$" noFlags

numDir :: Parser Unit
numDir = regexDir numRegex

numFile :: Parser Unit
numFile = regexFile numRegex

paramRegex :: Regex
paramRegex = regex "^[a-z0-9_$\\-_.+!*'(),]+$" noFlags

paramDir :: Parser Unit
paramDir = regexDir paramRegex

paramFile :: Parser Unit
paramFile = regexFile paramRegex

data Method = OPTIONS
			| GET
			| HEAD
			| POST
			| PUT
			| PATCH
			| DELETE
			| TRACE
			| CONNECT
			| Custom String

instance showMethod :: Show Method where
	show OPTIONS    = "OPTIONS"
	show GET        = "GET"
	show HEAD       = "HEAD"
	show POST       = "POST"
	show PUT        = "PUT"
	show PATCH      = "PATCH"
	show DELETE     = "DELETE"
	show TRACE      = "TRACE"
	show CONNECT    = "CONNECT"
	show (Custom c) = "Custom " <> show c

methodFromString :: String -> Method
methodFromString c = case c' of
	"OPTIONS" -> OPTIONS
	"GET"     -> GET
	"HEAD"    -> HEAD
	"POST"    -> POST
	"PUT"     -> PUT
	"PATCH"   -> PATCH
	"DELETE"  -> DELETE
	"TRACE"   -> TRACE
	"CONNECT" -> CONNECT
	_         -> Custom c'
		where
			c' = toUpper c

data Route a = NotFound | Route Method a

instance showRoute :: (Show a) => Show (Route a) where
	show NotFound    = "NotFound"
	show (Route m a) = "Route (" <> show m <> ") (" <> show a <> ")"

class IsRoute a where
	route :: URI -> Route a

toPathyArray :: forall a b. P.Path a b P.Sandboxed -> Array Pathy
toPathyArray p = [RootDir] <> folder (P.peel p) []
	where
		toSegmentDir  = P.runDirName  |> toLower |> P.dir  |> SegmentDir
		toSegmentFile = P.runFileName |> toLower |> P.file |> SegmentFile

		folder Nothing            xs = xs
		folder (Just (Tuple d e)) xs =
			folder (P.peel d) ([either toSegmentDir toSegmentFile e] <> xs)

makeRoute :: forall a. (IsRoute a) => URI -> a -> Parser a
makeRoute (URI _ (HierarchicalPart _ Nothing ) _ _) = Parser []
makeRoute (URI _ (HierarchicalPart _ (Just u)) _ _) = Parser (toPathyArray' u)
	where
		toPathyArray' = either toPathyArray toPathyArray
