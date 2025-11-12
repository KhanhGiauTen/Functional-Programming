{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Systems.MapLoader
	( TileMap(..)
	, TileLayer(..)
	, loadTileMap
	, layerRows
	) where

import Control.Exception (IOException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import GHC.Generics (Generic)

data TileMap = TileMap
	{ tmWidth      :: Int
	, tmHeight     :: Int
	, tmTileWidth  :: Int
	, tmTileHeight :: Int
	, tmLayers     :: [TileLayer]
	} deriving (Show, Generic)

data TileLayer = TileLayer
	{ tlId      :: Int
	, tlName    :: String
	, tlWidth   :: Int
	, tlHeight  :: Int
	, tlOpacity :: Double
	, tlVisible :: Bool
	, tlData    :: [Int]
	} deriving (Show, Generic)

instance Aeson.FromJSON TileMap where
	parseJSON = Aeson.withObject "TileMap" $ \obj ->
		TileMap <$> obj Aeson..: "width"
						<*> obj Aeson..: "height"
						<*> obj Aeson..: "tilewidth"
						<*> obj Aeson..: "tileheight"
						<*> obj Aeson..: "layers"

instance Aeson.FromJSON TileLayer where
	parseJSON = Aeson.withObject "TileLayer" $ \obj ->
		TileLayer <$> obj Aeson..: "id"
							<*> obj Aeson..: "name"
							<*> obj Aeson..: "width"
							<*> obj Aeson..: "height"
							<*> obj Aeson..: "opacity"
							<*> obj Aeson..: "visible"
							<*> obj Aeson..: "data"

loadTileMap :: FilePath -> IO (Either String TileMap)
loadTileMap path = do
	fileResult <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
	case fileResult of
		Left ioErr    -> pure (Left (show ioErr))
		Right payload -> pure (Aeson.eitherDecode payload)

layerRows :: TileLayer -> [[Int]]
layerRows layer = chunk (tlWidth layer) (tlData layer)
	where
		chunk _ [] = []
		chunk n xs
			| n <= 0    = []
			| otherwise = let (h, t) = splitAt n xs in h : chunk n t