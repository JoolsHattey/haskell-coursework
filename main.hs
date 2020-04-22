--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--


import Data.List
import Data.Maybe

--
-- Types (define Place type here)
--

type LatLng = (Double, Double)

data Place = Place { locationName :: String  
                     , position :: LatLng  
                     , rainData :: [Int]  
                     } deriving (Show)  

testData :: [Place]
testData = [ Place "London" (51.5, -0.1) [0, 0, 5, 8, 8, 0, 0],
             Place "Cardiff" (51.5, -3.2) [12, 8, 15, 0, 0, 0, 2],
             Place "Norwich" (52.6, 1.3) [0, 6, 5, 0, 0, 0, 3],
             Place "Birmingham" (52.5, -1.9) [0, 2, 10, 7, 8, 2, 2],
             Place "Liverpool" (53.4, -3.0) [8, 16, 20, 3, 4, 9, 2],
             Place "Hull" (53.8, -0.3) [0, 6, 5, 0, 0, 0, 4],
             Place "Newcastle" (55.0, -1.6) [0, 0, 8, 3, 6, 7, 5],
             Place "Belfast" (54.6, -5.9) [10, 18, 14, 0, 6, 5, 2],
             Place "Glasgow"  (55.9, -4.3) [7, 5, 3, 0, 6, 5, 0],
             Place "Plymouth" (50.4, -4.1) [4, 9, 0, 0, 0, 6, 5],
             Place "Aberdeen" (57.1, -2.1) [0, 0, 6, 5, 8, 2, 0],
             Place "Stornoway" (58.2, -6.4) [15, 6, 15, 0, 0, 4, 2],
             Place "Lerwick" (60.2, -1.1) [8, 10, 5, 5, 0, 0, 3],
             Place "St Helier" (49.2, -2.1) [0, 0, 0, 0, 6, 10, 0] ]

--
--  Your functional code goes here
--

-- demo 1

getPlaceNames :: [Place] -> [String]
getPlaceNames placeList = map locationName placeList


-- demo 2

getAverageRainfallFromName :: String -> [Place] -> Float
getAverageRainfallFromName searchName placeList = getAverageRainfall ( getPlaceByName searchName placeList )

getAverageRainfall :: Place -> Float
getAverageRainfall place = averageList ( rainData place )

getPlaceByName :: String -> [Place] -> Place
getPlaceByName searchName placeList = head [ x | x <- placeList, locationName x == searchName ]

averageList :: [Int] -> Float
averageList input = fromIntegral (sum input) / fromIntegral (length input)


-- demo 3

placesToString :: [Place] -> String
placesToString placeList = intercalate "" (map placeToString placeList)

placeToString :: Place -> String
placeToString placeData = formatLocationName ( locationName placeData ) ++ (rainDataToString ( rainData placeData ) ) ++ "\n"

formatLocationName :: String -> String
formatLocationName location = location ++ intercalate "" ( replicate (13-(length location)) " ")

rainDataToString :: [Int] -> String
rainDataToString rainData = intercalate " " ( map show rainData )


-- demo 4

outputDryPlaces :: Int -> [Place] -> [String]
outputDryPlaces numDays placeList = map locationName ( getDryPlaces numDays placeList )

getDryPlaces :: Int -> [Place] -> [Place]
getDryPlaces numDays placeList = [ x | x <- placeList, (rainData x) !! (numDays-1) == 0]


--  demo 5

-- updateRainfallData :: [Int] -> [Place] -> [Place]
-- updateRainfallData newData placeList = map getfst data


-- demo 6

updateData :: String -> Place -> [Place] -> [Place]
updateData oldPlaceName newPlace placeList = replace (getIndex oldPlaceName placeList) newPlace placeList

getIndex :: String -> [Place] -> Int
getIndex searchString list = fromMaybe 0 ( findIndex (==searchString) (map locationName list) )

replace :: Int -> Place -> [Place] -> [Place]
replace pos newVal list = take pos list ++ newVal : drop (pos+1) list


-- demo 7

returnClosestDryPlace :: LatLng -> [Place] -> Place
returnClosestDryPlace location placeList = ( getDryPlaces 1 placeList ) !! ( getSmallestIndex ( getDistanceFromPlaces location ( getDryPlaces 1 placeList ) ) )

getDistanceFromPlaces :: LatLng -> [Place] -> [Double]
getDistanceFromPlaces location places = map (getDistance location) (map position places)

getSmallestIndex :: [Double] -> Int
getSmallestIndex listData = fromMaybe 0 ( findIndex (==minimum listData) listData )

getDistance :: LatLng -> LatLng -> Double
getDistance a b = (fst b - fst a) ** 2 + (snd b - snd a) ** 2


--
--  Demo
--

demo :: Int -> IO ()
demo 1 = print ( getPlaceNames testData )
demo 2 = print ( getAverageRainfallFromName "Cardiff" testData )
demo 3 = putStrLn ( placesToString testData )
demo 4 = print ( outputDryPlaces 2 testData )
-- demo 5 = -- update the data with most recent rainfall 
--          --[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
demo 6 = print ( updateData "Plymouth" (Place "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1]) testData )
demo 7 = print ( returnClosestDryPlace (50.9, -1.3) testData )
demo 8 = showMarkers testData


--
-- Screen Utilities (use these to do the rainfall map - note that these do 
-- not work in WinGHCi on Windows, so use GHCi.)
--

type ScreenPosition = (Int,Int)

-- Clears the screen
clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

-- Moves to a position on the screen
goTo :: ScreenPosition -> IO ()
goTo (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Writes a string at a position on the screen
writeAt :: ScreenPosition -> String -> IO ()
writeAt position text = do
    goTo position
    putStr text


--
-- Your rainfall map code goes here
--

getTopBound :: Double
getTopBound = maximum (map fst (map position testData))

getRightBound :: Double
getRightBound = maximum (map snd (map position testData))

getBottomBound :: Double
getBottomBound = minimum (map fst (map position testData))

getLeftBound :: Double
getLeftBound = minimum (map snd (map position testData))

getWidthRange :: Double
getWidthRange = getRightBound - getLeftBound

getHeightRange :: Double
getHeightRange = getTopBound - getBottomBound

getWidthRatio :: Double
getWidthRatio = 80 / getWidthRange

getHeightRatio :: Double
getHeightRatio = 50 / getHeightRange

getScreenPos :: LatLng -> ScreenPosition
getScreenPos pos = (round (((snd pos) - getLeftBound) * getWidthRatio), round (((fst pos) - getBottomBound) * getHeightRatio))

-- showMarkers :: IO ()
showMarkers placeList = do
    sequence [ writeAt (getScreenPos (position x)) ("+ " ++ locationName x ++ " " ++ show (getAverageRainfall x)) | x <- placeList ]

--
-- Your user interface (and loading/saving) code goes here
--

-- main :: IO ()
-- main = do
--     putStrLn "Rainfall Program\nPlease Select an Option:\n1: Return list of name of places\n2: Return average rainfall given a placename"
--     option <- getLine
--     feature (read option)

-- feature :: Int -> IO ()
-- feature 1 = print ( getPlaceNames testData )
-- feature 2 = do
--     putStrLn "Enter place name"
--     placeName <- getLine
--     print ( getAverageRainfall placeName testData )
-- feature 3 = print ( placesToString testData )
-- feature 4 = do
--     putStrLn "Enter number of days ago"
--     numDays <- getLine
--     print ( outputDryPlaces numDays testData )
-- -- feature 5 =
-- feature 6 = do
--     putStrLn "Enter location name to replace"
--     locationName <- getLine
--     putStrLn "Enter new place in this format (Place '<LocationName>' (Latitude, Longtitude) [List of raindata])"
--     placeData <- getLine
--     print ( updateData "Plymouth" (Place "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1]) testData )
-- feature 7 = do
--     putStrLn ""