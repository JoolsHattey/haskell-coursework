--
-- MATHFUN
-- UP894547
--

import System.IO
import Data.List
import Text.Printf
import Data.Maybe
import Data.Bool
import Data.Char (isSpace)

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

-- Helper functions

replaceNthPlace :: Int -> Place -> [Place] -> [Place]
replaceNthPlace _ _ [] = []
replaceNthPlace n newPlace (x:xs)
    | n == 0 = newPlace:xs
    | otherwise = x:replaceNthPlace (n-1) newPlace xs

averageList :: [Int] -> Float
averageList input = fromIntegral (sum input) / fromIntegral (length input)

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


-- demo 3

placesToString :: [Place] -> String
placesToString placeList = intercalate "" (map placeToString placeList)

placeToString :: Place -> String
placeToString placeData = formatLocationName ( locationName placeData ) ++ (rainDataToString ( rainData placeData ) ) ++ "\n"

formatLocationName :: String -> String
formatLocationName location = location ++ intercalate "" ( replicate (13-(length location)) " ")

rainDataToString :: [Int] -> String
rainDataToString rainData = intercalate "" (map formatColumn rainData)

formatColumn :: Int -> String
formatColumn rainPoint = show rainPoint ++ intercalate "" ( replicate ( 4 - (length (show rainPoint))) " ")


-- demo 4

outputDryPlaces :: Int -> [Place] -> [String]
outputDryPlaces numDays placeList = map locationName ( getDryPlaces numDays placeList )

getDryPlaces :: Int -> [Place] -> [Place]
getDryPlaces numDays placeList = [ x | x <- placeList, (rainData x) !! (numDays-1) == 0]


--  demo 5

updateRainfallData :: [Int] -> [Place] -> [Place]
updateRainfallData newData placeList = updateThing newData 0 placeList

updateThing :: [Int] -> Int -> [Place] -> [Place]
updateThing newData index placeList
    | (length newData) > 0 = updateThing (tail newData) (index+1) (replaceNthPlace index (updatePlaceRainData (head newData) (placeList !! index) ) placeList)
    | otherwise = placeList

updatePlaceRainData :: Int -> Place -> Place
updatePlaceRainData newData place = place { rainData = (insertRainPoint newData (rainData place)) }

insertRainPoint :: Int -> [Int] -> [Int]
insertRainPoint newData dataList = newData : (init dataList)


-- demo 6

updateData :: String -> Place -> [Place] -> [Place]
updateData oldPlaceName newPlace placeList = replaceNthPlace (getIndex oldPlaceName placeList) newPlace placeList

getIndex :: String -> [Place] -> Int
getIndex searchString list = fromMaybe 0 ( findIndex (==searchString) (map locationName list) )


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
demo 5 = print ( updateRainfallData [0,8,0,0,5,0,0,3,4,2,0,8,0,0] testData )
demo 6 = print ( updateData "Plymouth" (Place "Portsmouth" (50.8, -1.1) [0, 0, 3, 2, 5, 2, 1]) testData )
demo 7 = print ( returnClosestDryPlace (50.9, -1.3) testData )
demo 8 = do
    clearScreen
    showMarkers testData


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

padding :: Double
padding = 1.0

mapWidth :: Double
mapWidth = 80

mapHeight :: Double
mapHeight = 50

getTopBound :: Double
getTopBound = maximum (map fst (map position testData)) + padding

getRightBound :: Double
getRightBound = maximum (map snd (map position testData)) + padding

getBottomBound :: Double
getBottomBound = minimum (map fst (map position testData)) - padding

getLeftBound :: Double
getLeftBound = (minimum (map snd (map position testData))) - padding

getWidthRange :: Double
getWidthRange = getRightBound - getLeftBound

getHeightRange :: Double
getHeightRange = getTopBound - getBottomBound

getWidthRatio :: Double
getWidthRatio = mapWidth / getWidthRange

getHeightRatio :: Double
getHeightRatio = mapHeight / getHeightRange

getScreenPos :: LatLng -> ScreenPosition
getScreenPos pos = (round (abs((getLeftBound - (snd pos))) * getWidthRatio), round ((getTopBound - (fst pos)) * getHeightRatio))

showMarkers :: [Place] -> IO ()
showMarkers placeList = sequence_ [ renderMarker (getScreenPos (position x)) x | x <- placeList ]

renderMarker :: ScreenPosition -> Place -> IO ()
renderMarker pos place = do
    writeAt pos "+"
    renderText pos 1 (locationName place)
    renderText pos 2 ("Avg rain: " ++ printf "%.2f" (getAverageRainfall place))

renderText :: ScreenPosition -> Int -> String -> IO ()
renderText pos yOffset text = writeAt (centerText pos yOffset text) text

centerText :: ScreenPosition -> Int -> String -> ScreenPosition
centerText origin yOffset text = ((fst origin - (div (length text) 2)),(snd origin + yOffset))


--
-- Your user interface (and loading/saving) code goes here
--

parsePlace :: String -> Place
parsePlace placeString = Place (parseName placeString) (parseLocation placeString) (parseRainData placeString)

parseName :: String -> String
parseName placeString = trim ( fst (splitAt 13 placeString) )

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseLocation :: String -> LatLng
parseLocation placeString = ( tuplify ( words ( fst (splitAt 10 (snd (splitAt 13 placeString))))))

tuplify :: [String] -> (Double,Double)
tuplify [x,y] = (read x,read y)

parseRainData :: String -> [Int]
parseRainData placeString = map read (map (filter (not . (`elem` ","))) (words ( snd ( splitAt 26 placeString ) ) ) ) :: [Int]


stringifyPlaces :: [Place] -> String
stringifyPlaces placeList = intercalate "" (map stringifyPlace placeList)

stringifyPlace :: Place -> String
stringifyPlace place = stringifyName place ++ stringifyLocation place ++ "   " ++ stringifyRainData place ++ "\n"

stringifyName :: Place -> String
stringifyName place = (locationName place) ++ concat (replicate (13 - (length (locationName place))) " ")

stringifyLocation :: Place -> String
stringifyLocation place = show (fst (position place)) ++ concat (replicate  (6-(length ( show ( snd ( position place ) ) ) )) " ") ++ show (snd (position place))

stringifyRainData :: Place -> String
stringifyRainData place = intercalate ", " (map show (rainData place))

filePath :: String
filePath = "places.txt"

main :: IO ()
main = do
    content <- readFile filePath
    putStrLn $ "Loaded " ++ show (length content) ++ " places"
    let placeStringList = lines content
    let places = map parsePlace placeStringList
    menu places

menu :: [Place] -> IO ()
menu placeList = do
    clearScreen
    putStrLn "Rainfall Program\n\
    \ Please Select an Option:\n\
    \ 1: Return list of name of places\n\
    \ 2: Return average rainfall given a placename\n\
    \ 3: Print all places and their raindata in columns\n\
    \ 4: Return places that were dry a given number of days ago\n\
    \ 5: Update raindata with a list of new values\n\
    \ 6: Replace location with a new location\n\
    \ 7: Return closest dry place to a location\n\
    \ 8: Display rainfall map\n\
    \ 9: Save and Exit"
    option <- getLine
    clearScreen
    menuOption (read option) placeList

menuOption :: Int -> [Place] -> IO ()
menuOption 1 placeData = do
    putStrLn (intercalate "\n" ( getPlaceNames placeData ))
    putStrLn "\nPress any key to return to menu"
    x <- getChar
    menu placeData

menuOption 2 placeData = do
    putStrLn "Enter place name"
    placeName <- getLine
    clearScreen
    putStrLn ("Average Rainfall for " ++ placeName ++ ":")
    printf "%.2f" ( getAverageRainfallFromName placeName placeData )
    putStrLn "\n"
    putStrLn "\nPress any key to return to menu"
    x <- getChar
    menu placeData

menuOption 3 placeData = do
    clearScreen
    putStrLn ( placesToString placeData )
    writeAt (0,80) "Press any key to return to menu"
    x <- getChar
    menu placeData

menuOption 4 placeData = do
    putStrLn "Enter number of days ago"
    numDays <- getLine
    clearScreen
    putStrLn ("Places dry " ++ numDays ++ (bool " days" " day"(numDays == "1")) ++ " ago:\n")
    putStrLn (intercalate "\n" ( outputDryPlaces (read numDays :: Int) placeData ) )
    putStrLn "\nPress any key to return to menu"
    x <- getChar
    menu placeData

menuOption 5 placeData = do
    putStrLn "Enter new rain data"
    newData <- getLine
    let updatedPlaceData = updateRainfallData (read newData :: [Int]) placeData
    menu updatedPlaceData

menuOption 6 placeData = do
    putStrLn "Enter location name to replace"
    oldPlaceName <- getLine
    putStrLn "\n"
    newPlace <- inputNewPlace
    let updatedPlaceData = updateData oldPlaceName newPlace placeData
    putStrLn ("Successfully Replaced " ++ oldPlaceName ++ " with " ++ (locationName newPlace))
    putStrLn "\nPress any key to return to menu"
    x <- getChar
    menu updatedPlaceData

menuOption 7 placeData = do
    putStrLn "Enter search loctation"
    position <- getLine
    print ( returnClosestDryPlace (read position :: LatLng) placeData )
    putStrLn "\nPress any key to return to menu"
    x <- getChar
    menu placeData

menuOption 8 placeData = do
    clearScreen
    showMarkers placeData
    writeAt (0,80) "Press any key to close map"
    x <- getChar
    clearScreen
    menu placeData

menuOption 9 placeData = do
    let placesString = stringifyPlaces placeData
    writeFile filePath placesString

menuOption x placeData = do
    print "Invalid option returning to menu"
    menu placeData

inputNewPlace = do
    putStrLn "Enter new place name"
    placeName <- getLine
    putStrLn "\n"
    putStrLn "Enter location Eg: (50.1, -1.2)"
    position <- getLine
    putStrLn "\n"
    let parsedPosition = read position :: LatLng
    putStrLn "Enter rain data Eg: [1,1,1,1,1,1,1]"
    rainData <- getLine
    putStrLn "\n"
    let parsedRainData = read rainData :: [Int]
    return (Place placeName parsedPosition parsedRainData)