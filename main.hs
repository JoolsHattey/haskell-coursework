--
-- MATHFUN
-- Template for the Haskell assignment program (replace this comment)
-- Add your student number
--


import Data.List

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

getPlaceNames :: [String]
getPlaceNames = map locationName testData


getAverageRainfall :: String -> Float
getAverageRainfall searchName = averageList ( rainData ( getPlaceByName searchName ) )

getPlaceByName :: String -> Place
getPlaceByName searchName = head [ x | x <- testData, locationName x == searchName ]

averageList :: [Int] -> Float
averageList input = fromIntegral (sum input) / fromIntegral (length input)


placesToString :: [Place] -> String
placesToString placesList = intercalate "" (map placeToString placesList)

placeToString :: Place -> String
placeToString placeData = formatLocationName ( locationName placeData ) ++ (rainDataToString ( rainData placeData ) ) ++ "\n"

formatLocationName :: String -> String
formatLocationName location = location ++ intercalate "" ( replicate (13-(length location)) " ")

rainDataToString :: [Int] -> String
rainDataToString rainData = intercalate " " ( map show rainData )


outputDryPlaces :: Int -> [String]
outputDryPlaces numDays = map locationName ( getDryPlaces numDays )

getDryPlaces :: Int -> [Place]
getDryPlaces numDays = [ x | x <- testData, (rainData x) !! (numDays-1) == 0]


--
--  Demo
--

demo :: Int -> IO ()
demo 1 = print getPlaceNames
demo 2 = print ( getAverageRainfall "Cardiff" )
demo 3 = putStrLn ( placesToString testData )
demo 4 = print ( outputDryPlaces 2 )
-- demo 5 = -- update the data with most recent rainfall 
--          --[0,8,0,0,5,0,0,3,4,2,0,8,0,0] (and remove oldest rainfall figures)
-- demo 6 = -- replace "Plymouth" with "Portsmouth" which has 
--          -- location 50.8 (N), -1.1 (E) and rainfall 0, 0, 3, 2, 5, 2, 1
-- demo 7 = -- display the name of the place closest to 50.9 (N), -1.3 (E) 
--          -- that was dry yesterday
-- demo 8 = -- display the rainfall map


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



--
-- Your user interface (and loading/saving) code goes here
--
 
