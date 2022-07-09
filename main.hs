{-# LANGUAGE CPP, MultiWayIf #-}

import System.Random ( randomRIO )
import System.Process ( callCommand )

#if defined(mingw32_HOST_OS)
#define CLEAN_SCREEN_COMMAND "cls"
#else
#define CLEAN_SCREEN_COMMAND "clear"
#endif

type Cell = String
type Table = [[Cell]]
type Command = Char
type Times = Int

data Direction = MoveUp | MoveDown | MoveLeft | MoveRight | StayMiddle deriving (Enum, Eq)
data Location = Location {x::Int, y::Int}

getDirectionFrom :: Command -> Direction
getDirectionFrom input
    | input == 'W' || input == 'w' = MoveUp
    | input == 'S' || input == 's' = MoveDown
    | input == 'A' || input == 'a' = MoveLeft
    | input == 'D' || input == 'd' = MoveRight
    | otherwise                    = StayMiddle

move :: (Location, Table) -> Direction -> (Location, Table)
move (location@Location {x = x0, y = y0}, table) direction = (newLocation, newTable)
    where   newLocation = case direction of
                MoveUp      -> Location x0 (if y0 > 2 then 3 else y0 + 1)
                MoveDown    -> Location x0 (if y0 < 1 then 0 else y0 - 1)
                MoveLeft    -> Location (if x0 > 2 then 3 else x0 + 1) y0
                MoveRight   -> Location (if x0 < 1 then 0 else x0 - 1) y0
                StayMiddle  -> location
            newTable = if direction == StayMiddle then table else map (map getCell) table
                where   getCell currentCell
                            | currentCell == originCell = targetCell
                            | currentCell == targetCell = originCell
                            | otherwise                 = currentCell
                        targetCell = table !! y newLocation !! x newLocation
                        originCell = table !! y location    !! x location


showTable :: Table -> IO ()
showTable table = do
    cleanScreen
    putStrLn $ "\n\n" ++ stringTable ++ "\n\n"
        where   stringTable = joinWith "\n\n\n\n" . map (joinWith "\t") $ table
                joinWith sep = foldl1 (\x y -> x ++ sep ++ y)

getActionLoop :: (Location, Table) -> IO ()
getActionLoop (location, table) = do
    input <- getChar
    let direction = getDirectionFrom input
    let (newLocation, newTable) = move (location, table) direction
    showTable newTable
    if newTable == innitialTable then return () else getActionLoop (newLocation, newTable)

cleanScreen :: IO ()
cleanScreen = callCommand CLEAN_SCREEN_COMMAND

getRandomActions :: Times -> IO [Direction]
getRandomActions times = sequence [toEnum <$> randomRIO (fromEnum MoveUp, fromEnum MoveRight) | _ <- [1..times]]

innitialLocation = Location {x=3, y=3}
innitialTable = [ ["  1",  "  2",  "  3",  "  4"]
                , ["  5",  "  6",  "  7",  "  8"]
                , ["  9",  " 10",  " 11",  " 12"]
                , [" 13",  " 14",  " 15",  "   "] ]

gameStart :: IO ()
gameStart = do
    cleanScreen
    (currentLocation, currentTable) <- foldl move (innitialLocation, innitialTable) <$> getRandomActions 369
    showTable currentTable
    getActionLoop (currentLocation, currentTable)
    askIfContinue
        where   askIfContinue = do
                    _ <- getLine
                    putStrLn "Well Done!\n\nContinue? ([Y] Yes, default; [N] No.)"
                    answer <- getChar
                    if  | answer `elem` "Yy\n"  ->  gameStart
                        | answer `elem` "NnQq"  ->  putStrLn "Bye!"
                        | otherwise             ->  askIfContinue

main :: IO ()
main = gameStart
