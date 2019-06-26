--Adventure Game Using Haskell Programming
--Game Name: Dragon Slayer


import Data.List
import Data.Char
--Variables that we will use during the Game
type Location = String
type Direction = String
type Thing = String
type Response = String
--Location map that represents [(Item at Location,Location)]
type LocationMap = [(Thing, Location)]
locations :: LocationMap
locations =  [
    ("princess", "dragon"),
    ("key", "cave entrance"),
    ("torch", "castle"),
    ("sword", "cupboard"),
    ("user", "valley"),
    ("dragon", "living")
    ]

--Path map that represents [((fromLocation,Direction),toLocation)]
type PathMap = [((Location, Direction), Location)]
paths :: PathMap
paths = [
    (("dragon", "d"), "cave"),
    (("cave", "u"), "dragon"),
    (("cave", "w"), "cave entrance"),
    (("cave entrance", "e"), "cave"),
    (("cave entrance", "s"), "valley"),
    (("valley", "s"), "castle"),
    (("valley", "n"), "cave entrance"),
    (("castle", "w"), "cage"),
    (("castle", "n"), "valley"),
    (("castle", "e"), "cupboard"),
    (("cage", "e"), "castle"),
    (("cupboard", "w"), "castle")
    ]

type World = (PathMap, LocationMap, Response)
world :: IO (PathMap, LocationMap, Response)
world = return (paths, locations, "")
--Entry Point of the Game
main :: IO (String)
main = do
    putStrLn "\n****** Welcome to the Dragon Slayer ******\n"
    putStrLn "\n In the Kingdom on Greendale a Dragon has Attacked by the order of the Evil Forces.\nIt has caused a great loss of Lives and Treasure\n The Kingdom rely on you the Might Knight ***Atnos***, to retrieve the princess and kill the Dragon.\nThis is your Jouney, may the gods be with you....\n"
    putStrLn instructions
    start_the_adventure $ return (paths, locations, "")
    return "Adios!"
--Instruction related to the game is store in the instructions
instructions =
    "---------- Instructions ----------\n In order to move ahead in your adventure you will have to make choices using the commands:\n" ++
    "The Commands are::\n" ++
    "main               -> Start the Quest.\n" ++
    "n  s  e  w  u  d   -> To go in the defined direction(North,South,East,West,Up,Down).\n" ++
    "pick 'object'      -> To pick up the object.\n" ++
    "drop 'object'      -> To drop the object.\n" ++
    "kill               -> To kill the enemy.\n" ++
    "look               -> To look around the vicinity.\n" ++
    "i                  -> Check Inventory(Things that you have in your Saddle Bag).\n" ++
    "quit               -> To Quit.\n----------------------------------"

start_the_adventure :: IO (World) -> IO (World)
start_the_adventure world = do
    (paths, locations, response) <- world
    putStrLn response
    putStrLn ""
    if game_over locations
        then return ([], [], "")
        else do
        --Each command will have a prefix 'dragonslayer>'
            putStr "dragonslayer> "
            command <- getLine
            --Check if the user enter quit
            if command == "quit"
                then return (paths, locations, "You are a Quitter, Exiting Now....")
                --In quit is not entered check the command and progress accordingly
                else  start_the_adventure $ return (user_choice command paths locations)

game_over :: LocationMap -> Bool
game_over locations =
    let user_location = get "user" locations
        princess_location = get "princess" locations
    in user_location == "dead" || (user_location == "valley" && princess_location == "having")
--Logic for valid movement of player
can_move :: Location -> Direction -> PathMap -> LocationMap -> Bool
can_move "valley" "n" _ locations= get "torch" locations == "having"
can_move "castle" "e" _ locations = get "key" locations == "having"
can_move from direction paths _ =
    elem (from, direction) keys
    where (keys, _) = unzip paths
--Logic for restricting user moverment
restrict_move :: Location -> Direction -> Response
restrict_move "valley" "n" = "The Cave is too Dark for you to see, you need a source of light to see, Look around.."
restrict_move "castle" "e" = "You cannot go there, the door is locked from the other side."
restrict_move _ _ = "You can't go that way."
--Movement logic
--Check againts the user entered command (n,s,e,w,u,d....) and process location or user action
move :: Location -> Direction -> PathMap -> Location
move from direction paths = get (from, direction) paths

user_choice :: String -> PathMap -> LocationMap -> World
user_choice "n" paths locations = go "n" paths locations
user_choice "e" paths locations = go "e" paths locations
user_choice "s" paths locations = go "s" paths locations
user_choice "w" paths locations = go "w" paths locations
user_choice "u" paths locations = go "u" paths locations
user_choice "d" paths locations = down_from_dragon "d" paths locations
user_choice "look" paths locations = look paths locations
user_choice "kill" paths locations = kill paths locations
user_choice "i" paths locations = (paths, locations, inventory locations)
user_choice "quit" paths locations = (paths, locations, "quit")
user_choice "dump" paths locations =
    (paths, locations, "paths = " ++ show paths ++ "\nlocations = " ++ show locations)
user_choice cmd paths locations = user_choice_2 cmd paths locations

user_choice_2 :: String -> PathMap -> LocationMap -> World
user_choice_2 cmd paths locations
    | isPrefixOf "pick " cmd =
          game_take (tail $ snd $ span isLetter cmd) paths locations
    | isPrefixOf "drop " cmd =
          game_drop (tail $ snd $ span isLetter cmd) paths locations
    | otherwise = (paths, locations, "Invalid Choice: " ++ cmd++", Please Use a Valid choice, the kingdom relies on your!")

game_take :: Thing -> PathMap -> LocationMap -> World
game_take thing paths locations =
    let here = get "user" locations
        there = get thing locations
    in if here == there
       then (paths, (put thing "having" locations), "Item is picked!.")
       else if there == "having"
            then (paths, locations, "You are already having it.")
            else (paths, locations, "I don't see it here.")

game_drop :: Thing -> PathMap -> LocationMap -> World
game_drop thing paths locations = --(paths, locations, "filler")
    let here = get "user" locations
        there = get thing locations
    in if there == "having"
        then (paths, (put thing here locations), "Item Dropped!")
        else (paths, locations, "You aren't having it.")
--Movement of user
go :: String -> PathMap -> LocationMap -> World
go direction paths locations = do
    let user_location = get "user" locations
    if can_move user_location direction paths locations
        then do
            let new_location = move user_location direction paths
            let new_locations = put "user" new_location locations
            let response = describe new_location new_locations
            (paths, new_locations, response)
        else (paths, locations, restrict_move user_location direction)

down_from_dragon :: String -> PathMap -> LocationMap -> World
down_from_dragon direction paths locations =
    if get "user" locations == "dragon" &&
       get "dragon" locations == "living" &&
       get "princess" locations == "having"
           then (paths, put "user" "dead" locations, description "cave3")
           else go direction paths locations

look :: PathMap -> LocationMap -> World
look paths locations =
    if things == []
        then (paths, locations, describe user_location locations)
        else (paths, locations, describe user_location locations ++ "\n\n" ++ things)
    where user_location = get "user" locations
          things = items_here locations
--kill functionality
kill :: PathMap -> LocationMap -> World
kill paths locations =
    case get "user" locations of
        "cage" -> (paths,
                   put "user" "dead" locations,
                   "Oh, Wrong choice! You have just been eaten by a tiger.")
        "cave" -> (paths, locations,
                   "The dragon's skin too tough to be punctured.")
        "dragon" ->
            if get "sword" locations == "having"
                then (paths,
                      put "dragon" "dead" locations,
                      "You attack repeatedly at the dragon's head.  It screams in pain,\n" ++
                     "blood is everywhere.\n" ++
                     "The Dragon has been killed.")
                else (paths,
                      locations,
                      "Your hands do not stand a chance in front of the dragon.\n" ++
                      "You need something else to kill it.")
        _ -> (paths, locations, "There is nothing harmful here")
--i(Inventory) functionality
inventory :: LocationMap -> Response
inventory locations =
    let my_stuff = [thing | (thing, "having") <- locations]
    in if my_stuff == []
        then "You do not have anything yet."
        else intercalate ", " my_stuff

items_here :: LocationMap -> Response
items_here locations =
    let here = get "user" locations
        things = ["There is a " ++ thing ++ " here." |
                  (thing, place) <- locations, place == here, thing /= "user"]
    in intercalate "\n" things

-- "get" finds the value of a key in a (key, value) list
get :: Eq a => a -> [(a, String)] -> String
get value list = case lookup value list of
                     Just result -> result
                     Nothing -> "Not found."

put :: Eq t => t -> t1 -> [(t, t1)] -> [(t, t1)]
put key value list =
    let without = filter (\(x, y) -> x /= key) list
    in (key, value) : without

describe :: Location -> LocationMap -> String
describe new_location locations =
    let here = get "user" locations
        dragon_status = get "dragon" locations
        princess_location = get "princess" locations
    in describe_helper here dragon_status princess_location  locations

describe_helper :: Location -> String -> String -> LocationMap -> String
describe_helper "valley" "dead" "having" locations = description "valley_second_des"
describe_helper "cave" "living" "having" locations = description "cave3"
describe_helper "cave" "dead" _ locations = description "cave2"
describe_helper "dragon" "dead" _ locations = description "dragon_second_des"
describe_helper here _ _ locations = description here
--Description of the various location in the game
description :: Location -> String
description "valley" =
    "You stand in a valley, the night is dark and there is silence every where..\nTo Your north lies the mouth" ++
    "of a cave; to the south is a small castle.  Your\n" ++
    "assignment, should you decide to accept it, is to\n" ++
    "recover the famed Royal Kindom's princess and return it to\n" ++
    "this Valley."

description "valley_second_des" = "Congratulations!!  You have recovered the princess and won the game."

description "castle" =
    "You are in a small castle.  The exit is to the north.\n" ++
    "There is a secured door to the west ,  it is\n" ++
    "open.  There is a smaller door to the east."

description "cage" =
    "You are in a tiger's den!  The tiger has a angry and\n" ++
    "hungry look. Save yourself and get out of here!"

description "cupboard" =
    "This is nothing but an old storage cupboard."

description "cave entrance" =
    "You are in the mouth of a dark cave.  The exit is to\n" ++
    "the south; there is a large, dark, round alley to\n" ++
    "the east of the cave."

description "cave" =
    "A giant dragon is here, screaming and breathing fire!  It is as big as the Royal Castle, \n" ++
    "shiny skin and horns, is in front of you!\n" ++
    "Is better to quitely leave the place , if you want to see another day...."

description "cave2" =
    "Gosh!  There is a giant dragon here, Eating Flesh."

description "cave3" =
     "The dragon sights you with the princess and attacks on you!!!\n" ++
     "\n The dragon opens its mouth charges at you will his fire breadth\n    ...You are Dead...."

description "dragon" =
    "You are on top of a giant dragon, standing in a \n" ++
    "pile of Human bones.  It stinks here."

description "dragon_second_des" =
    "Oh Crap!  You're on the giant dead dragon's head!"

description someplace = someplace ++ ", Nothing is Visible."
