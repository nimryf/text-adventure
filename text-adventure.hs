

------------------------- PART 0: Useful functions

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

minus :: Ord a => [a] -> [a] -> [a]
minus xs [] = xs
minus [] ys = []
minus (x:xs) (y:ys)
    | x <  y    = x : minus    xs (y:ys)
    | x == y    =     minus    xs    ys
    | otherwise =     minus (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2

type Node = Int
type Map  = [(Node,Node)]

bothWays :: Node -> Map -> [Node]
bothWays _ [] = []
bothWays n ((x,y):xs)
    | n == x    = y : bothWays n xs
    | n == y    = x : bothWays n xs
    | otherwise =     bothWays n xs

type Location  = String
type Character = String

type Party = [Character]


------------------------- PART 1: Events

data Game  = Game Node Party [Party] | Won
  deriving (Eq,Show)

type Event = Game -> Game

start :: Game
start =  Game 6 [] characters

end :: Game
end = Won


applyAt :: Int -> (a -> a) -> [a] -> [a]
applyAt i f [] = []
applyAt i f (x:xs) 
    | i > length xs = error "Index 'i' is out of bounds for xs."
    | i == 0        = f x : applyAt (i-1) f xs
    | otherwise     = x : applyAt (i-1) f xs

updateAt :: Node -> Party -> Party -> Event
updateAt m xs ys Won = Won
updateAt m xs ys (Game n p ps) = Game n p (applyAt m (merge ys) (applyAt m (`minus` xs) ps ))

update :: Party -> Party -> Party -> Event
update xs ys zs Won = Won
update xs ys zs (Game n p ps) = Game n (merge ys (minus p xs)) (applyAt n (merge zs) (applyAt n (`minus` xs) ps ))


------------------------- PART 2: Dialogues

data Dialogue = End     String
              | Choice  String  [( String , Dialogue )]
              | Action  String Event

exitWords = ["X","x","Q","q","Exit","exit","Quit","quit"]

enumerate :: Int -> [String] -> String
enumerate n xs = unlines [ "  " ++ show i ++ ". " ++ x | (i,x) <- zip [n..] xs ]

dialogue :: Game -> Dialogue -> IO Game
dialogue g (End endString) = do
    putStrLn endString
    return(g)
dialogue g (Action actionString actionEvent) = do
    putStrLn actionString
    return(actionEvent g)
dialogue g (Choice choiceString choiceDialogue) = do
    let choices = map fst choiceDialogue
    putStrLn choiceString
    putStr (enumerate 1 choices)
    str <- getLine
    if str `elem` exitWords
    then do
        return(g)
    else do
        let x = read str :: Int  
        dialogue (g) ((map snd choiceDialogue) !! (x-1))
   
findDialogue :: Party -> Dialogue
findDialogue p 
    | length [dialog | (party, dialog) <- dialogues, p == party] == 0 = End "There is nothing we can do."
    | otherwise = [dialog | (party, dialog) <- dialogues, p == party] !! 0

------------------------- PART 3: The game loop

loop :: Game -> IO ()
loop Won = do
    return()
loop (Game n p ps) = do
    putStrLn ("You are in " ++ (locations !! n))
    let ns = bothWays n theMap
    let i = length ns + 1
    
    let xs = map (locations !!) ns
    putStr ("You can travel to\n" ++ enumerate 1 xs)
    
    let ys = p
    putStr ("With you are\n" ++ enumerate i ys)
    
    let bs = ps !! n
    let otheri = length ns + length ys + 1
    putStr ("You can see\n" ++ enumerate otheri bs)
    
    putStr "What will you do?\n"
    str <- getLine
    if str `elem` exitWords
    then do
        return()
    else do
        let is = map (read :: String -> Int) (words(str))
        if ( length(is) == 1) && ((is!!0) `elem` [1..(length ns)])
        then do
            let m = (ns !! (read str - 1))
            loop (Game m p ps)
        else do
            let js = [tempc | (tempx, tempc) <- (merge (zip [i..] ys) (zip [otheri..] bs)), any (tempx==) is]
            g <- dialogue (Game n p ps) (findDialogue js)
            loop (g)
            
            
game :: IO ()
game = loop start

------------------------- Game data

characters :: [Party]
characters =
  [ ["Duke"]
  , ["Portal Gun"]
  , ["Priest"]
  , ["Lee"]
  , ["Chell","Cortana","Mario","Master Chief"]
  , ["Team Rocket"]
  , ["Peach","Rochelle"]
  ]

locations :: [Location]
locations =
  [ "You are not supposed to be here" -- 0
  , "Aperture Science" -- 1
  , "Church of Halo"   -- 2
  , "Macon"            -- 3
  , "Nintendo Land"    -- 4
  , "Pallet Town"      -- 5
  , "Princess Castle"  -- 6
  ]

theMap :: Map
theMap = [(1,5), (2,4), (2,6), (3,5), (4,5), (4,6)]


dialogues :: [(Party,Dialogue)]
dialogues =
 [ (["Mario"] , Choice "I need to save the Princess."
     [("Sure." ,          Action "Let's go." (update ["Mario"] ["Mario"] []))
     ,("Not right now." , Action "Ok."       (update ["Mario"] [] ["Mario"]))
     ])
 , (["Mario","Peach"] , Choice "Save me, Mario!"
    [("Sure." , Action "Thank you for bringing me my hero. Now I can conveniently leave this hat behind." (update ["Mario","Peach"] [] ["Baseball Cap"]))
    ,("Not right now." , End "Mario, pls.")])
 , (["Peach"] , End "That's *Princess* Peach to you, please. And where's my Mario?")
 , (["Master Chief"] , Choice "I want to marry Cortana. Can you escort us to the Church of Halo?"
     [("Sure." ,          Action "Let's go." (update ["Master Chief"] ["Master Chief"] []))
     ,("Not right now." , Action "Ok."       (update ["Master Chief"] [] ["Master Chief"]))
     ])
 , (["Cortana"] , Choice "I must go with Master Chief."
     [("Sure." ,          Action "Let's go." (update ["Cortana"] ["Cortana"] []))
     ,("Not right now." , Action "Ok."       (update ["Cortana"] [] ["Cortana"]))
     ])
 , (["Master Chief","Priest"] , End "I can't marry you without your bride-to-be.")
 , (["Cortana","Priest"] , End "I can't marry you without your husband-to-be.")
 , (["Priest"] , Choice "Welcome, my child. Have you accepted Master Chief as your savior?"
     [("Hail Master Chief (Blessed Be His Name)" , End "")])
 , (["Cortana","Master Chief","Priest"] , Choice "Do you, Master Chief, accept Cortana to be your beloved bride?" 
      [("I don't", End "The Wedding is cancelled"),
       ("I do", Choice "And do you, Cortana, take Master Chief to be your beloved Husband?" 
        [("I don't", End "The Wedding is cancelled"),
         ("I do", Action "What a beautiful wedding said the bridesmaid to the waiter. But what a shame, that there's some child lurking nearby." (update ["Cortana","Master Chief","Priest"] [] ["Clementine (hiding)"]) )
   ])])
 , (["Baseball Cap"] , Choice "It's a bit grubby, shall I take it?"
     [("Sure." ,          Action "Let's go." (update ["Baseball Cap"] ["Baseball Cap"] []))
     ,("Not right now." , Action "Ok."       (update ["Baseball Cap"] [] ["Baseball Cap"]))
     ])
 , (["Clementine (hiding)"] , End "I'm scared. Where are my parents?")
 , (["Baseball Cap", "Clementine (hiding)"] , Choice "Give the girl the hat?"
    [("Sure." , Action "I feel safe." (update ["Baseball Cap","Clementine (hiding)"] ["Clementine"] []))
    ,("Not right now." , End "")
    ])
 , (["Duke"] , End "Time to k*** a** and chew bubble gum. And I'm all outta gum.")
 , (["Clementine"] , Choice "Will you help me find my parents?"
      [("What do they look like?", Choice "My father's name is Lee" 
        [("I asked what do they look like!", End "Sorry")
        ,("I know him, Let's go!", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
      ])
      ,("Do you know your address?", Choice "I can't remember, I think it rhymes with Bacon" 
        [("How do you not know your own address?", End "Sorry!"),
         ("Are you thinking of Macon?", Choice "Yes! That's it! Do you know where it is?"
          [("Sure, I can take you there", Action "Yay!" (update ["Clementine"] ["Clementine"] []))
          ,("I don't know how to get there", End "Okay then")
          ])
        ,("Are you hungry?", Choice "Yes! Do you have any chocolate?" 
          [("I don't", End "Okay")
          ,("I can go find some", Action "Thanks, I'll stay here in case my parents come back" (update ["Clementine"] [] ["Clementine"]))
   ])])])
 , (["Clementine","Lee"] , Choice "GIVE ME BACK CLEMENTINE!"
     [("Sure...", Action "" (update ["Clementine","Lee"] ["Zombie Lee"] []))
     ])
 , (["Lee"] , End "Clem? Clem, where are you?!")
 , (["Zombie Lee"] , Choice "Uuurrurrhgghghhghgg."
     [("This way." ,  Action "Urg" (update ["Zombie Lee"] ["Zombie Lee"] []))
     ,("Not today." , Action "Hhuuuurgh" (update ["Zombie Lee"] [] ["Zombie Lee"]))
     ])
 , (["Rochelle"] , End "Girl, you should pray there aren't no Zombies around.")
 , (["Rochelle", "Zombie Lee"] , Action "What?! A zombie? You've left me for dead!" (update ["Rochelle","Zombie Lee"] [] ["Pikachu"]))
 , (["Chell"] , Choice "I've just got a volunteering position at Aperture Science. Can you help me find it? I'm not good with directions."
     [("This way." ,  Action "" (update ["Chell"] ["Chell"] []))
     ,("Not today." , Action "" (update ["Chell"] [] ["Chell"]))
    ])
 , (["Chell","Portal Gun"] , Action "This is your fault. It didn't have to be like this. I'm not kidding, now! Turn back, or I will kill you! I'm going to kill you, and all the cake is gone! You don't even care, do you? This is your last chance! ." (update ["Chell","Portal Gun"] [] [] . updateAt 5 ["Team Rocket"] ["Ash"]))
 , (["Team Rocket"] , End "Oh, prepare for trouble, that's what they should do. And make it double, we're grabbing Pikachu.") 
 , (["Pikachu"] , Choice "Pika-Pika"
     [("*throw pokeball*"  , Action "" (update ["Pikachu"] ["Pikachu"] []))
     ,("Nope." ,             Action "" (update ["Pikachu"] [] ["Pikachu"]))
     ])
 , (["Ash", "Pikachu"] , Action "You win." (\_ -> Won))
 , (["Pikachu","Team Rocket"] , End "Hey, look at this! Get a load! Let's grab- ALL GLORY TO THE HYPNOTOAD")
 , (["Portal Gun"] , End "I am an inanimate object. What did you expect?")
 ]


