# Small text adventure in Haskell

The game is small and text-based, and uses numbers as input for player action. There are several locations, characters, and quests to engage, with some dialogue. It is continuous until the player exits, and gives some freedom in where the player can go, do, and who to interact with. Note that characters used are from other video games and are not my creation.

The game takes advantage of several aspects of functional programming, namely recursiveness.

# Setting up and playing

Game data is included. Simply download Haskell's GHCi, run the file, and type/enter 'game' to start.

At each game loop, the player has to input a number that corresponds to a location or character. Multiple numbers can be entered for interactions with multiple characters.

Below is an example.
