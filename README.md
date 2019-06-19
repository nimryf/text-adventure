# Small text adventure in Haskell

The game uses numbers as input for player action, and has several locations, characters, and quests to engage, with some dialogue. It is continuous until the player exits, and gives some freedom in where the player can go, do, and who to interact with. Note that characters used are from other franchises and are not my creation.

It takes advantage of several aspects of functional programming, namely recursiveness.

# Setting up and playing

Game data is included. Simply download [Haskell's GHCi](https://www.haskell.org/downloads/), run the file, and type/enter _game_ to start.

At each game loop, the player has to input a number that corresponds to a location or character. Multiple numbers can be entered for interactions with multiple characters.

Below is an example.

![Example](https://github.com/nimryf/text-adventure/blob/master/example.jpg)
