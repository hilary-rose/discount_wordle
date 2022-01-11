# discount_wordle
Knock-off version of Wordle
I made this game because I love playing the real Wordle, but I wanted to play more than once per day.

This version is very bare-bones...the list of guessable words has a few odd words in it you'd never guess, and the only guess validation is that your guesses must be 5 characters long. So you could use up a turn guessing "123!@#" and the game would continue.

To play, clone the repo and open the .Rproj in RStudio. Open the .R script, click the "source" button, and the game will start! Enter your guesses in the console, and the gameboard will display in the plot window.

The object of the game is to guess the random 5-letter word in 6 guesses or fewer. Guess any 5-letter word for your first guess, and then the gameboard will display feedback. Any letter in your guess that is in the correct position in the target word will display in green. Any letter in your guess that is in the target word, but not in the same position, will display in yellow. Any letter that is not in the target word will display in grey. Use this information to solve the puzzle!
