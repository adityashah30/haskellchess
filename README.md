haskellchess
============

Chess Engine in Haskell

Steps to run Chess Engine
* Install xboard
* Make compile.sh executable
* Create native executable by either executing compile.sh on *nix systems or by the command "ghc -o chess -O2 -fforce-recomp Main" on windows
* Open xboard and load executable by "load new 1st engine" under Options and choose the directory where executable is saved
* In "Engine command" field type "./chess" to load the executable
* Now start the new game and play
