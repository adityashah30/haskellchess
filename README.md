haskellchess
============

Chess Engine in Haskell

Steps to run Chess Engine
* Install xboard
* Make compile.sh executable
* Create native executable by either executing compile.sh on *nix systems or by executing compile.bat on Windows.
* Open xboard and load executable by "load new 1st engine" under Options and choose the directory where executable is saved
* In "Engine command" field type "./chess" to load the executable
* Now start the new game and play

Notes
-----

1. In case that you are unable to load this engine in xboard, do the following
	* Close all instances of xboard
	* Edit ~/.xboardrc
	* In the section named "firstChessProgramNames", add the following line

		"<Name of the chess engine, eg HChess" -fcp "./chess" -fd "<Enter full dirpath where chess is stored>" -fUCI

	* Also add the <Name of the chess engine> to the section named "recentEngineList" for easy access.
	* Save and exit.
	* In xboard now you can find the name of the chess engine in the recent engine list as well as in the "Load New 1st engine" dialog box

2. In order to compile the source code, you need to have game-tree package installed.
	* In order to install the package, execute the following command

		sudo cabal install game-tree
