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

	a. Close all instances of xboard

	b. Edit ~/.xboardrc

	c. In the section named "firstChessProgramNames", add the following line

		"<Name of the chess engine, eg HChess" -fcp "./chess" -fd "<Enter full dirpath where chess is stored>" -fUCI

	d. Also add the <Name of the chess engine> to the section named "recentEngineList" for easy access.

	e. Save and exit.

	f. In xboard now you can find the name of the chess engine in the recent engine list as well as in the "Load New 1st engine" dialog box

2. In order to compile the source code, you need to have game-tree package installed.

	a. In order to install the package, execute the following command

		sudo cabal install game-tree

3. In order to load a position in xboard, do the following

	a. Load the FEN file using File -> Load Position.

	b. Make the move.

	c. Now load the Engine from either Engine -> Load First Engine or from the recentEngineList under Engine.

	d. Now if the engine is supposed to play Black, then choose Mode -> Machine Black else choose Mode -> Machine White.
