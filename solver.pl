% Written by Vishal Egbert, 835863
% A Fill-In Puzzle solver written in Prolog. 

% The program first replaces the underscores in the matrix with Prolog's free 
% variables. It then breaks up the matrix into horizontal and verticle "slots"
% which are then stored in a list. The list of slots is checked against the word
% list to ensure that any already filled in slots are accounted for. The program
% then builds a list of tuples that contain a word and all the slots the word 
% can potentially fit into. From there the program begins filling the puzzle by 
% always choosing the word that has the least potential slots it can fit into. 
% Once it has chosen a word, it will unify the word with the slot that is most
% populated by ground variables. Once unified, the word will be removed from the 
% word list. The program then iterates through all the tuples and from each 
% tuple, it removes any slots that no longer agree with the corresponding word.
% Once the slots are cleaned, the process continues with the next word with the 
% least possible slots. 

% Reads the puzzle and word list, solves the puzzle and saves the solution
main(PuzzleFile, WordlistFile, SolutionFile) :-
	read_file(PuzzleFile, Puzzle),
	read_file(WordlistFile, Wordlist),
	valid_puzzle(Puzzle),
	solve_puzzle(Puzzle, Wordlist, Solved),
	print_puzzle(SolutionFile, Solved), !.

% Opens and reads arbitrary files
read_file(Filename, Content) :-
	open(Filename, read, Stream),
	read_lines(Stream, Content),
	close(Stream).

% Reads every line from a stream into memory
read_lines(Stream, Content) :-
	read_line(Stream, Line, Last),
	(   Last = true
	->  (   Line = []
	    ->  Content = []
	    ;   Content = [Line]
	    )
	;  Content = [Line|Content1],
	    read_lines(Stream, Content1)
	).

% Reads a line into a list, character by character, stops on a \n or EOF 
read_line(Stream, Line, Last) :-
	get_char(Stream, Char),
	(   Char = end_of_file
	->  Line = [],
	    Last = true
	; Char = '\n'
	->  Line = [],
	    Last = false
	;   Line = [Char|Line1],
	    read_line(Stream, Line1, Last)
	).

% Saves the puzzle's solution to a file
print_puzzle(SolutionFile, Puzzle) :-
	open(SolutionFile, write, Stream),
	maplist(print_row(Stream), Puzzle),
	close(Stream).

% Maps a row in the matrix to a stream to be written to a file
print_row(Stream, Row) :-
	maplist(put_puzzle_char(Stream), Row),
	nl(Stream).

% Replace free variables with an underscore
put_puzzle_char(Stream, Char) :-
	(   var(Char)
	->  put_char(Stream, '_')
	;   put_char(Stream, Char)
	).

% Ensures that all rows in the puzzle are the same length
valid_puzzle([]).
valid_puzzle([Row|Rows]) :-
	maplist(same_length(Row), Rows).

% Process the puzzle, stores unifiable slots with corresponding words in tuples,
% attempts to solve puzzle based on the tuples
solve_puzzle(Puzzle0, WordList, Puzzle) :-
	populateFreeVarsMatrix(Puzzle0, Puzzle),
	getSlotsFromMatrix(Puzzle, Slots),
	removeWordsInSlots(WordList, Slots, CleanWordList),
	getAllAvailableSlots(CleanWordList, Slots, WordsWithSlots),
	fillInPuzzle(WordsWithSlots).

% Breaks a matrix up into rows and processes the underscores in each row
populateFreeVarsMatrix([Row], [Result]) :-
    populateFreeVarsSingleList(Row, Result), !.
populateFreeVarsMatrix([Row|Rows], Result) :-
    populateFreeVarsMatrix(Rows, Result1),
    populateFreeVarsSingleList(Row, SingleList),
    append([SingleList], Result1, Result).

% Takes a row and replaces any underscores with a free variable
populateFreeVarsSingleList(Row, FreeVarList) :-
    maplist(replaceWithFreeVar, Row, FreeVarList).

% Replaces any underscores with a free variable
replaceWithFreeVar(Char, FreeVar) :-
    (   Char = '_' ->  
    	FreeVar = _X
	;   FreeVar = Char
	).

% Breaks up a matrix into a list of occupiable slots by getting the slots in 
% every horizontal row and then transposing the matrix to get the slots in every
% verticle column 
getSlotsFromMatrix(Matrix, Slots) :-
    ensure_loaded(library(clpfd)),
    getHorizontalSlotsFromMatrix(Matrix, Slots1),
    transpose(Matrix, TransposedMatrix),
    getHorizontalSlotsFromMatrix(TransposedMatrix, Slots2),
    append(Slots1, Slots2, Slots), !.

% Gets the slots from all horizontal rows in the matrix and add them to a list
getHorizontalSlotsFromMatrix([Row], Slots) :-
    getSlotsFromRow(Row, Slots).
getHorizontalSlotsFromMatrix([Row|Rows], Slots) :-
    getSlotsFromRow(Row, Slots1),
    getHorizontalSlotsFromMatrix(Rows, Slots2),
    append(Slots1, Slots2, Slots), !.

% Takes a row from the puzzle matrix, skips over any initial #'s. When it 
% encounters a free variable or character, it reads the input until the next
% block and calls the predicate again. It terminates when there are no more 
% characters left to read. 
getSlotsFromRow([], _Slots).
getSlotsFromRow([Char|Row], Slots) :-
    (   Char == '#' ->  
    	getSlotsFromRow(Row, Slots)
    ;	readUntilBlock([Char|Row], Result, Rest),
		getSlotsFromRow(Rest, Slots2),
		append(Slots2, [Result], Slots)
	).

% Reads until it encounters a #. When it encounters a hash tag, it separates the
% non-# characters it saw before the # from the characters after the #. 
readUntilBlock(Row, Result, Rest) :-
	readUntilBlock(Row, [], Result, Rest), !.
readUntilBlock([Char], Slot, ResultSlot, []) :-
	(   Char == '#' ->  
		ResultSlot = Slot
	;   append(Slot, [Char], NewSlot),
		ResultSlot = NewSlot
	).
readUntilBlock([Char|List], Slot, ResultSlot, Rest) :-
	(   Char == '#' ->  
		Rest = List,
		ResultSlot = Slot
	;   append(Slot, [Char], NewSlot),
		readUntilBlock(List, NewSlot, ResultSlot, Rest)
	).

% If a word exists in a slot, remove it from the word list
removeWordsInSlots([Word], Slots, Result) :-
	(\+ findWordInSlots(Word, Slots) -> 
		Result = [Word]
	;	Result = []
	).
removeWordsInSlots([Word|Words], Slots, Result) :-
	(\+ findWordInSlots(Word, Slots) ->
		removeWordsInSlots(Words, Slots, Result1),
		append([Word], Result1, Result)
	;	removeWordsInSlots(Words, Slots, Result)
	).

% Checks if the word occurs anywhere in a list of slots
findWordInSlots(Word, [Slot]) :-
	Word == Slot.
findWordInSlots(Word, [Slot|Slots]) :-
	(	Word == Slot ->
		true
	;	findWordInSlots(Word, Slots)
	).

% From a list of slots, for each word, create a tuple with the word and a list
% of slots that it can potentially unify with 
getAllAvailableSlots(Words, Slots, Result) :-
	getAllAvailableSlots(Words, Slots, [], Result), !.
getAllAvailableSlots([Word], Slots, Acc, Result) :-
	getSlots(Word, Slots, NumSlots),
	append(Acc, [(NumSlots,Word)], Result), !.
getAllAvailableSlots([Word|Words], Slots, Acc, Result) :-
	getSlots(Word, Slots, NumSlots),
	append(Acc, [(NumSlots,Word)], Acc1),
	getAllAvailableSlots(Words, Slots, Acc1, Result), !.

% From a word and a lists of all slots, get the list of slots that the word can 
% potentially unify with 
getSlots(_Word, [], []).
getSlots(Word, [Slot], Fits) :-
	(	fitSlot(Word, Slot) ->
		Fits = [Slot]
	; 	Fits = []
	).
getSlots(Word, [Slot|Slots], Fits) :-
	getSlots(Word, Slots, Fits1),
	(	fitSlot(Word, Slot) ->
		append([Slot], Fits1, Fits)
	;	append([], Fits1, Fits)
	), !.

% Checks if the slot is the same length as the word and if it can unify with it
fitSlot(Word, Slot) :-
	same_length(Word, Slot),
	unifiesWithSlot(Word, Slot).

% Checks if the slot contains characters that do not allign with the word
unifiesWithSlot([Char], [Space]) :-
	unifiesWithChar(Char, Space), !.
unifiesWithSlot([Char|Word], [Space|Slot]) :-
	unifiesWithChar(Char, Space), 
	unifiesWithSlot(Word, Slot), !.

% Checks if the space in a slot is a free variable, if it is not, check if they 
% are the same characters
unifiesWithChar(Char, Space) :-
	(nonvar(Space) -> 
	Space = Char;
	var(Space)).

% From the list of words, get the word with the least number of slots, unify it
% with a slot, remove it from the list, clean the other slots and continue with 
% the next set of words
fillInPuzzle([]).
fillInPuzzle(WordsWithSlots) :-
	getMinNumSlots(WordsWithSlots, MinSlot),
	checkUnify(MinSlot),
	removeWordFromList(MinSlot, WordsWithSlots, NewWordsWithSlots),
	cleanSlots(NewWordsWithSlots, CleanedWordsWithSlots),
	fillInPuzzle(CleanedWordsWithSlots).

% From the list of slots-word tuples, return the tuples containing the least
% number of slots
getMinNumSlots([(Slots, Word)], (Slots, Word)).
getMinNumSlots([(Slots, Word)|Words], (ResultSlots, ResultWord)) :-
	length(Slots, LengthSlots),
	getMinNumSlots(Words, (ResultSlots2, ResultWord2)),
	length(ResultSlots2, LengthResultSlots2),
	( LengthSlots > LengthResultSlots2 ->
		ResultSlots = ResultSlots2,
		ResultWord = ResultWord2
	;	ResultSlots = Slots,
		ResultWord = Word
	), !.

% Unifies the word with the slot that has the least number of free variables, 
% ie: the slot with the most ground variables. Leaves a choice point for 
% the member predicate to continue with the next Slot if we run into a failure
checkUnify((Slots, Word)) :-
	length(Slots, LengthSlots),
	LengthSlots > 0,
	sortSlots(Slots, SortedSlots),
	member(Slot, SortedSlots),
	Word = Slot.

% Attach the number of free variables to each slot, sort the slots by the number
% of free variables finally return the list of slots without the number of 
% free variables 
sortSlots(Slots, SortedSlots) :-
	maplist(addNumFreeVar, Slots, KeyedSlots),
	keysort(KeyedSlots, KeyedSortedSlots),
	maplist(removeKey, KeyedSortedSlots, SortedSlots), !.

% Get the number of free variables and store it as a key for each slot
addNumFreeVar(Slot, Result) :-
	getNumFreeVar(Slot, Num),
	Result = Num-Slot.

% Counts the number of free variables in a slot
getNumFreeVar([Space], Num) :-
	(var(Space) -> 
		Num = 1
	;	Num = 0
	).
getNumFreeVar([Space|Slot], Num) :-
	(var(Space) -> 
		getNumFreeVar(Slot, Num1),
		Num is Num1 + 1
	;	getNumFreeVar(Slot, Num)
	), !.

% Removes the key value from a slot
removeKey(_Key-Slot, Slot).

% Remove a unified slots-word tuple from the list of tuples 
removeWordFromList((_Slots, RemovedWord), [(Slots2, Word)], CleanedList) :-
	(RemovedWord == Word ->
		CleanedList = []
	;	CleanedList = [(Slots2, Word)]
	), !.
removeWordFromList((Slots, RemovedWord), [(Slots2, Word)|Words], CleanedList) :-
	(RemovedWord == Word ->
		CleanedList = Words
	;	removeWordFromList((Slots, RemovedWord), Words, CleanedList1),
		append([(Slots2, Word)], CleanedList1, CleanedList)
	), !.

% For each slots-word tuple, recheck the list of slots and discard any slots
% that are no longer unifiable with the word
cleanSlots([], []).
cleanSlots([(Slots, Word)], CleanedWordsWithSlots) :-
	getSlots(Word, Slots, CleanedSlots), !,
	CleanedWordsWithSlots = [(CleanedSlots, Word)].
cleanSlots([(Slots, Word)|Words], CleanedWordsWithSlots) :-
	getSlots(Word, Slots, CleanedSlots),
	cleanSlots(Words, CleanedWordsWithSlots1),
	append([(CleanedSlots, Word)], CleanedWordsWithSlots1, 
		CleanedWordsWithSlots), !.