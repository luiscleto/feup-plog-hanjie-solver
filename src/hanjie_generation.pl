:- use_module(library(lists)).
:- use_module(library(random)).

?-ensure_loaded('hanjie.pl').


%this predicate will create a random hanjie grid and obtain the clues for generated solutions
generate_hanjie(Width,Height,ColHints,RowHints):-
	make_atom_grid(PuzzleGrid,Width,Height),
	generate_rows_hints(PuzzleGrid,RH),
	transpose(PuzzleGrid,Cols),
	generate_rows_hints(Cols,CH),
	strip_zeros(RH,RowHints),
	strip_zeros(CH,ColHints),
	nl,nl,nl,
	print('This is a possible solution (the larger the grid the more likely it will have multiple solutions):'),nl,
	draw_grid(PuzzleGrid,ColHints,RowHints).
	
%this predicate will create a hanjie grid with all squares painted
generate_hanjie_full(Width,Height,ColHints,RowHints):-
	make_atom_grid_full(PuzzleGrid,Width,Height),
	generate_rows_hints(PuzzleGrid,RH),
	transpose(PuzzleGrid,Cols),
	generate_rows_hints(Cols,CH),
	strip_zeros(RH,RowHints),
	strip_zeros(CH,ColHints).

%this predicate will create a hanjie grid with all squares blank	
generate_hanjie_empty(Width,Height,ColHints,RowHints):-
	make_atom_grid_empty(PuzzleGrid,Width,Height),
	generate_rows_hints(PuzzleGrid,RH),
	transpose(PuzzleGrid,Cols),
	generate_rows_hints(Cols,CH),
	strip_zeros(RH,RowHints),
	strip_zeros(CH,ColHints).
	
strip_zeros([],[]):-!.
strip_zeros([Row|Rest],[Result|Rs]):-
	delete(Row,0,Temp),
	(Temp = [],
	Result = [0]
	;
	Result=Temp),!,
	strip_zeros(Rest,Rs).
	
generate_rows_hints([],[]):-!.
generate_rows_hints([Row|Rest],[RH|RHs]):-
	generate_hints_for_row(Row,RH),
	generate_rows_hints(Rest,RHs).
	
generate_hints_for_row([],[0]):-!.
generate_hints_for_row([0|Rest],[0|RHs]):-
	generate_hints_for_row(Rest,RHs).
generate_hints_for_row([1|Rest],[Head|RHs]):-
	generate_hints_for_row(Rest,[NextHead|RHs]),
	Head is NextHead+1.
	
	
make_atom_grid_empty([],_,0):-!.
make_atom_grid_empty([Row|Rest],Width,Height):-
	Height > 0,
	make_atom_row_empty(Row,Width),
	RemainingHeight is Height-1,
	make_atom_grid_empty(Rest,Width,RemainingHeight).
make_atom_row_empty([],0):-!.
make_atom_row_empty([0|Rs],Width):-
	Width > 0,
	RemainingWidth is Width-1,
	make_atom_row_empty(Rs,RemainingWidth).	
	
make_atom_grid_full([],_,0):-!.
make_atom_grid_full([Row|Rest],Width,Height):-
	Height > 0,
	make_atom_row_full(Row,Width),
	RemainingHeight is Height-1,
	make_atom_grid_full(Rest,Width,RemainingHeight).
	
make_atom_row_full([],0):-!.
make_atom_row_full([1|Rs],Width):-
	Width > 0,
	RemainingWidth is Width-1,
	make_atom_row_full(Rs,RemainingWidth).
	
make_atom_grid([],_,0):-!.
make_atom_grid([Row|Rest],Width,Height):-
	Height > 0,
	make_atom_row(Row,Width),
	RemainingHeight is Height-1,
	make_atom_grid(Rest,Width,RemainingHeight).
	
make_atom_row([],0):-!.
make_atom_row([R|Rs],Width):-
	Width > 0,
	random(0,2,R),
	RemainingWidth is Width-1,
	make_atom_row(Rs,RemainingWidth).