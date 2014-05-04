:- use_module(library(clpfd)).
:- use_module(library(lists)).

?- ensure_loaded('hanjie_examples.pl').
?- ensure_loaded('hanjie_generation.pl').

%FOR EXAMPLES OF HANJIE PUZZLES, LOAD FILE "hanjie_examples.pl"
 
%predicate fo solving hanjie puzzles
%initializes a grid (list of lists) of uninstanciated variables with the apropriate size for the specified column and row hints.
%a hint must be a list of lists where every sublist contains the size and sequence of gray blocks in that line
%e.g.: [[1,1],[1]] means the first line must have two blocks of gray squares with length one and the second line must only have one
%the grid is then flattened to restrict the domain of the variables to [0,1].
%additional constrains are added with the predicate hanjie_constraints/3
%afterwards, the labeling/2 predicate is used and the resulting grid is drawn on screen
%IMPORTANT: EVERY ROW/COLUMN MUST HAVE A HINT, EVEN IF IT IS 0, OTHERWISE THE SIZE OF THE GRID WOULD HAVE TO BE INDICATED MANUALLY
hanjie(ColHints,RowHints):-
	length(ColHints,GridWidth),
	length(RowHints,GridHeight),
	make_grid(Solution,GridWidth,GridHeight),
	flatten(Solution,Vars),
	domain(Vars,0,1),
	!,
	hanjie_constraints(ColHints,RowHints,Solution),
	flatten(ColHints, TotalColHints),flatten(RowHints, TotalRowHints),
	sum(TotalColHints, #=, TotalValue), sum(TotalRowHints, #=, TotalValue),
	sum(Vars, #=, TotalValue),
	labeling([ff],Vars),
	draw_grid(Solution,ColHints,RowHints).
	
%this predicate allows the puzzle solution to be stored in a file given by the specified path
hanjie_solve_to_file(ColHints,RowHints, Filepath):-
	open(Filepath,append,S1),
	length(ColHints,GridWidth),
	length(RowHints,GridHeight),
	make_grid(Solution,GridWidth,GridHeight),
	flatten(Solution,Vars),
	domain(Vars,0,1),
	!,
	hanjie_constraints(ColHints,RowHints,Solution),
	flatten(ColHints, TotalColHints),flatten(RowHints, TotalRowHints),
	sum(TotalColHints, #=, TotalValue), sum(TotalRowHints, #=, TotalValue),
	sum(Vars, #=, TotalValue),
	labeling([ff],Vars),
	draw_grid(S1,Solution,ColHints,RowHints),
	close(S1).
	
%this predicate will generate a random hanjie puzzle and solve it by calling hanjie_generate and hanjie
hanjie_generate_and_solve(NumCols,NumRows):-
	generate_hanjie(NumCols,NumRows,ColHints,RowHints),
	!,
	hanjie(ColHints,RowHints).
	
%does the same as hanjie/2 but measures statistics for calculating the solution instead of drawing the grid
hanjie_stats(ColHints,RowHints):-
	length(ColHints,GridWidth),
	length(RowHints,GridHeight),
	make_grid(Solution,GridWidth,GridHeight),
	flatten(Solution,Vars),
	domain(Vars,0,1),
	!,
	print('dumping previous fd stats:'),nl,
	fd_statistics,
	statistics(walltime,[W0|_]),
	statistics(runtime,[T0|_]),
	hanjie_constraints(ColHints,RowHints,Solution),
	flatten(ColHints, TotalColHints),flatten(RowHints, TotalRowHints),
	sum(TotalColHints, #=, TotalValue), sum(TotalRowHints, #=, TotalValue),
	sum(Vars, #=, TotalValue),
	statistics(walltime,[W1|_]),
	statistics(runtime,[T1|_]),
	labeling([ff],Vars),
	statistics(walltime,[W2|_]),
	statistics(runtime,[T2|_]),
	T is T1-T0,
	W is W1-W0,
	Tl is T2-T1,
	Wl is W2-W1,
	nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,nl,
	format('creating constraints took CPU ~3d sec.~n', [T]),
	format('creating constraints took a total of ~3d sec.~n', [W]),
	format('labeling took CPU ~3d sec.~n', [Tl]),
	format('labeling took a total of ~3d sec.~n', [Wl]),
	nl,
	fd_statistics.
	
%creates the constrains for each row of the grid using restrict_rows/2 and the row hints.
%uses the predicate transpose/2 (lists library) to obtain a list of the columns and creates the constraints for
%them as well, using restrict_rows/2 and the column hints
hanjie_constraints(ColHints,RowHints,Grid):-
	restrict_rows(Grid,RowHints),
	transpose(Grid,Columns),
	restrict_rows(Columns,ColHints).
	
%restricts the domain of each element of the grid by ensuring the sum of all elements is equal to the sum of the hints
%(0 corresponds to blank squares and 1 corresponds to gray squares of the hanjie puzzle)
%to ensure the sequences of colored squares are maintained, this predicate creates an automaton for each row
restrict_rows([],[]).
restrict_rows([R|Rs],[H|Hs]):-
	length(R,MaxSum),
	(%This 'or' allows lists to be completely uninstantiated
	var(H),
	HintLength is floor((MaxSum+1)/2),
	length(H,HintLength),
	checkHints(H,MaxSum)
	;
	nonvar(H),
	checkHints(H,MaxSum)
	),
	RowSum #=< MaxSum,
	sum(H,#=,RowSum),
	sum(R,#=,RowSum),
	create_transitions(H, Arcs, start, FinalState,1),
	append(R, [0], RowWithExtraZero), %a zero is added to simplify the automaton (every gray block must be followed by at least one blank square, even the last one)
	automaton(RowWithExtraZero, [source(start), sink(FinalState)], [arc(start,0,start) | Arcs]),
	restrict_rows(Rs,Hs).

%checks if the hints are variables. If so, the domain is assigned to the variable
checkHints([],_):-!.
checkHints([H|Hs],MaxSum):-
	(var(H),domain([H],0,MaxSum);
	integer(H)),
	checkHints(Hs,MaxSum).

%this predicate is used to generate the transitions (arcs) between each state of the automaton
%it goes through every value of the Hint list, decrementing them until they reach 0 and creating mandatory transitions
%to ensure continuous gray blocks. When the Hint reaches 0 it creates transitions to ensure at least one blank block after the gray one
%(to allow for 1 or more white squares after each gray block, the automaton will be an NFA)
%if the first square of a block has a hint with value 0 (FirstSquare = 1), the CurState is set as NextState (hint is ignored)
create_transitions([], [], FinalState, FinalState,_).
create_transitions([Hint|Hs], Transitions, CurState, FinalState,FirstSquare) :-
    (Hint #= 0, %finished current 'gray' blocks segment
	%'gray' blocks must be followed by AT LEAST ONE 'blank' square (loop to current state with 'blank' blocks)
	%(an extra 'blank' square was added to end of the row for the case when the 'gray' block ends at the grid's border)
    (FirstSquare =:=0,
		Transitions = [arc(CurState,0,CurState), arc(CurState,0,NextState) | Ts],
		create_transitions(Hs, Ts, NextState, FinalState,1);
	FirstSquare =:=1,
		Transitions = [arc(CurState,0,CurState)],
		create_transitions(Hs, Ts, CurState, FinalState,1))
	;
	%in this case, we aren't finished with the gray block yet and as such need more 'gray' squares to advance
	Hint #> 0,
    Transitions = [arc(CurState,1,NextState) | Ts],
    NewHint #= Hint-1,
    create_transitions([NewHint|Hs], Ts, NextState, FinalState,0)).

	
	
%------------------------------------------------------UTILITIES-------------------------------------------------------
%flattens a list of lists into a single list
flatten([],[]):-!.
flatten([[]|Gs],Fg):-
	!,
	flatten(Gs,Fg).
flatten([[G1|G1s]|Gs],[G1|Fg]):-
	flatten([G1s|Gs],Fg).
	
	
%this predicate is used to create a grid (list of lists) of uninstanciated variables with the specified dimensions
make_grid(Grid,Width,Height):-
	length(Grid,Height),
	make_rows(Grid,Width).
make_rows([],_):-!.
make_rows([G|Gs],Width):-
	length(G,Width),
	make_rows(Gs,Width).
	
%this predicate is used to determine the size of the largest sublist in a list of lists
largest_sub_list([],0):-!.
largest_sub_list([L|Ls],N):-
	largest_sub_list(Ls,M1),
	length(L,M2),
	(M1 > M2,
	N is M1;
	N is M2),
	!.
%--------------------------------------------------END UTILITIES-------------------------------------------------------

%-----------------------------------------------DISPLAY PREDICATES-----------------------------------------------------
%predicates for drawing the hanjie's puzzle grid
sign(0,' ').
sign(1,'X').
draw_grid([B|Bs],ColHints,RowHints):-
	largest_sub_list(RowHints,HChars),
	HSpacing is HChars*2,
	nl,nl,
	HSpacingForCols is HSpacing+1,
	draw_column_hints(ColHints,HSpacingForCols),
	length(B,N),
	putChars(' ',HSpacing),
	print('|'),print_separator(N,'-'),
	pg([B|Bs],RowHints,HSpacing),
	putChars(' ',HSpacing),
	print('|'),print_separator(N,'-'),nl.
pg([B],[RH],HLength) :-
	draw_row_hints(RH,HLength),
	print_line(B), !.
pg([B|Bs],[RH|RHs],HLength) :-
	draw_row_hints(RH,HLength),
	print_line(B),
	length(B,N),
	putChars(' ',HLength),
	print('|'),
	print_separator(N,'+'),
	pg(Bs,RHs,HLength).
print_line([]) :-
	print('|'),nl.
print_line([L|Ls]):-
	sign(L,Symbol),
	print('|'),
	print(Symbol),
	print_line(Ls).
print_separator(1,_):-print('-|'), nl.
print_separator(N,MidChar):-
	N > 1,
	N1 is N-1,
	print('-'),print(MidChar),
	print_separator(N1,MidChar).
	
putChars(_,0):-!.
putChars(Char,N):-
	N > 0,
	N1 is N-1,
	print(Char),
	putChars(Char,N1).
	
draw_column_hints(ColHints,HSpacing):-
	largest_sub_list(ColHints,VSpacing),
	dch(ColHints,HSpacing,VSpacing).
dch(_,_,0):-!.
dch(ColHints,HSpacing,VSpacing):-
	VSpacing > 0,
	putChars(' ',HSpacing),
	draw_elements_at_vpos(ColHints, VSpacing),nl,
	NewVSpacing is VSpacing-1,
	dch(ColHints,HSpacing,NewVSpacing).
draw_elements_at_vpos([],_):-!.
draw_elements_at_vpos([CH|CHs],VSpacing):-
	length(CH,NumHints),
	(NumHints < VSpacing,!,
	print('  ')
	;
	ElementPos is NumHints-VSpacing,
	nth0(ElementPos,CH,Value),
	(Value < 10,!,
	 print(Value);
	 print('#')),
	print(' ')),
	draw_elements_at_vpos(CHs,VSpacing).
	
draw_row_hints([],_):-!.
draw_row_hints([R|Rs],HSize):-
    HSize>0,
	length([R|Rs],L),
	(HSize > L*2,!,
	NewHSize is HSize-2,
	print('  '),
	draw_row_hints([R|Rs],NewHSize)
	;
	NewHSize is HSize-2,
	(R<10,!,
	print(R),print(' ');
	print('# ')),
	draw_row_hints(Rs,NewHSize)).
%-------------------------------------------END DISPLAY PREDICATES-----------------------------------------------------

%-----------------------------------------------FILE I/O PREDICATES-----------------------------------------------------
%predicates for drawing the hanjie's puzzle grid in a file
draw_grid(Stream,[B|Bs],ColHints,RowHints):-
	largest_sub_list(RowHints,HChars),
	HSpacing is HChars*2,
	write(Stream, '\n\n'),
	HSpacingForCols is HSpacing+1,
	draw_column_hints(Stream, ColHints,HSpacingForCols),
	length(B,N),
	putChars(Stream,' ',HSpacing),
	write(Stream,'|'),print_separator(Stream,N,'-'),
	pg(Stream,[B|Bs],RowHints,HSpacing),
	putChars(Stream,' ',HSpacing),
	write(Stream,'|'),print_separator(Stream,N,'-'),write(Stream,'\n').
pg(Stream,[B],[RH],HLength) :-
	draw_row_hints(Stream,RH,HLength),
	print_line(Stream,B), !.
pg(Stream,[B|Bs],[RH|RHs],HLength) :-
	draw_row_hints(Stream,RH,HLength),
	print_line(Stream,B),
	length(B,N),
	putChars(Stream,' ',HLength),
	write(Stream,'|'),
	print_separator(Stream,N,'+'),
	pg(Stream,Bs,RHs,HLength).
print_line(Stream,[]) :-
	write(Stream,'|\n').
print_line(Stream,[L|Ls]):-
	sign(L,Symbol),
	write(Stream,'|'),
	write(Stream,Symbol),
	print_line(Stream,Ls).
print_separator(Stream,1,_):-write(Stream,'-|'), write(Stream,'\n').
print_separator(Stream,N,MidChar):-
	N > 1,
	N1 is N-1,
	write(Stream,'-'),write(Stream,MidChar),
	print_separator(Stream,N1,MidChar).
	
putChars(_,_,0):-!.
putChars(Stream,Char,N):-
	N > 0,
	N1 is N-1,
	write(Stream,Char),
	putChars(Stream,Char,N1).
	
draw_column_hints(Stream,ColHints,HSpacing):-
	largest_sub_list(ColHints,VSpacing),
	dch(Stream,ColHints,HSpacing,VSpacing).
dch(_,_,_,0):-!.
dch(Stream,ColHints,HSpacing,VSpacing):-
	VSpacing > 0,
	putChars(Stream,' ',HSpacing),
	draw_elements_at_vpos(Stream,ColHints, VSpacing),write(Stream,'\n'),
	NewVSpacing is VSpacing-1,
	dch(Stream,ColHints,HSpacing,NewVSpacing).
draw_elements_at_vpos(_,[],_):-!.
draw_elements_at_vpos(Stream,[CH|CHs],VSpacing):-
	length(CH,NumHints),
	(NumHints < VSpacing,!,
	write(Stream,'  ')
	;
	ElementPos is NumHints-VSpacing,
	nth0(ElementPos,CH,Value),
	(Value < 10,!,
	 write(Stream,Value);
	 write(Stream,'#')),
	write(Stream,' ')),
	draw_elements_at_vpos(Stream,CHs,VSpacing).
	
draw_row_hints(_,[],_):-!.
draw_row_hints(Stream,[R|Rs],HSize):-
    HSize>0,
	length([R|Rs],L),
	(HSize > L*2,!,
	NewHSize is HSize-2,
	write(Stream,'  '),
	draw_row_hints(Stream,[R|Rs],NewHSize)
	;
	NewHSize is HSize-2,
	(R<10,!,
	write(Stream,R),write(Stream,' ');
	write(Stream,'# ')),
	draw_row_hints(Stream,Rs,NewHSize)).
%-------------------------------------------END DISPLAY PREDICATES-----------------------------------------------------