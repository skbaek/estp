#!/usr/bin/env swipl
:- initialization(main, main).
:- ['$PRELUDE', solutions].

name_probpath(NAME, PROB_PATH) :- 
  atom_codes(NAME, [C0, C1, C2 | _]),
  atom_codes(CAT, [C0, C1, C2]),  
  format(string(PRE_PROB_PATH), "$TPTP/Problems/~w/~w.p", [CAT, NAME]),
  expand_file_name(PRE_PROB_PATH, [PROB_PATH]).

name_solpath(NAME, SOL_PATH) :- 
  format(string(PRE_SOL_PATH), "$TSTP/alt/~w.s", [NAME]),
  expand_file_name(PRE_SOL_PATH, [SOL_PATH]),
  exists_file(SOL_PATH), !.

name_solpath(NAME, SOL_PATH) :- 
  format(string(PRE_SOL_PATH), "$TSTP/vampire/~w.s", [NAME]),
  expand_file_name(PRE_SOL_PATH, [SOL_PATH]).

name_tuple(NAME, (SIZE, NAME, TPTP, TSTP)) :-  
  name_probpath(NAME, TPTP),
  name_solpath(NAME, TSTP),
  size_file(TPTP, TPTP_SIZE),
  size_file(TSTP, TSTP_SIZE),
  SIZE is TPTP_SIZE + TSTP_SIZE.

compare_pair('<', (M, _), (N, _)) :- compare('<', M, N), !.
compare_pair('>', (M, _), (N, _)) :- compare('>', M, N), !.
compare_pair(CMP, (_, F), (_, G)) :- compare(CMP, F, G).

print_triple((X,Y,Z)) :- format("    ('~w', '~w', '~w')", [X, Y, Z]).

%---------------------------------------

% 
% bad_solution('MSC015-1.020'). % Solvable give 16GB ram + 5? min time
% bad_solution('SWV545-1.010'). % Solvable give 16GB ram + 5? min time
% bad_solution('NUM378+1.015.010'). % Solvable give 16GB ram + 3? min time
% bad_solution('NUM378+1.015.015'). % Solvable give 16GB ram + 5? min time
% bad_solution('NUM378+1.015.020'). % Solvable give 16GB ram + 7? min time
% bad_solution('NUM378+1.020.015'). % Solvable give 16GB ram + 12 min time
% 
% bad_solution('SYO591+1'). % Vampire fails orig
% bad_solution('SYO592+1'). % Vampire fails orig
% bad_solution('SYO594+1'). % Vampire fails orig
% bad_solution('HWV057+1'). % Vampire fails orig
% bad_solution('HWV059+1'). % Vampire fails orig
% bad_solution('HWV058+1'). % Vampire fails orig
% 
% bad_solution('SYN007+1.014'). % needs >32GB memory 
% bad_solution('NUM378+1.020.020'). % needs >32GB memory

bad_solution('SWW294+1'). % PPR fail due to rare quantifier permutation
bad_solution('SYN986+1.004'). % Outlier problem with too many nested predicates, parsing issue
bad_solution('SYO561+1'). % Nonexistent hypothesis name

%---------------------------------------

namep_name(NAMEP, NAME) :-
  atom_chars(NAMEP, CHSP), 
  append(CHS, [_, _], CHSP),
  atom_chars(NAME, CHS).

main :- 
  findall(NAME, solution(vampire, NAME, passed(_, _)), NAMES), 
  % --------------------------------------------------
  % findall(NAME, fail(NAME, _), NAMES), 
  % --------------------------------------------------
  % directory_files('./elabs', FILES),
  % subtract(FILES, ['.', '..'], NAMEPS),
  % cmap(namep_name, NAMEPS, NAMES),
  % --------------------------------------------------
  exclude(bad_solution, NAMES, GOOD_NAMES), !,
  cmap(name_tuple, GOOD_NAMES, TUPS),
  predsort(compare_pair, TUPS, SORTED),
  cmap(snd, SORTED, TRIPLES),
  append(TRIPLES_, [TRIPLE], TRIPLES),
  write("inclist(\n  [\n"),
  cmap([X]>>(print_triple(X), write(","), nl), TRIPLES_),
  print_triple(TRIPLE), nl,
  write("  ]\n).\n").