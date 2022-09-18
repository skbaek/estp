#!/usr/bin/env swipl

:- initialization(main, main).
:- [inclist].
:- ['$PRELUDE'].

take_(0, _, []) :- !.
take_(NUM, [], _) :- !, format("Cannot take ~w additional elements", [NUM]), throw(take_fail).
take_(NUM, [ELEM | LIST], [ELEM | TAKE]) :- num_pre(NUM, PRE), !, take_(PRE, LIST, TAKE).

test_ovl(NUM, (NAME, TPTP, TSTP)) :- 
  format("Problem num = ~w", [NUM]), nl,
  format("Problem name = ~w", [NAME]), nl, 
  format(string(CMD), "./estp.pl ovl ~w ~w", [TPTP, TSTP]),
  shell(CMD),
  true.

test(NUM, (NAME, TPTP, TSTP)) :- 
  format("Problem num = ~w", [NUM]), nl,
  format("Problem name = ~w", [NAME]), nl, 
  format(string(CMD), "./estp.pl ~w ~w", [TPTP, TSTP]),
  shell(CMD),
  true.

time_test(TIME, NUM, (NAME, TPTP, TSTP)) :- 
  format("Problem num = ~w", [NUM]), nl,
  format("Problem name = ~w", [NAME]), nl, 
  format(string(CMD), "./estp.pl ~w ~w ~w", [TIME, TPTP, TSTP]),
  shell(CMD).

parse_test(NUM, (NAME, TPTP, TSTP)) :- 
  format("Problem ~w : ~w", [NUM, NAME]), nl, 
  format(string(CMD), "~~/projects/hstp/dist-newstyle/build/x86_64-linux/ghc-8.10.7/hstp-0.1.0.0/x/hstp/build/hstp/hstp", [TPTP, TSTP]),
  shell(CMD, 0), nl.

hstp_test(FLAGS, NUM, (NAME, TPTP, TSTP)) :- 
  write("---------------------------------------------------------------------------"), nl, nl,
  atomics_to_string(FLAGS, " ", FLAGS_STR),
  format(string(ESTP), "$ESTP/proofs/~w.estp", [NAME]), nl, nl,
  format("Problem ~w : ~w", [NUM, NAME]), nl, nl,
  % expand_file_name("$HSTP", [HSTP]),
  format(string(CMD), "cabal run :all -- elab ~w ~w ~w ~w", [TPTP, TSTP, ESTP, FLAGS_STR]), 
  shell(CMD, 0), nl.
  
main([DROP_ARG, TAKE_ARG | FLAGS]) :-
  inclist(INC),
  atom_number(DROP_ARG, DROP),
  atom_number(TAKE_ARG, TAKE),
  drop(DROP, INC, TEMP),
  take_(TAKE, TEMP, TUPS), !, 
  imap(hstp_test(FLAGS), DROP, TUPS).