fof(f2614, plain, $false, inference(avatar_sat_refutation, [], [f319, f329, f339, f341, f343, f526, f1073, f1291, f2608])).
fof(f2608, plain, (~ spl14_4 | spl14_6 | ~ spl14_42), inference(avatar_contradiction_clause, [], [f2607])).
fof(f2607, plain, ($false | (~ spl14_4 | spl14_6 | ~ spl14_42)), inference(subsumption_resolution, [], [f2606, f275])).
fof(f275, plain, aElementOf0(xm, szNzAzT0), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (aElementOf0(xn, szNzAzT0) & aElementOf0(xm, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', m__1936)).
fof(f2606, plain, (~ aElementOf0(xm, szNzAzT0) | (~ spl14_4 | spl14_6 | ~ spl14_42)), inference(subsumption_resolution, [], [f2605, f1114])).
fof(f1114, plain, (~ sdtlseqdt0(xm, xn) | spl14_6), inference(subsumption_resolution, [], [f1113, f275])).
fof(f1113, plain, (~ sdtlseqdt0(xm, xn) | ~ aElementOf0(xm, szNzAzT0) | spl14_6), inference(subsumption_resolution, [], [f1111, f276])).
fof(f276, plain, aElementOf0(xn, szNzAzT0), inference(cnf_transformation, [], [f53])).
fof(f1111, plain, (~ sdtlseqdt0(xm, xn) | ~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | spl14_6), inference(resolution, [], [f337, f240])).
fof(f240, plain, ! [X0, X1] : (sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1)) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f163])).
fof(f163, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ~ sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1))) & (sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1)) | ~ sdtlseqdt0(X0, X1))) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1))) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f100])).
fof(f100, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1))) | (~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => (sdtlseqdt0(X0, X1) <=> sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mSuccLess)).
fof(f337, plain, (~ sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)) | spl14_6), inference(avatar_component_clause, [], [f336])).
fof(f336, plain, (spl14_6 <=> sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))), introduced(avatar_definition, [new_symbols(naming, [spl14_6])])).
fof(f2605, plain, (sdtlseqdt0(xm, xn) | ~ aElementOf0(xm, szNzAzT0) | (~ spl14_4 | ~ spl14_42)), inference(duplicate_literal_removal, [], [f2598])).
fof(f2598, plain, (sdtlseqdt0(xm, xn) | ~ aElementOf0(xm, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | (~ spl14_4 | ~ spl14_42)), inference(resolution, [], [f1338, f242])).
fof(f242, plain, ! [X0] : (sdtlseqdt0(X0, szszuzczcdt0(X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (sdtlseqdt0(X0, szszuzczcdt0(X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => sdtlseqdt0(X0, szszuzczcdt0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mLessSucc)).
fof(f1338, plain, (! [X32] : (~ sdtlseqdt0(X32, szszuzczcdt0(xm)) | sdtlseqdt0(X32, xn) | ~ aElementOf0(X32, szNzAzT0)) | (~ spl14_4 | ~ spl14_42)), inference(subsumption_resolution, [], [f1337, f1045])).
fof(f1045, plain, (aElementOf0(szszuzczcdt0(xm), szNzAzT0) | ~ spl14_42), inference(avatar_component_clause, [], [f1044])).
fof(f1044, plain, (spl14_42 <=> aElementOf0(szszuzczcdt0(xm), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl14_42])])).
fof(f1337, plain, (! [X32] : (sdtlseqdt0(X32, xn) | ~ sdtlseqdt0(X32, szszuzczcdt0(xm)) | ~ aElementOf0(szszuzczcdt0(xm), szNzAzT0) | ~ aElementOf0(X32, szNzAzT0)) | ~ spl14_4), inference(subsumption_resolution, [], [f1311, f276])).
fof(f1311, plain, (! [X32] : (sdtlseqdt0(X32, xn) | ~ sdtlseqdt0(X32, szszuzczcdt0(xm)) | ~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(szszuzczcdt0(xm), szNzAzT0) | ~ aElementOf0(X32, szNzAzT0)) | ~ spl14_4), inference(resolution, [], [f245, f327])).
fof(f327, plain, (sdtlseqdt0(szszuzczcdt0(xm), xn) | ~ spl14_4), inference(avatar_component_clause, [], [f326])).
fof(f326, plain, (spl14_4 <=> sdtlseqdt0(szszuzczcdt0(xm), xn)), introduced(avatar_definition, [new_symbols(naming, [spl14_4])])).
fof(f245, plain, ! [X2, X0, X1] : (~ sdtlseqdt0(X1, X2) | sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ! [X0, X1, X2] : (sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f106])).
fof(f106, plain, ! [X0, X1, X2] : ((sdtlseqdt0(X0, X2) | (~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1))) | (~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1, X2] : ((aElementOf0(X2, szNzAzT0) & aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => ((sdtlseqdt0(X1, X2) & sdtlseqdt0(X0, X1)) => sdtlseqdt0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mLessTrans)).
fof(f1291, plain, spl14_42, inference(avatar_contradiction_clause, [], [f1290])).
fof(f1290, plain, ($false | spl14_42), inference(subsumption_resolution, [], [f1287, f275])).
fof(f1287, plain, (~ aElementOf0(xm, szNzAzT0) | spl14_42), inference(resolution, [], [f1046, f232])).
fof(f232, plain, ! [X0] : (aElementOf0(szszuzczcdt0(X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ! [X0] : ((~ (sz00 = szszuzczcdt0(X0)) & aElementOf0(szszuzczcdt0(X0), szNzAzT0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (~ (sz00 = szszuzczcdt0(X0)) & aElementOf0(szszuzczcdt0(X0), szNzAzT0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mSuccNum)).
fof(f1046, plain, (~ aElementOf0(szszuzczcdt0(xm), szNzAzT0) | spl14_42), inference(avatar_component_clause, [], [f1044])).
fof(f1073, plain, (spl14_2 | spl14_4 | ~ spl14_6), inference(avatar_contradiction_clause, [], [f1072])).
fof(f1072, plain, ($false | (spl14_2 | spl14_4 | ~ spl14_6)), inference(subsumption_resolution, [], [f1071, f275])).
fof(f1071, plain, (~ aElementOf0(xm, szNzAzT0) | (spl14_2 | spl14_4 | ~ spl14_6)), inference(subsumption_resolution, [], [f1070, f276])).
fof(f1070, plain, (~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | (spl14_2 | spl14_4 | ~ spl14_6)), inference(subsumption_resolution, [], [f1069, f993])).
fof(f993, plain, (sdtlseqdt0(xm, xn) | ~ spl14_6), inference(subsumption_resolution, [], [f992, f275])).
fof(f992, plain, (sdtlseqdt0(xm, xn) | ~ aElementOf0(xm, szNzAzT0) | ~ spl14_6), inference(subsumption_resolution, [], [f991, f276])).
fof(f991, plain, (sdtlseqdt0(xm, xn) | ~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | ~ spl14_6), inference(resolution, [], [f338, f241])).
fof(f241, plain, ! [X0, X1] : (~ sdtlseqdt0(szszuzczcdt0(X0), szszuzczcdt0(X1)) | sdtlseqdt0(X0, X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f163])).
fof(f338, plain, (sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)) | ~ spl14_6), inference(avatar_component_clause, [], [f336])).
fof(f1069, plain, (~ sdtlseqdt0(xm, xn) | ~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | (spl14_2 | spl14_4)), inference(subsumption_resolution, [], [f1021, f318])).
fof(f318, plain, (~ (xm = xn) | spl14_2), inference(avatar_component_clause, [], [f316])).
fof(f316, plain, (spl14_2 <=> (xm = xn)), introduced(avatar_definition, [new_symbols(naming, [spl14_2])])).
fof(f1021, plain, ((xm = xn) | ~ sdtlseqdt0(xm, xn) | ~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | spl14_4), inference(resolution, [], [f244, f977])).
fof(f977, plain, (sdtlseqdt0(xn, xm) | spl14_4), inference(subsumption_resolution, [], [f976, f276])).
fof(f976, plain, (sdtlseqdt0(xn, xm) | ~ aElementOf0(xn, szNzAzT0) | spl14_4), inference(subsumption_resolution, [], [f975, f275])).
fof(f975, plain, (sdtlseqdt0(xn, xm) | ~ aElementOf0(xm, szNzAzT0) | ~ aElementOf0(xn, szNzAzT0) | spl14_4), inference(resolution, [], [f328, f246])).
fof(f246, plain, ! [X0, X1] : (sdtlseqdt0(szszuzczcdt0(X1), X0) | sdtlseqdt0(X0, X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1] : (sdtlseqdt0(szszuzczcdt0(X1), X0) | sdtlseqdt0(X0, X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0, X1] : ((sdtlseqdt0(szszuzczcdt0(X1), X0) | sdtlseqdt0(X0, X1)) | (~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => (sdtlseqdt0(szszuzczcdt0(X1), X0) | sdtlseqdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mLessTotal)).
fof(f328, plain, (~ sdtlseqdt0(szszuzczcdt0(xm), xn) | spl14_4), inference(avatar_component_clause, [], [f326])).
fof(f244, plain, ! [X0, X1] : (~ sdtlseqdt0(X1, X0) | (X0 = X1) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ! [X0, X1] : ((X0 = X1) | ~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f104])).
fof(f104, plain, ! [X0, X1] : (((X0 = X1) | (~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1))) | (~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => ((sdtlseqdt0(X1, X0) & sdtlseqdt0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mLessASymm)).
fof(f526, plain, (~ spl14_2 | spl14_6), inference(avatar_contradiction_clause, [], [f525])).
fof(f525, plain, ($false | (~ spl14_2 | spl14_6)), inference(subsumption_resolution, [], [f524, f275])).
fof(f524, plain, (~ aElementOf0(xm, szNzAzT0) | (~ spl14_2 | spl14_6)), inference(resolution, [], [f368, f345])).
fof(f345, plain, (~ sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xm)) | (~ spl14_2 | spl14_6)), inference(backward_demodulation, [], [f337, f317])).
fof(f317, plain, ((xm = xn) | ~ spl14_2), inference(avatar_component_clause, [], [f316])).
fof(f368, plain, ! [X2] : (sdtlseqdt0(szszuzczcdt0(X2), szszuzczcdt0(X2)) | ~ aElementOf0(X2, szNzAzT0)), inference(resolution, [], [f232, f243])).
fof(f243, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | sdtlseqdt0(X0, X0)), inference(cnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (sdtlseqdt0(X0, X0) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => sdtlseqdt0(X0, X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', mLessRefl)).
fof(f343, plain, (spl14_1 | spl14_4 | spl14_2), inference(avatar_split_clause, [], [f282, f316, f326, f312])).
fof(f312, plain, (spl14_1 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl14_1])])).
fof(f282, plain, ((xm = xn) | sdtlseqdt0(szszuzczcdt0(xm), xn) | sP4), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ((~ aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & ~ sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)) & ((xm = xn) | (aElementOf0(xm, slbdtrb0(xn)) & sdtlseqdt0(szszuzczcdt0(xm), xn)))) | sP4), inference(definition_folding, [], [f130, e137])).
fof(f137, plain, ((~ (xm = xn) & ~ aElementOf0(xm, slbdtrb0(xn)) & ~ sdtlseqdt0(szszuzczcdt0(xm), xn) & aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))) | ~ sP4), inference(usedef, [], [e137])).
fof(e137, plain, (sP4 <=> (~ (xm = xn) & ~ aElementOf0(xm, slbdtrb0(xn)) & ~ sdtlseqdt0(szszuzczcdt0(xm), xn) & aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f130, plain, ((~ aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & ~ sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)) & ((xm = xn) | (aElementOf0(xm, slbdtrb0(xn)) & sdtlseqdt0(szszuzczcdt0(xm), xn)))) | (~ (xm = xn) & ~ aElementOf0(xm, slbdtrb0(xn)) & ~ sdtlseqdt0(szszuzczcdt0(xm), xn) & aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)))), inference(flattening, [], [f129])).
fof(f129, plain, (((~ aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & ~ sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))) & ((xm = xn) | (aElementOf0(xm, slbdtrb0(xn)) & sdtlseqdt0(szszuzczcdt0(xm), xn)))) | ((~ (xm = xn) & ~ aElementOf0(xm, slbdtrb0(xn)) & ~ sdtlseqdt0(szszuzczcdt0(xm), xn)) & (aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ~ ((((xm = xn) | (aElementOf0(xm, slbdtrb0(xn)) & sdtlseqdt0(szszuzczcdt0(xm), xn))) => (aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) | sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)))) & ((aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))) => ((xm = xn) | aElementOf0(xm, slbdtrb0(xn)) | sdtlseqdt0(szszuzczcdt0(xm), xn)))), inference(negated_conjecture, [], [f54])).
fof(f54, plain, ~ ((((xm = xn) | (aElementOf0(xm, slbdtrb0(xn)) & sdtlseqdt0(szszuzczcdt0(xm), xn))) => (aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) | sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)))) & ((aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))) => ((xm = xn) | aElementOf0(xm, slbdtrb0(xn)) | sdtlseqdt0(szszuzczcdt0(xm), xn)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM541+2.p', m__)).
fof(f341, plain, (spl14_1 | ~ spl14_6), inference(avatar_split_clause, [], [f284, f336, f312])).
fof(f284, plain, (~ sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)) | sP4), inference(cnf_transformation, [], [f138])).
fof(f339, plain, (~ spl14_1 | spl14_6), inference(avatar_split_clause, [], [f277, f336, f312])).
fof(f277, plain, (sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn)) | ~ sP4), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ((~ (xm = xn) & ~ aElementOf0(xm, slbdtrb0(xn)) & ~ sdtlseqdt0(szszuzczcdt0(xm), xn) & aElementOf0(xm, slbdtrb0(szszuzczcdt0(xn))) & sdtlseqdt0(szszuzczcdt0(xm), szszuzczcdt0(xn))) | ~ sP4), inference(nnf_transformation, [], [f137])).
fof(f329, plain, (~ spl14_1 | ~ spl14_4), inference(avatar_split_clause, [], [f279, f326, f312])).
fof(f279, plain, (~ sdtlseqdt0(szszuzczcdt0(xm), xn) | ~ sP4), inference(cnf_transformation, [], [f183])).
fof(f319, plain, (~ spl14_1 | ~ spl14_2), inference(avatar_split_clause, [], [f281, f316, f312])).
fof(f281, plain, (~ (xm = xn) | ~ sP4), inference(cnf_transformation, [], [f183])).