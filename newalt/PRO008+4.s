fof(f197134, plain, $false, inference(avatar_sat_refutation, [], [f388, f396, f452, f457, f472, f482, f1645, f1671, f2490, f2579, f2674, f2731, f4159, f5088, f5372, f6443, f6445, f7261, f16046, f16051, f16197, f17888, f20810, f20822, f21205, f24674, f25234, f29299, f50874, f50964, f82237, f82779, f107342, f110296, f110615, f147074, f147195, f165731, f168505, f168760, f168994, f169006, f169136, f170068, f170069, f170298, f170815, f170851, f194946, f195234, f195680, f197091, f197130, f197133])).
fof(f197133, plain, (~ spl19_31 | ~ spl19_33), inference(avatar_contradiction_clause, [], [f197132])).
fof(f197132, plain, ($false | (~ spl19_31 | ~ spl19_33)), inference(subsumption_resolution, [], [f197131, f250])).
fof(f250, plain, ~ (tptp1 = tptp2), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ~ (tptp1 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_44)).
fof(f197131, plain, ((tptp1 = tptp2) | (~ spl19_31 | ~ spl19_33)), inference(forward_demodulation, [], [f2561, f169187])).
fof(f169187, plain, ((tptp1 = sK5(sK15(sK17))) | ~ spl19_31), inference(resolution, [], [f2547, f302])).
fof(f302, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | (sK5(X1) = X0)), inference(subsumption_resolution, [], [f296, f182])).
fof(f182, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ! [X0, X1] : (activity_occurrence(X1) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0, X1] : (occurrence_of(X1, X0) => activity_occurrence(X1)), inference(pure_predicate_removal, [], [f50])).
fof(f50, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X12, X13] : (occurrence_of(X13, X12) => (activity_occurrence(X13) & activity(X12))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_03)).
fof(f296, plain, ! [X0, X1] : ((sK5(X1) = X0) | ~ occurrence_of(X1, X0) | ~ activity_occurrence(X1)), inference(resolution, [], [f190, f195])).
fof(f195, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f142])).
fof(f142, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f107, f141])).
fof(f141, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) => occurrence_of(X0, sK5(X0))), introduced(choice_axiom, [])).
fof(f107, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : occurrence_of(X0, X1)), inference(pure_predicate_removal, [], [f59])).
fof(f59, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X41] : (activity_occurrence(X41) => ? [X42] : (occurrence_of(X41, X42) & activity(X42))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_12)).
fof(f190, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f100])).
fof(f100, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X28, X29, X30] : ((occurrence_of(X28, X30) & occurrence_of(X28, X29)) => (X29 = X30)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_08)).
fof(f2547, plain, (occurrence_of(sK15(sK17), tptp1) | ~ spl19_31), inference(avatar_component_clause, [], [f2545])).
fof(f2545, plain, (spl19_31 <=> occurrence_of(sK15(sK17), tptp1)), introduced(avatar_definition, [new_symbols(naming, [spl19_31])])).
fof(f2561, plain, ((tptp2 = sK5(sK15(sK17))) | ~ spl19_33), inference(avatar_component_clause, [], [f2559])).
fof(f2559, plain, (spl19_33 <=> (tptp2 = sK5(sK15(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_33])])).
fof(f197130, plain, (~ spl19_31 | spl19_119 | ~ spl19_499 | ~ spl19_1543), inference(avatar_contradiction_clause, [], [f197129])).
fof(f197129, plain, ($false | (~ spl19_31 | spl19_119 | ~ spl19_499 | ~ spl19_1543)), inference(subsumption_resolution, [], [f197128, f169187])).
fof(f197128, plain, (~ (tptp1 = sK5(sK15(sK17))) | (spl19_119 | ~ spl19_499 | ~ spl19_1543)), inference(forward_demodulation, [], [f197127, f25233])).
fof(f25233, plain, ((sK15(sK17) = sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ spl19_499), inference(avatar_component_clause, [], [f25231])).
fof(f25231, plain, (spl19_499 <=> (sK15(sK17) = sK15(sK3(tptp0, sK13(sK17), sK15(sK17))))), introduced(avatar_definition, [new_symbols(naming, [spl19_499])])).
fof(f197127, plain, (~ (tptp1 = sK5(sK15(sK3(tptp0, sK13(sK17), sK15(sK17))))) | (spl19_119 | ~ spl19_1543)), inference(forward_demodulation, [], [f9727, f117412])).
fof(f117412, plain, ((sK13(sK17) = sK11(sK15(sK17), tptp0)) | ~ spl19_1543), inference(avatar_component_clause, [], [f117410])).
fof(f117410, plain, (spl19_1543 <=> (sK13(sK17) = sK11(sK15(sK17), tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl19_1543])])).
fof(f9727, plain, (~ (tptp1 = sK5(sK15(sK3(tptp0, sK11(sK15(sK17), tptp0), sK15(sK17))))) | spl19_119), inference(avatar_component_clause, [], [f9726])).
fof(f9726, plain, (spl19_119 <=> (tptp1 = sK5(sK15(sK3(tptp0, sK11(sK15(sK17), tptp0), sK15(sK17)))))), introduced(avatar_definition, [new_symbols(naming, [spl19_119])])).
fof(f197091, plain, (spl19_33 | ~ spl19_32), inference(avatar_split_clause, [], [f197082, f2549, f2559])).
fof(f2549, plain, (spl19_32 <=> occurrence_of(sK15(sK17), tptp2)), introduced(avatar_definition, [new_symbols(naming, [spl19_32])])).
fof(f197082, plain, ((tptp2 = sK5(sK15(sK17))) | ~ spl19_32), inference(subsumption_resolution, [], [f197081, f255])).
fof(f255, plain, occurrence_of(sK17, tptp0), inference(cnf_transformation, [], [f177])).
fof(f177, plain, (! [X1, X2] : (((min_precedes(X1, sK18(X1), tptp0) & subactivity_occurrence(sK18(X1), sK17) & occurrence_of(sK18(X1), tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17) | ~ min_precedes(X1, X2, tptp0) | (~ occurrence_of(X2, tptp2) & ~ occurrence_of(X2, tptp1)) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3)) & occurrence_of(sK17, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK17, sK18])], [f132, f176, f175])).
fof(f175, plain, (? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0) | ~ min_precedes(X1, X2, tptp0) | (~ occurrence_of(X2, tptp2) & ~ occurrence_of(X2, tptp1)) | ~ root_occ(X1, X0) | ~ occurrence_of(X1, tptp3)) & occurrence_of(X0, tptp0)) => (! [X2, X1] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK17) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17) | ~ min_precedes(X1, X2, tptp0) | (~ occurrence_of(X2, tptp2) & ~ occurrence_of(X2, tptp1)) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3)) & occurrence_of(sK17, tptp0))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X1] : (? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK17) & occurrence_of(X3, tptp1)) => (min_precedes(X1, sK18(X1), tptp0) & subactivity_occurrence(sK18(X1), sK17) & occurrence_of(sK18(X1), tptp1))), introduced(choice_axiom, [])).
fof(f132, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0) | ~ min_precedes(X1, X2, tptp0) | (~ occurrence_of(X2, tptp2) & ~ occurrence_of(X2, tptp1)) | ~ root_occ(X1, X0) | ~ occurrence_of(X1, tptp3)) & occurrence_of(X0, tptp0)), inference(definition_folding, [], [f130, e131])).
fof(f131, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(usedef, [], [e131])).
fof(e131, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f130, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ leaf_occ(X2, X0) | ~ min_precedes(X1, X2, tptp0) | (~ occurrence_of(X2, tptp2) & ~ occurrence_of(X2, tptp1)) | ~ root_occ(X1, X0) | ~ occurrence_of(X1, tptp3)) & occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ~ ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2] : ((occurrence_of(X2, tptp2) => ~ ? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1))) & (occurrence_of(X2, tptp1) => ~ ? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2))) & leaf_occ(X2, X0) & min_precedes(X1, X2, tptp0) & (occurrence_of(X2, tptp2) | occurrence_of(X2, tptp1)) & root_occ(X1, X0) & occurrence_of(X1, tptp3))), inference(rectify, [], [f47])).
fof(f47, plain, ~ ! [X105] : (occurrence_of(X105, tptp0) => ? [X106, X107] : ((occurrence_of(X107, tptp2) => ~ ? [X109] : (min_precedes(X106, X109, tptp0) & subactivity_occurrence(X109, X105) & occurrence_of(X109, tptp1))) & (occurrence_of(X107, tptp1) => ~ ? [X108] : (min_precedes(X106, X108, tptp0) & subactivity_occurrence(X108, X105) & occurrence_of(X108, tptp2))) & leaf_occ(X107, X105) & min_precedes(X106, X107, tptp0) & (occurrence_of(X107, tptp2) | occurrence_of(X107, tptp1)) & root_occ(X106, X105) & occurrence_of(X106, tptp3))), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ ! [X105] : (occurrence_of(X105, tptp0) => ? [X106, X107] : ((occurrence_of(X107, tptp2) => ~ ? [X109] : (min_precedes(X106, X109, tptp0) & subactivity_occurrence(X109, X105) & occurrence_of(X109, tptp1))) & (occurrence_of(X107, tptp1) => ~ ? [X108] : (min_precedes(X106, X108, tptp0) & subactivity_occurrence(X108, X105) & occurrence_of(X108, tptp2))) & leaf_occ(X107, X105) & min_precedes(X106, X107, tptp0) & (occurrence_of(X107, tptp2) | occurrence_of(X107, tptp1)) & root_occ(X106, X105) & occurrence_of(X106, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', goals)).
fof(f197081, plain, ((tptp2 = sK5(sK15(sK17))) | ~ occurrence_of(sK17, tptp0) | ~ spl19_32), inference(subsumption_resolution, [], [f197068, f250])).
fof(f197068, plain, ((tptp2 = sK5(sK15(sK17))) | (tptp1 = tptp2) | ~ occurrence_of(sK17, tptp0) | ~ spl19_32), inference(resolution, [], [f2551, f1733])).
fof(f1733, plain, ! [X2, X3] : (~ occurrence_of(sK15(X2), X3) | (tptp2 = sK5(sK15(X2))) | (tptp1 = X3) | ~ occurrence_of(X2, tptp0)), inference(resolution, [], [f389, f190])).
fof(f389, plain, ! [X0] : (occurrence_of(sK15(X0), tptp1) | ~ occurrence_of(X0, tptp0) | (tptp2 = sK5(sK15(X0)))), inference(resolution, [], [f237, f302])).
fof(f237, plain, ! [X0] : (occurrence_of(sK15(X0), tptp2) | occurrence_of(sK15(X0), tptp1) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0] : ((leaf_occ(sK15(X0), X0) & next_subocc(sK14(X0), sK15(X0), tptp0) & (occurrence_of(sK15(X0), tptp2) | occurrence_of(sK15(X0), tptp1)) & next_subocc(sK13(X0), sK14(X0), tptp0) & occurrence_of(sK14(X0), tptp4) & root_occ(sK13(X0), X0) & occurrence_of(sK13(X0), tptp3)) | ~ occurrence_of(X0, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14, sK15])], [f129, f169])).
fof(f169, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) => (leaf_occ(sK15(X0), X0) & next_subocc(sK14(X0), sK15(X0), tptp0) & (occurrence_of(sK15(X0), tptp2) | occurrence_of(sK15(X0), tptp1)) & next_subocc(sK13(X0), sK14(X0), tptp0) & occurrence_of(sK14(X0), tptp4) & root_occ(sK13(X0), X0) & occurrence_of(sK13(X0), tptp3))), introduced(choice_axiom, [])).
fof(f129, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) | ~ occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3))), inference(rectify, [], [f33])).
fof(f33, plain, ! [X101] : (occurrence_of(X101, tptp0) => ? [X102, X103, X104] : (leaf_occ(X104, X101) & next_subocc(X103, X104, tptp0) & (occurrence_of(X104, tptp2) | occurrence_of(X104, tptp1)) & next_subocc(X102, X103, tptp0) & occurrence_of(X103, tptp4) & root_occ(X102, X101) & occurrence_of(X102, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_32)).
fof(f2551, plain, (occurrence_of(sK15(sK17), tptp2) | ~ spl19_32), inference(avatar_component_clause, [], [f2549])).
fof(f195680, plain, (~ spl19_440 | ~ spl19_1854), inference(avatar_contradiction_clause, [], [f195679])).
fof(f195679, plain, ($false | (~ spl19_440 | ~ spl19_1854)), inference(subsumption_resolution, [], [f195678, f247])).
fof(f247, plain, ~ (tptp4 = tptp2), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ~ (tptp4 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_41)).
fof(f195678, plain, ((tptp4 = tptp2) | (~ spl19_440 | ~ spl19_1854)), inference(forward_demodulation, [], [f195377, f694])).
fof(f694, plain, (tptp4 = sK5(sK14(sK17))), inference(resolution, [], [f374, f255])).
fof(f374, plain, ! [X11] : (~ occurrence_of(X11, tptp0) | (tptp4 = sK5(sK14(X11)))), inference(resolution, [], [f302, f235])).
fof(f235, plain, ! [X0] : (occurrence_of(sK14(X0), tptp4) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f195377, plain, ((tptp2 = sK5(sK14(sK17))) | (~ spl19_440 | ~ spl19_1854)), inference(backward_demodulation, [], [f170806, f171932])).
fof(f171932, plain, ((sK14(sK17) = sK16(sK13(sK17), sK17)) | ~ spl19_1854), inference(avatar_component_clause, [], [f171930])).
fof(f171930, plain, (spl19_1854 <=> (sK14(sK17) = sK16(sK13(sK17), sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_1854])])).
fof(f170806, plain, ((tptp2 = sK5(sK16(sK13(sK17), sK17))) | ~ spl19_440), inference(resolution, [], [f21459, f302])).
fof(f21459, plain, (occurrence_of(sK16(sK13(sK17), sK17), tptp2) | ~ spl19_440), inference(avatar_component_clause, [], [f21457])).
fof(f21457, plain, (spl19_440 <=> occurrence_of(sK16(sK13(sK17), sK17), tptp2)), introduced(avatar_definition, [new_symbols(naming, [spl19_440])])).
fof(f195234, plain, (~ spl19_1838 | ~ spl19_1991), inference(avatar_contradiction_clause, [], [f195233])).
fof(f195233, plain, ($false | (~ spl19_1838 | ~ spl19_1991)), inference(subsumption_resolution, [], [f195232, f255])).
fof(f195232, plain, (~ occurrence_of(sK17, tptp0) | (~ spl19_1838 | ~ spl19_1991)), inference(subsumption_resolution, [], [f195143, f170293])).
fof(f170293, plain, (precedes(sK16(sK13(sK17), sK17), sK15(sK17)) | ~ spl19_1838), inference(avatar_component_clause, [], [f170291])).
fof(f170291, plain, (spl19_1838 <=> precedes(sK16(sK13(sK17), sK17), sK15(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_1838])])).
fof(f195143, plain, (~ precedes(sK16(sK13(sK17), sK17), sK15(sK17)) | ~ occurrence_of(sK17, tptp0) | ~ spl19_1991), inference(resolution, [], [f194945, f671])).
fof(f671, plain, ! [X23, X22] : (~ min_precedes(sK14(X22), X23, tptp0) | ~ precedes(X23, sK15(X22)) | ~ occurrence_of(X22, tptp0)), inference(subsumption_resolution, [], [f664, f438])).
fof(f438, plain, ! [X2, X3] : (~ min_precedes(sK14(X2), X3, tptp0) | ~ min_precedes(X3, sK15(X2), tptp0) | ~ occurrence_of(X2, tptp0)), inference(resolution, [], [f225, f238])).
fof(f238, plain, ! [X0] : (next_subocc(sK14(X0), sK15(X0), tptp0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f225, plain, ! [X4, X2, X0, X1] : (~ next_subocc(X0, X1, X2) | ~ min_precedes(X0, X4, X2) | ~ min_precedes(X4, X1, X2)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK12(X0, X1, X2), X1, X2) & min_precedes(X0, sK12(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f166, f167])).
fof(f167, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK12(X0, X1, X2), X1, X2) & min_precedes(X0, sK12(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f166, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f164])).
fof(f164, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f27])).
fof(f27, plain, ! [X78, X79, X80] : (next_subocc(X78, X79, X80) <=> (~ ? [X81] : (min_precedes(X81, X79, X80) & min_precedes(X78, X81, X80)) & min_precedes(X78, X79, X80))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_26)).
fof(f664, plain, ! [X23, X22] : (~ occurrence_of(X22, tptp0) | ~ precedes(X23, sK15(X22)) | min_precedes(X23, sK15(X22), tptp0) | ~ min_precedes(sK14(X22), X23, tptp0)), inference(resolution, [], [f314, f232])).
fof(f232, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X0, X2, X3) | ~ precedes(X1, X2) | min_precedes(X1, X2, X3) | ~ min_precedes(X0, X1, X3)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0, X1, X2, X3] : (min_precedes(X1, X2, X3) | ~ precedes(X1, X2) | ~ min_precedes(X0, X2, X3) | ~ min_precedes(X0, X1, X3)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0, X1, X2, X3] : (min_precedes(X1, X2, X3) | (~ precedes(X1, X2) | ~ min_precedes(X0, X2, X3) | ~ min_precedes(X0, X1, X3))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1, X2, X3] : ((precedes(X1, X2) & min_precedes(X0, X2, X3) & min_precedes(X0, X1, X3)) => min_precedes(X1, X2, X3)), inference(rectify, [], [f32])).
fof(f32, plain, ! [X97, X98, X99, X100] : ((precedes(X98, X99) & min_precedes(X97, X99, X100) & min_precedes(X97, X98, X100)) => min_precedes(X98, X99, X100)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_31)).
fof(f314, plain, ! [X0] : (min_precedes(sK14(X0), sK15(X0), tptp0) | ~ occurrence_of(X0, tptp0)), inference(resolution, [], [f238, f224])).
fof(f224, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f168])).
fof(f194945, plain, (min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0) | ~ spl19_1991), inference(avatar_component_clause, [], [f194943])).
fof(f194943, plain, (spl19_1991 <=> min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_1991])])).
fof(f194946, plain, (spl19_1854 | spl19_1991 | ~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_439 | ~ spl19_1806 | ~ spl19_1834), inference(avatar_split_clause, [], [f194941, f170272, f168758, f21450, f16887, f5085, f445, f194943, f171930])).
fof(f445, plain, (spl19_5 <=> root_occ(sK13(sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_5])])).
fof(f5085, plain, (spl19_77 <=> (sK13(sK17) = sK15(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_77])])).
fof(f16887, plain, (spl19_292 <=> next_subocc(sK13(sK17), sK14(sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_292])])).
fof(f21450, plain, (spl19_439 <=> subactivity_occurrence(sK16(sK13(sK17), sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_439])])).
fof(f168758, plain, (spl19_1806 <=> ! [X1] : (~ arboreal(X1) | min_precedes(X1, sK16(X1, sK17), tptp0) | ~ occurrence_of(X1, tptp3) | ~ root_occ(X1, sK17) | (sK15(sK17) = X1))), introduced(avatar_definition, [new_symbols(naming, [spl19_1806])])).
fof(f170272, plain, (spl19_1834 <=> arboreal(sK16(sK13(sK17), sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_1834])])).
fof(f194941, plain, (min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0) | (sK14(sK17) = sK16(sK13(sK17), sK17)) | (~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_439 | ~ spl19_1806 | ~ spl19_1834)), inference(subsumption_resolution, [], [f194940, f170273])).
fof(f170273, plain, (arboreal(sK16(sK13(sK17), sK17)) | ~ spl19_1834), inference(avatar_component_clause, [], [f170272])).
fof(f194940, plain, (min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0) | (sK14(sK17) = sK16(sK13(sK17), sK17)) | ~ arboreal(sK16(sK13(sK17), sK17)) | (~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_439 | ~ spl19_1806)), inference(subsumption_resolution, [], [f194939, f21452])).
fof(f21452, plain, (subactivity_occurrence(sK16(sK13(sK17), sK17), sK17) | ~ spl19_439), inference(avatar_component_clause, [], [f21450])).
fof(f194939, plain, (min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0) | (sK14(sK17) = sK16(sK13(sK17), sK17)) | ~ subactivity_occurrence(sK16(sK13(sK17), sK17), sK17) | ~ arboreal(sK16(sK13(sK17), sK17)) | (~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_1806)), inference(subsumption_resolution, [], [f194938, f255])).
fof(f194938, plain, (min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0) | ~ occurrence_of(sK17, tptp0) | (sK14(sK17) = sK16(sK13(sK17), sK17)) | ~ subactivity_occurrence(sK16(sK13(sK17), sK17), sK17) | ~ arboreal(sK16(sK13(sK17), sK17)) | (~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_1806)), inference(duplicate_literal_removal, [], [f194935])).
fof(f194935, plain, (min_precedes(sK14(sK17), sK16(sK13(sK17), sK17), tptp0) | ~ occurrence_of(sK17, tptp0) | (sK14(sK17) = sK16(sK13(sK17), sK17)) | ~ subactivity_occurrence(sK16(sK13(sK17), sK17), sK17) | ~ arboreal(sK16(sK13(sK17), sK17)) | ~ occurrence_of(sK17, tptp0) | (~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_1806)), inference(resolution, [], [f169545, f1858])).
fof(f1858, plain, ! [X2, X3, X1] : (min_precedes(sK14(X1), X2, X3) | min_precedes(X2, sK14(X1), X3) | ~ occurrence_of(X1, tptp0) | (sK14(X1) = X2) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X2) | ~ occurrence_of(X1, X3)), inference(subsumption_resolution, [], [f1850, f280])).
fof(f280, plain, ! [X0] : (~ occurrence_of(X0, tptp0) | arboreal(sK14(X0))), inference(subsumption_resolution, [], [f277, f241])).
fof(f241, plain, atomic(tptp4), inference(cnf_transformation, [], [f36])).
fof(f36, plain, atomic(tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_35)).
fof(f277, plain, ! [X0] : (~ occurrence_of(X0, tptp0) | ~ atomic(tptp4) | arboreal(sK14(X0))), inference(resolution, [], [f235, f204])).
fof(f204, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f111])).
fof(f111, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f17])).
fof(f17, plain, ! [X51, X52] : (occurrence_of(X51, X52) => (arboreal(X51) <=> atomic(X52))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_16)).
fof(f1850, plain, ! [X2, X3, X1] : (~ occurrence_of(X1, tptp0) | min_precedes(sK14(X1), X2, X3) | min_precedes(X2, sK14(X1), X3) | (sK14(X1) = X2) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(sK14(X1)) | ~ arboreal(X2) | ~ occurrence_of(X1, X3)), inference(resolution, [], [f1846, f183])).
fof(f183, plain, ! [X2, X0, X3, X1] : (~ subactivity_occurrence(X3, X1) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0) | (X2 = X3) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X3) | ~ arboreal(X2) | ~ occurrence_of(X1, X0)), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ! [X0, X1, X2, X3] : ((X2 = X3) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0) | ~ subactivity_occurrence(X3, X1) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X3) | ~ arboreal(X2) | ~ occurrence_of(X1, X0)), inference(flattening, [], [f94])).
fof(f94, plain, ! [X0, X1, X2, X3] : (((X2 = X3) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0)) | (~ subactivity_occurrence(X3, X1) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X3) | ~ arboreal(X2) | ~ occurrence_of(X1, X0))), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0, X1, X2, X3] : ((subactivity_occurrence(X3, X1) & subactivity_occurrence(X2, X1) & arboreal(X3) & arboreal(X2) & occurrence_of(X1, X0)) => ((X2 = X3) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0))), inference(rectify, [], [f5])).
fof(f5, plain, ! [X14, X15, X16, X17] : ((subactivity_occurrence(X17, X15) & subactivity_occurrence(X16, X15) & arboreal(X17) & arboreal(X16) & occurrence_of(X15, X14)) => ((X16 = X17) | min_precedes(X17, X16, X14) | min_precedes(X16, X17, X14))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_04)).
fof(f1846, plain, ! [X0] : (subactivity_occurrence(sK14(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(duplicate_literal_removal, [], [f1840])).
fof(f1840, plain, ! [X0] : (~ occurrence_of(X0, tptp0) | ~ occurrence_of(X0, tptp0) | subactivity_occurrence(sK14(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(resolution, [], [f663, f282])).
fof(f282, plain, ! [X0] : (subactivity_occurrence(sK15(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(resolution, [], [f239, f207])).
fof(f207, plain, ! [X0, X1] : (~ leaf_occ(X0, X1) | subactivity_occurrence(X0, X1)), inference(cnf_transformation, [], [f155])).
fof(f155, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & ((leaf(X0, sK9(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK9(X0, X1))) | ~ leaf_occ(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f153, f154])).
fof(f154, plain, ! [X1, X0] : (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) => (leaf(X0, sK9(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f153, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) | ~ leaf_occ(X0, X1))), inference(rectify, [], [f152])).
fof(f152, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2)) | ~ leaf_occ(X0, X1))), inference(nnf_transformation, [], [f65])).
fof(f65, plain, ! [X0, X1] : (leaf_occ(X0, X1) <=> ? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2))), inference(rectify, [], [f19])).
fof(f19, plain, ! [X55, X56] : (leaf_occ(X55, X56) <=> ? [X57] : (leaf(X55, X57) & subactivity_occurrence(X55, X56) & occurrence_of(X56, X57))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_18)).
fof(f239, plain, ! [X0] : (leaf_occ(sK15(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f663, plain, ! [X21, X20] : (~ subactivity_occurrence(sK15(X20), X21) | ~ occurrence_of(X20, tptp0) | ~ occurrence_of(X21, tptp0) | subactivity_occurrence(sK14(X20), X21)), inference(resolution, [], [f314, f228])).
fof(f228, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X0, X1, X2) | ~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | subactivity_occurrence(X0, X3)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0, X1, X2, X3] : (subactivity_occurrence(X0, X3) | ~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | ~ min_precedes(X0, X1, X2)), inference(flattening, [], [f119])).
fof(f119, plain, ! [X0, X1, X2, X3] : (subactivity_occurrence(X0, X3) | (~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | ~ min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0, X1, X2, X3] : ((subactivity_occurrence(X1, X3) & occurrence_of(X3, X2) & min_precedes(X0, X1, X2)) => subactivity_occurrence(X0, X3)), inference(rectify, [], [f28])).
fof(f28, plain, ! [X82, X83, X84, X85] : ((subactivity_occurrence(X83, X85) & occurrence_of(X85, X84) & min_precedes(X82, X83, X84)) => subactivity_occurrence(X82, X85)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_27)).
fof(f169545, plain, (~ min_precedes(sK16(sK13(sK17), sK17), sK14(sK17), tptp0) | (~ spl19_5 | spl19_77 | ~ spl19_292 | ~ spl19_1806)), inference(subsumption_resolution, [], [f169544, f5086])).
fof(f5086, plain, (~ (sK13(sK17) = sK15(sK17)) | spl19_77), inference(avatar_component_clause, [], [f5085])).
fof(f169544, plain, ((sK13(sK17) = sK15(sK17)) | ~ min_precedes(sK16(sK13(sK17), sK17), sK14(sK17), tptp0) | (~ spl19_5 | ~ spl19_292 | ~ spl19_1806)), inference(subsumption_resolution, [], [f169543, f446])).
fof(f446, plain, (root_occ(sK13(sK17), sK17) | ~ spl19_5), inference(avatar_component_clause, [], [f445])).
fof(f169543, plain, (~ root_occ(sK13(sK17), sK17) | (sK13(sK17) = sK15(sK17)) | ~ min_precedes(sK16(sK13(sK17), sK17), sK14(sK17), tptp0) | (~ spl19_5 | ~ spl19_292 | ~ spl19_1806)), inference(subsumption_resolution, [], [f169542, f611])).
fof(f611, plain, (occurrence_of(sK13(sK17), tptp3) | ~ spl19_5), inference(backward_demodulation, [], [f552, f609])).
fof(f609, plain, (tptp3 = sK5(sK13(sK17))), inference(resolution, [], [f373, f255])).
fof(f373, plain, ! [X10] : (~ occurrence_of(X10, tptp0) | (tptp3 = sK5(sK13(X10)))), inference(resolution, [], [f302, f233])).
fof(f233, plain, ! [X0] : (occurrence_of(sK13(X0), tptp3) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f552, plain, (occurrence_of(sK13(sK17), sK5(sK13(sK17))) | ~ spl19_5), inference(backward_demodulation, [], [f535, f546])).
fof(f546, plain, ((sK6(sK13(sK17)) = sK5(sK13(sK17))) | ~ spl19_5), inference(resolution, [], [f535, f302])).
fof(f535, plain, (occurrence_of(sK13(sK17), sK6(sK13(sK17))) | ~ spl19_5), inference(resolution, [], [f462, f198])).
fof(f198, plain, ! [X0, X1] : (~ atocc(X0, X1) | occurrence_of(X0, sK6(X0))), inference(cnf_transformation, [], [f144])).
fof(f144, plain, ! [X0, X1] : ((occurrence_of(X0, sK6(X0)) & atomic(sK6(X0))) | ~ atocc(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f109, f143])).
fof(f143, plain, ! [X0] : (? [X2] : (occurrence_of(X0, X2) & atomic(X2)) => (occurrence_of(X0, sK6(X0)) & atomic(sK6(X0)))), introduced(choice_axiom, [])).
fof(f109, plain, ! [X0, X1] : (? [X2] : (occurrence_of(X0, X2) & atomic(X2)) | ~ atocc(X0, X1)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0, X1] : (atocc(X0, X1) => ? [X2] : (occurrence_of(X0, X2) & atomic(X2))), inference(pure_predicate_removal, [], [f81])).
fof(f81, plain, ! [X0, X1] : (atocc(X0, X1) => ? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2))), inference(unused_predicate_definition_removal, [], [f61])).
fof(f61, plain, ! [X0, X1] : (atocc(X0, X1) <=> ? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2))), inference(rectify, [], [f15])).
fof(f15, plain, ! [X44, X45] : (atocc(X44, X45) <=> ? [X46] : (occurrence_of(X44, X46) & atomic(X46) & subactivity(X45, X46))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_14)).
fof(f462, plain, (atocc(sK13(sK17), sK2(sK13(sK17))) | ~ spl19_5), inference(resolution, [], [f446, f292])).
fof(f292, plain, ! [X0, X1] : (~ root_occ(X0, X1) | atocc(X0, sK2(X0))), inference(resolution, [], [f212, f184])).
fof(f184, plain, ! [X0, X1] : (~ root(X1, X0) | atocc(X1, sK2(X1))), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ! [X0, X1] : (atocc(X1, sK2(X1)) | ~ root(X1, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f96, f135])).
fof(f135, plain, ! [X1] : (? [X2] : atocc(X1, X2) => atocc(X1, sK2(X1))), introduced(choice_axiom, [])).
fof(f96, plain, ! [X0, X1] : (? [X2] : atocc(X1, X2) | ~ root(X1, X0)), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0, X1] : (root(X1, X0) => ? [X2] : atocc(X1, X2)), inference(pure_predicate_removal, [], [f52])).
fof(f52, plain, ! [X0, X1] : (root(X1, X0) => ? [X2] : (atocc(X1, X2) & subactivity(X2, X0))), inference(rectify, [], [f6])).
fof(f6, plain, ! [X18, X19] : (root(X19, X18) => ? [X20] : (atocc(X19, X20) & subactivity(X20, X18))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_05)).
fof(f212, plain, ! [X0, X1] : (root(X0, sK10(X0, X1)) | ~ root_occ(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0, X1] : ((root_occ(X0, X1) | ! [X2] : (~ root(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & ((root(X0, sK10(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK10(X0, X1))) | ~ root_occ(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f157, f158])).
fof(f158, plain, ! [X1, X0] : (? [X3] : (root(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) => (root(X0, sK10(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK10(X0, X1)))), introduced(choice_axiom, [])).
fof(f157, plain, ! [X0, X1] : ((root_occ(X0, X1) | ! [X2] : (~ root(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X3] : (root(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) | ~ root_occ(X0, X1))), inference(rectify, [], [f156])).
fof(f156, plain, ! [X0, X1] : ((root_occ(X0, X1) | ! [X2] : (~ root(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X2] : (root(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2)) | ~ root_occ(X0, X1))), inference(nnf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1] : (root_occ(X0, X1) <=> ? [X2] : (root(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2))), inference(rectify, [], [f20])).
fof(f20, plain, ! [X58, X59] : (root_occ(X58, X59) <=> ? [X60] : (root(X58, X60) & subactivity_occurrence(X58, X59) & occurrence_of(X59, X60))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_19)).
fof(f169542, plain, (~ occurrence_of(sK13(sK17), tptp3) | ~ root_occ(sK13(sK17), sK17) | (sK13(sK17) = sK15(sK17)) | ~ min_precedes(sK16(sK13(sK17), sK17), sK14(sK17), tptp0) | (~ spl19_292 | ~ spl19_1806)), inference(subsumption_resolution, [], [f169382, f281])).
fof(f281, plain, arboreal(sK13(sK17)), inference(resolution, [], [f274, f255])).
fof(f274, plain, ! [X0] : (~ occurrence_of(X0, tptp0) | arboreal(sK13(X0))), inference(subsumption_resolution, [], [f271, f244])).
fof(f244, plain, atomic(tptp3), inference(cnf_transformation, [], [f39])).
fof(f39, plain, atomic(tptp3), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_38)).
fof(f271, plain, ! [X0] : (~ occurrence_of(X0, tptp0) | ~ atomic(tptp3) | arboreal(sK13(X0))), inference(resolution, [], [f233, f204])).
fof(f169382, plain, (~ arboreal(sK13(sK17)) | ~ occurrence_of(sK13(sK17), tptp3) | ~ root_occ(sK13(sK17), sK17) | (sK13(sK17) = sK15(sK17)) | ~ min_precedes(sK16(sK13(sK17), sK17), sK14(sK17), tptp0) | (~ spl19_292 | ~ spl19_1806)), inference(resolution, [], [f168759, f24675])).
fof(f24675, plain, (! [X0] : (~ min_precedes(sK13(sK17), X0, tptp0) | ~ min_precedes(X0, sK14(sK17), tptp0)) | ~ spl19_292), inference(resolution, [], [f16889, f225])).
fof(f16889, plain, (next_subocc(sK13(sK17), sK14(sK17), tptp0) | ~ spl19_292), inference(avatar_component_clause, [], [f16887])).
fof(f168759, plain, (! [X1] : (min_precedes(X1, sK16(X1, sK17), tptp0) | ~ arboreal(X1) | ~ occurrence_of(X1, tptp3) | ~ root_occ(X1, sK17) | (sK15(sK17) = X1)) | ~ spl19_1806), inference(avatar_component_clause, [], [f168758])).
fof(f170851, plain, (~ spl19_119 | ~ spl19_440 | ~ spl19_499 | ~ spl19_1543 | ~ spl19_1839), inference(avatar_contradiction_clause, [], [f170850])).
fof(f170850, plain, ($false | (~ spl19_119 | ~ spl19_440 | ~ spl19_499 | ~ spl19_1543 | ~ spl19_1839)), inference(subsumption_resolution, [], [f170849, f250])).
fof(f170849, plain, ((tptp1 = tptp2) | (~ spl19_119 | ~ spl19_440 | ~ spl19_499 | ~ spl19_1543 | ~ spl19_1839)), inference(forward_demodulation, [], [f170838, f169111])).
fof(f169111, plain, ((tptp1 = sK5(sK15(sK17))) | (~ spl19_119 | ~ spl19_499 | ~ spl19_1543)), inference(forward_demodulation, [], [f169110, f25233])).
fof(f169110, plain, ((tptp1 = sK5(sK15(sK3(tptp0, sK13(sK17), sK15(sK17))))) | (~ spl19_119 | ~ spl19_1543)), inference(forward_demodulation, [], [f9728, f117412])).
fof(f9728, plain, ((tptp1 = sK5(sK15(sK3(tptp0, sK11(sK15(sK17), tptp0), sK15(sK17))))) | ~ spl19_119), inference(avatar_component_clause, [], [f9726])).
fof(f170838, plain, ((tptp2 = sK5(sK15(sK17))) | (~ spl19_440 | ~ spl19_1839)), inference(backward_demodulation, [], [f170806, f170297])).
fof(f170297, plain, ((sK15(sK17) = sK16(sK13(sK17), sK17)) | ~ spl19_1839), inference(avatar_component_clause, [], [f170295])).
fof(f170295, plain, (spl19_1839 <=> (sK15(sK17) = sK16(sK13(sK17), sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_1839])])).
fof(f170815, plain, (spl19_1834 | ~ spl19_440), inference(avatar_split_clause, [], [f170811, f21457, f170272])).
fof(f170811, plain, (arboreal(sK16(sK13(sK17), sK17)) | ~ spl19_440), inference(subsumption_resolution, [], [f170805, f243])).
fof(f243, plain, atomic(tptp2), inference(cnf_transformation, [], [f38])).
fof(f38, plain, atomic(tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_37)).
fof(f170805, plain, (~ atomic(tptp2) | arboreal(sK16(sK13(sK17), sK17)) | ~ spl19_440), inference(resolution, [], [f21459, f204])).
fof(f170298, plain, (spl19_1838 | spl19_7 | ~ spl19_1834 | spl19_1839 | ~ spl19_439), inference(avatar_split_clause, [], [f170289, f21450, f170295, f170272, f467, f170291])).
fof(f467, plain, (spl19_7 <=> ! [X1] : ~ occurrence_of(sK17, X1)), introduced(avatar_definition, [new_symbols(naming, [spl19_7])])).
fof(f170289, plain, (! [X6] : ((sK15(sK17) = sK16(sK13(sK17), sK17)) | ~ arboreal(sK16(sK13(sK17), sK17)) | ~ occurrence_of(sK17, X6) | precedes(sK16(sK13(sK17), sK17), sK15(sK17))) | ~ spl19_439), inference(subsumption_resolution, [], [f170224, f255])).
fof(f170224, plain, (! [X6] : ((sK15(sK17) = sK16(sK13(sK17), sK17)) | ~ arboreal(sK16(sK13(sK17), sK17)) | ~ occurrence_of(sK17, X6) | ~ occurrence_of(sK17, tptp0) | precedes(sK16(sK13(sK17), sK17), sK15(sK17))) | ~ spl19_439), inference(resolution, [], [f21452, f1001])).
fof(f1001, plain, ! [X30, X31, X29] : (~ subactivity_occurrence(X29, X30) | (sK15(X30) = X29) | ~ arboreal(X29) | ~ occurrence_of(X30, X31) | ~ occurrence_of(X30, tptp0) | precedes(X29, sK15(X30))), inference(resolution, [], [f532, f221])).
fof(f221, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | precedes(X0, X1)), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1, X2] : (precedes(X0, X1) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => precedes(X0, X1)), inference(rectify, [], [f25])).
fof(f25, plain, ! [X72, X73, X74] : (min_precedes(X72, X73, X74) => precedes(X72, X73)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_24)).
fof(f532, plain, ! [X2, X0, X1] : (min_precedes(X0, sK15(X1), X2) | ~ arboreal(X0) | (sK15(X1) = X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2) | ~ occurrence_of(X1, tptp0)), inference(resolution, [], [f181, f239])).
fof(f181, plain, ! [X2, X0, X3, X1] : (~ leaf_occ(X3, X1) | min_precedes(X2, X3, X0) | ~ arboreal(X2) | (X2 = X3) | ~ subactivity_occurrence(X2, X1) | ~ occurrence_of(X1, X0)), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ! [X0, X1, X2, X3] : ((X2 = X3) | min_precedes(X2, X3, X0) | ~ arboreal(X2) | ~ leaf_occ(X3, X1) | ~ subactivity_occurrence(X2, X1) | ~ occurrence_of(X1, X0)), inference(flattening, [], [f91])).
fof(f91, plain, ! [X0, X1, X2, X3] : ((X2 = X3) | (min_precedes(X2, X3, X0) | ~ arboreal(X2) | ~ leaf_occ(X3, X1) | ~ subactivity_occurrence(X2, X1) | ~ occurrence_of(X1, X0))), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0, X1, X2, X3] : ((~ min_precedes(X2, X3, X0) & arboreal(X2) & leaf_occ(X3, X1) & subactivity_occurrence(X2, X1) & occurrence_of(X1, X0)) => (X2 = X3)), inference(rectify, [], [f3])).
fof(f3, plain, ! [X8, X9, X10, X11] : ((~ min_precedes(X10, X11, X8) & arboreal(X10) & leaf_occ(X11, X9) & subactivity_occurrence(X10, X9) & occurrence_of(X9, X8)) => (X10 = X11)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_02)).
fof(f170069, plain, (spl19_440 | ~ spl19_256), inference(avatar_split_clause, [], [f170065, f16038, f21457])).
fof(f16038, plain, (spl19_256 <=> sP0(sK13(sK17), sK17, sK15(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_256])])).
fof(f170065, plain, (occurrence_of(sK16(sK13(sK17), sK17), tptp2) | ~ spl19_256), inference(resolution, [], [f16040, f252])).
fof(f252, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | occurrence_of(sK16(X0, X1), tptp2)), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ! [X0, X1, X2] : (((min_precedes(X0, sK16(X0, X1), tptp0) & subactivity_occurrence(sK16(X0, X1), X1) & occurrence_of(sK16(X0, X1), tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16])], [f172, f173])).
fof(f173, plain, ! [X1, X0] : (? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) => (min_precedes(X0, sK16(X0, X1), tptp0) & subactivity_occurrence(sK16(X0, X1), X1) & occurrence_of(sK16(X0, X1), tptp2))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0, X1, X2] : ((? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(rectify, [], [f171])).
fof(f171, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(nnf_transformation, [], [f131])).
fof(f16040, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ spl19_256), inference(avatar_component_clause, [], [f16038])).
fof(f170068, plain, (spl19_439 | ~ spl19_256), inference(avatar_split_clause, [], [f170064, f16038, f21450])).
fof(f170064, plain, (subactivity_occurrence(sK16(sK13(sK17), sK17), sK17) | ~ spl19_256), inference(resolution, [], [f16040, f253])).
fof(f253, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | subactivity_occurrence(sK16(X0, X1), X1)), inference(cnf_transformation, [], [f174])).
fof(f169136, plain, (~ spl19_96 | ~ spl19_97 | ~ spl19_309 | spl19_1781 | spl19_1782), inference(avatar_split_clause, [], [f169135, f164868, f164864, f17623, f7431, f7421])).
fof(f7421, plain, (spl19_96 <=> subactivity_occurrence(sK18(sK13(sK17)), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_96])])).
fof(f7431, plain, (spl19_97 <=> min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_97])])).
fof(f17623, plain, (spl19_309 <=> legal(sK18(sK13(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_309])])).
fof(f164864, plain, (spl19_1781 <=> (sK14(sK17) = sK18(sK13(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_1781])])).
fof(f164868, plain, (spl19_1782 <=> min_precedes(sK14(sK17), sK18(sK13(sK17)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_1782])])).
fof(f169135, plain, (~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | (~ spl19_97 | ~ spl19_309 | spl19_1781 | spl19_1782)), inference(subsumption_resolution, [], [f169134, f166504])).
fof(f166504, plain, (arboreal(sK18(sK13(sK17))) | ~ spl19_309), inference(resolution, [], [f17625, f196])).
fof(f196, plain, ! [X0] : (~ legal(X0) | arboreal(X0)), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ! [X0] : (arboreal(X0) | ~ legal(X0)), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : (legal(X0) => arboreal(X0)), inference(rectify, [], [f14])).
fof(f14, plain, ! [X43] : (legal(X43) => arboreal(X43)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_13)).
fof(f17625, plain, (legal(sK18(sK13(sK17))) | ~ spl19_309), inference(avatar_component_clause, [], [f17623])).
fof(f169134, plain, (~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ arboreal(sK18(sK13(sK17))) | (~ spl19_97 | spl19_1781 | spl19_1782)), inference(subsumption_resolution, [], [f169133, f164865])).
fof(f164865, plain, (~ (sK14(sK17) = sK18(sK13(sK17))) | spl19_1781), inference(avatar_component_clause, [], [f164864])).
fof(f169133, plain, ((sK14(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ arboreal(sK18(sK13(sK17))) | (~ spl19_97 | spl19_1782)), inference(subsumption_resolution, [], [f164860, f164869])).
fof(f164869, plain, (~ min_precedes(sK14(sK17), sK18(sK13(sK17)), tptp0) | spl19_1782), inference(avatar_component_clause, [], [f164868])).
fof(f164860, plain, (min_precedes(sK14(sK17), sK18(sK13(sK17)), tptp0) | (sK14(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ arboreal(sK18(sK13(sK17))) | ~ spl19_97), inference(subsumption_resolution, [], [f164717, f255])).
fof(f164717, plain, (min_precedes(sK14(sK17), sK18(sK13(sK17)), tptp0) | ~ occurrence_of(sK17, tptp0) | (sK14(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ arboreal(sK18(sK13(sK17))) | ~ spl19_97), inference(duplicate_literal_removal, [], [f164529])).
fof(f164529, plain, (min_precedes(sK14(sK17), sK18(sK13(sK17)), tptp0) | ~ occurrence_of(sK17, tptp0) | (sK14(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ arboreal(sK18(sK13(sK17))) | ~ occurrence_of(sK17, tptp0) | ~ spl19_97), inference(resolution, [], [f1858, f17596])).
fof(f17596, plain, (~ min_precedes(sK18(sK13(sK17)), sK14(sK17), tptp0) | ~ spl19_97), inference(subsumption_resolution, [], [f17559, f255])).
fof(f17559, plain, (~ min_precedes(sK18(sK13(sK17)), sK14(sK17), tptp0) | ~ occurrence_of(sK17, tptp0) | ~ spl19_97), inference(resolution, [], [f7433, f437])).
fof(f437, plain, ! [X0, X1] : (~ min_precedes(sK13(X0), X1, tptp0) | ~ min_precedes(X1, sK14(X0), tptp0) | ~ occurrence_of(X0, tptp0)), inference(resolution, [], [f225, f236])).
fof(f236, plain, ! [X0] : (next_subocc(sK13(X0), sK14(X0), tptp0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f7433, plain, (min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0) | ~ spl19_97), inference(avatar_component_clause, [], [f7431])).
fof(f169006, plain, (spl19_256 | ~ spl19_1 | ~ spl19_5 | spl19_32 | ~ spl19_1543), inference(avatar_split_clause, [], [f169005, f117410, f2549, f445, f381, f16038])).
fof(f381, plain, (spl19_1 <=> leaf_occ(sK15(sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_1])])).
fof(f169005, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | (~ spl19_1 | ~ spl19_5 | spl19_32 | ~ spl19_1543)), inference(subsumption_resolution, [], [f169004, f611])).
fof(f169004, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_5 | spl19_32 | ~ spl19_1543)), inference(subsumption_resolution, [], [f169003, f446])).
fof(f169003, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | spl19_32 | ~ spl19_1543)), inference(subsumption_resolution, [], [f169002, f2550])).
fof(f2550, plain, (~ occurrence_of(sK15(sK17), tptp2) | spl19_32), inference(avatar_component_clause, [], [f2549])).
fof(f169002, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_1543)), inference(subsumption_resolution, [], [f169001, f382])).
fof(f382, plain, (leaf_occ(sK15(sK17), sK17) | ~ spl19_1), inference(avatar_component_clause, [], [f381])).
fof(f169001, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ leaf_occ(sK15(sK17), sK17) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | ~ spl19_1543), inference(subsumption_resolution, [], [f147568, f255])).
fof(f147568, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ occurrence_of(sK17, tptp0) | ~ leaf_occ(sK15(sK17), sK17) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | ~ spl19_1543), inference(superposition, [], [f1694, f117412])).
fof(f1694, plain, ! [X0] : (sP0(sK11(sK15(X0), tptp0), sK17, sK15(X0)) | ~ occurrence_of(X0, tptp0) | ~ leaf_occ(sK15(X0), sK17) | occurrence_of(sK15(X0), tptp2) | ~ root_occ(sK11(sK15(X0), tptp0), sK17) | ~ occurrence_of(sK11(sK15(X0), tptp0), tptp3)), inference(subsumption_resolution, [], [f1672, f237])).
fof(f1672, plain, ! [X0] : (~ occurrence_of(X0, tptp0) | sP0(sK11(sK15(X0), tptp0), sK17, sK15(X0)) | ~ leaf_occ(sK15(X0), sK17) | occurrence_of(sK15(X0), tptp2) | ~ occurrence_of(sK15(X0), tptp1) | ~ root_occ(sK11(sK15(X0), tptp0), sK17) | ~ occurrence_of(sK11(sK15(X0), tptp0), tptp3)), inference(resolution, [], [f661, f256])).
fof(f256, plain, ! [X2, X1] : (~ min_precedes(X1, X2, tptp0) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17) | occurrence_of(X2, tptp2) | ~ occurrence_of(X2, tptp1) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3)), inference(cnf_transformation, [], [f177])).
fof(f661, plain, ! [X18] : (min_precedes(sK11(sK15(X18), tptp0), sK15(X18), tptp0) | ~ occurrence_of(X18, tptp0)), inference(resolution, [], [f314, f220])).
fof(f220, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | min_precedes(sK11(X1, X2), X1, X2)), inference(cnf_transformation, [], [f163])).
fof(f163, plain, ! [X0, X1, X2] : ((min_precedes(sK11(X1, X2), X1, X2) & root(sK11(X1, X2), X2)) | ~ min_precedes(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f115, f162])).
fof(f162, plain, ! [X2, X1] : (? [X3] : (min_precedes(X3, X1, X2) & root(X3, X2)) => (min_precedes(sK11(X1, X2), X1, X2) & root(sK11(X1, X2), X2))), introduced(choice_axiom, [])).
fof(f115, plain, ! [X0, X1, X2] : (? [X3] : (min_precedes(X3, X1, X2) & root(X3, X2)) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f70])).
fof(f70, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => ? [X3] : (min_precedes(X3, X1, X2) & root(X3, X2))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X68, X69, X70] : (min_precedes(X68, X69, X70) => ? [X71] : (min_precedes(X71, X69, X70) & root(X71, X70))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_23)).
fof(f168994, plain, (spl19_256 | ~ spl19_1 | ~ spl19_5 | spl19_95 | ~ spl19_1543), inference(avatar_split_clause, [], [f168993, f117410, f7411, f445, f381, f16038])).
fof(f7411, plain, (spl19_95 <=> occurrence_of(sK18(sK13(sK17)), tptp1)), introduced(avatar_definition, [new_symbols(naming, [spl19_95])])).
fof(f168993, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | (~ spl19_1 | ~ spl19_5 | spl19_95 | ~ spl19_1543)), inference(subsumption_resolution, [], [f168992, f611])).
fof(f168992, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_5 | spl19_95 | ~ spl19_1543)), inference(subsumption_resolution, [], [f168991, f446])).
fof(f168991, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | spl19_95 | ~ spl19_1543)), inference(subsumption_resolution, [], [f168990, f7412])).
fof(f7412, plain, (~ occurrence_of(sK18(sK13(sK17)), tptp1) | spl19_95), inference(avatar_component_clause, [], [f7411])).
fof(f168990, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | occurrence_of(sK18(sK13(sK17)), tptp1) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_1543)), inference(subsumption_resolution, [], [f168989, f382])).
fof(f168989, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ leaf_occ(sK15(sK17), sK17) | occurrence_of(sK18(sK13(sK17)), tptp1) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | ~ spl19_1543), inference(subsumption_resolution, [], [f147569, f255])).
fof(f147569, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ occurrence_of(sK17, tptp0) | ~ leaf_occ(sK15(sK17), sK17) | occurrence_of(sK18(sK13(sK17)), tptp1) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | ~ spl19_1543), inference(superposition, [], [f1695, f117412])).
fof(f1695, plain, ! [X2] : (sP0(sK11(sK15(X2), tptp0), sK17, sK15(X2)) | ~ occurrence_of(X2, tptp0) | ~ leaf_occ(sK15(X2), sK17) | occurrence_of(sK18(sK11(sK15(X2), tptp0)), tptp1) | ~ root_occ(sK11(sK15(X2), tptp0), sK17) | ~ occurrence_of(sK11(sK15(X2), tptp0), tptp3)), inference(subsumption_resolution, [], [f1674, f1694])).
fof(f1674, plain, ! [X2] : (~ occurrence_of(X2, tptp0) | sP0(sK11(sK15(X2), tptp0), sK17, sK15(X2)) | ~ leaf_occ(sK15(X2), sK17) | occurrence_of(sK18(sK11(sK15(X2), tptp0)), tptp1) | ~ occurrence_of(sK15(X2), tptp2) | ~ root_occ(sK11(sK15(X2), tptp0), sK17) | ~ occurrence_of(sK11(sK15(X2), tptp0), tptp3)), inference(resolution, [], [f661, f259])).
fof(f259, plain, ! [X2, X1] : (~ min_precedes(X1, X2, tptp0) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17) | occurrence_of(sK18(X1), tptp1) | ~ occurrence_of(X2, tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3)), inference(cnf_transformation, [], [f177])).
fof(f168760, plain, (spl19_32 | spl19_1806), inference(avatar_split_clause, [], [f168756, f168758, f2549])).
fof(f168756, plain, ! [X1] : (~ arboreal(X1) | (sK15(sK17) = X1) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3) | min_precedes(X1, sK16(X1, sK17), tptp0)), inference(subsumption_resolution, [], [f168755, f211])).
fof(f211, plain, ! [X0, X1] : (~ root_occ(X0, X1) | subactivity_occurrence(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f168755, plain, ! [X1] : (~ subactivity_occurrence(X1, sK17) | ~ arboreal(X1) | (sK15(sK17) = X1) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3) | min_precedes(X1, sK16(X1, sK17), tptp0)), inference(subsumption_resolution, [], [f24520, f255])).
fof(f24520, plain, ! [X1] : (~ subactivity_occurrence(X1, sK17) | ~ occurrence_of(sK17, tptp0) | ~ arboreal(X1) | (sK15(sK17) = X1) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3) | min_precedes(X1, sK16(X1, sK17), tptp0)), inference(duplicate_literal_removal, [], [f24518])).
fof(f24518, plain, ! [X1] : (~ subactivity_occurrence(X1, sK17) | ~ occurrence_of(sK17, tptp0) | ~ arboreal(X1) | (sK15(sK17) = X1) | occurrence_of(sK15(sK17), tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3) | min_precedes(X1, sK16(X1, sK17), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f4308, f239])).
fof(f4308, plain, ! [X0, X1] : (~ leaf_occ(sK15(X0), sK17) | ~ subactivity_occurrence(X1, X0) | ~ occurrence_of(X0, tptp0) | ~ arboreal(X1) | (sK15(X0) = X1) | occurrence_of(sK15(X0), tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3) | min_precedes(X1, sK16(X1, sK17), tptp0)), inference(resolution, [], [f1026, f254])).
fof(f254, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | min_precedes(X0, sK16(X0, X1), tptp0)), inference(cnf_transformation, [], [f174])).
fof(f1026, plain, ! [X47, X46] : (sP0(X46, sK17, sK15(X47)) | (sK15(X47) = X46) | ~ subactivity_occurrence(X46, X47) | ~ occurrence_of(X47, tptp0) | ~ arboreal(X46) | ~ leaf_occ(sK15(X47), sK17) | occurrence_of(sK15(X47), tptp2) | ~ root_occ(X46, sK17) | ~ occurrence_of(X46, tptp3)), inference(subsumption_resolution, [], [f1024, f237])).
fof(f1024, plain, ! [X47, X46] : (~ arboreal(X46) | (sK15(X47) = X46) | ~ subactivity_occurrence(X46, X47) | ~ occurrence_of(X47, tptp0) | sP0(X46, sK17, sK15(X47)) | ~ leaf_occ(sK15(X47), sK17) | occurrence_of(sK15(X47), tptp2) | ~ occurrence_of(sK15(X47), tptp1) | ~ root_occ(X46, sK17) | ~ occurrence_of(X46, tptp3)), inference(duplicate_literal_removal, [], [f1006])).
fof(f1006, plain, ! [X47, X46] : (~ arboreal(X46) | (sK15(X47) = X46) | ~ subactivity_occurrence(X46, X47) | ~ occurrence_of(X47, tptp0) | ~ occurrence_of(X47, tptp0) | sP0(X46, sK17, sK15(X47)) | ~ leaf_occ(sK15(X47), sK17) | occurrence_of(sK15(X47), tptp2) | ~ occurrence_of(sK15(X47), tptp1) | ~ root_occ(X46, sK17) | ~ occurrence_of(X46, tptp3)), inference(resolution, [], [f532, f256])).
fof(f168505, plain, (~ spl19_1337 | ~ spl19_1782), inference(avatar_contradiction_clause, [], [f168504])).
fof(f168504, plain, ($false | (~ spl19_1337 | ~ spl19_1782)), inference(subsumption_resolution, [], [f168503, f255])).
fof(f168503, plain, (~ occurrence_of(sK17, tptp0) | (~ spl19_1337 | ~ spl19_1782)), inference(subsumption_resolution, [], [f168415, f82176])).
fof(f82176, plain, (min_precedes(sK18(sK13(sK17)), sK15(sK17), tptp0) | ~ spl19_1337), inference(avatar_component_clause, [], [f82175])).
fof(f82175, plain, (spl19_1337 <=> min_precedes(sK18(sK13(sK17)), sK15(sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_1337])])).
fof(f168415, plain, (~ min_precedes(sK18(sK13(sK17)), sK15(sK17), tptp0) | ~ occurrence_of(sK17, tptp0) | ~ spl19_1782), inference(resolution, [], [f164870, f438])).
fof(f164870, plain, (min_precedes(sK14(sK17), sK18(sK13(sK17)), tptp0) | ~ spl19_1782), inference(avatar_component_clause, [], [f164868])).
fof(f165731, plain, (~ spl19_95 | ~ spl19_1781), inference(avatar_contradiction_clause, [], [f165730])).
fof(f165730, plain, ($false | (~ spl19_95 | ~ spl19_1781)), inference(subsumption_resolution, [], [f165729, f246])).
fof(f246, plain, ~ (tptp4 = tptp1), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ~ (tptp4 = tptp1), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_40)).
fof(f165729, plain, ((tptp4 = tptp1) | (~ spl19_95 | ~ spl19_1781)), inference(forward_demodulation, [], [f164930, f694])).
fof(f164930, plain, ((tptp1 = sK5(sK14(sK17))) | (~ spl19_95 | ~ spl19_1781)), inference(backward_demodulation, [], [f16096, f164866])).
fof(f164866, plain, ((sK14(sK17) = sK18(sK13(sK17))) | ~ spl19_1781), inference(avatar_component_clause, [], [f164864])).
fof(f16096, plain, ((tptp1 = sK5(sK18(sK13(sK17)))) | ~ spl19_95), inference(resolution, [], [f7413, f302])).
fof(f7413, plain, (occurrence_of(sK18(sK13(sK17)), tptp1) | ~ spl19_95), inference(avatar_component_clause, [], [f7411])).
fof(f147195, plain, (spl19_1543 | ~ spl19_8 | ~ spl19_69), inference(avatar_split_clause, [], [f147151, f4547, f470, f117410])).
fof(f470, plain, (spl19_8 <=> ! [X0] : ((sK13(sK17) = X0) | ~ root_occ(X0, sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_8])])).
fof(f4547, plain, (spl19_69 <=> root_occ(sK11(sK15(sK17), tptp0), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_69])])).
fof(f147151, plain, ((sK13(sK17) = sK11(sK15(sK17), tptp0)) | (~ spl19_8 | ~ spl19_69)), inference(resolution, [], [f4548, f471])).
fof(f471, plain, (! [X0] : (~ root_occ(X0, sK17) | (sK13(sK17) = X0)) | ~ spl19_8), inference(avatar_component_clause, [], [f470])).
fof(f4548, plain, (root_occ(sK11(sK15(sK17), tptp0), sK17) | ~ spl19_69), inference(avatar_component_clause, [], [f4547])).
fof(f147074, plain, (~ spl19_1 | ~ spl19_2 | spl19_12 | spl19_69), inference(avatar_contradiction_clause, [], [f147073])).
fof(f147073, plain, ($false | (~ spl19_1 | ~ spl19_2 | spl19_12 | spl19_69)), inference(subsumption_resolution, [], [f147072, f408])).
fof(f408, plain, (subactivity_occurrence(sK15(sK17), sK17) | ~ spl19_1), inference(resolution, [], [f382, f207])).
fof(f147072, plain, (~ subactivity_occurrence(sK15(sK17), sK17) | (~ spl19_2 | spl19_12 | spl19_69)), inference(subsumption_resolution, [], [f147071, f754])).
fof(f754, plain, (~ root(sK15(sK17), tptp0) | spl19_12), inference(avatar_component_clause, [], [f753])).
fof(f753, plain, (spl19_12 <=> root(sK15(sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_12])])).
fof(f147071, plain, (root(sK15(sK17), tptp0) | ~ subactivity_occurrence(sK15(sK17), sK17) | (~ spl19_2 | spl19_69)), inference(subsumption_resolution, [], [f147070, f387])).
fof(f387, plain, (leaf(sK15(sK17), tptp0) | ~ spl19_2), inference(avatar_component_clause, [], [f385])).
fof(f385, plain, (spl19_2 <=> leaf(sK15(sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_2])])).
fof(f147070, plain, (~ leaf(sK15(sK17), tptp0) | root(sK15(sK17), tptp0) | ~ subactivity_occurrence(sK15(sK17), sK17) | spl19_69), inference(subsumption_resolution, [], [f147028, f255])).
fof(f147028, plain, (~ occurrence_of(sK17, tptp0) | ~ leaf(sK15(sK17), tptp0) | root(sK15(sK17), tptp0) | ~ subactivity_occurrence(sK15(sK17), sK17) | spl19_69), inference(resolution, [], [f15618, f4549])).
fof(f4549, plain, (~ root_occ(sK11(sK15(sK17), tptp0), sK17) | spl19_69), inference(avatar_component_clause, [], [f4547])).
fof(f15618, plain, ! [X8, X7, X9] : (root_occ(sK11(X7, X9), X8) | ~ occurrence_of(X8, X9) | ~ leaf(X7, X9) | root(X7, X9) | ~ subactivity_occurrence(X7, X8)), inference(duplicate_literal_removal, [], [f15582])).
fof(f15582, plain, ! [X8, X7, X9] : (~ subactivity_occurrence(X7, X8) | ~ occurrence_of(X8, X9) | ~ leaf(X7, X9) | root(X7, X9) | root_occ(sK11(X7, X9), X8) | ~ occurrence_of(X8, X9) | ~ leaf(X7, X9) | root(X7, X9)), inference(resolution, [], [f2047, f401])).
fof(f401, plain, ! [X10, X11] : (root(sK11(X10, X11), X11) | ~ leaf(X10, X11) | root(X10, X11)), inference(resolution, [], [f199, f219])).
fof(f219, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | root(sK11(X1, X2), X2)), inference(cnf_transformation, [], [f163])).
fof(f199, plain, ! [X0, X1] : (min_precedes(sK8(X0, X1), X0, X1) | root(X0, X1) | ~ leaf(X0, X1)), inference(cnf_transformation, [], [f150])).
fof(f150, plain, ! [X0, X1] : ((leaf(X0, X1) | min_precedes(X0, sK7(X0, X1), X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (min_precedes(sK8(X0, X1), X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f147, f149, f148])).
fof(f148, plain, ! [X1, X0] : (? [X2] : min_precedes(X0, X2, X1) => min_precedes(X0, sK7(X0, X1), X1)), introduced(choice_axiom, [])).
fof(f149, plain, ! [X1, X0] : (? [X5] : min_precedes(X5, X0, X1) => min_precedes(sK8(X0, X1), X0, X1)), introduced(choice_axiom, [])).
fof(f147, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (? [X5] : min_precedes(X5, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(rectify, [], [f146])).
fof(f146, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(flattening, [], [f145])).
fof(f145, plain, ! [X0, X1] : ((leaf(X0, X1) | (? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1)))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(nnf_transformation, [], [f110])).
fof(f110, plain, ! [X0, X1] : (leaf(X0, X1) <=> (! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(ennf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1] : (leaf(X0, X1) <=> (~ ? [X2] : min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(rectify, [], [f16])).
fof(f16, plain, ! [X47, X48] : (leaf(X47, X48) <=> (~ ? [X50] : min_precedes(X47, X50, X48) & (? [X49] : min_precedes(X49, X47, X48) | root(X47, X48)))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_15)).
fof(f2047, plain, ! [X6, X8, X7, X5] : (~ root(sK11(X5, X6), X8) | ~ subactivity_occurrence(X5, X7) | ~ occurrence_of(X7, X6) | ~ leaf(X5, X6) | root(X5, X6) | root_occ(sK11(X5, X6), X7) | ~ occurrence_of(X7, X8)), inference(resolution, [], [f770, f213])).
fof(f213, plain, ! [X2, X0, X1] : (~ subactivity_occurrence(X0, X1) | ~ root(X0, X2) | root_occ(X0, X1) | ~ occurrence_of(X1, X2)), inference(cnf_transformation, [], [f159])).
fof(f770, plain, ! [X24, X23, X22] : (subactivity_occurrence(sK11(X22, X23), X24) | root(X22, X23) | ~ subactivity_occurrence(X22, X24) | ~ occurrence_of(X24, X23) | ~ leaf(X22, X23)), inference(resolution, [], [f402, f228])).
fof(f402, plain, ! [X12, X13] : (min_precedes(sK11(X12, X13), X12, X13) | ~ leaf(X12, X13) | root(X12, X13)), inference(resolution, [], [f199, f220])).
fof(f110615, plain, (spl19_257 | ~ spl19_420 | ~ spl19_627 | ~ spl19_877), inference(avatar_contradiction_clause, [], [f110614])).
fof(f110614, plain, ($false | (spl19_257 | ~ spl19_420 | ~ spl19_627 | ~ spl19_877)), inference(subsumption_resolution, [], [f110522, f16054])).
fof(f16054, plain, (~ next_subocc(sK13(sK17), sK15(sK17), tptp0) | spl19_257), inference(avatar_component_clause, [], [f16053])).
fof(f16053, plain, (spl19_257 <=> next_subocc(sK13(sK17), sK15(sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_257])])).
fof(f110522, plain, (next_subocc(sK13(sK17), sK15(sK17), tptp0) | (~ spl19_420 | ~ spl19_627 | ~ spl19_877)), inference(backward_demodulation, [], [f51528, f32488])).
fof(f32488, plain, ((sK15(sK17) = sK14(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ spl19_627), inference(avatar_component_clause, [], [f32486])).
fof(f32486, plain, (spl19_627 <=> (sK15(sK17) = sK14(sK3(tptp0, sK13(sK17), sK15(sK17))))), introduced(avatar_definition, [new_symbols(naming, [spl19_627])])).
fof(f51528, plain, (next_subocc(sK13(sK17), sK14(sK3(tptp0, sK13(sK17), sK15(sK17))), tptp0) | (~ spl19_420 | ~ spl19_877)), inference(subsumption_resolution, [], [f51356, f20809])).
fof(f20809, plain, (occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0) | ~ spl19_420), inference(avatar_component_clause, [], [f20807])).
fof(f20807, plain, (spl19_420 <=> occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_420])])).
fof(f51356, plain, (next_subocc(sK13(sK17), sK14(sK3(tptp0, sK13(sK17), sK15(sK17))), tptp0) | ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0) | ~ spl19_877), inference(superposition, [], [f236, f45806])).
fof(f45806, plain, ((sK13(sK17) = sK13(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ spl19_877), inference(avatar_component_clause, [], [f45804])).
fof(f45804, plain, (spl19_877 <=> (sK13(sK17) = sK13(sK3(tptp0, sK13(sK17), sK15(sK17))))), introduced(avatar_definition, [new_symbols(naming, [spl19_877])])).
fof(f110296, plain, (~ spl19_1 | ~ spl19_2 | ~ spl19_76 | spl19_1465), inference(avatar_contradiction_clause, [], [f110295])).
fof(f110295, plain, ($false | (~ spl19_1 | ~ spl19_2 | ~ spl19_76 | spl19_1465)), inference(subsumption_resolution, [], [f110294, f15980])).
fof(f15980, plain, (min_precedes(sK13(sK17), sK15(sK17), tptp0) | (~ spl19_2 | ~ spl19_76)), inference(subsumption_resolution, [], [f15979, f387])).
fof(f15979, plain, (min_precedes(sK13(sK17), sK15(sK17), tptp0) | ~ leaf(sK15(sK17), tptp0) | ~ spl19_76), inference(subsumption_resolution, [], [f15974, f240])).
fof(f240, plain, ~ atomic(tptp0), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ~ atomic(tptp0), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_34)).
fof(f15974, plain, (min_precedes(sK13(sK17), sK15(sK17), tptp0) | atomic(tptp0) | ~ leaf(sK15(sK17), tptp0) | ~ spl19_76), inference(resolution, [], [f5083, f188])).
fof(f188, plain, ! [X0, X1] : (occurrence_of(sK4(X0, X1), X1) | atomic(X1) | ~ leaf(X0, X1)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1] : ((leaf_occ(X0, sK4(X0, X1)) & occurrence_of(sK4(X0, X1), X1)) | atomic(X1) | ~ leaf(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f99, f139])).
fof(f139, plain, ! [X1, X0] : (? [X2] : (leaf_occ(X0, X2) & occurrence_of(X2, X1)) => (leaf_occ(X0, sK4(X0, X1)) & occurrence_of(sK4(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f99, plain, ! [X0, X1] : (? [X2] : (leaf_occ(X0, X2) & occurrence_of(X2, X1)) | atomic(X1) | ~ leaf(X0, X1)), inference(flattening, [], [f98])).
fof(f98, plain, ! [X0, X1] : (? [X2] : (leaf_occ(X0, X2) & occurrence_of(X2, X1)) | (atomic(X1) | ~ leaf(X0, X1))), inference(ennf_transformation, [], [f54])).
fof(f54, plain, ! [X0, X1] : ((~ atomic(X1) & leaf(X0, X1)) => ? [X2] : (leaf_occ(X0, X2) & occurrence_of(X2, X1))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X25, X26] : ((~ atomic(X26) & leaf(X25, X26)) => ? [X27] : (leaf_occ(X25, X27) & occurrence_of(X27, X26))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_07)).
fof(f5083, plain, (! [X0] : (~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | min_precedes(sK13(sK17), sK15(sK17), X0)) | ~ spl19_76), inference(avatar_component_clause, [], [f5082])).
fof(f5082, plain, (spl19_76 <=> ! [X0] : (min_precedes(sK13(sK17), sK15(sK17), X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0))), introduced(avatar_definition, [new_symbols(naming, [spl19_76])])).
fof(f110294, plain, (~ min_precedes(sK13(sK17), sK15(sK17), tptp0) | (~ spl19_1 | spl19_1465)), inference(resolution, [], [f107341, f11124])).
fof(f11124, plain, (! [X0] : (leaf_occ(sK15(sK17), sK3(tptp0, X0, sK15(sK17))) | ~ min_precedes(X0, sK15(sK17), tptp0)) | ~ spl19_1), inference(subsumption_resolution, [], [f11117, f382])).
fof(f11117, plain, ! [X0] : (leaf_occ(sK15(sK17), sK3(tptp0, X0, sK15(sK17))) | ~ leaf_occ(sK15(sK17), sK17) | ~ min_precedes(X0, sK15(sK17), tptp0)), inference(superposition, [], [f3000, f377])).
fof(f377, plain, (tptp0 = sK9(sK15(sK17), sK17)), inference(subsumption_resolution, [], [f376, f255])).
fof(f376, plain, ((tptp0 = sK9(sK15(sK17), sK17)) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f305, f239])).
fof(f305, plain, ! [X0] : (~ leaf_occ(X0, sK17) | (tptp0 = sK9(X0, sK17))), inference(resolution, [], [f301, f206])).
fof(f206, plain, ! [X0, X1] : (occurrence_of(X1, sK9(X0, X1)) | ~ leaf_occ(X0, X1)), inference(cnf_transformation, [], [f155])).
fof(f301, plain, ! [X12] : (~ occurrence_of(sK17, X12) | (tptp0 = X12)), inference(resolution, [], [f190, f255])).
fof(f3000, plain, ! [X4, X5, X3] : (leaf_occ(X3, sK3(sK9(X3, X4), X5, X3)) | ~ leaf_occ(X3, X4) | ~ min_precedes(X5, X3, sK9(X3, X4))), inference(duplicate_literal_removal, [], [f2994])).
fof(f2994, plain, ! [X4, X5, X3] : (leaf_occ(X3, sK3(sK9(X3, X4), X5, X3)) | ~ leaf_occ(X3, X4) | ~ min_precedes(X5, X3, sK9(X3, X4)) | ~ min_precedes(X5, X3, sK9(X3, X4))), inference(resolution, [], [f835, f187])).
fof(f187, plain, ! [X2, X0, X1] : (subactivity_occurrence(X2, sK3(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0, X1, X2] : ((subactivity_occurrence(X2, sK3(X0, X1, X2)) & subactivity_occurrence(X1, sK3(X0, X1, X2)) & occurrence_of(sK3(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f97, f137])).
fof(f137, plain, ! [X2, X1, X0] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) => (subactivity_occurrence(X2, sK3(X0, X1, X2)) & subactivity_occurrence(X1, sK3(X0, X1, X2)) & occurrence_of(sK3(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f97, plain, ! [X0, X1, X2] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0))), inference(rectify, [], [f7])).
fof(f7, plain, ! [X21, X22, X23] : (min_precedes(X22, X23, X21) => ? [X24] : (subactivity_occurrence(X23, X24) & subactivity_occurrence(X22, X24) & occurrence_of(X24, X21))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_06)).
fof(f835, plain, ! [X4, X2, X5, X3] : (~ subactivity_occurrence(X2, sK3(sK9(X2, X3), X4, X5)) | leaf_occ(X2, sK3(sK9(X2, X3), X4, X5)) | ~ leaf_occ(X2, X3) | ~ min_precedes(X4, X5, sK9(X2, X3))), inference(resolution, [], [f418, f185])).
fof(f185, plain, ! [X2, X0, X1] : (occurrence_of(sK3(X0, X1, X2), X0) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f138])).
fof(f418, plain, ! [X2, X0, X1] : (~ occurrence_of(X1, sK9(X0, X2)) | ~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ leaf_occ(X0, X2)), inference(resolution, [], [f209, f208])).
fof(f208, plain, ! [X0, X1] : (leaf(X0, sK9(X0, X1)) | ~ leaf_occ(X0, X1)), inference(cnf_transformation, [], [f155])).
fof(f209, plain, ! [X2, X0, X1] : (~ leaf(X0, X2) | leaf_occ(X0, X1) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2)), inference(cnf_transformation, [], [f155])).
fof(f107341, plain, (~ leaf_occ(sK15(sK17), sK3(tptp0, sK13(sK17), sK15(sK17))) | spl19_1465), inference(avatar_component_clause, [], [f107339])).
fof(f107339, plain, (spl19_1465 <=> leaf_occ(sK15(sK17), sK3(tptp0, sK13(sK17), sK15(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_1465])])).
fof(f107342, plain, (spl19_627 | ~ spl19_1465 | ~ spl19_420 | ~ spl19_498), inference(avatar_split_clause, [], [f107337, f25227, f20807, f107339, f32486])).
fof(f25227, plain, (spl19_498 <=> precedes(sK15(sK17), sK15(sK3(tptp0, sK13(sK17), sK15(sK17))))), introduced(avatar_definition, [new_symbols(naming, [spl19_498])])).
fof(f107337, plain, (~ leaf_occ(sK15(sK17), sK3(tptp0, sK13(sK17), sK15(sK17))) | (sK15(sK17) = sK14(sK3(tptp0, sK13(sK17), sK15(sK17)))) | (~ spl19_420 | ~ spl19_498)), inference(subsumption_resolution, [], [f107287, f20809])).
fof(f107287, plain, (~ leaf_occ(sK15(sK17), sK3(tptp0, sK13(sK17), sK15(sK17))) | ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0) | (sK15(sK17) = sK14(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ spl19_498), inference(resolution, [], [f10723, f25229])).
fof(f25229, plain, (precedes(sK15(sK17), sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ spl19_498), inference(avatar_component_clause, [], [f25227])).
fof(f10723, plain, ! [X2, X3] : (~ precedes(X3, sK15(X2)) | ~ leaf_occ(X3, X2) | ~ occurrence_of(X2, tptp0) | (sK14(X2) = X3)), inference(duplicate_literal_removal, [], [f10689])).
fof(f10689, plain, ! [X2, X3] : ((sK14(X2) = X3) | ~ leaf_occ(X3, X2) | ~ occurrence_of(X2, tptp0) | ~ precedes(X3, sK15(X2)) | ~ occurrence_of(X2, tptp0)), inference(resolution, [], [f3510, f671])).
fof(f3510, plain, ! [X0, X1] : (min_precedes(sK14(X0), X1, tptp0) | (sK14(X0) = X1) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, tptp0)), inference(subsumption_resolution, [], [f3509, f1846])).
fof(f3509, plain, ! [X0, X1] : ((sK14(X0) = X1) | min_precedes(sK14(X0), X1, tptp0) | ~ subactivity_occurrence(sK14(X0), X0) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, tptp0)), inference(duplicate_literal_removal, [], [f3505])).
fof(f3505, plain, ! [X0, X1] : ((sK14(X0) = X1) | min_precedes(sK14(X0), X1, tptp0) | ~ subactivity_occurrence(sK14(X0), X0) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, tptp0) | ~ occurrence_of(X0, tptp0) | ~ occurrence_of(X0, tptp0)), inference(resolution, [], [f621, f234])).
fof(f234, plain, ! [X0] : (root_occ(sK13(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f621, plain, ! [X8, X7, X9] : (~ root_occ(sK13(X7), X9) | (sK14(X7) = X8) | min_precedes(sK14(X7), X8, tptp0) | ~ subactivity_occurrence(sK14(X7), X9) | ~ leaf_occ(X8, X9) | ~ occurrence_of(X7, tptp0) | ~ occurrence_of(X9, tptp0)), inference(resolution, [], [f310, f180])).
fof(f180, plain, ! [X4, X2, X0, X3, X1] : (~ min_precedes(X3, X2, X0) | (X2 = X4) | min_precedes(X2, X4, X0) | ~ subactivity_occurrence(X2, X1) | ~ leaf_occ(X4, X1) | ~ root_occ(X3, X1) | ~ occurrence_of(X1, X0)), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ! [X0, X1, X2, X3, X4] : (min_precedes(X2, X4, X0) | (X2 = X4) | ~ min_precedes(X3, X2, X0) | ~ subactivity_occurrence(X2, X1) | ~ leaf_occ(X4, X1) | ~ root_occ(X3, X1) | ~ occurrence_of(X1, X0)), inference(flattening, [], [f89])).
fof(f89, plain, ! [X0, X1, X2, X3, X4] : (min_precedes(X2, X4, X0) | ((X2 = X4) | ~ min_precedes(X3, X2, X0) | ~ subactivity_occurrence(X2, X1) | ~ leaf_occ(X4, X1) | ~ root_occ(X3, X1) | ~ occurrence_of(X1, X0))), inference(ennf_transformation, [], [f48])).
fof(f48, plain, ! [X0, X1, X2, X3, X4] : ((~ (X2 = X4) & min_precedes(X3, X2, X0) & subactivity_occurrence(X2, X1) & leaf_occ(X4, X1) & root_occ(X3, X1) & occurrence_of(X1, X0)) => min_precedes(X2, X4, X0)), inference(rectify, [], [f2])).
fof(f2, plain, ! [X3, X4, X5, X6, X7] : ((~ (X5 = X7) & min_precedes(X6, X5, X3) & subactivity_occurrence(X5, X4) & leaf_occ(X7, X4) & root_occ(X6, X4) & occurrence_of(X4, X3)) => min_precedes(X5, X7, X3)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_01)).
fof(f310, plain, ! [X0] : (min_precedes(sK13(X0), sK14(X0), tptp0) | ~ occurrence_of(X0, tptp0)), inference(resolution, [], [f236, f224])).
fof(f82779, plain, (~ spl19_33 | ~ spl19_95 | ~ spl19_266), inference(avatar_contradiction_clause, [], [f82778])).
fof(f82778, plain, ($false | (~ spl19_33 | ~ spl19_95 | ~ spl19_266)), inference(subsumption_resolution, [], [f82777, f250])).
fof(f82777, plain, ((tptp1 = tptp2) | (~ spl19_33 | ~ spl19_95 | ~ spl19_266)), inference(backward_demodulation, [], [f2561, f82246])).
fof(f82246, plain, ((tptp1 = sK5(sK15(sK17))) | (~ spl19_95 | ~ spl19_266)), inference(backward_demodulation, [], [f16096, f16158])).
fof(f16158, plain, ((sK15(sK17) = sK18(sK13(sK17))) | ~ spl19_266), inference(avatar_component_clause, [], [f16156])).
fof(f16156, plain, (spl19_266 <=> (sK15(sK17) = sK18(sK13(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_266])])).
fof(f82237, plain, (spl19_266 | ~ spl19_95 | ~ spl19_96 | spl19_1337), inference(avatar_split_clause, [], [f82236, f82175, f7421, f7411, f16156])).
fof(f82236, plain, ((sK15(sK17) = sK18(sK13(sK17))) | (~ spl19_95 | ~ spl19_96 | spl19_1337)), inference(subsumption_resolution, [], [f82235, f255])).
fof(f82235, plain, ((sK15(sK17) = sK18(sK13(sK17))) | ~ occurrence_of(sK17, tptp0) | (~ spl19_95 | ~ spl19_96 | spl19_1337)), inference(subsumption_resolution, [], [f82234, f7423])).
fof(f7423, plain, (subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ spl19_96), inference(avatar_component_clause, [], [f7421])).
fof(f82234, plain, ((sK15(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ occurrence_of(sK17, tptp0) | (~ spl19_95 | spl19_1337)), inference(subsumption_resolution, [], [f82233, f16100])).
fof(f16100, plain, (arboreal(sK18(sK13(sK17))) | ~ spl19_95), inference(subsumption_resolution, [], [f16095, f242])).
fof(f242, plain, atomic(tptp1), inference(cnf_transformation, [], [f37])).
fof(f37, plain, atomic(tptp1), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_36)).
fof(f16095, plain, (~ atomic(tptp1) | arboreal(sK18(sK13(sK17))) | ~ spl19_95), inference(resolution, [], [f7413, f204])).
fof(f82233, plain, (~ arboreal(sK18(sK13(sK17))) | (sK15(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ occurrence_of(sK17, tptp0) | spl19_1337), inference(duplicate_literal_removal, [], [f82230])).
fof(f82230, plain, (~ arboreal(sK18(sK13(sK17))) | (sK15(sK17) = sK18(sK13(sK17))) | ~ subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ occurrence_of(sK17, tptp0) | ~ occurrence_of(sK17, tptp0) | spl19_1337), inference(resolution, [], [f82177, f532])).
fof(f82177, plain, (~ min_precedes(sK18(sK13(sK17)), sK15(sK17), tptp0) | spl19_1337), inference(avatar_component_clause, [], [f82175])).
fof(f50964, plain, (~ spl19_2 | ~ spl19_6 | ~ spl19_76 | spl19_977), inference(avatar_contradiction_clause, [], [f50963])).
fof(f50963, plain, ($false | (~ spl19_2 | ~ spl19_6 | ~ spl19_76 | spl19_977)), inference(subsumption_resolution, [], [f50962, f15980])).
fof(f50962, plain, (~ min_precedes(sK13(sK17), sK15(sK17), tptp0) | (~ spl19_6 | spl19_977)), inference(subsumption_resolution, [], [f50961, f451])).
fof(f451, plain, (root(sK13(sK17), tptp0) | ~ spl19_6), inference(avatar_component_clause, [], [f449])).
fof(f449, plain, (spl19_6 <=> root(sK13(sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_6])])).
fof(f50961, plain, (~ root(sK13(sK17), tptp0) | ~ min_precedes(sK13(sK17), sK15(sK17), tptp0) | spl19_977), inference(resolution, [], [f50776, f928])).
fof(f928, plain, ! [X2, X0, X1] : (root_occ(X0, sK3(X1, X0, X2)) | ~ root(X0, X1) | ~ min_precedes(X0, X2, X1)), inference(duplicate_literal_removal, [], [f924])).
fof(f924, plain, ! [X2, X0, X1] : (root_occ(X0, sK3(X1, X0, X2)) | ~ root(X0, X1) | ~ min_precedes(X0, X2, X1) | ~ min_precedes(X0, X2, X1)), inference(resolution, [], [f422, f185])).
fof(f422, plain, ! [X2, X0, X3, X1] : (~ occurrence_of(sK3(X2, X0, X3), X1) | root_occ(X0, sK3(X2, X0, X3)) | ~ root(X0, X1) | ~ min_precedes(X0, X3, X2)), inference(resolution, [], [f213, f186])).
fof(f186, plain, ! [X2, X0, X1] : (subactivity_occurrence(X1, sK3(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f138])).
fof(f50776, plain, (~ root_occ(sK13(sK17), sK3(tptp0, sK13(sK17), sK15(sK17))) | spl19_977), inference(avatar_component_clause, [], [f50775])).
fof(f50775, plain, (spl19_977 <=> root_occ(sK13(sK17), sK3(tptp0, sK13(sK17), sK15(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_977])])).
fof(f50874, plain, (spl19_421 | ~ spl19_420 | spl19_877 | ~ spl19_977), inference(avatar_split_clause, [], [f50873, f50775, f45804, f20807, f20895])).
fof(f20895, plain, (spl19_421 <=> ! [X4] : ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X4)), introduced(avatar_definition, [new_symbols(naming, [spl19_421])])).
fof(f50873, plain, (! [X7] : ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X7) | (~ spl19_420 | spl19_877 | ~ spl19_977)), inference(subsumption_resolution, [], [f50872, f20809])).
fof(f50872, plain, (! [X7] : (~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X7) | ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0)) | (spl19_877 | ~ spl19_977)), inference(subsumption_resolution, [], [f50857, f45805])).
fof(f45805, plain, (~ (sK13(sK17) = sK13(sK3(tptp0, sK13(sK17), sK15(sK17)))) | spl19_877), inference(avatar_component_clause, [], [f45804])).
fof(f50857, plain, (! [X7] : ((sK13(sK17) = sK13(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X7) | ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0)) | ~ spl19_977), inference(resolution, [], [f50777, f441])).
fof(f441, plain, ! [X2, X0, X1] : (~ root_occ(X0, X1) | (sK13(X1) = X0) | ~ occurrence_of(X1, X2) | ~ occurrence_of(X1, tptp0)), inference(resolution, [], [f230, f234])).
fof(f230, plain, ! [X2, X0, X3, X1] : (~ root_occ(X1, X2) | (X0 = X1) | ~ root_occ(X0, X2) | ~ occurrence_of(X2, X3)), inference(cnf_transformation, [], [f124])).
fof(f124, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | ~ root_occ(X1, X2) | ~ root_occ(X0, X2) | ~ occurrence_of(X2, X3)), inference(flattening, [], [f123])).
fof(f123, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | (~ root_occ(X1, X2) | ~ root_occ(X0, X2) | ~ occurrence_of(X2, X3))), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0, X1, X2, X3] : ((root_occ(X1, X2) & root_occ(X0, X2) & occurrence_of(X2, X3)) => (X0 = X1)), inference(rectify, [], [f30])).
fof(f30, plain, ! [X90, X91, X92, X93] : ((root_occ(X91, X92) & root_occ(X90, X92) & occurrence_of(X92, X93)) => (X90 = X91)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_29)).
fof(f50777, plain, (root_occ(sK13(sK17), sK3(tptp0, sK13(sK17), sK15(sK17))) | ~ spl19_977), inference(avatar_component_clause, [], [f50775])).
fof(f29299, plain, (~ spl19_21 | ~ spl19_29 | ~ spl19_86 | ~ spl19_257), inference(avatar_contradiction_clause, [], [f29298])).
fof(f29298, plain, ($false | (~ spl19_21 | ~ spl19_29 | ~ spl19_86 | ~ spl19_257)), inference(subsumption_resolution, [], [f29206, f2841])).
fof(f2841, plain, (min_precedes(sK14(sK4(sK15(sK17), tptp0)), sK15(sK17), tptp0) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f2797, f1644])).
fof(f1644, plain, (occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_21), inference(avatar_component_clause, [], [f1642])).
fof(f1642, plain, (spl19_21 <=> occurrence_of(sK4(sK15(sK17), tptp0), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_21])])).
fof(f2797, plain, (min_precedes(sK14(sK4(sK15(sK17), tptp0)), sK15(sK17), tptp0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_29), inference(superposition, [], [f314, f2486])).
fof(f2486, plain, ((sK15(sK17) = sK15(sK4(sK15(sK17), tptp0))) | ~ spl19_29), inference(avatar_component_clause, [], [f2484])).
fof(f2484, plain, (spl19_29 <=> (sK15(sK17) = sK15(sK4(sK15(sK17), tptp0)))), introduced(avatar_definition, [new_symbols(naming, [spl19_29])])).
fof(f29206, plain, (~ min_precedes(sK14(sK4(sK15(sK17), tptp0)), sK15(sK17), tptp0) | (~ spl19_21 | ~ spl19_86 | ~ spl19_257)), inference(resolution, [], [f17296, f7383])).
fof(f7383, plain, (min_precedes(sK13(sK17), sK14(sK4(sK15(sK17), tptp0)), tptp0) | (~ spl19_21 | ~ spl19_86)), inference(subsumption_resolution, [], [f7352, f1644])).
fof(f7352, plain, (min_precedes(sK13(sK17), sK14(sK4(sK15(sK17), tptp0)), tptp0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_86), inference(superposition, [], [f310, f6442])).
fof(f6442, plain, ((sK13(sK17) = sK13(sK4(sK15(sK17), tptp0))) | ~ spl19_86), inference(avatar_component_clause, [], [f6440])).
fof(f6440, plain, (spl19_86 <=> (sK13(sK17) = sK13(sK4(sK15(sK17), tptp0)))), introduced(avatar_definition, [new_symbols(naming, [spl19_86])])).
fof(f17296, plain, (! [X0] : (~ min_precedes(sK13(sK17), X0, tptp0) | ~ min_precedes(X0, sK15(sK17), tptp0)) | ~ spl19_257), inference(resolution, [], [f16055, f225])).
fof(f16055, plain, (next_subocc(sK13(sK17), sK15(sK17), tptp0) | ~ spl19_257), inference(avatar_component_clause, [], [f16053])).
fof(f25234, plain, (spl19_498 | spl19_499 | spl19_421 | ~ spl19_2 | ~ spl19_76 | ~ spl19_420), inference(avatar_split_clause, [], [f25225, f20807, f5082, f385, f20895, f25231, f25227])).
fof(f25225, plain, (! [X5] : (~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X5) | (sK15(sK17) = sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | precedes(sK15(sK17), sK15(sK3(tptp0, sK13(sK17), sK15(sK17))))) | (~ spl19_2 | ~ spl19_76 | ~ spl19_420)), inference(subsumption_resolution, [], [f25224, f15980])).
fof(f25224, plain, (! [X5] : (~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X5) | (sK15(sK17) = sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | precedes(sK15(sK17), sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ min_precedes(sK13(sK17), sK15(sK17), tptp0)) | ~ spl19_420), inference(subsumption_resolution, [], [f25175, f317])).
fof(f317, plain, arboreal(sK15(sK17)), inference(resolution, [], [f315, f255])).
fof(f315, plain, ! [X1] : (~ occurrence_of(X1, tptp0) | arboreal(sK15(X1))), inference(resolution, [], [f238, f223])).
fof(f223, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | arboreal(X1)), inference(cnf_transformation, [], [f117])).
fof(f117, plain, ! [X0, X1, X2] : ((arboreal(X1) & arboreal(X0)) | ~ next_subocc(X0, X1, X2)), inference(ennf_transformation, [], [f72])).
fof(f72, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) => (arboreal(X1) & arboreal(X0))), inference(rectify, [], [f26])).
fof(f26, plain, ! [X75, X76, X77] : (next_subocc(X75, X76, X77) => (arboreal(X76) & arboreal(X75))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_25)).
fof(f25175, plain, (! [X5] : (~ arboreal(sK15(sK17)) | ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X5) | (sK15(sK17) = sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | precedes(sK15(sK17), sK15(sK3(tptp0, sK13(sK17), sK15(sK17)))) | ~ min_precedes(sK13(sK17), sK15(sK17), tptp0)) | ~ spl19_420), inference(resolution, [], [f2273, f20809])).
fof(f2273, plain, ! [X6, X4, X7, X5] : (~ occurrence_of(sK3(X4, X5, X6), tptp0) | ~ arboreal(X6) | ~ occurrence_of(sK3(X4, X5, X6), X7) | (sK15(sK3(X4, X5, X6)) = X6) | precedes(X6, sK15(sK3(X4, X5, X6))) | ~ min_precedes(X5, X6, X4)), inference(resolution, [], [f1001, f187])).
fof(f24674, plain, spl19_292, inference(avatar_contradiction_clause, [], [f24673])).
fof(f24673, plain, ($false | spl19_292), inference(subsumption_resolution, [], [f24670, f255])).
fof(f24670, plain, (~ occurrence_of(sK17, tptp0) | spl19_292), inference(resolution, [], [f16888, f236])).
fof(f16888, plain, (~ next_subocc(sK13(sK17), sK14(sK17), tptp0) | spl19_292), inference(avatar_component_clause, [], [f16887])).
fof(f21205, plain, (~ spl19_2 | ~ spl19_76 | ~ spl19_421), inference(avatar_contradiction_clause, [], [f21204])).
fof(f21204, plain, ($false | (~ spl19_2 | ~ spl19_76 | ~ spl19_421)), inference(subsumption_resolution, [], [f21198, f15980])).
fof(f21198, plain, (~ min_precedes(sK13(sK17), sK15(sK17), tptp0) | ~ spl19_421), inference(resolution, [], [f20896, f185])).
fof(f20896, plain, (! [X4] : ~ occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), X4) | ~ spl19_421), inference(avatar_component_clause, [], [f20895])).
fof(f20822, plain, (~ spl19_2 | ~ spl19_76 | spl19_418), inference(avatar_contradiction_clause, [], [f20821])).
fof(f20821, plain, ($false | (~ spl19_2 | ~ spl19_76 | spl19_418)), inference(subsumption_resolution, [], [f20820, f15980])).
fof(f20820, plain, (~ min_precedes(sK13(sK17), sK15(sK17), tptp0) | spl19_418), inference(resolution, [], [f20800, f331])).
fof(f331, plain, ! [X12, X10, X11] : (activity_occurrence(sK3(X12, X10, X11)) | ~ min_precedes(X10, X11, X12)), inference(resolution, [], [f185, f182])).
fof(f20800, plain, (~ activity_occurrence(sK3(tptp0, sK13(sK17), sK15(sK17))) | spl19_418), inference(avatar_component_clause, [], [f20798])).
fof(f20798, plain, (spl19_418 <=> activity_occurrence(sK3(tptp0, sK13(sK17), sK15(sK17)))), introduced(avatar_definition, [new_symbols(naming, [spl19_418])])).
fof(f20810, plain, (~ spl19_418 | spl19_420 | ~ spl19_2 | ~ spl19_76), inference(avatar_split_clause, [], [f20795, f5082, f385, f20807, f20798])).
fof(f20795, plain, (occurrence_of(sK3(tptp0, sK13(sK17), sK15(sK17)), tptp0) | ~ activity_occurrence(sK3(tptp0, sK13(sK17), sK15(sK17))) | (~ spl19_2 | ~ spl19_76)), inference(superposition, [], [f195, f16019])).
fof(f16019, plain, ((tptp0 = sK5(sK3(tptp0, sK13(sK17), sK15(sK17)))) | (~ spl19_2 | ~ spl19_76)), inference(resolution, [], [f15980, f371])).
fof(f371, plain, ! [X6, X7, X5] : (~ min_precedes(X6, X7, X5) | (sK5(sK3(X5, X6, X7)) = X5)), inference(resolution, [], [f302, f185])).
fof(f17888, plain, (spl19_309 | ~ spl19_97), inference(avatar_split_clause, [], [f17886, f7431, f17623])).
fof(f17886, plain, (legal(sK18(sK13(sK17))) | ~ spl19_97), inference(resolution, [], [f17578, f216])).
fof(f216, plain, ! [X0, X1] : (~ precedes(X0, X1) | legal(X1)), inference(cnf_transformation, [], [f161])).
fof(f161, plain, ! [X0, X1] : ((precedes(X0, X1) | ~ legal(X1) | ~ earlier(X0, X1)) & ((legal(X1) & earlier(X0, X1)) | ~ precedes(X0, X1))), inference(flattening, [], [f160])).
fof(f160, plain, ! [X0, X1] : ((precedes(X0, X1) | (~ legal(X1) | ~ earlier(X0, X1))) & ((legal(X1) & earlier(X0, X1)) | ~ precedes(X0, X1))), inference(nnf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1] : (precedes(X0, X1) <=> (legal(X1) & earlier(X0, X1))), inference(rectify, [], [f22])).
fof(f22, plain, ! [X63, X64] : (precedes(X63, X64) <=> (legal(X64) & earlier(X63, X64))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_21)).
fof(f17578, plain, (precedes(sK13(sK17), sK18(sK13(sK17))) | ~ spl19_97), inference(resolution, [], [f7433, f221])).
fof(f16197, plain, (spl19_31 | ~ spl19_256), inference(avatar_contradiction_clause, [], [f16196])).
fof(f16196, plain, ($false | (spl19_31 | ~ spl19_256)), inference(subsumption_resolution, [], [f16195, f2546])).
fof(f2546, plain, (~ occurrence_of(sK15(sK17), tptp1) | spl19_31), inference(avatar_component_clause, [], [f2545])).
fof(f16195, plain, (occurrence_of(sK15(sK17), tptp1) | ~ spl19_256), inference(resolution, [], [f16040, f251])).
fof(f251, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | occurrence_of(X2, tptp1)), inference(cnf_transformation, [], [f174])).
fof(f16051, plain, (spl19_97 | spl19_256 | ~ spl19_1 | ~ spl19_2 | ~ spl19_5 | ~ spl19_32 | ~ spl19_76), inference(avatar_split_clause, [], [f16050, f5082, f2549, f445, f385, f381, f16038, f7431])).
fof(f16050, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0) | (~ spl19_1 | ~ spl19_2 | ~ spl19_5 | ~ spl19_32 | ~ spl19_76)), inference(subsumption_resolution, [], [f16049, f611])).
fof(f16049, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_2 | ~ spl19_5 | ~ spl19_32 | ~ spl19_76)), inference(subsumption_resolution, [], [f16048, f446])).
fof(f16048, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_2 | ~ spl19_32 | ~ spl19_76)), inference(subsumption_resolution, [], [f16047, f2551])).
fof(f16047, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0) | ~ occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_2 | ~ spl19_76)), inference(subsumption_resolution, [], [f16002, f382])).
fof(f16002, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ leaf_occ(sK15(sK17), sK17) | min_precedes(sK13(sK17), sK18(sK13(sK17)), tptp0) | ~ occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_2 | ~ spl19_76)), inference(resolution, [], [f15980, f263])).
fof(f263, plain, ! [X2, X1] : (~ min_precedes(X1, X2, tptp0) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17) | min_precedes(X1, sK18(X1), tptp0) | ~ occurrence_of(X2, tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3)), inference(cnf_transformation, [], [f177])).
fof(f16046, plain, (spl19_96 | spl19_256 | ~ spl19_1 | ~ spl19_2 | ~ spl19_5 | ~ spl19_32 | ~ spl19_76), inference(avatar_split_clause, [], [f16045, f5082, f2549, f445, f385, f381, f16038, f7421])).
fof(f16045, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | subactivity_occurrence(sK18(sK13(sK17)), sK17) | (~ spl19_1 | ~ spl19_2 | ~ spl19_5 | ~ spl19_32 | ~ spl19_76)), inference(subsumption_resolution, [], [f16044, f611])).
fof(f16044, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_2 | ~ spl19_5 | ~ spl19_32 | ~ spl19_76)), inference(subsumption_resolution, [], [f16043, f446])).
fof(f16043, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_2 | ~ spl19_32 | ~ spl19_76)), inference(subsumption_resolution, [], [f16042, f2551])).
fof(f16042, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_1 | ~ spl19_2 | ~ spl19_76)), inference(subsumption_resolution, [], [f16000, f382])).
fof(f16000, plain, (sP0(sK13(sK17), sK17, sK15(sK17)) | ~ leaf_occ(sK15(sK17), sK17) | subactivity_occurrence(sK18(sK13(sK17)), sK17) | ~ occurrence_of(sK15(sK17), tptp2) | ~ root_occ(sK13(sK17), sK17) | ~ occurrence_of(sK13(sK17), tptp3) | (~ spl19_2 | ~ spl19_76)), inference(resolution, [], [f15980, f261])).
fof(f261, plain, ! [X2, X1] : (~ min_precedes(X1, X2, tptp0) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17) | subactivity_occurrence(sK18(X1), sK17) | ~ occurrence_of(X2, tptp2) | ~ root_occ(X1, sK17) | ~ occurrence_of(X1, tptp3)), inference(cnf_transformation, [], [f177])).
fof(f7261, plain, (~ spl19_2 | ~ spl19_6 | ~ spl19_79), inference(avatar_contradiction_clause, [], [f7260])).
fof(f7260, plain, ($false | (~ spl19_2 | ~ spl19_6 | ~ spl19_79)), inference(subsumption_resolution, [], [f7259, f387])).
fof(f7259, plain, (~ leaf(sK15(sK17), tptp0) | (~ spl19_6 | ~ spl19_79)), inference(subsumption_resolution, [], [f7258, f240])).
fof(f7258, plain, (atomic(tptp0) | ~ leaf(sK15(sK17), tptp0) | (~ spl19_6 | ~ spl19_79)), inference(subsumption_resolution, [], [f7253, f451])).
fof(f7253, plain, (~ root(sK13(sK17), tptp0) | atomic(tptp0) | ~ leaf(sK15(sK17), tptp0) | ~ spl19_79), inference(resolution, [], [f5096, f188])).
fof(f5096, plain, (! [X3] : (~ occurrence_of(sK4(sK15(sK17), tptp0), X3) | ~ root(sK13(sK17), X3)) | ~ spl19_79), inference(avatar_component_clause, [], [f5095])).
fof(f5095, plain, (spl19_79 <=> ! [X3] : (~ root(sK13(sK17), X3) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X3))), introduced(avatar_definition, [new_symbols(naming, [spl19_79])])).
fof(f6445, plain, (spl19_78 | spl19_79 | ~ spl19_21 | ~ spl19_29), inference(avatar_split_clause, [], [f6387, f2484, f1642, f5095, f5091])).
fof(f5091, plain, (spl19_78 <=> root_occ(sK13(sK17), sK4(sK15(sK17), tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl19_78])])).
fof(f6387, plain, (! [X3] : (~ root(sK13(sK17), X3) | root_occ(sK13(sK17), sK4(sK15(sK17), tptp0)) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X3)) | (~ spl19_21 | ~ spl19_29)), inference(resolution, [], [f5040, f213])).
fof(f5040, plain, (subactivity_occurrence(sK13(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f5039, f1644])).
fof(f5039, plain, (~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | subactivity_occurrence(sK13(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f4367, f255])).
fof(f4367, plain, (~ occurrence_of(sK17, tptp0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | subactivity_occurrence(sK13(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(resolution, [], [f3122, f630])).
fof(f630, plain, ! [X21, X20] : (~ subactivity_occurrence(sK14(X20), X21) | ~ occurrence_of(X20, tptp0) | ~ occurrence_of(X21, tptp0) | subactivity_occurrence(sK13(X20), X21)), inference(resolution, [], [f310, f228])).
fof(f3122, plain, (subactivity_occurrence(sK14(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f3121, f1644])).
fof(f3121, plain, (~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | subactivity_occurrence(sK14(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f3108, f255])).
fof(f3108, plain, (~ occurrence_of(sK17, tptp0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | subactivity_occurrence(sK14(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(resolution, [], [f2840, f663])).
fof(f2840, plain, (subactivity_occurrence(sK15(sK17), sK4(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f2796, f1644])).
fof(f2796, plain, (subactivity_occurrence(sK15(sK17), sK4(sK15(sK17), tptp0)) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_29), inference(superposition, [], [f282, f2486])).
fof(f6443, plain, (spl19_43 | spl19_86 | ~ spl19_21 | ~ spl19_78), inference(avatar_split_clause, [], [f6438, f5091, f1642, f6440, f2735])).
fof(f2735, plain, (spl19_43 <=> ! [X3] : ~ occurrence_of(sK4(sK15(sK17), tptp0), X3)), introduced(avatar_definition, [new_symbols(naming, [spl19_43])])).
fof(f6438, plain, (! [X4] : ((sK13(sK17) = sK13(sK4(sK15(sK17), tptp0))) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X4)) | (~ spl19_21 | ~ spl19_78)), inference(subsumption_resolution, [], [f6424, f1644])).
fof(f6424, plain, (! [X4] : ((sK13(sK17) = sK13(sK4(sK15(sK17), tptp0))) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X4) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0)) | ~ spl19_78), inference(resolution, [], [f5093, f441])).
fof(f5093, plain, (root_occ(sK13(sK17), sK4(sK15(sK17), tptp0)) | ~ spl19_78), inference(avatar_component_clause, [], [f5091])).
fof(f5372, plain, (spl19_35 | ~ spl19_77 | ~ spl19_78), inference(avatar_contradiction_clause, [], [f5371])).
fof(f5371, plain, ($false | (spl19_35 | ~ spl19_77 | ~ spl19_78)), inference(subsumption_resolution, [], [f5274, f5171])).
fof(f5171, plain, (~ root_occ(sK13(sK17), sK4(sK13(sK17), tptp0)) | (spl19_35 | ~ spl19_77)), inference(backward_demodulation, [], [f2571, f5087])).
fof(f5087, plain, ((sK13(sK17) = sK15(sK17)) | ~ spl19_77), inference(avatar_component_clause, [], [f5085])).
fof(f2571, plain, (~ root_occ(sK15(sK17), sK4(sK15(sK17), tptp0)) | spl19_35), inference(avatar_component_clause, [], [f2570])).
fof(f2570, plain, (spl19_35 <=> root_occ(sK15(sK17), sK4(sK15(sK17), tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl19_35])])).
fof(f5274, plain, (root_occ(sK13(sK17), sK4(sK13(sK17), tptp0)) | (~ spl19_77 | ~ spl19_78)), inference(backward_demodulation, [], [f5093, f5087])).
fof(f5088, plain, (spl19_76 | spl19_77 | ~ spl19_2 | ~ spl19_21 | ~ spl19_29), inference(avatar_split_clause, [], [f5080, f2484, f1642, f385, f5085, f5082])).
fof(f5080, plain, (! [X0] : ((sK13(sK17) = sK15(sK17)) | min_precedes(sK13(sK17), sK15(sK17), X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0)) | (~ spl19_2 | ~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f5079, f387])).
fof(f5079, plain, (! [X0] : ((sK13(sK17) = sK15(sK17)) | min_precedes(sK13(sK17), sK15(sK17), X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | ~ leaf(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f5078, f240])).
fof(f5078, plain, (! [X0] : ((sK13(sK17) = sK15(sK17)) | min_precedes(sK13(sK17), sK15(sK17), X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | atomic(tptp0) | ~ leaf(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f5069, f281])).
fof(f5069, plain, (! [X0] : (~ arboreal(sK13(sK17)) | (sK13(sK17) = sK15(sK17)) | min_precedes(sK13(sK17), sK15(sK17), X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | atomic(tptp0) | ~ leaf(sK15(sK17), tptp0)) | (~ spl19_21 | ~ spl19_29)), inference(resolution, [], [f5040, f534])).
fof(f534, plain, ! [X6, X8, X7, X5] : (~ subactivity_occurrence(X5, sK4(X6, X8)) | ~ arboreal(X5) | (X5 = X6) | min_precedes(X5, X6, X7) | ~ occurrence_of(sK4(X6, X8), X7) | atomic(X8) | ~ leaf(X6, X8)), inference(resolution, [], [f181, f189])).
fof(f189, plain, ! [X0, X1] : (leaf_occ(X0, sK4(X0, X1)) | atomic(X1) | ~ leaf(X0, X1)), inference(cnf_transformation, [], [f140])).
fof(f4159, plain, (~ spl19_2 | ~ spl19_43), inference(avatar_contradiction_clause, [], [f4158])).
fof(f4158, plain, ($false | (~ spl19_2 | ~ spl19_43)), inference(subsumption_resolution, [], [f4157, f387])).
fof(f4157, plain, (~ leaf(sK15(sK17), tptp0) | ~ spl19_43), inference(subsumption_resolution, [], [f4151, f240])).
fof(f4151, plain, (atomic(tptp0) | ~ leaf(sK15(sK17), tptp0) | ~ spl19_43), inference(resolution, [], [f2736, f188])).
fof(f2736, plain, (! [X3] : ~ occurrence_of(sK4(sK15(sK17), tptp0), X3) | ~ spl19_43), inference(avatar_component_clause, [], [f2735])).
fof(f2731, plain, (~ spl19_21 | ~ spl19_35), inference(avatar_contradiction_clause, [], [f2730])).
fof(f2730, plain, ($false | (~ spl19_21 | ~ spl19_35)), inference(subsumption_resolution, [], [f2729, f1644])).
fof(f2729, plain, (~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_35), inference(subsumption_resolution, [], [f2708, f255])).
fof(f2708, plain, (~ occurrence_of(sK17, tptp0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_35), inference(resolution, [], [f2572, f656])).
fof(f656, plain, ! [X12, X13] : (~ root_occ(sK15(X12), X13) | ~ occurrence_of(X12, tptp0) | ~ occurrence_of(X13, tptp0)), inference(resolution, [], [f314, f192])).
fof(f192, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X3, X1, X2) | ~ root_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X3, X1, X2) | ~ root_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f104])).
fof(f104, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X3, X1, X2) | (~ root_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1, X2] : ((root_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X3, X1, X2)), inference(rectify, [], [f11])).
fof(f11, plain, ! [X35, X36, X37] : ((root_occ(X36, X35) & occurrence_of(X35, X37)) => ~ ? [X38] : min_precedes(X38, X36, X37)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_10)).
fof(f2572, plain, (root_occ(sK15(sK17), sK4(sK15(sK17), tptp0)) | ~ spl19_35), inference(avatar_component_clause, [], [f2570])).
fof(f2674, plain, (~ spl19_2 | ~ spl19_30), inference(avatar_contradiction_clause, [], [f2673])).
fof(f2673, plain, ($false | (~ spl19_2 | ~ spl19_30)), inference(subsumption_resolution, [], [f2672, f387])).
fof(f2672, plain, (~ leaf(sK15(sK17), tptp0) | ~ spl19_30), inference(subsumption_resolution, [], [f2671, f240])).
fof(f2671, plain, (atomic(tptp0) | ~ leaf(sK15(sK17), tptp0) | ~ spl19_30), inference(duplicate_literal_removal, [], [f2666])).
fof(f2666, plain, (atomic(tptp0) | atomic(tptp0) | ~ leaf(sK15(sK17), tptp0) | ~ spl19_30), inference(resolution, [], [f2489, f188])).
fof(f2489, plain, (! [X0] : (~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | atomic(X0)) | ~ spl19_30), inference(avatar_component_clause, [], [f2488])).
fof(f2488, plain, (spl19_30 <=> ! [X0] : (atomic(X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0))), introduced(avatar_definition, [new_symbols(naming, [spl19_30])])).
fof(f2579, plain, (~ spl19_12 | ~ spl19_21 | ~ spl19_29), inference(avatar_split_clause, [], [f2578, f2484, f1642, f753])).
fof(f2578, plain, (~ root(sK15(sK17), tptp0) | (~ spl19_21 | ~ spl19_29)), inference(subsumption_resolution, [], [f2510, f1644])).
fof(f2510, plain, (~ root(sK15(sK17), tptp0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ spl19_29), inference(superposition, [], [f659, f2486])).
fof(f659, plain, ! [X16] : (~ root(sK15(X16), tptp0) | ~ occurrence_of(X16, tptp0)), inference(resolution, [], [f314, f218])).
fof(f218, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | ~ root(X1, X2)), inference(cnf_transformation, [], [f114])).
fof(f114, plain, ! [X0, X1, X2] : (~ root(X1, X2) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f69])).
fof(f69, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => ~ root(X1, X2)), inference(rectify, [], [f23])).
fof(f23, plain, ! [X65, X66, X67] : (min_precedes(X65, X66, X67) => ~ root(X66, X67)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_22)).
fof(f2490, plain, (spl19_29 | spl19_30 | ~ spl19_2 | ~ spl19_21), inference(avatar_split_clause, [], [f2482, f1642, f385, f2488, f2484])).
fof(f2482, plain, (! [X0] : (atomic(X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | (sK15(sK17) = sK15(sK4(sK15(sK17), tptp0)))) | (~ spl19_2 | ~ spl19_21)), inference(subsumption_resolution, [], [f2481, f387])).
fof(f2481, plain, (! [X0] : (atomic(X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | (sK15(sK17) = sK15(sK4(sK15(sK17), tptp0))) | ~ leaf(sK15(sK17), tptp0)) | ~ spl19_21), inference(subsumption_resolution, [], [f2478, f240])).
fof(f2478, plain, (! [X0] : (atomic(X0) | ~ occurrence_of(sK4(sK15(sK17), tptp0), X0) | (sK15(sK17) = sK15(sK4(sK15(sK17), tptp0))) | atomic(tptp0) | ~ leaf(sK15(sK17), tptp0)) | ~ spl19_21), inference(resolution, [], [f858, f1644])).
fof(f858, plain, ! [X4, X5, X3] : (~ occurrence_of(sK4(X3, X4), tptp0) | atomic(X5) | ~ occurrence_of(sK4(X3, X4), X5) | (sK15(sK4(X3, X4)) = X3) | atomic(X4) | ~ leaf(X3, X4)), inference(resolution, [], [f473, f189])).
fof(f473, plain, ! [X2, X0, X1] : (~ leaf_occ(X0, X1) | (sK15(X1) = X0) | atomic(X2) | ~ occurrence_of(X1, X2) | ~ occurrence_of(X1, tptp0)), inference(resolution, [], [f229, f239])).
fof(f229, plain, ! [X2, X0, X3, X1] : (~ leaf_occ(X1, X2) | (X0 = X1) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3)), inference(cnf_transformation, [], [f122])).
fof(f122, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | ~ leaf_occ(X1, X2) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3)), inference(flattening, [], [f121])).
fof(f121, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | (~ leaf_occ(X1, X2) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3))), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1, X2, X3] : ((leaf_occ(X1, X2) & leaf_occ(X0, X2) & ~ atomic(X3) & occurrence_of(X2, X3)) => (X0 = X1)), inference(rectify, [], [f29])).
fof(f29, plain, ! [X86, X87, X88, X89] : ((leaf_occ(X87, X88) & leaf_occ(X86, X88) & ~ atomic(X89) & occurrence_of(X88, X89)) => (X86 = X87)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO008+4.p', sos_28)).
fof(f1671, plain, (~ spl19_2 | spl19_19), inference(avatar_contradiction_clause, [], [f1670])).
fof(f1670, plain, ($false | (~ spl19_2 | spl19_19)), inference(subsumption_resolution, [], [f1669, f240])).
fof(f1669, plain, (atomic(tptp0) | (~ spl19_2 | spl19_19)), inference(subsumption_resolution, [], [f1668, f387])).
fof(f1668, plain, (~ leaf(sK15(sK17), tptp0) | atomic(tptp0) | spl19_19), inference(resolution, [], [f1635, f347])).
fof(f347, plain, ! [X8, X7] : (activity_occurrence(sK4(X8, X7)) | ~ leaf(X8, X7) | atomic(X7)), inference(resolution, [], [f188, f182])).
fof(f1635, plain, (~ activity_occurrence(sK4(sK15(sK17), tptp0)) | spl19_19), inference(avatar_component_clause, [], [f1633])).
fof(f1633, plain, (spl19_19 <=> activity_occurrence(sK4(sK15(sK17), tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl19_19])])).
fof(f1645, plain, (~ spl19_19 | spl19_21 | ~ spl19_2), inference(avatar_split_clause, [], [f1630, f385, f1642, f1633])).
fof(f1630, plain, (occurrence_of(sK4(sK15(sK17), tptp0), tptp0) | ~ activity_occurrence(sK4(sK15(sK17), tptp0)) | ~ spl19_2), inference(superposition, [], [f195, f1627])).
fof(f1627, plain, ((tptp0 = sK5(sK4(sK15(sK17), tptp0))) | ~ spl19_2), inference(subsumption_resolution, [], [f1626, f240])).
fof(f1626, plain, (atomic(tptp0) | (tptp0 = sK5(sK4(sK15(sK17), tptp0))) | ~ spl19_2), inference(resolution, [], [f372, f387])).
fof(f372, plain, ! [X8, X9] : (~ leaf(X8, X9) | atomic(X9) | (sK5(sK4(X8, X9)) = X9)), inference(resolution, [], [f302, f188])).
fof(f482, plain, ~ spl19_7, inference(avatar_contradiction_clause, [], [f481])).
fof(f481, plain, ($false | ~ spl19_7), inference(subsumption_resolution, [], [f477, f264])).
fof(f264, plain, activity_occurrence(sK17), inference(resolution, [], [f182, f255])).
fof(f477, plain, (~ activity_occurrence(sK17) | ~ spl19_7), inference(resolution, [], [f468, f195])).
fof(f468, plain, (! [X1] : ~ occurrence_of(sK17, X1) | ~ spl19_7), inference(avatar_component_clause, [], [f467])).
fof(f472, plain, (spl19_7 | spl19_8 | ~ spl19_5), inference(avatar_split_clause, [], [f461, f445, f470, f467])).
fof(f461, plain, (! [X0, X1] : ((sK13(sK17) = X0) | ~ root_occ(X0, sK17) | ~ occurrence_of(sK17, X1)) | ~ spl19_5), inference(resolution, [], [f446, f230])).
fof(f457, plain, spl19_5, inference(avatar_contradiction_clause, [], [f456])).
fof(f456, plain, ($false | spl19_5), inference(subsumption_resolution, [], [f455, f255])).
fof(f455, plain, (~ occurrence_of(sK17, tptp0) | spl19_5), inference(resolution, [], [f447, f234])).
fof(f447, plain, (~ root_occ(sK13(sK17), sK17) | spl19_5), inference(avatar_component_clause, [], [f445])).
fof(f452, plain, (~ spl19_5 | spl19_6), inference(avatar_split_clause, [], [f442, f449, f445])).
fof(f442, plain, (root(sK13(sK17), tptp0) | ~ root_occ(sK13(sK17), sK17)), inference(superposition, [], [f212, f440])).
fof(f440, plain, (tptp0 = sK10(sK13(sK17), sK17)), inference(subsumption_resolution, [], [f439, f255])).
fof(f439, plain, ((tptp0 = sK10(sK13(sK17), sK17)) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f306, f234])).
fof(f306, plain, ! [X1] : (~ root_occ(X1, sK17) | (tptp0 = sK10(X1, sK17))), inference(resolution, [], [f301, f210])).
fof(f210, plain, ! [X0, X1] : (occurrence_of(X1, sK10(X0, X1)) | ~ root_occ(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f396, plain, spl19_1, inference(avatar_contradiction_clause, [], [f395])).
fof(f395, plain, ($false | spl19_1), inference(subsumption_resolution, [], [f394, f255])).
fof(f394, plain, (~ occurrence_of(sK17, tptp0) | spl19_1), inference(resolution, [], [f383, f239])).
fof(f383, plain, (~ leaf_occ(sK15(sK17), sK17) | spl19_1), inference(avatar_component_clause, [], [f381])).
fof(f388, plain, (~ spl19_1 | spl19_2), inference(avatar_split_clause, [], [f378, f385, f381])).
fof(f378, plain, (leaf(sK15(sK17), tptp0) | ~ leaf_occ(sK15(sK17), sK17)), inference(superposition, [], [f208, f377])).