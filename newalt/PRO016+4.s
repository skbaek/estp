fof(f53389, plain, $false, inference(avatar_sat_refutation, [], [f1178, f1223, f1304, f1464, f1480, f8427, f12213, f12303, f53380])).
fof(f53380, plain, (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316), inference(avatar_contradiction_clause, [], [f53379])).
fof(f53379, plain, ($false | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53378, f3729])).
fof(f3729, plain, (subactivity_occurrence(sK13(sK16, sK17), sK17) | ~ spl19_85), inference(avatar_component_clause, [], [f3728])).
fof(f3728, plain, (spl19_85 <=> subactivity_occurrence(sK13(sK16, sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_85])])).
fof(f53378, plain, (~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53377, f253])).
fof(f253, plain, arboreal(sK16), inference(cnf_transformation, [], [f177])).
fof(f177, plain, (! [X2, X3] : (((min_precedes(X2, sK18(X2), tptp0) & occurrence_of(sK18(X2), tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK17) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK16, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK16, sK17) & arboreal(sK16) & subactivity_occurrence(sK16, sK17) & occurrence_of(sK17, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16, sK17, sK18])], [f136, f176, f175])).
fof(f175, plain, (? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => (! [X3, X2] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK17) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK16, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK16, sK17) & arboreal(sK16) & subactivity_occurrence(sK16, sK17) & occurrence_of(sK17, tptp0))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X2] : (? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) => (min_precedes(X2, sK18(X2), tptp0) & occurrence_of(sK18(X2), tptp1))), introduced(choice_axiom, [])).
fof(f136, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(definition_folding, [], [f134, e135])).
fof(f135, plain, ! [X2, X3] : ((? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ sP0(X2, X3)), inference(usedef, [], [e135])).
fof(e135, plain, ! [X2, X3] : (sP0(X2, X3) <=> (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f134, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(flattening, [], [f133])).
fof(f133, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & (~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ~ ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3] : ((occurrence_of(X3, tptp2) => ~ ? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1))) & (occurrence_of(X3, tptp1) => ~ ? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2))) & leaf_occ(X3, X1) & min_precedes(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f47])).
fof(f47, plain, ~ ! [X106, X107] : ((~ leaf_occ(X106, X107) & arboreal(X106) & subactivity_occurrence(X106, X107) & occurrence_of(X107, tptp0)) => ? [X108, X109] : ((occurrence_of(X109, tptp2) => ~ ? [X111] : (min_precedes(X108, X111, tptp0) & occurrence_of(X111, tptp1))) & (occurrence_of(X109, tptp1) => ~ ? [X110] : (min_precedes(X108, X110, tptp0) & occurrence_of(X110, tptp2))) & leaf_occ(X109, X107) & min_precedes(X108, X109, tptp0) & (occurrence_of(X109, tptp2) | occurrence_of(X109, tptp1)) & next_subocc(X106, X108, tptp0) & occurrence_of(X108, tptp3))), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ ! [X106, X107] : ((~ leaf_occ(X106, X107) & arboreal(X106) & subactivity_occurrence(X106, X107) & occurrence_of(X107, tptp0)) => ? [X108, X109] : ((occurrence_of(X109, tptp2) => ~ ? [X111] : (min_precedes(X108, X111, tptp0) & occurrence_of(X111, tptp1))) & (occurrence_of(X109, tptp1) => ~ ? [X110] : (min_precedes(X108, X110, tptp0) & occurrence_of(X110, tptp2))) & leaf_occ(X109, X107) & min_precedes(X108, X109, tptp0) & (occurrence_of(X109, tptp2) | occurrence_of(X109, tptp1)) & next_subocc(X106, X108, tptp0) & occurrence_of(X108, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', goals)).
fof(f53377, plain, (~ arboreal(sK16) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53376, f252])).
fof(f252, plain, subactivity_occurrence(sK16, sK17), inference(cnf_transformation, [], [f177])).
fof(f53376, plain, (~ subactivity_occurrence(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53375, f254])).
fof(f254, plain, ~ leaf_occ(sK16, sK17), inference(cnf_transformation, [], [f177])).
fof(f53375, plain, (leaf_occ(sK16, sK17) | ~ subactivity_occurrence(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53374, f251])).
fof(f251, plain, occurrence_of(sK17, tptp0), inference(cnf_transformation, [], [f177])).
fof(f53374, plain, (~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | ~ subactivity_occurrence(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(duplicate_literal_removal, [], [f53367])).
fof(f53367, plain, (~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | ~ subactivity_occurrence(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | ~ occurrence_of(sK17, tptp0) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(resolution, [], [f53234, f2354])).
fof(f2354, plain, ! [X4, X2, X3] : (~ precedes(sK12(sK13(X2, X3), X4), sK14(X2, X3)) | ~ occurrence_of(X3, tptp0) | leaf_occ(X2, X3) | ~ subactivity_occurrence(X2, X3) | ~ arboreal(X2) | ~ subactivity_occurrence(sK13(X2, X3), X4) | ~ occurrence_of(X4, tptp0)), inference(subsumption_resolution, [], [f2353, f754])).
fof(f754, plain, ! [X6, X4, X5] : (~ leaf_occ(sK13(X4, X5), X6) | ~ subactivity_occurrence(X4, X5) | ~ occurrence_of(X5, tptp0) | leaf_occ(X4, X5) | ~ arboreal(X4) | ~ occurrence_of(X6, tptp0)), inference(resolution, [], [f430, f191])).
fof(f191, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f103])).
fof(f103, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | (~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f56])).
fof(f56, plain, ! [X0, X1, X2] : ((leaf_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X1, X3, X2)), inference(rectify, [], [f10])).
fof(f10, plain, ! [X31, X32, X33] : ((leaf_occ(X32, X31) & occurrence_of(X31, X33)) => ~ ? [X34] : min_precedes(X32, X34, X33)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_09)).
fof(f430, plain, ! [X4, X3] : (min_precedes(sK13(X3, X4), sK14(X3, X4), tptp0) | ~ arboreal(X3) | ~ subactivity_occurrence(X3, X4) | ~ occurrence_of(X4, tptp0) | leaf_occ(X3, X4)), inference(resolution, [], [f235, f221])).
fof(f221, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK11(X0, X1, X2), X1, X2) & min_precedes(X0, sK11(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f166, f167])).
fof(f167, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK11(X0, X1, X2), X1, X2) & min_precedes(X0, sK11(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f166, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f164])).
fof(f164, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f120])).
fof(f120, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f27])).
fof(f27, plain, ! [X78, X79, X80] : (next_subocc(X78, X79, X80) <=> (~ ? [X81] : (min_precedes(X81, X79, X80) & min_precedes(X78, X81, X80)) & min_precedes(X78, X79, X80))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_26)).
fof(f235, plain, ! [X0, X1] : (next_subocc(sK13(X0, X1), sK14(X0, X1), tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0, X1] : ((leaf_occ(sK14(X0, X1), X1) & next_subocc(sK13(X0, X1), sK14(X0, X1), tptp0) & (occurrence_of(sK14(X0, X1), tptp2) | occurrence_of(sK14(X0, X1), tptp1)) & next_subocc(sK12(X0, X1), sK13(X0, X1), tptp0) & occurrence_of(sK13(X0, X1), tptp4) & next_subocc(X0, sK12(X0, X1), tptp0) & occurrence_of(sK12(X0, X1), tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12, sK13, sK14])], [f132, f169])).
fof(f169, plain, ! [X1, X0] : (? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) => (leaf_occ(sK14(X0, X1), X1) & next_subocc(sK13(X0, X1), sK14(X0, X1), tptp0) & (occurrence_of(sK14(X0, X1), tptp2) | occurrence_of(sK14(X0, X1), tptp1)) & next_subocc(sK12(X0, X1), sK13(X0, X1), tptp0) & occurrence_of(sK13(X0, X1), tptp4) & next_subocc(X0, sK12(X0, X1), tptp0) & occurrence_of(sK12(X0, X1), tptp3))), introduced(choice_axiom, [])).
fof(f132, plain, ! [X0, X1] : (? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(flattening, [], [f131])).
fof(f131, plain, ! [X0, X1] : (? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | (leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f33])).
fof(f33, plain, ! [X101, X102] : ((~ leaf_occ(X101, X102) & arboreal(X101) & subactivity_occurrence(X101, X102) & occurrence_of(X102, tptp0)) => ? [X103, X104, X105] : (leaf_occ(X105, X102) & next_subocc(X104, X105, tptp0) & (occurrence_of(X105, tptp2) | occurrence_of(X105, tptp1)) & next_subocc(X103, X104, tptp0) & occurrence_of(X104, tptp4) & next_subocc(X101, X103, tptp0) & occurrence_of(X103, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_32)).
fof(f2353, plain, ! [X4, X2, X3] : (~ subactivity_occurrence(X2, X3) | ~ occurrence_of(X3, tptp0) | leaf_occ(X2, X3) | ~ precedes(sK12(sK13(X2, X3), X4), sK14(X2, X3)) | ~ arboreal(X2) | ~ subactivity_occurrence(sK13(X2, X3), X4) | ~ occurrence_of(X4, tptp0) | leaf_occ(sK13(X2, X3), X4)), inference(subsumption_resolution, [], [f2340, f402])).
fof(f402, plain, ! [X6, X5] : (arboreal(sK13(X5, X6)) | ~ arboreal(X5) | ~ subactivity_occurrence(X5, X6) | ~ occurrence_of(X6, tptp0) | leaf_occ(X5, X6)), inference(subsumption_resolution, [], [f399, f238])).
fof(f238, plain, atomic(tptp4), inference(cnf_transformation, [], [f36])).
fof(f36, plain, atomic(tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_35)).
fof(f399, plain, ! [X6, X5] : (leaf_occ(X5, X6) | ~ arboreal(X5) | ~ subactivity_occurrence(X5, X6) | ~ occurrence_of(X6, tptp0) | ~ atomic(tptp4) | arboreal(sK13(X5, X6))), inference(resolution, [], [f232, f204])).
fof(f204, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f155])).
fof(f155, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f112])).
fof(f112, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f17])).
fof(f17, plain, ! [X51, X52] : (occurrence_of(X51, X52) => (arboreal(X51) <=> atomic(X52))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_16)).
fof(f232, plain, ! [X0, X1] : (occurrence_of(sK13(X0, X1), tptp4) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f2340, plain, ! [X4, X2, X3] : (~ subactivity_occurrence(X2, X3) | ~ occurrence_of(X3, tptp0) | leaf_occ(X2, X3) | ~ precedes(sK12(sK13(X2, X3), X4), sK14(X2, X3)) | ~ arboreal(X2) | ~ arboreal(sK13(X2, X3)) | ~ subactivity_occurrence(sK13(X2, X3), X4) | ~ occurrence_of(X4, tptp0) | leaf_occ(sK13(X2, X3), X4)), inference(resolution, [], [f768, f415])).
fof(f415, plain, ! [X4, X3] : (min_precedes(X3, sK12(X3, X4), tptp0) | ~ arboreal(X3) | ~ subactivity_occurrence(X3, X4) | ~ occurrence_of(X4, tptp0) | leaf_occ(X3, X4)), inference(resolution, [], [f231, f221])).
fof(f231, plain, ! [X0, X1] : (next_subocc(X0, sK12(X0, X1), tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f768, plain, ! [X26, X27, X25] : (~ min_precedes(sK13(X25, X26), X27, tptp0) | ~ subactivity_occurrence(X25, X26) | ~ occurrence_of(X26, tptp0) | leaf_occ(X25, X26) | ~ precedes(X27, sK14(X25, X26)) | ~ arboreal(X25)), inference(subsumption_resolution, [], [f763, f429])).
fof(f429, plain, ! [X2, X0, X1] : (~ min_precedes(sK13(X0, X1), X2, tptp0) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ min_precedes(X2, sK14(X0, X1), tptp0)), inference(resolution, [], [f235, f222])).
fof(f222, plain, ! [X4, X2, X0, X1] : (~ next_subocc(X0, X1, X2) | ~ min_precedes(X0, X4, X2) | ~ min_precedes(X4, X1, X2)), inference(cnf_transformation, [], [f168])).
fof(f763, plain, ! [X26, X27, X25] : (~ arboreal(X25) | ~ subactivity_occurrence(X25, X26) | ~ occurrence_of(X26, tptp0) | leaf_occ(X25, X26) | ~ precedes(X27, sK14(X25, X26)) | min_precedes(X27, sK14(X25, X26), tptp0) | ~ min_precedes(sK13(X25, X26), X27, tptp0)), inference(resolution, [], [f430, f229])).
fof(f229, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X0, X2, X3) | ~ precedes(X1, X2) | min_precedes(X1, X2, X3) | ~ min_precedes(X0, X1, X3)), inference(cnf_transformation, [], [f130])).
fof(f130, plain, ! [X0, X1, X2, X3] : (min_precedes(X1, X2, X3) | ~ precedes(X1, X2) | ~ min_precedes(X0, X2, X3) | ~ min_precedes(X0, X1, X3)), inference(flattening, [], [f129])).
fof(f129, plain, ! [X0, X1, X2, X3] : (min_precedes(X1, X2, X3) | (~ precedes(X1, X2) | ~ min_precedes(X0, X2, X3) | ~ min_precedes(X0, X1, X3))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1, X2, X3] : ((precedes(X1, X2) & min_precedes(X0, X2, X3) & min_precedes(X0, X1, X3)) => min_precedes(X1, X2, X3)), inference(rectify, [], [f32])).
fof(f32, plain, ! [X97, X98, X99, X100] : ((precedes(X98, X99) & min_precedes(X97, X99, X100) & min_precedes(X97, X98, X100)) => min_precedes(X98, X99, X100)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_31)).
fof(f53234, plain, (precedes(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | (~ spl19_8 | ~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53228, f1216])).
fof(f1216, plain, (legal(sK14(sK16, sK17)) | ~ spl19_8), inference(resolution, [], [f1146, f457])).
fof(f457, plain, ! [X0, X1] : (~ leaf_occ(X0, X1) | legal(X0)), inference(resolution, [], [f454, f208])).
fof(f208, plain, ! [X0, X1] : (leaf(X0, sK9(X0, X1)) | ~ leaf_occ(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & ((leaf(X0, sK9(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK9(X0, X1))) | ~ leaf_occ(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f157, f158])).
fof(f158, plain, ! [X1, X0] : (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) => (leaf(X0, sK9(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f157, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) | ~ leaf_occ(X0, X1))), inference(rectify, [], [f156])).
fof(f156, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2)) | ~ leaf_occ(X0, X1))), inference(nnf_transformation, [], [f65])).
fof(f65, plain, ! [X0, X1] : (leaf_occ(X0, X1) <=> ? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2))), inference(rectify, [], [f19])).
fof(f19, plain, ! [X55, X56] : (leaf_occ(X55, X56) <=> ? [X57] : (leaf(X55, X57) & subactivity_occurrence(X55, X56) & occurrence_of(X56, X57))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_18)).
fof(f454, plain, ! [X2, X3] : (~ leaf(X2, X3) | legal(X2)), inference(subsumption_resolution, [], [f453, f205])).
fof(f205, plain, ! [X0, X1] : (~ root(X0, X1) | legal(X0)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (legal(X0) | ~ root(X0, X1)), inference(ennf_transformation, [], [f64])).
fof(f64, plain, ! [X0, X1] : (root(X0, X1) => legal(X0)), inference(rectify, [], [f18])).
fof(f18, plain, ! [X53, X54] : (root(X53, X54) => legal(X53)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_17)).
fof(f453, plain, ! [X2, X3] : (~ leaf(X2, X3) | root(X2, X3) | legal(X2)), inference(resolution, [], [f319, f213])).
fof(f213, plain, ! [X0, X1] : (~ precedes(X0, X1) | legal(X1)), inference(cnf_transformation, [], [f161])).
fof(f161, plain, ! [X0, X1] : ((precedes(X0, X1) | ~ legal(X1) | ~ earlier(X0, X1)) & ((legal(X1) & earlier(X0, X1)) | ~ precedes(X0, X1))), inference(flattening, [], [f160])).
fof(f160, plain, ! [X0, X1] : ((precedes(X0, X1) | (~ legal(X1) | ~ earlier(X0, X1))) & ((legal(X1) & earlier(X0, X1)) | ~ precedes(X0, X1))), inference(nnf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1] : (precedes(X0, X1) <=> (legal(X1) & earlier(X0, X1))), inference(rectify, [], [f22])).
fof(f22, plain, ! [X63, X64] : (precedes(X63, X64) <=> (legal(X64) & earlier(X63, X64))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_21)).
fof(f319, plain, ! [X14, X15] : (precedes(sK8(X14, X15), X14) | ~ leaf(X14, X15) | root(X14, X15)), inference(resolution, [], [f199, f218])).
fof(f218, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | precedes(X0, X1)), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : (precedes(X0, X1) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => precedes(X0, X1)), inference(rectify, [], [f25])).
fof(f25, plain, ! [X72, X73, X74] : (min_precedes(X72, X73, X74) => precedes(X72, X73)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_24)).
fof(f199, plain, ! [X0, X1] : (min_precedes(sK8(X0, X1), X0, X1) | root(X0, X1) | ~ leaf(X0, X1)), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ! [X0, X1] : ((leaf(X0, X1) | min_precedes(X0, sK7(X0, X1), X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (min_precedes(sK8(X0, X1), X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f151, f153, f152])).
fof(f152, plain, ! [X1, X0] : (? [X2] : min_precedes(X0, X2, X1) => min_precedes(X0, sK7(X0, X1), X1)), introduced(choice_axiom, [])).
fof(f153, plain, ! [X1, X0] : (? [X5] : min_precedes(X5, X0, X1) => min_precedes(sK8(X0, X1), X0, X1)), introduced(choice_axiom, [])).
fof(f151, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (? [X5] : min_precedes(X5, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(rectify, [], [f150])).
fof(f150, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(flattening, [], [f149])).
fof(f149, plain, ! [X0, X1] : ((leaf(X0, X1) | (? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1)))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(nnf_transformation, [], [f111])).
fof(f111, plain, ! [X0, X1] : (leaf(X0, X1) <=> (! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(ennf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1] : (leaf(X0, X1) <=> (~ ? [X2] : min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(rectify, [], [f16])).
fof(f16, plain, ! [X47, X48] : (leaf(X47, X48) <=> (~ ? [X50] : min_precedes(X47, X50, X48) & (? [X49] : min_precedes(X49, X47, X48) | root(X47, X48)))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_15)).
fof(f1146, plain, (leaf_occ(sK14(sK16, sK17), sK17) | ~ spl19_8), inference(avatar_component_clause, [], [f1145])).
fof(f1145, plain, (spl19_8 <=> leaf_occ(sK14(sK16, sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_8])])).
fof(f53228, plain, (~ legal(sK14(sK16, sK17)) | precedes(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(resolution, [], [f53226, f214])).
fof(f214, plain, ! [X0, X1] : (~ earlier(X0, X1) | ~ legal(X1) | precedes(X0, X1)), inference(cnf_transformation, [], [f161])).
fof(f53226, plain, (earlier(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53225, f3729])).
fof(f53225, plain, (earlier(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53224, f1463])).
fof(f1463, plain, (arboreal(sK13(sK16, sK17)) | ~ spl19_28), inference(avatar_component_clause, [], [f1461])).
fof(f1461, plain, (spl19_28 <=> arboreal(sK13(sK16, sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_28])])).
fof(f53224, plain, (earlier(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53223, f8728])).
fof(f8728, plain, (~ leaf_occ(sK13(sK16, sK17), sK17) | spl19_229), inference(avatar_component_clause, [], [f8727])).
fof(f8727, plain, (spl19_229 <=> leaf_occ(sK13(sK16, sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_229])])).
fof(f53223, plain, (earlier(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | leaf_occ(sK13(sK16, sK17), sK17) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f53221, f251])).
fof(f53221, plain, (earlier(sK12(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | ~ occurrence_of(sK17, tptp0) | leaf_occ(sK13(sK16, sK17), sK17) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(resolution, [], [f13997, f1799])).
fof(f1799, plain, ! [X0, X1] : (earlier(sK12(X0, X1), sK13(X0, X1)) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1)), inference(resolution, [], [f735, f212])).
fof(f212, plain, ! [X0, X1] : (~ precedes(X0, X1) | earlier(X0, X1)), inference(cnf_transformation, [], [f161])).
fof(f735, plain, ! [X21, X20] : (precedes(sK12(X20, X21), sK13(X20, X21)) | ~ subactivity_occurrence(X20, X21) | ~ occurrence_of(X21, tptp0) | leaf_occ(X20, X21) | ~ arboreal(X20)), inference(resolution, [], [f424, f218])).
fof(f424, plain, ! [X4, X3] : (min_precedes(sK12(X3, X4), sK13(X3, X4), tptp0) | ~ arboreal(X3) | ~ subactivity_occurrence(X3, X4) | ~ occurrence_of(X4, tptp0) | leaf_occ(X3, X4)), inference(resolution, [], [f233, f221])).
fof(f233, plain, ! [X0, X1] : (next_subocc(sK12(X0, X1), sK13(X0, X1), tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f13997, plain, (! [X0] : (~ earlier(X0, sK13(sK13(sK16, sK17), sK17)) | earlier(X0, sK14(sK16, sK17))) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(resolution, [], [f12637, f228])).
fof(f228, plain, ! [X2, X0, X1] : (~ earlier(X1, X2) | earlier(X0, X2) | ~ earlier(X0, X1)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0, X1, X2] : (earlier(X0, X2) | ~ earlier(X1, X2) | ~ earlier(X0, X1)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0, X1, X2] : (earlier(X0, X2) | (~ earlier(X1, X2) | ~ earlier(X0, X1))), inference(ennf_transformation, [], [f77])).
fof(f77, plain, ! [X0, X1, X2] : ((earlier(X1, X2) & earlier(X0, X1)) => earlier(X0, X2)), inference(rectify, [], [f31])).
fof(f31, plain, ! [X94, X95, X96] : ((earlier(X95, X96) & earlier(X94, X95)) => earlier(X94, X96)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_30)).
fof(f12637, plain, (earlier(sK13(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | (~ spl19_14 | ~ spl19_28 | ~ spl19_85 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f12636, f3729])).
fof(f12636, plain, (earlier(sK13(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_28 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f12635, f1463])).
fof(f12635, plain, (earlier(sK13(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | spl19_229 | ~ spl19_316)), inference(subsumption_resolution, [], [f12634, f8728])).
fof(f12634, plain, (earlier(sK13(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | leaf_occ(sK13(sK16, sK17), sK17) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_316)), inference(subsumption_resolution, [], [f12560, f251])).
fof(f12560, plain, (earlier(sK13(sK13(sK16, sK17), sK17), sK14(sK16, sK17)) | ~ occurrence_of(sK17, tptp0) | leaf_occ(sK13(sK16, sK17), sK17) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | (~ spl19_14 | ~ spl19_316)), inference(superposition, [], [f1804, f12433])).
fof(f12433, plain, ((sK14(sK16, sK17) = sK14(sK13(sK16, sK17), sK17)) | (~ spl19_14 | ~ spl19_316)), inference(resolution, [], [f12119, f1222])).
fof(f1222, plain, (! [X2] : (~ leaf_occ(X2, sK17) | (sK14(sK16, sK17) = X2)) | ~ spl19_14), inference(avatar_component_clause, [], [f1221])).
fof(f1221, plain, (spl19_14 <=> ! [X2] : ((sK14(sK16, sK17) = X2) | ~ leaf_occ(X2, sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_14])])).
fof(f12119, plain, (leaf_occ(sK14(sK13(sK16, sK17), sK17), sK17) | ~ spl19_316), inference(avatar_component_clause, [], [f12118])).
fof(f12118, plain, (spl19_316 <=> leaf_occ(sK14(sK13(sK16, sK17), sK17), sK17)), introduced(avatar_definition, [new_symbols(naming, [spl19_316])])).
fof(f1804, plain, ! [X0, X1] : (earlier(sK13(X0, X1), sK14(X0, X1)) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1)), inference(resolution, [], [f761, f212])).
fof(f761, plain, ! [X21, X20] : (precedes(sK13(X20, X21), sK14(X20, X21)) | ~ subactivity_occurrence(X20, X21) | ~ occurrence_of(X21, tptp0) | leaf_occ(X20, X21) | ~ arboreal(X20)), inference(resolution, [], [f430, f218])).
fof(f12303, plain, ~ spl19_229, inference(avatar_contradiction_clause, [], [f12302])).
fof(f12302, plain, ($false | ~ spl19_229), inference(subsumption_resolution, [], [f12301, f253])).
fof(f12301, plain, (~ arboreal(sK16) | ~ spl19_229), inference(subsumption_resolution, [], [f12300, f254])).
fof(f12300, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ spl19_229), inference(subsumption_resolution, [], [f12299, f251])).
fof(f12299, plain, (~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ spl19_229), inference(subsumption_resolution, [], [f12298, f252])).
fof(f12298, plain, (~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ spl19_229), inference(duplicate_literal_removal, [], [f12269])).
fof(f12269, plain, (~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ occurrence_of(sK17, tptp0) | ~ spl19_229), inference(resolution, [], [f8729, f754])).
fof(f8729, plain, (leaf_occ(sK13(sK16, sK17), sK17) | ~ spl19_229), inference(avatar_component_clause, [], [f8727])).
fof(f12213, plain, (spl19_229 | ~ spl19_28 | ~ spl19_85 | spl19_316), inference(avatar_split_clause, [], [f12212, f12118, f3728, f1461, f8727])).
fof(f12212, plain, (leaf_occ(sK13(sK16, sK17), sK17) | (~ spl19_28 | ~ spl19_85 | spl19_316)), inference(subsumption_resolution, [], [f12211, f251])).
fof(f12211, plain, (leaf_occ(sK13(sK16, sK17), sK17) | ~ occurrence_of(sK17, tptp0) | (~ spl19_28 | ~ spl19_85 | spl19_316)), inference(subsumption_resolution, [], [f12210, f3729])).
fof(f12210, plain, (leaf_occ(sK13(sK16, sK17), sK17) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | ~ occurrence_of(sK17, tptp0) | (~ spl19_28 | spl19_316)), inference(subsumption_resolution, [], [f12204, f1463])).
fof(f12204, plain, (leaf_occ(sK13(sK16, sK17), sK17) | ~ arboreal(sK13(sK16, sK17)) | ~ subactivity_occurrence(sK13(sK16, sK17), sK17) | ~ occurrence_of(sK17, tptp0) | spl19_316), inference(resolution, [], [f12120, f236])).
fof(f236, plain, ! [X0, X1] : (leaf_occ(sK14(X0, X1), X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f12120, plain, (~ leaf_occ(sK14(sK13(sK16, sK17), sK17), sK17) | spl19_316), inference(avatar_component_clause, [], [f12118])).
fof(f8427, plain, spl19_85, inference(avatar_contradiction_clause, [], [f8426])).
fof(f8426, plain, ($false | spl19_85), inference(subsumption_resolution, [], [f8425, f252])).
fof(f8425, plain, (~ subactivity_occurrence(sK16, sK17) | spl19_85), inference(subsumption_resolution, [], [f8424, f253])).
fof(f8424, plain, (~ arboreal(sK16) | ~ subactivity_occurrence(sK16, sK17) | spl19_85), inference(subsumption_resolution, [], [f8423, f254])).
fof(f8423, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK16, sK17) | spl19_85), inference(subsumption_resolution, [], [f8385, f251])).
fof(f8385, plain, (~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK16, sK17) | spl19_85), inference(resolution, [], [f2573, f3730])).
fof(f3730, plain, (~ subactivity_occurrence(sK13(sK16, sK17), sK17) | spl19_85), inference(avatar_component_clause, [], [f3728])).
fof(f2573, plain, ! [X0, X1] : (subactivity_occurrence(sK13(X0, X1), X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1)), inference(duplicate_literal_removal, [], [f2567])).
fof(f2567, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ occurrence_of(X1, tptp0) | subactivity_occurrence(sK13(X0, X1), X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1)), inference(resolution, [], [f762, f409])).
fof(f409, plain, ! [X12, X13] : (subactivity_occurrence(sK14(X12, X13), X13) | ~ arboreal(X12) | ~ subactivity_occurrence(X12, X13) | ~ occurrence_of(X13, tptp0) | leaf_occ(X12, X13)), inference(resolution, [], [f236, f207])).
fof(f207, plain, ! [X0, X1] : (~ leaf_occ(X0, X1) | subactivity_occurrence(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f762, plain, ! [X24, X23, X22] : (~ subactivity_occurrence(sK14(X22, X23), X24) | ~ subactivity_occurrence(X22, X23) | ~ occurrence_of(X23, tptp0) | leaf_occ(X22, X23) | ~ arboreal(X22) | ~ occurrence_of(X24, tptp0) | subactivity_occurrence(sK13(X22, X23), X24)), inference(resolution, [], [f430, f225])).
fof(f225, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X0, X1, X2) | ~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | subactivity_occurrence(X0, X3)), inference(cnf_transformation, [], [f122])).
fof(f122, plain, ! [X0, X1, X2, X3] : (subactivity_occurrence(X0, X3) | ~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | ~ min_precedes(X0, X1, X2)), inference(flattening, [], [f121])).
fof(f121, plain, ! [X0, X1, X2, X3] : (subactivity_occurrence(X0, X3) | (~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | ~ min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0, X1, X2, X3] : ((subactivity_occurrence(X1, X3) & occurrence_of(X3, X2) & min_precedes(X0, X1, X2)) => subactivity_occurrence(X0, X3)), inference(rectify, [], [f28])).
fof(f28, plain, ! [X82, X83, X84, X85] : ((subactivity_occurrence(X83, X85) & occurrence_of(X85, X84) & min_precedes(X82, X83, X84)) => subactivity_occurrence(X82, X85)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_27)).
fof(f1480, plain, spl19_27, inference(avatar_contradiction_clause, [], [f1479])).
fof(f1479, plain, ($false | spl19_27), inference(subsumption_resolution, [], [f1478, f254])).
fof(f1478, plain, (leaf_occ(sK16, sK17) | spl19_27), inference(subsumption_resolution, [], [f1477, f251])).
fof(f1477, plain, (~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | spl19_27), inference(subsumption_resolution, [], [f1476, f252])).
fof(f1476, plain, (~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | spl19_27), inference(subsumption_resolution, [], [f1475, f253])).
fof(f1475, plain, (~ arboreal(sK16) | ~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | leaf_occ(sK16, sK17) | spl19_27), inference(resolution, [], [f1459, f401])).
fof(f401, plain, ! [X10, X9] : (activity_occurrence(sK13(X9, X10)) | ~ arboreal(X9) | ~ subactivity_occurrence(X9, X10) | ~ occurrence_of(X10, tptp0) | leaf_occ(X9, X10)), inference(resolution, [], [f232, f182])).
fof(f182, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0, X1] : (activity_occurrence(X1) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0, X1] : (occurrence_of(X1, X0) => activity_occurrence(X1)), inference(pure_predicate_removal, [], [f50])).
fof(f50, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X12, X13] : (occurrence_of(X13, X12) => (activity_occurrence(X13) & activity(X12))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_03)).
fof(f1459, plain, (~ activity_occurrence(sK13(sK16, sK17)) | spl19_27), inference(avatar_component_clause, [], [f1457])).
fof(f1457, plain, (spl19_27 <=> activity_occurrence(sK13(sK16, sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_27])])).
fof(f1464, plain, (~ spl19_27 | spl19_28), inference(avatar_split_clause, [], [f1455, f1461, f1457])).
fof(f1455, plain, (arboreal(sK13(sK16, sK17)) | ~ activity_occurrence(sK13(sK16, sK17))), inference(subsumption_resolution, [], [f1452, f238])).
fof(f1452, plain, (~ atomic(tptp4) | arboreal(sK13(sK16, sK17)) | ~ activity_occurrence(sK13(sK16, sK17))), inference(superposition, [], [f269, f1406])).
fof(f1406, plain, (tptp4 = sK5(sK13(sK16, sK17))), inference(subsumption_resolution, [], [f1405, f251])).
fof(f1405, plain, (~ occurrence_of(sK17, tptp0) | (tptp4 = sK5(sK13(sK16, sK17)))), inference(subsumption_resolution, [], [f1404, f254])).
fof(f1404, plain, (leaf_occ(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | (tptp4 = sK5(sK13(sK16, sK17)))), inference(subsumption_resolution, [], [f1398, f253])).
fof(f1398, plain, (~ arboreal(sK16) | leaf_occ(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | (tptp4 = sK5(sK13(sK16, sK17)))), inference(resolution, [], [f397, f252])).
fof(f397, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | ~ arboreal(X0) | leaf_occ(X0, X1) | ~ occurrence_of(X1, tptp0) | (tptp4 = sK5(sK13(X0, X1)))), inference(resolution, [], [f232, f278])).
fof(f278, plain, ! [X2, X1] : (~ occurrence_of(X2, X1) | (sK5(X2) = X1)), inference(subsumption_resolution, [], [f276, f182])).
fof(f276, plain, ! [X2, X1] : ((sK5(X2) = X1) | ~ occurrence_of(X2, X1) | ~ activity_occurrence(X2)), inference(resolution, [], [f190, f195])).
fof(f195, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f108, f145])).
fof(f145, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) => occurrence_of(X0, sK5(X0))), introduced(choice_axiom, [])).
fof(f108, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : occurrence_of(X0, X1)), inference(pure_predicate_removal, [], [f59])).
fof(f59, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X41] : (activity_occurrence(X41) => ? [X42] : (occurrence_of(X41, X42) & activity(X42))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_12)).
fof(f190, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X28, X29, X30] : ((occurrence_of(X28, X30) & occurrence_of(X28, X29)) => (X29 = X30)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_08)).
fof(f269, plain, ! [X0] : (~ atomic(sK5(X0)) | arboreal(X0) | ~ activity_occurrence(X0)), inference(resolution, [], [f204, f195])).
fof(f1304, plain, ~ spl19_13, inference(avatar_contradiction_clause, [], [f1303])).
fof(f1303, plain, ($false | ~ spl19_13), inference(subsumption_resolution, [], [f1302, f237])).
fof(f237, plain, ~ atomic(tptp0), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ~ atomic(tptp0), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_34)).
fof(f1302, plain, (atomic(tptp0) | ~ spl19_13), inference(forward_demodulation, [], [f1301, f282])).
fof(f282, plain, (tptp0 = sK5(sK17)), inference(subsumption_resolution, [], [f280, f261])).
fof(f261, plain, activity_occurrence(sK17), inference(resolution, [], [f182, f251])).
fof(f280, plain, ((tptp0 = sK5(sK17)) | ~ activity_occurrence(sK17)), inference(resolution, [], [f275, f195])).
fof(f275, plain, ! [X0] : (~ occurrence_of(sK17, X0) | (tptp0 = X0)), inference(resolution, [], [f190, f251])).
fof(f1301, plain, (atomic(sK5(sK17)) | ~ spl19_13), inference(subsumption_resolution, [], [f1297, f261])).
fof(f1297, plain, (atomic(sK5(sK17)) | ~ activity_occurrence(sK17) | ~ spl19_13), inference(resolution, [], [f1219, f195])).
fof(f1219, plain, (! [X3] : (~ occurrence_of(sK17, X3) | atomic(X3)) | ~ spl19_13), inference(avatar_component_clause, [], [f1218])).
fof(f1218, plain, (spl19_13 <=> ! [X3] : (atomic(X3) | ~ occurrence_of(sK17, X3))), introduced(avatar_definition, [new_symbols(naming, [spl19_13])])).
fof(f1223, plain, (spl19_13 | spl19_14 | ~ spl19_8), inference(avatar_split_clause, [], [f1207, f1145, f1221, f1218])).
fof(f1207, plain, (! [X2, X3] : ((sK14(sK16, sK17) = X2) | ~ leaf_occ(X2, sK17) | atomic(X3) | ~ occurrence_of(sK17, X3)) | ~ spl19_8), inference(resolution, [], [f1146, f226])).
fof(f226, plain, ! [X2, X0, X3, X1] : (~ leaf_occ(X1, X2) | (X0 = X1) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3)), inference(cnf_transformation, [], [f124])).
fof(f124, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | ~ leaf_occ(X1, X2) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3)), inference(flattening, [], [f123])).
fof(f123, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | (~ leaf_occ(X1, X2) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3))), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1, X2, X3] : ((leaf_occ(X1, X2) & leaf_occ(X0, X2) & ~ atomic(X3) & occurrence_of(X2, X3)) => (X0 = X1)), inference(rectify, [], [f29])).
fof(f29, plain, ! [X86, X87, X88, X89] : ((leaf_occ(X87, X88) & leaf_occ(X86, X88) & ~ atomic(X89) & occurrence_of(X88, X89)) => (X86 = X87)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+4.p', sos_28)).
fof(f1178, plain, spl19_8, inference(avatar_contradiction_clause, [], [f1177])).
fof(f1177, plain, ($false | spl19_8), inference(subsumption_resolution, [], [f1176, f251])).
fof(f1176, plain, (~ occurrence_of(sK17, tptp0) | spl19_8), inference(subsumption_resolution, [], [f1175, f252])).
fof(f1175, plain, (~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | spl19_8), inference(subsumption_resolution, [], [f1174, f253])).
fof(f1174, plain, (~ arboreal(sK16) | ~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | spl19_8), inference(subsumption_resolution, [], [f1173, f254])).
fof(f1173, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK16, sK17) | ~ occurrence_of(sK17, tptp0) | spl19_8), inference(resolution, [], [f1147, f236])).
fof(f1147, plain, (~ leaf_occ(sK14(sK16, sK17), sK17) | spl19_8), inference(avatar_component_clause, [], [f1145])).