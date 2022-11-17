fof(f303, plain, $false, inference(avatar_sat_refutation, [], [f62, f72, f82, f86, f90, f94, f98, f297, f302])).
fof(f302, plain, (spl9_2 | ~ spl9_4 | ~ spl9_6), inference(avatar_contradiction_clause, [], [f301])).
fof(f301, plain, ($false | (spl9_2 | ~ spl9_4 | ~ spl9_6)), inference(subsumption_resolution, [], [f300, f81])).
fof(f81, plain, ((sK4 = sK5) | ~ spl9_6), inference(avatar_component_clause, [], [f79])).
fof(f79, plain, (spl9_6 <=> (sK4 = sK5)), introduced(avatar_definition, [new_symbols(naming, [spl9_6])])).
fof(f300, plain, (~ (sK4 = sK5) | (spl9_2 | ~ spl9_4)), inference(forward_demodulation, [], [f61, f71])).
fof(f71, plain, ((sK4 = sK6) | ~ spl9_4), inference(avatar_component_clause, [], [f69])).
fof(f69, plain, (spl9_4 <=> (sK4 = sK6)), introduced(avatar_definition, [new_symbols(naming, [spl9_4])])).
fof(f61, plain, (~ (sK5 = sK6) | spl9_2), inference(avatar_component_clause, [], [f59])).
fof(f59, plain, (spl9_2 <=> (sK5 = sK6)), introduced(avatar_definition, [new_symbols(naming, [spl9_2])])).
fof(f297, plain, (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10), inference(avatar_contradiction_clause, [], [f296])).
fof(f296, plain, ($false | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(subsumption_resolution, [], [f295, f104])).
fof(f104, plain, (! [X1] : ordinal(sK2(sK7(X1))) | ~ spl9_8), inference(duplicate_literal_removal, [], [f103])).
fof(f103, plain, (! [X1] : (ordinal(sK2(sK7(X1))) | ordinal(sK2(sK7(X1)))) | ~ spl9_8), inference(resolution, [], [f35, f89])).
fof(f89, plain, (! [X2, X0] : (~ in(X2, sK7(X0)) | ordinal(X2)) | ~ spl9_8), inference(avatar_component_clause, [], [f88])).
fof(f88, plain, (spl9_8 <=> ! [X0, X2] : (ordinal(X2) | ~ in(X2, sK7(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl9_8])])).
fof(f35, plain, ! [X1] : (in(sK2(X1), X1) | ordinal(sK2(X1))), inference(cnf_transformation, [], [f22])).
fof(f22, plain, ! [X1] : ((~ ordinal(sK2(X1)) | ~ in(sK2(X1), sK1) | ~ in(sK2(X1), X1)) & ((ordinal(sK2(X1)) & in(sK2(X1), sK1)) | in(sK2(X1), X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f19, f21, f20])).
fof(f20, plain, (? [X0] : ! [X1] : ? [X2] : ((~ ordinal(X2) | ~ in(X2, X0) | ~ in(X2, X1)) & ((ordinal(X2) & in(X2, X0)) | in(X2, X1))) => ! [X1] : ? [X2] : ((~ ordinal(X2) | ~ in(X2, sK1) | ~ in(X2, X1)) & ((ordinal(X2) & in(X2, sK1)) | in(X2, X1)))), introduced(choice_axiom, [])).
fof(f21, plain, ! [X1] : (? [X2] : ((~ ordinal(X2) | ~ in(X2, sK1) | ~ in(X2, X1)) & ((ordinal(X2) & in(X2, sK1)) | in(X2, X1))) => ((~ ordinal(sK2(X1)) | ~ in(sK2(X1), sK1) | ~ in(sK2(X1), X1)) & ((ordinal(sK2(X1)) & in(sK2(X1), sK1)) | in(sK2(X1), X1)))), introduced(choice_axiom, [])).
fof(f19, plain, ? [X0] : ! [X1] : ? [X2] : ((~ ordinal(X2) | ~ in(X2, X0) | ~ in(X2, X1)) & ((ordinal(X2) & in(X2, X0)) | in(X2, X1))), inference(flattening, [], [f18])).
fof(f18, plain, ? [X0] : ! [X1] : ? [X2] : (((~ ordinal(X2) | ~ in(X2, X0)) | ~ in(X2, X1)) & ((ordinal(X2) & in(X2, X0)) | in(X2, X1))), inference(nnf_transformation, [], [f9])).
fof(f9, plain, ? [X0] : ! [X1] : ? [X2] : ~ (in(X2, X1) <=> (ordinal(X2) & in(X2, X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ? [X1] : ! [X2] : (in(X2, X1) <=> (ordinal(X2) & in(X2, X0))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ? [X1] : ! [X2] : (in(X2, X1) <=> (ordinal(X2) & in(X2, X0))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU280+1.p', s1_xboole_0__e6_22__wellord2)).
fof(f295, plain, (~ ordinal(sK2(sK7(sK1))) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(subsumption_resolution, [], [f294, f263])).
fof(f263, plain, (in(sK2(sK7(sK1)), sK1) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(subsumption_resolution, [], [f261, f34])).
fof(f34, plain, ! [X1] : (in(sK2(X1), sK1) | in(sK2(X1), X1)), inference(cnf_transformation, [], [f22])).
fof(f261, plain, (in(sK2(sK7(sK1)), sK1) | ~ in(sK2(sK7(sK1)), sK7(sK1)) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(superposition, [], [f97, f255])).
fof(f255, plain, ((sK2(sK7(sK1)) = sK8(sK1, sK2(sK7(sK1)))) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9)), inference(trivial_inequality_removal, [], [f252])).
fof(f252, plain, (~ (sK2(sK7(sK1)) = sK2(sK7(sK1))) | (sK2(sK7(sK1)) = sK8(sK1, sK2(sK7(sK1)))) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9)), inference(equality_factoring, [], [f145])).
fof(f145, plain, (! [X1] : ((sK2(sK7(X1)) = sK8(X1, sK2(sK7(X1)))) | (sK2(sK7(X1)) = sK8(sK1, sK2(sK7(X1))))) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9)), inference(subsumption_resolution, [], [f143, f104])).
fof(f143, plain, (! [X1] : ((sK2(sK7(X1)) = sK8(X1, sK2(sK7(X1)))) | (sK2(sK7(X1)) = sK8(sK1, sK2(sK7(X1)))) | ~ ordinal(sK2(sK7(X1)))) | (~ spl9_7 | ~ spl9_9)), inference(resolution, [], [f115, f117])).
fof(f117, plain, (! [X2, X3] : (~ in(X3, X2) | (sK8(X2, X3) = X3) | ~ ordinal(X3)) | (~ spl9_7 | ~ spl9_9)), inference(resolution, [], [f93, f85])).
fof(f85, plain, (! [X0, X3] : (in(X3, sK7(X0)) | ~ in(X3, X0) | ~ ordinal(X3)) | ~ spl9_7), inference(avatar_component_clause, [], [f84])).
fof(f84, plain, (spl9_7 <=> ! [X3, X0] : (in(X3, sK7(X0)) | ~ in(X3, X0) | ~ ordinal(X3))), introduced(avatar_definition, [new_symbols(naming, [spl9_7])])).
fof(f93, plain, (! [X2, X0] : (~ in(X2, sK7(X0)) | (sK8(X0, X2) = X2)) | ~ spl9_9), inference(avatar_component_clause, [], [f92])).
fof(f92, plain, (spl9_9 <=> ! [X0, X2] : ((sK8(X0, X2) = X2) | ~ in(X2, sK7(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl9_9])])).
fof(f115, plain, (! [X0] : (in(sK2(sK7(X0)), sK1) | (sK2(sK7(X0)) = sK8(X0, sK2(sK7(X0))))) | ~ spl9_9), inference(resolution, [], [f93, f34])).
fof(f97, plain, (! [X2, X0] : (in(sK8(X0, X2), X0) | ~ in(X2, sK7(X0))) | ~ spl9_10), inference(avatar_component_clause, [], [f96])).
fof(f96, plain, (spl9_10 <=> ! [X0, X2] : (in(sK8(X0, X2), X0) | ~ in(X2, sK7(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl9_10])])).
fof(f294, plain, (~ in(sK2(sK7(sK1)), sK1) | ~ ordinal(sK2(sK7(sK1))) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(resolution, [], [f285, f85])).
fof(f285, plain, (~ in(sK2(sK7(sK1)), sK7(sK1)) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(subsumption_resolution, [], [f279, f104])).
fof(f279, plain, (~ ordinal(sK2(sK7(sK1))) | ~ in(sK2(sK7(sK1)), sK7(sK1)) | (~ spl9_7 | ~ spl9_8 | ~ spl9_9 | ~ spl9_10)), inference(resolution, [], [f263, f36])).
fof(f36, plain, ! [X1] : (~ in(sK2(X1), sK1) | ~ ordinal(sK2(X1)) | ~ in(sK2(X1), X1)), inference(cnf_transformation, [], [f22])).
fof(f98, plain, (spl9_1 | spl9_10), inference(avatar_split_clause, [], [f49, f96, f55])).
fof(f55, plain, (spl9_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl9_1])])).
fof(f49, plain, ! [X2, X0] : (in(sK8(X0, X2), X0) | ~ in(X2, sK7(X0)) | sP0), inference(cnf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (! [X2] : ((in(X2, sK7(X0)) | ! [X3] : (~ ordinal(X2) | ~ (X2 = X3) | ~ in(X3, X0))) & ((ordinal(X2) & (sK8(X0, X2) = X2) & in(sK8(X0, X2), X0)) | ~ in(X2, sK7(X0)))) | sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f30, f32, f31])).
fof(f31, plain, ! [X0] : (? [X1] : ! [X2] : ((in(X2, X1) | ! [X3] : (~ ordinal(X2) | ~ (X2 = X3) | ~ in(X3, X0))) & (? [X4] : (ordinal(X2) & (X2 = X4) & in(X4, X0)) | ~ in(X2, X1))) => ! [X2] : ((in(X2, sK7(X0)) | ! [X3] : (~ ordinal(X2) | ~ (X2 = X3) | ~ in(X3, X0))) & (? [X4] : (ordinal(X2) & (X2 = X4) & in(X4, X0)) | ~ in(X2, sK7(X0))))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X2, X0] : (? [X4] : (ordinal(X2) & (X2 = X4) & in(X4, X0)) => (ordinal(X2) & (sK8(X0, X2) = X2) & in(sK8(X0, X2), X0))), introduced(choice_axiom, [])).
fof(f30, plain, ! [X0] : (? [X1] : ! [X2] : ((in(X2, X1) | ! [X3] : (~ ordinal(X2) | ~ (X2 = X3) | ~ in(X3, X0))) & (? [X4] : (ordinal(X2) & (X2 = X4) & in(X4, X0)) | ~ in(X2, X1))) | sP0), inference(rectify, [], [f29])).
fof(f29, plain, ! [X0] : (? [X4] : ! [X5] : ((in(X5, X4) | ! [X6] : (~ ordinal(X5) | ~ (X5 = X6) | ~ in(X6, X0))) & (? [X6] : (ordinal(X5) & (X5 = X6) & in(X6, X0)) | ~ in(X5, X4))) | sP0), inference(nnf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (? [X4] : ! [X5] : (in(X5, X4) <=> ? [X6] : (ordinal(X5) & (X5 = X6) & in(X6, X0))) | sP0), inference(definition_folding, [], [f15, e16])).
fof(f16, plain, (? [X1, X2, X3] : (~ (X2 = X3) & ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2)) | ~ sP0), inference(usedef, [], [e16])).
fof(e16, plain, (sP0 <=> ? [X1, X2, X3] : (~ (X2 = X3) & ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f15, plain, ! [X0] : (? [X4] : ! [X5] : (in(X5, X4) <=> ? [X6] : (ordinal(X5) & (X5 = X6) & in(X6, X0))) | ? [X1, X2, X3] : (~ (X2 = X3) & ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2))), inference(flattening, [], [f14])).
fof(f14, plain, ! [X0] : (? [X4] : ! [X5] : (in(X5, X4) <=> ? [X6] : (ordinal(X5) & (X5 = X6) & in(X6, X0))) | ? [X1, X2, X3] : (~ (X2 = X3) & (ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2)))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : (! [X1, X2, X3] : ((ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2)) => (X2 = X3)) => ? [X4] : ! [X5] : (in(X5, X4) <=> ? [X6] : (ordinal(X5) & (X5 = X6) & in(X6, X0)))), inference(rectify, [], [f7])).
fof(f7, plain, ! [X0] : (! [X1, X2, X3] : ((ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2)) => (X2 = X3)) => ? [X1] : ! [X2] : (in(X2, X1) <=> ? [X3] : (ordinal(X2) & (X2 = X3) & in(X3, X0)))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU280+1.p', s1_tarski__e6_22__wellord2__1)).
fof(f94, plain, (spl9_1 | spl9_9), inference(avatar_split_clause, [], [f50, f92, f55])).
fof(f50, plain, ! [X2, X0] : ((sK8(X0, X2) = X2) | ~ in(X2, sK7(X0)) | sP0), inference(cnf_transformation, [], [f33])).
fof(f90, plain, (spl9_1 | spl9_8), inference(avatar_split_clause, [], [f51, f88, f55])).
fof(f51, plain, ! [X2, X0] : (ordinal(X2) | ~ in(X2, sK7(X0)) | sP0), inference(cnf_transformation, [], [f33])).
fof(f86, plain, (spl9_1 | spl9_7), inference(avatar_split_clause, [], [f53, f84, f55])).
fof(f53, plain, ! [X0, X3] : (in(X3, sK7(X0)) | ~ ordinal(X3) | ~ in(X3, X0) | sP0), inference(equality_resolution, [], [f52])).
fof(f52, plain, ! [X2, X0, X3] : (in(X2, sK7(X0)) | ~ ordinal(X2) | ~ (X2 = X3) | ~ in(X3, X0) | sP0), inference(cnf_transformation, [], [f33])).
fof(f82, plain, (~ spl9_1 | spl9_6), inference(avatar_split_clause, [], [f44, f79, f55])).
fof(f44, plain, ((sK4 = sK5) | ~ sP0), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ((~ (sK5 = sK6) & ordinal(sK6) & (sK4 = sK6) & ordinal(sK5) & (sK4 = sK5)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6])], [f26, f27])).
fof(f27, plain, (? [X0, X1, X2] : (~ (X1 = X2) & ordinal(X2) & (X0 = X2) & ordinal(X1) & (X0 = X1)) => (~ (sK5 = sK6) & ordinal(sK6) & (sK4 = sK6) & ordinal(sK5) & (sK4 = sK5))), introduced(choice_axiom, [])).
fof(f26, plain, (? [X0, X1, X2] : (~ (X1 = X2) & ordinal(X2) & (X0 = X2) & ordinal(X1) & (X0 = X1)) | ~ sP0), inference(rectify, [], [f25])).
fof(f25, plain, (? [X1, X2, X3] : (~ (X2 = X3) & ordinal(X3) & (X1 = X3) & ordinal(X2) & (X1 = X2)) | ~ sP0), inference(nnf_transformation, [], [f16])).
fof(f72, plain, (~ spl9_1 | spl9_4), inference(avatar_split_clause, [], [f46, f69, f55])).
fof(f46, plain, ((sK4 = sK6) | ~ sP0), inference(cnf_transformation, [], [f28])).
fof(f62, plain, (~ spl9_1 | ~ spl9_2), inference(avatar_split_clause, [], [f48, f59, f55])).
fof(f48, plain, (~ (sK5 = sK6) | ~ sP0), inference(cnf_transformation, [], [f28])).