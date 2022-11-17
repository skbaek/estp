fof(f96, plain, $false, inference(avatar_sat_refutation, [], [f42, f51, f76, f92, f95])).
fof(f95, plain, (~ spl4_1 | ~ spl4_5), inference(avatar_contradiction_clause, [], [f94])).
fof(f94, plain, ($false | (~ spl4_1 | ~ spl4_5)), inference(subsumption_resolution, [], [f93, f52])).
fof(f52, plain, (sP0(sK2) | ~ spl4_1), inference(resolution, [], [f38, f31])).
fof(f31, plain, big_f(sK2), inference(cnf_transformation, [], [f21])).
fof(f21, plain, (! [X1] : (big_j(sK2, X1) | ~ big_h(sK2, X1) | ~ big_g(X1)) & ! [X2] : (big_l(X2) | ~ big_h(sK2, X2)) & big_f(sK2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f12, f20])).
fof(f20, plain, (? [X0] : (! [X1] : (big_j(X0, X1) | ~ big_h(X0, X1) | ~ big_g(X1)) & ! [X2] : (big_l(X2) | ~ big_h(X0, X2)) & big_f(X0)) => (! [X1] : (big_j(sK2, X1) | ~ big_h(sK2, X1) | ~ big_g(X1)) & ! [X2] : (big_l(X2) | ~ big_h(sK2, X2)) & big_f(sK2))), introduced(choice_axiom, [])).
fof(f12, plain, ? [X0] : (! [X1] : (big_j(X0, X1) | ~ big_h(X0, X1) | ~ big_g(X1)) & ! [X2] : (big_l(X2) | ~ big_h(X0, X2)) & big_f(X0)), inference(flattening, [], [f11])).
fof(f11, plain, ? [X0] : (! [X1] : (big_j(X0, X1) | (~ big_h(X0, X1) | ~ big_g(X1))) & ! [X2] : (big_l(X2) | ~ big_h(X0, X2)) & big_f(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ? [X0] : (! [X1] : ((big_h(X0, X1) & big_g(X1)) => big_j(X0, X1)) & ! [X2] : (big_h(X0, X2) => big_l(X2)) & big_f(X0)), inference(rectify, [], [f3])).
fof(f3, plain, ? [X0] : (! [X2] : ((big_h(X0, X2) & big_g(X2)) => big_j(X0, X2)) & ! [X1] : (big_h(X0, X1) => big_l(X1)) & big_f(X0)), file('/home/ubuntu/library/tptp/Problems/SYN/SYN069+1.p', pel45_3)).
fof(f38, plain, (! [X0] : (~ big_f(X0) | sP0(X0)) | ~ spl4_1), inference(avatar_component_clause, [], [f37])).
fof(f37, plain, (spl4_1 <=> ! [X0] : (sP0(X0) | ~ big_f(X0))), introduced(avatar_definition, [new_symbols(naming, [spl4_1])])).
fof(f93, plain, (~ sP0(sK2) | ~ spl4_5), inference(resolution, [], [f75, f26])).
fof(f26, plain, ! [X0] : (~ big_j(X0, sK1(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : ((~ big_j(X0, sK1(X0)) & big_h(X0, sK1(X0)) & big_g(sK1(X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f16, f17])).
fof(f17, plain, ! [X0] : (? [X1] : (~ big_j(X0, X1) & big_h(X0, X1) & big_g(X1)) => (~ big_j(X0, sK1(X0)) & big_h(X0, sK1(X0)) & big_g(sK1(X0)))), introduced(choice_axiom, [])).
fof(f16, plain, ! [X0] : (? [X1] : (~ big_j(X0, X1) & big_h(X0, X1) & big_g(X1)) | ~ sP0(X0)), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0] : (? [X1] : (~ big_j(X0, X1) & big_h(X0, X1) & big_g(X1)) | ~ sP0(X0)), inference(usedef, [], [e14])).
fof(e14, plain, ! [X0] : (sP0(X0) <=> ? [X1] : (~ big_j(X0, X1) & big_h(X0, X1) & big_g(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f75, plain, (big_j(sK2, sK1(sK2)) | ~ spl4_5), inference(avatar_component_clause, [], [f73])).
fof(f73, plain, (spl4_5 <=> big_j(sK2, sK1(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl4_5])])).
fof(f92, plain, (~ spl4_1 | spl4_4), inference(avatar_contradiction_clause, [], [f91])).
fof(f91, plain, ($false | (~ spl4_1 | spl4_4)), inference(subsumption_resolution, [], [f90, f52])).
fof(f90, plain, (~ sP0(sK2) | spl4_4), inference(resolution, [], [f71, f24])).
fof(f24, plain, ! [X0] : (big_g(sK1(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f18])).
fof(f71, plain, (~ big_g(sK1(sK2)) | spl4_4), inference(avatar_component_clause, [], [f69])).
fof(f69, plain, (spl4_4 <=> big_g(sK1(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl4_4])])).
fof(f76, plain, (~ spl4_4 | spl4_5 | ~ spl4_1), inference(avatar_split_clause, [], [f67, f37, f73, f69])).
fof(f67, plain, (big_j(sK2, sK1(sK2)) | ~ big_g(sK1(sK2)) | ~ spl4_1), inference(subsumption_resolution, [], [f58, f52])).
fof(f58, plain, (big_j(sK2, sK1(sK2)) | ~ big_g(sK1(sK2)) | ~ sP0(sK2)), inference(resolution, [], [f33, f25])).
fof(f25, plain, ! [X0] : (big_h(X0, sK1(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f18])).
fof(f33, plain, ! [X1] : (~ big_h(sK2, X1) | big_j(sK2, X1) | ~ big_g(X1)), inference(cnf_transformation, [], [f21])).
fof(f51, plain, ~ spl4_2, inference(avatar_contradiction_clause, [], [f50])).
fof(f50, plain, ($false | ~ spl4_2), inference(subsumption_resolution, [], [f49, f41])).
fof(f41, plain, (! [X1] : big_k(X1) | ~ spl4_2), inference(avatar_component_clause, [], [f40])).
fof(f40, plain, (spl4_2 <=> ! [X1] : big_k(X1)), introduced(avatar_definition, [new_symbols(naming, [spl4_2])])).
fof(f49, plain, ~ big_k(sK3(sK2)), inference(resolution, [], [f48, f30])).
fof(f30, plain, ! [X0] : (~ big_l(X0) | ~ big_k(X0)), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (~ big_k(X0) | ~ big_l(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ? [X0] : (big_k(X0) & big_l(X0)), inference(rectify, [], [f2])).
fof(f2, plain, ~ ? [X1] : (big_k(X1) & big_l(X1)), file('/home/ubuntu/library/tptp/Problems/SYN/SYN069+1.p', pel45_2)).
fof(f48, plain, big_l(sK3(sK2)), inference(subsumption_resolution, [], [f47, f31])).
fof(f47, plain, (~ big_f(sK2) | big_l(sK3(sK2))), inference(resolution, [], [f35, f32])).
fof(f32, plain, ! [X2] : (~ big_h(sK2, X2) | big_l(X2)), inference(cnf_transformation, [], [f21])).
fof(f35, plain, ! [X0] : (big_h(X0, sK3(X0)) | ~ big_f(X0)), inference(cnf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : ((big_h(X0, sK3(X0)) & big_g(sK3(X0))) | ~ big_f(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f13, f22])).
fof(f22, plain, ! [X0] : (? [X1] : (big_h(X0, X1) & big_g(X1)) => (big_h(X0, sK3(X0)) & big_g(sK3(X0)))), introduced(choice_axiom, [])).
fof(f13, plain, ! [X0] : (? [X1] : (big_h(X0, X1) & big_g(X1)) | ~ big_f(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ~ ? [X0] : (~ ? [X1] : (big_h(X0, X1) & big_g(X1)) & big_f(X0)), inference(negated_conjecture, [], [f4])).
fof(f4, plain, ~ ? [X0] : (~ ? [X1] : (big_h(X0, X1) & big_g(X1)) & big_f(X0)), file('/home/ubuntu/library/tptp/Problems/SYN/SYN069+1.p', pel45)).
fof(f42, plain, (spl4_1 | spl4_2), inference(avatar_split_clause, [], [f29, f40, f37])).
fof(f29, plain, ! [X0, X1] : (big_k(X1) | sP0(X0) | ~ big_f(X0)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ! [X0] : (! [X1] : (big_k(X1) & big_h(X0, X1) & big_g(X1)) | sP0(X0) | ~ big_f(X0)), inference(rectify, [], [f15])).
fof(f15, plain, ! [X0] : (! [X2] : (big_k(X2) & big_h(X0, X2) & big_g(X2)) | sP0(X0) | ~ big_f(X0)), inference(definition_folding, [], [f9, e14])).
fof(f9, plain, ! [X0] : (! [X2] : (big_k(X2) & big_h(X0, X2) & big_g(X2)) | ? [X1] : (~ big_j(X0, X1) & big_h(X0, X1) & big_g(X1)) | ~ big_f(X0)), inference(flattening, [], [f8])).
fof(f8, plain, ! [X0] : (! [X2] : (big_k(X2) & big_h(X0, X2) & big_g(X2)) | (? [X1] : (~ big_j(X0, X1) & (big_h(X0, X1) & big_g(X1))) | ~ big_f(X0))), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ((! [X1] : ((big_h(X0, X1) & big_g(X1)) => big_j(X0, X1)) & big_f(X0)) => ! [X2] : (big_k(X2) & big_h(X0, X2) & big_g(X2))), file('/home/ubuntu/library/tptp/Problems/SYN/SYN069+1.p', pel45_1)).