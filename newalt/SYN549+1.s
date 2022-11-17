fof(f259, plain, $false, inference(resolution, [], [f257, f42])).
fof(f42, plain, sP1(sK6(sK6(initial_world))), inference(subsumption_resolution, [], [f41, f25])).
fof(f25, plain, ! [X0] : reachable(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : reachable(X0, X0), file('/home/ubuntu/library/tptp/Problems/SYN/SYN549+1.p', reflexivity_of_reachable)).
fof(f41, plain, (~ reachable(initial_world, initial_world) | sP1(sK6(sK6(initial_world)))), inference(resolution, [], [f38, f39])).
fof(f39, plain, ! [X0] : (~ reachable(initial_world, X0) | sP1(sK6(X0))), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : ((sP1(sK6(X0)) & reachable(X0, sK6(X0))) | ~ reachable(initial_world, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f11, f23])).
fof(f23, plain, ! [X0] : (? [X1] : (sP1(X1) & reachable(X0, X1)) => (sP1(sK6(X0)) & reachable(X0, sK6(X0)))), introduced(choice_axiom, [])).
fof(f11, plain, ! [X0] : (? [X1] : (sP1(X1) & reachable(X0, X1)) | ~ reachable(initial_world, X0)), inference(definition_folding, [], [f8, e10, e9])).
fof(f9, plain, ! [X1] : (sP0(X1) <=> (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X5] : (p(X5) & reachable(X1, X5)))), inference(usedef, [], [e9])).
fof(e9, plain, ! [X1] : (sP0(X1) <=> (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X5] : (p(X5) & reachable(X1, X5)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f10, plain, ! [X1] : (~ (? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)) <=> sP0(X1)) | ~ sP1(X1)), inference(usedef, [], [e10])).
fof(e10, plain, ! [X1] : (sP1(X1) <=> ~ (? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)) <=> sP0(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f8, plain, ! [X0] : (? [X1] : (~ (? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)) <=> (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X5] : (p(X5) & reachable(X1, X5)))) & reachable(X0, X1)) | ~ reachable(initial_world, X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ~ ? [X0] : (! [X1] : (reachable(X0, X1) => (? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)) <=> (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X5] : (p(X5) & reachable(X1, X5))))) & reachable(initial_world, X0)), inference(rectify, [], [f4])).
fof(f4, plain, ~ ? [X0] : (! [X1] : (reachable(X0, X1) => (? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)) <=> (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X4] : (p(X4) & reachable(X1, X4))))) & reachable(initial_world, X0)), inference(negated_conjecture, [], [f3])).
fof(f3, plain, ~ ? [X0] : (! [X1] : (reachable(X0, X1) => (? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)) <=> (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X4] : (p(X4) & reachable(X1, X4))))) & reachable(initial_world, X0)), file('/home/ubuntu/library/tptp/Problems/SYN/SYN549+1.p', prove_this)).
fof(f38, plain, ! [X0] : (reachable(X0, sK6(X0)) | ~ reachable(initial_world, X0)), inference(cnf_transformation, [], [f24])).
fof(f257, plain, ! [X0] : ~ sP1(X0), inference(subsumption_resolution, [], [f256, f149])).
fof(f149, plain, ! [X0] : (~ sP1(X0) | ~ sP0(X0)), inference(subsumption_resolution, [], [f148, f98])).
fof(f98, plain, ! [X0] : (q(sK4(X0)) | ~ sP0(X0) | ~ sP1(X0)), inference(subsumption_resolution, [], [f97, f35])).
fof(f35, plain, ! [X0] : (q(sK4(X0)) | p(sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : ((sP0(X0) | (! [X1] : (~ q(X1) | ~ reachable(X0, X1)) & ! [X2] : (~ p(X2) | ~ reachable(X0, X2)))) & ((q(sK4(X0)) & reachable(X0, sK4(X0))) | (p(sK5(X0)) & reachable(X0, sK5(X0))) | ~ sP0(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f19, f21, f20])).
fof(f20, plain, ! [X0] : (? [X3] : (q(X3) & reachable(X0, X3)) => (q(sK4(X0)) & reachable(X0, sK4(X0)))), introduced(choice_axiom, [])).
fof(f21, plain, ! [X0] : (? [X4] : (p(X4) & reachable(X0, X4)) => (p(sK5(X0)) & reachable(X0, sK5(X0)))), introduced(choice_axiom, [])).
fof(f19, plain, ! [X0] : ((sP0(X0) | (! [X1] : (~ q(X1) | ~ reachable(X0, X1)) & ! [X2] : (~ p(X2) | ~ reachable(X0, X2)))) & (? [X3] : (q(X3) & reachable(X0, X3)) | ? [X4] : (p(X4) & reachable(X0, X4)) | ~ sP0(X0))), inference(rectify, [], [f18])).
fof(f18, plain, ! [X1] : ((sP0(X1) | (! [X4] : (~ q(X4) | ~ reachable(X1, X4)) & ! [X5] : (~ p(X5) | ~ reachable(X1, X5)))) & (? [X4] : (q(X4) & reachable(X1, X4)) | ? [X5] : (p(X5) & reachable(X1, X5)) | ~ sP0(X1))), inference(flattening, [], [f17])).
fof(f17, plain, ! [X1] : ((sP0(X1) | (! [X4] : (~ q(X4) | ~ reachable(X1, X4)) & ! [X5] : (~ p(X5) | ~ reachable(X1, X5)))) & ((? [X4] : (q(X4) & reachable(X1, X4)) | ? [X5] : (p(X5) & reachable(X1, X5))) | ~ sP0(X1))), inference(nnf_transformation, [], [f9])).
fof(f97, plain, ! [X0] : (q(sK4(X0)) | ~ sP0(X0) | ~ p(sK5(X0)) | ~ sP1(X0)), inference(duplicate_literal_removal, [], [f93])).
fof(f93, plain, ! [X0] : (q(sK4(X0)) | ~ sP0(X0) | ~ p(sK5(X0)) | ~ sP0(X0) | ~ sP1(X0)), inference(resolution, [], [f34, f30])).
fof(f30, plain, ! [X0, X1] : (~ reachable(X0, X1) | ~ p(X1) | ~ sP0(X0) | ~ sP1(X0)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (((~ sP0(X0) | ! [X1] : ((! [X2] : (~ q(X2) | ~ reachable(X1, X2)) & ~ p(X1)) | ~ reachable(X0, X1))) & (sP0(X0) | (((q(sK3(X0)) & reachable(sK2(X0), sK3(X0))) | p(sK2(X0))) & reachable(X0, sK2(X0))))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f13, f15, f14])).
fof(f14, plain, ! [X0] : (? [X3] : ((? [X4] : (q(X4) & reachable(X3, X4)) | p(X3)) & reachable(X0, X3)) => ((? [X4] : (q(X4) & reachable(sK2(X0), X4)) | p(sK2(X0))) & reachable(X0, sK2(X0)))), introduced(choice_axiom, [])).
fof(f15, plain, ! [X0] : (? [X4] : (q(X4) & reachable(sK2(X0), X4)) => (q(sK3(X0)) & reachable(sK2(X0), sK3(X0)))), introduced(choice_axiom, [])).
fof(f13, plain, ! [X0] : (((~ sP0(X0) | ! [X1] : ((! [X2] : (~ q(X2) | ~ reachable(X1, X2)) & ~ p(X1)) | ~ reachable(X0, X1))) & (sP0(X0) | ? [X3] : ((? [X4] : (q(X4) & reachable(X3, X4)) | p(X3)) & reachable(X0, X3)))) | ~ sP1(X0)), inference(rectify, [], [f12])).
fof(f12, plain, ! [X1] : (((~ sP0(X1) | ! [X2] : ((! [X3] : (~ q(X3) | ~ reachable(X2, X3)) & ~ p(X2)) | ~ reachable(X1, X2))) & (sP0(X1) | ? [X2] : ((? [X3] : (q(X3) & reachable(X2, X3)) | p(X2)) & reachable(X1, X2)))) | ~ sP1(X1)), inference(nnf_transformation, [], [f10])).
fof(f34, plain, ! [X0] : (reachable(X0, sK5(X0)) | q(sK4(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f22])).
fof(f148, plain, ! [X0] : (~ sP0(X0) | ~ sP1(X0) | ~ q(sK4(X0))), inference(duplicate_literal_removal, [], [f141])).
fof(f141, plain, ! [X0] : (~ sP0(X0) | ~ sP1(X0) | ~ q(sK4(X0)) | ~ sP1(X0)), inference(resolution, [], [f107, f124])).
fof(f124, plain, ! [X0, X1] : (~ reachable(X1, X0) | ~ q(X0) | ~ sP1(X1)), inference(subsumption_resolution, [], [f117, f37])).
fof(f37, plain, ! [X0, X1] : (~ reachable(X0, X1) | ~ q(X1) | sP0(X0)), inference(cnf_transformation, [], [f22])).
fof(f117, plain, ! [X0, X1] : (~ q(X0) | ~ sP0(X1) | ~ reachable(X1, X0) | ~ sP1(X1)), inference(resolution, [], [f31, f25])).
fof(f31, plain, ! [X2, X0, X1] : (~ reachable(X1, X2) | ~ q(X2) | ~ sP0(X0) | ~ reachable(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f16])).
fof(f107, plain, ! [X0] : (reachable(X0, sK4(X0)) | ~ sP0(X0) | ~ sP1(X0)), inference(subsumption_resolution, [], [f106, f33])).
fof(f33, plain, ! [X0] : (reachable(X0, sK4(X0)) | p(sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f22])).
fof(f106, plain, ! [X0] : (reachable(X0, sK4(X0)) | ~ sP0(X0) | ~ p(sK5(X0)) | ~ sP1(X0)), inference(duplicate_literal_removal, [], [f102])).
fof(f102, plain, ! [X0] : (reachable(X0, sK4(X0)) | ~ sP0(X0) | ~ p(sK5(X0)) | ~ sP0(X0) | ~ sP1(X0)), inference(resolution, [], [f32, f30])).
fof(f32, plain, ! [X0] : (reachable(X0, sK5(X0)) | reachable(X0, sK4(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f22])).
fof(f256, plain, ! [X0] : (~ sP1(X0) | sP0(X0)), inference(duplicate_literal_removal, [], [f254])).
fof(f254, plain, ! [X0] : (~ sP1(X0) | ~ sP1(X0) | sP0(X0)), inference(resolution, [], [f234, f50])).
fof(f50, plain, ! [X1] : (~ p(sK2(X1)) | ~ sP1(X1) | sP0(X1)), inference(duplicate_literal_removal, [], [f49])).
fof(f49, plain, ! [X1] : (sP0(X1) | ~ sP1(X1) | ~ p(sK2(X1)) | sP0(X1)), inference(resolution, [], [f27, f36])).
fof(f36, plain, ! [X2, X0] : (~ reachable(X0, X2) | ~ p(X2) | sP0(X0)), inference(cnf_transformation, [], [f22])).
fof(f27, plain, ! [X0] : (reachable(X0, sK2(X0)) | sP0(X0) | ~ sP1(X0)), inference(cnf_transformation, [], [f16])).
fof(f234, plain, ! [X0] : (p(sK2(X0)) | ~ sP1(X0)), inference(subsumption_resolution, [], [f233, f149])).
fof(f233, plain, ! [X0] : (~ sP1(X0) | sP0(X0) | p(sK2(X0))), inference(duplicate_literal_removal, [], [f232])).
fof(f232, plain, ! [X0] : (~ sP1(X0) | sP0(X0) | p(sK2(X0)) | ~ sP1(X0)), inference(resolution, [], [f210, f29])).
fof(f29, plain, ! [X0] : (q(sK3(X0)) | sP0(X0) | p(sK2(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f16])).
fof(f210, plain, ! [X0] : (~ q(sK3(X0)) | ~ sP1(X0)), inference(duplicate_literal_removal, [], [f203])).
fof(f203, plain, ! [X0] : (~ sP1(X0) | ~ q(sK3(X0)) | ~ sP1(X0)), inference(resolution, [], [f199, f124])).
fof(f199, plain, ! [X0] : (reachable(X0, sK3(X0)) | ~ sP1(X0)), inference(subsumption_resolution, [], [f198, f149])).
fof(f198, plain, ! [X0] : (~ sP1(X0) | reachable(X0, sK3(X0)) | sP0(X0)), inference(duplicate_literal_removal, [], [f194])).
fof(f194, plain, ! [X0] : (~ sP1(X0) | reachable(X0, sK3(X0)) | sP0(X0) | sP0(X0) | ~ sP1(X0)), inference(resolution, [], [f114, f27])).
fof(f114, plain, ! [X2, X1] : (~ reachable(X2, sK2(X1)) | ~ sP1(X1) | reachable(X2, sK3(X1)) | sP0(X1)), inference(subsumption_resolution, [], [f110, f50])).
fof(f110, plain, ! [X2, X1] : (sP0(X1) | p(sK2(X1)) | ~ sP1(X1) | reachable(X2, sK3(X1)) | ~ reachable(X2, sK2(X1))), inference(resolution, [], [f28, f26])).
fof(f26, plain, ! [X2, X0, X1] : (~ reachable(X1, X2) | reachable(X0, X2) | ~ reachable(X0, X1)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, ! [X0, X1, X2] : (reachable(X0, X2) | ~ reachable(X1, X2) | ~ reachable(X0, X1)), inference(flattening, [], [f6])).
fof(f6, plain, ! [X0, X1, X2] : (reachable(X0, X2) | (~ reachable(X1, X2) | ~ reachable(X0, X1))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ! [X0, X1, X2] : ((reachable(X1, X2) & reachable(X0, X1)) => reachable(X0, X2)), file('/home/ubuntu/library/tptp/Problems/SYN/SYN549+1.p', transitivity_of_reachable)).
fof(f28, plain, ! [X0] : (reachable(sK2(X0), sK3(X0)) | sP0(X0) | p(sK2(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f16])).