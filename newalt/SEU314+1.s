fof(f1410, plain, $false, inference(avatar_sat_refutation, [], [f660, f661, f697, f745, f750, f759, f813, f851, f856, f892, f1405])).
fof(f1405, plain, (spl18_16 | spl18_20 | ~ spl18_21), inference(avatar_contradiction_clause, [], [f1404])).
fof(f1404, plain, ($false | (spl18_16 | spl18_20 | ~ spl18_21)), inference(subsumption_resolution, [], [f1403, f913])).
fof(f913, plain, (subset(sK3, sK4(sK15(sK2, sK3))) | spl18_20), inference(resolution, [], [f658, f107])).
fof(f107, plain, ! [X2] : (subset(sK3, sK4(X2)) | in(sK4(X2), X2)), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (! [X2] : ((! [X4] : (~ subset(sK3, sK4(X2)) | ~ closed_subset(X4, sK2) | ~ (sK4(X2) = X4) | ~ element(X4, powerset(the_carrier(sK2)))) | ~ in(sK4(X2), powerset(the_carrier(sK2))) | ~ in(sK4(X2), X2)) & (((subset(sK3, sK4(X2)) & closed_subset(sK5(X2), sK2) & (sK4(X2) = sK5(X2)) & element(sK5(X2), powerset(the_carrier(sK2)))) & in(sK4(X2), powerset(the_carrier(sK2)))) | in(sK4(X2), X2))) & element(sK3, powerset(the_carrier(sK2))) & top_str(sK2) & topological_space(sK2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4, sK5])], [f72, f75, f74, f73])).
fof(f73, plain, (? [X0, X1] : (! [X2] : ? [X3] : ((! [X4] : (~ subset(X1, X3) | ~ closed_subset(X4, X0) | ~ (X3 = X4) | ~ element(X4, powerset(the_carrier(X0)))) | ~ in(X3, powerset(the_carrier(X0))) | ~ in(X3, X2)) & ((? [X5] : (subset(X1, X3) & closed_subset(X5, X0) & (X3 = X5) & element(X5, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0)))) | in(X3, X2))) & element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)) => (! [X2] : ? [X3] : ((! [X4] : (~ subset(sK3, X3) | ~ closed_subset(X4, sK2) | ~ (X3 = X4) | ~ element(X4, powerset(the_carrier(sK2)))) | ~ in(X3, powerset(the_carrier(sK2))) | ~ in(X3, X2)) & ((? [X5] : (subset(sK3, X3) & closed_subset(X5, sK2) & (X3 = X5) & element(X5, powerset(the_carrier(sK2)))) & in(X3, powerset(the_carrier(sK2)))) | in(X3, X2))) & element(sK3, powerset(the_carrier(sK2))) & top_str(sK2) & topological_space(sK2))), introduced(choice_axiom, [])).
fof(f74, plain, ! [X2] : (? [X3] : ((! [X4] : (~ subset(sK3, X3) | ~ closed_subset(X4, sK2) | ~ (X3 = X4) | ~ element(X4, powerset(the_carrier(sK2)))) | ~ in(X3, powerset(the_carrier(sK2))) | ~ in(X3, X2)) & ((? [X5] : (subset(sK3, X3) & closed_subset(X5, sK2) & (X3 = X5) & element(X5, powerset(the_carrier(sK2)))) & in(X3, powerset(the_carrier(sK2)))) | in(X3, X2))) => ((! [X4] : (~ subset(sK3, sK4(X2)) | ~ closed_subset(X4, sK2) | ~ (sK4(X2) = X4) | ~ element(X4, powerset(the_carrier(sK2)))) | ~ in(sK4(X2), powerset(the_carrier(sK2))) | ~ in(sK4(X2), X2)) & ((? [X5] : (subset(sK3, sK4(X2)) & closed_subset(X5, sK2) & (sK4(X2) = X5) & element(X5, powerset(the_carrier(sK2)))) & in(sK4(X2), powerset(the_carrier(sK2)))) | in(sK4(X2), X2)))), introduced(choice_axiom, [])).
fof(f75, plain, ! [X2] : (? [X5] : (subset(sK3, sK4(X2)) & closed_subset(X5, sK2) & (sK4(X2) = X5) & element(X5, powerset(the_carrier(sK2)))) => (subset(sK3, sK4(X2)) & closed_subset(sK5(X2), sK2) & (sK4(X2) = sK5(X2)) & element(sK5(X2), powerset(the_carrier(sK2))))), introduced(choice_axiom, [])).
fof(f72, plain, ? [X0, X1] : (! [X2] : ? [X3] : ((! [X4] : (~ subset(X1, X3) | ~ closed_subset(X4, X0) | ~ (X3 = X4) | ~ element(X4, powerset(the_carrier(X0)))) | ~ in(X3, powerset(the_carrier(X0))) | ~ in(X3, X2)) & ((? [X5] : (subset(X1, X3) & closed_subset(X5, X0) & (X3 = X5) & element(X5, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0)))) | in(X3, X2))) & element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)), inference(rectify, [], [f71])).
fof(f71, plain, ? [X0, X1] : (! [X2] : ? [X3] : ((! [X4] : (~ subset(X1, X3) | ~ closed_subset(X4, X0) | ~ (X3 = X4) | ~ element(X4, powerset(the_carrier(X0)))) | ~ in(X3, powerset(the_carrier(X0))) | ~ in(X3, X2)) & ((? [X4] : (subset(X1, X3) & closed_subset(X4, X0) & (X3 = X4) & element(X4, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0)))) | in(X3, X2))) & element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)), inference(flattening, [], [f70])).
fof(f70, plain, ? [X0, X1] : (! [X2] : ? [X3] : (((! [X4] : (~ subset(X1, X3) | ~ closed_subset(X4, X0) | ~ (X3 = X4) | ~ element(X4, powerset(the_carrier(X0)))) | ~ in(X3, powerset(the_carrier(X0)))) | ~ in(X3, X2)) & ((? [X4] : (subset(X1, X3) & closed_subset(X4, X0) & (X3 = X4) & element(X4, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0)))) | in(X3, X2))) & element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)), inference(nnf_transformation, [], [f50])).
fof(f50, plain, ? [X0, X1] : (! [X2] : ? [X3] : ~ (in(X3, X2) <=> (? [X4] : (subset(X1, X3) & closed_subset(X4, X0) & (X3 = X4) & element(X4, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0))))) & element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)), inference(flattening, [], [f49])).
fof(f49, plain, ? [X0, X1] : (! [X2] : ? [X3] : ~ (in(X3, X2) <=> (? [X4] : (subset(X1, X3) & closed_subset(X4, X0) & (X3 = X4) & element(X4, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0))))) & (element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0, X1] : ((element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)) => ? [X2] : ! [X3] : (in(X3, X2) <=> (? [X4] : (subset(X1, X3) & closed_subset(X4, X0) & (X3 = X4) & element(X4, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0)))))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0, X1] : ((element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)) => ? [X2] : ! [X3] : (in(X3, X2) <=> (? [X4] : (subset(X1, X3) & closed_subset(X4, X0) & (X3 = X4) & element(X4, powerset(the_carrier(X0)))) & in(X3, powerset(the_carrier(X0)))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU314+1.p', s1_xboole_0__e1_40__pre_topc__1)).
fof(f658, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | spl18_20), inference(avatar_component_clause, [], [f657])).
fof(f657, plain, (spl18_20 <=> in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3))), introduced(avatar_definition, [new_symbols(naming, [spl18_20])])).
fof(f1403, plain, (~ subset(sK3, sK4(sK15(sK2, sK3))) | (spl18_16 | spl18_20 | ~ spl18_21)), inference(subsumption_resolution, [], [f1402, f610])).
fof(f610, plain, (~ sP1(sK3, sK2) | spl18_16), inference(avatar_component_clause, [], [f609])).
fof(f609, plain, (spl18_16 <=> sP1(sK3, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f1402, plain, (sP1(sK3, sK2) | ~ subset(sK3, sK4(sK15(sK2, sK3))) | (spl18_20 | ~ spl18_21)), inference(subsumption_resolution, [], [f1364, f102])).
fof(f102, plain, element(sK3, powerset(the_carrier(sK2))), inference(cnf_transformation, [], [f76])).
fof(f1364, plain, (~ element(sK3, powerset(the_carrier(sK2))) | sP1(sK3, sK2) | ~ subset(sK3, sK4(sK15(sK2, sK3))) | (spl18_20 | ~ spl18_21)), inference(resolution, [], [f719, f658])).
fof(f719, plain, (! [X0] : (in(sK4(sK15(sK2, sK3)), sK15(sK2, X0)) | ~ element(X0, powerset(the_carrier(sK2))) | sP1(X0, sK2) | ~ subset(X0, sK4(sK15(sK2, sK3)))) | ~ spl18_21), inference(avatar_component_clause, [], [f718])).
fof(f718, plain, (spl18_21 <=> ! [X0] : (~ subset(X0, sK4(sK15(sK2, sK3))) | ~ element(X0, powerset(the_carrier(sK2))) | sP1(X0, sK2) | in(sK4(sK15(sK2, sK3)), sK15(sK2, X0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_21])])).
fof(f892, plain, (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20), inference(avatar_contradiction_clause, [], [f891])).
fof(f891, plain, ($false | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f890, f610])).
fof(f890, plain, (sP1(sK3, sK2) | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f889, f100])).
fof(f100, plain, topological_space(sK2), inference(cnf_transformation, [], [f76])).
fof(f889, plain, (~ topological_space(sK2) | sP1(sK3, sK2) | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f888, f101])).
fof(f101, plain, top_str(sK2), inference(cnf_transformation, [], [f76])).
fof(f888, plain, (~ top_str(sK2) | ~ topological_space(sK2) | sP1(sK3, sK2) | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f878, f102])).
fof(f878, plain, (~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | sP1(sK3, sK2) | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(resolution, [], [f876, f445])).
fof(f445, plain, ! [X0] : (subset(sK3, sK4(sK15(X0, sK3))) | ~ element(sK3, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0) | sP1(sK3, X0)), inference(factoring, [], [f340])).
fof(f340, plain, ! [X6, X7] : (subset(sK3, sK4(sK15(X7, X6))) | subset(X6, sK4(sK15(X7, X6))) | ~ element(X6, powerset(the_carrier(X7))) | ~ top_str(X7) | ~ topological_space(X7) | sP1(X6, X7)), inference(resolution, [], [f165, f107])).
fof(f165, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | subset(X1, X3) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1] : (! [X3] : ((in(X3, sK15(X0, X1)) | ! [X4] : (! [X5] : (~ subset(X1, X3) | ~ closed_subset(X5, X0) | ~ (X3 = X5) | ~ element(X5, powerset(the_carrier(X0)))) | ~ (X3 = X4) | ~ in(X4, powerset(the_carrier(X0))))) & (((subset(X1, X3) & closed_subset(sK17(X0, X1, X3), X0) & (sK17(X0, X1, X3) = X3) & element(sK17(X0, X1, X3), powerset(the_carrier(X0)))) & (sK16(X0, X1, X3) = X3) & in(sK16(X0, X1, X3), powerset(the_carrier(X0)))) | ~ in(X3, sK15(X0, X1)))) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17])], [f95, f98, f97, f96])).
fof(f96, plain, ! [X1, X0] : (? [X2] : ! [X3] : ((in(X3, X2) | ! [X4] : (! [X5] : (~ subset(X1, X3) | ~ closed_subset(X5, X0) | ~ (X3 = X5) | ~ element(X5, powerset(the_carrier(X0)))) | ~ (X3 = X4) | ~ in(X4, powerset(the_carrier(X0))))) & (? [X6] : (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) & (X3 = X6) & in(X6, powerset(the_carrier(X0)))) | ~ in(X3, X2))) => ! [X3] : ((in(X3, sK15(X0, X1)) | ! [X4] : (! [X5] : (~ subset(X1, X3) | ~ closed_subset(X5, X0) | ~ (X3 = X5) | ~ element(X5, powerset(the_carrier(X0)))) | ~ (X3 = X4) | ~ in(X4, powerset(the_carrier(X0))))) & (? [X6] : (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) & (X3 = X6) & in(X6, powerset(the_carrier(X0)))) | ~ in(X3, sK15(X0, X1))))), introduced(choice_axiom, [])).
fof(f97, plain, ! [X3, X1, X0] : (? [X6] : (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) & (X3 = X6) & in(X6, powerset(the_carrier(X0)))) => (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) & (sK16(X0, X1, X3) = X3) & in(sK16(X0, X1, X3), powerset(the_carrier(X0))))), introduced(choice_axiom, [])).
fof(f98, plain, ! [X3, X1, X0] : (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) => (subset(X1, X3) & closed_subset(sK17(X0, X1, X3), X0) & (sK17(X0, X1, X3) = X3) & element(sK17(X0, X1, X3), powerset(the_carrier(X0))))), introduced(choice_axiom, [])).
fof(f95, plain, ! [X0, X1] : (? [X2] : ! [X3] : ((in(X3, X2) | ! [X4] : (! [X5] : (~ subset(X1, X3) | ~ closed_subset(X5, X0) | ~ (X3 = X5) | ~ element(X5, powerset(the_carrier(X0)))) | ~ (X3 = X4) | ~ in(X4, powerset(the_carrier(X0))))) & (? [X6] : (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) & (X3 = X6) & in(X6, powerset(the_carrier(X0)))) | ~ in(X3, X2))) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(rectify, [], [f94])).
fof(f94, plain, ! [X0, X1] : (? [X7] : ! [X8] : ((in(X8, X7) | ! [X9] : (! [X10] : (~ subset(X1, X8) | ~ closed_subset(X10, X0) | ~ (X8 = X10) | ~ element(X10, powerset(the_carrier(X0)))) | ~ (X8 = X9) | ~ in(X9, powerset(the_carrier(X0))))) & (? [X9] : (? [X10] : (subset(X1, X8) & closed_subset(X10, X0) & (X8 = X10) & element(X10, powerset(the_carrier(X0)))) & (X8 = X9) & in(X9, powerset(the_carrier(X0)))) | ~ in(X8, X7))) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(nnf_transformation, [], [f69])).
fof(f69, plain, ! [X0, X1] : (? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (subset(X1, X8) & closed_subset(X10, X0) & (X8 = X10) & element(X10, powerset(the_carrier(X0)))) & (X8 = X9) & in(X9, powerset(the_carrier(X0))))) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(definition_folding, [], [f66, e68, e67])).
fof(f67, plain, ! [X3, X1, X0] : (? [X6] : (subset(X1, X3) & closed_subset(X6, X0) & (X3 = X6) & element(X6, powerset(the_carrier(X0)))) | ~ sP0(X3, X1, X0)), inference(usedef, [], [e67])).
fof(e67, plain, ! [X3, X1, X0] : (sP0(X3, X1, X0) <=> ? [X6] : (subset(X1, X3) & closed_subset(X6, X0) & (X3 = X6) & element(X6, powerset(the_carrier(X0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f68, plain, ! [X1, X0] : (? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (subset(X1, X4) & closed_subset(X5, X0) & (X4 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X4) & sP0(X3, X1, X0) & (X2 = X3)) | ~ sP1(X1, X0)), inference(usedef, [], [e68])).
fof(e68, plain, ! [X1, X0] : (sP1(X1, X0) <=> ? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (subset(X1, X4) & closed_subset(X5, X0) & (X4 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X4) & sP0(X3, X1, X0) & (X2 = X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f66, plain, ! [X0, X1] : (? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (subset(X1, X8) & closed_subset(X10, X0) & (X8 = X10) & element(X10, powerset(the_carrier(X0)))) & (X8 = X9) & in(X9, powerset(the_carrier(X0))))) | ? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (subset(X1, X4) & closed_subset(X5, X0) & (X4 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X4) & ? [X6] : (subset(X1, X3) & closed_subset(X6, X0) & (X3 = X6) & element(X6, powerset(the_carrier(X0)))) & (X2 = X3)) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(flattening, [], [f65])).
fof(f65, plain, ! [X0, X1] : ((? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (subset(X1, X8) & closed_subset(X10, X0) & (X8 = X10) & element(X10, powerset(the_carrier(X0)))) & (X8 = X9) & in(X9, powerset(the_carrier(X0))))) | ? [X2, X3, X4] : (~ (X3 = X4) & (? [X5] : (subset(X1, X4) & closed_subset(X5, X0) & (X4 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X4) & ? [X6] : (subset(X1, X3) & closed_subset(X6, X0) & (X3 = X6) & element(X6, powerset(the_carrier(X0)))) & (X2 = X3)))) | (~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0, X1] : ((element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)) => (! [X2, X3, X4] : ((? [X5] : (subset(X1, X4) & closed_subset(X5, X0) & (X4 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X4) & ? [X6] : (subset(X1, X3) & closed_subset(X6, X0) & (X3 = X6) & element(X6, powerset(the_carrier(X0)))) & (X2 = X3)) => (X3 = X4)) => ? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (subset(X1, X8) & closed_subset(X10, X0) & (X8 = X10) & element(X10, powerset(the_carrier(X0)))) & (X8 = X9) & in(X9, powerset(the_carrier(X0))))))), inference(rectify, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((element(X1, powerset(the_carrier(X0))) & top_str(X0) & topological_space(X0)) => (! [X2, X3, X4] : ((? [X6] : (subset(X1, X4) & closed_subset(X6, X0) & (X4 = X6) & element(X6, powerset(the_carrier(X0)))) & (X2 = X4) & ? [X5] : (subset(X1, X3) & closed_subset(X5, X0) & (X3 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X3)) => (X3 = X4)) => ? [X2] : ! [X3] : (in(X3, X2) <=> ? [X4] : (? [X7] : (subset(X1, X3) & closed_subset(X7, X0) & (X3 = X7) & element(X7, powerset(the_carrier(X0)))) & (X3 = X4) & in(X4, powerset(the_carrier(X0))))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU314+1.p', s1_tarski__e1_40__pre_topc__1)).
fof(f876, plain, (~ subset(sK3, sK4(sK15(sK2, sK3))) | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f875, f659])).
fof(f659, plain, (in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ spl18_20), inference(avatar_component_clause, [], [f657])).
fof(f875, plain, (~ subset(sK3, sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (~ spl18_15 | spl18_16 | ~ spl18_18 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f874, f638])).
fof(f638, plain, (element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ spl18_18), inference(avatar_component_clause, [], [f636])).
fof(f636, plain, (spl18_18 <=> element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2)))), introduced(avatar_definition, [new_symbols(naming, [spl18_18])])).
fof(f874, plain, (~ element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ subset(sK3, sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (~ spl18_15 | spl18_16 | ~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f871, f650])).
fof(f650, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ spl18_19), inference(avatar_component_clause, [], [f648])).
fof(f648, plain, (spl18_19 <=> closed_subset(sK4(sK15(sK2, sK3)), sK2)), introduced(avatar_definition, [new_symbols(naming, [spl18_19])])).
fof(f871, plain, (~ closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ subset(sK3, sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (~ spl18_15 | spl18_16 | ~ spl18_20)), inference(resolution, [], [f869, f167])).
fof(f167, plain, ! [X2] : (~ in(sK4(X2), powerset(the_carrier(sK2))) | ~ closed_subset(sK4(X2), sK2) | ~ element(sK4(X2), powerset(the_carrier(sK2))) | ~ subset(sK3, sK4(X2)) | ~ in(sK4(X2), X2)), inference(equality_resolution, [], [f108])).
fof(f108, plain, ! [X4, X2] : (~ subset(sK3, sK4(X2)) | ~ closed_subset(X4, sK2) | ~ (sK4(X2) = X4) | ~ element(X4, powerset(the_carrier(sK2))) | ~ in(sK4(X2), powerset(the_carrier(sK2))) | ~ in(sK4(X2), X2)), inference(cnf_transformation, [], [f76])).
fof(f869, plain, (in(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | (~ spl18_15 | spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f868, f100])).
fof(f868, plain, (in(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ topological_space(sK2) | (~ spl18_15 | spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f867, f101])).
fof(f867, plain, (in(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (~ spl18_15 | spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f866, f102])).
fof(f866, plain, (in(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (~ spl18_15 | spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f865, f610])).
fof(f865, plain, (in(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (~ spl18_15 | ~ spl18_20)), inference(subsumption_resolution, [], [f859, f659])).
fof(f859, plain, (in(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | ~ spl18_15), inference(superposition, [], [f160, f607])).
fof(f607, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ spl18_15), inference(avatar_component_clause, [], [f605])).
fof(f605, plain, (spl18_15 <=> (sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f160, plain, ! [X0, X3, X1] : (in(sK16(X0, X1, X3), powerset(the_carrier(X0))) | ~ in(X3, sK15(X0, X1)) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f856, plain, (spl18_15 | spl18_16 | ~ spl18_20), inference(avatar_split_clause, [], [f855, f657, f609, f605])).
fof(f855, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f854, f100])).
fof(f854, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f853, f101])).
fof(f853, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f852, f102])).
fof(f852, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f822, f610])).
fof(f822, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | ~ spl18_20), inference(resolution, [], [f659, f161])).
fof(f161, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | (sK16(X0, X1, X3) = X3) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f851, plain, (spl18_17 | spl18_16 | ~ spl18_20), inference(avatar_split_clause, [], [f850, f657, f609, f632])).
fof(f632, plain, (spl18_17 <=> (sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_17])])).
fof(f850, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3)))) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f849, f100])).
fof(f849, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f848, f101])).
fof(f848, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f847, f102])).
fof(f847, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_20)), inference(subsumption_resolution, [], [f823, f610])).
fof(f823, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3)))) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | ~ spl18_20), inference(resolution, [], [f659, f163])).
fof(f163, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | (sK17(X0, X1, X3) = X3) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f813, plain, (spl18_20 | spl18_21 | ~ spl18_18 | ~ spl18_19), inference(avatar_split_clause, [], [f788, f648, f636, f718, f657])).
fof(f788, plain, (! [X0] : (~ subset(X0, sK4(sK15(sK2, sK3))) | in(sK4(sK15(sK2, sK3)), sK15(sK2, X0)) | sP1(X0, sK2) | ~ element(X0, powerset(the_carrier(sK2))) | in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3))) | (~ spl18_18 | ~ spl18_19)), inference(subsumption_resolution, [], [f770, f650])).
fof(f770, plain, (! [X0] : (~ closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ subset(X0, sK4(sK15(sK2, sK3))) | in(sK4(sK15(sK2, sK3)), sK15(sK2, X0)) | sP1(X0, sK2) | ~ element(X0, powerset(the_carrier(sK2))) | in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3))) | ~ spl18_18), inference(resolution, [], [f638, f418])).
fof(f418, plain, ! [X0, X1] : (~ element(sK4(X1), powerset(the_carrier(sK2))) | ~ closed_subset(sK4(X1), sK2) | ~ subset(X0, sK4(X1)) | in(sK4(X1), sK15(sK2, X0)) | sP1(X0, sK2) | ~ element(X0, powerset(the_carrier(sK2))) | in(sK4(X1), X1)), inference(subsumption_resolution, [], [f417, f100])).
fof(f417, plain, ! [X0, X1] : (~ subset(X0, sK4(X1)) | ~ closed_subset(sK4(X1), sK2) | ~ element(sK4(X1), powerset(the_carrier(sK2))) | in(sK4(X1), sK15(sK2, X0)) | sP1(X0, sK2) | ~ element(X0, powerset(the_carrier(sK2))) | ~ topological_space(sK2) | in(sK4(X1), X1)), inference(subsumption_resolution, [], [f408, f101])).
fof(f408, plain, ! [X0, X1] : (~ subset(X0, sK4(X1)) | ~ closed_subset(sK4(X1), sK2) | ~ element(sK4(X1), powerset(the_carrier(sK2))) | in(sK4(X1), sK15(sK2, X0)) | sP1(X0, sK2) | ~ element(X0, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | in(sK4(X1), X1)), inference(resolution, [], [f169, f103])).
fof(f103, plain, ! [X2] : (in(sK4(X2), powerset(the_carrier(sK2))) | in(sK4(X2), X2)), inference(cnf_transformation, [], [f76])).
fof(f169, plain, ! [X0, X5, X1] : (~ in(X5, powerset(the_carrier(X0))) | ~ subset(X1, X5) | ~ closed_subset(X5, X0) | ~ element(X5, powerset(the_carrier(X0))) | in(X5, sK15(X0, X1)) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(equality_resolution, [], [f168])).
fof(f168, plain, ! [X4, X0, X5, X1] : (in(X5, sK15(X0, X1)) | ~ subset(X1, X5) | ~ closed_subset(X5, X0) | ~ element(X5, powerset(the_carrier(X0))) | ~ (X4 = X5) | ~ in(X4, powerset(the_carrier(X0))) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(equality_resolution, [], [f166])).
fof(f166, plain, ! [X4, X0, X5, X3, X1] : (in(X3, sK15(X0, X1)) | ~ subset(X1, X3) | ~ closed_subset(X5, X0) | ~ (X3 = X5) | ~ element(X5, powerset(the_carrier(X0))) | ~ (X3 = X4) | ~ in(X4, powerset(the_carrier(X0))) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f759, plain, (spl18_14 | spl18_20), inference(avatar_split_clause, [], [f754, f657, f601])).
fof(f601, plain, (spl18_14 <=> (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_14])])).
fof(f754, plain, ((sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | spl18_20), inference(resolution, [], [f658, f105])).
fof(f105, plain, ! [X2] : (in(sK4(X2), X2) | (sK4(X2) = sK5(X2))), inference(cnf_transformation, [], [f76])).
fof(f750, plain, (~ spl18_20 | spl18_19 | spl18_16 | ~ spl18_17), inference(avatar_split_clause, [], [f749, f632, f609, f648, f657])).
fof(f749, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (spl18_16 | ~ spl18_17)), inference(subsumption_resolution, [], [f748, f100])).
fof(f748, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_17)), inference(subsumption_resolution, [], [f747, f101])).
fof(f747, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_17)), inference(subsumption_resolution, [], [f746, f102])).
fof(f746, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_17)), inference(subsumption_resolution, [], [f739, f610])).
fof(f739, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | ~ spl18_17), inference(superposition, [], [f164, f634])).
fof(f634, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ spl18_17), inference(avatar_component_clause, [], [f632])).
fof(f164, plain, ! [X0, X3, X1] : (closed_subset(sK17(X0, X1, X3), X0) | ~ in(X3, sK15(X0, X1)) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f745, plain, (~ spl18_20 | spl18_16 | ~ spl18_17 | spl18_18), inference(avatar_split_clause, [], [f744, f636, f632, f609, f657])).
fof(f744, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (spl18_16 | ~ spl18_17 | spl18_18)), inference(subsumption_resolution, [], [f743, f100])).
fof(f743, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_17 | spl18_18)), inference(subsumption_resolution, [], [f742, f101])).
fof(f742, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_17 | spl18_18)), inference(subsumption_resolution, [], [f741, f102])).
fof(f741, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (spl18_16 | ~ spl18_17 | spl18_18)), inference(subsumption_resolution, [], [f740, f610])).
fof(f740, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | (~ spl18_17 | spl18_18)), inference(subsumption_resolution, [], [f738, f637])).
fof(f637, plain, (~ element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | spl18_18), inference(avatar_component_clause, [], [f636])).
fof(f738, plain, (element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | sP1(sK3, sK2) | ~ element(sK3, powerset(the_carrier(sK2))) | ~ top_str(sK2) | ~ topological_space(sK2) | ~ spl18_17), inference(superposition, [], [f162, f634])).
fof(f162, plain, ! [X0, X3, X1] : (element(sK17(X0, X1, X3), powerset(the_carrier(X0))) | ~ in(X3, sK15(X0, X1)) | sP1(X1, X0) | ~ element(X1, powerset(the_carrier(X0))) | ~ top_str(X0) | ~ topological_space(X0)), inference(cnf_transformation, [], [f99])).
fof(f697, plain, ~ spl18_16, inference(avatar_contradiction_clause, [], [f696])).
fof(f696, plain, ($false | ~ spl18_16), inference(subsumption_resolution, [], [f695, f611])).
fof(f611, plain, (sP1(sK3, sK2) | ~ spl18_16), inference(avatar_component_clause, [], [f609])).
fof(f695, plain, (~ sP1(sK3, sK2) | ~ spl18_16), inference(subsumption_resolution, [], [f693, f690])).
fof(f690, plain, ((sK11(sK3, sK2) = sK10(sK3, sK2)) | ~ spl18_16), inference(resolution, [], [f611, f148])).
fof(f148, plain, ! [X0, X1] : (~ sP1(X0, X1) | (sK10(X0, X1) = sK11(X0, X1))), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ! [X0, X1] : ((~ (sK11(X0, X1) = sK12(X0, X1)) & (subset(X0, sK12(X0, X1)) & closed_subset(sK13(X0, X1), X1) & (sK12(X0, X1) = sK13(X0, X1)) & element(sK13(X0, X1), powerset(the_carrier(X1)))) & (sK10(X0, X1) = sK12(X0, X1)) & sP0(sK11(X0, X1), X0, X1) & (sK10(X0, X1) = sK11(X0, X1))) | ~ sP1(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12, sK13])], [f86, f88, f87])).
fof(f87, plain, ! [X1, X0] : (? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (subset(X0, X4) & closed_subset(X5, X1) & (X4 = X5) & element(X5, powerset(the_carrier(X1)))) & (X2 = X4) & sP0(X3, X0, X1) & (X2 = X3)) => (~ (sK11(X0, X1) = sK12(X0, X1)) & ? [X5] : (subset(X0, sK12(X0, X1)) & closed_subset(X5, X1) & (sK12(X0, X1) = X5) & element(X5, powerset(the_carrier(X1)))) & (sK10(X0, X1) = sK12(X0, X1)) & sP0(sK11(X0, X1), X0, X1) & (sK10(X0, X1) = sK11(X0, X1)))), introduced(choice_axiom, [])).
fof(f88, plain, ! [X1, X0] : (? [X5] : (subset(X0, sK12(X0, X1)) & closed_subset(X5, X1) & (sK12(X0, X1) = X5) & element(X5, powerset(the_carrier(X1)))) => (subset(X0, sK12(X0, X1)) & closed_subset(sK13(X0, X1), X1) & (sK12(X0, X1) = sK13(X0, X1)) & element(sK13(X0, X1), powerset(the_carrier(X1))))), introduced(choice_axiom, [])).
fof(f86, plain, ! [X0, X1] : (? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (subset(X0, X4) & closed_subset(X5, X1) & (X4 = X5) & element(X5, powerset(the_carrier(X1)))) & (X2 = X4) & sP0(X3, X0, X1) & (X2 = X3)) | ~ sP1(X0, X1)), inference(rectify, [], [f85])).
fof(f85, plain, ! [X1, X0] : (? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (subset(X1, X4) & closed_subset(X5, X0) & (X4 = X5) & element(X5, powerset(the_carrier(X0)))) & (X2 = X4) & sP0(X3, X1, X0) & (X2 = X3)) | ~ sP1(X1, X0)), inference(nnf_transformation, [], [f68])).
fof(f693, plain, (~ (sK11(sK3, sK2) = sK10(sK3, sK2)) | ~ sP1(sK3, sK2) | ~ spl18_16), inference(superposition, [], [f155, f689])).
fof(f689, plain, ((sK12(sK3, sK2) = sK10(sK3, sK2)) | ~ spl18_16), inference(resolution, [], [f611, f150])).
fof(f150, plain, ! [X0, X1] : (~ sP1(X0, X1) | (sK10(X0, X1) = sK12(X0, X1))), inference(cnf_transformation, [], [f89])).
fof(f155, plain, ! [X0, X1] : (~ (sK11(X0, X1) = sK12(X0, X1)) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f89])).
fof(f661, plain, (spl18_20 | spl18_19 | ~ spl18_14), inference(avatar_split_clause, [], [f627, f601, f648, f657])).
fof(f627, plain, (closed_subset(sK4(sK15(sK2, sK3)), sK2) | in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ spl18_14), inference(superposition, [], [f106, f603])).
fof(f603, plain, ((sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | ~ spl18_14), inference(avatar_component_clause, [], [f601])).
fof(f106, plain, ! [X2] : (closed_subset(sK5(X2), sK2) | in(sK4(X2), X2)), inference(cnf_transformation, [], [f76])).
fof(f660, plain, (spl18_20 | spl18_18 | ~ spl18_14), inference(avatar_split_clause, [], [f626, f601, f636, f657])).
fof(f626, plain, (element(sK4(sK15(sK2, sK3)), powerset(the_carrier(sK2))) | in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ spl18_14), inference(superposition, [], [f104, f603])).
fof(f104, plain, ! [X2] : (element(sK5(X2), powerset(the_carrier(sK2))) | in(sK4(X2), X2)), inference(cnf_transformation, [], [f76])).