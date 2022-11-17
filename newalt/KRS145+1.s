fof(f248, plain, $false, inference(avatar_sat_refutation, [], [f91, f96, f101, f106, f111, f116, f121, f129, f133, f151, f247])).
fof(f247, plain, (spl9_3 | spl9_4 | spl9_5 | ~ spl9_6 | ~ spl9_7 | ~ spl9_8 | ~ spl9_9), inference(avatar_contradiction_clause, [], [f246])).
fof(f246, plain, ($false | (spl9_3 | spl9_4 | spl9_5 | ~ spl9_6 | ~ spl9_7 | ~ spl9_8 | ~ spl9_9)), inference(subsumption_resolution, [], [f245, f100])).
fof(f100, plain, (~ (sK4 = sK5) | spl9_5), inference(avatar_component_clause, [], [f98])).
fof(f98, plain, (spl9_5 <=> (sK4 = sK5)), introduced(avatar_definition, [new_symbols(naming, [spl9_5])])).
fof(f245, plain, ((sK4 = sK5) | (spl9_3 | spl9_4 | ~ spl9_6 | ~ spl9_7 | ~ spl9_8 | ~ spl9_9)), inference(subsumption_resolution, [], [f242, f95])).
fof(f95, plain, (~ (sK4 = sK6) | spl9_4), inference(avatar_component_clause, [], [f93])).
fof(f93, plain, (spl9_4 <=> (sK4 = sK6)), introduced(avatar_definition, [new_symbols(naming, [spl9_4])])).
fof(f242, plain, ((sK4 = sK6) | (sK4 = sK5) | (spl9_3 | ~ spl9_6 | ~ spl9_7 | ~ spl9_8 | ~ spl9_9)), inference(resolution, [], [f164, f115])).
fof(f115, plain, (rp(sK3, sK4) | ~ spl9_8), inference(avatar_component_clause, [], [f113])).
fof(f113, plain, (spl9_8 <=> rp(sK3, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl9_8])])).
fof(f164, plain, (! [X1] : (~ rp(sK3, X1) | (sK6 = X1) | (sK5 = X1)) | (spl9_3 | ~ spl9_6 | ~ spl9_7 | ~ spl9_9)), inference(subsumption_resolution, [], [f159, f90])).
fof(f90, plain, (~ (sK5 = sK6) | spl9_3), inference(avatar_component_clause, [], [f88])).
fof(f88, plain, (spl9_3 <=> (sK5 = sK6)), introduced(avatar_definition, [new_symbols(naming, [spl9_3])])).
fof(f159, plain, (! [X1] : ((sK5 = X1) | (sK5 = sK6) | (sK6 = X1) | ~ rp(sK3, X1)) | (~ spl9_6 | ~ spl9_7 | ~ spl9_9)), inference(resolution, [], [f153, f110])).
fof(f110, plain, (rp(sK3, sK5) | ~ spl9_7), inference(avatar_component_clause, [], [f108])).
fof(f108, plain, (spl9_7 <=> rp(sK3, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl9_7])])).
fof(f153, plain, (! [X0, X1] : (~ rp(sK3, X1) | (X0 = X1) | (sK6 = X1) | (sK6 = X0) | ~ rp(sK3, X0)) | (~ spl9_6 | ~ spl9_9)), inference(subsumption_resolution, [], [f152, f120])).
fof(f120, plain, (cc(sK3) | ~ spl9_9), inference(avatar_component_clause, [], [f118])).
fof(f118, plain, (spl9_9 <=> cc(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl9_9])])).
fof(f152, plain, (! [X0, X1] : ((sK6 = X0) | (X0 = X1) | (sK6 = X1) | ~ rp(sK3, X1) | ~ rp(sK3, X0) | ~ cc(sK3)) | ~ spl9_6), inference(resolution, [], [f105, f60])).
fof(f60, plain, ! [X4, X0, X5, X3] : (~ rp(X0, X5) | (X3 = X5) | (X3 = X4) | (X4 = X5) | ~ rp(X0, X4) | ~ rp(X0, X3) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (((~ (sK1(X0) = sK2(X0)) & rp(X0, sK2(X0)) & rp(X0, sK1(X0))) & ! [X3, X4, X5] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | ~ rp(X0, X5) | ~ rp(X0, X4) | ~ rp(X0, X3))) | ~ cc(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f32, f38])).
fof(f38, plain, ! [X0] : (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) => (~ (sK1(X0) = sK2(X0)) & rp(X0, sK2(X0)) & rp(X0, sK1(X0)))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0] : ((? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & ! [X3, X4, X5] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | ~ rp(X0, X5) | ~ rp(X0, X4) | ~ rp(X0, X3))) | ~ cc(X0)), inference(flattening, [], [f31])).
fof(f31, plain, ! [X0] : ((? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & ! [X3, X4, X5] : (((X4 = X5) | (X3 = X5) | (X3 = X4)) | (~ rp(X0, X5) | ~ rp(X0, X4) | ~ rp(X0, X3)))) | ~ cc(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (cc(X0) => (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & ! [X3, X4, X5] : ((rp(X0, X5) & rp(X0, X4) & rp(X0, X3)) => ((X4 = X5) | (X3 = X5) | (X3 = X4))))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X3] : (cc(X3) => (? [X4, X5] : (~ (X4 = X5) & rp(X3, X5) & rp(X3, X4)) & ! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5))))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS145+1.p', axiom_2)).
fof(f105, plain, (rp(sK3, sK6) | ~ spl9_6), inference(avatar_component_clause, [], [f103])).
fof(f103, plain, (spl9_6 <=> rp(sK3, sK6)), introduced(avatar_definition, [new_symbols(naming, [spl9_6])])).
fof(f151, plain, (~ spl9_2 | ~ spl9_9), inference(avatar_contradiction_clause, [], [f150])).
fof(f150, plain, ($false | (~ spl9_2 | ~ spl9_9)), inference(subsumption_resolution, [], [f149, f120])).
fof(f149, plain, (~ cc(sK3) | (~ spl9_2 | ~ spl9_9)), inference(trivial_inequality_removal, [], [f147])).
fof(f147, plain, (~ (sK1(sK3) = sK1(sK3)) | ~ cc(sK3) | (~ spl9_2 | ~ spl9_9)), inference(superposition, [], [f63, f141])).
fof(f141, plain, ((sK1(sK3) = sK2(sK3)) | (~ spl9_2 | ~ spl9_9)), inference(subsumption_resolution, [], [f140, f120])).
fof(f140, plain, ((sK1(sK3) = sK2(sK3)) | ~ cc(sK3) | (~ spl9_2 | ~ spl9_9)), inference(resolution, [], [f137, f62])).
fof(f62, plain, ! [X0] : (rp(X0, sK2(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f137, plain, (! [X0] : (~ rp(sK3, X0) | (sK1(sK3) = X0)) | (~ spl9_2 | ~ spl9_9)), inference(subsumption_resolution, [], [f135, f120])).
fof(f135, plain, (! [X0] : (~ rp(sK3, X0) | (sK1(sK3) = X0) | ~ cc(sK3)) | ~ spl9_2), inference(resolution, [], [f86, f61])).
fof(f61, plain, ! [X0] : (rp(X0, sK1(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f86, plain, (! [X4, X5] : (~ rp(sK3, X5) | ~ rp(sK3, X4) | (X4 = X5)) | ~ spl9_2), inference(avatar_component_clause, [], [f85])).
fof(f85, plain, (spl9_2 <=> ! [X5, X4] : ((X4 = X5) | ~ rp(sK3, X4) | ~ rp(sK3, X5))), introduced(avatar_definition, [new_symbols(naming, [spl9_2])])).
fof(f63, plain, ! [X0] : (~ (sK1(X0) = sK2(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f133, plain, (spl9_10 | spl9_1), inference(avatar_split_clause, [], [f132, f81, f126])).
fof(f126, plain, (spl9_10 <=> xsd_string(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl9_10])])).
fof(f81, plain, (spl9_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl9_1])])).
fof(f132, plain, (sP0 | xsd_string(sK7)), inference(subsumption_resolution, [], [f131, f59])).
fof(f59, plain, ! [X0] : (xsd_string(X0) | xsd_integer(X0)), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : ((xsd_string(X0) | xsd_integer(X0)) & (~ xsd_integer(X0) | ~ xsd_string(X0))), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0] : (xsd_string(X0) <=> ~ xsd_integer(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS145+1.p', axiom_1)).
fof(f131, plain, (sP0 | ~ xsd_integer(sK7) | xsd_string(sK7)), inference(subsumption_resolution, [], [f130, f56])).
fof(f56, plain, ! [X0] : cowlThing(X0), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (~ cowlNothing(X0) & cowlThing(X0)), inference(rectify, [], [f8])).
fof(f8, plain, ! [X3] : (~ cowlNothing(X3) & cowlThing(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS145+1.p', axiom_0)).
fof(f130, plain, (sP0 | ~ xsd_integer(sK7) | xsd_string(sK7) | ~ cowlThing(sK8)), inference(subsumption_resolution, [], [f71, f57])).
fof(f57, plain, ! [X0] : ~ cowlNothing(X0), inference(cnf_transformation, [], [f13])).
fof(f71, plain, (sP0 | ~ xsd_integer(sK7) | xsd_string(sK7) | cowlNothing(sK8) | ~ cowlThing(sK8)), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (sP0 | ((xsd_integer(sK7) | ~ xsd_string(sK7)) & (~ xsd_integer(sK7) | xsd_string(sK7))) | (cowlNothing(sK8) | ~ cowlThing(sK8))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f45, f47, f46])).
fof(f46, plain, (? [X0] : ((xsd_integer(X0) | ~ xsd_string(X0)) & (~ xsd_integer(X0) | xsd_string(X0))) => ((xsd_integer(sK7) | ~ xsd_string(sK7)) & (~ xsd_integer(sK7) | xsd_string(sK7)))), introduced(choice_axiom, [])).
fof(f47, plain, (? [X1] : (cowlNothing(X1) | ~ cowlThing(X1)) => (cowlNothing(sK8) | ~ cowlThing(sK8))), introduced(choice_axiom, [])).
fof(f45, plain, (sP0 | ? [X0] : ((xsd_integer(X0) | ~ xsd_string(X0)) & (~ xsd_integer(X0) | xsd_string(X0))) | ? [X1] : (cowlNothing(X1) | ~ cowlThing(X1))), inference(rectify, [], [f44])).
fof(f44, plain, (sP0 | ? [X6] : ((xsd_integer(X6) | ~ xsd_string(X6)) & (~ xsd_integer(X6) | xsd_string(X6))) | ? [X7] : (cowlNothing(X7) | ~ cowlThing(X7))), inference(nnf_transformation, [], [f36])).
fof(f36, plain, (sP0 | ? [X6] : ~ (xsd_string(X6) <=> ~ xsd_integer(X6)) | ? [X7] : (cowlNothing(X7) | ~ cowlThing(X7))), inference(definition_folding, [], [f34, e35])).
fof(f35, plain, (? [X0] : ((? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) | ! [X4, X5] : ((X4 = X5) | ~ rp(X0, X5) | ~ rp(X0, X4))) & cc(X0)) | ~ sP0), inference(usedef, [], [e35])).
fof(e35, plain, (sP0 <=> ? [X0] : ((? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) | ! [X4, X5] : ((X4 = X5) | ~ rp(X0, X5) | ~ rp(X0, X4))) & cc(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f34, plain, (? [X0] : ((? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) | ! [X4, X5] : ((X4 = X5) | ~ rp(X0, X5) | ~ rp(X0, X4))) & cc(X0)) | ? [X6] : ~ (xsd_string(X6) <=> ~ xsd_integer(X6)) | ? [X7] : (cowlNothing(X7) | ~ cowlThing(X7))), inference(flattening, [], [f33])).
fof(f33, plain, (? [X0] : ((? [X1, X2, X3] : ((~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2)) & (rp(X0, X3) & rp(X0, X2) & rp(X0, X1))) | ! [X4, X5] : ((X4 = X5) | ~ rp(X0, X5) | ~ rp(X0, X4))) & cc(X0)) | ? [X6] : ~ (xsd_string(X6) <=> ~ xsd_integer(X6)) | ? [X7] : (cowlNothing(X7) | ~ cowlThing(X7))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ (! [X0] : (cc(X0) => (! [X1, X2, X3] : ((rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) => ((X2 = X3) | (X1 = X3) | (X1 = X2))) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4)))) & ! [X6] : (xsd_string(X6) <=> ~ xsd_integer(X6)) & ! [X7] : (~ cowlNothing(X7) & cowlThing(X7))), inference(rectify, [], [f12])).
fof(f12, plain, ~ (! [X3] : (cc(X3) => (! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5))) & ? [X4, X5] : (~ (X4 = X5) & rp(X3, X5) & rp(X3, X4)))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), inference(negated_conjecture, [], [f11])).
fof(f11, plain, ~ (! [X3] : (cc(X3) => (! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5))) & ? [X4, X5] : (~ (X4 = X5) & rp(X3, X5) & rp(X3, X4)))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS145+1.p', the_axiom)).
fof(f129, plain, (~ spl9_10 | spl9_1), inference(avatar_split_clause, [], [f124, f81, f126])).
fof(f124, plain, (sP0 | ~ xsd_string(sK7)), inference(subsumption_resolution, [], [f123, f58])).
fof(f58, plain, ! [X0] : (~ xsd_string(X0) | ~ xsd_integer(X0)), inference(cnf_transformation, [], [f37])).
fof(f123, plain, (sP0 | xsd_integer(sK7) | ~ xsd_string(sK7)), inference(subsumption_resolution, [], [f122, f56])).
fof(f122, plain, (sP0 | xsd_integer(sK7) | ~ xsd_string(sK7) | ~ cowlThing(sK8)), inference(subsumption_resolution, [], [f72, f57])).
fof(f72, plain, (sP0 | xsd_integer(sK7) | ~ xsd_string(sK7) | cowlNothing(sK8) | ~ cowlThing(sK8)), inference(cnf_transformation, [], [f48])).
fof(f121, plain, (~ spl9_1 | spl9_9), inference(avatar_split_clause, [], [f64, f118, f81])).
fof(f64, plain, (cc(sK3) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((((~ (sK5 = sK6) & ~ (sK4 = sK6) & ~ (sK4 = sK5) & rp(sK3, sK6) & rp(sK3, sK5) & rp(sK3, sK4)) | ! [X4, X5] : ((X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4))) & cc(sK3)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3, sK4, sK5, sK6])], [f40, f42, f41])).
fof(f41, plain, (? [X0] : ((? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) | ! [X4, X5] : ((X4 = X5) | ~ rp(X0, X5) | ~ rp(X0, X4))) & cc(X0)) => ((? [X3, X2, X1] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(sK3, X3) & rp(sK3, X2) & rp(sK3, X1)) | ! [X5, X4] : ((X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4))) & cc(sK3))), introduced(choice_axiom, [])).
fof(f42, plain, (? [X3, X2, X1] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(sK3, X3) & rp(sK3, X2) & rp(sK3, X1)) => (~ (sK5 = sK6) & ~ (sK4 = sK6) & ~ (sK4 = sK5) & rp(sK3, sK6) & rp(sK3, sK5) & rp(sK3, sK4))), introduced(choice_axiom, [])).
fof(f40, plain, (? [X0] : ((? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) | ! [X4, X5] : ((X4 = X5) | ~ rp(X0, X5) | ~ rp(X0, X4))) & cc(X0)) | ~ sP0), inference(nnf_transformation, [], [f35])).
fof(f116, plain, (~ spl9_1 | spl9_2 | spl9_8), inference(avatar_split_clause, [], [f65, f113, f85, f81])).
fof(f65, plain, ! [X4, X5] : (rp(sK3, sK4) | (X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f111, plain, (~ spl9_1 | spl9_2 | spl9_7), inference(avatar_split_clause, [], [f66, f108, f85, f81])).
fof(f66, plain, ! [X4, X5] : (rp(sK3, sK5) | (X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f106, plain, (~ spl9_1 | spl9_2 | spl9_6), inference(avatar_split_clause, [], [f67, f103, f85, f81])).
fof(f67, plain, ! [X4, X5] : (rp(sK3, sK6) | (X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f101, plain, (~ spl9_1 | spl9_2 | ~ spl9_5), inference(avatar_split_clause, [], [f68, f98, f85, f81])).
fof(f68, plain, ! [X4, X5] : (~ (sK4 = sK5) | (X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f96, plain, (~ spl9_1 | spl9_2 | ~ spl9_4), inference(avatar_split_clause, [], [f69, f93, f85, f81])).
fof(f69, plain, ! [X4, X5] : (~ (sK4 = sK6) | (X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f91, plain, (~ spl9_1 | spl9_2 | ~ spl9_3), inference(avatar_split_clause, [], [f70, f88, f85, f81])).
fof(f70, plain, ! [X4, X5] : (~ (sK5 = sK6) | (X4 = X5) | ~ rp(sK3, X5) | ~ rp(sK3, X4) | ~ sP0), inference(cnf_transformation, [], [f43])).