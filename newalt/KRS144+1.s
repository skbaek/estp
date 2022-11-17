fof(f279, plain, $false, inference(avatar_sat_refutation, [], [f92, f97, f102, f107, f112, f117, f122, f133, f137, f145, f149, f167, f278])).
fof(f278, plain, (spl10_2 | spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | ~ spl10_7 | ~ spl10_8), inference(avatar_contradiction_clause, [], [f277])).
fof(f277, plain, ($false | (spl10_2 | spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | ~ spl10_7 | ~ spl10_8)), inference(subsumption_resolution, [], [f276, f101])).
fof(f101, plain, (~ (sK4 = sK5) | spl10_4), inference(avatar_component_clause, [], [f99])).
fof(f99, plain, (spl10_4 <=> (sK4 = sK5)), introduced(avatar_definition, [new_symbols(naming, [spl10_4])])).
fof(f276, plain, ((sK4 = sK5) | (spl10_2 | spl10_3 | ~ spl10_5 | ~ spl10_6 | ~ spl10_7 | ~ spl10_8)), inference(subsumption_resolution, [], [f273, f96])).
fof(f96, plain, (~ (sK4 = sK6) | spl10_3), inference(avatar_component_clause, [], [f94])).
fof(f94, plain, (spl10_3 <=> (sK4 = sK6)), introduced(avatar_definition, [new_symbols(naming, [spl10_3])])).
fof(f273, plain, ((sK4 = sK6) | (sK4 = sK5) | (spl10_2 | ~ spl10_5 | ~ spl10_6 | ~ spl10_7 | ~ spl10_8)), inference(resolution, [], [f180, f116])).
fof(f116, plain, (rp(sK3, sK4) | ~ spl10_7), inference(avatar_component_clause, [], [f114])).
fof(f114, plain, (spl10_7 <=> rp(sK3, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_7])])).
fof(f180, plain, (! [X1] : (~ rp(sK3, X1) | (sK6 = X1) | (sK5 = X1)) | (spl10_2 | ~ spl10_5 | ~ spl10_6 | ~ spl10_8)), inference(subsumption_resolution, [], [f175, f91])).
fof(f91, plain, (~ (sK5 = sK6) | spl10_2), inference(avatar_component_clause, [], [f89])).
fof(f89, plain, (spl10_2 <=> (sK5 = sK6)), introduced(avatar_definition, [new_symbols(naming, [spl10_2])])).
fof(f175, plain, (! [X1] : ((sK5 = X1) | (sK5 = sK6) | (sK6 = X1) | ~ rp(sK3, X1)) | (~ spl10_5 | ~ spl10_6 | ~ spl10_8)), inference(resolution, [], [f169, f111])).
fof(f111, plain, (rp(sK3, sK5) | ~ spl10_6), inference(avatar_component_clause, [], [f109])).
fof(f109, plain, (spl10_6 <=> rp(sK3, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl10_6])])).
fof(f169, plain, (! [X0, X1] : (~ rp(sK3, X1) | (X0 = X1) | (sK6 = X1) | (sK6 = X0) | ~ rp(sK3, X0)) | (~ spl10_5 | ~ spl10_8)), inference(subsumption_resolution, [], [f168, f121])).
fof(f121, plain, (cc(sK3) | ~ spl10_8), inference(avatar_component_clause, [], [f119])).
fof(f119, plain, (spl10_8 <=> cc(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl10_8])])).
fof(f168, plain, (! [X0, X1] : ((sK6 = X0) | (X0 = X1) | (sK6 = X1) | ~ rp(sK3, X1) | ~ rp(sK3, X0) | ~ cc(sK3)) | ~ spl10_5), inference(resolution, [], [f106, f65])).
fof(f65, plain, ! [X2, X0, X3, X1] : (~ rp(X0, X3) | (X1 = X3) | (X1 = X2) | (X2 = X3) | ~ rp(X0, X2) | ~ rp(X0, X1) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : ((! [X1, X2, X3] : ((X2 = X3) | (X1 = X3) | (X1 = X2) | ~ rp(X0, X3) | ~ rp(X0, X2) | ~ rp(X0, X1)) & (~ (sK1(X0) = sK2(X0)) & rp(X0, sK2(X0)) & rp(X0, sK1(X0)))) | ~ cc(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f32, f38])).
fof(f38, plain, ! [X0] : (? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4)) => (~ (sK1(X0) = sK2(X0)) & rp(X0, sK2(X0)) & rp(X0, sK1(X0)))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0] : ((! [X1, X2, X3] : ((X2 = X3) | (X1 = X3) | (X1 = X2) | ~ rp(X0, X3) | ~ rp(X0, X2) | ~ rp(X0, X1)) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4))) | ~ cc(X0)), inference(flattening, [], [f31])).
fof(f31, plain, ! [X0] : ((! [X1, X2, X3] : (((X2 = X3) | (X1 = X3) | (X1 = X2)) | (~ rp(X0, X3) | ~ rp(X0, X2) | ~ rp(X0, X1))) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4))) | ~ cc(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (cc(X0) => (! [X1, X2, X3] : ((rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) => ((X2 = X3) | (X1 = X3) | (X1 = X2))) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4)))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X3] : (cc(X3) => (! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5))) & ? [X4, X5] : (~ (X4 = X5) & rp(X3, X5) & rp(X3, X4)))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS144+1.p', axiom_2)).
fof(f106, plain, (rp(sK3, sK6) | ~ spl10_5), inference(avatar_component_clause, [], [f104])).
fof(f104, plain, (spl10_5 <=> rp(sK3, sK6)), introduced(avatar_definition, [new_symbols(naming, [spl10_5])])).
fof(f167, plain, (~ spl10_10 | ~ spl10_11), inference(avatar_contradiction_clause, [], [f166])).
fof(f166, plain, ($false | (~ spl10_10 | ~ spl10_11)), inference(subsumption_resolution, [], [f165, f144])).
fof(f144, plain, (cc(sK7) | ~ spl10_11), inference(avatar_component_clause, [], [f142])).
fof(f142, plain, (spl10_11 <=> cc(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl10_11])])).
fof(f165, plain, (~ cc(sK7) | (~ spl10_10 | ~ spl10_11)), inference(trivial_inequality_removal, [], [f163])).
fof(f163, plain, (~ (sK1(sK7) = sK1(sK7)) | ~ cc(sK7) | (~ spl10_10 | ~ spl10_11)), inference(superposition, [], [f64, f157])).
fof(f157, plain, ((sK1(sK7) = sK2(sK7)) | (~ spl10_10 | ~ spl10_11)), inference(subsumption_resolution, [], [f156, f144])).
fof(f156, plain, ((sK1(sK7) = sK2(sK7)) | ~ cc(sK7) | (~ spl10_10 | ~ spl10_11)), inference(resolution, [], [f153, f63])).
fof(f63, plain, ! [X0] : (rp(X0, sK2(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f153, plain, (! [X0] : (~ rp(sK7, X0) | (sK1(sK7) = X0)) | (~ spl10_10 | ~ spl10_11)), inference(subsumption_resolution, [], [f151, f144])).
fof(f151, plain, (! [X0] : (~ rp(sK7, X0) | (sK1(sK7) = X0) | ~ cc(sK7)) | ~ spl10_10), inference(resolution, [], [f132, f62])).
fof(f62, plain, ! [X0] : (rp(X0, sK1(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f132, plain, (! [X2, X1] : (~ rp(sK7, X2) | ~ rp(sK7, X1) | (X1 = X2)) | ~ spl10_10), inference(avatar_component_clause, [], [f131])).
fof(f131, plain, (spl10_10 <=> ! [X1, X2] : ((X1 = X2) | ~ rp(sK7, X1) | ~ rp(sK7, X2))), introduced(avatar_definition, [new_symbols(naming, [spl10_10])])).
fof(f64, plain, ! [X0] : (~ (sK1(X0) = sK2(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f149, plain, (spl10_9 | spl10_1 | spl10_11), inference(avatar_split_clause, [], [f148, f142, f85, f127])).
fof(f127, plain, (spl10_9 <=> xsd_string(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_9])])).
fof(f85, plain, (spl10_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f148, plain, (cc(sK7) | sP0 | xsd_string(sK8)), inference(subsumption_resolution, [], [f147, f61])).
fof(f61, plain, ! [X0] : (xsd_string(X0) | xsd_integer(X0)), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : ((xsd_string(X0) | xsd_integer(X0)) & (~ xsd_integer(X0) | ~ xsd_string(X0))), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0] : (xsd_string(X0) <=> ~ xsd_integer(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS144+1.p', axiom_1)).
fof(f147, plain, (cc(sK7) | sP0 | ~ xsd_integer(sK8) | xsd_string(sK8)), inference(subsumption_resolution, [], [f146, f58])).
fof(f58, plain, ! [X0] : cowlThing(X0), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (~ cowlNothing(X0) & cowlThing(X0)), inference(rectify, [], [f8])).
fof(f8, plain, ! [X3] : (~ cowlNothing(X3) & cowlThing(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS144+1.p', axiom_0)).
fof(f146, plain, (cc(sK7) | sP0 | ~ xsd_integer(sK8) | xsd_string(sK8) | ~ cowlThing(sK9)), inference(subsumption_resolution, [], [f73, f59])).
fof(f59, plain, ! [X0] : ~ cowlNothing(X0), inference(cnf_transformation, [], [f13])).
fof(f73, plain, (cc(sK7) | sP0 | ~ xsd_integer(sK8) | xsd_string(sK8) | cowlNothing(sK9) | ~ cowlThing(sK9)), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((! [X1, X2] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1)) & cc(sK7)) | sP0 | ((xsd_integer(sK8) | ~ xsd_string(sK8)) & (~ xsd_integer(sK8) | xsd_string(sK8))) | (cowlNothing(sK9) | ~ cowlThing(sK9))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8, sK9])], [f46, f49, f48, f47])).
fof(f47, plain, (? [X0] : (! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & cc(X0)) => (! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1)) & cc(sK7))), introduced(choice_axiom, [])).
fof(f48, plain, (? [X3] : ((xsd_integer(X3) | ~ xsd_string(X3)) & (~ xsd_integer(X3) | xsd_string(X3))) => ((xsd_integer(sK8) | ~ xsd_string(sK8)) & (~ xsd_integer(sK8) | xsd_string(sK8)))), introduced(choice_axiom, [])).
fof(f49, plain, (? [X4] : (cowlNothing(X4) | ~ cowlThing(X4)) => (cowlNothing(sK9) | ~ cowlThing(sK9))), introduced(choice_axiom, [])).
fof(f46, plain, (? [X0] : (! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & cc(X0)) | sP0 | ? [X3] : ((xsd_integer(X3) | ~ xsd_string(X3)) & (~ xsd_integer(X3) | xsd_string(X3))) | ? [X4] : (cowlNothing(X4) | ~ cowlThing(X4))), inference(rectify, [], [f45])).
fof(f45, plain, (? [X0] : (! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & cc(X0)) | sP0 | ? [X7] : ((xsd_integer(X7) | ~ xsd_string(X7)) & (~ xsd_integer(X7) | xsd_string(X7))) | ? [X8] : (cowlNothing(X8) | ~ cowlThing(X8))), inference(nnf_transformation, [], [f36])).
fof(f36, plain, (? [X0] : (! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & cc(X0)) | sP0 | ? [X7] : ~ (xsd_string(X7) <=> ~ xsd_integer(X7)) | ? [X8] : (cowlNothing(X8) | ~ cowlThing(X8))), inference(definition_folding, [], [f34, e35])).
fof(f35, plain, (? [X3] : (? [X4, X5, X6] : (~ (X5 = X6) & ~ (X4 = X6) & ~ (X4 = X5) & rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) & cc(X3)) | ~ sP0), inference(usedef, [], [e35])).
fof(e35, plain, (sP0 <=> ? [X3] : (? [X4, X5, X6] : (~ (X5 = X6) & ~ (X4 = X6) & ~ (X4 = X5) & rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) & cc(X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f34, plain, (? [X0] : (! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & cc(X0)) | ? [X3] : (? [X4, X5, X6] : (~ (X5 = X6) & ~ (X4 = X6) & ~ (X4 = X5) & rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) & cc(X3)) | ? [X7] : ~ (xsd_string(X7) <=> ~ xsd_integer(X7)) | ? [X8] : (cowlNothing(X8) | ~ cowlThing(X8))), inference(flattening, [], [f33])).
fof(f33, plain, (? [X0] : (! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & cc(X0)) | ? [X3] : (? [X4, X5, X6] : ((~ (X5 = X6) & ~ (X4 = X6) & ~ (X4 = X5)) & (rp(X3, X6) & rp(X3, X5) & rp(X3, X4))) & cc(X3)) | ? [X7] : ~ (xsd_string(X7) <=> ~ xsd_integer(X7)) | ? [X8] : (cowlNothing(X8) | ~ cowlThing(X8))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ (! [X0] : (cc(X0) => ? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1))) & ! [X3] : (cc(X3) => ! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5)))) & ! [X7] : (xsd_string(X7) <=> ~ xsd_integer(X7)) & ! [X8] : (~ cowlNothing(X8) & cowlThing(X8))), inference(rectify, [], [f12])).
fof(f12, plain, ~ (! [X3] : (cc(X3) => ? [X4, X5] : (~ (X4 = X5) & rp(X3, X5) & rp(X3, X4))) & ! [X3] : (cc(X3) => ! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5)))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), inference(negated_conjecture, [], [f11])).
fof(f11, plain, ~ (! [X3] : (cc(X3) => ? [X4, X5] : (~ (X4 = X5) & rp(X3, X5) & rp(X3, X4))) & ! [X3] : (cc(X3) => ! [X4, X5, X6] : ((rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) => ((X5 = X6) | (X4 = X6) | (X4 = X5)))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS144+1.p', the_axiom)).
fof(f145, plain, (~ spl10_9 | spl10_1 | spl10_11), inference(avatar_split_clause, [], [f140, f142, f85, f127])).
fof(f140, plain, (cc(sK7) | sP0 | ~ xsd_string(sK8)), inference(subsumption_resolution, [], [f139, f60])).
fof(f60, plain, ! [X0] : (~ xsd_string(X0) | ~ xsd_integer(X0)), inference(cnf_transformation, [], [f37])).
fof(f139, plain, (cc(sK7) | sP0 | xsd_integer(sK8) | ~ xsd_string(sK8)), inference(subsumption_resolution, [], [f138, f58])).
fof(f138, plain, (cc(sK7) | sP0 | xsd_integer(sK8) | ~ xsd_string(sK8) | ~ cowlThing(sK9)), inference(subsumption_resolution, [], [f74, f59])).
fof(f74, plain, (cc(sK7) | sP0 | xsd_integer(sK8) | ~ xsd_string(sK8) | cowlNothing(sK9) | ~ cowlThing(sK9)), inference(cnf_transformation, [], [f50])).
fof(f137, plain, (spl10_9 | spl10_1 | spl10_10), inference(avatar_split_clause, [], [f136, f131, f85, f127])).
fof(f136, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | xsd_string(sK8)), inference(subsumption_resolution, [], [f135, f61])).
fof(f135, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | ~ xsd_integer(sK8) | xsd_string(sK8)), inference(subsumption_resolution, [], [f134, f58])).
fof(f134, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | ~ xsd_integer(sK8) | xsd_string(sK8) | ~ cowlThing(sK9)), inference(subsumption_resolution, [], [f75, f59])).
fof(f75, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | ~ xsd_integer(sK8) | xsd_string(sK8) | cowlNothing(sK9) | ~ cowlThing(sK9)), inference(cnf_transformation, [], [f50])).
fof(f133, plain, (~ spl10_9 | spl10_1 | spl10_10), inference(avatar_split_clause, [], [f125, f131, f85, f127])).
fof(f125, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | ~ xsd_string(sK8)), inference(subsumption_resolution, [], [f124, f60])).
fof(f124, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | xsd_integer(sK8) | ~ xsd_string(sK8)), inference(subsumption_resolution, [], [f123, f58])).
fof(f123, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | xsd_integer(sK8) | ~ xsd_string(sK8) | ~ cowlThing(sK9)), inference(subsumption_resolution, [], [f76, f59])).
fof(f76, plain, ! [X2, X1] : ((X1 = X2) | ~ rp(sK7, X2) | ~ rp(sK7, X1) | sP0 | xsd_integer(sK8) | ~ xsd_string(sK8) | cowlNothing(sK9) | ~ cowlThing(sK9)), inference(cnf_transformation, [], [f50])).
fof(f122, plain, (~ spl10_1 | spl10_8), inference(avatar_split_clause, [], [f66, f119, f85])).
fof(f66, plain, (cc(sK3) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((~ (sK5 = sK6) & ~ (sK4 = sK6) & ~ (sK4 = sK5) & rp(sK3, sK6) & rp(sK3, sK5) & rp(sK3, sK4)) & cc(sK3)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3, sK4, sK5, sK6])], [f41, f43, f42])).
fof(f42, plain, (? [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) & cc(X0)) => (? [X3, X2, X1] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(sK3, X3) & rp(sK3, X2) & rp(sK3, X1)) & cc(sK3))), introduced(choice_axiom, [])).
fof(f43, plain, (? [X3, X2, X1] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(sK3, X3) & rp(sK3, X2) & rp(sK3, X1)) => (~ (sK5 = sK6) & ~ (sK4 = sK6) & ~ (sK4 = sK5) & rp(sK3, sK6) & rp(sK3, sK5) & rp(sK3, sK4))), introduced(choice_axiom, [])).
fof(f41, plain, (? [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rp(X0, X3) & rp(X0, X2) & rp(X0, X1)) & cc(X0)) | ~ sP0), inference(rectify, [], [f40])).
fof(f40, plain, (? [X3] : (? [X4, X5, X6] : (~ (X5 = X6) & ~ (X4 = X6) & ~ (X4 = X5) & rp(X3, X6) & rp(X3, X5) & rp(X3, X4)) & cc(X3)) | ~ sP0), inference(nnf_transformation, [], [f35])).
fof(f117, plain, (~ spl10_1 | spl10_7), inference(avatar_split_clause, [], [f67, f114, f85])).
fof(f67, plain, (rp(sK3, sK4) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f112, plain, (~ spl10_1 | spl10_6), inference(avatar_split_clause, [], [f68, f109, f85])).
fof(f68, plain, (rp(sK3, sK5) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f107, plain, (~ spl10_1 | spl10_5), inference(avatar_split_clause, [], [f69, f104, f85])).
fof(f69, plain, (rp(sK3, sK6) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f102, plain, (~ spl10_1 | ~ spl10_4), inference(avatar_split_clause, [], [f70, f99, f85])).
fof(f70, plain, (~ (sK4 = sK5) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f97, plain, (~ spl10_1 | ~ spl10_3), inference(avatar_split_clause, [], [f71, f94, f85])).
fof(f71, plain, (~ (sK4 = sK6) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f92, plain, (~ spl10_1 | ~ spl10_2), inference(avatar_split_clause, [], [f72, f89, f85])).
fof(f72, plain, (~ (sK5 = sK6) | ~ sP0), inference(cnf_transformation, [], [f44])).