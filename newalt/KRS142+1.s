fof(f145, plain, $false, inference(avatar_sat_refutation, [], [f86, f91, f96, f101, f112, f116, f124, f128, f132, f143])).
fof(f143, plain, (spl8_2 | ~ spl8_3 | ~ spl8_4 | ~ spl8_5), inference(avatar_contradiction_clause, [], [f142])).
fof(f142, plain, ($false | (spl8_2 | ~ spl8_3 | ~ spl8_4 | ~ spl8_5)), inference(subsumption_resolution, [], [f140, f85])).
fof(f85, plain, (~ (sK3 = sK4) | spl8_2), inference(avatar_component_clause, [], [f83])).
fof(f83, plain, (spl8_2 <=> (sK3 = sK4)), introduced(avatar_definition, [new_symbols(naming, [spl8_2])])).
fof(f140, plain, ((sK3 = sK4) | (~ spl8_3 | ~ spl8_4 | ~ spl8_5)), inference(resolution, [], [f137, f95])).
fof(f95, plain, (rp(sK2, sK3) | ~ spl8_4), inference(avatar_component_clause, [], [f93])).
fof(f93, plain, (spl8_4 <=> rp(sK2, sK3)), introduced(avatar_definition, [new_symbols(naming, [spl8_4])])).
fof(f137, plain, (! [X2] : (~ rp(sK2, X2) | (sK4 = X2)) | (~ spl8_3 | ~ spl8_5)), inference(subsumption_resolution, [], [f134, f100])).
fof(f100, plain, (cc(sK2) | ~ spl8_5), inference(avatar_component_clause, [], [f98])).
fof(f98, plain, (spl8_5 <=> cc(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl8_5])])).
fof(f134, plain, (! [X2] : ((sK4 = X2) | ~ rp(sK2, X2) | ~ cc(sK2)) | ~ spl8_3), inference(resolution, [], [f62, f90])).
fof(f90, plain, (rp(sK2, sK4) | ~ spl8_3), inference(avatar_component_clause, [], [f88])).
fof(f88, plain, (spl8_3 <=> rp(sK2, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl8_3])])).
fof(f62, plain, ! [X2, X0, X1] : (~ rp(X0, X2) | (X1 = X2) | ~ rp(X0, X1) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : ((! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & rp(X0, sK1(X0))) | ~ cc(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f32, f38])).
fof(f38, plain, ! [X0] : (? [X3] : rp(X0, X3) => rp(X0, sK1(X0))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0] : ((! [X1, X2] : ((X1 = X2) | ~ rp(X0, X2) | ~ rp(X0, X1)) & ? [X3] : rp(X0, X3)) | ~ cc(X0)), inference(flattening, [], [f31])).
fof(f31, plain, ! [X0] : ((! [X1, X2] : ((X1 = X2) | (~ rp(X0, X2) | ~ rp(X0, X1))) & ? [X3] : rp(X0, X3)) | ~ cc(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (cc(X0) => (! [X1, X2] : ((rp(X0, X2) & rp(X0, X1)) => (X1 = X2)) & ? [X3] : rp(X0, X3))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X3] : (cc(X3) => (! [X4, X5] : ((rp(X3, X5) & rp(X3, X4)) => (X4 = X5)) & ? [X4] : rp(X3, X4))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS142+1.p', axiom_2)).
fof(f132, plain, (~ spl8_7 | ~ spl8_8), inference(avatar_contradiction_clause, [], [f131])).
fof(f131, plain, ($false | (~ spl8_7 | ~ spl8_8)), inference(subsumption_resolution, [], [f130, f123])).
fof(f123, plain, (cc(sK5) | ~ spl8_8), inference(avatar_component_clause, [], [f121])).
fof(f121, plain, (spl8_8 <=> cc(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl8_8])])).
fof(f130, plain, (~ cc(sK5) | ~ spl8_7), inference(resolution, [], [f61, f111])).
fof(f111, plain, (! [X1] : ~ rp(sK5, X1) | ~ spl8_7), inference(avatar_component_clause, [], [f110])).
fof(f110, plain, (spl8_7 <=> ! [X1] : ~ rp(sK5, X1)), introduced(avatar_definition, [new_symbols(naming, [spl8_7])])).
fof(f61, plain, ! [X0] : (rp(X0, sK1(X0)) | ~ cc(X0)), inference(cnf_transformation, [], [f39])).
fof(f128, plain, (spl8_6 | spl8_8 | spl8_1), inference(avatar_split_clause, [], [f127, f79, f121, f106])).
fof(f106, plain, (spl8_6 <=> xsd_string(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl8_6])])).
fof(f79, plain, (spl8_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl8_1])])).
fof(f127, plain, (sP0 | cc(sK5) | xsd_string(sK6)), inference(subsumption_resolution, [], [f126, f60])).
fof(f60, plain, ! [X0] : (xsd_string(X0) | xsd_integer(X0)), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : ((xsd_string(X0) | xsd_integer(X0)) & (~ xsd_integer(X0) | ~ xsd_string(X0))), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0] : (xsd_string(X0) <=> ~ xsd_integer(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS142+1.p', axiom_1)).
fof(f126, plain, (sP0 | cc(sK5) | ~ xsd_integer(sK6) | xsd_string(sK6)), inference(subsumption_resolution, [], [f125, f57])).
fof(f57, plain, ! [X0] : cowlThing(X0), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (~ cowlNothing(X0) & cowlThing(X0)), inference(rectify, [], [f8])).
fof(f8, plain, ! [X3] : (~ cowlNothing(X3) & cowlThing(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS142+1.p', axiom_0)).
fof(f125, plain, (sP0 | cc(sK5) | ~ xsd_integer(sK6) | xsd_string(sK6) | ~ cowlThing(sK7)), inference(subsumption_resolution, [], [f67, f58])).
fof(f58, plain, ! [X0] : ~ cowlNothing(X0), inference(cnf_transformation, [], [f13])).
fof(f67, plain, (sP0 | cc(sK5) | ~ xsd_integer(sK6) | xsd_string(sK6) | cowlNothing(sK7) | ~ cowlThing(sK7)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (sP0 | (! [X1] : ~ rp(sK5, X1) & cc(sK5)) | ((xsd_integer(sK6) | ~ xsd_string(sK6)) & (~ xsd_integer(sK6) | xsd_string(sK6))) | (cowlNothing(sK7) | ~ cowlThing(sK7))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6, sK7])], [f45, f48, f47, f46])).
fof(f46, plain, (? [X0] : (! [X1] : ~ rp(X0, X1) & cc(X0)) => (! [X1] : ~ rp(sK5, X1) & cc(sK5))), introduced(choice_axiom, [])).
fof(f47, plain, (? [X2] : ((xsd_integer(X2) | ~ xsd_string(X2)) & (~ xsd_integer(X2) | xsd_string(X2))) => ((xsd_integer(sK6) | ~ xsd_string(sK6)) & (~ xsd_integer(sK6) | xsd_string(sK6)))), introduced(choice_axiom, [])).
fof(f48, plain, (? [X3] : (cowlNothing(X3) | ~ cowlThing(X3)) => (cowlNothing(sK7) | ~ cowlThing(sK7))), introduced(choice_axiom, [])).
fof(f45, plain, (sP0 | ? [X0] : (! [X1] : ~ rp(X0, X1) & cc(X0)) | ? [X2] : ((xsd_integer(X2) | ~ xsd_string(X2)) & (~ xsd_integer(X2) | xsd_string(X2))) | ? [X3] : (cowlNothing(X3) | ~ cowlThing(X3))), inference(rectify, [], [f44])).
fof(f44, plain, (sP0 | ? [X3] : (! [X4] : ~ rp(X3, X4) & cc(X3)) | ? [X5] : ((xsd_integer(X5) | ~ xsd_string(X5)) & (~ xsd_integer(X5) | xsd_string(X5))) | ? [X6] : (cowlNothing(X6) | ~ cowlThing(X6))), inference(nnf_transformation, [], [f36])).
fof(f36, plain, (sP0 | ? [X3] : (! [X4] : ~ rp(X3, X4) & cc(X3)) | ? [X5] : ~ (xsd_string(X5) <=> ~ xsd_integer(X5)) | ? [X6] : (cowlNothing(X6) | ~ cowlThing(X6))), inference(definition_folding, [], [f34, e35])).
fof(f35, plain, (? [X0] : (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & cc(X0)) | ~ sP0), inference(usedef, [], [e35])).
fof(e35, plain, (sP0 <=> ? [X0] : (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & cc(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f34, plain, (? [X0] : (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & cc(X0)) | ? [X3] : (! [X4] : ~ rp(X3, X4) & cc(X3)) | ? [X5] : ~ (xsd_string(X5) <=> ~ xsd_integer(X5)) | ? [X6] : (cowlNothing(X6) | ~ cowlThing(X6))), inference(flattening, [], [f33])).
fof(f33, plain, (? [X0] : (? [X1, X2] : (~ (X1 = X2) & (rp(X0, X2) & rp(X0, X1))) & cc(X0)) | ? [X3] : (! [X4] : ~ rp(X3, X4) & cc(X3)) | ? [X5] : ~ (xsd_string(X5) <=> ~ xsd_integer(X5)) | ? [X6] : (cowlNothing(X6) | ~ cowlThing(X6))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ (! [X0] : (cc(X0) => ! [X1, X2] : ((rp(X0, X2) & rp(X0, X1)) => (X1 = X2))) & ! [X3] : (cc(X3) => ? [X4] : rp(X3, X4)) & ! [X5] : (xsd_string(X5) <=> ~ xsd_integer(X5)) & ! [X6] : (~ cowlNothing(X6) & cowlThing(X6))), inference(rectify, [], [f12])).
fof(f12, plain, ~ (! [X3] : (cc(X3) => ! [X4, X5] : ((rp(X3, X5) & rp(X3, X4)) => (X4 = X5))) & ! [X3] : (cc(X3) => ? [X4] : rp(X3, X4)) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), inference(negated_conjecture, [], [f11])).
fof(f11, plain, ~ (! [X3] : (cc(X3) => ! [X4, X5] : ((rp(X3, X5) & rp(X3, X4)) => (X4 = X5))) & ! [X3] : (cc(X3) => ? [X4] : rp(X3, X4)) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS142+1.p', the_axiom)).
fof(f124, plain, (~ spl8_6 | spl8_8 | spl8_1), inference(avatar_split_clause, [], [f119, f79, f121, f106])).
fof(f119, plain, (sP0 | cc(sK5) | ~ xsd_string(sK6)), inference(subsumption_resolution, [], [f118, f59])).
fof(f59, plain, ! [X0] : (~ xsd_string(X0) | ~ xsd_integer(X0)), inference(cnf_transformation, [], [f37])).
fof(f118, plain, (sP0 | cc(sK5) | xsd_integer(sK6) | ~ xsd_string(sK6)), inference(subsumption_resolution, [], [f117, f57])).
fof(f117, plain, (sP0 | cc(sK5) | xsd_integer(sK6) | ~ xsd_string(sK6) | ~ cowlThing(sK7)), inference(subsumption_resolution, [], [f68, f58])).
fof(f68, plain, (sP0 | cc(sK5) | xsd_integer(sK6) | ~ xsd_string(sK6) | cowlNothing(sK7) | ~ cowlThing(sK7)), inference(cnf_transformation, [], [f49])).
fof(f116, plain, (spl8_6 | spl8_7 | spl8_1), inference(avatar_split_clause, [], [f115, f79, f110, f106])).
fof(f115, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | xsd_string(sK6)), inference(subsumption_resolution, [], [f114, f60])).
fof(f114, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | ~ xsd_integer(sK6) | xsd_string(sK6)), inference(subsumption_resolution, [], [f113, f57])).
fof(f113, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | ~ xsd_integer(sK6) | xsd_string(sK6) | ~ cowlThing(sK7)), inference(subsumption_resolution, [], [f69, f58])).
fof(f69, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | ~ xsd_integer(sK6) | xsd_string(sK6) | cowlNothing(sK7) | ~ cowlThing(sK7)), inference(cnf_transformation, [], [f49])).
fof(f112, plain, (~ spl8_6 | spl8_7 | spl8_1), inference(avatar_split_clause, [], [f104, f79, f110, f106])).
fof(f104, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | ~ xsd_string(sK6)), inference(subsumption_resolution, [], [f103, f59])).
fof(f103, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | xsd_integer(sK6) | ~ xsd_string(sK6)), inference(subsumption_resolution, [], [f102, f57])).
fof(f102, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | xsd_integer(sK6) | ~ xsd_string(sK6) | ~ cowlThing(sK7)), inference(subsumption_resolution, [], [f70, f58])).
fof(f70, plain, ! [X1] : (sP0 | ~ rp(sK5, X1) | xsd_integer(sK6) | ~ xsd_string(sK6) | cowlNothing(sK7) | ~ cowlThing(sK7)), inference(cnf_transformation, [], [f49])).
fof(f101, plain, (~ spl8_1 | spl8_5), inference(avatar_split_clause, [], [f63, f98, f79])).
fof(f63, plain, (cc(sK2) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (((~ (sK3 = sK4) & rp(sK2, sK4) & rp(sK2, sK3)) & cc(sK2)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4])], [f40, f42, f41])).
fof(f41, plain, (? [X0] : (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & cc(X0)) => (? [X2, X1] : (~ (X1 = X2) & rp(sK2, X2) & rp(sK2, X1)) & cc(sK2))), introduced(choice_axiom, [])).
fof(f42, plain, (? [X2, X1] : (~ (X1 = X2) & rp(sK2, X2) & rp(sK2, X1)) => (~ (sK3 = sK4) & rp(sK2, sK4) & rp(sK2, sK3))), introduced(choice_axiom, [])).
fof(f40, plain, (? [X0] : (? [X1, X2] : (~ (X1 = X2) & rp(X0, X2) & rp(X0, X1)) & cc(X0)) | ~ sP0), inference(nnf_transformation, [], [f35])).
fof(f96, plain, (~ spl8_1 | spl8_4), inference(avatar_split_clause, [], [f64, f93, f79])).
fof(f64, plain, (rp(sK2, sK3) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f91, plain, (~ spl8_1 | spl8_3), inference(avatar_split_clause, [], [f65, f88, f79])).
fof(f65, plain, (rp(sK2, sK4) | ~ sP0), inference(cnf_transformation, [], [f43])).
fof(f86, plain, (~ spl8_1 | ~ spl8_2), inference(avatar_split_clause, [], [f66, f83, f79])).
fof(f66, plain, (~ (sK3 = sK4) | ~ sP0), inference(cnf_transformation, [], [f43])).