fof(f293, plain, $false, inference(avatar_sat_refutation, [], [f111, f116, f121, f137, f145, f157, f165, f181, f195, f207, f245, f270, f292])).
fof(f292, plain, (spl7_4 | ~ spl7_5 | ~ spl7_13), inference(avatar_contradiction_clause, [], [f291])).
fof(f291, plain, ($false | (spl7_4 | ~ spl7_5 | ~ spl7_13)), inference(subsumption_resolution, [], [f290, f115])).
fof(f115, plain, (~ (ia = sK1) | spl7_4), inference(avatar_component_clause, [], [f113])).
fof(f113, plain, (spl7_4 <=> (ia = sK1)), introduced(avatar_definition, [new_symbols(naming, [spl7_4])])).
fof(f290, plain, ((ia = sK1) | (~ spl7_5 | ~ spl7_13)), inference(resolution, [], [f187, f276])).
fof(f276, plain, (requalityOnA(sK1, ia) | (~ spl7_5 | ~ spl7_13)), inference(backward_demodulation, [], [f120, f240])).
fof(f240, plain, ((ia = sK2) | ~ spl7_13), inference(avatar_component_clause, [], [f238])).
fof(f238, plain, (spl7_13 <=> (ia = sK2)), introduced(avatar_definition, [new_symbols(naming, [spl7_13])])).
fof(f120, plain, (requalityOnA(sK1, sK2) | ~ spl7_5), inference(avatar_component_clause, [], [f118])).
fof(f118, plain, (spl7_5 <=> requalityOnA(sK1, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl7_5])])).
fof(f187, plain, ! [X1] : (~ requalityOnA(X1, ia) | (ia = X1)), inference(resolution, [], [f74, f77])).
fof(f77, plain, requalityOnA(ia, ia), inference(cnf_transformation, [], [f14])).
fof(f14, plain, requalityOnA(ia, ia), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_6)).
fof(f74, plain, ! [X2, X0, X1] : (~ requalityOnA(X2, X0) | (X1 = X2) | ~ requalityOnA(X1, X0)), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ requalityOnA(X2, X0) | ~ requalityOnA(X1, X0)), inference(flattening, [], [f40])).
fof(f40, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ requalityOnA(X2, X0) | ~ requalityOnA(X1, X0))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0, X1, X2] : ((requalityOnA(X2, X0) & requalityOnA(X1, X0)) => (X1 = X2)), inference(rectify, [], [f11])).
fof(f11, plain, ! [X3, X4, X5] : ((requalityOnA(X5, X3) & requalityOnA(X4, X3)) => (X4 = X5)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_3)).
fof(f270, plain, (spl7_3 | ~ spl7_5 | ~ spl7_14), inference(avatar_contradiction_clause, [], [f269])).
fof(f269, plain, ($false | (spl7_3 | ~ spl7_5 | ~ spl7_14)), inference(subsumption_resolution, [], [f268, f110])).
fof(f110, plain, (~ (ib = sK1) | spl7_3), inference(avatar_component_clause, [], [f108])).
fof(f108, plain, (spl7_3 <=> (ib = sK1)), introduced(avatar_definition, [new_symbols(naming, [spl7_3])])).
fof(f268, plain, ((ib = sK1) | (~ spl7_5 | ~ spl7_14)), inference(resolution, [], [f186, f248])).
fof(f248, plain, (requalityOnA(sK1, ib) | (~ spl7_5 | ~ spl7_14)), inference(backward_demodulation, [], [f120, f244])).
fof(f244, plain, ((ib = sK2) | ~ spl7_14), inference(avatar_component_clause, [], [f242])).
fof(f242, plain, (spl7_14 <=> (ib = sK2)), introduced(avatar_definition, [new_symbols(naming, [spl7_14])])).
fof(f186, plain, ! [X0] : (~ requalityOnA(X0, ib) | (ib = X0)), inference(resolution, [], [f74, f79])).
fof(f79, plain, requalityOnA(ib, ib), inference(cnf_transformation, [], [f16])).
fof(f16, plain, requalityOnA(ib, ib), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_8)).
fof(f245, plain, (spl7_13 | spl7_14 | ~ spl7_5), inference(avatar_split_clause, [], [f236, f118, f242, f238])).
fof(f236, plain, ((ib = sK2) | (ia = sK2) | ~ spl7_5), inference(resolution, [], [f235, f71])).
fof(f71, plain, ! [X0] : (~ cA(X0) | (ib = X0) | (ia = X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : ((cA(X0) | (~ (ia = X0) & ~ (ib = X0))) & ((ia = X0) | (ib = X0) | ~ cA(X0))), inference(flattening, [], [f48])).
fof(f48, plain, ! [X0] : ((cA(X0) | (~ (ia = X0) & ~ (ib = X0))) & (((ia = X0) | (ib = X0)) | ~ cA(X0))), inference(nnf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (cA(X0) <=> ((ia = X0) | (ib = X0))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X3] : (cA(X3) <=> ((ia = X3) | (ib = X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_2)).
fof(f235, plain, (cA(sK2) | ~ spl7_5), inference(resolution, [], [f120, f75])).
fof(f75, plain, ! [X0, X1] : (~ requalityOnA(X0, X1) | cA(X1)), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ! [X0, X1] : (cA(X1) | ~ requalityOnA(X0, X1)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : (requalityOnA(X0, X1) => cA(X1)), inference(rectify, [], [f12])).
fof(f12, plain, ! [X3, X4] : (requalityOnA(X3, X4) => cA(X4)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_4)).
fof(f207, plain, (spl7_7 | ~ spl7_8 | ~ spl7_9), inference(avatar_contradiction_clause, [], [f206])).
fof(f206, plain, ($false | (spl7_7 | ~ spl7_8 | ~ spl7_9)), inference(subsumption_resolution, [], [f205, f77])).
fof(f205, plain, (~ requalityOnA(ia, ia) | (spl7_7 | ~ spl7_8 | ~ spl7_9)), inference(backward_demodulation, [], [f197, f201])).
fof(f201, plain, ((ia = sK3) | (~ spl7_8 | ~ spl7_9)), inference(resolution, [], [f199, f77])).
fof(f199, plain, (! [X0] : (~ requalityOnA(X0, ia) | (sK3 = X0)) | (~ spl7_8 | ~ spl7_9)), inference(resolution, [], [f198, f74])).
fof(f198, plain, (requalityOnA(sK3, ia) | (~ spl7_8 | ~ spl7_9)), inference(backward_demodulation, [], [f156, f176])).
fof(f176, plain, ((ia = sK4) | ~ spl7_9), inference(avatar_component_clause, [], [f174])).
fof(f174, plain, (spl7_9 <=> (ia = sK4)), introduced(avatar_definition, [new_symbols(naming, [spl7_9])])).
fof(f156, plain, (requalityOnA(sK3, sK4) | ~ spl7_8), inference(avatar_component_clause, [], [f154])).
fof(f154, plain, (spl7_8 <=> requalityOnA(sK3, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl7_8])])).
fof(f197, plain, (~ requalityOnA(ia, sK3) | (spl7_7 | ~ spl7_9)), inference(backward_demodulation, [], [f136, f176])).
fof(f136, plain, (~ requalityOnA(sK4, sK3) | spl7_7), inference(avatar_component_clause, [], [f134])).
fof(f134, plain, (spl7_7 <=> requalityOnA(sK4, sK3)), introduced(avatar_definition, [new_symbols(naming, [spl7_7])])).
fof(f195, plain, (spl7_7 | ~ spl7_8 | ~ spl7_10), inference(avatar_contradiction_clause, [], [f194])).
fof(f194, plain, ($false | (spl7_7 | ~ spl7_8 | ~ spl7_10)), inference(subsumption_resolution, [], [f193, f79])).
fof(f193, plain, (~ requalityOnA(ib, ib) | (spl7_7 | ~ spl7_8 | ~ spl7_10)), inference(backward_demodulation, [], [f183, f189])).
fof(f189, plain, ((ib = sK3) | (~ spl7_8 | ~ spl7_10)), inference(resolution, [], [f188, f79])).
fof(f188, plain, (! [X2] : (~ requalityOnA(X2, ib) | (sK3 = X2)) | (~ spl7_8 | ~ spl7_10)), inference(resolution, [], [f74, f184])).
fof(f184, plain, (requalityOnA(sK3, ib) | (~ spl7_8 | ~ spl7_10)), inference(backward_demodulation, [], [f156, f180])).
fof(f180, plain, ((ib = sK4) | ~ spl7_10), inference(avatar_component_clause, [], [f178])).
fof(f178, plain, (spl7_10 <=> (ib = sK4)), introduced(avatar_definition, [new_symbols(naming, [spl7_10])])).
fof(f183, plain, (~ requalityOnA(ib, sK3) | (spl7_7 | ~ spl7_10)), inference(backward_demodulation, [], [f136, f180])).
fof(f181, plain, (spl7_9 | spl7_10 | ~ spl7_8), inference(avatar_split_clause, [], [f172, f154, f178, f174])).
fof(f172, plain, ((ib = sK4) | (ia = sK4) | ~ spl7_8), inference(resolution, [], [f71, f167])).
fof(f167, plain, (cA(sK4) | ~ spl7_8), inference(resolution, [], [f75, f156])).
fof(f165, plain, (spl7_6 | spl7_1 | spl7_8), inference(avatar_split_clause, [], [f164, f154, f99, f130])).
fof(f130, plain, (spl7_6 <=> xsd_string(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl7_6])])).
fof(f99, plain, (spl7_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl7_1])])).
fof(f164, plain, (requalityOnA(sK3, sK4) | sP0 | xsd_string(sK5)), inference(subsumption_resolution, [], [f163, f70])).
fof(f70, plain, ! [X0] : (xsd_string(X0) | xsd_integer(X0)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ! [X0] : ((xsd_string(X0) | xsd_integer(X0)) & (~ xsd_integer(X0) | ~ xsd_string(X0))), inference(nnf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (xsd_string(X0) <=> ~ xsd_integer(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_1)).
fof(f163, plain, (requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5)), inference(subsumption_resolution, [], [f162, f67])).
fof(f67, plain, ! [X0] : cowlThing(X0), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (~ cowlNothing(X0) & cowlThing(X0)), inference(rectify, [], [f8])).
fof(f8, plain, ! [X3] : (~ cowlNothing(X3) & cowlThing(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', axiom_0)).
fof(f162, plain, (requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f161, f68])).
fof(f68, plain, ! [X0] : ~ cowlNothing(X0), inference(cnf_transformation, [], [f20])).
fof(f161, plain, (requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f160, f67])).
fof(f160, plain, (~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f159, f77])).
fof(f159, plain, (~ requalityOnA(ia, ia) | ~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f158, f67])).
fof(f158, plain, (~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f85, f67])).
fof(f85, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | (~ requalityOnA(sK4, sK3) & requalityOnA(sK3, sK4)) | sP0 | ((xsd_integer(sK5) | ~ xsd_string(sK5)) & (~ xsd_integer(sK5) | xsd_string(sK5))) | (cowlNothing(sK6) | ~ cowlThing(sK6))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3, sK4, sK5, sK6])], [f55, f58, f57, f56])).
fof(f56, plain, (? [X0, X1] : (~ requalityOnA(X1, X0) & requalityOnA(X0, X1)) => (~ requalityOnA(sK4, sK3) & requalityOnA(sK3, sK4))), introduced(choice_axiom, [])).
fof(f57, plain, (? [X2] : ((xsd_integer(X2) | ~ xsd_string(X2)) & (~ xsd_integer(X2) | xsd_string(X2))) => ((xsd_integer(sK5) | ~ xsd_string(sK5)) & (~ xsd_integer(sK5) | xsd_string(sK5)))), introduced(choice_axiom, [])).
fof(f58, plain, (? [X3] : (cowlNothing(X3) | ~ cowlThing(X3)) => (cowlNothing(sK6) | ~ cowlThing(sK6))), introduced(choice_axiom, [])).
fof(f55, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ? [X0, X1] : (~ requalityOnA(X1, X0) & requalityOnA(X0, X1)) | sP0 | ? [X2] : ((xsd_integer(X2) | ~ xsd_string(X2)) & (~ xsd_integer(X2) | xsd_string(X2))) | ? [X3] : (cowlNothing(X3) | ~ cowlThing(X3))), inference(rectify, [], [f54])).
fof(f54, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ? [X0, X1] : (~ requalityOnA(X1, X0) & requalityOnA(X0, X1)) | sP0 | ? [X4] : ((xsd_integer(X4) | ~ xsd_string(X4)) & (~ xsd_integer(X4) | xsd_string(X4))) | ? [X5] : (cowlNothing(X5) | ~ cowlThing(X5))), inference(nnf_transformation, [], [f46])).
fof(f46, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ? [X0, X1] : (~ requalityOnA(X1, X0) & requalityOnA(X0, X1)) | sP0 | ? [X4] : ~ (xsd_string(X4) <=> ~ xsd_integer(X4)) | ? [X5] : (cowlNothing(X5) | ~ cowlThing(X5))), inference(definition_folding, [], [f44, e45])).
fof(f45, plain, (? [X2, X3] : (~ (ic = X2) & ~ (ib = X2) & ~ (ia = X2) & requalityOnA(X2, X3)) | ~ sP0), inference(usedef, [], [e45])).
fof(e45, plain, (sP0 <=> ? [X2, X3] : (~ (ic = X2) & ~ (ib = X2) & ~ (ia = X2) & requalityOnA(X2, X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f44, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ? [X0, X1] : (~ requalityOnA(X1, X0) & requalityOnA(X0, X1)) | ? [X2, X3] : (~ (ic = X2) & ~ (ib = X2) & ~ (ia = X2) & requalityOnA(X2, X3)) | ? [X4] : ~ (xsd_string(X4) <=> ~ xsd_integer(X4)) | ? [X5] : (cowlNothing(X5) | ~ cowlThing(X5))), inference(flattening, [], [f43])).
fof(f43, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ? [X0, X1] : (~ requalityOnA(X1, X0) & requalityOnA(X0, X1)) | ? [X2, X3] : ((~ (ic = X2) & ~ (ib = X2) & ~ (ia = X2)) & requalityOnA(X2, X3)) | ? [X4] : ~ (xsd_string(X4) <=> ~ xsd_integer(X4)) | ? [X5] : (cowlNothing(X5) | ~ cowlThing(X5))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ (cowlThing(ic) & cowlThing(ib) & requalityOnA(ia, ia) & cowlThing(ia) & ! [X0, X1] : (requalityOnA(X0, X1) => requalityOnA(X1, X0)) & ! [X2, X3] : (requalityOnA(X2, X3) => ((ic = X2) | (ib = X2) | (ia = X2))) & ! [X4] : (xsd_string(X4) <=> ~ xsd_integer(X4)) & ! [X5] : (~ cowlNothing(X5) & cowlThing(X5))), inference(rectify, [], [f19])).
fof(f19, plain, ~ (cowlThing(ic) & cowlThing(ib) & requalityOnA(ia, ia) & cowlThing(ia) & ! [X3, X4] : (requalityOnA(X3, X4) => requalityOnA(X4, X3)) & ! [X3, X4] : (requalityOnA(X3, X4) => ((ic = X3) | (ib = X3) | (ia = X3))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ (cowlThing(ic) & cowlThing(ib) & requalityOnA(ia, ia) & cowlThing(ia) & ! [X3, X4] : (requalityOnA(X3, X4) => requalityOnA(X4, X3)) & ! [X3, X4] : (requalityOnA(X3, X4) => ((ic = X3) | (ib = X3) | (ia = X3))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS138+1.p', the_axiom)).
fof(f157, plain, (~ spl7_6 | spl7_1 | spl7_8), inference(avatar_split_clause, [], [f152, f154, f99, f130])).
fof(f152, plain, (requalityOnA(sK3, sK4) | sP0 | ~ xsd_string(sK5)), inference(subsumption_resolution, [], [f151, f69])).
fof(f69, plain, ! [X0] : (~ xsd_string(X0) | ~ xsd_integer(X0)), inference(cnf_transformation, [], [f47])).
fof(f151, plain, (requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5)), inference(subsumption_resolution, [], [f150, f67])).
fof(f150, plain, (requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f149, f68])).
fof(f149, plain, (requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f148, f67])).
fof(f148, plain, (~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f147, f77])).
fof(f147, plain, (~ requalityOnA(ia, ia) | ~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f146, f67])).
fof(f146, plain, (~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f86, f67])).
fof(f86, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | requalityOnA(sK3, sK4) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(cnf_transformation, [], [f59])).
fof(f145, plain, (spl7_6 | spl7_1 | ~ spl7_7), inference(avatar_split_clause, [], [f144, f134, f99, f130])).
fof(f144, plain, (~ requalityOnA(sK4, sK3) | sP0 | xsd_string(sK5)), inference(subsumption_resolution, [], [f143, f70])).
fof(f143, plain, (~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5)), inference(subsumption_resolution, [], [f142, f67])).
fof(f142, plain, (~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f141, f68])).
fof(f141, plain, (~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f140, f67])).
fof(f140, plain, (~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f139, f77])).
fof(f139, plain, (~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f138, f67])).
fof(f138, plain, (~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f87, f67])).
fof(f87, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_integer(sK5) | xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(cnf_transformation, [], [f59])).
fof(f137, plain, (~ spl7_6 | spl7_1 | ~ spl7_7), inference(avatar_split_clause, [], [f128, f134, f99, f130])).
fof(f128, plain, (~ requalityOnA(sK4, sK3) | sP0 | ~ xsd_string(sK5)), inference(subsumption_resolution, [], [f127, f69])).
fof(f127, plain, (~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5)), inference(subsumption_resolution, [], [f126, f67])).
fof(f126, plain, (~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f125, f68])).
fof(f125, plain, (~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f124, f67])).
fof(f124, plain, (~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f123, f77])).
fof(f123, plain, (~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f122, f67])).
fof(f122, plain, (~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(subsumption_resolution, [], [f88, f67])).
fof(f88, plain, (~ cowlThing(ic) | ~ cowlThing(ib) | ~ requalityOnA(ia, ia) | ~ cowlThing(ia) | ~ requalityOnA(sK4, sK3) | sP0 | xsd_integer(sK5) | ~ xsd_string(sK5) | cowlNothing(sK6) | ~ cowlThing(sK6)), inference(cnf_transformation, [], [f59])).
fof(f121, plain, (~ spl7_1 | spl7_5), inference(avatar_split_clause, [], [f81, f118, f99])).
fof(f81, plain, (requalityOnA(sK1, sK2) | ~ sP0), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (ic = sK1) & ~ (ib = sK1) & ~ (ia = sK1) & requalityOnA(sK1, sK2)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f51, f52])).
fof(f52, plain, (? [X0, X1] : (~ (ic = X0) & ~ (ib = X0) & ~ (ia = X0) & requalityOnA(X0, X1)) => (~ (ic = sK1) & ~ (ib = sK1) & ~ (ia = sK1) & requalityOnA(sK1, sK2))), introduced(choice_axiom, [])).
fof(f51, plain, (? [X0, X1] : (~ (ic = X0) & ~ (ib = X0) & ~ (ia = X0) & requalityOnA(X0, X1)) | ~ sP0), inference(rectify, [], [f50])).
fof(f50, plain, (? [X2, X3] : (~ (ic = X2) & ~ (ib = X2) & ~ (ia = X2) & requalityOnA(X2, X3)) | ~ sP0), inference(nnf_transformation, [], [f45])).
fof(f116, plain, (~ spl7_1 | ~ spl7_4), inference(avatar_split_clause, [], [f82, f113, f99])).
fof(f82, plain, (~ (ia = sK1) | ~ sP0), inference(cnf_transformation, [], [f53])).
fof(f111, plain, (~ spl7_1 | ~ spl7_3), inference(avatar_split_clause, [], [f83, f108, f99])).
fof(f83, plain, (~ (ib = sK1) | ~ sP0), inference(cnf_transformation, [], [f53])).