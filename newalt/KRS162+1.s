fof(f552, plain, $false, inference(avatar_sat_refutation, [], [f131, f136, f141, f146, f151, f159, f163, f262, f288, f317, f346, f374, f403, f433, f456, f489, f551])).
fof(f551, plain, (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_12 | spl10_14 | spl10_16 | spl10_18 | spl10_20 | spl10_22 | spl10_24), inference(avatar_contradiction_clause, [], [f550])).
fof(f550, plain, ($false | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_12 | spl10_14 | spl10_16 | spl10_18 | spl10_20 | spl10_22 | spl10_24)), inference(subsumption_resolution, [], [f549, f135])).
fof(f135, plain, (sP0(sK2) | ~ spl10_3), inference(avatar_component_clause, [], [f133])).
fof(f133, plain, (spl10_3 <=> sP0(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl10_3])])).
fof(f549, plain, (~ sP0(sK2) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_12 | spl10_14 | spl10_16 | spl10_18 | spl10_20 | spl10_22 | spl10_24)), inference(subsumption_resolution, [], [f548, f214])).
fof(f214, plain, (~ (sK4 = sK7(sK2)) | spl10_12), inference(avatar_component_clause, [], [f213])).
fof(f213, plain, (spl10_12 <=> (sK4 = sK7(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_12])])).
fof(f548, plain, ((sK4 = sK7(sK2)) | ~ sP0(sK2) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_14 | spl10_16 | spl10_18 | spl10_20 | spl10_22 | spl10_24)), inference(subsumption_resolution, [], [f547, f326])).
fof(f326, plain, (~ (sK3 = sK7(sK2)) | spl10_18), inference(avatar_component_clause, [], [f325])).
fof(f325, plain, (spl10_18 <=> (sK3 = sK7(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_18])])).
fof(f547, plain, ((sK3 = sK7(sK2)) | (sK4 = sK7(sK2)) | ~ sP0(sK2) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_14 | spl10_16 | spl10_20 | spl10_22 | spl10_24)), inference(subsumption_resolution, [], [f546, f459])).
fof(f459, plain, (~ (sK6(sK2) = sK7(sK2)) | spl10_24), inference(avatar_component_clause, [], [f458])).
fof(f458, plain, (spl10_24 <=> (sK6(sK2) = sK7(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_24])])).
fof(f546, plain, ((sK6(sK2) = sK7(sK2)) | (sK3 = sK7(sK2)) | (sK4 = sK7(sK2)) | ~ sP0(sK2) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_14 | spl10_16 | spl10_20 | spl10_22)), inference(subsumption_resolution, [], [f545, f408])).
fof(f408, plain, (~ (sK5(sK2) = sK7(sK2)) | spl10_22), inference(avatar_component_clause, [], [f407])).
fof(f407, plain, (spl10_22 <=> (sK5(sK2) = sK7(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_22])])).
fof(f545, plain, ((sK5(sK2) = sK7(sK2)) | (sK6(sK2) = sK7(sK2)) | (sK3 = sK7(sK2)) | (sK4 = sK7(sK2)) | ~ sP0(sK2) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_14 | spl10_16 | spl10_20)), inference(resolution, [], [f523, f174])).
fof(f174, plain, ! [X0] : (rr(X0, sK7(X0)) | ~ sP0(X0)), inference(resolution, [], [f106, f97])).
fof(f97, plain, ! [X0, X1] : (~ rq(X0, X1) | rr(X0, X1)), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : (rr(X0, X1) | ~ rq(X0, X1)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0, X1] : (rq(X0, X1) => rr(X0, X1)), inference(rectify, [], [f18])).
fof(f18, plain, ! [X3, X4] : (rq(X3, X4) => rr(X3, X4)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_5)).
fof(f106, plain, ! [X0] : (rq(X0, sK7(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ! [X0] : ((~ (sK6(X0) = sK7(X0)) & ~ (sK5(X0) = sK7(X0)) & ~ (sK5(X0) = sK6(X0)) & rq(X0, sK7(X0)) & rq(X0, sK6(X0)) & rq(X0, sK5(X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6, sK7])], [f70, f71])).
fof(f71, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1)) => (~ (sK6(X0) = sK7(X0)) & ~ (sK5(X0) = sK7(X0)) & ~ (sK5(X0) = sK6(X0)) & rq(X0, sK7(X0)) & rq(X0, sK6(X0)) & rq(X0, sK5(X0)))), introduced(choice_axiom, [])).
fof(f70, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1)) | ~ sP0(X0)), inference(nnf_transformation, [], [f61])).
fof(f61, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1)) | ~ sP0(X0)), inference(usedef, [], [e61])).
fof(e61, plain, ! [X0] : (sP0(X0) <=> ? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f523, plain, (! [X3] : (~ rr(sK2, X3) | (sK5(sK2) = X3) | (sK6(sK2) = X3) | (sK3 = X3) | (sK4 = X3)) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_14 | spl10_16 | spl10_20)), inference(subsumption_resolution, [], [f522, f135])).
fof(f522, plain, (! [X3] : (~ rr(sK2, X3) | (sK5(sK2) = X3) | (sK6(sK2) = X3) | (sK3 = X3) | (sK4 = X3) | ~ sP0(sK2)) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_10 | spl10_14 | spl10_16 | spl10_20)), inference(subsumption_resolution, [], [f521, f205])).
fof(f205, plain, (~ (sK4 = sK6(sK2)) | spl10_10), inference(avatar_component_clause, [], [f204])).
fof(f204, plain, (spl10_10 <=> (sK4 = sK6(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_10])])).
fof(f521, plain, (! [X3] : (~ rr(sK2, X3) | (sK5(sK2) = X3) | (sK6(sK2) = X3) | (sK4 = sK6(sK2)) | (sK3 = X3) | (sK4 = X3) | ~ sP0(sK2)) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_14 | spl10_16 | spl10_20)), inference(subsumption_resolution, [], [f520, f295])).
fof(f295, plain, (~ (sK3 = sK6(sK2)) | spl10_16), inference(avatar_component_clause, [], [f294])).
fof(f294, plain, (spl10_16 <=> (sK3 = sK6(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_16])])).
fof(f520, plain, (! [X3] : (~ rr(sK2, X3) | (sK5(sK2) = X3) | (sK6(sK2) = X3) | (sK3 = sK6(sK2)) | (sK4 = sK6(sK2)) | (sK3 = X3) | (sK4 = X3) | ~ sP0(sK2)) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_14 | spl10_20)), inference(subsumption_resolution, [], [f515, f379])).
fof(f379, plain, (~ (sK5(sK2) = sK6(sK2)) | spl10_20), inference(avatar_component_clause, [], [f378])).
fof(f378, plain, (spl10_20 <=> (sK5(sK2) = sK6(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_20])])).
fof(f515, plain, (! [X3] : (~ rr(sK2, X3) | (sK5(sK2) = sK6(sK2)) | (sK5(sK2) = X3) | (sK6(sK2) = X3) | (sK3 = sK6(sK2)) | (sK4 = sK6(sK2)) | (sK3 = X3) | (sK4 = X3) | ~ sP0(sK2)) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_14)), inference(resolution, [], [f505, f171])).
fof(f171, plain, ! [X0] : (rr(X0, sK6(X0)) | ~ sP0(X0)), inference(resolution, [], [f105, f97])).
fof(f105, plain, ! [X0] : (rq(X0, sK6(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f72])).
fof(f505, plain, (! [X4, X5] : (~ rr(sK2, X5) | ~ rr(sK2, X4) | (sK5(sK2) = X5) | (sK5(sK2) = X4) | (X4 = X5) | (sK3 = X5) | (sK4 = X5) | (sK3 = X4) | (sK4 = X4)) | (~ spl10_2 | ~ spl10_3 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_14)), inference(subsumption_resolution, [], [f504, f135])).
fof(f504, plain, (! [X4, X5] : (~ rr(sK2, X4) | ~ rr(sK2, X5) | (sK5(sK2) = X5) | (sK5(sK2) = X4) | (X4 = X5) | (sK3 = X5) | (sK4 = X5) | (sK3 = X4) | (sK4 = X4) | ~ sP0(sK2)) | (~ spl10_2 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_8 | spl10_14)), inference(subsumption_resolution, [], [f503, f196])).
fof(f196, plain, (~ (sK4 = sK5(sK2)) | spl10_8), inference(avatar_component_clause, [], [f195])).
fof(f195, plain, (spl10_8 <=> (sK4 = sK5(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_8])])).
fof(f503, plain, (! [X4, X5] : (~ rr(sK2, X4) | ~ rr(sK2, X5) | (sK5(sK2) = X5) | (sK5(sK2) = X4) | (sK4 = sK5(sK2)) | (X4 = X5) | (sK3 = X5) | (sK4 = X5) | (sK3 = X4) | (sK4 = X4) | ~ sP0(sK2)) | (~ spl10_2 | spl10_4 | ~ spl10_5 | ~ spl10_6 | spl10_14)), inference(subsumption_resolution, [], [f498, f267])).
fof(f267, plain, (~ (sK3 = sK5(sK2)) | spl10_14), inference(avatar_component_clause, [], [f266])).
fof(f266, plain, (spl10_14 <=> (sK3 = sK5(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl10_14])])).
fof(f498, plain, (! [X4, X5] : (~ rr(sK2, X4) | ~ rr(sK2, X5) | (sK5(sK2) = X5) | (sK5(sK2) = X4) | (sK3 = sK5(sK2)) | (sK4 = sK5(sK2)) | (X4 = X5) | (sK3 = X5) | (sK4 = X5) | (sK3 = X4) | (sK4 = X4) | ~ sP0(sK2)) | (~ spl10_2 | spl10_4 | ~ spl10_5 | ~ spl10_6)), inference(resolution, [], [f192, f169])).
fof(f169, plain, ! [X0] : (rr(X0, sK5(X0)) | ~ sP0(X0)), inference(resolution, [], [f104, f97])).
fof(f104, plain, ! [X0] : (rq(X0, sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f72])).
fof(f192, plain, (! [X4, X5, X3] : (~ rr(sK2, X5) | ~ rr(sK2, X4) | ~ rr(sK2, X3) | (X3 = X5) | (X4 = X5) | (sK3 = X5) | (sK4 = X5) | (X3 = X4) | (sK3 = X3) | (sK4 = X3) | (sK3 = X4) | (sK4 = X4)) | (~ spl10_2 | spl10_4 | ~ spl10_5 | ~ spl10_6)), inference(subsumption_resolution, [], [f187, f140])).
fof(f140, plain, (~ (sK3 = sK4) | spl10_4), inference(avatar_component_clause, [], [f138])).
fof(f138, plain, (spl10_4 <=> (sK3 = sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_4])])).
fof(f187, plain, (! [X4, X5, X3] : (~ rr(sK2, X3) | ~ rr(sK2, X4) | ~ rr(sK2, X5) | (sK3 = sK4) | (X3 = X5) | (X4 = X5) | (sK3 = X5) | (sK4 = X5) | (X3 = X4) | (sK3 = X3) | (sK4 = X3) | (sK3 = X4) | (sK4 = X4)) | (~ spl10_2 | ~ spl10_5 | ~ spl10_6)), inference(resolution, [], [f178, f168])).
fof(f168, plain, (rr(sK2, sK3) | ~ spl10_6), inference(resolution, [], [f98, f150])).
fof(f150, plain, (rp(sK2, sK3) | ~ spl10_6), inference(avatar_component_clause, [], [f148])).
fof(f148, plain, (spl10_6 <=> rp(sK2, sK3)), introduced(avatar_definition, [new_symbols(naming, [spl10_6])])).
fof(f98, plain, ! [X0, X1] : (~ rp(X0, X1) | rr(X0, X1)), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ! [X0, X1] : (rr(X0, X1) | ~ rp(X0, X1)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0, X1] : (rp(X0, X1) => rr(X0, X1)), inference(rectify, [], [f19])).
fof(f19, plain, ! [X3, X4] : (rp(X3, X4) => rr(X3, X4)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_6)).
fof(f178, plain, (! [X2, X0, X3, X1] : (~ rr(sK2, X3) | ~ rr(sK2, X1) | ~ rr(sK2, X2) | ~ rr(sK2, X0) | (sK4 = X3) | (X0 = X1) | (X0 = X2) | (X0 = X3) | (sK4 = X0) | (X1 = X2) | (X1 = X3) | (sK4 = X1) | (X2 = X3) | (sK4 = X2)) | (~ spl10_2 | ~ spl10_5)), inference(resolution, [], [f130, f167])).
fof(f167, plain, (rr(sK2, sK4) | ~ spl10_5), inference(resolution, [], [f98, f145])).
fof(f145, plain, (rp(sK2, sK4) | ~ spl10_5), inference(avatar_component_clause, [], [f143])).
fof(f143, plain, (spl10_5 <=> rp(sK2, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_5])])).
fof(f130, plain, (! [X4, X2, X5, X3, X1] : (~ rr(sK2, X5) | ~ rr(sK2, X1) | ~ rr(sK2, X2) | ~ rr(sK2, X3) | ~ rr(sK2, X4) | (X4 = X5) | (X1 = X2) | (X1 = X3) | (X1 = X4) | (X1 = X5) | (X2 = X3) | (X2 = X4) | (X2 = X5) | (X3 = X4) | (X3 = X5)) | ~ spl10_2), inference(avatar_component_clause, [], [f129])).
fof(f129, plain, (spl10_2 <=> ! [X1, X3, X5, X2, X4] : ((X4 = X5) | ~ rr(sK2, X1) | ~ rr(sK2, X2) | ~ rr(sK2, X3) | ~ rr(sK2, X4) | ~ rr(sK2, X5) | (X1 = X2) | (X1 = X3) | (X1 = X4) | (X1 = X5) | (X2 = X3) | (X2 = X4) | (X2 = X5) | (X3 = X4) | (X3 = X5))), introduced(avatar_definition, [new_symbols(naming, [spl10_2])])).
fof(f489, plain, (~ spl10_3 | ~ spl10_24), inference(avatar_contradiction_clause, [], [f488])).
fof(f488, plain, ($false | (~ spl10_3 | ~ spl10_24)), inference(subsumption_resolution, [], [f487, f135])).
fof(f487, plain, (~ sP0(sK2) | ~ spl10_24), inference(trivial_inequality_removal, [], [f483])).
fof(f483, plain, (~ (sK6(sK2) = sK6(sK2)) | ~ sP0(sK2) | ~ spl10_24), inference(superposition, [], [f109, f460])).
fof(f460, plain, ((sK6(sK2) = sK7(sK2)) | ~ spl10_24), inference(avatar_component_clause, [], [f458])).
fof(f109, plain, ! [X0] : (~ (sK6(X0) = sK7(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f72])).
fof(f456, plain, (~ spl10_3 | ~ spl10_22), inference(avatar_contradiction_clause, [], [f455])).
fof(f455, plain, ($false | (~ spl10_3 | ~ spl10_22)), inference(subsumption_resolution, [], [f454, f135])).
fof(f454, plain, (~ sP0(sK2) | ~ spl10_22), inference(trivial_inequality_removal, [], [f451])).
fof(f451, plain, (~ (sK5(sK2) = sK5(sK2)) | ~ sP0(sK2) | ~ spl10_22), inference(superposition, [], [f108, f409])).
fof(f409, plain, ((sK5(sK2) = sK7(sK2)) | ~ spl10_22), inference(avatar_component_clause, [], [f407])).
fof(f108, plain, ! [X0] : (~ (sK5(X0) = sK7(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f72])).
fof(f433, plain, (~ spl10_3 | ~ spl10_20), inference(avatar_contradiction_clause, [], [f432])).
fof(f432, plain, ($false | (~ spl10_3 | ~ spl10_20)), inference(subsumption_resolution, [], [f431, f135])).
fof(f431, plain, (~ sP0(sK2) | ~ spl10_20), inference(trivial_inequality_removal, [], [f428])).
fof(f428, plain, (~ (sK5(sK2) = sK5(sK2)) | ~ sP0(sK2) | ~ spl10_20), inference(superposition, [], [f107, f380])).
fof(f380, plain, ((sK5(sK2) = sK6(sK2)) | ~ spl10_20), inference(avatar_component_clause, [], [f378])).
fof(f107, plain, ! [X0] : (~ (sK5(X0) = sK6(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f72])).
fof(f403, plain, (~ spl10_3 | ~ spl10_6 | ~ spl10_18), inference(avatar_contradiction_clause, [], [f402])).
fof(f402, plain, ($false | (~ spl10_3 | ~ spl10_6 | ~ spl10_18)), inference(subsumption_resolution, [], [f401, f135])).
fof(f401, plain, (~ sP0(sK2) | (~ spl10_6 | ~ spl10_18)), inference(subsumption_resolution, [], [f396, f166])).
fof(f166, plain, (cA(sK3) | ~ spl10_6), inference(resolution, [], [f94, f150])).
fof(f94, plain, ! [X0, X1] : (~ rp(X0, X1) | cA(X1)), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ! [X0, X1] : (cA(X1) | ~ rp(X0, X1)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : (rp(X0, X1) => cA(X1)), inference(rectify, [], [f15])).
fof(f15, plain, ! [X3, X4] : (rp(X3, X4) => cA(X4)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_2)).
fof(f396, plain, (~ cA(sK3) | ~ sP0(sK2) | ~ spl10_18), inference(superposition, [], [f177, f327])).
fof(f327, plain, ((sK3 = sK7(sK2)) | ~ spl10_18), inference(avatar_component_clause, [], [f325])).
fof(f177, plain, ! [X0] : (~ cA(sK7(X0)) | ~ sP0(X0)), inference(resolution, [], [f175, f96])).
fof(f96, plain, ! [X0] : (~ cB(X0) | ~ cA(X0)), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (~ cA(X0) | ~ cB(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : ~ (cA(X0) & cB(X0)), inference(rectify, [], [f17])).
fof(f17, plain, ! [X3] : ~ (cA(X3) & cB(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_4)).
fof(f175, plain, ! [X1] : (cB(sK7(X1)) | ~ sP0(X1)), inference(resolution, [], [f106, f95])).
fof(f95, plain, ! [X0, X1] : (~ rq(X0, X1) | cB(X1)), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1] : (cB(X1) | ~ rq(X0, X1)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0, X1] : (rq(X0, X1) => cB(X1)), inference(rectify, [], [f16])).
fof(f16, plain, ! [X3, X4] : (rq(X3, X4) => cB(X4)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_3)).
fof(f374, plain, (~ spl10_3 | ~ spl10_6 | ~ spl10_16), inference(avatar_contradiction_clause, [], [f373])).
fof(f373, plain, ($false | (~ spl10_3 | ~ spl10_6 | ~ spl10_16)), inference(subsumption_resolution, [], [f372, f135])).
fof(f372, plain, (~ sP0(sK2) | (~ spl10_6 | ~ spl10_16)), inference(subsumption_resolution, [], [f368, f166])).
fof(f368, plain, (~ cA(sK3) | ~ sP0(sK2) | ~ spl10_16), inference(superposition, [], [f176, f296])).
fof(f296, plain, ((sK3 = sK6(sK2)) | ~ spl10_16), inference(avatar_component_clause, [], [f294])).
fof(f176, plain, ! [X0] : (~ cA(sK6(X0)) | ~ sP0(X0)), inference(resolution, [], [f172, f96])).
fof(f172, plain, ! [X1] : (cB(sK6(X1)) | ~ sP0(X1)), inference(resolution, [], [f105, f95])).
fof(f346, plain, (~ spl10_3 | ~ spl10_6 | ~ spl10_14), inference(avatar_contradiction_clause, [], [f345])).
fof(f345, plain, ($false | (~ spl10_3 | ~ spl10_6 | ~ spl10_14)), inference(subsumption_resolution, [], [f344, f135])).
fof(f344, plain, (~ sP0(sK2) | (~ spl10_6 | ~ spl10_14)), inference(subsumption_resolution, [], [f341, f166])).
fof(f341, plain, (~ cA(sK3) | ~ sP0(sK2) | ~ spl10_14), inference(superposition, [], [f173, f268])).
fof(f268, plain, ((sK3 = sK5(sK2)) | ~ spl10_14), inference(avatar_component_clause, [], [f266])).
fof(f173, plain, ! [X0] : (~ cA(sK5(X0)) | ~ sP0(X0)), inference(resolution, [], [f170, f96])).
fof(f170, plain, ! [X1] : (cB(sK5(X1)) | ~ sP0(X1)), inference(resolution, [], [f104, f95])).
fof(f317, plain, (~ spl10_3 | ~ spl10_5 | ~ spl10_12), inference(avatar_contradiction_clause, [], [f316])).
fof(f316, plain, ($false | (~ spl10_3 | ~ spl10_5 | ~ spl10_12)), inference(subsumption_resolution, [], [f315, f135])).
fof(f315, plain, (~ sP0(sK2) | (~ spl10_5 | ~ spl10_12)), inference(subsumption_resolution, [], [f310, f165])).
fof(f165, plain, (cA(sK4) | ~ spl10_5), inference(resolution, [], [f94, f145])).
fof(f310, plain, (~ cA(sK4) | ~ sP0(sK2) | ~ spl10_12), inference(superposition, [], [f177, f215])).
fof(f215, plain, ((sK4 = sK7(sK2)) | ~ spl10_12), inference(avatar_component_clause, [], [f213])).
fof(f288, plain, (~ spl10_3 | ~ spl10_5 | ~ spl10_10), inference(avatar_contradiction_clause, [], [f287])).
fof(f287, plain, ($false | (~ spl10_3 | ~ spl10_5 | ~ spl10_10)), inference(subsumption_resolution, [], [f286, f135])).
fof(f286, plain, (~ sP0(sK2) | (~ spl10_5 | ~ spl10_10)), inference(subsumption_resolution, [], [f282, f165])).
fof(f282, plain, (~ cA(sK4) | ~ sP0(sK2) | ~ spl10_10), inference(superposition, [], [f176, f206])).
fof(f206, plain, ((sK4 = sK6(sK2)) | ~ spl10_10), inference(avatar_component_clause, [], [f204])).
fof(f262, plain, (~ spl10_3 | ~ spl10_5 | ~ spl10_8), inference(avatar_contradiction_clause, [], [f261])).
fof(f261, plain, ($false | (~ spl10_3 | ~ spl10_5 | ~ spl10_8)), inference(subsumption_resolution, [], [f260, f135])).
fof(f260, plain, (~ sP0(sK2) | (~ spl10_5 | ~ spl10_8)), inference(subsumption_resolution, [], [f257, f165])).
fof(f257, plain, (~ cA(sK4) | ~ sP0(sK2) | ~ spl10_8), inference(superposition, [], [f173, f197])).
fof(f197, plain, ((sK4 = sK5(sK2)) | ~ spl10_8), inference(avatar_component_clause, [], [f195])).
fof(f163, plain, (spl10_7 | spl10_1), inference(avatar_split_clause, [], [f162, f125, f156])).
fof(f156, plain, (spl10_7 <=> xsd_string(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_7])])).
fof(f125, plain, (spl10_1 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f162, plain, (sP1 | xsd_string(sK8)), inference(subsumption_resolution, [], [f161, f93])).
fof(f93, plain, ! [X0] : (xsd_string(X0) | xsd_integer(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : ((xsd_string(X0) | xsd_integer(X0)) & (~ xsd_integer(X0) | ~ xsd_string(X0))), inference(nnf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (xsd_string(X0) <=> ~ xsd_integer(X0)), inference(rectify, [], [f14])).
fof(f14, plain, ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_1)).
fof(f161, plain, (sP1 | ~ xsd_integer(sK8) | xsd_string(sK8)), inference(subsumption_resolution, [], [f160, f90])).
fof(f90, plain, ! [X0] : cowlThing(X0), inference(cnf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (~ cowlNothing(X0) & cowlThing(X0)), inference(rectify, [], [f13])).
fof(f13, plain, ! [X3] : (~ cowlNothing(X3) & cowlThing(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', axiom_0)).
fof(f160, plain, (sP1 | ~ xsd_integer(sK8) | xsd_string(sK8) | ~ cowlThing(sK9)), inference(subsumption_resolution, [], [f110, f91])).
fof(f91, plain, ! [X0] : ~ cowlNothing(X0), inference(cnf_transformation, [], [f22])).
fof(f110, plain, (sP1 | ~ xsd_integer(sK8) | xsd_string(sK8) | cowlNothing(sK9) | ~ cowlThing(sK9)), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (sP1 | ((xsd_integer(sK8) | ~ xsd_string(sK8)) & (~ xsd_integer(sK8) | xsd_string(sK8))) | (cowlNothing(sK9) | ~ cowlThing(sK9))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f74, f76, f75])).
fof(f75, plain, (? [X0] : ((xsd_integer(X0) | ~ xsd_string(X0)) & (~ xsd_integer(X0) | xsd_string(X0))) => ((xsd_integer(sK8) | ~ xsd_string(sK8)) & (~ xsd_integer(sK8) | xsd_string(sK8)))), introduced(choice_axiom, [])).
fof(f76, plain, (? [X1] : (cowlNothing(X1) | ~ cowlThing(X1)) => (cowlNothing(sK9) | ~ cowlThing(sK9))), introduced(choice_axiom, [])).
fof(f74, plain, (sP1 | ? [X0] : ((xsd_integer(X0) | ~ xsd_string(X0)) & (~ xsd_integer(X0) | xsd_string(X0))) | ? [X1] : (cowlNothing(X1) | ~ cowlThing(X1))), inference(rectify, [], [f73])).
fof(f73, plain, (sP1 | ? [X11] : ((xsd_integer(X11) | ~ xsd_string(X11)) & (~ xsd_integer(X11) | xsd_string(X11))) | ? [X12] : (cowlNothing(X12) | ~ cowlThing(X12))), inference(nnf_transformation, [], [f63])).
fof(f63, plain, (sP1 | ? [X11] : ~ (xsd_string(X11) <=> ~ xsd_integer(X11)) | ? [X12] : (cowlNothing(X12) | ~ cowlThing(X12))), inference(definition_folding, [], [f60, e62, e61])).
fof(f62, plain, (? [X0] : (! [X6, X7, X8, X9, X10] : ((X9 = X10) | (X8 = X10) | (X8 = X9) | (X7 = X10) | (X7 = X9) | (X7 = X8) | (X6 = X10) | (X6 = X9) | (X6 = X8) | (X6 = X7) | ~ rr(X0, X10) | ~ rr(X0, X9) | ~ rr(X0, X8) | ~ rr(X0, X7) | ~ rr(X0, X6)) & sP0(X0) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4))) | ~ sP1), inference(usedef, [], [e62])).
fof(e62, plain, (sP1 <=> ? [X0] : (! [X6, X7, X8, X9, X10] : ((X9 = X10) | (X8 = X10) | (X8 = X9) | (X7 = X10) | (X7 = X9) | (X7 = X8) | (X6 = X10) | (X6 = X9) | (X6 = X8) | (X6 = X7) | ~ rr(X0, X10) | ~ rr(X0, X9) | ~ rr(X0, X8) | ~ rr(X0, X7) | ~ rr(X0, X6)) & sP0(X0) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f60, plain, (? [X0] : (! [X6, X7, X8, X9, X10] : ((X9 = X10) | (X8 = X10) | (X8 = X9) | (X7 = X10) | (X7 = X9) | (X7 = X8) | (X6 = X10) | (X6 = X9) | (X6 = X8) | (X6 = X7) | ~ rr(X0, X10) | ~ rr(X0, X9) | ~ rr(X0, X8) | ~ rr(X0, X7) | ~ rr(X0, X6)) & ? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1)) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4))) | ? [X11] : ~ (xsd_string(X11) <=> ~ xsd_integer(X11)) | ? [X12] : (cowlNothing(X12) | ~ cowlThing(X12))), inference(flattening, [], [f59])).
fof(f59, plain, (? [X0] : (! [X6, X7, X8, X9, X10] : ((X9 = X10) | (X8 = X10) | (X8 = X9) | (X7 = X10) | (X7 = X9) | (X7 = X8) | (X6 = X10) | (X6 = X9) | (X6 = X8) | (X6 = X7) | ~ rr(X0, X10) | ~ rr(X0, X9) | ~ rr(X0, X8) | ~ rr(X0, X7) | ~ rr(X0, X6)) & (? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1)) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4)))) | ? [X11] : ~ (xsd_string(X11) <=> ~ xsd_integer(X11)) | ? [X12] : (cowlNothing(X12) | ~ cowlThing(X12))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ (! [X0] : ((? [X1, X2, X3] : (~ (X2 = X3) & ~ (X1 = X3) & ~ (X1 = X2) & rq(X0, X3) & rq(X0, X2) & rq(X0, X1)) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4))) => ? [X6, X7, X8, X9, X10] : (~ (X9 = X10) & ~ (X8 = X10) & ~ (X8 = X9) & ~ (X7 = X10) & ~ (X7 = X9) & ~ (X7 = X8) & ~ (X6 = X10) & ~ (X6 = X9) & ~ (X6 = X8) & ~ (X6 = X7) & rr(X0, X10) & rr(X0, X9) & rr(X0, X8) & rr(X0, X7) & rr(X0, X6))) & ! [X11] : (xsd_string(X11) <=> ~ xsd_integer(X11)) & ! [X12] : (~ cowlNothing(X12) & cowlThing(X12))), inference(rectify, [], [f21])).
fof(f21, plain, ~ (! [X3] : ((? [X5, X6, X7] : (~ (X6 = X7) & ~ (X5 = X7) & ~ (X5 = X6) & rq(X3, X7) & rq(X3, X6) & rq(X3, X5)) & ? [X5, X6] : (~ (X5 = X6) & rp(X3, X6) & rp(X3, X5))) => ? [X5, X6, X7, X8, X9] : (~ (X8 = X9) & ~ (X7 = X9) & ~ (X7 = X8) & ~ (X6 = X9) & ~ (X6 = X8) & ~ (X6 = X7) & ~ (X5 = X9) & ~ (X5 = X8) & ~ (X5 = X7) & ~ (X5 = X6) & rr(X3, X9) & rr(X3, X8) & rr(X3, X7) & rr(X3, X6) & rr(X3, X5))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), inference(negated_conjecture, [], [f20])).
fof(f20, plain, ~ (! [X3] : ((? [X5, X6, X7] : (~ (X6 = X7) & ~ (X5 = X7) & ~ (X5 = X6) & rq(X3, X7) & rq(X3, X6) & rq(X3, X5)) & ? [X5, X6] : (~ (X5 = X6) & rp(X3, X6) & rp(X3, X5))) => ? [X5, X6, X7, X8, X9] : (~ (X8 = X9) & ~ (X7 = X9) & ~ (X7 = X8) & ~ (X6 = X9) & ~ (X6 = X8) & ~ (X6 = X7) & ~ (X5 = X9) & ~ (X5 = X8) & ~ (X5 = X7) & ~ (X5 = X6) & rr(X3, X9) & rr(X3, X8) & rr(X3, X7) & rr(X3, X6) & rr(X3, X5))) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS162+1.p', the_axiom)).
fof(f159, plain, (~ spl10_7 | spl10_1), inference(avatar_split_clause, [], [f154, f125, f156])).
fof(f154, plain, (sP1 | ~ xsd_string(sK8)), inference(subsumption_resolution, [], [f153, f92])).
fof(f92, plain, ! [X0] : (~ xsd_string(X0) | ~ xsd_integer(X0)), inference(cnf_transformation, [], [f64])).
fof(f153, plain, (sP1 | xsd_integer(sK8) | ~ xsd_string(sK8)), inference(subsumption_resolution, [], [f152, f90])).
fof(f152, plain, (sP1 | xsd_integer(sK8) | ~ xsd_string(sK8) | ~ cowlThing(sK9)), inference(subsumption_resolution, [], [f111, f91])).
fof(f111, plain, (sP1 | xsd_integer(sK8) | ~ xsd_string(sK8) | cowlNothing(sK9) | ~ cowlThing(sK9)), inference(cnf_transformation, [], [f77])).
fof(f151, plain, (~ spl10_1 | spl10_6), inference(avatar_split_clause, [], [f99, f148, f125])).
fof(f99, plain, (rp(sK2, sK3) | ~ sP1), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ((! [X1, X2, X3, X4, X5] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | (X2 = X5) | (X2 = X4) | (X2 = X3) | (X1 = X5) | (X1 = X4) | (X1 = X3) | (X1 = X2) | ~ rr(sK2, X5) | ~ rr(sK2, X4) | ~ rr(sK2, X3) | ~ rr(sK2, X2) | ~ rr(sK2, X1)) & sP0(sK2) & (~ (sK3 = sK4) & rp(sK2, sK4) & rp(sK2, sK3))) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4])], [f66, f68, f67])).
fof(f67, plain, (? [X0] : (! [X1, X2, X3, X4, X5] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | (X2 = X5) | (X2 = X4) | (X2 = X3) | (X1 = X5) | (X1 = X4) | (X1 = X3) | (X1 = X2) | ~ rr(X0, X5) | ~ rr(X0, X4) | ~ rr(X0, X3) | ~ rr(X0, X2) | ~ rr(X0, X1)) & sP0(X0) & ? [X6, X7] : (~ (X6 = X7) & rp(X0, X7) & rp(X0, X6))) => (! [X5, X4, X3, X2, X1] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | (X2 = X5) | (X2 = X4) | (X2 = X3) | (X1 = X5) | (X1 = X4) | (X1 = X3) | (X1 = X2) | ~ rr(sK2, X5) | ~ rr(sK2, X4) | ~ rr(sK2, X3) | ~ rr(sK2, X2) | ~ rr(sK2, X1)) & sP0(sK2) & ? [X7, X6] : (~ (X6 = X7) & rp(sK2, X7) & rp(sK2, X6)))), introduced(choice_axiom, [])).
fof(f68, plain, (? [X7, X6] : (~ (X6 = X7) & rp(sK2, X7) & rp(sK2, X6)) => (~ (sK3 = sK4) & rp(sK2, sK4) & rp(sK2, sK3))), introduced(choice_axiom, [])).
fof(f66, plain, (? [X0] : (! [X1, X2, X3, X4, X5] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | (X2 = X5) | (X2 = X4) | (X2 = X3) | (X1 = X5) | (X1 = X4) | (X1 = X3) | (X1 = X2) | ~ rr(X0, X5) | ~ rr(X0, X4) | ~ rr(X0, X3) | ~ rr(X0, X2) | ~ rr(X0, X1)) & sP0(X0) & ? [X6, X7] : (~ (X6 = X7) & rp(X0, X7) & rp(X0, X6))) | ~ sP1), inference(rectify, [], [f65])).
fof(f65, plain, (? [X0] : (! [X6, X7, X8, X9, X10] : ((X9 = X10) | (X8 = X10) | (X8 = X9) | (X7 = X10) | (X7 = X9) | (X7 = X8) | (X6 = X10) | (X6 = X9) | (X6 = X8) | (X6 = X7) | ~ rr(X0, X10) | ~ rr(X0, X9) | ~ rr(X0, X8) | ~ rr(X0, X7) | ~ rr(X0, X6)) & sP0(X0) & ? [X4, X5] : (~ (X4 = X5) & rp(X0, X5) & rp(X0, X4))) | ~ sP1), inference(nnf_transformation, [], [f62])).
fof(f146, plain, (~ spl10_1 | spl10_5), inference(avatar_split_clause, [], [f100, f143, f125])).
fof(f100, plain, (rp(sK2, sK4) | ~ sP1), inference(cnf_transformation, [], [f69])).
fof(f141, plain, (~ spl10_1 | ~ spl10_4), inference(avatar_split_clause, [], [f101, f138, f125])).
fof(f101, plain, (~ (sK3 = sK4) | ~ sP1), inference(cnf_transformation, [], [f69])).
fof(f136, plain, (~ spl10_1 | spl10_3), inference(avatar_split_clause, [], [f102, f133, f125])).
fof(f102, plain, (sP0(sK2) | ~ sP1), inference(cnf_transformation, [], [f69])).
fof(f131, plain, (~ spl10_1 | spl10_2), inference(avatar_split_clause, [], [f103, f129, f125])).
fof(f103, plain, ! [X4, X2, X5, X3, X1] : ((X4 = X5) | (X3 = X5) | (X3 = X4) | (X2 = X5) | (X2 = X4) | (X2 = X3) | (X1 = X5) | (X1 = X4) | (X1 = X3) | (X1 = X2) | ~ rr(sK2, X5) | ~ rr(sK2, X4) | ~ rr(sK2, X3) | ~ rr(sK2, X2) | ~ rr(sK2, X1) | ~ sP1), inference(cnf_transformation, [], [f69])).