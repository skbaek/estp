fof(f9704, plain, $false, inference(avatar_sat_refutation, [], [f2513, f2530, f9369, f9373, f9703])).
fof(f9703, plain, (~ spl10_40 | spl10_81), inference(avatar_contradiction_clause, [], [f9702])).
fof(f9702, plain, ($false | (~ spl10_40 | spl10_81)), inference(subsumption_resolution, [], [f9701, f2502])).
fof(f2502, plain, (sP1(xI, xJ) | ~ spl10_40), inference(avatar_component_clause, [], [f2501])).
fof(f2501, plain, (spl10_40 <=> sP1(xI, xJ)), introduced(avatar_definition, [new_symbols(naming, [spl10_40])])).
fof(f9701, plain, (~ sP1(xI, xJ) | spl10_81), inference(subsumption_resolution, [], [f9700, f138])).
fof(f138, plain, aElementOf0(xy, sdtpldt1(xI, xJ)), inference(cnf_transformation, [], [f26])).
fof(f26, plain, (aElement0(xz) & aElementOf0(xy, sdtpldt1(xI, xJ)) & aElementOf0(xx, sdtpldt1(xI, xJ))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG087+1.p', m__901)).
fof(f9700, plain, (~ aElementOf0(xy, sdtpldt1(xI, xJ)) | ~ sP1(xI, xJ) | spl10_81), inference(resolution, [], [f9368, f375])).
fof(f375, plain, ! [X2, X0, X1] : (aElementOf0(sK8(X2, X1, X0), X2) | ~ aElementOf0(X0, sdtpldt1(X1, X2)) | ~ sP1(X1, X2)), inference(resolution, [], [f117, f144])).
fof(f144, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt1(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f113])).
fof(f113, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ! [X0, X1] : (! [X2] : (((sdtpldt1(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e67])).
fof(e67, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f117, plain, ! [X2, X0, X8, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X8, X2) | aElementOf0(sK8(X0, X1, X8), X0)), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1)) | aElementOf0(sK4(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6, sK7, sK8])], [f75, f78, f77, f76])).
fof(f76, plain, ! [X2, X1, X0] : (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) => ((! [X5, X4] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(sK4(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f77, plain, ! [X2, X1, X0] : (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) => ((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1))), introduced(choice_axiom, [])).
fof(f78, plain, ! [X8, X1, X0] : (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) => ((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1))), introduced(choice_axiom, [])).
fof(f75, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f74])).
fof(f74, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f73])).
fof(f73, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f66])).
fof(f66, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), inference(usedef, [], [e66])).
fof(e66, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f9368, plain, (~ aElementOf0(sK8(xJ, xI, xy), xJ) | spl10_81), inference(avatar_component_clause, [], [f9366])).
fof(f9366, plain, (spl10_81 <=> aElementOf0(sK8(xJ, xI, xy), xJ)), introduced(avatar_definition, [new_symbols(naming, [spl10_81])])).
fof(f9373, plain, (~ spl10_40 | spl10_80), inference(avatar_contradiction_clause, [], [f9372])).
fof(f9372, plain, ($false | (~ spl10_40 | spl10_80)), inference(subsumption_resolution, [], [f9371, f2502])).
fof(f9371, plain, (~ sP1(xI, xJ) | spl10_80), inference(subsumption_resolution, [], [f9370, f138])).
fof(f9370, plain, (~ aElementOf0(xy, sdtpldt1(xI, xJ)) | ~ sP1(xI, xJ) | spl10_80), inference(resolution, [], [f9364, f372])).
fof(f372, plain, ! [X2, X0, X1] : (aElementOf0(sK7(X2, X1, X0), X1) | ~ aElementOf0(X0, sdtpldt1(X1, X2)) | ~ sP1(X1, X2)), inference(resolution, [], [f116, f144])).
fof(f116, plain, ! [X2, X0, X8, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X8, X2) | aElementOf0(sK7(X0, X1, X8), X1)), inference(cnf_transformation, [], [f79])).
fof(f9364, plain, (~ aElementOf0(sK7(xJ, xI, xy), xI) | spl10_80), inference(avatar_component_clause, [], [f9362])).
fof(f9362, plain, (spl10_80 <=> aElementOf0(sK7(xJ, xI, xy), xI)), introduced(avatar_definition, [new_symbols(naming, [spl10_80])])).
fof(f9369, plain, (~ spl10_80 | ~ spl10_81 | ~ spl10_42), inference(avatar_split_clause, [], [f9360, f2510, f9366, f9362])).
fof(f2510, plain, (spl10_42 <=> (xy = sdtpldt0(sK7(xJ, xI, xy), sK8(xJ, xI, xy)))), introduced(avatar_definition, [new_symbols(naming, [spl10_42])])).
fof(f9360, plain, (~ aElementOf0(sK8(xJ, xI, xy), xJ) | ~ aElementOf0(sK7(xJ, xI, xy), xI) | ~ spl10_42), inference(trivial_inequality_removal, [], [f9358])).
fof(f9358, plain, (~ (xy = xy) | ~ aElementOf0(sK8(xJ, xI, xy), xJ) | ~ aElementOf0(sK7(xJ, xI, xy), xI) | ~ spl10_42), inference(superposition, [], [f143, f2512])).
fof(f2512, plain, ((xy = sdtpldt0(sK7(xJ, xI, xy), sK8(xJ, xI, xy))) | ~ spl10_42), inference(avatar_component_clause, [], [f2510])).
fof(f143, plain, ! [X0, X1] : (~ (sdtpldt0(X0, X1) = xy) | ~ aElementOf0(X1, xJ) | ~ aElementOf0(X0, xI)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0, X1] : (~ (sdtpldt0(X0, X1) = xy) | ~ aElementOf0(X1, xJ) | ~ aElementOf0(X0, xI)), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ? [X0, X1] : ((sdtpldt0(X0, X1) = xy) & aElementOf0(X1, xJ) & aElementOf0(X0, xI)), inference(negated_conjecture, [], [f28])).
fof(f28, plain, ~ ? [X0, X1] : ((sdtpldt0(X0, X1) = xy) & aElementOf0(X1, xJ) & aElementOf0(X0, xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG087+1.p', m__)).
fof(f2530, plain, spl10_40, inference(avatar_contradiction_clause, [], [f2529])).
fof(f2529, plain, ($false | spl10_40), inference(subsumption_resolution, [], [f2528, f150])).
fof(f150, plain, aSet0(xI), inference(resolution, [], [f132, f135])).
fof(f135, plain, aIdeal0(xI), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (aIdeal0(xJ) & aIdeal0(xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG087+1.p', m__870)).
fof(f132, plain, ! [X0] : (~ aIdeal0(X0) | aSet0(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0)), inference(ennf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (aIdeal0(X0) => (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(unused_predicate_definition_removal, [], [f33])).
fof(f33, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X2] : (aElementOf0(X2, X0) => aElementOf0(sdtpldt0(X1, X2), X0)))) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG087+1.p', mDefIdeal)).
fof(f2528, plain, (~ aSet0(xI) | spl10_40), inference(subsumption_resolution, [], [f2527, f151])).
fof(f151, plain, aSet0(xJ), inference(resolution, [], [f132, f136])).
fof(f136, plain, aIdeal0(xJ), inference(cnf_transformation, [], [f25])).
fof(f2527, plain, (~ aSet0(xJ) | ~ aSet0(xI) | spl10_40), inference(resolution, [], [f2503, f124])).
fof(f124, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f61, e67, e66])).
fof(f61, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f60])).
fof(f60, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG087+1.p', mDefSSum)).
fof(f2503, plain, (~ sP1(xI, xJ) | spl10_40), inference(avatar_component_clause, [], [f2501])).
fof(f2513, plain, (~ spl10_40 | spl10_42), inference(avatar_split_clause, [], [f2479, f2510, f2501])).
fof(f2479, plain, ((xy = sdtpldt0(sK7(xJ, xI, xy), sK8(xJ, xI, xy))) | ~ sP1(xI, xJ)), inference(resolution, [], [f484, f138])).
fof(f484, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtpldt1(X1, X2)) | (sdtpldt0(sK7(X2, X1, X0), sK8(X2, X1, X0)) = X0) | ~ sP1(X1, X2)), inference(resolution, [], [f118, f144])).
fof(f118, plain, ! [X2, X0, X8, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X8, X2) | (sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8)), inference(cnf_transformation, [], [f79])).