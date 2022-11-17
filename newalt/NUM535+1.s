fof(f22693, plain, $false, inference(avatar_sat_refutation, [], [f131, f1357, f1638, f1729, f1740, f20485, f20496, f21201, f22690])).
fof(f22690, plain, (~ spl8_17 | spl8_202), inference(avatar_contradiction_clause, [], [f22689])).
fof(f22689, plain, ($false | (~ spl8_17 | spl8_202)), inference(subsumption_resolution, [], [f22688, f1087])).
fof(f1087, plain, (sP1(sdtmndt0(xS, xx), xx) | ~ spl8_17), inference(avatar_component_clause, [], [f1086])).
fof(f1086, plain, (spl8_17 <=> sP1(sdtmndt0(xS, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_17])])).
fof(f22688, plain, (~ sP1(sdtmndt0(xS, xx), xx) | spl8_202), inference(subsumption_resolution, [], [f22681, f133])).
fof(f133, plain, aElement0(xx), inference(subsumption_resolution, [], [f132, f107])).
fof(f107, plain, aSet0(xS), inference(cnf_transformation, [], [f17])).
fof(f17, plain, aSet0(xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', m__617)).
fof(f132, plain, (aElement0(xx) | ~ aSet0(xS)), inference(resolution, [], [f70, f108])).
fof(f108, plain, aElementOf0(xx, xS), inference(cnf_transformation, [], [f18])).
fof(f18, plain, aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', m__617_02)).
fof(f70, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', mEOfElem)).
fof(f22681, plain, (~ aElement0(xx) | ~ sP1(sdtmndt0(xS, xx), xx) | spl8_202), inference(resolution, [], [f12477, f143])).
fof(f143, plain, ! [X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X0)) | ~ aElement0(X0) | ~ sP1(X1, X0)), inference(resolution, [], [f113, f112])).
fof(f112, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f83])).
fof(f83, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ! [X0, X1] : (! [X2] : (((sdtpldt0(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f43])).
fof(f43, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e43])).
fof(e43, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f113, plain, ! [X4, X2, X1] : (~ sP0(X4, X1, X2) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(equality_resolution, [], [f89])).
fof(f89, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (X0 = X4) | ~ aElement0(X4) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f61, f62])).
fof(f62, plain, ! [X2, X1, X0] : (? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) => (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f61, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f60])).
fof(f60, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : (((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f59])).
fof(f59, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3))) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f42])).
fof(f42, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e42])).
fof(e42, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f12477, plain, (~ aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | spl8_202), inference(avatar_component_clause, [], [f12476])).
fof(f12476, plain, (spl8_202 <=> aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_202])])).
fof(f21201, plain, (~ spl8_202 | spl8_1 | ~ spl8_25 | ~ spl8_245), inference(avatar_split_clause, [], [f21200, f18869, f1331, f124, f12476])).
fof(f124, plain, (spl8_1 <=> aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_1])])).
fof(f1331, plain, (spl8_25 <=> aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_25])])).
fof(f18869, plain, (spl8_245 <=> (xx = sK5(sdtpldt0(sdtmndt0(xS, xx), xx), xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_245])])).
fof(f21200, plain, (~ aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | (spl8_1 | ~ spl8_25 | ~ spl8_245)), inference(subsumption_resolution, [], [f21199, f1332])).
fof(f1332, plain, (aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ spl8_25), inference(avatar_component_clause, [], [f1331])).
fof(f21199, plain, (~ aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | (spl8_1 | ~ spl8_245)), inference(subsumption_resolution, [], [f21198, f107])).
fof(f21198, plain, (~ aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(xS) | ~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | (spl8_1 | ~ spl8_245)), inference(subsumption_resolution, [], [f21189, f126])).
fof(f126, plain, (~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) | spl8_1), inference(avatar_component_clause, [], [f124])).
fof(f21189, plain, (~ aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(xS) | ~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ spl8_245), inference(superposition, [], [f78, f18871])).
fof(f18871, plain, ((xx = sK5(sdtpldt0(sdtmndt0(xS, xx), xx), xS)) | ~ spl8_245), inference(avatar_component_clause, [], [f18869])).
fof(f78, plain, ! [X0, X1] : (~ aElementOf0(sK5(X0, X1), X0) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f55, f56])).
fof(f56, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f55, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f54])).
fof(f54, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f53])).
fof(f53, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', mDefSub)).
fof(f20496, plain, (spl8_245 | spl8_1 | ~ spl8_17 | ~ spl8_36), inference(avatar_split_clause, [], [f20495, f1590, f1086, f124, f18869])).
fof(f1590, plain, (spl8_36 <=> sP3(xS, xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_36])])).
fof(f20495, plain, ((xx = sK5(sdtpldt0(sdtmndt0(xS, xx), xx), xS)) | (spl8_1 | ~ spl8_17 | ~ spl8_36)), inference(subsumption_resolution, [], [f20494, f1591])).
fof(f1591, plain, (sP3(xS, xx) | ~ spl8_36), inference(avatar_component_clause, [], [f1590])).
fof(f20494, plain, ((xx = sK5(sdtpldt0(sdtmndt0(xS, xx), xx), xS)) | ~ sP3(xS, xx) | (spl8_1 | ~ spl8_17)), inference(subsumption_resolution, [], [f20493, f1087])).
fof(f20493, plain, (~ sP1(sdtmndt0(xS, xx), xx) | (xx = sK5(sdtpldt0(sdtmndt0(xS, xx), xx), xS)) | ~ sP3(xS, xx) | spl8_1), inference(subsumption_resolution, [], [f20486, f107])).
fof(f20486, plain, (~ aSet0(xS) | ~ sP1(sdtmndt0(xS, xx), xx) | (xx = sK5(sdtpldt0(sdtmndt0(xS, xx), xx), xS)) | ~ sP3(xS, xx) | spl8_1), inference(resolution, [], [f126, f4782])).
fof(f4782, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) | ~ aSet0(X0) | ~ sP1(sdtmndt0(X0, X1), X2) | (sK5(sdtpldt0(sdtmndt0(X0, X1), X2), X0) = X1) | ~ sP3(X0, X1)), inference(subsumption_resolution, [], [f4781, f142])).
fof(f142, plain, ! [X4, X3] : (aSet0(sdtpldt0(X3, X4)) | ~ sP1(X3, X4)), inference(resolution, [], [f112, f85])).
fof(f85, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f63])).
fof(f4781, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) | ~ aSet0(X0) | ~ sP1(sdtmndt0(X0, X1), X2) | (sK5(sdtpldt0(sdtmndt0(X0, X1), X2), X0) = X1) | ~ sP3(X0, X1) | ~ aSet0(sdtpldt0(sdtmndt0(X0, X1), X2))), inference(subsumption_resolution, [], [f4780, f164])).
fof(f164, plain, ! [X0, X1] : (aElement0(sK5(X1, X0)) | ~ aSet0(X0) | ~ aSet0(X1) | aSubsetOf0(X0, X1)), inference(duplicate_literal_removal, [], [f159])).
fof(f159, plain, ! [X0, X1] : (aSubsetOf0(X0, X1) | ~ aSet0(X0) | ~ aSet0(X1) | aElement0(sK5(X1, X0)) | ~ aSet0(X0)), inference(resolution, [], [f77, f70])).
fof(f77, plain, ! [X0, X1] : (aElementOf0(sK5(X0, X1), X1) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f57])).
fof(f4780, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) | ~ aSet0(X0) | ~ sP1(sdtmndt0(X0, X1), X2) | ~ aElement0(sK5(sdtpldt0(sdtmndt0(X0, X1), X2), X0)) | (sK5(sdtpldt0(sdtmndt0(X0, X1), X2), X0) = X1) | ~ sP3(X0, X1) | ~ aSet0(sdtpldt0(sdtmndt0(X0, X1), X2))), inference(duplicate_literal_removal, [], [f4758])).
fof(f4758, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) | ~ aSet0(X0) | ~ sP1(sdtmndt0(X0, X1), X2) | ~ aElement0(sK5(sdtpldt0(sdtmndt0(X0, X1), X2), X0)) | (sK5(sdtpldt0(sdtmndt0(X0, X1), X2), X0) = X1) | ~ sP3(X0, X1) | aSubsetOf0(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(sdtmndt0(X0, X1), X2))), inference(resolution, [], [f588, f77])).
fof(f588, plain, ! [X12, X10, X11, X9] : (~ aElementOf0(sK5(sdtpldt0(sdtmndt0(X9, X10), X11), X12), X9) | aSubsetOf0(X12, sdtpldt0(sdtmndt0(X9, X10), X11)) | ~ aSet0(X12) | ~ sP1(sdtmndt0(X9, X10), X11) | ~ aElement0(sK5(sdtpldt0(sdtmndt0(X9, X10), X11), X12)) | (sK5(sdtpldt0(sdtmndt0(X9, X10), X11), X12) = X10) | ~ sP3(X9, X10)), inference(resolution, [], [f323, f191])).
fof(f191, plain, ! [X2, X0, X1] : (aElementOf0(X1, sdtmndt0(X2, X0)) | ~ aElementOf0(X1, X2) | ~ aElement0(X1) | (X0 = X1) | ~ sP3(X2, X0)), inference(resolution, [], [f101, f114])).
fof(f114, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f95])).
fof(f95, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e46])).
fof(e46, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f101, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f67, f68])).
fof(f68, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f67, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f66])).
fof(f66, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f65])).
fof(f65, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f45])).
fof(f45, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e45])).
fof(e45, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f323, plain, ! [X6, X8, X7] : (~ aElementOf0(sK5(sdtpldt0(X6, X7), X8), X6) | ~ sP1(X6, X7) | aSubsetOf0(X8, sdtpldt0(X6, X7)) | ~ aSet0(X8)), inference(subsumption_resolution, [], [f322, f142])).
fof(f322, plain, ! [X6, X8, X7] : (~ aElementOf0(sK5(sdtpldt0(X6, X7), X8), X6) | ~ sP1(X6, X7) | aSubsetOf0(X8, sdtpldt0(X6, X7)) | ~ aSet0(X8) | ~ aSet0(sdtpldt0(X6, X7))), inference(subsumption_resolution, [], [f318, f164])).
fof(f318, plain, ! [X6, X8, X7] : (~ aElement0(sK5(sdtpldt0(X6, X7), X8)) | ~ aElementOf0(sK5(sdtpldt0(X6, X7), X8), X6) | ~ sP1(X6, X7) | aSubsetOf0(X8, sdtpldt0(X6, X7)) | ~ aSet0(X8) | ~ aSet0(sdtpldt0(X6, X7))), inference(resolution, [], [f183, f78])).
fof(f183, plain, ! [X2, X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X2)) | ~ aElement0(X0) | ~ aElementOf0(X0, X1) | ~ sP1(X1, X2)), inference(resolution, [], [f88, f112])).
fof(f88, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f63])).
fof(f20485, plain, (spl8_2 | ~ spl8_17 | ~ spl8_25 | ~ spl8_36), inference(avatar_contradiction_clause, [], [f20484])).
fof(f20484, plain, ($false | (spl8_2 | ~ spl8_17 | ~ spl8_25 | ~ spl8_36)), inference(subsumption_resolution, [], [f20483, f107])).
fof(f20483, plain, (~ aSet0(xS) | (spl8_2 | ~ spl8_17 | ~ spl8_25 | ~ spl8_36)), inference(subsumption_resolution, [], [f20482, f1332])).
fof(f20482, plain, (~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(xS) | (spl8_2 | ~ spl8_17 | ~ spl8_36)), inference(subsumption_resolution, [], [f20481, f130])).
fof(f130, plain, (~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | spl8_2), inference(avatar_component_clause, [], [f128])).
fof(f128, plain, (spl8_2 <=> aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_2])])).
fof(f20481, plain, (aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(xS) | (spl8_2 | ~ spl8_17 | ~ spl8_36)), inference(subsumption_resolution, [], [f20475, f108])).
fof(f20475, plain, (~ aElementOf0(xx, xS) | aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(xS) | (spl8_2 | ~ spl8_17 | ~ spl8_36)), inference(superposition, [], [f78, f19502])).
fof(f19502, plain, ((xx = sK5(xS, sdtpldt0(sdtmndt0(xS, xx), xx))) | (spl8_2 | ~ spl8_17 | ~ spl8_36)), inference(subsumption_resolution, [], [f19501, f1591])).
fof(f19501, plain, ((xx = sK5(xS, sdtpldt0(sdtmndt0(xS, xx), xx))) | ~ sP3(xS, xx) | (spl8_2 | ~ spl8_17)), inference(subsumption_resolution, [], [f19500, f107])).
fof(f19500, plain, (~ aSet0(xS) | (xx = sK5(xS, sdtpldt0(sdtmndt0(xS, xx), xx))) | ~ sP3(xS, xx) | (spl8_2 | ~ spl8_17)), inference(subsumption_resolution, [], [f19475, f1087])).
fof(f19475, plain, (~ sP1(sdtmndt0(xS, xx), xx) | ~ aSet0(xS) | (xx = sK5(xS, sdtpldt0(sdtmndt0(xS, xx), xx))) | ~ sP3(xS, xx) | spl8_2), inference(resolution, [], [f3727, f130])).
fof(f3727, plain, ! [X2, X0, X1] : (aSubsetOf0(sdtpldt0(sdtmndt0(X0, X1), X2), X0) | ~ sP1(sdtmndt0(X0, X1), X2) | ~ aSet0(X0) | (sK5(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) = X2) | ~ sP3(X0, X1)), inference(subsumption_resolution, [], [f3726, f142])).
fof(f3726, plain, ! [X2, X0, X1] : (~ sP1(sdtmndt0(X0, X1), X2) | aSubsetOf0(sdtpldt0(sdtmndt0(X0, X1), X2), X0) | ~ aSet0(X0) | (sK5(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) = X2) | ~ sP3(X0, X1) | ~ aSet0(sdtpldt0(sdtmndt0(X0, X1), X2))), inference(duplicate_literal_removal, [], [f3707])).
fof(f3707, plain, ! [X2, X0, X1] : (~ sP1(sdtmndt0(X0, X1), X2) | aSubsetOf0(sdtpldt0(sdtmndt0(X0, X1), X2), X0) | ~ aSet0(X0) | (sK5(X0, sdtpldt0(sdtmndt0(X0, X1), X2)) = X2) | ~ sP3(X0, X1) | aSubsetOf0(sdtpldt0(sdtmndt0(X0, X1), X2), X0) | ~ aSet0(sdtpldt0(sdtmndt0(X0, X1), X2)) | ~ aSet0(X0)), inference(resolution, [], [f648, f78])).
fof(f648, plain, ! [X24, X23, X21, X22] : (aElementOf0(sK5(X21, sdtpldt0(sdtmndt0(X22, X23), X24)), X22) | ~ sP1(sdtmndt0(X22, X23), X24) | aSubsetOf0(sdtpldt0(sdtmndt0(X22, X23), X24), X21) | ~ aSet0(X21) | (sK5(X21, sdtpldt0(sdtmndt0(X22, X23), X24)) = X24) | ~ sP3(X22, X23)), inference(resolution, [], [f363, f147])).
fof(f147, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtmndt0(X1, X2)) | aElementOf0(X0, X1) | ~ sP3(X1, X2)), inference(resolution, [], [f99, f114])).
fof(f99, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | ~ aElementOf0(X4, X2) | aElementOf0(X4, X1)), inference(cnf_transformation, [], [f69])).
fof(f363, plain, ! [X8, X7, X9] : (aElementOf0(sK5(X7, sdtpldt0(X8, X9)), X8) | (sK5(X7, sdtpldt0(X8, X9)) = X9) | ~ sP1(X8, X9) | aSubsetOf0(sdtpldt0(X8, X9), X7) | ~ aSet0(X7)), inference(subsumption_resolution, [], [f352, f142])).
fof(f352, plain, ! [X8, X7, X9] : (aElementOf0(sK5(X7, sdtpldt0(X8, X9)), X8) | (sK5(X7, sdtpldt0(X8, X9)) = X9) | ~ sP1(X8, X9) | aSubsetOf0(sdtpldt0(X8, X9), X7) | ~ aSet0(sdtpldt0(X8, X9)) | ~ aSet0(X7)), inference(resolution, [], [f186, f77])).
fof(f186, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtpldt0(X1, X2)) | aElementOf0(X0, X1) | (X0 = X2) | ~ sP1(X1, X2)), inference(resolution, [], [f87, f112])).
fof(f87, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | aElementOf0(X4, X1) | ~ aElementOf0(X4, X2) | (X0 = X4)), inference(cnf_transformation, [], [f63])).
fof(f1740, plain, (spl8_17 | ~ spl8_18), inference(avatar_contradiction_clause, [], [f1739])).
fof(f1739, plain, ($false | (spl8_17 | ~ spl8_18)), inference(subsumption_resolution, [], [f1738, f1091])).
fof(f1091, plain, (aSet0(sdtmndt0(xS, xx)) | ~ spl8_18), inference(avatar_component_clause, [], [f1090])).
fof(f1090, plain, (spl8_18 <=> aSet0(sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_18])])).
fof(f1738, plain, (~ aSet0(sdtmndt0(xS, xx)) | spl8_17), inference(subsumption_resolution, [], [f1737, f133])).
fof(f1737, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(xS, xx)) | spl8_17), inference(resolution, [], [f1088, f94])).
fof(f94, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f38, e43, e42])).
fof(f38, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f37])).
fof(f37, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', mDefCons)).
fof(f1088, plain, (~ sP1(sdtmndt0(xS, xx), xx) | spl8_17), inference(avatar_component_clause, [], [f1086])).
fof(f1729, plain, (~ spl8_17 | spl8_25), inference(avatar_split_clause, [], [f1728, f1331, f1086])).
fof(f1728, plain, (~ sP1(sdtmndt0(xS, xx), xx) | spl8_25), inference(resolution, [], [f1333, f142])).
fof(f1333, plain, (~ aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) | spl8_25), inference(avatar_component_clause, [], [f1331])).
fof(f1638, plain, spl8_36, inference(avatar_contradiction_clause, [], [f1637])).
fof(f1637, plain, ($false | spl8_36), inference(subsumption_resolution, [], [f1636, f107])).
fof(f1636, plain, (~ aSet0(xS) | spl8_36), inference(subsumption_resolution, [], [f1635, f133])).
fof(f1635, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_36), inference(resolution, [], [f1592, f106])).
fof(f106, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f40, e46, e45])).
fof(f40, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f39])).
fof(f39, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', mDefDiff)).
fof(f1592, plain, (~ sP3(xS, xx) | spl8_36), inference(avatar_component_clause, [], [f1590])).
fof(f1357, plain, spl8_18, inference(avatar_contradiction_clause, [], [f1356])).
fof(f1356, plain, ($false | spl8_18), inference(subsumption_resolution, [], [f1355, f107])).
fof(f1355, plain, (~ aSet0(xS) | spl8_18), inference(subsumption_resolution, [], [f1354, f133])).
fof(f1354, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_18), inference(resolution, [], [f1345, f106])).
fof(f1345, plain, (~ sP3(xS, xx) | spl8_18), inference(resolution, [], [f1092, f146])).
fof(f146, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f114, f97])).
fof(f97, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f69])).
fof(f1092, plain, (~ aSet0(sdtmndt0(xS, xx)) | spl8_18), inference(avatar_component_clause, [], [f1090])).
fof(f131, plain, (~ spl8_1 | ~ spl8_2), inference(avatar_split_clause, [], [f109, f128, f124])).
fof(f109, plain, (~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ (aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx))), inference(negated_conjecture, [], [f19])).
fof(f19, plain, ~ (aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+1.p', m__)).