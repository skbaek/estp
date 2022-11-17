fof(f697, plain, $false, inference(avatar_sat_refutation, [], [f199, f204, f209, f219, f237, f263, f268, f274, f276, f286, f291, f292, f294, f295, f435, f440, f471, f681, f687, f696])).
fof(f696, plain, (~ spl15_4 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f695])).
fof(f695, plain, ($false | (~ spl15_4 | ~ spl15_36)), inference(subsumption_resolution, [], [f689, f138])).
fof(f138, plain, ~ aElementOf0(xx, xS), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ~ aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+2.p', m__679_02)).
fof(f689, plain, (aElementOf0(xx, xS) | (~ spl15_4 | ~ spl15_36)), inference(backward_demodulation, [], [f203, f676])).
fof(f676, plain, ((xx = sK13) | ~ spl15_36), inference(avatar_component_clause, [], [f674])).
fof(f674, plain, (spl15_36 <=> (xx = sK13)), introduced(avatar_definition, [new_symbols(naming, [spl15_36])])).
fof(f203, plain, (aElementOf0(sK13, xS) | ~ spl15_4), inference(avatar_component_clause, [], [f201])).
fof(f201, plain, (spl15_4 <=> aElementOf0(sK13, xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f687, plain, (~ spl15_4 | ~ spl15_16 | spl15_37), inference(avatar_contradiction_clause, [], [f686])).
fof(f686, plain, ($false | (~ spl15_4 | ~ spl15_16 | spl15_37)), inference(subsumption_resolution, [], [f685, f203])).
fof(f685, plain, (~ aElementOf0(sK13, xS) | (~ spl15_4 | ~ spl15_16 | spl15_37)), inference(subsumption_resolution, [], [f684, f375])).
fof(f375, plain, (aElement0(sK13) | ~ spl15_4), inference(subsumption_resolution, [], [f374, f137])).
fof(f137, plain, aSet0(xS), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (aSet0(xS) & aElement0(xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+2.p', m__679)).
fof(f374, plain, (aElement0(sK13) | ~ aSet0(xS) | ~ spl15_4), inference(resolution, [], [f203, f98])).
fof(f98, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+2.p', mEOfElem)).
fof(f684, plain, (~ aElement0(sK13) | ~ aElementOf0(sK13, xS) | (~ spl15_16 | spl15_37)), inference(resolution, [], [f680, f258])).
fof(f258, plain, (! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElement0(X0) | ~ aElementOf0(X0, xS)) | ~ spl15_16), inference(avatar_component_clause, [], [f257])).
fof(f257, plain, (spl15_16 <=> ! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElement0(X0) | ~ aElementOf0(X0, xS))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f680, plain, (~ aElementOf0(sK13, sdtpldt0(xS, xx)) | spl15_37), inference(avatar_component_clause, [], [f678])).
fof(f678, plain, (spl15_37 <=> aElementOf0(sK13, sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_37])])).
fof(f681, plain, (spl15_36 | ~ spl15_37 | spl15_3 | ~ spl15_10 | ~ spl15_18), inference(avatar_split_clause, [], [f668, f265, f230, f196, f678, f674])).
fof(f196, plain, (spl15_3 <=> aElementOf0(sK13, sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f230, plain, (spl15_10 <=> ! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aElement0(X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | (xx = X0))), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f265, plain, (spl15_18 <=> ! [X0] : (aElement0(X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f668, plain, (~ aElementOf0(sK13, sdtpldt0(xS, xx)) | (xx = sK13) | (spl15_3 | ~ spl15_10 | ~ spl15_18)), inference(resolution, [], [f296, f198])).
fof(f198, plain, (~ aElementOf0(sK13, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl15_3), inference(avatar_component_clause, [], [f196])).
fof(f296, plain, (! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | (xx = X0)) | (~ spl15_10 | ~ spl15_18)), inference(subsumption_resolution, [], [f231, f266])).
fof(f266, plain, (! [X0] : (~ aElementOf0(X0, sdtpldt0(xS, xx)) | aElement0(X0)) | ~ spl15_18), inference(avatar_component_clause, [], [f265])).
fof(f231, plain, (! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aElement0(X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | (xx = X0)) | ~ spl15_10), inference(avatar_component_clause, [], [f230])).
fof(f471, plain, (spl15_11 | ~ spl15_17 | spl15_20 | ~ spl15_21 | ~ spl15_26), inference(avatar_contradiction_clause, [], [f470])).
fof(f470, plain, ($false | (spl15_11 | ~ spl15_17 | spl15_20 | ~ spl15_21 | ~ spl15_26)), inference(subsumption_resolution, [], [f467, f236])).
fof(f236, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl15_11), inference(avatar_component_clause, [], [f234])).
fof(f234, plain, (spl15_11 <=> aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f467, plain, (aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl15_17 | spl15_20 | ~ spl15_21 | ~ spl15_26)), inference(backward_demodulation, [], [f290, f464])).
fof(f464, plain, ((xx = sK14) | (~ spl15_17 | spl15_20 | ~ spl15_26)), inference(subsumption_resolution, [], [f459, f285])).
fof(f285, plain, (~ aElementOf0(sK14, xS) | spl15_20), inference(avatar_component_clause, [], [f283])).
fof(f283, plain, (spl15_20 <=> aElementOf0(sK14, xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f459, plain, ((xx = sK14) | aElementOf0(sK14, xS) | (~ spl15_17 | ~ spl15_26)), inference(resolution, [], [f434, f262])).
fof(f262, plain, (! [X0] : (~ aElementOf0(X0, sdtpldt0(xS, xx)) | (xx = X0) | aElementOf0(X0, xS)) | ~ spl15_17), inference(avatar_component_clause, [], [f261])).
fof(f261, plain, (spl15_17 <=> ! [X0] : ((xx = X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | aElementOf0(X0, xS))), introduced(avatar_definition, [new_symbols(naming, [spl15_17])])).
fof(f434, plain, (aElementOf0(sK14, sdtpldt0(xS, xx)) | ~ spl15_26), inference(avatar_component_clause, [], [f432])).
fof(f432, plain, (spl15_26 <=> aElementOf0(sK14, sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_26])])).
fof(f290, plain, (aElementOf0(sK14, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl15_21), inference(avatar_component_clause, [], [f288])).
fof(f288, plain, (spl15_21 <=> aElementOf0(sK14, sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f440, plain, (~ spl15_8 | spl15_25), inference(avatar_contradiction_clause, [], [f439])).
fof(f439, plain, ($false | (~ spl15_8 | spl15_25)), inference(subsumption_resolution, [], [f438, f223])).
fof(f223, plain, (aSet0(sdtpldt0(xS, xx)) | ~ spl15_8), inference(avatar_component_clause, [], [f221])).
fof(f221, plain, (spl15_8 <=> aSet0(sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f438, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl15_25), inference(subsumption_resolution, [], [f437, f136])).
fof(f136, plain, aElement0(xx), inference(cnf_transformation, [], [f18])).
fof(f437, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(xS, xx)) | spl15_25), inference(resolution, [], [f430, f134])).
fof(f134, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f42, e50, e49])).
fof(f49, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e49])).
fof(e49, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f50, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e50])).
fof(e50, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f42, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f41])).
fof(f41, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+2.p', mDefDiff)).
fof(f430, plain, (~ sP3(sdtpldt0(xS, xx), xx) | spl15_25), inference(avatar_component_clause, [], [f428])).
fof(f428, plain, (spl15_25 <=> sP3(sdtpldt0(xS, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl15_25])])).
fof(f435, plain, (~ spl15_25 | spl15_26 | ~ spl15_21), inference(avatar_split_clause, [], [f424, f288, f432, f428])).
fof(f424, plain, (aElementOf0(sK14, sdtpldt0(xS, xx)) | ~ sP3(sdtpldt0(xS, xx), xx) | ~ spl15_21), inference(resolution, [], [f317, f290])).
fof(f317, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtmndt0(X1, X2)) | aElementOf0(X0, X1) | ~ sP3(X1, X2)), inference(resolution, [], [f127, f173])).
fof(f173, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f123])).
fof(f123, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f50])).
fof(f127, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | ~ aElementOf0(X4, X2) | aElementOf0(X4, X1)), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK12(X0, X1, X2) = X0) | ~ aElementOf0(sK12(X0, X1, X2), X1) | ~ aElement0(sK12(X0, X1, X2)) | ~ aElementOf0(sK12(X0, X1, X2), X2)) & ((~ (sK12(X0, X1, X2) = X0) & aElementOf0(sK12(X0, X1, X2), X1) & aElement0(sK12(X0, X1, X2))) | aElementOf0(sK12(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f77, f78])).
fof(f78, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK12(X0, X1, X2) = X0) | ~ aElementOf0(sK12(X0, X1, X2), X1) | ~ aElement0(sK12(X0, X1, X2)) | ~ aElementOf0(sK12(X0, X1, X2), X2)) & ((~ (sK12(X0, X1, X2) = X0) & aElementOf0(sK12(X0, X1, X2), X1) & aElement0(sK12(X0, X1, X2))) | aElementOf0(sK12(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f77, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f76])).
fof(f76, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f75])).
fof(f75, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f49])).
fof(f295, plain, (spl15_1 | spl15_8), inference(avatar_split_clause, [], [f162, f221, f187])).
fof(f187, plain, (spl15_1 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f162, plain, (aSet0(sdtpldt0(xS, xx)) | sP8), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & (~ aElementOf0(sK14, xS) & aElementOf0(sK14, sdtmndt0(sdtpldt0(xS, xx), xx))) & sP7 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP6 & aSet0(sdtpldt0(xS, xx))) | sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f95, f96])).
fof(f96, plain, (? [X0] : (~ aElementOf0(X0, xS) & aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx))) => (~ aElementOf0(sK14, xS) & aElementOf0(sK14, sdtmndt0(sdtpldt0(xS, xx), xx)))), introduced(choice_axiom, [])).
fof(f95, plain, ((~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & ? [X0] : (~ aElementOf0(X0, xS) & aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx))) & sP7 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP6 & aSet0(sdtpldt0(xS, xx))) | sP8), inference(rectify, [], [f57])).
fof(f57, plain, ((~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & ? [X2] : (~ aElementOf0(X2, xS) & aElementOf0(X2, sdtmndt0(sdtpldt0(xS, xx), xx))) & sP7 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP6 & aSet0(sdtpldt0(xS, xx))) | sP8), inference(definition_folding, [], [f45, e56, e55, e54, e53, e52])).
fof(f52, plain, (! [X3] : (aElementOf0(X3, sdtpldt0(xS, xx)) <=> (((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3))) | ~ sP4), inference(usedef, [], [e52])).
fof(e52, plain, (sP4 <=> ! [X3] : (aElementOf0(X3, sdtpldt0(xS, xx)) <=> (((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f53, plain, (! [X4] : (aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4))) | ~ sP5), inference(usedef, [], [e53])).
fof(e53, plain, (sP5 <=> ! [X4] : (aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f54, plain, (! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) | ~ sP6), inference(usedef, [], [e54])).
fof(e54, plain, (sP6 <=> ! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f55, plain, (! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) | ~ sP7), inference(usedef, [], [e55])).
fof(e55, plain, (sP7 <=> ! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f56, plain, ((~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X5, xS)) & sP5 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP4 & aSet0(sdtpldt0(xS, xx))) | ~ sP8), inference(usedef, [], [e56])).
fof(e56, plain, (sP8 <=> (~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X5, xS)) & sP5 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP4 & aSet0(sdtpldt0(xS, xx)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f45, plain, ((~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & ? [X2] : (~ aElementOf0(X2, xS) & aElementOf0(X2, sdtmndt0(sdtpldt0(xS, xx), xx))) & ! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) | (~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X5, xS)) & ! [X4] : (aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X3] : (aElementOf0(X3, sdtpldt0(xS, xx)) <=> (((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3))) & aSet0(sdtpldt0(xS, xx)))), inference(flattening, [], [f44])).
fof(f44, plain, ((((~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & ? [X2] : (~ aElementOf0(X2, xS) & aElementOf0(X2, sdtmndt0(sdtpldt0(xS, xx), xx)))) & (! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)))) & (! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx)))) | (((~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X5, xS))) & (! [X4] : (aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)))) & (! [X3] : (aElementOf0(X3, sdtpldt0(xS, xx)) <=> (((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3))) & aSet0(sdtpldt0(xS, xx))))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ (((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ! [X2] : (aElementOf0(X2, sdtmndt0(sdtpldt0(xS, xx), xx)) => aElementOf0(X2, xS))))) & ((! [X3] : (aElementOf0(X3, sdtpldt0(xS, xx)) <=> (((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3))) & aSet0(sdtpldt0(xS, xx))) => ((! [X4] : (aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ! [X5] : (aElementOf0(X5, xS) => aElementOf0(X5, sdtmndt0(sdtpldt0(xS, xx), xx))))))), inference(rectify, [], [f21])).
fof(f21, plain, ~ (((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) => aElementOf0(X0, xS))))) & ((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ! [X0] : (aElementOf0(X0, xS) => aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx))))))), inference(negated_conjecture, [], [f20])).
fof(f20, plain, ~ (((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) => aElementOf0(X0, xS))))) & ((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ! [X0] : (aElementOf0(X0, xS) => aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx))))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+2.p', m__)).
fof(f294, plain, (spl15_1 | spl15_14), inference(avatar_split_clause, [], [f163, f248, f187])).
fof(f248, plain, (spl15_14 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f163, plain, (sP6 | sP8), inference(cnf_transformation, [], [f97])).
fof(f292, plain, (spl15_1 | spl15_9), inference(avatar_split_clause, [], [f165, f226, f187])).
fof(f226, plain, (spl15_9 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f165, plain, (sP7 | sP8), inference(cnf_transformation, [], [f97])).
fof(f291, plain, (spl15_1 | spl15_21), inference(avatar_split_clause, [], [f166, f288, f187])).
fof(f166, plain, (aElementOf0(sK14, sdtmndt0(sdtpldt0(xS, xx), xx)) | sP8), inference(cnf_transformation, [], [f97])).
fof(f286, plain, (spl15_1 | ~ spl15_20), inference(avatar_split_clause, [], [f167, f283, f187])).
fof(f167, plain, (~ aElementOf0(sK14, xS) | sP8), inference(cnf_transformation, [], [f97])).
fof(f276, plain, (~ spl15_7 | spl15_18), inference(avatar_split_clause, [], [f158, f265, f216])).
fof(f216, plain, (spl15_7 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f158, plain, ! [X0] : (aElement0(X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | ~ sP4), inference(cnf_transformation, [], [f94])).
fof(f94, plain, (! [X0] : ((aElementOf0(X0, sdtpldt0(xS, xx)) | (~ (xx = X0) & ~ aElementOf0(X0, xS)) | ~ aElement0(X0)) & ((((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(xS, xx)))) | ~ sP4), inference(rectify, [], [f93])).
fof(f93, plain, (! [X3] : ((aElementOf0(X3, sdtpldt0(xS, xx)) | (~ (xx = X3) & ~ aElementOf0(X3, xS)) | ~ aElement0(X3)) & ((((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3)) | ~ aElementOf0(X3, sdtpldt0(xS, xx)))) | ~ sP4), inference(flattening, [], [f92])).
fof(f92, plain, (! [X3] : ((aElementOf0(X3, sdtpldt0(xS, xx)) | ((~ (xx = X3) & ~ aElementOf0(X3, xS)) | ~ aElement0(X3))) & ((((xx = X3) | aElementOf0(X3, xS)) & aElement0(X3)) | ~ aElementOf0(X3, sdtpldt0(xS, xx)))) | ~ sP4), inference(nnf_transformation, [], [f52])).
fof(f274, plain, (~ spl15_7 | spl15_16), inference(avatar_split_clause, [], [f160, f257, f216])).
fof(f160, plain, ! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElementOf0(X0, xS) | ~ aElement0(X0) | ~ sP4), inference(cnf_transformation, [], [f94])).
fof(f268, plain, (~ spl15_5 | spl15_10), inference(avatar_split_clause, [], [f157, f230, f206])).
fof(f206, plain, (spl15_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f157, plain, ! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElement0(X0) | ~ sP5), inference(cnf_transformation, [], [f91])).
fof(f91, plain, (! [X0] : ((aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElement0(X0)) & ((~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0)) | ~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)))) | ~ sP5), inference(rectify, [], [f90])).
fof(f90, plain, (! [X4] : ((aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X4) | ~ aElementOf0(X4, sdtpldt0(xS, xx)) | ~ aElement0(X4)) & ((~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4)) | ~ aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)))) | ~ sP5), inference(flattening, [], [f89])).
fof(f89, plain, (! [X4] : ((aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)) | ((xx = X4) | ~ aElementOf0(X4, sdtpldt0(xS, xx)) | ~ aElement0(X4))) & ((~ (xx = X4) & aElementOf0(X4, sdtpldt0(xS, xx)) & aElement0(X4)) | ~ aElementOf0(X4, sdtmndt0(sdtpldt0(xS, xx), xx)))) | ~ sP5), inference(nnf_transformation, [], [f53])).
fof(f263, plain, (~ spl15_14 | spl15_17), inference(avatar_split_clause, [], [f151, f261, f248])).
fof(f151, plain, ! [X0] : ((xx = X0) | aElementOf0(X0, xS) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | ~ sP6), inference(cnf_transformation, [], [f88])).
fof(f88, plain, (! [X0] : ((aElementOf0(X0, sdtpldt0(xS, xx)) | (~ (xx = X0) & ~ aElementOf0(X0, xS)) | ~ aElement0(X0)) & ((((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(xS, xx)))) | ~ sP6), inference(flattening, [], [f87])).
fof(f87, plain, (! [X0] : ((aElementOf0(X0, sdtpldt0(xS, xx)) | ((~ (xx = X0) & ~ aElementOf0(X0, xS)) | ~ aElement0(X0))) & ((((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(xS, xx)))) | ~ sP6), inference(nnf_transformation, [], [f54])).
fof(f237, plain, (~ spl15_9 | ~ spl15_11), inference(avatar_split_clause, [], [f175, f234, f226])).
fof(f175, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ sP7), inference(equality_resolution, [], [f148])).
fof(f148, plain, ! [X0] : (~ (xx = X0) | ~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ sP7), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (! [X0] : ((aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElement0(X0)) & ((~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0)) | ~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)))) | ~ sP7), inference(rectify, [], [f85])).
fof(f85, plain, (! [X1] : ((aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X1) | ~ aElementOf0(X1, sdtpldt0(xS, xx)) | ~ aElement0(X1)) & ((~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1)) | ~ aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)))) | ~ sP7), inference(flattening, [], [f84])).
fof(f84, plain, (! [X1] : ((aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) | ((xx = X1) | ~ aElementOf0(X1, sdtpldt0(xS, xx)) | ~ aElement0(X1))) & ((~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1)) | ~ aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)))) | ~ sP7), inference(nnf_transformation, [], [f55])).
fof(f219, plain, (~ spl15_1 | spl15_7), inference(avatar_split_clause, [], [f140, f216, f187])).
fof(f140, plain, (sP4 | ~ sP8), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & (~ aElementOf0(sK13, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(sK13, xS)) & sP5 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP4 & aSet0(sdtpldt0(xS, xx))) | ~ sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f81, f82])).
fof(f82, plain, (? [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X0, xS)) => (~ aElementOf0(sK13, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(sK13, xS))), introduced(choice_axiom, [])).
fof(f81, plain, ((~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & ? [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X0, xS)) & sP5 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP4 & aSet0(sdtpldt0(xS, xx))) | ~ sP8), inference(rectify, [], [f80])).
fof(f80, plain, ((~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtmndt0(sdtpldt0(xS, xx), xx)) & aElementOf0(X5, xS)) & sP5 & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & sP4 & aSet0(sdtpldt0(xS, xx))) | ~ sP8), inference(nnf_transformation, [], [f56])).
fof(f209, plain, (~ spl15_1 | spl15_5), inference(avatar_split_clause, [], [f142, f206, f187])).
fof(f142, plain, (sP5 | ~ sP8), inference(cnf_transformation, [], [f83])).
fof(f204, plain, (~ spl15_1 | spl15_4), inference(avatar_split_clause, [], [f143, f201, f187])).
fof(f143, plain, (aElementOf0(sK13, xS) | ~ sP8), inference(cnf_transformation, [], [f83])).
fof(f199, plain, (~ spl15_1 | ~ spl15_3), inference(avatar_split_clause, [], [f144, f196, f187])).
fof(f144, plain, (~ aElementOf0(sK13, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ sP8), inference(cnf_transformation, [], [f83])).