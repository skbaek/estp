fof(f511798, plain, $false, inference(avatar_sat_refutation, [], [f258, f404, f718, f896, f1009, f1023, f1982, f2047, f6166, f6216, f6379, f6653, f7626, f23893, f24670, f207777, f231279, f231355, f247546, f247788, f248200, f248298, f375400, f510987, f511543])).
fof(f511543, plain, (spl8_879 | ~ spl8_1884 | ~ spl8_2128), inference(avatar_split_clause, [], [f248252, f207774, f186549, f30838])).
fof(f30838, plain, (spl8_879 <=> (xS = sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_879])])).
fof(f186549, plain, (spl8_1884 <=> (xS = sdtpldt0(xS, sK4(xS)))), introduced(avatar_definition, [new_symbols(naming, [spl8_1884])])).
fof(f207774, plain, (spl8_2128 <=> (sdtpldt0(xS, sK4(xS)) = sdtmndt0(sdtpldt0(xS, sK4(xS)), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_2128])])).
fof(f248252, plain, ((xS = sdtmndt0(xS, xx)) | (~ spl8_1884 | ~ spl8_2128)), inference(backward_demodulation, [], [f207776, f186551])).
fof(f186551, plain, ((xS = sdtpldt0(xS, sK4(xS))) | ~ spl8_1884), inference(avatar_component_clause, [], [f186549])).
fof(f207776, plain, ((sdtpldt0(xS, sK4(xS)) = sdtmndt0(sdtpldt0(xS, sK4(xS)), xx)) | ~ spl8_2128), inference(avatar_component_clause, [], [f207774])).
fof(f510987, plain, (~ spl8_6 | ~ spl8_12 | spl8_39 | ~ spl8_96 | ~ spl8_879 | ~ spl8_4875), inference(avatar_contradiction_clause, [], [f510986])).
fof(f510986, plain, ($false | (~ spl8_6 | ~ spl8_12 | spl8_39 | ~ spl8_96 | ~ spl8_879 | ~ spl8_4875)), inference(subsumption_resolution, [], [f510985, f1678])).
fof(f1678, plain, (~ (xS = sdtpldt0(xS, xx)) | spl8_39), inference(avatar_component_clause, [], [f1677])).
fof(f1677, plain, (spl8_39 <=> (xS = sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_39])])).
fof(f510985, plain, ((xS = sdtpldt0(xS, xx)) | (~ spl8_6 | ~ spl8_12 | ~ spl8_96 | ~ spl8_879 | ~ spl8_4875)), inference(forward_demodulation, [], [f510980, f30840])).
fof(f30840, plain, ((xS = sdtmndt0(xS, xx)) | ~ spl8_879), inference(avatar_component_clause, [], [f30838])).
fof(f510980, plain, ((xS = sdtpldt0(sdtmndt0(xS, xx), xx)) | (~ spl8_6 | ~ spl8_12 | ~ spl8_96 | ~ spl8_4875)), inference(backward_demodulation, [], [f6461, f510979])).
fof(f510979, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (~ spl8_6 | ~ spl8_12 | ~ spl8_96 | ~ spl8_4875)), inference(subsumption_resolution, [], [f510978, f393])).
fof(f393, plain, (sP1(xS, xx) | ~ spl8_12), inference(avatar_component_clause, [], [f392])).
fof(f392, plain, (spl8_12 <=> sP1(xS, xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_12])])).
fof(f510978, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (~ spl8_6 | ~ spl8_96 | ~ spl8_4875)), inference(subsumption_resolution, [], [f510977, f369927])).
fof(f369927, plain, (aSet0(xS) | ~ spl8_4875), inference(avatar_component_clause, [], [f369926])).
fof(f369926, plain, (spl8_4875 <=> aSet0(xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_4875])])).
fof(f510977, plain, (~ aSet0(xS) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (~ spl8_6 | ~ spl8_96)), inference(subsumption_resolution, [], [f510975, f226])).
fof(f226, plain, (sP3(sdtpldt0(xS, xx), xx) | ~ spl8_6), inference(avatar_component_clause, [], [f225])).
fof(f225, plain, (spl8_6 <=> sP3(sdtpldt0(xS, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_6])])).
fof(f510975, plain, (~ sP3(sdtpldt0(xS, xx), xx) | ~ aSet0(xS) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | ~ spl8_96), inference(resolution, [], [f181212, f6404])).
fof(f6404, plain, (~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_96), inference(subsumption_resolution, [], [f6403, f116])).
fof(f116, plain, aSet0(xS), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (aSet0(xS) & aElement0(xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', m__679)).
fof(f6403, plain, (~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ spl8_96), inference(subsumption_resolution, [], [f6395, f128])).
fof(f128, plain, ~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X0] : ((aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X0) | ~ aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aElement0(X0)) & ((~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0)) | ~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X1] : ((aElementOf0(X1, sdtpldt0(xS, xx)) | (~ (xx = X1) & ~ aElementOf0(X1, xS)) | ~ aElement0(X1)) & ((((xx = X1) | aElementOf0(X1, xS)) & aElement0(X1)) | ~ aElementOf0(X1, sdtpldt0(xS, xx)))) & aSet0(sdtpldt0(xS, xx))), inference(rectify, [], [f75])).
fof(f75, plain, (~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X1] : ((aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) | (xx = X1) | ~ aElementOf0(X1, sdtpldt0(xS, xx)) | ~ aElement0(X1)) & ((~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1)) | ~ aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X0] : ((aElementOf0(X0, sdtpldt0(xS, xx)) | (~ (xx = X0) & ~ aElementOf0(X0, xS)) | ~ aElement0(X0)) & ((((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(xS, xx)))) & aSet0(sdtpldt0(xS, xx))), inference(flattening, [], [f74])).
fof(f74, plain, (~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X1] : ((aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) | ((xx = X1) | ~ aElementOf0(X1, sdtpldt0(xS, xx)) | ~ aElement0(X1))) & ((~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1)) | ~ aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X0] : ((aElementOf0(X0, sdtpldt0(xS, xx)) | ((~ (xx = X0) & ~ aElementOf0(X0, xS)) | ~ aElement0(X0))) & ((((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(xS, xx)))) & aSet0(sdtpldt0(xS, xx))), inference(nnf_transformation, [], [f45])).
fof(f45, plain, (~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) & ! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))), inference(flattening, [], [f44])).
fof(f44, plain, ((~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)) & (! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)))) & (! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X1] : (aElementOf0(X1, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X1) & aElementOf0(X1, sdtpldt0(xS, xx)) & aElement0(X1))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (xS = sdtmndt0(sdtpldt0(xS, xx), xx)))), inference(rectify, [], [f21])).
fof(f21, plain, ~ ((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (xS = sdtmndt0(sdtpldt0(xS, xx), xx)))), inference(negated_conjecture, [], [f20])).
fof(f20, plain, ~ ((! [X0] : (aElementOf0(X0, sdtpldt0(xS, xx)) <=> (((xx = X0) | aElementOf0(X0, xS)) & aElement0(X0))) & aSet0(sdtpldt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) <=> (~ (xx = X0) & aElementOf0(X0, sdtpldt0(xS, xx)) & aElement0(X0))) & aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))) => (xS = sdtmndt0(sdtpldt0(xS, xx), xx)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', m__)).
fof(f6395, plain, ((xS = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ spl8_96), inference(resolution, [], [f6215, f137])).
fof(f137, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | (X0 = X1) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f88, f82])).
fof(f82, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f59, f60])).
fof(f60, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f59, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f58])).
fof(f58, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f57])).
fof(f57, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mDefSub)).
fof(f88, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f35])).
fof(f35, plain, ! [X0, X1] : (((X0 = X1) | (~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X0) & aSubsetOf0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mSubASymm)).
fof(f6215, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ spl8_96), inference(avatar_component_clause, [], [f6213])).
fof(f6213, plain, (spl8_96 <=> aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_96])])).
fof(f181212, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ sP3(sdtpldt0(X0, X1), X2) | ~ aSet0(X0) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1)), inference(subsumption_resolution, [], [f181211, f161])).
fof(f161, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f133, f104])).
fof(f104, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f71, f72])).
fof(f72, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f71, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f70])).
fof(f70, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f69])).
fof(f69, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f49])).
fof(f49, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e49])).
fof(e49, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f133, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f102])).
fof(f102, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f50])).
fof(f50, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e50])).
fof(e50, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f181211, plain, ! [X2, X0, X1] : (~ sP3(sdtpldt0(X0, X1), X2) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(subsumption_resolution, [], [f181210, f240])).
fof(f240, plain, ! [X0, X1] : (aElement0(sK5(X1, X0)) | ~ aSet0(X0) | ~ aSet0(X1) | aSubsetOf0(X0, X1)), inference(duplicate_literal_removal, [], [f233])).
fof(f233, plain, ! [X0, X1] : (aSubsetOf0(X0, X1) | ~ aSet0(X0) | ~ aSet0(X1) | aElement0(sK5(X1, X0)) | ~ aSet0(X0)), inference(resolution, [], [f84, f77])).
fof(f77, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mEOfElem)).
fof(f84, plain, ! [X0, X1] : (aElementOf0(sK5(X0, X1), X1) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f61])).
fof(f181210, plain, ! [X2, X0, X1] : (~ sP3(sdtpldt0(X0, X1), X2) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0)) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(duplicate_literal_removal, [], [f181125])).
fof(f181125, plain, ! [X2, X0, X1] : (~ sP3(sdtpldt0(X0, X1), X2) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0)) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(resolution, [], [f5769, f84])).
fof(f5769, plain, ! [X12, X10, X13, X11] : (~ aElementOf0(sK5(sdtmndt0(sdtpldt0(X10, X11), X12), X13), X10) | ~ sP3(sdtpldt0(X10, X11), X12) | aSubsetOf0(X13, sdtmndt0(sdtpldt0(X10, X11), X12)) | ~ aSet0(X13) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X10, X11), X12), X13)) | (sK5(sdtmndt0(sdtpldt0(X10, X11), X12), X13) = X12) | ~ sP1(X10, X11)), inference(resolution, [], [f1461, f313])).
fof(f313, plain, ! [X2, X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X2)) | ~ aElement0(X0) | ~ aElementOf0(X0, X1) | ~ sP1(X1, X2)), inference(resolution, [], [f95, f131])).
fof(f131, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f90])).
fof(f90, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1] : (! [X2] : (((sdtpldt0(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f47])).
fof(f47, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e47])).
fof(e47, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f95, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f65, f66])).
fof(f66, plain, ! [X2, X1, X0] : (? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) => (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f65, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f64])).
fof(f64, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : (((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f63])).
fof(f63, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3))) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f46])).
fof(f46, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e46])).
fof(e46, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f1461, plain, ! [X17, X18, X16] : (~ aElementOf0(sK5(sdtmndt0(X16, X17), X18), X16) | (sK5(sdtmndt0(X16, X17), X18) = X17) | ~ sP3(X16, X17) | aSubsetOf0(X18, sdtmndt0(X16, X17)) | ~ aSet0(X18)), inference(subsumption_resolution, [], [f1460, f161])).
fof(f1460, plain, ! [X17, X18, X16] : (~ aElementOf0(sK5(sdtmndt0(X16, X17), X18), X16) | (sK5(sdtmndt0(X16, X17), X18) = X17) | ~ sP3(X16, X17) | aSubsetOf0(X18, sdtmndt0(X16, X17)) | ~ aSet0(X18) | ~ aSet0(sdtmndt0(X16, X17))), inference(subsumption_resolution, [], [f1450, f240])).
fof(f1450, plain, ! [X17, X18, X16] : (~ aElementOf0(sK5(sdtmndt0(X16, X17), X18), X16) | ~ aElement0(sK5(sdtmndt0(X16, X17), X18)) | (sK5(sdtmndt0(X16, X17), X18) = X17) | ~ sP3(X16, X17) | aSubsetOf0(X18, sdtmndt0(X16, X17)) | ~ aSet0(X18) | ~ aSet0(sdtmndt0(X16, X17))), inference(resolution, [], [f461, f85])).
fof(f85, plain, ! [X0, X1] : (~ aElementOf0(sK5(X0, X1), X0) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f61])).
fof(f461, plain, ! [X2, X0, X1] : (aElementOf0(X1, sdtmndt0(X2, X0)) | ~ aElementOf0(X1, X2) | ~ aElement0(X1) | (X0 = X1) | ~ sP3(X2, X0)), inference(resolution, [], [f108, f133])).
fof(f108, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f73])).
fof(f6461, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ spl8_96), inference(subsumption_resolution, [], [f6460, f123])).
fof(f123, plain, aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)), inference(cnf_transformation, [], [f76])).
fof(f6460, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_96), inference(subsumption_resolution, [], [f6456, f116])).
fof(f6456, plain, (~ aSet0(xS) | (xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_96), inference(resolution, [], [f6404, f330])).
fof(f330, plain, ! [X4, X5] : (aSubsetOf0(X4, X5) | ~ aSet0(X4) | (sdtpldt0(sdtmndt0(X4, sK5(X5, X4)), sK5(X5, X4)) = X4) | ~ aSet0(X5)), inference(duplicate_literal_removal, [], [f329])).
fof(f329, plain, ! [X4, X5] : ((sdtpldt0(sdtmndt0(X4, sK5(X5, X4)), sK5(X5, X4)) = X4) | ~ aSet0(X4) | aSubsetOf0(X4, X5) | ~ aSet0(X4) | ~ aSet0(X5)), inference(resolution, [], [f114, f84])).
fof(f114, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | (sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (! [X1] : ((sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => (sdtpldt0(sdtmndt0(X0, X1), X1) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mConsDiff)).
fof(f375400, plain, spl8_4875, inference(avatar_split_clause, [], [f116, f369926])).
fof(f248298, plain, (~ spl8_94 | spl8_453 | ~ spl8_1884), inference(avatar_contradiction_clause, [], [f248297])).
fof(f248297, plain, ($false | (~ spl8_94 | spl8_453 | ~ spl8_1884)), inference(subsumption_resolution, [], [f248202, f6129])).
fof(f6129, plain, (aSubsetOf0(xS, xS) | ~ spl8_94), inference(avatar_component_clause, [], [f6128])).
fof(f6128, plain, (spl8_94 <=> aSubsetOf0(xS, xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_94])])).
fof(f248202, plain, (~ aSubsetOf0(xS, xS) | (spl8_453 | ~ spl8_1884)), inference(backward_demodulation, [], [f23861, f186551])).
fof(f23861, plain, (~ aSubsetOf0(sdtpldt0(xS, sK4(xS)), xS) | spl8_453), inference(avatar_component_clause, [], [f23860])).
fof(f23860, plain, (spl8_453 <=> aSubsetOf0(sdtpldt0(xS, sK4(xS)), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_453])])).
fof(f248200, plain, (spl8_1884 | ~ spl8_48 | ~ spl8_469 | ~ spl8_1883), inference(avatar_split_clause, [], [f248199, f186545, f24107, f1979, f186549])).
fof(f1979, plain, (spl8_48 <=> aElementOf0(sK4(xS), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_48])])).
fof(f24107, plain, (spl8_469 <=> sP1(xS, sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_469])])).
fof(f186545, plain, (spl8_1883 <=> (sK4(xS) = sK6(sK4(xS), xS, xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_1883])])).
fof(f248199, plain, ((xS = sdtpldt0(xS, sK4(xS))) | (~ spl8_48 | ~ spl8_469 | ~ spl8_1883)), inference(subsumption_resolution, [], [f248192, f24108])).
fof(f24108, plain, (sP1(xS, sK4(xS)) | ~ spl8_469), inference(avatar_component_clause, [], [f24107])).
fof(f248192, plain, ((xS = sdtpldt0(xS, sK4(xS))) | ~ sP1(xS, sK4(xS)) | (~ spl8_48 | ~ spl8_1883)), inference(resolution, [], [f248066, f91])).
fof(f91, plain, ! [X2, X0, X1] : (~ sP0(X1, X0, X2) | (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f62])).
fof(f248066, plain, (sP0(sK4(xS), xS, xS) | (~ spl8_48 | ~ spl8_1883)), inference(subsumption_resolution, [], [f248065, f116])).
fof(f248065, plain, (sP0(sK4(xS), xS, xS) | ~ aSet0(xS) | (~ spl8_48 | ~ spl8_1883)), inference(subsumption_resolution, [], [f248064, f1981])).
fof(f1981, plain, (aElementOf0(sK4(xS), xS) | ~ spl8_48), inference(avatar_component_clause, [], [f1979])).
fof(f248064, plain, (sP0(sK4(xS), xS, xS) | ~ aElementOf0(sK4(xS), xS) | ~ aSet0(xS) | ~ spl8_1883), inference(trivial_inequality_removal, [], [f248057])).
fof(f248057, plain, (~ (sK4(xS) = sK4(xS)) | sP0(sK4(xS), xS, xS) | ~ aElementOf0(sK4(xS), xS) | ~ aSet0(xS) | ~ spl8_1883), inference(superposition, [], [f139, f186547])).
fof(f186547, plain, ((sK4(xS) = sK6(sK4(xS), xS, xS)) | ~ spl8_1883), inference(avatar_component_clause, [], [f186545])).
fof(f139, plain, ! [X2, X0, X1] : (~ (sK6(X0, X1, X2) = X0) | sP0(X0, X1, X2) | ~ aElementOf0(sK6(X0, X1, X2), X2) | ~ aSet0(X2)), inference(subsumption_resolution, [], [f100, f77])).
fof(f100, plain, ! [X2, X0, X1] : (sP0(X0, X1, X2) | ~ (sK6(X0, X1, X2) = X0) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2) | ~ aSet0(X2)), inference(cnf_transformation, [], [f67])).
fof(f247788, plain, (~ spl8_453 | ~ spl8_469 | spl8_1884), inference(avatar_contradiction_clause, [], [f247787])).
fof(f247787, plain, ($false | (~ spl8_453 | ~ spl8_469 | spl8_1884)), inference(subsumption_resolution, [], [f247786, f24108])).
fof(f247786, plain, (~ sP1(xS, sK4(xS)) | (~ spl8_453 | spl8_1884)), inference(subsumption_resolution, [], [f247785, f186550])).
fof(f186550, plain, (~ (xS = sdtpldt0(xS, sK4(xS))) | spl8_1884), inference(avatar_component_clause, [], [f186549])).
fof(f247785, plain, ((xS = sdtpldt0(xS, sK4(xS))) | ~ sP1(xS, sK4(xS)) | ~ spl8_453), inference(subsumption_resolution, [], [f247768, f116])).
fof(f247768, plain, (~ aSet0(xS) | (xS = sdtpldt0(xS, sK4(xS))) | ~ sP1(xS, sK4(xS)) | ~ spl8_453), inference(resolution, [], [f23862, f3146])).
fof(f3146, plain, ! [X8, X7] : (~ aSubsetOf0(sdtpldt0(X7, X8), X7) | ~ aSet0(X7) | (sdtpldt0(X7, X8) = X7) | ~ sP1(X7, X8)), inference(subsumption_resolution, [], [f3137, f82])).
fof(f3137, plain, ! [X8, X7] : (~ sP1(X7, X8) | ~ aSet0(X7) | (sdtpldt0(X7, X8) = X7) | ~ aSubsetOf0(sdtpldt0(X7, X8), X7) | ~ aSet0(sdtpldt0(X7, X8))), inference(resolution, [], [f2643, f137])).
fof(f2643, plain, ! [X0, X1] : (aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f2642, f157])).
fof(f157, plain, ! [X4, X3] : (aSet0(sdtpldt0(X3, X4)) | ~ sP1(X3, X4)), inference(resolution, [], [f131, f92])).
fof(f92, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f67])).
fof(f2642, plain, ! [X0, X1] : (~ sP1(X0, X1) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(duplicate_literal_removal, [], [f2626])).
fof(f2626, plain, ! [X0, X1] : (~ sP1(X0, X1) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(resolution, [], [f1352, f84])).
fof(f1352, plain, ! [X12, X13, X11] : (~ aElementOf0(sK5(sdtpldt0(X11, X12), X13), X11) | ~ sP1(X11, X12) | aSubsetOf0(X13, sdtpldt0(X11, X12)) | ~ aSet0(X13)), inference(subsumption_resolution, [], [f1351, f157])).
fof(f1351, plain, ! [X12, X13, X11] : (~ aElementOf0(sK5(sdtpldt0(X11, X12), X13), X11) | ~ sP1(X11, X12) | aSubsetOf0(X13, sdtpldt0(X11, X12)) | ~ aSet0(X13) | ~ aSet0(sdtpldt0(X11, X12))), inference(subsumption_resolution, [], [f1345, f240])).
fof(f1345, plain, ! [X12, X13, X11] : (~ aElement0(sK5(sdtpldt0(X11, X12), X13)) | ~ aElementOf0(sK5(sdtpldt0(X11, X12), X13), X11) | ~ sP1(X11, X12) | aSubsetOf0(X13, sdtpldt0(X11, X12)) | ~ aSet0(X13) | ~ aSet0(sdtpldt0(X11, X12))), inference(resolution, [], [f313, f85])).
fof(f23862, plain, (aSubsetOf0(sdtpldt0(xS, sK4(xS)), xS) | ~ spl8_453), inference(avatar_component_clause, [], [f23860])).
fof(f247546, plain, (spl8_1883 | ~ spl8_452 | spl8_453 | ~ spl8_469), inference(avatar_split_clause, [], [f247545, f24107, f23860, f23856, f186545])).
fof(f23856, plain, (spl8_452 <=> aSet0(sdtpldt0(xS, sK4(xS)))), introduced(avatar_definition, [new_symbols(naming, [spl8_452])])).
fof(f247545, plain, ((sK4(xS) = sK6(sK4(xS), xS, xS)) | (~ spl8_452 | spl8_453 | ~ spl8_469)), inference(forward_demodulation, [], [f247544, f247540])).
fof(f247540, plain, ((sK4(xS) = sK5(xS, sdtpldt0(xS, sK4(xS)))) | (spl8_453 | ~ spl8_469)), inference(subsumption_resolution, [], [f247539, f116])).
fof(f247539, plain, ((sK4(xS) = sK5(xS, sdtpldt0(xS, sK4(xS)))) | ~ aSet0(xS) | (spl8_453 | ~ spl8_469)), inference(subsumption_resolution, [], [f247532, f24108])).
fof(f247532, plain, (~ sP1(xS, sK4(xS)) | (sK4(xS) = sK5(xS, sdtpldt0(xS, sK4(xS)))) | ~ aSet0(xS) | spl8_453), inference(resolution, [], [f23861, f5713])).
fof(f5713, plain, ! [X0, X1] : (aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ sP1(X0, X1) | (sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f5712, f157])).
fof(f5712, plain, ! [X0, X1] : ((sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(duplicate_literal_removal, [], [f5683])).
fof(f5683, plain, ! [X0, X1] : ((sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(X0) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(sdtpldt0(X0, X1)) | ~ aSet0(X0)), inference(resolution, [], [f1401, f85])).
fof(f1401, plain, ! [X10, X8, X9] : (aElementOf0(sK5(X8, sdtpldt0(X9, X10)), X9) | (sK5(X8, sdtpldt0(X9, X10)) = X10) | ~ sP1(X9, X10) | aSubsetOf0(sdtpldt0(X9, X10), X8) | ~ aSet0(X8)), inference(subsumption_resolution, [], [f1383, f157])).
fof(f1383, plain, ! [X10, X8, X9] : (aElementOf0(sK5(X8, sdtpldt0(X9, X10)), X9) | (sK5(X8, sdtpldt0(X9, X10)) = X10) | ~ sP1(X9, X10) | aSubsetOf0(sdtpldt0(X9, X10), X8) | ~ aSet0(sdtpldt0(X9, X10)) | ~ aSet0(X8)), inference(resolution, [], [f400, f84])).
fof(f400, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtpldt0(X1, X2)) | aElementOf0(X0, X1) | (X0 = X2) | ~ sP1(X1, X2)), inference(resolution, [], [f94, f131])).
fof(f94, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | aElementOf0(X4, X1) | ~ aElementOf0(X4, X2) | (X0 = X4)), inference(cnf_transformation, [], [f67])).
fof(f247544, plain, ((sK5(xS, sdtpldt0(xS, sK4(xS))) = sK6(sK5(xS, sdtpldt0(xS, sK4(xS))), xS, xS)) | (~ spl8_452 | spl8_453)), inference(subsumption_resolution, [], [f247543, f23857])).
fof(f23857, plain, (aSet0(sdtpldt0(xS, sK4(xS))) | ~ spl8_452), inference(avatar_component_clause, [], [f23856])).
fof(f247543, plain, ((sK5(xS, sdtpldt0(xS, sK4(xS))) = sK6(sK5(xS, sdtpldt0(xS, sK4(xS))), xS, xS)) | ~ aSet0(sdtpldt0(xS, sK4(xS))) | spl8_453), inference(subsumption_resolution, [], [f247534, f116])).
fof(f247534, plain, ((sK5(xS, sdtpldt0(xS, sK4(xS))) = sK6(sK5(xS, sdtpldt0(xS, sK4(xS))), xS, xS)) | ~ aSet0(xS) | ~ aSet0(sdtpldt0(xS, sK4(xS))) | spl8_453), inference(resolution, [], [f23861, f2383])).
fof(f2383, plain, ! [X28, X27] : (aSubsetOf0(X28, X27) | (sK5(X27, X28) = sK6(sK5(X27, X28), X27, X27)) | ~ aSet0(X27) | ~ aSet0(X28)), inference(subsumption_resolution, [], [f2366, f240])).
fof(f2366, plain, ! [X28, X27] : (~ aSet0(X27) | ~ aElement0(sK5(X27, X28)) | (sK5(X27, X28) = sK6(sK5(X27, X28), X27, X27)) | aSubsetOf0(X28, X27) | ~ aSet0(X28)), inference(duplicate_literal_removal, [], [f2352])).
fof(f2352, plain, ! [X28, X27] : (~ aSet0(X27) | ~ aElement0(sK5(X27, X28)) | (sK5(X27, X28) = sK6(sK5(X27, X28), X27, X27)) | aSubsetOf0(X28, X27) | ~ aSet0(X28) | ~ aSet0(X27)), inference(resolution, [], [f1015, f85])).
fof(f1015, plain, ! [X8, X9] : (aElementOf0(X8, X9) | ~ aSet0(X9) | ~ aElement0(X8) | (sK6(X8, X9, X9) = X8)), inference(resolution, [], [f544, f132])).
fof(f132, plain, ! [X4, X2, X1] : (~ sP0(X4, X1, X2) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(equality_resolution, [], [f96])).
fof(f96, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (X0 = X4) | ~ aElement0(X4) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f67])).
fof(f544, plain, ! [X0, X1] : (sP0(X0, X1, X1) | (sK6(X0, X1, X1) = X0) | ~ aSet0(X1)), inference(subsumption_resolution, [], [f536, f140])).
fof(f140, plain, ! [X2, X0, X1] : (~ aElementOf0(sK6(X0, X1, X2), X2) | ~ aElementOf0(sK6(X0, X1, X2), X1) | sP0(X0, X1, X2) | ~ aSet0(X2)), inference(subsumption_resolution, [], [f99, f77])).
fof(f99, plain, ! [X2, X0, X1] : (sP0(X0, X1, X2) | ~ aElementOf0(sK6(X0, X1, X2), X1) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2) | ~ aSet0(X2)), inference(cnf_transformation, [], [f67])).
fof(f536, plain, ! [X0, X1] : (aElementOf0(sK6(X0, X1, X1), X1) | (sK6(X0, X1, X1) = X0) | sP0(X0, X1, X1) | ~ aSet0(X1)), inference(factoring, [], [f98])).
fof(f98, plain, ! [X2, X0, X1] : (aElementOf0(sK6(X0, X1, X2), X2) | aElementOf0(sK6(X0, X1, X2), X1) | (sK6(X0, X1, X2) = X0) | sP0(X0, X1, X2) | ~ aSet0(X2)), inference(cnf_transformation, [], [f67])).
fof(f231355, plain, (~ spl8_452 | spl8_2270), inference(avatar_contradiction_clause, [], [f231354])).
fof(f231354, plain, ($false | (~ spl8_452 | spl8_2270)), inference(subsumption_resolution, [], [f231353, f23857])).
fof(f231353, plain, (~ aSet0(sdtpldt0(xS, sK4(xS))) | spl8_2270), inference(subsumption_resolution, [], [f231352, f115])).
fof(f115, plain, aElement0(xx), inference(cnf_transformation, [], [f18])).
fof(f231352, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(xS, sK4(xS))) | spl8_2270), inference(resolution, [], [f231278, f113])).
fof(f113, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f42, e50, e49])).
fof(f42, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f41])).
fof(f41, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mDefDiff)).
fof(f231278, plain, (~ sP3(sdtpldt0(xS, sK4(xS)), xx) | spl8_2270), inference(avatar_component_clause, [], [f231276])).
fof(f231276, plain, (spl8_2270 <=> sP3(sdtpldt0(xS, sK4(xS)), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_2270])])).
fof(f231279, plain, (~ spl8_2270 | spl8_2128 | spl8_18 | ~ spl8_469 | ~ spl8_2127), inference(avatar_split_clause, [], [f231270, f207770, f24107, f991, f207774, f231276])).
fof(f991, plain, (spl8_18 <=> (xx = sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_18])])).
fof(f207770, plain, (spl8_2127 <=> (xx = sK7(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS))))), introduced(avatar_definition, [new_symbols(naming, [spl8_2127])])).
fof(f231270, plain, ((sdtpldt0(xS, sK4(xS)) = sdtmndt0(sdtpldt0(xS, sK4(xS)), xx)) | ~ sP3(sdtpldt0(xS, sK4(xS)), xx) | (spl8_18 | ~ spl8_469 | ~ spl8_2127)), inference(resolution, [], [f231261, f103])).
fof(f103, plain, ! [X2, X0, X1] : (~ sP2(X1, X0, X2) | (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f68])).
fof(f231261, plain, (sP2(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS))) | (spl8_18 | ~ spl8_469 | ~ spl8_2127)), inference(subsumption_resolution, [], [f231260, f24108])).
fof(f231260, plain, (~ sP1(xS, sK4(xS)) | sP2(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS))) | (spl8_18 | ~ spl8_2127)), inference(subsumption_resolution, [], [f231259, f992])).
fof(f992, plain, (~ (xx = sK4(xS)) | spl8_18), inference(avatar_component_clause, [], [f991])).
fof(f231259, plain, ((xx = sK4(xS)) | ~ sP1(xS, sK4(xS)) | sP2(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS))) | ~ spl8_2127), inference(subsumption_resolution, [], [f231230, f117])).
fof(f117, plain, ~ aElementOf0(xx, xS), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ~ aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', m__679_02)).
fof(f231230, plain, (aElementOf0(xx, xS) | (xx = sK4(xS)) | ~ sP1(xS, sK4(xS)) | sP2(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS))) | ~ spl8_2127), inference(superposition, [], [f1404, f207772])).
fof(f207772, plain, ((xx = sK7(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS)))) | ~ spl8_2127), inference(avatar_component_clause, [], [f207770])).
fof(f1404, plain, ! [X30, X31, X32] : (aElementOf0(sK7(X30, sdtpldt0(X31, X32), sdtpldt0(X31, X32)), X31) | (sK7(X30, sdtpldt0(X31, X32), sdtpldt0(X31, X32)) = X32) | ~ sP1(X31, X32) | sP2(X30, sdtpldt0(X31, X32), sdtpldt0(X31, X32))), inference(subsumption_resolution, [], [f1389, f157])).
fof(f1389, plain, ! [X30, X31, X32] : (aElementOf0(sK7(X30, sdtpldt0(X31, X32), sdtpldt0(X31, X32)), X31) | (sK7(X30, sdtpldt0(X31, X32), sdtpldt0(X31, X32)) = X32) | ~ sP1(X31, X32) | sP2(X30, sdtpldt0(X31, X32), sdtpldt0(X31, X32)) | ~ aSet0(sdtpldt0(X31, X32))), inference(resolution, [], [f400, f484])).
fof(f484, plain, ! [X0, X1] : (aElementOf0(sK7(X0, X1, X1), X1) | sP2(X0, X1, X1) | ~ aSet0(X1)), inference(factoring, [], [f110])).
fof(f110, plain, ! [X2, X0, X1] : (aElementOf0(sK7(X0, X1, X2), X2) | aElementOf0(sK7(X0, X1, X2), X1) | sP2(X0, X1, X2) | ~ aSet0(X2)), inference(cnf_transformation, [], [f73])).
fof(f207777, plain, (spl8_2127 | spl8_2128 | ~ spl8_452), inference(avatar_split_clause, [], [f207752, f23856, f207774, f207770])).
fof(f207752, plain, ((sdtpldt0(xS, sK4(xS)) = sdtmndt0(sdtpldt0(xS, sK4(xS)), xx)) | (xx = sK7(xx, sdtpldt0(xS, sK4(xS)), sdtpldt0(xS, sK4(xS)))) | ~ spl8_452), inference(resolution, [], [f23857, f174926])).
fof(f174926, plain, ! [X0] : (~ aSet0(X0) | (sdtmndt0(X0, xx) = X0) | (xx = sK7(xx, X0, X0))), inference(resolution, [], [f18489, f115])).
fof(f18489, plain, ! [X0, X1] : (~ aElement0(X0) | (sdtmndt0(X1, X0) = X1) | ~ aSet0(X1) | (sK7(X0, X1, X1) = X0)), inference(duplicate_literal_removal, [], [f18482])).
fof(f18482, plain, ! [X0, X1] : ((sK7(X0, X1, X1) = X0) | (sdtmndt0(X1, X0) = X1) | ~ aSet0(X1) | ~ aElement0(X0) | ~ aSet0(X1)), inference(resolution, [], [f2267, f113])).
fof(f2267, plain, ! [X4, X3] : (~ sP3(X3, X4) | (sK7(X4, X3, X3) = X4) | (sdtmndt0(X3, X4) = X3) | ~ aSet0(X3)), inference(resolution, [], [f633, f103])).
fof(f633, plain, ! [X0, X1] : (sP2(X0, X1, X1) | ~ aSet0(X1) | (sK7(X0, X1, X1) = X0)), inference(subsumption_resolution, [], [f632, f110])).
fof(f632, plain, ! [X0, X1] : (sP2(X0, X1, X1) | ~ aSet0(X1) | (sK7(X0, X1, X1) = X0) | ~ aElementOf0(sK7(X0, X1, X1), X1)), inference(duplicate_literal_removal, [], [f619])).
fof(f619, plain, ! [X0, X1] : (sP2(X0, X1, X1) | ~ aSet0(X1) | (sK7(X0, X1, X1) = X0) | ~ aElementOf0(sK7(X0, X1, X1), X1) | sP2(X0, X1, X1) | ~ aSet0(X1)), inference(resolution, [], [f484, f142])).
fof(f142, plain, ! [X2, X0, X1] : (~ aElementOf0(sK7(X0, X1, X2), X2) | (sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | sP2(X0, X1, X2) | ~ aSet0(X2)), inference(subsumption_resolution, [], [f112, f77])).
fof(f112, plain, ! [X2, X0, X1] : (sP2(X0, X1, X2) | (sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2) | ~ aSet0(X2)), inference(cnf_transformation, [], [f73])).
fof(f24670, plain, (~ spl8_19 | spl8_469), inference(avatar_contradiction_clause, [], [f24669])).
fof(f24669, plain, ($false | (~ spl8_19 | spl8_469)), inference(subsumption_resolution, [], [f24668, f116])).
fof(f24668, plain, (~ aSet0(xS) | (~ spl8_19 | spl8_469)), inference(subsumption_resolution, [], [f24667, f996])).
fof(f996, plain, (aElement0(sK4(xS)) | ~ spl8_19), inference(avatar_component_clause, [], [f995])).
fof(f995, plain, (spl8_19 <=> aElement0(sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_19])])).
fof(f24667, plain, (~ aElement0(sK4(xS)) | ~ aSet0(xS) | spl8_469), inference(resolution, [], [f24109, f101])).
fof(f101, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f40, e47, e46])).
fof(f40, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f39])).
fof(f39, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mDefCons)).
fof(f24109, plain, (~ sP1(xS, sK4(xS)) | spl8_469), inference(avatar_component_clause, [], [f24107])).
fof(f23893, plain, (~ spl8_19 | spl8_452), inference(avatar_contradiction_clause, [], [f23892])).
fof(f23892, plain, ($false | (~ spl8_19 | spl8_452)), inference(subsumption_resolution, [], [f23891, f116])).
fof(f23891, plain, (~ aSet0(xS) | (~ spl8_19 | spl8_452)), inference(subsumption_resolution, [], [f23890, f996])).
fof(f23890, plain, (~ aElement0(sK4(xS)) | ~ aSet0(xS) | spl8_452), inference(resolution, [], [f23876, f101])).
fof(f23876, plain, (~ sP1(xS, sK4(xS)) | spl8_452), inference(resolution, [], [f23858, f157])).
fof(f23858, plain, (~ aSet0(sdtpldt0(xS, sK4(xS))) | spl8_452), inference(avatar_component_clause, [], [f23856])).
fof(f7626, plain, ~ spl8_39, inference(avatar_contradiction_clause, [], [f7625])).
fof(f7625, plain, ($false | ~ spl8_39), inference(subsumption_resolution, [], [f7044, f117])).
fof(f7044, plain, (aElementOf0(xx, xS) | ~ spl8_39), inference(backward_demodulation, [], [f144, f1679])).
fof(f1679, plain, ((xS = sdtpldt0(xS, xx)) | ~ spl8_39), inference(avatar_component_clause, [], [f1677])).
fof(f144, plain, aElementOf0(xx, sdtpldt0(xS, xx)), inference(subsumption_resolution, [], [f136, f115])).
fof(f136, plain, (aElementOf0(xx, sdtpldt0(xS, xx)) | ~ aElement0(xx)), inference(equality_resolution, [], [f122])).
fof(f122, plain, ! [X1] : (aElementOf0(X1, sdtpldt0(xS, xx)) | ~ (xx = X1) | ~ aElement0(X1)), inference(cnf_transformation, [], [f76])).
fof(f6653, plain, (spl8_9 | spl8_4 | ~ spl8_96), inference(avatar_split_clause, [], [f6573, f6213, f200, f281])).
fof(f281, plain, (spl8_9 <=> aElementOf0(sK4(sdtmndt0(sdtpldt0(xS, xx), xx)), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_9])])).
fof(f200, plain, (spl8_4 <=> (slcrc0 = sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_4])])).
fof(f6573, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | (spl8_4 | ~ spl8_96)), inference(subsumption_resolution, [], [f6572, f123])).
fof(f6572, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | (spl8_4 | ~ spl8_96)), inference(subsumption_resolution, [], [f6527, f201])).
fof(f201, plain, (~ (slcrc0 = sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_4), inference(avatar_component_clause, [], [f200])).
fof(f6527, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | (slcrc0 = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_96), inference(resolution, [], [f6400, f80])).
fof(f80, plain, ! [X0] : (aElementOf0(sK4(X0), X0) | (slcrc0 = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (((slcrc0 = X0) | aElementOf0(sK4(X0), X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f54, f55])).
fof(f55, plain, ! [X0] : (? [X1] : aElementOf0(X1, X0) => aElementOf0(sK4(X0), X0)), introduced(choice_axiom, [])).
fof(f54, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(rectify, [], [f53])).
fof(f53, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(flattening, [], [f52])).
fof(f52, plain, ! [X0] : (((slcrc0 = X0) | (? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0))) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(nnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : ((slcrc0 = X0) <=> (! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : ((slcrc0 = X0) <=> (~ ? [X1] : aElementOf0(X1, X0) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mDefEmp)).
fof(f6400, plain, (! [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | aElementOf0(X0, xS)) | ~ spl8_96), inference(subsumption_resolution, [], [f6393, f116])).
fof(f6393, plain, (! [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | aElementOf0(X0, xS) | ~ aSet0(xS)) | ~ spl8_96), inference(resolution, [], [f6215, f83])).
fof(f83, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f61])).
fof(f6379, plain, (spl8_96 | ~ spl8_95), inference(avatar_split_clause, [], [f6378, f6209, f6213])).
fof(f6209, plain, (spl8_95 <=> (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)))), introduced(avatar_definition, [new_symbols(naming, [spl8_95])])).
fof(f6378, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ spl8_95), inference(subsumption_resolution, [], [f6377, f116])).
fof(f6377, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(xS) | ~ spl8_95), inference(subsumption_resolution, [], [f6376, f123])).
fof(f6376, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ spl8_95), inference(subsumption_resolution, [], [f6375, f135])).
fof(f135, plain, ~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)), inference(equality_resolution, [], [f126])).
fof(f126, plain, ! [X0] : (~ (xx = X0) | ~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx))), inference(cnf_transformation, [], [f76])).
fof(f6375, plain, (aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ spl8_95), inference(superposition, [], [f84, f6211])).
fof(f6211, plain, ((xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ spl8_95), inference(avatar_component_clause, [], [f6209])).
fof(f6216, plain, (spl8_95 | spl8_96), inference(avatar_split_clause, [], [f6207, f6213, f6209])).
fof(f6207, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)))), inference(subsumption_resolution, [], [f6206, f123])).
fof(f6206, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))), inference(subsumption_resolution, [], [f6205, f116])).
fof(f6205, plain, (~ aSet0(xS) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))), inference(duplicate_literal_removal, [], [f6193])).
fof(f6193, plain, (~ aSet0(xS) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS)), inference(resolution, [], [f1592, f85])).
fof(f1592, plain, ! [X0] : (aElementOf0(sK5(X0, sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | ~ aSet0(X0) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), X0) | (xx = sK5(X0, sdtmndt0(sdtpldt0(xS, xx), xx)))), inference(resolution, [], [f265, f120])).
fof(f120, plain, ! [X1] : (~ aElementOf0(X1, sdtpldt0(xS, xx)) | aElementOf0(X1, xS) | (xx = X1)), inference(cnf_transformation, [], [f76])).
fof(f265, plain, ! [X0] : (aElementOf0(sK5(X0, sdtmndt0(sdtpldt0(xS, xx), xx)), sdtpldt0(xS, xx)) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), X0) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f264, f123])).
fof(f264, plain, ! [X0] : (aElementOf0(sK5(X0, sdtmndt0(sdtpldt0(xS, xx), xx)), sdtpldt0(xS, xx)) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), X0) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(X0)), inference(resolution, [], [f125, f84])).
fof(f125, plain, ! [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | aElementOf0(X0, sdtpldt0(xS, xx))), inference(cnf_transformation, [], [f76])).
fof(f6166, plain, spl8_94, inference(avatar_contradiction_clause, [], [f6165])).
fof(f6165, plain, ($false | spl8_94), inference(subsumption_resolution, [], [f6161, f116])).
fof(f6161, plain, (~ aSet0(xS) | spl8_94), inference(resolution, [], [f6130, f87])).
fof(f87, plain, ! [X0] : (aSubsetOf0(X0, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (aSubsetOf0(X0, X0) | ~ aSet0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aSet0(X0) => aSubsetOf0(X0, X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+2.p', mSubRefl)).
fof(f6130, plain, (~ aSubsetOf0(xS, xS) | spl8_94), inference(avatar_component_clause, [], [f6128])).
fof(f2047, plain, (~ spl8_19 | spl8_45), inference(avatar_contradiction_clause, [], [f2046])).
fof(f2046, plain, ($false | (~ spl8_19 | spl8_45)), inference(subsumption_resolution, [], [f2045, f116])).
fof(f2045, plain, (~ aSet0(xS) | (~ spl8_19 | spl8_45)), inference(subsumption_resolution, [], [f2044, f996])).
fof(f2044, plain, (~ aElement0(sK4(xS)) | ~ aSet0(xS) | (~ spl8_19 | spl8_45)), inference(resolution, [], [f2036, f113])).
fof(f2036, plain, (~ sP3(xS, sK4(xS)) | (~ spl8_19 | spl8_45)), inference(resolution, [], [f1999, f161])).
fof(f1999, plain, (~ aSet0(sdtmndt0(xS, sK4(xS))) | (~ spl8_19 | spl8_45)), inference(subsumption_resolution, [], [f1998, f996])).
fof(f1998, plain, (~ aElement0(sK4(xS)) | ~ aSet0(sdtmndt0(xS, sK4(xS))) | spl8_45), inference(resolution, [], [f1968, f101])).
fof(f1968, plain, (~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | spl8_45), inference(avatar_component_clause, [], [f1966])).
fof(f1966, plain, (spl8_45 <=> sP1(sdtmndt0(xS, sK4(xS)), sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_45])])).
fof(f1982, plain, (~ spl8_45 | spl8_48 | spl8_16 | ~ spl8_19), inference(avatar_split_clause, [], [f1977, f995, f645, f1979, f1966])).
fof(f645, plain, (spl8_16 <=> (slcrc0 = xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_16])])).
fof(f1977, plain, (aElementOf0(sK4(xS), xS) | ~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | (spl8_16 | ~ spl8_19)), inference(subsumption_resolution, [], [f1961, f996])).
fof(f1961, plain, (aElementOf0(sK4(xS), xS) | ~ aElement0(sK4(xS)) | ~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | spl8_16), inference(superposition, [], [f158, f1370])).
fof(f1370, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK4(xS)), sK4(xS))) | spl8_16), inference(subsumption_resolution, [], [f1366, f646])).
fof(f646, plain, (~ (slcrc0 = xS) | spl8_16), inference(avatar_component_clause, [], [f645])).
fof(f1366, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK4(xS)), sK4(xS))) | (slcrc0 = xS)), inference(resolution, [], [f331, f116])).
fof(f331, plain, ! [X3] : (~ aSet0(X3) | (sdtpldt0(sdtmndt0(X3, sK4(X3)), sK4(X3)) = X3) | (slcrc0 = X3)), inference(duplicate_literal_removal, [], [f328])).
fof(f328, plain, ! [X3] : ((sdtpldt0(sdtmndt0(X3, sK4(X3)), sK4(X3)) = X3) | ~ aSet0(X3) | (slcrc0 = X3) | ~ aSet0(X3)), inference(resolution, [], [f114, f80])).
fof(f158, plain, ! [X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X0)) | ~ aElement0(X0) | ~ sP1(X1, X0)), inference(resolution, [], [f132, f131])).
fof(f1023, plain, (spl8_16 | ~ spl8_18), inference(avatar_contradiction_clause, [], [f1022])).
fof(f1022, plain, ($false | (spl8_16 | ~ spl8_18)), inference(subsumption_resolution, [], [f1021, f116])).
fof(f1021, plain, (~ aSet0(xS) | (spl8_16 | ~ spl8_18)), inference(subsumption_resolution, [], [f1020, f646])).
fof(f1020, plain, ((slcrc0 = xS) | ~ aSet0(xS) | ~ spl8_18), inference(subsumption_resolution, [], [f1019, f117])).
fof(f1019, plain, (aElementOf0(xx, xS) | (slcrc0 = xS) | ~ aSet0(xS) | ~ spl8_18), inference(superposition, [], [f80, f993])).
fof(f993, plain, ((xx = sK4(xS)) | ~ spl8_18), inference(avatar_component_clause, [], [f991])).
fof(f1009, plain, (spl8_16 | spl8_19), inference(avatar_contradiction_clause, [], [f1008])).
fof(f1008, plain, ($false | (spl8_16 | spl8_19)), inference(subsumption_resolution, [], [f1007, f646])).
fof(f1007, plain, ((slcrc0 = xS) | spl8_19), inference(subsumption_resolution, [], [f1006, f116])).
fof(f1006, plain, (~ aSet0(xS) | (slcrc0 = xS) | spl8_19), inference(resolution, [], [f997, f153])).
fof(f153, plain, ! [X0] : (aElement0(sK4(X0)) | ~ aSet0(X0) | (slcrc0 = X0)), inference(duplicate_literal_removal, [], [f149])).
fof(f149, plain, ! [X0] : ((slcrc0 = X0) | ~ aSet0(X0) | aElement0(sK4(X0)) | ~ aSet0(X0)), inference(resolution, [], [f80, f77])).
fof(f997, plain, (~ aElement0(sK4(xS)) | spl8_19), inference(avatar_component_clause, [], [f995])).
fof(f896, plain, (~ spl8_16 | ~ spl8_4), inference(avatar_split_clause, [], [f895, f200, f645])).
fof(f895, plain, (~ (slcrc0 = xS) | ~ spl8_4), inference(forward_demodulation, [], [f894, f826])).
fof(f826, plain, ((slcrc0 = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ spl8_4), inference(backward_demodulation, [], [f202, f825])).
fof(f825, plain, ((sdtpldt0(xS, xx) = sdtpldt0(slcrc0, xx)) | ~ spl8_4), inference(forward_demodulation, [], [f334, f202])).
fof(f334, plain, (sdtpldt0(xS, xx) = sdtpldt0(sdtmndt0(sdtpldt0(xS, xx), xx), xx)), inference(subsumption_resolution, [], [f327, f118])).
fof(f118, plain, aSet0(sdtpldt0(xS, xx)), inference(cnf_transformation, [], [f76])).
fof(f327, plain, ((sdtpldt0(xS, xx) = sdtpldt0(sdtmndt0(sdtpldt0(xS, xx), xx), xx)) | ~ aSet0(sdtpldt0(xS, xx))), inference(resolution, [], [f114, f144])).
fof(f202, plain, ((slcrc0 = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_4), inference(avatar_component_clause, [], [f200])).
fof(f894, plain, (~ (xS = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ spl8_4), inference(forward_demodulation, [], [f128, f825])).
fof(f718, plain, (~ spl8_9 | ~ spl8_16), inference(avatar_contradiction_clause, [], [f717])).
fof(f717, plain, ($false | (~ spl8_9 | ~ spl8_16)), inference(subsumption_resolution, [], [f675, f129])).
fof(f129, plain, ! [X2] : ~ aElementOf0(X2, slcrc0), inference(equality_resolution, [], [f79])).
fof(f79, plain, ! [X2, X0] : (~ aElementOf0(X2, X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f56])).
fof(f675, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx)), slcrc0) | (~ spl8_9 | ~ spl8_16)), inference(backward_demodulation, [], [f283, f647])).
fof(f647, plain, ((slcrc0 = xS) | ~ spl8_16), inference(avatar_component_clause, [], [f645])).
fof(f283, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | ~ spl8_9), inference(avatar_component_clause, [], [f281])).
fof(f404, plain, spl8_12, inference(avatar_contradiction_clause, [], [f403])).
fof(f403, plain, ($false | spl8_12), inference(subsumption_resolution, [], [f402, f116])).
fof(f402, plain, (~ aSet0(xS) | spl8_12), inference(subsumption_resolution, [], [f401, f115])).
fof(f401, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_12), inference(resolution, [], [f394, f101])).
fof(f394, plain, (~ sP1(xS, xx) | spl8_12), inference(avatar_component_clause, [], [f392])).
fof(f258, plain, spl8_6, inference(avatar_contradiction_clause, [], [f257])).
fof(f257, plain, ($false | spl8_6), inference(subsumption_resolution, [], [f256, f118])).
fof(f256, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl8_6), inference(subsumption_resolution, [], [f255, f115])).
fof(f255, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(xS, xx)) | spl8_6), inference(resolution, [], [f227, f113])).
fof(f227, plain, (~ sP3(sdtpldt0(xS, xx), xx) | spl8_6), inference(avatar_component_clause, [], [f225])).