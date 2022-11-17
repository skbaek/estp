fof(f48978, plain, $false, inference(avatar_sat_refutation, [], [f135, f463, f506, f615, f3847, f5061, f16142, f16165, f16207, f19476, f24415, f24475, f24507, f24529, f24548, f26345, f26573, f26574, f40037, f41569, f47506, f47623, f48156, f48952])).
fof(f48952, plain, (spl8_2 | ~ spl8_12 | spl8_414), inference(avatar_contradiction_clause, [], [f48951])).
fof(f48951, plain, ($false | (spl8_2 | ~ spl8_12 | spl8_414)), inference(subsumption_resolution, [], [f48950, f111])).
fof(f111, plain, aSet0(xS), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (aSet0(xS) & aElement0(xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', m__679)).
fof(f48950, plain, (~ aSet0(xS) | (spl8_2 | ~ spl8_12 | spl8_414)), inference(subsumption_resolution, [], [f48949, f457])).
fof(f457, plain, (aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_12), inference(avatar_component_clause, [], [f456])).
fof(f456, plain, (spl8_12 <=> aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_12])])).
fof(f48949, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | (spl8_2 | spl8_414)), inference(subsumption_resolution, [], [f48941, f134])).
fof(f134, plain, (~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | spl8_2), inference(avatar_component_clause, [], [f132])).
fof(f132, plain, (spl8_2 <=> aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_2])])).
fof(f48941, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | spl8_414), inference(resolution, [], [f13010, f79])).
fof(f79, plain, ! [X0, X1] : (aElementOf0(sK5(X0, X1), X1) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f57, f58])).
fof(f58, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f57, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f56])).
fof(f56, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f55])).
fof(f55, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', mDefSub)).
fof(f13010, plain, (~ aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_414), inference(avatar_component_clause, [], [f13009])).
fof(f13009, plain, (spl8_414 <=> aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_414])])).
fof(f48156, plain, (spl8_2 | ~ spl8_12 | ~ spl8_722), inference(avatar_contradiction_clause, [], [f48155])).
fof(f48155, plain, ($false | (spl8_2 | ~ spl8_12 | ~ spl8_722)), inference(subsumption_resolution, [], [f48154, f111])).
fof(f48154, plain, (~ aSet0(xS) | (spl8_2 | ~ spl8_12 | ~ spl8_722)), inference(subsumption_resolution, [], [f48153, f457])).
fof(f48153, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | (spl8_2 | ~ spl8_722)), inference(subsumption_resolution, [], [f48124, f134])).
fof(f48124, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ spl8_722), inference(resolution, [], [f47505, f80])).
fof(f80, plain, ! [X0, X1] : (~ aElementOf0(sK5(X0, X1), X0) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f47505, plain, (aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | ~ spl8_722), inference(avatar_component_clause, [], [f47503])).
fof(f47503, plain, (spl8_722 <=> aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_722])])).
fof(f47623, plain, (~ spl8_414 | spl8_553 | ~ spl8_721), inference(avatar_contradiction_clause, [], [f47622])).
fof(f47622, plain, ($false | (~ spl8_414 | spl8_553 | ~ spl8_721)), inference(subsumption_resolution, [], [f47524, f24528])).
fof(f24528, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_553), inference(avatar_component_clause, [], [f24526])).
fof(f24526, plain, (spl8_553 <=> aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_553])])).
fof(f47524, plain, (aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_414 | ~ spl8_721)), inference(backward_demodulation, [], [f13011, f47501])).
fof(f47501, plain, ((xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ spl8_721), inference(avatar_component_clause, [], [f47499])).
fof(f47499, plain, (spl8_721 <=> (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)))), introduced(avatar_definition, [new_symbols(naming, [spl8_721])])).
fof(f13011, plain, (aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_414), inference(avatar_component_clause, [], [f13009])).
fof(f47506, plain, (spl8_721 | spl8_722 | ~ spl8_143 | ~ spl8_233 | ~ spl8_414 | ~ spl8_521), inference(avatar_split_clause, [], [f47497, f16139, f13009, f5039, f3638, f47503, f47499])).
fof(f3638, plain, (spl8_143 <=> aSet0(sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_143])])).
fof(f5039, plain, (spl8_233 <=> sP1(xS, xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_233])])).
fof(f16139, plain, (spl8_521 <=> aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_521])])).
fof(f47497, plain, (aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | (~ spl8_143 | ~ spl8_233 | ~ spl8_414 | ~ spl8_521)), inference(subsumption_resolution, [], [f47479, f5040])).
fof(f5040, plain, (sP1(xS, xx) | ~ spl8_233), inference(avatar_component_clause, [], [f5039])).
fof(f47479, plain, (aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ sP1(xS, xx) | (~ spl8_143 | ~ spl8_414 | ~ spl8_521)), inference(resolution, [], [f47342, f194])).
fof(f194, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtpldt0(X1, X2)) | aElementOf0(X0, X1) | (X0 = X2) | ~ sP1(X1, X2)), inference(resolution, [], [f89, f116])).
fof(f116, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f85])).
fof(f85, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ! [X0, X1] : (! [X2] : (((sdtpldt0(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f45])).
fof(f45, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e45])).
fof(e45, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f89, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | aElementOf0(X4, X1) | ~ aElementOf0(X4, X2) | (X0 = X4)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f63, f64])).
fof(f64, plain, ! [X2, X1, X0] : (? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) => (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f63, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f62])).
fof(f62, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : (((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f61])).
fof(f61, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3))) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f44])).
fof(f44, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e44])).
fof(e44, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f47342, plain, (aElementOf0(sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx)), sdtpldt0(xS, xx)) | (~ spl8_143 | ~ spl8_414 | ~ spl8_521)), inference(resolution, [], [f13011, f16354])).
fof(f16354, plain, (! [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | aElementOf0(X0, sdtpldt0(xS, xx))) | (~ spl8_143 | ~ spl8_521)), inference(subsumption_resolution, [], [f16347, f3639])).
fof(f3639, plain, (aSet0(sdtpldt0(xS, xx)) | ~ spl8_143), inference(avatar_component_clause, [], [f3638])).
fof(f16347, plain, (! [X0] : (~ aElementOf0(X0, sdtmndt0(sdtpldt0(xS, xx), xx)) | aElementOf0(X0, sdtpldt0(xS, xx)) | ~ aSet0(sdtpldt0(xS, xx))) | ~ spl8_521), inference(resolution, [], [f16141, f78])).
fof(f78, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f16141, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)) | ~ spl8_521), inference(avatar_component_clause, [], [f16139])).
fof(f41569, plain, (~ spl8_260 | ~ spl8_693), inference(avatar_contradiction_clause, [], [f41568])).
fof(f41568, plain, ($false | (~ spl8_260 | ~ spl8_693)), inference(subsumption_resolution, [], [f41567, f112])).
fof(f112, plain, ~ aElementOf0(xx, xS), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ~ aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', m__679_02)).
fof(f41567, plain, (aElementOf0(xx, xS) | (~ spl8_260 | ~ spl8_693)), inference(forward_demodulation, [], [f41566, f36578])).
fof(f36578, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ spl8_693), inference(avatar_component_clause, [], [f36576])).
fof(f36576, plain, (spl8_693 <=> (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_693])])).
fof(f41566, plain, (aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS) | (~ spl8_260 | ~ spl8_693)), inference(subsumption_resolution, [], [f41565, f110])).
fof(f110, plain, aElement0(xx), inference(cnf_transformation, [], [f18])).
fof(f41565, plain, (~ aElement0(xx) | aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS) | (~ spl8_260 | ~ spl8_693)), inference(forward_demodulation, [], [f29083, f36578])).
fof(f29083, plain, (~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS) | ~ spl8_260), inference(resolution, [], [f5710, f117])).
fof(f117, plain, ! [X4, X2, X1] : (~ sP0(X4, X1, X2) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(equality_resolution, [], [f91])).
fof(f91, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (X0 = X4) | ~ aElement0(X4) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f65])).
fof(f5710, plain, (sP0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), xS) | ~ spl8_260), inference(avatar_component_clause, [], [f5708])).
fof(f5708, plain, (spl8_260 <=> sP0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_260])])).
fof(f40037, plain, (spl8_693 | spl8_1 | ~ spl8_17 | ~ spl8_233 | ~ spl8_261 | ~ spl8_262), inference(avatar_split_clause, [], [f40036, f5717, f5713, f5039, f544, f128, f36576])).
fof(f128, plain, (spl8_1 <=> aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_1])])).
fof(f544, plain, (spl8_17 <=> sP3(sdtpldt0(xS, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_17])])).
fof(f5713, plain, (spl8_261 <=> aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_261])])).
fof(f5717, plain, (spl8_262 <=> aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_262])])).
fof(f40036, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (spl8_1 | ~ spl8_17 | ~ spl8_233 | ~ spl8_261 | ~ spl8_262)), inference(subsumption_resolution, [], [f40035, f5040])).
fof(f40035, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (spl8_1 | ~ spl8_17 | ~ spl8_261 | ~ spl8_262)), inference(subsumption_resolution, [], [f40034, f5714])).
fof(f5714, plain, (aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ spl8_261), inference(avatar_component_clause, [], [f5713])).
fof(f40034, plain, (~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (spl8_1 | ~ spl8_17 | ~ spl8_262)), inference(subsumption_resolution, [], [f40033, f111])).
fof(f40033, plain, (~ aSet0(xS) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (spl8_1 | ~ spl8_17 | ~ spl8_262)), inference(subsumption_resolution, [], [f40032, f130])).
fof(f130, plain, (~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_1), inference(avatar_component_clause, [], [f128])).
fof(f40032, plain, (aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (~ spl8_17 | ~ spl8_262)), inference(subsumption_resolution, [], [f39985, f545])).
fof(f545, plain, (sP3(sdtpldt0(xS, xx), xx) | ~ spl8_17), inference(avatar_component_clause, [], [f544])).
fof(f39985, plain, (~ sP3(sdtpldt0(xS, xx), xx) | aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | ~ spl8_262), inference(resolution, [], [f1115, f5719])).
fof(f5719, plain, (aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS) | ~ spl8_262), inference(avatar_component_clause, [], [f5717])).
fof(f1115, plain, ! [X10, X8, X11, X9] : (~ aElementOf0(sK5(sdtmndt0(sdtpldt0(X8, X9), X10), X11), X8) | ~ sP3(sdtpldt0(X8, X9), X10) | aSubsetOf0(X11, sdtmndt0(sdtpldt0(X8, X9), X10)) | ~ aSet0(X11) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X8, X9), X10), X11)) | (sK5(sdtmndt0(sdtpldt0(X8, X9), X10), X11) = X10) | ~ sP1(X8, X9)), inference(resolution, [], [f379, f185])).
fof(f185, plain, ! [X2, X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X2)) | ~ aElement0(X0) | ~ aElementOf0(X0, X1) | ~ sP1(X1, X2)), inference(resolution, [], [f90, f116])).
fof(f90, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f65])).
fof(f379, plain, ! [X14, X15, X16] : (~ aElementOf0(sK5(sdtmndt0(X14, X15), X16), X14) | (sK5(sdtmndt0(X14, X15), X16) = X15) | ~ sP3(X14, X15) | aSubsetOf0(X16, sdtmndt0(X14, X15)) | ~ aSet0(X16)), inference(subsumption_resolution, [], [f378, f148])).
fof(f148, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f118, f99])).
fof(f99, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f69, f70])).
fof(f70, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f69, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f68])).
fof(f68, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f67])).
fof(f67, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f47])).
fof(f47, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e47])).
fof(e47, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f118, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f97])).
fof(f97, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f48])).
fof(f48, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e48])).
fof(e48, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f378, plain, ! [X14, X15, X16] : (~ aElementOf0(sK5(sdtmndt0(X14, X15), X16), X14) | (sK5(sdtmndt0(X14, X15), X16) = X15) | ~ sP3(X14, X15) | aSubsetOf0(X16, sdtmndt0(X14, X15)) | ~ aSet0(X16) | ~ aSet0(sdtmndt0(X14, X15))), inference(subsumption_resolution, [], [f370, f166])).
fof(f166, plain, ! [X0, X1] : (aElement0(sK5(X1, X0)) | ~ aSet0(X0) | ~ aSet0(X1) | aSubsetOf0(X0, X1)), inference(duplicate_literal_removal, [], [f161])).
fof(f161, plain, ! [X0, X1] : (aSubsetOf0(X0, X1) | ~ aSet0(X0) | ~ aSet0(X1) | aElement0(sK5(X1, X0)) | ~ aSet0(X0)), inference(resolution, [], [f79, f72])).
fof(f72, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', mEOfElem)).
fof(f370, plain, ! [X14, X15, X16] : (~ aElementOf0(sK5(sdtmndt0(X14, X15), X16), X14) | ~ aElement0(sK5(sdtmndt0(X14, X15), X16)) | (sK5(sdtmndt0(X14, X15), X16) = X15) | ~ sP3(X14, X15) | aSubsetOf0(X16, sdtmndt0(X14, X15)) | ~ aSet0(X16) | ~ aSet0(sdtmndt0(X14, X15))), inference(resolution, [], [f203, f80])).
fof(f203, plain, ! [X2, X0, X1] : (aElementOf0(X1, sdtmndt0(X2, X0)) | ~ aElementOf0(X1, X2) | ~ aElement0(X1) | (X0 = X1) | ~ sP3(X2, X0)), inference(resolution, [], [f103, f118])).
fof(f103, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f71])).
fof(f26574, plain, (~ spl8_259 | spl8_260 | ~ spl8_13), inference(avatar_split_clause, [], [f6457, f460, f5708, f5704])).
fof(f5704, plain, (spl8_259 <=> sP1(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_259])])).
fof(f460, plain, (spl8_13 <=> (xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)))), introduced(avatar_definition, [new_symbols(naming, [spl8_13])])).
fof(f6457, plain, (sP0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), xS) | ~ sP1(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ spl8_13), inference(superposition, [], [f116, f462])).
fof(f462, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ spl8_13), inference(avatar_component_clause, [], [f460])).
fof(f26573, plain, (~ spl8_259 | spl8_262 | ~ spl8_13 | ~ spl8_261), inference(avatar_split_clause, [], [f26572, f5713, f460, f5717, f5704])).
fof(f26572, plain, (aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS) | ~ sP1(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (~ spl8_13 | ~ spl8_261)), inference(subsumption_resolution, [], [f6460, f5714])).
fof(f6460, plain, (aElementOf0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS), xS) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ spl8_13), inference(superposition, [], [f145, f462])).
fof(f145, plain, ! [X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X0)) | ~ aElement0(X0) | ~ sP1(X1, X0)), inference(resolution, [], [f117, f116])).
fof(f26345, plain, (~ spl8_261 | spl8_284), inference(avatar_contradiction_clause, [], [f26344])).
fof(f26344, plain, ($false | (~ spl8_261 | spl8_284)), inference(subsumption_resolution, [], [f26343, f111])).
fof(f26343, plain, (~ aSet0(xS) | (~ spl8_261 | spl8_284)), inference(subsumption_resolution, [], [f26342, f5714])).
fof(f26342, plain, (~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ aSet0(xS) | spl8_284), inference(resolution, [], [f26281, f108])).
fof(f108, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f41, e48, e47])).
fof(f41, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f40])).
fof(f40, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', mDefDiff)).
fof(f26281, plain, (~ sP3(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | spl8_284), inference(resolution, [], [f5814, f148])).
fof(f5814, plain, (~ aSet0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | spl8_284), inference(avatar_component_clause, [], [f5812])).
fof(f5812, plain, (spl8_284 <=> aSet0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)))), introduced(avatar_definition, [new_symbols(naming, [spl8_284])])).
fof(f24548, plain, (~ spl8_284 | ~ spl8_261 | spl8_259), inference(avatar_split_clause, [], [f11281, f5704, f5713, f5812])).
fof(f11281, plain, (~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ aSet0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | spl8_259), inference(resolution, [], [f5706, f96])).
fof(f96, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f39, e45, e44])).
fof(f39, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f38])).
fof(f38, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', mDefCons)).
fof(f5706, plain, (~ sP1(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | spl8_259), inference(avatar_component_clause, [], [f5704])).
fof(f24529, plain, (~ spl8_12 | spl8_526 | ~ spl8_553 | ~ spl8_143 | ~ spl8_525), inference(avatar_split_clause, [], [f20994, f16158, f3638, f24526, f16162, f456])).
fof(f16162, plain, (spl8_526 <=> aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_526])])).
fof(f16158, plain, (spl8_525 <=> (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl8_525])])).
fof(f20994, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_143 | ~ spl8_525)), inference(subsumption_resolution, [], [f18628, f3639])).
fof(f18628, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(xS, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_525), inference(superposition, [], [f80, f16160])).
fof(f16160, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))) | ~ spl8_525), inference(avatar_component_clause, [], [f16158])).
fof(f24507, plain, (~ spl8_12 | spl8_1 | spl8_261), inference(avatar_split_clause, [], [f24506, f5713, f128, f456])).
fof(f24506, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | (spl8_1 | spl8_261)), inference(subsumption_resolution, [], [f24335, f130])).
fof(f24335, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_261), inference(subsumption_resolution, [], [f18003, f111])).
fof(f18003, plain, (~ aSet0(xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_261), inference(resolution, [], [f5715, f166])).
fof(f5715, plain, (~ aElement0(sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | spl8_261), inference(avatar_component_clause, [], [f5713])).
fof(f24475, plain, (~ spl8_497 | spl8_237 | ~ spl8_233), inference(avatar_split_clause, [], [f24428, f5039, f5111, f16032])).
fof(f16032, plain, (spl8_497 <=> sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_497])])).
fof(f5111, plain, (spl8_237 <=> aElementOf0(xx, sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_237])])).
fof(f24428, plain, (aElementOf0(xx, sdtpldt0(xS, xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | ~ spl8_233), inference(subsumption_resolution, [], [f18501, f110])).
fof(f18501, plain, (aElementOf0(xx, sdtpldt0(xS, xx)) | ~ aElement0(xx) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | ~ spl8_233), inference(superposition, [], [f145, f5180])).
fof(f5180, plain, ((sdtpldt0(xS, xx) = sdtpldt0(sdtmndt0(sdtpldt0(xS, xx), xx), xx)) | ~ spl8_233), inference(subsumption_resolution, [], [f5179, f110])).
fof(f5179, plain, (~ aElement0(xx) | (sdtpldt0(xS, xx) = sdtpldt0(sdtmndt0(sdtpldt0(xS, xx), xx), xx)) | ~ spl8_233), inference(resolution, [], [f5040, f193])).
fof(f193, plain, ! [X2, X1] : (~ sP1(X1, X2) | ~ aElement0(X2) | (sdtpldt0(X1, X2) = sdtpldt0(sdtmndt0(sdtpldt0(X1, X2), X2), X2))), inference(subsumption_resolution, [], [f189, f144])).
fof(f144, plain, ! [X4, X3] : (aSet0(sdtpldt0(X3, X4)) | ~ sP1(X3, X4)), inference(resolution, [], [f116, f87])).
fof(f87, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f65])).
fof(f189, plain, ! [X2, X1] : ((sdtpldt0(X1, X2) = sdtpldt0(sdtmndt0(sdtpldt0(X1, X2), X2), X2)) | ~ aSet0(sdtpldt0(X1, X2)) | ~ aElement0(X2) | ~ sP1(X1, X2)), inference(resolution, [], [f109, f145])).
fof(f109, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | (sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ! [X0] : (! [X1] : ((sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => (sdtpldt0(sdtmndt0(X0, X1), X1) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', mConsDiff)).
fof(f24415, plain, (~ spl8_237 | ~ spl8_17 | ~ spl8_144), inference(avatar_split_clause, [], [f24414, f3747, f544, f5111])).
fof(f3747, plain, (spl8_144 <=> (sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_144])])).
fof(f24414, plain, (~ aElementOf0(xx, sdtpldt0(xS, xx)) | (~ spl8_17 | ~ spl8_144)), inference(subsumption_resolution, [], [f24238, f545])).
fof(f24238, plain, (~ aElementOf0(xx, sdtpldt0(xS, xx)) | ~ sP3(sdtpldt0(xS, xx), xx) | ~ spl8_144), inference(superposition, [], [f147, f3749])).
fof(f3749, plain, ((sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_144), inference(avatar_component_clause, [], [f3747])).
fof(f147, plain, ! [X4, X3] : (~ aElementOf0(X4, sdtmndt0(X3, X4)) | ~ sP3(X3, X4)), inference(resolution, [], [f118, f119])).
fof(f119, plain, ! [X4, X2, X1] : (~ sP2(X4, X1, X2) | ~ aElementOf0(X4, X2)), inference(equality_resolution, [], [f102])).
fof(f102, plain, ! [X4, X2, X0, X1] : (~ (X0 = X4) | ~ aElementOf0(X4, X2) | ~ sP2(X0, X1, X2)), inference(cnf_transformation, [], [f71])).
fof(f19476, plain, (~ spl8_526 | spl8_144 | ~ spl8_143 | ~ spl8_521), inference(avatar_split_clause, [], [f19475, f16139, f3638, f3747, f16162])).
fof(f19475, plain, ((sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_143 | ~ spl8_521)), inference(subsumption_resolution, [], [f18081, f3639])).
fof(f18081, plain, ((sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(xS, xx)) | ~ spl8_521), inference(resolution, [], [f16141, f120])).
fof(f120, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | (X0 = X1) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f83, f77])).
fof(f77, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f83, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f34])).
fof(f34, plain, ! [X0, X1] : (((X0 = X1) | (~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X0) & aSubsetOf0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', mSubASymm)).
fof(f16207, plain, (~ spl8_12 | spl8_497), inference(avatar_contradiction_clause, [], [f16206])).
fof(f16206, plain, ($false | (~ spl8_12 | spl8_497)), inference(subsumption_resolution, [], [f16205, f457])).
fof(f16205, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_497), inference(subsumption_resolution, [], [f16204, f110])).
fof(f16204, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_497), inference(resolution, [], [f16034, f96])).
fof(f16034, plain, (~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | spl8_497), inference(avatar_component_clause, [], [f16032])).
fof(f16165, plain, (spl8_525 | ~ spl8_497 | spl8_526 | ~ spl8_12 | ~ spl8_233), inference(avatar_split_clause, [], [f16156, f5039, f456, f16162, f16032, f16158])).
fof(f16156, plain, (aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))) | (~ spl8_12 | ~ spl8_233)), inference(subsumption_resolution, [], [f16025, f457])).
fof(f16025, plain, (aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_233), inference(superposition, [], [f1070, f5180])).
fof(f1070, plain, ! [X0, X1] : (aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ sP1(X0, X1) | (sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f1069, f144])).
fof(f1069, plain, ! [X0, X1] : ((sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(duplicate_literal_removal, [], [f1056])).
fof(f1056, plain, ! [X0, X1] : ((sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(X0) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(sdtpldt0(X0, X1)) | ~ aSet0(X0)), inference(resolution, [], [f353, f80])).
fof(f353, plain, ! [X8, X7, X9] : (aElementOf0(sK5(X7, sdtpldt0(X8, X9)), X8) | (sK5(X7, sdtpldt0(X8, X9)) = X9) | ~ sP1(X8, X9) | aSubsetOf0(sdtpldt0(X8, X9), X7) | ~ aSet0(X7)), inference(subsumption_resolution, [], [f342, f144])).
fof(f342, plain, ! [X8, X7, X9] : (aElementOf0(sK5(X7, sdtpldt0(X8, X9)), X8) | (sK5(X7, sdtpldt0(X8, X9)) = X9) | ~ sP1(X8, X9) | aSubsetOf0(sdtpldt0(X8, X9), X7) | ~ aSet0(sdtpldt0(X8, X9)) | ~ aSet0(X7)), inference(resolution, [], [f194, f79])).
fof(f16142, plain, (~ spl8_497 | spl8_521 | ~ spl8_12 | ~ spl8_233), inference(avatar_split_clause, [], [f16137, f5039, f456, f16139, f16032])).
fof(f16137, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | (~ spl8_12 | ~ spl8_233)), inference(subsumption_resolution, [], [f16019, f457])).
fof(f16019, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_233), inference(superposition, [], [f754, f5180])).
fof(f754, plain, ! [X0, X1] : (aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f753, f144])).
fof(f753, plain, ! [X0, X1] : (~ sP1(X0, X1) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(duplicate_literal_removal, [], [f747])).
fof(f747, plain, ! [X0, X1] : (~ sP1(X0, X1) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(resolution, [], [f211, f79])).
fof(f211, plain, ! [X10, X11, X9] : (~ aElementOf0(sK5(sdtpldt0(X9, X10), X11), X9) | ~ sP1(X9, X10) | aSubsetOf0(X11, sdtpldt0(X9, X10)) | ~ aSet0(X11)), inference(subsumption_resolution, [], [f210, f144])).
fof(f210, plain, ! [X10, X11, X9] : (~ aElementOf0(sK5(sdtpldt0(X9, X10), X11), X9) | ~ sP1(X9, X10) | aSubsetOf0(X11, sdtpldt0(X9, X10)) | ~ aSet0(X11) | ~ aSet0(sdtpldt0(X9, X10))), inference(subsumption_resolution, [], [f207, f166])).
fof(f207, plain, ! [X10, X11, X9] : (~ aElement0(sK5(sdtpldt0(X9, X10), X11)) | ~ aElementOf0(sK5(sdtpldt0(X9, X10), X11), X9) | ~ sP1(X9, X10) | aSubsetOf0(X11, sdtpldt0(X9, X10)) | ~ aSet0(X11) | ~ aSet0(sdtpldt0(X9, X10))), inference(resolution, [], [f185, f80])).
fof(f5061, plain, spl8_233, inference(avatar_contradiction_clause, [], [f5060])).
fof(f5060, plain, ($false | spl8_233), inference(subsumption_resolution, [], [f5059, f111])).
fof(f5059, plain, (~ aSet0(xS) | spl8_233), inference(subsumption_resolution, [], [f5058, f110])).
fof(f5058, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_233), inference(resolution, [], [f5041, f96])).
fof(f5041, plain, (~ sP1(xS, xx) | spl8_233), inference(avatar_component_clause, [], [f5039])).
fof(f3847, plain, spl8_143, inference(avatar_contradiction_clause, [], [f3846])).
fof(f3846, plain, ($false | spl8_143), inference(subsumption_resolution, [], [f3845, f111])).
fof(f3845, plain, (~ aSet0(xS) | spl8_143), inference(subsumption_resolution, [], [f3844, f110])).
fof(f3844, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_143), inference(resolution, [], [f3773, f96])).
fof(f3773, plain, (~ sP1(xS, xx) | spl8_143), inference(resolution, [], [f3640, f144])).
fof(f3640, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl8_143), inference(avatar_component_clause, [], [f3638])).
fof(f615, plain, spl8_17, inference(avatar_contradiction_clause, [], [f614])).
fof(f614, plain, ($false | spl8_17), inference(subsumption_resolution, [], [f613, f111])).
fof(f613, plain, (~ aSet0(xS) | spl8_17), inference(subsumption_resolution, [], [f612, f110])).
fof(f612, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_17), inference(resolution, [], [f578, f96])).
fof(f578, plain, (~ sP1(xS, xx) | spl8_17), inference(resolution, [], [f570, f144])).
fof(f570, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl8_17), inference(subsumption_resolution, [], [f569, f110])).
fof(f569, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(xS, xx)) | spl8_17), inference(resolution, [], [f546, f108])).
fof(f546, plain, (~ sP3(sdtpldt0(xS, xx), xx) | spl8_17), inference(avatar_component_clause, [], [f544])).
fof(f506, plain, spl8_12, inference(avatar_contradiction_clause, [], [f505])).
fof(f505, plain, ($false | spl8_12), inference(subsumption_resolution, [], [f504, f111])).
fof(f504, plain, (~ aSet0(xS) | spl8_12), inference(subsumption_resolution, [], [f503, f110])).
fof(f503, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_12), inference(resolution, [], [f489, f96])).
fof(f489, plain, (~ sP1(xS, xx) | spl8_12), inference(resolution, [], [f479, f144])).
fof(f479, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl8_12), inference(subsumption_resolution, [], [f478, f110])).
fof(f478, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(xS, xx)) | spl8_12), inference(resolution, [], [f464, f108])).
fof(f464, plain, (~ sP3(sdtpldt0(xS, xx), xx) | spl8_12), inference(resolution, [], [f458, f148])).
fof(f458, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_12), inference(avatar_component_clause, [], [f456])).
fof(f463, plain, (~ spl8_12 | spl8_13 | spl8_1), inference(avatar_split_clause, [], [f454, f128, f460, f456])).
fof(f454, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_1), inference(subsumption_resolution, [], [f442, f111])).
fof(f442, plain, (~ aSet0(xS) | (xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_1), inference(resolution, [], [f191, f130])).
fof(f191, plain, ! [X4, X3] : (aSubsetOf0(X3, X4) | ~ aSet0(X3) | (sdtpldt0(sdtmndt0(X3, sK5(X4, X3)), sK5(X4, X3)) = X3) | ~ aSet0(X4)), inference(duplicate_literal_removal, [], [f190])).
fof(f190, plain, ! [X4, X3] : ((sdtpldt0(sdtmndt0(X3, sK5(X4, X3)), sK5(X4, X3)) = X3) | ~ aSet0(X3) | aSubsetOf0(X3, X4) | ~ aSet0(X3) | ~ aSet0(X4)), inference(resolution, [], [f109, f79])).
fof(f135, plain, (~ spl8_1 | ~ spl8_2), inference(avatar_split_clause, [], [f113, f132, f128])).
fof(f113, plain, (~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx))), inference(negated_conjecture, [], [f20])).
fof(f20, plain, ~ (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) & aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM537+1.p', m__)).