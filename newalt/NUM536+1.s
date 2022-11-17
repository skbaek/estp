fof(f94193, plain, $false, inference(avatar_sat_refutation, [], [f338, f1165, f1186, f1214, f1391, f1438, f1785, f1807, f2277, f2289, f2328, f2378, f2565, f2760, f2816, f3700, f4087, f4164, f4579, f17584, f17710, f17774, f17864, f17903, f18102, f18176, f21356, f23550, f24228, f47953, f48485, f77866, f81259, f81928, f90224, f94192])).
fof(f94192, plain, (~ spl8_241 | ~ spl8_253 | ~ spl8_282 | ~ spl8_298 | spl8_299 | ~ spl8_327 | ~ spl8_341), inference(avatar_contradiction_clause, [], [f94191])).
fof(f94191, plain, ($false | (~ spl8_241 | ~ spl8_253 | ~ spl8_282 | ~ spl8_298 | spl8_299 | ~ spl8_327 | ~ spl8_341)), inference(subsumption_resolution, [], [f94190, f111])).
fof(f111, plain, aSet0(xS), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (aSet0(xS) & aElement0(xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', m__679)).
fof(f94190, plain, (~ aSet0(xS) | (~ spl8_241 | ~ spl8_253 | ~ spl8_282 | ~ spl8_298 | spl8_299 | ~ spl8_327 | ~ spl8_341)), inference(subsumption_resolution, [], [f94189, f17700])).
fof(f17700, plain, (aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_282), inference(avatar_component_clause, [], [f17699])).
fof(f17699, plain, (spl8_282 <=> aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_282])])).
fof(f94189, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | (~ spl8_241 | ~ spl8_253 | ~ spl8_282 | ~ spl8_298 | spl8_299 | ~ spl8_327 | ~ spl8_341)), inference(subsumption_resolution, [], [f94188, f90474])).
fof(f90474, plain, (~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | (~ spl8_282 | ~ spl8_341)), inference(subsumption_resolution, [], [f90473, f17700])).
fof(f90473, plain, (~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_341), inference(subsumption_resolution, [], [f90461, f113])).
fof(f113, plain, ~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)), inference(cnf_transformation, [], [f26])).
fof(f26, plain, ~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)), inference(flattening, [], [f21])).
fof(f21, plain, ~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)), inference(negated_conjecture, [], [f20])).
fof(f20, plain, ~ (xS = sdtmndt0(sdtpldt0(xS, xx), xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', m__)).
fof(f90461, plain, ((xS = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_341), inference(resolution, [], [f23422, f120])).
fof(f120, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | (X0 = X1) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f83, f77])).
fof(f77, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f57, f58])).
fof(f58, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f57, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f56])).
fof(f56, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f55])).
fof(f55, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mDefSub)).
fof(f83, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f35])).
fof(f35, plain, ! [X0, X1] : (((X0 = X1) | (~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X0) & aSubsetOf0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mSubASymm)).
fof(f23422, plain, (aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_341), inference(avatar_component_clause, [], [f23420])).
fof(f23420, plain, (spl8_341 <=> aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_341])])).
fof(f94188, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | (~ spl8_241 | ~ spl8_253 | ~ spl8_282 | ~ spl8_298 | spl8_299 | ~ spl8_327 | ~ spl8_341)), inference(subsumption_resolution, [], [f94184, f22820])).
fof(f22820, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_253 | ~ spl8_282 | ~ spl8_298 | spl8_299)), inference(subsumption_resolution, [], [f22819, f17700])).
fof(f22819, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_253 | ~ spl8_298 | spl8_299)), inference(subsumption_resolution, [], [f22818, f17577])).
fof(f17577, plain, (aSet0(sdtpldt0(xS, xx)) | ~ spl8_253), inference(avatar_component_clause, [], [f17575])).
fof(f17575, plain, (spl8_253 <=> aSet0(sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_253])])).
fof(f22818, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(xS, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_298 | spl8_299)), inference(subsumption_resolution, [], [f22816, f17772])).
fof(f17772, plain, (~ aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_299), inference(avatar_component_clause, [], [f17771])).
fof(f17771, plain, (spl8_299 <=> aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_299])])).
fof(f22816, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(xS, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_298), inference(superposition, [], [f80, f17769])).
fof(f17769, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))) | ~ spl8_298), inference(avatar_component_clause, [], [f17767])).
fof(f17767, plain, (spl8_298 <=> (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl8_298])])).
fof(f80, plain, ! [X0, X1] : (~ aElementOf0(sK5(X0, X1), X0) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f94184, plain, (aElementOf0(xx, sdtmndt0(sdtpldt0(xS, xx), xx)) | aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), xS) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(xS) | (~ spl8_241 | ~ spl8_282 | ~ spl8_327 | ~ spl8_341)), inference(superposition, [], [f79, f90536])).
fof(f90536, plain, ((xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | (~ spl8_241 | ~ spl8_282 | ~ spl8_327 | ~ spl8_341)), inference(subsumption_resolution, [], [f90535, f21327])).
fof(f21327, plain, (sP1(xS, xx) | ~ spl8_327), inference(avatar_component_clause, [], [f21326])).
fof(f21326, plain, (spl8_327 <=> sP1(xS, xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_327])])).
fof(f90535, plain, ((xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ sP1(xS, xx) | (~ spl8_241 | ~ spl8_282 | ~ spl8_341)), inference(subsumption_resolution, [], [f90534, f111])).
fof(f90534, plain, (~ aSet0(xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ sP1(xS, xx) | (~ spl8_241 | ~ spl8_282 | ~ spl8_341)), inference(subsumption_resolution, [], [f90514, f17525])).
fof(f17525, plain, (sP3(sdtpldt0(xS, xx), xx) | ~ spl8_241), inference(avatar_component_clause, [], [f17524])).
fof(f17524, plain, (spl8_241 <=> sP3(sdtpldt0(xS, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_241])])).
fof(f90514, plain, (~ sP3(sdtpldt0(xS, xx), xx) | ~ aSet0(xS) | (xx = sK5(xS, sdtmndt0(sdtpldt0(xS, xx), xx))) | ~ sP1(xS, xx) | (~ spl8_282 | ~ spl8_341)), inference(resolution, [], [f90474, f15530])).
fof(f15530, plain, ! [X2, X0, X1] : (aSubsetOf0(sdtmndt0(sdtpldt0(X0, X1), X2), X0) | ~ sP3(sdtpldt0(X0, X1), X2) | ~ aSet0(X0) | (sK5(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) = X1) | ~ sP1(X0, X1)), inference(subsumption_resolution, [], [f15529, f139])).
fof(f139, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f118, f99])).
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
fof(f15529, plain, ! [X2, X0, X1] : (aSubsetOf0(sdtmndt0(sdtpldt0(X0, X1), X2), X0) | ~ sP3(sdtpldt0(X0, X1), X2) | ~ aSet0(X0) | (sK5(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) = X1) | ~ sP1(X0, X1) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(duplicate_literal_removal, [], [f15494])).
fof(f15494, plain, ! [X2, X0, X1] : (aSubsetOf0(sdtmndt0(sdtpldt0(X0, X1), X2), X0) | ~ sP3(sdtpldt0(X0, X1), X2) | ~ aSet0(X0) | (sK5(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtmndt0(sdtpldt0(X0, X1), X2), X0) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0)), inference(resolution, [], [f614, f80])).
fof(f614, plain, ! [X14, X15, X13, X16] : (aElementOf0(sK5(X13, sdtmndt0(sdtpldt0(X14, X15), X16)), X14) | aSubsetOf0(sdtmndt0(sdtpldt0(X14, X15), X16), X13) | ~ sP3(sdtpldt0(X14, X15), X16) | ~ aSet0(X13) | (sK5(X13, sdtmndt0(sdtpldt0(X14, X15), X16)) = X15) | ~ sP1(X14, X15)), inference(resolution, [], [f159, f185])).
fof(f185, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtpldt0(X1, X2)) | aElementOf0(X0, X1) | (X0 = X2) | ~ sP1(X1, X2)), inference(resolution, [], [f89, f116])).
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
fof(f159, plain, ! [X6, X8, X7] : (aElementOf0(sK5(X8, sdtmndt0(X6, X7)), X6) | ~ aSet0(X8) | aSubsetOf0(sdtmndt0(X6, X7), X8) | ~ sP3(X6, X7)), inference(subsumption_resolution, [], [f155, f139])).
fof(f155, plain, ! [X6, X8, X7] : (aSubsetOf0(sdtmndt0(X6, X7), X8) | ~ aSet0(sdtmndt0(X6, X7)) | ~ aSet0(X8) | aElementOf0(sK5(X8, sdtmndt0(X6, X7)), X6) | ~ sP3(X6, X7)), inference(resolution, [], [f79, f140])).
fof(f140, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtmndt0(X1, X2)) | aElementOf0(X0, X1) | ~ sP3(X1, X2)), inference(resolution, [], [f101, f118])).
fof(f101, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | ~ aElementOf0(X4, X2) | aElementOf0(X4, X1)), inference(cnf_transformation, [], [f71])).
fof(f79, plain, ! [X0, X1] : (aElementOf0(sK5(X0, X1), X1) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f90224, plain, (~ spl8_241 | ~ spl8_282 | ~ spl8_327 | spl8_341 | ~ spl8_692 | spl8_738), inference(avatar_contradiction_clause, [], [f90223])).
fof(f90223, plain, ($false | (~ spl8_241 | ~ spl8_282 | ~ spl8_327 | spl8_341 | ~ spl8_692 | spl8_738)), inference(subsumption_resolution, [], [f90222, f51456])).
fof(f51456, plain, (~ (xS = sdtpldt0(xS, xx)) | spl8_738), inference(avatar_component_clause, [], [f51455])).
fof(f51455, plain, (spl8_738 <=> (xS = sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_738])])).
fof(f90222, plain, ((xS = sdtpldt0(xS, xx)) | (~ spl8_241 | ~ spl8_282 | ~ spl8_327 | spl8_341 | ~ spl8_692)), inference(forward_demodulation, [], [f90218, f48565])).
fof(f48565, plain, ((xS = sdtmndt0(xS, xx)) | ~ spl8_692), inference(avatar_component_clause, [], [f48563])).
fof(f48563, plain, (spl8_692 <=> (xS = sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_692])])).
fof(f90218, plain, ((xS = sdtpldt0(sdtmndt0(xS, xx), xx)) | (~ spl8_241 | ~ spl8_282 | ~ spl8_327 | spl8_341)), inference(backward_demodulation, [], [f26726, f90214])).
fof(f90214, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | (~ spl8_241 | ~ spl8_327 | spl8_341)), inference(subsumption_resolution, [], [f90213, f21327])).
fof(f90213, plain, ((xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (~ spl8_241 | spl8_341)), inference(subsumption_resolution, [], [f90212, f111])).
fof(f90212, plain, (~ aSet0(xS) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | (~ spl8_241 | spl8_341)), inference(subsumption_resolution, [], [f90187, f17525])).
fof(f90187, plain, (~ sP3(sdtpldt0(xS, xx), xx) | ~ aSet0(xS) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)) | ~ sP1(xS, xx) | spl8_341), inference(resolution, [], [f22622, f23421])).
fof(f23421, plain, (~ aSubsetOf0(xS, sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_341), inference(avatar_component_clause, [], [f23420])).
fof(f22622, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ sP3(sdtpldt0(X0, X1), X2) | ~ aSet0(X0) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1)), inference(subsumption_resolution, [], [f22621, f139])).
fof(f22621, plain, ! [X2, X0, X1] : (~ sP3(sdtpldt0(X0, X1), X2) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(subsumption_resolution, [], [f22620, f157])).
fof(f157, plain, ! [X0, X1] : (aElement0(sK5(X1, X0)) | ~ aSet0(X0) | ~ aSet0(X1) | aSubsetOf0(X0, X1)), inference(duplicate_literal_removal, [], [f152])).
fof(f152, plain, ! [X0, X1] : (aSubsetOf0(X0, X1) | ~ aSet0(X0) | ~ aSet0(X1) | aElement0(sK5(X1, X0)) | ~ aSet0(X0)), inference(resolution, [], [f79, f72])).
fof(f72, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mEOfElem)).
fof(f22620, plain, ! [X2, X0, X1] : (~ sP3(sdtpldt0(X0, X1), X2) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0)) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(duplicate_literal_removal, [], [f22589])).
fof(f22589, plain, ! [X2, X0, X1] : (~ sP3(sdtpldt0(X0, X1), X2) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0)) | (sK5(sdtmndt0(sdtpldt0(X0, X1), X2), X0) = X2) | ~ sP1(X0, X1) | aSubsetOf0(X0, sdtmndt0(sdtpldt0(X0, X1), X2)) | ~ aSet0(X0) | ~ aSet0(sdtmndt0(sdtpldt0(X0, X1), X2))), inference(resolution, [], [f971, f79])).
fof(f971, plain, ! [X10, X8, X11, X9] : (~ aElementOf0(sK5(sdtmndt0(sdtpldt0(X8, X9), X10), X11), X8) | ~ sP3(sdtpldt0(X8, X9), X10) | aSubsetOf0(X11, sdtmndt0(sdtpldt0(X8, X9), X10)) | ~ aSet0(X11) | ~ aElement0(sK5(sdtmndt0(sdtpldt0(X8, X9), X10), X11)) | (sK5(sdtmndt0(sdtpldt0(X8, X9), X10), X11) = X10) | ~ sP1(X8, X9)), inference(resolution, [], [f403, f176])).
fof(f176, plain, ! [X2, X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X2)) | ~ aElement0(X0) | ~ aElementOf0(X0, X1) | ~ sP1(X1, X2)), inference(resolution, [], [f90, f116])).
fof(f90, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f65])).
fof(f403, plain, ! [X19, X17, X18] : (~ aElementOf0(sK5(sdtmndt0(X17, X18), X19), X17) | (sK5(sdtmndt0(X17, X18), X19) = X18) | ~ sP3(X17, X18) | aSubsetOf0(X19, sdtmndt0(X17, X18)) | ~ aSet0(X19)), inference(subsumption_resolution, [], [f402, f139])).
fof(f402, plain, ! [X19, X17, X18] : (~ aElementOf0(sK5(sdtmndt0(X17, X18), X19), X17) | (sK5(sdtmndt0(X17, X18), X19) = X18) | ~ sP3(X17, X18) | aSubsetOf0(X19, sdtmndt0(X17, X18)) | ~ aSet0(X19) | ~ aSet0(sdtmndt0(X17, X18))), inference(subsumption_resolution, [], [f392, f157])).
fof(f392, plain, ! [X19, X17, X18] : (~ aElementOf0(sK5(sdtmndt0(X17, X18), X19), X17) | ~ aElement0(sK5(sdtmndt0(X17, X18), X19)) | (sK5(sdtmndt0(X17, X18), X19) = X18) | ~ sP3(X17, X18) | aSubsetOf0(X19, sdtmndt0(X17, X18)) | ~ aSet0(X19) | ~ aSet0(sdtmndt0(X17, X18))), inference(resolution, [], [f194, f80])).
fof(f194, plain, ! [X2, X0, X1] : (aElementOf0(X1, sdtmndt0(X2, X0)) | ~ aElementOf0(X1, X2) | ~ aElement0(X1) | (X0 = X1) | ~ sP3(X2, X0)), inference(resolution, [], [f103, f118])).
fof(f103, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f71])).
fof(f26726, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | (~ spl8_282 | spl8_341)), inference(subsumption_resolution, [], [f26725, f17700])).
fof(f26725, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_341), inference(subsumption_resolution, [], [f26717, f111])).
fof(f26717, plain, (~ aSet0(xS) | (xS = sdtpldt0(sdtmndt0(xS, sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS)), sK5(sdtmndt0(sdtpldt0(xS, xx), xx), xS))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_341), inference(resolution, [], [f23421, f182])).
fof(f182, plain, ! [X4, X3] : (aSubsetOf0(X3, X4) | ~ aSet0(X3) | (sdtpldt0(sdtmndt0(X3, sK5(X4, X3)), sK5(X4, X3)) = X3) | ~ aSet0(X4)), inference(duplicate_literal_removal, [], [f181])).
fof(f181, plain, ! [X4, X3] : ((sdtpldt0(sdtmndt0(X3, sK5(X4, X3)), sK5(X4, X3)) = X3) | ~ aSet0(X3) | aSubsetOf0(X3, X4) | ~ aSet0(X3) | ~ aSet0(X4)), inference(resolution, [], [f109, f79])).
fof(f109, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | (sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (! [X1] : ((sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => (sdtpldt0(sdtmndt0(X0, X1), X1) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mConsDiff)).
fof(f81928, plain, (~ spl8_254 | ~ spl8_738), inference(avatar_contradiction_clause, [], [f81927])).
fof(f81927, plain, ($false | (~ spl8_254 | ~ spl8_738)), inference(subsumption_resolution, [], [f81342, f112])).
fof(f112, plain, ~ aElementOf0(xx, xS), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ~ aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', m__679_02)).
fof(f81342, plain, (aElementOf0(xx, xS) | (~ spl8_254 | ~ spl8_738)), inference(backward_demodulation, [], [f17583, f51457])).
fof(f51457, plain, ((xS = sdtpldt0(xS, xx)) | ~ spl8_738), inference(avatar_component_clause, [], [f51455])).
fof(f17583, plain, (aElementOf0(xx, sdtpldt0(xS, xx)) | ~ spl8_254), inference(avatar_component_clause, [], [f17581])).
fof(f17581, plain, (spl8_254 <=> aElementOf0(xx, sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_254])])).
fof(f81259, plain, (spl8_692 | ~ spl8_3 | ~ spl8_13 | ~ spl8_116 | spl8_340 | ~ spl8_637 | ~ spl8_648 | ~ spl8_945), inference(avatar_split_clause, [], [f81258, f77863, f48304, f47853, f23416, f4547, f1258, f335, f48563])).
fof(f335, plain, (spl8_3 <=> (xS = sdtpldt0(sdtmndt0(xS, sK4(xS)), sK4(xS)))), introduced(avatar_definition, [new_symbols(naming, [spl8_3])])).
fof(f1258, plain, (spl8_13 <=> sP1(sdtmndt0(xS, sK4(xS)), sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_13])])).
fof(f4547, plain, (spl8_116 <=> sP3(xS, sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_116])])).
fof(f23416, plain, (spl8_340 <=> (xx = sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_340])])).
fof(f47853, plain, (spl8_637 <=> aSet0(sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_637])])).
fof(f48304, plain, (spl8_648 <=> sP3(xS, xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_648])])).
fof(f77863, plain, (spl8_945 <=> (xx = sK5(sdtmndt0(xS, xx), xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_945])])).
fof(f81258, plain, ((xS = sdtmndt0(xS, xx)) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | spl8_340 | ~ spl8_637 | ~ spl8_648 | ~ spl8_945)), inference(subsumption_resolution, [], [f81222, f111])).
fof(f81222, plain, ((xS = sdtmndt0(xS, xx)) | ~ aSet0(xS) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | spl8_340 | ~ spl8_637 | ~ spl8_648 | ~ spl8_945)), inference(subsumption_resolution, [], [f81206, f48305])).
fof(f48305, plain, (sP3(xS, xx) | ~ spl8_648), inference(avatar_component_clause, [], [f48304])).
fof(f81206, plain, (~ sP3(xS, xx) | (xS = sdtmndt0(xS, xx)) | ~ aSet0(xS) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | spl8_340 | ~ spl8_637 | ~ spl8_945)), inference(resolution, [], [f81117, f743])).
fof(f743, plain, ! [X4, X3] : (~ aSubsetOf0(X3, sdtmndt0(X3, X4)) | ~ sP3(X3, X4) | (sdtmndt0(X3, X4) = X3) | ~ aSet0(X3)), inference(duplicate_literal_removal, [], [f732])).
fof(f732, plain, ! [X4, X3] : (~ aSet0(X3) | ~ sP3(X3, X4) | (sdtmndt0(X3, X4) = X3) | ~ aSubsetOf0(X3, sdtmndt0(X3, X4)) | ~ aSet0(X3)), inference(resolution, [], [f621, f120])).
fof(f621, plain, ! [X0, X1] : (aSubsetOf0(sdtmndt0(X0, X1), X0) | ~ aSet0(X0) | ~ sP3(X0, X1)), inference(subsumption_resolution, [], [f620, f139])).
fof(f620, plain, ! [X0, X1] : (~ aSet0(X0) | aSubsetOf0(sdtmndt0(X0, X1), X0) | ~ sP3(X0, X1) | ~ aSet0(sdtmndt0(X0, X1))), inference(duplicate_literal_removal, [], [f609])).
fof(f609, plain, ! [X0, X1] : (~ aSet0(X0) | aSubsetOf0(sdtmndt0(X0, X1), X0) | ~ sP3(X0, X1) | aSubsetOf0(sdtmndt0(X0, X1), X0) | ~ aSet0(sdtmndt0(X0, X1)) | ~ aSet0(X0)), inference(resolution, [], [f159, f80])).
fof(f81117, plain, (aSubsetOf0(xS, sdtmndt0(xS, xx)) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | spl8_340 | ~ spl8_637 | ~ spl8_945)), inference(subsumption_resolution, [], [f81116, f23417])).
fof(f23417, plain, (~ (xx = sK4(xS)) | spl8_340), inference(avatar_component_clause, [], [f23416])).
fof(f81116, plain, (aSubsetOf0(xS, sdtmndt0(xS, xx)) | (xx = sK4(xS)) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | ~ spl8_637 | ~ spl8_945)), inference(subsumption_resolution, [], [f81115, f47854])).
fof(f47854, plain, (aSet0(sdtmndt0(xS, xx)) | ~ spl8_637), inference(avatar_component_clause, [], [f47853])).
fof(f81115, plain, (aSubsetOf0(xS, sdtmndt0(xS, xx)) | ~ aSet0(sdtmndt0(xS, xx)) | (xx = sK4(xS)) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | ~ spl8_945)), inference(subsumption_resolution, [], [f81108, f112])).
fof(f81108, plain, (aElementOf0(xx, xS) | aSubsetOf0(xS, sdtmndt0(xS, xx)) | ~ aSet0(sdtmndt0(xS, xx)) | (xx = sK4(xS)) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116 | ~ spl8_945)), inference(superposition, [], [f26659, f77865])).
fof(f77865, plain, ((xx = sK5(sdtmndt0(xS, xx), xS)) | ~ spl8_945), inference(avatar_component_clause, [], [f77863])).
fof(f26659, plain, (! [X1] : (aElementOf0(sK5(X1, xS), xS) | aSubsetOf0(xS, X1) | ~ aSet0(X1) | (sK4(xS) = sK5(X1, xS))) | (~ spl8_3 | ~ spl8_13 | ~ spl8_116)), inference(subsumption_resolution, [], [f26658, f4548])).
fof(f4548, plain, (sP3(xS, sK4(xS)) | ~ spl8_116), inference(avatar_component_clause, [], [f4547])).
fof(f26658, plain, (! [X1] : (aElementOf0(sK5(X1, xS), xS) | aSubsetOf0(xS, X1) | ~ aSet0(X1) | (sK4(xS) = sK5(X1, xS)) | ~ sP3(xS, sK4(xS))) | (~ spl8_3 | ~ spl8_13)), inference(subsumption_resolution, [], [f25156, f1259])).
fof(f1259, plain, (sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | ~ spl8_13), inference(avatar_component_clause, [], [f1258])).
fof(f25156, plain, (! [X1] : (aElementOf0(sK5(X1, xS), xS) | ~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | aSubsetOf0(xS, X1) | ~ aSet0(X1) | (sK4(xS) = sK5(X1, xS)) | ~ sP3(xS, sK4(xS))) | ~ spl8_3), inference(superposition, [], [f933, f337])).
fof(f337, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK4(xS)), sK4(xS))) | ~ spl8_3), inference(avatar_component_clause, [], [f335])).
fof(f933, plain, ! [X30, X28, X31, X29] : (aElementOf0(sK5(X28, sdtpldt0(sdtmndt0(X29, X30), X31)), X29) | ~ sP1(sdtmndt0(X29, X30), X31) | aSubsetOf0(sdtpldt0(sdtmndt0(X29, X30), X31), X28) | ~ aSet0(X28) | (sK5(X28, sdtpldt0(sdtmndt0(X29, X30), X31)) = X31) | ~ sP3(X29, X30)), inference(resolution, [], [f362, f140])).
fof(f362, plain, ! [X8, X7, X9] : (aElementOf0(sK5(X7, sdtpldt0(X8, X9)), X8) | (sK5(X7, sdtpldt0(X8, X9)) = X9) | ~ sP1(X8, X9) | aSubsetOf0(sdtpldt0(X8, X9), X7) | ~ aSet0(X7)), inference(subsumption_resolution, [], [f351, f135])).
fof(f135, plain, ! [X4, X3] : (aSet0(sdtpldt0(X3, X4)) | ~ sP1(X3, X4)), inference(resolution, [], [f116, f87])).
fof(f87, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f65])).
fof(f351, plain, ! [X8, X7, X9] : (aElementOf0(sK5(X7, sdtpldt0(X8, X9)), X8) | (sK5(X7, sdtpldt0(X8, X9)) = X9) | ~ sP1(X8, X9) | aSubsetOf0(sdtpldt0(X8, X9), X7) | ~ aSet0(sdtpldt0(X8, X9)) | ~ aSet0(X7)), inference(resolution, [], [f185, f79])).
fof(f77866, plain, (spl8_692 | spl8_945 | ~ spl8_648), inference(avatar_split_clause, [], [f77861, f48304, f77863, f48563])).
fof(f77861, plain, ((xx = sK5(sdtmndt0(xS, xx), xS)) | (xS = sdtmndt0(xS, xx)) | ~ spl8_648), inference(subsumption_resolution, [], [f51477, f111])).
fof(f51477, plain, ((xx = sK5(sdtmndt0(xS, xx), xS)) | ~ aSet0(xS) | (xS = sdtmndt0(xS, xx)) | ~ spl8_648), inference(resolution, [], [f2933, f48305])).
fof(f2933, plain, ! [X8, X7] : (~ sP3(X7, X8) | (sK5(sdtmndt0(X7, X8), X7) = X8) | ~ aSet0(X7) | (sdtmndt0(X7, X8) = X7)), inference(subsumption_resolution, [], [f2932, f621])).
fof(f2932, plain, ! [X8, X7] : (~ sP3(X7, X8) | (sK5(sdtmndt0(X7, X8), X7) = X8) | ~ aSet0(X7) | (sdtmndt0(X7, X8) = X7) | ~ aSubsetOf0(sdtmndt0(X7, X8), X7)), inference(subsumption_resolution, [], [f2922, f77])).
fof(f2922, plain, ! [X8, X7] : (~ sP3(X7, X8) | (sK5(sdtmndt0(X7, X8), X7) = X8) | ~ aSet0(X7) | (sdtmndt0(X7, X8) = X7) | ~ aSubsetOf0(sdtmndt0(X7, X8), X7) | ~ aSet0(sdtmndt0(X7, X8))), inference(resolution, [], [f979, f120])).
fof(f979, plain, ! [X0, X1] : (aSubsetOf0(X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1) | (sK5(sdtmndt0(X0, X1), X0) = X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f978, f139])).
fof(f978, plain, ! [X0, X1] : ((sK5(sdtmndt0(X0, X1), X0) = X1) | ~ sP3(X0, X1) | aSubsetOf0(X0, sdtmndt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtmndt0(X0, X1))), inference(duplicate_literal_removal, [], [f968])).
fof(f968, plain, ! [X0, X1] : ((sK5(sdtmndt0(X0, X1), X0) = X1) | ~ sP3(X0, X1) | aSubsetOf0(X0, sdtmndt0(X0, X1)) | ~ aSet0(X0) | aSubsetOf0(X0, sdtmndt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtmndt0(X0, X1))), inference(resolution, [], [f403, f79])).
fof(f48485, plain, spl8_648, inference(avatar_contradiction_clause, [], [f48484])).
fof(f48484, plain, ($false | spl8_648), inference(subsumption_resolution, [], [f48483, f111])).
fof(f48483, plain, (~ aSet0(xS) | spl8_648), inference(subsumption_resolution, [], [f48482, f110])).
fof(f110, plain, aElement0(xx), inference(cnf_transformation, [], [f18])).
fof(f48482, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_648), inference(resolution, [], [f48306, f108])).
fof(f108, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f42, e48, e47])).
fof(f42, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f41])).
fof(f41, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mDefDiff)).
fof(f48306, plain, (~ sP3(xS, xx) | spl8_648), inference(avatar_component_clause, [], [f48304])).
fof(f47953, plain, spl8_637, inference(avatar_contradiction_clause, [], [f47952])).
fof(f47952, plain, ($false | spl8_637), inference(subsumption_resolution, [], [f47951, f111])).
fof(f47951, plain, (~ aSet0(xS) | spl8_637), inference(subsumption_resolution, [], [f47950, f110])).
fof(f47950, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_637), inference(resolution, [], [f47923, f108])).
fof(f47923, plain, (~ sP3(xS, xx) | spl8_637), inference(resolution, [], [f47855, f139])).
fof(f47855, plain, (~ aSet0(sdtmndt0(xS, xx)) | spl8_637), inference(avatar_component_clause, [], [f47853])).
fof(f24228, plain, (~ spl8_17 | ~ spl8_340), inference(avatar_contradiction_clause, [], [f24227])).
fof(f24227, plain, ($false | (~ spl8_17 | ~ spl8_340)), inference(subsumption_resolution, [], [f23556, f112])).
fof(f23556, plain, (aElementOf0(xx, xS) | (~ spl8_17 | ~ spl8_340)), inference(backward_demodulation, [], [f1277, f23418])).
fof(f23418, plain, ((xx = sK4(xS)) | ~ spl8_340), inference(avatar_component_clause, [], [f23416])).
fof(f1277, plain, (aElementOf0(sK4(xS), xS) | ~ spl8_17), inference(avatar_component_clause, [], [f1275])).
fof(f1275, plain, (spl8_17 <=> aElementOf0(sK4(xS), xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_17])])).
fof(f23550, plain, spl8_327, inference(avatar_contradiction_clause, [], [f23549])).
fof(f23549, plain, ($false | spl8_327), inference(subsumption_resolution, [], [f23548, f111])).
fof(f23548, plain, (~ aSet0(xS) | spl8_327), inference(subsumption_resolution, [], [f23547, f110])).
fof(f23547, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_327), inference(resolution, [], [f21328, f96])).
fof(f96, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f40, e45, e44])).
fof(f40, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f39])).
fof(f39, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mDefCons)).
fof(f21328, plain, (~ sP1(xS, xx) | spl8_327), inference(avatar_component_clause, [], [f21326])).
fof(f21356, plain, (~ spl8_241 | ~ spl8_253 | ~ spl8_254 | ~ spl8_284 | ~ spl8_299), inference(avatar_contradiction_clause, [], [f21355])).
fof(f21355, plain, ($false | (~ spl8_241 | ~ spl8_253 | ~ spl8_254 | ~ spl8_284 | ~ spl8_299)), inference(subsumption_resolution, [], [f21354, f17525])).
fof(f21354, plain, (~ sP3(sdtpldt0(xS, xx), xx) | (~ spl8_253 | ~ spl8_254 | ~ spl8_284 | ~ spl8_299)), inference(subsumption_resolution, [], [f21213, f17583])).
fof(f21213, plain, (~ aElementOf0(xx, sdtpldt0(xS, xx)) | ~ sP3(sdtpldt0(xS, xx), xx) | (~ spl8_253 | ~ spl8_284 | ~ spl8_299)), inference(superposition, [], [f138, f20351])).
fof(f20351, plain, ((sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx)) | (~ spl8_253 | ~ spl8_284 | ~ spl8_299)), inference(subsumption_resolution, [], [f20350, f17577])).
fof(f20350, plain, ((sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(xS, xx)) | (~ spl8_284 | ~ spl8_299)), inference(subsumption_resolution, [], [f20346, f17773])).
fof(f17773, plain, (aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ spl8_299), inference(avatar_component_clause, [], [f17771])).
fof(f20346, plain, ((sdtpldt0(xS, xx) = sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ aSet0(sdtpldt0(xS, xx)) | ~ spl8_284), inference(resolution, [], [f17709, f120])).
fof(f17709, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)) | ~ spl8_284), inference(avatar_component_clause, [], [f17707])).
fof(f17707, plain, (spl8_284 <=> aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_284])])).
fof(f138, plain, ! [X4, X3] : (~ aElementOf0(X4, sdtmndt0(X3, X4)) | ~ sP3(X3, X4)), inference(resolution, [], [f118, f119])).
fof(f119, plain, ! [X4, X2, X1] : (~ sP2(X4, X1, X2) | ~ aElementOf0(X4, X2)), inference(equality_resolution, [], [f102])).
fof(f102, plain, ! [X4, X2, X0, X1] : (~ (X0 = X4) | ~ aElementOf0(X4, X2) | ~ sP2(X0, X1, X2)), inference(cnf_transformation, [], [f71])).
fof(f18176, plain, spl8_253, inference(avatar_contradiction_clause, [], [f18175])).
fof(f18175, plain, ($false | spl8_253), inference(subsumption_resolution, [], [f18174, f111])).
fof(f18174, plain, (~ aSet0(xS) | spl8_253), inference(subsumption_resolution, [], [f18173, f110])).
fof(f18173, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl8_253), inference(resolution, [], [f18120, f96])).
fof(f18120, plain, (~ sP1(xS, xx) | spl8_253), inference(resolution, [], [f17576, f135])).
fof(f17576, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl8_253), inference(avatar_component_clause, [], [f17575])).
fof(f18102, plain, (~ spl8_253 | spl8_241), inference(avatar_split_clause, [], [f18101, f17524, f17575])).
fof(f18101, plain, (~ aSet0(sdtpldt0(xS, xx)) | spl8_241), inference(subsumption_resolution, [], [f18100, f110])).
fof(f18100, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(xS, xx)) | spl8_241), inference(resolution, [], [f17526, f108])).
fof(f17526, plain, (~ sP3(sdtpldt0(xS, xx), xx) | spl8_241), inference(avatar_component_clause, [], [f17524])).
fof(f17903, plain, (~ spl8_241 | spl8_282), inference(avatar_split_clause, [], [f17902, f17699, f17524])).
fof(f17902, plain, (~ sP3(sdtpldt0(xS, xx), xx) | spl8_282), inference(resolution, [], [f17701, f139])).
fof(f17701, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_282), inference(avatar_component_clause, [], [f17699])).
fof(f17864, plain, (~ spl8_282 | spl8_244), inference(avatar_split_clause, [], [f17863, f17536, f17699])).
fof(f17536, plain, (spl8_244 <=> sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_244])])).
fof(f17863, plain, (~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_244), inference(subsumption_resolution, [], [f17862, f110])).
fof(f17862, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx)) | spl8_244), inference(resolution, [], [f17538, f96])).
fof(f17538, plain, (~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | spl8_244), inference(avatar_component_clause, [], [f17536])).
fof(f17774, plain, (~ spl8_282 | spl8_298 | ~ spl8_244 | spl8_299), inference(avatar_split_clause, [], [f17504, f17771, f17536, f17767, f17699])).
fof(f17504, plain, (aSubsetOf0(sdtpldt0(xS, xx), sdtmndt0(sdtpldt0(xS, xx), xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | (xx = sK5(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx))) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))), inference(superposition, [], [f938, f17408])).
fof(f17408, plain, (sdtpldt0(xS, xx) = sdtpldt0(sdtmndt0(sdtpldt0(xS, xx), xx), xx)), inference(resolution, [], [f2616, f111])).
fof(f2616, plain, ! [X0] : (~ aSet0(X0) | (sdtpldt0(X0, xx) = sdtpldt0(sdtmndt0(sdtpldt0(X0, xx), xx), xx))), inference(resolution, [], [f567, f110])).
fof(f567, plain, ! [X0, X1] : (~ aElement0(X0) | (sdtpldt0(X1, X0) = sdtpldt0(sdtmndt0(sdtpldt0(X1, X0), X0), X0)) | ~ aSet0(X1)), inference(duplicate_literal_removal, [], [f566])).
fof(f566, plain, ! [X0, X1] : (~ aElement0(X0) | (sdtpldt0(X1, X0) = sdtpldt0(sdtmndt0(sdtpldt0(X1, X0), X0), X0)) | ~ aElement0(X0) | ~ aSet0(X1)), inference(resolution, [], [f184, f96])).
fof(f184, plain, ! [X2, X1] : (~ sP1(X1, X2) | ~ aElement0(X2) | (sdtpldt0(X1, X2) = sdtpldt0(sdtmndt0(sdtpldt0(X1, X2), X2), X2))), inference(subsumption_resolution, [], [f180, f135])).
fof(f180, plain, ! [X2, X1] : ((sdtpldt0(X1, X2) = sdtpldt0(sdtmndt0(sdtpldt0(X1, X2), X2), X2)) | ~ aSet0(sdtpldt0(X1, X2)) | ~ aElement0(X2) | ~ sP1(X1, X2)), inference(resolution, [], [f109, f136])).
fof(f136, plain, ! [X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X0)) | ~ aElement0(X0) | ~ sP1(X1, X0)), inference(resolution, [], [f117, f116])).
fof(f117, plain, ! [X4, X2, X1] : (~ sP0(X4, X1, X2) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(equality_resolution, [], [f91])).
fof(f91, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (X0 = X4) | ~ aElement0(X4) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f65])).
fof(f938, plain, ! [X0, X1] : (aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ sP1(X0, X1) | (sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f937, f135])).
fof(f937, plain, ! [X0, X1] : ((sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(duplicate_literal_removal, [], [f924])).
fof(f924, plain, ! [X0, X1] : ((sK5(X0, sdtpldt0(X0, X1)) = X1) | ~ sP1(X0, X1) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(X0) | aSubsetOf0(sdtpldt0(X0, X1), X0) | ~ aSet0(sdtpldt0(X0, X1)) | ~ aSet0(X0)), inference(resolution, [], [f362, f80])).
fof(f17710, plain, (~ spl8_282 | ~ spl8_244 | spl8_284), inference(avatar_split_clause, [], [f17489, f17707, f17536, f17699])).
fof(f17489, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(xS, xx), xx), sdtpldt0(xS, xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx) | ~ aSet0(sdtmndt0(sdtpldt0(xS, xx), xx))), inference(superposition, [], [f650, f17408])).
fof(f650, plain, ! [X0, X1] : (aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f649, f135])).
fof(f649, plain, ! [X0, X1] : (~ sP1(X0, X1) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(duplicate_literal_removal, [], [f643])).
fof(f643, plain, ! [X0, X1] : (~ sP1(X0, X1) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | aSubsetOf0(X0, sdtpldt0(X0, X1)) | ~ aSet0(X0) | ~ aSet0(sdtpldt0(X0, X1))), inference(resolution, [], [f202, f79])).
fof(f202, plain, ! [X10, X11, X9] : (~ aElementOf0(sK5(sdtpldt0(X9, X10), X11), X9) | ~ sP1(X9, X10) | aSubsetOf0(X11, sdtpldt0(X9, X10)) | ~ aSet0(X11)), inference(subsumption_resolution, [], [f201, f135])).
fof(f201, plain, ! [X10, X11, X9] : (~ aElementOf0(sK5(sdtpldt0(X9, X10), X11), X9) | ~ sP1(X9, X10) | aSubsetOf0(X11, sdtpldt0(X9, X10)) | ~ aSet0(X11) | ~ aSet0(sdtpldt0(X9, X10))), inference(subsumption_resolution, [], [f198, f157])).
fof(f198, plain, ! [X10, X11, X9] : (~ aElement0(sK5(sdtpldt0(X9, X10), X11)) | ~ aElementOf0(sK5(sdtpldt0(X9, X10), X11), X9) | ~ sP1(X9, X10) | aSubsetOf0(X11, sdtpldt0(X9, X10)) | ~ aSet0(X11) | ~ aSet0(sdtpldt0(X9, X10))), inference(resolution, [], [f176, f80])).
fof(f17584, plain, (~ spl8_244 | spl8_254), inference(avatar_split_clause, [], [f17579, f17581, f17536])).
fof(f17579, plain, (aElementOf0(xx, sdtpldt0(xS, xx)) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx)), inference(subsumption_resolution, [], [f17456, f110])).
fof(f17456, plain, (aElementOf0(xx, sdtpldt0(xS, xx)) | ~ aElement0(xx) | ~ sP1(sdtmndt0(sdtpldt0(xS, xx), xx), xx)), inference(superposition, [], [f136, f17408])).
fof(f4579, plain, (~ spl8_16 | spl8_116), inference(avatar_contradiction_clause, [], [f4578])).
fof(f4578, plain, ($false | (~ spl8_16 | spl8_116)), inference(subsumption_resolution, [], [f4577, f111])).
fof(f4577, plain, (~ aSet0(xS) | (~ spl8_16 | spl8_116)), inference(subsumption_resolution, [], [f4576, f1272])).
fof(f1272, plain, (aElement0(sK4(xS)) | ~ spl8_16), inference(avatar_component_clause, [], [f1271])).
fof(f1271, plain, (spl8_16 <=> aElement0(sK4(xS))), introduced(avatar_definition, [new_symbols(naming, [spl8_16])])).
fof(f4576, plain, (~ aElement0(sK4(xS)) | ~ aSet0(xS) | spl8_116), inference(resolution, [], [f4549, f108])).
fof(f4549, plain, (~ sP3(xS, sK4(xS)) | spl8_116), inference(avatar_component_clause, [], [f4547])).
fof(f4164, plain, (~ spl8_13 | spl8_17 | ~ spl8_3 | ~ spl8_16), inference(avatar_split_clause, [], [f4163, f1271, f335, f1275, f1258])).
fof(f4163, plain, (aElementOf0(sK4(xS), xS) | ~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | (~ spl8_3 | ~ spl8_16)), inference(subsumption_resolution, [], [f4040, f1272])).
fof(f4040, plain, (aElementOf0(sK4(xS), xS) | ~ aElement0(sK4(xS)) | ~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | ~ spl8_3), inference(superposition, [], [f136, f337])).
fof(f4087, plain, (~ spl8_16 | spl8_26), inference(avatar_contradiction_clause, [], [f4086])).
fof(f4086, plain, ($false | (~ spl8_16 | spl8_26)), inference(subsumption_resolution, [], [f4085, f111])).
fof(f4085, plain, (~ aSet0(xS) | (~ spl8_16 | spl8_26)), inference(subsumption_resolution, [], [f4084, f1272])).
fof(f4084, plain, (~ aElement0(sK4(xS)) | ~ aSet0(xS) | spl8_26), inference(resolution, [], [f4082, f108])).
fof(f4082, plain, (~ sP3(xS, sK4(xS)) | spl8_26), inference(resolution, [], [f1314, f139])).
fof(f1314, plain, (~ aSet0(sdtmndt0(xS, sK4(xS))) | spl8_26), inference(avatar_component_clause, [], [f1312])).
fof(f1312, plain, (spl8_26 <=> aSet0(sdtmndt0(xS, sK4(xS)))), introduced(avatar_definition, [new_symbols(naming, [spl8_26])])).
fof(f3700, plain, (~ spl8_1 | ~ spl8_11 | ~ spl8_33 | ~ spl8_62 | ~ spl8_66 | spl8_73 | ~ spl8_75), inference(avatar_contradiction_clause, [], [f3699])).
fof(f3699, plain, ($false | (~ spl8_1 | ~ spl8_11 | ~ spl8_33 | ~ spl8_62 | ~ spl8_66 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f3698, f2271])).
fof(f2271, plain, (aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ spl8_62), inference(avatar_component_clause, [], [f2270])).
fof(f2270, plain, (spl8_62 <=> aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_62])])).
fof(f3698, plain, (~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_1 | ~ spl8_11 | ~ spl8_33 | ~ spl8_62 | ~ spl8_66 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f3697, f1854])).
fof(f1854, plain, (~ (slcrc0 = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ spl8_1), inference(backward_demodulation, [], [f113, f295])).
fof(f295, plain, ((slcrc0 = xS) | ~ spl8_1), inference(avatar_component_clause, [], [f293])).
fof(f293, plain, (spl8_1 <=> (slcrc0 = xS)), introduced(avatar_definition, [new_symbols(naming, [spl8_1])])).
fof(f3697, plain, ((slcrc0 = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_1 | ~ spl8_11 | ~ spl8_33 | ~ spl8_62 | ~ spl8_66 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f3696, f3170])).
fof(f3170, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_11 | ~ spl8_62 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f3169, f2271])).
fof(f3169, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_11 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f3168, f1180])).
fof(f1180, plain, (aSet0(sdtpldt0(slcrc0, xx)) | ~ spl8_11), inference(avatar_component_clause, [], [f1179])).
fof(f1179, plain, (spl8_11 <=> aSet0(sdtpldt0(slcrc0, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_11])])).
fof(f3168, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtpldt0(slcrc0, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_11 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f3166, f2564])).
fof(f2564, plain, (~ aSubsetOf0(sdtpldt0(slcrc0, xx), sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | spl8_73), inference(avatar_component_clause, [], [f2562])).
fof(f2562, plain, (spl8_73 <=> aSubsetOf0(sdtpldt0(slcrc0, xx), sdtmndt0(sdtpldt0(slcrc0, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_73])])).
fof(f3166, plain, (~ aElementOf0(xx, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | aSubsetOf0(sdtpldt0(slcrc0, xx), sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtpldt0(slcrc0, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_11 | spl8_73 | ~ spl8_75)), inference(superposition, [], [f80, f2963])).
fof(f2963, plain, ((xx = sK5(sdtmndt0(sdtpldt0(slcrc0, xx), xx), sdtpldt0(slcrc0, xx))) | (~ spl8_11 | spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f2962, f1180])).
fof(f2962, plain, ((xx = sK5(sdtmndt0(sdtpldt0(slcrc0, xx), xx), sdtpldt0(slcrc0, xx))) | ~ aSet0(sdtpldt0(slcrc0, xx)) | (spl8_73 | ~ spl8_75)), inference(subsumption_resolution, [], [f2957, f2752])).
fof(f2752, plain, (sP3(sdtpldt0(slcrc0, xx), xx) | ~ spl8_75), inference(avatar_component_clause, [], [f2751])).
fof(f2751, plain, (spl8_75 <=> sP3(sdtpldt0(slcrc0, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_75])])).
fof(f2957, plain, (~ sP3(sdtpldt0(slcrc0, xx), xx) | (xx = sK5(sdtmndt0(sdtpldt0(slcrc0, xx), xx), sdtpldt0(slcrc0, xx))) | ~ aSet0(sdtpldt0(slcrc0, xx)) | spl8_73), inference(resolution, [], [f2564, f979])).
fof(f3696, plain, (aElementOf0(xx, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (slcrc0 = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_1 | ~ spl8_33 | ~ spl8_62 | ~ spl8_66)), inference(superposition, [], [f75, f3588])).
fof(f3588, plain, ((xx = sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx))) | (~ spl8_1 | ~ spl8_33 | ~ spl8_62 | ~ spl8_66)), inference(subsumption_resolution, [], [f3587, f1377])).
fof(f1377, plain, (sP1(slcrc0, xx) | ~ spl8_33), inference(avatar_component_clause, [], [f1376])).
fof(f1376, plain, (spl8_33 <=> sP1(slcrc0, xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_33])])).
fof(f3587, plain, ((xx = sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx))) | ~ sP1(slcrc0, xx) | (~ spl8_1 | ~ spl8_62 | ~ spl8_66)), inference(subsumption_resolution, [], [f3577, f114])).
fof(f114, plain, ! [X2] : ~ aElementOf0(X2, slcrc0), inference(equality_resolution, [], [f74])).
fof(f74, plain, ! [X2, X0] : (~ aElementOf0(X2, X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ! [X0] : (((slcrc0 = X0) | aElementOf0(sK4(X0), X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f52, f53])).
fof(f53, plain, ! [X0] : (? [X1] : aElementOf0(X1, X0) => aElementOf0(sK4(X0), X0)), introduced(choice_axiom, [])).
fof(f52, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(rectify, [], [f51])).
fof(f51, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(flattening, [], [f50])).
fof(f50, plain, ! [X0] : (((slcrc0 = X0) | (? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0))) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(nnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : ((slcrc0 = X0) <=> (! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : ((slcrc0 = X0) <=> (~ ? [X1] : aElementOf0(X1, X0) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM536+1.p', mDefEmp)).
fof(f3577, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx)), slcrc0) | (xx = sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx))) | ~ sP1(slcrc0, xx) | (~ spl8_1 | ~ spl8_62 | ~ spl8_66)), inference(resolution, [], [f3557, f185])).
fof(f3557, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx)), sdtpldt0(slcrc0, xx)) | (~ spl8_1 | ~ spl8_62 | ~ spl8_66)), inference(subsumption_resolution, [], [f3556, f2271])).
fof(f3556, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx)), sdtpldt0(slcrc0, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_1 | ~ spl8_66)), inference(subsumption_resolution, [], [f3501, f1854])).
fof(f3501, plain, (aElementOf0(sK4(sdtmndt0(sdtpldt0(slcrc0, xx), xx)), sdtpldt0(slcrc0, xx)) | (slcrc0 = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ spl8_66), inference(resolution, [], [f2288, f75])).
fof(f2288, plain, (! [X34] : (~ aElementOf0(X34, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | aElementOf0(X34, sdtpldt0(slcrc0, xx))) | ~ spl8_66), inference(avatar_component_clause, [], [f2287])).
fof(f2287, plain, (spl8_66 <=> ! [X34] : (aElementOf0(X34, sdtpldt0(slcrc0, xx)) | ~ aElementOf0(X34, sdtmndt0(sdtpldt0(slcrc0, xx), xx)))), introduced(avatar_definition, [new_symbols(naming, [spl8_66])])).
fof(f75, plain, ! [X0] : (aElementOf0(sK4(X0), X0) | (slcrc0 = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f54])).
fof(f2816, plain, (~ spl8_11 | spl8_75), inference(avatar_contradiction_clause, [], [f2815])).
fof(f2815, plain, ($false | (~ spl8_11 | spl8_75)), inference(subsumption_resolution, [], [f2814, f1180])).
fof(f2814, plain, (~ aSet0(sdtpldt0(slcrc0, xx)) | spl8_75), inference(subsumption_resolution, [], [f2813, f110])).
fof(f2813, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(slcrc0, xx)) | spl8_75), inference(resolution, [], [f2753, f108])).
fof(f2753, plain, (~ sP3(sdtpldt0(slcrc0, xx), xx) | spl8_75), inference(avatar_component_clause, [], [f2751])).
fof(f2760, plain, (~ spl8_75 | ~ spl8_12 | ~ spl8_72), inference(avatar_split_clause, [], [f2759, f2475, f1183, f2751])).
fof(f1183, plain, (spl8_12 <=> aElementOf0(xx, sdtpldt0(slcrc0, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_12])])).
fof(f2475, plain, (spl8_72 <=> (sdtpldt0(slcrc0, xx) = sdtmndt0(sdtpldt0(slcrc0, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_72])])).
fof(f2759, plain, (~ sP3(sdtpldt0(slcrc0, xx), xx) | (~ spl8_12 | ~ spl8_72)), inference(subsumption_resolution, [], [f2718, f1185])).
fof(f1185, plain, (aElementOf0(xx, sdtpldt0(slcrc0, xx)) | ~ spl8_12), inference(avatar_component_clause, [], [f1183])).
fof(f2718, plain, (~ aElementOf0(xx, sdtpldt0(slcrc0, xx)) | ~ sP3(sdtpldt0(slcrc0, xx), xx) | ~ spl8_72), inference(superposition, [], [f138, f2477])).
fof(f2477, plain, ((sdtpldt0(slcrc0, xx) = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ spl8_72), inference(avatar_component_clause, [], [f2475])).
fof(f2565, plain, (~ spl8_73 | spl8_72 | ~ spl8_11 | ~ spl8_63), inference(avatar_split_clause, [], [f2560, f2274, f1179, f2475, f2562])).
fof(f2274, plain, (spl8_63 <=> aSubsetOf0(sdtmndt0(sdtpldt0(slcrc0, xx), xx), sdtpldt0(slcrc0, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_63])])).
fof(f2560, plain, ((sdtpldt0(slcrc0, xx) = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSubsetOf0(sdtpldt0(slcrc0, xx), sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_11 | ~ spl8_63)), inference(subsumption_resolution, [], [f2556, f1180])).
fof(f2556, plain, ((sdtpldt0(slcrc0, xx) = sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSubsetOf0(sdtpldt0(slcrc0, xx), sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aSet0(sdtpldt0(slcrc0, xx)) | ~ spl8_63), inference(resolution, [], [f2276, f120])).
fof(f2276, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(slcrc0, xx), xx), sdtpldt0(slcrc0, xx)) | ~ spl8_63), inference(avatar_component_clause, [], [f2274])).
fof(f2378, plain, (~ spl8_11 | spl8_62), inference(avatar_contradiction_clause, [], [f2377])).
fof(f2377, plain, ($false | (~ spl8_11 | spl8_62)), inference(subsumption_resolution, [], [f2376, f1180])).
fof(f2376, plain, (~ aSet0(sdtpldt0(slcrc0, xx)) | spl8_62), inference(subsumption_resolution, [], [f2375, f110])).
fof(f2375, plain, (~ aElement0(xx) | ~ aSet0(sdtpldt0(slcrc0, xx)) | spl8_62), inference(resolution, [], [f2346, f108])).
fof(f2346, plain, (~ sP3(sdtpldt0(slcrc0, xx), xx) | spl8_62), inference(resolution, [], [f2272, f139])).
fof(f2272, plain, (~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | spl8_62), inference(avatar_component_clause, [], [f2270])).
fof(f2328, plain, (~ spl8_62 | spl8_42), inference(avatar_split_clause, [], [f2327, f2185, f2270])).
fof(f2185, plain, (spl8_42 <=> sP1(sdtmndt0(sdtpldt0(slcrc0, xx), xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl8_42])])).
fof(f2327, plain, (~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | spl8_42), inference(subsumption_resolution, [], [f2326, f110])).
fof(f2326, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | spl8_42), inference(resolution, [], [f2187, f96])).
fof(f2187, plain, (~ sP1(sdtmndt0(sdtpldt0(slcrc0, xx), xx), xx) | spl8_42), inference(avatar_component_clause, [], [f2185])).
fof(f2289, plain, (~ spl8_42 | ~ spl8_62 | spl8_66 | ~ spl8_9 | spl8_10 | ~ spl8_33), inference(avatar_split_clause, [], [f2182, f1376, f1162, f1158, f2287, f2270, f2185])).
fof(f1158, plain, (spl8_9 <=> (xx = sK4(sdtpldt0(slcrc0, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl8_9])])).
fof(f1162, plain, (spl8_10 <=> (slcrc0 = sdtpldt0(slcrc0, xx))), introduced(avatar_definition, [new_symbols(naming, [spl8_10])])).
fof(f2182, plain, (! [X34] : (aElementOf0(X34, sdtpldt0(slcrc0, xx)) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ aElementOf0(X34, sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | ~ sP1(sdtmndt0(sdtpldt0(slcrc0, xx), xx), xx)) | (~ spl8_9 | spl8_10 | ~ spl8_33)), inference(superposition, [], [f761, f1459])).
fof(f1459, plain, ((sdtpldt0(slcrc0, xx) = sdtpldt0(sdtmndt0(sdtpldt0(slcrc0, xx), xx), xx)) | (~ spl8_9 | spl8_10 | ~ spl8_33)), inference(forward_demodulation, [], [f1458, f1160])).
fof(f1160, plain, ((xx = sK4(sdtpldt0(slcrc0, xx))) | ~ spl8_9), inference(avatar_component_clause, [], [f1158])).
fof(f1458, plain, ((sdtpldt0(slcrc0, xx) = sdtpldt0(sdtmndt0(sdtpldt0(slcrc0, xx), sK4(sdtpldt0(slcrc0, xx))), sK4(sdtpldt0(slcrc0, xx)))) | (spl8_10 | ~ spl8_33)), inference(subsumption_resolution, [], [f1454, f1163])).
fof(f1163, plain, (~ (slcrc0 = sdtpldt0(slcrc0, xx)) | spl8_10), inference(avatar_component_clause, [], [f1162])).
fof(f1454, plain, ((slcrc0 = sdtpldt0(slcrc0, xx)) | (sdtpldt0(slcrc0, xx) = sdtpldt0(sdtmndt0(sdtpldt0(slcrc0, xx), sK4(sdtpldt0(slcrc0, xx))), sK4(sdtpldt0(slcrc0, xx)))) | ~ spl8_33), inference(resolution, [], [f1377, f331])).
fof(f331, plain, ! [X0, X1] : (~ sP1(X0, X1) | (slcrc0 = sdtpldt0(X0, X1)) | (sdtpldt0(X0, X1) = sdtpldt0(sdtmndt0(sdtpldt0(X0, X1), sK4(sdtpldt0(X0, X1))), sK4(sdtpldt0(X0, X1))))), inference(resolution, [], [f183, f135])).
fof(f183, plain, ! [X0] : (~ aSet0(X0) | (sdtpldt0(sdtmndt0(X0, sK4(X0)), sK4(X0)) = X0) | (slcrc0 = X0)), inference(duplicate_literal_removal, [], [f179])).
fof(f179, plain, ! [X0] : ((sdtpldt0(sdtmndt0(X0, sK4(X0)), sK4(X0)) = X0) | ~ aSet0(X0) | (slcrc0 = X0) | ~ aSet0(X0)), inference(resolution, [], [f109, f75])).
fof(f761, plain, ! [X6, X7, X5] : (aElementOf0(X7, sdtpldt0(X5, X6)) | ~ aSet0(X5) | ~ aElementOf0(X7, X5) | ~ sP1(X5, X6)), inference(subsumption_resolution, [], [f755, f135])).
fof(f755, plain, ! [X6, X7, X5] : (~ sP1(X5, X6) | ~ aSet0(X5) | ~ aElementOf0(X7, X5) | aElementOf0(X7, sdtpldt0(X5, X6)) | ~ aSet0(sdtpldt0(X5, X6))), inference(resolution, [], [f650, f78])).
fof(f78, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f59])).
fof(f2277, plain, (~ spl8_62 | ~ spl8_42 | spl8_63 | ~ spl8_9 | spl8_10 | ~ spl8_33), inference(avatar_split_clause, [], [f2179, f1376, f1162, f1158, f2274, f2185, f2270])).
fof(f2179, plain, (aSubsetOf0(sdtmndt0(sdtpldt0(slcrc0, xx), xx), sdtpldt0(slcrc0, xx)) | ~ sP1(sdtmndt0(sdtpldt0(slcrc0, xx), xx), xx) | ~ aSet0(sdtmndt0(sdtpldt0(slcrc0, xx), xx)) | (~ spl8_9 | spl8_10 | ~ spl8_33)), inference(superposition, [], [f650, f1459])).
fof(f1807, plain, (spl8_1 | spl8_16), inference(avatar_contradiction_clause, [], [f1806])).
fof(f1806, plain, ($false | (spl8_1 | spl8_16)), inference(subsumption_resolution, [], [f1805, f294])).
fof(f294, plain, (~ (slcrc0 = xS) | spl8_1), inference(avatar_component_clause, [], [f293])).
fof(f1805, plain, ((slcrc0 = xS) | spl8_16), inference(subsumption_resolution, [], [f1804, f111])).
fof(f1804, plain, (~ aSet0(xS) | (slcrc0 = xS) | spl8_16), inference(resolution, [], [f1273, f131])).
fof(f131, plain, ! [X0] : (aElement0(sK4(X0)) | ~ aSet0(X0) | (slcrc0 = X0)), inference(duplicate_literal_removal, [], [f130])).
fof(f130, plain, ! [X0] : ((slcrc0 = X0) | ~ aSet0(X0) | aElement0(sK4(X0)) | ~ aSet0(X0)), inference(resolution, [], [f75, f72])).
fof(f1273, plain, (~ aElement0(sK4(xS)) | spl8_16), inference(avatar_component_clause, [], [f1271])).
fof(f1785, plain, (~ spl8_26 | ~ spl8_16 | spl8_13), inference(avatar_split_clause, [], [f1784, f1258, f1271, f1312])).
fof(f1784, plain, (~ aElement0(sK4(xS)) | ~ aSet0(sdtmndt0(xS, sK4(xS))) | spl8_13), inference(resolution, [], [f1260, f96])).
fof(f1260, plain, (~ sP1(sdtmndt0(xS, sK4(xS)), sK4(xS)) | spl8_13), inference(avatar_component_clause, [], [f1258])).
fof(f1438, plain, spl8_33, inference(avatar_contradiction_clause, [], [f1437])).
fof(f1437, plain, ($false | spl8_33), inference(subsumption_resolution, [], [f1436, f115])).
fof(f115, plain, aSet0(slcrc0), inference(equality_resolution, [], [f73])).
fof(f73, plain, ! [X0] : (aSet0(X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f54])).
fof(f1436, plain, (~ aSet0(slcrc0) | spl8_33), inference(subsumption_resolution, [], [f1435, f110])).
fof(f1435, plain, (~ aElement0(xx) | ~ aSet0(slcrc0) | spl8_33), inference(resolution, [], [f1378, f96])).
fof(f1378, plain, (~ sP1(slcrc0, xx) | spl8_33), inference(avatar_component_clause, [], [f1376])).
fof(f1391, plain, (~ spl8_33 | ~ spl8_10), inference(avatar_split_clause, [], [f1390, f1162, f1376])).
fof(f1390, plain, (~ sP1(slcrc0, xx) | ~ spl8_10), inference(subsumption_resolution, [], [f1389, f110])).
fof(f1389, plain, (~ aElement0(xx) | ~ sP1(slcrc0, xx) | ~ spl8_10), inference(subsumption_resolution, [], [f1360, f114])).
fof(f1360, plain, (aElementOf0(xx, slcrc0) | ~ aElement0(xx) | ~ sP1(slcrc0, xx) | ~ spl8_10), inference(superposition, [], [f136, f1164])).
fof(f1164, plain, ((slcrc0 = sdtpldt0(slcrc0, xx)) | ~ spl8_10), inference(avatar_component_clause, [], [f1162])).
fof(f1214, plain, spl8_11, inference(avatar_contradiction_clause, [], [f1213])).
fof(f1213, plain, ($false | spl8_11), inference(subsumption_resolution, [], [f1212, f115])).
fof(f1212, plain, (~ aSet0(slcrc0) | spl8_11), inference(subsumption_resolution, [], [f1211, f110])).
fof(f1211, plain, (~ aElement0(xx) | ~ aSet0(slcrc0) | spl8_11), inference(resolution, [], [f1203, f96])).
fof(f1203, plain, (~ sP1(slcrc0, xx) | spl8_11), inference(resolution, [], [f1181, f135])).
fof(f1181, plain, (~ aSet0(sdtpldt0(slcrc0, xx)) | spl8_11), inference(avatar_component_clause, [], [f1179])).
fof(f1186, plain, (~ spl8_11 | spl8_10 | spl8_12 | ~ spl8_9), inference(avatar_split_clause, [], [f1177, f1158, f1183, f1162, f1179])).
fof(f1177, plain, (aElementOf0(xx, sdtpldt0(slcrc0, xx)) | (slcrc0 = sdtpldt0(slcrc0, xx)) | ~ aSet0(sdtpldt0(slcrc0, xx)) | ~ spl8_9), inference(superposition, [], [f75, f1160])).
fof(f1165, plain, (spl8_9 | spl8_10), inference(avatar_split_clause, [], [f1146, f1162, f1158])).
fof(f1146, plain, ((slcrc0 = sdtpldt0(slcrc0, xx)) | (xx = sK4(sdtpldt0(slcrc0, xx)))), inference(resolution, [], [f1133, f110])).
fof(f1133, plain, ! [X0] : (~ aElement0(X0) | (slcrc0 = sdtpldt0(slcrc0, X0)) | (sK4(sdtpldt0(slcrc0, X0)) = X0)), inference(subsumption_resolution, [], [f1132, f115])).
fof(f1132, plain, ! [X0] : ((sK4(sdtpldt0(slcrc0, X0)) = X0) | (slcrc0 = sdtpldt0(slcrc0, X0)) | ~ aElement0(X0) | ~ aSet0(slcrc0)), inference(resolution, [], [f835, f96])).
fof(f835, plain, ! [X6] : (~ sP1(slcrc0, X6) | (sK4(sdtpldt0(slcrc0, X6)) = X6) | (slcrc0 = sdtpldt0(slcrc0, X6))), inference(resolution, [], [f361, f114])).
fof(f361, plain, ! [X6, X5] : (aElementOf0(sK4(sdtpldt0(X5, X6)), X5) | (sK4(sdtpldt0(X5, X6)) = X6) | ~ sP1(X5, X6) | (slcrc0 = sdtpldt0(X5, X6))), inference(subsumption_resolution, [], [f350, f135])).
fof(f350, plain, ! [X6, X5] : (aElementOf0(sK4(sdtpldt0(X5, X6)), X5) | (sK4(sdtpldt0(X5, X6)) = X6) | ~ sP1(X5, X6) | (slcrc0 = sdtpldt0(X5, X6)) | ~ aSet0(sdtpldt0(X5, X6))), inference(resolution, [], [f185, f75])).
fof(f338, plain, (spl8_1 | spl8_3), inference(avatar_split_clause, [], [f333, f335, f293])).
fof(f333, plain, ((xS = sdtpldt0(sdtmndt0(xS, sK4(xS)), sK4(xS))) | (slcrc0 = xS)), inference(resolution, [], [f183, f111])).