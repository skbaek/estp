fof(f5928, plain, $false, inference(avatar_sat_refutation, [], [f2008, f2030, f5670, f5674, f5927])).
fof(f5927, plain, (~ spl10_35 | spl10_51), inference(avatar_contradiction_clause, [], [f5926])).
fof(f5926, plain, ($false | (~ spl10_35 | spl10_51)), inference(subsumption_resolution, [], [f5925, f2002])).
fof(f2002, plain, (sP1(xI, xJ) | ~ spl10_35), inference(avatar_component_clause, [], [f2001])).
fof(f2001, plain, (spl10_35 <=> sP1(xI, xJ)), introduced(avatar_definition, [new_symbols(naming, [spl10_35])])).
fof(f5925, plain, (~ sP1(xI, xJ) | spl10_51), inference(subsumption_resolution, [], [f5924, f136])).
fof(f136, plain, aElementOf0(xx, sdtpldt1(xI, xJ)), inference(cnf_transformation, [], [f26])).
fof(f26, plain, (aElement0(xz) & aElementOf0(xy, sdtpldt1(xI, xJ)) & aElementOf0(xx, sdtpldt1(xI, xJ))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG086+1.p', m__901)).
fof(f5924, plain, (~ aElementOf0(xx, sdtpldt1(xI, xJ)) | ~ sP1(xI, xJ) | spl10_51), inference(resolution, [], [f5669, f349])).
fof(f349, plain, ! [X2, X0, X1] : (aElementOf0(sK8(X2, X1, X0), X2) | ~ aElementOf0(X0, sdtpldt1(X1, X2)) | ~ sP1(X1, X2)), inference(resolution, [], [f116, f140])).
fof(f140, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt1(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f112])).
fof(f112, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1] : (! [X2] : (((sdtpldt1(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e66])).
fof(e66, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f116, plain, ! [X2, X0, X8, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X8, X2) | aElementOf0(sK8(X0, X1, X8), X0)), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1)) | aElementOf0(sK4(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6, sK7, sK8])], [f74, f77, f76, f75])).
fof(f75, plain, ! [X2, X1, X0] : (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) => ((! [X5, X4] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(sK4(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f76, plain, ! [X2, X1, X0] : (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) => ((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1))), introduced(choice_axiom, [])).
fof(f77, plain, ! [X8, X1, X0] : (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) => ((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1))), introduced(choice_axiom, [])).
fof(f74, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f73])).
fof(f73, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f72])).
fof(f72, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f65])).
fof(f65, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), inference(usedef, [], [e65])).
fof(e65, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f5669, plain, (~ aElementOf0(sK8(xJ, xI, xx), xJ) | spl10_51), inference(avatar_component_clause, [], [f5667])).
fof(f5667, plain, (spl10_51 <=> aElementOf0(sK8(xJ, xI, xx), xJ)), introduced(avatar_definition, [new_symbols(naming, [spl10_51])])).
fof(f5674, plain, (~ spl10_35 | spl10_50), inference(avatar_contradiction_clause, [], [f5673])).
fof(f5673, plain, ($false | (~ spl10_35 | spl10_50)), inference(subsumption_resolution, [], [f5672, f2002])).
fof(f5672, plain, (~ sP1(xI, xJ) | spl10_50), inference(subsumption_resolution, [], [f5671, f136])).
fof(f5671, plain, (~ aElementOf0(xx, sdtpldt1(xI, xJ)) | ~ sP1(xI, xJ) | spl10_50), inference(resolution, [], [f5665, f338])).
fof(f338, plain, ! [X2, X0, X1] : (aElementOf0(sK7(X2, X1, X0), X1) | ~ aElementOf0(X0, sdtpldt1(X1, X2)) | ~ sP1(X1, X2)), inference(resolution, [], [f115, f140])).
fof(f115, plain, ! [X2, X0, X8, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X8, X2) | aElementOf0(sK7(X0, X1, X8), X1)), inference(cnf_transformation, [], [f78])).
fof(f5665, plain, (~ aElementOf0(sK7(xJ, xI, xx), xI) | spl10_50), inference(avatar_component_clause, [], [f5663])).
fof(f5663, plain, (spl10_50 <=> aElementOf0(sK7(xJ, xI, xx), xI)), introduced(avatar_definition, [new_symbols(naming, [spl10_50])])).
fof(f5670, plain, (~ spl10_50 | ~ spl10_51 | ~ spl10_36), inference(avatar_split_clause, [], [f5661, f2005, f5667, f5663])).
fof(f2005, plain, (spl10_36 <=> (xx = sdtpldt0(sK7(xJ, xI, xx), sK8(xJ, xI, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl10_36])])).
fof(f5661, plain, (~ aElementOf0(sK8(xJ, xI, xx), xJ) | ~ aElementOf0(sK7(xJ, xI, xx), xI) | ~ spl10_36), inference(trivial_inequality_removal, [], [f5659])).
fof(f5659, plain, (~ (xx = xx) | ~ aElementOf0(sK8(xJ, xI, xx), xJ) | ~ aElementOf0(sK7(xJ, xI, xx), xI) | ~ spl10_36), inference(superposition, [], [f139, f2007])).
fof(f2007, plain, ((xx = sdtpldt0(sK7(xJ, xI, xx), sK8(xJ, xI, xx))) | ~ spl10_36), inference(avatar_component_clause, [], [f2005])).
fof(f139, plain, ! [X0, X1] : (~ (sdtpldt0(X0, X1) = xx) | ~ aElementOf0(X1, xJ) | ~ aElementOf0(X0, xI)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0, X1] : (~ (sdtpldt0(X0, X1) = xx) | ~ aElementOf0(X1, xJ) | ~ aElementOf0(X0, xI)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ? [X0, X1] : ((sdtpldt0(X0, X1) = xx) & aElementOf0(X1, xJ) & aElementOf0(X0, xI)), inference(negated_conjecture, [], [f27])).
fof(f27, plain, ~ ? [X0, X1] : ((sdtpldt0(X0, X1) = xx) & aElementOf0(X1, xJ) & aElementOf0(X0, xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG086+1.p', m__)).
fof(f2030, plain, spl10_35, inference(avatar_contradiction_clause, [], [f2029])).
fof(f2029, plain, ($false | spl10_35), inference(subsumption_resolution, [], [f2028, f146])).
fof(f146, plain, aSet0(xI), inference(resolution, [], [f131, f134])).
fof(f134, plain, aIdeal0(xI), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (aIdeal0(xJ) & aIdeal0(xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG086+1.p', m__870)).
fof(f131, plain, ! [X0] : (~ aIdeal0(X0) | aSet0(X0)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0)), inference(ennf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (aIdeal0(X0) => (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(unused_predicate_definition_removal, [], [f32])).
fof(f32, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X2] : (aElementOf0(X2, X0) => aElementOf0(sdtpldt0(X1, X2), X0)))) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG086+1.p', mDefIdeal)).
fof(f2028, plain, (~ aSet0(xI) | spl10_35), inference(subsumption_resolution, [], [f2027, f147])).
fof(f147, plain, aSet0(xJ), inference(resolution, [], [f131, f135])).
fof(f135, plain, aIdeal0(xJ), inference(cnf_transformation, [], [f25])).
fof(f2027, plain, (~ aSet0(xJ) | ~ aSet0(xI) | spl10_35), inference(resolution, [], [f2003, f123])).
fof(f123, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f60, e66, e65])).
fof(f60, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f59])).
fof(f59, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG086+1.p', mDefSSum)).
fof(f2003, plain, (~ sP1(xI, xJ) | spl10_35), inference(avatar_component_clause, [], [f2001])).
fof(f2008, plain, (~ spl10_35 | spl10_36), inference(avatar_split_clause, [], [f1975, f2005, f2001])).
fof(f1975, plain, ((xx = sdtpldt0(sK7(xJ, xI, xx), sK8(xJ, xI, xx))) | ~ sP1(xI, xJ)), inference(resolution, [], [f478, f136])).
fof(f478, plain, ! [X2, X0, X1] : (~ aElementOf0(X0, sdtpldt1(X1, X2)) | (sdtpldt0(sK7(X2, X1, X0), sK8(X2, X1, X0)) = X0) | ~ sP1(X1, X2)), inference(resolution, [], [f117, f140])).
fof(f117, plain, ! [X2, X0, X8, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X8, X2) | (sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8)), inference(cnf_transformation, [], [f78])).