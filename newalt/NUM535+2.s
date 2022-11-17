fof(f547, plain, $false, inference(avatar_sat_refutation, [], [f195, f200, f205, f215, f220, f241, f262, f267, f271, f284, f289, f290, f292, f293, f302, f447, f506, f510, f526, f533, f545])).
fof(f545, plain, (~ spl15_18 | spl15_21 | ~ spl15_28), inference(avatar_contradiction_clause, [], [f544])).
fof(f544, plain, ($false | (~ spl15_18 | spl15_21 | ~ spl15_28)), inference(subsumption_resolution, [], [f539, f283])).
fof(f283, plain, (~ aElementOf0(sK14, xS) | spl15_21), inference(avatar_component_clause, [], [f281])).
fof(f281, plain, (spl15_21 <=> aElementOf0(sK14, xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f539, plain, (aElementOf0(sK14, xS) | (~ spl15_18 | ~ spl15_28)), inference(resolution, [], [f442, f261])).
fof(f261, plain, (! [X0] : (~ aElementOf0(X0, sdtmndt0(xS, xx)) | aElementOf0(X0, xS)) | ~ spl15_18), inference(avatar_component_clause, [], [f260])).
fof(f260, plain, (spl15_18 <=> ! [X0] : (aElementOf0(X0, xS) | ~ aElementOf0(X0, sdtmndt0(xS, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f442, plain, (aElementOf0(sK14, sdtmndt0(xS, xx)) | ~ spl15_28), inference(avatar_component_clause, [], [f440])).
fof(f440, plain, (spl15_28 <=> aElementOf0(sK14, sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_28])])).
fof(f533, plain, (spl15_21 | ~ spl15_29), inference(avatar_contradiction_clause, [], [f532])).
fof(f532, plain, ($false | (spl15_21 | ~ spl15_29)), inference(subsumption_resolution, [], [f531, f134])).
fof(f134, plain, aElementOf0(xx, xS), inference(cnf_transformation, [], [f18])).
fof(f18, plain, aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+2.p', m__617_02)).
fof(f531, plain, (~ aElementOf0(xx, xS) | (spl15_21 | ~ spl15_29)), inference(backward_demodulation, [], [f283, f446])).
fof(f446, plain, ((xx = sK14) | ~ spl15_29), inference(avatar_component_clause, [], [f444])).
fof(f444, plain, (spl15_29 <=> (xx = sK14)), introduced(avatar_definition, [new_symbols(naming, [spl15_29])])).
fof(f526, plain, (spl15_3 | ~ spl15_4 | ~ spl15_11 | ~ spl15_16 | spl15_35), inference(avatar_contradiction_clause, [], [f525])).
fof(f525, plain, ($false | (spl15_3 | ~ spl15_4 | ~ spl15_11 | ~ spl15_16 | spl15_35)), inference(subsumption_resolution, [], [f523, f232])).
fof(f232, plain, (aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ spl15_11), inference(avatar_component_clause, [], [f230])).
fof(f230, plain, (spl15_11 <=> aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f523, plain, (~ aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | (spl15_3 | ~ spl15_4 | ~ spl15_16 | spl15_35)), inference(backward_demodulation, [], [f194, f520])).
fof(f520, plain, ((xx = sK13) | (~ spl15_4 | ~ spl15_16 | spl15_35)), inference(subsumption_resolution, [], [f519, f199])).
fof(f199, plain, (aElementOf0(sK13, xS) | ~ spl15_4), inference(avatar_component_clause, [], [f197])).
fof(f197, plain, (spl15_4 <=> aElementOf0(sK13, xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f519, plain, (~ aElementOf0(sK13, xS) | (xx = sK13) | (~ spl15_4 | ~ spl15_16 | spl15_35)), inference(subsumption_resolution, [], [f518, f465])).
fof(f465, plain, (aElement0(sK13) | ~ spl15_4), inference(subsumption_resolution, [], [f464, f133])).
fof(f133, plain, aSet0(xS), inference(cnf_transformation, [], [f17])).
fof(f17, plain, aSet0(xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+2.p', m__617)).
fof(f464, plain, (aElement0(sK13) | ~ aSet0(xS) | ~ spl15_4), inference(resolution, [], [f199, f96])).
fof(f96, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+2.p', mEOfElem)).
fof(f518, plain, (~ aElement0(sK13) | ~ aElementOf0(sK13, xS) | (xx = sK13) | (~ spl15_16 | spl15_35)), inference(resolution, [], [f505, f252])).
fof(f252, plain, (! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) | ~ aElement0(X0) | ~ aElementOf0(X0, xS) | (xx = X0)) | ~ spl15_16), inference(avatar_component_clause, [], [f251])).
fof(f251, plain, (spl15_16 <=> ! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) | ~ aElement0(X0) | ~ aElementOf0(X0, xS) | (xx = X0))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f505, plain, (~ aElementOf0(sK13, sdtmndt0(xS, xx)) | spl15_35), inference(avatar_component_clause, [], [f503])).
fof(f503, plain, (spl15_35 <=> aElementOf0(sK13, sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_35])])).
fof(f194, plain, (~ aElementOf0(sK13, sdtpldt0(sdtmndt0(xS, xx), xx)) | spl15_3), inference(avatar_component_clause, [], [f192])).
fof(f192, plain, (spl15_3 <=> aElementOf0(sK13, sdtpldt0(sdtmndt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f510, plain, (~ spl15_8 | ~ spl15_10 | spl15_34), inference(avatar_contradiction_clause, [], [f509])).
fof(f509, plain, ($false | (~ spl15_8 | ~ spl15_10 | spl15_34)), inference(subsumption_resolution, [], [f508, f219])).
fof(f219, plain, (aSet0(sdtmndt0(xS, xx)) | ~ spl15_8), inference(avatar_component_clause, [], [f217])).
fof(f217, plain, (spl15_8 <=> aSet0(sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f508, plain, (~ aSet0(sdtmndt0(xS, xx)) | (~ spl15_10 | spl15_34)), inference(subsumption_resolution, [], [f507, f227])).
fof(f227, plain, (aElement0(xx) | ~ spl15_10), inference(avatar_component_clause, [], [f226])).
fof(f226, plain, (spl15_10 <=> aElement0(xx)), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f507, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(xS, xx)) | spl15_34), inference(resolution, [], [f497, f120])).
fof(f120, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f39, e45, e44])).
fof(f44, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e44])).
fof(e44, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f45, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e45])).
fof(e45, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f39, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f38])).
fof(f38, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+2.p', mDefCons)).
fof(f497, plain, (~ sP1(sdtmndt0(xS, xx), xx) | spl15_34), inference(avatar_component_clause, [], [f495])).
fof(f495, plain, (spl15_34 <=> sP1(sdtmndt0(xS, xx), xx)), introduced(avatar_definition, [new_symbols(naming, [spl15_34])])).
fof(f506, plain, (~ spl15_34 | ~ spl15_35 | spl15_3 | ~ spl15_4), inference(avatar_split_clause, [], [f501, f197, f192, f503, f495])).
fof(f501, plain, (~ aElementOf0(sK13, sdtmndt0(xS, xx)) | ~ sP1(sdtmndt0(xS, xx), xx) | (spl15_3 | ~ spl15_4)), inference(subsumption_resolution, [], [f500, f465])).
fof(f500, plain, (~ aElement0(sK13) | ~ aElementOf0(sK13, sdtmndt0(xS, xx)) | ~ sP1(sdtmndt0(xS, xx), xx) | spl15_3), inference(resolution, [], [f194, f377])).
fof(f377, plain, ! [X2, X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X2)) | ~ aElement0(X0) | ~ aElementOf0(X0, X1) | ~ sP1(X1, X2)), inference(resolution, [], [f114, f167])).
fof(f167, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f109])).
fof(f109, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1] : (! [X2] : (((sdtpldt0(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f45])).
fof(f114, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (((~ (sK11(X0, X1, X2) = X0) & ~ aElementOf0(sK11(X0, X1, X2), X1)) | ~ aElement0(sK11(X0, X1, X2)) | ~ aElementOf0(sK11(X0, X1, X2), X2)) & ((((sK11(X0, X1, X2) = X0) | aElementOf0(sK11(X0, X1, X2), X1)) & aElement0(sK11(X0, X1, X2))) | aElementOf0(sK11(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f69, f70])).
fof(f70, plain, ! [X2, X1, X0] : (? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) => (((~ (sK11(X0, X1, X2) = X0) & ~ aElementOf0(sK11(X0, X1, X2), X1)) | ~ aElement0(sK11(X0, X1, X2)) | ~ aElementOf0(sK11(X0, X1, X2), X2)) & ((((sK11(X0, X1, X2) = X0) | aElementOf0(sK11(X0, X1, X2), X1)) & aElement0(sK11(X0, X1, X2))) | aElementOf0(sK11(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f69, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f68])).
fof(f68, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : (((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f67])).
fof(f67, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3))) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f44])).
fof(f447, plain, (spl15_28 | spl15_29 | ~ spl15_13 | ~ spl15_22), inference(avatar_split_clause, [], [f423, f286, f239, f444, f440])).
fof(f239, plain, (spl15_13 <=> ! [X0] : ((xx = X0) | ~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) | aElementOf0(X0, sdtmndt0(xS, xx)))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f286, plain, (spl15_22 <=> aElementOf0(sK14, sdtpldt0(sdtmndt0(xS, xx), xx))), introduced(avatar_definition, [new_symbols(naming, [spl15_22])])).
fof(f423, plain, ((xx = sK14) | aElementOf0(sK14, sdtmndt0(xS, xx)) | (~ spl15_13 | ~ spl15_22)), inference(resolution, [], [f240, f288])).
fof(f288, plain, (aElementOf0(sK14, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ spl15_22), inference(avatar_component_clause, [], [f286])).
fof(f240, plain, (! [X0] : (~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) | (xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) | ~ spl15_13), inference(avatar_component_clause, [], [f239])).
fof(f302, plain, spl15_10, inference(avatar_split_clause, [], [f301, f226])).
fof(f301, plain, aElement0(xx), inference(subsumption_resolution, [], [f295, f133])).
fof(f295, plain, (aElement0(xx) | ~ aSet0(xS)), inference(resolution, [], [f96, f134])).
fof(f293, plain, (spl15_1 | spl15_8), inference(avatar_split_clause, [], [f158, f217, f183])).
fof(f183, plain, (spl15_1 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f158, plain, (aSet0(sdtmndt0(xS, xx)) | sP8), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & (~ aElementOf0(sK14, xS) & aElementOf0(sK14, sdtpldt0(sdtmndt0(xS, xx), xx))) & sP7 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP6 & aSet0(sdtmndt0(xS, xx))) | sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f93, f94])).
fof(f94, plain, (? [X0] : (~ aElementOf0(X0, xS) & aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx))) => (~ aElementOf0(sK14, xS) & aElementOf0(sK14, sdtpldt0(sdtmndt0(xS, xx), xx)))), introduced(choice_axiom, [])).
fof(f93, plain, ((~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & ? [X0] : (~ aElementOf0(X0, xS) & aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx))) & sP7 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP6 & aSet0(sdtmndt0(xS, xx))) | sP8), inference(rectify, [], [f55])).
fof(f55, plain, ((~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & ? [X2] : (~ aElementOf0(X2, xS) & aElementOf0(X2, sdtpldt0(sdtmndt0(xS, xx), xx))) & sP7 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP6 & aSet0(sdtmndt0(xS, xx))) | sP8), inference(definition_folding, [], [f43, e54, e53, e52, e51, e50])).
fof(f50, plain, (! [X3] : (aElementOf0(X3, sdtmndt0(xS, xx)) <=> (~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3))) | ~ sP4), inference(usedef, [], [e50])).
fof(e50, plain, (sP4 <=> ! [X3] : (aElementOf0(X3, sdtmndt0(xS, xx)) <=> (~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f51, plain, (! [X4] : (aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4))) | ~ sP5), inference(usedef, [], [e51])).
fof(e51, plain, (sP5 <=> ! [X4] : (aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f52, plain, (! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) | ~ sP6), inference(usedef, [], [e52])).
fof(e52, plain, (sP6 <=> ! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f53, plain, (! [X1] : (aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1))) | ~ sP7), inference(usedef, [], [e53])).
fof(e53, plain, (sP7 <=> ! [X1] : (aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f54, plain, ((~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X5, xS)) & sP5 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP4 & aSet0(sdtmndt0(xS, xx))) | ~ sP8), inference(usedef, [], [e54])).
fof(e54, plain, (sP8 <=> (~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X5, xS)) & sP5 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP4 & aSet0(sdtmndt0(xS, xx)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f43, plain, ((~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & ? [X2] : (~ aElementOf0(X2, xS) & aElementOf0(X2, sdtpldt0(sdtmndt0(xS, xx), xx))) & ! [X1] : (aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & ! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx))) | (~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X5, xS)) & ! [X4] : (aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & ! [X3] : (aElementOf0(X3, sdtmndt0(xS, xx)) <=> (~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3))) & aSet0(sdtmndt0(xS, xx)))), inference(flattening, [], [f42])).
fof(f42, plain, ((((~ aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) & ? [X2] : (~ aElementOf0(X2, xS) & aElementOf0(X2, sdtpldt0(sdtmndt0(xS, xx), xx)))) & (! [X1] : (aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)))) & (! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx)))) | (((~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X5, xS))) & (! [X4] : (aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)))) & (! [X3] : (aElementOf0(X3, sdtmndt0(xS, xx)) <=> (~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3))) & aSet0(sdtmndt0(xS, xx))))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ (((! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx))) => ((! [X1] : (aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))) => (aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ! [X2] : (aElementOf0(X2, sdtpldt0(sdtmndt0(xS, xx), xx)) => aElementOf0(X2, xS))))) & ((! [X3] : (aElementOf0(X3, sdtmndt0(xS, xx)) <=> (~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3))) & aSet0(sdtmndt0(xS, xx))) => ((! [X4] : (aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))) => (aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) | ! [X5] : (aElementOf0(X5, xS) => aElementOf0(X5, sdtpldt0(sdtmndt0(xS, xx), xx))))))), inference(rectify, [], [f20])).
fof(f20, plain, ~ (((! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) & aElement0(X0))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))) => (aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) => aElementOf0(X0, xS))))) & ((! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) & aElement0(X0))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))) => (aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) | ! [X0] : (aElementOf0(X0, xS) => aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx))))))), inference(negated_conjecture, [], [f19])).
fof(f19, plain, ~ (((! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) & aElement0(X0))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))) => (aSubsetOf0(sdtpldt0(sdtmndt0(xS, xx), xx), xS) | ! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) => aElementOf0(X0, xS))))) & ((! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) <=> (~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0))) & aSet0(sdtmndt0(xS, xx))) => ((! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) <=> (((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) & aElement0(X0))) & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx))) => (aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) | ! [X0] : (aElementOf0(X0, xS) => aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx))))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM535+2.p', m__)).
fof(f292, plain, (spl15_1 | spl15_15), inference(avatar_split_clause, [], [f159, f247, f183])).
fof(f247, plain, (spl15_15 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f159, plain, (sP6 | sP8), inference(cnf_transformation, [], [f95])).
fof(f290, plain, (spl15_1 | spl15_9), inference(avatar_split_clause, [], [f161, f222, f183])).
fof(f222, plain, (spl15_9 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f161, plain, (sP7 | sP8), inference(cnf_transformation, [], [f95])).
fof(f289, plain, (spl15_1 | spl15_22), inference(avatar_split_clause, [], [f162, f286, f183])).
fof(f162, plain, (aElementOf0(sK14, sdtpldt0(sdtmndt0(xS, xx), xx)) | sP8), inference(cnf_transformation, [], [f95])).
fof(f284, plain, (spl15_1 | ~ spl15_21), inference(avatar_split_clause, [], [f163, f281, f183])).
fof(f163, plain, (~ aElementOf0(sK14, xS) | sP8), inference(cnf_transformation, [], [f95])).
fof(f271, plain, (~ spl15_7 | spl15_16), inference(avatar_split_clause, [], [f157, f251, f212])).
fof(f212, plain, (spl15_7 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f157, plain, ! [X0] : (aElementOf0(X0, sdtmndt0(xS, xx)) | (xx = X0) | ~ aElementOf0(X0, xS) | ~ aElement0(X0) | ~ sP4), inference(cnf_transformation, [], [f92])).
fof(f92, plain, (! [X0] : ((aElementOf0(X0, sdtmndt0(xS, xx)) | (xx = X0) | ~ aElementOf0(X0, xS) | ~ aElement0(X0)) & ((~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0)) | ~ aElementOf0(X0, sdtmndt0(xS, xx)))) | ~ sP4), inference(rectify, [], [f91])).
fof(f91, plain, (! [X3] : ((aElementOf0(X3, sdtmndt0(xS, xx)) | (xx = X3) | ~ aElementOf0(X3, xS) | ~ aElement0(X3)) & ((~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3)) | ~ aElementOf0(X3, sdtmndt0(xS, xx)))) | ~ sP4), inference(flattening, [], [f90])).
fof(f90, plain, (! [X3] : ((aElementOf0(X3, sdtmndt0(xS, xx)) | ((xx = X3) | ~ aElementOf0(X3, xS) | ~ aElement0(X3))) & ((~ (xx = X3) & aElementOf0(X3, xS) & aElement0(X3)) | ~ aElementOf0(X3, sdtmndt0(xS, xx)))) | ~ sP4), inference(nnf_transformation, [], [f50])).
fof(f267, plain, (~ spl15_5 | ~ spl15_10 | spl15_11), inference(avatar_split_clause, [], [f173, f230, f226, f202])).
fof(f202, plain, (spl15_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f173, plain, (aElementOf0(xx, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aElement0(xx) | ~ sP5), inference(equality_resolution, [], [f153])).
fof(f153, plain, ! [X0] : (aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ (xx = X0) | ~ aElement0(X0) | ~ sP5), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (! [X0] : ((aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) | (~ (xx = X0) & ~ aElementOf0(X0, sdtmndt0(xS, xx))) | ~ aElement0(X0)) & ((((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)))) | ~ sP5), inference(rectify, [], [f88])).
fof(f88, plain, (! [X4] : ((aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) | (~ (xx = X4) & ~ aElementOf0(X4, sdtmndt0(xS, xx))) | ~ aElement0(X4)) & ((((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4)) | ~ aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)))) | ~ sP5), inference(flattening, [], [f87])).
fof(f87, plain, (! [X4] : ((aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)) | ((~ (xx = X4) & ~ aElementOf0(X4, sdtmndt0(xS, xx))) | ~ aElement0(X4))) & ((((xx = X4) | aElementOf0(X4, sdtmndt0(xS, xx))) & aElement0(X4)) | ~ aElementOf0(X4, sdtpldt0(sdtmndt0(xS, xx), xx)))) | ~ sP5), inference(nnf_transformation, [], [f51])).
fof(f262, plain, (~ spl15_15 | spl15_18), inference(avatar_split_clause, [], [f147, f260, f247])).
fof(f147, plain, ! [X0] : (aElementOf0(X0, xS) | ~ aElementOf0(X0, sdtmndt0(xS, xx)) | ~ sP6), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (! [X0] : ((aElementOf0(X0, sdtmndt0(xS, xx)) | (xx = X0) | ~ aElementOf0(X0, xS) | ~ aElement0(X0)) & ((~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0)) | ~ aElementOf0(X0, sdtmndt0(xS, xx)))) | ~ sP6), inference(flattening, [], [f85])).
fof(f85, plain, (! [X0] : ((aElementOf0(X0, sdtmndt0(xS, xx)) | ((xx = X0) | ~ aElementOf0(X0, xS) | ~ aElement0(X0))) & ((~ (xx = X0) & aElementOf0(X0, xS) & aElement0(X0)) | ~ aElementOf0(X0, sdtmndt0(xS, xx)))) | ~ sP6), inference(nnf_transformation, [], [f52])).
fof(f241, plain, (~ spl15_9 | spl15_13), inference(avatar_split_clause, [], [f143, f239, f222])).
fof(f143, plain, ! [X0] : ((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx)) | ~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ sP7), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (! [X0] : ((aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) | (~ (xx = X0) & ~ aElementOf0(X0, sdtmndt0(xS, xx))) | ~ aElement0(X0)) & ((((xx = X0) | aElementOf0(X0, sdtmndt0(xS, xx))) & aElement0(X0)) | ~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)))) | ~ sP7), inference(rectify, [], [f83])).
fof(f83, plain, (! [X1] : ((aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) | (~ (xx = X1) & ~ aElementOf0(X1, sdtmndt0(xS, xx))) | ~ aElement0(X1)) & ((((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1)) | ~ aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)))) | ~ sP7), inference(flattening, [], [f82])).
fof(f82, plain, (! [X1] : ((aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)) | ((~ (xx = X1) & ~ aElementOf0(X1, sdtmndt0(xS, xx))) | ~ aElement0(X1))) & ((((xx = X1) | aElementOf0(X1, sdtmndt0(xS, xx))) & aElement0(X1)) | ~ aElementOf0(X1, sdtpldt0(sdtmndt0(xS, xx), xx)))) | ~ sP7), inference(nnf_transformation, [], [f53])).
fof(f220, plain, (~ spl15_1 | spl15_8), inference(avatar_split_clause, [], [f135, f217, f183])).
fof(f135, plain, (aSet0(sdtmndt0(xS, xx)) | ~ sP8), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & (~ aElementOf0(sK13, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(sK13, xS)) & sP5 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP4 & aSet0(sdtmndt0(xS, xx))) | ~ sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f79, f80])).
fof(f80, plain, (? [X0] : (~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X0, xS)) => (~ aElementOf0(sK13, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(sK13, xS))), introduced(choice_axiom, [])).
fof(f79, plain, ((~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & ? [X0] : (~ aElementOf0(X0, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X0, xS)) & sP5 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP4 & aSet0(sdtmndt0(xS, xx))) | ~ sP8), inference(rectify, [], [f78])).
fof(f78, plain, ((~ aSubsetOf0(xS, sdtpldt0(sdtmndt0(xS, xx), xx)) & ? [X5] : (~ aElementOf0(X5, sdtpldt0(sdtmndt0(xS, xx), xx)) & aElementOf0(X5, xS)) & sP5 & aSet0(sdtpldt0(sdtmndt0(xS, xx), xx)) & sP4 & aSet0(sdtmndt0(xS, xx))) | ~ sP8), inference(nnf_transformation, [], [f54])).
fof(f215, plain, (~ spl15_1 | spl15_7), inference(avatar_split_clause, [], [f136, f212, f183])).
fof(f136, plain, (sP4 | ~ sP8), inference(cnf_transformation, [], [f81])).
fof(f205, plain, (~ spl15_1 | spl15_5), inference(avatar_split_clause, [], [f138, f202, f183])).
fof(f138, plain, (sP5 | ~ sP8), inference(cnf_transformation, [], [f81])).
fof(f200, plain, (~ spl15_1 | spl15_4), inference(avatar_split_clause, [], [f139, f197, f183])).
fof(f139, plain, (aElementOf0(sK13, xS) | ~ sP8), inference(cnf_transformation, [], [f81])).
fof(f195, plain, (~ spl15_1 | ~ spl15_3), inference(avatar_split_clause, [], [f140, f192, f183])).
fof(f140, plain, (~ aElementOf0(sK13, sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ sP8), inference(cnf_transformation, [], [f81])).