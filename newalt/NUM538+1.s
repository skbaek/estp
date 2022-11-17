fof(f26481, plain, $false, inference(avatar_sat_refutation, [], [f26398, f26416, f26480])).
fof(f26480, plain, spl9_802, inference(avatar_contradiction_clause, [], [f26479])).
fof(f26479, plain, ($false | spl9_802), inference(subsumption_resolution, [], [f26478, f210])).
fof(f210, plain, aSet0(xS), inference(cnf_transformation, [], [f44])).
fof(f44, plain, aSet0(xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', m__1522)).
fof(f26478, plain, (~ aSet0(xS) | spl9_802), inference(subsumption_resolution, [], [f26477, f239])).
fof(f239, plain, aElement0(xx), inference(subsumption_resolution, [], [f237, f210])).
fof(f237, plain, (aElement0(xx) | ~ aSet0(xS)), inference(resolution, [], [f141, f212])).
fof(f212, plain, aElementOf0(xx, xS), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (aElementOf0(xx, xS) & isFinite0(xS)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', m__1522_02)).
fof(f141, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', mEOfElem)).
fof(f26477, plain, (~ aElement0(xx) | ~ aSet0(xS) | spl9_802), inference(resolution, [], [f25139, f179])).
fof(f179, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f73, e112, e111])).
fof(f111, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e111])).
fof(e111, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f112, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e112])).
fof(e112, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f73, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f72])).
fof(f72, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', mDefDiff)).
fof(f25139, plain, (~ sP3(xS, xx) | spl9_802), inference(avatar_component_clause, [], [f25137])).
fof(f25137, plain, (spl9_802 <=> sP3(xS, xx)), introduced(avatar_definition, [new_symbols(naming, [spl9_802])])).
fof(f26416, plain, (~ spl9_802 | ~ spl9_30), inference(avatar_split_clause, [], [f26415, f1090, f25137])).
fof(f1090, plain, (spl9_30 <=> isFinite0(sdtmndt0(xS, xx))), introduced(avatar_definition, [new_symbols(naming, [spl9_30])])).
fof(f26415, plain, (~ sP3(xS, xx) | ~ spl9_30), inference(subsumption_resolution, [], [f26414, f213])).
fof(f213, plain, ~ (szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(xS)), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ~ (szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(xS)), inference(flattening, [], [f47])).
fof(f47, plain, ~ (szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(xS)), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ (szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(xS)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', m__)).
fof(f26414, plain, ((szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(xS)) | ~ sP3(xS, xx) | ~ spl9_30), inference(forward_demodulation, [], [f26413, f454])).
fof(f454, plain, (xS = sdtpldt0(sdtmndt0(xS, xx), xx)), inference(subsumption_resolution, [], [f444, f210])).
fof(f444, plain, ((xS = sdtpldt0(sdtmndt0(xS, xx), xx)) | ~ aSet0(xS)), inference(resolution, [], [f180, f212])).
fof(f180, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | (sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : (! [X1] : ((sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => (sdtpldt0(sdtmndt0(X0, X1), X1) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', mConsDiff)).
fof(f26413, plain, ((szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(sdtpldt0(sdtmndt0(xS, xx), xx))) | ~ sP3(xS, xx) | ~ spl9_30), inference(subsumption_resolution, [], [f26400, f239])).
fof(f26400, plain, (~ aElement0(xx) | (szszuzczcdt0(sbrdtbr0(sdtmndt0(xS, xx))) = sbrdtbr0(sdtpldt0(sdtmndt0(xS, xx), xx))) | ~ sP3(xS, xx) | ~ spl9_30), inference(resolution, [], [f1092, f601])).
fof(f601, plain, ! [X8, X9] : (~ isFinite0(sdtmndt0(X8, X9)) | ~ aElement0(X9) | (sbrdtbr0(sdtpldt0(sdtmndt0(X8, X9), X9)) = szszuzczcdt0(sbrdtbr0(sdtmndt0(X8, X9)))) | ~ sP3(X8, X9)), inference(subsumption_resolution, [], [f584, f348])).
fof(f348, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f219, f170])).
fof(f170, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f133, f134])).
fof(f134, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f133, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f132])).
fof(f132, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f131])).
fof(f131, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f111])).
fof(f219, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f168])).
fof(f168, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f130])).
fof(f130, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f112])).
fof(f584, plain, ! [X8, X9] : ((sbrdtbr0(sdtpldt0(sdtmndt0(X8, X9), X9)) = szszuzczcdt0(sbrdtbr0(sdtmndt0(X8, X9)))) | ~ aElement0(X9) | ~ isFinite0(sdtmndt0(X8, X9)) | ~ aSet0(sdtmndt0(X8, X9)) | ~ sP3(X8, X9)), inference(resolution, [], [f209, f347])).
fof(f347, plain, ! [X4, X3] : (~ aElementOf0(X4, sdtmndt0(X3, X4)) | ~ sP3(X3, X4)), inference(resolution, [], [f219, f220])).
fof(f220, plain, ! [X4, X2, X1] : (~ sP2(X4, X1, X2) | ~ aElementOf0(X4, X2)), inference(equality_resolution, [], [f173])).
fof(f173, plain, ! [X4, X2, X0, X1] : (~ (X0 = X4) | ~ aElementOf0(X4, X2) | ~ sP2(X0, X1, X2)), inference(cnf_transformation, [], [f135])).
fof(f209, plain, ! [X0, X1] : (aElementOf0(X1, X0) | (sbrdtbr0(sdtpldt0(X0, X1)) = szszuzczcdt0(sbrdtbr0(X0))) | ~ aElement0(X1) | ~ isFinite0(X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ! [X0] : (! [X1] : ((sbrdtbr0(sdtpldt0(X0, X1)) = szszuzczcdt0(sbrdtbr0(X0))) | aElementOf0(X1, X0) | ~ aElement0(X1)) | ~ isFinite0(X0) | ~ aSet0(X0)), inference(flattening, [], [f106])).
fof(f106, plain, ! [X0] : (! [X1] : (((sbrdtbr0(sdtpldt0(X0, X1)) = szszuzczcdt0(sbrdtbr0(X0))) | aElementOf0(X1, X0)) | ~ aElement0(X1)) | (~ isFinite0(X0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : ((isFinite0(X0) & aSet0(X0)) => ! [X1] : (aElement0(X1) => (~ aElementOf0(X1, X0) => (sbrdtbr0(sdtpldt0(X0, X1)) = szszuzczcdt0(sbrdtbr0(X0)))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', mCardCons)).
fof(f1092, plain, (isFinite0(sdtmndt0(xS, xx)) | ~ spl9_30), inference(avatar_component_clause, [], [f1090])).
fof(f26398, plain, spl9_30, inference(avatar_contradiction_clause, [], [f26397])).
fof(f26397, plain, ($false | spl9_30), inference(subsumption_resolution, [], [f26396, f239])).
fof(f26396, plain, (~ aElement0(xx) | spl9_30), inference(subsumption_resolution, [], [f26395, f210])).
fof(f26395, plain, (~ aSet0(xS) | ~ aElement0(xx) | spl9_30), inference(subsumption_resolution, [], [f26394, f211])).
fof(f211, plain, isFinite0(xS), inference(cnf_transformation, [], [f45])).
fof(f26394, plain, (~ isFinite0(xS) | ~ aSet0(xS) | ~ aElement0(xx) | spl9_30), inference(resolution, [], [f1091, f185])).
fof(f185, plain, ! [X0, X1] : (isFinite0(sdtmndt0(X1, X0)) | ~ isFinite0(X1) | ~ aSet0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (! [X1] : (isFinite0(sdtmndt0(X1, X0)) | ~ isFinite0(X1) | ~ aSet0(X1)) | ~ aElement0(X0)), inference(flattening, [], [f83])).
fof(f83, plain, ! [X0] : (! [X1] : (isFinite0(sdtmndt0(X1, X0)) | (~ isFinite0(X1) | ~ aSet0(X1))) | ~ aElement0(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (aElement0(X0) => ! [X1] : ((isFinite0(X1) & aSet0(X1)) => isFinite0(sdtmndt0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM538+1.p', mFDiffSet)).
fof(f1091, plain, (~ isFinite0(sdtmndt0(xS, xx)) | spl9_30), inference(avatar_component_clause, [], [f1090])).