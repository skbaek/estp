fof(f1327, plain, $false, inference(avatar_sat_refutation, [], [f1290, f1294, f1326])).
fof(f1326, plain, ~ spl29_42, inference(avatar_contradiction_clause, [], [f1325])).
fof(f1325, plain, ($false | ~ spl29_42), inference(subsumption_resolution, [], [f1323, f562])).
fof(f562, plain, aElementOf0(xx, xP), inference(cnf_transformation, [], [f113])).
fof(f113, plain, aElementOf0(xx, xP), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__5348)).
fof(f1323, plain, (~ aElementOf0(xx, xP) | ~ spl29_42), inference(resolution, [], [f1289, f584])).
fof(f584, plain, ! [X4, X2, X1] : (~ sP2(X4, X1, X2) | ~ aElementOf0(X4, X2)), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X4, X2, X0, X1] : (~ (X0 = X4) | ~ aElementOf0(X4, X2) | ~ sP2(X0, X1, X2)), inference(cnf_transformation, [], [f281])).
fof(f281, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f279, f280])).
fof(f280, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f279, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f278])).
fof(f278, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f277])).
fof(f277, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f257])).
fof(f257, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e257])).
fof(e257, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f1289, plain, (sP2(xx, xQ, xP) | ~ spl29_42), inference(avatar_component_clause, [], [f1287])).
fof(f1287, plain, (spl29_42 <=> sP2(xx, xQ, xP)), introduced(avatar_definition, [new_symbols(naming, [spl29_42])])).
fof(f1294, plain, spl29_41, inference(avatar_contradiction_clause, [], [f1293])).
fof(f1293, plain, ($false | spl29_41), inference(subsumption_resolution, [], [f1292, f834])).
fof(f834, plain, aSet0(xQ), inference(subsumption_resolution, [], [f816, f536])).
fof(f536, plain, aSet0(xO), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((xO = sdtlcdtrc0(xe, sdtlbdtrb0(xd, szDzizrdt0(xd)))) & aSet0(xO)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__4891)).
fof(f816, plain, (aSet0(xQ) | ~ aSet0(xO)), inference(resolution, [], [f352, f545])).
fof(f545, plain, aSubsetOf0(xQ, xO), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (~ (slcrc0 = xQ) & aSubsetOf0(xQ, xO)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__5093)).
fof(f352, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f269])).
fof(f269, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f267, f268])).
fof(f268, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f267, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f266])).
fof(f266, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f265])).
fof(f265, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f137])).
fof(f137, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', mDefSub)).
fof(f1292, plain, (~ aSet0(xQ) | spl29_41), inference(subsumption_resolution, [], [f1291, f781])).
fof(f781, plain, aElement0(xx), inference(subsumption_resolution, [], [f765, f390])).
fof(f390, plain, aSet0(szNzAzT0), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (isCountable0(szNzAzT0) & aSet0(szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', mNATSet)).
fof(f765, plain, (aElement0(xx) | ~ aSet0(szNzAzT0)), inference(resolution, [], [f345, f563])).
fof(f563, plain, aElementOf0(xx, szNzAzT0), inference(cnf_transformation, [], [f114])).
fof(f114, plain, (aElementOf0(xx, xO) & aElementOf0(xx, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__5365)).
fof(f345, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', mEOfElem)).
fof(f1291, plain, (~ aElement0(xx) | ~ aSet0(xQ) | spl29_41), inference(resolution, [], [f1285, f383])).
fof(f383, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f148, e258, e257])).
fof(f258, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e258])).
fof(e258, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f148, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f147])).
fof(f147, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', mDefDiff)).
fof(f1285, plain, (~ sP3(xQ, xx) | spl29_41), inference(avatar_component_clause, [], [f1283])).
fof(f1283, plain, (spl29_41 <=> sP3(xQ, xx)), introduced(avatar_definition, [new_symbols(naming, [spl29_41])])).
fof(f1290, plain, (~ spl29_41 | spl29_42), inference(avatar_split_clause, [], [f1281, f1287, f1283])).
fof(f1281, plain, (sP2(xx, xQ, xP) | ~ sP3(xQ, xx)), inference(superposition, [], [f583, f629])).
fof(f629, plain, (xP = sdtmndt0(xQ, xx)), inference(forward_demodulation, [], [f551, f573])).
fof(f573, plain, (szmzizndt0(xQ) = xx), inference(definition_unfolding, [], [f549, f572])).
fof(f572, plain, (xp = xx), inference(cnf_transformation, [], [f120])).
fof(f120, plain, (xp = xx), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__5496)).
fof(f549, plain, (xp = szmzizndt0(xQ)), inference(cnf_transformation, [], [f103])).
fof(f103, plain, (xp = szmzizndt0(xQ)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__5147)).
fof(f551, plain, (xP = sdtmndt0(xQ, szmzizndt0(xQ))), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ((xP = sdtmndt0(xQ, szmzizndt0(xQ))) & aSet0(xP)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM625+1.p', m__5164)).
fof(f583, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f372])).
fof(f372, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f276])).
fof(f276, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f258])).