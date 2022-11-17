fof(f284225, plain, $false, inference(avatar_sat_refutation, [], [f12387, f49180, f57566, f57972, f284224])).
fof(f284224, plain, (~ spl29_1505 | spl29_1513), inference(avatar_contradiction_clause, [], [f284223])).
fof(f284223, plain, ($false | (~ spl29_1505 | spl29_1513)), inference(subsumption_resolution, [], [f284222, f544])).
fof(f544, plain, aSet0(xP), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ((xP = sdtmndt0(xQ, szmzizndt0(xQ))) & aSet0(xP)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__5164)).
fof(f284222, plain, (~ aSet0(xP) | (~ spl29_1505 | spl29_1513)), inference(subsumption_resolution, [], [f284203, f49171])).
fof(f49171, plain, (~ aSubsetOf0(xP, xD) | spl29_1513), inference(avatar_component_clause, [], [f49169])).
fof(f49169, plain, (spl29_1513 <=> aSubsetOf0(xP, xD)), introduced(avatar_definition, [new_symbols(naming, [spl29_1513])])).
fof(f284203, plain, (aSubsetOf0(xP, xD) | ~ aSet0(xP) | ~ spl29_1505), inference(resolution, [], [f72724, f556])).
fof(f556, plain, aSubsetOf0(xP, sdtlpdtrp0(xN, szszuzczcdt0(xn))), inference(cnf_transformation, [], [f113])).
fof(f113, plain, aSubsetOf0(xP, sdtlpdtrp0(xN, szszuzczcdt0(xn))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__5334)).
fof(f72724, plain, (! [X2] : (~ aSubsetOf0(X2, sdtlpdtrp0(xN, szszuzczcdt0(xn))) | aSubsetOf0(X2, xD) | ~ aSet0(X2)) | ~ spl29_1505), inference(subsumption_resolution, [], [f72723, f49130])).
fof(f49130, plain, (aSet0(xD) | ~ spl29_1505), inference(avatar_component_clause, [], [f49129])).
fof(f49129, plain, (spl29_1505 <=> aSet0(xD)), introduced(avatar_definition, [new_symbols(naming, [spl29_1505])])).
fof(f72723, plain, ! [X2] : (~ aSet0(xD) | aSubsetOf0(X2, xD) | ~ aSubsetOf0(X2, sdtlpdtrp0(xN, szszuzczcdt0(xn))) | ~ aSet0(X2)), inference(forward_demodulation, [], [f72722, f1531])).
fof(f1531, plain, (xD = sdtmndt0(sdtlpdtrp0(xN, xn), xp)), inference(backward_demodulation, [], [f557, f1528])).
fof(f1528, plain, (xp = szmzizndt0(sdtlpdtrp0(xN, xn))), inference(forward_demodulation, [], [f1521, f554])).
fof(f554, plain, (xp = sdtlpdtrp0(xe, xn)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ((xp = sdtlpdtrp0(xe, xn)) & aElementOf0(xn, szNzAzT0) & aElementOf0(xn, sdtlbdtrb0(xd, szDzizrdt0(xd)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__5309)).
fof(f1521, plain, (sdtlpdtrp0(xe, xn) = szmzizndt0(sdtlpdtrp0(xN, xn))), inference(resolution, [], [f523, f553])).
fof(f553, plain, aElementOf0(xn, szNzAzT0), inference(cnf_transformation, [], [f111])).
fof(f523, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (szmzizndt0(sdtlpdtrp0(xN, X0)) = sdtlpdtrp0(xe, X0))), inference(cnf_transformation, [], [f244])).
fof(f244, plain, (! [X0] : ((szmzizndt0(sdtlpdtrp0(xN, X0)) = sdtlpdtrp0(xe, X0)) | ~ aElementOf0(X0, szNzAzT0)) & (szNzAzT0 = szDzozmdt0(xe)) & aFunction0(xe)), inference(ennf_transformation, [], [f91])).
fof(f91, plain, (! [X0] : (aElementOf0(X0, szNzAzT0) => (szmzizndt0(sdtlpdtrp0(xN, X0)) = sdtlpdtrp0(xe, X0))) & (szNzAzT0 = szDzozmdt0(xe)) & aFunction0(xe)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__4660)).
fof(f557, plain, (xD = sdtmndt0(sdtlpdtrp0(xN, xn), szmzizndt0(sdtlpdtrp0(xN, xn)))), inference(cnf_transformation, [], [f114])).
fof(f114, plain, (xD = sdtmndt0(sdtlpdtrp0(xN, xn), szmzizndt0(sdtlpdtrp0(xN, xn)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__5585)).
fof(f72722, plain, ! [X2] : (aSubsetOf0(X2, xD) | ~ aSubsetOf0(X2, sdtlpdtrp0(xN, szszuzczcdt0(xn))) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, xn), xp)) | ~ aSet0(X2)), inference(forward_demodulation, [], [f72721, f1531])).
fof(f72721, plain, ! [X2] : (aSubsetOf0(X2, sdtmndt0(sdtlpdtrp0(xN, xn), xp)) | ~ aSubsetOf0(X2, sdtlpdtrp0(xN, szszuzczcdt0(xn))) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, xn), xp)) | ~ aSet0(X2)), inference(subsumption_resolution, [], [f72707, f553])).
fof(f72707, plain, ! [X2] : (aSubsetOf0(X2, sdtmndt0(sdtlpdtrp0(xN, xn), xp)) | ~ aElementOf0(xn, szNzAzT0) | ~ aSubsetOf0(X2, sdtlpdtrp0(xN, szszuzczcdt0(xn))) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, xn), xp)) | ~ aSet0(X2)), inference(superposition, [], [f2719, f1528])).
fof(f2719, plain, ! [X0, X1] : (aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0)))) | ~ aElementOf0(X0, szNzAzT0) | ~ aSubsetOf0(X1, sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0)))) | ~ aSet0(X1)), inference(resolution, [], [f1017, f599])).
fof(f599, plain, ! [X2, X0, X1] : (~ aSubsetOf0(X1, X2) | aSubsetOf0(X0, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f353, f346])).
fof(f346, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f263])).
fof(f263, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f261, f262])).
fof(f262, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f261, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f260])).
fof(f260, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f259])).
fof(f259, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', mDefSub)).
fof(f353, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, X2) | ~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0, X1, X2] : (aSubsetOf0(X0, X2) | ~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f137])).
fof(f137, plain, ! [X0, X1, X2] : ((aSubsetOf0(X0, X2) | (~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1, X2] : ((aSet0(X2) & aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X2) & aSubsetOf0(X0, X1)) => aSubsetOf0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', mSubTrans)).
fof(f1017, plain, ! [X0] : (aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0)))) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f1016, f503])).
fof(f503, plain, ! [X0] : (aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ! [X0] : ((isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__3671)).
fof(f1016, plain, ! [X0] : (aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0)))) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f501, f504])).
fof(f504, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f228])).
fof(f501, plain, ! [X0] : (aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0)))) | ~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f227])).
fof(f227, plain, (! [X0] : ((isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) | ~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), inference(flattening, [], [f226])).
fof(f226, plain, (! [X0] : (((isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) | (~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0))) | ~ aElementOf0(X0, szNzAzT0)) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, (! [X0] : (aElementOf0(X0, szNzAzT0) => ((isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0)) => (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))))) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__3623)).
fof(f57972, plain, (~ spl29_185 | spl29_1505), inference(avatar_split_clause, [], [f56680, f49129, f3795])).
fof(f3795, plain, (spl29_185 <=> sP3(sdtlpdtrp0(xN, xn), xp)), introduced(avatar_definition, [new_symbols(naming, [spl29_185])])).
fof(f56680, plain, (aSet0(xD) | ~ sP3(sdtlpdtrp0(xN, xn), xp)), inference(superposition, [], [f1275, f1531])).
fof(f1275, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f564, f368])).
fof(f368, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f275])).
fof(f275, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f273, f274])).
fof(f274, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f273, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f272])).
fof(f272, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f271])).
fof(f271, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f251])).
fof(f251, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e251])).
fof(e251, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f564, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f366])).
fof(f366, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f270])).
fof(f270, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f252])).
fof(f252, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e252])).
fof(e252, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f57566, plain, (~ spl29_44 | spl29_185), inference(avatar_contradiction_clause, [], [f57565])).
fof(f57565, plain, ($false | (~ spl29_44 | spl29_185)), inference(subsumption_resolution, [], [f57564, f1337])).
fof(f1337, plain, (aSet0(sdtlpdtrp0(xN, xn)) | ~ spl29_44), inference(avatar_component_clause, [], [f1336])).
fof(f1336, plain, (spl29_44 <=> aSet0(sdtlpdtrp0(xN, xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_44])])).
fof(f57564, plain, (~ aSet0(sdtlpdtrp0(xN, xn)) | spl29_185), inference(subsumption_resolution, [], [f57563, f755])).
fof(f755, plain, aElement0(xp), inference(subsumption_resolution, [], [f724, f530])).
fof(f530, plain, aSet0(xO), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((xO = sdtlcdtrc0(xe, sdtlbdtrb0(xd, szDzizrdt0(xd)))) & aSet0(xO)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__4891)).
fof(f724, plain, (aElement0(xp) | ~ aSet0(xO)), inference(resolution, [], [f339, f547])).
fof(f547, plain, aElementOf0(xp, xO), inference(cnf_transformation, [], [f106])).
fof(f106, plain, aElementOf0(xp, xO), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__5182)).
fof(f339, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', mEOfElem)).
fof(f57563, plain, (~ aElement0(xp) | ~ aSet0(sdtlpdtrp0(xN, xn)) | spl29_185), inference(resolution, [], [f3797, f377])).
fof(f377, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f253])).
fof(f253, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f142, e252, e251])).
fof(f142, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f141])).
fof(f141, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', mDefDiff)).
fof(f3797, plain, (~ sP3(sdtlpdtrp0(xN, xn), xp) | spl29_185), inference(avatar_component_clause, [], [f3795])).
fof(f49180, plain, (~ spl29_1505 | ~ spl29_1513), inference(avatar_split_clause, [], [f10789, f49169, f49129])).
fof(f10789, plain, (~ aSubsetOf0(xP, xD) | ~ aSet0(xD)), inference(resolution, [], [f5740, f558])).
fof(f558, plain, ~ aElementOf0(xP, slbdtsldtrb0(xD, xk)), inference(cnf_transformation, [], [f124])).
fof(f124, plain, ~ aElementOf0(xP, slbdtsldtrb0(xD, xk)), inference(flattening, [], [f116])).
fof(f116, plain, ~ aElementOf0(xP, slbdtsldtrb0(xD, xk)), inference(negated_conjecture, [], [f115])).
fof(f115, plain, ~ aElementOf0(xP, slbdtsldtrb0(xD, xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__)).
fof(f5740, plain, ! [X3] : (aElementOf0(xP, slbdtsldtrb0(X3, xk)) | ~ aSubsetOf0(xP, X3) | ~ aSet0(X3)), inference(subsumption_resolution, [], [f5737, f496])).
fof(f496, plain, aElementOf0(xk, szNzAzT0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((xK = szszuzczcdt0(xk)) & aElementOf0(xk, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__3533)).
fof(f5737, plain, ! [X3] : (aElementOf0(xP, slbdtsldtrb0(X3, xk)) | ~ aSubsetOf0(xP, X3) | ~ aElementOf0(xk, szNzAzT0) | ~ aSet0(X3)), inference(superposition, [], [f577, f550])).
fof(f550, plain, (xk = sbrdtbr0(xP)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, (xk = sbrdtbr0(xP)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', m__5217)).
fof(f577, plain, ! [X4, X0] : (aElementOf0(X4, slbdtsldtrb0(X0, sbrdtbr0(X4))) | ~ aSubsetOf0(X4, X0) | ~ aElementOf0(sbrdtbr0(X4), szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f576])).
fof(f576, plain, ! [X4, X2, X0] : (aElementOf0(X4, X2) | ~ aSubsetOf0(X4, X0) | ~ (slbdtsldtrb0(X0, sbrdtbr0(X4)) = X2) | ~ aElementOf0(sbrdtbr0(X4), szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f442])).
fof(f442, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f307])).
fof(f307, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f305, f306])).
fof(f306, plain, ! [X2, X1, X0] : (? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) => ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f305, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(rectify, [], [f304])).
fof(f304, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f303])).
fof(f303, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | (? [X3] : (((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0))) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(nnf_transformation, [], [f200])).
fof(f200, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f199])).
fof(f199, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | (~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aSet0(X0)) => ! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', mDefSel)).
fof(f12387, plain, spl29_44, inference(avatar_contradiction_clause, [], [f12386])).
fof(f12386, plain, ($false | spl29_44), inference(subsumption_resolution, [], [f12385, f553])).
fof(f12385, plain, (~ aElementOf0(xn, szNzAzT0) | spl29_44), inference(resolution, [], [f1338, f794])).
fof(f794, plain, ! [X0] : (aSet0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f792, f384])).
fof(f384, plain, aSet0(szNzAzT0), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (isCountable0(szNzAzT0) & aSet0(szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM629+1.p', mNATSet)).
fof(f792, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | aSet0(sdtlpdtrp0(xN, X0)) | ~ aSet0(szNzAzT0)), inference(resolution, [], [f503, f346])).
fof(f1338, plain, (~ aSet0(sdtlpdtrp0(xN, xn)) | spl29_44), inference(avatar_component_clause, [], [f1336])).