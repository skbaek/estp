fof(f7057, plain, $false, inference(avatar_sat_refutation, [], [f1349, f1356, f6956])).
fof(f6956, plain, (~ spl92_1 | spl92_2), inference(avatar_contradiction_clause, [], [f6955])).
fof(f6955, plain, ($false | (~ spl92_1 | spl92_2)), inference(subsumption_resolution, [], [f6887, f452])).
fof(f452, plain, ! [X3] : visFreeVar(X3, vvar(X3)), inference(equality_resolution, [], [f451])).
fof(f451, plain, ! [X0, X3] : (visFreeVar(X0, vvar(X3)) | ~ (X0 = X3)), inference(equality_resolution, [], [f450])).
fof(f450, plain, ! [X0, X3, X1] : (visFreeVar(X0, X1) | ~ (vvar(X3) = X1) | ~ (X0 = X3)), inference(equality_resolution, [], [f301])).
fof(f301, plain, ! [X2, X0, X3, X1] : (visFreeVar(X0, X1) | ~ (X2 = X3) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | (~ (vvar(X2) = X1) | ~ (X0 = X3))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1, X2, X3] : (((vvar(X2) = X1) & (X0 = X3)) => ((visFreeVar(X0, X1) => (X2 = X3)) & ((X2 = X3) => visFreeVar(X0, X1)))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X0, X3, X8, X13] : (((vvar(X8) = X3) & (X0 = X13)) => ((visFreeVar(X0, X3) => (X8 = X13)) & ((X8 = X13) => visFreeVar(X0, X3)))), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', isFreeVar0)).
fof(f6887, plain, (~ visFreeVar(sK87, vvar(sK87)) | (~ spl92_1 | spl92_2)), inference(backward_demodulation, [], [f436, f6883])).
fof(f6883, plain, ((sK87 = sK90) | (~ spl92_1 | spl92_2)), inference(resolution, [], [f6882, f438])).
fof(f438, plain, ~ vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91), inference(cnf_transformation, [], [f285])).
fof(f285, plain, (~ vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91) & vtcheck(sK89, vvar(sK90), sK91) & ~ visFreeVar(sK87, vvar(sK90))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87, sK88, sK89, sK90, sK91])], [f196, f284])).
fof(f284, plain, (? [X0, X1, X2, X3, X4] : (~ vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & vtcheck(X2, vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3))) => (~ vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91) & vtcheck(sK89, vvar(sK90), sK91) & ~ visFreeVar(sK87, vvar(sK90)))), introduced(choice_axiom, [])).
fof(f196, plain, ? [X0, X1, X2, X3, X4] : (~ vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & vtcheck(X2, vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3))), inference(flattening, [], [f195])).
fof(f195, plain, ? [X0, X1, X2, X3, X4] : (~ vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & (vtcheck(X2, vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3)))), inference(ennf_transformation, [], [f113])).
fof(f113, plain, ~ ! [X0, X1, X2, X3, X4] : ((vtcheck(X2, vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3))) => vtcheck(vbind(X0, X1, X2), vvar(X3), X4)), inference(rectify, [], [f58])).
fof(f58, plain, ~ ! [X8, X9, X19, X20, X14] : ((vtcheck(X19, vvar(X20), X14) & ~ visFreeVar(X8, vvar(X20))) => vtcheck(vbind(X8, X9, X19), vvar(X20), X14)), inference(negated_conjecture, [], [f57])).
fof(f57, plain, ~ ! [X8, X9, X19, X20, X14] : ((vtcheck(X19, vvar(X20), X14) & ~ visFreeVar(X8, vvar(X20))) => vtcheck(vbind(X8, X9, X19), vvar(X20), X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', 'T-Weak-FreeVar-var')).
fof(f6882, plain, (! [X0, X1] : (vtcheck(vbind(X0, X1, sK89), vvar(sK90), sK91) | (sK90 = X0)) | (~ spl92_1 | spl92_2)), inference(equality_resolution, [], [f1937])).
fof(f1937, plain, (! [X19, X17, X18] : (~ (vsomeType(sK91) = vsomeType(X17)) | vtcheck(vbind(X18, X19, sK89), vvar(sK90), X17) | (sK90 = X18)) | (~ spl92_1 | spl92_2)), inference(superposition, [], [f641, f1421])).
fof(f1421, plain, ((vsomeType(sK91) = vlookup(sK90, sK89)) | (~ spl92_1 | spl92_2)), inference(backward_demodulation, [], [f1404, f1406])).
fof(f1406, plain, ((sK90 = sK86(vvar(sK90), sK91, sK89)) | ~ spl92_1), inference(resolution, [], [f1372, f449])).
fof(f449, plain, ! [X2, X3] : (~ visFreeVar(X3, vvar(X2)) | (X2 = X3)), inference(equality_resolution, [], [f448])).
fof(f448, plain, ! [X2, X0, X3] : ((X2 = X3) | ~ visFreeVar(X0, vvar(X2)) | ~ (X0 = X3)), inference(equality_resolution, [], [f302])).
fof(f302, plain, ! [X2, X0, X3, X1] : ((X2 = X3) | ~ visFreeVar(X0, X1) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f123])).
fof(f1372, plain, (visFreeVar(sK86(vvar(sK90), sK91, sK89), vvar(sK90)) | ~ spl92_1), inference(superposition, [], [f452, f1344])).
fof(f1344, plain, ((vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89))) | ~ spl92_1), inference(avatar_component_clause, [], [f1342])).
fof(f1342, plain, (spl92_1 <=> (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89)))), introduced(avatar_definition, [new_symbols(naming, [spl92_1])])).
fof(f1404, plain, ((vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, sK89), sK89)) | spl92_2), inference(subsumption_resolution, [], [f1403, f296])).
fof(f296, plain, ! [X2, X0, X1] : ~ (vvar(X0) = vapp(X1, X2)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1, X2] : ~ (vvar(X0) = vapp(X1, X2)), inference(rectify, [], [f5])).
fof(f5, plain, ! [X0, X3, X5] : ~ (vvar(X0) = vapp(X3, X5)), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', 'DIFF-var-app')).
fof(f1403, plain, ((vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, sK89), sK89)) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, sK89), sK84(vvar(sK90), sK91, sK89))) | spl92_2), inference(subsumption_resolution, [], [f1396, f1347])).
fof(f1347, plain, (~ sP12(sK89, sK91, vvar(sK90)) | spl92_2), inference(avatar_component_clause, [], [f1346])).
fof(f1346, plain, (spl92_2 <=> sP12(sK89, sK91, vvar(sK90))), introduced(avatar_definition, [new_symbols(naming, [spl92_2])])).
fof(f1396, plain, (sP12(sK89, sK91, vvar(sK90)) | (vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, sK89), sK89)) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, sK89), sK84(vvar(sK90), sK91, sK89)))), inference(resolution, [], [f429, f437])).
fof(f437, plain, vtcheck(sK89, vvar(sK90), sK91), inference(cnf_transformation, [], [f285])).
fof(f429, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f283])).
fof(f283, plain, ! [X0, X1, X2] : ((vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)) | sP12(X2, X1, X0) | ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0)) | ~ vtcheck(X2, X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK83, sK84, sK85, sK86])], [f280, f282, f281])).
fof(f281, plain, ! [X2, X1, X0] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) => (vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f282, plain, ! [X2, X1, X0] : (? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) => ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f280, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) | ~ vtcheck(X2, X0, X1)), inference(rectify, [], [f213])).
fof(f213, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(definition_folding, [], [f190, e212])).
fof(f212, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(usedef, [], [e212])).
fof(e212, plain, ! [X2, X1, X0] : (sP12(X2, X1, X0) <=> ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f190, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(flattening, [], [f189])).
fof(f189, plain, ! [X0, X1, X2] : ((? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0))) | ~ vtcheck(X2, X0, X1)), inference(ennf_transformation, [], [f110])).
fof(f110, plain, ! [X0, X1, X2] : (vtcheck(X2, X0, X1) => (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)))), inference(rectify, [], [f54])).
fof(f54, plain, ! [X10, X14, X19] : (vtcheck(X19, X10, X14) => (? [X11, X12, X9] : (vtcheck(X19, X12, X9) & vtcheck(X19, X11, varrow(X9, X14)) & (vapp(X11, X12) = X10)) | ? [X8, X12, X32, X33] : (vtcheck(vbind(X8, X32, X19), X12, X33) & (varrow(X32, X33) = X14) & (vabs(X8, X32, X12) = X10)) | ? [X8] : ((vlookup(X8, X19) = vsomeType(X14)) & (vvar(X8) = X10)))), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', 'T-inv')).
fof(f641, plain, ! [X6, X4, X7, X5, X3] : (~ (vlookup(X3, X6) = vsomeType(X7)) | vtcheck(vbind(X4, X5, X6), vvar(X3), X7) | (X3 = X4)), inference(superposition, [], [f422, f483])).
fof(f483, plain, ! [X6, X0, X5, X1] : ((vlookup(X5, X6) = vlookup(X5, vbind(X1, X0, X6))) | (X1 = X5)), inference(equality_resolution, [], [f482])).
fof(f482, plain, ! [X6, X2, X0, X5, X1] : ((vlookup(X5, X6) = vlookup(X2, vbind(X1, X0, X6))) | (X1 = X5) | ~ (X2 = X5)), inference(equality_resolution, [], [f481])).
fof(f481, plain, ! [X6, X2, X0, X5, X3, X1] : ((vlookup(X2, X3) = vlookup(X5, X6)) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(equality_resolution, [], [f324])).
fof(f324, plain, ! [X6, X4, X2, X0, X5, X3, X1] : ((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(flattening, [], [f139])).
fof(f139, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4)) | (X1 = X5)) | (~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0, X1, X2, X3, X4, X5, X6] : (((vbind(X1, X0, X6) = X3) & (X2 = X5)) => (~ (X1 = X5) => ((vlookup(X2, X3) = X4) => (vlookup(X5, X6) = X4)))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X21, X20, X0, X15, X18, X8, X19] : (((vbind(X20, X21, X19) = X15) & (X0 = X8)) => (~ (X8 = X20) => ((vlookup(X0, X15) = X18) => (vlookup(X8, X19) = X18)))), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', lookup2)).
fof(f422, plain, ! [X2, X0, X1] : (~ (vsomeType(X2) = vlookup(X1, X0)) | vtcheck(X0, vvar(X1), X2)), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ! [X0, X1, X2] : (vtcheck(X0, vvar(X1), X2) | ~ (vsomeType(X2) = vlookup(X1, X0))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ! [X0, X1, X2] : ((vsomeType(X2) = vlookup(X1, X0)) => vtcheck(X0, vvar(X1), X2)), inference(rectify, [], [f51])).
fof(f51, plain, ! [X19, X8, X14] : ((vlookup(X8, X19) = vsomeType(X14)) => vtcheck(X19, vvar(X8), X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', 'T-var')).
fof(f436, plain, ~ visFreeVar(sK87, vvar(sK90)), inference(cnf_transformation, [], [f285])).
fof(f1356, plain, ~ spl92_2, inference(avatar_contradiction_clause, [], [f1355])).
fof(f1355, plain, ($false | ~ spl92_2), inference(subsumption_resolution, [], [f1353, f295])).
fof(f295, plain, ! [X2, X0, X3, X1] : ~ (vvar(X0) = vabs(X1, X2, X3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1, X2, X3] : ~ (vvar(X0) = vabs(X1, X2, X3)), file('/home/ubuntu/library/tptp/Problems/COM/COM127+1.p', 'DIFF-var-abs')).
fof(f1353, plain, ((vvar(sK90) = vabs(sK79(sK89, sK91, vvar(sK90)), sK81(sK89, sK91, vvar(sK90)), sK80(sK89, sK91, vvar(sK90)))) | ~ spl92_2), inference(resolution, [], [f1348, f425])).
fof(f425, plain, ! [X2, X0, X1] : (~ sP12(X0, X1, X2) | (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)), inference(cnf_transformation, [], [f279])).
fof(f279, plain, ! [X0, X1, X2] : ((vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)) | ~ sP12(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79, sK80, sK81, sK82])], [f277, f278])).
fof(f278, plain, ! [X2, X1, X0] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) => (vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2))), introduced(choice_axiom, [])).
fof(f277, plain, ! [X0, X1, X2] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) | ~ sP12(X0, X1, X2)), inference(rectify, [], [f276])).
fof(f276, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(nnf_transformation, [], [f212])).
fof(f1348, plain, (sP12(sK89, sK91, vvar(sK90)) | ~ spl92_2), inference(avatar_component_clause, [], [f1346])).
fof(f1349, plain, (spl92_1 | spl92_2), inference(avatar_split_clause, [], [f1340, f1346, f1342])).
fof(f1340, plain, (sP12(sK89, sK91, vvar(sK90)) | (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89)))), inference(subsumption_resolution, [], [f1335, f296])).
fof(f1335, plain, (sP12(sK89, sK91, vvar(sK90)) | (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89))) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, sK89), sK84(vvar(sK90), sK91, sK89)))), inference(resolution, [], [f428, f437])).
fof(f428, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vvar(sK86(X0, X1, X2)) = X0) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f283])).