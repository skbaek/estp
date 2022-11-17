fof(f1745, plain, $false, inference(avatar_sat_refutation, [], [f1325, f1332, f1418, f1474, f1744])).
fof(f1744, plain, ~ spl92_4, inference(avatar_contradiction_clause, [], [f1743])).
fof(f1743, plain, ($false | ~ spl92_4), inference(subsumption_resolution, [], [f1742, f433])).
fof(f433, plain, ~ vtcheck(sK89, vvar(sK90), sK91), inference(cnf_transformation, [], [f281])).
fof(f281, plain, (~ vtcheck(sK89, vvar(sK90), sK91) & vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91) & ~ visFreeVar(sK87, vvar(sK90))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87, sK88, sK89, sK90, sK91])], [f192, f280])).
fof(f280, plain, (? [X0, X1, X2, X3, X4] : (~ vtcheck(X2, vvar(X3), X4) & vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3))) => (~ vtcheck(sK89, vvar(sK90), sK91) & vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91) & ~ visFreeVar(sK87, vvar(sK90)))), introduced(choice_axiom, [])).
fof(f192, plain, ? [X0, X1, X2, X3, X4] : (~ vtcheck(X2, vvar(X3), X4) & vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3))), inference(flattening, [], [f191])).
fof(f191, plain, ? [X0, X1, X2, X3, X4] : (~ vtcheck(X2, vvar(X3), X4) & (vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3)))), inference(ennf_transformation, [], [f111])).
fof(f111, plain, ~ ! [X0, X1, X2, X3, X4] : ((vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & ~ visFreeVar(X0, vvar(X3))) => vtcheck(X2, vvar(X3), X4)), inference(rectify, [], [f57])).
fof(f57, plain, ~ ! [X8, X9, X19, X20, X14] : ((vtcheck(vbind(X8, X9, X19), vvar(X20), X14) & ~ visFreeVar(X8, vvar(X20))) => vtcheck(X19, vvar(X20), X14)), inference(negated_conjecture, [], [f56])).
fof(f56, plain, ~ ! [X8, X9, X19, X20, X14] : ((vtcheck(vbind(X8, X9, X19), vvar(X20), X14) & ~ visFreeVar(X8, vvar(X20))) => vtcheck(X19, vvar(X20), X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', 'T-Strong-var')).
fof(f1742, plain, (vtcheck(sK89, vvar(sK90), sK91) | ~ spl92_4), inference(equality_resolution, [], [f1500])).
fof(f1500, plain, (! [X3] : (~ (vsomeType(X3) = vsomeType(sK91)) | vtcheck(sK89, vvar(sK90), X3)) | ~ spl92_4), inference(superposition, [], [f418, f1416])).
fof(f1416, plain, ((vsomeType(sK91) = vlookup(sK90, sK89)) | ~ spl92_4), inference(avatar_component_clause, [], [f1414])).
fof(f1414, plain, (spl92_4 <=> (vsomeType(sK91) = vlookup(sK90, sK89))), introduced(avatar_definition, [new_symbols(naming, [spl92_4])])).
fof(f418, plain, ! [X2, X0, X1] : (~ (vsomeType(X2) = vlookup(X1, X0)) | vtcheck(X0, vvar(X1), X2)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0, X1, X2] : (vtcheck(X0, vvar(X1), X2) | ~ (vsomeType(X2) = vlookup(X1, X0))), inference(ennf_transformation, [], [f106])).
fof(f106, plain, ! [X0, X1, X2] : ((vsomeType(X2) = vlookup(X1, X0)) => vtcheck(X0, vvar(X1), X2)), inference(rectify, [], [f51])).
fof(f51, plain, ! [X19, X8, X14] : ((vlookup(X8, X19) = vsomeType(X14)) => vtcheck(X19, vvar(X8), X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', 'T-var')).
fof(f1474, plain, ~ spl92_3, inference(avatar_contradiction_clause, [], [f1473])).
fof(f1473, plain, ($false | ~ spl92_3), inference(subsumption_resolution, [], [f1461, f447])).
fof(f447, plain, ! [X3] : visFreeVar(X3, vvar(X3)), inference(equality_resolution, [], [f446])).
fof(f446, plain, ! [X0, X3] : (visFreeVar(X0, vvar(X3)) | ~ (X0 = X3)), inference(equality_resolution, [], [f445])).
fof(f445, plain, ! [X0, X3, X1] : (visFreeVar(X0, X1) | ~ (vvar(X3) = X1) | ~ (X0 = X3)), inference(equality_resolution, [], [f297])).
fof(f297, plain, ! [X2, X0, X3, X1] : (visFreeVar(X0, X1) | ~ (X2 = X3) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(flattening, [], [f120])).
fof(f120, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | (~ (vvar(X2) = X1) | ~ (X0 = X3))), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0, X1, X2, X3] : (((vvar(X2) = X1) & (X0 = X3)) => ((visFreeVar(X0, X1) => (X2 = X3)) & ((X2 = X3) => visFreeVar(X0, X1)))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X0, X3, X8, X13] : (((vvar(X8) = X3) & (X0 = X13)) => ((visFreeVar(X0, X3) => (X8 = X13)) & ((X8 = X13) => visFreeVar(X0, X3)))), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', isFreeVar0)).
fof(f1461, plain, (~ visFreeVar(sK87, vvar(sK87)) | ~ spl92_3), inference(backward_demodulation, [], [f431, f1412])).
fof(f1412, plain, ((sK87 = sK90) | ~ spl92_3), inference(avatar_component_clause, [], [f1410])).
fof(f1410, plain, (spl92_3 <=> (sK87 = sK90)), introduced(avatar_definition, [new_symbols(naming, [spl92_3])])).
fof(f431, plain, ~ visFreeVar(sK87, vvar(sK90)), inference(cnf_transformation, [], [f281])).
fof(f1418, plain, (spl92_3 | spl92_4 | ~ spl92_1 | spl92_2), inference(avatar_split_clause, [], [f1403, f1322, f1318, f1414, f1410])).
fof(f1318, plain, (spl92_1 <=> (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89))))), introduced(avatar_definition, [new_symbols(naming, [spl92_1])])).
fof(f1322, plain, (spl92_2 <=> sP12(vbind(sK87, sK88, sK89), sK91, vvar(sK90))), introduced(avatar_definition, [new_symbols(naming, [spl92_2])])).
fof(f1403, plain, ((vsomeType(sK91) = vlookup(sK90, sK89)) | (sK87 = sK90) | (~ spl92_1 | spl92_2)), inference(superposition, [], [f478, f1396])).
fof(f1396, plain, ((vsomeType(sK91) = vlookup(sK90, vbind(sK87, sK88, sK89))) | (~ spl92_1 | spl92_2)), inference(backward_demodulation, [], [f1379, f1381])).
fof(f1381, plain, ((sK90 = sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89))) | ~ spl92_1), inference(resolution, [], [f1346, f444])).
fof(f444, plain, ! [X2, X3] : (~ visFreeVar(X3, vvar(X2)) | (X2 = X3)), inference(equality_resolution, [], [f443])).
fof(f443, plain, ! [X2, X0, X3] : ((X2 = X3) | ~ visFreeVar(X0, vvar(X2)) | ~ (X0 = X3)), inference(equality_resolution, [], [f298])).
fof(f298, plain, ! [X2, X0, X3, X1] : ((X2 = X3) | ~ visFreeVar(X0, X1) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f121])).
fof(f1346, plain, (visFreeVar(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), vvar(sK90)) | ~ spl92_1), inference(superposition, [], [f447, f1320])).
fof(f1320, plain, ((vvar(sK90) = vvar(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89)))) | ~ spl92_1), inference(avatar_component_clause, [], [f1318])).
fof(f1379, plain, ((vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), vbind(sK87, sK88, sK89))) | spl92_2), inference(subsumption_resolution, [], [f1378, f292])).
fof(f292, plain, ! [X2, X0, X1] : ~ (vvar(X0) = vapp(X1, X2)), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ! [X0, X1, X2] : ~ (vvar(X0) = vapp(X1, X2)), inference(rectify, [], [f5])).
fof(f5, plain, ! [X0, X3, X5] : ~ (vvar(X0) = vapp(X3, X5)), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', 'DIFF-var-app')).
fof(f1378, plain, ((vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), vbind(sK87, sK88, sK89))) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), sK84(vvar(sK90), sK91, vbind(sK87, sK88, sK89)))) | spl92_2), inference(subsumption_resolution, [], [f1370, f1323])).
fof(f1323, plain, (~ sP12(vbind(sK87, sK88, sK89), sK91, vvar(sK90)) | spl92_2), inference(avatar_component_clause, [], [f1322])).
fof(f1370, plain, (sP12(vbind(sK87, sK88, sK89), sK91, vvar(sK90)) | (vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), vbind(sK87, sK88, sK89))) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), sK84(vvar(sK90), sK91, vbind(sK87, sK88, sK89))))), inference(resolution, [], [f425, f432])).
fof(f432, plain, vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91), inference(cnf_transformation, [], [f281])).
fof(f425, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f279])).
fof(f279, plain, ! [X0, X1, X2] : ((vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)) | sP12(X2, X1, X0) | ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0)) | ~ vtcheck(X2, X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK83, sK84, sK85, sK86])], [f276, f278, f277])).
fof(f277, plain, ! [X2, X1, X0] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) => (vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f278, plain, ! [X2, X1, X0] : (? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) => ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f276, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) | ~ vtcheck(X2, X0, X1)), inference(rectify, [], [f209])).
fof(f209, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(definition_folding, [], [f188, e208])).
fof(f208, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(usedef, [], [e208])).
fof(e208, plain, ! [X2, X1, X0] : (sP12(X2, X1, X0) <=> ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f188, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(flattening, [], [f187])).
fof(f187, plain, ! [X0, X1, X2] : ((? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0))) | ~ vtcheck(X2, X0, X1)), inference(ennf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1, X2] : (vtcheck(X2, X0, X1) => (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)))), inference(rectify, [], [f54])).
fof(f54, plain, ! [X10, X14, X19] : (vtcheck(X19, X10, X14) => (? [X11, X12, X9] : (vtcheck(X19, X12, X9) & vtcheck(X19, X11, varrow(X9, X14)) & (vapp(X11, X12) = X10)) | ? [X8, X12, X32, X33] : (vtcheck(vbind(X8, X32, X19), X12, X33) & (varrow(X32, X33) = X14) & (vabs(X8, X32, X12) = X10)) | ? [X8] : ((vlookup(X8, X19) = vsomeType(X14)) & (vvar(X8) = X10)))), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', 'T-inv')).
fof(f478, plain, ! [X6, X0, X5, X1] : ((vlookup(X5, X6) = vlookup(X5, vbind(X1, X0, X6))) | (X1 = X5)), inference(equality_resolution, [], [f477])).
fof(f477, plain, ! [X6, X2, X0, X5, X1] : ((vlookup(X5, X6) = vlookup(X2, vbind(X1, X0, X6))) | (X1 = X5) | ~ (X2 = X5)), inference(equality_resolution, [], [f476])).
fof(f476, plain, ! [X6, X2, X0, X5, X3, X1] : ((vlookup(X2, X3) = vlookup(X5, X6)) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(equality_resolution, [], [f320])).
fof(f320, plain, ! [X6, X4, X2, X0, X5, X3, X1] : ((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(flattening, [], [f137])).
fof(f137, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4)) | (X1 = X5)) | (~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1, X2, X3, X4, X5, X6] : (((vbind(X1, X0, X6) = X3) & (X2 = X5)) => (~ (X1 = X5) => ((vlookup(X2, X3) = X4) => (vlookup(X5, X6) = X4)))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X21, X20, X0, X15, X18, X8, X19] : (((vbind(X20, X21, X19) = X15) & (X0 = X8)) => (~ (X8 = X20) => ((vlookup(X0, X15) = X18) => (vlookup(X8, X19) = X18)))), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', lookup2)).
fof(f1332, plain, ~ spl92_2, inference(avatar_contradiction_clause, [], [f1331])).
fof(f1331, plain, ($false | ~ spl92_2), inference(subsumption_resolution, [], [f1329, f291])).
fof(f291, plain, ! [X2, X0, X3, X1] : ~ (vvar(X0) = vabs(X1, X2, X3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1, X2, X3] : ~ (vvar(X0) = vabs(X1, X2, X3)), file('/home/ubuntu/library/tptp/Problems/COM/COM138+1.p', 'DIFF-var-abs')).
fof(f1329, plain, ((vvar(sK90) = vabs(sK79(vbind(sK87, sK88, sK89), sK91, vvar(sK90)), sK81(vbind(sK87, sK88, sK89), sK91, vvar(sK90)), sK80(vbind(sK87, sK88, sK89), sK91, vvar(sK90)))) | ~ spl92_2), inference(resolution, [], [f1324, f421])).
fof(f421, plain, ! [X2, X0, X1] : (~ sP12(X0, X1, X2) | (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)), inference(cnf_transformation, [], [f275])).
fof(f275, plain, ! [X0, X1, X2] : ((vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)) | ~ sP12(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79, sK80, sK81, sK82])], [f273, f274])).
fof(f274, plain, ! [X2, X1, X0] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) => (vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2))), introduced(choice_axiom, [])).
fof(f273, plain, ! [X0, X1, X2] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) | ~ sP12(X0, X1, X2)), inference(rectify, [], [f272])).
fof(f272, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(nnf_transformation, [], [f208])).
fof(f1324, plain, (sP12(vbind(sK87, sK88, sK89), sK91, vvar(sK90)) | ~ spl92_2), inference(avatar_component_clause, [], [f1322])).
fof(f1325, plain, (spl92_1 | spl92_2), inference(avatar_split_clause, [], [f1316, f1322, f1318])).
fof(f1316, plain, (sP12(vbind(sK87, sK88, sK89), sK91, vvar(sK90)) | (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89))))), inference(subsumption_resolution, [], [f1310, f292])).
fof(f1310, plain, (sP12(vbind(sK87, sK88, sK89), sK91, vvar(sK90)) | (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, vbind(sK87, sK88, sK89)))) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, vbind(sK87, sK88, sK89)), sK84(vvar(sK90), sK91, vbind(sK87, sK88, sK89))))), inference(resolution, [], [f424, f432])).
fof(f424, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vvar(sK86(X0, X1, X2)) = X0) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f279])).