fof(f6066, plain, $false, inference(avatar_sat_refutation, [], [f1006, f1050, f1054, f6049])).
fof(f6049, plain, ~ spl92_13, inference(avatar_contradiction_clause, [], [f6048])).
fof(f6048, plain, ($false | ~ spl92_13), inference(subsumption_resolution, [], [f6047, f310])).
fof(f310, plain, ! [X0] : ~ (vnoType = vsomeType(X0)), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ! [X0] : ~ (vnoType = vsomeType(X0)), inference(rectify, [], [f18])).
fof(f18, plain, ! [X2] : ~ (vnoType = vsomeType(X2)), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', 'DIFF-noType-someType')).
fof(f6047, plain, ((vnoType = vsomeType(sK91)) | ~ spl92_13), inference(forward_demodulation, [], [f6025, f426])).
fof(f426, plain, (vnoType = vlookup(sK87, sK89)), inference(cnf_transformation, [], [f277])).
fof(f277, plain, (~ vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91) & vtcheck(sK89, vvar(sK90), sK91) & (vnoType = vlookup(sK87, sK89))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87, sK88, sK89, sK90, sK91])], [f188, f276])).
fof(f276, plain, (? [X0, X1, X2, X3, X4] : (~ vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & vtcheck(X2, vvar(X3), X4) & (vnoType = vlookup(X0, X2))) => (~ vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91) & vtcheck(sK89, vvar(sK90), sK91) & (vnoType = vlookup(sK87, sK89)))), introduced(choice_axiom, [])).
fof(f188, plain, ? [X0, X1, X2, X3, X4] : (~ vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & vtcheck(X2, vvar(X3), X4) & (vnoType = vlookup(X0, X2))), inference(flattening, [], [f187])).
fof(f187, plain, ? [X0, X1, X2, X3, X4] : (~ vtcheck(vbind(X0, X1, X2), vvar(X3), X4) & (vtcheck(X2, vvar(X3), X4) & (vnoType = vlookup(X0, X2)))), inference(ennf_transformation, [], [f109])).
fof(f109, plain, ~ ! [X0, X1, X2, X3, X4] : ((vtcheck(X2, vvar(X3), X4) & (vnoType = vlookup(X0, X2))) => vtcheck(vbind(X0, X1, X2), vvar(X3), X4)), inference(rectify, [], [f56])).
fof(f56, plain, ~ ! [X8, X9, X19, X20, X14] : ((vtcheck(X19, vvar(X20), X14) & (vnoType = vlookup(X8, X19))) => vtcheck(vbind(X8, X9, X19), vvar(X20), X14)), inference(negated_conjecture, [], [f55])).
fof(f55, plain, ~ ! [X8, X9, X19, X20, X14] : ((vtcheck(X19, vvar(X20), X14) & (vnoType = vlookup(X8, X19))) => vtcheck(vbind(X8, X9, X19), vvar(X20), X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', 'T-Weak-var')).
fof(f6025, plain, ((vlookup(sK87, sK89) = vsomeType(sK91)) | ~ spl92_13), inference(backward_demodulation, [], [f1049, f6016])).
fof(f6016, plain, ((sK87 = sK90) | ~ spl92_13), inference(resolution, [], [f5796, f428])).
fof(f428, plain, ~ vtcheck(vbind(sK87, sK88, sK89), vvar(sK90), sK91), inference(cnf_transformation, [], [f277])).
fof(f5796, plain, (! [X0, X1] : (vtcheck(vbind(X0, X1, sK89), vvar(sK90), sK91) | (sK90 = X0)) | ~ spl92_13), inference(equality_resolution, [], [f1598])).
fof(f1598, plain, (! [X26, X24, X25] : (~ (vsomeType(sK91) = vsomeType(X24)) | vtcheck(vbind(X25, X26, sK89), vvar(sK90), X24) | (sK90 = X25)) | ~ spl92_13), inference(superposition, [], [f632, f1049])).
fof(f632, plain, ! [X6, X4, X7, X5, X3] : (~ (vlookup(X3, X6) = vsomeType(X7)) | vtcheck(vbind(X4, X5, X6), vvar(X3), X7) | (X3 = X4)), inference(superposition, [], [f414, f473])).
fof(f473, plain, ! [X6, X0, X5, X1] : ((vlookup(X5, X6) = vlookup(X5, vbind(X1, X0, X6))) | (X1 = X5)), inference(equality_resolution, [], [f472])).
fof(f472, plain, ! [X6, X2, X0, X5, X1] : ((vlookup(X5, X6) = vlookup(X2, vbind(X1, X0, X6))) | (X1 = X5) | ~ (X2 = X5)), inference(equality_resolution, [], [f471])).
fof(f471, plain, ! [X6, X2, X0, X5, X3, X1] : ((vlookup(X2, X3) = vlookup(X5, X6)) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(equality_resolution, [], [f316])).
fof(f316, plain, ! [X6, X4, X2, X0, X5, X3, X1] : ((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4) | (X1 = X5) | ~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5)), inference(flattening, [], [f135])).
fof(f135, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((((vlookup(X5, X6) = X4) | ~ (vlookup(X2, X3) = X4)) | (X1 = X5)) | (~ (vbind(X1, X0, X6) = X3) | ~ (X2 = X5))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1, X2, X3, X4, X5, X6] : (((vbind(X1, X0, X6) = X3) & (X2 = X5)) => (~ (X1 = X5) => ((vlookup(X2, X3) = X4) => (vlookup(X5, X6) = X4)))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X21, X20, X0, X15, X18, X8, X19] : (((vbind(X20, X21, X19) = X15) & (X0 = X8)) => (~ (X8 = X20) => ((vlookup(X0, X15) = X18) => (vlookup(X8, X19) = X18)))), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', lookup2)).
fof(f414, plain, ! [X2, X0, X1] : (~ (vsomeType(X2) = vlookup(X1, X0)) | vtcheck(X0, vvar(X1), X2)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0, X1, X2] : (vtcheck(X0, vvar(X1), X2) | ~ (vsomeType(X2) = vlookup(X1, X0))), inference(ennf_transformation, [], [f105])).
fof(f105, plain, ! [X0, X1, X2] : ((vsomeType(X2) = vlookup(X1, X0)) => vtcheck(X0, vvar(X1), X2)), inference(rectify, [], [f51])).
fof(f51, plain, ! [X19, X8, X14] : ((vlookup(X8, X19) = vsomeType(X14)) => vtcheck(X19, vvar(X8), X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', 'T-var')).
fof(f1049, plain, ((vsomeType(sK91) = vlookup(sK90, sK89)) | ~ spl92_13), inference(avatar_component_clause, [], [f1047])).
fof(f1047, plain, (spl92_13 <=> (vsomeType(sK91) = vlookup(sK90, sK89))), introduced(avatar_definition, [new_symbols(naming, [spl92_13])])).
fof(f1054, plain, ~ spl92_12, inference(avatar_contradiction_clause, [], [f1053])).
fof(f1053, plain, ($false | ~ spl92_12), inference(subsumption_resolution, [], [f1051, f287])).
fof(f287, plain, ! [X2, X0, X3, X1] : ~ (vvar(X0) = vabs(X1, X2, X3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1, X2, X3] : ~ (vvar(X0) = vabs(X1, X2, X3)), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', 'DIFF-var-abs')).
fof(f1051, plain, ((vvar(sK90) = vabs(sK79(sK89, sK91, vvar(sK90)), sK81(sK89, sK91, vvar(sK90)), sK80(sK89, sK91, vvar(sK90)))) | ~ spl92_12), inference(resolution, [], [f1005, f417])).
fof(f417, plain, ! [X2, X0, X1] : (~ sP12(X0, X1, X2) | (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)), inference(cnf_transformation, [], [f271])).
fof(f271, plain, ! [X0, X1, X2] : ((vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)) | ~ sP12(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79, sK80, sK81, sK82])], [f269, f270])).
fof(f270, plain, ! [X2, X1, X0] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) => (vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2))), introduced(choice_axiom, [])).
fof(f269, plain, ! [X0, X1, X2] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) | ~ sP12(X0, X1, X2)), inference(rectify, [], [f268])).
fof(f268, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(nnf_transformation, [], [f204])).
fof(f204, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(usedef, [], [e204])).
fof(e204, plain, ! [X2, X1, X0] : (sP12(X2, X1, X0) <=> ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f1005, plain, (sP12(sK89, sK91, vvar(sK90)) | ~ spl92_12), inference(avatar_component_clause, [], [f1003])).
fof(f1003, plain, (spl92_12 <=> sP12(sK89, sK91, vvar(sK90))), introduced(avatar_definition, [new_symbols(naming, [spl92_12])])).
fof(f1050, plain, (spl92_12 | spl92_13 | ~ spl92_11), inference(avatar_split_clause, [], [f1045, f999, f1047, f1003])).
fof(f999, plain, (spl92_11 <=> (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89)))), introduced(avatar_definition, [new_symbols(naming, [spl92_11])])).
fof(f1045, plain, ((vsomeType(sK91) = vlookup(sK90, sK89)) | sP12(sK89, sK91, vvar(sK90)) | ~ spl92_11), inference(forward_demodulation, [], [f1044, f1032])).
fof(f1032, plain, ((sK90 = sK86(vvar(sK90), sK91, sK89)) | ~ spl92_11), inference(resolution, [], [f1017, f439])).
fof(f439, plain, ! [X2, X3] : (~ visFreeVar(X3, vvar(X2)) | (X2 = X3)), inference(equality_resolution, [], [f438])).
fof(f438, plain, ! [X2, X0, X3] : ((X2 = X3) | ~ visFreeVar(X0, vvar(X2)) | ~ (X0 = X3)), inference(equality_resolution, [], [f294])).
fof(f294, plain, ! [X2, X0, X3, X1] : ((X2 = X3) | ~ visFreeVar(X0, X1) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(flattening, [], [f118])).
fof(f118, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | (~ (vvar(X2) = X1) | ~ (X0 = X3))), inference(ennf_transformation, [], [f64])).
fof(f64, plain, ! [X0, X1, X2, X3] : (((vvar(X2) = X1) & (X0 = X3)) => ((visFreeVar(X0, X1) => (X2 = X3)) & ((X2 = X3) => visFreeVar(X0, X1)))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X0, X3, X8, X13] : (((vvar(X8) = X3) & (X0 = X13)) => ((visFreeVar(X0, X3) => (X8 = X13)) & ((X8 = X13) => visFreeVar(X0, X3)))), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', isFreeVar0)).
fof(f1017, plain, (visFreeVar(sK86(vvar(sK90), sK91, sK89), vvar(sK90)) | ~ spl92_11), inference(superposition, [], [f442, f1001])).
fof(f1001, plain, ((vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89))) | ~ spl92_11), inference(avatar_component_clause, [], [f999])).
fof(f442, plain, ! [X3] : visFreeVar(X3, vvar(X3)), inference(equality_resolution, [], [f441])).
fof(f441, plain, ! [X0, X3] : (visFreeVar(X0, vvar(X3)) | ~ (X0 = X3)), inference(equality_resolution, [], [f440])).
fof(f440, plain, ! [X0, X3, X1] : (visFreeVar(X0, X1) | ~ (vvar(X3) = X1) | ~ (X0 = X3)), inference(equality_resolution, [], [f293])).
fof(f293, plain, ! [X2, X0, X3, X1] : (visFreeVar(X0, X1) | ~ (X2 = X3) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f119])).
fof(f1044, plain, (sP12(sK89, sK91, vvar(sK90)) | (vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, sK89), sK89))), inference(subsumption_resolution, [], [f1043, f288])).
fof(f288, plain, ! [X2, X0, X1] : ~ (vvar(X0) = vapp(X1, X2)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0, X1, X2] : ~ (vvar(X0) = vapp(X1, X2)), inference(rectify, [], [f5])).
fof(f5, plain, ! [X0, X3, X5] : ~ (vvar(X0) = vapp(X3, X5)), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', 'DIFF-var-app')).
fof(f1043, plain, (sP12(sK89, sK91, vvar(sK90)) | (vsomeType(sK91) = vlookup(sK86(vvar(sK90), sK91, sK89), sK89)) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, sK89), sK84(vvar(sK90), sK91, sK89)))), inference(resolution, [], [f421, f427])).
fof(f427, plain, vtcheck(sK89, vvar(sK90), sK91), inference(cnf_transformation, [], [f277])).
fof(f421, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f275])).
fof(f275, plain, ! [X0, X1, X2] : ((vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)) | sP12(X2, X1, X0) | ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0)) | ~ vtcheck(X2, X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK83, sK84, sK85, sK86])], [f272, f274, f273])).
fof(f273, plain, ! [X2, X1, X0] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) => (vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f274, plain, ! [X2, X1, X0] : (? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) => ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f272, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) | ~ vtcheck(X2, X0, X1)), inference(rectify, [], [f205])).
fof(f205, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(definition_folding, [], [f186, e204])).
fof(f186, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(flattening, [], [f185])).
fof(f185, plain, ! [X0, X1, X2] : ((? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0))) | ~ vtcheck(X2, X0, X1)), inference(ennf_transformation, [], [f108])).
fof(f108, plain, ! [X0, X1, X2] : (vtcheck(X2, X0, X1) => (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)))), inference(rectify, [], [f54])).
fof(f54, plain, ! [X10, X14, X19] : (vtcheck(X19, X10, X14) => (? [X11, X12, X9] : (vtcheck(X19, X12, X9) & vtcheck(X19, X11, varrow(X9, X14)) & (vapp(X11, X12) = X10)) | ? [X8, X12, X32, X33] : (vtcheck(vbind(X8, X32, X19), X12, X33) & (varrow(X32, X33) = X14) & (vabs(X8, X32, X12) = X10)) | ? [X8] : ((vlookup(X8, X19) = vsomeType(X14)) & (vvar(X8) = X10)))), file('/home/ubuntu/library/tptp/Problems/COM/COM143+1.p', 'T-inv')).
fof(f1006, plain, (spl92_11 | spl92_12), inference(avatar_split_clause, [], [f997, f1003, f999])).
fof(f997, plain, (sP12(sK89, sK91, vvar(sK90)) | (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89)))), inference(subsumption_resolution, [], [f994, f288])).
fof(f994, plain, (sP12(sK89, sK91, vvar(sK90)) | (vvar(sK90) = vvar(sK86(vvar(sK90), sK91, sK89))) | (vvar(sK90) = vapp(sK83(vvar(sK90), sK91, sK89), sK84(vvar(sK90), sK91, sK89)))), inference(resolution, [], [f420, f427])).
fof(f420, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vvar(sK86(X0, X1, X2)) = X0) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f275])).