fof(f1997, plain, $false, inference(avatar_sat_refutation, [], [f612, f663, f1379, f1465, f1516, f1919, f1996])).
fof(f1996, plain, (~ spl93_6 | spl93_9), inference(avatar_contradiction_clause, [], [f1995])).
fof(f1995, plain, ($false | (~ spl93_6 | spl93_9)), inference(subsumption_resolution, [], [f1993, f1856])).
fof(f1856, plain, (~ (sK89 = sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88))) | spl93_9), inference(avatar_component_clause, [], [f1855])).
fof(f1855, plain, (spl93_9 <=> (sK89 = sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)))), introduced(avatar_definition, [new_symbols(naming, [spl93_9])])).
fof(f1993, plain, ((sK89 = sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88))) | ~ spl93_6), inference(resolution, [], [f1568, f454])).
fof(f454, plain, ! [X2, X3] : (~ visFreeVar(X3, vvar(X2)) | (X2 = X3)), inference(equality_resolution, [], [f453])).
fof(f453, plain, ! [X2, X0, X3] : ((X2 = X3) | ~ visFreeVar(X0, vvar(X2)) | ~ (X0 = X3)), inference(equality_resolution, [], [f306])).
fof(f306, plain, ! [X2, X0, X3, X1] : ((X2 = X3) | ~ visFreeVar(X0, X1) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0, X1, X2, X3] : ((((X2 = X3) | ~ visFreeVar(X0, X1)) & (visFreeVar(X0, X1) | ~ (X2 = X3))) | (~ (vvar(X2) = X1) | ~ (X0 = X3))), inference(ennf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1, X2, X3] : (((vvar(X2) = X1) & (X0 = X3)) => ((visFreeVar(X0, X1) => (X2 = X3)) & ((X2 = X3) => visFreeVar(X0, X1)))), inference(rectify, [], [f10])).
fof(f10, plain, ! [X0, X3, X8, X13] : (((vvar(X8) = X3) & (X0 = X13)) => ((visFreeVar(X0, X3) => (X8 = X13)) & ((X8 = X13) => visFreeVar(X0, X3)))), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', isFreeVar0)).
fof(f1568, plain, (visFreeVar(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), vvar(sK89)) | ~ spl93_6), inference(superposition, [], [f457, f1374])).
fof(f1374, plain, ((vvar(sK89) = vvar(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)))) | ~ spl93_6), inference(avatar_component_clause, [], [f1372])).
fof(f1372, plain, (spl93_6 <=> (vvar(sK89) = vvar(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88))))), introduced(avatar_definition, [new_symbols(naming, [spl93_6])])).
fof(f457, plain, ! [X3] : visFreeVar(X3, vvar(X3)), inference(equality_resolution, [], [f456])).
fof(f456, plain, ! [X0, X3] : (visFreeVar(X0, vvar(X3)) | ~ (X0 = X3)), inference(equality_resolution, [], [f455])).
fof(f455, plain, ! [X0, X3, X1] : (visFreeVar(X0, X1) | ~ (vvar(X3) = X1) | ~ (X0 = X3)), inference(equality_resolution, [], [f305])).
fof(f305, plain, ! [X2, X0, X3, X1] : (visFreeVar(X0, X1) | ~ (X2 = X3) | ~ (vvar(X2) = X1) | ~ (X0 = X3)), inference(cnf_transformation, [], [f125])).
fof(f1919, plain, (~ spl93_1 | ~ spl93_8 | ~ spl93_9), inference(avatar_contradiction_clause, [], [f1918])).
fof(f1918, plain, ($false | (~ spl93_1 | ~ spl93_8 | ~ spl93_9)), inference(subsumption_resolution, [], [f1912, f441])).
fof(f441, plain, vtcheck(sK88, sK90, sK87), inference(cnf_transformation, [], [f289])).
fof(f289, plain, (~ vtcheck(sK88, vsubst(sK89, sK90, vvar(sK91)), sK92) & vtcheck(vbind(sK89, sK87, sK88), vvar(sK91), sK92) & vtcheck(sK88, sK90, sK87)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87, sK88, sK89, sK90, sK91, sK92])], [f200, f288])).
fof(f288, plain, (? [X0, X1, X2, X3, X4, X5] : (~ vtcheck(X1, vsubst(X2, X3, vvar(X4)), X5) & vtcheck(vbind(X2, X0, X1), vvar(X4), X5) & vtcheck(X1, X3, X0)) => (~ vtcheck(sK88, vsubst(sK89, sK90, vvar(sK91)), sK92) & vtcheck(vbind(sK89, sK87, sK88), vvar(sK91), sK92) & vtcheck(sK88, sK90, sK87))), introduced(choice_axiom, [])).
fof(f200, plain, ? [X0, X1, X2, X3, X4, X5] : (~ vtcheck(X1, vsubst(X2, X3, vvar(X4)), X5) & vtcheck(vbind(X2, X0, X1), vvar(X4), X5) & vtcheck(X1, X3, X0)), inference(flattening, [], [f199])).
fof(f199, plain, ? [X0, X1, X2, X3, X4, X5] : (~ vtcheck(X1, vsubst(X2, X3, vvar(X4)), X5) & (vtcheck(vbind(X2, X0, X1), vvar(X4), X5) & vtcheck(X1, X3, X0))), inference(ennf_transformation, [], [f115])).
fof(f115, plain, ~ ! [X0, X1, X2, X3, X4, X5] : ((vtcheck(vbind(X2, X0, X1), vvar(X4), X5) & vtcheck(X1, X3, X0)) => vtcheck(X1, vsubst(X2, X3, vvar(X4)), X5)), inference(rectify, [], [f59])).
fof(f59, plain, ~ ! [X14, X19, X8, X10, X20, X33] : ((vtcheck(vbind(X8, X14, X19), vvar(X20), X33) & vtcheck(X19, X10, X14)) => vtcheck(X19, vsubst(X8, X10, vvar(X20)), X33)), inference(negated_conjecture, [], [f58])).
fof(f58, plain, ~ ! [X14, X19, X8, X10, X20, X33] : ((vtcheck(vbind(X8, X14, X19), vvar(X20), X33) & vtcheck(X19, X10, X14)) => vtcheck(X19, vsubst(X8, X10, vvar(X20)), X33)), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', 'T-subst-var')).
fof(f1912, plain, (~ vtcheck(sK88, sK90, sK87) | (~ spl93_1 | ~ spl93_8 | ~ spl93_9)), inference(backward_demodulation, [], [f668, f1909])).
fof(f1909, plain, ((sK87 = sK92) | (~ spl93_8 | ~ spl93_9)), inference(forward_demodulation, [], [f1906, f478])).
fof(f478, plain, ! [X2] : (vgetSomeType(vsomeType(X2)) = X2), inference(equality_resolution, [], [f477])).
fof(f477, plain, ! [X2, X0] : ((vgetSomeType(X0) = X2) | ~ (vsomeType(X2) = X0)), inference(equality_resolution, [], [f325])).
fof(f325, plain, ! [X2, X0, X1] : ((X1 = X2) | ~ (vgetSomeType(X0) = X1) | ~ (vsomeType(X2) = X0)), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ (vgetSomeType(X0) = X1) | ~ (vsomeType(X2) = X0)), inference(flattening, [], [f135])).
fof(f135, plain, ! [X0, X1, X2] : (((X1 = X2) | ~ (vgetSomeType(X0) = X1)) | ~ (vsomeType(X2) = X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1, X2] : ((vsomeType(X2) = X0) => ((vgetSomeType(X0) = X1) => (X1 = X2))), inference(rectify, [], [f21])).
fof(f21, plain, ! [X17, X18, X10] : ((vsomeType(X10) = X17) => ((vgetSomeType(X17) = X18) => (X10 = X18))), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', getSomeType0)).
fof(f1906, plain, ((sK92 = vgetSomeType(vsomeType(sK87))) | (~ spl93_8 | ~ spl93_9)), inference(superposition, [], [f478, f1899])).
fof(f1899, plain, ((vsomeType(sK87) = vsomeType(sK92)) | (~ spl93_8 | ~ spl93_9)), inference(forward_demodulation, [], [f1882, f485])).
fof(f485, plain, ! [X6, X0, X3] : (vsomeType(X6) = vlookup(X3, vbind(X3, X6, X0))), inference(equality_resolution, [], [f484])).
fof(f484, plain, ! [X6, X2, X0, X3] : ((vsomeType(X6) = vlookup(X3, vbind(X2, X6, X0))) | ~ (X2 = X3)), inference(equality_resolution, [], [f483])).
fof(f483, plain, ! [X6, X4, X2, X0, X3] : ((vlookup(X3, X4) = vsomeType(X6)) | ~ (vbind(X2, X6, X0) = X4) | ~ (X2 = X3)), inference(equality_resolution, [], [f482])).
fof(f482, plain, ! [X6, X4, X2, X0, X3, X1] : ((vlookup(X3, X4) = vsomeType(X6)) | ~ (X1 = X2) | ~ (vbind(X2, X6, X0) = X4) | ~ (X1 = X3)), inference(equality_resolution, [], [f327])).
fof(f327, plain, ! [X6, X4, X2, X0, X5, X3, X1] : ((vsomeType(X6) = X5) | ~ (vlookup(X3, X4) = X5) | ~ (X1 = X2) | ~ (vbind(X2, X6, X0) = X4) | ~ (X1 = X3)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((vsomeType(X6) = X5) | ~ (vlookup(X3, X4) = X5) | ~ (X1 = X2) | ~ (vbind(X2, X6, X0) = X4) | ~ (X1 = X3)), inference(flattening, [], [f139])).
fof(f139, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((((vsomeType(X6) = X5) | ~ (vlookup(X3, X4) = X5)) | ~ (X1 = X2)) | (~ (vbind(X2, X6, X0) = X4) | ~ (X1 = X3))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0, X1, X2, X3, X4, X5, X6] : (((vbind(X2, X6, X0) = X4) & (X1 = X3)) => ((X1 = X2) => ((vlookup(X3, X4) = X5) => (vsomeType(X6) = X5)))), inference(rectify, [], [f23])).
fof(f23, plain, ! [X19, X8, X20, X0, X15, X18, X21] : (((vbind(X20, X21, X19) = X15) & (X0 = X8)) => ((X8 = X20) => ((vlookup(X0, X15) = X18) => (vsomeType(X21) = X18)))), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', lookup1)).
fof(f1882, plain, ((vsomeType(sK92) = vlookup(sK89, vbind(sK89, sK87, sK88))) | (~ spl93_8 | ~ spl93_9)), inference(backward_demodulation, [], [f1464, f1857])).
fof(f1857, plain, ((sK89 = sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88))) | ~ spl93_9), inference(avatar_component_clause, [], [f1855])).
fof(f1464, plain, ((vsomeType(sK92) = vlookup(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), vbind(sK89, sK87, sK88))) | ~ spl93_8), inference(avatar_component_clause, [], [f1462])).
fof(f1462, plain, (spl93_8 <=> (vsomeType(sK92) = vlookup(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), vbind(sK89, sK87, sK88)))), introduced(avatar_definition, [new_symbols(naming, [spl93_8])])).
fof(f668, plain, (~ vtcheck(sK88, sK90, sK92) | ~ spl93_1), inference(forward_demodulation, [], [f667, f498])).
fof(f498, plain, ! [X6, X2] : (vsubst(X2, X6, vvar(X2)) = X6), inference(equality_resolution, [], [f497])).
fof(f497, plain, ! [X6, X2, X1] : ((vsubst(X2, X6, vvar(X1)) = X6) | ~ (X1 = X2)), inference(equality_resolution, [], [f496])).
fof(f496, plain, ! [X6, X2, X3, X1] : ((vsubst(X2, X3, vvar(X1)) = X6) | ~ (X3 = X6) | ~ (X1 = X2)), inference(equality_resolution, [], [f495])).
fof(f495, plain, ! [X6, X4, X2, X3, X1] : ((vsubst(X2, X3, X4) = X6) | ~ (vvar(X1) = X4) | ~ (X3 = X6) | ~ (X1 = X2)), inference(equality_resolution, [], [f494])).
fof(f494, plain, ! [X6, X4, X2, X0, X3, X1] : ((vsubst(X2, X3, X4) = X6) | ~ (X0 = X1) | ~ (vvar(X1) = X4) | ~ (X3 = X6) | ~ (X0 = X2)), inference(equality_resolution, [], [f343])).
fof(f343, plain, ! [X6, X4, X2, X0, X5, X3, X1] : ((X5 = X6) | ~ (vsubst(X2, X3, X4) = X5) | ~ (X0 = X1) | ~ (vvar(X1) = X4) | ~ (X3 = X6) | ~ (X0 = X2)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((X5 = X6) | ~ (vsubst(X2, X3, X4) = X5) | ~ (X0 = X1) | ~ (vvar(X1) = X4) | ~ (X3 = X6) | ~ (X0 = X2)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((((X5 = X6) | ~ (vsubst(X2, X3, X4) = X5)) | ~ (X0 = X1)) | (~ (vvar(X1) = X4) | ~ (X3 = X6) | ~ (X0 = X2))), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0, X1, X2, X3, X4, X5, X6] : (((vvar(X1) = X4) & (X3 = X6) & (X0 = X2)) => ((X0 = X1) => ((vsubst(X2, X3, X4) = X5) => (X5 = X6)))), inference(rectify, [], [f29])).
fof(f29, plain, ! [X8, X20, X0, X3, X5, X18, X10] : (((vvar(X20) = X5) & (X3 = X10) & (X0 = X8)) => ((X8 = X20) => ((vsubst(X0, X3, X5) = X18) => (X10 = X18)))), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', subst0)).
fof(f667, plain, (~ vtcheck(sK88, vsubst(sK89, sK90, vvar(sK89)), sK92) | ~ spl93_1), inference(backward_demodulation, [], [f443, f607])).
fof(f607, plain, ((sK89 = sK91) | ~ spl93_1), inference(avatar_component_clause, [], [f605])).
fof(f605, plain, (spl93_1 <=> (sK89 = sK91)), introduced(avatar_definition, [new_symbols(naming, [spl93_1])])).
fof(f443, plain, ~ vtcheck(sK88, vsubst(sK89, sK90, vvar(sK91)), sK92), inference(cnf_transformation, [], [f289])).
fof(f1516, plain, ~ spl93_7, inference(avatar_contradiction_clause, [], [f1515])).
fof(f1515, plain, ($false | ~ spl93_7), inference(subsumption_resolution, [], [f1513, f299])).
fof(f299, plain, ! [X2, X0, X3, X1] : ~ (vvar(X0) = vabs(X1, X2, X3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1, X2, X3] : ~ (vvar(X0) = vabs(X1, X2, X3)), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', 'DIFF-var-abs')).
fof(f1513, plain, ((vvar(sK89) = vabs(sK79(vbind(sK89, sK87, sK88), sK92, vvar(sK89)), sK81(vbind(sK89, sK87, sK88), sK92, vvar(sK89)), sK80(vbind(sK89, sK87, sK88), sK92, vvar(sK89)))) | ~ spl93_7), inference(resolution, [], [f1378, f429])).
fof(f429, plain, ! [X2, X0, X1] : (~ sP12(X0, X1, X2) | (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)), inference(cnf_transformation, [], [f283])).
fof(f283, plain, ! [X0, X1, X2] : ((vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)) | ~ sP12(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79, sK80, sK81, sK82])], [f281, f282])).
fof(f282, plain, ! [X2, X1, X0] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) => (vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2))), introduced(choice_axiom, [])).
fof(f281, plain, ! [X0, X1, X2] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) | ~ sP12(X0, X1, X2)), inference(rectify, [], [f280])).
fof(f280, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(nnf_transformation, [], [f216])).
fof(f216, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(usedef, [], [e216])).
fof(e216, plain, ! [X2, X1, X0] : (sP12(X2, X1, X0) <=> ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f1378, plain, (sP12(vbind(sK89, sK87, sK88), sK92, vvar(sK89)) | ~ spl93_7), inference(avatar_component_clause, [], [f1376])).
fof(f1376, plain, (spl93_7 <=> sP12(vbind(sK89, sK87, sK88), sK92, vvar(sK89))), introduced(avatar_definition, [new_symbols(naming, [spl93_7])])).
fof(f1465, plain, (spl93_8 | spl93_7 | ~ spl93_1), inference(avatar_split_clause, [], [f1460, f605, f1376, f1462])).
fof(f1460, plain, (sP12(vbind(sK89, sK87, sK88), sK92, vvar(sK89)) | (vsomeType(sK92) = vlookup(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), vbind(sK89, sK87, sK88))) | ~ spl93_1), inference(subsumption_resolution, [], [f1453, f300])).
fof(f300, plain, ! [X2, X0, X1] : ~ (vvar(X0) = vapp(X1, X2)), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1, X2] : ~ (vvar(X0) = vapp(X1, X2)), inference(rectify, [], [f5])).
fof(f5, plain, ! [X0, X3, X5] : ~ (vvar(X0) = vapp(X3, X5)), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', 'DIFF-var-app')).
fof(f1453, plain, (sP12(vbind(sK89, sK87, sK88), sK92, vvar(sK89)) | (vsomeType(sK92) = vlookup(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), vbind(sK89, sK87, sK88))) | (vvar(sK89) = vapp(sK83(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), sK84(vvar(sK89), sK92, vbind(sK89, sK87, sK88)))) | ~ spl93_1), inference(resolution, [], [f433, f666])).
fof(f666, plain, (vtcheck(vbind(sK89, sK87, sK88), vvar(sK89), sK92) | ~ spl93_1), inference(backward_demodulation, [], [f442, f607])).
fof(f442, plain, vtcheck(vbind(sK89, sK87, sK88), vvar(sK91), sK92), inference(cnf_transformation, [], [f289])).
fof(f433, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f287])).
fof(f287, plain, ! [X0, X1, X2] : ((vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)) | sP12(X2, X1, X0) | ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0)) | ~ vtcheck(X2, X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK83, sK84, sK85, sK86])], [f284, f286, f285])).
fof(f285, plain, ! [X2, X1, X0] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) => (vtcheck(X2, sK84(X0, X1, X2), sK85(X0, X1, X2)) & vtcheck(X2, sK83(X0, X1, X2), varrow(sK85(X0, X1, X2), X1)) & (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f286, plain, ! [X2, X1, X0] : (? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) => ((vsomeType(X1) = vlookup(sK86(X0, X1, X2), X2)) & (vvar(sK86(X0, X1, X2)) = X0))), introduced(choice_axiom, [])).
fof(f284, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X6] : ((vsomeType(X1) = vlookup(X6, X2)) & (vvar(X6) = X0)) | ~ vtcheck(X2, X0, X1)), inference(rectify, [], [f217])).
fof(f217, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | sP12(X2, X1, X0) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(definition_folding, [], [f192, e216])).
fof(f192, plain, ! [X0, X1, X2] : (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)) | ~ vtcheck(X2, X0, X1)), inference(flattening, [], [f191])).
fof(f191, plain, ! [X0, X1, X2] : ((? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0))) | ~ vtcheck(X2, X0, X1)), inference(ennf_transformation, [], [f111])).
fof(f111, plain, ! [X0, X1, X2] : (vtcheck(X2, X0, X1) => (? [X3, X4, X5] : (vtcheck(X2, X4, X5) & vtcheck(X2, X3, varrow(X5, X1)) & (vapp(X3, X4) = X0)) | ? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ? [X10] : ((vsomeType(X1) = vlookup(X10, X2)) & (vvar(X10) = X0)))), inference(rectify, [], [f54])).
fof(f54, plain, ! [X10, X14, X19] : (vtcheck(X19, X10, X14) => (? [X11, X12, X9] : (vtcheck(X19, X12, X9) & vtcheck(X19, X11, varrow(X9, X14)) & (vapp(X11, X12) = X10)) | ? [X8, X12, X32, X33] : (vtcheck(vbind(X8, X32, X19), X12, X33) & (varrow(X32, X33) = X14) & (vabs(X8, X32, X12) = X10)) | ? [X8] : ((vlookup(X8, X19) = vsomeType(X14)) & (vvar(X8) = X10)))), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', 'T-inv')).
fof(f1379, plain, (spl93_6 | spl93_7 | ~ spl93_1), inference(avatar_split_clause, [], [f1370, f605, f1376, f1372])).
fof(f1370, plain, (sP12(vbind(sK89, sK87, sK88), sK92, vvar(sK89)) | (vvar(sK89) = vvar(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)))) | ~ spl93_1), inference(subsumption_resolution, [], [f1352, f300])).
fof(f1352, plain, (sP12(vbind(sK89, sK87, sK88), sK92, vvar(sK89)) | (vvar(sK89) = vvar(sK86(vvar(sK89), sK92, vbind(sK89, sK87, sK88)))) | (vvar(sK89) = vapp(sK83(vvar(sK89), sK92, vbind(sK89, sK87, sK88)), sK84(vvar(sK89), sK92, vbind(sK89, sK87, sK88)))) | ~ spl93_1), inference(resolution, [], [f432, f666])).
fof(f432, plain, ! [X2, X0, X1] : (~ vtcheck(X2, X0, X1) | sP12(X2, X1, X0) | (vvar(sK86(X0, X1, X2)) = X0) | (vapp(sK83(X0, X1, X2), sK84(X0, X1, X2)) = X0)), inference(cnf_transformation, [], [f287])).
fof(f663, plain, (spl93_1 | spl93_2), inference(avatar_split_clause, [], [f662, f609, f605])).
fof(f609, plain, (spl93_2 <=> vtcheck(sK88, vvar(sK91), sK92)), introduced(avatar_definition, [new_symbols(naming, [spl93_2])])).
fof(f662, plain, ((sK89 = sK91) | spl93_2), inference(resolution, [], [f661, f454])).
fof(f661, plain, (visFreeVar(sK89, vvar(sK91)) | spl93_2), inference(subsumption_resolution, [], [f660, f611])).
fof(f611, plain, (~ vtcheck(sK88, vvar(sK91), sK92) | spl93_2), inference(avatar_component_clause, [], [f609])).
fof(f660, plain, (vtcheck(sK88, vvar(sK91), sK92) | visFreeVar(sK89, vvar(sK91))), inference(resolution, [], [f439, f442])).
fof(f439, plain, ! [X4, X2, X0, X3, X1] : (~ vtcheck(vbind(X0, X1, X2), X3, X4) | vtcheck(X2, X3, X4) | visFreeVar(X0, X3)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0, X1, X2, X3, X4] : (vtcheck(X2, X3, X4) | ~ vtcheck(vbind(X0, X1, X2), X3, X4) | visFreeVar(X0, X3)), inference(flattening, [], [f195])).
fof(f195, plain, ! [X0, X1, X2, X3, X4] : (vtcheck(X2, X3, X4) | (~ vtcheck(vbind(X0, X1, X2), X3, X4) | visFreeVar(X0, X3))), inference(ennf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1, X2, X3, X4] : ((vtcheck(vbind(X0, X1, X2), X3, X4) & ~ visFreeVar(X0, X3)) => vtcheck(X2, X3, X4)), inference(rectify, [], [f56])).
fof(f56, plain, ! [X8, X9, X19, X10, X14] : ((vtcheck(vbind(X8, X9, X19), X10, X14) & ~ visFreeVar(X8, X10)) => vtcheck(X19, X10, X14)), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', 'T-Strong')).
fof(f612, plain, (spl93_1 | ~ spl93_2), inference(avatar_split_clause, [], [f603, f609, f605])).
fof(f603, plain, (~ vtcheck(sK88, vvar(sK91), sK92) | (sK89 = sK91)), inference(superposition, [], [f443, f502])).
fof(f502, plain, ! [X6, X2, X3] : ((vvar(X6) = vsubst(X2, X3, vvar(X6))) | (X2 = X6)), inference(equality_resolution, [], [f501])).
fof(f501, plain, ! [X6, X2, X3, X1] : ((vvar(X6) = vsubst(X2, X3, vvar(X6))) | (X1 = X6) | ~ (X1 = X2)), inference(equality_resolution, [], [f500])).
fof(f500, plain, ! [X6, X2, X0, X3, X1] : ((vvar(X6) = vsubst(X2, X3, vvar(X6))) | (X1 = X6) | ~ (X0 = X3) | ~ (X1 = X2)), inference(equality_resolution, [], [f499])).
fof(f499, plain, ! [X6, X4, X2, X0, X3, X1] : ((vsubst(X2, X3, X4) = vvar(X6)) | (X1 = X6) | ~ (vvar(X6) = X4) | ~ (X0 = X3) | ~ (X1 = X2)), inference(equality_resolution, [], [f344])).
fof(f344, plain, ! [X6, X4, X2, X0, X5, X3, X1] : ((vvar(X6) = X5) | ~ (vsubst(X2, X3, X4) = X5) | (X1 = X6) | ~ (vvar(X6) = X4) | ~ (X0 = X3) | ~ (X1 = X2)), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((vvar(X6) = X5) | ~ (vsubst(X2, X3, X4) = X5) | (X1 = X6) | ~ (vvar(X6) = X4) | ~ (X0 = X3) | ~ (X1 = X2)), inference(flattening, [], [f152])).
fof(f152, plain, ! [X0, X1, X2, X3, X4, X5, X6] : ((((vvar(X6) = X5) | ~ (vsubst(X2, X3, X4) = X5)) | (X1 = X6)) | (~ (vvar(X6) = X4) | ~ (X0 = X3) | ~ (X1 = X2))), inference(ennf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1, X2, X3, X4, X5, X6] : (((vvar(X6) = X4) & (X0 = X3) & (X1 = X2)) => (~ (X1 = X6) => ((vsubst(X2, X3, X4) = X5) => (vvar(X6) = X5)))), inference(rectify, [], [f30])).
fof(f30, plain, ! [X10, X8, X0, X3, X5, X18, X20] : (((vvar(X20) = X5) & (X3 = X10) & (X0 = X8)) => (~ (X8 = X20) => ((vsubst(X0, X3, X5) = X18) => (vvar(X20) = X18)))), file('/home/ubuntu/library/tptp/Problems/COM/COM135+1.p', subst1)).