fof(f1409, plain, $false, inference(avatar_sat_refutation, [], [f1356, f1407])).
fof(f1407, plain, spl89_2, inference(avatar_contradiction_clause, [], [f1406])).
fof(f1406, plain, ($false | spl89_2), inference(subsumption_resolution, [], [f1405, f318])).
fof(f318, plain, ! [X0] : ~ (vnoType = vsomeType(X0)), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ~ (vnoType = vsomeType(X0)), inference(rectify, [], [f18])).
fof(f18, plain, ! [X2] : ~ (vnoType = vsomeType(X2)), file('/home/ubuntu/library/tptp/Problems/COM/COM149+1.p', 'DIFF-noType-someType')).
fof(f1405, plain, ((vnoType = vsomeType(sK87)) | spl89_2), inference(forward_demodulation, [], [f1404, f476])).
fof(f476, plain, ! [X1] : (vnoType = vlookup(X1, vempty)), inference(equality_resolution, [], [f475])).
fof(f475, plain, ! [X0, X1] : ((vnoType = vlookup(X1, vempty)) | ~ (X0 = X1)), inference(equality_resolution, [], [f474])).
fof(f474, plain, ! [X2, X0, X1] : ((vnoType = vlookup(X1, X2)) | ~ (vempty = X2) | ~ (X0 = X1)), inference(equality_resolution, [], [f322])).
fof(f322, plain, ! [X2, X0, X3, X1] : ((vnoType = X3) | ~ (vlookup(X1, X2) = X3) | ~ (vempty = X2) | ~ (X0 = X1)), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ! [X0, X1, X2, X3] : ((vnoType = X3) | ~ (vlookup(X1, X2) = X3) | ~ (vempty = X2) | ~ (X0 = X1)), inference(flattening, [], [f135])).
fof(f135, plain, ! [X0, X1, X2, X3] : (((vnoType = X3) | ~ (vlookup(X1, X2) = X3)) | (~ (vempty = X2) | ~ (X0 = X1))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1, X2, X3] : (((vempty = X2) & (X0 = X1)) => ((vlookup(X1, X2) = X3) => (vnoType = X3))), inference(rectify, [], [f22])).
fof(f22, plain, ! [X8, X0, X15, X18] : (((vempty = X15) & (X0 = X8)) => ((vlookup(X0, X15) = X18) => (vnoType = X18))), file('/home/ubuntu/library/tptp/Problems/COM/COM149+1.p', lookup0)).
fof(f1404, plain, ((vsomeType(sK87) = vlookup(sK86(vvar(sK88), sK87, vempty), vempty)) | spl89_2), inference(subsumption_resolution, [], [f1403, f296])).
fof(f296, plain, ! [X2, X0, X1] : ~ (vvar(X0) = vapp(X1, X2)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1, X2] : ~ (vvar(X0) = vapp(X1, X2)), inference(rectify, [], [f5])).
fof(f5, plain, ! [X0, X3, X5] : ~ (vvar(X0) = vapp(X3, X5)), file('/home/ubuntu/library/tptp/Problems/COM/COM149+1.p', 'DIFF-var-app')).
fof(f1403, plain, ((vsomeType(sK87) = vlookup(sK86(vvar(sK88), sK87, vempty), vempty)) | (vvar(sK88) = vapp(sK83(vvar(sK88), sK87, vempty), sK84(vvar(sK88), sK87, vempty))) | spl89_2), inference(subsumption_resolution, [], [f1396, f1347])).
fof(f1347, plain, (~ sP12(vempty, sK87, vvar(sK88)) | spl89_2), inference(avatar_component_clause, [], [f1346])).
fof(f1346, plain, (spl89_2 <=> sP12(vempty, sK87, vvar(sK88))), introduced(avatar_definition, [new_symbols(naming, [spl89_2])])).
fof(f1396, plain, (sP12(vempty, sK87, vvar(sK88)) | (vsomeType(sK87) = vlookup(sK86(vvar(sK88), sK87, vempty), vempty)) | (vvar(sK88) = vapp(sK83(vvar(sK88), sK87, vempty), sK84(vvar(sK88), sK87, vempty)))), inference(resolution, [], [f429, f436])).
fof(f436, plain, vtcheck(vempty, vvar(sK88), sK87), inference(cnf_transformation, [], [f285])).
fof(f285, plain, (! [X2] : ~ (vsomeExp(X2) = vreduce(vvar(sK88))) & ~ visValue(vvar(sK88)) & vtcheck(vempty, vvar(sK88), sK87)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87, sK88])], [f196, f284])).
fof(f284, plain, (? [X0, X1] : (! [X2] : ~ (vsomeExp(X2) = vreduce(vvar(X1))) & ~ visValue(vvar(X1)) & vtcheck(vempty, vvar(X1), X0)) => (! [X2] : ~ (vsomeExp(X2) = vreduce(vvar(sK88))) & ~ visValue(vvar(sK88)) & vtcheck(vempty, vvar(sK88), sK87))), introduced(choice_axiom, [])).
fof(f196, plain, ? [X0, X1] : (! [X2] : ~ (vsomeExp(X2) = vreduce(vvar(X1))) & ~ visValue(vvar(X1)) & vtcheck(vempty, vvar(X1), X0)), inference(flattening, [], [f195])).
fof(f195, plain, ? [X0, X1] : (! [X2] : ~ (vsomeExp(X2) = vreduce(vvar(X1))) & (~ visValue(vvar(X1)) & vtcheck(vempty, vvar(X1), X0))), inference(ennf_transformation, [], [f113])).
fof(f113, plain, ~ ! [X0, X1] : ((~ visValue(vvar(X1)) & vtcheck(vempty, vvar(X1), X0)) => ? [X2] : (vsomeExp(X2) = vreduce(vvar(X1)))), inference(rectify, [], [f58])).
fof(f58, plain, ~ ! [X14, X8] : ((~ visValue(vvar(X8)) & vtcheck(vempty, vvar(X8), X14)) => ? [X34] : (vreduce(vvar(X8)) = vsomeExp(X34))), inference(negated_conjecture, [], [f57])).
fof(f57, plain, ~ ! [X14, X8] : ((~ visValue(vvar(X8)) & vtcheck(vempty, vvar(X8), X14)) => ? [X34] : (vreduce(vvar(X8)) = vsomeExp(X34))), file('/home/ubuntu/library/tptp/Problems/COM/COM149+1.p', 'T-Progress-T-var')).
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
fof(f54, plain, ! [X10, X14, X19] : (vtcheck(X19, X10, X14) => (? [X11, X12, X9] : (vtcheck(X19, X12, X9) & vtcheck(X19, X11, varrow(X9, X14)) & (vapp(X11, X12) = X10)) | ? [X8, X12, X32, X33] : (vtcheck(vbind(X8, X32, X19), X12, X33) & (varrow(X32, X33) = X14) & (vabs(X8, X32, X12) = X10)) | ? [X8] : ((vlookup(X8, X19) = vsomeType(X14)) & (vvar(X8) = X10)))), file('/home/ubuntu/library/tptp/Problems/COM/COM149+1.p', 'T-inv')).
fof(f1356, plain, ~ spl89_2, inference(avatar_contradiction_clause, [], [f1355])).
fof(f1355, plain, ($false | ~ spl89_2), inference(subsumption_resolution, [], [f1353, f295])).
fof(f295, plain, ! [X2, X0, X3, X1] : ~ (vvar(X0) = vabs(X1, X2, X3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1, X2, X3] : ~ (vvar(X0) = vabs(X1, X2, X3)), file('/home/ubuntu/library/tptp/Problems/COM/COM149+1.p', 'DIFF-var-abs')).
fof(f1353, plain, ((vvar(sK88) = vabs(sK79(vempty, sK87, vvar(sK88)), sK81(vempty, sK87, vvar(sK88)), sK80(vempty, sK87, vvar(sK88)))) | ~ spl89_2), inference(resolution, [], [f1348, f425])).
fof(f425, plain, ! [X2, X0, X1] : (~ sP12(X0, X1, X2) | (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)), inference(cnf_transformation, [], [f279])).
fof(f279, plain, ! [X0, X1, X2] : ((vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2)) | ~ sP12(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79, sK80, sK81, sK82])], [f277, f278])).
fof(f278, plain, ! [X2, X1, X0] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) => (vtcheck(vbind(sK79(X0, X1, X2), sK81(X0, X1, X2), X0), sK80(X0, X1, X2), sK82(X0, X1, X2)) & (varrow(sK81(X0, X1, X2), sK82(X0, X1, X2)) = X1) & (vabs(sK79(X0, X1, X2), sK81(X0, X1, X2), sK80(X0, X1, X2)) = X2))), introduced(choice_axiom, [])).
fof(f277, plain, ! [X0, X1, X2] : (? [X3, X4, X5, X6] : (vtcheck(vbind(X3, X5, X0), X4, X6) & (varrow(X5, X6) = X1) & (vabs(X3, X5, X4) = X2)) | ~ sP12(X0, X1, X2)), inference(rectify, [], [f276])).
fof(f276, plain, ! [X2, X1, X0] : (? [X6, X7, X8, X9] : (vtcheck(vbind(X6, X8, X2), X7, X9) & (varrow(X8, X9) = X1) & (vabs(X6, X8, X7) = X0)) | ~ sP12(X2, X1, X0)), inference(nnf_transformation, [], [f212])).
fof(f1348, plain, (sP12(vempty, sK87, vvar(sK88)) | ~ spl89_2), inference(avatar_component_clause, [], [f1346])).