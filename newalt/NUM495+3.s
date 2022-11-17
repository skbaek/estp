fof(f125747, plain, $false, inference(avatar_sat_refutation, [], [f851, f2636, f5976, f6104, f9232, f13433, f94213, f94688, f125746])).
fof(f125746, plain, (~ spl15_47 | ~ spl15_126 | ~ spl15_310 | ~ spl15_1008), inference(avatar_contradiction_clause, [], [f125745])).
fof(f125745, plain, ($false | (~ spl15_47 | ~ spl15_126 | ~ spl15_310 | ~ spl15_1008)), inference(subsumption_resolution, [], [f125730, f282])).
fof(f282, plain, ~ doDivides0(xp, xm), inference(cnf_transformation, [], [f122])).
fof(f122, plain, (~ doDivides0(xp, xm) & ! [X0] : (~ (xm = sdtasdt0(xp, X0)) | ~ aNaturalNumber0(X0)) & ~ doDivides0(xp, xr) & ! [X1] : (~ (xr = sdtasdt0(xp, X1)) | ~ aNaturalNumber0(X1))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xr) | ? [X1] : ((xr = sdtasdt0(xp, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f48])).
fof(f48, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xr) | ? [X0] : ((sdtasdt0(xp, X0) = xr) & aNaturalNumber0(X0))), inference(negated_conjecture, [], [f47])).
fof(f47, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xr) | ? [X0] : ((sdtasdt0(xp, X0) = xr) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__)).
fof(f125730, plain, (doDivides0(xp, xm) | (~ spl15_47 | ~ spl15_126 | ~ spl15_310 | ~ spl15_1008)), inference(resolution, [], [f125728, f240])).
fof(f240, plain, ! [X0, X1] : (~ sP1(X0, X1) | doDivides0(X1, X0)), inference(cnf_transformation, [], [f148])).
fof(f148, plain, ! [X0, X1] : ((doDivides0(X1, X0) & ((sdtasdt0(X1, sK6(X0, X1)) = X0) & aNaturalNumber0(sK6(X0, X1)))) | ~ sP1(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f146, f147])).
fof(f147, plain, ! [X1, X0] : (? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(X1, sK6(X0, X1)) = X0) & aNaturalNumber0(sK6(X0, X1)))), introduced(choice_axiom, [])).
fof(f146, plain, ! [X0, X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2))) | ~ sP1(X0, X1)), inference(rectify, [], [f145])).
fof(f145, plain, ! [X0, X2] : ((doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ sP1(X0, X2)), inference(nnf_transformation, [], [f124])).
fof(f124, plain, ! [X0, X2] : ((doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ sP1(X0, X2)), inference(usedef, [], [e124])).
fof(e124, plain, ! [X0, X2] : (sP1(X0, X2) <=> (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f125728, plain, (sP1(xm, xp) | (~ spl15_47 | ~ spl15_126 | ~ spl15_310 | ~ spl15_1008)), inference(subsumption_resolution, [], [f125727, f15624])).
fof(f15624, plain, (iLess0(sdtpldt0(xp, sdtpldt0(xm, xr)), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl15_47 | ~ spl15_126 | ~ spl15_310)), inference(backward_demodulation, [], [f7430, f15439])).
fof(f15439, plain, ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl15_310), inference(resolution, [], [f13202, f461])).
fof(f461, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtpldt0(X12, xp) = sdtpldt0(xp, X12))), inference(resolution, [], [f172, f237])).
fof(f237, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__1837)).
fof(f172, plain, ! [X0, X1] : (~ aNaturalNumber0(X1) | (sdtpldt0(X0, X1) = sdtpldt0(X1, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0, X1] : ((sdtpldt0(X0, X1) = sdtpldt0(X1, X0)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f58])).
fof(f58, plain, ! [X0, X1] : ((sdtpldt0(X0, X1) = sdtpldt0(X1, X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtpldt0(X0, X1) = sdtpldt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', mAddComm)).
fof(f13202, plain, (aNaturalNumber0(sdtpldt0(xn, xm)) | ~ spl15_310), inference(avatar_component_clause, [], [f13201])).
fof(f13201, plain, (spl15_310 <=> aNaturalNumber0(sdtpldt0(xn, xm))), introduced(avatar_definition, [new_symbols(naming, [spl15_310])])).
fof(f7430, plain, (iLess0(sdtpldt0(xp, sdtpldt0(xm, xr)), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ spl15_47 | ~ spl15_126)), inference(backward_demodulation, [], [f7284, f7398])).
fof(f7398, plain, ((sdtpldt0(sdtpldt0(xm, xr), xp) = sdtpldt0(xp, sdtpldt0(xm, xr))) | ~ spl15_47), inference(resolution, [], [f461, f7264])).
fof(f7264, plain, (aNaturalNumber0(sdtpldt0(xm, xr)) | ~ spl15_47), inference(backward_demodulation, [], [f2610, f7244])).
fof(f7244, plain, (sdtpldt0(xr, xm) = sdtpldt0(xm, xr)), inference(resolution, [], [f460, f265])).
fof(f265, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((xr = sdtmndt0(xn, xp)) & (xn = sdtpldt0(xp, xr)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__1883)).
fof(f460, plain, ! [X11] : (~ aNaturalNumber0(X11) | (sdtpldt0(X11, xm) = sdtpldt0(xm, X11))), inference(resolution, [], [f172, f236])).
fof(f236, plain, aNaturalNumber0(xm), inference(cnf_transformation, [], [f39])).
fof(f2610, plain, (aNaturalNumber0(sdtpldt0(xr, xm)) | ~ spl15_47), inference(avatar_component_clause, [], [f2609])).
fof(f2609, plain, (spl15_47 <=> aNaturalNumber0(sdtpldt0(xr, xm))), introduced(avatar_definition, [new_symbols(naming, [spl15_47])])).
fof(f7284, plain, (iLess0(sdtpldt0(sdtpldt0(xm, xr), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ spl15_126), inference(backward_demodulation, [], [f6050, f7244])).
fof(f6050, plain, (iLess0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ spl15_126), inference(avatar_component_clause, [], [f6048])).
fof(f6048, plain, (spl15_126 <=> iLess0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp))), introduced(avatar_definition, [new_symbols(naming, [spl15_126])])).
fof(f125727, plain, (~ iLess0(sdtpldt0(xp, sdtpldt0(xm, xr)), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(xm, xp) | (~ spl15_47 | ~ spl15_1008)), inference(forward_demodulation, [], [f125726, f70810])).
fof(f70810, plain, ((sdtpldt0(sdtpldt0(xm, xr), xp) = sdtpldt0(xp, sdtpldt0(xm, xr))) | ~ spl15_47), inference(resolution, [], [f7264, f461])).
fof(f125726, plain, (sP1(xm, xp) | ~ iLess0(sdtpldt0(sdtpldt0(xm, xr), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl15_1008), inference(subsumption_resolution, [], [f125725, f280])).
fof(f280, plain, ~ doDivides0(xp, xr), inference(cnf_transformation, [], [f122])).
fof(f125725, plain, (doDivides0(xp, xr) | sP1(xm, xp) | ~ iLess0(sdtpldt0(sdtpldt0(xm, xr), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl15_1008), inference(subsumption_resolution, [], [f125724, f265])).
fof(f125724, plain, (~ aNaturalNumber0(xr) | doDivides0(xp, xr) | sP1(xm, xp) | ~ iLess0(sdtpldt0(sdtpldt0(xm, xr), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl15_1008), inference(subsumption_resolution, [], [f125715, f236])).
fof(f125715, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | doDivides0(xp, xr) | sP1(xm, xp) | ~ iLess0(sdtpldt0(sdtpldt0(xm, xr), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl15_1008), inference(equality_resolution, [], [f94499])).
fof(f94499, plain, (! [X52, X53] : (~ (sdtasdt0(xm, xr) = sdtasdt0(X52, X53)) | ~ aNaturalNumber0(X52) | ~ aNaturalNumber0(X53) | doDivides0(xp, X53) | sP1(X52, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X52, X53), xp), sdtpldt0(xp, sdtpldt0(xn, xm)))) | ~ spl15_1008), inference(avatar_component_clause, [], [f94498])).
fof(f94498, plain, (spl15_1008 <=> ! [X53, X52] : (~ (sdtasdt0(xm, xr) = sdtasdt0(X52, X53)) | ~ aNaturalNumber0(X52) | ~ aNaturalNumber0(X53) | doDivides0(xp, X53) | sP1(X52, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X52, X53), xp), sdtpldt0(xp, sdtpldt0(xn, xm))))), introduced(avatar_definition, [new_symbols(naming, [spl15_1008])])).
fof(f94688, plain, (spl15_11 | spl15_1008 | ~ spl15_310), inference(avatar_split_clause, [], [f93592, f13201, f94498, f808])).
fof(f808, plain, (spl15_11 <=> sP0(xp)), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f93592, plain, (! [X19, X20] : (~ (sdtasdt0(xm, xr) = sdtasdt0(X19, X20)) | ~ iLess0(sdtpldt0(sdtpldt0(X19, X20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(X19, xp) | doDivides0(xp, X20) | sP0(xp) | ~ aNaturalNumber0(X20) | ~ aNaturalNumber0(X19)) | ~ spl15_310), inference(subsumption_resolution, [], [f93591, f237])).
fof(f93591, plain, (! [X19, X20] : (~ (sdtasdt0(xm, xr) = sdtasdt0(X19, X20)) | ~ iLess0(sdtpldt0(sdtpldt0(X19, X20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(X19, xp) | doDivides0(xp, X20) | sP0(xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X20) | ~ aNaturalNumber0(X19)) | ~ spl15_310), inference(subsumption_resolution, [], [f45494, f272])).
fof(f272, plain, aNaturalNumber0(sK13), inference(cnf_transformation, [], [f164])).
fof(f164, plain, (doDivides0(xp, sdtasdt0(xr, xm)) & ((sdtasdt0(xr, xm) = sdtasdt0(xp, sK13)) & aNaturalNumber0(sK13))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f45, f163])).
fof(f163, plain, (? [X0] : ((sdtasdt0(xp, X0) = sdtasdt0(xr, xm)) & aNaturalNumber0(X0)) => ((sdtasdt0(xr, xm) = sdtasdt0(xp, sK13)) & aNaturalNumber0(sK13))), introduced(choice_axiom, [])).
fof(f45, plain, (doDivides0(xp, sdtasdt0(xr, xm)) & ? [X0] : ((sdtasdt0(xp, X0) = sdtasdt0(xr, xm)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__1913)).
fof(f45494, plain, (! [X19, X20] : (~ (sdtasdt0(xm, xr) = sdtasdt0(X19, X20)) | ~ iLess0(sdtpldt0(sdtpldt0(X19, X20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(X19, xp) | doDivides0(xp, X20) | ~ aNaturalNumber0(sK13) | sP0(xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X20) | ~ aNaturalNumber0(X19)) | ~ spl15_310), inference(superposition, [], [f15467, f8077])).
fof(f8077, plain, (sdtasdt0(xp, sK13) = sdtasdt0(xm, xr)), inference(backward_demodulation, [], [f273, f8063])).
fof(f8063, plain, (sdtasdt0(xr, xm) = sdtasdt0(xm, xr)), inference(resolution, [], [f476, f265])).
fof(f476, plain, ! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, xm) = sdtasdt0(xm, X11))), inference(resolution, [], [f176, f236])).
fof(f176, plain, ! [X0, X1] : (~ aNaturalNumber0(X1) | (sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f63])).
fof(f63, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtasdt0(X0, X1) = sdtasdt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', mMulComm)).
fof(f273, plain, (sdtasdt0(xr, xm) = sdtasdt0(xp, sK13)), inference(cnf_transformation, [], [f164])).
fof(f15467, plain, (! [X4, X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(X0, X2) | doDivides0(X2, X1) | ~ aNaturalNumber0(X4) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)) | ~ spl15_310), inference(backward_demodulation, [], [f252, f15439])).
fof(f252, plain, ! [X4, X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(X2, X1) | ~ aNaturalNumber0(X4) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ((sdtasdt0(X2, sK9(X1, X2)) = X1) & aNaturalNumber0(sK9(X1, X2)))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X4] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ aNaturalNumber0(X4))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f154, f155])).
fof(f155, plain, ! [X2, X1] : (? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X2, sK9(X1, X2)) = X1) & aNaturalNumber0(sK9(X1, X2)))), introduced(choice_axiom, [])).
fof(f154, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X4] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ aNaturalNumber0(X4))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f125])).
fof(f125, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(definition_folding, [], [f119, e124, e123])).
fof(f123, plain, ! [X2] : ((~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ sP0(X2)), inference(usedef, [], [e123])).
fof(e123, plain, ! [X2] : (sP0(X2) <=> (~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f119, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | (~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : (((((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7)))) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp))) | ((~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | (~ isPrime0(X2) & (? [X4] : ((~ (X2 = X4) & ~ (sz10 = X4)) & (doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4))) | (sz10 = X2) | (sz00 = X2))))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((doDivides0(X2, sdtasdt0(X0, X1)) | ? [X3] : ((sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) & aNaturalNumber0(X3))) & (isPrime0(X2) | (! [X4] : ((doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) => ((X2 = X4) | (sz10 = X4))) & ~ (sz10 = X2) & ~ (sz00 = X2)))) => (iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) => ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))))))), inference(rectify, [], [f40])).
fof(f40, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((doDivides0(X2, sdtasdt0(X0, X1)) | ? [X3] : ((sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) & aNaturalNumber0(X3))) & (isPrime0(X2) | (! [X3] : ((doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2)))) => (iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) => ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) | (doDivides0(X2, X0) & ? [X3] : ((sdtasdt0(X2, X3) = X0) & aNaturalNumber0(X3))))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__1799)).
fof(f94213, plain, (~ spl15_11 | spl15_81), inference(avatar_split_clause, [], [f9343, f3898, f808])).
fof(f3898, plain, (spl15_81 <=> sP0(sK12)), introduced(avatar_definition, [new_symbols(naming, [spl15_81])])).
fof(f9343, plain, (~ sP0(xp) | spl15_81), inference(forward_demodulation, [], [f3899, f9027])).
fof(f9027, plain, (xp = sK12), inference(forward_demodulation, [], [f9026, f8547])).
fof(f8547, plain, (xp = sdtmndt0(xn, xr)), inference(forward_demodulation, [], [f8526, f7468])).
fof(f7468, plain, (xn = sdtpldt0(xr, xp)), inference(forward_demodulation, [], [f7408, f266])).
fof(f266, plain, (xn = sdtpldt0(xp, xr)), inference(cnf_transformation, [], [f43])).
fof(f7408, plain, (sdtpldt0(xp, xr) = sdtpldt0(xr, xp)), inference(resolution, [], [f461, f265])).
fof(f8526, plain, (xp = sdtmndt0(sdtpldt0(xr, xp), xr)), inference(resolution, [], [f563, f265])).
fof(f563, plain, ! [X15] : (~ aNaturalNumber0(X15) | (xp = sdtmndt0(sdtpldt0(X15, xp), X15))), inference(resolution, [], [f298, f237])).
fof(f298, plain, ! [X2, X0] : (~ aNaturalNumber0(X2) | (sdtmndt0(sdtpldt0(X0, X2), X0) = X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f297, f170])).
fof(f170, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f54])).
fof(f54, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtpldt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', mSortsB)).
fof(f297, plain, ! [X2, X0] : ((sdtmndt0(sdtpldt0(X0, X2), X0) = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f284, f296])).
fof(f296, plain, ! [X2, X0] : (sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f283, f170])).
fof(f283, plain, ! [X2, X0] : (sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f193])).
fof(f193, plain, ! [X2, X0, X1] : (sdtlseqdt0(X0, X1) | ~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtpldt0(X0, sK2(X0, X1)) = X1) & aNaturalNumber0(sK2(X0, X1))) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f127, f128])).
fof(f128, plain, ! [X1, X0] : (? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtpldt0(X0, sK2(X0, X1)) = X1) & aNaturalNumber0(sK2(X0, X1)))), introduced(choice_axiom, [])).
fof(f127, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f126])).
fof(f126, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f80])).
fof(f80, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f79])).
fof(f79, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', mDefLE)).
fof(f284, plain, ! [X2, X0] : ((sdtmndt0(sdtpldt0(X0, X2), X0) = X2) | ~ aNaturalNumber0(X2) | ~ sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f196])).
fof(f196, plain, ! [X2, X0, X1] : ((sdtmndt0(X1, X0) = X2) | ~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X1, X0) = X2) | ~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2)) & (((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtmndt0(X1, X0) = X2))) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X1, X0) = X2) | (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtmndt0(X1, X0) = X2))) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f82])).
fof(f82, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X1, X0) = X2) <=> ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f81])).
fof(f81, plain, ! [X0, X1] : ((! [X2] : ((sdtmndt0(X1, X0) = X2) <=> ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ sdtlseqdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtlseqdt0(X0, X1) => ! [X2] : ((sdtmndt0(X1, X0) = X2) <=> ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', mDefDiff)).
fof(f9026, plain, (sK12 = sdtmndt0(xn, xr)), inference(forward_demodulation, [], [f9006, f270])).
fof(f270, plain, (xn = sdtpldt0(xr, sK12)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, (sdtlseqdt0(xr, xn) & ((xn = sdtpldt0(xr, sK12)) & aNaturalNumber0(sK12)) & ~ (xn = xr)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f44, f161])).
fof(f161, plain, (? [X0] : ((xn = sdtpldt0(xr, X0)) & aNaturalNumber0(X0)) => ((xn = sdtpldt0(xr, sK12)) & aNaturalNumber0(sK12))), introduced(choice_axiom, [])).
fof(f44, plain, (sdtlseqdt0(xr, xn) & ? [X0] : ((xn = sdtpldt0(xr, X0)) & aNaturalNumber0(X0)) & ~ (xn = xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__1894)).
fof(f9006, plain, (sK12 = sdtmndt0(sdtpldt0(xr, sK12), xr)), inference(resolution, [], [f573, f265])).
fof(f573, plain, ! [X34] : (~ aNaturalNumber0(X34) | (sK12 = sdtmndt0(sdtpldt0(X34, sK12), X34))), inference(resolution, [], [f298, f269])).
fof(f269, plain, aNaturalNumber0(sK12), inference(cnf_transformation, [], [f162])).
fof(f3899, plain, (~ sP0(sK12) | spl15_81), inference(avatar_component_clause, [], [f3898])).
fof(f13433, plain, spl15_310, inference(avatar_contradiction_clause, [], [f13432])).
fof(f13432, plain, ($false | spl15_310), inference(subsumption_resolution, [], [f13431, f235])).
fof(f235, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f13431, plain, (~ aNaturalNumber0(xn) | spl15_310), inference(subsumption_resolution, [], [f13430, f236])).
fof(f13430, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xn) | spl15_310), inference(resolution, [], [f13203, f170])).
fof(f13203, plain, (~ aNaturalNumber0(sdtpldt0(xn, xm)) | spl15_310), inference(avatar_component_clause, [], [f13201])).
fof(f9232, plain, ~ spl15_81, inference(avatar_contradiction_clause, [], [f9231])).
fof(f9231, plain, ($false | ~ spl15_81), inference(subsumption_resolution, [], [f9083, f258])).
fof(f258, plain, isPrime0(xp), inference(cnf_transformation, [], [f158])).
fof(f158, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK10)) & aNaturalNumber0(sK10)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f121, f157])).
fof(f157, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK10)) & aNaturalNumber0(sK10))), introduced(choice_axiom, [])).
fof(f121, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(flattening, [], [f120])).
fof(f120, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((xp = X1) | (sz10 = X1)) | ((~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(ennf_transformation, [], [f52])).
fof(f52, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((doDivides0(X1, xp) | ? [X2] : ((sdtasdt0(X1, X2) = xp) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)) => ((xp = X1) | (sz10 = X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(rectify, [], [f41])).
fof(f41, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X0] : (((doDivides0(X0, xp) | ? [X1] : ((sdtasdt0(X0, X1) = xp) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xp = X0) | (sz10 = X0))) & ~ (sz10 = xp) & ~ (sz00 = xp)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__1860)).
fof(f9083, plain, (~ isPrime0(xp) | ~ spl15_81), inference(backward_demodulation, [], [f3924, f9027])).
fof(f3924, plain, (~ isPrime0(sK12) | ~ spl15_81), inference(resolution, [], [f3900, f247])).
fof(f247, plain, ! [X0] : (~ sP0(X0) | ~ isPrime0(X0)), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ! [X0] : ((~ isPrime0(X0) & ((~ (sK7(X0) = X0) & ~ (sz10 = sK7(X0)) & doDivides0(sK7(X0), X0) & ((sdtasdt0(sK7(X0), sK8(X0)) = X0) & aNaturalNumber0(sK8(X0))) & aNaturalNumber0(sK7(X0))) | (sz10 = X0) | (sz00 = X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f150, f152, f151])).
fof(f151, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => (~ (sK7(X0) = X0) & ~ (sz10 = sK7(X0)) & doDivides0(sK7(X0), X0) & ? [X2] : ((sdtasdt0(sK7(X0), X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(sK7(X0)))), introduced(choice_axiom, [])).
fof(f152, plain, ! [X0] : (? [X2] : ((sdtasdt0(sK7(X0), X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(sK7(X0), sK8(X0)) = X0) & aNaturalNumber0(sK8(X0)))), introduced(choice_axiom, [])).
fof(f150, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP0(X0)), inference(rectify, [], [f149])).
fof(f149, plain, ! [X2] : ((~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ sP0(X2)), inference(nnf_transformation, [], [f123])).
fof(f3900, plain, (sP0(sK12) | ~ spl15_81), inference(avatar_component_clause, [], [f3898])).
fof(f6104, plain, (~ spl15_12 | ~ spl15_16 | spl15_126), inference(avatar_split_clause, [], [f6103, f6048, f848, f828])).
fof(f828, plain, (spl15_12 <=> aNaturalNumber0(sdtpldt0(sdtpldt0(xr, xm), xp))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f848, plain, (spl15_16 <=> aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f6103, plain, (iLess0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xr, xm), xp))), inference(subsumption_resolution, [], [f968, f275])).
fof(f275, plain, ~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(xr, xm), xp)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, (sdtlseqdt0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) & ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(xr, xm), xp), sK14)) & aNaturalNumber0(sK14)) & ~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(xr, xm), xp))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f46, f165])).
fof(f165, plain, (? [X0] : ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(xr, xm), xp), X0)) & aNaturalNumber0(X0)) => ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(xr, xm), xp), sK14)) & aNaturalNumber0(sK14))), introduced(choice_axiom, [])).
fof(f46, plain, (sdtlseqdt0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) & ? [X0] : ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(xr, xm), xp), X0)) & aNaturalNumber0(X0)) & ~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(xr, xm), xp))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', m__2062)).
fof(f968, plain, (iLess0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(xr, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xr, xm), xp))), inference(resolution, [], [f213, f278])).
fof(f278, plain, sdtlseqdt0(sdtpldt0(sdtpldt0(xr, xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)), inference(cnf_transformation, [], [f166])).
fof(f213, plain, ! [X0, X1] : (~ sdtlseqdt0(X0, X1) | iLess0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1] : (iLess0(X0, X1) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f98])).
fof(f98, plain, ! [X0, X1] : ((iLess0(X0, X1) | (~ sdtlseqdt0(X0, X1) | (X0 = X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X0, X1) & ~ (X0 = X1)) => iLess0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM495+3.p', mIH_03)).
fof(f5976, plain, (spl15_12 | ~ spl15_47), inference(avatar_contradiction_clause, [], [f5975])).
fof(f5975, plain, ($false | (spl15_12 | ~ spl15_47)), inference(subsumption_resolution, [], [f5974, f2610])).
fof(f5974, plain, (~ aNaturalNumber0(sdtpldt0(xr, xm)) | spl15_12), inference(subsumption_resolution, [], [f5973, f237])).
fof(f5973, plain, (~ aNaturalNumber0(xp) | ~ aNaturalNumber0(sdtpldt0(xr, xm)) | spl15_12), inference(resolution, [], [f830, f170])).
fof(f830, plain, (~ aNaturalNumber0(sdtpldt0(sdtpldt0(xr, xm), xp)) | spl15_12), inference(avatar_component_clause, [], [f828])).
fof(f2636, plain, spl15_47, inference(avatar_contradiction_clause, [], [f2635])).
fof(f2635, plain, ($false | spl15_47), inference(subsumption_resolution, [], [f2634, f265])).
fof(f2634, plain, (~ aNaturalNumber0(xr) | spl15_47), inference(subsumption_resolution, [], [f2633, f236])).
fof(f2633, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | spl15_47), inference(resolution, [], [f2611, f170])).
fof(f2611, plain, (~ aNaturalNumber0(sdtpldt0(xr, xm)) | spl15_47), inference(avatar_component_clause, [], [f2609])).
fof(f851, plain, (~ spl15_12 | spl15_16), inference(avatar_split_clause, [], [f846, f848, f828])).
fof(f846, plain, (aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xr, xm), xp))), inference(subsumption_resolution, [], [f825, f276])).
fof(f276, plain, aNaturalNumber0(sK14), inference(cnf_transformation, [], [f166])).
fof(f825, plain, (aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sK14) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xr, xm), xp))), inference(superposition, [], [f170, f277])).
fof(f277, plain, (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(xr, xm), xp), sK14)), inference(cnf_transformation, [], [f166])).