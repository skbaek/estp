fof(f980853, plain, $false, inference(avatar_sat_refutation, [], [f8776, f51020, f51114, f295878, f329613, f980610, f980740])).
fof(f980740, plain, ~ spl17_8656, inference(avatar_contradiction_clause, [], [f980739])).
fof(f980739, plain, ($false | ~ spl17_8656), inference(subsumption_resolution, [], [f980638, f314])).
fof(f314, plain, ~ doDivides0(xr, xn), inference(cnf_transformation, [], [f134])).
fof(f134, plain, (~ doDivides0(xr, xm) & ! [X0] : (~ (xm = sdtasdt0(xr, X0)) | ~ aNaturalNumber0(X0)) & ~ doDivides0(xr, xn) & ! [X1] : (~ (xn = sdtasdt0(xr, X1)) | ~ aNaturalNumber0(X1))), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ~ (doDivides0(xr, xm) | ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) | doDivides0(xr, xn) | ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f52])).
fof(f52, plain, ~ (doDivides0(xr, xm) | ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) | doDivides0(xr, xn) | ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), inference(negated_conjecture, [], [f51])).
fof(f51, plain, ~ (doDivides0(xr, xm) | ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) | doDivides0(xr, xn) | ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__)).
fof(f980638, plain, (doDivides0(xr, xn) | ~ spl17_8656), inference(resolution, [], [f659826, f254])).
fof(f254, plain, ! [X0, X1] : (~ sP1(X0, X1) | doDivides0(X1, X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0, X1] : ((doDivides0(X1, X0) & ((sdtasdt0(X1, sK6(X0, X1)) = X0) & aNaturalNumber0(sK6(X0, X1)))) | ~ sP1(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f158, f159])).
fof(f159, plain, ! [X1, X0] : (? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(X1, sK6(X0, X1)) = X0) & aNaturalNumber0(sK6(X0, X1)))), introduced(choice_axiom, [])).
fof(f158, plain, ! [X0, X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2))) | ~ sP1(X0, X1)), inference(rectify, [], [f157])).
fof(f157, plain, ! [X0, X2] : ((doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ sP1(X0, X2)), inference(nnf_transformation, [], [f136])).
fof(f136, plain, ! [X0, X2] : ((doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ sP1(X0, X2)), inference(usedef, [], [e136])).
fof(e136, plain, ! [X0, X2] : (sP1(X0, X2) <=> (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f659826, plain, (sP1(xn, xr) | ~ spl17_8656), inference(avatar_component_clause, [], [f659824])).
fof(f659824, plain, (spl17_8656 <=> sP1(xn, xr)), introduced(avatar_definition, [new_symbols(naming, [spl17_8656])])).
fof(f980610, plain, (spl17_8656 | spl17_18 | spl17_41 | ~ spl17_649 | ~ spl17_4487), inference(avatar_split_clause, [], [f980609, f295713, f51018, f2382, f794, f659824])).
fof(f794, plain, (spl17_18 <=> sP0(xr)), introduced(avatar_definition, [new_symbols(naming, [spl17_18])])).
fof(f2382, plain, (spl17_41 <=> (xp = xr)), introduced(avatar_definition, [new_symbols(naming, [spl17_41])])).
fof(f51018, plain, (spl17_649 <=> ! [X2] : (~ sdtlseqdt0(X2, xp) | sP0(X2) | ~ doDivides0(X2, sdtasdt0(xn, xm)) | doDivides0(X2, xm) | sP1(xn, X2) | ~ aNaturalNumber0(X2) | (xp = X2))), introduced(avatar_definition, [new_symbols(naming, [spl17_649])])).
fof(f295713, plain, (spl17_4487 <=> aNaturalNumber0(xr)), introduced(avatar_definition, [new_symbols(naming, [spl17_4487])])).
fof(f980609, plain, (sP1(xn, xr) | (spl17_18 | spl17_41 | ~ spl17_649 | ~ spl17_4487)), inference(subsumption_resolution, [], [f980608, f2383])).
fof(f2383, plain, (~ (xp = xr) | spl17_41), inference(avatar_component_clause, [], [f2382])).
fof(f980608, plain, (sP1(xn, xr) | (xp = xr) | (spl17_18 | ~ spl17_649 | ~ spl17_4487)), inference(subsumption_resolution, [], [f980607, f295714])).
fof(f295714, plain, (aNaturalNumber0(xr) | ~ spl17_4487), inference(avatar_component_clause, [], [f295713])).
fof(f980607, plain, (sP1(xn, xr) | ~ aNaturalNumber0(xr) | (xp = xr) | (spl17_18 | ~ spl17_649)), inference(subsumption_resolution, [], [f980606, f316])).
fof(f316, plain, ~ doDivides0(xr, xm), inference(cnf_transformation, [], [f134])).
fof(f980606, plain, (doDivides0(xr, xm) | sP1(xn, xr) | ~ aNaturalNumber0(xr) | (xp = xr) | (spl17_18 | ~ spl17_649)), inference(subsumption_resolution, [], [f980605, f25471])).
fof(f25471, plain, sdtlseqdt0(xr, xp), inference(subsumption_resolution, [], [f25465, f295])).
fof(f295, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f175])).
fof(f175, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ((xk = sdtasdt0(xr, sK13)) & aNaturalNumber0(sK13)) & aNaturalNumber0(xr)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f133, f174])).
fof(f174, plain, (? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) => ((xk = sdtasdt0(xr, sK13)) & aNaturalNumber0(sK13))), introduced(choice_axiom, [])).
fof(f133, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(flattening, [], [f132])).
fof(f132, plain, (isPrime0(xr) & ! [X0] : (((xr = X0) | (sz10 = X0)) | ((~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(rectify, [], [f48])).
fof(f48, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X0] : ((xk = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__2342)).
fof(f25465, plain, (sdtlseqdt0(xr, xp) | ~ aNaturalNumber0(xr)), inference(resolution, [], [f485, f1632])).
fof(f1632, plain, ! [X21] : (~ sdtlseqdt0(X21, xk) | sdtlseqdt0(X21, xp) | ~ aNaturalNumber0(X21)), inference(subsumption_resolution, [], [f1631, f288])).
fof(f288, plain, aNaturalNumber0(xk), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((xk = sdtsldt0(sdtasdt0(xn, xm), xp)) & (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)) & aNaturalNumber0(xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__2306)).
fof(f1631, plain, ! [X21] : (sdtlseqdt0(X21, xp) | ~ sdtlseqdt0(X21, xk) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(X21)), inference(subsumption_resolution, [], [f1606, f251])).
fof(f251, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__1837)).
fof(f1606, plain, ! [X21] : (sdtlseqdt0(X21, xp) | ~ sdtlseqdt0(X21, xk) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(X21)), inference(resolution, [], [f213, f312])).
fof(f312, plain, sdtlseqdt0(xk, xp), inference(cnf_transformation, [], [f180])).
fof(f180, plain, (sdtlseqdt0(xk, xp) & ((xp = sdtpldt0(xk, sK16)) & aNaturalNumber0(sK16)) & ~ (xp = xk)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16])], [f50, f179])).
fof(f179, plain, (? [X0] : ((xp = sdtpldt0(xk, X0)) & aNaturalNumber0(X0)) => ((xp = sdtpldt0(xk, sK16)) & aNaturalNumber0(sK16))), introduced(choice_axiom, [])).
fof(f50, plain, (sdtlseqdt0(xk, xp) & ? [X0] : ((xp = sdtpldt0(xk, X0)) & aNaturalNumber0(X0)) & ~ (xp = xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__2377)).
fof(f213, plain, ! [X2, X0, X1] : (~ sdtlseqdt0(X1, X2) | sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0, X1, X2] : (sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f93])).
fof(f93, plain, ! [X0, X1, X2] : ((sdtlseqdt0(X0, X2) | (~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X2) & sdtlseqdt0(X0, X1)) => sdtlseqdt0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mLETran)).
fof(f485, plain, sdtlseqdt0(xr, xk), inference(subsumption_resolution, [], [f484, f295])).
fof(f484, plain, (sdtlseqdt0(xr, xk) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f480, f304])).
fof(f304, plain, aNaturalNumber0(sK15), inference(cnf_transformation, [], [f178])).
fof(f178, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xr, sK14)) & aNaturalNumber0(sK14)) & ((xk = sdtpldt0(xr, sK15)) & aNaturalNumber0(sK15))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f59, f177, f176])).
fof(f176, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xr, sK14)) & aNaturalNumber0(sK14))), introduced(choice_axiom, [])).
fof(f177, plain, (? [X1] : ((xk = sdtpldt0(xr, X1)) & aNaturalNumber0(X1)) => ((xk = sdtpldt0(xr, sK15)) & aNaturalNumber0(sK15))), introduced(choice_axiom, [])).
fof(f59, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & ? [X1] : ((xk = sdtpldt0(xr, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f49])).
fof(f49, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & ? [X0] : ((xk = sdtpldt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__2362)).
fof(f480, plain, (sdtlseqdt0(xr, xk) | ~ aNaturalNumber0(sK15) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f330, f305])).
fof(f305, plain, (xk = sdtpldt0(xr, sK15)), inference(cnf_transformation, [], [f178])).
fof(f330, plain, ! [X2, X0] : (sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f317, f184])).
fof(f184, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f61])).
fof(f61, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtpldt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mSortsB)).
fof(f317, plain, ! [X2, X0] : (sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f207])).
fof(f207, plain, ! [X2, X0, X1] : (sdtlseqdt0(X0, X1) | ~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtpldt0(X0, sK2(X0, X1)) = X1) & aNaturalNumber0(sK2(X0, X1))) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f139, f140])).
fof(f140, plain, ! [X1, X0] : (? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtpldt0(X0, sK2(X0, X1)) = X1) & aNaturalNumber0(sK2(X0, X1)))), introduced(choice_axiom, [])).
fof(f139, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f138])).
fof(f138, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f86])).
fof(f86, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mDefLE)).
fof(f980605, plain, (~ sdtlseqdt0(xr, xp) | doDivides0(xr, xm) | sP1(xn, xr) | ~ aNaturalNumber0(xr) | (xp = xr) | (spl17_18 | ~ spl17_649)), inference(subsumption_resolution, [], [f980593, f796])).
fof(f796, plain, (~ sP0(xr) | spl17_18), inference(avatar_component_clause, [], [f794])).
fof(f980593, plain, (sP0(xr) | ~ sdtlseqdt0(xr, xp) | doDivides0(xr, xm) | sP1(xn, xr) | ~ aNaturalNumber0(xr) | (xp = xr) | ~ spl17_649), inference(resolution, [], [f51019, f308])).
fof(f308, plain, doDivides0(xr, sdtasdt0(xn, xm)), inference(cnf_transformation, [], [f178])).
fof(f51019, plain, (! [X2] : (~ doDivides0(X2, sdtasdt0(xn, xm)) | sP0(X2) | ~ sdtlseqdt0(X2, xp) | doDivides0(X2, xm) | sP1(xn, X2) | ~ aNaturalNumber0(X2) | (xp = X2)) | ~ spl17_649), inference(avatar_component_clause, [], [f51018])).
fof(f329613, plain, ~ spl17_18, inference(avatar_contradiction_clause, [], [f329612])).
fof(f329612, plain, ($false | ~ spl17_18), inference(subsumption_resolution, [], [f329578, f303])).
fof(f303, plain, isPrime0(xr), inference(cnf_transformation, [], [f175])).
fof(f329578, plain, (~ isPrime0(xr) | ~ spl17_18), inference(resolution, [], [f795, f261])).
fof(f261, plain, ! [X0] : (~ sP0(X0) | ~ isPrime0(X0)), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ! [X0] : ((~ isPrime0(X0) & ((~ (sK7(X0) = X0) & ~ (sz10 = sK7(X0)) & doDivides0(sK7(X0), X0) & ((sdtasdt0(sK7(X0), sK8(X0)) = X0) & aNaturalNumber0(sK8(X0))) & aNaturalNumber0(sK7(X0))) | (sz10 = X0) | (sz00 = X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f162, f164, f163])).
fof(f163, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => (~ (sK7(X0) = X0) & ~ (sz10 = sK7(X0)) & doDivides0(sK7(X0), X0) & ? [X2] : ((sdtasdt0(sK7(X0), X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(sK7(X0)))), introduced(choice_axiom, [])).
fof(f164, plain, ! [X0] : (? [X2] : ((sdtasdt0(sK7(X0), X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(sK7(X0), sK8(X0)) = X0) & aNaturalNumber0(sK8(X0)))), introduced(choice_axiom, [])).
fof(f162, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP0(X0)), inference(rectify, [], [f161])).
fof(f161, plain, ! [X2] : ((~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ sP0(X2)), inference(nnf_transformation, [], [f135])).
fof(f135, plain, ! [X2] : ((~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ sP0(X2)), inference(usedef, [], [e135])).
fof(e135, plain, ! [X2] : (sP0(X2) <=> (~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f795, plain, (sP0(xr) | ~ spl17_18), inference(avatar_component_clause, [], [f794])).
fof(f295878, plain, spl17_4487, inference(avatar_split_clause, [], [f295, f295713])).
fof(f51114, plain, spl17_646, inference(avatar_contradiction_clause, [], [f51113])).
fof(f51113, plain, ($false | spl17_646), inference(subsumption_resolution, [], [f51112, f249])).
fof(f249, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f51112, plain, (~ aNaturalNumber0(xn) | spl17_646), inference(subsumption_resolution, [], [f51111, f250])).
fof(f250, plain, aNaturalNumber0(xm), inference(cnf_transformation, [], [f39])).
fof(f51111, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xn) | spl17_646), inference(resolution, [], [f51002, f184])).
fof(f51002, plain, (~ aNaturalNumber0(sdtpldt0(xn, xm)) | spl17_646), inference(avatar_component_clause, [], [f51000])).
fof(f51000, plain, (spl17_646 <=> aNaturalNumber0(sdtpldt0(xn, xm))), introduced(avatar_definition, [new_symbols(naming, [spl17_646])])).
fof(f51020, plain, (~ spl17_646 | spl17_649), inference(avatar_split_clause, [], [f51016, f51018, f51000])).
fof(f51016, plain, ! [X2] : (~ sdtlseqdt0(X2, xp) | (xp = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(xn, xm)) | sP1(xn, X2) | doDivides0(X2, xm) | ~ doDivides0(X2, sdtasdt0(xn, xm)) | sP0(X2)), inference(subsumption_resolution, [], [f51015, f249])).
fof(f51015, plain, ! [X2] : (~ sdtlseqdt0(X2, xp) | (xp = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(xn, xm)) | sP1(xn, X2) | doDivides0(X2, xm) | ~ doDivides0(X2, sdtasdt0(xn, xm)) | sP0(X2) | ~ aNaturalNumber0(xn)), inference(subsumption_resolution, [], [f51014, f250])).
fof(f51014, plain, ! [X2] : (~ sdtlseqdt0(X2, xp) | (xp = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(xn, xm)) | sP1(xn, X2) | doDivides0(X2, xm) | ~ doDivides0(X2, sdtasdt0(xn, xm)) | sP0(X2) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xn)), inference(subsumption_resolution, [], [f50993, f251])).
fof(f50993, plain, ! [X2] : (~ sdtlseqdt0(X2, xp) | (xp = X2) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(xn, xm)) | sP1(xn, X2) | doDivides0(X2, xm) | ~ doDivides0(X2, sdtasdt0(xn, xm)) | sP0(X2) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xn)), inference(duplicate_literal_removal, [], [f50944])).
fof(f50944, plain, ! [X2] : (~ sdtlseqdt0(X2, xp) | (xp = X2) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(xn, xm)) | sP1(xn, X2) | doDivides0(X2, xm) | ~ doDivides0(X2, sdtasdt0(xn, xm)) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xn)), inference(resolution, [], [f2505, f267])).
fof(f267, plain, ! [X2, X0, X1] : (~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | sP1(X0, X2) | doDivides0(X2, X1) | ~ doDivides0(X2, sdtasdt0(X0, X1)) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ((sdtasdt0(X2, sK9(X1, X2)) = X1) & aNaturalNumber0(sK9(X1, X2)))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X4] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ aNaturalNumber0(X4))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f166, f167])).
fof(f167, plain, ! [X2, X1] : (? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X2, sK9(X1, X2)) = X1) & aNaturalNumber0(sK9(X1, X2)))), introduced(choice_axiom, [])).
fof(f166, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X4] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ aNaturalNumber0(X4))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f137])).
fof(f137, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(definition_folding, [], [f126, e136, e135])).
fof(f126, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | (~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f125])).
fof(f125, plain, ! [X0, X1, X2] : (((((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7)))) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp))) | ((~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | (~ isPrime0(X2) & (? [X4] : ((~ (X2 = X4) & ~ (sz10 = X4)) & (doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4))) | (sz10 = X2) | (sz00 = X2))))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((doDivides0(X2, sdtasdt0(X0, X1)) | ? [X3] : ((sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) & aNaturalNumber0(X3))) & (isPrime0(X2) | (! [X4] : ((doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) => ((X2 = X4) | (sz10 = X4))) & ~ (sz10 = X2) & ~ (sz00 = X2)))) => (iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) => ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))))))), inference(rectify, [], [f40])).
fof(f40, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((doDivides0(X2, sdtasdt0(X0, X1)) | ? [X3] : ((sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) & aNaturalNumber0(X3))) & (isPrime0(X2) | (! [X3] : ((doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2)))) => (iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) => ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) | (doDivides0(X2, X0) & ? [X3] : ((sdtasdt0(X2, X3) = X0) & aNaturalNumber0(X3))))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', m__1799)).
fof(f2505, plain, ! [X12, X10, X11] : (iLess0(sdtpldt0(X10, X11), sdtpldt0(X10, X12)) | ~ sdtlseqdt0(X11, X12) | (X11 = X12) | ~ aNaturalNumber0(X12) | ~ aNaturalNumber0(X11) | ~ aNaturalNumber0(X10)), inference(subsumption_resolution, [], [f2504, f184])).
fof(f2504, plain, ! [X12, X10, X11] : (~ aNaturalNumber0(X10) | ~ sdtlseqdt0(X11, X12) | (X11 = X12) | ~ aNaturalNumber0(X12) | ~ aNaturalNumber0(X11) | iLess0(sdtpldt0(X10, X11), sdtpldt0(X10, X12)) | ~ aNaturalNumber0(sdtpldt0(X10, X11))), inference(subsumption_resolution, [], [f2503, f184])).
fof(f2503, plain, ! [X12, X10, X11] : (~ aNaturalNumber0(X10) | ~ sdtlseqdt0(X11, X12) | (X11 = X12) | ~ aNaturalNumber0(X12) | ~ aNaturalNumber0(X11) | iLess0(sdtpldt0(X10, X11), sdtpldt0(X10, X12)) | ~ aNaturalNumber0(sdtpldt0(X10, X12)) | ~ aNaturalNumber0(sdtpldt0(X10, X11))), inference(subsumption_resolution, [], [f2459, f198])).
fof(f198, plain, ! [X2, X0, X1] : (~ (sdtpldt0(X0, X1) = sdtpldt0(X0, X2)) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ (sdtpldt0(X1, X0) = sdtpldt0(X2, X0)) & ~ (sdtpldt0(X0, X1) = sdtpldt0(X0, X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f78])).
fof(f78, plain, ! [X0, X1, X2] : (((X1 = X2) | (~ (sdtpldt0(X1, X0) = sdtpldt0(X2, X0)) & ~ (sdtpldt0(X0, X1) = sdtpldt0(X0, X2)))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((sdtpldt0(X1, X0) = sdtpldt0(X2, X0)) | (sdtpldt0(X0, X1) = sdtpldt0(X0, X2))) => (X1 = X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mAddCanc)).
fof(f2459, plain, ! [X12, X10, X11] : (~ aNaturalNumber0(X10) | ~ sdtlseqdt0(X11, X12) | (X11 = X12) | ~ aNaturalNumber0(X12) | ~ aNaturalNumber0(X11) | iLess0(sdtpldt0(X10, X11), sdtpldt0(X10, X12)) | (sdtpldt0(X10, X11) = sdtpldt0(X10, X12)) | ~ aNaturalNumber0(sdtpldt0(X10, X12)) | ~ aNaturalNumber0(sdtpldt0(X10, X11))), inference(resolution, [], [f217, f227])).
fof(f227, plain, ! [X0, X1] : (~ sdtlseqdt0(X0, X1) | iLess0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ! [X0, X1] : (iLess0(X0, X1) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f105])).
fof(f105, plain, ! [X0, X1] : ((iLess0(X0, X1) | (~ sdtlseqdt0(X0, X1) | (X0 = X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X0, X1) & ~ (X0 = X1)) => iLess0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mIH_03)).
fof(f217, plain, ! [X2, X0, X1] : (sdtlseqdt0(sdtpldt0(X2, X0), sdtpldt0(X2, X1)) | ~ aNaturalNumber0(X2) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ! [X0, X1] : (! [X2] : ((sdtlseqdt0(sdtpldt0(X0, X2), sdtpldt0(X1, X2)) & ~ (sdtpldt0(X1, X2) = sdtpldt0(X0, X2)) & sdtlseqdt0(sdtpldt0(X2, X0), sdtpldt0(X2, X1)) & ~ (sdtpldt0(X2, X0) = sdtpldt0(X2, X1))) | ~ aNaturalNumber0(X2)) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f97])).
fof(f97, plain, ! [X0, X1] : ((! [X2] : ((sdtlseqdt0(sdtpldt0(X0, X2), sdtpldt0(X1, X2)) & ~ (sdtpldt0(X1, X2) = sdtpldt0(X0, X2)) & sdtlseqdt0(sdtpldt0(X2, X0), sdtpldt0(X2, X1)) & ~ (sdtpldt0(X2, X0) = sdtpldt0(X2, X1))) | ~ aNaturalNumber0(X2)) | (~ sdtlseqdt0(X0, X1) | (X0 = X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X0, X1) & ~ (X0 = X1)) => ! [X2] : (aNaturalNumber0(X2) => (sdtlseqdt0(sdtpldt0(X0, X2), sdtpldt0(X1, X2)) & ~ (sdtpldt0(X1, X2) = sdtpldt0(X0, X2)) & sdtlseqdt0(sdtpldt0(X2, X0), sdtpldt0(X2, X1)) & ~ (sdtpldt0(X2, X0) = sdtpldt0(X2, X1)))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mMonAdd)).
fof(f8776, plain, ~ spl17_41, inference(avatar_contradiction_clause, [], [f8775])).
fof(f8775, plain, ($false | ~ spl17_41), inference(subsumption_resolution, [], [f8646, f861])).
fof(f861, plain, ~ sdtlseqdt0(xp, xk), inference(subsumption_resolution, [], [f860, f251])).
fof(f860, plain, (~ sdtlseqdt0(xp, xk) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f859, f288])).
fof(f859, plain, (~ sdtlseqdt0(xp, xk) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f819, f309])).
fof(f309, plain, ~ (xp = xk), inference(cnf_transformation, [], [f180])).
fof(f819, plain, ((xp = xk) | ~ sdtlseqdt0(xp, xk) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xp)), inference(resolution, [], [f212, f312])).
fof(f212, plain, ! [X0, X1] : (~ sdtlseqdt0(X1, X0) | (X0 = X1) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ! [X0, X1] : ((X0 = X1) | ~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f91])).
fof(f91, plain, ! [X0, X1] : (((X0 = X1) | (~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X0) & sdtlseqdt0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM506+3.p', mLEAsym)).
fof(f8646, plain, (sdtlseqdt0(xp, xk) | ~ spl17_41), inference(backward_demodulation, [], [f485, f2384])).
fof(f2384, plain, ((xp = xr) | ~ spl17_41), inference(avatar_component_clause, [], [f2382])).