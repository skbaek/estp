fof(f33803, plain, $false, inference(avatar_sat_refutation, [], [f412, f421, f427, f3572, f3735, f13883, f16564, f33737])).
fof(f33737, plain, (~ spl23_2 | spl23_9 | ~ spl23_10 | ~ spl23_286), inference(avatar_contradiction_clause, [], [f33736])).
fof(f33736, plain, ($false | (~ spl23_2 | spl23_9 | ~ spl23_10 | ~ spl23_286)), inference(subsumption_resolution, [], [f33589, f18508])).
fof(f18508, plain, (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(xm, sK21), xr)) | spl23_9), inference(backward_demodulation, [], [f13774, f18407])).
fof(f18407, plain, (sdtsldt0(xn, xr) = sK21), inference(subsumption_resolution, [], [f18396, f346])).
fof(f346, plain, aNaturalNumber0(sdtsldt0(xn, xr)), inference(cnf_transformation, [], [f200])).
fof(f200, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ((xn = sdtpldt0(sdtsldt0(xn, xr), sK22)) & aNaturalNumber0(sK22)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22])], [f138, f199])).
fof(f199, plain, (? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) => ((xn = sdtpldt0(sdtsldt0(xn, xr), sK22)) & aNaturalNumber0(sK22))), introduced(choice_axiom, [])).
fof(f138, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(flattening, [], [f137])).
fof(f137, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & (~ (xn = sdtsldt0(xn, xr)) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (xn = sdtsldt0(xn, xr)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__2504)).
fof(f18396, plain, ((sdtsldt0(xn, xr) = sK21) | ~ aNaturalNumber0(sdtsldt0(xn, xr))), inference(trivial_inequality_removal, [], [f18387])).
fof(f18387, plain, (~ (xn = xn) | (sdtsldt0(xn, xr) = sK21) | ~ aNaturalNumber0(sdtsldt0(xn, xr))), inference(superposition, [], [f3617, f347])).
fof(f347, plain, (xn = sdtasdt0(xr, sdtsldt0(xn, xr))), inference(cnf_transformation, [], [f200])).
fof(f3617, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f3616, f316])).
fof(f316, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f185])).
fof(f185, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ((xk = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15)) & aNaturalNumber0(xr)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15])], [f136, f184])).
fof(f184, plain, (? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) => ((xk = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15))), introduced(choice_axiom, [])).
fof(f136, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(flattening, [], [f135])).
fof(f135, plain, (isPrime0(xr) & ! [X0] : (((xr = X0) | (sz10 = X0)) | ((~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(ennf_transformation, [], [f61])).
fof(f61, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(rectify, [], [f48])).
fof(f48, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X0] : ((xk = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__2342)).
fof(f3616, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3615, f320])).
fof(f320, plain, ~ (sz00 = xr), inference(cnf_transformation, [], [f185])).
fof(f3615, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3609, f340])).
fof(f340, plain, aNaturalNumber0(sK21), inference(cnf_transformation, [], [f198])).
fof(f198, plain, (doDivides0(xr, xn) & ((xn = sdtasdt0(xr, sK21)) & aNaturalNumber0(sK21))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK21])], [f52, f197])).
fof(f197, plain, (? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xn = sdtasdt0(xr, sK21)) & aNaturalNumber0(sK21))), introduced(choice_axiom, [])).
fof(f52, plain, (doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__2487)).
fof(f3609, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(sK21) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f221, f341])).
fof(f341, plain, (xn = sdtasdt0(xr, sK21)), inference(cnf_transformation, [], [f198])).
fof(f221, plain, ! [X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (! [X1, X2] : ((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1)) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f83])).
fof(f83, plain, ! [X0] : ((! [X1, X2] : (((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1))) | (sz00 = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (aNaturalNumber0(X0) => (~ (sz00 = X0) => ! [X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1)) => (((sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) | (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) => (X1 = X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', mMulCanc)).
fof(f13774, plain, (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(xm, sdtsldt0(xn, xr)), xr)) | spl23_9), inference(backward_demodulation, [], [f420, f13748])).
fof(f13748, plain, (sdtasdt0(sdtsldt0(xn, xr), xm) = sdtasdt0(xm, sdtsldt0(xn, xr))), inference(resolution, [], [f663, f346])).
fof(f663, plain, ! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, xm) = sdtasdt0(xm, X11))), inference(resolution, [], [f211, f271])).
fof(f271, plain, aNaturalNumber0(xm), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__1837)).
fof(f211, plain, ! [X0, X1] : (~ aNaturalNumber0(X1) | (sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f73])).
fof(f73, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtasdt0(X0, X1) = sdtasdt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', mMulComm)).
fof(f420, plain, (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) | spl23_9), inference(avatar_component_clause, [], [f418])).
fof(f418, plain, (spl23_9 <=> (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr))), introduced(avatar_definition, [new_symbols(naming, [spl23_9])])).
fof(f33589, plain, ((sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(xm, sK21), xr)) | (~ spl23_2 | ~ spl23_10 | ~ spl23_286)), inference(backward_demodulation, [], [f32066, f33587])).
fof(f33587, plain, ((sdtasdt0(xm, sK21) = sdtasdt0(xp, sK15)) | (~ spl23_2 | ~ spl23_286)), inference(backward_demodulation, [], [f32065, f33586])).
fof(f33586, plain, ((sdtsldt0(sdtasdt0(xn, xm), xr) = sdtasdt0(xm, sK21)) | ~ spl23_2), inference(forward_demodulation, [], [f33558, f13718])).
fof(f13718, plain, (sdtasdt0(xn, xm) = sdtasdt0(xm, xn)), inference(resolution, [], [f662, f271])).
fof(f662, plain, ! [X10] : (~ aNaturalNumber0(X10) | (sdtasdt0(X10, xn) = sdtasdt0(xn, X10))), inference(resolution, [], [f211, f270])).
fof(f270, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f33558, plain, ((sdtasdt0(xm, sK21) = sdtsldt0(sdtasdt0(xm, xn), xr)) | ~ spl23_2), inference(resolution, [], [f18444, f271])).
fof(f18444, plain, (! [X10] : (~ aNaturalNumber0(X10) | (sdtsldt0(sdtasdt0(X10, xn), xr) = sdtasdt0(X10, sK21))) | ~ spl23_2), inference(backward_demodulation, [], [f4329, f18407])).
fof(f4329, plain, (! [X10] : (~ aNaturalNumber0(X10) | (sdtasdt0(X10, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X10, xn), xr))) | ~ spl23_2), inference(subsumption_resolution, [], [f4328, f316])).
fof(f4328, plain, (! [X10] : (~ aNaturalNumber0(X10) | (sdtasdt0(X10, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X10, xn), xr)) | ~ aNaturalNumber0(xr)) | ~ spl23_2), inference(subsumption_resolution, [], [f4327, f270])).
fof(f4327, plain, (! [X10] : (~ aNaturalNumber0(X10) | (sdtasdt0(X10, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X10, xn), xr)) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(xr)) | ~ spl23_2), inference(subsumption_resolution, [], [f4303, f320])).
fof(f4303, plain, (! [X10] : (~ aNaturalNumber0(X10) | (sdtasdt0(X10, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X10, xn), xr)) | (sz00 = xr) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(xr)) | ~ spl23_2), inference(resolution, [], [f259, f385])).
fof(f385, plain, (doDivides0(xr, xn) | ~ spl23_2), inference(avatar_component_clause, [], [f383])).
fof(f383, plain, (spl23_2 <=> doDivides0(xr, xn)), introduced(avatar_definition, [new_symbols(naming, [spl23_2])])).
fof(f259, plain, ! [X2, X0, X1] : (~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | (sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0)) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0, X1] : (! [X2] : ((sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0)) | ~ aNaturalNumber0(X2)) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0, X1] : ((! [X2] : ((sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0)) | ~ aNaturalNumber0(X2)) | (~ doDivides0(X0, X1) | (sz00 = X0))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X0, X1) & ~ (sz00 = X0)) => ! [X2] : (aNaturalNumber0(X2) => (sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', mDivAsso)).
fof(f32065, plain, ((sdtsldt0(sdtasdt0(xn, xm), xr) = sdtasdt0(xp, sK15)) | ~ spl23_286), inference(forward_demodulation, [], [f32034, f3391])).
fof(f3391, plain, (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)), inference(backward_demodulation, [], [f3174, f311])).
fof(f311, plain, (xk = sdtsldt0(sdtasdt0(xn, xm), xp)), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((xk = sdtsldt0(sdtasdt0(xn, xm), xp)) & (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)) & aNaturalNumber0(xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__2306)).
fof(f3174, plain, (sdtasdt0(xn, xm) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xn, xm), xp))), inference(subsumption_resolution, [], [f3173, f272])).
fof(f272, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f3173, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xn, xm), xp))) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f3172, f588])).
fof(f588, plain, aNaturalNumber0(sdtasdt0(xn, xm)), inference(subsumption_resolution, [], [f587, f316])).
fof(f587, plain, (aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f582, f327])).
fof(f327, plain, aNaturalNumber0(sK16), inference(cnf_transformation, [], [f188])).
fof(f188, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xr, sK16)) & aNaturalNumber0(sK16)) & ((xk = sdtpldt0(xr, sK17)) & aNaturalNumber0(sK17))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16, sK17])], [f62, f187, f186])).
fof(f186, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xr, sK16)) & aNaturalNumber0(sK16))), introduced(choice_axiom, [])).
fof(f187, plain, (? [X1] : ((xk = sdtpldt0(xr, X1)) & aNaturalNumber0(X1)) => ((xk = sdtpldt0(xr, sK17)) & aNaturalNumber0(sK17))), introduced(choice_axiom, [])).
fof(f62, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & ? [X1] : ((xk = sdtpldt0(xr, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f49])).
fof(f49, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & ? [X0] : ((xk = sdtpldt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__2362)).
fof(f582, plain, (aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(sK16) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f206, f328])).
fof(f328, plain, (sdtasdt0(xn, xm) = sdtasdt0(xr, sK16)), inference(cnf_transformation, [], [f188])).
fof(f206, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f66])).
fof(f66, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', mSortsB_02)).
fof(f3172, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xn, xm), xp))) | ~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f3153, f289])).
fof(f289, plain, ~ (sz00 = xp), inference(cnf_transformation, [], [f180])).
fof(f180, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)) & aNaturalNumber0(sK12)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f131, f179])).
fof(f179, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)) & aNaturalNumber0(sK12))), introduced(choice_axiom, [])).
fof(f131, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(flattening, [], [f130])).
fof(f130, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((xp = X1) | (sz10 = X1)) | ((~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(ennf_transformation, [], [f59])).
fof(f59, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((doDivides0(X1, xp) | ? [X2] : ((sdtasdt0(X1, X2) = xp) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)) => ((xp = X1) | (sz10 = X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(rectify, [], [f41])).
fof(f41, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X0] : (((doDivides0(X0, xp) | ? [X1] : ((sdtasdt0(X0, X1) = xp) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xp = X0) | (sz10 = X0))) & ~ (sz10 = xp) & ~ (sz00 = xp)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__1860)).
fof(f3153, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xn, xm), xp))) | (sz00 = xp) | ~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xp)), inference(resolution, [], [f365, f296])).
fof(f296, plain, doDivides0(xp, sdtasdt0(xn, xm)), inference(cnf_transformation, [], [f180])).
fof(f365, plain, ! [X0, X1] : (~ doDivides0(X0, X1) | (sdtasdt0(X0, sdtsldt0(X1, X0)) = X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f253])).
fof(f253, plain, ! [X2, X0, X1] : ((sdtasdt0(X0, X2) = X1) | ~ (sdtsldt0(X1, X0) = X2) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0, X1] : (! [X2] : (((sdtsldt0(X1, X0) = X2) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2)) & (((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtsldt0(X1, X0) = X2))) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f158])).
fof(f158, plain, ! [X0, X1] : (! [X2] : (((sdtsldt0(X1, X0) = X2) | (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtsldt0(X1, X0) = X2))) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (! [X2] : ((sdtsldt0(X1, X0) = X2) <=> ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f112])).
fof(f112, plain, ! [X0, X1] : ((! [X2] : ((sdtsldt0(X1, X0) = X2) <=> ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ doDivides0(X0, X1) | (sz00 = X0))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X0, X1) & ~ (sz00 = X0)) => ! [X2] : ((sdtsldt0(X1, X0) = X2) <=> ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', mDefQuot)).
fof(f32034, plain, ((sdtsldt0(sdtasdt0(xp, xk), xr) = sdtasdt0(xp, sK15)) | ~ spl23_286), inference(resolution, [], [f18060, f272])).
fof(f18060, plain, (! [X12] : (~ aNaturalNumber0(X12) | (sdtsldt0(sdtasdt0(X12, xk), xr) = sdtasdt0(X12, sK15))) | ~ spl23_286), inference(backward_demodulation, [], [f4335, f18058])).
fof(f18058, plain, ((sK15 = sdtsldt0(xk, xr)) | ~ spl23_286), inference(subsumption_resolution, [], [f18027, f12254])).
fof(f12254, plain, (aNaturalNumber0(sdtsldt0(xk, xr)) | ~ spl23_286), inference(avatar_component_clause, [], [f12253])).
fof(f12253, plain, (spl23_286 <=> aNaturalNumber0(sdtsldt0(xk, xr))), introduced(avatar_definition, [new_symbols(naming, [spl23_286])])).
fof(f18027, plain, ((sK15 = sdtsldt0(xk, xr)) | ~ aNaturalNumber0(sdtsldt0(xk, xr))), inference(trivial_inequality_removal, [], [f18018])).
fof(f18018, plain, (~ (xk = xk) | (sK15 = sdtsldt0(xk, xr)) | ~ aNaturalNumber0(sdtsldt0(xk, xr))), inference(superposition, [], [f3524, f3452])).
fof(f3452, plain, (xk = sdtasdt0(xr, sdtsldt0(xk, xr))), inference(subsumption_resolution, [], [f3451, f316])).
fof(f3451, plain, ((xk = sdtasdt0(xr, sdtsldt0(xk, xr))) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3450, f309])).
fof(f309, plain, aNaturalNumber0(xk), inference(cnf_transformation, [], [f45])).
fof(f3450, plain, ((xk = sdtasdt0(xr, sdtsldt0(xk, xr))) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3445, f320])).
fof(f3445, plain, ((xk = sdtasdt0(xr, sdtsldt0(xk, xr))) | (sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(resolution, [], [f319, f365])).
fof(f319, plain, doDivides0(xr, xk), inference(cnf_transformation, [], [f185])).
fof(f3524, plain, ! [X0] : (~ (xk = sdtasdt0(xr, X0)) | (sK15 = X0) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f3523, f316])).
fof(f3523, plain, ! [X0] : (~ (xk = sdtasdt0(xr, X0)) | (sK15 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3522, f320])).
fof(f3522, plain, ! [X0] : (~ (xk = sdtasdt0(xr, X0)) | (sK15 = X0) | ~ aNaturalNumber0(X0) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3516, f317])).
fof(f317, plain, aNaturalNumber0(sK15), inference(cnf_transformation, [], [f185])).
fof(f3516, plain, ! [X0] : (~ (xk = sdtasdt0(xr, X0)) | (sK15 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(sK15) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f221, f318])).
fof(f318, plain, (xk = sdtasdt0(xr, sK15)), inference(cnf_transformation, [], [f185])).
fof(f4335, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X12, xk), xr))), inference(subsumption_resolution, [], [f4334, f316])).
fof(f4334, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X12, xk), xr)) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f4333, f309])).
fof(f4333, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X12, xk), xr)) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f4305, f320])).
fof(f4305, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X12, xk), xr)) | (sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(resolution, [], [f259, f319])).
fof(f32066, plain, ((sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(xp, sK15), xr)) | (~ spl23_10 | ~ spl23_286)), inference(backward_demodulation, [], [f425, f32065])).
fof(f425, plain, ((sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xn, xm), xr), xr)) | ~ spl23_10), inference(avatar_component_clause, [], [f424])).
fof(f424, plain, (spl23_10 <=> (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xn, xm), xr), xr))), introduced(avatar_definition, [new_symbols(naming, [spl23_10])])).
fof(f16564, plain, spl23_286, inference(avatar_contradiction_clause, [], [f16563])).
fof(f16563, plain, ($false | spl23_286), inference(subsumption_resolution, [], [f16562, f316])).
fof(f16562, plain, (~ aNaturalNumber0(xr) | spl23_286), inference(subsumption_resolution, [], [f16561, f309])).
fof(f16561, plain, (~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr) | spl23_286), inference(subsumption_resolution, [], [f16560, f320])).
fof(f16560, plain, ((sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr) | spl23_286), inference(subsumption_resolution, [], [f16559, f319])).
fof(f16559, plain, (~ doDivides0(xr, xk) | (sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr) | spl23_286), inference(resolution, [], [f12255, f366])).
fof(f366, plain, ! [X0, X1] : (aNaturalNumber0(sdtsldt0(X1, X0)) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f252])).
fof(f252, plain, ! [X2, X0, X1] : (aNaturalNumber0(X2) | ~ (sdtsldt0(X1, X0) = X2) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f159])).
fof(f12255, plain, (~ aNaturalNumber0(sdtsldt0(xk, xr)) | spl23_286), inference(avatar_component_clause, [], [f12253])).
fof(f13883, plain, (spl23_10 | ~ spl23_11 | ~ spl23_12), inference(avatar_split_clause, [], [f13882, f436, f430, f424])).
fof(f430, plain, (spl23_11 <=> (sdtasdt0(xn, xm) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr)))), introduced(avatar_definition, [new_symbols(naming, [spl23_11])])).
fof(f436, plain, (spl23_12 <=> aNaturalNumber0(sdtsldt0(sdtasdt0(xn, xm), xr))), introduced(avatar_definition, [new_symbols(naming, [spl23_12])])).
fof(f13882, plain, ((sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xn, xm), xr), xr)) | (~ spl23_11 | ~ spl23_12)), inference(forward_demodulation, [], [f13856, f432])).
fof(f432, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr))) | ~ spl23_11), inference(avatar_component_clause, [], [f430])).
fof(f13856, plain, ((sdtasdt0(sdtsldt0(sdtasdt0(xn, xm), xr), xr) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr))) | ~ spl23_12), inference(resolution, [], [f666, f438])).
fof(f438, plain, (aNaturalNumber0(sdtsldt0(sdtasdt0(xn, xm), xr)) | ~ spl23_12), inference(avatar_component_clause, [], [f436])).
fof(f666, plain, ! [X14] : (~ aNaturalNumber0(X14) | (sdtasdt0(X14, xr) = sdtasdt0(xr, X14))), inference(resolution, [], [f211, f316])).
fof(f3735, plain, spl23_12, inference(avatar_contradiction_clause, [], [f3734])).
fof(f3734, plain, ($false | spl23_12), inference(subsumption_resolution, [], [f3733, f316])).
fof(f3733, plain, (~ aNaturalNumber0(xr) | spl23_12), inference(subsumption_resolution, [], [f3732, f588])).
fof(f3732, plain, (~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xr) | spl23_12), inference(subsumption_resolution, [], [f3731, f320])).
fof(f3731, plain, ((sz00 = xr) | ~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xr) | spl23_12), inference(subsumption_resolution, [], [f3730, f329])).
fof(f329, plain, doDivides0(xr, sdtasdt0(xn, xm)), inference(cnf_transformation, [], [f188])).
fof(f3730, plain, (~ doDivides0(xr, sdtasdt0(xn, xm)) | (sz00 = xr) | ~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xr) | spl23_12), inference(resolution, [], [f437, f366])).
fof(f437, plain, (~ aNaturalNumber0(sdtsldt0(sdtasdt0(xn, xm), xr)) | spl23_12), inference(avatar_component_clause, [], [f436])).
fof(f3572, plain, spl23_11, inference(avatar_split_clause, [], [f3571, f430])).
fof(f3571, plain, (sdtasdt0(xn, xm) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr))), inference(subsumption_resolution, [], [f3570, f316])).
fof(f3570, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr))) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3569, f588])).
fof(f3569, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr))) | ~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3564, f320])).
fof(f3564, plain, ((sdtasdt0(xn, xm) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xn, xm), xr))) | (sz00 = xr) | ~ aNaturalNumber0(sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xr)), inference(resolution, [], [f329, f365])).
fof(f427, plain, (spl23_8 | ~ spl23_10), inference(avatar_split_clause, [], [f422, f424, f414])).
fof(f414, plain, (spl23_8 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl23_8])])).
fof(f422, plain, (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xn, xm), xr), xr)) | sP3), inference(forward_demodulation, [], [f356, f310])).
fof(f310, plain, (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)), inference(cnf_transformation, [], [f45])).
fof(f356, plain, (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xp, xk), xr), xr)) | sP3), inference(cnf_transformation, [], [f147])).
fof(f147, plain, ((~ (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xp, xk), xr), xr)) & (sdtasdt0(xp, xk) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xp, xk), xr))) & aNaturalNumber0(sdtsldt0(sdtasdt0(xp, xk), xr))) | sP3), inference(definition_folding, [], [f140, e146])).
fof(f146, plain, ((~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(usedef, [], [e146])).
fof(e146, plain, (sP3 <=> (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f140, plain, ((~ (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xp, xk), xr), xr)) & (sdtasdt0(xp, xk) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xp, xk), xr))) & aNaturalNumber0(sdtsldt0(sdtasdt0(xp, xk), xr))) | (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), inference(flattening, [], [f139])).
fof(f139, plain, ((~ (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xp, xk), xr), xr)) & ((sdtasdt0(xp, xk) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xp, xk), xr))) & aNaturalNumber0(sdtsldt0(sdtasdt0(xp, xk), xr)))) | (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ~ ((((sdtasdt0(xp, xk) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xp, xk), xr))) & aNaturalNumber0(sdtsldt0(sdtasdt0(xp, xk), xr))) => (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xp, xk), xr), xr))) & (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)))), inference(negated_conjecture, [], [f54])).
fof(f54, plain, ~ ((((sdtasdt0(xp, xk) = sdtasdt0(xr, sdtsldt0(sdtasdt0(xp, xk), xr))) & aNaturalNumber0(sdtsldt0(sdtasdt0(xp, xk), xr))) => (sdtasdt0(xn, xm) = sdtasdt0(sdtsldt0(sdtasdt0(xp, xk), xr), xr))) & (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM512+3.p', m__)).
fof(f421, plain, (~ spl23_8 | ~ spl23_9), inference(avatar_split_clause, [], [f353, f418, f414])).
fof(f353, plain, (~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) | ~ sP3), inference(cnf_transformation, [], [f201])).
fof(f201, plain, ((~ (sdtasdt0(xn, xm) = sdtasdt0(sdtasdt0(sdtsldt0(xn, xr), xm), xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(nnf_transformation, [], [f146])).
fof(f412, plain, spl23_2, inference(avatar_split_clause, [], [f342, f383])).
fof(f342, plain, doDivides0(xr, xn), inference(cnf_transformation, [], [f198])).