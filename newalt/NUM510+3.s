fof(f52481, plain, $false, inference(avatar_sat_refutation, [], [f409, f418, f423, f428, f429, f879, f1590, f18093, f51532, f52366])).
fof(f52366, plain, (~ spl22_20 | spl22_39), inference(avatar_contradiction_clause, [], [f52365])).
fof(f52365, plain, ($false | (~ spl22_20 | spl22_39)), inference(subsumption_resolution, [], [f52364, f1204])).
fof(f1204, plain, (~ (sz00 = xn) | spl22_39), inference(avatar_component_clause, [], [f1203])).
fof(f1203, plain, (spl22_39 <=> (sz00 = xn)), introduced(avatar_definition, [new_symbols(naming, [spl22_39])])).
fof(f52364, plain, ((sz00 = xn) | ~ spl22_20), inference(forward_demodulation, [], [f52134, f525])).
fof(f525, plain, (sz00 = sdtasdt0(sz00, xr)), inference(resolution, [], [f211, f311])).
fof(f311, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f182])).
fof(f182, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ((xk = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15)) & aNaturalNumber0(xr)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15])], [f135, f181])).
fof(f181, plain, (? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) => ((xk = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15))), introduced(choice_axiom, [])).
fof(f135, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(flattening, [], [f134])).
fof(f134, plain, (isPrime0(xr) & ! [X0] : (((xr = X0) | (sz10 = X0)) | ((~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(ennf_transformation, [], [f60])).
fof(f60, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(rectify, [], [f48])).
fof(f48, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X0] : ((xk = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__2342)).
fof(f211, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(sz00, X0))), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ! [X0] : (((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aNaturalNumber0(X0) => ((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m_MulZero)).
fof(f52134, plain, ((xn = sdtasdt0(sz00, xr)) | ~ spl22_20), inference(backward_demodulation, [], [f12241, f863])).
fof(f863, plain, ((sz00 = sK21) | ~ spl22_20), inference(avatar_component_clause, [], [f861])).
fof(f861, plain, (spl22_20 <=> (sz00 = sK21)), introduced(avatar_definition, [new_symbols(naming, [spl22_20])])).
fof(f12241, plain, (xn = sdtasdt0(sK21, xr)), inference(forward_demodulation, [], [f12219, f336])).
fof(f336, plain, (xn = sdtasdt0(xr, sK21)), inference(cnf_transformation, [], [f195])).
fof(f195, plain, (doDivides0(xr, xn) & ((xn = sdtasdt0(xr, sK21)) & aNaturalNumber0(sK21))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK21])], [f52, f194])).
fof(f194, plain, (? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xn = sdtasdt0(xr, sK21)) & aNaturalNumber0(sK21))), introduced(choice_axiom, [])).
fof(f52, plain, (doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__2487)).
fof(f12219, plain, (sdtasdt0(xr, sK21) = sdtasdt0(sK21, xr)), inference(resolution, [], [f311, f668])).
fof(f668, plain, ! [X26] : (~ aNaturalNumber0(X26) | (sdtasdt0(X26, sK21) = sdtasdt0(sK21, X26))), inference(resolution, [], [f206, f335])).
fof(f335, plain, aNaturalNumber0(sK21), inference(cnf_transformation, [], [f195])).
fof(f206, plain, ! [X0, X1] : (~ aNaturalNumber0(X1) | (sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f72])).
fof(f72, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtasdt0(X0, X1) = sdtasdt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mMulComm)).
fof(f51532, plain, (spl22_9 | ~ spl22_11 | ~ spl22_12 | spl22_20), inference(avatar_contradiction_clause, [], [f51531])).
fof(f51531, plain, ($false | (spl22_9 | ~ spl22_11 | ~ spl22_12 | spl22_20)), inference(subsumption_resolution, [], [f51530, f198])).
fof(f198, plain, aNaturalNumber0(sz10), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (sz00 = sz10) & aNaturalNumber0(sz10)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mSortsC_01)).
fof(f51530, plain, (~ aNaturalNumber0(sz10) | (spl22_9 | ~ spl22_11 | ~ spl22_12 | spl22_20)), inference(subsumption_resolution, [], [f51529, f316])).
fof(f316, plain, ~ (sz10 = xr), inference(cnf_transformation, [], [f182])).
fof(f51529, plain, ((sz10 = xr) | ~ aNaturalNumber0(sz10) | (spl22_9 | ~ spl22_11 | ~ spl22_12 | spl22_20)), inference(subsumption_resolution, [], [f51528, f14192])).
fof(f14192, plain, sdtlseqdt0(sz10, xr), inference(subsumption_resolution, [], [f14191, f311])).
fof(f14191, plain, (sdtlseqdt0(sz10, xr) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f14190, f198])).
fof(f14190, plain, (sdtlseqdt0(sz10, xr) | ~ aNaturalNumber0(sz10) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f14162, f315])).
fof(f315, plain, ~ (sz00 = xr), inference(cnf_transformation, [], [f182])).
fof(f14162, plain, (sdtlseqdt0(sz10, xr) | (sz00 = xr) | ~ aNaturalNumber0(sz10) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f242, f489])).
fof(f489, plain, (xr = sdtasdt0(sz10, xr)), inference(resolution, [], [f209, f311])).
fof(f209, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sdtasdt0(sz10, X0) = X0)), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ! [X0] : (((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : (aNaturalNumber0(X0) => ((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m_MulUnit)).
fof(f242, plain, ! [X0, X1] : (sdtlseqdt0(X1, sdtasdt0(X1, X0)) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ! [X0, X1] : (sdtlseqdt0(X1, sdtasdt0(X1, X0)) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f105])).
fof(f105, plain, ! [X0, X1] : ((sdtlseqdt0(X1, sdtasdt0(X1, X0)) | (sz00 = X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (~ (sz00 = X0) => sdtlseqdt0(X1, sdtasdt0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mMonMul2)).
fof(f51528, plain, (~ sdtlseqdt0(sz10, xr) | (sz10 = xr) | ~ aNaturalNumber0(sz10) | (spl22_9 | ~ spl22_11 | ~ spl22_12 | spl22_20)), inference(subsumption_resolution, [], [f51519, f24328])).
fof(f24328, plain, (~ sdtlseqdt0(sK21, xn) | (spl22_9 | ~ spl22_11 | ~ spl22_12)), inference(backward_demodulation, [], [f408, f24327])).
fof(f24327, plain, ((sdtsldt0(xn, xr) = sK21) | (~ spl22_11 | ~ spl22_12)), inference(subsumption_resolution, [], [f24326, f422])).
fof(f422, plain, (aNaturalNumber0(sdtsldt0(xn, xr)) | ~ spl22_12), inference(avatar_component_clause, [], [f420])).
fof(f420, plain, (spl22_12 <=> aNaturalNumber0(sdtsldt0(xn, xr))), introduced(avatar_definition, [new_symbols(naming, [spl22_12])])).
fof(f24326, plain, ((sdtsldt0(xn, xr) = sK21) | ~ aNaturalNumber0(sdtsldt0(xn, xr)) | ~ spl22_11), inference(trivial_inequality_removal, [], [f24318])).
fof(f24318, plain, (~ (xn = xn) | (sdtsldt0(xn, xr) = sK21) | ~ aNaturalNumber0(sdtsldt0(xn, xr)) | ~ spl22_11), inference(superposition, [], [f12828, f417])).
fof(f417, plain, ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) | ~ spl22_11), inference(avatar_component_clause, [], [f415])).
fof(f415, plain, (spl22_11 <=> (xn = sdtasdt0(xr, sdtsldt0(xn, xr)))), introduced(avatar_definition, [new_symbols(naming, [spl22_11])])).
fof(f12828, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f12827, f311])).
fof(f12827, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f12826, f315])).
fof(f12826, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f12808, f335])).
fof(f12808, plain, ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | (sK21 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(sK21) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f216, f336])).
fof(f216, plain, ! [X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ! [X0] : (! [X1, X2] : ((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1)) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f82])).
fof(f82, plain, ! [X0] : ((! [X1, X2] : (((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1))) | (sz00 = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (aNaturalNumber0(X0) => (~ (sz00 = X0) => ! [X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1)) => (((sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) | (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) => (X1 = X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mMulCanc)).
fof(f408, plain, (~ sdtlseqdt0(sdtsldt0(xn, xr), xn) | spl22_9), inference(avatar_component_clause, [], [f406])).
fof(f406, plain, (spl22_9 <=> sdtlseqdt0(sdtsldt0(xn, xr), xn)), introduced(avatar_definition, [new_symbols(naming, [spl22_9])])).
fof(f51519, plain, (sdtlseqdt0(sK21, xn) | ~ sdtlseqdt0(sz10, xr) | (sz10 = xr) | ~ aNaturalNumber0(sz10) | spl22_20), inference(superposition, [], [f12843, f498])).
fof(f498, plain, (sK21 = sdtasdt0(sz10, sK21)), inference(resolution, [], [f209, f335])).
fof(f12843, plain, (! [X7] : (sdtlseqdt0(sdtasdt0(X7, sK21), xn) | ~ sdtlseqdt0(X7, xr) | (xr = X7) | ~ aNaturalNumber0(X7)) | spl22_20), inference(subsumption_resolution, [], [f12842, f335])).
fof(f12842, plain, (! [X7] : (sdtlseqdt0(sdtasdt0(X7, sK21), xn) | ~ sdtlseqdt0(X7, xr) | (xr = X7) | ~ aNaturalNumber0(X7) | ~ aNaturalNumber0(sK21)) | spl22_20), inference(subsumption_resolution, [], [f12841, f311])).
fof(f12841, plain, (! [X7] : (sdtlseqdt0(sdtasdt0(X7, sK21), xn) | ~ sdtlseqdt0(X7, xr) | (xr = X7) | ~ aNaturalNumber0(xr) | ~ aNaturalNumber0(X7) | ~ aNaturalNumber0(sK21)) | spl22_20), inference(subsumption_resolution, [], [f12816, f862])).
fof(f862, plain, (~ (sz00 = sK21) | spl22_20), inference(avatar_component_clause, [], [f861])).
fof(f12816, plain, ! [X7] : (sdtlseqdt0(sdtasdt0(X7, sK21), xn) | ~ sdtlseqdt0(X7, xr) | (xr = X7) | (sz00 = sK21) | ~ aNaturalNumber0(xr) | ~ aNaturalNumber0(X7) | ~ aNaturalNumber0(sK21)), inference(superposition, [], [f239, f336])).
fof(f239, plain, ! [X2, X0, X1] : (sdtlseqdt0(sdtasdt0(X1, X0), sdtasdt0(X2, X0)) | ~ sdtlseqdt0(X1, X2) | (X1 = X2) | (sz00 = X0) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ! [X0, X1, X2] : ((sdtlseqdt0(sdtasdt0(X1, X0), sdtasdt0(X2, X0)) & ~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & sdtlseqdt0(sdtasdt0(X0, X1), sdtasdt0(X0, X2)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) | ~ sdtlseqdt0(X1, X2) | (X1 = X2) | (sz00 = X0) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : (((sdtlseqdt0(sdtasdt0(X1, X0), sdtasdt0(X2, X0)) & ~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & sdtlseqdt0(sdtasdt0(X0, X1), sdtasdt0(X0, X2)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) | (~ sdtlseqdt0(X1, X2) | (X1 = X2) | (sz00 = X0))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X2) & ~ (X1 = X2) & ~ (sz00 = X0)) => (sdtlseqdt0(sdtasdt0(X1, X0), sdtasdt0(X2, X0)) & ~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & sdtlseqdt0(sdtasdt0(X0, X1), sdtasdt0(X0, X2)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mMonMul)).
fof(f18093, plain, (~ spl22_11 | ~ spl22_13 | spl22_39), inference(avatar_contradiction_clause, [], [f18092])).
fof(f18092, plain, ($false | (~ spl22_11 | ~ spl22_13 | spl22_39)), inference(subsumption_resolution, [], [f18091, f198])).
fof(f18091, plain, (~ aNaturalNumber0(sz10) | (~ spl22_11 | ~ spl22_13 | spl22_39)), inference(subsumption_resolution, [], [f18090, f316])).
fof(f18090, plain, ((sz10 = xr) | ~ aNaturalNumber0(sz10) | (~ spl22_11 | ~ spl22_13 | spl22_39)), inference(trivial_inequality_removal, [], [f18088])).
fof(f18088, plain, (~ (xn = xn) | (sz10 = xr) | ~ aNaturalNumber0(sz10) | (~ spl22_11 | ~ spl22_13 | spl22_39)), inference(superposition, [], [f12615, f485])).
fof(f485, plain, (xn = sdtasdt0(sz10, xn)), inference(resolution, [], [f209, f265])).
fof(f265, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__1837)).
fof(f12615, plain, (! [X2] : (~ (xn = sdtasdt0(X2, xn)) | (xr = X2) | ~ aNaturalNumber0(X2)) | (~ spl22_11 | ~ spl22_13 | spl22_39)), inference(subsumption_resolution, [], [f12614, f265])).
fof(f12614, plain, (! [X2] : (~ (xn = sdtasdt0(X2, xn)) | (xr = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(xn)) | (~ spl22_11 | ~ spl22_13 | spl22_39)), inference(subsumption_resolution, [], [f12613, f1204])).
fof(f12613, plain, (! [X2] : (~ (xn = sdtasdt0(X2, xn)) | (xr = X2) | ~ aNaturalNumber0(X2) | (sz00 = xn) | ~ aNaturalNumber0(xn)) | (~ spl22_11 | ~ spl22_13)), inference(subsumption_resolution, [], [f12594, f311])).
fof(f12594, plain, (! [X2] : (~ (xn = sdtasdt0(X2, xn)) | (xr = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(xr) | (sz00 = xn) | ~ aNaturalNumber0(xn)) | (~ spl22_11 | ~ spl22_13)), inference(superposition, [], [f217, f12066])).
fof(f12066, plain, ((xn = sdtasdt0(xr, xn)) | (~ spl22_11 | ~ spl22_13)), inference(forward_demodulation, [], [f417, f427])).
fof(f427, plain, ((xn = sdtsldt0(xn, xr)) | ~ spl22_13), inference(avatar_component_clause, [], [f425])).
fof(f425, plain, (spl22_13 <=> (xn = sdtsldt0(xn, xr))), introduced(avatar_definition, [new_symbols(naming, [spl22_13])])).
fof(f217, plain, ! [X2, X0, X1] : (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f83])).
fof(f1590, plain, (~ spl22_22 | ~ spl22_39), inference(avatar_contradiction_clause, [], [f1589])).
fof(f1589, plain, ($false | (~ spl22_22 | ~ spl22_39)), inference(subsumption_resolution, [], [f1588, f1238])).
fof(f1238, plain, ~ sdtlseqdt0(xp, sz00), inference(subsumption_resolution, [], [f1237, f267])).
fof(f267, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f1237, plain, (~ sdtlseqdt0(xp, sz00) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f1236, f197])).
fof(f197, plain, aNaturalNumber0(sz00), inference(cnf_transformation, [], [f2])).
fof(f2, plain, aNaturalNumber0(sz00), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mSortsC)).
fof(f1236, plain, (~ sdtlseqdt0(xp, sz00) | ~ aNaturalNumber0(sz00) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f1182, f284])).
fof(f284, plain, ~ (sz00 = xp), inference(cnf_transformation, [], [f177])).
fof(f177, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)) & aNaturalNumber0(sK12)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f130, f176])).
fof(f176, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)) & aNaturalNumber0(sK12))), introduced(choice_axiom, [])).
fof(f130, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(flattening, [], [f129])).
fof(f129, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((xp = X1) | (sz10 = X1)) | ((~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((doDivides0(X1, xp) | ? [X2] : ((sdtasdt0(X1, X2) = xp) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)) => ((xp = X1) | (sz10 = X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(rectify, [], [f41])).
fof(f41, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X0] : (((doDivides0(X0, xp) | ? [X1] : ((sdtasdt0(X0, X1) = xp) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xp = X0) | (sz10 = X0))) & ~ (sz10 = xp) & ~ (sz00 = xp)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__1860)).
fof(f1182, plain, ((sz00 = xp) | ~ sdtlseqdt0(xp, sz00) | ~ aNaturalNumber0(sz00) | ~ aNaturalNumber0(xp)), inference(resolution, [], [f228, f692])).
fof(f692, plain, sdtlseqdt0(sz00, xp), inference(subsumption_resolution, [], [f691, f197])).
fof(f691, plain, (sdtlseqdt0(sz00, xp) | ~ aNaturalNumber0(sz00)), inference(subsumption_resolution, [], [f689, f267])).
fof(f689, plain, (sdtlseqdt0(sz00, xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(sz00)), inference(superposition, [], [f358, f453])).
fof(f453, plain, (xp = sdtpldt0(sz00, xp)), inference(resolution, [], [f205, f267])).
fof(f205, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sdtpldt0(sz00, X0) = X0)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ! [X0] : (((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : (aNaturalNumber0(X0) => ((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m_AddZero)).
fof(f358, plain, ! [X2, X0] : (sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f345, f200])).
fof(f200, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f63])).
fof(f63, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtpldt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mSortsB)).
fof(f345, plain, ! [X2, X0] : (sdtlseqdt0(X0, sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtpldt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f223])).
fof(f223, plain, ! [X2, X0, X1] : (sdtlseqdt0(X0, X1) | ~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f148])).
fof(f148, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtpldt0(X0, sK4(X0, X1)) = X1) & aNaturalNumber0(sK4(X0, X1))) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f146, f147])).
fof(f147, plain, ! [X1, X0] : (? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtpldt0(X0, sK4(X0, X1)) = X1) & aNaturalNumber0(sK4(X0, X1)))), introduced(choice_axiom, [])).
fof(f146, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f145])).
fof(f145, plain, ! [X0, X1] : (((sdtlseqdt0(X0, X1) | ! [X2] : (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ sdtlseqdt0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f89])).
fof(f89, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f88])).
fof(f88, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtlseqdt0(X0, X1) <=> ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mDefLE)).
fof(f228, plain, ! [X0, X1] : (~ sdtlseqdt0(X1, X0) | (X0 = X1) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0, X1] : ((X0 = X1) | ~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f93])).
fof(f93, plain, ! [X0, X1] : (((X0 = X1) | (~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X0) & sdtlseqdt0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', mLEAsym)).
fof(f1588, plain, (sdtlseqdt0(xp, sz00) | (~ spl22_22 | ~ spl22_39)), inference(forward_demodulation, [], [f1534, f522])).
fof(f522, plain, (sz00 = sdtasdt0(sz00, xm)), inference(resolution, [], [f211, f266])).
fof(f266, plain, aNaturalNumber0(xm), inference(cnf_transformation, [], [f39])).
fof(f1534, plain, (sdtlseqdt0(xp, sdtasdt0(sz00, xm)) | (~ spl22_22 | ~ spl22_39)), inference(backward_demodulation, [], [f874, f1205])).
fof(f1205, plain, ((sz00 = xn) | ~ spl22_39), inference(avatar_component_clause, [], [f1203])).
fof(f874, plain, (sdtlseqdt0(xp, sdtasdt0(xn, xm)) | ~ spl22_22), inference(avatar_component_clause, [], [f872])).
fof(f872, plain, (spl22_22 <=> sdtlseqdt0(xp, sdtasdt0(xn, xm))), introduced(avatar_definition, [new_symbols(naming, [spl22_22])])).
fof(f879, plain, spl22_22, inference(avatar_split_clause, [], [f878, f872])).
fof(f878, plain, sdtlseqdt0(xp, sdtasdt0(xn, xm)), inference(subsumption_resolution, [], [f877, f304])).
fof(f304, plain, aNaturalNumber0(xk), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((xk = sdtsldt0(sdtasdt0(xn, xm), xp)) & (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)) & aNaturalNumber0(xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__2306)).
fof(f877, plain, (sdtlseqdt0(xp, sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xk)), inference(subsumption_resolution, [], [f876, f267])).
fof(f876, plain, (sdtlseqdt0(xp, sdtasdt0(xn, xm)) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(xk)), inference(subsumption_resolution, [], [f824, f307])).
fof(f307, plain, ~ (sz00 = xk), inference(cnf_transformation, [], [f133])).
fof(f133, plain, (~ (sz10 = xk) & ~ (sz00 = xk)), inference(ennf_transformation, [], [f46])).
fof(f46, plain, ~ ((sz10 = xk) | (sz00 = xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__2315)).
fof(f824, plain, (sdtlseqdt0(xp, sdtasdt0(xn, xm)) | (sz00 = xk) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(xk)), inference(superposition, [], [f242, f305])).
fof(f305, plain, (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)), inference(cnf_transformation, [], [f45])).
fof(f429, plain, (spl22_11 | spl22_8), inference(avatar_split_clause, [], [f343, f402, f415])).
fof(f402, plain, (spl22_8 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl22_8])])).
fof(f343, plain, (sP3 | (xn = sdtasdt0(xr, sdtsldt0(xn, xr)))), inference(cnf_transformation, [], [f144])).
fof(f144, plain, (sP3 | ((xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), inference(definition_folding, [], [f137, e143])).
fof(f143, plain, ((~ sdtlseqdt0(sdtsldt0(xn, xr), xn) & ! [X0] : (~ (xn = sdtpldt0(sdtsldt0(xn, xr), X0)) | ~ aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(usedef, [], [e143])).
fof(e143, plain, (sP3 <=> (~ sdtlseqdt0(sdtsldt0(xn, xr), xn) & ! [X0] : (~ (xn = sdtpldt0(sdtsldt0(xn, xr), X0)) | ~ aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f137, plain, ((~ sdtlseqdt0(sdtsldt0(xn, xr), xn) & ! [X0] : (~ (xn = sdtpldt0(sdtsldt0(xn, xr), X0)) | ~ aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ((xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), inference(flattening, [], [f136])).
fof(f136, plain, (((~ sdtlseqdt0(sdtsldt0(xn, xr), xn) & ! [X0] : (~ (xn = sdtpldt0(sdtsldt0(xn, xr), X0)) | ~ aNaturalNumber0(X0))) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))) | ((xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), inference(ennf_transformation, [], [f54])).
fof(f54, plain, ~ ((((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (sdtlseqdt0(sdtsldt0(xn, xr), xn) | ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)))) & ~ ((xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ ((((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (sdtlseqdt0(sdtsldt0(xn, xr), xn) | ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)))) & ~ ((xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM510+3.p', m__)).
fof(f428, plain, (spl22_13 | spl22_8), inference(avatar_split_clause, [], [f344, f402, f425])).
fof(f344, plain, (sP3 | (xn = sdtsldt0(xn, xr))), inference(cnf_transformation, [], [f144])).
fof(f423, plain, (~ spl22_8 | spl22_12), inference(avatar_split_clause, [], [f338, f420, f402])).
fof(f338, plain, (aNaturalNumber0(sdtsldt0(xn, xr)) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ((~ sdtlseqdt0(sdtsldt0(xn, xr), xn) & ! [X0] : (~ (xn = sdtpldt0(sdtsldt0(xn, xr), X0)) | ~ aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(nnf_transformation, [], [f143])).
fof(f418, plain, (~ spl22_8 | spl22_11), inference(avatar_split_clause, [], [f339, f415, f402])).
fof(f339, plain, ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f409, plain, (~ spl22_8 | ~ spl22_9), inference(avatar_split_clause, [], [f341, f406, f402])).
fof(f341, plain, (~ sdtlseqdt0(sdtsldt0(xn, xr), xn) | ~ sP3), inference(cnf_transformation, [], [f196])).