fof(f15424, plain, $false, inference(avatar_sat_refutation, [], [f443, f448, f458, f469, f1740, f1793, f1794, f4835, f4901, f15250, f15253, f15377])).
fof(f15377, plain, (~ spl26_10 | ~ spl26_60), inference(avatar_contradiction_clause, [], [f15376])).
fof(f15376, plain, ($false | (~ spl26_10 | ~ spl26_60)), inference(subsumption_resolution, [], [f15271, f314])).
fof(f314, plain, sdtlseqdt0(xn, xp), inference(cnf_transformation, [], [f186])).
fof(f186, plain, (sdtlseqdt0(xm, xp) & ((xp = sdtpldt0(xm, sK13)) & aNaturalNumber0(sK13)) & ~ (xm = xp) & sdtlseqdt0(xn, xp) & ((xp = sdtpldt0(xn, sK14)) & aNaturalNumber0(sK14)) & ~ (xn = xp)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14])], [f62, f185, f184])).
fof(f184, plain, (? [X0] : ((xp = sdtpldt0(xm, X0)) & aNaturalNumber0(X0)) => ((xp = sdtpldt0(xm, sK13)) & aNaturalNumber0(sK13))), introduced(choice_axiom, [])).
fof(f185, plain, (? [X1] : ((xp = sdtpldt0(xn, X1)) & aNaturalNumber0(X1)) => ((xp = sdtpldt0(xn, sK14)) & aNaturalNumber0(sK14))), introduced(choice_axiom, [])).
fof(f62, plain, (sdtlseqdt0(xm, xp) & ? [X0] : ((xp = sdtpldt0(xm, X0)) & aNaturalNumber0(X0)) & ~ (xm = xp) & sdtlseqdt0(xn, xp) & ? [X1] : ((xp = sdtpldt0(xn, X1)) & aNaturalNumber0(X1)) & ~ (xn = xp)), inference(rectify, [], [f44])).
fof(f44, plain, (sdtlseqdt0(xm, xp) & ? [X0] : ((xp = sdtpldt0(xm, X0)) & aNaturalNumber0(X0)) & ~ (xm = xp) & sdtlseqdt0(xn, xp) & ? [X0] : ((xp = sdtpldt0(xn, X0)) & aNaturalNumber0(X0)) & ~ (xn = xp)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__2287)).
fof(f15271, plain, (~ sdtlseqdt0(xn, xp) | (~ spl26_10 | ~ spl26_60)), inference(backward_demodulation, [], [f1333, f2477])).
fof(f2477, plain, ((xp = sdtasdt0(xp, sK24)) | ~ spl26_60), inference(avatar_component_clause, [], [f2475])).
fof(f2475, plain, (spl26_60 <=> (xp = sdtasdt0(xp, sK24))), introduced(avatar_definition, [new_symbols(naming, [spl26_60])])).
fof(f1333, plain, (~ sdtlseqdt0(xn, sdtasdt0(xp, sK24)) | ~ spl26_10), inference(subsumption_resolution, [], [f1332, f280])).
fof(f280, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__1837)).
fof(f1332, plain, (~ sdtlseqdt0(xn, sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(xn) | ~ spl26_10), inference(subsumption_resolution, [], [f1331, f471])).
fof(f471, plain, (aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ spl26_10), inference(backward_demodulation, [], [f356, f447])).
fof(f447, plain, ((sdtsldt0(xn, xr) = sdtasdt0(xp, sK24)) | ~ spl26_10), inference(avatar_component_clause, [], [f445])).
fof(f445, plain, (spl26_10 <=> (sdtsldt0(xn, xr) = sdtasdt0(xp, sK24))), introduced(avatar_definition, [new_symbols(naming, [spl26_10])])).
fof(f356, plain, aNaturalNumber0(sdtsldt0(xn, xr)), inference(cnf_transformation, [], [f203])).
fof(f203, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ((xn = sdtpldt0(sdtsldt0(xn, xr), sK22)) & aNaturalNumber0(sK22)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22])], [f142, f202])).
fof(f202, plain, (? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) => ((xn = sdtpldt0(sdtsldt0(xn, xr), sK22)) & aNaturalNumber0(sK22))), introduced(choice_axiom, [])).
fof(f142, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(flattening, [], [f141])).
fof(f141, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & (~ (xn = sdtsldt0(xn, xr)) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (xn = sdtsldt0(xn, xr)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__2504)).
fof(f1331, plain, (~ sdtlseqdt0(xn, sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(xn) | ~ spl26_10), inference(subsumption_resolution, [], [f1300, f610])).
fof(f610, plain, (~ (xn = sdtasdt0(xp, sK24)) | ~ spl26_10), inference(superposition, [], [f355, f447])).
fof(f355, plain, ~ (xn = sdtsldt0(xn, xr)), inference(cnf_transformation, [], [f203])).
fof(f1300, plain, ((xn = sdtasdt0(xp, sK24)) | ~ sdtlseqdt0(xn, sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(xn) | ~ spl26_10), inference(resolution, [], [f243, f474])).
fof(f474, plain, (sdtlseqdt0(sdtasdt0(xp, sK24), xn) | ~ spl26_10), inference(backward_demodulation, [], [f360, f447])).
fof(f360, plain, sdtlseqdt0(sdtsldt0(xn, xr), xn), inference(cnf_transformation, [], [f203])).
fof(f243, plain, ! [X0, X1] : (~ sdtlseqdt0(X1, X0) | (X0 = X1) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1] : ((X0 = X1) | ~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f98])).
fof(f98, plain, ! [X0, X1] : (((X0 = X1) | (~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X0) & sdtlseqdt0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', mLEAsym)).
fof(f15253, plain, (~ spl26_254 | spl26_60 | ~ spl26_50 | ~ spl26_95), inference(avatar_split_clause, [], [f15252, f4832, f1764, f2475, f11903])).
fof(f11903, plain, (spl26_254 <=> sdtlseqdt0(sdtasdt0(xp, sK24), xp)), introduced(avatar_definition, [new_symbols(naming, [spl26_254])])).
fof(f1764, plain, (spl26_50 <=> aNaturalNumber0(sdtasdt0(xp, sK24))), introduced(avatar_definition, [new_symbols(naming, [spl26_50])])).
fof(f4832, plain, (spl26_95 <=> sdtlseqdt0(xp, sdtasdt0(xp, sK24))), introduced(avatar_definition, [new_symbols(naming, [spl26_95])])).
fof(f15252, plain, ((xp = sdtasdt0(xp, sK24)) | ~ sdtlseqdt0(sdtasdt0(xp, sK24), xp) | (~ spl26_50 | ~ spl26_95)), inference(subsumption_resolution, [], [f15251, f1765])).
fof(f1765, plain, (aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ spl26_50), inference(avatar_component_clause, [], [f1764])).
fof(f15251, plain, ((xp = sdtasdt0(xp, sK24)) | ~ sdtlseqdt0(sdtasdt0(xp, sK24), xp) | ~ aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ spl26_95), inference(subsumption_resolution, [], [f13568, f282])).
fof(f282, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f13568, plain, ((xp = sdtasdt0(xp, sK24)) | ~ sdtlseqdt0(sdtasdt0(xp, sK24), xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ spl26_95), inference(resolution, [], [f4834, f243])).
fof(f4834, plain, (sdtlseqdt0(xp, sdtasdt0(xp, sK24)) | ~ spl26_95), inference(avatar_component_clause, [], [f4832])).
fof(f15250, plain, (spl26_254 | ~ spl26_10 | ~ spl26_50 | ~ spl26_53), inference(avatar_split_clause, [], [f15249, f1778, f1764, f445, f11903])).
fof(f1778, plain, (spl26_53 <=> aNaturalNumber0(xn)), introduced(avatar_definition, [new_symbols(naming, [spl26_53])])).
fof(f15249, plain, (sdtlseqdt0(sdtasdt0(xp, sK24), xp) | (~ spl26_10 | ~ spl26_50 | ~ spl26_53)), inference(subsumption_resolution, [], [f15235, f1765])).
fof(f15235, plain, (sdtlseqdt0(sdtasdt0(xp, sK24), xp) | ~ aNaturalNumber0(sdtasdt0(xp, sK24)) | (~ spl26_10 | ~ spl26_53)), inference(resolution, [], [f2573, f474])).
fof(f2573, plain, (! [X22] : (~ sdtlseqdt0(X22, xn) | sdtlseqdt0(X22, xp) | ~ aNaturalNumber0(X22)) | ~ spl26_53), inference(subsumption_resolution, [], [f2572, f1779])).
fof(f1779, plain, (aNaturalNumber0(xn) | ~ spl26_53), inference(avatar_component_clause, [], [f1778])).
fof(f2572, plain, ! [X22] : (sdtlseqdt0(X22, xp) | ~ sdtlseqdt0(X22, xn) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(X22)), inference(subsumption_resolution, [], [f2549, f282])).
fof(f2549, plain, ! [X22] : (sdtlseqdt0(X22, xp) | ~ sdtlseqdt0(X22, xn) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(X22)), inference(resolution, [], [f244, f314])).
fof(f244, plain, ! [X2, X0, X1] : (~ sdtlseqdt0(X1, X2) | sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : (sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f100])).
fof(f100, plain, ! [X0, X1, X2] : ((sdtlseqdt0(X0, X2) | (~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X2) & sdtlseqdt0(X0, X1)) => sdtlseqdt0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', mLETran)).
fof(f4901, plain, (~ spl26_10 | ~ spl26_15 | spl26_16), inference(avatar_contradiction_clause, [], [f4900])).
fof(f4900, plain, ($false | (~ spl26_10 | ~ spl26_15 | spl26_16)), inference(subsumption_resolution, [], [f4899, f886])).
fof(f886, plain, (~ (sz00 = xn) | spl26_16), inference(avatar_component_clause, [], [f884])).
fof(f884, plain, (spl26_16 <=> (sz00 = xn)), introduced(avatar_definition, [new_symbols(naming, [spl26_16])])).
fof(f4899, plain, ((sz00 = xn) | (~ spl26_10 | ~ spl26_15)), inference(forward_demodulation, [], [f4838, f571])).
fof(f571, plain, (sz00 = sdtasdt0(xr, sz00)), inference(resolution, [], [f225, f326])).
fof(f326, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f188])).
fof(f188, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ((xk = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15)) & aNaturalNumber0(xr)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15])], [f140, f187])).
fof(f187, plain, (? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) => ((xk = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15))), introduced(choice_axiom, [])).
fof(f140, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(flattening, [], [f139])).
fof(f139, plain, (isPrime0(xr) & ! [X0] : (((xr = X0) | (sz10 = X0)) | ((~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(rectify, [], [f48])).
fof(f48, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X0] : ((xk = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__2342)).
fof(f225, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(X0, sz00))), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : (((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aNaturalNumber0(X0) => ((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m_MulZero)).
fof(f4838, plain, ((xn = sdtasdt0(xr, sz00)) | (~ spl26_10 | ~ spl26_15)), inference(backward_demodulation, [], [f472, f882])).
fof(f882, plain, ((sz00 = sdtasdt0(xp, sK24)) | ~ spl26_15), inference(avatar_component_clause, [], [f880])).
fof(f880, plain, (spl26_15 <=> (sz00 = sdtasdt0(xp, sK24))), introduced(avatar_definition, [new_symbols(naming, [spl26_15])])).
fof(f472, plain, ((xn = sdtasdt0(xr, sdtasdt0(xp, sK24))) | ~ spl26_10), inference(backward_demodulation, [], [f357, f447])).
fof(f357, plain, (xn = sdtasdt0(xr, sdtsldt0(xn, xr))), inference(cnf_transformation, [], [f203])).
fof(f4835, plain, (spl26_95 | spl26_15 | ~ spl26_9 | ~ spl26_10 | ~ spl26_50), inference(avatar_split_clause, [], [f4830, f1764, f445, f440, f880, f4832])).
fof(f440, plain, (spl26_9 <=> doDivides0(xp, sdtsldt0(xn, xr))), introduced(avatar_definition, [new_symbols(naming, [spl26_9])])).
fof(f4830, plain, ((sz00 = sdtasdt0(xp, sK24)) | sdtlseqdt0(xp, sdtasdt0(xp, sK24)) | (~ spl26_9 | ~ spl26_10 | ~ spl26_50)), inference(subsumption_resolution, [], [f4829, f282])).
fof(f4829, plain, ((sz00 = sdtasdt0(xp, sK24)) | sdtlseqdt0(xp, sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(xp) | (~ spl26_9 | ~ spl26_10 | ~ spl26_50)), inference(subsumption_resolution, [], [f2033, f1765])).
fof(f2033, plain, ((sz00 = sdtasdt0(xp, sK24)) | sdtlseqdt0(xp, sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(sdtasdt0(xp, sK24)) | ~ aNaturalNumber0(xp) | (~ spl26_9 | ~ spl26_10)), inference(resolution, [], [f268, f475])).
fof(f475, plain, (doDivides0(xp, sdtasdt0(xp, sK24)) | (~ spl26_9 | ~ spl26_10)), inference(forward_demodulation, [], [f442, f447])).
fof(f442, plain, (doDivides0(xp, sdtsldt0(xn, xr)) | ~ spl26_9), inference(avatar_component_clause, [], [f440])).
fof(f268, plain, ! [X0, X1] : (~ doDivides0(X0, X1) | (sz00 = X1) | sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ! [X0, X1] : (sdtlseqdt0(X0, X1) | (sz00 = X1) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) | ((sz00 = X1) | ~ doDivides0(X0, X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((~ (sz00 = X1) & doDivides0(X0, X1)) => sdtlseqdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', mDivLE)).
fof(f1794, plain, spl26_53, inference(avatar_split_clause, [], [f280, f1778])).
fof(f1793, plain, (spl26_50 | ~ spl26_10), inference(avatar_split_clause, [], [f471, f445, f1764])).
fof(f1740, plain, ~ spl26_16, inference(avatar_contradiction_clause, [], [f1739])).
fof(f1739, plain, ($false | ~ spl26_16), inference(subsumption_resolution, [], [f1682, f588])).
fof(f588, plain, (sz00 = sdtasdt0(sz00, xm)), inference(resolution, [], [f226, f281])).
fof(f281, plain, aNaturalNumber0(xm), inference(cnf_transformation, [], [f39])).
fof(f226, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(sz00, X0))), inference(cnf_transformation, [], [f82])).
fof(f1682, plain, (~ (sz00 = sdtasdt0(sz00, xm)) | ~ spl26_16), inference(backward_demodulation, [], [f479, f885])).
fof(f885, plain, ((sz00 = xn) | ~ spl26_16), inference(avatar_component_clause, [], [f884])).
fof(f479, plain, ~ (xn = sdtasdt0(xn, xm)), inference(subsumption_resolution, [], [f477, f304])).
fof(f304, plain, aNaturalNumber0(sK12), inference(cnf_transformation, [], [f183])).
fof(f183, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)) & aNaturalNumber0(sK12)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f135, f182])).
fof(f182, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)) & aNaturalNumber0(sK12))), introduced(choice_axiom, [])).
fof(f135, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(flattening, [], [f134])).
fof(f134, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((xp = X1) | (sz10 = X1)) | ((~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(ennf_transformation, [], [f61])).
fof(f61, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((doDivides0(X1, xp) | ? [X2] : ((sdtasdt0(X1, X2) = xp) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)) => ((xp = X1) | (sz10 = X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(rectify, [], [f41])).
fof(f41, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X0] : (((doDivides0(X0, xp) | ? [X1] : ((sdtasdt0(X0, X1) = xp) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xp = X0) | (sz10 = X0))) & ~ (sz10 = xp) & ~ (sz00 = xp)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__1860)).
fof(f477, plain, (~ (xn = sdtasdt0(xn, xm)) | ~ aNaturalNumber0(sK12)), inference(superposition, [], [f374, f305])).
fof(f305, plain, (sdtasdt0(xn, xm) = sdtasdt0(xp, sK12)), inference(cnf_transformation, [], [f183])).
fof(f374, plain, ! [X1] : (~ (xn = sdtasdt0(xp, X1)) | ~ aNaturalNumber0(X1)), inference(cnf_transformation, [], [f143])).
fof(f143, plain, (~ doDivides0(xp, xm) & ! [X0] : (~ (xm = sdtasdt0(xp, X0)) | ~ aNaturalNumber0(X0)) & ~ doDivides0(xp, xn) & ! [X1] : (~ (xn = sdtasdt0(xp, X1)) | ~ aNaturalNumber0(X1))), inference(ennf_transformation, [], [f67])).
fof(f67, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xn) | ? [X1] : ((xn = sdtasdt0(xp, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f57])).
fof(f57, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xn) | ? [X0] : ((xn = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))), inference(negated_conjecture, [], [f56])).
fof(f56, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xn) | ? [X0] : ((xn = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__)).
fof(f469, plain, ~ spl26_12, inference(avatar_split_clause, [], [f377, f455])).
fof(f455, plain, (spl26_12 <=> doDivides0(xp, xm)), introduced(avatar_definition, [new_symbols(naming, [spl26_12])])).
fof(f377, plain, ~ doDivides0(xp, xm), inference(cnf_transformation, [], [f143])).
fof(f458, plain, (spl26_8 | spl26_12), inference(avatar_split_clause, [], [f373, f455, f436])).
fof(f436, plain, (spl26_8 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl26_8])])).
fof(f373, plain, (doDivides0(xp, xm) | sP3), inference(cnf_transformation, [], [f211])).
fof(f211, plain, ((doDivides0(xp, xm) & ((xm = sdtasdt0(xp, sK25)) & aNaturalNumber0(sK25))) | sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK25])], [f150, f210])).
fof(f210, plain, (? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((xm = sdtasdt0(xp, sK25)) & aNaturalNumber0(sK25))), introduced(choice_axiom, [])).
fof(f150, plain, ((doDivides0(xp, xm) & ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))) | sP3), inference(definition_folding, [], [f66, e149])).
fof(f149, plain, ((doDivides0(xp, sdtsldt0(xn, xr)) & ? [X1] : ((sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) & aNaturalNumber0(X1)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(usedef, [], [e149])).
fof(e149, plain, (sP3 <=> (doDivides0(xp, sdtsldt0(xn, xr)) & ? [X1] : ((sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) & aNaturalNumber0(X1)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f66, plain, ((doDivides0(xp, xm) & ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))) | (doDivides0(xp, sdtsldt0(xn, xr)) & ? [X1] : ((sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) & aNaturalNumber0(X1)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), inference(rectify, [], [f55])).
fof(f55, plain, ((doDivides0(xp, xm) & ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))) | (doDivides0(xp, sdtsldt0(xn, xr)) & ? [X0] : ((sdtasdt0(xp, X0) = sdtsldt0(xn, xr)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM518+3.p', m__2645)).
fof(f448, plain, (~ spl26_8 | spl26_10), inference(avatar_split_clause, [], [f369, f445, f436])).
fof(f369, plain, ((sdtsldt0(xn, xr) = sdtasdt0(xp, sK24)) | ~ sP3), inference(cnf_transformation, [], [f209])).
fof(f209, plain, ((doDivides0(xp, sdtsldt0(xn, xr)) & ((sdtsldt0(xn, xr) = sdtasdt0(xp, sK24)) & aNaturalNumber0(sK24)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK24])], [f207, f208])).
fof(f208, plain, (? [X0] : ((sdtasdt0(xp, X0) = sdtsldt0(xn, xr)) & aNaturalNumber0(X0)) => ((sdtsldt0(xn, xr) = sdtasdt0(xp, sK24)) & aNaturalNumber0(sK24))), introduced(choice_axiom, [])).
fof(f207, plain, ((doDivides0(xp, sdtsldt0(xn, xr)) & ? [X0] : ((sdtasdt0(xp, X0) = sdtsldt0(xn, xr)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(rectify, [], [f206])).
fof(f206, plain, ((doDivides0(xp, sdtsldt0(xn, xr)) & ? [X1] : ((sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) & aNaturalNumber0(X1)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) | ~ sP3), inference(nnf_transformation, [], [f149])).
fof(f443, plain, (~ spl26_8 | spl26_9), inference(avatar_split_clause, [], [f370, f440, f436])).
fof(f370, plain, (doDivides0(xp, sdtsldt0(xn, xr)) | ~ sP3), inference(cnf_transformation, [], [f209])).