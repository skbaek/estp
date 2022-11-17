fof(f470648, plain, $false, inference(avatar_sat_refutation, [], [f410, f415, f420, f425, f430, f431, f2825, f5779, f5780, f6560, f9597, f15764, f15806, f16093, f19750, f19795, f45820, f61690, f61700, f180665, f187844, f267487, f268292, f379863, f380681, f380941, f436914, f470643])).
fof(f470643, plain, (~ spl24_2 | spl24_29 | ~ spl24_79 | ~ spl24_89 | ~ spl24_401 | ~ spl24_412 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1362 | ~ spl24_1976 | spl24_2928), inference(avatar_contradiction_clause, [], [f470642])).
fof(f470642, plain, ($false | (~ spl24_2 | spl24_29 | ~ spl24_79 | ~ spl24_89 | ~ spl24_401 | ~ spl24_412 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1362 | ~ spl24_1976 | spl24_2928)), inference(subsumption_resolution, [], [f470641, f50361])).
fof(f50361, plain, (iLess0(sdtpldt0(xp, sdtpldt0(xm, sK20)), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl24_79 | ~ spl24_89 | ~ spl24_412 | ~ spl24_683)), inference(backward_demodulation, [], [f32428, f49827])).
fof(f49827, plain, ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl24_683), inference(resolution, [], [f45501, f663])).
fof(f663, plain, ! [X13] : (~ aNaturalNumber0(X13) | (sdtpldt0(X13, xp) = sdtpldt0(xp, X13))), inference(resolution, [], [f213, f278])).
fof(f278, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__1837)).
fof(f213, plain, ! [X0, X1] : (~ aNaturalNumber0(X1) | (sdtpldt0(X0, X1) = sdtpldt0(X1, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ! [X0, X1] : ((sdtpldt0(X0, X1) = sdtpldt0(X1, X0)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f71])).
fof(f71, plain, ! [X0, X1] : ((sdtpldt0(X0, X1) = sdtpldt0(X1, X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtpldt0(X0, X1) = sdtpldt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mAddComm)).
fof(f45501, plain, (aNaturalNumber0(sdtpldt0(xn, xm)) | ~ spl24_683), inference(avatar_component_clause, [], [f45500])).
fof(f45500, plain, (spl24_683 <=> aNaturalNumber0(sdtpldt0(xn, xm))), introduced(avatar_definition, [new_symbols(naming, [spl24_683])])).
fof(f32428, plain, (iLess0(sdtpldt0(xp, sdtpldt0(xm, sK20)), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ spl24_79 | ~ spl24_89 | ~ spl24_412)), inference(backward_demodulation, [], [f21707, f32259])).
fof(f32259, plain, ((sdtsldt0(xn, xr) = sK20) | ~ spl24_89), inference(subsumption_resolution, [], [f32248, f5733])).
fof(f5733, plain, (aNaturalNumber0(sdtsldt0(xn, xr)) | ~ spl24_89), inference(avatar_component_clause, [], [f5732])).
fof(f5732, plain, (spl24_89 <=> aNaturalNumber0(sdtsldt0(xn, xr))), introduced(avatar_definition, [new_symbols(naming, [spl24_89])])).
fof(f32248, plain, ((sdtsldt0(xn, xr) = sK20) | ~ aNaturalNumber0(sdtsldt0(xn, xr))), inference(trivial_inequality_removal, [], [f32240])).
fof(f32240, plain, (~ (xn = xn) | (sdtsldt0(xn, xr) = sK20) | ~ aNaturalNumber0(sdtsldt0(xn, xr))), inference(superposition, [], [f3653, f353])).
fof(f353, plain, (xn = sdtasdt0(xr, sdtsldt0(xn, xr))), inference(cnf_transformation, [], [f203])).
fof(f203, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ((xn = sdtpldt0(sdtsldt0(xn, xr), sK21)) & aNaturalNumber0(sK21)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK21])], [f141, f202])).
fof(f202, plain, (? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) => ((xn = sdtpldt0(sdtsldt0(xn, xr), sK21)) & aNaturalNumber0(sK21))), introduced(choice_axiom, [])).
fof(f141, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (xn = sdtsldt0(xn, xr)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(flattening, [], [f140])).
fof(f140, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & (~ (xn = sdtsldt0(xn, xr)) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, (sdtlseqdt0(sdtsldt0(xn, xr), xn) & ? [X0] : ((xn = sdtpldt0(sdtsldt0(xn, xr), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (xn = sdtsldt0(xn, xr)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2504)).
fof(f3653, plain, ! [X6] : (~ (xn = sdtasdt0(xr, X6)) | (sK20 = X6) | ~ aNaturalNumber0(X6)), inference(subsumption_resolution, [], [f3652, f322])).
fof(f322, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f188])).
fof(f188, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ((xk = sdtasdt0(xr, sK14)) & aNaturalNumber0(sK14)) & aNaturalNumber0(xr)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f139, f187])).
fof(f187, plain, (? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) => ((xk = sdtasdt0(xr, sK14)) & aNaturalNumber0(sK14))), introduced(choice_axiom, [])).
fof(f139, plain, (isPrime0(xr) & ! [X0] : ((xr = X0) | (sz10 = X0) | (~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(flattening, [], [f138])).
fof(f138, plain, (isPrime0(xr) & ! [X0] : (((xr = X0) | (sz10 = X0)) | ((~ doDivides0(X0, xr) & ! [X1] : (~ (sdtasdt0(X0, X1) = xr) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X2] : ((xk = sdtasdt0(xr, X2)) & aNaturalNumber0(X2)) & aNaturalNumber0(xr)), inference(rectify, [], [f48])).
fof(f48, plain, (isPrime0(xr) & ! [X0] : (((doDivides0(X0, xr) | ? [X1] : ((sdtasdt0(X0, X1) = xr) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xr = X0) | (sz10 = X0))) & ~ (sz10 = xr) & ~ (sz00 = xr) & doDivides0(xr, xk) & ? [X0] : ((xk = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2342)).
fof(f3652, plain, ! [X6] : (~ (xn = sdtasdt0(xr, X6)) | (sK20 = X6) | ~ aNaturalNumber0(X6) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3651, f326])).
fof(f326, plain, ~ (sz00 = xr), inference(cnf_transformation, [], [f188])).
fof(f3651, plain, ! [X6] : (~ (xn = sdtasdt0(xr, X6)) | (sK20 = X6) | ~ aNaturalNumber0(X6) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3621, f346])).
fof(f346, plain, aNaturalNumber0(sK20), inference(cnf_transformation, [], [f201])).
fof(f201, plain, (doDivides0(xr, xn) & ((xn = sdtasdt0(xr, sK20)) & aNaturalNumber0(sK20))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK20])], [f52, f200])).
fof(f200, plain, (? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xn = sdtasdt0(xr, sK20)) & aNaturalNumber0(sK20))), introduced(choice_axiom, [])).
fof(f52, plain, (doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2487)).
fof(f3621, plain, ! [X6] : (~ (xn = sdtasdt0(xr, X6)) | (sK20 = X6) | ~ aNaturalNumber0(X6) | ~ aNaturalNumber0(sK20) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f227, f347])).
fof(f347, plain, (xn = sdtasdt0(xr, sK20)), inference(cnf_transformation, [], [f201])).
fof(f227, plain, ! [X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ! [X0] : (! [X1, X2] : ((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1)) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f86])).
fof(f86, plain, ! [X0] : ((! [X1, X2] : (((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1))) | (sz00 = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (aNaturalNumber0(X0) => (~ (sz00 = X0) => ! [X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1)) => (((sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) | (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) => (X1 = X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mMulCanc)).
fof(f21707, plain, (iLess0(sdtpldt0(xp, sdtpldt0(xm, sdtsldt0(xn, xr))), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ spl24_79 | ~ spl24_89 | ~ spl24_412)), inference(backward_demodulation, [], [f20985, f21662])).
fof(f21662, plain, ((sdtpldt0(sdtpldt0(xm, sdtsldt0(xn, xr)), xp) = sdtpldt0(xp, sdtpldt0(xm, sdtsldt0(xn, xr)))) | (~ spl24_79 | ~ spl24_89)), inference(resolution, [], [f663, f20981])).
fof(f20981, plain, (aNaturalNumber0(sdtpldt0(xm, sdtsldt0(xn, xr))) | (~ spl24_79 | ~ spl24_89)), inference(backward_demodulation, [], [f5003, f20941])).
fof(f20941, plain, ((sdtpldt0(sdtsldt0(xn, xr), xm) = sdtpldt0(xm, sdtsldt0(xn, xr))) | ~ spl24_89), inference(resolution, [], [f662, f5733])).
fof(f662, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtpldt0(X12, xm) = sdtpldt0(xm, X12))), inference(resolution, [], [f213, f277])).
fof(f277, plain, aNaturalNumber0(xm), inference(cnf_transformation, [], [f39])).
fof(f5003, plain, (aNaturalNumber0(sdtpldt0(sdtsldt0(xn, xr), xm)) | ~ spl24_79), inference(avatar_component_clause, [], [f5002])).
fof(f5002, plain, (spl24_79 <=> aNaturalNumber0(sdtpldt0(sdtsldt0(xn, xr), xm))), introduced(avatar_definition, [new_symbols(naming, [spl24_79])])).
fof(f20985, plain, (iLess0(sdtpldt0(sdtpldt0(xm, sdtsldt0(xn, xr)), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ spl24_89 | ~ spl24_412)), inference(backward_demodulation, [], [f15769, f20941])).
fof(f15769, plain, (iLess0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ spl24_412), inference(avatar_component_clause, [], [f15767])).
fof(f15767, plain, (spl24_412 <=> iLess0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp))), introduced(avatar_definition, [new_symbols(naming, [spl24_412])])).
fof(f470641, plain, (~ iLess0(sdtpldt0(xp, sdtpldt0(xm, sK20)), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl24_2 | spl24_29 | ~ spl24_79 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1362 | ~ spl24_1976 | spl24_2928)), inference(forward_demodulation, [], [f470640, f32423])).
fof(f32423, plain, ((sdtpldt0(sdtpldt0(xm, sK20), xp) = sdtpldt0(xp, sdtpldt0(xm, sK20))) | (~ spl24_79 | ~ spl24_89)), inference(backward_demodulation, [], [f21662, f32259])).
fof(f470640, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1362 | ~ spl24_1976 | spl24_2928)), inference(subsumption_resolution, [], [f470639, f277])).
fof(f470639, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ aNaturalNumber0(xm) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1362 | ~ spl24_1976 | spl24_2928)), inference(subsumption_resolution, [], [f470638, f187651])).
fof(f187651, plain, (aNaturalNumber0(sK20) | ~ spl24_1362), inference(avatar_component_clause, [], [f187650])).
fof(f187650, plain, (spl24_1362 <=> aNaturalNumber0(sK20)), introduced(avatar_definition, [new_symbols(naming, [spl24_1362])])).
fof(f470638, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ aNaturalNumber0(sK20) | ~ aNaturalNumber0(xm) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976 | spl24_2928)), inference(subsumption_resolution, [], [f470637, f32264])).
fof(f32264, plain, (~ doDivides0(xp, sK20) | ~ spl24_89), inference(backward_demodulation, [], [f373, f32259])).
fof(f373, plain, ~ doDivides0(xp, sdtsldt0(xn, xr)), inference(cnf_transformation, [], [f145])).
fof(f145, plain, (~ doDivides0(xp, xm) & ! [X0] : (~ (xm = sdtasdt0(xp, X0)) | ~ aNaturalNumber0(X0)) & ~ doDivides0(xp, sdtsldt0(xn, xr)) & ! [X1] : (~ (sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) | ~ aNaturalNumber0(X1)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(flattening, [], [f144])).
fof(f144, plain, (~ doDivides0(xp, xm) & ! [X0] : (~ (xm = sdtasdt0(xp, X0)) | ~ aNaturalNumber0(X0)) & ((~ doDivides0(xp, sdtsldt0(xn, xr)) & ! [X1] : (~ (sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) | ~ aNaturalNumber0(X1))) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (doDivides0(xp, sdtsldt0(xn, xr)) | ? [X1] : ((sdtsldt0(xn, xr) = sdtasdt0(xp, X1)) & aNaturalNumber0(X1))))), inference(rectify, [], [f57])).
fof(f57, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (doDivides0(xp, sdtsldt0(xn, xr)) | ? [X0] : ((sdtasdt0(xp, X0) = sdtsldt0(xn, xr)) & aNaturalNumber0(X0))))), inference(negated_conjecture, [], [f56])).
fof(f56, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (doDivides0(xp, sdtsldt0(xn, xr)) | ? [X0] : ((sdtasdt0(xp, X0) = sdtsldt0(xn, xr)) & aNaturalNumber0(X0))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__)).
fof(f470637, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | doDivides0(xp, sK20) | ~ aNaturalNumber0(sK20) | ~ aNaturalNumber0(xm) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976 | spl24_2928)), inference(subsumption_resolution, [], [f470633, f379620])).
fof(f379620, plain, (~ sP1(xm, xp) | spl24_2928), inference(avatar_component_clause, [], [f379619])).
fof(f379619, plain, (spl24_2928 <=> sP1(xm, xp)), introduced(avatar_definition, [new_symbols(naming, [spl24_2928])])).
fof(f470633, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(xm, xp) | doDivides0(xp, sK20) | ~ aNaturalNumber0(sK20) | ~ aNaturalNumber0(xm) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976)), inference(trivial_inequality_removal, [], [f470606])).
fof(f470606, plain, (~ (sK15 = sK15) | ~ iLess0(sdtpldt0(sdtpldt0(xm, sK20), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(xm, xp) | doDivides0(xp, sK20) | ~ aNaturalNumber0(sK20) | ~ aNaturalNumber0(xm) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976)), inference(superposition, [], [f438150, f438147])).
fof(f438147, plain, ((sK15 = sdtasdt0(xm, sK20)) | (~ spl24_2 | ~ spl24_89 | ~ spl24_401 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976)), inference(forward_demodulation, [], [f40960, f437740])).
fof(f437740, plain, ((sK15 = sdtsldt0(sdtasdt0(xn, xm), xr)) | (~ spl24_401 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976)), inference(backward_demodulation, [], [f427137, f324260])).
fof(f324260, plain, ((sK15 = sdtasdt0(xp, sK14)) | (~ spl24_748 | ~ spl24_1344 | ~ spl24_1976)), inference(forward_demodulation, [], [f324259, f61699])).
fof(f61699, plain, ((sK14 = sK4(xp, sK15)) | ~ spl24_748), inference(avatar_component_clause, [], [f61697])).
fof(f61697, plain, (spl24_748 <=> (sK14 = sK4(xp, sK15))), introduced(avatar_definition, [new_symbols(naming, [spl24_748])])).
fof(f324259, plain, ((sK15 = sdtasdt0(xp, sK4(xp, sK15))) | (~ spl24_1344 | ~ spl24_1976)), inference(subsumption_resolution, [], [f324258, f278])).
fof(f324258, plain, ((sK15 = sdtasdt0(xp, sK4(xp, sK15))) | ~ aNaturalNumber0(xp) | (~ spl24_1344 | ~ spl24_1976)), inference(subsumption_resolution, [], [f324220, f180266])).
fof(f180266, plain, (aNaturalNumber0(sK15) | ~ spl24_1344), inference(avatar_component_clause, [], [f180265])).
fof(f180265, plain, (spl24_1344 <=> aNaturalNumber0(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl24_1344])])).
fof(f324220, plain, ((sK15 = sdtasdt0(xp, sK4(xp, sK15))) | ~ aNaturalNumber0(sK15) | ~ aNaturalNumber0(xp) | ~ spl24_1976), inference(resolution, [], [f266488, f256])).
fof(f256, plain, ! [X0, X1] : (~ doDivides0(X0, X1) | (sdtasdt0(X0, sK4(X0, X1)) = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, sK4(X0, X1)) = X1) & aNaturalNumber0(sK4(X0, X1))) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f158, f159])).
fof(f159, plain, ! [X1, X0] : (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X0, sK4(X0, X1)) = X1) & aNaturalNumber0(sK4(X0, X1)))), introduced(choice_axiom, [])).
fof(f158, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f157])).
fof(f157, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f114])).
fof(f114, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f113])).
fof(f113, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mDefDiv)).
fof(f266488, plain, (doDivides0(xp, sK15) | ~ spl24_1976), inference(avatar_component_clause, [], [f266486])).
fof(f266486, plain, (spl24_1976 <=> doDivides0(xp, sK15)), introduced(avatar_definition, [new_symbols(naming, [spl24_1976])])).
fof(f427137, plain, ((sdtsldt0(sdtasdt0(xn, xm), xr) = sdtasdt0(xp, sK14)) | ~ spl24_401), inference(forward_demodulation, [], [f427091, f316])).
fof(f316, plain, (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((xk = sdtsldt0(sdtasdt0(xn, xm), xp)) & (sdtasdt0(xn, xm) = sdtasdt0(xp, xk)) & aNaturalNumber0(xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2306)).
fof(f427091, plain, ((sdtasdt0(xp, sK14) = sdtsldt0(sdtasdt0(xp, xk), xr)) | ~ spl24_401), inference(resolution, [], [f32036, f278])).
fof(f32036, plain, (! [X13] : (~ aNaturalNumber0(X13) | (sdtsldt0(sdtasdt0(X13, xk), xr) = sdtasdt0(X13, sK14))) | ~ spl24_401), inference(backward_demodulation, [], [f4861, f32034])).
fof(f32034, plain, ((sK14 = sdtsldt0(xk, xr)) | ~ spl24_401), inference(subsumption_resolution, [], [f32013, f15704])).
fof(f15704, plain, (aNaturalNumber0(sdtsldt0(xk, xr)) | ~ spl24_401), inference(avatar_component_clause, [], [f15703])).
fof(f15703, plain, (spl24_401 <=> aNaturalNumber0(sdtsldt0(xk, xr))), introduced(avatar_definition, [new_symbols(naming, [spl24_401])])).
fof(f32013, plain, ((sK14 = sdtsldt0(xk, xr)) | ~ aNaturalNumber0(sdtsldt0(xk, xr))), inference(trivial_inequality_removal, [], [f32004])).
fof(f32004, plain, (~ (xk = xk) | (sK14 = sdtsldt0(xk, xr)) | ~ aNaturalNumber0(sdtsldt0(xk, xr))), inference(superposition, [], [f3644, f3527])).
fof(f3527, plain, (xk = sdtasdt0(xr, sdtsldt0(xk, xr))), inference(subsumption_resolution, [], [f3526, f322])).
fof(f3526, plain, ((xk = sdtasdt0(xr, sdtsldt0(xk, xr))) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3525, f315])).
fof(f315, plain, aNaturalNumber0(xk), inference(cnf_transformation, [], [f45])).
fof(f3525, plain, ((xk = sdtasdt0(xr, sdtsldt0(xk, xr))) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3497, f326])).
fof(f3497, plain, ((xk = sdtasdt0(xr, sdtsldt0(xk, xr))) | (sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(resolution, [], [f384, f325])).
fof(f325, plain, doDivides0(xr, xk), inference(cnf_transformation, [], [f188])).
fof(f384, plain, ! [X0, X1] : (~ doDivides0(X0, X1) | (sdtasdt0(X0, sdtsldt0(X1, X0)) = X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f259])).
fof(f259, plain, ! [X2, X0, X1] : ((sdtasdt0(X0, X2) = X1) | ~ (sdtsldt0(X1, X0) = X2) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, ! [X0, X1] : (! [X2] : (((sdtsldt0(X1, X0) = X2) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2)) & (((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtsldt0(X1, X0) = X2))) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f161])).
fof(f161, plain, ! [X0, X1] : (! [X2] : (((sdtsldt0(X1, X0) = X2) | (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtsldt0(X1, X0) = X2))) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1] : (! [X2] : ((sdtsldt0(X1, X0) = X2) <=> ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f115])).
fof(f115, plain, ! [X0, X1] : ((! [X2] : ((sdtsldt0(X1, X0) = X2) <=> ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ doDivides0(X0, X1) | (sz00 = X0))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X0, X1) & ~ (sz00 = X0)) => ! [X2] : ((sdtsldt0(X1, X0) = X2) <=> ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mDefQuot)).
fof(f3644, plain, ! [X3] : (~ (xk = sdtasdt0(xr, X3)) | (sK14 = X3) | ~ aNaturalNumber0(X3)), inference(subsumption_resolution, [], [f3643, f322])).
fof(f3643, plain, ! [X3] : (~ (xk = sdtasdt0(xr, X3)) | (sK14 = X3) | ~ aNaturalNumber0(X3) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3642, f326])).
fof(f3642, plain, ! [X3] : (~ (xk = sdtasdt0(xr, X3)) | (sK14 = X3) | ~ aNaturalNumber0(X3) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f3618, f323])).
fof(f323, plain, aNaturalNumber0(sK14), inference(cnf_transformation, [], [f188])).
fof(f3618, plain, ! [X3] : (~ (xk = sdtasdt0(xr, X3)) | (sK14 = X3) | ~ aNaturalNumber0(X3) | ~ aNaturalNumber0(sK14) | (sz00 = xr) | ~ aNaturalNumber0(xr)), inference(superposition, [], [f227, f324])).
fof(f324, plain, (xk = sdtasdt0(xr, sK14)), inference(cnf_transformation, [], [f188])).
fof(f4861, plain, ! [X13] : (~ aNaturalNumber0(X13) | (sdtasdt0(X13, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X13, xk), xr))), inference(subsumption_resolution, [], [f4860, f322])).
fof(f4860, plain, ! [X13] : (~ aNaturalNumber0(X13) | (sdtasdt0(X13, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X13, xk), xr)) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f4859, f315])).
fof(f4859, plain, ! [X13] : (~ aNaturalNumber0(X13) | (sdtasdt0(X13, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X13, xk), xr)) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(subsumption_resolution, [], [f4828, f326])).
fof(f4828, plain, ! [X13] : (~ aNaturalNumber0(X13) | (sdtasdt0(X13, sdtsldt0(xk, xr)) = sdtsldt0(sdtasdt0(X13, xk), xr)) | (sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr)), inference(resolution, [], [f265, f325])).
fof(f265, plain, ! [X2, X0, X1] : (~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | (sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0)) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0, X1] : (! [X2] : ((sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0)) | ~ aNaturalNumber0(X2)) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f125])).
fof(f125, plain, ! [X0, X1] : ((! [X2] : ((sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0)) | ~ aNaturalNumber0(X2)) | (~ doDivides0(X0, X1) | (sz00 = X0))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X0, X1) & ~ (sz00 = X0)) => ! [X2] : (aNaturalNumber0(X2) => (sdtasdt0(X2, sdtsldt0(X1, X0)) = sdtsldt0(sdtasdt0(X2, X1), X0))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mDivAsso)).
fof(f40960, plain, ((sdtasdt0(xm, sK20) = sdtsldt0(sdtasdt0(xn, xm), xr)) | (~ spl24_2 | ~ spl24_89)), inference(forward_demodulation, [], [f40921, f22621])).
fof(f22621, plain, (sdtasdt0(xn, xm) = sdtasdt0(xm, xn)), inference(resolution, [], [f688, f277])).
fof(f688, plain, ! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, xn) = sdtasdt0(xn, X11))), inference(resolution, [], [f217, f276])).
fof(f276, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f217, plain, ! [X0, X1] : (~ aNaturalNumber0(X1) | (sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f76])).
fof(f76, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtasdt0(X0, X1) = sdtasdt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mMulComm)).
fof(f40921, plain, ((sdtasdt0(xm, sK20) = sdtsldt0(sdtasdt0(xm, xn), xr)) | (~ spl24_2 | ~ spl24_89)), inference(resolution, [], [f32291, f277])).
fof(f32291, plain, (! [X11] : (~ aNaturalNumber0(X11) | (sdtsldt0(sdtasdt0(X11, xn), xr) = sdtasdt0(X11, sK20))) | (~ spl24_2 | ~ spl24_89)), inference(backward_demodulation, [], [f4855, f32259])).
fof(f4855, plain, (! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X11, xn), xr))) | ~ spl24_2), inference(subsumption_resolution, [], [f4854, f322])).
fof(f4854, plain, (! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X11, xn), xr)) | ~ aNaturalNumber0(xr)) | ~ spl24_2), inference(subsumption_resolution, [], [f4853, f276])).
fof(f4853, plain, (! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X11, xn), xr)) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(xr)) | ~ spl24_2), inference(subsumption_resolution, [], [f4826, f326])).
fof(f4826, plain, (! [X11] : (~ aNaturalNumber0(X11) | (sdtasdt0(X11, sdtsldt0(xn, xr)) = sdtsldt0(sdtasdt0(X11, xn), xr)) | (sz00 = xr) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(xr)) | ~ spl24_2), inference(resolution, [], [f265, f404])).
fof(f404, plain, (doDivides0(xr, xn) | ~ spl24_2), inference(avatar_component_clause, [], [f402])).
fof(f402, plain, (spl24_2 <=> doDivides0(xr, xn)), introduced(avatar_definition, [new_symbols(naming, [spl24_2])])).
fof(f438150, plain, (! [X19, X18] : (~ (sK15 = sdtasdt0(X18, X19)) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(X18, xp) | doDivides0(xp, X19) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_2 | spl24_29 | ~ spl24_89 | ~ spl24_401 | ~ spl24_683 | ~ spl24_748 | ~ spl24_1344 | ~ spl24_1976)), inference(backward_demodulation, [], [f437634, f438147])).
fof(f437634, plain, (! [X19, X18] : (~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | doDivides0(xp, X19) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (spl24_29 | ~ spl24_89 | ~ spl24_683)), inference(forward_demodulation, [], [f437633, f338492])).
fof(f338492, plain, ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ spl24_683), inference(resolution, [], [f45501, f663])).
fof(f437633, plain, (! [X19, X18] : (~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (spl24_29 | ~ spl24_89)), inference(subsumption_resolution, [], [f380474, f1220])).
fof(f1220, plain, (~ sP0(xp) | spl24_29), inference(avatar_component_clause, [], [f1218])).
fof(f1218, plain, (spl24_29 <=> sP0(xp)), introduced(avatar_definition, [new_symbols(naming, [spl24_29])])).
fof(f380474, plain, (! [X19, X18] : (~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | ~ spl24_89), inference(subsumption_resolution, [], [f33224, f278])).
fof(f33224, plain, (! [X19, X18] : (~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | sP0(xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | ~ spl24_89), inference(subsumption_resolution, [], [f33189, f359])).
fof(f359, plain, aNaturalNumber0(sK22), inference(cnf_transformation, [], [f205])).
fof(f205, plain, (doDivides0(xp, sdtasdt0(sdtsldt0(xn, xr), xm)) & ((sdtasdt0(sdtsldt0(xn, xr), xm) = sdtasdt0(xp, sK22)) & aNaturalNumber0(sK22)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22])], [f54, f204])).
fof(f204, plain, (? [X0] : ((sdtasdt0(xp, X0) = sdtasdt0(sdtsldt0(xn, xr), xm)) & aNaturalNumber0(X0)) => ((sdtasdt0(sdtsldt0(xn, xr), xm) = sdtasdt0(xp, sK22)) & aNaturalNumber0(sK22))), introduced(choice_axiom, [])).
fof(f54, plain, (doDivides0(xp, sdtasdt0(sdtsldt0(xn, xr), xm)) & ? [X0] : ((sdtasdt0(xp, X0) = sdtasdt0(sdtsldt0(xn, xr), xm)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2529)).
fof(f33189, plain, (! [X19, X18] : (~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | ~ aNaturalNumber0(sK22) | sP0(xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | ~ spl24_89), inference(superposition, [], [f293, f32600])).
fof(f32600, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xm, sK20)) | ~ spl24_89), inference(forward_demodulation, [], [f32262, f22749])).
fof(f22749, plain, (sdtasdt0(xm, sK20) = sdtasdt0(sK20, xm)), inference(resolution, [], [f689, f346])).
fof(f689, plain, ! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, xm) = sdtasdt0(xm, X12))), inference(resolution, [], [f217, f277])).
fof(f32262, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(sK20, xm)) | ~ spl24_89), inference(backward_demodulation, [], [f360, f32259])).
fof(f360, plain, (sdtasdt0(sdtsldt0(xn, xr), xm) = sdtasdt0(xp, sK22)), inference(cnf_transformation, [], [f205])).
fof(f293, plain, ! [X4, X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(X2, X1) | ~ aNaturalNumber0(X4) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ((sdtasdt0(X2, sK10(X1, X2)) = X1) & aNaturalNumber0(sK10(X1, X2)))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X4] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ aNaturalNumber0(X4))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f179, f180])).
fof(f180, plain, ! [X2, X1] : (? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X2, sK10(X1, X2)) = X1) & aNaturalNumber0(sK10(X1, X2)))), introduced(choice_axiom, [])).
fof(f179, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X4] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X4)) | ~ aNaturalNumber0(X4))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f148])).
fof(f148, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | sP1(X0, X2) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | sP0(X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(definition_folding, [], [f132, e147, e146])).
fof(f146, plain, ! [X2] : ((~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ sP0(X2)), inference(usedef, [], [e146])).
fof(e146, plain, ! [X2] : (sP0(X2) <=> (~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f147, plain, ! [X0, X2] : ((doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ sP1(X0, X2)), inference(usedef, [], [e147])).
fof(e147, plain, ! [X0, X2] : (sP1(X0, X2) <=> (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f132, plain, ! [X0, X1, X2] : ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) | (~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | (~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f131])).
fof(f131, plain, ! [X0, X1, X2] : (((((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7)))) | ~ iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp))) | ((~ doDivides0(X2, sdtasdt0(X0, X1)) & ! [X3] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | (~ isPrime0(X2) & (? [X4] : ((~ (X2 = X4) & ~ (sz10 = X4)) & (doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4))) | (sz10 = X2) | (sz00 = X2))))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((doDivides0(X2, sdtasdt0(X0, X1)) | ? [X3] : ((sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) & aNaturalNumber0(X3))) & (isPrime0(X2) | (! [X4] : ((doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) => ((X2 = X4) | (sz10 = X4))) & ~ (sz10 = X2) & ~ (sz00 = X2)))) => (iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) => ((doDivides0(X2, X1) & ? [X6] : ((sdtasdt0(X2, X6) = X1) & aNaturalNumber0(X6))) | (doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))))))), inference(rectify, [], [f40])).
fof(f40, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (((doDivides0(X2, sdtasdt0(X0, X1)) | ? [X3] : ((sdtasdt0(X0, X1) = sdtasdt0(X2, X3)) & aNaturalNumber0(X3))) & (isPrime0(X2) | (! [X3] : ((doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2)))) => (iLess0(sdtpldt0(sdtpldt0(X0, X1), X2), sdtpldt0(sdtpldt0(xn, xm), xp)) => ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) | (doDivides0(X2, X0) & ? [X3] : ((sdtasdt0(X2, X3) = X0) & aNaturalNumber0(X3))))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__1799)).
fof(f436914, plain, (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_79 | ~ spl24_89 | ~ spl24_93 | ~ spl24_412 | ~ spl24_683 | spl24_2928 | ~ spl24_2966), inference(avatar_contradiction_clause, [], [f436913])).
fof(f436913, plain, ($false | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_79 | ~ spl24_89 | ~ spl24_93 | ~ spl24_412 | ~ spl24_683 | spl24_2928 | ~ spl24_2966)), inference(subsumption_resolution, [], [f436912, f334358])).
fof(f334358, plain, (iLess0(sdtpldt0(xp, sdtpldt0(xm, sK18)), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl24_3 | ~ spl24_4 | ~ spl24_79 | ~ spl24_89 | ~ spl24_93 | ~ spl24_412 | ~ spl24_683)), inference(forward_demodulation, [], [f50361, f278330])).
fof(f278330, plain, ((sK18 = sK20) | (~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(backward_demodulation, [], [f32259, f278329])).
fof(f278329, plain, ((sdtsldt0(xn, xr) = sK18) | (~ spl24_3 | ~ spl24_4 | ~ spl24_93)), inference(forward_demodulation, [], [f62921, f409])).
fof(f409, plain, ((xn = sdtasdt0(xr, sK18)) | ~ spl24_3), inference(avatar_component_clause, [], [f407])).
fof(f407, plain, (spl24_3 <=> (xn = sdtasdt0(xr, sK18))), introduced(avatar_definition, [new_symbols(naming, [spl24_3])])).
fof(f62921, plain, ((sK18 = sdtsldt0(sdtasdt0(xr, sK18), xr)) | (~ spl24_4 | ~ spl24_93)), inference(subsumption_resolution, [], [f62881, f326])).
fof(f62881, plain, ((sz00 = xr) | (sK18 = sdtsldt0(sdtasdt0(xr, sK18), xr)) | (~ spl24_4 | ~ spl24_93)), inference(resolution, [], [f9611, f5753])).
fof(f5753, plain, (aNaturalNumber0(xr) | ~ spl24_93), inference(avatar_component_clause, [], [f5752])).
fof(f5752, plain, (spl24_93 <=> aNaturalNumber0(xr)), introduced(avatar_definition, [new_symbols(naming, [spl24_93])])).
fof(f9611, plain, (! [X11] : (~ aNaturalNumber0(X11) | (sz00 = X11) | (sK18 = sdtsldt0(sdtasdt0(X11, sK18), X11))) | ~ spl24_4), inference(resolution, [], [f414, f394])).
fof(f394, plain, ! [X2, X0] : (~ aNaturalNumber0(X2) | (sdtsldt0(sdtasdt0(X0, X2), X0) = X2) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f393, f212])).
fof(f212, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f69])).
fof(f69, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mSortsB_02)).
fof(f393, plain, ! [X2, X0] : ((sdtsldt0(sdtasdt0(X0, X2), X0) = X2) | ~ aNaturalNumber0(X2) | (sz00 = X0) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f383, f392])).
fof(f392, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f382, f212])).
fof(f382, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f257])).
fof(f257, plain, ! [X2, X0, X1] : (doDivides0(X0, X1) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f160])).
fof(f383, plain, ! [X2, X0] : ((sdtsldt0(sdtasdt0(X0, X2), X0) = X2) | ~ aNaturalNumber0(X2) | ~ doDivides0(X0, sdtasdt0(X0, X2)) | (sz00 = X0) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f260])).
fof(f260, plain, ! [X2, X0, X1] : ((sdtsldt0(X1, X0) = X2) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f162])).
fof(f414, plain, (aNaturalNumber0(sK18) | ~ spl24_4), inference(avatar_component_clause, [], [f412])).
fof(f412, plain, (spl24_4 <=> aNaturalNumber0(sK18)), introduced(avatar_definition, [new_symbols(naming, [spl24_4])])).
fof(f436912, plain, (~ iLess0(sdtpldt0(xp, sdtpldt0(xm, sK18)), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_79 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | spl24_2928 | ~ spl24_2966)), inference(forward_demodulation, [], [f436911, f284434])).
fof(f284434, plain, ((sdtpldt0(xp, sdtpldt0(xm, sK18)) = sdtpldt0(sdtpldt0(xm, sK18), xp)) | (~ spl24_3 | ~ spl24_4 | ~ spl24_79 | ~ spl24_89 | ~ spl24_93)), inference(resolution, [], [f278490, f663])).
fof(f278490, plain, (aNaturalNumber0(sdtpldt0(xm, sK18)) | (~ spl24_3 | ~ spl24_4 | ~ spl24_79 | ~ spl24_89 | ~ spl24_93)), inference(backward_demodulation, [], [f32403, f278330])).
fof(f32403, plain, (aNaturalNumber0(sdtpldt0(xm, sK20)) | (~ spl24_79 | ~ spl24_89)), inference(backward_demodulation, [], [f20981, f32259])).
fof(f436911, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK18), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | spl24_2928 | ~ spl24_2966)), inference(subsumption_resolution, [], [f436910, f379620])).
fof(f436910, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK18), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | sP1(xm, xp) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | ~ spl24_2966)), inference(subsumption_resolution, [], [f436909, f278473])).
fof(f278473, plain, (~ doDivides0(xp, sK18) | (~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(backward_demodulation, [], [f32264, f278330])).
fof(f436909, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK18), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | doDivides0(xp, sK18) | sP1(xm, xp) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | ~ spl24_2966)), inference(subsumption_resolution, [], [f436908, f414])).
fof(f436908, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK18), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ aNaturalNumber0(sK18) | doDivides0(xp, sK18) | sP1(xm, xp) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | ~ spl24_2966)), inference(subsumption_resolution, [], [f436860, f277])).
fof(f436860, plain, (~ iLess0(sdtpldt0(sdtpldt0(xm, sK18), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(sK18) | doDivides0(xp, sK18) | sP1(xm, xp) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | ~ spl24_2966)), inference(trivial_inequality_removal, [], [f436852])).
fof(f436852, plain, (~ (sK15 = sK15) | ~ iLess0(sdtpldt0(sdtpldt0(xm, sK18), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(sK18) | doDivides0(xp, sK18) | sP1(xm, xp) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_683 | ~ spl24_2966)), inference(superposition, [], [f381685, f279080])).
fof(f279080, plain, ((sK15 = sdtasdt0(xm, sK18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(backward_demodulation, [], [f203301, f278330])).
fof(f203301, plain, ((sK15 = sdtasdt0(xm, sK20)) | (~ spl24_2 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f203300, f60354])).
fof(f60354, plain, ((sK15 = sdtsldt0(sdtasdt0(xn, xm), xr)) | ~ spl24_93), inference(forward_demodulation, [], [f60353, f334])).
fof(f334, plain, (sdtasdt0(xn, xm) = sdtasdt0(xr, sK15)), inference(cnf_transformation, [], [f191])).
fof(f191, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15)) & ((xk = sdtpldt0(xr, sK16)) & aNaturalNumber0(sK16))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16])], [f64, f190, f189])).
fof(f189, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xr, sK15)) & aNaturalNumber0(sK15))), introduced(choice_axiom, [])).
fof(f190, plain, (? [X1] : ((xk = sdtpldt0(xr, X1)) & aNaturalNumber0(X1)) => ((xk = sdtpldt0(xr, sK16)) & aNaturalNumber0(sK16))), introduced(choice_axiom, [])).
fof(f64, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & ? [X1] : ((xk = sdtpldt0(xr, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f49])).
fof(f49, plain, (doDivides0(xr, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) & ? [X0] : ((xk = sdtpldt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2362)).
fof(f60353, plain, ((sK15 = sdtsldt0(sdtasdt0(xr, sK15), xr)) | ~ spl24_93), inference(subsumption_resolution, [], [f60310, f326])).
fof(f60310, plain, ((sz00 = xr) | (sK15 = sdtsldt0(sdtasdt0(xr, sK15), xr)) | ~ spl24_93), inference(resolution, [], [f2435, f5753])).
fof(f2435, plain, ! [X41] : (~ aNaturalNumber0(X41) | (sz00 = X41) | (sK15 = sdtsldt0(sdtasdt0(X41, sK15), X41))), inference(resolution, [], [f394, f333])).
fof(f333, plain, aNaturalNumber0(sK15), inference(cnf_transformation, [], [f191])).
fof(f203300, plain, ((sdtasdt0(xm, sK20) = sdtsldt0(sdtasdt0(xn, xm), xr)) | (~ spl24_2 | ~ spl24_89)), inference(forward_demodulation, [], [f203264, f22621])).
fof(f203264, plain, ((sdtasdt0(xm, sK20) = sdtsldt0(sdtasdt0(xm, xn), xr)) | (~ spl24_2 | ~ spl24_89)), inference(resolution, [], [f32291, f277])).
fof(f381685, plain, (! [X19, X18] : (~ (sK15 = sdtasdt0(X18, X19)) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(xp, sdtpldt0(xn, xm))) | ~ aNaturalNumber0(X18) | ~ aNaturalNumber0(X19) | doDivides0(xp, X19) | sP1(X18, xp)) | (~ spl24_683 | ~ spl24_2966)), inference(forward_demodulation, [], [f380883, f338492])).
fof(f380883, plain, (! [X19, X18] : (~ (sK15 = sdtasdt0(X18, X19)) | ~ aNaturalNumber0(X18) | ~ aNaturalNumber0(X19) | doDivides0(xp, X19) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | sP1(X18, xp)) | ~ spl24_2966), inference(avatar_component_clause, [], [f380882])).
fof(f380882, plain, (spl24_2966 <=> ! [X18, X19] : (~ (sK15 = sdtasdt0(X18, X19)) | ~ aNaturalNumber0(X18) | ~ aNaturalNumber0(X19) | doDivides0(xp, X19) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | sP1(X18, xp))), introduced(avatar_definition, [new_symbols(naming, [spl24_2966])])).
fof(f380941, plain, (spl24_29 | spl24_2966 | ~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_401), inference(avatar_split_clause, [], [f380940, f15703, f5752, f5732, f412, f407, f402, f380882, f1218])).
fof(f380940, plain, (! [X19, X18] : (~ (sK15 = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_401)), inference(subsumption_resolution, [], [f380939, f323])).
fof(f380939, plain, (! [X19, X18] : (~ aNaturalNumber0(sK14) | ~ (sK15 = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_401)), inference(forward_demodulation, [], [f380938, f318723])).
fof(f318723, plain, ((sK14 = sdtsldt0(sK15, xp)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_401)), inference(backward_demodulation, [], [f250584, f318719])).
fof(f318719, plain, ((sK15 = sdtasdt0(xp, sK14)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93 | ~ spl24_401)), inference(backward_demodulation, [], [f250408, f318718])).
fof(f318718, plain, ((sK15 = sdtsldt0(sdtasdt0(xn, xm), xr)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f318717, f279080])).
fof(f318717, plain, ((sdtsldt0(sdtasdt0(xn, xm), xr) = sdtasdt0(xm, sK18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f43556, f278330])).
fof(f43556, plain, ((sdtasdt0(xm, sK20) = sdtsldt0(sdtasdt0(xn, xm), xr)) | (~ spl24_2 | ~ spl24_89)), inference(forward_demodulation, [], [f43516, f22621])).
fof(f43516, plain, ((sdtasdt0(xm, sK20) = sdtsldt0(sdtasdt0(xm, xn), xr)) | (~ spl24_2 | ~ spl24_89)), inference(resolution, [], [f32291, f277])).
fof(f250408, plain, ((sdtsldt0(sdtasdt0(xn, xm), xr) = sdtasdt0(xp, sK14)) | ~ spl24_401), inference(forward_demodulation, [], [f250373, f316])).
fof(f250373, plain, ((sdtasdt0(xp, sK14) = sdtsldt0(sdtasdt0(xp, xk), xr)) | ~ spl24_401), inference(resolution, [], [f32036, f278])).
fof(f250584, plain, (sK14 = sdtsldt0(sdtasdt0(xp, sK14), xp)), inference(subsumption_resolution, [], [f250551, f295])).
fof(f295, plain, ~ (sz00 = xp), inference(cnf_transformation, [], [f183])).
fof(f183, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK11)) & aNaturalNumber0(sK11)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f134, f182])).
fof(f182, plain, (? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((sdtasdt0(xn, xm) = sdtasdt0(xp, sK11)) & aNaturalNumber0(sK11))), introduced(choice_axiom, [])).
fof(f134, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : ((xp = X1) | (sz10 = X1) | (~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(flattening, [], [f133])).
fof(f133, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((xp = X1) | (sz10 = X1)) | ((~ doDivides0(X1, xp) & ! [X2] : (~ (sdtasdt0(X1, X2) = xp) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(ennf_transformation, [], [f61])).
fof(f61, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X1] : (((doDivides0(X1, xp) | ? [X2] : ((sdtasdt0(X1, X2) = xp) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)) => ((xp = X1) | (sz10 = X1))) & ~ (sz10 = xp) & ~ (sz00 = xp)), inference(rectify, [], [f41])).
fof(f41, plain, (doDivides0(xp, sdtasdt0(xn, xm)) & ? [X0] : ((sdtasdt0(xn, xm) = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) & isPrime0(xp) & ! [X0] : (((doDivides0(X0, xp) | ? [X1] : ((sdtasdt0(X0, X1) = xp) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xp = X0) | (sz10 = X0))) & ~ (sz10 = xp) & ~ (sz00 = xp)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__1860)).
fof(f250551, plain, ((sz00 = xp) | (sK14 = sdtsldt0(sdtasdt0(xp, sK14), xp))), inference(resolution, [], [f2434, f278])).
fof(f2434, plain, ! [X40] : (~ aNaturalNumber0(X40) | (sz00 = X40) | (sK14 = sdtsldt0(sdtasdt0(X40, sK14), X40))), inference(resolution, [], [f394, f323])).
fof(f380938, plain, (! [X19, X18] : (~ aNaturalNumber0(sdtsldt0(sK15, xp)) | ~ (sK15 = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f380937, f279080])).
fof(f380937, plain, (! [X19, X18] : (~ aNaturalNumber0(sdtsldt0(sdtasdt0(xm, sK18), xp)) | ~ (sK15 = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f380341, f278330])).
fof(f380341, plain, (! [X19, X18] : (~ (sK15 = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | ~ aNaturalNumber0(sdtsldt0(sdtasdt0(xm, sK20), xp)) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_2 | ~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f380340, f279080])).
fof(f380340, plain, (! [X19, X18] : (~ (sdtasdt0(X18, X19) = sdtasdt0(xm, sK18)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | ~ aNaturalNumber0(sdtsldt0(sdtasdt0(xm, sK20), xp)) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | (~ spl24_3 | ~ spl24_4 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f380339, f278330])).
fof(f380339, plain, (! [X19, X18] : (~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | ~ aNaturalNumber0(sdtsldt0(sdtasdt0(xm, sK20), xp)) | sP0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | ~ spl24_89), inference(subsumption_resolution, [], [f41198, f278])).
fof(f41198, plain, (! [X19, X18] : (~ (sdtasdt0(xm, sK20) = sdtasdt0(X18, X19)) | sP1(X18, xp) | ~ iLess0(sdtpldt0(sdtpldt0(X18, X19), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | doDivides0(xp, X19) | ~ aNaturalNumber0(sdtsldt0(sdtasdt0(xm, sK20), xp)) | sP0(xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X19) | ~ aNaturalNumber0(X18)) | ~ spl24_89), inference(superposition, [], [f293, f32619])).
fof(f32619, plain, ((sdtasdt0(xm, sK20) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xm, sK20), xp))) | ~ spl24_89), inference(backward_demodulation, [], [f3514, f32600])).
fof(f3514, plain, (sdtasdt0(xp, sK22) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xp, sK22), xp))), inference(subsumption_resolution, [], [f3513, f278])).
fof(f3513, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xp, sK22), xp))) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f3512, f606])).
fof(f606, plain, aNaturalNumber0(sdtasdt0(xp, sK22)), inference(subsumption_resolution, [], [f605, f352])).
fof(f352, plain, aNaturalNumber0(sdtsldt0(xn, xr)), inference(cnf_transformation, [], [f203])).
fof(f605, plain, (aNaturalNumber0(sdtasdt0(xp, sK22)) | ~ aNaturalNumber0(sdtsldt0(xn, xr))), inference(subsumption_resolution, [], [f604, f277])).
fof(f604, plain, (aNaturalNumber0(sdtasdt0(xp, sK22)) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(sdtsldt0(xn, xr))), inference(superposition, [], [f212, f360])).
fof(f3512, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xp, sK22), xp))) | ~ aNaturalNumber0(sdtasdt0(xp, sK22)) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f3492, f295])).
fof(f3492, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xp, sdtsldt0(sdtasdt0(xp, sK22), xp))) | (sz00 = xp) | ~ aNaturalNumber0(sdtasdt0(xp, sK22)) | ~ aNaturalNumber0(xp)), inference(resolution, [], [f384, f432])).
fof(f432, plain, doDivides0(xp, sdtasdt0(xp, sK22)), inference(backward_demodulation, [], [f361, f360])).
fof(f361, plain, doDivides0(xp, sdtasdt0(sdtsldt0(xn, xr), xm)), inference(cnf_transformation, [], [f205])).
fof(f380681, plain, ~ spl24_29, inference(avatar_contradiction_clause, [], [f380680])).
fof(f380680, plain, ($false | ~ spl24_29), inference(subsumption_resolution, [], [f380645, f299])).
fof(f299, plain, isPrime0(xp), inference(cnf_transformation, [], [f183])).
fof(f380645, plain, (~ isPrime0(xp) | ~ spl24_29), inference(resolution, [], [f1219, f288])).
fof(f288, plain, ! [X0] : (~ sP0(X0) | ~ isPrime0(X0)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : ((~ isPrime0(X0) & ((~ (sK8(X0) = X0) & ~ (sz10 = sK8(X0)) & doDivides0(sK8(X0), X0) & ((sdtasdt0(sK8(X0), sK9(X0)) = X0) & aNaturalNumber0(sK9(X0))) & aNaturalNumber0(sK8(X0))) | (sz10 = X0) | (sz00 = X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f175, f177, f176])).
fof(f176, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => (~ (sK8(X0) = X0) & ~ (sz10 = sK8(X0)) & doDivides0(sK8(X0), X0) & ? [X2] : ((sdtasdt0(sK8(X0), X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(sK8(X0)))), introduced(choice_axiom, [])).
fof(f177, plain, ! [X0] : (? [X2] : ((sdtasdt0(sK8(X0), X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(sK8(X0), sK9(X0)) = X0) & aNaturalNumber0(sK9(X0)))), introduced(choice_axiom, [])).
fof(f175, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP0(X0)), inference(rectify, [], [f174])).
fof(f174, plain, ! [X2] : ((~ isPrime0(X2) & (? [X4] : (~ (X2 = X4) & ~ (sz10 = X4) & doDivides0(X4, X2) & ? [X5] : ((sdtasdt0(X4, X5) = X2) & aNaturalNumber0(X5)) & aNaturalNumber0(X4)) | (sz10 = X2) | (sz00 = X2))) | ~ sP0(X2)), inference(nnf_transformation, [], [f146])).
fof(f1219, plain, (sP0(xp) | ~ spl24_29), inference(avatar_component_clause, [], [f1218])).
fof(f379863, plain, ~ spl24_2928, inference(avatar_contradiction_clause, [], [f379862])).
fof(f379862, plain, ($false | ~ spl24_2928), inference(subsumption_resolution, [], [f379847, f375])).
fof(f375, plain, ~ doDivides0(xp, xm), inference(cnf_transformation, [], [f145])).
fof(f379847, plain, (doDivides0(xp, xm) | ~ spl24_2928), inference(resolution, [], [f379621, f281])).
fof(f281, plain, ! [X0, X1] : (~ sP1(X0, X1) | doDivides0(X1, X0)), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ! [X0, X1] : ((doDivides0(X1, X0) & ((sdtasdt0(X1, sK7(X0, X1)) = X0) & aNaturalNumber0(sK7(X0, X1)))) | ~ sP1(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f171, f172])).
fof(f172, plain, ! [X1, X0] : (? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(X1, sK7(X0, X1)) = X0) & aNaturalNumber0(sK7(X0, X1)))), introduced(choice_axiom, [])).
fof(f171, plain, ! [X0, X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2))) | ~ sP1(X0, X1)), inference(rectify, [], [f170])).
fof(f170, plain, ! [X0, X2] : ((doDivides0(X2, X0) & ? [X7] : ((sdtasdt0(X2, X7) = X0) & aNaturalNumber0(X7))) | ~ sP1(X0, X2)), inference(nnf_transformation, [], [f147])).
fof(f379621, plain, (sP1(xm, xp) | ~ spl24_2928), inference(avatar_component_clause, [], [f379619])).
fof(f268292, plain, (spl24_1976 | ~ spl24_2 | ~ spl24_89 | ~ spl24_93), inference(avatar_split_clause, [], [f268291, f5752, f5732, f402, f266486])).
fof(f268291, plain, (doDivides0(xp, sK15) | (~ spl24_2 | ~ spl24_89 | ~ spl24_93)), inference(forward_demodulation, [], [f32601, f203301])).
fof(f32601, plain, (doDivides0(xp, sdtasdt0(xm, sK20)) | ~ spl24_89), inference(backward_demodulation, [], [f432, f32600])).
fof(f267487, plain, (spl24_747 | ~ spl24_2 | ~ spl24_89 | ~ spl24_93 | ~ spl24_647), inference(avatar_split_clause, [], [f267486, f41122, f5752, f5732, f402, f61693])).
fof(f61693, plain, (spl24_747 <=> aNaturalNumber0(sK4(xp, sK15))), introduced(avatar_definition, [new_symbols(naming, [spl24_747])])).
fof(f41122, plain, (spl24_647 <=> aNaturalNumber0(sK4(xp, sdtasdt0(xm, sK20)))), introduced(avatar_definition, [new_symbols(naming, [spl24_647])])).
fof(f267486, plain, (aNaturalNumber0(sK4(xp, sK15)) | (~ spl24_2 | ~ spl24_89 | ~ spl24_93 | ~ spl24_647)), inference(forward_demodulation, [], [f41123, f203301])).
fof(f41123, plain, (aNaturalNumber0(sK4(xp, sdtasdt0(xm, sK20))) | ~ spl24_647), inference(avatar_component_clause, [], [f41122])).
fof(f187844, plain, spl24_1362, inference(avatar_split_clause, [], [f346, f187650])).
fof(f180665, plain, spl24_1344, inference(avatar_split_clause, [], [f333, f180265])).
fof(f61700, plain, (~ spl24_747 | spl24_748 | ~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | ~ spl24_401), inference(avatar_split_clause, [], [f61691, f15703, f15620, f9547, f6529, f5752, f5732, f417, f402, f61697, f61693])).
fof(f417, plain, (spl24_5 <=> doDivides0(xr, xm)), introduced(avatar_definition, [new_symbols(naming, [spl24_5])])).
fof(f6529, plain, (spl24_103 <=> aNaturalNumber0(xn)), introduced(avatar_definition, [new_symbols(naming, [spl24_103])])).
fof(f9547, plain, (spl24_183 <=> ! [X1] : (~ (xm = sdtasdt0(xr, X1)) | ~ aNaturalNumber0(X1) | (sK19 = X1))), introduced(avatar_definition, [new_symbols(naming, [spl24_183])])).
fof(f15620, plain, (spl24_390 <=> aNaturalNumber0(sdtsldt0(xm, xr))), introduced(avatar_definition, [new_symbols(naming, [spl24_390])])).
fof(f61691, plain, ((sK14 = sK4(xp, sK15)) | ~ aNaturalNumber0(sK4(xp, sK15)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | ~ spl24_401)), inference(forward_demodulation, [], [f60804, f61176])).
fof(f61176, plain, ((sK14 = sK22) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | ~ spl24_401)), inference(forward_demodulation, [], [f61175, f60522])).
fof(f60522, plain, ((sK14 = sdtsldt0(sK15, xp)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | ~ spl24_401)), inference(backward_demodulation, [], [f60108, f60355])).
fof(f60355, plain, ((sK15 = sdtasdt0(xn, sK19)) | (~ spl24_5 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f41274, f60354])).
fof(f41274, plain, ((sdtasdt0(xn, sK19) = sdtsldt0(sdtasdt0(xn, xm), xr)) | (~ spl24_5 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(resolution, [], [f38524, f6530])).
fof(f6530, plain, (aNaturalNumber0(xn) | ~ spl24_103), inference(avatar_component_clause, [], [f6529])).
fof(f38524, plain, (! [X12] : (~ aNaturalNumber0(X12) | (sdtsldt0(sdtasdt0(X12, xm), xr) = sdtasdt0(X12, sK19))) | (~ spl24_5 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f4858, f38522])).
fof(f38522, plain, ((sK19 = sdtsldt0(xm, xr)) | (~ spl24_5 | ~ spl24_183 | ~ spl24_390)), inference(subsumption_resolution, [], [f38521, f15621])).
fof(f15621, plain, (aNaturalNumber0(sdtsldt0(xm, xr)) | ~ spl24_390), inference(avatar_component_clause, [], [f15620])).
fof(f38521, plain, (~ aNaturalNumber0(sdtsldt0(xm, xr)) | (sK19 = sdtsldt0(xm, xr)) | (~ spl24_5 | ~ spl24_183)), inference(trivial_inequality_removal, [], [f38514])).
fof(f38514, plain, (~ (xm = xm) | ~ aNaturalNumber0(sdtsldt0(xm, xr)) | (sK19 = sdtsldt0(xm, xr)) | (~ spl24_5 | ~ spl24_183)), inference(superposition, [], [f9548, f3524])).
fof(f3524, plain, ((xm = sdtasdt0(xr, sdtsldt0(xm, xr))) | ~ spl24_5), inference(subsumption_resolution, [], [f3523, f322])).
fof(f3523, plain, ((xm = sdtasdt0(xr, sdtsldt0(xm, xr))) | ~ aNaturalNumber0(xr) | ~ spl24_5), inference(subsumption_resolution, [], [f3522, f277])).
fof(f3522, plain, ((xm = sdtasdt0(xr, sdtsldt0(xm, xr))) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | ~ spl24_5), inference(subsumption_resolution, [], [f3496, f326])).
fof(f3496, plain, ((xm = sdtasdt0(xr, sdtsldt0(xm, xr))) | (sz00 = xr) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | ~ spl24_5), inference(resolution, [], [f384, f419])).
fof(f419, plain, (doDivides0(xr, xm) | ~ spl24_5), inference(avatar_component_clause, [], [f417])).
fof(f9548, plain, (! [X1] : (~ (xm = sdtasdt0(xr, X1)) | ~ aNaturalNumber0(X1) | (sK19 = X1)) | ~ spl24_183), inference(avatar_component_clause, [], [f9547])).
fof(f4858, plain, (! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xm, xr)) = sdtsldt0(sdtasdt0(X12, xm), xr))) | ~ spl24_5), inference(subsumption_resolution, [], [f4857, f322])).
fof(f4857, plain, (! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xm, xr)) = sdtsldt0(sdtasdt0(X12, xm), xr)) | ~ aNaturalNumber0(xr)) | ~ spl24_5), inference(subsumption_resolution, [], [f4856, f277])).
fof(f4856, plain, (! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xm, xr)) = sdtsldt0(sdtasdt0(X12, xm), xr)) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr)) | ~ spl24_5), inference(subsumption_resolution, [], [f4827, f326])).
fof(f4827, plain, (! [X12] : (~ aNaturalNumber0(X12) | (sdtasdt0(X12, sdtsldt0(xm, xr)) = sdtsldt0(sdtasdt0(X12, xm), xr)) | (sz00 = xr) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr)) | ~ spl24_5), inference(resolution, [], [f265, f419])).
fof(f60108, plain, ((sK14 = sdtsldt0(sdtasdt0(xn, sK19), xp)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | ~ spl24_401)), inference(forward_demodulation, [], [f60107, f41454])).
fof(f41454, plain, ((sdtasdt0(xn, sK19) = sdtasdt0(xp, sK14)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | ~ spl24_401)), inference(backward_demodulation, [], [f40961, f41314])).
fof(f41314, plain, ((sdtasdt0(xm, sK20) = sdtasdt0(xn, sK19)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f40960, f41274])).
fof(f40961, plain, ((sdtasdt0(xm, sK20) = sdtasdt0(xp, sK14)) | (~ spl24_2 | ~ spl24_89 | ~ spl24_401)), inference(backward_demodulation, [], [f40856, f40960])).
fof(f40856, plain, ((sdtsldt0(sdtasdt0(xn, xm), xr) = sdtasdt0(xp, sK14)) | ~ spl24_401), inference(forward_demodulation, [], [f40816, f316])).
fof(f40816, plain, ((sdtasdt0(xp, sK14) = sdtsldt0(sdtasdt0(xp, xk), xr)) | ~ spl24_401), inference(resolution, [], [f32036, f278])).
fof(f60107, plain, (sK14 = sdtsldt0(sdtasdt0(xp, sK14), xp)), inference(subsumption_resolution, [], [f60064, f295])).
fof(f60064, plain, ((sz00 = xp) | (sK14 = sdtsldt0(sdtasdt0(xp, sK14), xp))), inference(resolution, [], [f2434, f278])).
fof(f61175, plain, ((sK22 = sdtsldt0(sK15, xp)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(forward_demodulation, [], [f61174, f60360])).
fof(f60360, plain, ((sK15 = sdtasdt0(xp, sK22)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f41316, f60355])).
fof(f41316, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xn, sK19)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f32600, f41314])).
fof(f61174, plain, (sK22 = sdtsldt0(sdtasdt0(xp, sK22), xp)), inference(subsumption_resolution, [], [f61133, f295])).
fof(f61133, plain, ((sz00 = xp) | (sK22 = sdtsldt0(sdtasdt0(xp, sK22), xp))), inference(resolution, [], [f2441, f278])).
fof(f2441, plain, ! [X47] : (~ aNaturalNumber0(X47) | (sz00 = X47) | (sK22 = sdtsldt0(sdtasdt0(X47, sK22), X47))), inference(resolution, [], [f394, f359])).
fof(f60804, plain, ((sK22 = sK4(xp, sK15)) | ~ aNaturalNumber0(sK4(xp, sK15)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(trivial_inequality_removal, [], [f60802])).
fof(f60802, plain, (~ (sK15 = sK15) | (sK22 = sK4(xp, sK15)) | ~ aNaturalNumber0(sK4(xp, sK15)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(superposition, [], [f60459, f60373])).
fof(f60373, plain, ((sK15 = sdtasdt0(xp, sK4(xp, sK15))) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f41329, f60355])).
fof(f41329, plain, ((sdtasdt0(xn, sK19) = sdtasdt0(xp, sK4(xp, sdtasdt0(xn, sK19)))) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f32613, f41314])).
fof(f32613, plain, ((sdtasdt0(xm, sK20) = sdtasdt0(xp, sK4(xp, sdtasdt0(xm, sK20)))) | ~ spl24_89), inference(backward_demodulation, [], [f2270, f32600])).
fof(f2270, plain, (sdtasdt0(xp, sK22) = sdtasdt0(xp, sK4(xp, sdtasdt0(xp, sK22)))), inference(subsumption_resolution, [], [f2269, f278])).
fof(f2269, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xp, sK4(xp, sdtasdt0(xp, sK22)))) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f2254, f606])).
fof(f2254, plain, ((sdtasdt0(xp, sK22) = sdtasdt0(xp, sK4(xp, sdtasdt0(xp, sK22)))) | ~ aNaturalNumber0(sdtasdt0(xp, sK22)) | ~ aNaturalNumber0(xp)), inference(resolution, [], [f256, f432])).
fof(f60459, plain, (! [X0] : (~ (sdtasdt0(xp, X0) = sK15) | (sK22 = X0) | ~ aNaturalNumber0(X0)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f41437, f60355])).
fof(f41437, plain, (! [X0] : (~ (sdtasdt0(xp, X0) = sdtasdt0(xn, sK19)) | (sK22 = X0) | ~ aNaturalNumber0(X0)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f33196, f41314])).
fof(f33196, plain, (! [X0] : (~ (sdtasdt0(xp, X0) = sdtasdt0(xm, sK20)) | (sK22 = X0) | ~ aNaturalNumber0(X0)) | ~ spl24_89), inference(subsumption_resolution, [], [f33195, f278])).
fof(f33195, plain, (! [X0] : (~ (sdtasdt0(xp, X0) = sdtasdt0(xm, sK20)) | (sK22 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(xp)) | ~ spl24_89), inference(subsumption_resolution, [], [f33194, f295])).
fof(f33194, plain, (! [X0] : (~ (sdtasdt0(xp, X0) = sdtasdt0(xm, sK20)) | (sK22 = X0) | ~ aNaturalNumber0(X0) | (sz00 = xp) | ~ aNaturalNumber0(xp)) | ~ spl24_89), inference(subsumption_resolution, [], [f33174, f359])).
fof(f33174, plain, (! [X0] : (~ (sdtasdt0(xp, X0) = sdtasdt0(xm, sK20)) | (sK22 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(sK22) | (sz00 = xp) | ~ aNaturalNumber0(xp)) | ~ spl24_89), inference(superposition, [], [f227, f32600])).
fof(f61690, plain, (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647), inference(avatar_contradiction_clause, [], [f61689])).
fof(f61689, plain, ($false | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647)), inference(subsumption_resolution, [], [f61688, f278])).
fof(f61688, plain, (~ aNaturalNumber0(xp) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647)), inference(subsumption_resolution, [], [f61687, f333])).
fof(f61687, plain, (~ aNaturalNumber0(sK15) | ~ aNaturalNumber0(xp) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647)), inference(subsumption_resolution, [], [f61686, f60361])).
fof(f60361, plain, (doDivides0(xp, sK15) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f41317, f60355])).
fof(f41317, plain, (doDivides0(xp, sdtasdt0(xn, sK19)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390)), inference(backward_demodulation, [], [f32601, f41314])).
fof(f61686, plain, (~ doDivides0(xp, sK15) | ~ aNaturalNumber0(sK15) | ~ aNaturalNumber0(xp) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647)), inference(resolution, [], [f60479, f255])).
fof(f255, plain, ! [X0, X1] : (aNaturalNumber0(sK4(X0, X1)) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f160])).
fof(f60479, plain, (~ aNaturalNumber0(sK4(xp, sK15)) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_93 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647)), inference(backward_demodulation, [], [f41468, f60355])).
fof(f41468, plain, (~ aNaturalNumber0(sK4(xp, sdtasdt0(xn, sK19))) | (~ spl24_2 | ~ spl24_5 | ~ spl24_89 | ~ spl24_103 | ~ spl24_183 | ~ spl24_390 | spl24_647)), inference(backward_demodulation, [], [f41124, f41314])).
fof(f41124, plain, (~ aNaturalNumber0(sK4(xp, sdtasdt0(xm, sK20))) | spl24_647), inference(avatar_component_clause, [], [f41122])).
fof(f45820, plain, (~ spl24_103 | spl24_683), inference(avatar_contradiction_clause, [], [f45819])).
fof(f45819, plain, ($false | (~ spl24_103 | spl24_683)), inference(subsumption_resolution, [], [f45818, f6530])).
fof(f45818, plain, (~ aNaturalNumber0(xn) | spl24_683), inference(subsumption_resolution, [], [f45817, f277])).
fof(f45817, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xn) | spl24_683), inference(resolution, [], [f45502, f211])).
fof(f211, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f67])).
fof(f67, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtpldt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mSortsB)).
fof(f45502, plain, (~ aNaturalNumber0(sdtpldt0(xn, xm)) | spl24_683), inference(avatar_component_clause, [], [f45500])).
fof(f19795, plain, (~ spl24_93 | spl24_401), inference(avatar_contradiction_clause, [], [f19794])).
fof(f19794, plain, ($false | (~ spl24_93 | spl24_401)), inference(subsumption_resolution, [], [f19793, f5753])).
fof(f19793, plain, (~ aNaturalNumber0(xr) | spl24_401), inference(subsumption_resolution, [], [f19792, f315])).
fof(f19792, plain, (~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr) | spl24_401), inference(subsumption_resolution, [], [f19791, f326])).
fof(f19791, plain, ((sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr) | spl24_401), inference(subsumption_resolution, [], [f19790, f325])).
fof(f19790, plain, (~ doDivides0(xr, xk) | (sz00 = xr) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(xr) | spl24_401), inference(resolution, [], [f15705, f385])).
fof(f385, plain, ! [X0, X1] : (aNaturalNumber0(sdtsldt0(X1, X0)) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f258])).
fof(f258, plain, ! [X2, X0, X1] : (aNaturalNumber0(X2) | ~ (sdtsldt0(X1, X0) = X2) | ~ doDivides0(X0, X1) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f162])).
fof(f15705, plain, (~ aNaturalNumber0(sdtsldt0(xk, xr)) | spl24_401), inference(avatar_component_clause, [], [f15703])).
fof(f19750, plain, (~ spl24_5 | ~ spl24_93 | spl24_390), inference(avatar_contradiction_clause, [], [f19749])).
fof(f19749, plain, ($false | (~ spl24_5 | ~ spl24_93 | spl24_390)), inference(subsumption_resolution, [], [f19748, f5753])).
fof(f19748, plain, (~ aNaturalNumber0(xr) | (~ spl24_5 | spl24_390)), inference(subsumption_resolution, [], [f19747, f277])).
fof(f19747, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | (~ spl24_5 | spl24_390)), inference(subsumption_resolution, [], [f19746, f326])).
fof(f19746, plain, ((sz00 = xr) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | (~ spl24_5 | spl24_390)), inference(subsumption_resolution, [], [f19745, f419])).
fof(f19745, plain, (~ doDivides0(xr, xm) | (sz00 = xr) | ~ aNaturalNumber0(xm) | ~ aNaturalNumber0(xr) | spl24_390), inference(resolution, [], [f15622, f385])).
fof(f15622, plain, (~ aNaturalNumber0(sdtsldt0(xm, xr)) | spl24_390), inference(avatar_component_clause, [], [f15620])).
fof(f16093, plain, (spl24_79 | ~ spl24_89), inference(avatar_contradiction_clause, [], [f16092])).
fof(f16092, plain, ($false | (spl24_79 | ~ spl24_89)), inference(subsumption_resolution, [], [f16091, f5733])).
fof(f16091, plain, (~ aNaturalNumber0(sdtsldt0(xn, xr)) | spl24_79), inference(subsumption_resolution, [], [f16090, f277])).
fof(f16090, plain, (~ aNaturalNumber0(xm) | ~ aNaturalNumber0(sdtsldt0(xn, xr)) | spl24_79), inference(resolution, [], [f5004, f211])).
fof(f5004, plain, (~ aNaturalNumber0(sdtpldt0(sdtsldt0(xn, xr), xm)) | spl24_79), inference(avatar_component_clause, [], [f5002])).
fof(f15806, plain, (~ spl24_35 | ~ spl24_34 | spl24_412), inference(avatar_split_clause, [], [f15805, f15767, f1297, f1301])).
fof(f1301, plain, (spl24_35 <=> aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp))), introduced(avatar_definition, [new_symbols(naming, [spl24_35])])).
fof(f1297, plain, (spl24_34 <=> aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp))), introduced(avatar_definition, [new_symbols(naming, [spl24_34])])).
fof(f15805, plain, (iLess0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp))), inference(subsumption_resolution, [], [f1403, f364])).
fof(f364, plain, ~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)), inference(cnf_transformation, [], [f207])).
fof(f207, plain, (sdtlseqdt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) & ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sK23)) & aNaturalNumber0(sK23)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK23])], [f143, f206])).
fof(f206, plain, (? [X0] : ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), X0)) & aNaturalNumber0(X0)) => ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sK23)) & aNaturalNumber0(sK23))), introduced(choice_axiom, [])).
fof(f143, plain, (sdtlseqdt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) & ? [X0] : ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))), inference(flattening, [], [f142])).
fof(f142, plain, (sdtlseqdt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) & ? [X0] : ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & (~ (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) & ((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, (sdtlseqdt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) & ? [X0] : ((sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), X0)) & aNaturalNumber0(X0)) & (xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr)) & ~ (((xn = sdtasdt0(xr, sdtsldt0(xn, xr))) & aNaturalNumber0(sdtsldt0(xn, xr))) => (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2686)).
fof(f1403, plain, (iLess0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)) | (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp))), inference(resolution, [], [f254, f369])).
fof(f369, plain, sdtlseqdt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sdtpldt0(sdtpldt0(xn, xm), xp)), inference(cnf_transformation, [], [f207])).
fof(f254, plain, ! [X0, X1] : (~ sdtlseqdt0(X0, X1) | iLess0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0, X1] : (iLess0(X0, X1) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f111])).
fof(f111, plain, ! [X0, X1] : ((iLess0(X0, X1) | (~ sdtlseqdt0(X0, X1) | (X0 = X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X0, X1) & ~ (X0 = X1)) => iLess0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', mIH_03)).
fof(f15764, plain, (spl24_35 | ~ spl24_79), inference(avatar_contradiction_clause, [], [f15763])).
fof(f15763, plain, ($false | (spl24_35 | ~ spl24_79)), inference(subsumption_resolution, [], [f15762, f5003])).
fof(f15762, plain, (~ aNaturalNumber0(sdtpldt0(sdtsldt0(xn, xr), xm)) | spl24_35), inference(subsumption_resolution, [], [f15761, f278])).
fof(f15761, plain, (~ aNaturalNumber0(xp) | ~ aNaturalNumber0(sdtpldt0(sdtsldt0(xn, xr), xm)) | spl24_35), inference(resolution, [], [f1303, f211])).
fof(f1303, plain, (~ aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) | spl24_35), inference(avatar_component_clause, [], [f1301])).
fof(f9597, plain, (~ spl24_7 | spl24_183 | ~ spl24_6 | ~ spl24_93), inference(avatar_split_clause, [], [f9596, f5752, f422, f9547, f427])).
fof(f427, plain, (spl24_7 <=> aNaturalNumber0(sK19)), introduced(avatar_definition, [new_symbols(naming, [spl24_7])])).
fof(f422, plain, (spl24_6 <=> (xm = sdtasdt0(xr, sK19))), introduced(avatar_definition, [new_symbols(naming, [spl24_6])])).
fof(f9596, plain, (! [X5] : (~ (xm = sdtasdt0(xr, X5)) | (sK19 = X5) | ~ aNaturalNumber0(X5) | ~ aNaturalNumber0(sK19)) | (~ spl24_6 | ~ spl24_93)), inference(subsumption_resolution, [], [f9595, f5753])).
fof(f9595, plain, (! [X5] : (~ (xm = sdtasdt0(xr, X5)) | (sK19 = X5) | ~ aNaturalNumber0(X5) | ~ aNaturalNumber0(sK19) | ~ aNaturalNumber0(xr)) | ~ spl24_6), inference(subsumption_resolution, [], [f3620, f326])).
fof(f3620, plain, (! [X5] : (~ (xm = sdtasdt0(xr, X5)) | (sK19 = X5) | ~ aNaturalNumber0(X5) | ~ aNaturalNumber0(sK19) | (sz00 = xr) | ~ aNaturalNumber0(xr)) | ~ spl24_6), inference(superposition, [], [f227, f424])).
fof(f424, plain, ((xm = sdtasdt0(xr, sK19)) | ~ spl24_6), inference(avatar_component_clause, [], [f422])).
fof(f6560, plain, spl24_103, inference(avatar_split_clause, [], [f276, f6529])).
fof(f5780, plain, spl24_89, inference(avatar_split_clause, [], [f352, f5732])).
fof(f5779, plain, spl24_93, inference(avatar_split_clause, [], [f322, f5752])).
fof(f2825, plain, (~ spl24_35 | spl24_34), inference(avatar_split_clause, [], [f2824, f1297, f1301])).
fof(f2824, plain, (~ aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) | spl24_34), inference(subsumption_resolution, [], [f2823, f367])).
fof(f367, plain, aNaturalNumber0(sK23), inference(cnf_transformation, [], [f207])).
fof(f2823, plain, (~ aNaturalNumber0(sK23) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp)) | spl24_34), inference(subsumption_resolution, [], [f2814, f1299])).
fof(f1299, plain, (~ aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | spl24_34), inference(avatar_component_clause, [], [f1297])).
fof(f2814, plain, (aNaturalNumber0(sdtpldt0(sdtpldt0(xn, xm), xp)) | ~ aNaturalNumber0(sK23) | ~ aNaturalNumber0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp))), inference(superposition, [], [f211, f368])).
fof(f368, plain, (sdtpldt0(sdtpldt0(xn, xm), xp) = sdtpldt0(sdtpldt0(sdtpldt0(sdtsldt0(xn, xr), xm), xp), sK23)), inference(cnf_transformation, [], [f207])).
fof(f431, plain, spl24_2, inference(avatar_split_clause, [], [f348, f402])).
fof(f348, plain, doDivides0(xr, xn), inference(cnf_transformation, [], [f201])).
fof(f430, plain, (spl24_1 | spl24_7), inference(avatar_split_clause, [], [f343, f427, f398])).
fof(f398, plain, (spl24_1 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl24_1])])).
fof(f343, plain, (aNaturalNumber0(sK19) | sP2), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ((doDivides0(xr, xm) & ((xm = sdtasdt0(xr, sK19)) & aNaturalNumber0(sK19))) | sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19])], [f150, f198])).
fof(f198, plain, (? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xm = sdtasdt0(xr, sK19)) & aNaturalNumber0(sK19))), introduced(choice_axiom, [])).
fof(f150, plain, ((doDivides0(xr, xm) & ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | sP2), inference(definition_folding, [], [f65, e149])).
fof(f149, plain, ((doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1))) | ~ sP2), inference(usedef, [], [e149])).
fof(e149, plain, (sP2 <=> (doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f65, plain, ((doDivides0(xr, xm) & ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | (doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1)))), inference(rectify, [], [f51])).
fof(f51, plain, ((doDivides0(xr, xm) & ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | (doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM517+3.p', m__2449)).
fof(f425, plain, (spl24_1 | spl24_6), inference(avatar_split_clause, [], [f344, f422, f398])).
fof(f344, plain, ((xm = sdtasdt0(xr, sK19)) | sP2), inference(cnf_transformation, [], [f199])).
fof(f420, plain, (spl24_1 | spl24_5), inference(avatar_split_clause, [], [f345, f417, f398])).
fof(f345, plain, (doDivides0(xr, xm) | sP2), inference(cnf_transformation, [], [f199])).
fof(f415, plain, (~ spl24_1 | spl24_4), inference(avatar_split_clause, [], [f340, f412, f398])).
fof(f340, plain, (aNaturalNumber0(sK18) | ~ sP2), inference(cnf_transformation, [], [f197])).
fof(f197, plain, ((doDivides0(xr, xn) & ((xn = sdtasdt0(xr, sK18)) & aNaturalNumber0(sK18))) | ~ sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f195, f196])).
fof(f196, plain, (? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xn = sdtasdt0(xr, sK18)) & aNaturalNumber0(sK18))), introduced(choice_axiom, [])).
fof(f195, plain, ((doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | ~ sP2), inference(rectify, [], [f194])).
fof(f194, plain, ((doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1))) | ~ sP2), inference(nnf_transformation, [], [f149])).
fof(f410, plain, (~ spl24_1 | spl24_3), inference(avatar_split_clause, [], [f341, f407, f398])).
fof(f341, plain, ((xn = sdtasdt0(xr, sK18)) | ~ sP2), inference(cnf_transformation, [], [f197])).