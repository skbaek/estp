fof(f111060, plain, $false, inference(avatar_sat_refutation, [], [f279, f395, f405, f438, f467, f1568, f1583, f35669, f52917, f57940, f79033, f110960, f110985])).
fof(f110985, plain, (~ spl22_4 | spl22_578), inference(avatar_contradiction_clause, [], [f110984])).
fof(f110984, plain, ($false | (~ spl22_4 | spl22_578)), inference(subsumption_resolution, [], [f110983, f256])).
fof(f256, plain, aElementOf0(xb, slsdtgt0(xb)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (aElementOf0(xb, slsdtgt0(xb)) & aElementOf0(sz00, slsdtgt0(xb)) & aElementOf0(xa, slsdtgt0(xa)) & aElementOf0(sz00, slsdtgt0(xa))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', m__2203)).
fof(f110983, plain, (~ aElementOf0(xb, slsdtgt0(xb)) | (~ spl22_4 | spl22_578)), inference(subsumption_resolution, [], [f110982, f253])).
fof(f253, plain, aElementOf0(sz00, slsdtgt0(xa)), inference(cnf_transformation, [], [f43])).
fof(f110982, plain, (~ aElementOf0(sz00, slsdtgt0(xa)) | ~ aElementOf0(xb, slsdtgt0(xb)) | (~ spl22_4 | spl22_578)), inference(subsumption_resolution, [], [f110919, f79024])).
fof(f79024, plain, (~ aElementOf0(xb, xI) | spl22_578), inference(avatar_component_clause, [], [f79022])).
fof(f79022, plain, (spl22_578 <=> aElementOf0(xb, xI)), introduced(avatar_definition, [new_symbols(naming, [spl22_578])])).
fof(f110919, plain, (aElementOf0(xb, xI) | ~ aElementOf0(sz00, slsdtgt0(xa)) | ~ aElementOf0(xb, slsdtgt0(xb)) | ~ spl22_4), inference(superposition, [], [f1512, f292])).
fof(f292, plain, (xb = sdtpldt0(sz00, xb)), inference(resolution, [], [f165, f248])).
fof(f248, plain, aElement0(xb), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aElement0(xb) & aElement0(xa)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', m__2091)).
fof(f165, plain, ! [X0] : (~ aElement0(X0) | (sdtpldt0(sz00, X0) = X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0)) | ~ aElement0(X0)), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0] : (aElement0(X0) => ((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mAddZero)).
fof(f1512, plain, (! [X2, X1] : (aElementOf0(sdtpldt0(X2, X1), xI) | ~ aElementOf0(X2, slsdtgt0(xa)) | ~ aElementOf0(X1, slsdtgt0(xb))) | ~ spl22_4), inference(resolution, [], [f394, f259])).
fof(f259, plain, ! [X2, X0, X10, X1, X9] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1) | aElementOf0(sdtpldt0(X9, X10), X2)), inference(equality_resolution, [], [f191])).
fof(f191, plain, ! [X2, X0, X10, X8, X1, X9] : (aElementOf0(X8, X2) | ~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1)) | aElementOf0(sK4(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6, sK7, sK8])], [f114, f117, f116, f115])).
fof(f115, plain, ! [X2, X1, X0] : (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) => ((! [X5, X4] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(sK4(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f116, plain, ! [X2, X1, X0] : (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) => ((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X8, X1, X0] : (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) => ((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1))), introduced(choice_axiom, [])).
fof(f114, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f112])).
fof(f112, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f105])).
fof(f105, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), inference(usedef, [], [e105])).
fof(e105, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f394, plain, (sP0(slsdtgt0(xb), slsdtgt0(xa), xI) | ~ spl22_4), inference(avatar_component_clause, [], [f392])).
fof(f392, plain, (spl22_4 <=> sP0(slsdtgt0(xb), slsdtgt0(xa), xI)), introduced(avatar_definition, [new_symbols(naming, [spl22_4])])).
fof(f110960, plain, (~ spl22_4 | spl22_420), inference(avatar_contradiction_clause, [], [f110959])).
fof(f110959, plain, ($false | (~ spl22_4 | spl22_420)), inference(subsumption_resolution, [], [f110958, f255])).
fof(f255, plain, aElementOf0(sz00, slsdtgt0(xb)), inference(cnf_transformation, [], [f43])).
fof(f110958, plain, (~ aElementOf0(sz00, slsdtgt0(xb)) | (~ spl22_4 | spl22_420)), inference(subsumption_resolution, [], [f110957, f254])).
fof(f254, plain, aElementOf0(xa, slsdtgt0(xa)), inference(cnf_transformation, [], [f43])).
fof(f110957, plain, (~ aElementOf0(xa, slsdtgt0(xa)) | ~ aElementOf0(sz00, slsdtgt0(xb)) | (~ spl22_4 | spl22_420)), inference(subsumption_resolution, [], [f110889, f57931])).
fof(f57931, plain, (~ aElementOf0(xa, xI) | spl22_420), inference(avatar_component_clause, [], [f57929])).
fof(f57929, plain, (spl22_420 <=> aElementOf0(xa, xI)), introduced(avatar_definition, [new_symbols(naming, [spl22_420])])).
fof(f110889, plain, (aElementOf0(xa, xI) | ~ aElementOf0(xa, slsdtgt0(xa)) | ~ aElementOf0(sz00, slsdtgt0(xb)) | ~ spl22_4), inference(superposition, [], [f1512, f286])).
fof(f286, plain, (xa = sdtpldt0(xa, sz00)), inference(resolution, [], [f164, f247])).
fof(f247, plain, aElement0(xa), inference(cnf_transformation, [], [f39])).
fof(f164, plain, ! [X0] : (~ aElement0(X0) | (sdtpldt0(X0, sz00) = X0)), inference(cnf_transformation, [], [f64])).
fof(f79033, plain, (~ spl22_578 | ~ spl22_28 | spl22_34), inference(avatar_split_clause, [], [f79032, f1592, f1553, f79022])).
fof(f1553, plain, (spl22_28 <=> aElement0(smndt0(sz10))), introduced(avatar_definition, [new_symbols(naming, [spl22_28])])).
fof(f1592, plain, (spl22_34 <=> (sz00 = smndt0(xb))), introduced(avatar_definition, [new_symbols(naming, [spl22_34])])).
fof(f79032, plain, (~ aElementOf0(xb, xI) | (~ spl22_28 | spl22_34)), inference(subsumption_resolution, [], [f79031, f1594])).
fof(f1594, plain, (~ (sz00 = smndt0(xb)) | spl22_34), inference(avatar_component_clause, [], [f1592])).
fof(f79031, plain, (~ aElementOf0(xb, xI) | (sz00 = smndt0(xb)) | ~ spl22_28), inference(subsumption_resolution, [], [f78987, f251])).
fof(f251, plain, aIdeal0(xI), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((xI = sdtpldt1(slsdtgt0(xa), slsdtgt0(xb))) & aIdeal0(xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', m__2174)).
fof(f78987, plain, (~ aElementOf0(xb, xI) | ~ aIdeal0(xI) | (sz00 = smndt0(xb)) | ~ spl22_28), inference(resolution, [], [f1596, f280])).
fof(f280, plain, ! [X0] : (~ aElementOf0(X0, xI) | (sz00 = X0)), inference(forward_demodulation, [], [f257, f252])).
fof(f252, plain, (xI = sdtpldt1(slsdtgt0(xa), slsdtgt0(xb))), inference(cnf_transformation, [], [f42])).
fof(f257, plain, ! [X0] : ((sz00 = X0) | ~ aElementOf0(X0, sdtpldt1(slsdtgt0(xa), slsdtgt0(xb)))), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : ((sz00 = X0) | ~ aElementOf0(X0, sdtpldt1(slsdtgt0(xa), slsdtgt0(xb)))), inference(ennf_transformation, [], [f45])).
fof(f45, plain, ~ ? [X0] : (~ (sz00 = X0) & aElementOf0(X0, sdtpldt1(slsdtgt0(xa), slsdtgt0(xb)))), inference(negated_conjecture, [], [f44])).
fof(f44, plain, ~ ? [X0] : (~ (sz00 = X0) & aElementOf0(X0, sdtpldt1(slsdtgt0(xa), slsdtgt0(xb)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', m__)).
fof(f1596, plain, (! [X1] : (aElementOf0(smndt0(xb), X1) | ~ aElementOf0(xb, X1) | ~ aIdeal0(X1)) | ~ spl22_28), inference(subsumption_resolution, [], [f1585, f1554])).
fof(f1554, plain, (aElement0(smndt0(sz10)) | ~ spl22_28), inference(avatar_component_clause, [], [f1553])).
fof(f1585, plain, ! [X1] : (aElementOf0(smndt0(xb), X1) | ~ aElement0(smndt0(sz10)) | ~ aElementOf0(xb, X1) | ~ aIdeal0(X1)), inference(superposition, [], [f206, f374])).
fof(f374, plain, (smndt0(xb) = sdtasdt0(smndt0(sz10), xb)), inference(resolution, [], [f174, f248])).
fof(f174, plain, ! [X0] : (~ aElement0(X0) | (smndt0(X0) = sdtasdt0(smndt0(sz10), X0))), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (((smndt0(X0) = sdtasdt0(X0, smndt0(sz10))) & (smndt0(X0) = sdtasdt0(smndt0(sz10), X0))) | ~ aElement0(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (aElement0(X0) => ((smndt0(X0) = sdtasdt0(X0, smndt0(sz10))) & (smndt0(X0) = sdtasdt0(smndt0(sz10), X0)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mMulMnOne)).
fof(f206, plain, ! [X4, X0, X5] : (aElementOf0(sdtasdt0(X5, X4), X0) | ~ aElement0(X5) | ~ aElementOf0(X4, X0) | ~ aIdeal0(X0)), inference(cnf_transformation, [], [f130])).
fof(f130, plain, ! [X0] : ((aIdeal0(X0) | (((~ aElementOf0(sdtasdt0(sK11(X0), sK10(X0)), X0) & aElement0(sK11(X0))) | (~ aElementOf0(sdtpldt0(sK10(X0), sK12(X0)), X0) & aElementOf0(sK12(X0), X0))) & aElementOf0(sK10(X0), X0)) | ~ aSet0(X0)) & ((! [X4] : ((! [X5] : (aElementOf0(sdtasdt0(X5, X4), X0) | ~ aElement0(X5)) & ! [X6] : (aElementOf0(sdtpldt0(X4, X6), X0) | ~ aElementOf0(X6, X0))) | ~ aElementOf0(X4, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12])], [f126, f129, f128, f127])).
fof(f127, plain, ! [X0] : (? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) => ((? [X2] : (~ aElementOf0(sdtasdt0(X2, sK10(X0)), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(sK10(X0), X3), X0) & aElementOf0(X3, X0))) & aElementOf0(sK10(X0), X0))), introduced(choice_axiom, [])).
fof(f128, plain, ! [X0] : (? [X2] : (~ aElementOf0(sdtasdt0(X2, sK10(X0)), X0) & aElement0(X2)) => (~ aElementOf0(sdtasdt0(sK11(X0), sK10(X0)), X0) & aElement0(sK11(X0)))), introduced(choice_axiom, [])).
fof(f129, plain, ! [X0] : (? [X3] : (~ aElementOf0(sdtpldt0(sK10(X0), X3), X0) & aElementOf0(X3, X0)) => (~ aElementOf0(sdtpldt0(sK10(X0), sK12(X0)), X0) & aElementOf0(sK12(X0), X0))), introduced(choice_axiom, [])).
fof(f126, plain, ! [X0] : ((aIdeal0(X0) | ? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) | ~ aSet0(X0)) & ((! [X4] : ((! [X5] : (aElementOf0(sdtasdt0(X5, X4), X0) | ~ aElement0(X5)) & ! [X6] : (aElementOf0(sdtpldt0(X4, X6), X0) | ~ aElementOf0(X6, X0))) | ~ aElementOf0(X4, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(rectify, [], [f125])).
fof(f125, plain, ! [X0] : ((aIdeal0(X0) | ? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) | ~ aSet0(X0)) & ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((aIdeal0(X0) | (? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) | ~ aSet0(X0))) & ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(nnf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0))), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X2] : (aElementOf0(X2, X0) => aElementOf0(sdtpldt0(X1, X2), X0)))) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mDefIdeal)).
fof(f57940, plain, (~ spl22_420 | spl22_30 | ~ spl22_31), inference(avatar_split_clause, [], [f57939, f1566, f1561, f57929])).
fof(f1561, plain, (spl22_30 <=> (sz00 = smndt0(xa))), introduced(avatar_definition, [new_symbols(naming, [spl22_30])])).
fof(f1566, plain, (spl22_31 <=> ! [X1] : (aElementOf0(smndt0(xa), X1) | ~ aIdeal0(X1) | ~ aElementOf0(xa, X1))), introduced(avatar_definition, [new_symbols(naming, [spl22_31])])).
fof(f57939, plain, (~ aElementOf0(xa, xI) | (spl22_30 | ~ spl22_31)), inference(subsumption_resolution, [], [f57938, f1563])).
fof(f1563, plain, (~ (sz00 = smndt0(xa)) | spl22_30), inference(avatar_component_clause, [], [f1561])).
fof(f57938, plain, (~ aElementOf0(xa, xI) | (sz00 = smndt0(xa)) | ~ spl22_31), inference(subsumption_resolution, [], [f57906, f251])).
fof(f57906, plain, (~ aIdeal0(xI) | ~ aElementOf0(xa, xI) | (sz00 = smndt0(xa)) | ~ spl22_31), inference(resolution, [], [f1567, f280])).
fof(f1567, plain, (! [X1] : (aElementOf0(smndt0(xa), X1) | ~ aIdeal0(X1) | ~ aElementOf0(xa, X1)) | ~ spl22_31), inference(avatar_component_clause, [], [f1566])).
fof(f52917, plain, (spl22_2 | ~ spl22_34), inference(avatar_split_clause, [], [f52916, f1592, f276])).
fof(f276, plain, (spl22_2 <=> (sz00 = xb)), introduced(avatar_definition, [new_symbols(naming, [spl22_2])])).
fof(f52916, plain, ((sz00 = xb) | ~ spl22_34), inference(backward_demodulation, [], [f292, f52484])).
fof(f52484, plain, ((sz00 = sdtpldt0(sz00, xb)) | ~ spl22_34), inference(backward_demodulation, [], [f353, f1593])).
fof(f1593, plain, ((sz00 = smndt0(xb)) | ~ spl22_34), inference(avatar_component_clause, [], [f1592])).
fof(f353, plain, (sz00 = sdtpldt0(smndt0(xb), xb)), inference(resolution, [], [f167, f248])).
fof(f167, plain, ! [X0] : (~ aElement0(X0) | (sz00 = sdtpldt0(smndt0(X0), X0))), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (((sz00 = sdtpldt0(smndt0(X0), X0)) & (sz00 = sdtpldt0(X0, smndt0(X0)))) | ~ aElement0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aElement0(X0) => ((sz00 = sdtpldt0(smndt0(X0), X0)) & (sz00 = sdtpldt0(X0, smndt0(X0))))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mAddInvr)).
fof(f35669, plain, (spl22_1 | ~ spl22_30), inference(avatar_split_clause, [], [f35668, f1561, f272])).
fof(f272, plain, (spl22_1 <=> (sz00 = xa)), introduced(avatar_definition, [new_symbols(naming, [spl22_1])])).
fof(f35668, plain, ((sz00 = xa) | ~ spl22_30), inference(backward_demodulation, [], [f291, f35247])).
fof(f35247, plain, ((sz00 = sdtpldt0(sz00, xa)) | ~ spl22_30), inference(backward_demodulation, [], [f352, f1562])).
fof(f1562, plain, ((sz00 = smndt0(xa)) | ~ spl22_30), inference(avatar_component_clause, [], [f1561])).
fof(f352, plain, (sz00 = sdtpldt0(smndt0(xa), xa)), inference(resolution, [], [f167, f247])).
fof(f291, plain, (xa = sdtpldt0(sz00, xa)), inference(resolution, [], [f165, f247])).
fof(f1583, plain, spl22_28, inference(avatar_contradiction_clause, [], [f1582])).
fof(f1582, plain, ($false | spl22_28), inference(subsumption_resolution, [], [f1581, f158])).
fof(f158, plain, aElement0(sz10), inference(cnf_transformation, [], [f3])).
fof(f3, plain, aElement0(sz10), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mSortsC_01)).
fof(f1581, plain, (~ aElement0(sz10) | spl22_28), inference(resolution, [], [f1555, f159])).
fof(f159, plain, ! [X0] : (aElement0(smndt0(X0)) | ~ aElement0(X0)), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ! [X0] : (aElement0(smndt0(X0)) | ~ aElement0(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (aElement0(X0) => aElement0(smndt0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mSortsU)).
fof(f1555, plain, (~ aElement0(smndt0(sz10)) | spl22_28), inference(avatar_component_clause, [], [f1553])).
fof(f1568, plain, (~ spl22_28 | spl22_31), inference(avatar_split_clause, [], [f1546, f1566, f1553])).
fof(f1546, plain, ! [X1] : (aElementOf0(smndt0(xa), X1) | ~ aElement0(smndt0(sz10)) | ~ aElementOf0(xa, X1) | ~ aIdeal0(X1)), inference(superposition, [], [f206, f373])).
fof(f373, plain, (smndt0(xa) = sdtasdt0(smndt0(sz10), xa)), inference(resolution, [], [f174, f247])).
fof(f467, plain, spl22_6, inference(avatar_contradiction_clause, [], [f466])).
fof(f466, plain, ($false | spl22_6), inference(subsumption_resolution, [], [f465, f248])).
fof(f465, plain, (~ aElement0(xb) | spl22_6), inference(resolution, [], [f404, f269])).
fof(f269, plain, ! [X0] : (aSet0(slsdtgt0(X0)) | ~ aElement0(X0)), inference(equality_resolution, [], [f239])).
fof(f239, plain, ! [X0, X1] : (aSet0(X1) | ~ (slsdtgt0(X0) = X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : (((slsdtgt0(X0) = X1) | ((! [X3] : (~ (sdtasdt0(X0, X3) = sK19(X0, X1)) | ~ aElement0(X3)) | ~ aElementOf0(sK19(X0, X1), X1)) & (((sK19(X0, X1) = sdtasdt0(X0, sK20(X0, X1))) & aElement0(sK20(X0, X1))) | aElementOf0(sK19(X0, X1), X1))) | ~ aSet0(X1)) & ((! [X5] : ((aElementOf0(X5, X1) | ! [X6] : (~ (sdtasdt0(X0, X6) = X5) | ~ aElement0(X6))) & (((sdtasdt0(X0, sK21(X0, X5)) = X5) & aElement0(sK21(X0, X5))) | ~ aElementOf0(X5, X1))) & aSet0(X1)) | ~ (slsdtgt0(X0) = X1))) | ~ aElement0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20, sK21])], [f152, f155, f154, f153])).
fof(f153, plain, ! [X1, X0] : (? [X2] : ((! [X3] : (~ (sdtasdt0(X0, X3) = X2) | ~ aElement0(X3)) | ~ aElementOf0(X2, X1)) & (? [X4] : ((sdtasdt0(X0, X4) = X2) & aElement0(X4)) | aElementOf0(X2, X1))) => ((! [X3] : (~ (sdtasdt0(X0, X3) = sK19(X0, X1)) | ~ aElement0(X3)) | ~ aElementOf0(sK19(X0, X1), X1)) & (? [X4] : ((sdtasdt0(X0, X4) = sK19(X0, X1)) & aElement0(X4)) | aElementOf0(sK19(X0, X1), X1)))), introduced(choice_axiom, [])).
fof(f154, plain, ! [X1, X0] : (? [X4] : ((sdtasdt0(X0, X4) = sK19(X0, X1)) & aElement0(X4)) => ((sK19(X0, X1) = sdtasdt0(X0, sK20(X0, X1))) & aElement0(sK20(X0, X1)))), introduced(choice_axiom, [])).
fof(f155, plain, ! [X5, X0] : (? [X7] : ((sdtasdt0(X0, X7) = X5) & aElement0(X7)) => ((sdtasdt0(X0, sK21(X0, X5)) = X5) & aElement0(sK21(X0, X5)))), introduced(choice_axiom, [])).
fof(f152, plain, ! [X0] : (! [X1] : (((slsdtgt0(X0) = X1) | ? [X2] : ((! [X3] : (~ (sdtasdt0(X0, X3) = X2) | ~ aElement0(X3)) | ~ aElementOf0(X2, X1)) & (? [X4] : ((sdtasdt0(X0, X4) = X2) & aElement0(X4)) | aElementOf0(X2, X1))) | ~ aSet0(X1)) & ((! [X5] : ((aElementOf0(X5, X1) | ! [X6] : (~ (sdtasdt0(X0, X6) = X5) | ~ aElement0(X6))) & (? [X7] : ((sdtasdt0(X0, X7) = X5) & aElement0(X7)) | ~ aElementOf0(X5, X1))) & aSet0(X1)) | ~ (slsdtgt0(X0) = X1))) | ~ aElement0(X0)), inference(rectify, [], [f151])).
fof(f151, plain, ! [X0] : (! [X1] : (((slsdtgt0(X0) = X1) | ? [X2] : ((! [X3] : (~ (sdtasdt0(X0, X3) = X2) | ~ aElement0(X3)) | ~ aElementOf0(X2, X1)) & (? [X3] : ((sdtasdt0(X0, X3) = X2) & aElement0(X3)) | aElementOf0(X2, X1))) | ~ aSet0(X1)) & ((! [X2] : ((aElementOf0(X2, X1) | ! [X3] : (~ (sdtasdt0(X0, X3) = X2) | ~ aElement0(X3))) & (? [X3] : ((sdtasdt0(X0, X3) = X2) & aElement0(X3)) | ~ aElementOf0(X2, X1))) & aSet0(X1)) | ~ (slsdtgt0(X0) = X1))) | ~ aElement0(X0)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0] : (! [X1] : (((slsdtgt0(X0) = X1) | (? [X2] : ((! [X3] : (~ (sdtasdt0(X0, X3) = X2) | ~ aElement0(X3)) | ~ aElementOf0(X2, X1)) & (? [X3] : ((sdtasdt0(X0, X3) = X2) & aElement0(X3)) | aElementOf0(X2, X1))) | ~ aSet0(X1))) & ((! [X2] : ((aElementOf0(X2, X1) | ! [X3] : (~ (sdtasdt0(X0, X3) = X2) | ~ aElement0(X3))) & (? [X3] : ((sdtasdt0(X0, X3) = X2) & aElement0(X3)) | ~ aElementOf0(X2, X1))) & aSet0(X1)) | ~ (slsdtgt0(X0) = X1))) | ~ aElement0(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((slsdtgt0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X1) <=> ? [X3] : ((sdtasdt0(X0, X3) = X2) & aElement0(X3))) & aSet0(X1))) | ~ aElement0(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (aElement0(X0) => ! [X1] : ((slsdtgt0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X1) <=> ? [X3] : ((sdtasdt0(X0, X3) = X2) & aElement0(X3))) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mDefPrIdeal)).
fof(f404, plain, (~ aSet0(slsdtgt0(xb)) | spl22_6), inference(avatar_component_clause, [], [f402])).
fof(f402, plain, (spl22_6 <=> aSet0(slsdtgt0(xb))), introduced(avatar_definition, [new_symbols(naming, [spl22_6])])).
fof(f438, plain, spl22_5, inference(avatar_contradiction_clause, [], [f437])).
fof(f437, plain, ($false | spl22_5), inference(subsumption_resolution, [], [f436, f247])).
fof(f436, plain, (~ aElement0(xa) | spl22_5), inference(resolution, [], [f400, f269])).
fof(f400, plain, (~ aSet0(slsdtgt0(xa)) | spl22_5), inference(avatar_component_clause, [], [f398])).
fof(f398, plain, (spl22_5 <=> aSet0(slsdtgt0(xa))), introduced(avatar_definition, [new_symbols(naming, [spl22_5])])).
fof(f405, plain, (~ spl22_5 | ~ spl22_6 | spl22_3), inference(avatar_split_clause, [], [f396, f388, f402, f398])).
fof(f388, plain, (spl22_3 <=> sP1(slsdtgt0(xa), slsdtgt0(xb))), introduced(avatar_definition, [new_symbols(naming, [spl22_3])])).
fof(f396, plain, (~ aSet0(slsdtgt0(xb)) | ~ aSet0(slsdtgt0(xa)) | spl22_3), inference(resolution, [], [f390, f196])).
fof(f196, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f81, e106, e105])).
fof(f106, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e106])).
fof(e106, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f81, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f80])).
fof(f80, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', mDefSSum)).
fof(f390, plain, (~ sP1(slsdtgt0(xa), slsdtgt0(xb)) | spl22_3), inference(avatar_component_clause, [], [f388])).
fof(f395, plain, (~ spl22_3 | spl22_4), inference(avatar_split_clause, [], [f386, f392, f388])).
fof(f386, plain, (sP0(slsdtgt0(xb), slsdtgt0(xa), xI) | ~ sP1(slsdtgt0(xa), slsdtgt0(xb))), inference(superposition, [], [f258, f252])).
fof(f258, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt1(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f185])).
fof(f185, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ! [X0, X1] : (! [X2] : (((sdtpldt1(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f106])).
fof(f279, plain, (~ spl22_1 | ~ spl22_2), inference(avatar_split_clause, [], [f249, f276, f272])).
fof(f249, plain, (~ (sz00 = xb) | ~ (sz00 = xa)), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (sz00 = xb) | ~ (sz00 = xa)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG109+1.p', m__2110)).