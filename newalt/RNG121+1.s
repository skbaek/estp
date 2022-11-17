fof(f208803, plain, $false, inference(avatar_sat_refutation, [], [f511, f1534, f1548, f1845, f208548])).
fof(f208548, plain, ~ spl25_8, inference(avatar_contradiction_clause, [], [f208547])).
fof(f208547, plain, ($false | ~ spl25_8), inference(subsumption_resolution, [], [f208546, f272])).
fof(f272, plain, aElementOf0(xb, slsdtgt0(xb)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (aElementOf0(xb, slsdtgt0(xb)) & aElementOf0(sz00, slsdtgt0(xb)) & aElementOf0(xa, slsdtgt0(xa)) & aElementOf0(sz00, slsdtgt0(xa))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', m__2203)).
fof(f208546, plain, (~ aElementOf0(xb, slsdtgt0(xb)) | ~ spl25_8), inference(subsumption_resolution, [], [f208545, f269])).
fof(f269, plain, aElementOf0(sz00, slsdtgt0(xa)), inference(cnf_transformation, [], [f43])).
fof(f208545, plain, (~ aElementOf0(sz00, slsdtgt0(xa)) | ~ aElementOf0(xb, slsdtgt0(xb)) | ~ spl25_8), inference(subsumption_resolution, [], [f208407, f290])).
fof(f290, plain, ~ aElementOf0(xb, xI), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ~ aElementOf0(xb, xI), inference(flattening, [], [f54])).
fof(f54, plain, ~ aElementOf0(xb, xI), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ aElementOf0(xb, xI), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', m__)).
fof(f208407, plain, (aElementOf0(xb, xI) | ~ aElementOf0(sz00, slsdtgt0(xa)) | ~ aElementOf0(xb, slsdtgt0(xb)) | ~ spl25_8), inference(superposition, [], [f2242, f348])).
fof(f348, plain, (xb = sdtpldt0(sz00, xb)), inference(resolution, [], [f180, f264])).
fof(f264, plain, aElement0(xb), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aElement0(xb) & aElement0(xa)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', m__2091)).
fof(f180, plain, ! [X0] : (~ aElement0(X0) | (sdtpldt0(sz00, X0) = X0)), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0)) | ~ aElement0(X0)), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0] : (aElement0(X0) => ((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', mAddZero)).
fof(f2242, plain, (! [X2, X1] : (aElementOf0(sdtpldt0(X2, X1), xI) | ~ aElementOf0(X2, slsdtgt0(xa)) | ~ aElementOf0(X1, slsdtgt0(xb))) | ~ spl25_8), inference(resolution, [], [f510, f292])).
fof(f292, plain, ! [X2, X0, X10, X1, X9] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1) | aElementOf0(sdtpldt0(X9, X10), X2)), inference(equality_resolution, [], [f206])).
fof(f206, plain, ! [X2, X0, X10, X8, X1, X9] : (aElementOf0(X8, X2) | ~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1)) | aElementOf0(sK4(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6, sK7, sK8])], [f125, f128, f127, f126])).
fof(f126, plain, ! [X2, X1, X0] : (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) => ((! [X5, X4] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(sK4(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f127, plain, ! [X2, X1, X0] : (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) => ((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1))), introduced(choice_axiom, [])).
fof(f128, plain, ! [X8, X1, X0] : (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) => ((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f124])).
fof(f124, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f123])).
fof(f123, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f116])).
fof(f116, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), inference(usedef, [], [e116])).
fof(e116, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f510, plain, (sP0(slsdtgt0(xb), slsdtgt0(xa), xI) | ~ spl25_8), inference(avatar_component_clause, [], [f508])).
fof(f508, plain, (spl25_8 <=> sP0(slsdtgt0(xb), slsdtgt0(xa), xI)), introduced(avatar_definition, [new_symbols(naming, [spl25_8])])).
fof(f1845, plain, (spl25_7 | ~ spl25_34 | ~ spl25_35), inference(avatar_contradiction_clause, [], [f1844])).
fof(f1844, plain, ($false | (spl25_7 | ~ spl25_34 | ~ spl25_35)), inference(subsumption_resolution, [], [f1843, f1545])).
fof(f1545, plain, (aSet0(slsdtgt0(xa)) | ~ spl25_34), inference(resolution, [], [f1518, f219])).
fof(f219, plain, ! [X0] : (~ aIdeal0(X0) | aSet0(X0)), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ! [X0] : ((aIdeal0(X0) | (((~ aElementOf0(sdtasdt0(sK11(X0), sK10(X0)), X0) & aElement0(sK11(X0))) | (~ aElementOf0(sdtpldt0(sK10(X0), sK12(X0)), X0) & aElementOf0(sK12(X0), X0))) & aElementOf0(sK10(X0), X0)) | ~ aSet0(X0)) & ((! [X4] : ((! [X5] : (aElementOf0(sdtasdt0(X5, X4), X0) | ~ aElement0(X5)) & ! [X6] : (aElementOf0(sdtpldt0(X4, X6), X0) | ~ aElementOf0(X6, X0))) | ~ aElementOf0(X4, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12])], [f137, f140, f139, f138])).
fof(f138, plain, ! [X0] : (? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) => ((? [X2] : (~ aElementOf0(sdtasdt0(X2, sK10(X0)), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(sK10(X0), X3), X0) & aElementOf0(X3, X0))) & aElementOf0(sK10(X0), X0))), introduced(choice_axiom, [])).
fof(f139, plain, ! [X0] : (? [X2] : (~ aElementOf0(sdtasdt0(X2, sK10(X0)), X0) & aElement0(X2)) => (~ aElementOf0(sdtasdt0(sK11(X0), sK10(X0)), X0) & aElement0(sK11(X0)))), introduced(choice_axiom, [])).
fof(f140, plain, ! [X0] : (? [X3] : (~ aElementOf0(sdtpldt0(sK10(X0), X3), X0) & aElementOf0(X3, X0)) => (~ aElementOf0(sdtpldt0(sK10(X0), sK12(X0)), X0) & aElementOf0(sK12(X0), X0))), introduced(choice_axiom, [])).
fof(f137, plain, ! [X0] : ((aIdeal0(X0) | ? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) | ~ aSet0(X0)) & ((! [X4] : ((! [X5] : (aElementOf0(sdtasdt0(X5, X4), X0) | ~ aElement0(X5)) & ! [X6] : (aElementOf0(sdtpldt0(X4, X6), X0) | ~ aElementOf0(X6, X0))) | ~ aElementOf0(X4, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(rectify, [], [f136])).
fof(f136, plain, ! [X0] : ((aIdeal0(X0) | ? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) | ~ aSet0(X0)) & ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(flattening, [], [f135])).
fof(f135, plain, ! [X0] : ((aIdeal0(X0) | (? [X1] : ((? [X2] : (~ aElementOf0(sdtasdt0(X2, X1), X0) & aElement0(X2)) | ? [X3] : (~ aElementOf0(sdtpldt0(X1, X3), X0) & aElementOf0(X3, X0))) & aElementOf0(X1, X0)) | ~ aSet0(X0))) & ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0))), inference(nnf_transformation, [], [f93])).
fof(f93, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0))), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X2] : (aElementOf0(X2, X0) => aElementOf0(sdtpldt0(X1, X2), X0)))) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', mDefIdeal)).
fof(f1518, plain, (aIdeal0(slsdtgt0(xa)) | ~ spl25_34), inference(avatar_component_clause, [], [f1517])).
fof(f1517, plain, (spl25_34 <=> aIdeal0(slsdtgt0(xa))), introduced(avatar_definition, [new_symbols(naming, [spl25_34])])).
fof(f1843, plain, (~ aSet0(slsdtgt0(xa)) | (spl25_7 | ~ spl25_35)), inference(subsumption_resolution, [], [f1842, f1572])).
fof(f1572, plain, (aSet0(slsdtgt0(xb)) | ~ spl25_35), inference(resolution, [], [f1522, f219])).
fof(f1522, plain, (aIdeal0(slsdtgt0(xb)) | ~ spl25_35), inference(avatar_component_clause, [], [f1521])).
fof(f1521, plain, (spl25_35 <=> aIdeal0(slsdtgt0(xb))), introduced(avatar_definition, [new_symbols(naming, [spl25_35])])).
fof(f1842, plain, (~ aSet0(slsdtgt0(xb)) | ~ aSet0(slsdtgt0(xa)) | spl25_7), inference(resolution, [], [f506, f211])).
fof(f211, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f90, e117, e116])).
fof(f117, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e117])).
fof(e117, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f90, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f89])).
fof(f89, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', mDefSSum)).
fof(f506, plain, (~ sP1(slsdtgt0(xa), slsdtgt0(xb)) | spl25_7), inference(avatar_component_clause, [], [f504])).
fof(f504, plain, (spl25_7 <=> sP1(slsdtgt0(xa), slsdtgt0(xb))), introduced(avatar_definition, [new_symbols(naming, [spl25_7])])).
fof(f1548, plain, spl25_35, inference(avatar_contradiction_clause, [], [f1547])).
fof(f1547, plain, ($false | spl25_35), inference(subsumption_resolution, [], [f1546, f264])).
fof(f1546, plain, (~ aElement0(xb) | spl25_35), inference(resolution, [], [f1523, f262])).
fof(f262, plain, ! [X0] : (aIdeal0(slsdtgt0(X0)) | ~ aElement0(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : (aIdeal0(slsdtgt0(X0)) | ~ aElement0(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (aElement0(X0) => aIdeal0(slsdtgt0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', mPrIdeal)).
fof(f1523, plain, (~ aIdeal0(slsdtgt0(xb)) | spl25_35), inference(avatar_component_clause, [], [f1521])).
fof(f1534, plain, spl25_34, inference(avatar_contradiction_clause, [], [f1533])).
fof(f1533, plain, ($false | spl25_34), inference(subsumption_resolution, [], [f1532, f263])).
fof(f263, plain, aElement0(xa), inference(cnf_transformation, [], [f39])).
fof(f1532, plain, (~ aElement0(xa) | spl25_34), inference(resolution, [], [f1519, f262])).
fof(f1519, plain, (~ aIdeal0(slsdtgt0(xa)) | spl25_34), inference(avatar_component_clause, [], [f1517])).
fof(f511, plain, (~ spl25_7 | spl25_8), inference(avatar_split_clause, [], [f502, f508, f504])).
fof(f502, plain, (sP0(slsdtgt0(xb), slsdtgt0(xa), xI) | ~ sP1(slsdtgt0(xa), slsdtgt0(xb))), inference(superposition, [], [f291, f268])).
fof(f268, plain, (xI = sdtpldt1(slsdtgt0(xa), slsdtgt0(xb))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((xI = sdtpldt1(slsdtgt0(xa), slsdtgt0(xb))) & aIdeal0(xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG121+1.p', m__2174)).
fof(f291, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt1(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f200])).
fof(f200, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f122])).
fof(f122, plain, ! [X0, X1] : (! [X2] : (((sdtpldt1(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f117])).