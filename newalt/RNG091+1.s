fof(f143626, plain, $false, inference(avatar_sat_refutation, [], [f172, f255, f3451, f142801, f143614])).
fof(f143614, plain, (spl10_2 | ~ spl10_4 | ~ spl10_23), inference(avatar_contradiction_clause, [], [f143613])).
fof(f143613, plain, ($false | (spl10_2 | ~ spl10_4 | ~ spl10_23)), inference(subsumption_resolution, [], [f143612, f2616])).
fof(f2616, plain, (sP1(xI, xJ) | ~ spl10_23), inference(avatar_component_clause, [], [f2615])).
fof(f2615, plain, (spl10_23 <=> sP1(xI, xJ)), introduced(avatar_definition, [new_symbols(naming, [spl10_23])])).
fof(f143612, plain, (~ sP1(xI, xJ) | (spl10_2 | ~ spl10_4)), inference(subsumption_resolution, [], [f143611, f154])).
fof(f154, plain, aElementOf0(sdtasdt0(xz, xl), xJ), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (aElementOf0(sdtasdt0(xz, xl), xJ) & aElementOf0(sdtasdt0(xz, xk), xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__1021)).
fof(f143611, plain, (~ aElementOf0(sdtasdt0(xz, xl), xJ) | ~ sP1(xI, xJ) | (spl10_2 | ~ spl10_4)), inference(subsumption_resolution, [], [f143597, f153])).
fof(f153, plain, aElementOf0(sdtasdt0(xz, xk), xI), inference(cnf_transformation, [], [f30])).
fof(f143597, plain, (~ aElementOf0(sdtasdt0(xz, xk), xI) | ~ aElementOf0(sdtasdt0(xz, xl), xJ) | ~ sP1(xI, xJ) | (spl10_2 | ~ spl10_4)), inference(resolution, [], [f2597, f142859])).
fof(f142859, plain, (~ aElementOf0(sdtasdt0(xx, xz), sdtpldt1(xI, xJ)) | (spl10_2 | ~ spl10_4)), inference(forward_demodulation, [], [f171, f912])).
fof(f912, plain, ((sdtasdt0(xz, xx) = sdtasdt0(xx, xz)) | ~ spl10_4), inference(resolution, [], [f438, f220])).
fof(f220, plain, (aElement0(xx) | ~ spl10_4), inference(avatar_component_clause, [], [f218])).
fof(f218, plain, (spl10_4 <=> aElement0(xx)), introduced(avatar_definition, [new_symbols(naming, [spl10_4])])).
fof(f438, plain, ! [X18] : (~ aElement0(X18) | (sdtasdt0(X18, xz) = sdtasdt0(xz, X18))), inference(resolution, [], [f101, f144])).
fof(f144, plain, aElement0(xz), inference(cnf_transformation, [], [f26])).
fof(f26, plain, (aElement0(xz) & aElementOf0(xy, sdtpldt1(xI, xJ)) & aElementOf0(xx, sdtpldt1(xI, xJ))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__901)).
fof(f101, plain, ! [X0, X1] : (~ aElement0(X1) | (sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aElement0(X0)), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aElement0(X1) | ~ aElement0(X0)), inference(flattening, [], [f51])).
fof(f51, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | (~ aElement0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1] : ((aElement0(X1) & aElement0(X0)) => (sdtasdt0(X0, X1) = sdtasdt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', mMulComm)).
fof(f171, plain, (~ aElementOf0(sdtasdt0(xz, xx), sdtpldt1(xI, xJ)) | spl10_2), inference(avatar_component_clause, [], [f169])).
fof(f169, plain, (spl10_2 <=> aElementOf0(sdtasdt0(xz, xx), sdtpldt1(xI, xJ))), introduced(avatar_definition, [new_symbols(naming, [spl10_2])])).
fof(f2597, plain, (! [X41, X40] : (aElementOf0(sdtasdt0(xx, xz), sdtpldt1(X40, X41)) | ~ aElementOf0(sdtasdt0(xz, xk), X40) | ~ aElementOf0(sdtasdt0(xz, xl), X41) | ~ sP1(X40, X41)) | ~ spl10_4), inference(superposition, [], [f499, f922])).
fof(f922, plain, ((sdtpldt0(sdtasdt0(xz, xk), sdtasdt0(xz, xl)) = sdtasdt0(xx, xz)) | ~ spl10_4), inference(backward_demodulation, [], [f156, f912])).
fof(f156, plain, (sdtasdt0(xz, xx) = sdtpldt0(sdtasdt0(xz, xk), sdtasdt0(xz, xl))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (sdtasdt0(xz, xx) = sdtpldt0(sdtasdt0(xz, xk), sdtasdt0(xz, xl))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__1098)).
fof(f499, plain, ! [X2, X0, X3, X1] : (aElementOf0(sdtpldt0(X2, X0), sdtpldt1(X3, X1)) | ~ aElementOf0(X2, X3) | ~ aElementOf0(X0, X1) | ~ sP1(X3, X1)), inference(resolution, [], [f159, f158])).
fof(f158, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt1(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f118])).
fof(f118, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ! [X0, X1] : (! [X2] : (((sdtpldt1(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt1(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f72])).
fof(f72, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e72])).
fof(e72, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f159, plain, ! [X2, X0, X10, X1, X9] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1) | aElementOf0(sdtpldt0(X9, X10), X2)), inference(equality_resolution, [], [f124])).
fof(f124, plain, ! [X2, X0, X10, X8, X1, X9] : (aElementOf0(X8, X2) | ~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1)) | aElementOf0(sK4(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6, sK7, sK8])], [f80, f83, f82, f81])).
fof(f81, plain, ! [X2, X1, X0] : (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) => ((! [X5, X4] : (~ (sdtpldt0(X4, X5) = sK4(X0, X1, X2)) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(sK4(X0, X1, X2), X2)) & (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(sK4(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f82, plain, ! [X2, X1, X0] : (? [X7, X6] : ((sdtpldt0(X6, X7) = sK4(X0, X1, X2)) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) => ((sK4(X0, X1, X2) = sdtpldt0(sK5(X0, X1, X2), sK6(X0, X1, X2))) & aElementOf0(sK6(X0, X1, X2), X0) & aElementOf0(sK5(X0, X1, X2), X1))), introduced(choice_axiom, [])).
fof(f83, plain, ! [X8, X1, X0] : (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) => ((sdtpldt0(sK7(X0, X1, X8), sK8(X0, X1, X8)) = X8) & aElementOf0(sK8(X0, X1, X8), X0) & aElementOf0(sK7(X0, X1, X8), X1))), introduced(choice_axiom, [])).
fof(f80, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X0) | ~ aElementOf0(X4, X1)) | ~ aElementOf0(X3, X2)) & (? [X6, X7] : ((sdtpldt0(X6, X7) = X3) & aElementOf0(X7, X0) & aElementOf0(X6, X1)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X8] : ((aElementOf0(X8, X2) | ! [X9, X10] : (~ (sdtpldt0(X9, X10) = X8) | ~ aElementOf0(X10, X0) | ~ aElementOf0(X9, X1))) & (? [X11, X12] : ((sdtpldt0(X11, X12) = X8) & aElementOf0(X12, X0) & aElementOf0(X11, X1)) | ~ aElementOf0(X8, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f79])).
fof(f79, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f78])).
fof(f78, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2)) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ! [X4, X5] : (~ (sdtpldt0(X4, X5) = X3) | ~ aElementOf0(X5, X1) | ~ aElementOf0(X4, X0))) & (? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f71])).
fof(f71, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), inference(usedef, [], [e71])).
fof(e71, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f142801, plain, (spl10_1 | ~ spl10_23), inference(avatar_contradiction_clause, [], [f142800])).
fof(f142800, plain, ($false | (spl10_1 | ~ spl10_23)), inference(subsumption_resolution, [], [f142799, f2616])).
fof(f142799, plain, (~ sP1(xI, xJ) | spl10_1), inference(subsumption_resolution, [], [f142798, f152])).
fof(f152, plain, aElementOf0(sdtpldt0(xl, xn), xJ), inference(cnf_transformation, [], [f29])).
fof(f29, plain, (aElementOf0(sdtpldt0(xl, xn), xJ) & aElementOf0(sdtpldt0(xk, xm), xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__994)).
fof(f142798, plain, (~ aElementOf0(sdtpldt0(xl, xn), xJ) | ~ sP1(xI, xJ) | spl10_1), inference(subsumption_resolution, [], [f142784, f151])).
fof(f151, plain, aElementOf0(sdtpldt0(xk, xm), xI), inference(cnf_transformation, [], [f29])).
fof(f142784, plain, (~ aElementOf0(sdtpldt0(xk, xm), xI) | ~ aElementOf0(sdtpldt0(xl, xn), xJ) | ~ sP1(xI, xJ) | spl10_1), inference(resolution, [], [f2596, f167])).
fof(f167, plain, (~ aElementOf0(sdtpldt0(xx, xy), sdtpldt1(xI, xJ)) | spl10_1), inference(avatar_component_clause, [], [f165])).
fof(f165, plain, (spl10_1 <=> aElementOf0(sdtpldt0(xx, xy), sdtpldt1(xI, xJ))), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f2596, plain, ! [X39, X38] : (aElementOf0(sdtpldt0(xx, xy), sdtpldt1(X38, X39)) | ~ aElementOf0(sdtpldt0(xk, xm), X38) | ~ aElementOf0(sdtpldt0(xl, xn), X39) | ~ sP1(X38, X39)), inference(superposition, [], [f499, f155])).
fof(f155, plain, (sdtpldt0(xx, xy) = sdtpldt0(sdtpldt0(xk, xm), sdtpldt0(xl, xn))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (sdtpldt0(xx, xy) = sdtpldt0(sdtpldt0(xk, xm), sdtpldt0(xl, xn))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__1061)).
fof(f3451, plain, spl10_23, inference(avatar_contradiction_clause, [], [f3450])).
fof(f3450, plain, ($false | spl10_23), inference(subsumption_resolution, [], [f3449, f173])).
fof(f173, plain, aSet0(xI), inference(resolution, [], [f137, f140])).
fof(f140, plain, aIdeal0(xI), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (aIdeal0(xJ) & aIdeal0(xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__870)).
fof(f137, plain, ! [X0] : (~ aIdeal0(X0) | aSet0(X0)), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ! [X0] : ((! [X1] : ((! [X2] : (aElementOf0(sdtasdt0(X2, X1), X0) | ~ aElement0(X2)) & ! [X3] : (aElementOf0(sdtpldt0(X1, X3), X0) | ~ aElementOf0(X3, X0))) | ~ aElementOf0(X1, X0)) & aSet0(X0)) | ~ aIdeal0(X0)), inference(ennf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (aIdeal0(X0) => (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(unused_predicate_definition_removal, [], [f38])).
fof(f38, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X3] : (aElementOf0(X3, X0) => aElementOf0(sdtpldt0(X1, X3), X0)))) & aSet0(X0))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X0] : (aIdeal0(X0) <=> (! [X1] : (aElementOf0(X1, X0) => (! [X2] : (aElement0(X2) => aElementOf0(sdtasdt0(X2, X1), X0)) & ! [X2] : (aElementOf0(X2, X0) => aElementOf0(sdtpldt0(X1, X2), X0)))) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', mDefIdeal)).
fof(f3449, plain, (~ aSet0(xI) | spl10_23), inference(subsumption_resolution, [], [f3448, f174])).
fof(f174, plain, aSet0(xJ), inference(resolution, [], [f137, f141])).
fof(f141, plain, aIdeal0(xJ), inference(cnf_transformation, [], [f25])).
fof(f3448, plain, (~ aSet0(xJ) | ~ aSet0(xI) | spl10_23), inference(resolution, [], [f2617, f129])).
fof(f129, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f66, e72, e71])).
fof(f66, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f65])).
fof(f65, plain, ! [X0, X1] : (! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt1(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ? [X4, X5] : ((sdtpldt0(X4, X5) = X3) & aElementOf0(X5, X1) & aElementOf0(X4, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', mDefSSum)).
fof(f2617, plain, (~ sP1(xI, xJ) | spl10_23), inference(avatar_component_clause, [], [f2615])).
fof(f255, plain, spl10_4, inference(avatar_split_clause, [], [f254, f218])).
fof(f254, plain, aElement0(xx), inference(subsumption_resolution, [], [f253, f227])).
fof(f227, plain, aElement0(xk), inference(subsumption_resolution, [], [f205, f173])).
fof(f205, plain, (aElement0(xk) | ~ aSet0(xI)), inference(resolution, [], [f113, f145])).
fof(f145, plain, aElementOf0(xk, xI), inference(cnf_transformation, [], [f27])).
fof(f27, plain, ((xx = sdtpldt0(xk, xl)) & aElementOf0(xl, xJ) & aElementOf0(xk, xI)), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__934)).
fof(f113, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', mEOfElem)).
fof(f253, plain, (aElement0(xx) | ~ aElement0(xk)), inference(subsumption_resolution, [], [f251, f228])).
fof(f228, plain, aElement0(xl), inference(subsumption_resolution, [], [f206, f174])).
fof(f206, plain, (aElement0(xl) | ~ aSet0(xJ)), inference(resolution, [], [f113, f146])).
fof(f146, plain, aElementOf0(xl, xJ), inference(cnf_transformation, [], [f27])).
fof(f251, plain, (aElement0(xx) | ~ aElement0(xl) | ~ aElement0(xk)), inference(superposition, [], [f93, f147])).
fof(f147, plain, (xx = sdtpldt0(xk, xl)), inference(cnf_transformation, [], [f27])).
fof(f93, plain, ! [X0, X1] : (aElement0(sdtpldt0(X0, X1)) | ~ aElement0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ! [X0, X1] : (aElement0(sdtpldt0(X0, X1)) | ~ aElement0(X1) | ~ aElement0(X0)), inference(flattening, [], [f41])).
fof(f41, plain, ! [X0, X1] : (aElement0(sdtpldt0(X0, X1)) | (~ aElement0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aElement0(X1) & aElement0(X0)) => aElement0(sdtpldt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', mSortsB)).
fof(f172, plain, (~ spl10_1 | ~ spl10_2), inference(avatar_split_clause, [], [f157, f169, f165])).
fof(f157, plain, (~ aElementOf0(sdtasdt0(xz, xx), sdtpldt1(xI, xJ)) | ~ aElementOf0(sdtpldt0(xx, xy), sdtpldt1(xI, xJ))), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (~ aElementOf0(sdtasdt0(xz, xx), sdtpldt1(xI, xJ)) | ~ aElementOf0(sdtpldt0(xx, xy), sdtpldt1(xI, xJ))), inference(ennf_transformation, [], [f34])).
fof(f34, plain, ~ (aElementOf0(sdtasdt0(xz, xx), sdtpldt1(xI, xJ)) & aElementOf0(sdtpldt0(xx, xy), sdtpldt1(xI, xJ))), inference(negated_conjecture, [], [f33])).
fof(f33, plain, ~ (aElementOf0(sdtasdt0(xz, xx), sdtpldt1(xI, xJ)) & aElementOf0(sdtpldt0(xx, xy), sdtpldt1(xI, xJ))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG091+1.p', m__)).