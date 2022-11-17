fof(f1629, plain, $false, inference(avatar_sat_refutation, [], [f658, f1601, f1604, f1607, f1610, f1613, f1616, f1619, f1622, f1625, f1628])).
fof(f1628, plain, spl34_10, inference(avatar_split_clause, [], [f1627, f655])).
fof(f655, plain, (spl34_10 <=> holdsAt(spinning(trolley10), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_10])])).
fof(f1627, plain, holdsAt(spinning(trolley10), n1), inference(forward_demodulation, [], [f1626, f315])).
fof(f315, plain, (n1 = plus(n0, n1)), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (n1 = plus(n0, n1)), file('/home/ubuntu/library/tptp/Problems/CSR/CSR024+1.010.p', plus0_1)).
fof(f1626, plain, holdsAt(spinning(trolley10), plus(n0, n1)), inference(subsumption_resolution, [], [f1597, f780])).
fof(f780, plain, happens(push(agent10, trolley10), n0), inference(resolution, [], [f430, f574])).
fof(f574, plain, sP25(n0, push(agent10, trolley10)), inference(equality_resolution, [], [f573])).
fof(f573, plain, ! [X1] : (sP25(n0, X1) | ~ (push(agent10, trolley10) = X1)), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X0, X1] : (sP25(X0, X1) | ~ (n0 = X0) | ~ (push(agent10, trolley10) = X1)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0, X1] : ((sP25(X0, X1) | ((~ (n0 = X0) | ~ (push(agent10, trolley10) = X1)) & (~ (n0 = X0) | ~ (pull(agent10, trolley10) = X1)) & (~ (n0 = X0) | ~ (push(agent9, trolley9) = X1)) & ~ sP24(X0, X1) & ~ sP23(X0, X1) & ~ sP22(X0, X1) & ~ sP21(X0, X1) & ~ sP20(X0, X1) & ~ sP19(X0, X1) & ~ sP18(X0, X1) & ~ sP17(X0, X1) & ~ sP16(X0, X1) & ~ sP15(X0, X1) & ~ sP14(X0, X1) & ~ sP13(X0, X1) & ~ sP12(X0, X1) & ~ sP11(X0, X1) & ~ sP10(X0, X1) & ~ sP9(X0, X1) & ~ sP8(X0, X1))) & (((n0 = X0) & (push(agent10, trolley10) = X1)) | ((n0 = X0) & (pull(agent10, trolley10) = X1)) | ((n0 = X0) & (push(agent9, trolley9) = X1)) | sP24(X0, X1) | sP23(X0, X1) | sP22(X0, X1) | sP21(X0, X1) | sP20(X0, X1) | sP19(X0, X1) | sP18(X0, X1) | sP17(X0, X1) | sP16(X0, X1) | sP15(X0, X1) | sP14(X0, X1) | sP13(X0, X1) | sP12(X0, X1) | sP11(X0, X1) | sP10(X0, X1) | sP9(X0, X1) | sP8(X0, X1) | ~ sP25(X0, X1))), inference(rectify, [], [f195])).
fof(f195, plain, ! [X1, X0] : ((sP25(X1, X0) | ((~ (n0 = X1) | ~ (push(agent10, trolley10) = X0)) & (~ (n0 = X1) | ~ (pull(agent10, trolley10) = X0)) & (~ (n0 = X1) | ~ (push(agent9, trolley9) = X0)) & ~ sP24(X1, X0) & ~ sP23(X1, X0) & ~ sP22(X1, X0) & ~ sP21(X1, X0) & ~ sP20(X1, X0) & ~ sP19(X1, X0) & ~ sP18(X1, X0) & ~ sP17(X1, X0) & ~ sP16(X1, X0) & ~ sP15(X1, X0) & ~ sP14(X1, X0) & ~ sP13(X1, X0) & ~ sP12(X1, X0) & ~ sP11(X1, X0) & ~ sP10(X1, X0) & ~ sP9(X1, X0) & ~ sP8(X1, X0))) & (((n0 = X1) & (push(agent10, trolley10) = X0)) | ((n0 = X1) & (pull(agent10, trolley10) = X0)) | ((n0 = X1) & (push(agent9, trolley9) = X0)) | sP24(X1, X0) | sP23(X1, X0) | sP22(X1, X0) | sP21(X1, X0) | sP20(X1, X0) | sP19(X1, X0) | sP18(X1, X0) | sP17(X1, X0) | sP16(X1, X0) | sP15(X1, X0) | sP14(X1, X0) | sP13(X1, X0) | sP12(X1, X0) | sP11(X1, X0) | sP10(X1, X0) | sP9(X1, X0) | sP8(X1, X0) | ~ sP25(X1, X0))), inference(flattening, [], [f194])).
fof(f194, plain, ! [X1, X0] : ((sP25(X1, X0) | ((~ (n0 = X1) | ~ (push(agent10, trolley10) = X0)) & (~ (n0 = X1) | ~ (pull(agent10, trolley10) = X0)) & (~ (n0 = X1) | ~ (push(agent9, trolley9) = X0)) & ~ sP24(X1, X0) & ~ sP23(X1, X0) & ~ sP22(X1, X0) & ~ sP21(X1, X0) & ~ sP20(X1, X0) & ~ sP19(X1, X0) & ~ sP18(X1, X0) & ~ sP17(X1, X0) & ~ sP16(X1, X0) & ~ sP15(X1, X0) & ~ sP14(X1, X0) & ~ sP13(X1, X0) & ~ sP12(X1, X0) & ~ sP11(X1, X0) & ~ sP10(X1, X0) & ~ sP9(X1, X0) & ~ sP8(X1, X0))) & ((((n0 = X1) & (push(agent10, trolley10) = X0)) | ((n0 = X1) & (pull(agent10, trolley10) = X0)) | ((n0 = X1) & (push(agent9, trolley9) = X0)) | sP24(X1, X0) | sP23(X1, X0) | sP22(X1, X0) | sP21(X1, X0) | sP20(X1, X0) | sP19(X1, X0) | sP18(X1, X0) | sP17(X1, X0) | sP16(X1, X0) | sP15(X1, X0) | sP14(X1, X0) | sP13(X1, X0) | sP12(X1, X0) | sP11(X1, X0) | sP10(X1, X0) | sP9(X1, X0) | sP8(X1, X0)) | ~ sP25(X1, X0))), inference(nnf_transformation, [], [f141])).
fof(f141, plain, ! [X1, X0] : (sP25(X1, X0) <=> (((n0 = X1) & (push(agent10, trolley10) = X0)) | ((n0 = X1) & (pull(agent10, trolley10) = X0)) | ((n0 = X1) & (push(agent9, trolley9) = X0)) | sP24(X1, X0) | sP23(X1, X0) | sP22(X1, X0) | sP21(X1, X0) | sP20(X1, X0) | sP19(X1, X0) | sP18(X1, X0) | sP17(X1, X0) | sP16(X1, X0) | sP15(X1, X0) | sP14(X1, X0) | sP13(X1, X0) | sP12(X1, X0) | sP11(X1, X0) | sP10(X1, X0) | sP9(X1, X0) | sP8(X1, X0))), inference(usedef, [], [e141])).
fof(e141, plain, ! [X1, X0] : (sP25(X1, X0) <=> (((n0 = X1) & (push(agent10, trolley10) = X0)) | ((n0 = X1) & (pull(agent10, trolley10) = X0)) | ((n0 = X1) & (push(agent9, trolley9) = X0)) | sP24(X1, X0) | sP23(X1, X0) | sP22(X1, X0) | sP21(X1, X0) | sP20(X1, X0) | sP19(X1, X0) | sP18(X1, X0) | sP17(X1, X0) | sP16(X1, X0) | sP15(X1, X0) | sP14(X1, X0) | sP13(X1, X0) | sP12(X1, X0) | sP11(X1, X0) | sP10(X1, X0) | sP9(X1, X0) | sP8(X1, X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f430, plain, ! [X0, X1] : (~ sP25(X1, X0) | happens(X0, X1)), inference(cnf_transformation, [], [f248])).
fof(f248, plain, ! [X0, X1] : ((happens(X0, X1) | ~ sP25(X1, X0)) & (sP25(X1, X0) | ~ happens(X0, X1))), inference(nnf_transformation, [], [f142])).
fof(f142, plain, ! [X0, X1] : (happens(X0, X1) <=> sP25(X1, X0)), inference(definition_folding, [], [f84, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124])).
fof(f124, plain, ! [X1, X0] : (sP8(X1, X0) <=> ((n0 = X1) & (pull(agent1, trolley1) = X0))), inference(usedef, [], [e124])).
fof(e124, plain, ! [X1, X0] : (sP8(X1, X0) <=> ((n0 = X1) & (pull(agent1, trolley1) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f125, plain, ! [X1, X0] : (sP9(X1, X0) <=> ((n0 = X1) & (push(agent1, trolley1) = X0))), inference(usedef, [], [e125])).
fof(e125, plain, ! [X1, X0] : (sP9(X1, X0) <=> ((n0 = X1) & (push(agent1, trolley1) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f126, plain, ! [X1, X0] : (sP10(X1, X0) <=> ((n0 = X1) & (pull(agent2, trolley2) = X0))), inference(usedef, [], [e126])).
fof(e126, plain, ! [X1, X0] : (sP10(X1, X0) <=> ((n0 = X1) & (pull(agent2, trolley2) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f127, plain, ! [X1, X0] : (sP11(X1, X0) <=> ((n0 = X1) & (push(agent2, trolley2) = X0))), inference(usedef, [], [e127])).
fof(e127, plain, ! [X1, X0] : (sP11(X1, X0) <=> ((n0 = X1) & (push(agent2, trolley2) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f128, plain, ! [X1, X0] : (sP12(X1, X0) <=> ((n0 = X1) & (pull(agent3, trolley3) = X0))), inference(usedef, [], [e128])).
fof(e128, plain, ! [X1, X0] : (sP12(X1, X0) <=> ((n0 = X1) & (pull(agent3, trolley3) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f129, plain, ! [X1, X0] : (sP13(X1, X0) <=> ((n0 = X1) & (push(agent3, trolley3) = X0))), inference(usedef, [], [e129])).
fof(e129, plain, ! [X1, X0] : (sP13(X1, X0) <=> ((n0 = X1) & (push(agent3, trolley3) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f130, plain, ! [X1, X0] : (sP14(X1, X0) <=> ((n0 = X1) & (pull(agent4, trolley4) = X0))), inference(usedef, [], [e130])).
fof(e130, plain, ! [X1, X0] : (sP14(X1, X0) <=> ((n0 = X1) & (pull(agent4, trolley4) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f131, plain, ! [X1, X0] : (sP15(X1, X0) <=> ((n0 = X1) & (push(agent4, trolley4) = X0))), inference(usedef, [], [e131])).
fof(e131, plain, ! [X1, X0] : (sP15(X1, X0) <=> ((n0 = X1) & (push(agent4, trolley4) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f132, plain, ! [X1, X0] : (sP16(X1, X0) <=> ((n0 = X1) & (pull(agent5, trolley5) = X0))), inference(usedef, [], [e132])).
fof(e132, plain, ! [X1, X0] : (sP16(X1, X0) <=> ((n0 = X1) & (pull(agent5, trolley5) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f133, plain, ! [X1, X0] : (sP17(X1, X0) <=> ((n0 = X1) & (push(agent5, trolley5) = X0))), inference(usedef, [], [e133])).
fof(e133, plain, ! [X1, X0] : (sP17(X1, X0) <=> ((n0 = X1) & (push(agent5, trolley5) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f134, plain, ! [X1, X0] : (sP18(X1, X0) <=> ((n0 = X1) & (pull(agent6, trolley6) = X0))), inference(usedef, [], [e134])).
fof(e134, plain, ! [X1, X0] : (sP18(X1, X0) <=> ((n0 = X1) & (pull(agent6, trolley6) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f135, plain, ! [X1, X0] : (sP19(X1, X0) <=> ((n0 = X1) & (push(agent6, trolley6) = X0))), inference(usedef, [], [e135])).
fof(e135, plain, ! [X1, X0] : (sP19(X1, X0) <=> ((n0 = X1) & (push(agent6, trolley6) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f136, plain, ! [X1, X0] : (sP20(X1, X0) <=> ((n0 = X1) & (pull(agent7, trolley7) = X0))), inference(usedef, [], [e136])).
fof(e136, plain, ! [X1, X0] : (sP20(X1, X0) <=> ((n0 = X1) & (pull(agent7, trolley7) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f137, plain, ! [X1, X0] : (sP21(X1, X0) <=> ((n0 = X1) & (push(agent7, trolley7) = X0))), inference(usedef, [], [e137])).
fof(e137, plain, ! [X1, X0] : (sP21(X1, X0) <=> ((n0 = X1) & (push(agent7, trolley7) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f138, plain, ! [X1, X0] : (sP22(X1, X0) <=> ((n0 = X1) & (pull(agent8, trolley8) = X0))), inference(usedef, [], [e138])).
fof(e138, plain, ! [X1, X0] : (sP22(X1, X0) <=> ((n0 = X1) & (pull(agent8, trolley8) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f139, plain, ! [X1, X0] : (sP23(X1, X0) <=> ((n0 = X1) & (push(agent8, trolley8) = X0))), inference(usedef, [], [e139])).
fof(e139, plain, ! [X1, X0] : (sP23(X1, X0) <=> ((n0 = X1) & (push(agent8, trolley8) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f140, plain, ! [X1, X0] : (sP24(X1, X0) <=> ((n0 = X1) & (pull(agent9, trolley9) = X0))), inference(usedef, [], [e140])).
fof(e140, plain, ! [X1, X0] : (sP24(X1, X0) <=> ((n0 = X1) & (pull(agent9, trolley9) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f84, plain, ! [X0, X1] : (happens(X0, X1) <=> (((n0 = X1) & (push(agent10, trolley10) = X0)) | ((n0 = X1) & (pull(agent10, trolley10) = X0)) | ((n0 = X1) & (push(agent9, trolley9) = X0)) | ((n0 = X1) & (pull(agent9, trolley9) = X0)) | ((n0 = X1) & (push(agent8, trolley8) = X0)) | ((n0 = X1) & (pull(agent8, trolley8) = X0)) | ((n0 = X1) & (push(agent7, trolley7) = X0)) | ((n0 = X1) & (pull(agent7, trolley7) = X0)) | ((n0 = X1) & (push(agent6, trolley6) = X0)) | ((n0 = X1) & (pull(agent6, trolley6) = X0)) | ((n0 = X1) & (push(agent5, trolley5) = X0)) | ((n0 = X1) & (pull(agent5, trolley5) = X0)) | ((n0 = X1) & (push(agent4, trolley4) = X0)) | ((n0 = X1) & (pull(agent4, trolley4) = X0)) | ((n0 = X1) & (push(agent3, trolley3) = X0)) | ((n0 = X1) & (pull(agent3, trolley3) = X0)) | ((n0 = X1) & (push(agent2, trolley2) = X0)) | ((n0 = X1) & (pull(agent2, trolley2) = X0)) | ((n0 = X1) & (push(agent1, trolley1) = X0)) | ((n0 = X1) & (pull(agent1, trolley1) = X0)))), inference(rectify, [], [f45])).
fof(f45, plain, ! [X3, X4] : (happens(X3, X4) <=> (((n0 = X4) & (push(agent10, trolley10) = X3)) | ((n0 = X4) & (pull(agent10, trolley10) = X3)) | ((n0 = X4) & (push(agent9, trolley9) = X3)) | ((n0 = X4) & (pull(agent9, trolley9) = X3)) | ((n0 = X4) & (push(agent8, trolley8) = X3)) | ((n0 = X4) & (pull(agent8, trolley8) = X3)) | ((n0 = X4) & (push(agent7, trolley7) = X3)) | ((n0 = X4) & (pull(agent7, trolley7) = X3)) | ((n0 = X4) & (push(agent6, trolley6) = X3)) | ((n0 = X4) & (pull(agent6, trolley6) = X3)) | ((n0 = X4) & (push(agent5, trolley5) = X3)) | ((n0 = X4) & (pull(agent5, trolley5) = X3)) | ((n0 = X4) & (push(agent4, trolley4) = X3)) | ((n0 = X4) & (pull(agent4, trolley4) = X3)) | ((n0 = X4) & (push(agent3, trolley3) = X3)) | ((n0 = X4) & (pull(agent3, trolley3) = X3)) | ((n0 = X4) & (push(agent2, trolley2) = X3)) | ((n0 = X4) & (pull(agent2, trolley2) = X3)) | ((n0 = X4) & (push(agent1, trolley1) = X3)) | ((n0 = X4) & (pull(agent1, trolley1) = X3)))), file('/home/ubuntu/library/tptp/Problems/CSR/CSR024+1.010.p', happens_all_defn)).
fof(f1597, plain, (holdsAt(spinning(trolley10), plus(n0, n1)) | ~ happens(push(agent10, trolley10), n0)), inference(resolution, [], [f1016, f782])).
fof(f782, plain, happens(pull(agent10, trolley10), n0), inference(resolution, [], [f430, f576])).
fof(f576, plain, sP25(n0, pull(agent10, trolley10)), inference(equality_resolution, [], [f575])).
fof(f575, plain, ! [X1] : (sP25(n0, X1) | ~ (pull(agent10, trolley10) = X1)), inference(equality_resolution, [], [f376])).
fof(f376, plain, ! [X0, X1] : (sP25(X0, X1) | ~ (n0 = X0) | ~ (pull(agent10, trolley10) = X1)), inference(cnf_transformation, [], [f196])).
fof(f1016, plain, ! [X2, X0, X1] : (~ happens(pull(X0, X1), X2) | holdsAt(spinning(X1), plus(X2, n1)) | ~ happens(push(X0, X1), X2)), inference(resolution, [], [f558, f257])).
fof(f257, plain, ! [X2, X0, X1] : (~ initiates(X0, X2, X1) | holdsAt(X2, plus(X1, n1)) | ~ happens(X0, X1)), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : (holdsAt(X2, plus(X1, n1)) | ~ initiates(X0, X2, X1) | ~ happens(X0, X1)), inference(flattening, [], [f100])).
fof(f100, plain, ! [X0, X1, X2] : (holdsAt(X2, plus(X1, n1)) | (~ initiates(X0, X2, X1) | ~ happens(X0, X1))), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0, X1, X2] : ((initiates(X0, X2, X1) & happens(X0, X1)) => holdsAt(X2, plus(X1, n1))), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3, X4, X1] : ((initiates(X3, X1, X4) & happens(X3, X4)) => holdsAt(X1, plus(X4, n1))), file('/home/ubuntu/library/tptp/Problems/CSR/CSR024+1.010.p', happens_holds)).
fof(f558, plain, ! [X4, X2, X3] : (initiates(pull(X3, X4), spinning(X4), X2) | ~ happens(push(X3, X4), X2)), inference(equality_resolution, [], [f557])).
fof(f557, plain, ! [X4, X2, X0, X3] : (initiates(X0, spinning(X4), X2) | ~ happens(push(X3, X4), X2) | ~ (pull(X3, X4) = X0)), inference(equality_resolution, [], [f275])).
fof(f275, plain, ! [X4, X2, X0, X3, X1] : (initiates(X0, X1, X2) | ~ happens(push(X3, X4), X2) | ~ (spinning(X4) = X1) | ~ (pull(X3, X4) = X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0, X1, X2] : ((initiates(X0, X1, X2) | ! [X3, X4] : ((~ happens(push(X3, X4), X2) | ~ (spinning(X4) = X1) | ~ (pull(X3, X4) = X0)) & ~ sP1(X2, X4, X3, X1, X0) & ~ sP0(X2, X4, X3, X1, X0))) & (((happens(push(sK30(X0, X1, X2), sK31(X0, X1, X2)), X2) & (spinning(sK31(X0, X1, X2)) = X1) & (pull(sK30(X0, X1, X2), sK31(X0, X1, X2)) = X0)) | sP1(X2, sK31(X0, X1, X2), sK30(X0, X1, X2), X1, X0) | sP0(X2, sK31(X0, X1, X2), sK30(X0, X1, X2), X1, X0)) | ~ initiates(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK30, sK31])], [f158, f159])).
fof(f159, plain, ! [X2, X1, X0] : (? [X5, X6] : ((happens(push(X5, X6), X2) & (spinning(X6) = X1) & (pull(X5, X6) = X0)) | sP1(X2, X6, X5, X1, X0) | sP0(X2, X6, X5, X1, X0)) => ((happens(push(sK30(X0, X1, X2), sK31(X0, X1, X2)), X2) & (spinning(sK31(X0, X1, X2)) = X1) & (pull(sK30(X0, X1, X2), sK31(X0, X1, X2)) = X0)) | sP1(X2, sK31(X0, X1, X2), sK30(X0, X1, X2), X1, X0) | sP0(X2, sK31(X0, X1, X2), sK30(X0, X1, X2), X1, X0))), introduced(choice_axiom, [])).
fof(f158, plain, ! [X0, X1, X2] : ((initiates(X0, X1, X2) | ! [X3, X4] : ((~ happens(push(X3, X4), X2) | ~ (spinning(X4) = X1) | ~ (pull(X3, X4) = X0)) & ~ sP1(X2, X4, X3, X1, X0) & ~ sP0(X2, X4, X3, X1, X0))) & (? [X5, X6] : ((happens(push(X5, X6), X2) & (spinning(X6) = X1) & (pull(X5, X6) = X0)) | sP1(X2, X6, X5, X1, X0) | sP0(X2, X6, X5, X1, X0)) | ~ initiates(X0, X1, X2))), inference(rectify, [], [f157])).
fof(f157, plain, ! [X0, X1, X2] : ((initiates(X0, X1, X2) | ! [X3, X4] : ((~ happens(push(X3, X4), X2) | ~ (spinning(X4) = X1) | ~ (pull(X3, X4) = X0)) & ~ sP1(X2, X4, X3, X1, X0) & ~ sP0(X2, X4, X3, X1, X0))) & (? [X3, X4] : ((happens(push(X3, X4), X2) & (spinning(X4) = X1) & (pull(X3, X4) = X0)) | sP1(X2, X4, X3, X1, X0) | sP0(X2, X4, X3, X1, X0)) | ~ initiates(X0, X1, X2))), inference(nnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1, X2] : (initiates(X0, X1, X2) <=> ? [X3, X4] : ((happens(push(X3, X4), X2) & (spinning(X4) = X1) & (pull(X3, X4) = X0)) | sP1(X2, X4, X3, X1, X0) | sP0(X2, X4, X3, X1, X0))), inference(definition_folding, [], [f62, e115, e114])).
fof(f114, plain, ! [X2, X4, X3, X1, X0] : (sP0(X2, X4, X3, X1, X0) <=> (~ happens(pull(X3, X4), X2) & (forwards(X4) = X1) & (push(X3, X4) = X0))), inference(usedef, [], [e114])).
fof(e114, plain, ! [X2, X4, X3, X1, X0] : (sP0(X2, X4, X3, X1, X0) <=> (~ happens(pull(X3, X4), X2) & (forwards(X4) = X1) & (push(X3, X4) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f115, plain, ! [X2, X4, X3, X1, X0] : (sP1(X2, X4, X3, X1, X0) <=> (~ happens(push(X3, X4), X2) & (backwards(X4) = X1) & (pull(X3, X4) = X0))), inference(usedef, [], [e115])).
fof(e115, plain, ! [X2, X4, X3, X1, X0] : (sP1(X2, X4, X3, X1, X0) <=> (~ happens(push(X3, X4), X2) & (backwards(X4) = X1) & (pull(X3, X4) = X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f62, plain, ! [X0, X1, X2] : (initiates(X0, X1, X2) <=> ? [X3, X4] : ((happens(push(X3, X4), X2) & (spinning(X4) = X1) & (pull(X3, X4) = X0)) | (~ happens(push(X3, X4), X2) & (backwards(X4) = X1) & (pull(X3, X4) = X0)) | (~ happens(pull(X3, X4), X2) & (forwards(X4) = X1) & (push(X3, X4) = X0)))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X3, X1, X4] : (initiates(X3, X1, X4) <=> ? [X8, X9] : ((happens(push(X8, X9), X4) & (spinning(X9) = X1) & (pull(X8, X9) = X3)) | (~ happens(push(X8, X9), X4) & (backwards(X9) = X1) & (pull(X8, X9) = X3)) | (~ happens(pull(X8, X9), X4) & (forwards(X9) = X1) & (push(X8, X9) = X3)))), file('/home/ubuntu/library/tptp/Problems/CSR/CSR024+1.010.p', initiates_all_defn)).
fof(f1625, plain, spl34_9, inference(avatar_split_clause, [], [f1624, f651])).
fof(f651, plain, (spl34_9 <=> holdsAt(spinning(trolley9), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_9])])).
fof(f1624, plain, holdsAt(spinning(trolley9), n1), inference(forward_demodulation, [], [f1623, f315])).
fof(f1623, plain, holdsAt(spinning(trolley9), plus(n0, n1)), inference(subsumption_resolution, [], [f1596, f781])).
fof(f781, plain, happens(push(agent9, trolley9), n0), inference(resolution, [], [f430, f578])).
fof(f578, plain, sP25(n0, push(agent9, trolley9)), inference(equality_resolution, [], [f577])).
fof(f577, plain, ! [X1] : (sP25(n0, X1) | ~ (push(agent9, trolley9) = X1)), inference(equality_resolution, [], [f375])).
fof(f375, plain, ! [X0, X1] : (sP25(X0, X1) | ~ (n0 = X0) | ~ (push(agent9, trolley9) = X1)), inference(cnf_transformation, [], [f196])).
fof(f1596, plain, (holdsAt(spinning(trolley9), plus(n0, n1)) | ~ happens(push(agent9, trolley9), n0)), inference(resolution, [], [f1016, f1219])).
fof(f1219, plain, happens(pull(agent9, trolley9), n0), inference(resolution, [], [f743, f430])).
fof(f743, plain, sP25(n0, pull(agent9, trolley9)), inference(resolution, [], [f374, f580])).
fof(f580, plain, sP24(n0, pull(agent9, trolley9)), inference(equality_resolution, [], [f579])).
fof(f579, plain, ! [X1] : (sP24(n0, X1) | ~ (pull(agent9, trolley9) = X1)), inference(equality_resolution, [], [f380])).
fof(f380, plain, ! [X0, X1] : (sP24(X0, X1) | ~ (n0 = X0) | ~ (pull(agent9, trolley9) = X1)), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0, X1] : ((sP24(X0, X1) | ~ (n0 = X0) | ~ (pull(agent9, trolley9) = X1)) & (((n0 = X0) & (pull(agent9, trolley9) = X1)) | ~ sP24(X0, X1))), inference(rectify, [], [f198])).
fof(f198, plain, ! [X1, X0] : ((sP24(X1, X0) | ~ (n0 = X1) | ~ (pull(agent9, trolley9) = X0)) & (((n0 = X1) & (pull(agent9, trolley9) = X0)) | ~ sP24(X1, X0))), inference(flattening, [], [f197])).
fof(f197, plain, ! [X1, X0] : ((sP24(X1, X0) | (~ (n0 = X1) | ~ (pull(agent9, trolley9) = X0))) & (((n0 = X1) & (pull(agent9, trolley9) = X0)) | ~ sP24(X1, X0))), inference(nnf_transformation, [], [f140])).
fof(f374, plain, ! [X0, X1] : (~ sP24(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1622, plain, spl34_8, inference(avatar_split_clause, [], [f1621, f647])).
fof(f647, plain, (spl34_8 <=> holdsAt(spinning(trolley8), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_8])])).
fof(f1621, plain, holdsAt(spinning(trolley8), n1), inference(forward_demodulation, [], [f1620, f315])).
fof(f1620, plain, holdsAt(spinning(trolley8), plus(n0, n1)), inference(subsumption_resolution, [], [f1595, f1216])).
fof(f1216, plain, happens(push(agent8, trolley8), n0), inference(resolution, [], [f739, f430])).
fof(f739, plain, sP25(n0, push(agent8, trolley8)), inference(resolution, [], [f373, f582])).
fof(f582, plain, sP23(n0, push(agent8, trolley8)), inference(equality_resolution, [], [f581])).
fof(f581, plain, ! [X1] : (sP23(n0, X1) | ~ (push(agent8, trolley8) = X1)), inference(equality_resolution, [], [f383])).
fof(f383, plain, ! [X0, X1] : (sP23(X0, X1) | ~ (n0 = X0) | ~ (push(agent8, trolley8) = X1)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0, X1] : ((sP23(X0, X1) | ~ (n0 = X0) | ~ (push(agent8, trolley8) = X1)) & (((n0 = X0) & (push(agent8, trolley8) = X1)) | ~ sP23(X0, X1))), inference(rectify, [], [f201])).
fof(f201, plain, ! [X1, X0] : ((sP23(X1, X0) | ~ (n0 = X1) | ~ (push(agent8, trolley8) = X0)) & (((n0 = X1) & (push(agent8, trolley8) = X0)) | ~ sP23(X1, X0))), inference(flattening, [], [f200])).
fof(f200, plain, ! [X1, X0] : ((sP23(X1, X0) | (~ (n0 = X1) | ~ (push(agent8, trolley8) = X0))) & (((n0 = X1) & (push(agent8, trolley8) = X0)) | ~ sP23(X1, X0))), inference(nnf_transformation, [], [f139])).
fof(f373, plain, ! [X0, X1] : (~ sP23(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1595, plain, (holdsAt(spinning(trolley8), plus(n0, n1)) | ~ happens(push(agent8, trolley8), n0)), inference(resolution, [], [f1016, f1213])).
fof(f1213, plain, happens(pull(agent8, trolley8), n0), inference(resolution, [], [f735, f430])).
fof(f735, plain, sP25(n0, pull(agent8, trolley8)), inference(resolution, [], [f372, f584])).
fof(f584, plain, sP22(n0, pull(agent8, trolley8)), inference(equality_resolution, [], [f583])).
fof(f583, plain, ! [X1] : (sP22(n0, X1) | ~ (pull(agent8, trolley8) = X1)), inference(equality_resolution, [], [f386])).
fof(f386, plain, ! [X0, X1] : (sP22(X0, X1) | ~ (n0 = X0) | ~ (pull(agent8, trolley8) = X1)), inference(cnf_transformation, [], [f205])).
fof(f205, plain, ! [X0, X1] : ((sP22(X0, X1) | ~ (n0 = X0) | ~ (pull(agent8, trolley8) = X1)) & (((n0 = X0) & (pull(agent8, trolley8) = X1)) | ~ sP22(X0, X1))), inference(rectify, [], [f204])).
fof(f204, plain, ! [X1, X0] : ((sP22(X1, X0) | ~ (n0 = X1) | ~ (pull(agent8, trolley8) = X0)) & (((n0 = X1) & (pull(agent8, trolley8) = X0)) | ~ sP22(X1, X0))), inference(flattening, [], [f203])).
fof(f203, plain, ! [X1, X0] : ((sP22(X1, X0) | (~ (n0 = X1) | ~ (pull(agent8, trolley8) = X0))) & (((n0 = X1) & (pull(agent8, trolley8) = X0)) | ~ sP22(X1, X0))), inference(nnf_transformation, [], [f138])).
fof(f372, plain, ! [X0, X1] : (~ sP22(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1619, plain, spl34_7, inference(avatar_split_clause, [], [f1618, f643])).
fof(f643, plain, (spl34_7 <=> holdsAt(spinning(trolley7), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_7])])).
fof(f1618, plain, holdsAt(spinning(trolley7), n1), inference(forward_demodulation, [], [f1617, f315])).
fof(f1617, plain, holdsAt(spinning(trolley7), plus(n0, n1)), inference(subsumption_resolution, [], [f1594, f1210])).
fof(f1210, plain, happens(push(agent7, trolley7), n0), inference(resolution, [], [f731, f430])).
fof(f731, plain, sP25(n0, push(agent7, trolley7)), inference(resolution, [], [f371, f586])).
fof(f586, plain, sP21(n0, push(agent7, trolley7)), inference(equality_resolution, [], [f585])).
fof(f585, plain, ! [X1] : (sP21(n0, X1) | ~ (push(agent7, trolley7) = X1)), inference(equality_resolution, [], [f389])).
fof(f389, plain, ! [X0, X1] : (sP21(X0, X1) | ~ (n0 = X0) | ~ (push(agent7, trolley7) = X1)), inference(cnf_transformation, [], [f208])).
fof(f208, plain, ! [X0, X1] : ((sP21(X0, X1) | ~ (n0 = X0) | ~ (push(agent7, trolley7) = X1)) & (((n0 = X0) & (push(agent7, trolley7) = X1)) | ~ sP21(X0, X1))), inference(rectify, [], [f207])).
fof(f207, plain, ! [X1, X0] : ((sP21(X1, X0) | ~ (n0 = X1) | ~ (push(agent7, trolley7) = X0)) & (((n0 = X1) & (push(agent7, trolley7) = X0)) | ~ sP21(X1, X0))), inference(flattening, [], [f206])).
fof(f206, plain, ! [X1, X0] : ((sP21(X1, X0) | (~ (n0 = X1) | ~ (push(agent7, trolley7) = X0))) & (((n0 = X1) & (push(agent7, trolley7) = X0)) | ~ sP21(X1, X0))), inference(nnf_transformation, [], [f137])).
fof(f371, plain, ! [X0, X1] : (~ sP21(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1594, plain, (holdsAt(spinning(trolley7), plus(n0, n1)) | ~ happens(push(agent7, trolley7), n0)), inference(resolution, [], [f1016, f1207])).
fof(f1207, plain, happens(pull(agent7, trolley7), n0), inference(resolution, [], [f727, f430])).
fof(f727, plain, sP25(n0, pull(agent7, trolley7)), inference(resolution, [], [f370, f588])).
fof(f588, plain, sP20(n0, pull(agent7, trolley7)), inference(equality_resolution, [], [f587])).
fof(f587, plain, ! [X1] : (sP20(n0, X1) | ~ (pull(agent7, trolley7) = X1)), inference(equality_resolution, [], [f392])).
fof(f392, plain, ! [X0, X1] : (sP20(X0, X1) | ~ (n0 = X0) | ~ (pull(agent7, trolley7) = X1)), inference(cnf_transformation, [], [f211])).
fof(f211, plain, ! [X0, X1] : ((sP20(X0, X1) | ~ (n0 = X0) | ~ (pull(agent7, trolley7) = X1)) & (((n0 = X0) & (pull(agent7, trolley7) = X1)) | ~ sP20(X0, X1))), inference(rectify, [], [f210])).
fof(f210, plain, ! [X1, X0] : ((sP20(X1, X0) | ~ (n0 = X1) | ~ (pull(agent7, trolley7) = X0)) & (((n0 = X1) & (pull(agent7, trolley7) = X0)) | ~ sP20(X1, X0))), inference(flattening, [], [f209])).
fof(f209, plain, ! [X1, X0] : ((sP20(X1, X0) | (~ (n0 = X1) | ~ (pull(agent7, trolley7) = X0))) & (((n0 = X1) & (pull(agent7, trolley7) = X0)) | ~ sP20(X1, X0))), inference(nnf_transformation, [], [f136])).
fof(f370, plain, ! [X0, X1] : (~ sP20(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1616, plain, spl34_6, inference(avatar_split_clause, [], [f1615, f639])).
fof(f639, plain, (spl34_6 <=> holdsAt(spinning(trolley6), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_6])])).
fof(f1615, plain, holdsAt(spinning(trolley6), n1), inference(forward_demodulation, [], [f1614, f315])).
fof(f1614, plain, holdsAt(spinning(trolley6), plus(n0, n1)), inference(subsumption_resolution, [], [f1593, f1204])).
fof(f1204, plain, happens(push(agent6, trolley6), n0), inference(resolution, [], [f723, f430])).
fof(f723, plain, sP25(n0, push(agent6, trolley6)), inference(resolution, [], [f369, f590])).
fof(f590, plain, sP19(n0, push(agent6, trolley6)), inference(equality_resolution, [], [f589])).
fof(f589, plain, ! [X1] : (sP19(n0, X1) | ~ (push(agent6, trolley6) = X1)), inference(equality_resolution, [], [f395])).
fof(f395, plain, ! [X0, X1] : (sP19(X0, X1) | ~ (n0 = X0) | ~ (push(agent6, trolley6) = X1)), inference(cnf_transformation, [], [f214])).
fof(f214, plain, ! [X0, X1] : ((sP19(X0, X1) | ~ (n0 = X0) | ~ (push(agent6, trolley6) = X1)) & (((n0 = X0) & (push(agent6, trolley6) = X1)) | ~ sP19(X0, X1))), inference(rectify, [], [f213])).
fof(f213, plain, ! [X1, X0] : ((sP19(X1, X0) | ~ (n0 = X1) | ~ (push(agent6, trolley6) = X0)) & (((n0 = X1) & (push(agent6, trolley6) = X0)) | ~ sP19(X1, X0))), inference(flattening, [], [f212])).
fof(f212, plain, ! [X1, X0] : ((sP19(X1, X0) | (~ (n0 = X1) | ~ (push(agent6, trolley6) = X0))) & (((n0 = X1) & (push(agent6, trolley6) = X0)) | ~ sP19(X1, X0))), inference(nnf_transformation, [], [f135])).
fof(f369, plain, ! [X0, X1] : (~ sP19(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1593, plain, (holdsAt(spinning(trolley6), plus(n0, n1)) | ~ happens(push(agent6, trolley6), n0)), inference(resolution, [], [f1016, f1201])).
fof(f1201, plain, happens(pull(agent6, trolley6), n0), inference(resolution, [], [f719, f430])).
fof(f719, plain, sP25(n0, pull(agent6, trolley6)), inference(resolution, [], [f368, f592])).
fof(f592, plain, sP18(n0, pull(agent6, trolley6)), inference(equality_resolution, [], [f591])).
fof(f591, plain, ! [X1] : (sP18(n0, X1) | ~ (pull(agent6, trolley6) = X1)), inference(equality_resolution, [], [f398])).
fof(f398, plain, ! [X0, X1] : (sP18(X0, X1) | ~ (n0 = X0) | ~ (pull(agent6, trolley6) = X1)), inference(cnf_transformation, [], [f217])).
fof(f217, plain, ! [X0, X1] : ((sP18(X0, X1) | ~ (n0 = X0) | ~ (pull(agent6, trolley6) = X1)) & (((n0 = X0) & (pull(agent6, trolley6) = X1)) | ~ sP18(X0, X1))), inference(rectify, [], [f216])).
fof(f216, plain, ! [X1, X0] : ((sP18(X1, X0) | ~ (n0 = X1) | ~ (pull(agent6, trolley6) = X0)) & (((n0 = X1) & (pull(agent6, trolley6) = X0)) | ~ sP18(X1, X0))), inference(flattening, [], [f215])).
fof(f215, plain, ! [X1, X0] : ((sP18(X1, X0) | (~ (n0 = X1) | ~ (pull(agent6, trolley6) = X0))) & (((n0 = X1) & (pull(agent6, trolley6) = X0)) | ~ sP18(X1, X0))), inference(nnf_transformation, [], [f134])).
fof(f368, plain, ! [X0, X1] : (~ sP18(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1613, plain, spl34_5, inference(avatar_split_clause, [], [f1612, f635])).
fof(f635, plain, (spl34_5 <=> holdsAt(spinning(trolley5), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_5])])).
fof(f1612, plain, holdsAt(spinning(trolley5), n1), inference(forward_demodulation, [], [f1611, f315])).
fof(f1611, plain, holdsAt(spinning(trolley5), plus(n0, n1)), inference(subsumption_resolution, [], [f1592, f1190])).
fof(f1190, plain, happens(push(agent5, trolley5), n0), inference(resolution, [], [f715, f430])).
fof(f715, plain, sP25(n0, push(agent5, trolley5)), inference(resolution, [], [f367, f594])).
fof(f594, plain, sP17(n0, push(agent5, trolley5)), inference(equality_resolution, [], [f593])).
fof(f593, plain, ! [X1] : (sP17(n0, X1) | ~ (push(agent5, trolley5) = X1)), inference(equality_resolution, [], [f401])).
fof(f401, plain, ! [X0, X1] : (sP17(X0, X1) | ~ (n0 = X0) | ~ (push(agent5, trolley5) = X1)), inference(cnf_transformation, [], [f220])).
fof(f220, plain, ! [X0, X1] : ((sP17(X0, X1) | ~ (n0 = X0) | ~ (push(agent5, trolley5) = X1)) & (((n0 = X0) & (push(agent5, trolley5) = X1)) | ~ sP17(X0, X1))), inference(rectify, [], [f219])).
fof(f219, plain, ! [X1, X0] : ((sP17(X1, X0) | ~ (n0 = X1) | ~ (push(agent5, trolley5) = X0)) & (((n0 = X1) & (push(agent5, trolley5) = X0)) | ~ sP17(X1, X0))), inference(flattening, [], [f218])).
fof(f218, plain, ! [X1, X0] : ((sP17(X1, X0) | (~ (n0 = X1) | ~ (push(agent5, trolley5) = X0))) & (((n0 = X1) & (push(agent5, trolley5) = X0)) | ~ sP17(X1, X0))), inference(nnf_transformation, [], [f133])).
fof(f367, plain, ! [X0, X1] : (~ sP17(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1592, plain, (holdsAt(spinning(trolley5), plus(n0, n1)) | ~ happens(push(agent5, trolley5), n0)), inference(resolution, [], [f1016, f1175])).
fof(f1175, plain, happens(pull(agent5, trolley5), n0), inference(resolution, [], [f711, f430])).
fof(f711, plain, sP25(n0, pull(agent5, trolley5)), inference(resolution, [], [f366, f596])).
fof(f596, plain, sP16(n0, pull(agent5, trolley5)), inference(equality_resolution, [], [f595])).
fof(f595, plain, ! [X1] : (sP16(n0, X1) | ~ (pull(agent5, trolley5) = X1)), inference(equality_resolution, [], [f404])).
fof(f404, plain, ! [X0, X1] : (sP16(X0, X1) | ~ (n0 = X0) | ~ (pull(agent5, trolley5) = X1)), inference(cnf_transformation, [], [f223])).
fof(f223, plain, ! [X0, X1] : ((sP16(X0, X1) | ~ (n0 = X0) | ~ (pull(agent5, trolley5) = X1)) & (((n0 = X0) & (pull(agent5, trolley5) = X1)) | ~ sP16(X0, X1))), inference(rectify, [], [f222])).
fof(f222, plain, ! [X1, X0] : ((sP16(X1, X0) | ~ (n0 = X1) | ~ (pull(agent5, trolley5) = X0)) & (((n0 = X1) & (pull(agent5, trolley5) = X0)) | ~ sP16(X1, X0))), inference(flattening, [], [f221])).
fof(f221, plain, ! [X1, X0] : ((sP16(X1, X0) | (~ (n0 = X1) | ~ (pull(agent5, trolley5) = X0))) & (((n0 = X1) & (pull(agent5, trolley5) = X0)) | ~ sP16(X1, X0))), inference(nnf_transformation, [], [f132])).
fof(f366, plain, ! [X0, X1] : (~ sP16(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1610, plain, spl34_4, inference(avatar_split_clause, [], [f1609, f631])).
fof(f631, plain, (spl34_4 <=> holdsAt(spinning(trolley4), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_4])])).
fof(f1609, plain, holdsAt(spinning(trolley4), n1), inference(forward_demodulation, [], [f1608, f315])).
fof(f1608, plain, holdsAt(spinning(trolley4), plus(n0, n1)), inference(subsumption_resolution, [], [f1591, f1165])).
fof(f1165, plain, happens(push(agent4, trolley4), n0), inference(resolution, [], [f710, f430])).
fof(f710, plain, sP25(n0, push(agent4, trolley4)), inference(resolution, [], [f365, f598])).
fof(f598, plain, sP15(n0, push(agent4, trolley4)), inference(equality_resolution, [], [f597])).
fof(f597, plain, ! [X1] : (sP15(n0, X1) | ~ (push(agent4, trolley4) = X1)), inference(equality_resolution, [], [f407])).
fof(f407, plain, ! [X0, X1] : (sP15(X0, X1) | ~ (n0 = X0) | ~ (push(agent4, trolley4) = X1)), inference(cnf_transformation, [], [f226])).
fof(f226, plain, ! [X0, X1] : ((sP15(X0, X1) | ~ (n0 = X0) | ~ (push(agent4, trolley4) = X1)) & (((n0 = X0) & (push(agent4, trolley4) = X1)) | ~ sP15(X0, X1))), inference(rectify, [], [f225])).
fof(f225, plain, ! [X1, X0] : ((sP15(X1, X0) | ~ (n0 = X1) | ~ (push(agent4, trolley4) = X0)) & (((n0 = X1) & (push(agent4, trolley4) = X0)) | ~ sP15(X1, X0))), inference(flattening, [], [f224])).
fof(f224, plain, ! [X1, X0] : ((sP15(X1, X0) | (~ (n0 = X1) | ~ (push(agent4, trolley4) = X0))) & (((n0 = X1) & (push(agent4, trolley4) = X0)) | ~ sP15(X1, X0))), inference(nnf_transformation, [], [f131])).
fof(f365, plain, ! [X0, X1] : (~ sP15(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1591, plain, (holdsAt(spinning(trolley4), plus(n0, n1)) | ~ happens(push(agent4, trolley4), n0)), inference(resolution, [], [f1016, f1152])).
fof(f1152, plain, happens(pull(agent4, trolley4), n0), inference(resolution, [], [f709, f430])).
fof(f709, plain, sP25(n0, pull(agent4, trolley4)), inference(resolution, [], [f364, f600])).
fof(f600, plain, sP14(n0, pull(agent4, trolley4)), inference(equality_resolution, [], [f599])).
fof(f599, plain, ! [X1] : (sP14(n0, X1) | ~ (pull(agent4, trolley4) = X1)), inference(equality_resolution, [], [f410])).
fof(f410, plain, ! [X0, X1] : (sP14(X0, X1) | ~ (n0 = X0) | ~ (pull(agent4, trolley4) = X1)), inference(cnf_transformation, [], [f229])).
fof(f229, plain, ! [X0, X1] : ((sP14(X0, X1) | ~ (n0 = X0) | ~ (pull(agent4, trolley4) = X1)) & (((n0 = X0) & (pull(agent4, trolley4) = X1)) | ~ sP14(X0, X1))), inference(rectify, [], [f228])).
fof(f228, plain, ! [X1, X0] : ((sP14(X1, X0) | ~ (n0 = X1) | ~ (pull(agent4, trolley4) = X0)) & (((n0 = X1) & (pull(agent4, trolley4) = X0)) | ~ sP14(X1, X0))), inference(flattening, [], [f227])).
fof(f227, plain, ! [X1, X0] : ((sP14(X1, X0) | (~ (n0 = X1) | ~ (pull(agent4, trolley4) = X0))) & (((n0 = X1) & (pull(agent4, trolley4) = X0)) | ~ sP14(X1, X0))), inference(nnf_transformation, [], [f130])).
fof(f364, plain, ! [X0, X1] : (~ sP14(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1607, plain, spl34_3, inference(avatar_split_clause, [], [f1606, f627])).
fof(f627, plain, (spl34_3 <=> holdsAt(spinning(trolley3), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_3])])).
fof(f1606, plain, holdsAt(spinning(trolley3), n1), inference(forward_demodulation, [], [f1605, f315])).
fof(f1605, plain, holdsAt(spinning(trolley3), plus(n0, n1)), inference(subsumption_resolution, [], [f1590, f1141])).
fof(f1141, plain, happens(push(agent3, trolley3), n0), inference(resolution, [], [f708, f430])).
fof(f708, plain, sP25(n0, push(agent3, trolley3)), inference(resolution, [], [f363, f602])).
fof(f602, plain, sP13(n0, push(agent3, trolley3)), inference(equality_resolution, [], [f601])).
fof(f601, plain, ! [X1] : (sP13(n0, X1) | ~ (push(agent3, trolley3) = X1)), inference(equality_resolution, [], [f413])).
fof(f413, plain, ! [X0, X1] : (sP13(X0, X1) | ~ (n0 = X0) | ~ (push(agent3, trolley3) = X1)), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ! [X0, X1] : ((sP13(X0, X1) | ~ (n0 = X0) | ~ (push(agent3, trolley3) = X1)) & (((n0 = X0) & (push(agent3, trolley3) = X1)) | ~ sP13(X0, X1))), inference(rectify, [], [f231])).
fof(f231, plain, ! [X1, X0] : ((sP13(X1, X0) | ~ (n0 = X1) | ~ (push(agent3, trolley3) = X0)) & (((n0 = X1) & (push(agent3, trolley3) = X0)) | ~ sP13(X1, X0))), inference(flattening, [], [f230])).
fof(f230, plain, ! [X1, X0] : ((sP13(X1, X0) | (~ (n0 = X1) | ~ (push(agent3, trolley3) = X0))) & (((n0 = X1) & (push(agent3, trolley3) = X0)) | ~ sP13(X1, X0))), inference(nnf_transformation, [], [f129])).
fof(f363, plain, ! [X0, X1] : (~ sP13(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1590, plain, (holdsAt(spinning(trolley3), plus(n0, n1)) | ~ happens(push(agent3, trolley3), n0)), inference(resolution, [], [f1016, f1136])).
fof(f1136, plain, happens(pull(agent3, trolley3), n0), inference(resolution, [], [f707, f430])).
fof(f707, plain, sP25(n0, pull(agent3, trolley3)), inference(resolution, [], [f362, f604])).
fof(f604, plain, sP12(n0, pull(agent3, trolley3)), inference(equality_resolution, [], [f603])).
fof(f603, plain, ! [X1] : (sP12(n0, X1) | ~ (pull(agent3, trolley3) = X1)), inference(equality_resolution, [], [f416])).
fof(f416, plain, ! [X0, X1] : (sP12(X0, X1) | ~ (n0 = X0) | ~ (pull(agent3, trolley3) = X1)), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ! [X0, X1] : ((sP12(X0, X1) | ~ (n0 = X0) | ~ (pull(agent3, trolley3) = X1)) & (((n0 = X0) & (pull(agent3, trolley3) = X1)) | ~ sP12(X0, X1))), inference(rectify, [], [f234])).
fof(f234, plain, ! [X1, X0] : ((sP12(X1, X0) | ~ (n0 = X1) | ~ (pull(agent3, trolley3) = X0)) & (((n0 = X1) & (pull(agent3, trolley3) = X0)) | ~ sP12(X1, X0))), inference(flattening, [], [f233])).
fof(f233, plain, ! [X1, X0] : ((sP12(X1, X0) | (~ (n0 = X1) | ~ (pull(agent3, trolley3) = X0))) & (((n0 = X1) & (pull(agent3, trolley3) = X0)) | ~ sP12(X1, X0))), inference(nnf_transformation, [], [f128])).
fof(f362, plain, ! [X0, X1] : (~ sP12(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1604, plain, spl34_2, inference(avatar_split_clause, [], [f1603, f623])).
fof(f623, plain, (spl34_2 <=> holdsAt(spinning(trolley2), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_2])])).
fof(f1603, plain, holdsAt(spinning(trolley2), n1), inference(forward_demodulation, [], [f1602, f315])).
fof(f1602, plain, holdsAt(spinning(trolley2), plus(n0, n1)), inference(subsumption_resolution, [], [f1589, f1131])).
fof(f1131, plain, happens(push(agent2, trolley2), n0), inference(resolution, [], [f706, f430])).
fof(f706, plain, sP25(n0, push(agent2, trolley2)), inference(resolution, [], [f361, f606])).
fof(f606, plain, sP11(n0, push(agent2, trolley2)), inference(equality_resolution, [], [f605])).
fof(f605, plain, ! [X1] : (sP11(n0, X1) | ~ (push(agent2, trolley2) = X1)), inference(equality_resolution, [], [f419])).
fof(f419, plain, ! [X0, X1] : (sP11(X0, X1) | ~ (n0 = X0) | ~ (push(agent2, trolley2) = X1)), inference(cnf_transformation, [], [f238])).
fof(f238, plain, ! [X0, X1] : ((sP11(X0, X1) | ~ (n0 = X0) | ~ (push(agent2, trolley2) = X1)) & (((n0 = X0) & (push(agent2, trolley2) = X1)) | ~ sP11(X0, X1))), inference(rectify, [], [f237])).
fof(f237, plain, ! [X1, X0] : ((sP11(X1, X0) | ~ (n0 = X1) | ~ (push(agent2, trolley2) = X0)) & (((n0 = X1) & (push(agent2, trolley2) = X0)) | ~ sP11(X1, X0))), inference(flattening, [], [f236])).
fof(f236, plain, ! [X1, X0] : ((sP11(X1, X0) | (~ (n0 = X1) | ~ (push(agent2, trolley2) = X0))) & (((n0 = X1) & (push(agent2, trolley2) = X0)) | ~ sP11(X1, X0))), inference(nnf_transformation, [], [f127])).
fof(f361, plain, ! [X0, X1] : (~ sP11(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1589, plain, (holdsAt(spinning(trolley2), plus(n0, n1)) | ~ happens(push(agent2, trolley2), n0)), inference(resolution, [], [f1016, f1126])).
fof(f1126, plain, happens(pull(agent2, trolley2), n0), inference(resolution, [], [f705, f430])).
fof(f705, plain, sP25(n0, pull(agent2, trolley2)), inference(resolution, [], [f360, f608])).
fof(f608, plain, sP10(n0, pull(agent2, trolley2)), inference(equality_resolution, [], [f607])).
fof(f607, plain, ! [X1] : (sP10(n0, X1) | ~ (pull(agent2, trolley2) = X1)), inference(equality_resolution, [], [f422])).
fof(f422, plain, ! [X0, X1] : (sP10(X0, X1) | ~ (n0 = X0) | ~ (pull(agent2, trolley2) = X1)), inference(cnf_transformation, [], [f241])).
fof(f241, plain, ! [X0, X1] : ((sP10(X0, X1) | ~ (n0 = X0) | ~ (pull(agent2, trolley2) = X1)) & (((n0 = X0) & (pull(agent2, trolley2) = X1)) | ~ sP10(X0, X1))), inference(rectify, [], [f240])).
fof(f240, plain, ! [X1, X0] : ((sP10(X1, X0) | ~ (n0 = X1) | ~ (pull(agent2, trolley2) = X0)) & (((n0 = X1) & (pull(agent2, trolley2) = X0)) | ~ sP10(X1, X0))), inference(flattening, [], [f239])).
fof(f239, plain, ! [X1, X0] : ((sP10(X1, X0) | (~ (n0 = X1) | ~ (pull(agent2, trolley2) = X0))) & (((n0 = X1) & (pull(agent2, trolley2) = X0)) | ~ sP10(X1, X0))), inference(nnf_transformation, [], [f126])).
fof(f360, plain, ! [X0, X1] : (~ sP10(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1601, plain, spl34_1, inference(avatar_contradiction_clause, [], [f1600])).
fof(f1600, plain, ($false | spl34_1), inference(subsumption_resolution, [], [f1599, f621])).
fof(f621, plain, (~ holdsAt(spinning(trolley1), n1) | spl34_1), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl34_1 <=> holdsAt(spinning(trolley1), n1)), introduced(avatar_definition, [new_symbols(naming, [spl34_1])])).
fof(f1599, plain, holdsAt(spinning(trolley1), n1), inference(forward_demodulation, [], [f1598, f315])).
fof(f1598, plain, holdsAt(spinning(trolley1), plus(n0, n1)), inference(subsumption_resolution, [], [f1588, f1124])).
fof(f1124, plain, happens(push(agent1, trolley1), n0), inference(resolution, [], [f704, f430])).
fof(f704, plain, sP25(n0, push(agent1, trolley1)), inference(resolution, [], [f359, f610])).
fof(f610, plain, sP9(n0, push(agent1, trolley1)), inference(equality_resolution, [], [f609])).
fof(f609, plain, ! [X1] : (sP9(n0, X1) | ~ (push(agent1, trolley1) = X1)), inference(equality_resolution, [], [f425])).
fof(f425, plain, ! [X0, X1] : (sP9(X0, X1) | ~ (n0 = X0) | ~ (push(agent1, trolley1) = X1)), inference(cnf_transformation, [], [f244])).
fof(f244, plain, ! [X0, X1] : ((sP9(X0, X1) | ~ (n0 = X0) | ~ (push(agent1, trolley1) = X1)) & (((n0 = X0) & (push(agent1, trolley1) = X1)) | ~ sP9(X0, X1))), inference(rectify, [], [f243])).
fof(f243, plain, ! [X1, X0] : ((sP9(X1, X0) | ~ (n0 = X1) | ~ (push(agent1, trolley1) = X0)) & (((n0 = X1) & (push(agent1, trolley1) = X0)) | ~ sP9(X1, X0))), inference(flattening, [], [f242])).
fof(f242, plain, ! [X1, X0] : ((sP9(X1, X0) | (~ (n0 = X1) | ~ (push(agent1, trolley1) = X0))) & (((n0 = X1) & (push(agent1, trolley1) = X0)) | ~ sP9(X1, X0))), inference(nnf_transformation, [], [f125])).
fof(f359, plain, ! [X0, X1] : (~ sP9(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f1588, plain, (holdsAt(spinning(trolley1), plus(n0, n1)) | ~ happens(push(agent1, trolley1), n0)), inference(resolution, [], [f1016, f1122])).
fof(f1122, plain, happens(pull(agent1, trolley1), n0), inference(resolution, [], [f703, f430])).
fof(f703, plain, sP25(n0, pull(agent1, trolley1)), inference(resolution, [], [f358, f612])).
fof(f612, plain, sP8(n0, pull(agent1, trolley1)), inference(equality_resolution, [], [f611])).
fof(f611, plain, ! [X1] : (sP8(n0, X1) | ~ (pull(agent1, trolley1) = X1)), inference(equality_resolution, [], [f428])).
fof(f428, plain, ! [X0, X1] : (sP8(X0, X1) | ~ (n0 = X0) | ~ (pull(agent1, trolley1) = X1)), inference(cnf_transformation, [], [f247])).
fof(f247, plain, ! [X0, X1] : ((sP8(X0, X1) | ~ (n0 = X0) | ~ (pull(agent1, trolley1) = X1)) & (((n0 = X0) & (pull(agent1, trolley1) = X1)) | ~ sP8(X0, X1))), inference(rectify, [], [f246])).
fof(f246, plain, ! [X1, X0] : ((sP8(X1, X0) | ~ (n0 = X1) | ~ (pull(agent1, trolley1) = X0)) & (((n0 = X1) & (pull(agent1, trolley1) = X0)) | ~ sP8(X1, X0))), inference(flattening, [], [f245])).
fof(f245, plain, ! [X1, X0] : ((sP8(X1, X0) | (~ (n0 = X1) | ~ (pull(agent1, trolley1) = X0))) & (((n0 = X1) & (pull(agent1, trolley1) = X0)) | ~ sP8(X1, X0))), inference(nnf_transformation, [], [f124])).
fof(f358, plain, ! [X0, X1] : (~ sP8(X0, X1) | sP25(X0, X1)), inference(cnf_transformation, [], [f196])).
fof(f658, plain, (~ spl34_1 | ~ spl34_2 | ~ spl34_3 | ~ spl34_4 | ~ spl34_5 | ~ spl34_6 | ~ spl34_7 | ~ spl34_8 | ~ spl34_9 | ~ spl34_10), inference(avatar_split_clause, [], [f552, f655, f651, f647, f643, f639, f635, f631, f627, f623, f619])).
fof(f552, plain, (~ holdsAt(spinning(trolley10), n1) | ~ holdsAt(spinning(trolley9), n1) | ~ holdsAt(spinning(trolley8), n1) | ~ holdsAt(spinning(trolley7), n1) | ~ holdsAt(spinning(trolley6), n1) | ~ holdsAt(spinning(trolley5), n1) | ~ holdsAt(spinning(trolley4), n1) | ~ holdsAt(spinning(trolley3), n1) | ~ holdsAt(spinning(trolley2), n1) | ~ holdsAt(spinning(trolley1), n1)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, (~ holdsAt(spinning(trolley10), n1) | ~ holdsAt(spinning(trolley9), n1) | ~ holdsAt(spinning(trolley8), n1) | ~ holdsAt(spinning(trolley7), n1) | ~ holdsAt(spinning(trolley6), n1) | ~ holdsAt(spinning(trolley5), n1) | ~ holdsAt(spinning(trolley4), n1) | ~ holdsAt(spinning(trolley3), n1) | ~ holdsAt(spinning(trolley2), n1) | ~ holdsAt(spinning(trolley1), n1)), inference(ennf_transformation, [], [f50])).
fof(f50, plain, ~ (holdsAt(spinning(trolley10), n1) & holdsAt(spinning(trolley9), n1) & holdsAt(spinning(trolley8), n1) & holdsAt(spinning(trolley7), n1) & holdsAt(spinning(trolley6), n1) & holdsAt(spinning(trolley5), n1) & holdsAt(spinning(trolley4), n1) & holdsAt(spinning(trolley3), n1) & holdsAt(spinning(trolley2), n1) & holdsAt(spinning(trolley1), n1)), inference(negated_conjecture, [], [f49])).
fof(f49, plain, ~ (holdsAt(spinning(trolley10), n1) & holdsAt(spinning(trolley9), n1) & holdsAt(spinning(trolley8), n1) & holdsAt(spinning(trolley7), n1) & holdsAt(spinning(trolley6), n1) & holdsAt(spinning(trolley5), n1) & holdsAt(spinning(trolley4), n1) & holdsAt(spinning(trolley3), n1) & holdsAt(spinning(trolley2), n1) & holdsAt(spinning(trolley1), n1)), file('/home/ubuntu/library/tptp/Problems/CSR/CSR024+1.010.p', spinning_3)).