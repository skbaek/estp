fof(f2775, plain, $false, inference(resolution, [], [f2736, f64])).
fof(f64, plain, ! [X0] : in(X0, sK10(X0)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (! [X2] : (in(X2, sK10(X0)) | are_equipotent(X2, sK10(X0)) | ~ subset(X2, sK10(X0))) & ! [X3] : ((! [X5] : (in(X5, sK11(X0, X3)) | ~ subset(X5, X3)) & in(sK11(X0, X3), sK10(X0))) | ~ in(X3, sK10(X0))) & ! [X6, X7] : (in(X7, sK10(X0)) | ~ subset(X7, X6) | ~ in(X6, sK10(X0))) & in(X0, sK10(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11])], [f18, f42, f41])).
fof(f41, plain, ! [X0] : (? [X1] : (! [X2] : (in(X2, X1) | are_equipotent(X2, X1) | ~ subset(X2, X1)) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, X1)) | ~ in(X3, X1)) & ! [X6, X7] : (in(X7, X1) | ~ subset(X7, X6) | ~ in(X6, X1)) & in(X0, X1)) => (! [X2] : (in(X2, sK10(X0)) | are_equipotent(X2, sK10(X0)) | ~ subset(X2, sK10(X0))) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, sK10(X0))) | ~ in(X3, sK10(X0))) & ! [X7, X6] : (in(X7, sK10(X0)) | ~ subset(X7, X6) | ~ in(X6, sK10(X0))) & in(X0, sK10(X0)))), introduced(choice_axiom, [])).
fof(f42, plain, ! [X3, X0] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, sK10(X0))) => (! [X5] : (in(X5, sK11(X0, X3)) | ~ subset(X5, X3)) & in(sK11(X0, X3), sK10(X0)))), introduced(choice_axiom, [])).
fof(f18, plain, ! [X0] : ? [X1] : (! [X2] : (in(X2, X1) | are_equipotent(X2, X1) | ~ subset(X2, X1)) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, X1)) | ~ in(X3, X1)) & ! [X6, X7] : (in(X7, X1) | ~ subset(X7, X6) | ~ in(X6, X1)) & in(X0, X1)), inference(flattening, [], [f17])).
fof(f17, plain, ! [X0] : ? [X1] : (! [X2] : (in(X2, X1) | are_equipotent(X2, X1) | ~ subset(X2, X1)) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, X1)) | ~ in(X3, X1)) & ! [X6, X7] : (in(X7, X1) | (~ subset(X7, X6) | ~ in(X6, X1))) & in(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X3] : ~ (! [X4] : ~ (! [X5] : (subset(X5, X3) => in(X5, X4)) & in(X4, X1)) & in(X3, X1)) & ! [X6, X7] : ((subset(X7, X6) & in(X6, X1)) => in(X7, X1)) & in(X0, X1)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X2] : ~ (! [X3] : ~ (! [X4] : (subset(X4, X2) => in(X4, X3)) & in(X3, X1)) & in(X2, X1)) & ! [X2, X3] : ((subset(X3, X2) & in(X2, X1)) => in(X3, X1)) & in(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+3.p', t9_tarski)).
fof(f2736, plain, ! [X0] : ~ in(sK7, sK10(X0)), inference(subsumption_resolution, [], [f2735, f280])).
fof(f280, plain, ! [X0] : (in(sK9(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0))), inference(resolution, [], [f164, f54])).
fof(f54, plain, ! [X0] : subset(X0, X0), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : subset(X0, X0), inference(rectify, [], [f6])).
fof(f6, plain, ! [X0, X1] : subset(X0, X0), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+3.p', reflexivity_r1_tarski)).
fof(f164, plain, ! [X0, X1] : (~ subset(X1, sK9(sK10(X0))) | ~ in(sK7, sK10(X0)) | in(X1, sK10(X0))), inference(subsumption_resolution, [], [f162, f154])).
fof(f154, plain, ! [X6] : ~ sP0(sK10(X6)), inference(subsumption_resolution, [], [f153, f57])).
fof(f57, plain, ! [X0] : (~ in(sK6(X0), X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : ((~ in(sK6(X0), X0) & subset(sK6(X0), sK5(X0)) & in(sK5(X0), X0)) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6])], [f34, f35])).
fof(f35, plain, ! [X0] : (? [X1, X2] : (~ in(X2, X0) & subset(X2, X1) & in(X1, X0)) => (~ in(sK6(X0), X0) & subset(sK6(X0), sK5(X0)) & in(sK5(X0), X0))), introduced(choice_axiom, [])).
fof(f34, plain, ! [X0] : (? [X1, X2] : (~ in(X2, X0) & subset(X2, X1) & in(X1, X0)) | ~ sP0(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X1] : (? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1)) | ~ sP0(X1)), inference(nnf_transformation, [], [f19])).
fof(f19, plain, ! [X1] : (? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1)) | ~ sP0(X1)), inference(usedef, [], [e19])).
fof(e19, plain, ! [X1] : (sP0(X1) <=> ? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f153, plain, ! [X6] : (in(sK6(sK10(X6)), sK10(X6)) | ~ sP0(sK10(X6))), inference(duplicate_literal_removal, [], [f151])).
fof(f151, plain, ! [X6] : (in(sK6(sK10(X6)), sK10(X6)) | ~ sP0(sK10(X6)) | ~ sP0(sK10(X6))), inference(resolution, [], [f91, f56])).
fof(f56, plain, ! [X0] : (subset(sK6(X0), sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f36])).
fof(f91, plain, ! [X6, X5] : (~ subset(X5, sK5(sK10(X6))) | in(X5, sK10(X6)) | ~ sP0(sK10(X6))), inference(resolution, [], [f65, f55])).
fof(f55, plain, ! [X0] : (in(sK5(X0), X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f36])).
fof(f65, plain, ! [X6, X0, X7] : (~ in(X6, sK10(X0)) | ~ subset(X7, X6) | in(X7, sK10(X0))), inference(cnf_transformation, [], [f43])).
fof(f162, plain, ! [X0, X1] : (sP0(sK10(X0)) | ~ in(sK7, sK10(X0)) | ~ subset(X1, sK9(sK10(X0))) | in(X1, sK10(X0))), inference(resolution, [], [f110, f65])).
fof(f110, plain, ! [X2] : (in(sK9(sK10(X2)), sK10(X2)) | sP0(sK10(X2)) | ~ in(sK7, sK10(X2))), inference(subsumption_resolution, [], [f109, f60])).
fof(f60, plain, ! [X1] : (~ are_equipotent(sK8(X1), X1) | in(sK9(X1), X1) | sP0(X1) | ~ in(sK7, X1)), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X1] : ((~ in(sK8(X1), X1) & ~ are_equipotent(sK8(X1), X1) & subset(sK8(X1), X1)) | (~ in(powerset(sK9(X1)), X1) & in(sK9(X1), X1)) | sP0(X1) | ~ in(sK7, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8, sK9])], [f20, f39, f38, f37])).
fof(f37, plain, (? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | sP0(X1) | ~ in(X0, X1)) => ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | sP0(X1) | ~ in(sK7, X1))), introduced(choice_axiom, [])).
fof(f38, plain, ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) => (~ in(sK8(X1), X1) & ~ are_equipotent(sK8(X1), X1) & subset(sK8(X1), X1))), introduced(choice_axiom, [])).
fof(f39, plain, ! [X1] : (? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) => (~ in(powerset(sK9(X1)), X1) & in(sK9(X1), X1))), introduced(choice_axiom, [])).
fof(f20, plain, ? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | sP0(X1) | ~ in(X0, X1)), inference(definition_folding, [], [f16, e19])).
fof(f16, plain, ? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | ? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1)) | ~ in(X0, X1)), inference(flattening, [], [f15])).
fof(f15, plain, ? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | ? [X4, X5] : (~ in(X5, X1) & (subset(X5, X4) & in(X4, X1))) | ~ in(X0, X1)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X3] : (in(X3, X1) => in(powerset(X3), X1)) & ! [X4, X5] : ((subset(X5, X4) & in(X4, X1)) => in(X5, X1)) & in(X0, X1)), inference(rectify, [], [f8])).
fof(f8, plain, ~ ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X2] : (in(X2, X1) => in(powerset(X2), X1)) & ! [X2, X3] : ((subset(X3, X2) & in(X2, X1)) => in(X3, X1)) & in(X0, X1)), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X2] : (in(X2, X1) => in(powerset(X2), X1)) & ! [X2, X3] : ((subset(X3, X2) & in(X2, X1)) => in(X3, X1)) & in(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+3.p', t136_zfmisc_1)).
fof(f109, plain, ! [X2] : (in(sK9(sK10(X2)), sK10(X2)) | sP0(sK10(X2)) | ~ in(sK7, sK10(X2)) | are_equipotent(sK8(sK10(X2)), sK10(X2))), inference(subsumption_resolution, [], [f108, f62])).
fof(f62, plain, ! [X1] : (~ in(sK8(X1), X1) | in(sK9(X1), X1) | sP0(X1) | ~ in(sK7, X1)), inference(cnf_transformation, [], [f40])).
fof(f108, plain, ! [X2] : (in(sK9(sK10(X2)), sK10(X2)) | sP0(sK10(X2)) | ~ in(sK7, sK10(X2)) | are_equipotent(sK8(sK10(X2)), sK10(X2)) | in(sK8(sK10(X2)), sK10(X2))), inference(resolution, [], [f58, f68])).
fof(f68, plain, ! [X2, X0] : (~ subset(X2, sK10(X0)) | are_equipotent(X2, sK10(X0)) | in(X2, sK10(X0))), inference(cnf_transformation, [], [f43])).
fof(f58, plain, ! [X1] : (subset(sK8(X1), X1) | in(sK9(X1), X1) | sP0(X1) | ~ in(sK7, X1)), inference(cnf_transformation, [], [f40])).
fof(f2735, plain, ! [X0] : (~ in(sK7, sK10(X0)) | ~ in(sK9(sK10(X0)), sK10(X0))), inference(duplicate_literal_removal, [], [f2730])).
fof(f2730, plain, ! [X0] : (~ in(sK7, sK10(X0)) | ~ in(sK9(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0))), inference(resolution, [], [f2699, f244])).
fof(f244, plain, ! [X3] : (~ are_equipotent(sK8(sK10(X3)), sK10(X3)) | ~ in(sK9(sK10(X3)), sK10(X3)) | ~ in(sK7, sK10(X3))), inference(subsumption_resolution, [], [f239, f154])).
fof(f239, plain, ! [X3] : (~ in(sK9(sK10(X3)), sK10(X3)) | ~ are_equipotent(sK8(sK10(X3)), sK10(X3)) | sP0(sK10(X3)) | ~ in(sK7, sK10(X3))), inference(resolution, [], [f236, f61])).
fof(f61, plain, ! [X1] : (~ in(powerset(sK9(X1)), X1) | ~ are_equipotent(sK8(X1), X1) | sP0(X1) | ~ in(sK7, X1)), inference(cnf_transformation, [], [f40])).
fof(f236, plain, ! [X0, X1] : (in(powerset(X0), sK10(X1)) | ~ in(X0, sK10(X1))), inference(duplicate_literal_removal, [], [f232])).
fof(f232, plain, ! [X0, X1] : (~ in(X0, sK10(X1)) | in(powerset(X0), sK10(X1)) | ~ in(X0, sK10(X1))), inference(resolution, [], [f196, f92])).
fof(f92, plain, ! [X8, X7, X9] : (~ subset(X7, sK11(X8, X9)) | in(X7, sK10(X8)) | ~ in(X9, sK10(X8))), inference(resolution, [], [f65, f66])).
fof(f66, plain, ! [X0, X3] : (in(sK11(X0, X3), sK10(X0)) | ~ in(X3, sK10(X0))), inference(cnf_transformation, [], [f43])).
fof(f196, plain, ! [X0, X1] : (subset(powerset(X0), sK11(X1, X0)) | ~ in(X0, sK10(X1))), inference(duplicate_literal_removal, [], [f195])).
fof(f195, plain, ! [X0, X1] : (~ in(X0, sK10(X1)) | subset(powerset(X0), sK11(X1, X0)) | subset(powerset(X0), sK11(X1, X0))), inference(resolution, [], [f101, f79])).
fof(f79, plain, ! [X2, X3] : (subset(sK2(powerset(X2), X3), X2) | subset(powerset(X2), X3)), inference(resolution, [], [f50, f70])).
fof(f70, plain, ! [X0, X3] : (~ in(X3, powerset(X0)) | subset(X3, X0)), inference(equality_resolution, [], [f45])).
fof(f45, plain, ! [X0, X3, X1] : (subset(X3, X0) | ~ in(X3, X1) | ~ (powerset(X0) = X1)), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : (((powerset(X0) = X1) | ((~ subset(sK1(X0, X1), X0) | ~ in(sK1(X0, X1), X1)) & (subset(sK1(X0, X1), X0) | in(sK1(X0, X1), X1)))) & (! [X3] : ((in(X3, X1) | ~ subset(X3, X0)) & (subset(X3, X0) | ~ in(X3, X1))) | ~ (powerset(X0) = X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f22, f23])).
fof(f23, plain, ! [X1, X0] : (? [X2] : ((~ subset(X2, X0) | ~ in(X2, X1)) & (subset(X2, X0) | in(X2, X1))) => ((~ subset(sK1(X0, X1), X0) | ~ in(sK1(X0, X1), X1)) & (subset(sK1(X0, X1), X0) | in(sK1(X0, X1), X1)))), introduced(choice_axiom, [])).
fof(f22, plain, ! [X0, X1] : (((powerset(X0) = X1) | ? [X2] : ((~ subset(X2, X0) | ~ in(X2, X1)) & (subset(X2, X0) | in(X2, X1)))) & (! [X3] : ((in(X3, X1) | ~ subset(X3, X0)) & (subset(X3, X0) | ~ in(X3, X1))) | ~ (powerset(X0) = X1))), inference(rectify, [], [f21])).
fof(f21, plain, ! [X0, X1] : (((powerset(X0) = X1) | ? [X2] : ((~ subset(X2, X0) | ~ in(X2, X1)) & (subset(X2, X0) | in(X2, X1)))) & (! [X2] : ((in(X2, X1) | ~ subset(X2, X0)) & (subset(X2, X0) | ~ in(X2, X1))) | ~ (powerset(X0) = X1))), inference(nnf_transformation, [], [f2])).
fof(f2, plain, ! [X0, X1] : ((powerset(X0) = X1) <=> ! [X2] : (in(X2, X1) <=> subset(X2, X0))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+3.p', d1_zfmisc_1)).
fof(f50, plain, ! [X0, X1] : (in(sK2(X0, X1), X0) | subset(X0, X1)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ! [X0, X1] : ((subset(X0, X1) | (~ in(sK2(X0, X1), X1) & in(sK2(X0, X1), X0))) & (! [X3] : (in(X3, X1) | ~ in(X3, X0)) | ~ subset(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f26, f27])).
fof(f27, plain, ! [X1, X0] : (? [X2] : (~ in(X2, X1) & in(X2, X0)) => (~ in(sK2(X0, X1), X1) & in(sK2(X0, X1), X0))), introduced(choice_axiom, [])).
fof(f26, plain, ! [X0, X1] : ((subset(X0, X1) | ? [X2] : (~ in(X2, X1) & in(X2, X0))) & (! [X3] : (in(X3, X1) | ~ in(X3, X0)) | ~ subset(X0, X1))), inference(rectify, [], [f25])).
fof(f25, plain, ! [X0, X1] : ((subset(X0, X1) | ? [X2] : (~ in(X2, X1) & in(X2, X0))) & (! [X2] : (in(X2, X1) | ~ in(X2, X0)) | ~ subset(X0, X1))), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1] : (subset(X0, X1) <=> ! [X2] : (in(X2, X1) | ~ in(X2, X0))), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0, X1] : (subset(X0, X1) <=> ! [X2] : (in(X2, X0) => in(X2, X1))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+3.p', d3_tarski)).
fof(f101, plain, ! [X6, X8, X7] : (~ subset(sK2(X6, sK11(X7, X8)), X8) | ~ in(X8, sK10(X7)) | subset(X6, sK11(X7, X8))), inference(resolution, [], [f67, f51])).
fof(f51, plain, ! [X0, X1] : (~ in(sK2(X0, X1), X1) | subset(X0, X1)), inference(cnf_transformation, [], [f28])).
fof(f67, plain, ! [X0, X5, X3] : (in(X5, sK11(X0, X3)) | ~ subset(X5, X3) | ~ in(X3, sK10(X0))), inference(cnf_transformation, [], [f43])).
fof(f2699, plain, ! [X0] : (are_equipotent(sK8(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0))), inference(subsumption_resolution, [], [f2696, f2519])).
fof(f2519, plain, ! [X0] : (~ in(sK8(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0))), inference(duplicate_literal_removal, [], [f2509])).
fof(f2509, plain, ! [X0] : (~ in(sK8(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0)) | ~ in(sK7, sK10(X0))), inference(resolution, [], [f243, f280])).
fof(f243, plain, ! [X2] : (~ in(sK9(sK10(X2)), sK10(X2)) | ~ in(sK8(sK10(X2)), sK10(X2)) | ~ in(sK7, sK10(X2))), inference(subsumption_resolution, [], [f238, f154])).
fof(f238, plain, ! [X2] : (~ in(sK9(sK10(X2)), sK10(X2)) | ~ in(sK8(sK10(X2)), sK10(X2)) | sP0(sK10(X2)) | ~ in(sK7, sK10(X2))), inference(resolution, [], [f236, f63])).
fof(f63, plain, ! [X1] : (~ in(powerset(sK9(X1)), X1) | ~ in(sK8(X1), X1) | sP0(X1) | ~ in(sK7, X1)), inference(cnf_transformation, [], [f40])).
fof(f2696, plain, ! [X0] : (~ in(sK7, sK10(X0)) | are_equipotent(sK8(sK10(X0)), sK10(X0)) | in(sK8(sK10(X0)), sK10(X0))), inference(resolution, [], [f2663, f68])).
fof(f2663, plain, ! [X0] : (subset(sK8(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0))), inference(duplicate_literal_removal, [], [f2653])).
fof(f2653, plain, ! [X0] : (subset(sK8(sK10(X0)), sK10(X0)) | ~ in(sK7, sK10(X0)) | ~ in(sK7, sK10(X0))), inference(resolution, [], [f245, f280])).
fof(f245, plain, ! [X4] : (~ in(sK9(sK10(X4)), sK10(X4)) | subset(sK8(sK10(X4)), sK10(X4)) | ~ in(sK7, sK10(X4))), inference(subsumption_resolution, [], [f240, f154])).
fof(f240, plain, ! [X4] : (~ in(sK9(sK10(X4)), sK10(X4)) | subset(sK8(sK10(X4)), sK10(X4)) | sP0(sK10(X4)) | ~ in(sK7, sK10(X4))), inference(resolution, [], [f236, f59])).
fof(f59, plain, ! [X1] : (~ in(powerset(sK9(X1)), X1) | subset(sK8(X1), X1) | sP0(X1) | ~ in(sK7, X1)), inference(cnf_transformation, [], [f40])).