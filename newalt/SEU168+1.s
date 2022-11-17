fof(f2768, plain, $false, inference(resolution, [], [f2729, f57])).
fof(f57, plain, ! [X0] : in(X0, sK8(X0)), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (! [X2] : (in(X2, sK8(X0)) | are_equipotent(X2, sK8(X0)) | ~ subset(X2, sK8(X0))) & ! [X3] : ((! [X5] : (in(X5, sK9(X0, X3)) | ~ subset(X5, X3)) & in(sK9(X0, X3), sK8(X0))) | ~ in(X3, sK8(X0))) & ! [X6, X7] : (in(X7, sK8(X0)) | ~ subset(X7, X6) | ~ in(X6, sK8(X0))) & in(X0, sK8(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f17, f37, f36])).
fof(f36, plain, ! [X0] : (? [X1] : (! [X2] : (in(X2, X1) | are_equipotent(X2, X1) | ~ subset(X2, X1)) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, X1)) | ~ in(X3, X1)) & ! [X6, X7] : (in(X7, X1) | ~ subset(X7, X6) | ~ in(X6, X1)) & in(X0, X1)) => (! [X2] : (in(X2, sK8(X0)) | are_equipotent(X2, sK8(X0)) | ~ subset(X2, sK8(X0))) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, sK8(X0))) | ~ in(X3, sK8(X0))) & ! [X7, X6] : (in(X7, sK8(X0)) | ~ subset(X7, X6) | ~ in(X6, sK8(X0))) & in(X0, sK8(X0)))), introduced(choice_axiom, [])).
fof(f37, plain, ! [X3, X0] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, sK8(X0))) => (! [X5] : (in(X5, sK9(X0, X3)) | ~ subset(X5, X3)) & in(sK9(X0, X3), sK8(X0)))), introduced(choice_axiom, [])).
fof(f17, plain, ! [X0] : ? [X1] : (! [X2] : (in(X2, X1) | are_equipotent(X2, X1) | ~ subset(X2, X1)) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, X1)) | ~ in(X3, X1)) & ! [X6, X7] : (in(X7, X1) | ~ subset(X7, X6) | ~ in(X6, X1)) & in(X0, X1)), inference(flattening, [], [f16])).
fof(f16, plain, ! [X0] : ? [X1] : (! [X2] : (in(X2, X1) | are_equipotent(X2, X1) | ~ subset(X2, X1)) & ! [X3] : (? [X4] : (! [X5] : (in(X5, X4) | ~ subset(X5, X3)) & in(X4, X1)) | ~ in(X3, X1)) & ! [X6, X7] : (in(X7, X1) | (~ subset(X7, X6) | ~ in(X6, X1))) & in(X0, X1)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X3] : ~ (! [X4] : ~ (! [X5] : (subset(X5, X3) => in(X5, X4)) & in(X4, X1)) & in(X3, X1)) & ! [X6, X7] : ((subset(X7, X6) & in(X6, X1)) => in(X7, X1)) & in(X0, X1)), inference(rectify, [], [f8])).
fof(f8, plain, ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X2] : ~ (! [X3] : ~ (! [X4] : (subset(X4, X2) => in(X4, X3)) & in(X3, X1)) & in(X2, X1)) & ! [X2, X3] : ((subset(X3, X2) & in(X2, X1)) => in(X3, X1)) & in(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+1.p', t9_tarski)).
fof(f2729, plain, ! [X0] : ~ in(sK5, sK8(X0)), inference(subsumption_resolution, [], [f2728, f273])).
fof(f273, plain, ! [X0] : (in(sK7(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0))), inference(resolution, [], [f157, f47])).
fof(f47, plain, ! [X0] : subset(X0, X0), inference(cnf_transformation, [], [f9])).
fof(f9, plain, ! [X0] : subset(X0, X0), inference(rectify, [], [f5])).
fof(f5, plain, ! [X0, X1] : subset(X0, X0), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+1.p', reflexivity_r1_tarski)).
fof(f157, plain, ! [X0, X1] : (~ subset(X1, sK7(sK8(X0))) | ~ in(sK5, sK8(X0)) | in(X1, sK8(X0))), inference(subsumption_resolution, [], [f155, f147])).
fof(f147, plain, ! [X6] : ~ sP0(sK8(X6)), inference(subsumption_resolution, [], [f146, f50])).
fof(f50, plain, ! [X0] : (~ in(sK4(X0), X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : ((~ in(sK4(X0), X0) & subset(sK4(X0), sK3(X0)) & in(sK3(X0), X0)) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3, sK4])], [f29, f30])).
fof(f30, plain, ! [X0] : (? [X1, X2] : (~ in(X2, X0) & subset(X2, X1) & in(X1, X0)) => (~ in(sK4(X0), X0) & subset(sK4(X0), sK3(X0)) & in(sK3(X0), X0))), introduced(choice_axiom, [])).
fof(f29, plain, ! [X0] : (? [X1, X2] : (~ in(X2, X0) & subset(X2, X1) & in(X1, X0)) | ~ sP0(X0)), inference(rectify, [], [f28])).
fof(f28, plain, ! [X1] : (? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1)) | ~ sP0(X1)), inference(nnf_transformation, [], [f18])).
fof(f18, plain, ! [X1] : (? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1)) | ~ sP0(X1)), inference(usedef, [], [e18])).
fof(e18, plain, ! [X1] : (sP0(X1) <=> ? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f146, plain, ! [X6] : (in(sK4(sK8(X6)), sK8(X6)) | ~ sP0(sK8(X6))), inference(duplicate_literal_removal, [], [f144])).
fof(f144, plain, ! [X6] : (in(sK4(sK8(X6)), sK8(X6)) | ~ sP0(sK8(X6)) | ~ sP0(sK8(X6))), inference(resolution, [], [f84, f49])).
fof(f49, plain, ! [X0] : (subset(sK4(X0), sK3(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f84, plain, ! [X6, X5] : (~ subset(X5, sK3(sK8(X6))) | in(X5, sK8(X6)) | ~ sP0(sK8(X6))), inference(resolution, [], [f58, f48])).
fof(f48, plain, ! [X0] : (in(sK3(X0), X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f58, plain, ! [X6, X0, X7] : (~ in(X6, sK8(X0)) | ~ subset(X7, X6) | in(X7, sK8(X0))), inference(cnf_transformation, [], [f38])).
fof(f155, plain, ! [X0, X1] : (sP0(sK8(X0)) | ~ in(sK5, sK8(X0)) | ~ subset(X1, sK7(sK8(X0))) | in(X1, sK8(X0))), inference(resolution, [], [f103, f58])).
fof(f103, plain, ! [X2] : (in(sK7(sK8(X2)), sK8(X2)) | sP0(sK8(X2)) | ~ in(sK5, sK8(X2))), inference(subsumption_resolution, [], [f102, f53])).
fof(f53, plain, ! [X1] : (~ are_equipotent(sK6(X1), X1) | in(sK7(X1), X1) | sP0(X1) | ~ in(sK5, X1)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X1] : ((~ in(sK6(X1), X1) & ~ are_equipotent(sK6(X1), X1) & subset(sK6(X1), X1)) | (~ in(powerset(sK7(X1)), X1) & in(sK7(X1), X1)) | sP0(X1) | ~ in(sK5, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6, sK7])], [f19, f34, f33, f32])).
fof(f32, plain, (? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | sP0(X1) | ~ in(X0, X1)) => ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | sP0(X1) | ~ in(sK5, X1))), introduced(choice_axiom, [])).
fof(f33, plain, ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) => (~ in(sK6(X1), X1) & ~ are_equipotent(sK6(X1), X1) & subset(sK6(X1), X1))), introduced(choice_axiom, [])).
fof(f34, plain, ! [X1] : (? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) => (~ in(powerset(sK7(X1)), X1) & in(sK7(X1), X1))), introduced(choice_axiom, [])).
fof(f19, plain, ? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | sP0(X1) | ~ in(X0, X1)), inference(definition_folding, [], [f15, e18])).
fof(f15, plain, ? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | ? [X4, X5] : (~ in(X5, X1) & subset(X5, X4) & in(X4, X1)) | ~ in(X0, X1)), inference(flattening, [], [f14])).
fof(f14, plain, ? [X0] : ! [X1] : (? [X2] : (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) | ? [X3] : (~ in(powerset(X3), X1) & in(X3, X1)) | ? [X4, X5] : (~ in(X5, X1) & (subset(X5, X4) & in(X4, X1))) | ~ in(X0, X1)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X3] : (in(X3, X1) => in(powerset(X3), X1)) & ! [X4, X5] : ((subset(X5, X4) & in(X4, X1)) => in(X5, X1)) & in(X0, X1)), inference(rectify, [], [f7])).
fof(f7, plain, ~ ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X2] : (in(X2, X1) => in(powerset(X2), X1)) & ! [X2, X3] : ((subset(X3, X2) & in(X2, X1)) => in(X3, X1)) & in(X0, X1)), inference(negated_conjecture, [], [f6])).
fof(f6, plain, ~ ! [X0] : ? [X1] : (! [X2] : ~ (~ in(X2, X1) & ~ are_equipotent(X2, X1) & subset(X2, X1)) & ! [X2] : (in(X2, X1) => in(powerset(X2), X1)) & ! [X2, X3] : ((subset(X3, X2) & in(X2, X1)) => in(X3, X1)) & in(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+1.p', t136_zfmisc_1)).
fof(f102, plain, ! [X2] : (in(sK7(sK8(X2)), sK8(X2)) | sP0(sK8(X2)) | ~ in(sK5, sK8(X2)) | are_equipotent(sK6(sK8(X2)), sK8(X2))), inference(subsumption_resolution, [], [f101, f55])).
fof(f55, plain, ! [X1] : (~ in(sK6(X1), X1) | in(sK7(X1), X1) | sP0(X1) | ~ in(sK5, X1)), inference(cnf_transformation, [], [f35])).
fof(f101, plain, ! [X2] : (in(sK7(sK8(X2)), sK8(X2)) | sP0(sK8(X2)) | ~ in(sK5, sK8(X2)) | are_equipotent(sK6(sK8(X2)), sK8(X2)) | in(sK6(sK8(X2)), sK8(X2))), inference(resolution, [], [f51, f61])).
fof(f61, plain, ! [X2, X0] : (~ subset(X2, sK8(X0)) | are_equipotent(X2, sK8(X0)) | in(X2, sK8(X0))), inference(cnf_transformation, [], [f38])).
fof(f51, plain, ! [X1] : (subset(sK6(X1), X1) | in(sK7(X1), X1) | sP0(X1) | ~ in(sK5, X1)), inference(cnf_transformation, [], [f35])).
fof(f2728, plain, ! [X0] : (~ in(sK5, sK8(X0)) | ~ in(sK7(sK8(X0)), sK8(X0))), inference(duplicate_literal_removal, [], [f2723])).
fof(f2723, plain, ! [X0] : (~ in(sK5, sK8(X0)) | ~ in(sK7(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0))), inference(resolution, [], [f2692, f237])).
fof(f237, plain, ! [X3] : (~ are_equipotent(sK6(sK8(X3)), sK8(X3)) | ~ in(sK7(sK8(X3)), sK8(X3)) | ~ in(sK5, sK8(X3))), inference(subsumption_resolution, [], [f232, f147])).
fof(f232, plain, ! [X3] : (~ in(sK7(sK8(X3)), sK8(X3)) | ~ are_equipotent(sK6(sK8(X3)), sK8(X3)) | sP0(sK8(X3)) | ~ in(sK5, sK8(X3))), inference(resolution, [], [f229, f54])).
fof(f54, plain, ! [X1] : (~ in(powerset(sK7(X1)), X1) | ~ are_equipotent(sK6(X1), X1) | sP0(X1) | ~ in(sK5, X1)), inference(cnf_transformation, [], [f35])).
fof(f229, plain, ! [X0, X1] : (in(powerset(X0), sK8(X1)) | ~ in(X0, sK8(X1))), inference(duplicate_literal_removal, [], [f225])).
fof(f225, plain, ! [X0, X1] : (~ in(X0, sK8(X1)) | in(powerset(X0), sK8(X1)) | ~ in(X0, sK8(X1))), inference(resolution, [], [f189, f85])).
fof(f85, plain, ! [X8, X7, X9] : (~ subset(X7, sK9(X8, X9)) | in(X7, sK8(X8)) | ~ in(X9, sK8(X8))), inference(resolution, [], [f58, f59])).
fof(f59, plain, ! [X0, X3] : (in(sK9(X0, X3), sK8(X0)) | ~ in(X3, sK8(X0))), inference(cnf_transformation, [], [f38])).
fof(f189, plain, ! [X0, X1] : (subset(powerset(X0), sK9(X1, X0)) | ~ in(X0, sK8(X1))), inference(duplicate_literal_removal, [], [f188])).
fof(f188, plain, ! [X0, X1] : (~ in(X0, sK8(X1)) | subset(powerset(X0), sK9(X1, X0)) | subset(powerset(X0), sK9(X1, X0))), inference(resolution, [], [f94, f72])).
fof(f72, plain, ! [X2, X3] : (subset(sK2(powerset(X2), X3), X2) | subset(powerset(X2), X3)), inference(resolution, [], [f45, f63])).
fof(f63, plain, ! [X0, X3] : (~ in(X3, powerset(X0)) | subset(X3, X0)), inference(equality_resolution, [], [f40])).
fof(f40, plain, ! [X0, X3, X1] : (subset(X3, X0) | ~ in(X3, X1) | ~ (powerset(X0) = X1)), inference(cnf_transformation, [], [f23])).
fof(f23, plain, ! [X0, X1] : (((powerset(X0) = X1) | ((~ subset(sK1(X0, X1), X0) | ~ in(sK1(X0, X1), X1)) & (subset(sK1(X0, X1), X0) | in(sK1(X0, X1), X1)))) & (! [X3] : ((in(X3, X1) | ~ subset(X3, X0)) & (subset(X3, X0) | ~ in(X3, X1))) | ~ (powerset(X0) = X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f21, f22])).
fof(f22, plain, ! [X1, X0] : (? [X2] : ((~ subset(X2, X0) | ~ in(X2, X1)) & (subset(X2, X0) | in(X2, X1))) => ((~ subset(sK1(X0, X1), X0) | ~ in(sK1(X0, X1), X1)) & (subset(sK1(X0, X1), X0) | in(sK1(X0, X1), X1)))), introduced(choice_axiom, [])).
fof(f21, plain, ! [X0, X1] : (((powerset(X0) = X1) | ? [X2] : ((~ subset(X2, X0) | ~ in(X2, X1)) & (subset(X2, X0) | in(X2, X1)))) & (! [X3] : ((in(X3, X1) | ~ subset(X3, X0)) & (subset(X3, X0) | ~ in(X3, X1))) | ~ (powerset(X0) = X1))), inference(rectify, [], [f20])).
fof(f20, plain, ! [X0, X1] : (((powerset(X0) = X1) | ? [X2] : ((~ subset(X2, X0) | ~ in(X2, X1)) & (subset(X2, X0) | in(X2, X1)))) & (! [X2] : ((in(X2, X1) | ~ subset(X2, X0)) & (subset(X2, X0) | ~ in(X2, X1))) | ~ (powerset(X0) = X1))), inference(nnf_transformation, [], [f2])).
fof(f2, plain, ! [X0, X1] : ((powerset(X0) = X1) <=> ! [X2] : (in(X2, X1) <=> subset(X2, X0))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+1.p', d1_zfmisc_1)).
fof(f45, plain, ! [X0, X1] : (in(sK2(X0, X1), X0) | subset(X0, X1)), inference(cnf_transformation, [], [f27])).
fof(f27, plain, ! [X0, X1] : ((subset(X0, X1) | (~ in(sK2(X0, X1), X1) & in(sK2(X0, X1), X0))) & (! [X3] : (in(X3, X1) | ~ in(X3, X0)) | ~ subset(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f25, f26])).
fof(f26, plain, ! [X1, X0] : (? [X2] : (~ in(X2, X1) & in(X2, X0)) => (~ in(sK2(X0, X1), X1) & in(sK2(X0, X1), X0))), introduced(choice_axiom, [])).
fof(f25, plain, ! [X0, X1] : ((subset(X0, X1) | ? [X2] : (~ in(X2, X1) & in(X2, X0))) & (! [X3] : (in(X3, X1) | ~ in(X3, X0)) | ~ subset(X0, X1))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X0, X1] : ((subset(X0, X1) | ? [X2] : (~ in(X2, X1) & in(X2, X0))) & (! [X2] : (in(X2, X1) | ~ in(X2, X0)) | ~ subset(X0, X1))), inference(nnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : (subset(X0, X1) <=> ! [X2] : (in(X2, X1) | ~ in(X2, X0))), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0, X1] : (subset(X0, X1) <=> ! [X2] : (in(X2, X0) => in(X2, X1))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU168+1.p', d3_tarski)).
fof(f94, plain, ! [X6, X8, X7] : (~ subset(sK2(X6, sK9(X7, X8)), X8) | ~ in(X8, sK8(X7)) | subset(X6, sK9(X7, X8))), inference(resolution, [], [f60, f46])).
fof(f46, plain, ! [X0, X1] : (~ in(sK2(X0, X1), X1) | subset(X0, X1)), inference(cnf_transformation, [], [f27])).
fof(f60, plain, ! [X0, X5, X3] : (in(X5, sK9(X0, X3)) | ~ subset(X5, X3) | ~ in(X3, sK8(X0))), inference(cnf_transformation, [], [f38])).
fof(f2692, plain, ! [X0] : (are_equipotent(sK6(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0))), inference(subsumption_resolution, [], [f2689, f2512])).
fof(f2512, plain, ! [X0] : (~ in(sK6(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0))), inference(duplicate_literal_removal, [], [f2502])).
fof(f2502, plain, ! [X0] : (~ in(sK6(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0)) | ~ in(sK5, sK8(X0))), inference(resolution, [], [f236, f273])).
fof(f236, plain, ! [X2] : (~ in(sK7(sK8(X2)), sK8(X2)) | ~ in(sK6(sK8(X2)), sK8(X2)) | ~ in(sK5, sK8(X2))), inference(subsumption_resolution, [], [f231, f147])).
fof(f231, plain, ! [X2] : (~ in(sK7(sK8(X2)), sK8(X2)) | ~ in(sK6(sK8(X2)), sK8(X2)) | sP0(sK8(X2)) | ~ in(sK5, sK8(X2))), inference(resolution, [], [f229, f56])).
fof(f56, plain, ! [X1] : (~ in(powerset(sK7(X1)), X1) | ~ in(sK6(X1), X1) | sP0(X1) | ~ in(sK5, X1)), inference(cnf_transformation, [], [f35])).
fof(f2689, plain, ! [X0] : (~ in(sK5, sK8(X0)) | are_equipotent(sK6(sK8(X0)), sK8(X0)) | in(sK6(sK8(X0)), sK8(X0))), inference(resolution, [], [f2656, f61])).
fof(f2656, plain, ! [X0] : (subset(sK6(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0))), inference(duplicate_literal_removal, [], [f2646])).
fof(f2646, plain, ! [X0] : (subset(sK6(sK8(X0)), sK8(X0)) | ~ in(sK5, sK8(X0)) | ~ in(sK5, sK8(X0))), inference(resolution, [], [f238, f273])).
fof(f238, plain, ! [X4] : (~ in(sK7(sK8(X4)), sK8(X4)) | subset(sK6(sK8(X4)), sK8(X4)) | ~ in(sK5, sK8(X4))), inference(subsumption_resolution, [], [f233, f147])).
fof(f233, plain, ! [X4] : (~ in(sK7(sK8(X4)), sK8(X4)) | subset(sK6(sK8(X4)), sK8(X4)) | sP0(sK8(X4)) | ~ in(sK5, sK8(X4))), inference(resolution, [], [f229, f52])).
fof(f52, plain, ! [X1] : (~ in(powerset(sK7(X1)), X1) | subset(sK6(X1), X1) | sP0(X1) | ~ in(sK5, X1)), inference(cnf_transformation, [], [f35])).