fof(f9359, plain, $false, inference(avatar_sat_refutation, [], [f449, f2484, f4067, f4795, f4918, f5360, f5375, f5414, f6338, f6346, f7360, f7956, f9100, f9356, f9358])).
fof(f9358, plain, (~ spl27_15 | ~ spl27_14), inference(avatar_split_clause, [], [f9357, f442, f446])).
fof(f446, plain, (spl27_15 <=> s1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_15])])).
fof(f442, plain, (spl27_14 <=> conditionhyper(sK26(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_14])])).
fof(f9357, plain, (~ s1(n0) | ~ spl27_14), inference(subsumption_resolution, [], [f6473, f194])).
fof(f194, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', irreflexivity_gt)).
fof(f6473, plain, (~ s1(n0) | gt(n0, n0) | ~ spl27_14), inference(resolution, [], [f444, f299])).
fof(f299, plain, ! [X0] : (~ conditionhyper(sK26(X0)) | ~ s1(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f193, plain, (! [X0] : ((~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0))) | ~ s1(X0) | gt(n0, X0)) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s0(n0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK26])], [f191, f192])).
fof(f192, plain, ! [X0] : (? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) => (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0)))), introduced(choice_axiom, [])).
fof(f191, plain, (! [X0] : (? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ~ s1(X0) | gt(n0, X0)) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s0(n0)), inference(rectify, [], [f125])).
fof(f125, plain, (! [X1] : (? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s1(X1) | gt(n0, X1)) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s0(n0)), inference(flattening, [], [f124])).
fof(f124, plain, (! [X1] : (? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s1(X1) | gt(n0, X1)) & (! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s0(n0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ~ ((! [X0] : (gt(n0, X0) => conditionhyper(X0)) & s0(n0)) => ? [X1] : (! [X2] : (gt(X1, X2) => conditionhyper(X2)) & s1(X1) & ~ gt(n0, X1))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s0(n0)) => ? [X3] : (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s1(X3) & ~ gt(n0, X3))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s0(n0)) => ? [X3] : (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s1(X3) & ~ gt(n0, X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', transs0s1)).
fof(f444, plain, (conditionhyper(sK26(n0)) | ~ spl27_14), inference(avatar_component_clause, [], [f442])).
fof(f9356, plain, (spl27_7 | spl27_16 | spl27_1 | spl27_4 | ~ spl27_218), inference(avatar_split_clause, [], [f9341, f5353, f344, f322, f484, f369])).
fof(f369, plain, (spl27_7 <=> sP1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_7])])).
fof(f484, plain, (spl27_16 <=> ! [X6] : (gt(n0, X6) | uptakepg(X6))), introduced(avatar_definition, [new_symbols(naming, [spl27_16])])).
fof(f322, plain, (spl27_1 <=> sP3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_1])])).
fof(f344, plain, (spl27_4 <=> sP2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_4])])).
fof(f5353, plain, (spl27_218 <=> conditionnormo(sK18(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_218])])).
fof(f9341, plain, (! [X0] : (gt(n0, X0) | sP1(n0) | uptakepg(X0)) | (spl27_1 | spl27_4 | ~ spl27_218)), inference(subsumption_resolution, [], [f9340, f346])).
fof(f346, plain, (~ sP2(n0) | spl27_4), inference(avatar_component_clause, [], [f344])).
fof(f9340, plain, (! [X0] : (gt(n0, X0) | sP2(n0) | sP1(n0) | uptakepg(X0)) | (spl27_1 | ~ spl27_218)), inference(subsumption_resolution, [], [f9334, f324])).
fof(f324, plain, (~ sP3(n0) | spl27_1), inference(avatar_component_clause, [], [f322])).
fof(f9334, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X0)) | ~ spl27_218), inference(resolution, [], [f5355, f259])).
fof(f259, plain, ! [X2, X0] : (~ conditionnormo(sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & bcapacityex(X0) & ! [X2] : (uptakepg(X2) | gt(X0, X2)) & ! [X3] : (uptakelg(X3) | gt(X0, X3))) | sP3(X0) | sP2(X0) | sP1(X0) | (~ conditionnormo(sK18(X0)) & ~ gt(X0, sK18(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f169, f170])).
fof(f170, plain, ! [X0] : (? [X4] : (~ conditionnormo(X4) & ~ gt(X0, X4)) => (~ conditionnormo(sK18(X0)) & ~ gt(X0, sK18(X0)))), introduced(choice_axiom, [])).
fof(f169, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & bcapacityex(X0) & ! [X2] : (uptakepg(X2) | gt(X0, X2)) & ! [X3] : (uptakelg(X3) | gt(X0, X3))) | sP3(X0) | sP2(X0) | sP1(X0) | ? [X4] : (~ conditionnormo(X4) & ~ gt(X0, X4))), inference(rectify, [], [f131])).
fof(f131, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | sP3(X0) | sP2(X0) | sP1(X0) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(definition_folding, [], [f100, e130, e129, e128])).
fof(f128, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(usedef, [], [e128])).
fof(e128, plain, ! [X0] : (sP1(X0) <=> (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f129, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(usedef, [], [e129])).
fof(e129, plain, ! [X0] : (sP2(X0) <=> (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f130, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(usedef, [], [e130])).
fof(e130, plain, ! [X0] : (sP3(X0) <=> (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f100, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(flattening, [], [f99])).
fof(f99, plain, ! [X0] : (((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12)))) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) => ((! [X2] : (gt(X0, X2) => conditionhyper(X2)) & bcapacityex(X0) & ! [X3] : (~ gt(X0, X3) => uptakepg(X3)) & ! [X4] : (~ gt(X0, X4) => uptakelg(X4))) | (! [X5] : (gt(X0, X5) => conditionhyper(X5)) & ! [X6] : (~ gt(X0, X6) => bsecretioni(X6)) & bcapacityne(X0) & (! [X7] : (~ gt(X0, X7) => uptakepg(X7)) | ! [X8] : (~ gt(X0, X8) => ~ releaselg(X8)))) | (! [X9] : (gt(X0, X9) => conditionhyper(X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ gt(X0, X10) => ~ releaselg(X10))) | (! [X11] : (gt(X0, X11) => conditionhyper(X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (~ gt(X0, X12) => bsecretioni(X12))))), inference(rectify, [], [f26])).
fof(f26, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) => ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => uptakepg(X4)) & ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ~ qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', normo)).
fof(f5355, plain, (conditionnormo(sK18(n0)) | ~ spl27_218), inference(avatar_component_clause, [], [f5353])).
fof(f9100, plain, (~ spl27_7 | spl27_15 | spl27_59), inference(avatar_contradiction_clause, [], [f9099])).
fof(f9099, plain, ($false | (~ spl27_7 | spl27_15 | spl27_59)), inference(subsumption_resolution, [], [f9098, f296])).
fof(f296, plain, s0(n0), inference(cnf_transformation, [], [f193])).
fof(f9098, plain, (~ s0(n0) | (~ spl27_7 | spl27_15 | spl27_59)), inference(resolution, [], [f8980, f240])).
fof(f240, plain, ! [X0] : (~ s3(X0) | ~ s0(X0)), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ! [X0] : (~ s3(X0) | ~ s0(X0)), inference(rectify, [], [f22])).
fof(f22, plain, ! [X3] : (~ s3(X3) | ~ s0(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', xorstep4)).
fof(f8980, plain, (s3(n0) | (~ spl27_7 | spl27_15 | spl27_59)), inference(subsumption_resolution, [], [f8979, f448])).
fof(f448, plain, (~ s1(n0) | spl27_15), inference(avatar_component_clause, [], [f446])).
fof(f8979, plain, (s1(n0) | s3(n0) | (~ spl27_7 | spl27_59)), inference(subsumption_resolution, [], [f8473, f867])).
fof(f867, plain, (~ s2(n0) | spl27_59), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl27_59 <=> s2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_59])])).
fof(f8473, plain, (s2(n0) | s1(n0) | s3(n0) | ~ spl27_7), inference(resolution, [], [f8391, f272])).
fof(f272, plain, ! [X0] : (~ drugsu(X0) | s2(X0) | s1(X0) | s3(X0)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ! [X0] : (s3(X0) | s2(X0) | s1(X0) | ~ drugsu(X0)), inference(flattening, [], [f110])).
fof(f110, plain, ! [X0] : ((s3(X0) | s2(X0) | s1(X0)) | ~ drugsu(X0)), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : (drugsu(X0) => (s3(X0) | s2(X0) | s1(X0))), inference(pure_predicate_removal, [], [f72])).
fof(f72, plain, ! [X0] : (drugsu(X0) => (s3(X0) | s2(X0) | (qillt27(X0) & s1(X0)))), inference(rectify, [], [f32])).
fof(f32, plain, ! [X3] : (drugsu(X3) => (s3(X3) | s2(X3) | (qillt27(X3) & s1(X3)))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', sucomp)).
fof(f8391, plain, (drugsu(n0) | ~ spl27_7), inference(resolution, [], [f370, f1081])).
fof(f1081, plain, ! [X0] : (~ sP1(X0) | drugsu(X0)), inference(resolution, [], [f596, f194])).
fof(f596, plain, ! [X4, X5] : (gt(X4, X5) | drugsu(X5) | ~ sP1(X4)), inference(subsumption_resolution, [], [f591, f280])).
fof(f280, plain, ! [X0, X1] : (~ bsecretioni(sK21(X0)) | gt(X0, X1) | drugsu(X1)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X1] : (drugsu(X1) | gt(X0, X1))) | (~ bsecretioni(sK21(X0)) & ~ gt(X0, sK21(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK21])], [f176, f177])).
fof(f177, plain, ! [X0] : (? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2)) => (~ bsecretioni(sK21(X0)) & ~ gt(X0, sK21(X0)))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X1] : (drugsu(X1) | gt(X0, X1))) | ? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2))), inference(rectify, [], [f116])).
fof(f116, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X2] : (drugsu(X2) | gt(X0, X2))) | ? [X1] : (~ bsecretioni(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => bsecretioni(X1)) => (~ bcapacityex(X0) & ! [X2] : (~ gt(X0, X2) => drugsu(X2)))), inference(rectify, [], [f36])).
fof(f36, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) => (~ bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => drugsu(X4)))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', su_completion)).
fof(f591, plain, ! [X4, X5] : (gt(X4, X5) | drugsu(X5) | bsecretioni(sK21(X4)) | ~ sP1(X4)), inference(resolution, [], [f279, f252])).
fof(f252, plain, ! [X2, X0] : (gt(X0, X2) | bsecretioni(X2) | ~ sP1(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & qilt27(X0) & bcapacitysn(X0) & ! [X2] : (bsecretioni(X2) | gt(X0, X2))) | ~ sP1(X0)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(nnf_transformation, [], [f128])).
fof(f279, plain, ! [X0, X1] : (~ gt(X0, sK21(X0)) | gt(X0, X1) | drugsu(X1)), inference(cnf_transformation, [], [f178])).
fof(f370, plain, (sP1(n0) | ~ spl27_7), inference(avatar_component_clause, [], [f369])).
fof(f7956, plain, (~ spl27_1 | spl27_5 | spl27_16), inference(avatar_split_clause, [], [f875, f484, f348, f322])).
fof(f348, plain, (spl27_5 <=> ! [X1] : (~ releaselg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_5])])).
fof(f875, plain, ! [X2, X3] : (gt(n0, X2) | ~ releaselg(X3) | uptakepg(X2) | ~ sP3(n0) | conditionhyper(X3)), inference(resolution, [], [f244, f297])).
fof(f297, plain, ! [X2] : (~ gt(n0, X2) | conditionhyper(X2)), inference(cnf_transformation, [], [f193])).
fof(f244, plain, ! [X4, X0, X3] : (gt(X0, X4) | gt(X0, X3) | ~ releaselg(X4) | uptakepg(X3) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ! [X2] : (bsecretioni(X2) | gt(X0, X2)) & bcapacityne(X0) & (! [X3] : (uptakepg(X3) | gt(X0, X3)) | ! [X4] : (~ releaselg(X4) | gt(X0, X4)))) | ~ sP3(X0)), inference(rectify, [], [f163])).
fof(f163, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(nnf_transformation, [], [f130])).
fof(f7360, plain, (~ spl27_5 | ~ spl27_120 | ~ spl27_220), inference(avatar_contradiction_clause, [], [f7359])).
fof(f7359, plain, ($false | (~ spl27_5 | ~ spl27_120 | ~ spl27_220)), inference(subsumption_resolution, [], [f7358, f2559])).
fof(f2559, plain, (conditionhyper(sK20(n0)) | (~ spl27_5 | ~ spl27_120)), inference(resolution, [], [f1985, f349])).
fof(f349, plain, (! [X1] : (~ releaselg(X1) | conditionhyper(X1)) | ~ spl27_5), inference(avatar_component_clause, [], [f348])).
fof(f1985, plain, (releaselg(sK20(n0)) | ~ spl27_120), inference(avatar_component_clause, [], [f1984])).
fof(f1984, plain, (spl27_120 <=> releaselg(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_120])])).
fof(f7358, plain, (~ conditionhyper(sK20(n0)) | ~ spl27_220), inference(resolution, [], [f5374, f202])).
fof(f202, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (~ conditionnormo(X3) | ~ conditionhyper(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', xorcondition3)).
fof(f5374, plain, (conditionnormo(sK20(n0)) | ~ spl27_220), inference(avatar_component_clause, [], [f5372])).
fof(f5372, plain, (spl27_220 <=> conditionnormo(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_220])])).
fof(f6346, plain, ~ spl27_59, inference(avatar_contradiction_clause, [], [f6345])).
fof(f6345, plain, ($false | ~ spl27_59), inference(subsumption_resolution, [], [f6344, f296])).
fof(f6344, plain, (~ s0(n0) | ~ spl27_59), inference(resolution, [], [f866, f239])).
fof(f239, plain, ! [X0] : (~ s2(X0) | ~ s0(X0)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0] : (~ s2(X0) | ~ s0(X0)), inference(rectify, [], [f21])).
fof(f21, plain, ! [X3] : (~ s2(X3) | ~ s0(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', xorstep3)).
fof(f866, plain, (s2(n0) | ~ spl27_59), inference(avatar_component_clause, [], [f865])).
fof(f6338, plain, (~ spl27_4 | spl27_15 | spl27_59), inference(avatar_contradiction_clause, [], [f6337])).
fof(f6337, plain, ($false | (~ spl27_4 | spl27_15 | spl27_59)), inference(subsumption_resolution, [], [f6336, f296])).
fof(f6336, plain, (~ s0(n0) | (~ spl27_4 | spl27_15 | spl27_59)), inference(resolution, [], [f5958, f240])).
fof(f5958, plain, (s3(n0) | (~ spl27_4 | spl27_15 | spl27_59)), inference(subsumption_resolution, [], [f5957, f448])).
fof(f5957, plain, (s1(n0) | s3(n0) | (~ spl27_4 | spl27_59)), inference(subsumption_resolution, [], [f5956, f867])).
fof(f5956, plain, (s2(n0) | s1(n0) | s3(n0) | ~ spl27_4), inference(resolution, [], [f5644, f270])).
fof(f270, plain, ! [X0] : (~ drugbg(X0) | s2(X0) | s1(X0) | s3(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0] : (s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0)) | ~ drugbg(X0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0] : ((s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0))) | ~ drugbg(X0)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0] : (drugbg(X0) => (s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0)))), inference(rectify, [], [f31])).
fof(f31, plain, ! [X3] : (drugbg(X3) => (s3(X3) | s2(X3) | (~ qilt27(X3) & s1(X3)))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', bgcomp)).
fof(f5644, plain, (drugbg(n0) | ~ spl27_4), inference(resolution, [], [f345, f1302])).
fof(f1302, plain, ! [X0] : (~ sP2(X0) | drugbg(X0)), inference(resolution, [], [f604, f194])).
fof(f604, plain, ! [X6, X7] : (gt(X6, X7) | drugbg(X7) | ~ sP2(X6)), inference(subsumption_resolution, [], [f600, f284])).
fof(f284, plain, ! [X0, X1] : (gt(X0, X1) | drugbg(X1) | releaselg(sK22(X0))), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : (! [X1] : (drugbg(X1) | gt(X0, X1)) | (releaselg(sK22(X0)) & ~ gt(X0, sK22(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22])], [f179, f180])).
fof(f180, plain, ! [X0] : (? [X2] : (releaselg(X2) & ~ gt(X0, X2)) => (releaselg(sK22(X0)) & ~ gt(X0, sK22(X0)))), introduced(choice_axiom, [])).
fof(f179, plain, ! [X0] : (! [X1] : (drugbg(X1) | gt(X0, X1)) | ? [X2] : (releaselg(X2) & ~ gt(X0, X2))), inference(rectify, [], [f117])).
fof(f117, plain, ! [X0] : (! [X2] : (drugbg(X2) | gt(X0, X2)) | ? [X1] : (releaselg(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f77])).
fof(f77, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => ~ releaselg(X1)) => ! [X2] : (~ gt(X0, X2) => drugbg(X2))), inference(rectify, [], [f37])).
fof(f37, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)) => ! [X4] : (~ gt(X3, X4) => drugbg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', bg_completion)).
fof(f600, plain, ! [X6, X7] : (gt(X6, X7) | drugbg(X7) | ~ releaselg(sK22(X6)) | ~ sP2(X6)), inference(resolution, [], [f283, f248])).
fof(f248, plain, ! [X2, X0] : (gt(X0, X2) | ~ releaselg(X2) | ~ sP2(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X2] : (~ releaselg(X2) | gt(X0, X2))) | ~ sP2(X0)), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(nnf_transformation, [], [f129])).
fof(f283, plain, ! [X0, X1] : (~ gt(X0, sK22(X0)) | gt(X0, X1) | drugbg(X1)), inference(cnf_transformation, [], [f181])).
fof(f345, plain, (sP2(n0) | ~ spl27_4), inference(avatar_component_clause, [], [f344])).
fof(f5414, plain, (spl27_55 | spl27_55), inference(avatar_split_clause, [], [f5413, f820, f820])).
fof(f820, plain, (spl27_55 <=> ! [X20] : (conditionnormo(X20) | gt(n0, X20))), introduced(avatar_definition, [new_symbols(naming, [spl27_55])])).
fof(f5413, plain, ! [X0, X1] : (gt(n0, X0) | conditionnormo(X0) | conditionnormo(X1) | gt(n0, X1)), inference(subsumption_resolution, [], [f2545, f296])).
fof(f2545, plain, ! [X0, X1] : (gt(n0, X0) | ~ s0(n0) | conditionnormo(X0) | conditionnormo(X1) | gt(n0, X1)), inference(duplicate_literal_removal, [], [f2485])).
fof(f2485, plain, ! [X0, X1] : (gt(n0, X0) | ~ s0(n0) | conditionnormo(X0) | conditionnormo(X1) | gt(n0, X1) | ~ s0(n0)), inference(resolution, [], [f972, f285])).
fof(f285, plain, ! [X0, X3] : (~ gt(X0, sK23(X0)) | conditionnormo(X3) | gt(X0, X3) | ~ s0(X0)), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(sK23(X0), X2)) & s1(sK23(X0)) & ~ gt(X0, sK23(X0))) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK23])], [f182, f183])).
fof(f183, plain, ! [X0] : (? [X1] : (! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s1(X1) & ~ gt(X0, X1)) => (! [X2] : (conditionhyper(X2) | ~ gt(sK23(X0), X2)) & s1(sK23(X0)) & ~ gt(X0, sK23(X0)))), introduced(choice_axiom, [])).
fof(f182, plain, ! [X0] : (? [X1] : (! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s1(X1) & ~ gt(X0, X1)) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s0(X0)), inference(rectify, [], [f119])).
fof(f119, plain, ! [X0] : (? [X2] : (! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s1(X2) & ~ gt(X0, X2)) | ! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s0(X0)), inference(flattening, [], [f118])).
fof(f118, plain, ! [X0] : (? [X2] : (! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s1(X2) & ~ gt(X0, X2)) | (! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s0(X0))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : ((~ ! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) & s0(X0)) => ? [X2] : (! [X3] : (gt(X2, X3) => conditionhyper(X3)) & s1(X2) & ~ gt(X0, X2))), inference(rectify, [], [f38])).
fof(f38, plain, ! [X3] : ((~ ! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) & s0(X3)) => ? [X4] : (! [X5] : (gt(X4, X5) => conditionhyper(X5)) & s1(X4) & ~ gt(X3, X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', trans_ax1)).
fof(f972, plain, ! [X39, X38] : (gt(n0, sK23(X38)) | gt(X38, X39) | ~ s0(X38) | conditionnormo(X39)), inference(subsumption_resolution, [], [f971, f286])).
fof(f286, plain, ! [X0, X3] : (gt(X0, X3) | conditionnormo(X3) | s1(sK23(X0)) | ~ s0(X0)), inference(cnf_transformation, [], [f184])).
fof(f971, plain, ! [X39, X38] : (conditionnormo(X39) | gt(X38, X39) | ~ s0(X38) | ~ s1(sK23(X38)) | gt(n0, sK23(X38))), inference(subsumption_resolution, [], [f970, f299])).
fof(f970, plain, ! [X39, X38] : (conditionhyper(sK26(sK23(X38))) | conditionnormo(X39) | gt(X38, X39) | ~ s0(X38) | ~ s1(sK23(X38)) | gt(n0, sK23(X38))), inference(resolution, [], [f287, f298])).
fof(f298, plain, ! [X0] : (gt(X0, sK26(X0)) | ~ s1(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f287, plain, ! [X2, X0, X3] : (~ gt(sK23(X0), X2) | conditionhyper(X2) | conditionnormo(X3) | gt(X0, X3) | ~ s0(X0)), inference(cnf_transformation, [], [f184])).
fof(f5375, plain, (spl27_33 | spl27_220 | ~ spl27_55), inference(avatar_split_clause, [], [f5325, f820, f5372, f570])).
fof(f570, plain, (spl27_33 <=> ! [X12] : (gt(n0, X12) | drugi(X12))), introduced(avatar_definition, [new_symbols(naming, [spl27_33])])).
fof(f5325, plain, (! [X46] : (conditionnormo(sK20(n0)) | gt(n0, X46) | drugi(X46)) | ~ spl27_55), inference(resolution, [], [f821, f274])).
fof(f274, plain, ! [X0, X1] : (~ gt(X0, sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | ((~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0))) & (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0))))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f172, f174, f173])).
fof(f173, plain, ! [X0] : (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) => (~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0)))), introduced(choice_axiom, [])).
fof(f174, plain, ! [X0] : (? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)) => (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0)))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) & ? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X0] : (! [X3] : (drugi(X3) | gt(X0, X3)) | (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (~ uptakelg(X2) & ~ gt(X0, X2)))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ((! [X1] : (~ gt(X0, X1) => uptakepg(X1)) | ! [X2] : (~ gt(X0, X2) => uptakelg(X2))) => ! [X3] : (~ gt(X0, X3) => drugi(X3))), inference(rectify, [], [f34])).
fof(f34, plain, ! [X3] : ((! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) => ! [X4] : (~ gt(X3, X4) => drugi(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', insulin_completion)).
fof(f821, plain, (! [X20] : (gt(n0, X20) | conditionnormo(X20)) | ~ spl27_55), inference(avatar_component_clause, [], [f820])).
fof(f5360, plain, (spl27_19 | spl27_218 | spl27_1 | spl27_4 | spl27_7 | ~ spl27_55), inference(avatar_split_clause, [], [f5359, f820, f369, f344, f322, f5353, f496])).
fof(f496, plain, (spl27_19 <=> ! [X7] : (gt(n0, X7) | uptakelg(X7))), introduced(avatar_definition, [new_symbols(naming, [spl27_19])])).
fof(f5359, plain, (! [X43] : (conditionnormo(sK18(n0)) | gt(n0, X43) | uptakelg(X43)) | (spl27_1 | spl27_4 | spl27_7 | ~ spl27_55)), inference(subsumption_resolution, [], [f5358, f371])).
fof(f371, plain, (~ sP1(n0) | spl27_7), inference(avatar_component_clause, [], [f369])).
fof(f5358, plain, (! [X43] : (conditionnormo(sK18(n0)) | gt(n0, X43) | sP1(n0) | uptakelg(X43)) | (spl27_1 | spl27_4 | ~ spl27_55)), inference(subsumption_resolution, [], [f5357, f346])).
fof(f5357, plain, (! [X43] : (conditionnormo(sK18(n0)) | gt(n0, X43) | sP2(n0) | sP1(n0) | uptakelg(X43)) | (spl27_1 | ~ spl27_55)), inference(subsumption_resolution, [], [f5321, f324])).
fof(f5321, plain, (! [X43] : (conditionnormo(sK18(n0)) | gt(n0, X43) | sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X43)) | ~ spl27_55), inference(resolution, [], [f821, f256])).
fof(f256, plain, ! [X0, X3] : (~ gt(X0, sK18(X0)) | gt(X0, X3) | sP3(X0) | sP2(X0) | sP1(X0) | uptakelg(X3)), inference(cnf_transformation, [], [f171])).
fof(f4918, plain, (spl27_33 | ~ spl27_19 | ~ spl27_120), inference(avatar_split_clause, [], [f4917, f1984, f496, f570])).
fof(f4917, plain, (! [X44] : (gt(n0, X44) | drugi(X44)) | (~ spl27_19 | ~ spl27_120)), inference(subsumption_resolution, [], [f4905, f2558])).
fof(f2558, plain, (~ uptakelg(sK20(n0)) | ~ spl27_120), inference(resolution, [], [f1985, f311])).
fof(f311, plain, ! [X0] : (~ releaselg(X0) | ~ uptakelg(X0)), inference(resolution, [], [f208, f194])).
fof(f208, plain, ! [X0, X1] : (gt(X0, X1) | ~ uptakelg(X1) | ~ releaselg(X1)), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : (~ releaselg(X1) | ~ uptakelg(X1) | gt(X0, X1)), inference(flattening, [], [f86])).
fof(f86, plain, ! [X0, X1] : ((~ releaselg(X1) | ~ uptakelg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1] : (~ gt(X0, X1) => (uptakelg(X1) => ~ releaselg(X1))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X3, X4] : (~ gt(X3, X4) => (uptakelg(X4) => ~ releaselg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', liver_glucose)).
fof(f4905, plain, (! [X44] : (uptakelg(sK20(n0)) | gt(n0, X44) | drugi(X44)) | ~ spl27_19), inference(resolution, [], [f497, f274])).
fof(f497, plain, (! [X7] : (gt(n0, X7) | uptakelg(X7)) | ~ spl27_19), inference(avatar_component_clause, [], [f496])).
fof(f4795, plain, (spl27_33 | ~ spl27_16), inference(avatar_split_clause, [], [f4794, f484, f570])).
fof(f4794, plain, (! [X43] : (gt(n0, X43) | drugi(X43)) | ~ spl27_16), inference(subsumption_resolution, [], [f4777, f277])).
fof(f277, plain, ! [X0, X1] : (~ uptakepg(sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f4777, plain, (! [X43] : (uptakepg(sK19(n0)) | gt(n0, X43) | drugi(X43)) | ~ spl27_16), inference(resolution, [], [f485, f276])).
fof(f276, plain, ! [X0, X1] : (~ gt(X0, sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f485, plain, (! [X6] : (gt(n0, X6) | uptakepg(X6)) | ~ spl27_16), inference(avatar_component_clause, [], [f484])).
fof(f4067, plain, ~ spl27_33, inference(avatar_contradiction_clause, [], [f4066])).
fof(f4066, plain, ($false | ~ spl27_33), inference(subsumption_resolution, [], [f4065, f296])).
fof(f4065, plain, (~ s0(n0) | ~ spl27_33), inference(resolution, [], [f4003, f240])).
fof(f4003, plain, (s3(n0) | ~ spl27_33), inference(resolution, [], [f3883, f273])).
fof(f273, plain, ! [X0] : (~ drugi(X0) | s3(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : (s3(X0) | ~ drugi(X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (drugi(X0) => s3(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X3] : (drugi(X3) => s3(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', insulincomp)).
fof(f3883, plain, (drugi(n0) | ~ spl27_33), inference(resolution, [], [f571, f194])).
fof(f571, plain, (! [X12] : (gt(n0, X12) | drugi(X12)) | ~ spl27_33), inference(avatar_component_clause, [], [f570])).
fof(f2484, plain, (spl27_33 | spl27_120), inference(avatar_split_clause, [], [f2483, f1984, f570])).
fof(f2483, plain, (! [X0] : (gt(n0, X0) | drugi(X0)) | spl27_120), inference(resolution, [], [f2281, f275])).
fof(f275, plain, ! [X0, X1] : (~ uptakelg(sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f2281, plain, (uptakelg(sK20(n0)) | spl27_120), inference(resolution, [], [f1986, f388])).
fof(f388, plain, ! [X0] : (releaselg(X0) | uptakelg(X0)), inference(resolution, [], [f278, f194])).
fof(f278, plain, ! [X0, X1] : (gt(X0, X1) | releaselg(X1) | uptakelg(X1)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : (uptakelg(X1) | releaselg(X1) | gt(X0, X1)), inference(flattening, [], [f114])).
fof(f114, plain, ! [X0, X1] : ((uptakelg(X1) | releaselg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1] : (~ gt(X0, X1) => (~ releaselg(X1) => uptakelg(X1))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X3, X4] : (~ gt(X3, X4) => (~ releaselg(X4) => uptakelg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED005+1.p', uptake_completion)).
fof(f1986, plain, (~ releaselg(sK20(n0)) | spl27_120), inference(avatar_component_clause, [], [f1984])).
fof(f449, plain, (spl27_14 | ~ spl27_15), inference(avatar_split_clause, [], [f440, f446, f442])).
fof(f440, plain, (~ s1(n0) | conditionhyper(sK26(n0))), inference(subsumption_resolution, [], [f436, f194])).
fof(f436, plain, (~ s1(n0) | gt(n0, n0) | conditionhyper(sK26(n0))), inference(resolution, [], [f298, f297])).