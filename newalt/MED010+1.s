fof(f5292, plain, $false, inference(avatar_sat_refutation, [], [f2397, f2476, f3062, f3069, f3997, f4001, f5127, f5169, f5270, f5272, f5291])).
fof(f5291, plain, ~ spl27_7, inference(avatar_contradiction_clause, [], [f5290])).
fof(f5290, plain, ($false | ~ spl27_7), inference(subsumption_resolution, [], [f5278, f298])).
fof(f298, plain, bcapacityex(n0), inference(cnf_transformation, [], [f193])).
fof(f193, plain, (! [X0] : (~ bcapacityex(X0) | (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0))) | ~ s3(X0) | gt(n0, X0)) & bcapacityex(n0) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s2(n0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK26])], [f191, f192])).
fof(f192, plain, ! [X0] : (? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) => (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0)))), introduced(choice_axiom, [])).
fof(f191, plain, (! [X0] : (~ bcapacityex(X0) | ? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ~ s3(X0) | gt(n0, X0)) & bcapacityex(n0) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s2(n0)), inference(rectify, [], [f125])).
fof(f125, plain, (! [X1] : (~ bcapacityex(X1) | ? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s3(X1) | gt(n0, X1)) & bcapacityex(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s2(n0)), inference(flattening, [], [f124])).
fof(f124, plain, (! [X1] : (~ bcapacityex(X1) | ? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s3(X1) | gt(n0, X1)) & (bcapacityex(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s2(n0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ~ ((bcapacityex(n0) & ! [X0] : (gt(n0, X0) => conditionhyper(X0)) & s2(n0)) => ? [X1] : (bcapacityex(X1) & ! [X2] : (gt(X1, X2) => conditionhyper(X2)) & s3(X1) & ~ gt(n0, X1))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((bcapacityex(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s2(n0)) => ? [X3] : (bcapacityex(X3) & ! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s3(X3) & ~ gt(n0, X3))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((bcapacityex(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s2(n0)) => ? [X3] : (bcapacityex(X3) & ! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s3(X3) & ~ gt(n0, X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', unsuccesfuls2)).
fof(f5278, plain, (~ bcapacityex(n0) | ~ spl27_7), inference(resolution, [], [f376, f383])).
fof(f383, plain, ! [X8] : (~ sP1(X8) | ~ bcapacityex(X8)), inference(subsumption_resolution, [], [f373, f282])).
fof(f282, plain, ! [X0] : (~ bsecretioni(sK21(X0)) | ~ bcapacityex(X0)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X1] : (drugsu(X1) | gt(X0, X1))) | (~ bsecretioni(sK21(X0)) & ~ gt(X0, sK21(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK21])], [f176, f177])).
fof(f177, plain, ! [X0] : (? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2)) => (~ bsecretioni(sK21(X0)) & ~ gt(X0, sK21(X0)))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X1] : (drugsu(X1) | gt(X0, X1))) | ? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2))), inference(rectify, [], [f116])).
fof(f116, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X2] : (drugsu(X2) | gt(X0, X2))) | ? [X1] : (~ bsecretioni(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => bsecretioni(X1)) => (~ bcapacityex(X0) & ! [X2] : (~ gt(X0, X2) => drugsu(X2)))), inference(rectify, [], [f36])).
fof(f36, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) => (~ bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => drugsu(X4)))), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', su_completion)).
fof(f373, plain, ! [X8] : (bsecretioni(sK21(X8)) | ~ sP1(X8) | ~ bcapacityex(X8)), inference(resolution, [], [f252, f281])).
fof(f281, plain, ! [X0] : (~ gt(X0, sK21(X0)) | ~ bcapacityex(X0)), inference(cnf_transformation, [], [f178])).
fof(f252, plain, ! [X2, X0] : (gt(X0, X2) | bsecretioni(X2) | ~ sP1(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & qilt27(X0) & bcapacitysn(X0) & ! [X2] : (bsecretioni(X2) | gt(X0, X2))) | ~ sP1(X0)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(nnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(usedef, [], [e128])).
fof(e128, plain, ! [X0] : (sP1(X0) <=> (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f376, plain, (sP1(n0) | ~ spl27_7), inference(avatar_component_clause, [], [f375])).
fof(f375, plain, (spl27_7 <=> sP1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_7])])).
fof(f5272, plain, (spl27_7 | spl27_1 | spl27_33 | spl27_4 | ~ spl27_110), inference(avatar_split_clause, [], [f5271, f2469, f350, f1429, f328, f375])).
fof(f328, plain, (spl27_1 <=> sP3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_1])])).
fof(f1429, plain, (spl27_33 <=> ! [X7] : (gt(n0, X7) | uptakelg(X7))), introduced(avatar_definition, [new_symbols(naming, [spl27_33])])).
fof(f350, plain, (spl27_4 <=> sP2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_4])])).
fof(f2469, plain, (spl27_110 <=> conditionnormo(sK18(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_110])])).
fof(f5271, plain, (! [X1] : (gt(n0, X1) | sP3(n0) | sP1(n0) | uptakelg(X1)) | (spl27_4 | ~ spl27_110)), inference(subsumption_resolution, [], [f4064, f352])).
fof(f352, plain, (~ sP2(n0) | spl27_4), inference(avatar_component_clause, [], [f350])).
fof(f4064, plain, (! [X1] : (gt(n0, X1) | sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X1)) | ~ spl27_110), inference(resolution, [], [f2471, f257])).
fof(f257, plain, ! [X0, X3] : (~ conditionnormo(sK18(X0)) | gt(X0, X3) | sP3(X0) | sP2(X0) | sP1(X0) | uptakelg(X3)), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & bcapacityex(X0) & ! [X2] : (uptakepg(X2) | gt(X0, X2)) & ! [X3] : (uptakelg(X3) | gt(X0, X3))) | sP3(X0) | sP2(X0) | sP1(X0) | (~ conditionnormo(sK18(X0)) & ~ gt(X0, sK18(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f169, f170])).
fof(f170, plain, ! [X0] : (? [X4] : (~ conditionnormo(X4) & ~ gt(X0, X4)) => (~ conditionnormo(sK18(X0)) & ~ gt(X0, sK18(X0)))), introduced(choice_axiom, [])).
fof(f169, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & bcapacityex(X0) & ! [X2] : (uptakepg(X2) | gt(X0, X2)) & ! [X3] : (uptakelg(X3) | gt(X0, X3))) | sP3(X0) | sP2(X0) | sP1(X0) | ? [X4] : (~ conditionnormo(X4) & ~ gt(X0, X4))), inference(rectify, [], [f131])).
fof(f131, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | sP3(X0) | sP2(X0) | sP1(X0) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(definition_folding, [], [f100, e130, e129, e128])).
fof(f129, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(usedef, [], [e129])).
fof(e129, plain, ! [X0] : (sP2(X0) <=> (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f130, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(usedef, [], [e130])).
fof(e130, plain, ! [X0] : (sP3(X0) <=> (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f100, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(flattening, [], [f99])).
fof(f99, plain, ! [X0] : (((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12)))) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) => ((! [X2] : (gt(X0, X2) => conditionhyper(X2)) & bcapacityex(X0) & ! [X3] : (~ gt(X0, X3) => uptakepg(X3)) & ! [X4] : (~ gt(X0, X4) => uptakelg(X4))) | (! [X5] : (gt(X0, X5) => conditionhyper(X5)) & ! [X6] : (~ gt(X0, X6) => bsecretioni(X6)) & bcapacityne(X0) & (! [X7] : (~ gt(X0, X7) => uptakepg(X7)) | ! [X8] : (~ gt(X0, X8) => ~ releaselg(X8)))) | (! [X9] : (gt(X0, X9) => conditionhyper(X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ gt(X0, X10) => ~ releaselg(X10))) | (! [X11] : (gt(X0, X11) => conditionhyper(X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (~ gt(X0, X12) => bsecretioni(X12))))), inference(rectify, [], [f26])).
fof(f26, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) => ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => uptakepg(X4)) & ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ~ qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))))), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', normo)).
fof(f2471, plain, (conditionnormo(sK18(n0)) | ~ spl27_110), inference(avatar_component_clause, [], [f2469])).
fof(f5270, plain, ~ spl27_1, inference(avatar_contradiction_clause, [], [f5269])).
fof(f5269, plain, ($false | ~ spl27_1), inference(subsumption_resolution, [], [f5266, f308])).
fof(f308, plain, ~ bcapacityne(n0), inference(resolution, [], [f197, f298])).
fof(f197, plain, ! [X0] : (~ bcapacityex(X0) | ~ bcapacityne(X0)), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (~ bcapacityex(X0) | ~ bcapacityne(X0)), inference(rectify, [], [f4])).
fof(f4, plain, ! [X3] : (~ bcapacityex(X3) | ~ bcapacityne(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', xorcapacity2)).
fof(f5266, plain, (bcapacityne(n0) | ~ spl27_1), inference(resolution, [], [f329, f245])).
fof(f245, plain, ! [X0] : (~ sP3(X0) | bcapacityne(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ! [X2] : (bsecretioni(X2) | gt(X0, X2)) & bcapacityne(X0) & (! [X3] : (uptakepg(X3) | gt(X0, X3)) | ! [X4] : (~ releaselg(X4) | gt(X0, X4)))) | ~ sP3(X0)), inference(rectify, [], [f163])).
fof(f163, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(nnf_transformation, [], [f130])).
fof(f329, plain, (sP3(n0) | ~ spl27_1), inference(avatar_component_clause, [], [f328])).
fof(f5169, plain, (spl27_59 | ~ spl27_33), inference(avatar_split_clause, [], [f5168, f1429, f1561])).
fof(f1561, plain, (spl27_59 <=> ! [X25] : (gt(n0, X25) | drugi(X25))), introduced(avatar_definition, [new_symbols(naming, [spl27_59])])).
fof(f5168, plain, (! [X42] : (gt(n0, X42) | drugi(X42)) | ~ spl27_33), inference(subsumption_resolution, [], [f5009, f275])).
fof(f275, plain, ! [X0, X1] : (~ uptakelg(sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | ((~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0))) & (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0))))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f172, f174, f173])).
fof(f173, plain, ! [X0] : (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) => (~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0)))), introduced(choice_axiom, [])).
fof(f174, plain, ! [X0] : (? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)) => (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0)))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) & ? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X0] : (! [X3] : (drugi(X3) | gt(X0, X3)) | (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (~ uptakelg(X2) & ~ gt(X0, X2)))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ((! [X1] : (~ gt(X0, X1) => uptakepg(X1)) | ! [X2] : (~ gt(X0, X2) => uptakelg(X2))) => ! [X3] : (~ gt(X0, X3) => drugi(X3))), inference(rectify, [], [f34])).
fof(f34, plain, ! [X3] : ((! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) => ! [X4] : (~ gt(X3, X4) => drugi(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', insulin_completion)).
fof(f5009, plain, (! [X42] : (uptakelg(sK20(n0)) | gt(n0, X42) | drugi(X42)) | ~ spl27_33), inference(resolution, [], [f1430, f274])).
fof(f274, plain, ! [X0, X1] : (~ gt(X0, sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f1430, plain, (! [X7] : (gt(n0, X7) | uptakelg(X7)) | ~ spl27_33), inference(avatar_component_clause, [], [f1429])).
fof(f5127, plain, (spl27_133 | ~ spl27_59), inference(avatar_split_clause, [], [f5080, f1561, f2803])).
fof(f2803, plain, (spl27_133 <=> ! [X1] : (drugi(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_133])])).
fof(f5080, plain, (! [X0] : (drugi(X0) | conditionhyper(X0)) | ~ spl27_59), inference(resolution, [], [f1562, f297])).
fof(f297, plain, ! [X2] : (~ gt(n0, X2) | conditionhyper(X2)), inference(cnf_transformation, [], [f193])).
fof(f1562, plain, (! [X25] : (gt(n0, X25) | drugi(X25)) | ~ spl27_59), inference(avatar_component_clause, [], [f1561])).
fof(f4001, plain, ~ spl27_21, inference(avatar_contradiction_clause, [], [f4000])).
fof(f4000, plain, ($false | ~ spl27_21), inference(subsumption_resolution, [], [f3998, f298])).
fof(f3998, plain, (~ bcapacityex(n0) | ~ spl27_21), inference(resolution, [], [f1003, f199])).
fof(f199, plain, ! [X0] : (~ bcapacitysn(X0) | ~ bcapacityex(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0] : (~ bcapacitysn(X0) | ~ bcapacityex(X0)), inference(rectify, [], [f6])).
fof(f6, plain, ! [X3] : (~ bcapacitysn(X3) | ~ bcapacityex(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', xorcapacity4)).
fof(f1003, plain, (bcapacitysn(n0) | ~ spl27_21), inference(avatar_component_clause, [], [f1002])).
fof(f1002, plain, (spl27_21 <=> bcapacitysn(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_21])])).
fof(f3997, plain, (spl27_21 | ~ spl27_4), inference(avatar_split_clause, [], [f3986, f350, f1002])).
fof(f3986, plain, (bcapacitysn(n0) | ~ spl27_4), inference(resolution, [], [f351, f249])).
fof(f249, plain, ! [X0] : (~ sP2(X0) | bcapacitysn(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X2] : (~ releaselg(X2) | gt(X0, X2))) | ~ sP2(X0)), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(nnf_transformation, [], [f129])).
fof(f351, plain, (sP2(n0) | ~ spl27_4), inference(avatar_component_clause, [], [f350])).
fof(f3069, plain, ~ spl27_19, inference(avatar_contradiction_clause, [], [f3068])).
fof(f3068, plain, ($false | ~ spl27_19), inference(subsumption_resolution, [], [f3065, f296])).
fof(f296, plain, s2(n0), inference(cnf_transformation, [], [f193])).
fof(f3065, plain, (~ s2(n0) | ~ spl27_19), inference(resolution, [], [f644, f243])).
fof(f243, plain, ! [X0] : (~ s3(X0) | ~ s2(X0)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (~ s3(X0) | ~ s2(X0)), inference(rectify, [], [f25])).
fof(f25, plain, ! [X3] : (~ s3(X3) | ~ s2(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', xorstep7)).
fof(f644, plain, (s3(n0) | ~ spl27_19), inference(avatar_component_clause, [], [f643])).
fof(f643, plain, (spl27_19 <=> s3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_19])])).
fof(f3062, plain, (spl27_19 | ~ spl27_23 | ~ spl27_133), inference(avatar_contradiction_clause, [], [f3061])).
fof(f3061, plain, ($false | (spl27_19 | ~ spl27_23 | ~ spl27_133)), inference(subsumption_resolution, [], [f3058, f2495])).
fof(f2495, plain, (~ conditionhyper(n0) | ~ spl27_23), inference(resolution, [], [f2418, f202])).
fof(f202, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (~ conditionnormo(X3) | ~ conditionhyper(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', xorcondition3)).
fof(f2418, plain, (conditionnormo(n0) | ~ spl27_23), inference(resolution, [], [f1011, f194])).
fof(f194, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', irreflexivity_gt)).
fof(f1011, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0)) | ~ spl27_23), inference(avatar_component_clause, [], [f1010])).
fof(f1010, plain, (spl27_23 <=> ! [X0] : (gt(n0, X0) | conditionnormo(X0))), introduced(avatar_definition, [new_symbols(naming, [spl27_23])])).
fof(f3058, plain, (conditionhyper(n0) | (spl27_19 | ~ spl27_133)), inference(resolution, [], [f3035, f645])).
fof(f645, plain, (~ s3(n0) | spl27_19), inference(avatar_component_clause, [], [f643])).
fof(f3035, plain, (! [X0] : (s3(X0) | conditionhyper(X0)) | ~ spl27_133), inference(resolution, [], [f2804, f273])).
fof(f273, plain, ! [X0] : (~ drugi(X0) | s3(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : (s3(X0) | ~ drugi(X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (drugi(X0) => s3(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X3] : (drugi(X3) => s3(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', insulincomp)).
fof(f2804, plain, (! [X1] : (drugi(X1) | conditionhyper(X1)) | ~ spl27_133), inference(avatar_component_clause, [], [f2803])).
fof(f2476, plain, (spl27_33 | spl27_110 | spl27_1 | spl27_4 | spl27_7 | ~ spl27_23), inference(avatar_split_clause, [], [f2475, f1010, f375, f350, f328, f2469, f1429])).
fof(f2475, plain, (! [X31] : (conditionnormo(sK18(n0)) | gt(n0, X31) | uptakelg(X31)) | (spl27_1 | spl27_4 | spl27_7 | ~ spl27_23)), inference(subsumption_resolution, [], [f2474, f377])).
fof(f377, plain, (~ sP1(n0) | spl27_7), inference(avatar_component_clause, [], [f375])).
fof(f2474, plain, (! [X31] : (conditionnormo(sK18(n0)) | gt(n0, X31) | sP1(n0) | uptakelg(X31)) | (spl27_1 | spl27_4 | ~ spl27_23)), inference(subsumption_resolution, [], [f2473, f352])).
fof(f2473, plain, (! [X31] : (conditionnormo(sK18(n0)) | gt(n0, X31) | sP2(n0) | sP1(n0) | uptakelg(X31)) | (spl27_1 | ~ spl27_23)), inference(subsumption_resolution, [], [f2448, f330])).
fof(f330, plain, (~ sP3(n0) | spl27_1), inference(avatar_component_clause, [], [f328])).
fof(f2448, plain, (! [X31] : (conditionnormo(sK18(n0)) | gt(n0, X31) | sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X31)) | ~ spl27_23), inference(resolution, [], [f1011, f256])).
fof(f256, plain, ! [X0, X3] : (~ gt(X0, sK18(X0)) | gt(X0, X3) | sP3(X0) | sP2(X0) | sP1(X0) | uptakelg(X3)), inference(cnf_transformation, [], [f171])).
fof(f2397, plain, (spl27_23 | spl27_23), inference(avatar_split_clause, [], [f2396, f1010, f1010])).
fof(f2396, plain, ! [X0, X1] : (gt(n0, X0) | conditionnormo(X0) | conditionnormo(X1) | gt(n0, X1)), inference(subsumption_resolution, [], [f2395, f296])).
fof(f2395, plain, ! [X0, X1] : (gt(n0, X0) | ~ s2(n0) | conditionnormo(X0) | conditionnormo(X1) | gt(n0, X1)), inference(duplicate_literal_removal, [], [f2334])).
fof(f2334, plain, ! [X0, X1] : (gt(n0, X0) | ~ s2(n0) | conditionnormo(X0) | conditionnormo(X1) | gt(n0, X1) | ~ s2(n0)), inference(resolution, [], [f872, f292])).
fof(f292, plain, ! [X0, X3] : (~ gt(X0, sK25(X0)) | conditionnormo(X3) | gt(X0, X3) | ~ s2(X0)), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ! [X0] : ((bcapacityex(sK25(X0)) & ! [X2] : (conditionhyper(X2) | ~ gt(sK25(X0), X2)) & s3(sK25(X0)) & ~ gt(X0, sK25(X0))) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s2(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK25])], [f188, f189])).
fof(f189, plain, ! [X0] : (? [X1] : (bcapacityex(X1) & ! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s3(X1) & ~ gt(X0, X1)) => (bcapacityex(sK25(X0)) & ! [X2] : (conditionhyper(X2) | ~ gt(sK25(X0), X2)) & s3(sK25(X0)) & ~ gt(X0, sK25(X0)))), introduced(choice_axiom, [])).
fof(f188, plain, ! [X0] : (? [X1] : (bcapacityex(X1) & ! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s3(X1) & ~ gt(X0, X1)) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s2(X0)), inference(rectify, [], [f123])).
fof(f123, plain, ! [X0] : (? [X2] : (bcapacityex(X2) & ! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s3(X2) & ~ gt(X0, X2)) | ! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s2(X0)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0] : (? [X2] : (bcapacityex(X2) & ! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s3(X2) & ~ gt(X0, X2)) | (! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s2(X0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : ((~ ! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) & s2(X0)) => ? [X2] : (bcapacityex(X2) & ! [X3] : (gt(X2, X3) => conditionhyper(X3)) & s3(X2) & ~ gt(X0, X2))), inference(rectify, [], [f40])).
fof(f40, plain, ! [X3] : ((~ ! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) & s2(X3)) => ? [X4] : (bcapacityex(X4) & ! [X5] : (gt(X4, X5) => conditionhyper(X5)) & s3(X4) & ~ gt(X3, X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED010+1.p', trans_ax3)).
fof(f872, plain, ! [X47, X48] : (gt(n0, sK25(X47)) | gt(X47, X48) | ~ s2(X47) | conditionnormo(X48)), inference(subsumption_resolution, [], [f871, f293])).
fof(f293, plain, ! [X0, X3] : (gt(X0, X3) | conditionnormo(X3) | s3(sK25(X0)) | ~ s2(X0)), inference(cnf_transformation, [], [f190])).
fof(f871, plain, ! [X47, X48] : (conditionnormo(X48) | gt(X47, X48) | ~ s2(X47) | ~ s3(sK25(X47)) | gt(n0, sK25(X47))), inference(subsumption_resolution, [], [f870, f295])).
fof(f295, plain, ! [X0, X3] : (gt(X0, X3) | conditionnormo(X3) | bcapacityex(sK25(X0)) | ~ s2(X0)), inference(cnf_transformation, [], [f190])).
fof(f870, plain, ! [X47, X48] : (conditionnormo(X48) | gt(X47, X48) | ~ s2(X47) | ~ bcapacityex(sK25(X47)) | ~ s3(sK25(X47)) | gt(n0, sK25(X47))), inference(subsumption_resolution, [], [f869, f300])).
fof(f300, plain, ! [X0] : (~ conditionhyper(sK26(X0)) | ~ bcapacityex(X0) | ~ s3(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f869, plain, ! [X47, X48] : (conditionhyper(sK26(sK25(X47))) | conditionnormo(X48) | gt(X47, X48) | ~ s2(X47) | ~ bcapacityex(sK25(X47)) | ~ s3(sK25(X47)) | gt(n0, sK25(X47))), inference(resolution, [], [f294, f299])).
fof(f299, plain, ! [X0] : (gt(X0, sK26(X0)) | ~ bcapacityex(X0) | ~ s3(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f294, plain, ! [X2, X0, X3] : (~ gt(sK25(X0), X2) | conditionhyper(X2) | conditionnormo(X3) | gt(X0, X3) | ~ s2(X0)), inference(cnf_transformation, [], [f190])).