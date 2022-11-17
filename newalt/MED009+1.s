fof(f17548, plain, $false, inference(avatar_sat_refutation, [], [f7419, f7524, f7549, f7554, f7566, f8311, f11371, f12240, f12571, f12605, f12613, f12625, f12641, f12661, f12897, f13002, f14867, f15112, f15629, f16886, f17060, f17278, f17447])).
fof(f17447, plain, (spl27_746 | spl27_3 | spl27_6 | spl27_14 | spl27_75 | spl27_519), inference(avatar_split_clause, [], [f17446, f11374, f1726, f513, f365, f342, f12900])).
fof(f12900, plain, (spl27_746 <=> ! [X219] : (conditionhyper(X219) | uptakelg(X219))), introduced(avatar_definition, [new_symbols(naming, [spl27_746])])).
fof(f342, plain, (spl27_3 <=> sP3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_3])])).
fof(f365, plain, (spl27_6 <=> sP2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_6])])).
fof(f513, plain, (spl27_14 <=> sP1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_14])])).
fof(f1726, plain, (spl27_75 <=> bcapacityne(sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_75])])).
fof(f11374, plain, (spl27_519 <=> bcapacityex(sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_519])])).
fof(f17446, plain, (! [X1] : (uptakelg(X1) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_14 | spl27_75 | spl27_519)), inference(subsumption_resolution, [], [f17445, f296])).
fof(f296, plain, s1(n0), inference(cnf_transformation, [], [f193])).
fof(f193, plain, (! [X0] : ((~ bcapacityex(X0) & ~ bcapacityne(X0)) | (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0))) | ~ s2(X0) | gt(n0, X0)) & ~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s1(n0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK26])], [f191, f192])).
fof(f192, plain, ! [X0] : (? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) => (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0)))), introduced(choice_axiom, [])).
fof(f191, plain, (! [X0] : ((~ bcapacityex(X0) & ~ bcapacityne(X0)) | ? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ~ s2(X0) | gt(n0, X0)) & ~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s1(n0)), inference(rectify, [], [f125])).
fof(f125, plain, (! [X1] : ((~ bcapacityex(X1) & ~ bcapacityne(X1)) | ? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s2(X1) | gt(n0, X1)) & ~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0)), inference(flattening, [], [f124])).
fof(f124, plain, (! [X1] : ((~ bcapacityex(X1) & ~ bcapacityne(X1)) | ? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s2(X1) | gt(n0, X1)) & (~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ~ ((~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (gt(n0, X0) => conditionhyper(X0)) & s1(n0)) => ? [X1] : ((bcapacityex(X1) | bcapacityne(X1)) & ! [X2] : (gt(X1, X2) => conditionhyper(X2)) & s2(X1) & ~ gt(n0, X1))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ? [X3] : ((bcapacityex(X3) | bcapacityne(X3)) & ! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s2(X3) & ~ gt(n0, X3))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ? [X3] : ((bcapacityex(X3) | bcapacityne(X3)) & ! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s2(X3) & ~ gt(n0, X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', transs1s2_qige27)).
fof(f17445, plain, (! [X1] : (uptakelg(X1) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_14 | spl27_75 | spl27_519)), inference(subsumption_resolution, [], [f17444, f11375])).
fof(f11375, plain, (~ bcapacityex(sK24(n0)) | spl27_519), inference(avatar_component_clause, [], [f11374])).
fof(f17444, plain, (! [X1] : (uptakelg(X1) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_14 | spl27_75)), inference(subsumption_resolution, [], [f17443, f1728])).
fof(f1728, plain, (~ bcapacityne(sK24(n0)) | spl27_75), inference(avatar_component_clause, [], [f1726])).
fof(f17443, plain, (! [X1] : (uptakelg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_14)), inference(subsumption_resolution, [], [f17442, f515])).
fof(f515, plain, (~ sP1(n0) | spl27_14), inference(avatar_component_clause, [], [f513])).
fof(f17442, plain, (! [X1] : (sP1(n0) | uptakelg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6)), inference(subsumption_resolution, [], [f17441, f367])).
fof(f367, plain, (~ sP2(n0) | spl27_6), inference(avatar_component_clause, [], [f365])).
fof(f17441, plain, (! [X1] : (sP2(n0) | sP1(n0) | uptakelg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | spl27_3), inference(subsumption_resolution, [], [f15128, f344])).
fof(f344, plain, (~ sP3(n0) | spl27_3), inference(avatar_component_clause, [], [f342])).
fof(f15128, plain, ! [X1] : (sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)), inference(resolution, [], [f972, f297])).
fof(f297, plain, ! [X2] : (~ gt(n0, X2) | conditionhyper(X2)), inference(cnf_transformation, [], [f193])).
fof(f972, plain, ! [X23, X22] : (gt(X22, X23) | sP3(X22) | sP2(X22) | sP1(X22) | uptakelg(X23) | bcapacityne(sK24(X22)) | bcapacityex(sK24(X22)) | ~ s1(X22)), inference(subsumption_resolution, [], [f964, f257])).
fof(f257, plain, ! [X0, X3] : (~ conditionnormo(sK18(X0)) | gt(X0, X3) | sP3(X0) | sP2(X0) | sP1(X0) | uptakelg(X3)), inference(cnf_transformation, [], [f171])).
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
fof(f26, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) => ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => uptakepg(X4)) & ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ~ qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))))), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', normo)).
fof(f964, plain, ! [X23, X22] : (gt(X22, X23) | sP3(X22) | sP2(X22) | sP1(X22) | uptakelg(X23) | bcapacityne(sK24(X22)) | conditionnormo(sK18(X22)) | bcapacityex(sK24(X22)) | ~ s1(X22)), inference(resolution, [], [f256, f291])).
fof(f291, plain, ! [X0, X3] : (gt(X0, X3) | bcapacityne(sK24(X0)) | conditionnormo(X3) | bcapacityex(sK24(X0)) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ! [X0] : (((bcapacityex(sK24(X0)) | bcapacityne(sK24(X0))) & ! [X2] : (conditionhyper(X2) | ~ gt(sK24(X0), X2)) & s2(sK24(X0)) & ~ gt(X0, sK24(X0))) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK24])], [f185, f186])).
fof(f186, plain, ! [X0] : (? [X1] : ((bcapacityex(X1) | bcapacityne(X1)) & ! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s2(X1) & ~ gt(X0, X1)) => ((bcapacityex(sK24(X0)) | bcapacityne(sK24(X0))) & ! [X2] : (conditionhyper(X2) | ~ gt(sK24(X0), X2)) & s2(sK24(X0)) & ~ gt(X0, sK24(X0)))), introduced(choice_axiom, [])).
fof(f185, plain, ! [X0] : (? [X1] : ((bcapacityex(X1) | bcapacityne(X1)) & ! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s2(X1) & ~ gt(X0, X1)) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s1(X0)), inference(rectify, [], [f121])).
fof(f121, plain, ! [X0] : (? [X2] : ((bcapacityex(X2) | bcapacityne(X2)) & ! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s2(X2) & ~ gt(X0, X2)) | ! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s1(X0)), inference(flattening, [], [f120])).
fof(f120, plain, ! [X0] : (? [X2] : ((bcapacityex(X2) | bcapacityne(X2)) & ! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s2(X2) & ~ gt(X0, X2)) | (! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s1(X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((~ ! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) & s1(X0)) => ? [X2] : ((bcapacityex(X2) | bcapacityne(X2)) & ! [X3] : (gt(X2, X3) => conditionhyper(X3)) & s2(X2) & ~ gt(X0, X2))), inference(rectify, [], [f39])).
fof(f39, plain, ! [X3] : ((~ ! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) & s1(X3)) => ? [X4] : ((bcapacityex(X4) | bcapacityne(X4)) & ! [X5] : (gt(X4, X5) => conditionhyper(X5)) & s2(X4) & ~ gt(X3, X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', trans_ax2)).
fof(f256, plain, ! [X0, X3] : (~ gt(X0, sK18(X0)) | gt(X0, X3) | sP3(X0) | sP2(X0) | sP1(X0) | uptakelg(X3)), inference(cnf_transformation, [], [f171])).
fof(f17278, plain, (~ spl27_279 | ~ spl27_723 | ~ spl27_746), inference(avatar_contradiction_clause, [], [f17277])).
fof(f17277, plain, ($false | (~ spl27_279 | ~ spl27_723 | ~ spl27_746)), inference(subsumption_resolution, [], [f17276, f14836])).
fof(f14836, plain, (conditionhyper(sK20(n0)) | (~ spl27_279 | ~ spl27_746)), inference(resolution, [], [f12901, f12894])).
fof(f12894, plain, (~ uptakelg(sK20(n0)) | ~ spl27_279), inference(resolution, [], [f7162, f331])).
fof(f331, plain, ! [X0] : (~ releaselg(X0) | ~ uptakelg(X0)), inference(resolution, [], [f208, f194])).
fof(f194, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', irreflexivity_gt)).
fof(f208, plain, ! [X0, X1] : (gt(X0, X1) | ~ uptakelg(X1) | ~ releaselg(X1)), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : (~ releaselg(X1) | ~ uptakelg(X1) | gt(X0, X1)), inference(flattening, [], [f86])).
fof(f86, plain, ! [X0, X1] : ((~ releaselg(X1) | ~ uptakelg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1] : (~ gt(X0, X1) => (uptakelg(X1) => ~ releaselg(X1))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X3, X4] : (~ gt(X3, X4) => (uptakelg(X4) => ~ releaselg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', liver_glucose)).
fof(f7162, plain, (releaselg(sK20(n0)) | ~ spl27_279), inference(avatar_component_clause, [], [f7161])).
fof(f7161, plain, (spl27_279 <=> releaselg(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_279])])).
fof(f12901, plain, (! [X219] : (uptakelg(X219) | conditionhyper(X219)) | ~ spl27_746), inference(avatar_component_clause, [], [f12900])).
fof(f17276, plain, (~ conditionhyper(sK20(n0)) | ~ spl27_723), inference(resolution, [], [f12239, f202])).
fof(f202, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (~ conditionnormo(X3) | ~ conditionhyper(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', xorcondition3)).
fof(f12239, plain, (conditionnormo(sK20(n0)) | ~ spl27_723), inference(avatar_component_clause, [], [f12237])).
fof(f12237, plain, (spl27_723 <=> conditionnormo(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_723])])).
fof(f17060, plain, (spl27_46 | ~ spl27_519 | spl27_745), inference(avatar_split_clause, [], [f17059, f12658, f11374, f1587])).
fof(f1587, plain, (spl27_46 <=> ! [X16] : (conditionnormo(X16) | gt(n0, X16))), introduced(avatar_definition, [new_symbols(naming, [spl27_46])])).
fof(f12658, plain, (spl27_745 <=> gt(n0, sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_745])])).
fof(f17059, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0)) | (~ spl27_519 | spl27_745)), inference(subsumption_resolution, [], [f17058, f12659])).
fof(f12659, plain, (~ gt(n0, sK24(n0)) | spl27_745), inference(avatar_component_clause, [], [f12658])).
fof(f17058, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_519), inference(subsumption_resolution, [], [f17056, f296])).
fof(f17056, plain, (! [X0] : (gt(n0, X0) | ~ s1(n0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_519), inference(resolution, [], [f11376, f894])).
fof(f894, plain, ! [X47, X48] : (~ bcapacityex(sK24(X47)) | gt(X47, X48) | ~ s1(X47) | conditionnormo(X48) | gt(n0, sK24(X47))), inference(subsumption_resolution, [], [f893, f289])).
fof(f289, plain, ! [X0, X3] : (gt(X0, X3) | conditionnormo(X3) | s2(sK24(X0)) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f893, plain, ! [X47, X48] : (conditionnormo(X48) | gt(X47, X48) | ~ s1(X47) | ~ bcapacityex(sK24(X47)) | ~ s2(sK24(X47)) | gt(n0, sK24(X47))), inference(subsumption_resolution, [], [f891, f303])).
fof(f303, plain, ! [X0] : (~ conditionhyper(sK26(X0)) | ~ bcapacityex(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f891, plain, ! [X47, X48] : (conditionhyper(sK26(sK24(X47))) | conditionnormo(X48) | gt(X47, X48) | ~ s1(X47) | ~ bcapacityex(sK24(X47)) | ~ s2(sK24(X47)) | gt(n0, sK24(X47))), inference(resolution, [], [f290, f302])).
fof(f302, plain, ! [X0] : (gt(X0, sK26(X0)) | ~ bcapacityex(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f290, plain, ! [X2, X0, X3] : (~ gt(sK24(X0), X2) | conditionhyper(X2) | conditionnormo(X3) | gt(X0, X3) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f11376, plain, (bcapacityex(sK24(n0)) | ~ spl27_519), inference(avatar_component_clause, [], [f11374])).
fof(f16886, plain, (spl27_76 | ~ spl27_163), inference(avatar_contradiction_clause, [], [f16885])).
fof(f16885, plain, ($false | (spl27_76 | ~ spl27_163)), inference(subsumption_resolution, [], [f16875, f16068])).
fof(f16068, plain, (~ conditionhyper(n0) | spl27_76), inference(resolution, [], [f15963, f202])).
fof(f15963, plain, (conditionnormo(n0) | spl27_76), inference(subsumption_resolution, [], [f15961, f296])).
fof(f15961, plain, (conditionnormo(n0) | ~ s1(n0) | spl27_76), inference(resolution, [], [f1732, f622])).
fof(f622, plain, ! [X0] : (s2(sK24(X0)) | conditionnormo(X0) | ~ s1(X0)), inference(resolution, [], [f289, f194])).
fof(f1732, plain, (~ s2(sK24(n0)) | spl27_76), inference(avatar_component_clause, [], [f1730])).
fof(f1730, plain, (spl27_76 <=> s2(sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_76])])).
fof(f16875, plain, (conditionhyper(n0) | ~ spl27_163), inference(resolution, [], [f15455, f296])).
fof(f15455, plain, (! [X3] : (~ s1(X3) | conditionhyper(X3)) | ~ spl27_163), inference(resolution, [], [f14830, f242])).
fof(f242, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(rectify, [], [f24])).
fof(f24, plain, ! [X3] : (~ s3(X3) | ~ s1(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', xorstep6)).
fof(f14830, plain, (! [X0] : (s3(X0) | conditionhyper(X0)) | ~ spl27_163), inference(resolution, [], [f2994, f273])).
fof(f273, plain, ! [X0] : (~ drugi(X0) | s3(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : (s3(X0) | ~ drugi(X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (drugi(X0) => s3(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X3] : (drugi(X3) => s3(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', insulincomp)).
fof(f2994, plain, (! [X1] : (drugi(X1) | conditionhyper(X1)) | ~ spl27_163), inference(avatar_component_clause, [], [f2993])).
fof(f2993, plain, (spl27_163 <=> ! [X1] : (drugi(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_163])])).
fof(f15629, plain, (~ spl27_76 | ~ spl27_163 | ~ spl27_957), inference(avatar_contradiction_clause, [], [f15628])).
fof(f15628, plain, ($false | (~ spl27_76 | ~ spl27_163 | ~ spl27_957)), inference(subsumption_resolution, [], [f15623, f15125])).
fof(f15125, plain, (~ conditionhyper(sK24(n0)) | ~ spl27_957), inference(resolution, [], [f15111, f202])).
fof(f15111, plain, (conditionnormo(sK24(n0)) | ~ spl27_957), inference(avatar_component_clause, [], [f15109])).
fof(f15109, plain, (spl27_957 <=> conditionnormo(sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_957])])).
fof(f15623, plain, (conditionhyper(sK24(n0)) | (~ spl27_76 | ~ spl27_163)), inference(resolution, [], [f15454, f1731])).
fof(f1731, plain, (s2(sK24(n0)) | ~ spl27_76), inference(avatar_component_clause, [], [f1730])).
fof(f15454, plain, (! [X2] : (~ s2(X2) | conditionhyper(X2)) | ~ spl27_163), inference(resolution, [], [f14830, f243])).
fof(f243, plain, ! [X0] : (~ s3(X0) | ~ s2(X0)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (~ s3(X0) | ~ s2(X0)), inference(rectify, [], [f25])).
fof(f25, plain, ! [X3] : (~ s3(X3) | ~ s2(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', xorstep7)).
fof(f15112, plain, (spl27_519 | spl27_957 | spl27_75 | spl27_745), inference(avatar_split_clause, [], [f15107, f12658, f1726, f15109, f11374])).
fof(f15107, plain, (conditionnormo(sK24(n0)) | bcapacityex(sK24(n0)) | (spl27_75 | spl27_745)), inference(subsumption_resolution, [], [f15106, f296])).
fof(f15106, plain, (conditionnormo(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | (spl27_75 | spl27_745)), inference(subsumption_resolution, [], [f14990, f1728])).
fof(f14990, plain, (bcapacityne(sK24(n0)) | conditionnormo(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | spl27_745), inference(resolution, [], [f12659, f291])).
fof(f14867, plain, (spl27_46 | ~ spl27_745), inference(avatar_split_clause, [], [f14866, f12658, f1587])).
fof(f14866, plain, (! [X0] : (conditionnormo(X0) | gt(n0, X0)) | ~ spl27_745), inference(subsumption_resolution, [], [f14841, f296])).
fof(f14841, plain, (! [X0] : (conditionnormo(X0) | gt(n0, X0) | ~ s1(n0)) | ~ spl27_745), inference(resolution, [], [f12660, f288])).
fof(f288, plain, ! [X0, X3] : (~ gt(X0, sK24(X0)) | conditionnormo(X3) | gt(X0, X3) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f12660, plain, (gt(n0, sK24(n0)) | ~ spl27_745), inference(avatar_component_clause, [], [f12658])).
fof(f13002, plain, (spl27_7 | spl27_5 | ~ spl27_27), inference(avatar_split_clause, [], [f12359, f830, f354, f369])).
fof(f369, plain, (spl27_7 <=> ! [X1] : (~ releaselg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_7])])).
fof(f354, plain, (spl27_5 <=> ! [X1] : ~ sP3(X1)), introduced(avatar_definition, [new_symbols(naming, [spl27_5])])).
fof(f830, plain, (spl27_27 <=> ! [X15, X14] : (gt(X14, X15) | ~ sP3(X14) | ~ releaselg(X15))), introduced(avatar_definition, [new_symbols(naming, [spl27_27])])).
fof(f12359, plain, (! [X6, X5] : (~ sP3(X5) | ~ releaselg(X6) | conditionhyper(X6)) | ~ spl27_27), inference(duplicate_literal_removal, [], [f12305])).
fof(f12305, plain, (! [X6, X5] : (~ sP3(X5) | ~ releaselg(X6) | conditionhyper(X6) | ~ sP3(X5)) | ~ spl27_27), inference(resolution, [], [f831, f247])).
fof(f247, plain, ! [X0, X1] : (~ gt(X0, X1) | conditionhyper(X1) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ! [X2] : (bsecretioni(X2) | gt(X0, X2)) & bcapacityne(X0) & (! [X3] : (uptakepg(X3) | gt(X0, X3)) | ! [X4] : (~ releaselg(X4) | gt(X0, X4)))) | ~ sP3(X0)), inference(rectify, [], [f163])).
fof(f163, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(nnf_transformation, [], [f130])).
fof(f831, plain, (! [X14, X15] : (gt(X14, X15) | ~ sP3(X14) | ~ releaselg(X15)) | ~ spl27_27), inference(avatar_component_clause, [], [f830])).
fof(f12897, plain, (~ spl27_7 | ~ spl27_279 | ~ spl27_723), inference(avatar_contradiction_clause, [], [f12896])).
fof(f12896, plain, ($false | (~ spl27_7 | ~ spl27_279 | ~ spl27_723)), inference(subsumption_resolution, [], [f12895, f12762])).
fof(f12762, plain, (~ conditionhyper(sK20(n0)) | ~ spl27_723), inference(resolution, [], [f12239, f202])).
fof(f12895, plain, (conditionhyper(sK20(n0)) | (~ spl27_7 | ~ spl27_279)), inference(resolution, [], [f7162, f370])).
fof(f370, plain, (! [X1] : (~ releaselg(X1) | conditionhyper(X1)) | ~ spl27_7), inference(avatar_component_clause, [], [f369])).
fof(f12661, plain, (spl27_745 | spl27_46 | ~ spl27_75), inference(avatar_split_clause, [], [f12656, f1726, f1587, f12658])).
fof(f12656, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_75), inference(subsumption_resolution, [], [f12301, f296])).
fof(f12301, plain, (! [X0] : (gt(n0, X0) | ~ s1(n0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_75), inference(resolution, [], [f1727, f896])).
fof(f896, plain, ! [X50, X49] : (~ bcapacityne(sK24(X49)) | gt(X49, X50) | ~ s1(X49) | conditionnormo(X50) | gt(n0, sK24(X49))), inference(subsumption_resolution, [], [f895, f289])).
fof(f895, plain, ! [X50, X49] : (conditionnormo(X50) | gt(X49, X50) | ~ s1(X49) | ~ bcapacityne(sK24(X49)) | ~ s2(sK24(X49)) | gt(n0, sK24(X49))), inference(subsumption_resolution, [], [f892, f301])).
fof(f301, plain, ! [X0] : (~ conditionhyper(sK26(X0)) | ~ bcapacityne(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f892, plain, ! [X50, X49] : (conditionhyper(sK26(sK24(X49))) | conditionnormo(X50) | gt(X49, X50) | ~ s1(X49) | ~ bcapacityne(sK24(X49)) | ~ s2(sK24(X49)) | gt(n0, sK24(X49))), inference(resolution, [], [f290, f300])).
fof(f300, plain, ! [X0] : (gt(X0, sK26(X0)) | ~ bcapacityne(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f1727, plain, (bcapacityne(sK24(n0)) | ~ spl27_75), inference(avatar_component_clause, [], [f1726])).
fof(f12641, plain, (spl27_3 | spl27_28 | spl27_6 | spl27_14 | ~ spl27_282), inference(avatar_split_clause, [], [f12640, f7519, f513, f365, f1511, f342])).
fof(f1511, plain, (spl27_28 <=> ! [X6] : (gt(n0, X6) | uptakepg(X6))), introduced(avatar_definition, [new_symbols(naming, [spl27_28])])).
fof(f7519, plain, (spl27_282 <=> conditionnormo(sK18(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_282])])).
fof(f12640, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | uptakepg(X0)) | (spl27_6 | spl27_14 | ~ spl27_282)), inference(subsumption_resolution, [], [f12639, f515])).
fof(f12639, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP1(n0) | uptakepg(X0)) | (spl27_6 | ~ spl27_282)), inference(subsumption_resolution, [], [f8898, f367])).
fof(f8898, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X0)) | ~ spl27_282), inference(resolution, [], [f7521, f259])).
fof(f259, plain, ! [X2, X0] : (~ conditionnormo(sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f7521, plain, (conditionnormo(sK18(n0)) | ~ spl27_282), inference(avatar_component_clause, [], [f7519])).
fof(f12625, plain, (spl27_59 | ~ spl27_32), inference(avatar_split_clause, [], [f12624, f1527, f1658])).
fof(f1658, plain, (spl27_59 <=> ! [X25] : (gt(n0, X25) | drugi(X25))), introduced(avatar_definition, [new_symbols(naming, [spl27_59])])).
fof(f1527, plain, (spl27_32 <=> ! [X7] : (gt(n0, X7) | uptakelg(X7))), introduced(avatar_definition, [new_symbols(naming, [spl27_32])])).
fof(f12624, plain, (! [X57] : (gt(n0, X57) | drugi(X57)) | ~ spl27_32), inference(subsumption_resolution, [], [f11253, f275])).
fof(f275, plain, ! [X0, X1] : (~ uptakelg(sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | ((~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0))) & (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0))))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f172, f174, f173])).
fof(f173, plain, ! [X0] : (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) => (~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0)))), introduced(choice_axiom, [])).
fof(f174, plain, ! [X0] : (? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)) => (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0)))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) & ? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X0] : (! [X3] : (drugi(X3) | gt(X0, X3)) | (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (~ uptakelg(X2) & ~ gt(X0, X2)))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ((! [X1] : (~ gt(X0, X1) => uptakepg(X1)) | ! [X2] : (~ gt(X0, X2) => uptakelg(X2))) => ! [X3] : (~ gt(X0, X3) => drugi(X3))), inference(rectify, [], [f34])).
fof(f34, plain, ! [X3] : ((! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) => ! [X4] : (~ gt(X3, X4) => drugi(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', insulin_completion)).
fof(f11253, plain, (! [X57] : (uptakelg(sK20(n0)) | gt(n0, X57) | drugi(X57)) | ~ spl27_32), inference(resolution, [], [f1528, f274])).
fof(f274, plain, ! [X0, X1] : (~ gt(X0, sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f1528, plain, (! [X7] : (gt(n0, X7) | uptakelg(X7)) | ~ spl27_32), inference(avatar_component_clause, [], [f1527])).
fof(f12613, plain, (spl27_59 | ~ spl27_28), inference(avatar_split_clause, [], [f12612, f1511, f1658])).
fof(f12612, plain, (! [X56] : (gt(n0, X56) | drugi(X56)) | ~ spl27_28), inference(subsumption_resolution, [], [f10979, f277])).
fof(f277, plain, ! [X0, X1] : (~ uptakepg(sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f10979, plain, (! [X56] : (uptakepg(sK19(n0)) | gt(n0, X56) | drugi(X56)) | ~ spl27_28), inference(resolution, [], [f1512, f276])).
fof(f276, plain, ! [X0, X1] : (~ gt(X0, sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f1512, plain, (! [X6] : (gt(n0, X6) | uptakepg(X6)) | ~ spl27_28), inference(avatar_component_clause, [], [f1511])).
fof(f12605, plain, (spl27_59 | spl27_723 | ~ spl27_46), inference(avatar_split_clause, [], [f11334, f1587, f12237, f1658])).
fof(f11334, plain, (! [X57] : (conditionnormo(sK20(n0)) | gt(n0, X57) | drugi(X57)) | ~ spl27_46), inference(resolution, [], [f1588, f274])).
fof(f1588, plain, (! [X16] : (gt(n0, X16) | conditionnormo(X16)) | ~ spl27_46), inference(avatar_component_clause, [], [f1587])).
fof(f12571, plain, (spl27_163 | ~ spl27_59), inference(avatar_split_clause, [], [f12516, f1658, f2993])).
fof(f12516, plain, (! [X0] : (drugi(X0) | conditionhyper(X0)) | ~ spl27_59), inference(resolution, [], [f1659, f297])).
fof(f1659, plain, (! [X25] : (gt(n0, X25) | drugi(X25)) | ~ spl27_59), inference(avatar_component_clause, [], [f1658])).
fof(f12240, plain, (spl27_163 | spl27_75 | spl27_519 | spl27_723), inference(avatar_split_clause, [], [f12235, f12237, f11374, f1726, f2993])).
fof(f12235, plain, ! [X1] : (conditionnormo(sK20(n0)) | bcapacityex(sK24(n0)) | bcapacityne(sK24(n0)) | drugi(X1) | conditionhyper(X1)), inference(subsumption_resolution, [], [f10233, f296])).
fof(f10233, plain, ! [X1] : (conditionnormo(sK20(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | bcapacityne(sK24(n0)) | drugi(X1) | conditionhyper(X1)), inference(resolution, [], [f915, f297])).
fof(f915, plain, ! [X33, X32] : (gt(X32, X33) | conditionnormo(sK20(X32)) | bcapacityex(sK24(X32)) | ~ s1(X32) | bcapacityne(sK24(X32)) | drugi(X33)), inference(resolution, [], [f291, f274])).
fof(f11371, plain, (~ spl27_46 | ~ spl27_163), inference(avatar_contradiction_clause, [], [f11370])).
fof(f11370, plain, ($false | (~ spl27_46 | ~ spl27_163)), inference(subsumption_resolution, [], [f11369, f10295])).
fof(f10295, plain, (conditionhyper(n0) | ~ spl27_163), inference(resolution, [], [f9436, f296])).
fof(f9436, plain, (! [X3] : (~ s1(X3) | conditionhyper(X3)) | ~ spl27_163), inference(resolution, [], [f9205, f242])).
fof(f9205, plain, (! [X0] : (s3(X0) | conditionhyper(X0)) | ~ spl27_163), inference(resolution, [], [f2994, f273])).
fof(f11369, plain, (~ conditionhyper(n0) | ~ spl27_46), inference(resolution, [], [f11288, f202])).
fof(f11288, plain, (conditionnormo(n0) | ~ spl27_46), inference(resolution, [], [f1588, f194])).
fof(f8311, plain, (spl27_163 | spl27_27), inference(avatar_split_clause, [], [f4627, f830, f2993])).
fof(f4627, plain, ! [X10, X8, X9] : (gt(X8, X9) | ~ sP3(X8) | ~ releaselg(X9) | drugi(X10) | conditionhyper(X10)), inference(duplicate_literal_removal, [], [f4530])).
fof(f4530, plain, ! [X10, X8, X9] : (gt(X8, X9) | ~ sP3(X8) | ~ releaselg(X9) | drugi(X10) | conditionhyper(X10) | ~ sP3(X8)), inference(resolution, [], [f834, f247])).
fof(f834, plain, ! [X37, X38, X36] : (gt(X36, X38) | gt(X36, X37) | ~ sP3(X36) | ~ releaselg(X37) | drugi(X38)), inference(subsumption_resolution, [], [f809, f277])).
fof(f809, plain, ! [X37, X38, X36] : (gt(X36, X37) | ~ releaselg(X37) | uptakepg(sK19(X36)) | ~ sP3(X36) | gt(X36, X38) | drugi(X38)), inference(resolution, [], [f244, f276])).
fof(f244, plain, ! [X4, X0, X3] : (gt(X0, X4) | gt(X0, X3) | ~ releaselg(X4) | uptakepg(X3) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f7566, plain, ~ spl27_6, inference(avatar_contradiction_clause, [], [f7565])).
fof(f7565, plain, ($false | ~ spl27_6), inference(subsumption_resolution, [], [f7555, f298])).
fof(f298, plain, ~ bcapacitysn(n0), inference(cnf_transformation, [], [f193])).
fof(f7555, plain, (bcapacitysn(n0) | ~ spl27_6), inference(resolution, [], [f366, f249])).
fof(f249, plain, ! [X0] : (~ sP2(X0) | bcapacitysn(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X2] : (~ releaselg(X2) | gt(X0, X2))) | ~ sP2(X0)), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(nnf_transformation, [], [f129])).
fof(f366, plain, (sP2(n0) | ~ spl27_6), inference(avatar_component_clause, [], [f365])).
fof(f7554, plain, (~ spl27_3 | ~ spl27_5), inference(avatar_contradiction_clause, [], [f7553])).
fof(f7553, plain, ($false | (~ spl27_3 | ~ spl27_5)), inference(subsumption_resolution, [], [f343, f355])).
fof(f355, plain, (! [X1] : ~ sP3(X1) | ~ spl27_5), inference(avatar_component_clause, [], [f354])).
fof(f343, plain, (sP3(n0) | ~ spl27_3), inference(avatar_component_clause, [], [f342])).
fof(f7549, plain, ~ spl27_14, inference(avatar_contradiction_clause, [], [f7548])).
fof(f7548, plain, ($false | ~ spl27_14), inference(subsumption_resolution, [], [f7536, f299])).
fof(f299, plain, ~ qilt27(n0), inference(cnf_transformation, [], [f193])).
fof(f7536, plain, (qilt27(n0) | ~ spl27_14), inference(resolution, [], [f514, f254])).
fof(f254, plain, ! [X0] : (~ sP1(X0) | qilt27(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & qilt27(X0) & bcapacitysn(X0) & ! [X2] : (bsecretioni(X2) | gt(X0, X2))) | ~ sP1(X0)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(nnf_transformation, [], [f128])).
fof(f514, plain, (sP1(n0) | ~ spl27_14), inference(avatar_component_clause, [], [f513])).
fof(f7524, plain, (spl27_14 | spl27_6 | spl27_32 | spl27_282 | spl27_3 | ~ spl27_46), inference(avatar_split_clause, [], [f7523, f1587, f342, f7519, f1527, f365, f513])).
fof(f7523, plain, (! [X46] : (conditionnormo(sK18(n0)) | gt(n0, X46) | sP2(n0) | sP1(n0) | uptakelg(X46)) | (spl27_3 | ~ spl27_46)), inference(subsumption_resolution, [], [f7500, f344])).
fof(f7500, plain, (! [X46] : (conditionnormo(sK18(n0)) | gt(n0, X46) | sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X46)) | ~ spl27_46), inference(resolution, [], [f1588, f256])).
fof(f7419, plain, (spl27_59 | spl27_279), inference(avatar_split_clause, [], [f7418, f7161, f1658])).
fof(f7418, plain, (! [X0] : (gt(n0, X0) | drugi(X0)) | spl27_279), inference(resolution, [], [f7299, f275])).
fof(f7299, plain, (uptakelg(sK20(n0)) | spl27_279), inference(resolution, [], [f7163, f396])).
fof(f396, plain, ! [X0] : (releaselg(X0) | uptakelg(X0)), inference(resolution, [], [f278, f194])).
fof(f278, plain, ! [X0, X1] : (gt(X0, X1) | releaselg(X1) | uptakelg(X1)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : (uptakelg(X1) | releaselg(X1) | gt(X0, X1)), inference(flattening, [], [f114])).
fof(f114, plain, ! [X0, X1] : ((uptakelg(X1) | releaselg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1] : (~ gt(X0, X1) => (~ releaselg(X1) => uptakelg(X1))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X3, X4] : (~ gt(X3, X4) => (~ releaselg(X4) => uptakelg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED009+1.p', uptake_completion)).
fof(f7163, plain, (~ releaselg(sK20(n0)) | spl27_279), inference(avatar_component_clause, [], [f7161])).