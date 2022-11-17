fof(f1629, plain, $false, inference(avatar_sat_refutation, [], [f388, f444, f489, f507, f538, f568, f624, f680, f860, f1005, f1050, f1121, f1284, f1503, f1596, f1625])).
fof(f1625, plain, (spl26_35 | spl26_46 | spl26_8 | spl26_17 | ~ spl26_60), inference(avatar_split_clause, [], [f1624, f1119, f463, f382, f1055, f844])).
fof(f844, plain, (spl26_35 <=> conditionnormo(sK18(n0))), introduced(avatar_definition, [new_symbols(naming, [spl26_35])])).
fof(f1055, plain, (spl26_46 <=> ! [X6] : (gt(n0, X6) | uptakepg(X6))), introduced(avatar_definition, [new_symbols(naming, [spl26_46])])).
fof(f382, plain, (spl26_8 <=> sP2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_8])])).
fof(f463, plain, (spl26_17 <=> sP1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_17])])).
fof(f1119, plain, (spl26_60 <=> ! [X15, X14] : (gt(X14, X15) | ~ sP3(X14) | uptakepg(X15))), introduced(avatar_definition, [new_symbols(naming, [spl26_60])])).
fof(f1624, plain, (! [X0] : (gt(n0, X0) | uptakepg(X0) | conditionnormo(sK18(n0))) | (spl26_8 | spl26_17 | ~ spl26_60)), inference(subsumption_resolution, [], [f1623, f1120])).
fof(f1120, plain, (! [X14, X15] : (gt(X14, X15) | ~ sP3(X14) | uptakepg(X15)) | ~ spl26_60), inference(avatar_component_clause, [], [f1119])).
fof(f1623, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | uptakepg(X0) | conditionnormo(sK18(n0))) | (spl26_8 | spl26_17)), inference(subsumption_resolution, [], [f1622, f465])).
fof(f465, plain, (~ sP1(n0) | spl26_17), inference(avatar_component_clause, [], [f463])).
fof(f1622, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP1(n0) | uptakepg(X0) | conditionnormo(sK18(n0))) | spl26_8), inference(subsumption_resolution, [], [f1156, f384])).
fof(f384, plain, (~ sP2(n0) | spl26_8), inference(avatar_component_clause, [], [f382])).
fof(f1156, plain, ! [X0] : (gt(n0, X0) | sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X0) | conditionnormo(sK18(n0))), inference(resolution, [], [f256, f298])).
fof(f298, plain, ! [X0] : (gt(n0, X0) | conditionnormo(X0)), inference(cnf_transformation, [], [f191])).
fof(f191, plain, (! [X0] : (conditionnormo(X0) | gt(n0, X0)) & qilt27(n0) & ~ bcapacitysn(n0) & ! [X1] : (conditionhyper(X1) | ~ gt(n0, X1)) & s1(n0)), inference(rectify, [], [f125])).
fof(f125, plain, (! [X1] : (conditionnormo(X1) | gt(n0, X1)) & qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0)), inference(flattening, [], [f124])).
fof(f124, plain, (! [X1] : (conditionnormo(X1) | gt(n0, X1)) & (qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ~ ((qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (gt(n0, X0) => conditionhyper(X0)) & s1(n0)) => ~ ! [X1] : (~ gt(n0, X1) => conditionnormo(X1))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ~ ! [X3] : (~ gt(n0, X3) => conditionnormo(X3))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ~ ! [X3] : (~ gt(n0, X3) => conditionnormo(X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', unsuccessfuls1_qilt27)).
fof(f256, plain, ! [X2, X0] : (~ gt(X0, sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
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
fof(f26, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) => ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => uptakepg(X4)) & ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ~ qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', normo)).
fof(f1596, plain, (spl26_32 | ~ spl26_19 | ~ spl26_24), inference(avatar_split_clause, [], [f1595, f528, f483, f622])).
fof(f622, plain, (spl26_32 <=> ! [X0] : (gt(n0, X0) | drugbg(X0))), introduced(avatar_definition, [new_symbols(naming, [spl26_32])])).
fof(f483, plain, (spl26_19 <=> releaselg(sK22(n0))), introduced(avatar_definition, [new_symbols(naming, [spl26_19])])).
fof(f528, plain, (spl26_24 <=> ! [X0] : (gt(n0, X0) | uptakelg(X0))), introduced(avatar_definition, [new_symbols(naming, [spl26_24])])).
fof(f1595, plain, (! [X28] : (gt(n0, X28) | drugbg(X28)) | (~ spl26_19 | ~ spl26_24)), inference(subsumption_resolution, [], [f1586, f626])).
fof(f626, plain, (~ uptakelg(sK22(n0)) | ~ spl26_19), inference(resolution, [], [f485, f346])).
fof(f346, plain, ! [X0] : (~ releaselg(X0) | ~ uptakelg(X0)), inference(resolution, [], [f206, f192])).
fof(f192, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', irreflexivity_gt)).
fof(f206, plain, ! [X0, X1] : (gt(X0, X1) | ~ uptakelg(X1) | ~ releaselg(X1)), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : (~ releaselg(X1) | ~ uptakelg(X1) | gt(X0, X1)), inference(flattening, [], [f86])).
fof(f86, plain, ! [X0, X1] : ((~ releaselg(X1) | ~ uptakelg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1] : (~ gt(X0, X1) => (uptakelg(X1) => ~ releaselg(X1))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X3, X4] : (~ gt(X3, X4) => (uptakelg(X4) => ~ releaselg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', liver_glucose)).
fof(f485, plain, (releaselg(sK22(n0)) | ~ spl26_19), inference(avatar_component_clause, [], [f483])).
fof(f1586, plain, (! [X28] : (uptakelg(sK22(n0)) | gt(n0, X28) | drugbg(X28)) | ~ spl26_24), inference(resolution, [], [f529, f281])).
fof(f281, plain, ! [X0, X1] : (~ gt(X0, sK22(X0)) | gt(X0, X1) | drugbg(X1)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : (! [X1] : (drugbg(X1) | gt(X0, X1)) | (releaselg(sK22(X0)) & ~ gt(X0, sK22(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22])], [f179, f180])).
fof(f180, plain, ! [X0] : (? [X2] : (releaselg(X2) & ~ gt(X0, X2)) => (releaselg(sK22(X0)) & ~ gt(X0, sK22(X0)))), introduced(choice_axiom, [])).
fof(f179, plain, ! [X0] : (! [X1] : (drugbg(X1) | gt(X0, X1)) | ? [X2] : (releaselg(X2) & ~ gt(X0, X2))), inference(rectify, [], [f117])).
fof(f117, plain, ! [X0] : (! [X2] : (drugbg(X2) | gt(X0, X2)) | ? [X1] : (releaselg(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f77])).
fof(f77, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => ~ releaselg(X1)) => ! [X2] : (~ gt(X0, X2) => drugbg(X2))), inference(rectify, [], [f37])).
fof(f37, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)) => ! [X4] : (~ gt(X3, X4) => drugbg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', bg_completion)).
fof(f529, plain, (! [X0] : (gt(n0, X0) | uptakelg(X0)) | ~ spl26_24), inference(avatar_component_clause, [], [f528])).
fof(f1503, plain, (spl26_28 | ~ spl26_46), inference(avatar_split_clause, [], [f1502, f1055, f581])).
fof(f581, plain, (spl26_28 <=> ! [X0] : (gt(n0, X0) | drugi(X0))), introduced(avatar_definition, [new_symbols(naming, [spl26_28])])).
fof(f1502, plain, (! [X23] : (gt(n0, X23) | drugi(X23)) | ~ spl26_46), inference(subsumption_resolution, [], [f1478, f275])).
fof(f275, plain, ! [X0, X1] : (~ uptakepg(sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | ((~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0))) & (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0))))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f172, f174, f173])).
fof(f173, plain, ! [X0] : (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) => (~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0)))), introduced(choice_axiom, [])).
fof(f174, plain, ! [X0] : (? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)) => (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0)))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) & ? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X0] : (! [X3] : (drugi(X3) | gt(X0, X3)) | (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (~ uptakelg(X2) & ~ gt(X0, X2)))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ((! [X1] : (~ gt(X0, X1) => uptakepg(X1)) | ! [X2] : (~ gt(X0, X2) => uptakelg(X2))) => ! [X3] : (~ gt(X0, X3) => drugi(X3))), inference(rectify, [], [f34])).
fof(f34, plain, ! [X3] : ((! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) => ! [X4] : (~ gt(X3, X4) => drugi(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', insulin_completion)).
fof(f1478, plain, (! [X23] : (uptakepg(sK19(n0)) | gt(n0, X23) | drugi(X23)) | ~ spl26_46), inference(resolution, [], [f1056, f274])).
fof(f274, plain, ! [X0, X1] : (~ gt(X0, sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f1056, plain, (! [X6] : (gt(n0, X6) | uptakepg(X6)) | ~ spl26_46), inference(avatar_component_clause, [], [f1055])).
fof(f1284, plain, (spl26_46 | spl26_8 | spl26_17 | ~ spl26_35 | ~ spl26_60), inference(avatar_split_clause, [], [f1283, f1119, f844, f463, f382, f1055])).
fof(f1283, plain, (! [X0] : (gt(n0, X0) | uptakepg(X0)) | (spl26_8 | spl26_17 | ~ spl26_35 | ~ spl26_60)), inference(subsumption_resolution, [], [f1282, f1120])).
fof(f1282, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | uptakepg(X0)) | (spl26_8 | spl26_17 | ~ spl26_35)), inference(subsumption_resolution, [], [f1281, f465])).
fof(f1281, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP1(n0) | uptakepg(X0)) | (spl26_8 | ~ spl26_35)), inference(subsumption_resolution, [], [f1255, f384])).
fof(f1255, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X0)) | ~ spl26_35), inference(resolution, [], [f846, f257])).
fof(f257, plain, ! [X2, X0] : (~ conditionnormo(sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f846, plain, (conditionnormo(sK18(n0)) | ~ spl26_35), inference(avatar_component_clause, [], [f844])).
fof(f1121, plain, (spl26_9 | spl26_60), inference(avatar_split_clause, [], [f827, f1119, f386])).
fof(f386, plain, (spl26_9 <=> ! [X1] : (~ releaselg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl26_9])])).
fof(f827, plain, ! [X14, X15, X16] : (gt(X14, X15) | ~ releaselg(X16) | uptakepg(X15) | ~ sP3(X14) | conditionhyper(X16)), inference(duplicate_literal_removal, [], [f788])).
fof(f788, plain, ! [X14, X15, X16] : (gt(X14, X15) | ~ releaselg(X16) | uptakepg(X15) | ~ sP3(X14) | conditionhyper(X16) | ~ sP3(X14)), inference(resolution, [], [f242, f245])).
fof(f245, plain, ! [X0, X1] : (~ gt(X0, X1) | conditionhyper(X1) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ! [X2] : (bsecretioni(X2) | gt(X0, X2)) & bcapacityne(X0) & (! [X3] : (uptakepg(X3) | gt(X0, X3)) | ! [X4] : (~ releaselg(X4) | gt(X0, X4)))) | ~ sP3(X0)), inference(rectify, [], [f163])).
fof(f163, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(nnf_transformation, [], [f130])).
fof(f242, plain, ! [X4, X0, X3] : (gt(X0, X4) | gt(X0, X3) | ~ releaselg(X4) | uptakepg(X3) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f1050, plain, (spl26_11 | ~ spl26_32), inference(avatar_contradiction_clause, [], [f1049])).
fof(f1049, plain, ($false | (spl26_11 | ~ spl26_32)), inference(subsumption_resolution, [], [f1027, f435])).
fof(f435, plain, (~ drugbg(n0) | spl26_11), inference(avatar_component_clause, [], [f433])).
fof(f433, plain, (spl26_11 <=> drugbg(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_11])])).
fof(f1027, plain, (drugbg(n0) | ~ spl26_32), inference(resolution, [], [f623, f192])).
fof(f623, plain, (! [X0] : (gt(n0, X0) | drugbg(X0)) | ~ spl26_32), inference(avatar_component_clause, [], [f622])).
fof(f1005, plain, (spl26_24 | ~ spl26_28), inference(avatar_split_clause, [], [f1004, f581, f528])).
fof(f1004, plain, (! [X7] : (gt(n0, X7) | uptakelg(X7)) | ~ spl26_28), inference(subsumption_resolution, [], [f988, f203])).
fof(f203, plain, ! [X0, X1] : (~ drugi(sK4(X0)) | gt(X0, X1) | uptakelg(X1)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (! [X1] : ((uptakepg(X1) & uptakelg(X1)) | gt(X0, X1)) | (~ drugi(sK4(X0)) & ~ gt(X0, sK4(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f132, f133])).
fof(f133, plain, ! [X0] : (? [X2] : (~ drugi(X2) & ~ gt(X0, X2)) => (~ drugi(sK4(X0)) & ~ gt(X0, sK4(X0)))), introduced(choice_axiom, [])).
fof(f132, plain, ! [X0] : (! [X1] : ((uptakepg(X1) & uptakelg(X1)) | gt(X0, X1)) | ? [X2] : (~ drugi(X2) & ~ gt(X0, X2))), inference(rectify, [], [f85])).
fof(f85, plain, ! [X0] : (! [X2] : ((uptakepg(X2) & uptakelg(X2)) | gt(X0, X2)) | ? [X1] : (~ drugi(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => drugi(X1)) => ! [X2] : (~ gt(X0, X2) => (uptakepg(X2) & uptakelg(X2)))), inference(rectify, [], [f11])).
fof(f11, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => drugi(X4)) => ! [X4] : (~ gt(X3, X4) => (uptakepg(X4) & uptakelg(X4)))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', insulin_effect)).
fof(f988, plain, (! [X7] : (drugi(sK4(n0)) | gt(n0, X7) | uptakelg(X7)) | ~ spl26_28), inference(resolution, [], [f582, f202])).
fof(f202, plain, ! [X0, X1] : (~ gt(X0, sK4(X0)) | gt(X0, X1) | uptakelg(X1)), inference(cnf_transformation, [], [f134])).
fof(f582, plain, (! [X0] : (gt(n0, X0) | drugi(X0)) | ~ spl26_28), inference(avatar_component_clause, [], [f581])).
fof(f860, plain, ~ spl26_17, inference(avatar_contradiction_clause, [], [f859])).
fof(f859, plain, ($false | ~ spl26_17), inference(subsumption_resolution, [], [f858, f296])).
fof(f296, plain, ~ bcapacitysn(n0), inference(cnf_transformation, [], [f191])).
fof(f858, plain, (bcapacitysn(n0) | ~ spl26_17), inference(resolution, [], [f464, f251])).
fof(f251, plain, ! [X0] : (~ sP1(X0) | bcapacitysn(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & qilt27(X0) & bcapacitysn(X0) & ! [X2] : (bsecretioni(X2) | gt(X0, X2))) | ~ sP1(X0)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(nnf_transformation, [], [f128])).
fof(f464, plain, (sP1(n0) | ~ spl26_17), inference(avatar_component_clause, [], [f463])).
fof(f680, plain, (~ spl26_9 | ~ spl26_19 | ~ spl26_31), inference(avatar_contradiction_clause, [], [f679])).
fof(f679, plain, ($false | (~ spl26_9 | ~ spl26_19 | ~ spl26_31)), inference(subsumption_resolution, [], [f678, f627])).
fof(f627, plain, (conditionhyper(sK22(n0)) | (~ spl26_9 | ~ spl26_19)), inference(resolution, [], [f485, f387])).
fof(f387, plain, (! [X1] : (~ releaselg(X1) | conditionhyper(X1)) | ~ spl26_9), inference(avatar_component_clause, [], [f386])).
fof(f678, plain, (~ conditionhyper(sK22(n0)) | ~ spl26_31), inference(resolution, [], [f620, f200])).
fof(f200, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (~ conditionnormo(X3) | ~ conditionhyper(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', xorcondition3)).
fof(f620, plain, (conditionnormo(sK22(n0)) | ~ spl26_31), inference(avatar_component_clause, [], [f618])).
fof(f618, plain, (spl26_31 <=> conditionnormo(sK22(n0))), introduced(avatar_definition, [new_symbols(naming, [spl26_31])])).
fof(f624, plain, (spl26_31 | spl26_32), inference(avatar_split_clause, [], [f610, f622, f618])).
fof(f610, plain, ! [X0] : (gt(n0, X0) | drugbg(X0) | conditionnormo(sK22(n0))), inference(resolution, [], [f281, f298])).
fof(f568, plain, ~ spl26_13, inference(avatar_contradiction_clause, [], [f567])).
fof(f567, plain, ($false | ~ spl26_13), inference(subsumption_resolution, [], [f565, f294])).
fof(f294, plain, s1(n0), inference(cnf_transformation, [], [f191])).
fof(f565, plain, (~ s1(n0) | ~ spl26_13), inference(resolution, [], [f443, f239])).
fof(f239, plain, ! [X0] : (~ s2(X0) | ~ s1(X0)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : (~ s2(X0) | ~ s1(X0)), inference(rectify, [], [f23])).
fof(f23, plain, ! [X3] : (~ s2(X3) | ~ s1(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', xorstep5)).
fof(f443, plain, (s2(n0) | ~ spl26_13), inference(avatar_component_clause, [], [f441])).
fof(f441, plain, (spl26_13 <=> s2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_13])])).
fof(f538, plain, ~ spl26_12, inference(avatar_contradiction_clause, [], [f537])).
fof(f537, plain, ($false | ~ spl26_12), inference(subsumption_resolution, [], [f534, f294])).
fof(f534, plain, (~ s1(n0) | ~ spl26_12), inference(resolution, [], [f439, f240])).
fof(f240, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(rectify, [], [f24])).
fof(f24, plain, ! [X3] : (~ s3(X3) | ~ s1(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', xorstep6)).
fof(f439, plain, (s3(n0) | ~ spl26_12), inference(avatar_component_clause, [], [f437])).
fof(f437, plain, (spl26_12 <=> s3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_12])])).
fof(f507, plain, (spl26_11 | ~ spl26_20), inference(avatar_contradiction_clause, [], [f506])).
fof(f506, plain, ($false | (spl26_11 | ~ spl26_20)), inference(subsumption_resolution, [], [f505, f310])).
fof(f310, plain, ~ conditionhyper(n0), inference(resolution, [], [f308, f200])).
fof(f308, plain, conditionnormo(n0), inference(resolution, [], [f298, f192])).
fof(f505, plain, (conditionhyper(n0) | (spl26_11 | ~ spl26_20)), inference(resolution, [], [f488, f435])).
fof(f488, plain, (! [X1] : (drugbg(X1) | conditionhyper(X1)) | ~ spl26_20), inference(avatar_component_clause, [], [f487])).
fof(f487, plain, (spl26_20 <=> ! [X1] : (drugbg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl26_20])])).
fof(f489, plain, (spl26_19 | spl26_20), inference(avatar_split_clause, [], [f475, f487, f483])).
fof(f475, plain, ! [X1] : (drugbg(X1) | releaselg(sK22(n0)) | conditionhyper(X1)), inference(resolution, [], [f282, f295])).
fof(f295, plain, ! [X1] : (~ gt(n0, X1) | conditionhyper(X1)), inference(cnf_transformation, [], [f191])).
fof(f282, plain, ! [X0, X1] : (gt(X0, X1) | drugbg(X1) | releaselg(sK22(X0))), inference(cnf_transformation, [], [f181])).
fof(f444, plain, (~ spl26_11 | spl26_12 | spl26_13), inference(avatar_split_clause, [], [f431, f441, f437, f433])).
fof(f431, plain, (s2(n0) | s3(n0) | ~ drugbg(n0)), inference(resolution, [], [f269, f297])).
fof(f297, plain, qilt27(n0), inference(cnf_transformation, [], [f191])).
fof(f269, plain, ! [X0] : (~ qilt27(X0) | s2(X0) | s3(X0) | ~ drugbg(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0] : (s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0)) | ~ drugbg(X0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0] : ((s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0))) | ~ drugbg(X0)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0] : (drugbg(X0) => (s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0)))), inference(rectify, [], [f31])).
fof(f31, plain, ! [X3] : (drugbg(X3) => (s3(X3) | s2(X3) | (~ qilt27(X3) & s1(X3)))), file('/home/ubuntu/library/tptp/Problems/MED/MED006+1.p', bgcomp)).
fof(f388, plain, (~ spl26_8 | spl26_9), inference(avatar_split_clause, [], [f376, f386, f382])).
fof(f376, plain, ! [X1] : (~ releaselg(X1) | ~ sP2(n0) | conditionhyper(X1)), inference(resolution, [], [f246, f295])).
fof(f246, plain, ! [X2, X0] : (gt(X0, X2) | ~ releaselg(X2) | ~ sP2(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X2] : (~ releaselg(X2) | gt(X0, X2))) | ~ sP2(X0)), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(nnf_transformation, [], [f129])).