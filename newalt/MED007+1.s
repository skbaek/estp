fof(f75628, plain, $false, inference(avatar_sat_refutation, [], [f424, f444, f7431, f7536, f7551, f7898, f7932, f7934, f7968, f9304, f9312, f11905, f11949, f11980, f12571, f13780, f18242, f18402, f22591, f22797, f24659, f26869, f53538, f57665, f57821, f74038, f74383, f74603, f74708, f75467, f75627])).
fof(f75627, plain, (spl27_34 | spl27_3 | spl27_6 | spl27_17 | ~ spl27_284), inference(avatar_split_clause, [], [f75626, f7531, f536, f365, f342, f1538])).
fof(f1538, plain, (spl27_34 <=> ! [X7] : (gt(n0, X7) | uptakelg(X7))), introduced(avatar_definition, [new_symbols(naming, [spl27_34])])).
fof(f342, plain, (spl27_3 <=> sP3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_3])])).
fof(f365, plain, (spl27_6 <=> sP2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_6])])).
fof(f536, plain, (spl27_17 <=> sP1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_17])])).
fof(f7531, plain, (spl27_284 <=> conditionnormo(sK18(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_284])])).
fof(f75626, plain, (! [X1] : (gt(n0, X1) | uptakelg(X1)) | (spl27_3 | spl27_6 | spl27_17 | ~ spl27_284)), inference(subsumption_resolution, [], [f75625, f538])).
fof(f538, plain, (~ sP1(n0) | spl27_17), inference(avatar_component_clause, [], [f536])).
fof(f75625, plain, (! [X1] : (gt(n0, X1) | sP1(n0) | uptakelg(X1)) | (spl27_3 | spl27_6 | ~ spl27_284)), inference(subsumption_resolution, [], [f75624, f367])).
fof(f367, plain, (~ sP2(n0) | spl27_6), inference(avatar_component_clause, [], [f365])).
fof(f75624, plain, (! [X1] : (gt(n0, X1) | sP2(n0) | sP1(n0) | uptakelg(X1)) | (spl27_3 | ~ spl27_284)), inference(subsumption_resolution, [], [f75615, f344])).
fof(f344, plain, (~ sP3(n0) | spl27_3), inference(avatar_component_clause, [], [f342])).
fof(f75615, plain, (! [X1] : (gt(n0, X1) | sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X1)) | ~ spl27_284), inference(resolution, [], [f7533, f257])).
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
fof(f26, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) => ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => uptakepg(X4)) & ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ~ qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', normo)).
fof(f7533, plain, (conditionnormo(sK18(n0)) | ~ spl27_284), inference(avatar_component_clause, [], [f7531])).
fof(f75467, plain, (spl27_72 | ~ spl27_12 | ~ spl27_34), inference(avatar_split_clause, [], [f75466, f1538, f438, f1716])).
fof(f1716, plain, (spl27_72 <=> ! [X28] : (gt(n0, X28) | drugbg(X28))), introduced(avatar_definition, [new_symbols(naming, [spl27_72])])).
fof(f438, plain, (spl27_12 <=> releaselg(sK22(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_12])])).
fof(f75466, plain, (! [X137] : (gt(n0, X137) | drugbg(X137)) | (~ spl27_12 | ~ spl27_34)), inference(subsumption_resolution, [], [f75446, f5428])).
fof(f5428, plain, (~ uptakelg(sK22(n0)) | ~ spl27_12), inference(resolution, [], [f440, f331])).
fof(f331, plain, ! [X0] : (~ releaselg(X0) | ~ uptakelg(X0)), inference(resolution, [], [f208, f194])).
fof(f194, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', irreflexivity_gt)).
fof(f208, plain, ! [X0, X1] : (gt(X0, X1) | ~ uptakelg(X1) | ~ releaselg(X1)), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : (~ releaselg(X1) | ~ uptakelg(X1) | gt(X0, X1)), inference(flattening, [], [f86])).
fof(f86, plain, ! [X0, X1] : ((~ releaselg(X1) | ~ uptakelg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1] : (~ gt(X0, X1) => (uptakelg(X1) => ~ releaselg(X1))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X3, X4] : (~ gt(X3, X4) => (uptakelg(X4) => ~ releaselg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', liver_glucose)).
fof(f440, plain, (releaselg(sK22(n0)) | ~ spl27_12), inference(avatar_component_clause, [], [f438])).
fof(f75446, plain, (! [X137] : (uptakelg(sK22(n0)) | gt(n0, X137) | drugbg(X137)) | ~ spl27_34), inference(resolution, [], [f1539, f283])).
fof(f283, plain, ! [X0, X1] : (~ gt(X0, sK22(X0)) | gt(X0, X1) | drugbg(X1)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : (! [X1] : (drugbg(X1) | gt(X0, X1)) | (releaselg(sK22(X0)) & ~ gt(X0, sK22(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22])], [f179, f180])).
fof(f180, plain, ! [X0] : (? [X2] : (releaselg(X2) & ~ gt(X0, X2)) => (releaselg(sK22(X0)) & ~ gt(X0, sK22(X0)))), introduced(choice_axiom, [])).
fof(f179, plain, ! [X0] : (! [X1] : (drugbg(X1) | gt(X0, X1)) | ? [X2] : (releaselg(X2) & ~ gt(X0, X2))), inference(rectify, [], [f117])).
fof(f117, plain, ! [X0] : (! [X2] : (drugbg(X2) | gt(X0, X2)) | ? [X1] : (releaselg(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f77])).
fof(f77, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => ~ releaselg(X1)) => ! [X2] : (~ gt(X0, X2) => drugbg(X2))), inference(rectify, [], [f37])).
fof(f37, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)) => ! [X4] : (~ gt(X3, X4) => drugbg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', bg_completion)).
fof(f1539, plain, (! [X7] : (gt(n0, X7) | uptakelg(X7)) | ~ spl27_34), inference(avatar_component_clause, [], [f1538])).
fof(f74708, plain, (spl27_77 | spl27_27 | spl27_3 | spl27_6 | spl27_17 | spl27_644), inference(avatar_split_clause, [], [f74707, f9224, f536, f365, f342, f832, f1737])).
fof(f1737, plain, (spl27_77 <=> bcapacityne(sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_77])])).
fof(f832, plain, (spl27_27 <=> ! [X10] : (uptakepg(X10) | conditionhyper(X10))), introduced(avatar_definition, [new_symbols(naming, [spl27_27])])).
fof(f9224, plain, (spl27_644 <=> bcapacityex(sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_644])])).
fof(f74707, plain, (! [X1] : (uptakepg(X1) | bcapacityne(sK24(n0)) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_17 | spl27_644)), inference(subsumption_resolution, [], [f74706, f296])).
fof(f296, plain, s1(n0), inference(cnf_transformation, [], [f193])).
fof(f193, plain, (! [X0] : ((~ bcapacityex(X0) & ~ bcapacityne(X0)) | (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0))) | ~ s2(X0) | gt(n0, X0)) & qilt27(n0) & ~ bcapacitysn(n0) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s1(n0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK26])], [f191, f192])).
fof(f192, plain, ! [X0] : (? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) => (~ conditionhyper(sK26(X0)) & gt(X0, sK26(X0)))), introduced(choice_axiom, [])).
fof(f191, plain, (! [X0] : ((~ bcapacityex(X0) & ~ bcapacityne(X0)) | ? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ~ s2(X0) | gt(n0, X0)) & qilt27(n0) & ~ bcapacitysn(n0) & ! [X2] : (conditionhyper(X2) | ~ gt(n0, X2)) & s1(n0)), inference(rectify, [], [f125])).
fof(f125, plain, (! [X1] : ((~ bcapacityex(X1) & ~ bcapacityne(X1)) | ? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s2(X1) | gt(n0, X1)) & qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0)), inference(flattening, [], [f124])).
fof(f124, plain, (! [X1] : ((~ bcapacityex(X1) & ~ bcapacityne(X1)) | ? [X2] : (~ conditionhyper(X2) & gt(X1, X2)) | ~ s2(X1) | gt(n0, X1)) & (qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ~ ((qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (gt(n0, X0) => conditionhyper(X0)) & s1(n0)) => ? [X1] : ((bcapacityex(X1) | bcapacityne(X1)) & ! [X2] : (gt(X1, X2) => conditionhyper(X2)) & s2(X1) & ~ gt(n0, X1))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ? [X3] : ((bcapacityex(X3) | bcapacityne(X3)) & ! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s2(X3) & ~ gt(n0, X3))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ? [X3] : ((bcapacityex(X3) | bcapacityne(X3)) & ! [X4] : (gt(X3, X4) => conditionhyper(X4)) & s2(X3) & ~ gt(n0, X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', transs1s2_qilt27)).
fof(f74706, plain, (! [X1] : (uptakepg(X1) | bcapacityne(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_17 | spl27_644)), inference(subsumption_resolution, [], [f74292, f9225])).
fof(f9225, plain, (~ bcapacityex(sK24(n0)) | spl27_644), inference(avatar_component_clause, [], [f9224])).
fof(f74292, plain, (! [X1] : (uptakepg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6 | spl27_17)), inference(subsumption_resolution, [], [f74291, f538])).
fof(f74291, plain, (! [X1] : (sP1(n0) | uptakepg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | (spl27_3 | spl27_6)), inference(subsumption_resolution, [], [f74290, f367])).
fof(f74290, plain, (! [X1] : (sP2(n0) | sP1(n0) | uptakepg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)) | spl27_3), inference(subsumption_resolution, [], [f14867, f344])).
fof(f14867, plain, ! [X1] : (sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X1) | bcapacityne(sK24(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | conditionhyper(X1)), inference(resolution, [], [f1004, f297])).
fof(f297, plain, ! [X2] : (~ gt(n0, X2) | conditionhyper(X2)), inference(cnf_transformation, [], [f193])).
fof(f1004, plain, ! [X23, X22] : (gt(X22, X23) | sP3(X22) | sP2(X22) | sP1(X22) | uptakepg(X23) | bcapacityne(sK24(X22)) | bcapacityex(sK24(X22)) | ~ s1(X22)), inference(subsumption_resolution, [], [f996, f259])).
fof(f259, plain, ! [X2, X0] : (~ conditionnormo(sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f996, plain, ! [X23, X22] : (gt(X22, X23) | sP3(X22) | sP2(X22) | sP1(X22) | uptakepg(X23) | bcapacityne(sK24(X22)) | conditionnormo(sK18(X22)) | bcapacityex(sK24(X22)) | ~ s1(X22)), inference(resolution, [], [f258, f291])).
fof(f291, plain, ! [X0, X3] : (gt(X0, X3) | bcapacityne(sK24(X0)) | conditionnormo(X3) | bcapacityex(sK24(X0)) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ! [X0] : (((bcapacityex(sK24(X0)) | bcapacityne(sK24(X0))) & ! [X2] : (conditionhyper(X2) | ~ gt(sK24(X0), X2)) & s2(sK24(X0)) & ~ gt(X0, sK24(X0))) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK24])], [f185, f186])).
fof(f186, plain, ! [X0] : (? [X1] : ((bcapacityex(X1) | bcapacityne(X1)) & ! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s2(X1) & ~ gt(X0, X1)) => ((bcapacityex(sK24(X0)) | bcapacityne(sK24(X0))) & ! [X2] : (conditionhyper(X2) | ~ gt(sK24(X0), X2)) & s2(sK24(X0)) & ~ gt(X0, sK24(X0)))), introduced(choice_axiom, [])).
fof(f185, plain, ! [X0] : (? [X1] : ((bcapacityex(X1) | bcapacityne(X1)) & ! [X2] : (conditionhyper(X2) | ~ gt(X1, X2)) & s2(X1) & ~ gt(X0, X1)) | ! [X3] : (conditionnormo(X3) | gt(X0, X3)) | ~ s1(X0)), inference(rectify, [], [f121])).
fof(f121, plain, ! [X0] : (? [X2] : ((bcapacityex(X2) | bcapacityne(X2)) & ! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s2(X2) & ~ gt(X0, X2)) | ! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s1(X0)), inference(flattening, [], [f120])).
fof(f120, plain, ! [X0] : (? [X2] : ((bcapacityex(X2) | bcapacityne(X2)) & ! [X3] : (conditionhyper(X3) | ~ gt(X2, X3)) & s2(X2) & ~ gt(X0, X2)) | (! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ~ s1(X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((~ ! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) & s1(X0)) => ? [X2] : ((bcapacityex(X2) | bcapacityne(X2)) & ! [X3] : (gt(X2, X3) => conditionhyper(X3)) & s2(X2) & ~ gt(X0, X2))), inference(rectify, [], [f39])).
fof(f39, plain, ! [X3] : ((~ ! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) & s1(X3)) => ? [X4] : ((bcapacityex(X4) | bcapacityne(X4)) & ! [X5] : (gt(X4, X5) => conditionhyper(X5)) & s2(X4) & ~ gt(X3, X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', trans_ax2)).
fof(f258, plain, ! [X2, X0] : (~ gt(X0, sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f74603, plain, (spl27_165 | ~ spl27_27 | ~ spl27_702), inference(avatar_split_clause, [], [f74602, f11946, f832, f3004])).
fof(f3004, plain, (spl27_165 <=> ! [X1] : (drugi(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_165])])).
fof(f11946, plain, (spl27_702 <=> conditionnormo(sK19(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_702])])).
fof(f74602, plain, (! [X1] : (drugi(X1) | conditionhyper(X1)) | (~ spl27_27 | ~ spl27_702)), inference(subsumption_resolution, [], [f74493, f74385])).
fof(f74385, plain, (~ conditionhyper(sK19(n0)) | ~ spl27_702), inference(resolution, [], [f11948, f202])).
fof(f202, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (~ conditionnormo(X3) | ~ conditionhyper(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', xorcondition3)).
fof(f11948, plain, (conditionnormo(sK19(n0)) | ~ spl27_702), inference(avatar_component_clause, [], [f11946])).
fof(f74493, plain, (! [X1] : (conditionhyper(sK19(n0)) | drugi(X1) | conditionhyper(X1)) | ~ spl27_27), inference(resolution, [], [f74387, f297])).
fof(f74387, plain, (! [X2, X1] : (gt(X1, X2) | conditionhyper(sK19(X1)) | drugi(X2)) | ~ spl27_27), inference(resolution, [], [f833, f277])).
fof(f277, plain, ! [X0, X1] : (~ uptakepg(sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | ((~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0))) & (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0))))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f172, f174, f173])).
fof(f173, plain, ! [X0] : (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) => (~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0)))), introduced(choice_axiom, [])).
fof(f174, plain, ! [X0] : (? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)) => (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0)))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) & ? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X0] : (! [X3] : (drugi(X3) | gt(X0, X3)) | (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (~ uptakelg(X2) & ~ gt(X0, X2)))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ((! [X1] : (~ gt(X0, X1) => uptakepg(X1)) | ! [X2] : (~ gt(X0, X2) => uptakelg(X2))) => ! [X3] : (~ gt(X0, X3) => drugi(X3))), inference(rectify, [], [f34])).
fof(f34, plain, ! [X3] : ((! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) => ! [X4] : (~ gt(X3, X4) => drugi(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', insulin_completion)).
fof(f833, plain, (! [X10] : (uptakepg(X10) | conditionhyper(X10)) | ~ spl27_27), inference(avatar_component_clause, [], [f832])).
fof(f74383, plain, (~ spl27_7 | ~ spl27_281 | ~ spl27_692), inference(avatar_contradiction_clause, [], [f74382])).
fof(f74382, plain, ($false | (~ spl27_7 | ~ spl27_281 | ~ spl27_692)), inference(subsumption_resolution, [], [f74338, f74329])).
fof(f74329, plain, (~ conditionhyper(sK20(n0)) | ~ spl27_692), inference(resolution, [], [f11904, f202])).
fof(f11904, plain, (conditionnormo(sK20(n0)) | ~ spl27_692), inference(avatar_component_clause, [], [f11902])).
fof(f11902, plain, (spl27_692 <=> conditionnormo(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_692])])).
fof(f74338, plain, (conditionhyper(sK20(n0)) | (~ spl27_7 | ~ spl27_281)), inference(resolution, [], [f370, f7174])).
fof(f7174, plain, (releaselg(sK20(n0)) | ~ spl27_281), inference(avatar_component_clause, [], [f7173])).
fof(f7173, plain, (spl27_281 <=> releaselg(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_281])])).
fof(f370, plain, (! [X1] : (~ releaselg(X1) | conditionhyper(X1)) | ~ spl27_7), inference(avatar_component_clause, [], [f369])).
fof(f369, plain, (spl27_7 <=> ! [X1] : (~ releaselg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_7])])).
fof(f74038, plain, (spl27_48 | ~ spl27_644 | spl27_797), inference(avatar_split_clause, [], [f74037, f13777, f9224, f1598])).
fof(f1598, plain, (spl27_48 <=> ! [X16] : (conditionnormo(X16) | gt(n0, X16))), introduced(avatar_definition, [new_symbols(naming, [spl27_48])])).
fof(f13777, plain, (spl27_797 <=> gt(n0, sK24(n0))), introduced(avatar_definition, [new_symbols(naming, [spl27_797])])).
fof(f74037, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0)) | (~ spl27_644 | spl27_797)), inference(subsumption_resolution, [], [f74036, f13778])).
fof(f13778, plain, (~ gt(n0, sK24(n0)) | spl27_797), inference(avatar_component_clause, [], [f13777])).
fof(f74036, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_644), inference(subsumption_resolution, [], [f52711, f296])).
fof(f52711, plain, (! [X0] : (gt(n0, X0) | ~ s1(n0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_644), inference(resolution, [], [f9226, f903])).
fof(f903, plain, ! [X47, X48] : (~ bcapacityex(sK24(X47)) | gt(X47, X48) | ~ s1(X47) | conditionnormo(X48) | gt(n0, sK24(X47))), inference(subsumption_resolution, [], [f902, f289])).
fof(f289, plain, ! [X0, X3] : (gt(X0, X3) | conditionnormo(X3) | s2(sK24(X0)) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f902, plain, ! [X47, X48] : (conditionnormo(X48) | gt(X47, X48) | ~ s1(X47) | ~ bcapacityex(sK24(X47)) | ~ s2(sK24(X47)) | gt(n0, sK24(X47))), inference(subsumption_resolution, [], [f900, f303])).
fof(f303, plain, ! [X0] : (~ conditionhyper(sK26(X0)) | ~ bcapacityex(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f900, plain, ! [X47, X48] : (conditionhyper(sK26(sK24(X47))) | conditionnormo(X48) | gt(X47, X48) | ~ s1(X47) | ~ bcapacityex(sK24(X47)) | ~ s2(sK24(X47)) | gt(n0, sK24(X47))), inference(resolution, [], [f290, f302])).
fof(f302, plain, ! [X0] : (gt(X0, sK26(X0)) | ~ bcapacityex(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f290, plain, ! [X2, X0, X3] : (~ gt(sK24(X0), X2) | conditionhyper(X2) | conditionnormo(X3) | gt(X0, X3) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f9226, plain, (bcapacityex(sK24(n0)) | ~ spl27_644), inference(avatar_component_clause, [], [f9224])).
fof(f57821, plain, (~ spl27_165 | ~ spl27_984 | spl27_985), inference(avatar_contradiction_clause, [], [f57820])).
fof(f57820, plain, ($false | (~ spl27_165 | ~ spl27_984 | spl27_985)), inference(subsumption_resolution, [], [f57819, f22981])).
fof(f22981, plain, (conditionhyper(n0) | (~ spl27_165 | spl27_985)), inference(resolution, [], [f22568, f3005])).
fof(f3005, plain, (! [X1] : (drugi(X1) | conditionhyper(X1)) | ~ spl27_165), inference(avatar_component_clause, [], [f3004])).
fof(f22568, plain, (~ drugi(n0) | spl27_985), inference(avatar_component_clause, [], [f22567])).
fof(f22567, plain, (spl27_985 <=> drugi(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_985])])).
fof(f57819, plain, (~ conditionhyper(n0) | ~ spl27_984), inference(resolution, [], [f22564, f202])).
fof(f22564, plain, (conditionnormo(n0) | ~ spl27_984), inference(avatar_component_clause, [], [f22562])).
fof(f22562, plain, (spl27_984 <=> conditionnormo(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_984])])).
fof(f57665, plain, (spl27_48 | spl27_984 | spl27_48), inference(avatar_split_clause, [], [f57465, f1598, f22562, f1598])).
fof(f57465, plain, ! [X2, X1] : (gt(n0, X1) | conditionnormo(X1) | conditionnormo(n0) | conditionnormo(X2) | gt(n0, X2)), inference(subsumption_resolution, [], [f57461, f296])).
fof(f57461, plain, ! [X2, X1] : (gt(n0, X1) | conditionnormo(X1) | ~ s1(n0) | conditionnormo(n0) | conditionnormo(X2) | gt(n0, X2)), inference(duplicate_literal_removal, [], [f57303])).
fof(f57303, plain, ! [X2, X1] : (gt(n0, X1) | conditionnormo(X1) | ~ s1(n0) | conditionnormo(n0) | conditionnormo(X2) | gt(n0, X2) | ~ s1(n0)), inference(resolution, [], [f7471, f288])).
fof(f288, plain, ! [X0, X3] : (~ gt(X0, sK24(X0)) | conditionnormo(X3) | gt(X0, X3) | ~ s1(X0)), inference(cnf_transformation, [], [f187])).
fof(f7471, plain, ! [X0, X1] : (gt(n0, sK24(X0)) | gt(X0, X1) | conditionnormo(X1) | ~ s1(X0) | conditionnormo(X0)), inference(subsumption_resolution, [], [f7470, f905])).
fof(f905, plain, ! [X50, X49] : (~ bcapacityne(sK24(X49)) | gt(X49, X50) | ~ s1(X49) | conditionnormo(X50) | gt(n0, sK24(X49))), inference(subsumption_resolution, [], [f904, f289])).
fof(f904, plain, ! [X50, X49] : (conditionnormo(X50) | gt(X49, X50) | ~ s1(X49) | ~ bcapacityne(sK24(X49)) | ~ s2(sK24(X49)) | gt(n0, sK24(X49))), inference(subsumption_resolution, [], [f901, f301])).
fof(f301, plain, ! [X0] : (~ conditionhyper(sK26(X0)) | ~ bcapacityne(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f901, plain, ! [X50, X49] : (conditionhyper(sK26(sK24(X49))) | conditionnormo(X50) | gt(X49, X50) | ~ s1(X49) | ~ bcapacityne(sK24(X49)) | ~ s2(sK24(X49)) | gt(n0, sK24(X49))), inference(resolution, [], [f290, f300])).
fof(f300, plain, ! [X0] : (gt(X0, sK26(X0)) | ~ bcapacityne(X0) | ~ s2(X0) | gt(n0, X0)), inference(cnf_transformation, [], [f193])).
fof(f7470, plain, ! [X0, X1] : (gt(X0, X1) | ~ s1(X0) | conditionnormo(X1) | gt(n0, sK24(X0)) | conditionnormo(X0) | bcapacityne(sK24(X0))), inference(duplicate_literal_removal, [], [f7469])).
fof(f7469, plain, ! [X0, X1] : (gt(X0, X1) | ~ s1(X0) | conditionnormo(X1) | gt(n0, sK24(X0)) | conditionnormo(X0) | bcapacityne(sK24(X0)) | ~ s1(X0)), inference(resolution, [], [f903, f907])).
fof(f907, plain, ! [X0] : (bcapacityex(sK24(X0)) | conditionnormo(X0) | bcapacityne(sK24(X0)) | ~ s1(X0)), inference(resolution, [], [f291, f194])).
fof(f53538, plain, (spl27_48 | ~ spl27_797), inference(avatar_split_clause, [], [f53537, f13777, f1598])).
fof(f53537, plain, (! [X0] : (conditionnormo(X0) | gt(n0, X0)) | ~ spl27_797), inference(subsumption_resolution, [], [f53485, f296])).
fof(f53485, plain, (! [X0] : (conditionnormo(X0) | gt(n0, X0) | ~ s1(n0)) | ~ spl27_797), inference(resolution, [], [f13779, f288])).
fof(f13779, plain, (gt(n0, sK24(n0)) | ~ spl27_797), inference(avatar_component_clause, [], [f13777])).
fof(f26869, plain, (spl27_13 | spl27_5 | ~ spl27_343), inference(avatar_split_clause, [], [f19126, f7852, f354, f442])).
fof(f442, plain, (spl27_13 <=> ! [X1] : (drugbg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl27_13])])).
fof(f354, plain, (spl27_5 <=> ! [X1] : ~ sP3(X1)), introduced(avatar_definition, [new_symbols(naming, [spl27_5])])).
fof(f7852, plain, (spl27_343 <=> ! [X15, X14] : (gt(X14, X15) | ~ sP3(X14) | drugbg(X15))), introduced(avatar_definition, [new_symbols(naming, [spl27_343])])).
fof(f19126, plain, (! [X6, X5] : (~ sP3(X5) | drugbg(X6) | conditionhyper(X6)) | ~ spl27_343), inference(duplicate_literal_removal, [], [f19067])).
fof(f19067, plain, (! [X6, X5] : (~ sP3(X5) | drugbg(X6) | conditionhyper(X6) | ~ sP3(X5)) | ~ spl27_343), inference(resolution, [], [f7853, f247])).
fof(f247, plain, ! [X0, X1] : (~ gt(X0, X1) | conditionhyper(X1) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ! [X2] : (bsecretioni(X2) | gt(X0, X2)) & bcapacityne(X0) & (! [X3] : (uptakepg(X3) | gt(X0, X3)) | ! [X4] : (~ releaselg(X4) | gt(X0, X4)))) | ~ sP3(X0)), inference(rectify, [], [f163])).
fof(f163, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(nnf_transformation, [], [f130])).
fof(f7853, plain, (! [X14, X15] : (gt(X14, X15) | ~ sP3(X14) | drugbg(X15)) | ~ spl27_343), inference(avatar_component_clause, [], [f7852])).
fof(f24659, plain, (spl27_7 | spl27_5 | ~ spl27_29), inference(avatar_split_clause, [], [f19191, f839, f354, f369])).
fof(f839, plain, (spl27_29 <=> ! [X15, X14] : (gt(X14, X15) | ~ sP3(X14) | ~ releaselg(X15))), introduced(avatar_definition, [new_symbols(naming, [spl27_29])])).
fof(f19191, plain, (! [X6, X5] : (~ sP3(X5) | ~ releaselg(X6) | conditionhyper(X6)) | ~ spl27_29), inference(duplicate_literal_removal, [], [f19132])).
fof(f19132, plain, (! [X6, X5] : (~ sP3(X5) | ~ releaselg(X6) | conditionhyper(X6) | ~ sP3(X5)) | ~ spl27_29), inference(resolution, [], [f840, f247])).
fof(f840, plain, (! [X14, X15] : (gt(X14, X15) | ~ sP3(X14) | ~ releaselg(X15)) | ~ spl27_29), inference(avatar_component_clause, [], [f839])).
fof(f22797, plain, (spl27_10 | ~ spl27_985), inference(avatar_contradiction_clause, [], [f22796])).
fof(f22796, plain, ($false | (spl27_10 | ~ spl27_985)), inference(subsumption_resolution, [], [f22795, f418])).
fof(f418, plain, (~ s3(n0) | spl27_10), inference(avatar_component_clause, [], [f417])).
fof(f417, plain, (spl27_10 <=> s3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_10])])).
fof(f22795, plain, (s3(n0) | ~ spl27_985), inference(resolution, [], [f22569, f273])).
fof(f273, plain, ! [X0] : (~ drugi(X0) | s3(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : (s3(X0) | ~ drugi(X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (drugi(X0) => s3(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X3] : (drugi(X3) => s3(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', insulincomp)).
fof(f22569, plain, (drugi(n0) | ~ spl27_985), inference(avatar_component_clause, [], [f22567])).
fof(f22591, plain, (spl27_9 | ~ spl27_13 | ~ spl27_984), inference(avatar_contradiction_clause, [], [f22590])).
fof(f22590, plain, ($false | (spl27_9 | ~ spl27_13 | ~ spl27_984)), inference(subsumption_resolution, [], [f22589, f11981])).
fof(f11981, plain, (conditionhyper(n0) | (spl27_9 | ~ spl27_13)), inference(resolution, [], [f415, f443])).
fof(f443, plain, (! [X1] : (drugbg(X1) | conditionhyper(X1)) | ~ spl27_13), inference(avatar_component_clause, [], [f442])).
fof(f415, plain, (~ drugbg(n0) | spl27_9), inference(avatar_component_clause, [], [f413])).
fof(f413, plain, (spl27_9 <=> drugbg(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_9])])).
fof(f22589, plain, (~ conditionhyper(n0) | ~ spl27_984), inference(resolution, [], [f22564, f202])).
fof(f18402, plain, (spl27_9 | ~ spl27_72), inference(avatar_contradiction_clause, [], [f18401])).
fof(f18401, plain, ($false | (spl27_9 | ~ spl27_72)), inference(subsumption_resolution, [], [f18343, f415])).
fof(f18343, plain, (drugbg(n0) | ~ spl27_72), inference(resolution, [], [f1717, f194])).
fof(f1717, plain, (! [X28] : (gt(n0, X28) | drugbg(X28)) | ~ spl27_72), inference(avatar_component_clause, [], [f1716])).
fof(f18242, plain, (spl27_34 | ~ spl27_61), inference(avatar_split_clause, [], [f18241, f1669, f1538])).
fof(f1669, plain, (spl27_61 <=> ! [X25] : (gt(n0, X25) | drugi(X25))), introduced(avatar_definition, [new_symbols(naming, [spl27_61])])).
fof(f18241, plain, (! [X43] : (gt(n0, X43) | uptakelg(X43)) | ~ spl27_61), inference(subsumption_resolution, [], [f18204, f205])).
fof(f205, plain, ! [X0, X1] : (~ drugi(sK4(X0)) | gt(X0, X1) | uptakelg(X1)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (! [X1] : ((uptakepg(X1) & uptakelg(X1)) | gt(X0, X1)) | (~ drugi(sK4(X0)) & ~ gt(X0, sK4(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f132, f133])).
fof(f133, plain, ! [X0] : (? [X2] : (~ drugi(X2) & ~ gt(X0, X2)) => (~ drugi(sK4(X0)) & ~ gt(X0, sK4(X0)))), introduced(choice_axiom, [])).
fof(f132, plain, ! [X0] : (! [X1] : ((uptakepg(X1) & uptakelg(X1)) | gt(X0, X1)) | ? [X2] : (~ drugi(X2) & ~ gt(X0, X2))), inference(rectify, [], [f85])).
fof(f85, plain, ! [X0] : (! [X2] : ((uptakepg(X2) & uptakelg(X2)) | gt(X0, X2)) | ? [X1] : (~ drugi(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => drugi(X1)) => ! [X2] : (~ gt(X0, X2) => (uptakepg(X2) & uptakelg(X2)))), inference(rectify, [], [f11])).
fof(f11, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => drugi(X4)) => ! [X4] : (~ gt(X3, X4) => (uptakepg(X4) & uptakelg(X4)))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', insulin_effect)).
fof(f18204, plain, (! [X43] : (drugi(sK4(n0)) | gt(n0, X43) | uptakelg(X43)) | ~ spl27_61), inference(resolution, [], [f1670, f204])).
fof(f204, plain, ! [X0, X1] : (~ gt(X0, sK4(X0)) | gt(X0, X1) | uptakelg(X1)), inference(cnf_transformation, [], [f134])).
fof(f1670, plain, (! [X25] : (gt(n0, X25) | drugi(X25)) | ~ spl27_61), inference(avatar_component_clause, [], [f1669])).
fof(f13780, plain, (spl27_797 | spl27_48 | ~ spl27_77), inference(avatar_split_clause, [], [f13775, f1737, f1598, f13777])).
fof(f13775, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_77), inference(subsumption_resolution, [], [f13774, f296])).
fof(f13774, plain, (! [X0] : (gt(n0, X0) | ~ s1(n0) | conditionnormo(X0) | gt(n0, sK24(n0))) | ~ spl27_77), inference(resolution, [], [f1738, f905])).
fof(f1738, plain, (bcapacityne(sK24(n0)) | ~ spl27_77), inference(avatar_component_clause, [], [f1737])).
fof(f12571, plain, ~ spl27_17, inference(avatar_contradiction_clause, [], [f12570])).
fof(f12570, plain, ($false | ~ spl27_17), inference(subsumption_resolution, [], [f12559, f298])).
fof(f298, plain, ~ bcapacitysn(n0), inference(cnf_transformation, [], [f193])).
fof(f12559, plain, (bcapacitysn(n0) | ~ spl27_17), inference(resolution, [], [f537, f253])).
fof(f253, plain, ! [X0] : (~ sP1(X0) | bcapacitysn(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & qilt27(X0) & bcapacitysn(X0) & ! [X2] : (bsecretioni(X2) | gt(X0, X2))) | ~ sP1(X0)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(nnf_transformation, [], [f128])).
fof(f537, plain, (sP1(n0) | ~ spl27_17), inference(avatar_component_clause, [], [f536])).
fof(f11980, plain, ~ spl27_11, inference(avatar_contradiction_clause, [], [f11979])).
fof(f11979, plain, ($false | ~ spl27_11), inference(subsumption_resolution, [], [f11977, f296])).
fof(f11977, plain, (~ s1(n0) | ~ spl27_11), inference(resolution, [], [f423, f241])).
fof(f241, plain, ! [X0] : (~ s2(X0) | ~ s1(X0)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : (~ s2(X0) | ~ s1(X0)), inference(rectify, [], [f23])).
fof(f23, plain, ! [X3] : (~ s2(X3) | ~ s1(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', xorstep5)).
fof(f423, plain, (s2(n0) | ~ spl27_11), inference(avatar_component_clause, [], [f421])).
fof(f421, plain, (spl27_11 <=> s2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl27_11])])).
fof(f11949, plain, (spl27_165 | spl27_77 | spl27_644 | spl27_702), inference(avatar_split_clause, [], [f11944, f11946, f9224, f1737, f3004])).
fof(f11944, plain, ! [X1] : (conditionnormo(sK19(n0)) | bcapacityex(sK24(n0)) | bcapacityne(sK24(n0)) | drugi(X1) | conditionhyper(X1)), inference(subsumption_resolution, [], [f10899, f296])).
fof(f10899, plain, ! [X1] : (conditionnormo(sK19(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | bcapacityne(sK24(n0)) | drugi(X1) | conditionhyper(X1)), inference(resolution, [], [f923, f297])).
fof(f923, plain, ! [X30, X31] : (gt(X30, X31) | conditionnormo(sK19(X30)) | bcapacityex(sK24(X30)) | ~ s1(X30) | bcapacityne(sK24(X30)) | drugi(X31)), inference(resolution, [], [f291, f276])).
fof(f276, plain, ! [X0, X1] : (~ gt(X0, sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f11905, plain, (spl27_165 | spl27_77 | spl27_644 | spl27_692), inference(avatar_split_clause, [], [f11900, f11902, f9224, f1737, f3004])).
fof(f11900, plain, ! [X1] : (conditionnormo(sK20(n0)) | bcapacityex(sK24(n0)) | bcapacityne(sK24(n0)) | drugi(X1) | conditionhyper(X1)), inference(subsumption_resolution, [], [f10962, f296])).
fof(f10962, plain, ! [X1] : (conditionnormo(sK20(n0)) | bcapacityex(sK24(n0)) | ~ s1(n0) | bcapacityne(sK24(n0)) | drugi(X1) | conditionhyper(X1)), inference(resolution, [], [f924, f297])).
fof(f924, plain, ! [X33, X32] : (gt(X32, X33) | conditionnormo(sK20(X32)) | bcapacityex(sK24(X32)) | ~ s1(X32) | bcapacityne(sK24(X32)) | drugi(X33)), inference(resolution, [], [f291, f274])).
fof(f274, plain, ! [X0, X1] : (~ gt(X0, sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f9312, plain, ~ spl27_10, inference(avatar_contradiction_clause, [], [f9311])).
fof(f9311, plain, ($false | ~ spl27_10), inference(subsumption_resolution, [], [f9309, f296])).
fof(f9309, plain, (~ s1(n0) | ~ spl27_10), inference(resolution, [], [f419, f242])).
fof(f242, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(rectify, [], [f24])).
fof(f24, plain, ! [X3] : (~ s3(X3) | ~ s1(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', xorstep6)).
fof(f419, plain, (s3(n0) | ~ spl27_10), inference(avatar_component_clause, [], [f417])).
fof(f9304, plain, ~ spl27_6, inference(avatar_contradiction_clause, [], [f9303])).
fof(f9303, plain, ($false | ~ spl27_6), inference(subsumption_resolution, [], [f9292, f299])).
fof(f299, plain, qilt27(n0), inference(cnf_transformation, [], [f193])).
fof(f9292, plain, (~ qilt27(n0) | ~ spl27_6), inference(resolution, [], [f366, f250])).
fof(f250, plain, ! [X0] : (~ sP2(X0) | ~ qilt27(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X2] : (~ releaselg(X2) | gt(X0, X2))) | ~ sP2(X0)), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(nnf_transformation, [], [f129])).
fof(f366, plain, (sP2(n0) | ~ spl27_6), inference(avatar_component_clause, [], [f365])).
fof(f7968, plain, (spl27_165 | spl27_29), inference(avatar_split_clause, [], [f4638, f839, f3004])).
fof(f4638, plain, ! [X10, X8, X9] : (gt(X8, X9) | ~ sP3(X8) | ~ releaselg(X9) | drugi(X10) | conditionhyper(X10)), inference(duplicate_literal_removal, [], [f4541])).
fof(f4541, plain, ! [X10, X8, X9] : (gt(X8, X9) | ~ sP3(X8) | ~ releaselg(X9) | drugi(X10) | conditionhyper(X10) | ~ sP3(X8)), inference(resolution, [], [f843, f247])).
fof(f843, plain, ! [X37, X38, X36] : (gt(X36, X38) | gt(X36, X37) | ~ sP3(X36) | ~ releaselg(X37) | drugi(X38)), inference(subsumption_resolution, [], [f818, f277])).
fof(f818, plain, ! [X37, X38, X36] : (gt(X36, X37) | ~ releaselg(X37) | uptakepg(sK19(X36)) | ~ sP3(X36) | gt(X36, X38) | drugi(X38)), inference(resolution, [], [f244, f276])).
fof(f244, plain, ! [X4, X0, X3] : (gt(X0, X4) | gt(X0, X3) | ~ releaselg(X4) | uptakepg(X3) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f7934, plain, (~ spl27_48 | ~ spl27_165), inference(avatar_contradiction_clause, [], [f7933])).
fof(f7933, plain, ($false | (~ spl27_48 | ~ spl27_165)), inference(subsumption_resolution, [], [f7080, f7930])).
fof(f7930, plain, (~ conditionhyper(n0) | ~ spl27_48), inference(resolution, [], [f7473, f202])).
fof(f7473, plain, (conditionnormo(n0) | ~ spl27_48), inference(resolution, [], [f1599, f194])).
fof(f1599, plain, (! [X16] : (gt(n0, X16) | conditionnormo(X16)) | ~ spl27_48), inference(avatar_component_clause, [], [f1598])).
fof(f7080, plain, (conditionhyper(n0) | ~ spl27_165), inference(resolution, [], [f6508, f296])).
fof(f6508, plain, (! [X3] : (~ s1(X3) | conditionhyper(X3)) | ~ spl27_165), inference(resolution, [], [f5713, f242])).
fof(f5713, plain, (! [X0] : (s3(X0) | conditionhyper(X0)) | ~ spl27_165), inference(resolution, [], [f3005, f273])).
fof(f7932, plain, (spl27_9 | ~ spl27_13 | ~ spl27_48), inference(avatar_contradiction_clause, [], [f7931])).
fof(f7931, plain, ($false | (spl27_9 | ~ spl27_13 | ~ spl27_48)), inference(subsumption_resolution, [], [f7930, f5432])).
fof(f5432, plain, (conditionhyper(n0) | (spl27_9 | ~ spl27_13)), inference(resolution, [], [f443, f415])).
fof(f7898, plain, (spl27_343 | ~ spl27_29), inference(avatar_split_clause, [], [f7897, f839, f7852])).
fof(f7897, plain, (! [X74, X75] : (~ sP3(X74) | gt(X74, X75) | drugbg(X75)) | ~ spl27_29), inference(subsumption_resolution, [], [f2572, f284])).
fof(f284, plain, ! [X0, X1] : (gt(X0, X1) | drugbg(X1) | releaselg(sK22(X0))), inference(cnf_transformation, [], [f181])).
fof(f2572, plain, (! [X74, X75] : (~ sP3(X74) | ~ releaselg(sK22(X74)) | gt(X74, X75) | drugbg(X75)) | ~ spl27_29), inference(resolution, [], [f840, f283])).
fof(f7551, plain, (~ spl27_3 | ~ spl27_5), inference(avatar_contradiction_clause, [], [f7550])).
fof(f7550, plain, ($false | (~ spl27_3 | ~ spl27_5)), inference(subsumption_resolution, [], [f343, f355])).
fof(f355, plain, (! [X1] : ~ sP3(X1) | ~ spl27_5), inference(avatar_component_clause, [], [f354])).
fof(f343, plain, (sP3(n0) | ~ spl27_3), inference(avatar_component_clause, [], [f342])).
fof(f7536, plain, (spl27_17 | spl27_6 | spl27_34 | spl27_284 | spl27_3 | ~ spl27_48), inference(avatar_split_clause, [], [f7535, f1598, f342, f7531, f1538, f365, f536])).
fof(f7535, plain, (! [X46] : (conditionnormo(sK18(n0)) | gt(n0, X46) | sP2(n0) | sP1(n0) | uptakelg(X46)) | (spl27_3 | ~ spl27_48)), inference(subsumption_resolution, [], [f7512, f344])).
fof(f7512, plain, (! [X46] : (conditionnormo(sK18(n0)) | gt(n0, X46) | sP3(n0) | sP2(n0) | sP1(n0) | uptakelg(X46)) | ~ spl27_48), inference(resolution, [], [f1599, f256])).
fof(f256, plain, ! [X0, X3] : (~ gt(X0, sK18(X0)) | gt(X0, X3) | sP3(X0) | sP2(X0) | sP1(X0) | uptakelg(X3)), inference(cnf_transformation, [], [f171])).
fof(f7431, plain, (spl27_61 | spl27_281), inference(avatar_split_clause, [], [f7430, f7173, f1669])).
fof(f7430, plain, (! [X0] : (gt(n0, X0) | drugi(X0)) | spl27_281), inference(resolution, [], [f7311, f275])).
fof(f275, plain, ! [X0, X1] : (~ uptakelg(sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f7311, plain, (uptakelg(sK20(n0)) | spl27_281), inference(resolution, [], [f7175, f396])).
fof(f396, plain, ! [X0] : (releaselg(X0) | uptakelg(X0)), inference(resolution, [], [f278, f194])).
fof(f278, plain, ! [X0, X1] : (gt(X0, X1) | releaselg(X1) | uptakelg(X1)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : (uptakelg(X1) | releaselg(X1) | gt(X0, X1)), inference(flattening, [], [f114])).
fof(f114, plain, ! [X0, X1] : ((uptakelg(X1) | releaselg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1] : (~ gt(X0, X1) => (~ releaselg(X1) => uptakelg(X1))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X3, X4] : (~ gt(X3, X4) => (~ releaselg(X4) => uptakelg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', uptake_completion)).
fof(f7175, plain, (~ releaselg(sK20(n0)) | spl27_281), inference(avatar_component_clause, [], [f7173])).
fof(f444, plain, (spl27_12 | spl27_13), inference(avatar_split_clause, [], [f430, f442, f438])).
fof(f430, plain, ! [X1] : (drugbg(X1) | releaselg(sK22(n0)) | conditionhyper(X1)), inference(resolution, [], [f284, f297])).
fof(f424, plain, (~ spl27_9 | spl27_10 | spl27_11), inference(avatar_split_clause, [], [f411, f421, f417, f413])).
fof(f411, plain, (s2(n0) | s3(n0) | ~ drugbg(n0)), inference(resolution, [], [f271, f299])).
fof(f271, plain, ! [X0] : (~ qilt27(X0) | s2(X0) | s3(X0) | ~ drugbg(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0] : (s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0)) | ~ drugbg(X0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0] : ((s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0))) | ~ drugbg(X0)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0] : (drugbg(X0) => (s3(X0) | s2(X0) | (~ qilt27(X0) & s1(X0)))), inference(rectify, [], [f31])).
fof(f31, plain, ! [X3] : (drugbg(X3) => (s3(X3) | s2(X3) | (~ qilt27(X3) & s1(X3)))), file('/home/ubuntu/library/tptp/Problems/MED/MED007+1.p', bgcomp)).