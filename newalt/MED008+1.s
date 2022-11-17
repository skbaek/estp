fof(f2337, plain, $false, inference(avatar_sat_refutation, [], [f556, f997, f1019, f1041, f1750, f1775, f1820, f2074, f2226, f2336])).
fof(f2336, plain, ~ spl26_14, inference(avatar_contradiction_clause, [], [f2335])).
fof(f2335, plain, ($false | ~ spl26_14), inference(subsumption_resolution, [], [f2332, f296])).
fof(f296, plain, ~ bcapacitysn(n0), inference(cnf_transformation, [], [f191])).
fof(f191, plain, (! [X0] : (conditionnormo(X0) | gt(n0, X0)) & ~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X1] : (conditionhyper(X1) | ~ gt(n0, X1)) & s1(n0)), inference(rectify, [], [f125])).
fof(f125, plain, (! [X1] : (conditionnormo(X1) | gt(n0, X1)) & ~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0)), inference(flattening, [], [f124])).
fof(f124, plain, (! [X1] : (conditionnormo(X1) | gt(n0, X1)) & (~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & s1(n0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ~ ((~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X0] : (gt(n0, X0) => conditionhyper(X0)) & s1(n0)) => ~ ! [X1] : (~ gt(n0, X1) => conditionnormo(X1))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ~ ! [X3] : (~ gt(n0, X3) => conditionnormo(X3))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((~ qilt27(n0) & ~ bcapacitysn(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & s1(n0)) => ~ ! [X3] : (~ gt(n0, X3) => conditionnormo(X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', unsuccessfuls1_qige27)).
fof(f2332, plain, (bcapacitysn(n0) | ~ spl26_14), inference(resolution, [], [f450, f251])).
fof(f251, plain, ! [X0] : (~ sP1(X0) | bcapacitysn(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & qilt27(X0) & bcapacitysn(X0) & ! [X2] : (bsecretioni(X2) | gt(X0, X2))) | ~ sP1(X0)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(nnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : ((! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ~ sP1(X0)), inference(usedef, [], [e128])).
fof(e128, plain, ! [X0] : (sP1(X0) <=> (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f450, plain, (sP1(n0) | ~ spl26_14), inference(avatar_component_clause, [], [f449])).
fof(f449, plain, (spl26_14 <=> sP1(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_14])])).
fof(f2226, plain, (spl26_25 | ~ spl26_36), inference(avatar_split_clause, [], [f2225, f1017, f554])).
fof(f554, plain, (spl26_25 <=> ! [X0] : (gt(n0, X0) | drugi(X0))), introduced(avatar_definition, [new_symbols(naming, [spl26_25])])).
fof(f1017, plain, (spl26_36 <=> ! [X0] : (gt(n0, X0) | uptakepg(X0))), introduced(avatar_definition, [new_symbols(naming, [spl26_36])])).
fof(f2225, plain, (! [X26] : (gt(n0, X26) | drugi(X26)) | ~ spl26_36), inference(subsumption_resolution, [], [f2201, f275])).
fof(f275, plain, ! [X0, X1] : (~ uptakepg(sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | ((~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0))) & (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0))))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f172, f174, f173])).
fof(f173, plain, ! [X0] : (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) => (~ uptakepg(sK19(X0)) & ~ gt(X0, sK19(X0)))), introduced(choice_axiom, [])).
fof(f174, plain, ! [X0] : (? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)) => (~ uptakelg(sK20(X0)) & ~ gt(X0, sK20(X0)))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0] : (! [X1] : (drugi(X1) | gt(X0, X1)) | (? [X2] : (~ uptakepg(X2) & ~ gt(X0, X2)) & ? [X3] : (~ uptakelg(X3) & ~ gt(X0, X3)))), inference(rectify, [], [f113])).
fof(f113, plain, ! [X0] : (! [X3] : (drugi(X3) | gt(X0, X3)) | (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (~ uptakelg(X2) & ~ gt(X0, X2)))), inference(ennf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : ((! [X1] : (~ gt(X0, X1) => uptakepg(X1)) | ! [X2] : (~ gt(X0, X2) => uptakelg(X2))) => ! [X3] : (~ gt(X0, X3) => drugi(X3))), inference(rectify, [], [f34])).
fof(f34, plain, ! [X3] : ((! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) => ! [X4] : (~ gt(X3, X4) => drugi(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', insulin_completion)).
fof(f2201, plain, (! [X26] : (uptakepg(sK19(n0)) | gt(n0, X26) | drugi(X26)) | ~ spl26_36), inference(resolution, [], [f1018, f274])).
fof(f274, plain, ! [X0, X1] : (~ gt(X0, sK19(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f1018, plain, (! [X0] : (gt(n0, X0) | uptakepg(X0)) | ~ spl26_36), inference(avatar_component_clause, [], [f1017])).
fof(f2074, plain, ~ spl26_25, inference(avatar_contradiction_clause, [], [f2073])).
fof(f2073, plain, ($false | ~ spl26_25), inference(subsumption_resolution, [], [f2071, f294])).
fof(f294, plain, s1(n0), inference(cnf_transformation, [], [f191])).
fof(f2071, plain, (~ s1(n0) | ~ spl26_25), inference(resolution, [], [f1992, f240])).
fof(f240, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (~ s3(X0) | ~ s1(X0)), inference(rectify, [], [f24])).
fof(f24, plain, ! [X3] : (~ s3(X3) | ~ s1(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', xorstep6)).
fof(f1992, plain, (s3(n0) | ~ spl26_25), inference(resolution, [], [f1948, f271])).
fof(f271, plain, ! [X0] : (~ drugi(X0) | s3(X0)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : (s3(X0) | ~ drugi(X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (drugi(X0) => s3(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X3] : (drugi(X3) => s3(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', insulincomp)).
fof(f1948, plain, (drugi(n0) | ~ spl26_25), inference(resolution, [], [f555, f192])).
fof(f192, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', irreflexivity_gt)).
fof(f555, plain, (! [X0] : (gt(n0, X0) | drugi(X0)) | ~ spl26_25), inference(avatar_component_clause, [], [f554])).
fof(f1820, plain, (spl26_9 | spl26_36 | ~ spl26_5), inference(avatar_split_clause, [], [f1819, f359, f1017, f386])).
fof(f386, plain, (spl26_9 <=> ! [X1] : (~ releaselg(X1) | conditionhyper(X1))), introduced(avatar_definition, [new_symbols(naming, [spl26_9])])).
fof(f359, plain, (spl26_5 <=> sP3(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_5])])).
fof(f1819, plain, (! [X2, X3] : (gt(n0, X2) | ~ releaselg(X3) | uptakepg(X2) | conditionhyper(X3)) | ~ spl26_5), inference(subsumption_resolution, [], [f784, f360])).
fof(f360, plain, (sP3(n0) | ~ spl26_5), inference(avatar_component_clause, [], [f359])).
fof(f784, plain, ! [X2, X3] : (gt(n0, X2) | ~ releaselg(X3) | uptakepg(X2) | ~ sP3(n0) | conditionhyper(X3)), inference(resolution, [], [f242, f295])).
fof(f295, plain, ! [X1] : (~ gt(n0, X1) | conditionhyper(X1)), inference(cnf_transformation, [], [f191])).
fof(f242, plain, ! [X4, X0, X3] : (gt(X0, X4) | gt(X0, X3) | ~ releaselg(X4) | uptakepg(X3) | ~ sP3(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ! [X2] : (bsecretioni(X2) | gt(X0, X2)) & bcapacityne(X0) & (! [X3] : (uptakepg(X3) | gt(X0, X3)) | ! [X4] : (~ releaselg(X4) | gt(X0, X4)))) | ~ sP3(X0)), inference(rectify, [], [f163])).
fof(f163, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(nnf_transformation, [], [f130])).
fof(f130, plain, ! [X0] : ((! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | ~ sP3(X0)), inference(usedef, [], [e130])).
fof(e130, plain, ! [X0] : (sP3(X0) <=> (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f1775, plain, (~ spl26_9 | ~ spl26_24 | ~ spl26_50), inference(avatar_contradiction_clause, [], [f1774])).
fof(f1774, plain, ($false | (~ spl26_9 | ~ spl26_24 | ~ spl26_50)), inference(subsumption_resolution, [], [f1773, f558])).
fof(f558, plain, (~ conditionhyper(sK20(n0)) | ~ spl26_24), inference(resolution, [], [f552, f200])).
fof(f200, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (~ conditionnormo(X0) | ~ conditionhyper(X0)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3] : (~ conditionnormo(X3) | ~ conditionhyper(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', xorcondition3)).
fof(f552, plain, (conditionnormo(sK20(n0)) | ~ spl26_24), inference(avatar_component_clause, [], [f550])).
fof(f550, plain, (spl26_24 <=> conditionnormo(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl26_24])])).
fof(f1773, plain, (conditionhyper(sK20(n0)) | (~ spl26_9 | ~ spl26_50)), inference(resolution, [], [f1401, f387])).
fof(f387, plain, (! [X1] : (~ releaselg(X1) | conditionhyper(X1)) | ~ spl26_9), inference(avatar_component_clause, [], [f386])).
fof(f1401, plain, (releaselg(sK20(n0)) | ~ spl26_50), inference(avatar_component_clause, [], [f1400])).
fof(f1400, plain, (spl26_50 <=> releaselg(sK20(n0))), introduced(avatar_definition, [new_symbols(naming, [spl26_50])])).
fof(f1750, plain, (spl26_25 | spl26_50), inference(avatar_split_clause, [], [f1749, f1400, f554])).
fof(f1749, plain, (! [X0] : (gt(n0, X0) | drugi(X0)) | spl26_50), inference(resolution, [], [f1652, f273])).
fof(f273, plain, ! [X0, X1] : (~ uptakelg(sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).
fof(f1652, plain, (uptakelg(sK20(n0)) | spl26_50), inference(resolution, [], [f1402, f416])).
fof(f416, plain, ! [X0] : (releaselg(X0) | uptakelg(X0)), inference(resolution, [], [f276, f192])).
fof(f276, plain, ! [X0, X1] : (gt(X0, X1) | releaselg(X1) | uptakelg(X1)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : (uptakelg(X1) | releaselg(X1) | gt(X0, X1)), inference(flattening, [], [f114])).
fof(f114, plain, ! [X0, X1] : ((uptakelg(X1) | releaselg(X1)) | gt(X0, X1)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1] : (~ gt(X0, X1) => (~ releaselg(X1) => uptakelg(X1))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X3, X4] : (~ gt(X3, X4) => (~ releaselg(X4) => uptakelg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', uptake_completion)).
fof(f1402, plain, (~ releaselg(sK20(n0)) | spl26_50), inference(avatar_component_clause, [], [f1400])).
fof(f1041, plain, (spl26_36 | spl26_5 | spl26_8 | spl26_14 | ~ spl26_35), inference(avatar_split_clause, [], [f1040, f975, f449, f382, f359, f1017])).
fof(f382, plain, (spl26_8 <=> sP2(n0)), introduced(avatar_definition, [new_symbols(naming, [spl26_8])])).
fof(f975, plain, (spl26_35 <=> conditionnormo(sK18(n0))), introduced(avatar_definition, [new_symbols(naming, [spl26_35])])).
fof(f1040, plain, (! [X0] : (gt(n0, X0) | uptakepg(X0)) | (spl26_5 | spl26_8 | spl26_14 | ~ spl26_35)), inference(subsumption_resolution, [], [f1039, f451])).
fof(f451, plain, (~ sP1(n0) | spl26_14), inference(avatar_component_clause, [], [f449])).
fof(f1039, plain, (! [X0] : (gt(n0, X0) | sP1(n0) | uptakepg(X0)) | (spl26_5 | spl26_8 | ~ spl26_35)), inference(subsumption_resolution, [], [f1038, f384])).
fof(f384, plain, (~ sP2(n0) | spl26_8), inference(avatar_component_clause, [], [f382])).
fof(f1038, plain, (! [X0] : (gt(n0, X0) | sP2(n0) | sP1(n0) | uptakepg(X0)) | (spl26_5 | ~ spl26_35)), inference(subsumption_resolution, [], [f1032, f361])).
fof(f361, plain, (~ sP3(n0) | spl26_5), inference(avatar_component_clause, [], [f359])).
fof(f1032, plain, (! [X0] : (gt(n0, X0) | sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X0)) | ~ spl26_35), inference(resolution, [], [f977, f257])).
fof(f257, plain, ! [X2, X0] : (~ conditionnormo(sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & bcapacityex(X0) & ! [X2] : (uptakepg(X2) | gt(X0, X2)) & ! [X3] : (uptakelg(X3) | gt(X0, X3))) | sP3(X0) | sP2(X0) | sP1(X0) | (~ conditionnormo(sK18(X0)) & ~ gt(X0, sK18(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f169, f170])).
fof(f170, plain, ! [X0] : (? [X4] : (~ conditionnormo(X4) & ~ gt(X0, X4)) => (~ conditionnormo(sK18(X0)) & ~ gt(X0, sK18(X0)))), introduced(choice_axiom, [])).
fof(f169, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & bcapacityex(X0) & ! [X2] : (uptakepg(X2) | gt(X0, X2)) & ! [X3] : (uptakelg(X3) | gt(X0, X3))) | sP3(X0) | sP2(X0) | sP1(X0) | ? [X4] : (~ conditionnormo(X4) & ~ gt(X0, X4))), inference(rectify, [], [f131])).
fof(f131, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | sP3(X0) | sP2(X0) | sP1(X0) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(definition_folding, [], [f100, e130, e129, e128])).
fof(f129, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(usedef, [], [e129])).
fof(e129, plain, ! [X0] : (sP2(X0) <=> (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f100, plain, ! [X0] : ((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12))) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(flattening, [], [f99])).
fof(f99, plain, ! [X0] : (((! [X2] : (conditionhyper(X2) | ~ gt(X0, X2)) & bcapacityex(X0) & ! [X3] : (uptakepg(X3) | gt(X0, X3)) & ! [X4] : (uptakelg(X4) | gt(X0, X4))) | (! [X5] : (conditionhyper(X5) | ~ gt(X0, X5)) & ! [X6] : (bsecretioni(X6) | gt(X0, X6)) & bcapacityne(X0) & (! [X7] : (uptakepg(X7) | gt(X0, X7)) | ! [X8] : (~ releaselg(X8) | gt(X0, X8)))) | (! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | (! [X11] : (conditionhyper(X11) | ~ gt(X0, X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (bsecretioni(X12) | gt(X0, X12)))) | ? [X1] : (~ conditionnormo(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => conditionnormo(X1)) => ((! [X2] : (gt(X0, X2) => conditionhyper(X2)) & bcapacityex(X0) & ! [X3] : (~ gt(X0, X3) => uptakepg(X3)) & ! [X4] : (~ gt(X0, X4) => uptakelg(X4))) | (! [X5] : (gt(X0, X5) => conditionhyper(X5)) & ! [X6] : (~ gt(X0, X6) => bsecretioni(X6)) & bcapacityne(X0) & (! [X7] : (~ gt(X0, X7) => uptakepg(X7)) | ! [X8] : (~ gt(X0, X8) => ~ releaselg(X8)))) | (! [X9] : (gt(X0, X9) => conditionhyper(X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ gt(X0, X10) => ~ releaselg(X10))) | (! [X11] : (gt(X0, X11) => conditionhyper(X11)) & qilt27(X0) & bcapacitysn(X0) & ! [X12] : (~ gt(X0, X12) => bsecretioni(X12))))), inference(rectify, [], [f26])).
fof(f26, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => conditionnormo(X4)) => ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => uptakepg(X4)) & ! [X4] : (~ gt(X3, X4) => uptakelg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ~ qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))) | (! [X4] : (gt(X3, X4) => conditionhyper(X4)) & qilt27(X3) & bcapacitysn(X3) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))))), file('/home/ubuntu/library/tptp/Problems/MED/MED008+1.p', normo)).
fof(f977, plain, (conditionnormo(sK18(n0)) | ~ spl26_35), inference(avatar_component_clause, [], [f975])).
fof(f1019, plain, (spl26_35 | spl26_36 | spl26_5 | spl26_8 | spl26_14), inference(avatar_split_clause, [], [f1015, f449, f382, f359, f1017, f975])).
fof(f1015, plain, (! [X0] : (gt(n0, X0) | uptakepg(X0) | conditionnormo(sK18(n0))) | (spl26_5 | spl26_8 | spl26_14)), inference(subsumption_resolution, [], [f1014, f451])).
fof(f1014, plain, (! [X0] : (gt(n0, X0) | sP1(n0) | uptakepg(X0) | conditionnormo(sK18(n0))) | (spl26_5 | spl26_8)), inference(subsumption_resolution, [], [f1013, f384])).
fof(f1013, plain, (! [X0] : (gt(n0, X0) | sP2(n0) | sP1(n0) | uptakepg(X0) | conditionnormo(sK18(n0))) | spl26_5), inference(subsumption_resolution, [], [f998, f361])).
fof(f998, plain, ! [X0] : (gt(n0, X0) | sP3(n0) | sP2(n0) | sP1(n0) | uptakepg(X0) | conditionnormo(sK18(n0))), inference(resolution, [], [f256, f298])).
fof(f298, plain, ! [X0] : (gt(n0, X0) | conditionnormo(X0)), inference(cnf_transformation, [], [f191])).
fof(f256, plain, ! [X2, X0] : (~ gt(X0, sK18(X0)) | gt(X0, X2) | sP3(X0) | sP2(X0) | sP1(X0) | uptakepg(X2)), inference(cnf_transformation, [], [f171])).
fof(f997, plain, ~ spl26_8, inference(avatar_contradiction_clause, [], [f996])).
fof(f996, plain, ($false | ~ spl26_8), inference(subsumption_resolution, [], [f995, f296])).
fof(f995, plain, (bcapacitysn(n0) | ~ spl26_8), inference(resolution, [], [f383, f247])).
fof(f247, plain, ! [X0] : (~ sP2(X0) | bcapacitysn(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((! [X1] : (conditionhyper(X1) | ~ gt(X0, X1)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X2] : (~ releaselg(X2) | gt(X0, X2))) | ~ sP2(X0)), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0] : ((! [X9] : (conditionhyper(X9) | ~ gt(X0, X9)) & ~ qilt27(X0) & bcapacitysn(X0) & ! [X10] : (~ releaselg(X10) | gt(X0, X10))) | ~ sP2(X0)), inference(nnf_transformation, [], [f129])).
fof(f383, plain, (sP2(n0) | ~ spl26_8), inference(avatar_component_clause, [], [f382])).
fof(f556, plain, (spl26_24 | spl26_25), inference(avatar_split_clause, [], [f542, f554, f550])).
fof(f542, plain, ! [X0] : (gt(n0, X0) | drugi(X0) | conditionnormo(sK20(n0))), inference(resolution, [], [f272, f298])).
fof(f272, plain, ! [X0, X1] : (~ gt(X0, sK20(X0)) | gt(X0, X1) | drugi(X1)), inference(cnf_transformation, [], [f175])).