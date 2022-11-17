fof(f646, plain, $false, inference(avatar_sat_refutation, [], [f249, f262, f270, f379, f538, f556, f568, f633, f642, f645])).
fof(f645, plain, (~ spl16_28 | spl16_23 | spl16_4 | ~ spl16_27), inference(avatar_split_clause, [], [f644, f372, f169, f308, f376])).
fof(f376, plain, (spl16_28 <=> conditionhyper(sK10(n0))), introduced(avatar_definition, [new_symbols(naming, [spl16_28])])).
fof(f308, plain, (spl16_23 <=> ! [X0] : (gt(n0, X0) | conditionnormo(X0))), introduced(avatar_definition, [new_symbols(naming, [spl16_23])])).
fof(f169, plain, (spl16_4 <=> sP0(n0)), introduced(avatar_definition, [new_symbols(naming, [spl16_4])])).
fof(f372, plain, (spl16_27 <=> bsecretioni(sK11(n0))), introduced(avatar_definition, [new_symbols(naming, [spl16_27])])).
fof(f644, plain, (! [X1] : (gt(n0, X1) | ~ conditionhyper(sK10(n0)) | conditionnormo(X1)) | (spl16_4 | ~ spl16_27)), inference(subsumption_resolution, [], [f643, f171])).
fof(f171, plain, (~ sP0(n0) | spl16_4), inference(avatar_component_clause, [], [f169])).
fof(f643, plain, (! [X1] : (gt(n0, X1) | ~ conditionhyper(sK10(n0)) | conditionnormo(X1) | sP0(n0)) | ~ spl16_27), inference(subsumption_resolution, [], [f630, f137])).
fof(f137, plain, bcapacityne(n0), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((~ conditionnormo(sK15) & ~ gt(n0, sK15)) & bcapacityne(n0) & ! [X1] : (conditionhyper(X1) | ~ gt(n0, X1)) & ! [X2] : ((drugsu(X2) & drugbg(X2)) | gt(n0, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15])], [f90, f91])).
fof(f91, plain, (? [X0] : (~ conditionnormo(X0) & ~ gt(n0, X0)) => (~ conditionnormo(sK15) & ~ gt(n0, sK15))), introduced(choice_axiom, [])).
fof(f90, plain, (? [X0] : (~ conditionnormo(X0) & ~ gt(n0, X0)) & bcapacityne(n0) & ! [X1] : (conditionhyper(X1) | ~ gt(n0, X1)) & ! [X2] : ((drugsu(X2) & drugbg(X2)) | gt(n0, X2))), inference(rectify, [], [f56])).
fof(f56, plain, (? [X2] : (~ conditionnormo(X2) & ~ gt(n0, X2)) & bcapacityne(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & ! [X1] : ((drugsu(X1) & drugbg(X1)) | gt(n0, X1))), inference(flattening, [], [f55])).
fof(f55, plain, (? [X2] : (~ conditionnormo(X2) & ~ gt(n0, X2)) & (bcapacityne(n0) & ! [X0] : (conditionhyper(X0) | ~ gt(n0, X0)) & ! [X1] : ((drugsu(X1) & drugbg(X1)) | gt(n0, X1)))), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ~ ((bcapacityne(n0) & ! [X0] : (gt(n0, X0) => conditionhyper(X0)) & ! [X1] : (~ gt(n0, X1) => (drugsu(X1) & drugbg(X1)))) => ! [X2] : (~ gt(n0, X2) => conditionnormo(X2))), inference(rectify, [], [f20])).
fof(f20, plain, ~ ((bcapacityne(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & ! [X3] : (~ gt(n0, X3) => (drugsu(X3) & drugbg(X3)))) => ! [X3] : (~ gt(n0, X3) => conditionnormo(X3))), inference(negated_conjecture, [], [f19])).
fof(f19, plain, ~ ((bcapacityne(n0) & ! [X3] : (gt(n0, X3) => conditionhyper(X3)) & ! [X3] : (~ gt(n0, X3) => (drugsu(X3) & drugbg(X3)))) => ! [X3] : (~ gt(n0, X3) => conditionnormo(X3))), file('/home/ubuntu/library/tptp/Problems/MED/MED002+1.p', treatmentne)).
fof(f630, plain, (! [X1] : (gt(n0, X1) | ~ conditionhyper(sK10(n0)) | conditionnormo(X1) | ~ bcapacityne(n0) | sP0(n0)) | ~ spl16_27), inference(resolution, [], [f374, f125])).
fof(f125, plain, ! [X0, X1] : (~ bsecretioni(sK11(X0)) | gt(X0, X1) | ~ conditionhyper(sK10(X0)) | conditionnormo(X1) | ~ bcapacityne(X0) | sP0(X0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (! [X1] : (conditionnormo(X1) | gt(X0, X1)) | (~ conditionhyper(sK10(X0)) & gt(X0, sK10(X0))) | (~ bsecretioni(sK11(X0)) & ~ gt(X0, sK11(X0))) | ~ bcapacityne(X0) | sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11])], [f81, f83, f82])).
fof(f82, plain, ! [X0] : (? [X2] : (~ conditionhyper(X2) & gt(X0, X2)) => (~ conditionhyper(sK10(X0)) & gt(X0, sK10(X0)))), introduced(choice_axiom, [])).
fof(f83, plain, ! [X0] : (? [X3] : (~ bsecretioni(X3) & ~ gt(X0, X3)) => (~ bsecretioni(sK11(X0)) & ~ gt(X0, sK11(X0)))), introduced(choice_axiom, [])).
fof(f81, plain, ! [X0] : (! [X1] : (conditionnormo(X1) | gt(X0, X1)) | ? [X2] : (~ conditionhyper(X2) & gt(X0, X2)) | ? [X3] : (~ bsecretioni(X3) & ~ gt(X0, X3)) | ~ bcapacityne(X0) | sP0(X0)), inference(rectify, [], [f58])).
fof(f58, plain, ! [X0] : (! [X5] : (conditionnormo(X5) | gt(X0, X5)) | ? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2)) | ~ bcapacityne(X0) | sP0(X0)), inference(definition_folding, [], [f52, e57])).
fof(f57, plain, ! [X0] : ((? [X3] : (~ uptakepg(X3) & ~ gt(X0, X3)) & ? [X4] : (releaselg(X4) & ~ gt(X0, X4))) | ~ sP0(X0)), inference(usedef, [], [e57])).
fof(e57, plain, ! [X0] : (sP0(X0) <=> (? [X3] : (~ uptakepg(X3) & ~ gt(X0, X3)) & ? [X4] : (releaselg(X4) & ~ gt(X0, X4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f52, plain, ! [X0] : (! [X5] : (conditionnormo(X5) | gt(X0, X5)) | ? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2)) | ~ bcapacityne(X0) | (? [X3] : (~ uptakepg(X3) & ~ gt(X0, X3)) & ? [X4] : (releaselg(X4) & ~ gt(X0, X4)))), inference(flattening, [], [f51])).
fof(f51, plain, ! [X0] : (! [X5] : (conditionnormo(X5) | gt(X0, X5)) | (? [X1] : (~ conditionhyper(X1) & gt(X0, X1)) | ? [X2] : (~ bsecretioni(X2) & ~ gt(X0, X2)) | ~ bcapacityne(X0) | (? [X3] : (~ uptakepg(X3) & ~ gt(X0, X3)) & ? [X4] : (releaselg(X4) & ~ gt(X0, X4))))), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : ((! [X1] : (gt(X0, X1) => conditionhyper(X1)) & ! [X2] : (~ gt(X0, X2) => bsecretioni(X2)) & bcapacityne(X0) & (! [X3] : (~ gt(X0, X3) => uptakepg(X3)) | ! [X4] : (~ gt(X0, X4) => ~ releaselg(X4)))) => ! [X5] : (~ gt(X0, X5) => conditionnormo(X5))), inference(rectify, [], [f17])).
fof(f17, plain, ! [X3] : ((! [X4] : (gt(X3, X4) => conditionhyper(X4)) & ! [X4] : (~ gt(X3, X4) => bsecretioni(X4)) & bcapacityne(X3) & (! [X4] : (~ gt(X3, X4) => uptakepg(X4)) | ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4)))) => ! [X4] : (~ gt(X3, X4) => conditionnormo(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED002+1.p', ne_cure)).
fof(f374, plain, (bsecretioni(sK11(n0)) | ~ spl16_27), inference(avatar_component_clause, [], [f372])).
fof(f642, plain, (spl16_28 | ~ spl16_32), inference(avatar_split_clause, [], [f638, f428, f376])).
fof(f428, plain, (spl16_32 <=> gt(n0, sK10(n0))), introduced(avatar_definition, [new_symbols(naming, [spl16_32])])).
fof(f638, plain, (conditionhyper(sK10(n0)) | ~ spl16_32), inference(resolution, [], [f430, f136])).
fof(f136, plain, ! [X1] : (~ gt(n0, X1) | conditionhyper(X1)), inference(cnf_transformation, [], [f92])).
fof(f430, plain, (gt(n0, sK10(n0)) | ~ spl16_32), inference(avatar_component_clause, [], [f428])).
fof(f633, plain, (spl16_32 | spl16_23 | spl16_4 | ~ spl16_27), inference(avatar_split_clause, [], [f632, f372, f169, f308, f428])).
fof(f632, plain, (! [X0] : (gt(n0, X0) | gt(n0, sK10(n0)) | conditionnormo(X0)) | (spl16_4 | ~ spl16_27)), inference(subsumption_resolution, [], [f631, f171])).
fof(f631, plain, (! [X0] : (gt(n0, X0) | gt(n0, sK10(n0)) | conditionnormo(X0) | sP0(n0)) | ~ spl16_27), inference(subsumption_resolution, [], [f629, f137])).
fof(f629, plain, (! [X0] : (gt(n0, X0) | gt(n0, sK10(n0)) | conditionnormo(X0) | ~ bcapacityne(n0) | sP0(n0)) | ~ spl16_27), inference(resolution, [], [f374, f123])).
fof(f123, plain, ! [X0, X1] : (~ bsecretioni(sK11(X0)) | gt(X0, X1) | gt(X0, sK10(X0)) | conditionnormo(X1) | ~ bcapacityne(X0) | sP0(X0)), inference(cnf_transformation, [], [f84])).
fof(f568, plain, (~ spl16_4 | ~ spl16_14), inference(avatar_split_clause, [], [f567, f245, f169])).
fof(f245, plain, (spl16_14 <=> ! [X0] : (gt(n0, X0) | ~ releaselg(X0))), introduced(avatar_definition, [new_symbols(naming, [spl16_14])])).
fof(f567, plain, (~ sP0(n0) | ~ spl16_14), inference(subsumption_resolution, [], [f280, f119])).
fof(f119, plain, ! [X0] : (releaselg(sK9(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : (((~ uptakepg(sK8(X0)) & ~ gt(X0, sK8(X0))) & (releaselg(sK9(X0)) & ~ gt(X0, sK9(X0)))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f77, f79, f78])).
fof(f78, plain, ! [X0] : (? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) => (~ uptakepg(sK8(X0)) & ~ gt(X0, sK8(X0)))), introduced(choice_axiom, [])).
fof(f79, plain, ! [X0] : (? [X2] : (releaselg(X2) & ~ gt(X0, X2)) => (releaselg(sK9(X0)) & ~ gt(X0, sK9(X0)))), introduced(choice_axiom, [])).
fof(f77, plain, ! [X0] : ((? [X1] : (~ uptakepg(X1) & ~ gt(X0, X1)) & ? [X2] : (releaselg(X2) & ~ gt(X0, X2))) | ~ sP0(X0)), inference(rectify, [], [f76])).
fof(f76, plain, ! [X0] : ((? [X3] : (~ uptakepg(X3) & ~ gt(X0, X3)) & ? [X4] : (releaselg(X4) & ~ gt(X0, X4))) | ~ sP0(X0)), inference(nnf_transformation, [], [f57])).
fof(f280, plain, (~ releaselg(sK9(n0)) | ~ sP0(n0) | ~ spl16_14), inference(resolution, [], [f246, f118])).
fof(f118, plain, ! [X0] : (~ gt(X0, sK9(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f80])).
fof(f246, plain, (! [X0] : (gt(n0, X0) | ~ releaselg(X0)) | ~ spl16_14), inference(avatar_component_clause, [], [f245])).
fof(f556, plain, (spl16_27 | spl16_4 | spl16_32 | spl16_23 | ~ spl16_16), inference(avatar_split_clause, [], [f425, f260, f308, f428, f169, f372])).
fof(f260, plain, (spl16_16 <=> ! [X0] : (gt(n0, X0) | bsecretioni(X0))), introduced(avatar_definition, [new_symbols(naming, [spl16_16])])).
fof(f425, plain, (! [X0] : (gt(n0, X0) | gt(n0, sK10(n0)) | conditionnormo(X0) | sP0(n0) | bsecretioni(sK11(n0))) | ~ spl16_16), inference(subsumption_resolution, [], [f419, f137])).
fof(f419, plain, (! [X0] : (gt(n0, X0) | gt(n0, sK10(n0)) | conditionnormo(X0) | ~ bcapacityne(n0) | sP0(n0) | bsecretioni(sK11(n0))) | ~ spl16_16), inference(resolution, [], [f122, f261])).
fof(f261, plain, (! [X0] : (gt(n0, X0) | bsecretioni(X0)) | ~ spl16_16), inference(avatar_component_clause, [], [f260])).
fof(f122, plain, ! [X0, X1] : (~ gt(X0, sK11(X0)) | gt(X0, X1) | gt(X0, sK10(X0)) | conditionnormo(X1) | ~ bcapacityne(X0) | sP0(X0)), inference(cnf_transformation, [], [f84])).
fof(f538, plain, ~ spl16_23, inference(avatar_contradiction_clause, [], [f537])).
fof(f537, plain, ($false | ~ spl16_23), inference(subsumption_resolution, [], [f516, f139])).
fof(f139, plain, ~ conditionnormo(sK15), inference(cnf_transformation, [], [f92])).
fof(f516, plain, (conditionnormo(sK15) | ~ spl16_23), inference(resolution, [], [f309, f138])).
fof(f138, plain, ~ gt(n0, sK15), inference(cnf_transformation, [], [f92])).
fof(f309, plain, (! [X0] : (gt(n0, X0) | conditionnormo(X0)) | ~ spl16_23), inference(avatar_component_clause, [], [f308])).
fof(f379, plain, (spl16_27 | ~ spl16_28 | spl16_23 | spl16_4 | ~ spl16_16), inference(avatar_split_clause, [], [f370, f260, f169, f308, f376, f372])).
fof(f370, plain, (! [X0] : (gt(n0, X0) | ~ conditionhyper(sK10(n0)) | conditionnormo(X0) | bsecretioni(sK11(n0))) | (spl16_4 | ~ spl16_16)), inference(subsumption_resolution, [], [f369, f171])).
fof(f369, plain, (! [X0] : (gt(n0, X0) | ~ conditionhyper(sK10(n0)) | conditionnormo(X0) | sP0(n0) | bsecretioni(sK11(n0))) | ~ spl16_16), inference(subsumption_resolution, [], [f363, f137])).
fof(f363, plain, (! [X0] : (gt(n0, X0) | ~ conditionhyper(sK10(n0)) | conditionnormo(X0) | ~ bcapacityne(n0) | sP0(n0) | bsecretioni(sK11(n0))) | ~ spl16_16), inference(resolution, [], [f124, f261])).
fof(f124, plain, ! [X0, X1] : (~ gt(X0, sK11(X0)) | gt(X0, X1) | ~ conditionhyper(sK10(X0)) | conditionnormo(X1) | ~ bcapacityne(X0) | sP0(X0)), inference(cnf_transformation, [], [f84])).
fof(f270, plain, ~ spl16_15, inference(avatar_contradiction_clause, [], [f269])).
fof(f269, plain, ($false | ~ spl16_15), inference(subsumption_resolution, [], [f268, f137])).
fof(f268, plain, (~ bcapacityne(n0) | ~ spl16_15), inference(resolution, [], [f258, f96])).
fof(f96, plain, ! [X0] : (~ bcapacityex(X0) | ~ bcapacityne(X0)), inference(cnf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (~ bcapacityex(X0) | ~ bcapacityne(X0)), inference(rectify, [], [f4])).
fof(f4, plain, ! [X3] : (~ bcapacityex(X3) | ~ bcapacityne(X3)), file('/home/ubuntu/library/tptp/Problems/MED/MED002+1.p', xorcapacity2)).
fof(f258, plain, (bcapacityex(n0) | ~ spl16_15), inference(avatar_component_clause, [], [f256])).
fof(f256, plain, (spl16_15 <=> bcapacityex(n0)), introduced(avatar_definition, [new_symbols(naming, [spl16_15])])).
fof(f262, plain, (spl16_15 | spl16_16), inference(avatar_split_clause, [], [f254, f260, f256])).
fof(f254, plain, ! [X0] : (gt(n0, X0) | bcapacityex(n0) | bsecretioni(X0)), inference(subsumption_resolution, [], [f251, f107])).
fof(f107, plain, ! [X0, X1] : (~ drugsu(sK2(X0)) | gt(X0, X1) | bcapacityex(X0) | bsecretioni(X1)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (! [X1] : (bsecretioni(X1) | gt(X0, X1)) | bcapacityex(X0) | (~ drugsu(sK2(X0)) & ~ gt(X0, sK2(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f62, f63])).
fof(f63, plain, ! [X0] : (? [X2] : (~ drugsu(X2) & ~ gt(X0, X2)) => (~ drugsu(sK2(X0)) & ~ gt(X0, sK2(X0)))), introduced(choice_axiom, [])).
fof(f62, plain, ! [X0] : (! [X1] : (bsecretioni(X1) | gt(X0, X1)) | bcapacityex(X0) | ? [X2] : (~ drugsu(X2) & ~ gt(X0, X2))), inference(rectify, [], [f45])).
fof(f45, plain, ! [X0] : (! [X2] : (bsecretioni(X2) | gt(X0, X2)) | bcapacityex(X0) | ? [X1] : (~ drugsu(X1) & ~ gt(X0, X1))), inference(flattening, [], [f44])).
fof(f44, plain, ! [X0] : (! [X2] : (bsecretioni(X2) | gt(X0, X2)) | (bcapacityex(X0) | ? [X1] : (~ drugsu(X1) & ~ gt(X0, X1)))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : ((~ bcapacityex(X0) & ! [X1] : (~ gt(X0, X1) => drugsu(X1))) => ! [X2] : (~ gt(X0, X2) => bsecretioni(X2))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X3] : ((~ bcapacityex(X3) & ! [X4] : (~ gt(X3, X4) => drugsu(X4))) => ! [X4] : (~ gt(X3, X4) => bsecretioni(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED002+1.p', sulfonylurea_effect)).
fof(f251, plain, ! [X0] : (gt(n0, X0) | bcapacityex(n0) | bsecretioni(X0) | drugsu(sK2(n0))), inference(resolution, [], [f106, f135])).
fof(f135, plain, ! [X2] : (gt(n0, X2) | drugsu(X2)), inference(cnf_transformation, [], [f92])).
fof(f106, plain, ! [X0, X1] : (~ gt(X0, sK2(X0)) | gt(X0, X1) | bcapacityex(X0) | bsecretioni(X1)), inference(cnf_transformation, [], [f64])).
fof(f249, plain, spl16_14, inference(avatar_split_clause, [], [f248, f245])).
fof(f248, plain, ! [X1] : (gt(n0, X1) | ~ releaselg(X1)), inference(subsumption_resolution, [], [f238, f109])).
fof(f109, plain, ! [X0, X1] : (~ drugbg(sK3(X0)) | gt(X0, X1) | ~ releaselg(X1)), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ! [X0] : (! [X1] : (~ releaselg(X1) | gt(X0, X1)) | (~ drugbg(sK3(X0)) & ~ gt(X0, sK3(X0)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f65, f66])).
fof(f66, plain, ! [X0] : (? [X2] : (~ drugbg(X2) & ~ gt(X0, X2)) => (~ drugbg(sK3(X0)) & ~ gt(X0, sK3(X0)))), introduced(choice_axiom, [])).
fof(f65, plain, ! [X0] : (! [X1] : (~ releaselg(X1) | gt(X0, X1)) | ? [X2] : (~ drugbg(X2) & ~ gt(X0, X2))), inference(rectify, [], [f46])).
fof(f46, plain, ! [X0] : (! [X2] : (~ releaselg(X2) | gt(X0, X2)) | ? [X1] : (~ drugbg(X1) & ~ gt(X0, X1))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : (! [X1] : (~ gt(X0, X1) => drugbg(X1)) => ! [X2] : (~ gt(X0, X2) => ~ releaselg(X2))), inference(rectify, [], [f14])).
fof(f14, plain, ! [X3] : (! [X4] : (~ gt(X3, X4) => drugbg(X4)) => ! [X4] : (~ gt(X3, X4) => ~ releaselg(X4))), file('/home/ubuntu/library/tptp/Problems/MED/MED002+1.p', biguanide_effect)).
fof(f238, plain, ! [X1] : (gt(n0, X1) | ~ releaselg(X1) | drugbg(sK3(n0))), inference(resolution, [], [f108, f134])).
fof(f134, plain, ! [X2] : (gt(n0, X2) | drugbg(X2)), inference(cnf_transformation, [], [f92])).
fof(f108, plain, ! [X0, X1] : (~ gt(X0, sK3(X0)) | gt(X0, X1) | ~ releaselg(X1)), inference(cnf_transformation, [], [f67])).