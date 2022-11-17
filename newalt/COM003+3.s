fof(f430, plain, $false, inference(avatar_sat_refutation, [], [f69, f73, f78, f79, f86, f90, f97, f101, f165, f177, f192, f223, f239, f260, f273, f276, f282, f290, f292, f297, f335, f351, f364, f367, f373, f375, f377, f386, f390, f398, f400, f406, f415, f421, f423, f429])).
fof(f429, plain, (~ spl11_4 | spl11_13 | ~ spl11_21 | ~ spl11_32), inference(avatar_contradiction_clause, [], [f428])).
fof(f428, plain, ($false | (~ spl11_4 | spl11_13 | ~ spl11_21 | ~ spl11_32)), inference(subsumption_resolution, [], [f427, f77])).
fof(f77, plain, (program(sK2) | ~ spl11_4), inference(avatar_component_clause, [], [f75])).
fof(f75, plain, (spl11_4 <=> program(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl11_4])])).
fof(f427, plain, (~ program(sK2) | (spl11_13 | ~ spl11_21 | ~ spl11_32)), inference(subsumption_resolution, [], [f425, f124])).
fof(f124, plain, (~ program(sK5(sK2)) | spl11_13), inference(avatar_component_clause, [], [f122])).
fof(f122, plain, (spl11_13 <=> program(sK5(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl11_13])])).
fof(f425, plain, (program(sK5(sK2)) | ~ program(sK2) | (~ spl11_21 | ~ spl11_32)), inference(resolution, [], [f222, f362])).
fof(f362, plain, (program(sK7(sK2)) | ~ spl11_32), inference(avatar_component_clause, [], [f361])).
fof(f361, plain, (spl11_32 <=> program(sK7(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl11_32])])).
fof(f222, plain, (! [X2] : (~ program(sK7(X2)) | program(sK5(X2)) | ~ program(X2)) | ~ spl11_21), inference(avatar_component_clause, [], [f221])).
fof(f221, plain, (spl11_21 <=> ! [X2] : (program(sK5(X2)) | ~ program(sK7(X2)) | ~ program(X2))), introduced(avatar_definition, [new_symbols(naming, [spl11_21])])).
fof(f423, plain, (spl11_22 | ~ spl11_7 | ~ spl11_23), inference(avatar_split_clause, [], [f422, f280, f88, f237])).
fof(f237, plain, (spl11_22 <=> ! [X0] : (~ sP1(X0) | ~ program(X0) | program(sK5(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl11_22])])).
fof(f88, plain, (spl11_7 <=> ! [X0] : (outputs(X0, bad) | ~ program(X0) | program(sK5(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl11_7])])).
fof(f280, plain, (spl11_23 <=> ! [X0] : (~ sP1(X0) | ~ outputs(X0, bad) | ~ program(X0) | program(sK5(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl11_23])])).
fof(f422, plain, (! [X0] : (~ sP1(X0) | ~ program(X0) | program(sK5(X0))) | (~ spl11_7 | ~ spl11_23)), inference(subsumption_resolution, [], [f281, f89])).
fof(f89, plain, (! [X0] : (outputs(X0, bad) | ~ program(X0) | program(sK5(X0))) | ~ spl11_7), inference(avatar_component_clause, [], [f88])).
fof(f281, plain, (! [X0] : (~ sP1(X0) | ~ outputs(X0, bad) | ~ program(X0) | program(sK5(X0))) | ~ spl11_23), inference(avatar_component_clause, [], [f280])).
fof(f421, plain, (~ spl11_4 | spl11_13 | ~ spl11_22 | ~ spl11_26), inference(avatar_contradiction_clause, [], [f420])).
fof(f420, plain, ($false | (~ spl11_4 | spl11_13 | ~ spl11_22 | ~ spl11_26)), inference(subsumption_resolution, [], [f419, f311])).
fof(f311, plain, (sP1(sK2) | ~ spl11_26), inference(avatar_component_clause, [], [f310])).
fof(f310, plain, (spl11_26 <=> sP1(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl11_26])])).
fof(f419, plain, (~ sP1(sK2) | (~ spl11_4 | spl11_13 | ~ spl11_22)), inference(subsumption_resolution, [], [f418, f77])).
fof(f418, plain, (~ program(sK2) | ~ sP1(sK2) | (spl11_13 | ~ spl11_22)), inference(resolution, [], [f238, f124])).
fof(f238, plain, (! [X0] : (program(sK5(X0)) | ~ program(X0) | ~ sP1(X0)) | ~ spl11_22), inference(avatar_component_clause, [], [f237])).
fof(f415, plain, (spl11_26 | ~ spl11_4 | spl11_13 | ~ spl11_14 | spl11_31), inference(avatar_split_clause, [], [f414, f339, f126, f122, f75, f310])).
fof(f126, plain, (spl11_14 <=> outputs(sK2, bad)), introduced(avatar_definition, [new_symbols(naming, [spl11_14])])).
fof(f339, plain, (spl11_31 <=> sP0(sK2, sK9(sK2), sK8(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl11_31])])).
fof(f414, plain, (sP1(sK2) | (~ spl11_4 | spl11_13 | ~ spl11_14 | spl11_31)), inference(subsumption_resolution, [], [f413, f124])).
fof(f413, plain, (sP1(sK2) | program(sK5(sK2)) | (~ spl11_4 | ~ spl11_14 | spl11_31)), inference(subsumption_resolution, [], [f412, f77])).
fof(f412, plain, (sP1(sK2) | ~ program(sK2) | program(sK5(sK2)) | (~ spl11_14 | spl11_31)), inference(subsumption_resolution, [], [f410, f128])).
fof(f128, plain, (outputs(sK2, bad) | ~ spl11_14), inference(avatar_component_clause, [], [f126])).
fof(f410, plain, (sP1(sK2) | ~ outputs(sK2, bad) | ~ program(sK2) | program(sK5(sK2)) | spl11_31), inference(resolution, [], [f340, f277])).
fof(f277, plain, ! [X1] : (sP0(X1, sK9(X1), sK8(X1)) | sP1(X1) | ~ outputs(X1, bad) | ~ program(X1) | program(sK5(X1))), inference(subsumption_resolution, [], [f199, f104])).
fof(f104, plain, ! [X0] : (program(sK8(X0)) | sP1(X0) | ~ program(X0)), inference(subsumption_resolution, [], [f58, f55])).
fof(f55, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | program(X2)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ! [X0, X1, X2] : (((~ outputs(X0, good) | ~ halts3(X0, X2, X1)) & halts2(X2, X1) & program(X2)) | ~ sP0(X0, X1, X2)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X0, X2, X1] : (((~ outputs(X0, good) | ~ halts3(X0, X1, X2)) & halts2(X1, X2) & program(X1)) | ~ sP0(X0, X2, X1)), inference(nnf_transformation, [], [f17])).
fof(f17, plain, ! [X0, X2, X1] : (((~ outputs(X0, good) | ~ halts3(X0, X1, X2)) & halts2(X1, X2) & program(X1)) | ~ sP0(X0, X2, X1)), inference(usedef, [], [e17])).
fof(e17, plain, ! [X0, X2, X1] : (sP0(X0, X2, X1) <=> ((~ outputs(X0, good) | ~ halts3(X0, X1, X2)) & halts2(X1, X2) & program(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f58, plain, ! [X0] : (sP1(X0) | program(sK8(X0)) | sP0(X0, sK9(X0), sK8(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (sP1(X0) | (((~ outputs(X0, bad) | ~ halts3(X0, sK8(X0), sK9(X0))) & ~ halts2(sK8(X0), sK9(X0)) & program(sK8(X0))) | sP0(X0, sK9(X0), sK8(X0))) | ~ program(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f19, f35])).
fof(f35, plain, ! [X0] : (? [X1, X2] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X2)) & ~ halts2(X1, X2) & program(X1)) | sP0(X0, X2, X1)) => (((~ outputs(X0, bad) | ~ halts3(X0, sK8(X0), sK9(X0))) & ~ halts2(sK8(X0), sK9(X0)) & program(sK8(X0))) | sP0(X0, sK9(X0), sK8(X0)))), introduced(choice_axiom, [])).
fof(f19, plain, ! [X0] : (sP1(X0) | ? [X1, X2] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X2)) & ~ halts2(X1, X2) & program(X1)) | sP0(X0, X2, X1)) | ~ program(X0)), inference(definition_folding, [], [f15, e18, e17])).
fof(f18, plain, ! [X0] : (? [X3] : (! [X4] : (((outputs(X3, bad) & halts2(X3, X4)) | ~ outputs(X0, bad) | ~ halts3(X0, X4, X4) | ~ program(X4)) & (~ halts2(X3, X4) | ~ outputs(X0, good) | ~ halts3(X0, X4, X4) | ~ program(X4))) & program(X3)) | ~ sP1(X0)), inference(usedef, [], [e18])).
fof(e18, plain, ! [X0] : (sP1(X0) <=> ? [X3] : (! [X4] : (((outputs(X3, bad) & halts2(X3, X4)) | ~ outputs(X0, bad) | ~ halts3(X0, X4, X4) | ~ program(X4)) & (~ halts2(X3, X4) | ~ outputs(X0, good) | ~ halts3(X0, X4, X4) | ~ program(X4))) & program(X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f15, plain, ! [X0] : (? [X3] : (! [X4] : (((outputs(X3, bad) & halts2(X3, X4)) | ~ outputs(X0, bad) | ~ halts3(X0, X4, X4) | ~ program(X4)) & (~ halts2(X3, X4) | ~ outputs(X0, good) | ~ halts3(X0, X4, X4) | ~ program(X4))) & program(X3)) | ? [X1, X2] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X2)) & ~ halts2(X1, X2) & program(X1)) | ((~ outputs(X0, good) | ~ halts3(X0, X1, X2)) & halts2(X1, X2) & program(X1))) | ~ program(X0)), inference(flattening, [], [f14])).
fof(f14, plain, ! [X0] : (? [X3] : (! [X4] : (((outputs(X3, bad) & halts2(X3, X4)) | (~ outputs(X0, bad) | ~ halts3(X0, X4, X4) | ~ program(X4))) & (~ halts2(X3, X4) | (~ outputs(X0, good) | ~ halts3(X0, X4, X4) | ~ program(X4)))) & program(X3)) | (? [X1, X2] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X2)) & (~ halts2(X1, X2) & program(X1))) | ((~ outputs(X0, good) | ~ halts3(X0, X1, X2)) & (halts2(X1, X2) & program(X1)))) | ~ program(X0))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : ((! [X1, X2] : (((~ halts2(X1, X2) & program(X1)) => (outputs(X0, bad) & halts3(X0, X1, X2))) & ((halts2(X1, X2) & program(X1)) => (outputs(X0, good) & halts3(X0, X1, X2)))) & program(X0)) => ? [X3] : (! [X4] : (((outputs(X0, bad) & halts3(X0, X4, X4) & program(X4)) => (outputs(X3, bad) & halts2(X3, X4))) & ((outputs(X0, good) & halts3(X0, X4, X4) & program(X4)) => ~ halts2(X3, X4))) & program(X3))), inference(rectify, [], [f3])).
fof(f3, plain, ! [X3] : ((! [X1, X2] : (((~ halts2(X1, X2) & program(X1)) => (outputs(X3, bad) & halts3(X3, X1, X2))) & ((halts2(X1, X2) & program(X1)) => (outputs(X3, good) & halts3(X3, X1, X2)))) & program(X3)) => ? [X4] : (! [X1] : (((outputs(X3, bad) & halts3(X3, X1, X1) & program(X1)) => (outputs(X4, bad) & halts2(X4, X1))) & ((outputs(X3, good) & halts3(X3, X1, X1) & program(X1)) => ~ halts2(X4, X1))) & program(X4))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+3.p', p3)).
fof(f199, plain, ! [X1] : (~ outputs(X1, bad) | sP1(X1) | sP0(X1, sK9(X1), sK8(X1)) | ~ program(X1) | ~ program(sK8(X1)) | program(sK5(X1))), inference(duplicate_literal_removal, [], [f198])).
fof(f198, plain, ! [X1] : (~ outputs(X1, bad) | sP1(X1) | sP0(X1, sK9(X1), sK8(X1)) | ~ program(X1) | ~ program(sK8(X1)) | program(sK5(X1)) | ~ program(X1)), inference(resolution, [], [f60, f103])).
fof(f103, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | ~ program(X1) | program(sK5(X0)) | ~ program(X0)), inference(subsumption_resolution, [], [f43, f47])).
fof(f47, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | halts2(X1, X2) | ~ program(X1) | program(sK5(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (! [X1, X2] : (((outputs(X0, bad) & halts3(X0, X1, X2)) | halts2(X1, X2) | ~ program(X1)) & ((outputs(X0, good) & halts3(X0, X1, X2)) | ~ halts2(X1, X2) | ~ program(X1))) | (~ decides(X0, sK5(X0), sK6(X0)) & program(sK5(X0))) | ~ program(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6])], [f25, f27, f26])).
fof(f26, plain, ! [X0] : (? [X3] : (? [X4] : ~ decides(X0, X3, X4) & program(X3)) => (? [X4] : ~ decides(X0, sK5(X0), X4) & program(sK5(X0)))), introduced(choice_axiom, [])).
fof(f27, plain, ! [X0] : (? [X4] : ~ decides(X0, sK5(X0), X4) => ~ decides(X0, sK5(X0), sK6(X0))), introduced(choice_axiom, [])).
fof(f25, plain, ! [X0] : (! [X1, X2] : (((outputs(X0, bad) & halts3(X0, X1, X2)) | halts2(X1, X2) | ~ program(X1)) & ((outputs(X0, good) & halts3(X0, X1, X2)) | ~ halts2(X1, X2) | ~ program(X1))) | ? [X3] : (? [X4] : ~ decides(X0, X3, X4) & program(X3)) | ~ program(X0)), inference(rectify, [], [f13])).
fof(f13, plain, ! [X0] : (! [X3, X4] : (((outputs(X0, bad) & halts3(X0, X3, X4)) | halts2(X3, X4) | ~ program(X3)) & ((outputs(X0, good) & halts3(X0, X3, X4)) | ~ halts2(X3, X4) | ~ program(X3))) | ? [X1] : (? [X2] : ~ decides(X0, X1, X2) & program(X1)) | ~ program(X0)), inference(flattening, [], [f12])).
fof(f12, plain, ! [X0] : (! [X3, X4] : (((outputs(X0, bad) & halts3(X0, X3, X4)) | (halts2(X3, X4) | ~ program(X3))) & ((outputs(X0, good) & halts3(X0, X3, X4)) | (~ halts2(X3, X4) | ~ program(X3)))) | (? [X1] : (? [X2] : ~ decides(X0, X1, X2) & program(X1)) | ~ program(X0))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : ((! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & program(X0)) => ! [X3, X4] : (((~ halts2(X3, X4) & program(X3)) => (outputs(X0, bad) & halts3(X0, X3, X4))) & ((halts2(X3, X4) & program(X3)) => (outputs(X0, good) & halts3(X0, X3, X4))))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X3] : ((! [X1] : (program(X1) => ! [X2] : decides(X3, X1, X2)) & program(X3)) => ! [X1, X2] : (((~ halts2(X1, X2) & program(X1)) => (outputs(X3, bad) & halts3(X3, X1, X2))) & ((halts2(X1, X2) & program(X1)) => (outputs(X3, good) & halts3(X3, X1, X2))))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+3.p', p2)).
fof(f43, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | ~ halts2(X1, X2) | ~ program(X1) | program(sK5(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f60, plain, ! [X0] : (~ halts3(X0, sK8(X0), sK9(X0)) | ~ outputs(X0, bad) | sP1(X0) | sP0(X0, sK9(X0), sK8(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f36])).
fof(f340, plain, (~ sP0(sK2, sK9(sK2), sK8(sK2)) | spl11_31), inference(avatar_component_clause, [], [f339])).
fof(f406, plain, (spl11_15 | ~ spl11_8 | ~ spl11_28 | ~ spl11_32), inference(avatar_split_clause, [], [f405, f361, f320, f92, f150])).
fof(f150, plain, (spl11_15 <=> ! [X1] : ~ program(X1)), introduced(avatar_definition, [new_symbols(naming, [spl11_15])])).
fof(f92, plain, (spl11_8 <=> ! [X1, X2] : (~ halts2(X1, X2) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl11_8])])).
fof(f320, plain, (spl11_28 <=> ! [X1] : (~ program(X1) | halts2(sK7(sK2), X1))), introduced(avatar_definition, [new_symbols(naming, [spl11_28])])).
fof(f405, plain, (! [X1] : ~ program(X1) | (~ spl11_8 | ~ spl11_28 | ~ spl11_32)), inference(subsumption_resolution, [], [f403, f362])).
fof(f403, plain, (! [X1] : (~ program(X1) | ~ program(sK7(sK2))) | (~ spl11_8 | ~ spl11_28)), inference(resolution, [], [f321, f93])).
fof(f93, plain, (! [X2, X1] : (~ halts2(X1, X2) | ~ program(X1)) | ~ spl11_8), inference(avatar_component_clause, [], [f92])).
fof(f321, plain, (! [X1] : (halts2(sK7(sK2), X1) | ~ program(X1)) | ~ spl11_28), inference(avatar_component_clause, [], [f320])).
fof(f400, plain, (~ spl11_26 | spl11_28 | ~ spl11_14 | ~ spl11_25), inference(avatar_split_clause, [], [f399, f295, f126, f320, f310])).
fof(f295, plain, (spl11_25 <=> ! [X3, X2] : (~ program(X2) | halts3(sK2, X2, X3))), introduced(avatar_definition, [new_symbols(naming, [spl11_25])])).
fof(f399, plain, (! [X1] : (~ program(X1) | halts2(sK7(sK2), X1) | ~ sP1(sK2)) | (~ spl11_14 | ~ spl11_25)), inference(subsumption_resolution, [], [f306, f128])).
fof(f306, plain, (! [X1] : (~ program(X1) | ~ outputs(sK2, bad) | halts2(sK7(sK2), X1) | ~ sP1(sK2)) | ~ spl11_25), inference(duplicate_literal_removal, [], [f301])).
fof(f301, plain, (! [X1] : (~ program(X1) | ~ outputs(sK2, bad) | halts2(sK7(sK2), X1) | ~ program(X1) | ~ sP1(sK2)) | ~ spl11_25), inference(resolution, [], [f296, f53])).
fof(f53, plain, ! [X2, X0] : (~ halts3(X0, X2, X2) | ~ outputs(X0, bad) | halts2(sK7(X0), X2) | ~ program(X2) | ~ sP1(X0)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : ((! [X2] : (((outputs(sK7(X0), bad) & halts2(sK7(X0), X2)) | ~ outputs(X0, bad) | ~ halts3(X0, X2, X2) | ~ program(X2)) & (~ halts2(sK7(X0), X2) | ~ outputs(X0, good) | ~ halts3(X0, X2, X2) | ~ program(X2))) & program(sK7(X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f30, f31])).
fof(f31, plain, ! [X0] : (? [X1] : (! [X2] : (((outputs(X1, bad) & halts2(X1, X2)) | ~ outputs(X0, bad) | ~ halts3(X0, X2, X2) | ~ program(X2)) & (~ halts2(X1, X2) | ~ outputs(X0, good) | ~ halts3(X0, X2, X2) | ~ program(X2))) & program(X1)) => (! [X2] : (((outputs(sK7(X0), bad) & halts2(sK7(X0), X2)) | ~ outputs(X0, bad) | ~ halts3(X0, X2, X2) | ~ program(X2)) & (~ halts2(sK7(X0), X2) | ~ outputs(X0, good) | ~ halts3(X0, X2, X2) | ~ program(X2))) & program(sK7(X0)))), introduced(choice_axiom, [])).
fof(f30, plain, ! [X0] : (? [X1] : (! [X2] : (((outputs(X1, bad) & halts2(X1, X2)) | ~ outputs(X0, bad) | ~ halts3(X0, X2, X2) | ~ program(X2)) & (~ halts2(X1, X2) | ~ outputs(X0, good) | ~ halts3(X0, X2, X2) | ~ program(X2))) & program(X1)) | ~ sP1(X0)), inference(rectify, [], [f29])).
fof(f29, plain, ! [X0] : (? [X3] : (! [X4] : (((outputs(X3, bad) & halts2(X3, X4)) | ~ outputs(X0, bad) | ~ halts3(X0, X4, X4) | ~ program(X4)) & (~ halts2(X3, X4) | ~ outputs(X0, good) | ~ halts3(X0, X4, X4) | ~ program(X4))) & program(X3)) | ~ sP1(X0)), inference(nnf_transformation, [], [f18])).
fof(f296, plain, (! [X2, X3] : (halts3(sK2, X2, X3) | ~ program(X2)) | ~ spl11_25), inference(avatar_component_clause, [], [f295])).
fof(f398, plain, (~ spl11_26 | ~ spl11_24 | spl11_29 | ~ spl11_25), inference(avatar_split_clause, [], [f305, f295, f325, f287, f310])).
fof(f287, plain, (spl11_24 <=> outputs(sK2, good)), introduced(avatar_definition, [new_symbols(naming, [spl11_24])])).
fof(f325, plain, (spl11_29 <=> ! [X2] : (~ program(X2) | ~ halts2(sK7(sK2), X2))), introduced(avatar_definition, [new_symbols(naming, [spl11_29])])).
fof(f305, plain, (! [X2] : (~ program(X2) | ~ outputs(sK2, good) | ~ halts2(sK7(sK2), X2) | ~ sP1(sK2)) | ~ spl11_25), inference(duplicate_literal_removal, [], [f302])).
fof(f302, plain, (! [X2] : (~ program(X2) | ~ outputs(sK2, good) | ~ halts2(sK7(sK2), X2) | ~ program(X2) | ~ sP1(sK2)) | ~ spl11_25), inference(resolution, [], [f296, f52])).
fof(f52, plain, ! [X2, X0] : (~ halts3(X0, X2, X2) | ~ outputs(X0, good) | ~ halts2(sK7(X0), X2) | ~ program(X2) | ~ sP1(X0)), inference(cnf_transformation, [], [f32])).
fof(f390, plain, (~ spl11_4 | ~ spl11_24 | ~ spl11_25 | spl11_26 | ~ spl11_30 | ~ spl11_31), inference(avatar_contradiction_clause, [], [f389])).
fof(f389, plain, ($false | (~ spl11_4 | ~ spl11_24 | ~ spl11_25 | spl11_26 | ~ spl11_30 | ~ spl11_31)), inference(subsumption_resolution, [], [f388, f381])).
fof(f381, plain, (halts2(sK8(sK2), sK9(sK2)) | ~ spl11_31), inference(resolution, [], [f341, f56])).
fof(f56, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | halts2(X2, X1)), inference(cnf_transformation, [], [f34])).
fof(f341, plain, (sP0(sK2, sK9(sK2), sK8(sK2)) | ~ spl11_31), inference(avatar_component_clause, [], [f339])).
fof(f388, plain, (~ halts2(sK8(sK2), sK9(sK2)) | (~ spl11_4 | ~ spl11_24 | ~ spl11_25 | spl11_26 | ~ spl11_30)), inference(subsumption_resolution, [], [f387, f312])).
fof(f312, plain, (~ sP1(sK2) | spl11_26), inference(avatar_component_clause, [], [f310])).
fof(f387, plain, (sP1(sK2) | ~ halts2(sK8(sK2), sK9(sK2)) | (~ spl11_4 | ~ spl11_24 | ~ spl11_25 | ~ spl11_30)), inference(subsumption_resolution, [], [f329, f333])).
fof(f333, plain, (program(sK8(sK2)) | ~ spl11_30), inference(avatar_component_clause, [], [f332])).
fof(f332, plain, (spl11_30 <=> program(sK8(sK2))), introduced(avatar_definition, [new_symbols(naming, [spl11_30])])).
fof(f329, plain, (~ program(sK8(sK2)) | sP1(sK2) | ~ halts2(sK8(sK2), sK9(sK2)) | (~ spl11_4 | ~ spl11_24 | ~ spl11_25)), inference(subsumption_resolution, [], [f328, f289])).
fof(f289, plain, (outputs(sK2, good) | ~ spl11_24), inference(avatar_component_clause, [], [f287])).
fof(f328, plain, (~ program(sK8(sK2)) | sP1(sK2) | ~ halts2(sK8(sK2), sK9(sK2)) | ~ outputs(sK2, good) | (~ spl11_4 | ~ spl11_25)), inference(subsumption_resolution, [], [f303, f77])).
fof(f303, plain, (~ program(sK8(sK2)) | sP1(sK2) | ~ program(sK2) | ~ halts2(sK8(sK2), sK9(sK2)) | ~ outputs(sK2, good) | ~ spl11_25), inference(resolution, [], [f296, f171])).
fof(f171, plain, ! [X0] : (~ halts3(X0, sK8(X0), sK9(X0)) | sP1(X0) | ~ program(X0) | ~ halts2(sK8(X0), sK9(X0)) | ~ outputs(X0, good)), inference(resolution, [], [f59, f57])).
fof(f57, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ halts3(X0, X2, X1) | ~ outputs(X0, good)), inference(cnf_transformation, [], [f34])).
fof(f59, plain, ! [X0] : (sP0(X0, sK9(X0), sK8(X0)) | ~ halts2(sK8(X0), sK9(X0)) | sP1(X0) | ~ program(X0)), inference(cnf_transformation, [], [f36])).
fof(f386, plain, (~ spl11_8 | ~ spl11_30 | ~ spl11_31), inference(avatar_contradiction_clause, [], [f385])).
fof(f385, plain, ($false | (~ spl11_8 | ~ spl11_30 | ~ spl11_31)), inference(subsumption_resolution, [], [f384, f333])).
fof(f384, plain, (~ program(sK8(sK2)) | (~ spl11_8 | ~ spl11_31)), inference(resolution, [], [f381, f93])).
fof(f377, plain, (spl11_31 | spl11_26 | ~ spl11_14 | ~ spl11_30 | ~ spl11_4 | ~ spl11_25), inference(avatar_split_clause, [], [f376, f295, f75, f332, f126, f310, f339])).
fof(f376, plain, (~ program(sK8(sK2)) | ~ outputs(sK2, bad) | sP1(sK2) | sP0(sK2, sK9(sK2), sK8(sK2)) | (~ spl11_4 | ~ spl11_25)), inference(subsumption_resolution, [], [f304, f77])).
fof(f304, plain, (~ program(sK8(sK2)) | ~ outputs(sK2, bad) | sP1(sK2) | sP0(sK2, sK9(sK2), sK8(sK2)) | ~ program(sK2) | ~ spl11_25), inference(resolution, [], [f296, f60])).
fof(f375, plain, (~ spl11_26 | ~ spl11_14 | spl11_15 | ~ spl11_25 | ~ spl11_29), inference(avatar_split_clause, [], [f374, f325, f295, f150, f126, f310])).
fof(f374, plain, (! [X1] : (~ program(X1) | ~ outputs(sK2, bad) | ~ sP1(sK2)) | (~ spl11_25 | ~ spl11_29)), inference(subsumption_resolution, [], [f306, f326])).
fof(f326, plain, (! [X2] : (~ halts2(sK7(sK2), X2) | ~ program(X2)) | ~ spl11_29), inference(avatar_component_clause, [], [f325])).
fof(f373, plain, (~ spl11_4 | ~ spl11_7 | spl11_13 | spl11_14), inference(avatar_contradiction_clause, [], [f372])).
fof(f372, plain, ($false | (~ spl11_4 | ~ spl11_7 | spl11_13 | spl11_14)), inference(subsumption_resolution, [], [f371, f124])).
fof(f371, plain, (program(sK5(sK2)) | (~ spl11_4 | ~ spl11_7 | spl11_14)), inference(subsumption_resolution, [], [f370, f77])).
fof(f370, plain, (~ program(sK2) | program(sK5(sK2)) | (~ spl11_7 | spl11_14)), inference(resolution, [], [f89, f127])).
fof(f127, plain, (~ outputs(sK2, bad) | spl11_14), inference(avatar_component_clause, [], [f126])).
fof(f367, plain, (~ spl11_26 | spl11_32), inference(avatar_contradiction_clause, [], [f366])).
fof(f366, plain, ($false | (~ spl11_26 | spl11_32)), inference(subsumption_resolution, [], [f365, f311])).
fof(f365, plain, (~ sP1(sK2) | spl11_32), inference(resolution, [], [f363, f51])).
fof(f51, plain, ! [X0] : (program(sK7(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f32])).
fof(f363, plain, (~ program(sK7(sK2)) | spl11_32), inference(avatar_component_clause, [], [f361])).
fof(f364, plain, (~ spl11_32 | spl11_15 | ~ spl11_5 | ~ spl11_29), inference(avatar_split_clause, [], [f358, f325, f81, f150, f361])).
fof(f81, plain, (spl11_5 <=> ! [X1, X2] : (halts2(X1, X2) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl11_5])])).
fof(f358, plain, (! [X1] : (~ program(X1) | ~ program(sK7(sK2))) | (~ spl11_5 | ~ spl11_29)), inference(resolution, [], [f326, f82])).
fof(f82, plain, (! [X2, X1] : (halts2(X1, X2) | ~ program(X1)) | ~ spl11_5), inference(avatar_component_clause, [], [f81])).
fof(f351, plain, (spl11_26 | ~ spl11_4 | spl11_30), inference(avatar_split_clause, [], [f350, f332, f75, f310])).
fof(f350, plain, (sP1(sK2) | (~ spl11_4 | spl11_30)), inference(subsumption_resolution, [], [f346, f77])).
fof(f346, plain, (sP1(sK2) | ~ program(sK2) | spl11_30), inference(resolution, [], [f334, f104])).
fof(f334, plain, (~ program(sK8(sK2)) | spl11_30), inference(avatar_component_clause, [], [f332])).
fof(f335, plain, (spl11_26 | ~ spl11_30 | ~ spl11_4 | ~ spl11_5 | ~ spl11_24 | ~ spl11_25), inference(avatar_split_clause, [], [f330, f295, f287, f81, f75, f332, f310])).
fof(f330, plain, (~ program(sK8(sK2)) | sP1(sK2) | (~ spl11_4 | ~ spl11_5 | ~ spl11_24 | ~ spl11_25)), inference(subsumption_resolution, [], [f329, f82])).
fof(f297, plain, (~ spl11_13 | spl11_25 | ~ spl11_2 | ~ spl11_4), inference(avatar_split_clause, [], [f293, f75, f67, f295, f122])).
fof(f67, plain, (spl11_2 <=> ! [X1, X2] : (decides(sK2, X1, X2) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl11_2])])).
fof(f293, plain, (! [X2, X3] : (~ program(X2) | halts3(sK2, X2, X3) | ~ program(sK5(sK2))) | (~ spl11_2 | ~ spl11_4)), inference(subsumption_resolution, [], [f131, f77])).
fof(f131, plain, (! [X2, X3] : (~ program(X2) | halts3(sK2, X2, X3) | ~ program(sK2) | ~ program(sK5(sK2))) | ~ spl11_2), inference(resolution, [], [f102, f68])).
fof(f68, plain, (! [X2, X1] : (decides(sK2, X1, X2) | ~ program(X1)) | ~ spl11_2), inference(avatar_component_clause, [], [f67])).
fof(f102, plain, ! [X2, X0, X1] : (~ decides(X0, sK5(X0), sK6(X0)) | ~ program(X1) | halts3(X0, X1, X2) | ~ program(X0)), inference(subsumption_resolution, [], [f44, f48])).
fof(f48, plain, ! [X2, X0, X1] : (~ decides(X0, sK5(X0), sK6(X0)) | halts2(X1, X2) | ~ program(X1) | halts3(X0, X1, X2) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f44, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | ~ halts2(X1, X2) | ~ program(X1) | ~ decides(X0, sK5(X0), sK6(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f292, plain, (~ spl11_13 | spl11_14 | ~ spl11_2 | ~ spl11_4 | ~ spl11_6), inference(avatar_split_clause, [], [f291, f84, f75, f67, f126, f122])).
fof(f84, plain, (spl11_6 <=> ! [X0] : (outputs(X0, bad) | ~ program(X0) | ~ decides(X0, sK5(X0), sK6(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl11_6])])).
fof(f291, plain, (outputs(sK2, bad) | ~ program(sK5(sK2)) | (~ spl11_2 | ~ spl11_4 | ~ spl11_6)), inference(subsumption_resolution, [], [f109, f77])).
fof(f109, plain, (~ program(sK2) | outputs(sK2, bad) | ~ program(sK5(sK2)) | (~ spl11_2 | ~ spl11_6)), inference(resolution, [], [f85, f68])).
fof(f85, plain, (! [X0] : (~ decides(X0, sK5(X0), sK6(X0)) | ~ program(X0) | outputs(X0, bad)) | ~ spl11_6), inference(avatar_component_clause, [], [f84])).
fof(f290, plain, (~ spl11_13 | spl11_24 | ~ spl11_2 | ~ spl11_4 | ~ spl11_9), inference(avatar_split_clause, [], [f285, f95, f75, f67, f287, f122])).
fof(f95, plain, (spl11_9 <=> ! [X0] : (outputs(X0, good) | ~ program(X0) | ~ decides(X0, sK5(X0), sK6(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl11_9])])).
fof(f285, plain, (outputs(sK2, good) | ~ program(sK5(sK2)) | (~ spl11_2 | ~ spl11_4 | ~ spl11_9)), inference(subsumption_resolution, [], [f227, f77])).
fof(f227, plain, (~ program(sK2) | outputs(sK2, good) | ~ program(sK5(sK2)) | (~ spl11_2 | ~ spl11_9)), inference(resolution, [], [f96, f68])).
fof(f96, plain, (! [X0] : (~ decides(X0, sK5(X0), sK6(X0)) | ~ program(X0) | outputs(X0, good)) | ~ spl11_9), inference(avatar_component_clause, [], [f95])).
fof(f282, plain, (spl11_23 | spl11_15 | ~ spl11_10), inference(avatar_split_clause, [], [f278, f99, f150, f280])).
fof(f99, plain, (spl11_10 <=> ! [X0] : (outputs(X0, good) | ~ program(X0) | program(sK5(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl11_10])])).
fof(f278, plain, (! [X0, X1] : (~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0) | ~ outputs(X0, bad)) | ~ spl11_10), inference(subsumption_resolution, [], [f233, f100])).
fof(f100, plain, (! [X0] : (outputs(X0, good) | ~ program(X0) | program(sK5(X0))) | ~ spl11_10), inference(avatar_component_clause, [], [f99])).
fof(f233, plain, ! [X0, X1] : (~ outputs(X0, good) | ~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0) | ~ outputs(X0, bad)), inference(duplicate_literal_removal, [], [f232])).
fof(f232, plain, ! [X0, X1] : (~ outputs(X0, good) | ~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0) | ~ outputs(X0, bad) | ~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0)), inference(resolution, [], [f134, f139])).
fof(f139, plain, ! [X2, X3] : (halts2(sK7(X2), X3) | ~ outputs(X2, bad) | ~ program(X3) | ~ sP1(X2) | program(sK5(X2)) | ~ program(X2)), inference(duplicate_literal_removal, [], [f138])).
fof(f138, plain, ! [X2, X3] : (~ outputs(X2, bad) | halts2(sK7(X2), X3) | ~ program(X3) | ~ sP1(X2) | ~ program(X3) | program(sK5(X2)) | ~ program(X2)), inference(resolution, [], [f53, f103])).
fof(f134, plain, ! [X2, X3] : (~ halts2(sK7(X2), X3) | ~ outputs(X2, good) | ~ program(X3) | ~ sP1(X2) | program(sK5(X2)) | ~ program(X2)), inference(duplicate_literal_removal, [], [f133])).
fof(f133, plain, ! [X2, X3] : (~ outputs(X2, good) | ~ halts2(sK7(X2), X3) | ~ program(X3) | ~ sP1(X2) | ~ program(X3) | program(sK5(X2)) | ~ program(X2)), inference(resolution, [], [f52, f103])).
fof(f276, plain, (spl11_22 | spl11_15 | ~ spl11_5 | ~ spl11_10), inference(avatar_split_clause, [], [f275, f99, f81, f150, f237])).
fof(f275, plain, (! [X0, X1] : (~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0)) | (~ spl11_5 | ~ spl11_10)), inference(subsumption_resolution, [], [f274, f51])).
fof(f274, plain, (! [X0, X1] : (~ program(sK7(X0)) | ~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0)) | (~ spl11_5 | ~ spl11_10)), inference(subsumption_resolution, [], [f262, f100])).
fof(f262, plain, (! [X0, X1] : (~ program(sK7(X0)) | ~ outputs(X0, good) | ~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0)) | ~ spl11_5), inference(resolution, [], [f82, f134])).
fof(f273, plain, (~ spl11_4 | ~ spl11_5 | ~ spl11_10 | spl11_13 | ~ spl11_22), inference(avatar_contradiction_clause, [], [f272])).
fof(f272, plain, ($false | (~ spl11_4 | ~ spl11_5 | ~ spl11_10 | spl11_13 | ~ spl11_22)), inference(subsumption_resolution, [], [f271, f77])).
fof(f271, plain, (~ program(sK2) | (~ spl11_5 | ~ spl11_10 | spl11_13 | ~ spl11_22)), inference(resolution, [], [f270, f124])).
fof(f270, plain, (! [X0] : (program(sK5(X0)) | ~ program(X0)) | (~ spl11_5 | ~ spl11_10 | ~ spl11_22)), inference(subsumption_resolution, [], [f269, f238])).
fof(f269, plain, (! [X0] : (~ program(X0) | program(sK5(X0)) | sP1(X0)) | (~ spl11_5 | ~ spl11_10 | ~ spl11_22)), inference(duplicate_literal_removal, [], [f268])).
fof(f268, plain, (! [X0] : (~ program(X0) | program(sK5(X0)) | sP1(X0) | ~ program(X0)) | (~ spl11_5 | ~ spl11_10 | ~ spl11_22)), inference(resolution, [], [f264, f104])).
fof(f264, plain, (! [X2] : (~ program(sK8(X2)) | ~ program(X2) | program(sK5(X2))) | (~ spl11_5 | ~ spl11_10 | ~ spl11_22)), inference(duplicate_literal_removal, [], [f263])).
fof(f263, plain, (! [X2] : (~ program(sK8(X2)) | ~ program(X2) | ~ program(sK8(X2)) | program(sK5(X2))) | (~ spl11_5 | ~ spl11_10 | ~ spl11_22)), inference(resolution, [], [f82, f247])).
fof(f247, plain, (! [X1] : (~ halts2(sK8(X1), sK9(X1)) | ~ program(X1) | ~ program(sK8(X1)) | program(sK5(X1))) | (~ spl11_10 | ~ spl11_22)), inference(subsumption_resolution, [], [f246, f238])).
fof(f246, plain, (! [X1] : (sP1(X1) | ~ program(X1) | ~ halts2(sK8(X1), sK9(X1)) | ~ program(sK8(X1)) | program(sK5(X1))) | ~ spl11_10), inference(subsumption_resolution, [], [f244, f100])).
fof(f244, plain, ! [X1] : (sP1(X1) | ~ program(X1) | ~ halts2(sK8(X1), sK9(X1)) | ~ outputs(X1, good) | ~ program(sK8(X1)) | program(sK5(X1))), inference(duplicate_literal_removal, [], [f243])).
fof(f243, plain, ! [X1] : (sP1(X1) | ~ program(X1) | ~ halts2(sK8(X1), sK9(X1)) | ~ outputs(X1, good) | ~ program(sK8(X1)) | program(sK5(X1)) | ~ program(X1)), inference(resolution, [], [f171, f103])).
fof(f260, plain, (~ spl11_4 | ~ spl11_7 | ~ spl11_10 | spl11_13 | ~ spl11_22), inference(avatar_contradiction_clause, [], [f259])).
fof(f259, plain, ($false | (~ spl11_4 | ~ spl11_7 | ~ spl11_10 | spl11_13 | ~ spl11_22)), inference(subsumption_resolution, [], [f258, f77])).
fof(f258, plain, (~ program(sK2) | (~ spl11_7 | ~ spl11_10 | spl11_13 | ~ spl11_22)), inference(resolution, [], [f257, f124])).
fof(f257, plain, (! [X0] : (program(sK5(X0)) | ~ program(X0)) | (~ spl11_7 | ~ spl11_10 | ~ spl11_22)), inference(subsumption_resolution, [], [f256, f238])).
fof(f256, plain, (! [X0] : (program(sK5(X0)) | ~ program(X0) | sP1(X0)) | (~ spl11_7 | ~ spl11_10 | ~ spl11_22)), inference(duplicate_literal_removal, [], [f255])).
fof(f255, plain, (! [X0] : (program(sK5(X0)) | ~ program(X0) | sP1(X0) | ~ program(X0)) | (~ spl11_7 | ~ spl11_10 | ~ spl11_22)), inference(resolution, [], [f252, f104])).
fof(f252, plain, (! [X1] : (~ program(sK8(X1)) | program(sK5(X1)) | ~ program(X1)) | (~ spl11_7 | ~ spl11_10 | ~ spl11_22)), inference(subsumption_resolution, [], [f250, f238])).
fof(f250, plain, (! [X1] : (~ program(X1) | program(sK5(X1)) | sP1(X1) | ~ program(sK8(X1))) | (~ spl11_7 | ~ spl11_10)), inference(duplicate_literal_removal, [], [f249])).
fof(f249, plain, (! [X1] : (~ program(X1) | program(sK5(X1)) | sP1(X1) | ~ program(sK8(X1)) | program(sK5(X1)) | ~ program(X1)) | (~ spl11_7 | ~ spl11_10)), inference(resolution, [], [f231, f103])).
fof(f231, plain, (! [X0] : (~ halts3(X0, sK8(X0), sK9(X0)) | ~ program(X0) | program(sK5(X0)) | sP1(X0)) | (~ spl11_7 | ~ spl11_10)), inference(subsumption_resolution, [], [f228, f100])).
fof(f228, plain, (! [X0] : (sP1(X0) | ~ program(X0) | program(sK5(X0)) | ~ halts3(X0, sK8(X0), sK9(X0)) | ~ outputs(X0, good)) | ~ spl11_7), inference(resolution, [], [f225, f57])).
fof(f225, plain, (! [X1] : (sP0(X1, sK9(X1), sK8(X1)) | sP1(X1) | ~ program(X1) | program(sK5(X1))) | ~ spl11_7), inference(subsumption_resolution, [], [f224, f104])).
fof(f224, plain, (! [X1] : (sP1(X1) | sP0(X1, sK9(X1), sK8(X1)) | ~ program(X1) | ~ program(sK8(X1)) | program(sK5(X1))) | ~ spl11_7), inference(subsumption_resolution, [], [f199, f89])).
fof(f239, plain, (spl11_22 | spl11_15 | ~ spl11_7 | ~ spl11_10), inference(avatar_split_clause, [], [f235, f99, f88, f150, f237])).
fof(f235, plain, (! [X0, X1] : (~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0)) | (~ spl11_7 | ~ spl11_10)), inference(subsumption_resolution, [], [f234, f100])).
fof(f234, plain, (! [X0, X1] : (~ outputs(X0, good) | ~ program(X1) | ~ sP1(X0) | program(sK5(X0)) | ~ program(X0)) | ~ spl11_7), inference(subsumption_resolution, [], [f233, f89])).
fof(f223, plain, (spl11_21 | spl11_15 | ~ spl11_7 | ~ spl11_8), inference(avatar_split_clause, [], [f219, f92, f88, f150, f221])).
fof(f219, plain, (! [X2, X3] : (~ program(X3) | program(sK5(X2)) | ~ program(X2) | ~ program(sK7(X2))) | (~ spl11_7 | ~ spl11_8)), inference(subsumption_resolution, [], [f218, f208])).
fof(f208, plain, (! [X0] : (~ program(X0) | program(sK5(X0)) | sP1(X0)) | (~ spl11_7 | ~ spl11_8)), inference(subsumption_resolution, [], [f207, f104])).
fof(f207, plain, (! [X0] : (~ program(X0) | program(sK5(X0)) | sP1(X0) | ~ program(sK8(X0))) | (~ spl11_7 | ~ spl11_8)), inference(resolution, [], [f205, f93])).
fof(f205, plain, (! [X1] : (halts2(sK8(X1), sK9(X1)) | ~ program(X1) | program(sK5(X1)) | sP1(X1)) | (~ spl11_7 | ~ spl11_8)), inference(resolution, [], [f203, f56])).
fof(f203, plain, (! [X0] : (sP0(X0, sK9(X0), sK8(X0)) | sP1(X0) | ~ program(X0) | program(sK5(X0))) | (~ spl11_7 | ~ spl11_8)), inference(subsumption_resolution, [], [f202, f104])).
fof(f202, plain, (! [X0] : (sP1(X0) | sP0(X0, sK9(X0), sK8(X0)) | ~ program(X0) | ~ program(sK8(X0)) | program(sK5(X0))) | (~ spl11_7 | ~ spl11_8)), inference(subsumption_resolution, [], [f201, f93])).
fof(f201, plain, (! [X0] : (sP1(X0) | sP0(X0, sK9(X0), sK8(X0)) | ~ program(X0) | halts2(sK8(X0), sK9(X0)) | ~ program(sK8(X0)) | program(sK5(X0))) | ~ spl11_7), inference(subsumption_resolution, [], [f200, f89])).
fof(f200, plain, ! [X0] : (~ outputs(X0, bad) | sP1(X0) | sP0(X0, sK9(X0), sK8(X0)) | ~ program(X0) | halts2(sK8(X0), sK9(X0)) | ~ program(sK8(X0)) | program(sK5(X0))), inference(duplicate_literal_removal, [], [f197])).
fof(f197, plain, ! [X0] : (~ outputs(X0, bad) | sP1(X0) | sP0(X0, sK9(X0), sK8(X0)) | ~ program(X0) | halts2(sK8(X0), sK9(X0)) | ~ program(sK8(X0)) | program(sK5(X0)) | ~ program(X0)), inference(resolution, [], [f60, f47])).
fof(f218, plain, (! [X2, X3] : (~ program(X3) | ~ sP1(X2) | program(sK5(X2)) | ~ program(X2) | ~ program(sK7(X2))) | (~ spl11_7 | ~ spl11_8)), inference(subsumption_resolution, [], [f210, f89])).
fof(f210, plain, (! [X2, X3] : (~ outputs(X2, bad) | ~ program(X3) | ~ sP1(X2) | program(sK5(X2)) | ~ program(X2) | ~ program(sK7(X2))) | ~ spl11_8), inference(resolution, [], [f139, f93])).
fof(f192, plain, (~ spl11_1 | ~ spl11_3), inference(avatar_contradiction_clause, [], [f191])).
fof(f191, plain, ($false | (~ spl11_1 | ~ spl11_3)), inference(subsumption_resolution, [], [f190, f61])).
fof(f61, plain, algorithm(sK10), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (! [X1] : (! [X2] : decides(sK10, X1, X2) | ~ program(X1)) & algorithm(sK10)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f16, f37])).
fof(f37, plain, (? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & algorithm(X0)) => (! [X1] : (! [X2] : decides(sK10, X1, X2) | ~ program(X1)) & algorithm(sK10))), introduced(choice_axiom, [])).
fof(f16, plain, ? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & algorithm(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)), inference(flattening, [], [f9])).
fof(f9, plain, ~ ~ ? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)), inference(rectify, [], [f5])).
fof(f5, plain, ~ ~ ? [X5] : (! [X6] : (program(X6) => ! [X7] : decides(X5, X6, X7)) & algorithm(X5)), inference(negated_conjecture, [], [f4])).
fof(f4, plain, ~ ~ ? [X5] : (! [X6] : (program(X6) => ! [X7] : decides(X5, X6, X7)) & algorithm(X5)), file('/home/ubuntu/library/tptp/Problems/COM/COM003+3.p', prove_this)).
fof(f190, plain, (~ algorithm(sK10) | (~ spl11_1 | ~ spl11_3)), inference(resolution, [], [f178, f72])).
fof(f72, plain, (! [X3] : (program(sK3(X3)) | ~ algorithm(X3)) | ~ spl11_3), inference(avatar_component_clause, [], [f71])).
fof(f71, plain, (spl11_3 <=> ! [X3] : (program(sK3(X3)) | ~ algorithm(X3))), introduced(avatar_definition, [new_symbols(naming, [spl11_3])])).
fof(f178, plain, (~ program(sK3(sK10)) | ~ spl11_1), inference(subsumption_resolution, [], [f169, f61])).
fof(f169, plain, (~ algorithm(sK10) | ~ program(sK3(sK10)) | ~ spl11_1), inference(resolution, [], [f65, f62])).
fof(f62, plain, ! [X2, X1] : (decides(sK10, X1, X2) | ~ program(X1)), inference(cnf_transformation, [], [f38])).
fof(f65, plain, (! [X3] : (~ decides(X3, sK3(X3), sK4(X3)) | ~ algorithm(X3)) | ~ spl11_1), inference(avatar_component_clause, [], [f64])).
fof(f64, plain, (spl11_1 <=> ! [X3] : (~ decides(X3, sK3(X3), sK4(X3)) | ~ algorithm(X3))), introduced(avatar_definition, [new_symbols(naming, [spl11_1])])).
fof(f177, plain, (~ spl11_4 | ~ spl11_15), inference(avatar_contradiction_clause, [], [f176])).
fof(f176, plain, ($false | (~ spl11_4 | ~ spl11_15)), inference(subsumption_resolution, [], [f77, f151])).
fof(f151, plain, (! [X1] : ~ program(X1) | ~ spl11_15), inference(avatar_component_clause, [], [f150])).
fof(f165, plain, (spl11_15 | ~ spl11_5 | ~ spl11_8), inference(avatar_split_clause, [], [f164, f92, f81, f150])).
fof(f164, plain, (! [X1] : ~ program(X1) | (~ spl11_5 | ~ spl11_8)), inference(subsumption_resolution, [], [f82, f93])).
fof(f101, plain, (spl11_8 | spl11_10), inference(avatar_split_clause, [], [f45, f99, f92])).
fof(f45, plain, ! [X2, X0, X1] : (outputs(X0, good) | ~ halts2(X1, X2) | ~ program(X1) | program(sK5(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f97, plain, (spl11_8 | spl11_9), inference(avatar_split_clause, [], [f46, f95, f92])).
fof(f46, plain, ! [X2, X0, X1] : (outputs(X0, good) | ~ halts2(X1, X2) | ~ program(X1) | ~ decides(X0, sK5(X0), sK6(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f90, plain, (spl11_5 | spl11_7), inference(avatar_split_clause, [], [f49, f88, f81])).
fof(f49, plain, ! [X2, X0, X1] : (outputs(X0, bad) | halts2(X1, X2) | ~ program(X1) | program(sK5(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f86, plain, (spl11_5 | spl11_6), inference(avatar_split_clause, [], [f50, f84, f81])).
fof(f50, plain, ! [X2, X0, X1] : (outputs(X0, bad) | halts2(X1, X2) | ~ program(X1) | ~ decides(X0, sK5(X0), sK6(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f28])).
fof(f79, plain, (spl11_3 | spl11_4), inference(avatar_split_clause, [], [f39, f75, f71])).
fof(f39, plain, ! [X3] : (program(sK2) | program(sK3(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ((! [X1] : (! [X2] : decides(sK2, X1, X2) | ~ program(X1)) & program(sK2)) | ! [X3] : ((~ decides(X3, sK3(X3), sK4(X3)) & program(sK3(X3))) | ~ algorithm(X3))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4])], [f20, f23, f22, f21])).
fof(f21, plain, (? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & program(X0)) => (! [X1] : (! [X2] : decides(sK2, X1, X2) | ~ program(X1)) & program(sK2))), introduced(choice_axiom, [])).
fof(f22, plain, ! [X3] : (? [X4] : (? [X5] : ~ decides(X3, X4, X5) & program(X4)) => (? [X5] : ~ decides(X3, sK3(X3), X5) & program(sK3(X3)))), introduced(choice_axiom, [])).
fof(f23, plain, ! [X3] : (? [X5] : ~ decides(X3, sK3(X3), X5) => ~ decides(X3, sK3(X3), sK4(X3))), introduced(choice_axiom, [])).
fof(f20, plain, (? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & program(X0)) | ! [X3] : (? [X4] : (? [X5] : ~ decides(X3, X4, X5) & program(X4)) | ~ algorithm(X3))), inference(rectify, [], [f11])).
fof(f11, plain, (? [X3] : (! [X4] : (! [X5] : decides(X3, X4, X5) | ~ program(X4)) & program(X3)) | ! [X0] : (? [X1] : (? [X2] : ~ decides(X0, X1, X2) & program(X1)) | ~ algorithm(X0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, (? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)) => ? [X3] : (! [X4] : (program(X4) => ! [X5] : decides(X3, X4, X5)) & program(X3))), inference(rectify, [], [f1])).
fof(f1, plain, (? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)) => ? [X3] : (! [X1] : (program(X1) => ! [X2] : decides(X3, X1, X2)) & program(X3))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+3.p', p1)).
fof(f78, plain, (spl11_1 | spl11_4), inference(avatar_split_clause, [], [f40, f75, f64])).
fof(f40, plain, ! [X3] : (program(sK2) | ~ decides(X3, sK3(X3), sK4(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f24])).
fof(f73, plain, (spl11_3 | spl11_2), inference(avatar_split_clause, [], [f41, f67, f71])).
fof(f41, plain, ! [X2, X3, X1] : (decides(sK2, X1, X2) | ~ program(X1) | program(sK3(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f24])).
fof(f69, plain, (spl11_1 | spl11_2), inference(avatar_split_clause, [], [f42, f67, f64])).
fof(f42, plain, ! [X2, X3, X1] : (decides(sK2, X1, X2) | ~ program(X1) | ~ decides(X3, sK3(X3), sK4(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f24])).