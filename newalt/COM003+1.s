fof(f525, plain, $false, inference(avatar_sat_refutation, [], [f93, f97, f102, f103, f110, f114, f121, f125, f139, f143, f151, f155, f160, f164, f173, f186, f190, f195, f199, f203, f208, f217, f219, f307, f326, f349, f351, f353, f357, f368, f386, f395, f397, f413, f422, f428, f447, f495, f509, f516, f520, f524])).
fof(f524, plain, (~ spl14_4 | ~ spl14_7 | spl14_32 | spl14_41), inference(avatar_contradiction_clause, [], [f523])).
fof(f523, plain, ($false | (~ spl14_4 | ~ spl14_7 | spl14_32 | spl14_41)), inference(subsumption_resolution, [], [f522, f263])).
fof(f263, plain, (~ program(sK7(sK4)) | spl14_32), inference(avatar_component_clause, [], [f261])).
fof(f261, plain, (spl14_32 <=> program(sK7(sK4))), introduced(avatar_definition, [new_symbols(naming, [spl14_32])])).
fof(f522, plain, (program(sK7(sK4)) | (~ spl14_4 | ~ spl14_7 | spl14_41)), inference(subsumption_resolution, [], [f521, f101])).
fof(f101, plain, (program(sK4) | ~ spl14_4), inference(avatar_component_clause, [], [f99])).
fof(f99, plain, (spl14_4 <=> program(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl14_4])])).
fof(f521, plain, (~ program(sK4) | program(sK7(sK4)) | (~ spl14_7 | spl14_41)), inference(resolution, [], [f426, f113])).
fof(f113, plain, (! [X0] : (outputs(X0, bad) | ~ program(X0) | program(sK7(X0))) | ~ spl14_7), inference(avatar_component_clause, [], [f112])).
fof(f112, plain, (spl14_7 <=> ! [X0] : (outputs(X0, bad) | ~ program(X0) | program(sK7(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_7])])).
fof(f426, plain, (~ outputs(sK4, bad) | spl14_41), inference(avatar_component_clause, [], [f425])).
fof(f425, plain, (spl14_41 <=> outputs(sK4, bad)), introduced(avatar_definition, [new_symbols(naming, [spl14_41])])).
fof(f520, plain, (~ spl14_41 | spl14_43 | ~ spl14_4 | ~ spl14_19 | ~ spl14_34 | ~ spl14_42), inference(avatar_split_clause, [], [f519, f450, f310, f162, f99, f454, f425])).
fof(f454, plain, (spl14_43 <=> sP0(sK4, sK10(sK4))), introduced(avatar_definition, [new_symbols(naming, [spl14_43])])).
fof(f162, plain, (spl14_19 <=> ! [X0] : (~ outputs(X0, bad) | ~ program(X0) | sP0(X0, sK10(X0)) | ~ halts3(X0, sK10(X0), sK10(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_19])])).
fof(f310, plain, (spl14_34 <=> ! [X3, X2] : (~ program(X2) | halts3(sK4, X2, X3))), introduced(avatar_definition, [new_symbols(naming, [spl14_34])])).
fof(f450, plain, (spl14_42 <=> program(sK10(sK4))), introduced(avatar_definition, [new_symbols(naming, [spl14_42])])).
fof(f519, plain, (sP0(sK4, sK10(sK4)) | ~ outputs(sK4, bad) | (~ spl14_4 | ~ spl14_19 | ~ spl14_34 | ~ spl14_42)), inference(subsumption_resolution, [], [f442, f451])).
fof(f451, plain, (program(sK10(sK4)) | ~ spl14_42), inference(avatar_component_clause, [], [f450])).
fof(f442, plain, (sP0(sK4, sK10(sK4)) | ~ outputs(sK4, bad) | ~ program(sK10(sK4)) | (~ spl14_4 | ~ spl14_19 | ~ spl14_34)), inference(subsumption_resolution, [], [f433, f101])).
fof(f433, plain, (~ program(sK4) | sP0(sK4, sK10(sK4)) | ~ outputs(sK4, bad) | ~ program(sK10(sK4)) | (~ spl14_19 | ~ spl14_34)), inference(resolution, [], [f163, f311])).
fof(f311, plain, (! [X2, X3] : (halts3(sK4, X2, X3) | ~ program(X2)) | ~ spl14_34), inference(avatar_component_clause, [], [f310])).
fof(f163, plain, (! [X0] : (~ halts3(X0, sK10(X0), sK10(X0)) | ~ program(X0) | sP0(X0, sK10(X0)) | ~ outputs(X0, bad)) | ~ spl14_19), inference(avatar_component_clause, [], [f162])).
fof(f516, plain, (~ spl14_8 | ~ spl14_42 | ~ spl14_43), inference(avatar_contradiction_clause, [], [f515])).
fof(f515, plain, ($false | (~ spl14_8 | ~ spl14_42 | ~ spl14_43)), inference(subsumption_resolution, [], [f514, f451])).
fof(f514, plain, (~ program(sK10(sK4)) | (~ spl14_8 | ~ spl14_43)), inference(resolution, [], [f511, f117])).
fof(f117, plain, (! [X2, X1] : (~ halts2(X1, X2) | ~ program(X1)) | ~ spl14_8), inference(avatar_component_clause, [], [f116])).
fof(f116, plain, (spl14_8 <=> ! [X1, X2] : (~ halts2(X1, X2) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_8])])).
fof(f511, plain, (halts2(sK10(sK4), sK10(sK4)) | ~ spl14_43), inference(resolution, [], [f456, f70])).
fof(f70, plain, ! [X0, X1] : (~ sP0(X0, X1) | halts2(X1, X1)), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0, X1] : (((~ outputs(X0, good) | ~ halts3(X0, X1, X1)) & halts2(X1, X1) & program(X1)) | ~ sP0(X0, X1)), inference(nnf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1] : (((~ outputs(X0, good) | ~ halts3(X0, X1, X1)) & halts2(X1, X1) & program(X1)) | ~ sP0(X0, X1)), inference(usedef, [], [e21])).
fof(e21, plain, ! [X0, X1] : (sP0(X0, X1) <=> ((~ outputs(X0, good) | ~ halts3(X0, X1, X1)) & halts2(X1, X1) & program(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f456, plain, (sP0(sK4, sK10(sK4)) | ~ spl14_43), inference(avatar_component_clause, [], [f454])).
fof(f509, plain, (~ spl14_4 | ~ spl14_21 | spl14_42), inference(avatar_contradiction_clause, [], [f508])).
fof(f508, plain, ($false | (~ spl14_4 | ~ spl14_21 | spl14_42)), inference(subsumption_resolution, [], [f507, f101])).
fof(f507, plain, (~ program(sK4) | (~ spl14_21 | spl14_42)), inference(resolution, [], [f452, f172])).
fof(f172, plain, (! [X0] : (program(sK10(X0)) | ~ program(X0)) | ~ spl14_21), inference(avatar_component_clause, [], [f171])).
fof(f171, plain, (spl14_21 <=> ! [X0] : (program(sK10(X0)) | ~ program(X0))), introduced(avatar_definition, [new_symbols(naming, [spl14_21])])).
fof(f452, plain, (~ program(sK10(sK4)) | spl14_42), inference(avatar_component_clause, [], [f450])).
fof(f495, plain, (~ spl14_8 | ~ spl14_14 | ~ spl14_18), inference(avatar_contradiction_clause, [], [f488])).
fof(f488, plain, ($false | (~ spl14_8 | ~ spl14_14 | ~ spl14_18)), inference(resolution, [], [f483, f159])).
fof(f159, plain, (program(sK9) | ~ spl14_18), inference(avatar_component_clause, [], [f157])).
fof(f157, plain, (spl14_18 <=> program(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl14_18])])).
fof(f483, plain, (! [X0] : ~ program(X0) | (~ spl14_8 | ~ spl14_14 | ~ spl14_18)), inference(subsumption_resolution, [], [f481, f159])).
fof(f481, plain, (! [X0] : (~ program(X0) | ~ program(sK9)) | (~ spl14_8 | ~ spl14_14)), inference(resolution, [], [f480, f117])).
fof(f480, plain, (! [X1] : (halts2(sK9, X1) | ~ program(X1)) | (~ spl14_8 | ~ spl14_14)), inference(subsumption_resolution, [], [f142, f117])).
fof(f142, plain, (! [X1] : (halts2(sK9, X1) | ~ program(X1) | halts2(X1, X1)) | ~ spl14_14), inference(avatar_component_clause, [], [f141])).
fof(f141, plain, (spl14_14 <=> ! [X1] : (halts2(sK9, X1) | ~ program(X1) | halts2(X1, X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_14])])).
fof(f447, plain, (~ spl14_4 | ~ spl14_19 | ~ spl14_21 | ~ spl14_33 | ~ spl14_34 | ~ spl14_41), inference(avatar_contradiction_clause, [], [f446])).
fof(f446, plain, ($false | (~ spl14_4 | ~ spl14_19 | ~ spl14_21 | ~ spl14_33 | ~ spl14_34 | ~ spl14_41)), inference(subsumption_resolution, [], [f445, f101])).
fof(f445, plain, (~ program(sK4) | (~ spl14_4 | ~ spl14_19 | ~ spl14_21 | ~ spl14_33 | ~ spl14_34 | ~ spl14_41)), inference(resolution, [], [f444, f172])).
fof(f444, plain, (~ program(sK10(sK4)) | (~ spl14_4 | ~ spl14_19 | ~ spl14_33 | ~ spl14_34 | ~ spl14_41)), inference(subsumption_resolution, [], [f443, f427])).
fof(f427, plain, (outputs(sK4, bad) | ~ spl14_41), inference(avatar_component_clause, [], [f425])).
fof(f443, plain, (~ outputs(sK4, bad) | ~ program(sK10(sK4)) | (~ spl14_4 | ~ spl14_19 | ~ spl14_33 | ~ spl14_34)), inference(subsumption_resolution, [], [f442, f407])).
fof(f407, plain, (! [X0] : ~ sP0(sK4, X0) | (~ spl14_33 | ~ spl14_34)), inference(subsumption_resolution, [], [f406, f69])).
fof(f69, plain, ! [X0, X1] : (~ sP0(X0, X1) | program(X1)), inference(cnf_transformation, [], [f40])).
fof(f406, plain, (! [X0] : (~ program(X0) | ~ sP0(sK4, X0)) | (~ spl14_33 | ~ spl14_34)), inference(subsumption_resolution, [], [f404, f267])).
fof(f267, plain, (outputs(sK4, good) | ~ spl14_33), inference(avatar_component_clause, [], [f265])).
fof(f265, plain, (spl14_33 <=> outputs(sK4, good)), introduced(avatar_definition, [new_symbols(naming, [spl14_33])])).
fof(f404, plain, (! [X0] : (~ program(X0) | ~ outputs(sK4, good) | ~ sP0(sK4, X0)) | ~ spl14_34), inference(resolution, [], [f311, f71])).
fof(f71, plain, ! [X0, X1] : (~ halts3(X0, X1, X1) | ~ outputs(X0, good) | ~ sP0(X0, X1)), inference(cnf_transformation, [], [f40])).
fof(f428, plain, (~ spl14_32 | spl14_41 | ~ spl14_2 | ~ spl14_4 | ~ spl14_6), inference(avatar_split_clause, [], [f423, f108, f99, f91, f425, f261])).
fof(f91, plain, (spl14_2 <=> ! [X1, X2] : (decides(sK4, X1, X2) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_2])])).
fof(f108, plain, (spl14_6 <=> ! [X0] : (outputs(X0, bad) | ~ program(X0) | ~ decides(X0, sK7(X0), sK8(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_6])])).
fof(f423, plain, (outputs(sK4, bad) | ~ program(sK7(sK4)) | (~ spl14_2 | ~ spl14_4 | ~ spl14_6)), inference(subsumption_resolution, [], [f370, f101])).
fof(f370, plain, (~ program(sK4) | outputs(sK4, bad) | ~ program(sK7(sK4)) | (~ spl14_2 | ~ spl14_6)), inference(resolution, [], [f109, f92])).
fof(f92, plain, (! [X2, X1] : (decides(sK4, X1, X2) | ~ program(X1)) | ~ spl14_2), inference(avatar_component_clause, [], [f91])).
fof(f109, plain, (! [X0] : (~ decides(X0, sK7(X0), sK8(X0)) | ~ program(X0) | outputs(X0, bad)) | ~ spl14_6), inference(avatar_component_clause, [], [f108])).
fof(f422, plain, (~ spl14_25 | ~ spl14_26 | ~ spl14_38), inference(avatar_contradiction_clause, [], [f421])).
fof(f421, plain, ($false | (~ spl14_25 | ~ spl14_26 | ~ spl14_38)), inference(subsumption_resolution, [], [f420, f348])).
fof(f348, plain, (halts2(sK11, sK11) | ~ spl14_38), inference(avatar_component_clause, [], [f346])).
fof(f346, plain, (spl14_38 <=> halts2(sK11, sK11)), introduced(avatar_definition, [new_symbols(naming, [spl14_38])])).
fof(f420, plain, (~ halts2(sK11, sK11) | (~ spl14_25 | ~ spl14_26 | ~ spl14_38)), inference(subsumption_resolution, [], [f416, f194])).
fof(f194, plain, (program(sK11) | ~ spl14_26), inference(avatar_component_clause, [], [f192])).
fof(f192, plain, (spl14_26 <=> program(sK11)), introduced(avatar_definition, [new_symbols(naming, [spl14_26])])).
fof(f416, plain, (~ program(sK11) | ~ halts2(sK11, sK11) | (~ spl14_25 | ~ spl14_38)), inference(resolution, [], [f189, f348])).
fof(f189, plain, (! [X1] : (~ halts2(sK11, X1) | ~ program(X1) | ~ halts2(X1, X1)) | ~ spl14_25), inference(avatar_component_clause, [], [f188])).
fof(f188, plain, (spl14_25 <=> ! [X1] : (~ halts2(sK11, X1) | ~ program(X1) | ~ halts2(X1, X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_25])])).
fof(f413, plain, (~ spl14_12 | ~ spl14_18 | ~ spl14_28 | ~ spl14_29 | spl14_40), inference(avatar_contradiction_clause, [], [f412])).
fof(f412, plain, ($false | (~ spl14_12 | ~ spl14_18 | ~ spl14_28 | ~ spl14_29 | spl14_40)), inference(subsumption_resolution, [], [f411, f159])).
fof(f411, plain, (~ program(sK9) | (~ spl14_12 | ~ spl14_28 | ~ spl14_29 | spl14_40)), inference(resolution, [], [f403, f381])).
fof(f381, plain, (~ sP2(sK9, sK12(sK9)) | spl14_40), inference(avatar_component_clause, [], [f380])).
fof(f380, plain, (spl14_40 <=> sP2(sK9, sK12(sK9))), introduced(avatar_definition, [new_symbols(naming, [spl14_40])])).
fof(f403, plain, (! [X2] : (sP2(X2, sK12(X2)) | ~ program(X2)) | (~ spl14_12 | ~ spl14_28 | ~ spl14_29)), inference(subsumption_resolution, [], [f401, f207])).
fof(f207, plain, (! [X0] : (program(sK12(X0)) | ~ program(X0)) | ~ spl14_29), inference(avatar_component_clause, [], [f206])).
fof(f206, plain, (spl14_29 <=> ! [X0] : (program(sK12(X0)) | ~ program(X0))), introduced(avatar_definition, [new_symbols(naming, [spl14_29])])).
fof(f401, plain, (! [X2] : (~ program(sK12(X2)) | ~ program(X2) | sP2(X2, sK12(X2))) | (~ spl14_12 | ~ spl14_28)), inference(resolution, [], [f134, f202])).
fof(f202, plain, (! [X0] : (~ halts2(sK12(X0), sK12(X0)) | ~ program(X0) | sP2(X0, sK12(X0))) | ~ spl14_28), inference(avatar_component_clause, [], [f201])).
fof(f201, plain, (spl14_28 <=> ! [X0] : (~ halts2(sK12(X0), sK12(X0)) | ~ program(X0) | sP2(X0, sK12(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_28])])).
fof(f134, plain, (! [X1] : (halts2(X1, X1) | ~ program(X1)) | ~ spl14_12), inference(avatar_component_clause, [], [f133])).
fof(f133, plain, (spl14_12 <=> ! [X1] : (halts2(X1, X1) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_12])])).
fof(f397, plain, (~ spl14_13 | spl14_40 | ~ spl14_14 | ~ spl14_17 | ~ spl14_18 | ~ spl14_27 | ~ spl14_39), inference(avatar_split_clause, [], [f396, f376, f197, f157, f153, f141, f380, f136])).
fof(f136, plain, (spl14_13 <=> outputs(sK9, bad)), introduced(avatar_definition, [new_symbols(naming, [spl14_13])])).
fof(f153, plain, (spl14_17 <=> ! [X1] : (halts2(sK9, X1) | ~ program(X1) | ~ halts2(X1, X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_17])])).
fof(f197, plain, (spl14_27 <=> ! [X0] : (~ outputs(X0, bad) | ~ program(X0) | sP2(X0, sK12(X0)) | ~ halts2(X0, sK12(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_27])])).
fof(f376, plain, (spl14_39 <=> program(sK12(sK9))), introduced(avatar_definition, [new_symbols(naming, [spl14_39])])).
fof(f396, plain, (sP2(sK9, sK12(sK9)) | ~ outputs(sK9, bad) | (~ spl14_14 | ~ spl14_17 | ~ spl14_18 | ~ spl14_27 | ~ spl14_39)), inference(subsumption_resolution, [], [f373, f377])).
fof(f377, plain, (program(sK12(sK9)) | ~ spl14_39), inference(avatar_component_clause, [], [f376])).
fof(f373, plain, (sP2(sK9, sK12(sK9)) | ~ outputs(sK9, bad) | ~ program(sK12(sK9)) | (~ spl14_14 | ~ spl14_17 | ~ spl14_18 | ~ spl14_27)), inference(subsumption_resolution, [], [f371, f159])).
fof(f371, plain, (~ program(sK9) | sP2(sK9, sK12(sK9)) | ~ outputs(sK9, bad) | ~ program(sK12(sK9)) | (~ spl14_14 | ~ spl14_17 | ~ spl14_27)), inference(resolution, [], [f198, f209])).
fof(f209, plain, (! [X1] : (halts2(sK9, X1) | ~ program(X1)) | (~ spl14_14 | ~ spl14_17)), inference(subsumption_resolution, [], [f142, f154])).
fof(f154, plain, (! [X1] : (~ halts2(X1, X1) | ~ program(X1) | halts2(sK9, X1)) | ~ spl14_17), inference(avatar_component_clause, [], [f153])).
fof(f198, plain, (! [X0] : (~ halts2(X0, sK12(X0)) | ~ program(X0) | sP2(X0, sK12(X0)) | ~ outputs(X0, bad)) | ~ spl14_27), inference(avatar_component_clause, [], [f197])).
fof(f395, plain, (~ spl14_14 | ~ spl14_16 | ~ spl14_17 | ~ spl14_39 | ~ spl14_40), inference(avatar_contradiction_clause, [], [f394])).
fof(f394, plain, ($false | (~ spl14_14 | ~ spl14_16 | ~ spl14_17 | ~ spl14_39 | ~ spl14_40)), inference(subsumption_resolution, [], [f393, f377])).
fof(f393, plain, (~ program(sK12(sK9)) | (~ spl14_14 | ~ spl14_16 | ~ spl14_17 | ~ spl14_40)), inference(resolution, [], [f392, f209])).
fof(f392, plain, (~ halts2(sK9, sK12(sK9)) | (~ spl14_16 | ~ spl14_40)), inference(subsumption_resolution, [], [f389, f150])).
fof(f150, plain, (outputs(sK9, good) | ~ spl14_16), inference(avatar_component_clause, [], [f148])).
fof(f148, plain, (spl14_16 <=> outputs(sK9, good)), introduced(avatar_definition, [new_symbols(naming, [spl14_16])])).
fof(f389, plain, (~ halts2(sK9, sK12(sK9)) | ~ outputs(sK9, good) | ~ spl14_40), inference(resolution, [], [f382, f81])).
fof(f81, plain, ! [X0, X1] : (~ sP2(X0, X1) | ~ halts2(X0, X1) | ~ outputs(X0, good)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ! [X0, X1] : (((~ outputs(X0, good) | ~ halts2(X0, X1)) & halts2(X1, X1) & program(X1)) | ~ sP2(X0, X1)), inference(nnf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : (((~ outputs(X0, good) | ~ halts2(X0, X1)) & halts2(X1, X1) & program(X1)) | ~ sP2(X0, X1)), inference(usedef, [], [e24])).
fof(e24, plain, ! [X0, X1] : (sP2(X0, X1) <=> ((~ outputs(X0, good) | ~ halts2(X0, X1)) & halts2(X1, X1) & program(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f382, plain, (sP2(sK9, sK12(sK9)) | ~ spl14_40), inference(avatar_component_clause, [], [f380])).
fof(f386, plain, (~ spl14_18 | ~ spl14_29 | spl14_39), inference(avatar_contradiction_clause, [], [f385])).
fof(f385, plain, ($false | (~ spl14_18 | ~ spl14_29 | spl14_39)), inference(subsumption_resolution, [], [f384, f159])).
fof(f384, plain, (~ program(sK9) | (~ spl14_29 | spl14_39)), inference(resolution, [], [f378, f207])).
fof(f378, plain, (~ program(sK12(sK9)) | spl14_39), inference(avatar_component_clause, [], [f376])).
fof(f368, plain, (~ spl14_14 | ~ spl14_15 | ~ spl14_17 | ~ spl14_18), inference(avatar_contradiction_clause, [], [f367])).
fof(f367, plain, ($false | (~ spl14_14 | ~ spl14_15 | ~ spl14_17 | ~ spl14_18)), inference(subsumption_resolution, [], [f365, f159])).
fof(f365, plain, (~ program(sK9) | (~ spl14_14 | ~ spl14_15 | ~ spl14_17)), inference(duplicate_literal_removal, [], [f362])).
fof(f362, plain, (~ program(sK9) | ~ program(sK9) | (~ spl14_14 | ~ spl14_15 | ~ spl14_17)), inference(resolution, [], [f146, f209])).
fof(f146, plain, (! [X1] : (~ halts2(X1, X1) | ~ program(X1)) | ~ spl14_15), inference(avatar_component_clause, [], [f145])).
fof(f145, plain, (spl14_15 <=> ! [X1] : (~ halts2(X1, X1) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_15])])).
fof(f357, plain, (~ spl14_4 | ~ spl14_7 | ~ spl14_10 | ~ spl14_19 | ~ spl14_21 | spl14_32), inference(avatar_contradiction_clause, [], [f356])).
fof(f356, plain, ($false | (~ spl14_4 | ~ spl14_7 | ~ spl14_10 | ~ spl14_19 | ~ spl14_21 | spl14_32)), inference(subsumption_resolution, [], [f355, f101])).
fof(f355, plain, (~ program(sK4) | (~ spl14_7 | ~ spl14_10 | ~ spl14_19 | ~ spl14_21 | spl14_32)), inference(resolution, [], [f263, f354])).
fof(f354, plain, (! [X0] : (program(sK7(X0)) | ~ program(X0)) | (~ spl14_7 | ~ spl14_10 | ~ spl14_19 | ~ spl14_21)), inference(subsumption_resolution, [], [f113, f337])).
fof(f337, plain, (! [X1] : (~ program(X1) | ~ outputs(X1, bad) | program(sK7(X1))) | (~ spl14_10 | ~ spl14_19 | ~ spl14_21)), inference(subsumption_resolution, [], [f336, f172])).
fof(f336, plain, (! [X1] : (~ program(X1) | ~ outputs(X1, bad) | ~ program(sK10(X1)) | program(sK7(X1))) | (~ spl14_10 | ~ spl14_19)), inference(subsumption_resolution, [], [f322, f271])).
fof(f271, plain, (! [X0, X1] : (~ sP0(X1, X0) | ~ program(X1) | program(sK7(X1))) | ~ spl14_10), inference(subsumption_resolution, [], [f270, f124])).
fof(f124, plain, (! [X0] : (outputs(X0, good) | ~ program(X0) | program(sK7(X0))) | ~ spl14_10), inference(avatar_component_clause, [], [f123])).
fof(f123, plain, (spl14_10 <=> ! [X0] : (outputs(X0, good) | ~ program(X0) | program(sK7(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_10])])).
fof(f270, plain, ! [X0, X1] : (program(sK7(X1)) | ~ program(X1) | ~ outputs(X1, good) | ~ sP0(X1, X0)), inference(subsumption_resolution, [], [f269, f69])).
fof(f269, plain, ! [X0, X1] : (~ program(X0) | program(sK7(X1)) | ~ program(X1) | ~ outputs(X1, good) | ~ sP0(X1, X0)), inference(resolution, [], [f127, f71])).
fof(f127, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | ~ program(X1) | program(sK7(X0)) | ~ program(X0)), inference(subsumption_resolution, [], [f56, f60])).
fof(f60, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | halts2(X1, X2) | ~ program(X1) | program(sK7(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : (! [X1, X2] : (((outputs(X0, bad) & halts3(X0, X1, X2)) | halts2(X1, X2) | ~ program(X1)) & ((outputs(X0, good) & halts3(X0, X1, X2)) | ~ halts2(X1, X2) | ~ program(X1))) | (~ decides(X0, sK7(X0), sK8(X0)) & program(sK7(X0))) | ~ program(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f32, f34, f33])).
fof(f33, plain, ! [X0] : (? [X3] : (? [X4] : ~ decides(X0, X3, X4) & program(X3)) => (? [X4] : ~ decides(X0, sK7(X0), X4) & program(sK7(X0)))), introduced(choice_axiom, [])).
fof(f34, plain, ! [X0] : (? [X4] : ~ decides(X0, sK7(X0), X4) => ~ decides(X0, sK7(X0), sK8(X0))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0] : (! [X1, X2] : (((outputs(X0, bad) & halts3(X0, X1, X2)) | halts2(X1, X2) | ~ program(X1)) & ((outputs(X0, good) & halts3(X0, X1, X2)) | ~ halts2(X1, X2) | ~ program(X1))) | ? [X3] : (? [X4] : ~ decides(X0, X3, X4) & program(X3)) | ~ program(X0)), inference(rectify, [], [f15])).
fof(f15, plain, ! [X0] : (! [X3, X4] : (((outputs(X0, bad) & halts3(X0, X3, X4)) | halts2(X3, X4) | ~ program(X3)) & ((outputs(X0, good) & halts3(X0, X3, X4)) | ~ halts2(X3, X4) | ~ program(X3))) | ? [X1] : (? [X2] : ~ decides(X0, X1, X2) & program(X1)) | ~ program(X0)), inference(flattening, [], [f14])).
fof(f14, plain, ! [X0] : (! [X3, X4] : (((outputs(X0, bad) & halts3(X0, X3, X4)) | (halts2(X3, X4) | ~ program(X3))) & ((outputs(X0, good) & halts3(X0, X3, X4)) | (~ halts2(X3, X4) | ~ program(X3)))) | (? [X1] : (? [X2] : ~ decides(X0, X1, X2) & program(X1)) | ~ program(X0))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : ((! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & program(X0)) => ! [X3, X4] : (((~ halts2(X3, X4) & program(X3)) => (outputs(X0, bad) & halts3(X0, X3, X4))) & ((halts2(X3, X4) & program(X3)) => (outputs(X0, good) & halts3(X0, X3, X4))))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X3] : ((! [X1] : (program(X1) => ! [X2] : decides(X3, X1, X2)) & program(X3)) => ! [X1, X2] : (((~ halts2(X1, X2) & program(X1)) => (outputs(X3, bad) & halts3(X3, X1, X2))) & ((halts2(X1, X2) & program(X1)) => (outputs(X3, good) & halts3(X3, X1, X2))))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+1.p', p2)).
fof(f56, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | ~ halts2(X1, X2) | ~ program(X1) | program(sK7(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f322, plain, (! [X1] : (~ program(X1) | sP0(X1, sK10(X1)) | ~ outputs(X1, bad) | ~ program(sK10(X1)) | program(sK7(X1))) | ~ spl14_19), inference(duplicate_literal_removal, [], [f321])).
fof(f321, plain, (! [X1] : (~ program(X1) | sP0(X1, sK10(X1)) | ~ outputs(X1, bad) | ~ program(sK10(X1)) | program(sK7(X1)) | ~ program(X1)) | ~ spl14_19), inference(resolution, [], [f163, f127])).
fof(f353, plain, (~ spl14_4 | spl14_34 | ~ spl14_2), inference(avatar_split_clause, [], [f352, f91, f310, f99])).
fof(f352, plain, (! [X2, X3] : (~ program(X2) | halts3(sK4, X2, X3) | ~ program(sK4)) | ~ spl14_2), inference(subsumption_resolution, [], [f302, f127])).
fof(f302, plain, (! [X2, X3] : (~ program(X2) | halts3(sK4, X2, X3) | ~ program(sK4) | ~ program(sK7(sK4))) | ~ spl14_2), inference(resolution, [], [f126, f92])).
fof(f126, plain, ! [X2, X0, X1] : (~ decides(X0, sK7(X0), sK8(X0)) | ~ program(X1) | halts3(X0, X1, X2) | ~ program(X0)), inference(subsumption_resolution, [], [f57, f61])).
fof(f61, plain, ! [X2, X0, X1] : (~ decides(X0, sK7(X0), sK8(X0)) | halts2(X1, X2) | ~ program(X1) | halts3(X0, X1, X2) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f57, plain, ! [X2, X0, X1] : (halts3(X0, X1, X2) | ~ halts2(X1, X2) | ~ program(X1) | ~ decides(X0, sK7(X0), sK8(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f351, plain, (spl14_33 | ~ spl14_4 | ~ spl14_2 | ~ spl14_9 | ~ spl14_10), inference(avatar_split_clause, [], [f350, f123, f119, f91, f99, f265])).
fof(f119, plain, (spl14_9 <=> ! [X0] : (outputs(X0, good) | ~ program(X0) | ~ decides(X0, sK7(X0), sK8(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl14_9])])).
fof(f350, plain, (~ program(sK4) | outputs(sK4, good) | (~ spl14_2 | ~ spl14_9 | ~ spl14_10)), inference(subsumption_resolution, [], [f248, f124])).
fof(f248, plain, (~ program(sK4) | outputs(sK4, good) | ~ program(sK7(sK4)) | (~ spl14_2 | ~ spl14_9)), inference(resolution, [], [f120, f92])).
fof(f120, plain, (! [X0] : (~ decides(X0, sK7(X0), sK8(X0)) | ~ program(X0) | outputs(X0, good)) | ~ spl14_9), inference(avatar_component_clause, [], [f119])).
fof(f349, plain, (~ spl14_26 | spl14_38 | ~ spl14_24), inference(avatar_split_clause, [], [f226, f184, f346, f192])).
fof(f184, plain, (spl14_24 <=> ! [X1] : (halts2(sK11, X1) | ~ program(X1) | halts2(X1, X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_24])])).
fof(f226, plain, (halts2(sK11, sK11) | ~ program(sK11) | ~ spl14_24), inference(factoring, [], [f185])).
fof(f185, plain, (! [X1] : (halts2(sK11, X1) | halts2(X1, X1) | ~ program(X1)) | ~ spl14_24), inference(avatar_component_clause, [], [f184])).
fof(f326, plain, (~ spl14_1 | ~ spl14_3), inference(avatar_contradiction_clause, [], [f325])).
fof(f325, plain, ($false | (~ spl14_1 | ~ spl14_3)), inference(subsumption_resolution, [], [f324, f85])).
fof(f85, plain, algorithm(sK13), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (! [X1] : (! [X2] : decides(sK13, X1, X2) | ~ program(X1)) & algorithm(sK13)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f20, f50])).
fof(f50, plain, (? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & algorithm(X0)) => (! [X1] : (! [X2] : decides(sK13, X1, X2) | ~ program(X1)) & algorithm(sK13))), introduced(choice_axiom, [])).
fof(f20, plain, ? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & algorithm(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)), inference(flattening, [], [f11])).
fof(f11, plain, ~ ~ ? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)), inference(rectify, [], [f6])).
fof(f6, plain, ~ ~ ? [X6] : (! [X7] : (program(X7) => ! [X8] : decides(X6, X7, X8)) & algorithm(X6)), inference(negated_conjecture, [], [f5])).
fof(f5, plain, ~ ~ ? [X6] : (! [X7] : (program(X7) => ! [X8] : decides(X6, X7, X8)) & algorithm(X6)), file('/home/ubuntu/library/tptp/Problems/COM/COM003+1.p', prove_this)).
fof(f324, plain, (~ algorithm(sK13) | (~ spl14_1 | ~ spl14_3)), inference(resolution, [], [f318, f96])).
fof(f96, plain, (! [X3] : (program(sK5(X3)) | ~ algorithm(X3)) | ~ spl14_3), inference(avatar_component_clause, [], [f95])).
fof(f95, plain, (spl14_3 <=> ! [X3] : (program(sK5(X3)) | ~ algorithm(X3))), introduced(avatar_definition, [new_symbols(naming, [spl14_3])])).
fof(f318, plain, (~ program(sK5(sK13)) | ~ spl14_1), inference(subsumption_resolution, [], [f316, f85])).
fof(f316, plain, (~ algorithm(sK13) | ~ program(sK5(sK13)) | ~ spl14_1), inference(resolution, [], [f89, f86])).
fof(f86, plain, ! [X2, X1] : (decides(sK13, X1, X2) | ~ program(X1)), inference(cnf_transformation, [], [f51])).
fof(f89, plain, (! [X3] : (~ decides(X3, sK5(X3), sK6(X3)) | ~ algorithm(X3)) | ~ spl14_1), inference(avatar_component_clause, [], [f88])).
fof(f88, plain, (spl14_1 <=> ! [X3] : (~ decides(X3, sK5(X3), sK6(X3)) | ~ algorithm(X3))), introduced(avatar_definition, [new_symbols(naming, [spl14_1])])).
fof(f307, plain, (~ spl14_4 | ~ spl14_5 | ~ spl14_28 | ~ spl14_29 | ~ spl14_33), inference(avatar_contradiction_clause, [], [f306])).
fof(f306, plain, ($false | (~ spl14_4 | ~ spl14_5 | ~ spl14_28 | ~ spl14_29 | ~ spl14_33)), inference(subsumption_resolution, [], [f305, f101])).
fof(f305, plain, (~ program(sK4) | (~ spl14_5 | ~ spl14_28 | ~ spl14_29 | ~ spl14_33)), inference(resolution, [], [f267, f278])).
fof(f278, plain, (! [X0] : (~ outputs(X0, good) | ~ program(X0)) | (~ spl14_5 | ~ spl14_28 | ~ spl14_29)), inference(subsumption_resolution, [], [f275, f106])).
fof(f106, plain, (! [X2, X1] : (halts2(X1, X2) | ~ program(X1)) | ~ spl14_5), inference(avatar_component_clause, [], [f105])).
fof(f105, plain, (spl14_5 <=> ! [X1, X2] : (halts2(X1, X2) | ~ program(X1))), introduced(avatar_definition, [new_symbols(naming, [spl14_5])])).
fof(f275, plain, (! [X0] : (~ program(X0) | ~ halts2(X0, sK12(X0)) | ~ outputs(X0, good)) | (~ spl14_5 | ~ spl14_28 | ~ spl14_29)), inference(resolution, [], [f274, f81])).
fof(f274, plain, (! [X0] : (sP2(X0, sK12(X0)) | ~ program(X0)) | (~ spl14_5 | ~ spl14_28 | ~ spl14_29)), inference(subsumption_resolution, [], [f272, f207])).
fof(f272, plain, (! [X0] : (~ program(X0) | sP2(X0, sK12(X0)) | ~ program(sK12(X0))) | (~ spl14_5 | ~ spl14_28)), inference(resolution, [], [f202, f106])).
fof(f219, plain, (~ spl14_4 | ~ spl14_5 | ~ spl14_8), inference(avatar_contradiction_clause, [], [f214])).
fof(f214, plain, ($false | (~ spl14_4 | ~ spl14_5 | ~ spl14_8)), inference(resolution, [], [f213, f101])).
fof(f213, plain, (! [X1] : ~ program(X1) | (~ spl14_5 | ~ spl14_8)), inference(subsumption_resolution, [], [f117, f106])).
fof(f217, plain, (~ spl14_5 | ~ spl14_8 | ~ spl14_26), inference(avatar_contradiction_clause, [], [f216])).
fof(f216, plain, ($false | (~ spl14_5 | ~ spl14_8 | ~ spl14_26)), inference(resolution, [], [f213, f194])).
fof(f208, plain, (spl14_29 | spl14_22), inference(avatar_split_clause, [], [f204, f175, f206])).
fof(f175, plain, (spl14_22 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl14_22])])).
fof(f204, plain, ! [X0] : (sP3 | program(sK12(X0)) | ~ program(X0)), inference(subsumption_resolution, [], [f82, f79])).
fof(f79, plain, ! [X0, X1] : (~ sP2(X0, X1) | program(X1)), inference(cnf_transformation, [], [f47])).
fof(f82, plain, ! [X0] : (sP3 | program(sK12(X0)) | sP2(X0, sK12(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (sP3 | ! [X0] : ((((~ outputs(X0, bad) | ~ halts2(X0, sK12(X0))) & ~ halts2(sK12(X0), sK12(X0)) & program(sK12(X0))) | sP2(X0, sK12(X0))) | ~ program(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f26, f48])).
fof(f48, plain, ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts2(X0, X1)) & ~ halts2(X1, X1) & program(X1)) | sP2(X0, X1)) => (((~ outputs(X0, bad) | ~ halts2(X0, sK12(X0))) & ~ halts2(sK12(X0), sK12(X0)) & program(sK12(X0))) | sP2(X0, sK12(X0)))), introduced(choice_axiom, [])).
fof(f26, plain, (sP3 | ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts2(X0, X1)) & ~ halts2(X1, X1) & program(X1)) | sP2(X0, X1)) | ~ program(X0))), inference(definition_folding, [], [f19, e25, e24])).
fof(f25, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & (~ halts2(X2, X3) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2)) | ~ sP3), inference(usedef, [], [e25])).
fof(e25, plain, (sP3 <=> ? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & (~ halts2(X2, X3) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f19, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & (~ halts2(X2, X3) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2)) | ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts2(X0, X1)) & ~ halts2(X1, X1) & program(X1)) | ((~ outputs(X0, good) | ~ halts2(X0, X1)) & halts2(X1, X1) & program(X1))) | ~ program(X0))), inference(flattening, [], [f18])).
fof(f18, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | (halts2(X3, X3) | ~ program(X3))) & (~ halts2(X2, X3) | (~ halts2(X3, X3) | ~ program(X3)))) & program(X2)) | ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts2(X0, X1)) & (~ halts2(X1, X1) & program(X1))) | ((~ outputs(X0, good) | ~ halts2(X0, X1)) & (halts2(X1, X1) & program(X1)))) | ~ program(X0))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, (? [X0] : (! [X1] : (((~ halts2(X1, X1) & program(X1)) => (outputs(X0, bad) & halts2(X0, X1))) & ((halts2(X1, X1) & program(X1)) => (outputs(X0, good) & halts2(X0, X1)))) & program(X0)) => ? [X2] : (! [X3] : (((~ halts2(X3, X3) & program(X3)) => (outputs(X2, bad) & halts2(X2, X3))) & ((halts2(X3, X3) & program(X3)) => ~ halts2(X2, X3))) & program(X2))), inference(rectify, [], [f4])).
fof(f4, plain, (? [X4] : (! [X1] : (((~ halts2(X1, X1) & program(X1)) => (outputs(X4, bad) & halts2(X4, X1))) & ((halts2(X1, X1) & program(X1)) => (outputs(X4, good) & halts2(X4, X1)))) & program(X4)) => ? [X5] : (! [X1] : (((~ halts2(X1, X1) & program(X1)) => (outputs(X5, bad) & halts2(X5, X1))) & ((halts2(X1, X1) & program(X1)) => ~ halts2(X5, X1))) & program(X5))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+1.p', p4)).
fof(f203, plain, (spl14_28 | spl14_22), inference(avatar_split_clause, [], [f83, f175, f201])).
fof(f83, plain, ! [X0] : (sP3 | ~ halts2(sK12(X0), sK12(X0)) | sP2(X0, sK12(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f49])).
fof(f199, plain, (spl14_27 | spl14_22), inference(avatar_split_clause, [], [f84, f175, f197])).
fof(f84, plain, ! [X0] : (sP3 | ~ outputs(X0, bad) | ~ halts2(X0, sK12(X0)) | sP2(X0, sK12(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f49])).
fof(f195, plain, (~ spl14_22 | spl14_26), inference(avatar_split_clause, [], [f75, f192, f175])).
fof(f75, plain, (program(sK11) | ~ sP3), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((! [X1] : (((outputs(sK11, bad) & halts2(sK11, X1)) | halts2(X1, X1) | ~ program(X1)) & (~ halts2(sK11, X1) | ~ halts2(X1, X1) | ~ program(X1))) & program(sK11)) | ~ sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f44, f45])).
fof(f45, plain, (? [X0] : (! [X1] : (((outputs(X0, bad) & halts2(X0, X1)) | halts2(X1, X1) | ~ program(X1)) & (~ halts2(X0, X1) | ~ halts2(X1, X1) | ~ program(X1))) & program(X0)) => (! [X1] : (((outputs(sK11, bad) & halts2(sK11, X1)) | halts2(X1, X1) | ~ program(X1)) & (~ halts2(sK11, X1) | ~ halts2(X1, X1) | ~ program(X1))) & program(sK11))), introduced(choice_axiom, [])).
fof(f44, plain, (? [X0] : (! [X1] : (((outputs(X0, bad) & halts2(X0, X1)) | halts2(X1, X1) | ~ program(X1)) & (~ halts2(X0, X1) | ~ halts2(X1, X1) | ~ program(X1))) & program(X0)) | ~ sP3), inference(rectify, [], [f43])).
fof(f43, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & (~ halts2(X2, X3) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2)) | ~ sP3), inference(nnf_transformation, [], [f25])).
fof(f190, plain, (~ spl14_22 | spl14_25), inference(avatar_split_clause, [], [f76, f188, f175])).
fof(f76, plain, ! [X1] : (~ halts2(sK11, X1) | ~ halts2(X1, X1) | ~ program(X1) | ~ sP3), inference(cnf_transformation, [], [f46])).
fof(f186, plain, (~ spl14_22 | spl14_24), inference(avatar_split_clause, [], [f77, f184, f175])).
fof(f77, plain, ! [X1] : (halts2(sK11, X1) | halts2(X1, X1) | ~ program(X1) | ~ sP3), inference(cnf_transformation, [], [f46])).
fof(f173, plain, (spl14_21 | spl14_11), inference(avatar_split_clause, [], [f169, f129, f171])).
fof(f129, plain, (spl14_11 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl14_11])])).
fof(f169, plain, ! [X0] : (sP1 | program(sK10(X0)) | ~ program(X0)), inference(subsumption_resolution, [], [f72, f69])).
fof(f72, plain, ! [X0] : (sP1 | program(sK10(X0)) | sP0(X0, sK10(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (sP1 | ! [X0] : ((((~ outputs(X0, bad) | ~ halts3(X0, sK10(X0), sK10(X0))) & ~ halts2(sK10(X0), sK10(X0)) & program(sK10(X0))) | sP0(X0, sK10(X0))) | ~ program(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f23, f41])).
fof(f41, plain, ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X1)) & ~ halts2(X1, X1) & program(X1)) | sP0(X0, X1)) => (((~ outputs(X0, bad) | ~ halts3(X0, sK10(X0), sK10(X0))) & ~ halts2(sK10(X0), sK10(X0)) & program(sK10(X0))) | sP0(X0, sK10(X0)))), introduced(choice_axiom, [])).
fof(f23, plain, (sP1 | ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X1)) & ~ halts2(X1, X1) & program(X1)) | sP0(X0, X1)) | ~ program(X0))), inference(definition_folding, [], [f17, e22, e21])).
fof(f22, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & ((outputs(X2, good) & halts2(X2, X3)) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2)) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> ? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & ((outputs(X2, good) & halts2(X2, X3)) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f17, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & ((outputs(X2, good) & halts2(X2, X3)) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2)) | ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X1)) & ~ halts2(X1, X1) & program(X1)) | ((~ outputs(X0, good) | ~ halts3(X0, X1, X1)) & halts2(X1, X1) & program(X1))) | ~ program(X0))), inference(flattening, [], [f16])).
fof(f16, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | (halts2(X3, X3) | ~ program(X3))) & ((outputs(X2, good) & halts2(X2, X3)) | (~ halts2(X3, X3) | ~ program(X3)))) & program(X2)) | ! [X0] : (? [X1] : (((~ outputs(X0, bad) | ~ halts3(X0, X1, X1)) & (~ halts2(X1, X1) & program(X1))) | ((~ outputs(X0, good) | ~ halts3(X0, X1, X1)) & (halts2(X1, X1) & program(X1)))) | ~ program(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, (? [X0] : (! [X1] : (((~ halts2(X1, X1) & program(X1)) => (outputs(X0, bad) & halts3(X0, X1, X1))) & ((halts2(X1, X1) & program(X1)) => (outputs(X0, good) & halts3(X0, X1, X1)))) & program(X0)) => ? [X2] : (! [X3] : (((~ halts2(X3, X3) & program(X3)) => (outputs(X2, bad) & halts2(X2, X3))) & ((halts2(X3, X3) & program(X3)) => (outputs(X2, good) & halts2(X2, X3)))) & program(X2))), inference(rectify, [], [f3])).
fof(f3, plain, (? [X3] : (! [X1] : (((~ halts2(X1, X1) & program(X1)) => (outputs(X3, bad) & halts3(X3, X1, X1))) & ((halts2(X1, X1) & program(X1)) => (outputs(X3, good) & halts3(X3, X1, X1)))) & program(X3)) => ? [X4] : (! [X1] : (((~ halts2(X1, X1) & program(X1)) => (outputs(X4, bad) & halts2(X4, X1))) & ((halts2(X1, X1) & program(X1)) => (outputs(X4, good) & halts2(X4, X1)))) & program(X4))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+1.p', p3)).
fof(f164, plain, (spl14_19 | spl14_11), inference(avatar_split_clause, [], [f74, f129, f162])).
fof(f74, plain, ! [X0] : (sP1 | ~ outputs(X0, bad) | ~ halts3(X0, sK10(X0), sK10(X0)) | sP0(X0, sK10(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f42])).
fof(f160, plain, (~ spl14_11 | spl14_18), inference(avatar_split_clause, [], [f64, f157, f129])).
fof(f64, plain, (program(sK9) | ~ sP1), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ((! [X1] : (((outputs(sK9, bad) & halts2(sK9, X1)) | halts2(X1, X1) | ~ program(X1)) & ((outputs(sK9, good) & halts2(sK9, X1)) | ~ halts2(X1, X1) | ~ program(X1))) & program(sK9)) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f37, f38])).
fof(f38, plain, (? [X0] : (! [X1] : (((outputs(X0, bad) & halts2(X0, X1)) | halts2(X1, X1) | ~ program(X1)) & ((outputs(X0, good) & halts2(X0, X1)) | ~ halts2(X1, X1) | ~ program(X1))) & program(X0)) => (! [X1] : (((outputs(sK9, bad) & halts2(sK9, X1)) | halts2(X1, X1) | ~ program(X1)) & ((outputs(sK9, good) & halts2(sK9, X1)) | ~ halts2(X1, X1) | ~ program(X1))) & program(sK9))), introduced(choice_axiom, [])).
fof(f37, plain, (? [X0] : (! [X1] : (((outputs(X0, bad) & halts2(X0, X1)) | halts2(X1, X1) | ~ program(X1)) & ((outputs(X0, good) & halts2(X0, X1)) | ~ halts2(X1, X1) | ~ program(X1))) & program(X0)) | ~ sP1), inference(rectify, [], [f36])).
fof(f36, plain, (? [X2] : (! [X3] : (((outputs(X2, bad) & halts2(X2, X3)) | halts2(X3, X3) | ~ program(X3)) & ((outputs(X2, good) & halts2(X2, X3)) | ~ halts2(X3, X3) | ~ program(X3))) & program(X2)) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f155, plain, (~ spl14_11 | spl14_17), inference(avatar_split_clause, [], [f65, f153, f129])).
fof(f65, plain, ! [X1] : (halts2(sK9, X1) | ~ halts2(X1, X1) | ~ program(X1) | ~ sP1), inference(cnf_transformation, [], [f39])).
fof(f151, plain, (~ spl14_11 | spl14_15 | spl14_16), inference(avatar_split_clause, [], [f66, f148, f145, f129])).
fof(f66, plain, ! [X1] : (outputs(sK9, good) | ~ halts2(X1, X1) | ~ program(X1) | ~ sP1), inference(cnf_transformation, [], [f39])).
fof(f143, plain, (~ spl14_11 | spl14_14), inference(avatar_split_clause, [], [f67, f141, f129])).
fof(f67, plain, ! [X1] : (halts2(sK9, X1) | halts2(X1, X1) | ~ program(X1) | ~ sP1), inference(cnf_transformation, [], [f39])).
fof(f139, plain, (~ spl14_11 | spl14_12 | spl14_13), inference(avatar_split_clause, [], [f68, f136, f133, f129])).
fof(f68, plain, ! [X1] : (outputs(sK9, bad) | halts2(X1, X1) | ~ program(X1) | ~ sP1), inference(cnf_transformation, [], [f39])).
fof(f125, plain, (spl14_8 | spl14_10), inference(avatar_split_clause, [], [f58, f123, f116])).
fof(f58, plain, ! [X2, X0, X1] : (outputs(X0, good) | ~ halts2(X1, X2) | ~ program(X1) | program(sK7(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f121, plain, (spl14_8 | spl14_9), inference(avatar_split_clause, [], [f59, f119, f116])).
fof(f59, plain, ! [X2, X0, X1] : (outputs(X0, good) | ~ halts2(X1, X2) | ~ program(X1) | ~ decides(X0, sK7(X0), sK8(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f114, plain, (spl14_5 | spl14_7), inference(avatar_split_clause, [], [f62, f112, f105])).
fof(f62, plain, ! [X2, X0, X1] : (outputs(X0, bad) | halts2(X1, X2) | ~ program(X1) | program(sK7(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f110, plain, (spl14_5 | spl14_6), inference(avatar_split_clause, [], [f63, f108, f105])).
fof(f63, plain, ! [X2, X0, X1] : (outputs(X0, bad) | halts2(X1, X2) | ~ program(X1) | ~ decides(X0, sK7(X0), sK8(X0)) | ~ program(X0)), inference(cnf_transformation, [], [f35])).
fof(f103, plain, (spl14_3 | spl14_4), inference(avatar_split_clause, [], [f52, f99, f95])).
fof(f52, plain, ! [X3] : (program(sK4) | program(sK5(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ((! [X1] : (! [X2] : decides(sK4, X1, X2) | ~ program(X1)) & program(sK4)) | ! [X3] : ((~ decides(X3, sK5(X3), sK6(X3)) & program(sK5(X3))) | ~ algorithm(X3))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6])], [f27, f30, f29, f28])).
fof(f28, plain, (? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & program(X0)) => (! [X1] : (! [X2] : decides(sK4, X1, X2) | ~ program(X1)) & program(sK4))), introduced(choice_axiom, [])).
fof(f29, plain, ! [X3] : (? [X4] : (? [X5] : ~ decides(X3, X4, X5) & program(X4)) => (? [X5] : ~ decides(X3, sK5(X3), X5) & program(sK5(X3)))), introduced(choice_axiom, [])).
fof(f30, plain, ! [X3] : (? [X5] : ~ decides(X3, sK5(X3), X5) => ~ decides(X3, sK5(X3), sK6(X3))), introduced(choice_axiom, [])).
fof(f27, plain, (? [X0] : (! [X1] : (! [X2] : decides(X0, X1, X2) | ~ program(X1)) & program(X0)) | ! [X3] : (? [X4] : (? [X5] : ~ decides(X3, X4, X5) & program(X4)) | ~ algorithm(X3))), inference(rectify, [], [f13])).
fof(f13, plain, (? [X3] : (! [X4] : (! [X5] : decides(X3, X4, X5) | ~ program(X4)) & program(X3)) | ! [X0] : (? [X1] : (? [X2] : ~ decides(X0, X1, X2) & program(X1)) | ~ algorithm(X0))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, (? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)) => ? [X3] : (! [X4] : (program(X4) => ! [X5] : decides(X3, X4, X5)) & program(X3))), inference(rectify, [], [f1])).
fof(f1, plain, (? [X0] : (! [X1] : (program(X1) => ! [X2] : decides(X0, X1, X2)) & algorithm(X0)) => ? [X3] : (! [X1] : (program(X1) => ! [X2] : decides(X3, X1, X2)) & program(X3))), file('/home/ubuntu/library/tptp/Problems/COM/COM003+1.p', p1)).
fof(f102, plain, (spl14_1 | spl14_4), inference(avatar_split_clause, [], [f53, f99, f88])).
fof(f53, plain, ! [X3] : (program(sK4) | ~ decides(X3, sK5(X3), sK6(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f31])).
fof(f97, plain, (spl14_3 | spl14_2), inference(avatar_split_clause, [], [f54, f91, f95])).
fof(f54, plain, ! [X2, X3, X1] : (decides(sK4, X1, X2) | ~ program(X1) | program(sK5(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f31])).
fof(f93, plain, (spl14_1 | spl14_2), inference(avatar_split_clause, [], [f55, f91, f88])).
fof(f55, plain, ! [X2, X3, X1] : (decides(sK4, X1, X2) | ~ program(X1) | ~ decides(X3, sK5(X3), sK6(X3)) | ~ algorithm(X3)), inference(cnf_transformation, [], [f31])).