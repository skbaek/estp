fof(f2572, plain, $false, inference(avatar_sat_refutation, [], [f121, f129, f135, f143, f147, f155, f163, f167, f171, f182, f186, f191, f195, f199, f203, f210, f215, f220, f221, f371, f388, f482, f485, f501, f510, f524, f529, f541, f758, f781, f798, f811, f813, f819, f838, f889, f940, f970, f1019, f1054, f1066, f1122, f1132, f1289, f1290, f1298, f1410, f1503, f1519, f1665, f1726, f1735, f1750, f1775, f1807, f1886, f1934, f2013, f2118, f2300, f2309, f2328, f2335, f2343, f2385, f2415, f2416, f2430, f2571])).
fof(f2571, plain, (spl23_35 | ~ spl23_12 | ~ spl23_37 | ~ spl23_61), inference(avatar_split_clause, [], [f2570, f1337, f505, f161, f472])).
fof(f472, plain, (spl23_35 <=> ! [X1] : ~ g_both(sK21, X1)), introduced(avatar_definition, [new_symbols(naming, [spl23_35])])).
fof(f161, plain, (spl23_12 <=> ! [X3, X0] : (~ h_false_only(X0, sK10(X0)) | ~ g_both(X0, X3) | g_true_only(X0, sK11(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl23_12])])).
fof(f505, plain, (spl23_37 <=> ! [X1] : h_false_only(sK21, X1)), introduced(avatar_definition, [new_symbols(naming, [spl23_37])])).
fof(f1337, plain, (spl23_61 <=> ! [X10] : ~ g_true_only(sK21, X10)), introduced(avatar_definition, [new_symbols(naming, [spl23_61])])).
fof(f2570, plain, (! [X2] : ~ g_both(sK21, X2) | (~ spl23_12 | ~ spl23_37 | ~ spl23_61)), inference(subsumption_resolution, [], [f2568, f1338])).
fof(f1338, plain, (! [X10] : ~ g_true_only(sK21, X10) | ~ spl23_61), inference(avatar_component_clause, [], [f1337])).
fof(f2568, plain, (! [X2] : (~ g_both(sK21, X2) | g_true_only(sK21, sK11(sK21))) | (~ spl23_12 | ~ spl23_37)), inference(resolution, [], [f506, f162])).
fof(f162, plain, (! [X0, X3] : (~ h_false_only(X0, sK10(X0)) | ~ g_both(X0, X3) | g_true_only(X0, sK11(X0))) | ~ spl23_12), inference(avatar_component_clause, [], [f161])).
fof(f506, plain, (! [X1] : h_false_only(sK21, X1) | ~ spl23_37), inference(avatar_component_clause, [], [f505])).
fof(f2430, plain, (~ spl23_27 | ~ spl23_39), inference(avatar_contradiction_clause, [], [f2429])).
fof(f2429, plain, ($false | (~ spl23_27 | ~ spl23_39)), inference(subsumption_resolution, [], [f366, f538])).
fof(f538, plain, (! [X3] : ~ h_both(sK21, X3) | ~ spl23_39), inference(avatar_component_clause, [], [f537])).
fof(f537, plain, (spl23_39 <=> ! [X3] : ~ h_both(sK21, X3)), introduced(avatar_definition, [new_symbols(naming, [spl23_39])])).
fof(f366, plain, (h_both(sK21, sK10(sK21)) | ~ spl23_27), inference(avatar_component_clause, [], [f364])).
fof(f364, plain, (spl23_27 <=> h_both(sK21, sK10(sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_27])])).
fof(f2416, plain, (~ spl23_4 | ~ spl23_49), inference(avatar_contradiction_clause, [], [f2409])).
fof(f2409, plain, ($false | (~ spl23_4 | ~ spl23_49)), inference(resolution, [], [f128, f1076])).
fof(f1076, plain, (~ h_false_only(sK4, sK10(sK4)) | ~ spl23_49), inference(resolution, [], [f1072, f109])).
fof(f109, plain, ! [X0, X1] : (~ h_true(X0, X1) | ~ h_false_only(X0, X1)), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1] : ((h_false_only(X0, X1) | h_true(X0, X1) | ~ h_false(X0, X1)) & ((~ h_true(X0, X1) & h_false(X0, X1)) | ~ h_false_only(X0, X1))), inference(flattening, [], [f66])).
fof(f66, plain, ! [X0, X1] : ((h_false_only(X0, X1) | (h_true(X0, X1) | ~ h_false(X0, X1))) & ((~ h_true(X0, X1) & h_false(X0, X1)) | ~ h_false_only(X0, X1))), inference(nnf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1] : (h_false_only(X0, X1) <=> (~ h_true(X0, X1) & h_false(X0, X1))), inference(rectify, [], [f9])).
fof(f9, plain, ! [X3, X4] : (h_false_only(X3, X4) <=> (~ h_true(X3, X4) & h_false(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', false_only_h)).
fof(f1072, plain, (h_true(sK4, sK10(sK4)) | ~ spl23_49), inference(resolution, [], [f899, f105])).
fof(f105, plain, ! [X0, X1] : (~ h_both(X0, X1) | h_true(X0, X1)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0, X1] : ((h_both(X0, X1) | ~ h_false(X0, X1) | ~ h_true(X0, X1)) & ((h_false(X0, X1) & h_true(X0, X1)) | ~ h_both(X0, X1))), inference(flattening, [], [f64])).
fof(f64, plain, ! [X0, X1] : ((h_both(X0, X1) | (~ h_false(X0, X1) | ~ h_true(X0, X1))) & ((h_false(X0, X1) & h_true(X0, X1)) | ~ h_both(X0, X1))), inference(nnf_transformation, [], [f17])).
fof(f17, plain, ! [X0, X1] : (h_both(X0, X1) <=> (h_false(X0, X1) & h_true(X0, X1))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X3, X4] : (h_both(X3, X4) <=> (h_false(X3, X4) & h_true(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', both_h)).
fof(f899, plain, (h_both(sK4, sK10(sK4)) | ~ spl23_49), inference(avatar_component_clause, [], [f897])).
fof(f897, plain, (spl23_49 <=> h_both(sK4, sK10(sK4))), introduced(avatar_definition, [new_symbols(naming, [spl23_49])])).
fof(f128, plain, (! [X2] : h_false_only(sK4, X2) | ~ spl23_4), inference(avatar_component_clause, [], [f127])).
fof(f127, plain, (spl23_4 <=> ! [X2] : h_false_only(sK4, X2)), introduced(avatar_definition, [new_symbols(naming, [spl23_4])])).
fof(f2415, plain, (~ spl23_4 | ~ spl23_40), inference(avatar_contradiction_clause, [], [f2410])).
fof(f2410, plain, ($false | (~ spl23_4 | ~ spl23_40)), inference(resolution, [], [f128, f1009])).
fof(f1009, plain, (~ h_false_only(sK4, sK12(sK4)) | ~ spl23_40), inference(resolution, [], [f938, f109])).
fof(f938, plain, (h_true(sK4, sK12(sK4)) | ~ spl23_40), inference(resolution, [], [f793, f105])).
fof(f793, plain, (h_both(sK4, sK12(sK4)) | ~ spl23_40), inference(avatar_component_clause, [], [f791])).
fof(f791, plain, (spl23_40 <=> h_both(sK4, sK12(sK4))), introduced(avatar_definition, [new_symbols(naming, [spl23_40])])).
fof(f2385, plain, (~ spl23_12 | ~ spl23_17 | ~ spl23_18 | ~ spl23_19), inference(avatar_contradiction_clause, [], [f2381])).
fof(f2381, plain, ($false | (~ spl23_12 | ~ spl23_17 | ~ spl23_18 | ~ spl23_19)), inference(resolution, [], [f2353, f190])).
fof(f190, plain, (g_both(sK14, sK15) | ~ spl23_19), inference(avatar_component_clause, [], [f188])).
fof(f188, plain, (spl23_19 <=> g_both(sK14, sK15)), introduced(avatar_definition, [new_symbols(naming, [spl23_19])])).
fof(f2353, plain, (! [X2] : ~ g_both(sK14, X2) | (~ spl23_12 | ~ spl23_17 | ~ spl23_18)), inference(subsumption_resolution, [], [f2351, f185])).
fof(f185, plain, (! [X2] : ~ g_true_only(sK14, X2) | ~ spl23_18), inference(avatar_component_clause, [], [f184])).
fof(f184, plain, (spl23_18 <=> ! [X2] : ~ g_true_only(sK14, X2)), introduced(avatar_definition, [new_symbols(naming, [spl23_18])])).
fof(f2351, plain, (! [X2] : (~ g_both(sK14, X2) | g_true_only(sK14, sK11(sK14))) | (~ spl23_12 | ~ spl23_17)), inference(resolution, [], [f181, f162])).
fof(f181, plain, (! [X1] : h_false_only(sK14, X1) | ~ spl23_17), inference(avatar_component_clause, [], [f180])).
fof(f180, plain, (spl23_17 <=> ! [X1] : h_false_only(sK14, X1)), introduced(avatar_definition, [new_symbols(naming, [spl23_17])])).
fof(f2343, plain, (spl23_65 | ~ spl23_12 | ~ spl23_13 | ~ spl23_56), inference(avatar_split_clause, [], [f2342, f1116, f165, f161, f1400])).
fof(f1400, plain, (spl23_65 <=> ! [X12] : ~ g_both(sK19, X12)), introduced(avatar_definition, [new_symbols(naming, [spl23_65])])).
fof(f165, plain, (spl23_13 <=> ! [X7, X0] : (~ h_false_only(X0, sK12(X0)) | ~ g_true_only(X0, X7))), introduced(avatar_definition, [new_symbols(naming, [spl23_13])])).
fof(f1116, plain, (spl23_56 <=> ! [X4] : h_false_only(sK19, X4)), introduced(avatar_definition, [new_symbols(naming, [spl23_56])])).
fof(f2342, plain, (! [X2] : ~ g_both(sK19, X2) | (~ spl23_12 | ~ spl23_13 | ~ spl23_56)), inference(subsumption_resolution, [], [f2338, f2339])).
fof(f2339, plain, (! [X3] : ~ g_true_only(sK19, X3) | (~ spl23_13 | ~ spl23_56)), inference(resolution, [], [f1117, f166])).
fof(f166, plain, (! [X0, X7] : (~ h_false_only(X0, sK12(X0)) | ~ g_true_only(X0, X7)) | ~ spl23_13), inference(avatar_component_clause, [], [f165])).
fof(f1117, plain, (! [X4] : h_false_only(sK19, X4) | ~ spl23_56), inference(avatar_component_clause, [], [f1116])).
fof(f2338, plain, (! [X2] : (~ g_both(sK19, X2) | g_true_only(sK19, sK11(sK19))) | (~ spl23_12 | ~ spl23_56)), inference(resolution, [], [f1117, f162])).
fof(f2335, plain, (spl23_53 | ~ spl23_14 | ~ spl23_24 | ~ spl23_32 | ~ spl23_65), inference(avatar_split_clause, [], [f2334, f1400, f385, f208, f169, f1062])).
fof(f1062, plain, (spl23_53 <=> ! [X11] : g_false_only(sK19, X11)), introduced(avatar_definition, [new_symbols(naming, [spl23_53])])).
fof(f169, plain, (spl23_14 <=> ! [X7, X0, X6] : (h_true_only(X0, sK13(X0)) | ~ g_true_only(X0, X7) | ~ h_both(X0, X6))), introduced(avatar_definition, [new_symbols(naming, [spl23_14])])).
fof(f208, plain, (spl23_24 <=> ! [X14] : ~ h_true_only(sK19, X14)), introduced(avatar_definition, [new_symbols(naming, [spl23_24])])).
fof(f385, plain, (spl23_32 <=> h_both(sK19, sK12(sK19))), introduced(avatar_definition, [new_symbols(naming, [spl23_32])])).
fof(f2334, plain, (! [X0] : g_false_only(sK19, X0) | (~ spl23_14 | ~ spl23_24 | ~ spl23_32 | ~ spl23_65)), inference(subsumption_resolution, [], [f2331, f2321])).
fof(f2321, plain, (! [X0] : ~ g_true_only(sK19, X0) | (~ spl23_14 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2316, f209])).
fof(f209, plain, (! [X14] : ~ h_true_only(sK19, X14) | ~ spl23_24), inference(avatar_component_clause, [], [f208])).
fof(f2316, plain, (! [X0] : (~ g_true_only(sK19, X0) | h_true_only(sK19, sK13(sK19))) | (~ spl23_14 | ~ spl23_32)), inference(resolution, [], [f387, f170])).
fof(f170, plain, (! [X6, X0, X7] : (~ h_both(X0, X6) | ~ g_true_only(X0, X7) | h_true_only(X0, sK13(X0))) | ~ spl23_14), inference(avatar_component_clause, [], [f169])).
fof(f387, plain, (h_both(sK19, sK12(sK19)) | ~ spl23_32), inference(avatar_component_clause, [], [f385])).
fof(f2331, plain, (! [X0] : (g_false_only(sK19, X0) | g_true_only(sK19, X0)) | ~ spl23_65), inference(resolution, [], [f1401, f101])).
fof(f101, plain, ! [X0, X1] : (g_both(X0, X1) | g_false_only(X0, X1) | g_true_only(X0, X1)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : (g_false_only(X0, X1) | g_both(X0, X1) | g_true_only(X0, X1)), inference(rectify, [], [f6])).
fof(f6, plain, ! [X3, X4] : (g_false_only(X3, X4) | g_both(X3, X4) | g_true_only(X3, X4)), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', exhaustion_g)).
fof(f1401, plain, (! [X12] : ~ g_both(sK19, X12) | ~ spl23_65), inference(avatar_component_clause, [], [f1400])).
fof(f2328, plain, (spl23_53 | spl23_56 | ~ spl23_8 | ~ spl23_14 | ~ spl23_24 | ~ spl23_32), inference(avatar_split_clause, [], [f2327, f385, f208, f169, f145, f1116, f1062])).
fof(f145, plain, (spl23_8 <=> ! [X3, X5, X0, X2] : (h_false_only(X0, X2) | ~ g_both(X0, X3) | ~ h_both(X0, X5) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl23_8])])).
fof(f2327, plain, (! [X6, X7] : (h_false_only(sK19, X6) | g_false_only(sK19, X7)) | (~ spl23_8 | ~ spl23_14 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2326, f2321])).
fof(f2326, plain, (! [X6, X7] : (h_false_only(sK19, X6) | g_false_only(sK19, X7) | g_true_only(sK19, X7)) | (~ spl23_8 | ~ spl23_14 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2322, f2321])).
fof(f2322, plain, (! [X6, X7] : (h_false_only(sK19, X6) | g_false_only(sK19, X7) | g_true_only(sK19, sK6(sK19)) | g_true_only(sK19, X7)) | (~ spl23_8 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2320, f209])).
fof(f2320, plain, (! [X6, X7] : (h_false_only(sK19, X6) | g_false_only(sK19, X7) | h_true_only(sK19, sK7(sK19, X7)) | g_true_only(sK19, sK6(sK19)) | g_true_only(sK19, X7)) | (~ spl23_8 | ~ spl23_32)), inference(resolution, [], [f387, f2133])).
fof(f2133, plain, (! [X2, X0, X3, X1] : (~ h_both(X0, X2) | h_false_only(X0, X1) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)) | g_true_only(X0, X3)) | ~ spl23_8), inference(duplicate_literal_removal, [], [f2131])).
fof(f2131, plain, (! [X2, X0, X3, X1] : (h_false_only(X0, X1) | ~ h_both(X0, X2) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)) | g_false_only(X0, X3) | g_true_only(X0, X3)) | ~ spl23_8), inference(resolution, [], [f146, f101])).
fof(f146, plain, (! [X2, X0, X5, X3] : (~ g_both(X0, X3) | h_false_only(X0, X2) | ~ h_both(X0, X5) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0))) | ~ spl23_8), inference(avatar_component_clause, [], [f145])).
fof(f2309, plain, (spl23_53 | ~ spl23_14 | ~ spl23_24 | ~ spl23_29 | ~ spl23_65), inference(avatar_split_clause, [], [f2308, f1400, f373, f208, f169, f1062])).
fof(f373, plain, (spl23_29 <=> h_both(sK19, sK10(sK19))), introduced(avatar_definition, [new_symbols(naming, [spl23_29])])).
fof(f2308, plain, (! [X0] : g_false_only(sK19, X0) | (~ spl23_14 | ~ spl23_24 | ~ spl23_29 | ~ spl23_65)), inference(subsumption_resolution, [], [f2305, f2130])).
fof(f2130, plain, (! [X0] : ~ g_true_only(sK19, X0) | (~ spl23_14 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f2126, f209])).
fof(f2126, plain, (! [X0] : (~ g_true_only(sK19, X0) | h_true_only(sK19, sK13(sK19))) | (~ spl23_14 | ~ spl23_29)), inference(resolution, [], [f375, f170])).
fof(f375, plain, (h_both(sK19, sK10(sK19)) | ~ spl23_29), inference(avatar_component_clause, [], [f373])).
fof(f2305, plain, (! [X0] : (g_false_only(sK19, X0) | g_true_only(sK19, X0)) | ~ spl23_65), inference(resolution, [], [f1401, f101])).
fof(f2300, plain, (spl23_53 | spl23_56 | ~ spl23_8 | ~ spl23_14 | ~ spl23_24 | ~ spl23_29), inference(avatar_split_clause, [], [f2299, f373, f208, f169, f145, f1116, f1062])).
fof(f2299, plain, (! [X33, X34] : (h_false_only(sK19, X33) | g_false_only(sK19, X34)) | (~ spl23_8 | ~ spl23_14 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f2298, f2130])).
fof(f2298, plain, (! [X33, X34] : (h_false_only(sK19, X33) | g_false_only(sK19, X34) | g_true_only(sK19, X34)) | (~ spl23_8 | ~ spl23_14 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f2297, f2130])).
fof(f2297, plain, (! [X33, X34] : (h_false_only(sK19, X33) | g_false_only(sK19, X34) | g_true_only(sK19, sK6(sK19)) | g_true_only(sK19, X34)) | (~ spl23_8 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f2148, f209])).
fof(f2148, plain, (! [X33, X34] : (h_false_only(sK19, X33) | g_false_only(sK19, X34) | h_true_only(sK19, sK7(sK19, X34)) | g_true_only(sK19, sK6(sK19)) | g_true_only(sK19, X34)) | (~ spl23_8 | ~ spl23_29)), inference(resolution, [], [f2133, f375])).
fof(f2118, plain, (spl23_53 | ~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_32), inference(avatar_split_clause, [], [f2117, f385, f208, f173, f169, f1062])).
fof(f173, plain, (spl23_15 <=> ! [X8, X4, X6] : (h_true_only(X4, sK16(X4)) | ~ g_both(X4, X8) | g_true_only(X4, sK17(X4)) | ~ h_both(X4, X6))), introduced(avatar_definition, [new_symbols(naming, [spl23_15])])).
fof(f2117, plain, (! [X6] : g_false_only(sK19, X6) | (~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2116, f2109])).
fof(f2109, plain, (! [X0] : ~ g_true_only(sK19, X0) | (~ spl23_14 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2104, f209])).
fof(f2104, plain, (! [X0] : (~ g_true_only(sK19, X0) | h_true_only(sK19, sK13(sK19))) | (~ spl23_14 | ~ spl23_32)), inference(resolution, [], [f387, f170])).
fof(f2116, plain, (! [X6] : (g_false_only(sK19, X6) | g_true_only(sK19, X6)) | (~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2115, f209])).
fof(f2115, plain, (! [X6] : (h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X6) | g_true_only(sK19, X6)) | (~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_32)), inference(subsumption_resolution, [], [f2108, f2109])).
fof(f2108, plain, (! [X6] : (g_true_only(sK19, sK17(sK19)) | h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X6) | g_true_only(sK19, X6)) | (~ spl23_15 | ~ spl23_32)), inference(resolution, [], [f387, f1820])).
fof(f1820, plain, (! [X2, X0, X1] : (~ h_both(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK16(X0)) | g_false_only(X0, X2) | g_true_only(X0, X2)) | ~ spl23_15), inference(resolution, [], [f174, f101])).
fof(f174, plain, (! [X6, X4, X8] : (~ g_both(X4, X8) | h_true_only(X4, sK16(X4)) | g_true_only(X4, sK17(X4)) | ~ h_both(X4, X6)) | ~ spl23_15), inference(avatar_component_clause, [], [f173])).
fof(f2013, plain, (spl23_33 | ~ spl23_12 | ~ spl23_23 | spl23_27 | ~ spl23_61), inference(avatar_split_clause, [], [f2012, f1337, f364, f205, f161, f441])).
fof(f441, plain, (spl23_33 <=> ! [X2] : g_false_only(sK21, X2)), introduced(avatar_definition, [new_symbols(naming, [spl23_33])])).
fof(f205, plain, (spl23_23 <=> ! [X16] : ~ h_true_only(sK21, X16)), introduced(avatar_definition, [new_symbols(naming, [spl23_23])])).
fof(f2012, plain, (! [X15] : g_false_only(sK21, X15) | (~ spl23_12 | ~ spl23_23 | spl23_27 | ~ spl23_61)), inference(subsumption_resolution, [], [f2011, f1338])).
fof(f2011, plain, (! [X15] : (g_false_only(sK21, X15) | g_true_only(sK21, sK11(sK21))) | (~ spl23_12 | ~ spl23_23 | spl23_27 | ~ spl23_61)), inference(subsumption_resolution, [], [f2010, f206])).
fof(f206, plain, (! [X16] : ~ h_true_only(sK21, X16) | ~ spl23_23), inference(avatar_component_clause, [], [f205])).
fof(f2010, plain, (! [X15] : (h_true_only(sK21, sK10(sK21)) | g_false_only(sK21, X15) | g_true_only(sK21, sK11(sK21))) | (~ spl23_12 | spl23_27 | ~ spl23_61)), inference(subsumption_resolution, [], [f1993, f365])).
fof(f365, plain, (~ h_both(sK21, sK10(sK21)) | spl23_27), inference(avatar_component_clause, [], [f364])).
fof(f1993, plain, (! [X15] : (h_both(sK21, sK10(sK21)) | h_true_only(sK21, sK10(sK21)) | g_false_only(sK21, X15) | g_true_only(sK21, sK11(sK21))) | (~ spl23_12 | ~ spl23_61)), inference(resolution, [], [f1795, f1338])).
fof(f1795, plain, (! [X0, X1] : (h_both(X0, sK10(X0)) | g_true_only(X0, X1) | h_true_only(X0, sK10(X0)) | g_false_only(X0, X1) | g_true_only(X0, sK11(X0))) | ~ spl23_12), inference(resolution, [], [f1737, f101])).
fof(f1737, plain, (! [X2, X3] : (~ g_both(X2, X3) | g_true_only(X2, sK11(X2)) | h_both(X2, sK10(X2)) | h_true_only(X2, sK10(X2))) | ~ spl23_12), inference(resolution, [], [f162, f111])).
fof(f111, plain, ! [X0, X1] : (h_false_only(X0, X1) | h_both(X0, X1) | h_true_only(X0, X1)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ! [X0, X1] : (h_false_only(X0, X1) | h_both(X0, X1) | h_true_only(X0, X1)), inference(rectify, [], [f10])).
fof(f10, plain, ! [X3, X4] : (h_false_only(X3, X4) | h_both(X3, X4) | h_true_only(X3, X4)), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', exhaustion_h)).
fof(f1934, plain, (spl23_53 | ~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_29), inference(avatar_split_clause, [], [f1933, f373, f208, f173, f169, f1062])).
fof(f1933, plain, (! [X6] : g_false_only(sK19, X6) | (~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f1932, f1925])).
fof(f1925, plain, (! [X0] : ~ g_true_only(sK19, X0) | (~ spl23_14 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f1920, f209])).
fof(f1920, plain, (! [X0] : (~ g_true_only(sK19, X0) | h_true_only(sK19, sK13(sK19))) | (~ spl23_14 | ~ spl23_29)), inference(resolution, [], [f375, f170])).
fof(f1932, plain, (! [X6] : (g_false_only(sK19, X6) | g_true_only(sK19, X6)) | (~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f1931, f209])).
fof(f1931, plain, (! [X6] : (h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X6) | g_true_only(sK19, X6)) | (~ spl23_14 | ~ spl23_15 | ~ spl23_24 | ~ spl23_29)), inference(subsumption_resolution, [], [f1924, f1925])).
fof(f1924, plain, (! [X6] : (g_true_only(sK19, sK17(sK19)) | h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X6) | g_true_only(sK19, X6)) | (~ spl23_15 | ~ spl23_29)), inference(resolution, [], [f375, f1820])).
fof(f1886, plain, (spl23_33 | ~ spl23_15 | ~ spl23_23 | ~ spl23_27 | ~ spl23_61), inference(avatar_split_clause, [], [f1885, f1337, f364, f205, f173, f441])).
fof(f1885, plain, (! [X6] : g_false_only(sK21, X6) | (~ spl23_15 | ~ spl23_23 | ~ spl23_27 | ~ spl23_61)), inference(subsumption_resolution, [], [f1884, f1338])).
fof(f1884, plain, (! [X6] : (g_false_only(sK21, X6) | g_true_only(sK21, X6)) | (~ spl23_15 | ~ spl23_23 | ~ spl23_27 | ~ spl23_61)), inference(subsumption_resolution, [], [f1883, f206])).
fof(f1883, plain, (! [X6] : (h_true_only(sK21, sK16(sK21)) | g_false_only(sK21, X6) | g_true_only(sK21, X6)) | (~ spl23_15 | ~ spl23_27 | ~ spl23_61)), inference(subsumption_resolution, [], [f1877, f1338])).
fof(f1877, plain, (! [X6] : (g_true_only(sK21, sK17(sK21)) | h_true_only(sK21, sK16(sK21)) | g_false_only(sK21, X6) | g_true_only(sK21, X6)) | (~ spl23_15 | ~ spl23_27)), inference(resolution, [], [f366, f1820])).
fof(f1807, plain, (spl23_33 | ~ spl23_35 | ~ spl23_61), inference(avatar_split_clause, [], [f1806, f1337, f472, f441])).
fof(f1806, plain, (! [X0] : g_false_only(sK21, X0) | (~ spl23_35 | ~ spl23_61)), inference(subsumption_resolution, [], [f1804, f1338])).
fof(f1804, plain, (! [X0] : (g_false_only(sK21, X0) | g_true_only(sK21, X0)) | ~ spl23_35), inference(resolution, [], [f473, f101])).
fof(f473, plain, (! [X1] : ~ g_both(sK21, X1) | ~ spl23_35), inference(avatar_component_clause, [], [f472])).
fof(f1775, plain, (spl23_39 | spl23_37 | ~ spl23_8 | ~ spl23_23 | spl23_25 | ~ spl23_48 | ~ spl23_61), inference(avatar_split_clause, [], [f1774, f1337, f877, f212, f205, f145, f505, f537])).
fof(f212, plain, (spl23_25 <=> g_false_only(sK21, sK22)), introduced(avatar_definition, [new_symbols(naming, [spl23_25])])).
fof(f877, plain, (spl23_48 <=> g_both(sK21, sK22)), introduced(avatar_definition, [new_symbols(naming, [spl23_48])])).
fof(f1774, plain, (! [X2, X3] : (h_false_only(sK21, X2) | ~ h_both(sK21, X3)) | (~ spl23_8 | ~ spl23_23 | spl23_25 | ~ spl23_48 | ~ spl23_61)), inference(subsumption_resolution, [], [f1773, f1338])).
fof(f1773, plain, (! [X2, X3] : (h_false_only(sK21, X2) | ~ h_both(sK21, X3) | g_true_only(sK21, sK6(sK21))) | (~ spl23_8 | ~ spl23_23 | spl23_25 | ~ spl23_48)), inference(subsumption_resolution, [], [f1772, f206])).
fof(f1772, plain, (! [X2, X3] : (h_false_only(sK21, X2) | ~ h_both(sK21, X3) | h_true_only(sK21, sK7(sK21, sK22)) | g_true_only(sK21, sK6(sK21))) | (~ spl23_8 | spl23_25 | ~ spl23_48)), inference(subsumption_resolution, [], [f1763, f214])).
fof(f214, plain, (~ g_false_only(sK21, sK22) | spl23_25), inference(avatar_component_clause, [], [f212])).
fof(f1763, plain, (! [X2, X3] : (h_false_only(sK21, X2) | ~ h_both(sK21, X3) | g_false_only(sK21, sK22) | h_true_only(sK21, sK7(sK21, sK22)) | g_true_only(sK21, sK6(sK21))) | (~ spl23_8 | ~ spl23_48)), inference(resolution, [], [f879, f146])).
fof(f879, plain, (g_both(sK21, sK22) | ~ spl23_48), inference(avatar_component_clause, [], [f877])).
fof(f1750, plain, (spl23_61 | ~ spl23_14 | ~ spl23_23 | ~ spl23_28), inference(avatar_split_clause, [], [f1749, f368, f205, f169, f1337])).
fof(f368, plain, (spl23_28 <=> h_both(sK21, sK12(sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_28])])).
fof(f1749, plain, (! [X0] : ~ g_true_only(sK21, X0) | (~ spl23_14 | ~ spl23_23 | ~ spl23_28)), inference(subsumption_resolution, [], [f1746, f206])).
fof(f1746, plain, (! [X0] : (~ g_true_only(sK21, X0) | h_true_only(sK21, sK13(sK21))) | (~ spl23_14 | ~ spl23_28)), inference(resolution, [], [f370, f170])).
fof(f370, plain, (h_both(sK21, sK12(sK21)) | ~ spl23_28), inference(avatar_component_clause, [], [f368])).
fof(f1735, plain, (~ spl23_57 | ~ spl23_61), inference(avatar_contradiction_clause, [], [f1733])).
fof(f1733, plain, ($false | (~ spl23_57 | ~ spl23_61)), inference(resolution, [], [f1338, f1297])).
fof(f1297, plain, (g_true_only(sK21, sK22) | ~ spl23_57), inference(avatar_component_clause, [], [f1295])).
fof(f1295, plain, (spl23_57 <=> g_true_only(sK21, sK22)), introduced(avatar_definition, [new_symbols(naming, [spl23_57])])).
fof(f1726, plain, (spl23_61 | ~ spl23_14 | ~ spl23_23 | ~ spl23_27), inference(avatar_split_clause, [], [f1725, f364, f205, f169, f1337])).
fof(f1725, plain, (! [X0] : ~ g_true_only(sK21, X0) | (~ spl23_14 | ~ spl23_23 | ~ spl23_27)), inference(subsumption_resolution, [], [f1722, f206])).
fof(f1722, plain, (! [X0] : (~ g_true_only(sK21, X0) | h_true_only(sK21, sK13(sK21))) | (~ spl23_14 | ~ spl23_27)), inference(resolution, [], [f366, f170])).
fof(f1665, plain, (~ spl23_16 | ~ spl23_21 | ~ spl23_22), inference(avatar_contradiction_clause, [], [f1664])).
fof(f1664, plain, ($false | (~ spl23_16 | ~ spl23_21 | ~ spl23_22)), inference(resolution, [], [f1663, f178])).
fof(f178, plain, (sP1(sK14) | ~ spl23_16), inference(avatar_component_clause, [], [f176])).
fof(f176, plain, (spl23_16 <=> sP1(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl23_16])])).
fof(f1663, plain, (! [X0] : ~ sP1(X0) | (~ spl23_21 | ~ spl23_22)), inference(duplicate_literal_removal, [], [f1660])).
fof(f1660, plain, (! [X0] : (~ sP1(X0) | ~ sP1(X0)) | (~ spl23_21 | ~ spl23_22)), inference(resolution, [], [f1088, f76])).
fof(f76, plain, ! [X0] : (g_true_only(X0, sK9(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (((! [X1] : h_false_only(X0, X1) | (! [X2] : ~ h_true_only(X0, X2) & h_both(X0, sK8(X0)))) & g_true_only(X0, sK9(X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f35, f37, f36])).
fof(f36, plain, ! [X0] : (? [X3] : h_both(X0, X3) => h_both(X0, sK8(X0))), introduced(choice_axiom, [])).
fof(f37, plain, ! [X0] : (? [X4] : g_true_only(X0, X4) => g_true_only(X0, sK9(X0))), introduced(choice_axiom, [])).
fof(f35, plain, ! [X0] : (((! [X1] : h_false_only(X0, X1) | (! [X2] : ~ h_true_only(X0, X2) & ? [X3] : h_both(X0, X3))) & ? [X4] : g_true_only(X0, X4)) | ~ sP1(X0)), inference(rectify, [], [f34])).
fof(f34, plain, ! [X9] : (((! [X13] : h_false_only(X9, X13) | (! [X14] : ~ h_true_only(X9, X14) & ? [X15] : h_both(X9, X15))) & ? [X16] : g_true_only(X9, X16)) | ~ sP1(X9)), inference(nnf_transformation, [], [f22])).
fof(f22, plain, ! [X9] : (((! [X13] : h_false_only(X9, X13) | (! [X14] : ~ h_true_only(X9, X14) & ? [X15] : h_both(X9, X15))) & ? [X16] : g_true_only(X9, X16)) | ~ sP1(X9)), inference(usedef, [], [e22])).
fof(e22, plain, ! [X9] : (sP1(X9) <=> ((! [X13] : h_false_only(X9, X13) | (! [X14] : ~ h_true_only(X9, X14) & ? [X15] : h_both(X9, X15))) & ? [X16] : g_true_only(X9, X16))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f1088, plain, (! [X8, X9] : (~ g_true_only(X8, X9) | ~ sP1(X8)) | (~ spl23_21 | ~ spl23_22)), inference(resolution, [], [f994, f198])).
fof(f198, plain, (! [X10, X9] : (~ h_false_only(X9, sK18(X9, X10)) | ~ g_true_only(X9, X10)) | ~ spl23_21), inference(avatar_component_clause, [], [f197])).
fof(f197, plain, (spl23_21 <=> ! [X9, X10] : (~ h_false_only(X9, sK18(X9, X10)) | ~ g_true_only(X9, X10))), introduced(avatar_definition, [new_symbols(naming, [spl23_21])])).
fof(f994, plain, (! [X0, X1] : (h_false_only(X0, X1) | ~ sP1(X0)) | (~ spl23_21 | ~ spl23_22)), inference(duplicate_literal_removal, [], [f993])).
fof(f993, plain, (! [X0, X1] : (h_false_only(X0, X1) | ~ sP1(X0) | ~ sP1(X0)) | (~ spl23_21 | ~ spl23_22)), inference(resolution, [], [f952, f76])).
fof(f952, plain, (! [X2, X0, X1] : (~ g_true_only(X0, X1) | h_false_only(X0, X2) | ~ sP1(X0)) | (~ spl23_21 | ~ spl23_22)), inference(resolution, [], [f947, f78])).
fof(f78, plain, ! [X2, X0, X1] : (~ h_true_only(X0, X2) | h_false_only(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f38])).
fof(f947, plain, (! [X2, X3] : (h_true_only(X2, sK18(X2, X3)) | ~ g_true_only(X2, X3)) | (~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f942, f202])).
fof(f202, plain, (! [X10, X9] : (~ h_both(X9, sK18(X9, X10)) | ~ g_true_only(X9, X10)) | ~ spl23_22), inference(avatar_component_clause, [], [f201])).
fof(f201, plain, (spl23_22 <=> ! [X9, X10] : (~ h_both(X9, sK18(X9, X10)) | ~ g_true_only(X9, X10))), introduced(avatar_definition, [new_symbols(naming, [spl23_22])])).
fof(f942, plain, (! [X2, X3] : (~ g_true_only(X2, X3) | h_both(X2, sK18(X2, X3)) | h_true_only(X2, sK18(X2, X3))) | ~ spl23_21), inference(resolution, [], [f198, f111])).
fof(f1519, plain, (spl23_53 | spl23_53 | spl23_56 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24), inference(avatar_split_clause, [], [f1518, f208, f201, f197, f193, f145, f1116, f1062, f1062])).
fof(f193, plain, (spl23_20 <=> ! [X9, X10] : (~ h_false_only(X9, sK18(X9, X10)) | ~ g_both(X9, X10))), introduced(avatar_definition, [new_symbols(naming, [spl23_20])])).
fof(f1518, plain, (! [X17, X15, X16] : (h_false_only(sK19, X16) | g_false_only(sK19, X15) | g_false_only(sK19, X17)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(subsumption_resolution, [], [f1517, f956])).
fof(f956, plain, (! [X7] : ~ g_true_only(sK19, X7) | (~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(resolution, [], [f947, f209])).
fof(f1517, plain, (! [X17, X15, X16] : (h_false_only(sK19, X16) | g_false_only(sK19, X15) | g_false_only(sK19, X17) | g_true_only(sK19, X17)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(subsumption_resolution, [], [f1516, f956])).
fof(f1516, plain, (! [X17, X15, X16] : (h_false_only(sK19, X16) | g_false_only(sK19, X15) | g_true_only(sK19, X15) | g_false_only(sK19, X17) | g_true_only(sK19, X17)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(subsumption_resolution, [], [f1515, f956])).
fof(f1515, plain, (! [X17, X15, X16] : (h_false_only(sK19, X16) | g_false_only(sK19, X15) | g_true_only(sK19, sK6(sK19)) | g_true_only(sK19, X15) | g_false_only(sK19, X17) | g_true_only(sK19, X17)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_24)), inference(subsumption_resolution, [], [f1481, f209])).
fof(f1481, plain, (! [X17, X15, X16] : (h_true_only(sK19, sK7(sK19, X15)) | h_false_only(sK19, X16) | g_false_only(sK19, X15) | g_true_only(sK19, sK6(sK19)) | g_true_only(sK19, X15) | g_false_only(sK19, X17) | g_true_only(sK19, X17)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_24)), inference(resolution, [], [f452, f209])).
fof(f452, plain, (! [X2, X0, X3, X1] : (h_true_only(X0, sK18(X0, X1)) | h_true_only(X0, sK7(X0, X3)) | h_false_only(X0, X2) | g_false_only(X0, X3) | g_true_only(X0, sK6(X0)) | g_true_only(X0, X3) | g_false_only(X0, X1) | g_true_only(X0, X1)) | (~ spl23_8 | ~ spl23_20)), inference(resolution, [], [f406, f101])).
fof(f406, plain, (! [X6, X8, X7, X5] : (~ g_both(X5, X6) | h_true_only(X5, sK18(X5, X6)) | h_false_only(X5, X7) | g_false_only(X5, X8) | h_true_only(X5, sK7(X5, X8)) | g_true_only(X5, sK6(X5)) | g_true_only(X5, X8)) | (~ spl23_8 | ~ spl23_20)), inference(resolution, [], [f392, f261])).
fof(f261, plain, (! [X2, X0, X3, X1] : (~ h_both(X0, X2) | h_false_only(X0, X1) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)) | g_true_only(X0, X3)) | ~ spl23_8), inference(duplicate_literal_removal, [], [f260])).
fof(f260, plain, (! [X2, X0, X3, X1] : (h_false_only(X0, X1) | ~ h_both(X0, X2) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)) | g_false_only(X0, X3) | g_true_only(X0, X3)) | ~ spl23_8), inference(resolution, [], [f146, f101])).
fof(f392, plain, (! [X6, X7] : (h_both(X6, sK18(X6, X7)) | ~ g_both(X6, X7) | h_true_only(X6, sK18(X6, X7))) | ~ spl23_20), inference(resolution, [], [f194, f111])).
fof(f194, plain, (! [X10, X9] : (~ h_false_only(X9, sK18(X9, X10)) | ~ g_both(X9, X10)) | ~ spl23_20), inference(avatar_component_clause, [], [f193])).
fof(f1503, plain, (spl23_65 | ~ spl23_20 | ~ spl23_56), inference(avatar_split_clause, [], [f1502, f1116, f193, f1400])).
fof(f1502, plain, (! [X4] : ~ g_both(sK19, X4) | (~ spl23_20 | ~ spl23_56)), inference(resolution, [], [f1117, f194])).
fof(f1410, plain, (spl23_53 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24 | ~ spl23_65), inference(avatar_split_clause, [], [f1409, f1400, f208, f201, f197, f1062])).
fof(f1409, plain, (! [X0] : g_false_only(sK19, X0) | (~ spl23_21 | ~ spl23_22 | ~ spl23_24 | ~ spl23_65)), inference(subsumption_resolution, [], [f1407, f956])).
fof(f1407, plain, (! [X0] : (g_false_only(sK19, X0) | g_true_only(sK19, X0)) | ~ spl23_65), inference(resolution, [], [f1401, f101])).
fof(f1298, plain, (spl23_57 | spl23_48 | ~ spl23_47), inference(avatar_split_clause, [], [f1291, f873, f877, f1295])).
fof(f873, plain, (spl23_47 <=> g_true(sK21, sK22)), introduced(avatar_definition, [new_symbols(naming, [spl23_47])])).
fof(f1291, plain, (g_both(sK21, sK22) | g_true_only(sK21, sK22) | ~ spl23_47), inference(resolution, [], [f874, f234])).
fof(f234, plain, ! [X2, X3] : (~ g_true(X2, X3) | g_both(X2, X3) | g_true_only(X2, X3)), inference(resolution, [], [f231, f97])).
fof(f97, plain, ! [X0, X1] : (~ g_false(X0, X1) | g_both(X0, X1) | ~ g_true(X0, X1)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0, X1] : ((g_both(X0, X1) | ~ g_false(X0, X1) | ~ g_true(X0, X1)) & ((g_false(X0, X1) & g_true(X0, X1)) | ~ g_both(X0, X1))), inference(flattening, [], [f58])).
fof(f58, plain, ! [X0, X1] : ((g_both(X0, X1) | (~ g_false(X0, X1) | ~ g_true(X0, X1))) & ((g_false(X0, X1) & g_true(X0, X1)) | ~ g_both(X0, X1))), inference(nnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : (g_both(X0, X1) <=> (g_false(X0, X1) & g_true(X0, X1))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X3, X4] : (g_both(X3, X4) <=> (g_false(X3, X4) & g_true(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', both_g)).
fof(f231, plain, ! [X0, X1] : (g_false(X0, X1) | g_true_only(X0, X1)), inference(subsumption_resolution, [], [f229, f98])).
fof(f98, plain, ! [X0, X1] : (~ g_false_only(X0, X1) | g_false(X0, X1)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1] : ((g_false_only(X0, X1) | g_true(X0, X1) | ~ g_false(X0, X1)) & ((~ g_true(X0, X1) & g_false(X0, X1)) | ~ g_false_only(X0, X1))), inference(flattening, [], [f60])).
fof(f60, plain, ! [X0, X1] : ((g_false_only(X0, X1) | (g_true(X0, X1) | ~ g_false(X0, X1))) & ((~ g_true(X0, X1) & g_false(X0, X1)) | ~ g_false_only(X0, X1))), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1] : (g_false_only(X0, X1) <=> (~ g_true(X0, X1) & g_false(X0, X1))), inference(rectify, [], [f5])).
fof(f5, plain, ! [X3, X4] : (g_false_only(X3, X4) <=> (~ g_true(X3, X4) & g_false(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', false_only_g)).
fof(f229, plain, ! [X0, X1] : (g_false_only(X0, X1) | g_true_only(X0, X1) | g_false(X0, X1)), inference(resolution, [], [f101, f96])).
fof(f96, plain, ! [X0, X1] : (~ g_both(X0, X1) | g_false(X0, X1)), inference(cnf_transformation, [], [f59])).
fof(f874, plain, (g_true(sK21, sK22) | ~ spl23_47), inference(avatar_component_clause, [], [f873])).
fof(f1290, plain, (spl23_25 | spl23_47), inference(avatar_split_clause, [], [f933, f873, f212])).
fof(f933, plain, (g_false_only(sK21, sK22) | spl23_47), inference(resolution, [], [f875, f232])).
fof(f232, plain, ! [X2, X3] : (g_true(X2, X3) | g_false_only(X2, X3)), inference(subsumption_resolution, [], [f230, f92])).
fof(f92, plain, ! [X0, X1] : (~ g_true_only(X0, X1) | g_true(X0, X1)), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : ((g_true_only(X0, X1) | g_false(X0, X1) | ~ g_true(X0, X1)) & ((~ g_false(X0, X1) & g_true(X0, X1)) | ~ g_true_only(X0, X1))), inference(flattening, [], [f56])).
fof(f56, plain, ! [X0, X1] : ((g_true_only(X0, X1) | (g_false(X0, X1) | ~ g_true(X0, X1))) & ((~ g_false(X0, X1) & g_true(X0, X1)) | ~ g_true_only(X0, X1))), inference(nnf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (g_true_only(X0, X1) <=> (~ g_false(X0, X1) & g_true(X0, X1))), inference(rectify, [], [f3])).
fof(f3, plain, ! [X3, X4] : (g_true_only(X3, X4) <=> (~ g_false(X3, X4) & g_true(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', true_only_g)).
fof(f230, plain, ! [X2, X3] : (g_false_only(X2, X3) | g_true_only(X2, X3) | g_true(X2, X3)), inference(resolution, [], [f101, f95])).
fof(f95, plain, ! [X0, X1] : (~ g_both(X0, X1) | g_true(X0, X1)), inference(cnf_transformation, [], [f59])).
fof(f875, plain, (~ g_true(sK21, sK22) | spl23_47), inference(avatar_component_clause, [], [f873])).
fof(f1289, plain, (spl23_53 | spl23_53 | ~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24), inference(avatar_split_clause, [], [f1288, f208, f201, f197, f193, f173, f1062, f1062])).
fof(f1288, plain, (! [X24, X25] : (g_false_only(sK19, X25) | g_false_only(sK19, X24)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(subsumption_resolution, [], [f1287, f209])).
fof(f1287, plain, (! [X24, X25] : (g_false_only(sK19, X25) | h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X24)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(subsumption_resolution, [], [f1286, f956])).
fof(f1286, plain, (! [X24, X25] : (g_false_only(sK19, X25) | g_true_only(sK19, sK17(sK19)) | h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X24)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(subsumption_resolution, [], [f1276, f209])).
fof(f1276, plain, (! [X24, X25] : (h_true_only(sK19, sK18(sK19, X24)) | g_false_only(sK19, X25) | g_true_only(sK19, sK17(sK19)) | h_true_only(sK19, sK16(sK19)) | g_false_only(sK19, X24)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_24)), inference(resolution, [], [f1078, f956])).
fof(f1078, plain, (! [X2, X0, X1] : (h_true_only(X0, sK18(X0, X2)) | g_true_only(X0, X1) | g_false_only(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK16(X0)) | g_false_only(X0, X2)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f1077, f947])).
fof(f1077, plain, (! [X2, X0, X1] : (h_true_only(X0, sK16(X0)) | g_false_only(X0, X1) | g_true_only(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK18(X0, X2)) | g_false_only(X0, X2) | g_true_only(X0, X2)) | (~ spl23_15 | ~ spl23_20)), inference(resolution, [], [f962, f101])).
fof(f962, plain, (! [X10, X8, X9] : (~ g_both(X8, X10) | h_true_only(X8, sK16(X8)) | g_false_only(X8, X9) | g_true_only(X8, X9) | g_true_only(X8, sK17(X8)) | h_true_only(X8, sK18(X8, X10))) | (~ spl23_15 | ~ spl23_20)), inference(resolution, [], [f951, f392])).
fof(f951, plain, (! [X2, X0, X1] : (~ h_both(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK16(X0)) | g_false_only(X0, X2) | g_true_only(X0, X2)) | ~ spl23_15), inference(resolution, [], [f174, f101])).
fof(f1132, plain, (spl23_45 | ~ spl23_52), inference(avatar_contradiction_clause, [], [f1129])).
fof(f1129, plain, ($false | (spl23_45 | ~ spl23_52)), inference(resolution, [], [f1053, f853])).
fof(f853, plain, (~ g_false_only(sK4, sK5) | spl23_45), inference(avatar_component_clause, [], [f852])).
fof(f852, plain, (spl23_45 <=> g_false_only(sK4, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl23_45])])).
fof(f1053, plain, (! [X9] : g_false_only(sK4, X9) | ~ spl23_52), inference(avatar_component_clause, [], [f1052])).
fof(f1052, plain, (spl23_52 <=> ! [X9] : g_false_only(sK4, X9)), introduced(avatar_definition, [new_symbols(naming, [spl23_52])])).
fof(f1122, plain, (spl23_26 | ~ spl23_53), inference(avatar_contradiction_clause, [], [f1119])).
fof(f1119, plain, ($false | (spl23_26 | ~ spl23_53)), inference(resolution, [], [f1063, f219])).
fof(f219, plain, (~ g_false_only(sK19, sK20) | spl23_26), inference(avatar_component_clause, [], [f217])).
fof(f217, plain, (spl23_26 <=> g_false_only(sK19, sK20)), introduced(avatar_definition, [new_symbols(naming, [spl23_26])])).
fof(f1063, plain, (! [X11] : g_false_only(sK19, X11) | ~ spl23_53), inference(avatar_component_clause, [], [f1062])).
fof(f1066, plain, (~ spl23_24 | ~ spl23_30), inference(avatar_contradiction_clause, [], [f1065])).
fof(f1065, plain, ($false | (~ spl23_24 | ~ spl23_30)), inference(subsumption_resolution, [], [f379, f209])).
fof(f379, plain, (h_true_only(sK19, sK12(sK19)) | ~ spl23_30), inference(avatar_component_clause, [], [f377])).
fof(f377, plain, (spl23_30 <=> h_true_only(sK19, sK12(sK19))), introduced(avatar_definition, [new_symbols(naming, [spl23_30])])).
fof(f1054, plain, (spl23_52 | spl23_49 | ~ spl23_2 | ~ spl23_12 | ~ spl23_42), inference(avatar_split_clause, [], [f1050, f809, f161, f119, f897, f1052])).
fof(f119, plain, (spl23_2 <=> ! [X2] : (h_false_only(sK4, X2) | h_both(sK4, X2))), introduced(avatar_definition, [new_symbols(naming, [spl23_2])])).
fof(f809, plain, (spl23_42 <=> ! [X2] : ~ g_true_only(sK4, X2)), introduced(avatar_definition, [new_symbols(naming, [spl23_42])])).
fof(f1050, plain, (! [X9] : (h_both(sK4, sK10(sK4)) | g_false_only(sK4, X9)) | (~ spl23_2 | ~ spl23_12 | ~ spl23_42)), inference(subsumption_resolution, [], [f1049, f810])).
fof(f810, plain, (! [X2] : ~ g_true_only(sK4, X2) | ~ spl23_42), inference(avatar_component_clause, [], [f809])).
fof(f1049, plain, (! [X9] : (h_both(sK4, sK10(sK4)) | g_false_only(sK4, X9) | g_true_only(sK4, sK11(sK4))) | (~ spl23_2 | ~ spl23_12 | ~ spl23_42)), inference(subsumption_resolution, [], [f1041, f816])).
fof(f816, plain, (! [X2] : ~ h_true_only(sK4, X2) | ~ spl23_2), inference(resolution, [], [f807, f103])).
fof(f103, plain, ! [X0, X1] : (~ h_false(X0, X1) | ~ h_true_only(X0, X1)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ! [X0, X1] : ((h_true_only(X0, X1) | h_false(X0, X1) | ~ h_true(X0, X1)) & ((~ h_false(X0, X1) & h_true(X0, X1)) | ~ h_true_only(X0, X1))), inference(flattening, [], [f62])).
fof(f62, plain, ! [X0, X1] : ((h_true_only(X0, X1) | (h_false(X0, X1) | ~ h_true(X0, X1))) & ((~ h_false(X0, X1) & h_true(X0, X1)) | ~ h_true_only(X0, X1))), inference(nnf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : (h_true_only(X0, X1) <=> (~ h_false(X0, X1) & h_true(X0, X1))), inference(rectify, [], [f7])).
fof(f7, plain, ! [X3, X4] : (h_true_only(X3, X4) <=> (~ h_false(X3, X4) & h_true(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', true_only_h)).
fof(f807, plain, (! [X0] : h_false(sK4, X0) | ~ spl23_2), inference(subsumption_resolution, [], [f802, f106])).
fof(f106, plain, ! [X0, X1] : (~ h_both(X0, X1) | h_false(X0, X1)), inference(cnf_transformation, [], [f65])).
fof(f802, plain, (! [X0] : (h_both(sK4, X0) | h_false(sK4, X0)) | ~ spl23_2), inference(resolution, [], [f120, f108])).
fof(f108, plain, ! [X0, X1] : (~ h_false_only(X0, X1) | h_false(X0, X1)), inference(cnf_transformation, [], [f67])).
fof(f120, plain, (! [X2] : (h_false_only(sK4, X2) | h_both(sK4, X2)) | ~ spl23_2), inference(avatar_component_clause, [], [f119])).
fof(f1041, plain, (! [X9] : (h_both(sK4, sK10(sK4)) | h_true_only(sK4, sK10(sK4)) | g_false_only(sK4, X9) | g_true_only(sK4, sK11(sK4))) | (~ spl23_12 | ~ spl23_42)), inference(resolution, [], [f932, f810])).
fof(f932, plain, (! [X0, X1] : (h_both(X0, sK10(X0)) | g_true_only(X0, X1) | h_true_only(X0, sK10(X0)) | g_false_only(X0, X1) | g_true_only(X0, sK11(X0))) | ~ spl23_12), inference(resolution, [], [f891, f101])).
fof(f891, plain, (! [X2, X3] : (~ g_both(X2, X3) | g_true_only(X2, sK11(X2)) | h_both(X2, sK10(X2)) | h_true_only(X2, sK10(X2))) | ~ spl23_12), inference(resolution, [], [f162, f111])).
fof(f1019, plain, (~ spl23_24 | ~ spl23_31), inference(avatar_contradiction_clause, [], [f1016])).
fof(f1016, plain, ($false | (~ spl23_24 | ~ spl23_31)), inference(resolution, [], [f383, f209])).
fof(f383, plain, (h_true_only(sK19, sK10(sK19)) | ~ spl23_31), inference(avatar_component_clause, [], [f381])).
fof(f381, plain, (spl23_31 <=> h_true_only(sK19, sK10(sK19))), introduced(avatar_definition, [new_symbols(naming, [spl23_31])])).
fof(f970, plain, (~ spl23_2 | ~ spl23_41), inference(avatar_contradiction_clause, [], [f967])).
fof(f967, plain, ($false | (~ spl23_2 | ~ spl23_41)), inference(resolution, [], [f797, f816])).
fof(f797, plain, (h_true_only(sK4, sK12(sK4)) | ~ spl23_41), inference(avatar_component_clause, [], [f795])).
fof(f795, plain, (spl23_41 <=> h_true_only(sK4, sK12(sK4))), introduced(avatar_definition, [new_symbols(naming, [spl23_41])])).
fof(f940, plain, (spl23_42 | ~ spl23_2 | ~ spl23_14 | ~ spl23_40), inference(avatar_split_clause, [], [f939, f791, f169, f119, f809])).
fof(f939, plain, (! [X0] : ~ g_true_only(sK4, X0) | (~ spl23_2 | ~ spl23_14 | ~ spl23_40)), inference(subsumption_resolution, [], [f934, f816])).
fof(f934, plain, (! [X0] : (~ g_true_only(sK4, X0) | h_true_only(sK4, sK13(sK4))) | (~ spl23_14 | ~ spl23_40)), inference(resolution, [], [f793, f170])).
fof(f889, plain, (~ spl23_45 | ~ spl23_5), inference(avatar_split_clause, [], [f888, f131, f852])).
fof(f131, plain, (spl23_5 <=> g_both(sK4, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl23_5])])).
fof(f888, plain, (~ g_false_only(sK4, sK5) | ~ spl23_5), inference(resolution, [], [f825, f99])).
fof(f99, plain, ! [X0, X1] : (~ g_true(X0, X1) | ~ g_false_only(X0, X1)), inference(cnf_transformation, [], [f61])).
fof(f825, plain, (g_true(sK4, sK5) | ~ spl23_5), inference(resolution, [], [f133, f95])).
fof(f133, plain, (g_both(sK4, sK5) | ~ spl23_5), inference(avatar_component_clause, [], [f131])).
fof(f838, plain, (~ spl23_4 | ~ spl23_5 | ~ spl23_20), inference(avatar_contradiction_clause, [], [f835])).
fof(f835, plain, ($false | (~ spl23_4 | ~ spl23_5 | ~ spl23_20)), inference(resolution, [], [f824, f133])).
fof(f824, plain, (! [X4] : ~ g_both(sK4, X4) | (~ spl23_4 | ~ spl23_20)), inference(resolution, [], [f128, f194])).
fof(f819, plain, (~ spl23_3 | ~ spl23_42), inference(avatar_contradiction_clause, [], [f817])).
fof(f817, plain, ($false | (~ spl23_3 | ~ spl23_42)), inference(resolution, [], [f810, f125])).
fof(f125, plain, (g_true_only(sK4, sK5) | ~ spl23_3), inference(avatar_component_clause, [], [f123])).
fof(f123, plain, (spl23_3 <=> g_true_only(sK4, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl23_3])])).
fof(f813, plain, (spl23_42 | ~ spl23_2 | ~ spl23_21 | ~ spl23_22), inference(avatar_split_clause, [], [f812, f201, f197, f119, f809])).
fof(f812, plain, (! [X3] : ~ g_true_only(sK4, X3) | (~ spl23_2 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f805, f202])).
fof(f805, plain, (! [X3] : (h_both(sK4, sK18(sK4, X3)) | ~ g_true_only(sK4, X3)) | (~ spl23_2 | ~ spl23_21)), inference(resolution, [], [f120, f198])).
fof(f811, plain, (spl23_42 | spl23_40 | ~ spl23_2 | ~ spl23_13), inference(avatar_split_clause, [], [f804, f165, f119, f791, f809])).
fof(f804, plain, (! [X2] : (h_both(sK4, sK12(sK4)) | ~ g_true_only(sK4, X2)) | (~ spl23_2 | ~ spl23_13)), inference(resolution, [], [f120, f166])).
fof(f798, plain, (spl23_40 | spl23_41 | ~ spl23_3 | ~ spl23_13), inference(avatar_split_clause, [], [f785, f165, f123, f795, f791])).
fof(f785, plain, (h_true_only(sK4, sK12(sK4)) | h_both(sK4, sK12(sK4)) | (~ spl23_3 | ~ spl23_13)), inference(resolution, [], [f125, f239])).
fof(f239, plain, (! [X2, X3] : (~ g_true_only(X2, X3) | h_true_only(X2, sK12(X2)) | h_both(X2, sK12(X2))) | ~ spl23_13), inference(resolution, [], [f111, f166])).
fof(f781, plain, (~ spl23_17 | ~ spl23_19 | ~ spl23_20), inference(avatar_contradiction_clause, [], [f778])).
fof(f778, plain, ($false | (~ spl23_17 | ~ spl23_19 | ~ spl23_20)), inference(resolution, [], [f763, f190])).
fof(f763, plain, (! [X4] : ~ g_both(sK14, X4) | (~ spl23_17 | ~ spl23_20)), inference(resolution, [], [f181, f194])).
fof(f758, plain, (spl23_33 | spl23_33 | ~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23), inference(avatar_split_clause, [], [f757, f205, f201, f197, f193, f173, f441, f441])).
fof(f757, plain, (! [X23, X22] : (g_false_only(sK21, X23) | g_false_only(sK21, X22)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f756, f206])).
fof(f756, plain, (! [X23, X22] : (g_false_only(sK21, X23) | h_true_only(sK21, sK16(sK21)) | g_false_only(sK21, X22)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f755, f400])).
fof(f400, plain, (! [X0] : ~ g_true_only(sK21, X0) | (~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(resolution, [], [f399, f206])).
fof(f399, plain, (! [X6, X7] : (h_true_only(X6, sK18(X6, X7)) | ~ g_true_only(X6, X7)) | (~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f397, f202])).
fof(f397, plain, (! [X6, X7] : (~ g_true_only(X6, X7) | h_both(X6, sK18(X6, X7)) | h_true_only(X6, sK18(X6, X7))) | ~ spl23_21), inference(resolution, [], [f198, f111])).
fof(f755, plain, (! [X23, X22] : (g_false_only(sK21, X23) | g_true_only(sK21, sK17(sK21)) | h_true_only(sK21, sK16(sK21)) | g_false_only(sK21, X22)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f748, f206])).
fof(f748, plain, (! [X23, X22] : (h_true_only(sK21, sK18(sK21, X22)) | g_false_only(sK21, X23) | g_true_only(sK21, sK17(sK21)) | h_true_only(sK21, sK16(sK21)) | g_false_only(sK21, X22)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(resolution, [], [f683, f400])).
fof(f683, plain, (! [X2, X0, X1] : (h_true_only(X0, sK18(X0, X2)) | g_true_only(X0, X1) | g_false_only(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK16(X0)) | g_false_only(X0, X2)) | (~ spl23_15 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f682, f399])).
fof(f682, plain, (! [X2, X0, X1] : (h_true_only(X0, sK16(X0)) | g_false_only(X0, X1) | g_true_only(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK18(X0, X2)) | g_false_only(X0, X2) | g_true_only(X0, X2)) | (~ spl23_15 | ~ spl23_20)), inference(resolution, [], [f614, f101])).
fof(f614, plain, (! [X10, X8, X9] : (~ g_both(X8, X10) | h_true_only(X8, sK16(X8)) | g_false_only(X8, X9) | g_true_only(X8, X9) | g_true_only(X8, sK17(X8)) | h_true_only(X8, sK18(X8, X10))) | (~ spl23_15 | ~ spl23_20)), inference(resolution, [], [f609, f392])).
fof(f609, plain, (! [X2, X0, X1] : (~ h_both(X0, X1) | g_true_only(X0, sK17(X0)) | h_true_only(X0, sK16(X0)) | g_false_only(X0, X2) | g_true_only(X0, X2)) | ~ spl23_15), inference(resolution, [], [f174, f101])).
fof(f541, plain, (~ spl23_13 | ~ spl23_14 | ~ spl23_16), inference(avatar_contradiction_clause, [], [f540])).
fof(f540, plain, ($false | (~ spl23_13 | ~ spl23_14 | ~ spl23_16)), inference(subsumption_resolution, [], [f178, f268])).
fof(f268, plain, (! [X0] : ~ sP1(X0) | (~ spl23_13 | ~ spl23_14)), inference(duplicate_literal_removal, [], [f267])).
fof(f267, plain, (! [X0] : (~ sP1(X0) | ~ sP1(X0)) | (~ spl23_13 | ~ spl23_14)), inference(resolution, [], [f257, f76])).
fof(f257, plain, (! [X2, X3] : (~ g_true_only(X2, X3) | ~ sP1(X2)) | (~ spl23_13 | ~ spl23_14)), inference(resolution, [], [f253, f166])).
fof(f253, plain, (! [X0, X1] : (h_false_only(X0, X1) | ~ sP1(X0)) | ~ spl23_14), inference(duplicate_literal_removal, [], [f252])).
fof(f252, plain, (! [X0, X1] : (h_false_only(X0, X1) | ~ sP1(X0) | ~ sP1(X0)) | ~ spl23_14), inference(resolution, [], [f246, f76])).
fof(f246, plain, (! [X2, X0, X1] : (~ g_true_only(X0, X1) | h_false_only(X0, X2) | ~ sP1(X0)) | ~ spl23_14), inference(subsumption_resolution, [], [f245, f78])).
fof(f245, plain, (! [X2, X0, X1] : (~ g_true_only(X0, X1) | h_true_only(X0, sK13(X0)) | h_false_only(X0, X2) | ~ sP1(X0)) | ~ spl23_14), inference(resolution, [], [f170, f77])).
fof(f77, plain, ! [X0, X1] : (h_both(X0, sK8(X0)) | h_false_only(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f38])).
fof(f529, plain, (spl23_33 | spl23_37 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23 | ~ spl23_36), inference(avatar_split_clause, [], [f528, f475, f205, f201, f197, f193, f145, f505, f441])).
fof(f475, plain, (spl23_36 <=> g_both(sK21, sK6(sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_36])])).
fof(f528, plain, (! [X8, X7] : (h_false_only(sK21, X7) | g_false_only(sK21, X8)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23 | ~ spl23_36)), inference(subsumption_resolution, [], [f527, f400])).
fof(f527, plain, (! [X8, X7] : (h_false_only(sK21, X7) | g_false_only(sK21, X8) | g_true_only(sK21, X8)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23 | ~ spl23_36)), inference(subsumption_resolution, [], [f526, f400])).
fof(f526, plain, (! [X8, X7] : (h_false_only(sK21, X7) | g_false_only(sK21, X8) | g_true_only(sK21, sK6(sK21)) | g_true_only(sK21, X8)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_23 | ~ spl23_36)), inference(subsumption_resolution, [], [f525, f206])).
fof(f525, plain, (! [X8, X7] : (h_false_only(sK21, X7) | g_false_only(sK21, X8) | h_true_only(sK21, sK7(sK21, X8)) | g_true_only(sK21, sK6(sK21)) | g_true_only(sK21, X8)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_23 | ~ spl23_36)), inference(subsumption_resolution, [], [f517, f206])).
fof(f517, plain, (! [X8, X7] : (h_true_only(sK21, sK18(sK21, sK6(sK21))) | h_false_only(sK21, X7) | g_false_only(sK21, X8) | h_true_only(sK21, sK7(sK21, X8)) | g_true_only(sK21, sK6(sK21)) | g_true_only(sK21, X8)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_36)), inference(resolution, [], [f477, f406])).
fof(f477, plain, (g_both(sK21, sK6(sK21)) | ~ spl23_36), inference(avatar_component_clause, [], [f475])).
fof(f524, plain, (spl23_35 | ~ spl23_20 | ~ spl23_37), inference(avatar_split_clause, [], [f523, f505, f193, f472])).
fof(f523, plain, (! [X3] : ~ g_both(sK21, X3) | (~ spl23_20 | ~ spl23_37)), inference(resolution, [], [f506, f194])).
fof(f510, plain, (spl23_33 | spl23_33 | spl23_37 | ~ spl23_7 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23), inference(avatar_split_clause, [], [f509, f205, f201, f197, f193, f145, f141, f505, f441, f441])).
fof(f141, plain, (spl23_7 <=> ! [X3, X5, X0, X2] : (h_false_only(X0, X2) | ~ g_both(X0, X3) | ~ h_both(X0, X5) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | h_both(X0, X2))), introduced(avatar_definition, [new_symbols(naming, [spl23_7])])).
fof(f509, plain, (! [X2, X0, X1] : (h_false_only(sK21, X1) | g_false_only(sK21, X0) | g_false_only(sK21, X2)) | (~ spl23_7 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f508, f400])).
fof(f508, plain, (! [X2, X0, X1] : (h_false_only(sK21, X1) | g_false_only(sK21, X0) | g_true_only(sK21, X0) | g_false_only(sK21, X2)) | (~ spl23_7 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f486, f206])).
fof(f486, plain, (! [X2, X0, X1] : (h_true_only(sK21, sK7(sK21, X0)) | h_false_only(sK21, X1) | g_false_only(sK21, X0) | g_true_only(sK21, X0) | g_false_only(sK21, X2)) | (~ spl23_7 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(resolution, [], [f454, f206])).
fof(f454, plain, (! [X2, X0, X3, X1] : (h_true_only(X0, sK18(X0, X1)) | h_true_only(X0, sK7(X0, X3)) | h_false_only(X0, X2) | g_false_only(X0, X3) | g_true_only(X0, X3) | g_false_only(X0, X1)) | (~ spl23_7 | ~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f453, f451])).
fof(f451, plain, (! [X10, X8, X11, X9] : (h_true_only(X8, sK18(X8, X9)) | h_true_only(X8, sK7(X8, X10)) | g_false_only(X8, X10) | g_true_only(X8, X10) | g_false_only(X8, X9) | ~ g_true_only(X8, X11)) | (~ spl23_7 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f436, f202])).
fof(f436, plain, (! [X10, X8, X11, X9] : (h_true_only(X8, sK18(X8, X9)) | h_true_only(X8, sK7(X8, X10)) | g_false_only(X8, X10) | h_both(X8, sK18(X8, X11)) | g_true_only(X8, X10) | g_false_only(X8, X9) | ~ g_true_only(X8, X11)) | (~ spl23_7 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(resolution, [], [f425, f198])).
fof(f425, plain, (! [X2, X0, X3, X1] : (h_true_only(X0, sK18(X0, X1)) | h_true_only(X0, sK7(X0, X3)) | h_false_only(X0, X2) | g_false_only(X0, X3) | h_both(X0, X2) | g_true_only(X0, X3) | g_false_only(X0, X1)) | (~ spl23_7 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f424, f399])).
fof(f424, plain, (! [X2, X0, X3, X1] : (h_true_only(X0, sK18(X0, X1)) | h_false_only(X0, X2) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | h_both(X0, X2) | g_true_only(X0, X3) | g_false_only(X0, X1) | g_true_only(X0, X1)) | (~ spl23_7 | ~ spl23_20)), inference(resolution, [], [f407, f101])).
fof(f407, plain, (! [X12, X10, X11, X9] : (~ g_both(X9, X10) | h_true_only(X9, sK18(X9, X10)) | h_false_only(X9, X11) | g_false_only(X9, X12) | h_true_only(X9, sK7(X9, X12)) | h_both(X9, X11) | g_true_only(X9, X12)) | (~ spl23_7 | ~ spl23_20)), inference(resolution, [], [f392, f255])).
fof(f255, plain, (! [X2, X0, X3, X1] : (~ h_both(X0, X2) | h_false_only(X0, X1) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | h_both(X0, X1) | g_true_only(X0, X3)) | ~ spl23_7), inference(duplicate_literal_removal, [], [f254])).
fof(f254, plain, (! [X2, X0, X3, X1] : (h_false_only(X0, X1) | ~ h_both(X0, X2) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | h_both(X0, X1) | g_false_only(X0, X3) | g_true_only(X0, X3)) | ~ spl23_7), inference(resolution, [], [f142, f101])).
fof(f142, plain, (! [X2, X0, X5, X3] : (~ g_both(X0, X3) | h_false_only(X0, X2) | ~ h_both(X0, X5) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | h_both(X0, X2)) | ~ spl23_7), inference(avatar_component_clause, [], [f141])).
fof(f453, plain, (! [X2, X0, X3, X1] : (h_true_only(X0, sK18(X0, X1)) | h_false_only(X0, X2) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)) | g_true_only(X0, X3) | g_false_only(X0, X1)) | (~ spl23_8 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22)), inference(subsumption_resolution, [], [f452, f399])).
fof(f501, plain, (spl23_25 | ~ spl23_33), inference(avatar_contradiction_clause, [], [f498])).
fof(f498, plain, ($false | (spl23_25 | ~ spl23_33)), inference(resolution, [], [f442, f214])).
fof(f442, plain, (! [X2] : g_false_only(sK21, X2) | ~ spl23_33), inference(avatar_component_clause, [], [f441])).
fof(f485, plain, (spl23_33 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23 | ~ spl23_35), inference(avatar_split_clause, [], [f484, f472, f205, f201, f197, f441])).
fof(f484, plain, (! [X0] : g_false_only(sK21, X0) | (~ spl23_21 | ~ spl23_22 | ~ spl23_23 | ~ spl23_35)), inference(subsumption_resolution, [], [f483, f400])).
fof(f483, plain, (! [X0] : (g_false_only(sK21, X0) | g_true_only(sK21, X0)) | ~ spl23_35), inference(resolution, [], [f473, f101])).
fof(f482, plain, (spl23_35 | spl23_33 | spl23_36 | ~ spl23_10 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23), inference(avatar_split_clause, [], [f481, f205, f201, f197, f193, f153, f475, f441, f472])).
fof(f153, plain, (spl23_10 <=> ! [X3, X5, X0] : (g_both(X0, sK6(X0)) | ~ g_both(X0, X3) | ~ h_both(X0, X5) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl23_10])])).
fof(f481, plain, (! [X0, X1] : (g_both(sK21, sK6(sK21)) | g_false_only(sK21, X1) | ~ g_both(sK21, X0)) | (~ spl23_10 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f480, f400])).
fof(f480, plain, (! [X0, X1] : (g_both(sK21, sK6(sK21)) | g_false_only(sK21, X1) | ~ g_both(sK21, X0) | g_true_only(sK21, X1)) | (~ spl23_10 | ~ spl23_20 | ~ spl23_21 | ~ spl23_22 | ~ spl23_23)), inference(subsumption_resolution, [], [f479, f400])).
fof(f479, plain, (! [X0, X1] : (g_both(sK21, sK6(sK21)) | g_false_only(sK21, X1) | ~ g_both(sK21, X0) | g_true_only(sK21, sK6(sK21)) | g_true_only(sK21, X1)) | (~ spl23_10 | ~ spl23_20 | ~ spl23_23)), inference(subsumption_resolution, [], [f464, f206])).
fof(f464, plain, (! [X0, X1] : (h_true_only(sK21, sK18(sK21, X0)) | g_both(sK21, sK6(sK21)) | g_false_only(sK21, X1) | ~ g_both(sK21, X0) | g_true_only(sK21, sK6(sK21)) | g_true_only(sK21, X1)) | (~ spl23_10 | ~ spl23_20 | ~ spl23_23)), inference(resolution, [], [f405, f206])).
fof(f405, plain, (! [X4, X2, X3] : (h_true_only(X2, sK18(X2, X3)) | h_true_only(X2, sK7(X2, X4)) | g_both(X2, sK6(X2)) | g_false_only(X2, X4) | ~ g_both(X2, X3) | g_true_only(X2, sK6(X2)) | g_true_only(X2, X4)) | (~ spl23_10 | ~ spl23_20)), inference(resolution, [], [f392, f266])).
fof(f266, plain, (! [X2, X0, X1] : (~ h_both(X0, X1) | g_both(X0, sK6(X0)) | g_false_only(X0, X2) | h_true_only(X0, sK7(X0, X2)) | g_true_only(X0, sK6(X0)) | g_true_only(X0, X2)) | ~ spl23_10), inference(duplicate_literal_removal, [], [f265])).
fof(f265, plain, (! [X2, X0, X1] : (g_both(X0, sK6(X0)) | ~ h_both(X0, X1) | g_false_only(X0, X2) | h_true_only(X0, sK7(X0, X2)) | g_true_only(X0, sK6(X0)) | g_false_only(X0, X2) | g_true_only(X0, X2)) | ~ spl23_10), inference(resolution, [], [f154, f101])).
fof(f154, plain, (! [X0, X5, X3] : (~ g_both(X0, X3) | g_both(X0, sK6(X0)) | ~ h_both(X0, X5) | g_false_only(X0, X3) | h_true_only(X0, sK7(X0, X3)) | g_true_only(X0, sK6(X0))) | ~ spl23_10), inference(avatar_component_clause, [], [f153])).
fof(f388, plain, (spl23_29 | spl23_30 | spl23_31 | spl23_32 | ~ spl23_12 | ~ spl23_13 | spl23_26), inference(avatar_split_clause, [], [f354, f217, f165, f161, f385, f381, f377, f373])).
fof(f354, plain, (h_both(sK19, sK12(sK19)) | h_true_only(sK19, sK10(sK19)) | h_true_only(sK19, sK12(sK19)) | h_both(sK19, sK10(sK19)) | (~ spl23_12 | ~ spl23_13 | spl23_26)), inference(resolution, [], [f314, f219])).
fof(f314, plain, (! [X0, X1] : (h_both(X0, sK12(X0)) | g_false_only(X0, X1) | h_true_only(X0, sK10(X0)) | h_true_only(X0, sK12(X0)) | h_both(X0, sK10(X0))) | (~ spl23_12 | ~ spl23_13)), inference(subsumption_resolution, [], [f307, f239])).
fof(f307, plain, (! [X0, X1] : (h_both(X0, sK10(X0)) | h_true_only(X0, sK10(X0)) | g_false_only(X0, X1) | g_true_only(X0, sK11(X0)) | h_true_only(X0, sK12(X0)) | h_both(X0, sK12(X0))) | (~ spl23_12 | ~ spl23_13)), inference(resolution, [], [f298, f239])).
fof(f298, plain, (! [X0, X1] : (h_both(X0, sK10(X0)) | g_true_only(X0, X1) | h_true_only(X0, sK10(X0)) | g_false_only(X0, X1) | g_true_only(X0, sK11(X0))) | ~ spl23_12), inference(resolution, [], [f249, f101])).
fof(f249, plain, (! [X0, X1] : (~ g_both(X0, X1) | g_true_only(X0, sK11(X0)) | h_both(X0, sK10(X0)) | h_true_only(X0, sK10(X0))) | ~ spl23_12), inference(resolution, [], [f162, f111])).
fof(f371, plain, (spl23_27 | spl23_28 | ~ spl23_12 | ~ spl23_13 | ~ spl23_23 | spl23_25), inference(avatar_split_clause, [], [f362, f212, f205, f165, f161, f368, f364])).
fof(f362, plain, (h_both(sK21, sK12(sK21)) | h_both(sK21, sK10(sK21)) | (~ spl23_12 | ~ spl23_13 | ~ spl23_23 | spl23_25)), inference(subsumption_resolution, [], [f361, f206])).
fof(f361, plain, (h_both(sK21, sK12(sK21)) | h_true_only(sK21, sK12(sK21)) | h_both(sK21, sK10(sK21)) | (~ spl23_12 | ~ spl23_13 | ~ spl23_23 | spl23_25)), inference(subsumption_resolution, [], [f353, f206])).
fof(f353, plain, (h_both(sK21, sK12(sK21)) | h_true_only(sK21, sK10(sK21)) | h_true_only(sK21, sK12(sK21)) | h_both(sK21, sK10(sK21)) | (~ spl23_12 | ~ spl23_13 | spl23_25)), inference(resolution, [], [f314, f214])).
fof(f221, plain, (~ spl23_25 | ~ spl23_26), inference(avatar_split_clause, [], [f82, f217, f212])).
fof(f82, plain, (~ g_false_only(sK19, sK20) | ~ g_false_only(sK21, sK22)), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((sP3 | sP2 | ((! [X1] : h_false_only(sK14, X1) & ! [X2] : ~ g_true_only(sK14, X2) & g_both(sK14, sK15)) | sP1(sK14)) | ! [X4] : (h_true_only(X4, sK16(X4)) | ! [X6] : ~ h_both(X4, X6) | g_true_only(X4, sK17(X4)) | ! [X8] : ~ g_both(X4, X8))) & (! [X9, X10] : ((~ h_false_only(X9, sK18(X9, X10)) | ~ g_both(X9, X10)) & ((~ h_false_only(X9, sK18(X9, X10)) & ~ h_both(X9, sK18(X9, X10))) | ~ g_true_only(X9, X10))) | sP0) & (! [X14] : (~ h_true_only(sK19, X14) & ~ g_false_only(sK19, sK20)) | (! [X16] : ~ h_true_only(sK21, X16) & ~ g_false_only(sK21, sK22)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15, sK16, sK17, sK18, sK19, sK20, sK21, sK22])], [f46, f54, f53, f52, f51, f50, f49, f48, f47])).
fof(f47, plain, (? [X0] : ((! [X1] : h_false_only(X0, X1) & ! [X2] : ~ g_true_only(X0, X2) & ? [X3] : g_both(X0, X3)) | sP1(X0)) => ((! [X1] : h_false_only(sK14, X1) & ! [X2] : ~ g_true_only(sK14, X2) & ? [X3] : g_both(sK14, X3)) | sP1(sK14))), introduced(choice_axiom, [])).
fof(f48, plain, (? [X3] : g_both(sK14, X3) => g_both(sK14, sK15)), introduced(choice_axiom, [])).
fof(f49, plain, ! [X4] : (? [X5] : h_true_only(X4, X5) => h_true_only(X4, sK16(X4))), introduced(choice_axiom, [])).
fof(f50, plain, ! [X4] : (? [X7] : g_true_only(X4, X7) => g_true_only(X4, sK17(X4))), introduced(choice_axiom, [])).
fof(f51, plain, ! [X10, X9] : (? [X11] : ((~ h_false_only(X9, X11) | ~ g_both(X9, X10)) & ((~ h_false_only(X9, X11) & ~ h_both(X9, X11)) | ~ g_true_only(X9, X10))) => ((~ h_false_only(X9, sK18(X9, X10)) | ~ g_both(X9, X10)) & ((~ h_false_only(X9, sK18(X9, X10)) & ~ h_both(X9, sK18(X9, X10))) | ~ g_true_only(X9, X10)))), introduced(choice_axiom, [])).
fof(f52, plain, (? [X12, X13] : ! [X14] : (~ h_true_only(X12, X14) & ~ g_false_only(X12, X13)) => ! [X14] : (~ h_true_only(sK19, X14) & ~ g_false_only(sK19, sK20))), introduced(choice_axiom, [])).
fof(f53, plain, (? [X15] : (! [X16] : ~ h_true_only(X15, X16) & ? [X17] : ~ g_false_only(X15, X17)) => (! [X16] : ~ h_true_only(sK21, X16) & ? [X17] : ~ g_false_only(sK21, X17))), introduced(choice_axiom, [])).
fof(f54, plain, (? [X17] : ~ g_false_only(sK21, X17) => ~ g_false_only(sK21, sK22)), introduced(choice_axiom, [])).
fof(f46, plain, ((sP3 | sP2 | ? [X0] : ((! [X1] : h_false_only(X0, X1) & ! [X2] : ~ g_true_only(X0, X2) & ? [X3] : g_both(X0, X3)) | sP1(X0)) | ! [X4] : (? [X5] : h_true_only(X4, X5) | ! [X6] : ~ h_both(X4, X6) | ? [X7] : g_true_only(X4, X7) | ! [X8] : ~ g_both(X4, X8))) & (! [X9, X10] : ? [X11] : ((~ h_false_only(X9, X11) | ~ g_both(X9, X10)) & ((~ h_false_only(X9, X11) & ~ h_both(X9, X11)) | ~ g_true_only(X9, X10))) | sP0) & (? [X12, X13] : ! [X14] : (~ h_true_only(X12, X14) & ~ g_false_only(X12, X13)) | ? [X15] : (! [X16] : ~ h_true_only(X15, X16) & ? [X17] : ~ g_false_only(X15, X17)))), inference(rectify, [], [f25])).
fof(f25, plain, ((sP3 | sP2 | ? [X9] : ((! [X10] : h_false_only(X9, X10) & ! [X11] : ~ g_true_only(X9, X11) & ? [X12] : g_both(X9, X12)) | sP1(X9)) | ! [X17] : (? [X18] : h_true_only(X17, X18) | ! [X19] : ~ h_both(X17, X19) | ? [X20] : g_true_only(X17, X20) | ! [X21] : ~ g_both(X17, X21))) & (! [X22, X23] : ? [X24] : ((~ h_false_only(X22, X24) | ~ g_both(X22, X23)) & ((~ h_false_only(X22, X24) & ~ h_both(X22, X24)) | ~ g_true_only(X22, X23))) | sP0) & (? [X33, X34] : ! [X35] : (~ h_true_only(X33, X35) & ~ g_false_only(X33, X34)) | ? [X36] : (! [X37] : ~ h_true_only(X36, X37) & ? [X38] : ~ g_false_only(X36, X38)))), inference(definition_folding, [], [f20, e24, e23, e22, e21])).
fof(f21, plain, (! [X25] : ((? [X26] : ~ h_false_only(X25, X26) | ? [X27] : g_true_only(X25, X27) | ! [X28] : ~ g_both(X25, X28)) & ((? [X29] : ~ h_false_only(X25, X29) & (? [X30] : h_true_only(X25, X30) | ! [X31] : ~ h_both(X25, X31))) | ! [X32] : ~ g_true_only(X25, X32))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> ! [X25] : ((? [X26] : ~ h_false_only(X25, X26) | ? [X27] : g_true_only(X25, X27) | ! [X28] : ~ g_both(X25, X28)) & ((? [X29] : ~ h_false_only(X25, X29) & (? [X30] : h_true_only(X25, X30) | ! [X31] : ~ h_both(X25, X31))) | ! [X32] : ~ g_true_only(X25, X32)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f23, plain, (! [X3] : (? [X4] : ! [X5] : ((h_false_only(X3, X5) & g_both(X3, X4)) | ((h_false_only(X3, X5) | h_both(X3, X5)) & g_true_only(X3, X4))) | ! [X6] : (? [X7] : (h_true_only(X3, X7) | g_false_only(X3, X6)) | ! [X8] : (~ h_both(X3, X8) | ~ g_both(X3, X6)))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> ! [X3] : (? [X4] : ! [X5] : ((h_false_only(X3, X5) & g_both(X3, X4)) | ((h_false_only(X3, X5) | h_both(X3, X5)) & g_true_only(X3, X4))) | ! [X6] : (? [X7] : (h_true_only(X3, X7) | g_false_only(X3, X6)) | ! [X8] : (~ h_both(X3, X8) | ~ g_both(X3, X6))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, (? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> ? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f20, plain, ((? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) | ! [X3] : (? [X4] : ! [X5] : ((h_false_only(X3, X5) & g_both(X3, X4)) | ((h_false_only(X3, X5) | h_both(X3, X5)) & g_true_only(X3, X4))) | ! [X6] : (? [X7] : (h_true_only(X3, X7) | g_false_only(X3, X6)) | ! [X8] : (~ h_both(X3, X8) | ~ g_both(X3, X6)))) | ? [X9] : ((! [X10] : h_false_only(X9, X10) & ! [X11] : ~ g_true_only(X9, X11) & ? [X12] : g_both(X9, X12)) | ((! [X13] : h_false_only(X9, X13) | (! [X14] : ~ h_true_only(X9, X14) & ? [X15] : h_both(X9, X15))) & ? [X16] : g_true_only(X9, X16))) | ! [X17] : (? [X18] : h_true_only(X17, X18) | ! [X19] : ~ h_both(X17, X19) | ? [X20] : g_true_only(X17, X20) | ! [X21] : ~ g_both(X17, X21))) & (! [X22, X23] : ? [X24] : ((~ h_false_only(X22, X24) | ~ g_both(X22, X23)) & ((~ h_false_only(X22, X24) & ~ h_both(X22, X24)) | ~ g_true_only(X22, X23))) | ! [X25] : ((? [X26] : ~ h_false_only(X25, X26) | ? [X27] : g_true_only(X25, X27) | ! [X28] : ~ g_both(X25, X28)) & ((? [X29] : ~ h_false_only(X25, X29) & (? [X30] : h_true_only(X25, X30) | ! [X31] : ~ h_both(X25, X31))) | ! [X32] : ~ g_true_only(X25, X32)))) & (? [X33, X34] : ! [X35] : (~ h_true_only(X33, X35) & ~ g_false_only(X33, X34)) | ? [X36] : (! [X37] : ~ h_true_only(X36, X37) & ? [X38] : ~ g_false_only(X36, X38)))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((~ ? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X3] : (~ ? [X4] : ! [X5] : ((h_false_only(X3, X5) & g_both(X3, X4)) | ((h_false_only(X3, X5) | h_both(X3, X5)) & g_true_only(X3, X4))) & ? [X6] : (~ ? [X7] : (h_true_only(X3, X7) | g_false_only(X3, X6)) & ? [X8] : (h_both(X3, X8) & g_both(X3, X6)))) & ~ ? [X9] : ((! [X10] : h_false_only(X9, X10) & ~ ? [X11] : g_true_only(X9, X11) & ? [X12] : g_both(X9, X12)) | ((! [X13] : h_false_only(X9, X13) | (~ ? [X14] : h_true_only(X9, X14) & ? [X15] : h_both(X9, X15))) & ? [X16] : g_true_only(X9, X16))) & ? [X17] : (~ ? [X18] : h_true_only(X17, X18) & ? [X19] : h_both(X17, X19) & ~ ? [X20] : g_true_only(X17, X20) & ? [X21] : g_both(X17, X21))) | (? [X22, X23] : ! [X24] : ((h_false_only(X22, X24) & g_both(X22, X23)) | ((h_false_only(X22, X24) | h_both(X22, X24)) & g_true_only(X22, X23))) & ? [X25] : ((! [X26] : h_false_only(X25, X26) & ~ ? [X27] : g_true_only(X25, X27) & ? [X28] : g_both(X25, X28)) | ((! [X29] : h_false_only(X25, X29) | (~ ? [X30] : h_true_only(X25, X30) & ? [X31] : h_both(X25, X31))) & ? [X32] : g_true_only(X25, X32)))) | (! [X33, X34] : ? [X35] : (h_true_only(X33, X35) | g_false_only(X33, X34)) & ! [X36] : (? [X37] : h_true_only(X36, X37) | ! [X38] : g_false_only(X36, X38)))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ((~ ? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X0] : (~ ? [X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X1] : (~ ? [X2] : (h_true_only(X0, X2) | g_false_only(X0, X1)) & ? [X2] : (h_both(X0, X2) & g_both(X0, X1)))) & ~ ? [X0] : ((! [X1] : h_false_only(X0, X1) & ~ ? [X1] : g_true_only(X0, X1) & ? [X1] : g_both(X0, X1)) | ((! [X1] : h_false_only(X0, X1) | (~ ? [X1] : h_true_only(X0, X1) & ? [X1] : h_both(X0, X1))) & ? [X1] : g_true_only(X0, X1))) & ? [X0] : (~ ? [X1] : h_true_only(X0, X1) & ? [X1] : h_both(X0, X1) & ~ ? [X1] : g_true_only(X0, X1) & ? [X1] : g_both(X0, X1))) | (? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X0] : ((! [X1] : h_false_only(X0, X1) & ~ ? [X1] : g_true_only(X0, X1) & ? [X1] : g_both(X0, X1)) | ((! [X1] : h_false_only(X0, X1) | (~ ? [X1] : h_true_only(X0, X1) & ? [X1] : h_both(X0, X1))) & ? [X1] : g_true_only(X0, X1)))) | (! [X0, X1] : ? [X2] : (h_true_only(X0, X2) | g_false_only(X0, X1)) & ! [X0] : (? [X1] : h_true_only(X0, X1) | ! [X1] : g_false_only(X0, X1)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ((~ ? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X0] : (~ ? [X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X1] : (~ ? [X2] : (h_true_only(X0, X2) | g_false_only(X0, X1)) & ? [X2] : (h_both(X0, X2) & g_both(X0, X1)))) & ~ ? [X0] : ((! [X1] : h_false_only(X0, X1) & ~ ? [X1] : g_true_only(X0, X1) & ? [X1] : g_both(X0, X1)) | ((! [X1] : h_false_only(X0, X1) | (~ ? [X1] : h_true_only(X0, X1) & ? [X1] : h_both(X0, X1))) & ? [X1] : g_true_only(X0, X1))) & ? [X0] : (~ ? [X1] : h_true_only(X0, X1) & ? [X1] : h_both(X0, X1) & ~ ? [X1] : g_true_only(X0, X1) & ? [X1] : g_both(X0, X1))) | (? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) & ? [X0] : ((! [X1] : h_false_only(X0, X1) & ~ ? [X1] : g_true_only(X0, X1) & ? [X1] : g_both(X0, X1)) | ((! [X1] : h_false_only(X0, X1) | (~ ? [X1] : h_true_only(X0, X1) & ? [X1] : h_both(X0, X1))) & ? [X1] : g_true_only(X0, X1)))) | (! [X0, X1] : ? [X2] : (h_true_only(X0, X2) | g_false_only(X0, X1)) & ! [X0] : (? [X1] : h_true_only(X0, X1) | ! [X1] : g_false_only(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO606+1.p', nc5)).
fof(f220, plain, (spl23_23 | ~ spl23_26), inference(avatar_split_clause, [], [f83, f217, f205])).
fof(f83, plain, ! [X16] : (~ g_false_only(sK19, sK20) | ~ h_true_only(sK21, X16)), inference(cnf_transformation, [], [f55])).
fof(f215, plain, (~ spl23_25 | spl23_24), inference(avatar_split_clause, [], [f84, f208, f212])).
fof(f84, plain, ! [X14] : (~ h_true_only(sK19, X14) | ~ g_false_only(sK21, sK22)), inference(cnf_transformation, [], [f55])).
fof(f210, plain, (spl23_23 | spl23_24), inference(avatar_split_clause, [], [f85, f208, f205])).
fof(f85, plain, ! [X14, X16] : (~ h_true_only(sK19, X14) | ~ h_true_only(sK21, X16)), inference(cnf_transformation, [], [f55])).
fof(f203, plain, (spl23_11 | spl23_22), inference(avatar_split_clause, [], [f86, f201, f157])).
fof(f157, plain, (spl23_11 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl23_11])])).
fof(f86, plain, ! [X10, X9] : (~ h_both(X9, sK18(X9, X10)) | ~ g_true_only(X9, X10) | sP0), inference(cnf_transformation, [], [f55])).
fof(f199, plain, (spl23_11 | spl23_21), inference(avatar_split_clause, [], [f87, f197, f157])).
fof(f87, plain, ! [X10, X9] : (~ h_false_only(X9, sK18(X9, X10)) | ~ g_true_only(X9, X10) | sP0), inference(cnf_transformation, [], [f55])).
fof(f195, plain, (spl23_11 | spl23_20), inference(avatar_split_clause, [], [f88, f193, f157])).
fof(f88, plain, ! [X10, X9] : (~ h_false_only(X9, sK18(X9, X10)) | ~ g_both(X9, X10) | sP0), inference(cnf_transformation, [], [f55])).
fof(f191, plain, (spl23_15 | spl23_16 | spl23_19 | spl23_6 | spl23_1), inference(avatar_split_clause, [], [f89, f115, f137, f188, f176, f173])).
fof(f137, plain, (spl23_6 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl23_6])])).
fof(f115, plain, (spl23_1 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl23_1])])).
fof(f89, plain, ! [X6, X4, X8] : (sP3 | sP2 | g_both(sK14, sK15) | sP1(sK14) | h_true_only(X4, sK16(X4)) | ~ h_both(X4, X6) | g_true_only(X4, sK17(X4)) | ~ g_both(X4, X8)), inference(cnf_transformation, [], [f55])).
fof(f186, plain, (spl23_15 | spl23_16 | spl23_18 | spl23_6 | spl23_1), inference(avatar_split_clause, [], [f90, f115, f137, f184, f176, f173])).
fof(f90, plain, ! [X6, X4, X2, X8] : (sP3 | sP2 | ~ g_true_only(sK14, X2) | sP1(sK14) | h_true_only(X4, sK16(X4)) | ~ h_both(X4, X6) | g_true_only(X4, sK17(X4)) | ~ g_both(X4, X8)), inference(cnf_transformation, [], [f55])).
fof(f182, plain, (spl23_15 | spl23_16 | spl23_17 | spl23_6 | spl23_1), inference(avatar_split_clause, [], [f91, f115, f137, f180, f176, f173])).
fof(f91, plain, ! [X6, X4, X8, X1] : (sP3 | sP2 | h_false_only(sK14, X1) | sP1(sK14) | h_true_only(X4, sK16(X4)) | ~ h_both(X4, X6) | g_true_only(X4, sK17(X4)) | ~ g_both(X4, X8)), inference(cnf_transformation, [], [f55])).
fof(f171, plain, (~ spl23_11 | spl23_14), inference(avatar_split_clause, [], [f79, f169, f157])).
fof(f79, plain, ! [X6, X0, X7] : (h_true_only(X0, sK13(X0)) | ~ h_both(X0, X6) | ~ g_true_only(X0, X7) | ~ sP0), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (! [X0] : ((~ h_false_only(X0, sK10(X0)) | g_true_only(X0, sK11(X0)) | ! [X3] : ~ g_both(X0, X3)) & ((~ h_false_only(X0, sK12(X0)) & (h_true_only(X0, sK13(X0)) | ! [X6] : ~ h_both(X0, X6))) | ! [X7] : ~ g_true_only(X0, X7))) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12, sK13])], [f40, f44, f43, f42, f41])).
fof(f41, plain, ! [X0] : (? [X1] : ~ h_false_only(X0, X1) => ~ h_false_only(X0, sK10(X0))), introduced(choice_axiom, [])).
fof(f42, plain, ! [X0] : (? [X2] : g_true_only(X0, X2) => g_true_only(X0, sK11(X0))), introduced(choice_axiom, [])).
fof(f43, plain, ! [X0] : (? [X4] : ~ h_false_only(X0, X4) => ~ h_false_only(X0, sK12(X0))), introduced(choice_axiom, [])).
fof(f44, plain, ! [X0] : (? [X5] : h_true_only(X0, X5) => h_true_only(X0, sK13(X0))), introduced(choice_axiom, [])).
fof(f40, plain, (! [X0] : ((? [X1] : ~ h_false_only(X0, X1) | ? [X2] : g_true_only(X0, X2) | ! [X3] : ~ g_both(X0, X3)) & ((? [X4] : ~ h_false_only(X0, X4) & (? [X5] : h_true_only(X0, X5) | ! [X6] : ~ h_both(X0, X6))) | ! [X7] : ~ g_true_only(X0, X7))) | ~ sP0), inference(rectify, [], [f39])).
fof(f39, plain, (! [X25] : ((? [X26] : ~ h_false_only(X25, X26) | ? [X27] : g_true_only(X25, X27) | ! [X28] : ~ g_both(X25, X28)) & ((? [X29] : ~ h_false_only(X25, X29) & (? [X30] : h_true_only(X25, X30) | ! [X31] : ~ h_both(X25, X31))) | ! [X32] : ~ g_true_only(X25, X32))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f167, plain, (~ spl23_11 | spl23_13), inference(avatar_split_clause, [], [f80, f165, f157])).
fof(f80, plain, ! [X0, X7] : (~ h_false_only(X0, sK12(X0)) | ~ g_true_only(X0, X7) | ~ sP0), inference(cnf_transformation, [], [f45])).
fof(f163, plain, (~ spl23_11 | spl23_12), inference(avatar_split_clause, [], [f81, f161, f157])).
fof(f81, plain, ! [X0, X3] : (~ h_false_only(X0, sK10(X0)) | g_true_only(X0, sK11(X0)) | ~ g_both(X0, X3) | ~ sP0), inference(cnf_transformation, [], [f45])).
fof(f155, plain, (~ spl23_6 | spl23_10), inference(avatar_split_clause, [], [f72, f153, f137])).
fof(f72, plain, ! [X0, X5, X3] : (g_both(X0, sK6(X0)) | g_true_only(X0, sK6(X0)) | h_true_only(X0, sK7(X0, X3)) | g_false_only(X0, X3) | ~ h_both(X0, X5) | ~ g_both(X0, X3) | ~ sP2), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (! [X0] : (! [X2] : ((h_false_only(X0, X2) & g_both(X0, sK6(X0))) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, sK6(X0)))) | ! [X3] : ((h_true_only(X0, sK7(X0, X3)) | g_false_only(X0, X3)) | ! [X5] : (~ h_both(X0, X5) | ~ g_both(X0, X3)))) | ~ sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f30, f32, f31])).
fof(f31, plain, ! [X0] : (? [X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) => ! [X2] : ((h_false_only(X0, X2) & g_both(X0, sK6(X0))) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, sK6(X0))))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X3, X0] : (? [X4] : (h_true_only(X0, X4) | g_false_only(X0, X3)) => (h_true_only(X0, sK7(X0, X3)) | g_false_only(X0, X3))), introduced(choice_axiom, [])).
fof(f30, plain, (! [X0] : (? [X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) | ! [X3] : (? [X4] : (h_true_only(X0, X4) | g_false_only(X0, X3)) | ! [X5] : (~ h_both(X0, X5) | ~ g_both(X0, X3)))) | ~ sP2), inference(rectify, [], [f29])).
fof(f29, plain, (! [X3] : (? [X4] : ! [X5] : ((h_false_only(X3, X5) & g_both(X3, X4)) | ((h_false_only(X3, X5) | h_both(X3, X5)) & g_true_only(X3, X4))) | ! [X6] : (? [X7] : (h_true_only(X3, X7) | g_false_only(X3, X6)) | ! [X8] : (~ h_both(X3, X8) | ~ g_both(X3, X6)))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f147, plain, (~ spl23_6 | spl23_8), inference(avatar_split_clause, [], [f74, f145, f137])).
fof(f74, plain, ! [X2, X0, X5, X3] : (h_false_only(X0, X2) | g_true_only(X0, sK6(X0)) | h_true_only(X0, sK7(X0, X3)) | g_false_only(X0, X3) | ~ h_both(X0, X5) | ~ g_both(X0, X3) | ~ sP2), inference(cnf_transformation, [], [f33])).
fof(f143, plain, (~ spl23_6 | spl23_7), inference(avatar_split_clause, [], [f112, f141, f137])).
fof(f112, plain, ! [X2, X0, X5, X3] : (h_false_only(X0, X2) | h_both(X0, X2) | h_true_only(X0, sK7(X0, X3)) | g_false_only(X0, X3) | ~ h_both(X0, X5) | ~ g_both(X0, X3) | ~ sP2), inference(duplicate_literal_removal, [], [f75])).
fof(f75, plain, ! [X2, X0, X5, X3] : (h_false_only(X0, X2) | h_false_only(X0, X2) | h_both(X0, X2) | h_true_only(X0, sK7(X0, X3)) | g_false_only(X0, X3) | ~ h_both(X0, X5) | ~ g_both(X0, X3) | ~ sP2), inference(cnf_transformation, [], [f33])).
fof(f135, plain, (~ spl23_1 | spl23_3 | spl23_5), inference(avatar_split_clause, [], [f68, f131, f123, f115])).
fof(f68, plain, (g_both(sK4, sK5) | g_true_only(sK4, sK5) | ~ sP3), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (! [X2] : ((h_false_only(sK4, X2) & g_both(sK4, sK5)) | ((h_false_only(sK4, X2) | h_both(sK4, X2)) & g_true_only(sK4, sK5))) | ~ sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f26, f27])).
fof(f27, plain, (? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) => ! [X2] : ((h_false_only(sK4, X2) & g_both(sK4, sK5)) | ((h_false_only(sK4, X2) | h_both(sK4, X2)) & g_true_only(sK4, sK5)))), introduced(choice_axiom, [])).
fof(f26, plain, (? [X0, X1] : ! [X2] : ((h_false_only(X0, X2) & g_both(X0, X1)) | ((h_false_only(X0, X2) | h_both(X0, X2)) & g_true_only(X0, X1))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f129, plain, (~ spl23_1 | spl23_3 | spl23_4), inference(avatar_split_clause, [], [f70, f127, f123, f115])).
fof(f70, plain, ! [X2] : (h_false_only(sK4, X2) | g_true_only(sK4, sK5) | ~ sP3), inference(cnf_transformation, [], [f28])).
fof(f121, plain, (~ spl23_1 | spl23_2), inference(avatar_split_clause, [], [f113, f119, f115])).
fof(f113, plain, ! [X2] : (h_false_only(sK4, X2) | h_both(sK4, X2) | ~ sP3), inference(duplicate_literal_removal, [], [f71])).
fof(f71, plain, ! [X2] : (h_false_only(sK4, X2) | h_false_only(sK4, X2) | h_both(sK4, X2) | ~ sP3), inference(cnf_transformation, [], [f28])).