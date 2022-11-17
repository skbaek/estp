fof(f3994, plain, $false, inference(avatar_sat_refutation, [], [f741, f775, f809, f826, f843, f894, f911, f912, f915, f917, f920, f926, f928, f931, f934, f938, f940, f941, f977, f1045, f1096, f1113, f1130, f1147, f1198, f1219, f1221, f1224, f1225, f1228, f1230, f1237, f1242, f1244, f1245, f1246, f1256, f1265, f1274, f1283, f1289, f1292, f1301, f1307, f1310, f1317, f1318, f1327, f1328, f1335, f1336, f1345, f1346, f1355, f1360, f1369, f1378, f1383, f1384, f1385, f1386, f1388, f1389, f1392, f1395, f1400, f1405, f1409, f1414, f1418, f1427, f1436, f1442, f1445, f1454, f1460, f1463, f1470, f1471, f1479, f1481, f1488, f1490, f1497, f1498, f1508, f1517, f1522, f1531, f1536, f1537, f1538, f1539, f1541, f1542, f1543, f1548, f1549, f1553, f1555, f1556, f1558, f1559, f1647, f1672, f1971, f2243, f2280, f2289, f2354, f2359, f2360, f2363, f2381, f2386, f2432, f2442, f2444, f2475, f2483, f2489, f2497, f2501, f2502, f2503, f2506, f2525, f2527, f2533, f2537, f2543, f2545, f2551, f2555, f2580, f2582, f2584, f2629, f2648, f2661, f2697, f2704, f2711, f2718, f2736, f2738, f2747, f2774, f2784, f2818, f2828, f2842, f2848, f2850, f2852, f2854, f2856, f2865, f2874, f2876, f2880, f2891, f2898, f2923, f2934, f2937, f2947, f2950, f2965, f2969, f2975, f2985, f2987, f2989, f2993, f3002, f3006, f3009, f3013, f3016, f3024, f3045, f3063, f3071, f3080, f3090, f3108, f3128, f3129, f3154, f3173, f3177, f3180, f3205, f3218, f3221, f3238, f3250, f3269, f3321, f3346, f3378, f3415, f3445, f3471, f3503, f3538, f3570, f3607, f3642, f3662, f3690, f3706, f3750, f3780, f3808, f3830, f3832, f3868, f3913, f3921, f3922, f3993])).
fof(f3993, plain, (~ spl42_18 | ~ spl42_19), inference(avatar_contradiction_clause, [], [f3992])).
fof(f3992, plain, ($false | (~ spl42_18 | ~ spl42_19)), inference(subsumption_resolution, [], [f3991, f303])).
fof(f303, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax7)).
fof(f3991, plain, ((e11 = e12) | (~ spl42_18 | ~ spl42_19)), inference(forward_demodulation, [], [f719, f715])).
fof(f715, plain, ((e11 = op1(e12, e13)) | ~ spl42_18), inference(avatar_component_clause, [], [f713])).
fof(f713, plain, (spl42_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_18])])).
fof(f719, plain, ((e12 = op1(e12, e13)) | ~ spl42_19), inference(avatar_component_clause, [], [f717])).
fof(f717, plain, (spl42_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_19])])).
fof(f3922, plain, (~ spl42_58 | ~ spl42_62), inference(avatar_split_clause, [], [f3915, f900, f883])).
fof(f883, plain, (spl42_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_58])])).
fof(f900, plain, (spl42_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl42_62])])).
fof(f3915, plain, (~ (e11 = op1(e10, e11)) | ~ spl42_62), inference(backward_demodulation, [], [f228, f902])).
fof(f902, plain, ((op1(e10, e10) = e11) | ~ spl42_62), inference(avatar_component_clause, [], [f900])).
fof(f228, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax5)).
fof(f3921, plain, (~ spl42_46 | ~ spl42_62), inference(avatar_split_clause, [], [f3914, f900, f832])).
fof(f832, plain, (spl42_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_46])])).
fof(f3914, plain, (~ (e11 = op1(e11, e10)) | ~ spl42_62), inference(backward_demodulation, [], [f204, f902])).
fof(f204, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f3913, plain, (~ spl42_82 | ~ spl42_83), inference(avatar_contradiction_clause, [], [f3912])).
fof(f3912, plain, ($false | (~ spl42_82 | ~ spl42_83)), inference(subsumption_resolution, [], [f3911, f309])).
fof(f309, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax8)).
fof(f3911, plain, ((e21 = e22) | (~ spl42_82 | ~ spl42_83)), inference(forward_demodulation, [], [f1023, f1019])).
fof(f1019, plain, ((e21 = op2(e22, e23)) | ~ spl42_82), inference(avatar_component_clause, [], [f1017])).
fof(f1017, plain, (spl42_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_82])])).
fof(f1023, plain, ((e22 = op2(e22, e23)) | ~ spl42_83), inference(avatar_component_clause, [], [f1021])).
fof(f1021, plain, (spl42_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_83])])).
fof(f3868, plain, (~ spl42_53 | ~ spl42_56), inference(avatar_contradiction_clause, [], [f3867])).
fof(f3867, plain, ($false | (~ spl42_53 | ~ spl42_56)), inference(subsumption_resolution, [], [f3866, f302])).
fof(f302, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f3866, plain, ((e10 = e13) | (~ spl42_53 | ~ spl42_56)), inference(backward_demodulation, [], [f876, f864])).
fof(f864, plain, ((e10 = op1(e10, e12)) | ~ spl42_53), inference(avatar_component_clause, [], [f862])).
fof(f862, plain, (spl42_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_53])])).
fof(f876, plain, ((e13 = op1(e10, e12)) | ~ spl42_56), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl42_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_56])])).
fof(f3832, plain, (~ spl42_63 | ~ spl42_51), inference(avatar_split_clause, [], [f2861, f853, f904])).
fof(f904, plain, (spl42_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl42_63])])).
fof(f853, plain, (spl42_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_51])])).
fof(f2861, plain, (~ (op1(e10, e10) = e12) | ~ spl42_51), inference(forward_demodulation, [], [f230, f855])).
fof(f855, plain, ((e12 = op1(e10, e13)) | ~ spl42_51), inference(avatar_component_clause, [], [f853])).
fof(f230, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3830, plain, (~ spl42_23 | ~ spl42_39), inference(avatar_split_clause, [], [f3703, f802, f734])).
fof(f734, plain, (spl42_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_23])])).
fof(f802, plain, (spl42_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_39])])).
fof(f3703, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_39), inference(forward_demodulation, [], [f219, f804])).
fof(f804, plain, ((e12 = op1(e11, e12)) | ~ spl42_39), inference(avatar_component_clause, [], [f802])).
fof(f219, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f3808, plain, (~ spl42_21 | ~ spl42_29), inference(avatar_split_clause, [], [f3800, f760, f726])).
fof(f726, plain, (spl42_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_21])])).
fof(f760, plain, (spl42_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_29])])).
fof(f3800, plain, (~ (e10 = op1(e12, e12)) | ~ spl42_29), inference(backward_demodulation, [], [f241, f762])).
fof(f762, plain, ((e10 = op1(e12, e10)) | ~ spl42_29), inference(avatar_component_clause, [], [f760])).
fof(f241, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f3780, plain, (~ spl42_57 | ~ spl42_122 | ~ spl42_167 | ~ spl42_172 | ~ spl42_208), inference(avatar_contradiction_clause, [], [f3779])).
fof(f3779, plain, ($false | (~ spl42_57 | ~ spl42_122 | ~ spl42_167 | ~ spl42_172 | ~ spl42_208)), inference(subsumption_resolution, [], [f3778, f306])).
fof(f306, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f3778, plain, ((e20 = e21) | (~ spl42_57 | ~ spl42_122 | ~ spl42_167 | ~ spl42_172 | ~ spl42_208)), inference(forward_demodulation, [], [f3773, f1670])).
fof(f1670, plain, ((e20 = h3(e10)) | ~ spl42_172), inference(avatar_component_clause, [], [f1669])).
fof(f1669, plain, (spl42_172 <=> (e20 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_172])])).
fof(f3773, plain, ((e21 = h3(e10)) | (~ spl42_57 | ~ spl42_122 | ~ spl42_167 | ~ spl42_172 | ~ spl42_208)), inference(backward_demodulation, [], [f3724, f881])).
fof(f881, plain, ((e10 = op1(e10, e11)) | ~ spl42_57), inference(avatar_component_clause, [], [f879])).
fof(f879, plain, (spl42_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_57])])).
fof(f3724, plain, ((e21 = h3(op1(e10, e11))) | (~ spl42_122 | ~ spl42_167 | ~ spl42_172 | ~ spl42_208)), inference(forward_demodulation, [], [f3572, f1189])).
fof(f1189, plain, ((e21 = op2(e20, e21)) | ~ spl42_122), inference(avatar_component_clause, [], [f1187])).
fof(f1187, plain, (spl42_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_122])])).
fof(f3572, plain, ((op2(e20, e21) = h3(op1(e10, e11))) | (~ spl42_167 | ~ spl42_172 | ~ spl42_208)), inference(forward_demodulation, [], [f3571, f1670])).
fof(f3571, plain, ((h3(op1(e10, e11)) = op2(h3(e10), e21)) | (~ spl42_167 | ~ spl42_208)), inference(forward_demodulation, [], [f1909, f1645])).
fof(f1645, plain, ((e21 = h3(e11)) | ~ spl42_167), inference(avatar_component_clause, [], [f1644])).
fof(f1644, plain, (spl42_167 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_167])])).
fof(f1909, plain, ((h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ spl42_208), inference(avatar_component_clause, [], [f1908])).
fof(f1908, plain, (spl42_208 <=> (h3(op1(e10, e11)) = op2(h3(e10), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_208])])).
fof(f3750, plain, (~ spl42_36 | ~ spl42_103 | ~ spl42_167 | spl42_222), inference(avatar_contradiction_clause, [], [f3749])).
fof(f3749, plain, ($false | (~ spl42_36 | ~ spl42_103 | ~ spl42_167 | spl42_222)), inference(subsumption_resolution, [], [f3748, f1108])).
fof(f1108, plain, ((e22 = op2(e21, e22)) | ~ spl42_103), inference(avatar_component_clause, [], [f1106])).
fof(f1106, plain, (spl42_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_103])])).
fof(f3748, plain, (~ (e22 = op2(e21, e22)) | (~ spl42_36 | ~ spl42_167 | spl42_222)), inference(forward_demodulation, [], [f3747, f536])).
fof(f536, plain, (e22 = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(op2(e22, e22), e22) = h3(e12)) & (h3(e11) = op2(op2(op2(e22, e22), e22), e22)) & (op2(e22, e22) = h3(e10)) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax16)).
fof(f3747, plain, (~ (op2(e21, e22) = h3(e13)) | (~ spl42_36 | ~ spl42_167 | spl42_222)), inference(forward_demodulation, [], [f3746, f791])).
fof(f791, plain, ((e13 = op1(e11, e13)) | ~ spl42_36), inference(avatar_component_clause, [], [f789])).
fof(f789, plain, (spl42_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_36])])).
fof(f3746, plain, (~ (op2(e21, e22) = h3(op1(e11, e13))) | (~ spl42_167 | spl42_222)), inference(forward_demodulation, [], [f1966, f1645])).
fof(f1966, plain, (~ (h3(op1(e11, e13)) = op2(h3(e11), e22)) | spl42_222), inference(avatar_component_clause, [], [f1964])).
fof(f1964, plain, (spl42_222 <=> (h3(op1(e11, e13)) = op2(h3(e11), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_222])])).
fof(f3706, plain, (~ spl42_22 | ~ spl42_18), inference(avatar_split_clause, [], [f2729, f713, f730])).
fof(f730, plain, (spl42_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_22])])).
fof(f2729, plain, (~ (e11 = op1(e12, e12)) | ~ spl42_18), inference(forward_demodulation, [], [f245, f715])).
fof(f245, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3690, plain, (~ spl42_16 | ~ spl42_95 | ~ spl42_172 | spl42_220), inference(avatar_contradiction_clause, [], [f3689])).
fof(f3689, plain, ($false | (~ spl42_16 | ~ spl42_95 | ~ spl42_172 | spl42_220)), inference(subsumption_resolution, [], [f3688, f1074])).
fof(f1074, plain, ((e22 = op2(e22, e20)) | ~ spl42_95), inference(avatar_component_clause, [], [f1072])).
fof(f1072, plain, (spl42_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_95])])).
fof(f3688, plain, (~ (e22 = op2(e22, e20)) | (~ spl42_16 | ~ spl42_172 | spl42_220)), inference(forward_demodulation, [], [f3687, f536])).
fof(f3687, plain, (~ (op2(e22, e20) = h3(e13)) | (~ spl42_16 | ~ spl42_172 | spl42_220)), inference(forward_demodulation, [], [f3686, f706])).
fof(f706, plain, ((e13 = op1(e13, e10)) | ~ spl42_16), inference(avatar_component_clause, [], [f704])).
fof(f704, plain, (spl42_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_16])])).
fof(f3686, plain, (~ (op2(e22, e20) = h3(op1(e13, e10))) | (~ spl42_172 | spl42_220)), inference(forward_demodulation, [], [f1958, f1670])).
fof(f1958, plain, (~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | spl42_220), inference(avatar_component_clause, [], [f1956])).
fof(f1956, plain, (spl42_220 <=> (h3(op1(e13, e10)) = op2(e22, h3(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_220])])).
fof(f3662, plain, (~ spl42_1 | spl42_217), inference(avatar_contradiction_clause, [], [f3661])).
fof(f3661, plain, ($false | (~ spl42_1 | spl42_217)), inference(trivial_inequality_removal, [], [f3660])).
fof(f3660, plain, (~ (h3(e10) = h3(e10)) | (~ spl42_1 | spl42_217)), inference(forward_demodulation, [], [f1946, f643])).
fof(f643, plain, ((e10 = op1(e13, e13)) | ~ spl42_1), inference(avatar_component_clause, [], [f641])).
fof(f641, plain, (spl42_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_1])])).
fof(f1946, plain, (~ (h3(e10) = h3(op1(e13, e13))) | spl42_217), inference(avatar_component_clause, [], [f1944])).
fof(f1944, plain, (spl42_217 <=> (h3(e10) = h3(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_217])])).
fof(f3642, plain, (~ spl42_31 | ~ spl42_80 | ~ spl42_172 | spl42_213 | ~ spl42_216), inference(avatar_contradiction_clause, [], [f3641])).
fof(f3641, plain, ($false | (~ spl42_31 | ~ spl42_80 | ~ spl42_172 | spl42_213 | ~ spl42_216)), inference(subsumption_resolution, [], [f3640, f1010])).
fof(f1010, plain, ((e23 = op2(e23, e20)) | ~ spl42_80), inference(avatar_component_clause, [], [f1008])).
fof(f1008, plain, (spl42_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_80])])).
fof(f3640, plain, (~ (e23 = op2(e23, e20)) | (~ spl42_31 | ~ spl42_172 | spl42_213 | ~ spl42_216)), inference(forward_demodulation, [], [f3639, f1941])).
fof(f1941, plain, ((e23 = h3(e12)) | ~ spl42_216), inference(avatar_component_clause, [], [f1940])).
fof(f1940, plain, (spl42_216 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_216])])).
fof(f3639, plain, (~ (op2(e23, e20) = h3(e12)) | (~ spl42_31 | ~ spl42_172 | spl42_213 | ~ spl42_216)), inference(forward_demodulation, [], [f3638, f770])).
fof(f770, plain, ((e12 = op1(e12, e10)) | ~ spl42_31), inference(avatar_component_clause, [], [f768])).
fof(f768, plain, (spl42_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_31])])).
fof(f3638, plain, (~ (op2(e23, e20) = h3(op1(e12, e10))) | (~ spl42_172 | spl42_213 | ~ spl42_216)), inference(forward_demodulation, [], [f3637, f1941])).
fof(f3637, plain, (~ (h3(op1(e12, e10)) = op2(h3(e12), e20)) | (~ spl42_172 | spl42_213)), inference(forward_demodulation, [], [f1930, f1670])).
fof(f1930, plain, (~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | spl42_213), inference(avatar_component_clause, [], [f1928])).
fof(f1928, plain, (spl42_213 <=> (h3(op1(e12, e10)) = op2(h3(e12), h3(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_213])])).
fof(f3607, plain, (~ spl42_28 | ~ spl42_75 | ~ spl42_167 | spl42_214 | ~ spl42_216), inference(avatar_contradiction_clause, [], [f3606])).
fof(f3606, plain, ($false | (~ spl42_28 | ~ spl42_75 | ~ spl42_167 | spl42_214 | ~ spl42_216)), inference(subsumption_resolution, [], [f3605, f989])).
fof(f989, plain, ((e22 = op2(e23, e21)) | ~ spl42_75), inference(avatar_component_clause, [], [f987])).
fof(f987, plain, (spl42_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_75])])).
fof(f3605, plain, (~ (e22 = op2(e23, e21)) | (~ spl42_28 | ~ spl42_167 | spl42_214 | ~ spl42_216)), inference(forward_demodulation, [], [f3604, f536])).
fof(f3604, plain, (~ (op2(e23, e21) = h3(e13)) | (~ spl42_28 | ~ spl42_167 | spl42_214 | ~ spl42_216)), inference(forward_demodulation, [], [f3603, f757])).
fof(f757, plain, ((e13 = op1(e12, e11)) | ~ spl42_28), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl42_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_28])])).
fof(f3603, plain, (~ (op2(e23, e21) = h3(op1(e12, e11))) | (~ spl42_167 | spl42_214 | ~ spl42_216)), inference(forward_demodulation, [], [f3602, f1941])).
fof(f3602, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), e21)) | (~ spl42_167 | spl42_214)), inference(forward_demodulation, [], [f1934, f1645])).
fof(f1934, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | spl42_214), inference(avatar_component_clause, [], [f1932])).
fof(f1932, plain, (spl42_214 <=> (h3(op1(e12, e11)) = op2(h3(e12), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_214])])).
fof(f3570, plain, (~ spl42_56 | ~ spl42_115 | ~ spl42_172 | spl42_209 | ~ spl42_216), inference(avatar_contradiction_clause, [], [f3569])).
fof(f3569, plain, ($false | (~ spl42_56 | ~ spl42_115 | ~ spl42_172 | spl42_209 | ~ spl42_216)), inference(subsumption_resolution, [], [f3568, f1159])).
fof(f1159, plain, ((e22 = op2(e20, e23)) | ~ spl42_115), inference(avatar_component_clause, [], [f1157])).
fof(f1157, plain, (spl42_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_115])])).
fof(f3568, plain, (~ (e22 = op2(e20, e23)) | (~ spl42_56 | ~ spl42_172 | spl42_209 | ~ spl42_216)), inference(forward_demodulation, [], [f3567, f536])).
fof(f3567, plain, (~ (op2(e20, e23) = h3(e13)) | (~ spl42_56 | ~ spl42_172 | spl42_209 | ~ spl42_216)), inference(forward_demodulation, [], [f3566, f876])).
fof(f3566, plain, (~ (op2(e20, e23) = h3(op1(e10, e12))) | (~ spl42_172 | spl42_209 | ~ spl42_216)), inference(forward_demodulation, [], [f3565, f1670])).
fof(f3565, plain, (~ (h3(op1(e10, e12)) = op2(h3(e10), e23)) | (spl42_209 | ~ spl42_216)), inference(forward_demodulation, [], [f1914, f1941])).
fof(f1914, plain, (~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | spl42_209), inference(avatar_component_clause, [], [f1912])).
fof(f1912, plain, (spl42_209 <=> (h3(op1(e10, e12)) = op2(h3(e10), h3(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_209])])).
fof(f3538, plain, (~ spl42_58 | ~ spl42_122 | ~ spl42_167 | ~ spl42_172 | spl42_208), inference(avatar_contradiction_clause, [], [f3537])).
fof(f3537, plain, ($false | (~ spl42_58 | ~ spl42_122 | ~ spl42_167 | ~ spl42_172 | spl42_208)), inference(subsumption_resolution, [], [f3536, f1189])).
fof(f3536, plain, (~ (e21 = op2(e20, e21)) | (~ spl42_58 | ~ spl42_167 | ~ spl42_172 | spl42_208)), inference(forward_demodulation, [], [f3535, f1645])).
fof(f3535, plain, (~ (op2(e20, e21) = h3(e11)) | (~ spl42_58 | ~ spl42_167 | ~ spl42_172 | spl42_208)), inference(forward_demodulation, [], [f3534, f885])).
fof(f885, plain, ((e11 = op1(e10, e11)) | ~ spl42_58), inference(avatar_component_clause, [], [f883])).
fof(f3534, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | (~ spl42_167 | ~ spl42_172 | spl42_208)), inference(forward_demodulation, [], [f3533, f1670])).
fof(f3533, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), e21)) | (~ spl42_167 | spl42_208)), inference(forward_demodulation, [], [f1910, f1645])).
fof(f1910, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | spl42_208), inference(avatar_component_clause, [], [f1908])).
fof(f3503, plain, (~ spl42_18 | spl42_221), inference(avatar_contradiction_clause, [], [f3502])).
fof(f3502, plain, ($false | (~ spl42_18 | spl42_221)), inference(trivial_inequality_removal, [], [f3501])).
fof(f3501, plain, (~ (h3(e11) = h3(e11)) | (~ spl42_18 | spl42_221)), inference(forward_demodulation, [], [f1962, f715])).
fof(f1962, plain, (~ (h3(e11) = h3(op1(e12, e13))) | spl42_221), inference(avatar_component_clause, [], [f1960])).
fof(f1960, plain, (spl42_221 <=> (h3(e11) = h3(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_221])])).
fof(f3471, plain, (~ spl42_11 | ~ spl42_92 | ~ spl42_167 | ~ spl42_216 | spl42_219), inference(avatar_contradiction_clause, [], [f3470])).
fof(f3470, plain, ($false | (~ spl42_11 | ~ spl42_92 | ~ spl42_167 | ~ spl42_216 | spl42_219)), inference(subsumption_resolution, [], [f3469, f1061])).
fof(f1061, plain, ((e23 = op2(e22, e21)) | ~ spl42_92), inference(avatar_component_clause, [], [f1059])).
fof(f1059, plain, (spl42_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_92])])).
fof(f3469, plain, (~ (e23 = op2(e22, e21)) | (~ spl42_11 | ~ spl42_167 | ~ spl42_216 | spl42_219)), inference(forward_demodulation, [], [f3468, f1941])).
fof(f3468, plain, (~ (op2(e22, e21) = h3(e12)) | (~ spl42_11 | ~ spl42_167 | spl42_219)), inference(forward_demodulation, [], [f3467, f685])).
fof(f685, plain, ((e12 = op1(e13, e11)) | ~ spl42_11), inference(avatar_component_clause, [], [f683])).
fof(f683, plain, (spl42_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_11])])).
fof(f3467, plain, (~ (op2(e22, e21) = h3(op1(e13, e11))) | (~ spl42_167 | spl42_219)), inference(forward_demodulation, [], [f1954, f1645])).
fof(f1954, plain, (~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | spl42_219), inference(avatar_component_clause, [], [f1952])).
fof(f1952, plain, (spl42_219 <=> (h3(op1(e13, e11)) = op2(e22, h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_219])])).
fof(f3445, plain, (~ spl42_6 | ~ spl42_82 | ~ spl42_167 | ~ spl42_216 | spl42_218), inference(avatar_contradiction_clause, [], [f3444])).
fof(f3444, plain, ($false | (~ spl42_6 | ~ spl42_82 | ~ spl42_167 | ~ spl42_216 | spl42_218)), inference(subsumption_resolution, [], [f3443, f1019])).
fof(f3443, plain, (~ (e21 = op2(e22, e23)) | (~ spl42_6 | ~ spl42_167 | ~ spl42_216 | spl42_218)), inference(forward_demodulation, [], [f3442, f1645])).
fof(f3442, plain, (~ (op2(e22, e23) = h3(e11)) | (~ spl42_6 | ~ spl42_216 | spl42_218)), inference(forward_demodulation, [], [f3441, f664])).
fof(f664, plain, ((e11 = op1(e13, e12)) | ~ spl42_6), inference(avatar_component_clause, [], [f662])).
fof(f662, plain, (spl42_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_6])])).
fof(f3441, plain, (~ (op2(e22, e23) = h3(op1(e13, e12))) | (~ spl42_216 | spl42_218)), inference(forward_demodulation, [], [f1950, f1941])).
fof(f1950, plain, (~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | spl42_218), inference(avatar_component_clause, [], [f1948])).
fof(f1948, plain, (spl42_218 <=> (h3(op1(e13, e12)) = op2(e22, h3(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_218])])).
fof(f3415, plain, (~ spl42_21 | ~ spl42_65 | ~ spl42_172 | spl42_215 | ~ spl42_216), inference(avatar_contradiction_clause, [], [f3414])).
fof(f3414, plain, ($false | (~ spl42_21 | ~ spl42_65 | ~ spl42_172 | spl42_215 | ~ spl42_216)), inference(subsumption_resolution, [], [f3413, f947])).
fof(f947, plain, ((e20 = op2(e23, e23)) | ~ spl42_65), inference(avatar_component_clause, [], [f945])).
fof(f945, plain, (spl42_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_65])])).
fof(f3413, plain, (~ (e20 = op2(e23, e23)) | (~ spl42_21 | ~ spl42_172 | spl42_215 | ~ spl42_216)), inference(forward_demodulation, [], [f3412, f1670])).
fof(f3412, plain, (~ (op2(e23, e23) = h3(e10)) | (~ spl42_21 | spl42_215 | ~ spl42_216)), inference(forward_demodulation, [], [f3411, f728])).
fof(f728, plain, ((e10 = op1(e12, e12)) | ~ spl42_21), inference(avatar_component_clause, [], [f726])).
fof(f3411, plain, (~ (op2(e23, e23) = h3(op1(e12, e12))) | (spl42_215 | ~ spl42_216)), inference(forward_demodulation, [], [f1938, f1941])).
fof(f1938, plain, (~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | spl42_215), inference(avatar_component_clause, [], [f1936])).
fof(f1936, plain, (spl42_215 <=> (h3(op1(e12, e12)) = op2(h3(e12), h3(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_215])])).
fof(f3378, plain, (~ spl42_39 | ~ spl42_100 | ~ spl42_167 | spl42_212 | ~ spl42_216), inference(avatar_contradiction_clause, [], [f3377])).
fof(f3377, plain, ($false | (~ spl42_39 | ~ spl42_100 | ~ spl42_167 | spl42_212 | ~ spl42_216)), inference(subsumption_resolution, [], [f3376, f1095])).
fof(f1095, plain, ((e23 = op2(e21, e23)) | ~ spl42_100), inference(avatar_component_clause, [], [f1093])).
fof(f1093, plain, (spl42_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_100])])).
fof(f3376, plain, (~ (e23 = op2(e21, e23)) | (~ spl42_39 | ~ spl42_167 | spl42_212 | ~ spl42_216)), inference(forward_demodulation, [], [f3375, f1941])).
fof(f3375, plain, (~ (op2(e21, e23) = h3(e12)) | (~ spl42_39 | ~ spl42_167 | spl42_212 | ~ spl42_216)), inference(forward_demodulation, [], [f3374, f804])).
fof(f3374, plain, (~ (op2(e21, e23) = h3(op1(e11, e12))) | (~ spl42_167 | spl42_212 | ~ spl42_216)), inference(forward_demodulation, [], [f3373, f1645])).
fof(f3373, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), e23)) | (spl42_212 | ~ spl42_216)), inference(forward_demodulation, [], [f1926, f1941])).
fof(f1926, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | spl42_212), inference(avatar_component_clause, [], [f1924])).
fof(f1924, plain, (spl42_212 <=> (h3(op1(e11, e12)) = op2(h3(e11), h3(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_212])])).
fof(f3346, plain, (~ spl42_46 | ~ spl42_110 | ~ spl42_167 | ~ spl42_172 | spl42_210), inference(avatar_contradiction_clause, [], [f3345])).
fof(f3345, plain, ($false | (~ spl42_46 | ~ spl42_110 | ~ spl42_167 | ~ spl42_172 | spl42_210)), inference(subsumption_resolution, [], [f3344, f1138])).
fof(f1138, plain, ((e21 = op2(e21, e20)) | ~ spl42_110), inference(avatar_component_clause, [], [f1136])).
fof(f1136, plain, (spl42_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_110])])).
fof(f3344, plain, (~ (e21 = op2(e21, e20)) | (~ spl42_46 | ~ spl42_167 | ~ spl42_172 | spl42_210)), inference(forward_demodulation, [], [f3343, f1645])).
fof(f3343, plain, (~ (op2(e21, e20) = h3(e11)) | (~ spl42_46 | ~ spl42_167 | ~ spl42_172 | spl42_210)), inference(forward_demodulation, [], [f3342, f834])).
fof(f834, plain, ((e11 = op1(e11, e10)) | ~ spl42_46), inference(avatar_component_clause, [], [f832])).
fof(f3342, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | (~ spl42_167 | ~ spl42_172 | spl42_210)), inference(forward_demodulation, [], [f3341, f1645])).
fof(f3341, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | (~ spl42_172 | spl42_210)), inference(forward_demodulation, [], [f1918, f1670])).
fof(f1918, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | spl42_210), inference(avatar_component_clause, [], [f1916])).
fof(f1916, plain, (spl42_210 <=> (h3(op1(e11, e10)) = op2(h3(e11), h3(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_210])])).
fof(f3321, plain, (~ spl42_41 | ~ spl42_105 | ~ spl42_167 | ~ spl42_172 | spl42_211), inference(avatar_contradiction_clause, [], [f3320])).
fof(f3320, plain, ($false | (~ spl42_41 | ~ spl42_105 | ~ spl42_167 | ~ spl42_172 | spl42_211)), inference(subsumption_resolution, [], [f3319, f1117])).
fof(f1117, plain, ((e20 = op2(e21, e21)) | ~ spl42_105), inference(avatar_component_clause, [], [f1115])).
fof(f1115, plain, (spl42_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_105])])).
fof(f3319, plain, (~ (e20 = op2(e21, e21)) | (~ spl42_41 | ~ spl42_167 | ~ spl42_172 | spl42_211)), inference(backward_demodulation, [], [f3265, f1645])).
fof(f3265, plain, (~ (e20 = op2(h3(e11), h3(e11))) | (~ spl42_41 | ~ spl42_172 | spl42_211)), inference(backward_demodulation, [], [f3104, f1670])).
fof(f3104, plain, (~ (h3(e10) = op2(h3(e11), h3(e11))) | (~ spl42_41 | spl42_211)), inference(forward_demodulation, [], [f1922, f813])).
fof(f813, plain, ((e10 = op1(e11, e11)) | ~ spl42_41), inference(avatar_component_clause, [], [f811])).
fof(f811, plain, (spl42_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_41])])).
fof(f1922, plain, (~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | spl42_211), inference(avatar_component_clause, [], [f1920])).
fof(f1920, plain, (spl42_211 <=> (h3(op1(e11, e11)) = op2(h3(e11), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_211])])).
fof(f3269, plain, (~ spl42_93 | ~ spl42_172), inference(avatar_split_clause, [], [f3261, f1669, f1064])).
fof(f1064, plain, (spl42_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_93])])).
fof(f3261, plain, (~ (e20 = op2(e22, e20)) | ~ spl42_172), inference(backward_demodulation, [], [f1580, f1670])).
fof(f1580, plain, ~ (op2(e22, e20) = h3(e10)), inference(backward_demodulation, [], [f289, f537])).
fof(f537, plain, (op2(e22, e22) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f289, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax6)).
fof(f3250, plain, (spl42_167 | ~ spl42_70 | ~ spl42_216), inference(avatar_split_clause, [], [f3249, f1940, f966, f1644])).
fof(f966, plain, (spl42_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_70])])).
fof(f3249, plain, ((e21 = h3(e11)) | (~ spl42_70 | ~ spl42_216)), inference(forward_demodulation, [], [f3248, f968])).
fof(f968, plain, ((e21 = op2(e23, e22)) | ~ spl42_70), inference(avatar_component_clause, [], [f966])).
fof(f3248, plain, ((op2(e23, e22) = h3(e11)) | ~ spl42_216), inference(backward_demodulation, [], [f1576, f1941])).
fof(f1576, plain, (h3(e11) = op2(h3(e12), e22)), inference(forward_demodulation, [], [f538, f539])).
fof(f539, plain, (op2(op2(e22, e22), e22) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f538, plain, (h3(e11) = op2(op2(op2(e22, e22), e22), e22)), inference(cnf_transformation, [], [f16])).
fof(f3238, plain, (~ spl42_188 | ~ spl42_110), inference(avatar_split_clause, [], [f3237, f1136, f1750])).
fof(f1750, plain, (spl42_188 <=> (e21 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_188])])).
fof(f3237, plain, (~ (e21 = h1(e10)) | ~ spl42_110), inference(forward_demodulation, [], [f1561, f1138])).
fof(f1561, plain, ~ (op2(e21, e20) = h1(e10)), inference(backward_demodulation, [], [f252, f529])).
fof(f529, plain, (op2(e20, e20) = h1(e10)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(op2(e20, e20), e20) = h1(e12)) & (h1(e11) = op2(op2(op2(e20, e20), e20), e20)) & (op2(e20, e20) = h1(e10)) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax14)).
fof(f252, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f3221, plain, (~ spl42_110 | ~ spl42_111), inference(avatar_contradiction_clause, [], [f3220])).
fof(f3220, plain, ($false | (~ spl42_110 | ~ spl42_111)), inference(subsumption_resolution, [], [f3219, f309])).
fof(f3219, plain, ((e21 = e22) | (~ spl42_110 | ~ spl42_111)), inference(backward_demodulation, [], [f1142, f1138])).
fof(f1142, plain, ((e22 = op2(e21, e20)) | ~ spl42_111), inference(avatar_component_clause, [], [f1140])).
fof(f1140, plain, (spl42_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_111])])).
fof(f3218, plain, (~ spl42_117 | ~ spl42_120), inference(avatar_contradiction_clause, [], [f3217])).
fof(f3217, plain, ($false | (~ spl42_117 | ~ spl42_120)), inference(subsumption_resolution, [], [f3215, f308])).
fof(f308, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f3215, plain, ((e20 = e23) | (~ spl42_117 | ~ spl42_120)), inference(backward_demodulation, [], [f1180, f1168])).
fof(f1168, plain, ((e20 = op2(e20, e22)) | ~ spl42_117), inference(avatar_component_clause, [], [f1166])).
fof(f1166, plain, (spl42_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_117])])).
fof(f1180, plain, ((e23 = op2(e20, e22)) | ~ spl42_120), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl42_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_120])])).
fof(f3205, plain, (~ spl42_127 | spl42_184), inference(avatar_contradiction_clause, [], [f3204])).
fof(f3204, plain, ($false | (~ spl42_127 | spl42_184)), inference(subsumption_resolution, [], [f3203, f1732])).
fof(f1732, plain, (~ (e22 = h1(e10)) | spl42_184), inference(avatar_component_clause, [], [f1730])).
fof(f1730, plain, (spl42_184 <=> (e22 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_184])])).
fof(f3203, plain, ((e22 = h1(e10)) | ~ spl42_127), inference(backward_demodulation, [], [f529, f1210])).
fof(f1210, plain, ((op2(e20, e20) = e22) | ~ spl42_127), inference(avatar_component_clause, [], [f1208])).
fof(f1208, plain, (spl42_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl42_127])])).
fof(f3180, plain, (~ spl42_102 | ~ spl42_70), inference(avatar_split_clause, [], [f3179, f966, f1102])).
fof(f1102, plain, (spl42_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_102])])).
fof(f3179, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_70), inference(forward_demodulation, [], [f268, f968])).
fof(f268, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3177, plain, (~ spl42_95 | ~ spl42_87), inference(avatar_split_clause, [], [f3176, f1038, f1072])).
fof(f1038, plain, (spl42_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_87])])).
fof(f3176, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_87), inference(forward_demodulation, [], [f1580, f3143])).
fof(f3143, plain, ((e22 = h3(e10)) | ~ spl42_87), inference(backward_demodulation, [], [f537, f1040])).
fof(f1040, plain, ((e22 = op2(e22, e22)) | ~ spl42_87), inference(avatar_component_clause, [], [f1038])).
fof(f3173, plain, (~ spl42_95 | ~ spl42_111), inference(avatar_split_clause, [], [f3172, f1140, f1072])).
fof(f3172, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_111), inference(forward_demodulation, [], [f255, f1142])).
fof(f255, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f3154, plain, (~ spl42_77 | ~ spl42_80), inference(avatar_contradiction_clause, [], [f3153])).
fof(f3153, plain, ($false | (~ spl42_77 | ~ spl42_80)), inference(subsumption_resolution, [], [f3151, f308])).
fof(f3151, plain, ((e20 = e23) | (~ spl42_77 | ~ spl42_80)), inference(backward_demodulation, [], [f1010, f998])).
fof(f998, plain, ((e20 = op2(e23, e20)) | ~ spl42_77), inference(avatar_component_clause, [], [f996])).
fof(f996, plain, (spl42_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_77])])).
fof(f3129, plain, (~ spl42_102 | ~ spl42_106), inference(avatar_split_clause, [], [f3123, f1119, f1102])).
fof(f1119, plain, (spl42_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_106])])).
fof(f3123, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_106), inference(backward_demodulation, [], [f1573, f3120])).
fof(f3120, plain, ((e21 = h2(e10)) | ~ spl42_106), inference(backward_demodulation, [], [f533, f1121])).
fof(f1121, plain, ((e21 = op2(e21, e21)) | ~ spl42_106), inference(avatar_component_clause, [], [f1119])).
fof(f533, plain, (op2(e21, e21) = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(op2(e21, e21), e21) = h2(e12)) & (h2(e11) = op2(op2(op2(e21, e21), e21), e21)) & (op2(e21, e21) = h2(e10)) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax15)).
fof(f1573, plain, ~ (op2(e21, e22) = h2(e10)), inference(backward_demodulation, [], [f285, f533])).
fof(f285, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3128, plain, (~ spl42_74 | ~ spl42_106), inference(avatar_split_clause, [], [f3122, f1119, f983])).
fof(f983, plain, (spl42_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_74])])).
fof(f3122, plain, (~ (e21 = op2(e23, e21)) | ~ spl42_106), inference(backward_demodulation, [], [f1571, f3120])).
fof(f1571, plain, ~ (op2(e23, e21) = h2(e10)), inference(backward_demodulation, [], [f262, f533])).
fof(f262, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3108, plain, (spl42_188 | ~ spl42_126), inference(avatar_split_clause, [], [f3107, f1204, f1750])).
fof(f1204, plain, (spl42_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl42_126])])).
fof(f3107, plain, ((e21 = h1(e10)) | ~ spl42_126), inference(backward_demodulation, [], [f529, f1206])).
fof(f1206, plain, ((op2(e20, e20) = e21) | ~ spl42_126), inference(avatar_component_clause, [], [f1204])).
fof(f3090, plain, (~ spl42_184 | ~ spl42_115), inference(avatar_split_clause, [], [f3089, f1157, f1730])).
fof(f3089, plain, (~ (e22 = h1(e10)) | ~ spl42_115), inference(forward_demodulation, [], [f1566, f1159])).
fof(f1566, plain, ~ (op2(e20, e23) = h1(e10)), inference(backward_demodulation, [], [f278, f529])).
fof(f278, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3080, plain, (~ spl42_69 | ~ spl42_164), inference(avatar_split_clause, [], [f3079, f1628, f962])).
fof(f962, plain, (spl42_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_69])])).
fof(f1628, plain, (spl42_164 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_164])])).
fof(f3079, plain, (~ (e20 = op2(e23, e22)) | ~ spl42_164), inference(forward_demodulation, [], [f1595, f1629])).
fof(f1629, plain, ((e20 = h4(e10)) | ~ spl42_164), inference(avatar_component_clause, [], [f1628])).
fof(f1595, plain, ~ (op2(e23, e22) = h4(e10)), inference(backward_demodulation, [], [f299, f541])).
fof(f541, plain, (op2(e23, e23) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(op2(e23, e23), e23) = h4(e12)) & (op2(op2(op2(e23, e23), e23), e23) = h4(e11)) & (op2(e23, e23) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax17)).
fof(f299, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3071, plain, (~ spl42_6 | ~ spl42_7), inference(avatar_contradiction_clause, [], [f3070])).
fof(f3070, plain, ($false | (~ spl42_6 | ~ spl42_7)), inference(subsumption_resolution, [], [f3069, f303])).
fof(f3069, plain, ((e11 = e12) | (~ spl42_6 | ~ spl42_7)), inference(backward_demodulation, [], [f668, f664])).
fof(f668, plain, ((e12 = op1(e13, e12)) | ~ spl42_7), inference(avatar_component_clause, [], [f666])).
fof(f666, plain, (spl42_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_7])])).
fof(f3063, plain, (~ spl42_10 | ~ spl42_58), inference(avatar_split_clause, [], [f3060, f883, f679])).
fof(f679, plain, (spl42_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_10])])).
fof(f3060, plain, (~ (e11 = op1(e13, e11)) | ~ spl42_58), inference(backward_demodulation, [], [f212, f885])).
fof(f212, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3045, plain, (spl42_216 | ~ spl42_120 | ~ spl42_172), inference(avatar_split_clause, [], [f3044, f1669, f1178, f1940])).
fof(f3044, plain, ((e23 = h3(e12)) | (~ spl42_120 | ~ spl42_172)), inference(backward_demodulation, [], [f2918, f1180])).
fof(f2918, plain, ((op2(e20, e22) = h3(e12)) | ~ spl42_172), inference(backward_demodulation, [], [f1583, f1670])).
fof(f1583, plain, (h3(e12) = op2(h3(e10), e22)), inference(backward_demodulation, [], [f539, f537])).
fof(f3024, plain, (~ spl42_51 | spl42_223), inference(avatar_contradiction_clause, [], [f3023])).
fof(f3023, plain, ($false | (~ spl42_51 | spl42_223)), inference(trivial_inequality_removal, [], [f3022])).
fof(f3022, plain, (~ (h3(e12) = h3(e12)) | (~ spl42_51 | spl42_223)), inference(forward_demodulation, [], [f1970, f855])).
fof(f1970, plain, (~ (h3(e12) = h3(op1(e10, e13))) | spl42_223), inference(avatar_component_clause, [], [f1968])).
fof(f1968, plain, (spl42_223 <=> (h3(e12) = h3(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_223])])).
fof(f3016, plain, (~ spl42_122 | ~ spl42_74), inference(avatar_split_clause, [], [f3015, f983, f1187])).
fof(f3015, plain, (~ (e21 = op2(e20, e21)) | ~ spl42_74), inference(forward_demodulation, [], [f260, f985])).
fof(f985, plain, ((e21 = op2(e23, e21)) | ~ spl42_74), inference(avatar_component_clause, [], [f983])).
fof(f260, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3013, plain, (~ spl42_123 | ~ spl42_115), inference(avatar_split_clause, [], [f3012, f1157, f1191])).
fof(f1191, plain, (spl42_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_123])])).
fof(f3012, plain, (~ (e22 = op2(e20, e21)) | ~ spl42_115), inference(forward_demodulation, [], [f280, f1159])).
fof(f280, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3009, plain, (~ spl42_119 | ~ spl42_115), inference(avatar_split_clause, [], [f3008, f1157, f1174])).
fof(f1174, plain, (spl42_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_119])])).
fof(f3008, plain, (~ (e22 = op2(e20, e22)) | ~ spl42_115), inference(forward_demodulation, [], [f281, f1159])).
fof(f281, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3006, plain, (~ spl42_110 | ~ spl42_102), inference(avatar_split_clause, [], [f3005, f1102, f1136])).
fof(f3005, plain, (~ (e21 = op2(e21, e20)) | ~ spl42_102), inference(forward_demodulation, [], [f283, f1104])).
fof(f1104, plain, ((e21 = op2(e21, e22)) | ~ spl42_102), inference(avatar_component_clause, [], [f1102])).
fof(f283, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3002, plain, (~ spl42_112 | ~ spl42_80), inference(avatar_split_clause, [], [f2885, f1008, f1144])).
fof(f1144, plain, (spl42_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_112])])).
fof(f2885, plain, (~ (e23 = op2(e21, e20)) | ~ spl42_80), inference(forward_demodulation, [], [f256, f1010])).
fof(f256, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2993, plain, (~ spl42_37 | ~ spl42_41), inference(avatar_split_clause, [], [f2992, f811, f794])).
fof(f794, plain, (spl42_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_37])])).
fof(f2992, plain, (~ (e10 = op1(e11, e12)) | ~ spl42_41), inference(forward_demodulation, [], [f237, f813])).
fof(f237, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2989, plain, (~ spl42_39 | ~ spl42_7), inference(avatar_split_clause, [], [f2988, f666, f802])).
fof(f2988, plain, (~ (e12 = op1(e11, e12)) | ~ spl42_7), inference(forward_demodulation, [], [f220, f668])).
fof(f220, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2987, plain, (~ spl42_40 | ~ spl42_36), inference(avatar_split_clause, [], [f2986, f789, f806])).
fof(f806, plain, (spl42_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_40])])).
fof(f2986, plain, (~ (e13 = op1(e11, e12)) | ~ spl42_36), inference(forward_demodulation, [], [f239, f791])).
fof(f239, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2985, plain, (~ spl42_38 | ~ spl42_46), inference(avatar_split_clause, [], [f2984, f832, f798])).
fof(f798, plain, (spl42_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_38])])).
fof(f2984, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_46), inference(forward_demodulation, [], [f235, f834])).
fof(f235, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2975, plain, (~ spl42_54 | ~ spl42_56), inference(avatar_contradiction_clause, [], [f2974])).
fof(f2974, plain, ($false | (~ spl42_54 | ~ spl42_56)), inference(subsumption_resolution, [], [f2973, f304])).
fof(f304, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2973, plain, ((e11 = e13) | (~ spl42_54 | ~ spl42_56)), inference(backward_demodulation, [], [f876, f868])).
fof(f868, plain, ((e11 = op1(e10, e12)) | ~ spl42_54), inference(avatar_component_clause, [], [f866])).
fof(f866, plain, (spl42_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_54])])).
fof(f2969, plain, (~ spl42_61 | ~ spl42_125 | ~ spl42_172 | spl42_207), inference(avatar_contradiction_clause, [], [f2968])).
fof(f2968, plain, ($false | (~ spl42_61 | ~ spl42_125 | ~ spl42_172 | spl42_207)), inference(subsumption_resolution, [], [f2963, f1670])).
fof(f2963, plain, (~ (e20 = h3(e10)) | (~ spl42_61 | ~ spl42_125 | ~ spl42_172 | spl42_207)), inference(backward_demodulation, [], [f2926, f898])).
fof(f898, plain, ((e10 = op1(e10, e10)) | ~ spl42_61), inference(avatar_component_clause, [], [f896])).
fof(f896, plain, (spl42_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_61])])).
fof(f2926, plain, (~ (e20 = h3(op1(e10, e10))) | (~ spl42_125 | ~ spl42_172 | spl42_207)), inference(forward_demodulation, [], [f2920, f1202])).
fof(f1202, plain, ((e20 = op2(e20, e20)) | ~ spl42_125), inference(avatar_component_clause, [], [f1200])).
fof(f1200, plain, (spl42_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_125])])).
fof(f2920, plain, (~ (op2(e20, e20) = h3(op1(e10, e10))) | (~ spl42_172 | spl42_207)), inference(backward_demodulation, [], [f1906, f1670])).
fof(f1906, plain, (~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10))) | spl42_207), inference(avatar_component_clause, [], [f1904])).
fof(f1904, plain, (spl42_207 <=> (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_207])])).
fof(f2965, plain, (~ spl42_57 | ~ spl42_61), inference(avatar_split_clause, [], [f2958, f896, f879])).
fof(f2958, plain, (~ (e10 = op1(e10, e11)) | ~ spl42_61), inference(backward_demodulation, [], [f228, f898])).
fof(f2950, plain, (~ spl42_94 | ~ spl42_95), inference(avatar_contradiction_clause, [], [f2949])).
fof(f2949, plain, ($false | (~ spl42_94 | ~ spl42_95)), inference(subsumption_resolution, [], [f2948, f309])).
fof(f2948, plain, ((e21 = e22) | (~ spl42_94 | ~ spl42_95)), inference(backward_demodulation, [], [f1074, f1070])).
fof(f1070, plain, ((e21 = op2(e22, e20)) | ~ spl42_94), inference(avatar_component_clause, [], [f1068])).
fof(f1068, plain, (spl42_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_94])])).
fof(f2947, plain, (~ spl42_95 | ~ spl42_96), inference(avatar_contradiction_clause, [], [f2946])).
fof(f2946, plain, ($false | (~ spl42_95 | ~ spl42_96)), inference(subsumption_resolution, [], [f2945, f311])).
fof(f311, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f2945, plain, ((e22 = e23) | (~ spl42_95 | ~ spl42_96)), inference(forward_demodulation, [], [f1078, f1074])).
fof(f1078, plain, ((e23 = op2(e22, e20)) | ~ spl42_96), inference(avatar_component_clause, [], [f1076])).
fof(f1076, plain, (spl42_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_96])])).
fof(f2937, plain, (~ spl42_100 | ~ spl42_104), inference(avatar_split_clause, [], [f2936, f1110, f1093])).
fof(f1110, plain, (spl42_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_104])])).
fof(f2936, plain, (~ (e23 = op2(e21, e23)) | ~ spl42_104), inference(backward_demodulation, [], [f287, f1112])).
fof(f1112, plain, ((e23 = op2(e21, e22)) | ~ spl42_104), inference(avatar_component_clause, [], [f1110])).
fof(f287, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2934, plain, (~ spl42_99 | ~ spl42_115), inference(avatar_split_clause, [], [f2933, f1157, f1089])).
fof(f1089, plain, (spl42_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_99])])).
fof(f2933, plain, (~ (e22 = op2(e21, e23)) | ~ spl42_115), inference(backward_demodulation, [], [f270, f1159])).
fof(f270, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2923, plain, (~ spl42_101 | ~ spl42_172), inference(avatar_split_clause, [], [f2917, f1669, f1098])).
fof(f1098, plain, (spl42_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_101])])).
fof(f2917, plain, (~ (e20 = op2(e21, e22)) | ~ spl42_172), inference(backward_demodulation, [], [f1578, f1670])).
fof(f1578, plain, ~ (op2(e21, e22) = h3(e10)), inference(backward_demodulation, [], [f267, f537])).
fof(f267, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2898, plain, (spl42_172 | ~ spl42_85), inference(avatar_split_clause, [], [f2790, f1030, f1669])).
fof(f1030, plain, (spl42_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_85])])).
fof(f2790, plain, ((e20 = h3(e10)) | ~ spl42_85), inference(backward_demodulation, [], [f537, f1032])).
fof(f1032, plain, ((e20 = op2(e22, e22)) | ~ spl42_85), inference(avatar_component_clause, [], [f1030])).
fof(f2891, plain, (spl42_115 | ~ spl42_164), inference(avatar_split_clause, [], [f2622, f1628, f1157])).
fof(f2622, plain, ((e22 = op2(e20, e23)) | ~ spl42_164), inference(backward_demodulation, [], [f1596, f1629])).
fof(f1596, plain, (e22 = op2(h4(e10), e23)), inference(backward_demodulation, [], [f527, f541])).
fof(f527, plain, (e22 = op2(op2(e23, e23), e23)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(op2(e23, e23), e23)) & (e21 = op2(op2(op2(e23, e23), e23), e23)) & (e20 = op2(e23, e23))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax13)).
fof(f2880, plain, (~ spl42_103 | ~ spl42_71), inference(avatar_split_clause, [], [f2879, f970, f1106])).
fof(f970, plain, (spl42_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_71])])).
fof(f2879, plain, (~ (e22 = op2(e21, e22)) | ~ spl42_71), inference(forward_demodulation, [], [f268, f972])).
fof(f972, plain, ((e22 = op2(e23, e22)) | ~ spl42_71), inference(avatar_component_clause, [], [f970])).
fof(f2876, plain, (~ spl42_98 | ~ spl42_160), inference(avatar_split_clause, [], [f2875, f1607, f1085])).
fof(f1085, plain, (spl42_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_98])])).
fof(f1607, plain, (spl42_160 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_160])])).
fof(f2875, plain, (~ (e21 = op2(e21, e23)) | ~ spl42_160), inference(forward_demodulation, [], [f1587, f1608])).
fof(f1608, plain, ((e21 = h4(e11)) | ~ spl42_160), inference(avatar_component_clause, [], [f1607])).
fof(f1587, plain, ~ (op2(e21, e23) = h4(e11)), inference(backward_demodulation, [], [f273, f1585])).
fof(f1585, plain, (op2(e22, e23) = h4(e11)), inference(forward_demodulation, [], [f542, f527])).
fof(f542, plain, (op2(op2(op2(e23, e23), e23), e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f273, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2874, plain, (~ spl42_97 | ~ spl42_164), inference(avatar_split_clause, [], [f2873, f1628, f1081])).
fof(f1081, plain, (spl42_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_97])])).
fof(f2873, plain, (~ (e20 = op2(e21, e23)) | ~ spl42_164), inference(forward_demodulation, [], [f1592, f1629])).
fof(f1592, plain, ~ (op2(e21, e23) = h4(e10)), inference(backward_demodulation, [], [f274, f541])).
fof(f274, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2865, plain, (~ spl42_90 | ~ spl42_160), inference(avatar_split_clause, [], [f2864, f1607, f1051])).
fof(f1051, plain, (spl42_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_90])])).
fof(f2864, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_160), inference(forward_demodulation, [], [f1590, f1608])).
fof(f1590, plain, ~ (op2(e22, e21) = h4(e11)), inference(backward_demodulation, [], [f292, f1585])).
fof(f292, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2856, plain, (~ spl42_60 | ~ spl42_56), inference(avatar_split_clause, [], [f2855, f874, f891])).
fof(f891, plain, (spl42_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_60])])).
fof(f2855, plain, (~ (e13 = op1(e10, e11)) | ~ spl42_56), inference(forward_demodulation, [], [f231, f876])).
fof(f231, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f2854, plain, (~ spl42_59 | ~ spl42_51), inference(avatar_split_clause, [], [f2853, f853, f887])).
fof(f887, plain, (spl42_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_59])])).
fof(f2853, plain, (~ (e12 = op1(e10, e11)) | ~ spl42_51), inference(forward_demodulation, [], [f232, f855])).
fof(f232, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2852, plain, (~ spl42_47 | ~ spl42_31), inference(avatar_split_clause, [], [f2851, f768, f836])).
fof(f836, plain, (spl42_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_47])])).
fof(f2851, plain, (~ (e12 = op1(e11, e10)) | ~ spl42_31), inference(forward_demodulation, [], [f207, f770])).
fof(f207, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2850, plain, (~ spl42_48 | ~ spl42_16), inference(avatar_split_clause, [], [f2849, f704, f840])).
fof(f840, plain, (spl42_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_48])])).
fof(f2849, plain, (~ (e13 = op1(e11, e10)) | ~ spl42_16), inference(forward_demodulation, [], [f208, f706])).
fof(f208, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2848, plain, (~ spl42_47 | ~ spl42_43), inference(avatar_split_clause, [], [f2847, f819, f836])).
fof(f819, plain, (spl42_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_43])])).
fof(f2847, plain, (~ (e12 = op1(e11, e10)) | ~ spl42_43), inference(forward_demodulation, [], [f234, f821])).
fof(f821, plain, ((e12 = op1(e11, e11)) | ~ spl42_43), inference(avatar_component_clause, [], [f819])).
fof(f234, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2842, plain, (~ spl42_37 | ~ spl42_21), inference(avatar_split_clause, [], [f2735, f726, f794])).
fof(f2735, plain, (~ (e10 = op1(e11, e12)) | ~ spl42_21), inference(forward_demodulation, [], [f219, f728])).
fof(f2828, plain, (~ spl42_42 | ~ spl42_43), inference(avatar_contradiction_clause, [], [f2827])).
fof(f2827, plain, ($false | (~ spl42_42 | ~ spl42_43)), inference(subsumption_resolution, [], [f2826, f303])).
fof(f2826, plain, ((e11 = e12) | (~ spl42_42 | ~ spl42_43)), inference(backward_demodulation, [], [f821, f817])).
fof(f817, plain, ((e11 = op1(e11, e11)) | ~ spl42_42), inference(avatar_component_clause, [], [f815])).
fof(f815, plain, (spl42_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_42])])).
fof(f2818, plain, (~ spl42_35 | ~ spl42_51), inference(avatar_split_clause, [], [f2817, f853, f785])).
fof(f785, plain, (spl42_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_35])])).
fof(f2817, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f222, f855])).
fof(f222, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2784, plain, (~ spl42_105 | spl42_180), inference(avatar_contradiction_clause, [], [f2783])).
fof(f2783, plain, ($false | (~ spl42_105 | spl42_180)), inference(subsumption_resolution, [], [f2781, f1712])).
fof(f1712, plain, (~ (e20 = h2(e10)) | spl42_180), inference(avatar_component_clause, [], [f1710])).
fof(f1710, plain, (spl42_180 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_180])])).
fof(f2781, plain, ((e20 = h2(e10)) | ~ spl42_105), inference(backward_demodulation, [], [f533, f1117])).
fof(f2774, plain, (~ spl42_118 | ~ spl42_120), inference(avatar_contradiction_clause, [], [f2773])).
fof(f2773, plain, ($false | (~ spl42_118 | ~ spl42_120)), inference(subsumption_resolution, [], [f2772, f310])).
fof(f310, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f2772, plain, ((e21 = e23) | (~ spl42_118 | ~ spl42_120)), inference(backward_demodulation, [], [f1180, f1172])).
fof(f1172, plain, ((e21 = op2(e20, e22)) | ~ spl42_118), inference(avatar_component_clause, [], [f1170])).
fof(f1170, plain, (spl42_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_118])])).
fof(f2747, plain, (~ spl42_124 | ~ spl42_92), inference(avatar_split_clause, [], [f2746, f1059, f1195])).
fof(f1195, plain, (spl42_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_124])])).
fof(f2746, plain, (~ (e23 = op2(e20, e21)) | ~ spl42_92), inference(forward_demodulation, [], [f259, f1061])).
fof(f259, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2738, plain, (~ spl42_49 | ~ spl42_1), inference(avatar_split_clause, [], [f2737, f641, f845])).
fof(f845, plain, (spl42_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_49])])).
fof(f2737, plain, (~ (e10 = op1(e10, e13)) | ~ spl42_1), inference(forward_demodulation, [], [f224, f643])).
fof(f224, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2736, plain, (~ spl42_42 | ~ spl42_10), inference(avatar_split_clause, [], [f2554, f679, f815])).
fof(f2554, plain, (~ (e11 = op1(e11, e11)) | ~ spl42_10), inference(forward_demodulation, [], [f214, f681])).
fof(f681, plain, ((e11 = op1(e13, e11)) | ~ spl42_10), inference(avatar_component_clause, [], [f679])).
fof(f214, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2718, plain, (~ spl42_5 | ~ spl42_1), inference(avatar_split_clause, [], [f2717, f641, f658])).
fof(f658, plain, (spl42_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_5])])).
fof(f2717, plain, (~ (e10 = op1(e13, e12)) | ~ spl42_1), inference(forward_demodulation, [], [f251, f643])).
fof(f251, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2711, plain, (~ spl42_10 | ~ spl42_11), inference(avatar_contradiction_clause, [], [f2710])).
fof(f2710, plain, ($false | (~ spl42_10 | ~ spl42_11)), inference(subsumption_resolution, [], [f2709, f303])).
fof(f2709, plain, ((e11 = e12) | (~ spl42_10 | ~ spl42_11)), inference(forward_demodulation, [], [f685, f681])).
fof(f2704, plain, (~ spl42_21 | ~ spl42_23), inference(avatar_contradiction_clause, [], [f2703])).
fof(f2703, plain, ($false | (~ spl42_21 | ~ spl42_23)), inference(subsumption_resolution, [], [f2702, f301])).
fof(f301, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2702, plain, ((e10 = e12) | (~ spl42_21 | ~ spl42_23)), inference(backward_demodulation, [], [f736, f728])).
fof(f736, plain, ((e12 = op1(e12, e12)) | ~ spl42_23), inference(avatar_component_clause, [], [f734])).
fof(f2697, plain, (~ spl42_28 | ~ spl42_32), inference(avatar_split_clause, [], [f2696, f772, f755])).
fof(f772, plain, (spl42_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_32])])).
fof(f2696, plain, (~ (e13 = op1(e12, e11)) | ~ spl42_32), inference(backward_demodulation, [], [f240, f774])).
fof(f774, plain, ((e13 = op1(e12, e10)) | ~ spl42_32), inference(avatar_component_clause, [], [f772])).
fof(f240, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2661, plain, (spl42_168 | ~ spl42_86), inference(avatar_split_clause, [], [f2660, f1034, f1649])).
fof(f1649, plain, (spl42_168 <=> (e21 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_168])])).
fof(f1034, plain, (spl42_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_86])])).
fof(f2660, plain, ((e21 = h3(e10)) | ~ spl42_86), inference(backward_demodulation, [], [f537, f1036])).
fof(f1036, plain, ((e21 = op2(e22, e22)) | ~ spl42_86), inference(avatar_component_clause, [], [f1034])).
fof(f2648, plain, (~ spl42_101 | ~ spl42_102), inference(avatar_contradiction_clause, [], [f2647])).
fof(f2647, plain, ($false | (~ spl42_101 | ~ spl42_102)), inference(subsumption_resolution, [], [f2646, f306])).
fof(f2646, plain, ((e20 = e21) | (~ spl42_101 | ~ spl42_102)), inference(forward_demodulation, [], [f1104, f1100])).
fof(f1100, plain, ((e20 = op2(e21, e22)) | ~ spl42_101), inference(avatar_component_clause, [], [f1098])).
fof(f2629, plain, (~ spl42_114 | ~ spl42_164), inference(avatar_contradiction_clause, [], [f2628])).
fof(f2628, plain, ($false | (~ spl42_114 | ~ spl42_164)), inference(subsumption_resolution, [], [f2627, f309])).
fof(f2627, plain, ((e21 = e22) | (~ spl42_114 | ~ spl42_164)), inference(forward_demodulation, [], [f2622, f1155])).
fof(f1155, plain, ((e21 = op2(e20, e23)) | ~ spl42_114), inference(avatar_component_clause, [], [f1153])).
fof(f1153, plain, (spl42_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_114])])).
fof(f2584, plain, (~ spl42_168 | ~ spl42_82), inference(avatar_split_clause, [], [f2583, f1017, f1649])).
fof(f2583, plain, (~ (e21 = h3(e10)) | ~ spl42_82), inference(forward_demodulation, [], [f1582, f1019])).
fof(f1582, plain, ~ (op2(e22, e23) = h3(e10)), inference(backward_demodulation, [], [f293, f537])).
fof(f293, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2582, plain, (spl42_160 | ~ spl42_82), inference(avatar_split_clause, [], [f2581, f1017, f1607])).
fof(f2581, plain, ((e21 = h4(e11)) | ~ spl42_82), inference(forward_demodulation, [], [f1585, f1019])).
fof(f2580, plain, (~ spl42_78 | ~ spl42_80), inference(avatar_contradiction_clause, [], [f2579])).
fof(f2579, plain, ($false | (~ spl42_78 | ~ spl42_80)), inference(subsumption_resolution, [], [f2578, f310])).
fof(f2578, plain, ((e21 = e23) | (~ spl42_78 | ~ spl42_80)), inference(forward_demodulation, [], [f1010, f1002])).
fof(f1002, plain, ((e21 = op2(e23, e20)) | ~ spl42_78), inference(avatar_component_clause, [], [f1000])).
fof(f1000, plain, (spl42_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_78])])).
fof(f2555, plain, (spl42_51 | ~ spl42_1), inference(avatar_split_clause, [], [f2519, f641, f853])).
fof(f2519, plain, ((e12 = op1(e10, e13)) | ~ spl42_1), inference(backward_demodulation, [], [f524, f643])).
fof(f524, plain, (e12 = op1(op1(e13, e13), e13)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(op1(e13, e13), e13)) & (e11 = op1(op1(op1(e13, e13), e13), e13)) & (e10 = op1(e13, e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax12)).
fof(f2551, plain, (~ spl42_34 | ~ spl42_18), inference(avatar_split_clause, [], [f2550, f713, f781])).
fof(f781, plain, (spl42_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_34])])).
fof(f2550, plain, (~ (e11 = op1(e11, e13)) | ~ spl42_18), inference(forward_demodulation, [], [f225, f715])).
fof(f225, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2545, plain, (~ spl42_31 | ~ spl42_23), inference(avatar_split_clause, [], [f2544, f734, f768])).
fof(f2544, plain, (~ (e12 = op1(e12, e10)) | ~ spl42_23), inference(forward_demodulation, [], [f241, f736])).
fof(f2543, plain, (~ spl42_30 | ~ spl42_18), inference(avatar_split_clause, [], [f2542, f713, f764])).
fof(f764, plain, (spl42_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_30])])).
fof(f2542, plain, (~ (e11 = op1(e12, e10)) | ~ spl42_18), inference(forward_demodulation, [], [f242, f715])).
fof(f242, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2537, plain, (~ spl42_26 | ~ spl42_18), inference(avatar_split_clause, [], [f2536, f713, f747])).
fof(f747, plain, (spl42_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_26])])).
fof(f2536, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_18), inference(forward_demodulation, [], [f244, f715])).
fof(f244, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2533, plain, (~ spl42_14 | ~ spl42_16), inference(avatar_contradiction_clause, [], [f2532])).
fof(f2532, plain, ($false | (~ spl42_14 | ~ spl42_16)), inference(subsumption_resolution, [], [f2531, f304])).
fof(f2531, plain, ((e11 = e13) | (~ spl42_14 | ~ spl42_16)), inference(forward_demodulation, [], [f706, f698])).
fof(f698, plain, ((e11 = op1(e13, e10)) | ~ spl42_14), inference(avatar_component_clause, [], [f696])).
fof(f696, plain, (spl42_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_14])])).
fof(f2527, plain, (~ spl42_9 | ~ spl42_1), inference(avatar_split_clause, [], [f2526, f641, f675])).
fof(f675, plain, (spl42_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_9])])).
fof(f2526, plain, (~ (e10 = op1(e13, e11)) | ~ spl42_1), inference(forward_demodulation, [], [f250, f643])).
fof(f250, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2525, plain, (~ spl42_1 | ~ spl42_50), inference(avatar_contradiction_clause, [], [f2524])).
fof(f2524, plain, ($false | (~ spl42_1 | ~ spl42_50)), inference(subsumption_resolution, [], [f2523, f303])).
fof(f2523, plain, ((e11 = e12) | (~ spl42_1 | ~ spl42_50)), inference(forward_demodulation, [], [f2519, f851])).
fof(f851, plain, ((e11 = op1(e10, e13)) | ~ spl42_50), inference(avatar_component_clause, [], [f849])).
fof(f849, plain, (spl42_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_50])])).
fof(f2506, plain, (~ spl42_15 | ~ spl42_16), inference(avatar_contradiction_clause, [], [f2505])).
fof(f2505, plain, ($false | (~ spl42_15 | ~ spl42_16)), inference(subsumption_resolution, [], [f2504, f305])).
fof(f305, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2504, plain, ((e12 = e13) | (~ spl42_15 | ~ spl42_16)), inference(backward_demodulation, [], [f706, f702])).
fof(f702, plain, ((e12 = op1(e13, e10)) | ~ spl42_15), inference(avatar_component_clause, [], [f700])).
fof(f700, plain, (spl42_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_15])])).
fof(f2503, plain, (~ spl42_4 | ~ spl42_16), inference(avatar_split_clause, [], [f2500, f704, f653])).
fof(f653, plain, (spl42_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_4])])).
fof(f2500, plain, (~ (e13 = op1(e13, e13)) | ~ spl42_16), inference(backward_demodulation, [], [f248, f706])).
fof(f248, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2502, plain, (~ spl42_8 | ~ spl42_16), inference(avatar_split_clause, [], [f2499, f704, f670])).
fof(f670, plain, (spl42_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_8])])).
fof(f2499, plain, (~ (e13 = op1(e13, e12)) | ~ spl42_16), inference(backward_demodulation, [], [f247, f706])).
fof(f247, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2501, plain, (~ spl42_12 | ~ spl42_16), inference(avatar_split_clause, [], [f2498, f704, f687])).
fof(f687, plain, (spl42_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_12])])).
fof(f2498, plain, (~ (e13 = op1(e13, e11)) | ~ spl42_16), inference(backward_demodulation, [], [f246, f706])).
fof(f246, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2497, plain, (~ spl42_18 | ~ spl42_20), inference(avatar_contradiction_clause, [], [f2496])).
fof(f2496, plain, ($false | (~ spl42_18 | ~ spl42_20)), inference(subsumption_resolution, [], [f2495, f304])).
fof(f2495, plain, ((e11 = e13) | (~ spl42_18 | ~ spl42_20)), inference(backward_demodulation, [], [f723, f715])).
fof(f723, plain, ((e13 = op1(e12, e13)) | ~ spl42_20), inference(avatar_component_clause, [], [f721])).
fof(f721, plain, (spl42_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_20])])).
fof(f2489, plain, (~ spl42_7 | ~ spl42_23), inference(avatar_split_clause, [], [f2485, f734, f666])).
fof(f2485, plain, (~ (e12 = op1(e13, e12)) | ~ spl42_23), inference(backward_demodulation, [], [f221, f736])).
fof(f221, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2483, plain, (~ spl42_21 | ~ spl42_25), inference(avatar_split_clause, [], [f2480, f743, f726])).
fof(f743, plain, (spl42_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_25])])).
fof(f2480, plain, (~ (e10 = op1(e12, e12)) | ~ spl42_25), inference(backward_demodulation, [], [f243, f745])).
fof(f745, plain, ((e10 = op1(e12, e11)) | ~ spl42_25), inference(avatar_component_clause, [], [f743])).
fof(f243, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2475, plain, (~ spl42_13 | ~ spl42_29), inference(avatar_split_clause, [], [f2471, f760, f692])).
fof(f692, plain, (spl42_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_13])])).
fof(f2471, plain, (~ (e10 = op1(e13, e10)) | ~ spl42_29), inference(backward_demodulation, [], [f209, f762])).
fof(f209, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2444, plain, (~ spl42_37 | ~ spl42_45), inference(avatar_split_clause, [], [f2439, f828, f794])).
fof(f828, plain, (spl42_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_45])])).
fof(f2439, plain, (~ (e10 = op1(e11, e12)) | ~ spl42_45), inference(backward_demodulation, [], [f235, f830])).
fof(f830, plain, ((e10 = op1(e11, e10)) | ~ spl42_45), inference(avatar_component_clause, [], [f828])).
fof(f2442, plain, (~ spl42_13 | ~ spl42_45), inference(avatar_split_clause, [], [f2437, f828, f692])).
fof(f2437, plain, (~ (e10 = op1(e13, e10)) | ~ spl42_45), inference(backward_demodulation, [], [f208, f830])).
fof(f2432, plain, (~ spl42_51 | ~ spl42_52), inference(avatar_contradiction_clause, [], [f2431])).
fof(f2431, plain, ($false | (~ spl42_51 | ~ spl42_52)), inference(subsumption_resolution, [], [f2430, f305])).
fof(f2430, plain, ((e12 = e13) | (~ spl42_51 | ~ spl42_52)), inference(backward_demodulation, [], [f859, f855])).
fof(f859, plain, ((e13 = op1(e10, e13)) | ~ spl42_52), inference(avatar_component_clause, [], [f857])).
fof(f857, plain, (spl42_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_52])])).
fof(f2386, plain, (spl42_164 | ~ spl42_65), inference(avatar_split_clause, [], [f2385, f945, f1628])).
fof(f2385, plain, ((e20 = h4(e10)) | ~ spl42_65), inference(backward_demodulation, [], [f541, f947])).
fof(f2381, plain, (~ spl42_70 | ~ spl42_71), inference(avatar_contradiction_clause, [], [f2380])).
fof(f2380, plain, ($false | (~ spl42_70 | ~ spl42_71)), inference(subsumption_resolution, [], [f2379, f309])).
fof(f2379, plain, ((e21 = e22) | (~ spl42_70 | ~ spl42_71)), inference(backward_demodulation, [], [f972, f968])).
fof(f2363, plain, (~ spl42_79 | ~ spl42_80), inference(avatar_contradiction_clause, [], [f2362])).
fof(f2362, plain, ($false | (~ spl42_79 | ~ spl42_80)), inference(subsumption_resolution, [], [f2361, f311])).
fof(f2361, plain, ((e22 = e23) | (~ spl42_79 | ~ spl42_80)), inference(backward_demodulation, [], [f1010, f1006])).
fof(f1006, plain, ((e22 = op2(e23, e20)) | ~ spl42_79), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl42_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_79])])).
fof(f2360, plain, (~ spl42_72 | ~ spl42_80), inference(avatar_split_clause, [], [f2357, f1008, f974])).
fof(f974, plain, (spl42_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_72])])).
fof(f2357, plain, (~ (e23 = op2(e23, e22)) | ~ spl42_80), inference(backward_demodulation, [], [f295, f1010])).
fof(f295, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2359, plain, (~ spl42_76 | ~ spl42_80), inference(avatar_split_clause, [], [f2356, f1008, f991])).
fof(f991, plain, (spl42_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_76])])).
fof(f2356, plain, (~ (e23 = op2(e23, e21)) | ~ spl42_80), inference(backward_demodulation, [], [f294, f1010])).
fof(f294, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2354, plain, (~ spl42_82 | ~ spl42_84), inference(avatar_contradiction_clause, [], [f2353])).
fof(f2353, plain, ($false | (~ spl42_82 | ~ spl42_84)), inference(subsumption_resolution, [], [f2352, f310])).
fof(f2352, plain, ((e21 = e23) | (~ spl42_82 | ~ spl42_84)), inference(backward_demodulation, [], [f1027, f1019])).
fof(f1027, plain, ((e23 = op2(e22, e23)) | ~ spl42_84), inference(avatar_component_clause, [], [f1025])).
fof(f1025, plain, (spl42_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_84])])).
fof(f2289, plain, (~ spl42_180 | ~ spl42_109), inference(avatar_split_clause, [], [f2284, f1132, f1710])).
fof(f1132, plain, (spl42_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_109])])).
fof(f2284, plain, (~ (e20 = h2(e10)) | ~ spl42_109), inference(backward_demodulation, [], [f1572, f1134])).
fof(f1134, plain, ((e20 = op2(e21, e20)) | ~ spl42_109), inference(avatar_component_clause, [], [f1132])).
fof(f1572, plain, ~ (op2(e21, e20) = h2(e10)), inference(backward_demodulation, [], [f282, f533])).
fof(f282, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2280, plain, (~ spl42_115 | ~ spl42_116), inference(avatar_contradiction_clause, [], [f2279])).
fof(f2279, plain, ($false | (~ spl42_115 | ~ spl42_116)), inference(subsumption_resolution, [], [f2278, f311])).
fof(f2278, plain, ((e22 = e23) | (~ spl42_115 | ~ spl42_116)), inference(backward_demodulation, [], [f1163, f1159])).
fof(f1163, plain, ((e23 = op2(e20, e23)) | ~ spl42_116), inference(avatar_component_clause, [], [f1161])).
fof(f1161, plain, (spl42_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_116])])).
fof(f2243, plain, (~ spl42_109 | ~ spl42_125), inference(avatar_split_clause, [], [f2236, f1200, f1132])).
fof(f2236, plain, (~ (e20 = op2(e21, e20)) | ~ spl42_125), inference(backward_demodulation, [], [f1561, f2235])).
fof(f2235, plain, ((e20 = h1(e10)) | ~ spl42_125), inference(backward_demodulation, [], [f529, f1202])).
fof(f1971, plain, (~ spl42_207 | ~ spl42_208 | ~ spl42_209 | ~ spl42_210 | ~ spl42_211 | ~ spl42_212 | ~ spl42_213 | ~ spl42_214 | ~ spl42_215 | spl42_169 | spl42_165 | ~ spl42_216 | ~ spl42_217 | ~ spl42_218 | ~ spl42_219 | ~ spl42_220 | ~ spl42_221 | ~ spl42_222 | ~ spl42_223), inference(avatar_split_clause, [], [f1902, f1968, f1964, f1960, f1956, f1952, f1948, f1944, f1940, f1635, f1655, f1936, f1932, f1928, f1924, f1920, f1916, f1912, f1908, f1904])).
fof(f1655, plain, (spl42_169 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl42_169])])).
fof(f1635, plain, (spl42_165 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl42_165])])).
fof(f1902, plain, (~ (h3(e12) = h3(op1(e10, e13))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | ~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1901, f1583])).
fof(f1901, plain, (~ (h3(op1(e10, e13)) = op2(h3(e10), e22)) | ~ (h3(op1(e11, e13)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | ~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1900, f536])).
fof(f1900, plain, (~ (h3(op1(e11, e13)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | ~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1899, f536])).
fof(f1899, plain, (~ (h3(e11) = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | ~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1898, f1576])).
fof(f1898, plain, (~ (h3(op1(e12, e13)) = op2(h3(e12), e22)) | ~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | ~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1897, f536])).
fof(f1897, plain, (~ (h3(op1(e13, e10)) = op2(e22, h3(e10))) | ~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1896, f536])).
fof(f1896, plain, (~ (h3(op1(e13, e11)) = op2(e22, h3(e11))) | ~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1895, f536])).
fof(f1895, plain, (~ (h3(op1(e13, e12)) = op2(e22, h3(e12))) | ~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1894, f536])).
fof(f1894, plain, (~ (h3(e10) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1893, f537])).
fof(f1893, plain, (~ (op2(e22, e22) = h3(op1(e13, e13))) | ~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1892, f536])).
fof(f1892, plain, (~ (e23 = h3(e12)) | sP37 | sP36 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(subsumption_resolution, [], [f602, f1632])).
fof(f1632, plain, ~ sP38, inference(subsumption_resolution, [], [f559, f536])).
fof(f559, plain, (~ (e22 = h3(e13)) | ~ sP38), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP38), inference(nnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP38), inference(usedef, [], [e61])).
fof(e61, plain, (sP38 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f602, plain, (~ (e23 = h3(e12)) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(cnf_transformation, [], [f65])).
fof(f65, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP41 | sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP35 | sP34 | sP33 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP32 | sP31 | sP30 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53])).
fof(f53, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP30), inference(usedef, [], [e53])).
fof(e53, plain, (sP30 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f54, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP31), inference(usedef, [], [e54])).
fof(e54, plain, (sP31 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f55, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP32), inference(usedef, [], [e55])).
fof(e55, plain, (sP32 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f56, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP33), inference(usedef, [], [e56])).
fof(e56, plain, (sP33 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f57, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP34), inference(usedef, [], [e57])).
fof(e57, plain, (sP34 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f58, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP35), inference(usedef, [], [e58])).
fof(e58, plain, (sP35 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f59, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP36), inference(usedef, [], [e59])).
fof(e59, plain, (sP36 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f60, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP37), inference(usedef, [], [e60])).
fof(e60, plain, (sP37 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f62, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP39), inference(usedef, [], [e62])).
fof(e62, plain, (sP39 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f63, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP40), inference(usedef, [], [e63])).
fof(e63, plain, (sP40 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f64, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP41), inference(usedef, [], [e64])).
fof(e64, plain, (sP41 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', co1)).
fof(f1672, plain, (~ spl42_169 | ~ spl42_172), inference(avatar_split_clause, [], [f564, f1669, f1655])).
fof(f564, plain, (~ (e20 = h3(e10)) | ~ sP36), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP36), inference(nnf_transformation, [], [f59])).
fof(f1647, plain, (~ spl42_165 | ~ spl42_167), inference(avatar_split_clause, [], [f561, f1644, f1635])).
fof(f561, plain, (~ (e21 = h3(e11)) | ~ sP37), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP37), inference(nnf_transformation, [], [f60])).
fof(f1559, plain, spl42_65, inference(avatar_split_clause, [], [f525, f945])).
fof(f525, plain, (e20 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1558, plain, spl42_82, inference(avatar_split_clause, [], [f1557, f1017])).
fof(f1557, plain, (e21 = op2(e22, e23)), inference(forward_demodulation, [], [f526, f527])).
fof(f526, plain, (e21 = op2(op2(op2(e23, e23), e23), e23)), inference(cnf_transformation, [], [f13])).
fof(f1556, plain, spl42_1, inference(avatar_split_clause, [], [f522, f641])).
fof(f522, plain, (e10 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1555, plain, spl42_18, inference(avatar_split_clause, [], [f1554, f713])).
fof(f1554, plain, (e11 = op1(e12, e13)), inference(forward_demodulation, [], [f523, f524])).
fof(f523, plain, (e11 = op1(op1(op1(e13, e13), e13), e13)), inference(cnf_transformation, [], [f12])).
fof(f1553, plain, (spl42_158 | spl42_157 | spl42_156 | spl42_155 | spl42_154 | spl42_153 | spl42_152 | spl42_151 | spl42_150 | spl42_149 | spl42_148 | spl42_147 | spl42_146 | spl42_145 | spl42_144 | spl42_68), inference(avatar_split_clause, [], [f500, f957, f1402, f1411, f1420, f1429, f1438, f1447, f1456, f1465, f1474, f1483, f1492, f1501, f1510, f1519, f1528])).
fof(f1528, plain, (spl42_158 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl42_158])])).
fof(f1519, plain, (spl42_157 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl42_157])])).
fof(f1510, plain, (spl42_156 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl42_156])])).
fof(f1501, plain, (spl42_155 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl42_155])])).
fof(f1492, plain, (spl42_154 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl42_154])])).
fof(f1483, plain, (spl42_153 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl42_153])])).
fof(f1474, plain, (spl42_152 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl42_152])])).
fof(f1465, plain, (spl42_151 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl42_151])])).
fof(f1456, plain, (spl42_150 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl42_150])])).
fof(f1447, plain, (spl42_149 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl42_149])])).
fof(f1438, plain, (spl42_148 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl42_148])])).
fof(f1429, plain, (spl42_147 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl42_147])])).
fof(f1420, plain, (spl42_146 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl42_146])])).
fof(f1411, plain, (spl42_145 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl42_145])])).
fof(f1402, plain, (spl42_144 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl42_144])])).
fof(f957, plain, (spl42_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_68])])).
fof(f500, plain, ((e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & ((e23 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e20))) & (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15)), inference(definition_folding, [], [f11, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37])).
fof(f37, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP15), inference(usedef, [], [e37])).
fof(e37, plain, (sP15 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f38, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e20, e21))) | ~ sP16), inference(usedef, [], [e38])).
fof(e38, plain, (sP16 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f39, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e20, e22))) | ~ sP17), inference(usedef, [], [e39])).
fof(e39, plain, (sP17 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f40, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, e23))) | ~ sP18), inference(usedef, [], [e40])).
fof(e40, plain, (sP18 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f41, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e21, e20))) | ~ sP19), inference(usedef, [], [e41])).
fof(e41, plain, (sP19 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f42, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(usedef, [], [e42])).
fof(e42, plain, (sP20 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f43, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e21, e22))) | ~ sP21), inference(usedef, [], [e43])).
fof(e43, plain, (sP21 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f44, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e21, e23))) | ~ sP22), inference(usedef, [], [e44])).
fof(e44, plain, (sP22 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f45, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e22, e20))) | ~ sP23), inference(usedef, [], [e45])).
fof(e45, plain, (sP23 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f46, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e22, e21))) | ~ sP24), inference(usedef, [], [e46])).
fof(e46, plain, (sP24 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f47, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(usedef, [], [e47])).
fof(e47, plain, (sP25 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f48, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e22, e23))) | ~ sP26), inference(usedef, [], [e48])).
fof(e48, plain, (sP26 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f49, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e23, e20))) | ~ sP27), inference(usedef, [], [e49])).
fof(e49, plain, (sP27 <=> ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f50, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e23, e21))) | ~ sP28), inference(usedef, [], [e50])).
fof(e50, plain, (sP28 <=> ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f51, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e23, e22))) | ~ sP29), inference(usedef, [], [e51])).
fof(e51, plain, (sP29 <=> ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f11, plain, (((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & ((e23 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e20))) & (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e23, e22))) | ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e23, e21))) | ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e23, e20))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e22, e23))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e22, e21))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e22, e20))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e21, e23))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e21, e22))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e21, e20))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, e23))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e20, e22))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e20, e21))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax11)).
fof(f1549, plain, (spl42_158 | spl42_157 | spl42_156 | spl42_155 | spl42_154 | spl42_153 | spl42_152 | spl42_151 | spl42_150 | spl42_149 | spl42_148 | spl42_147 | spl42_146 | spl42_145 | spl42_144 | ~ spl42_68), inference(avatar_split_clause, [], [f608, f957, f1402, f1411, f1420, f1429, f1438, f1447, f1456, f1465, f1474, f1483, f1492, f1501, f1510, f1519, f1528])).
fof(f608, plain, (~ (e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(duplicate_literal_removal, [], [f504])).
fof(f504, plain, (~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f1548, plain, (spl42_125 | spl42_106 | spl42_87 | spl42_68), inference(avatar_split_clause, [], [f505, f957, f1038, f1119, f1200])).
fof(f505, plain, ((e23 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f52])).
fof(f1543, plain, (~ spl42_107 | spl42_102), inference(avatar_split_clause, [], [f512, f1102, f1123])).
fof(f1123, plain, (spl42_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_107])])).
fof(f512, plain, ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))), inference(cnf_transformation, [], [f52])).
fof(f1542, plain, (~ spl42_108 | spl42_98), inference(avatar_split_clause, [], [f513, f1085, f1127])).
fof(f1127, plain, (spl42_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_108])])).
fof(f513, plain, ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))), inference(cnf_transformation, [], [f52])).
fof(f1541, plain, (~ spl42_85 | spl42_95), inference(avatar_split_clause, [], [f514, f1072, f1030])).
fof(f514, plain, ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f52])).
fof(f1539, plain, (~ spl42_88 | spl42_83), inference(avatar_split_clause, [], [f517, f1021, f1042])).
fof(f1042, plain, (spl42_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_88])])).
fof(f517, plain, ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))), inference(cnf_transformation, [], [f52])).
fof(f1538, plain, (~ spl42_65 | spl42_80), inference(avatar_split_clause, [], [f518, f1008, f945])).
fof(f518, plain, ((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))), inference(cnf_transformation, [], [f52])).
fof(f1537, plain, (~ spl42_66 | spl42_76), inference(avatar_split_clause, [], [f519, f991, f949])).
fof(f949, plain, (spl42_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_66])])).
fof(f519, plain, ((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))), inference(cnf_transformation, [], [f52])).
fof(f1536, plain, (~ spl42_67 | spl42_72), inference(avatar_split_clause, [], [f520, f974, f953])).
fof(f953, plain, (spl42_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_67])])).
fof(f520, plain, ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))), inference(cnf_transformation, [], [f52])).
fof(f1531, plain, (~ spl42_158 | ~ spl42_65 | ~ spl42_80), inference(avatar_split_clause, [], [f499, f1008, f945, f1528])).
fof(f499, plain, (~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f95, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP15), inference(nnf_transformation, [], [f37])).
fof(f1522, plain, (~ spl42_157 | ~ spl42_65 | ~ spl42_80), inference(avatar_split_clause, [], [f494, f1008, f945, f1519])).
fof(f494, plain, (~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f94, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e20, e21))) | ~ sP16), inference(nnf_transformation, [], [f38])).
fof(f1517, plain, (~ spl42_156 | spl42_119), inference(avatar_split_clause, [], [f485, f1174, f1510])).
fof(f485, plain, ((e22 = op2(e20, e22)) | ~ sP17), inference(cnf_transformation, [], [f93])).
fof(f93, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e20, e22))) | ~ sP17), inference(nnf_transformation, [], [f39])).
fof(f1508, plain, (~ spl42_155 | spl42_116), inference(avatar_split_clause, [], [f480, f1161, f1501])).
fof(f480, plain, ((e23 = op2(e20, e23)) | ~ sP18), inference(cnf_transformation, [], [f92])).
fof(f92, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, e23))) | ~ sP18), inference(nnf_transformation, [], [f40])).
fof(f1498, plain, (~ spl42_154 | ~ spl42_126 | ~ spl42_121), inference(avatar_split_clause, [], [f476, f1183, f1204, f1492])).
fof(f1183, plain, (spl42_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_121])])).
fof(f476, plain, (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f91, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e21, e20))) | ~ sP19), inference(nnf_transformation, [], [f41])).
fof(f1497, plain, (~ spl42_154 | ~ spl42_106), inference(avatar_split_clause, [], [f613, f1119, f1492])).
fof(f613, plain, (~ (e21 = op2(e21, e21)) | ~ sP19), inference(duplicate_literal_removal, [], [f477])).
fof(f477, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f1490, plain, (~ spl42_153 | spl42_106), inference(avatar_split_clause, [], [f470, f1119, f1483])).
fof(f470, plain, ((e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f90, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(nnf_transformation, [], [f42])).
fof(f1488, plain, (~ spl42_153 | ~ spl42_106), inference(avatar_split_clause, [], [f614, f1119, f1483])).
fof(f614, plain, (~ (e21 = op2(e21, e21)) | ~ sP20), inference(duplicate_literal_removal, [], [f472])).
fof(f472, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1481, plain, (~ spl42_152 | spl42_103), inference(avatar_split_clause, [], [f465, f1106, f1474])).
fof(f465, plain, ((e22 = op2(e21, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e21, e22))) | ~ sP21), inference(nnf_transformation, [], [f43])).
fof(f1479, plain, (~ spl42_152 | ~ spl42_106), inference(avatar_split_clause, [], [f615, f1119, f1474])).
fof(f615, plain, (~ (e21 = op2(e21, e21)) | ~ sP21), inference(duplicate_literal_removal, [], [f467])).
fof(f467, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1471, plain, (~ spl42_151 | ~ spl42_126 | ~ spl42_121), inference(avatar_split_clause, [], [f461, f1183, f1204, f1465])).
fof(f461, plain, (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f88, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e21, e23))) | ~ sP22), inference(nnf_transformation, [], [f44])).
fof(f1470, plain, (~ spl42_151 | ~ spl42_106), inference(avatar_split_clause, [], [f616, f1119, f1465])).
fof(f616, plain, (~ (e21 = op2(e21, e21)) | ~ sP22), inference(duplicate_literal_removal, [], [f462])).
fof(f462, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f1463, plain, (~ spl42_150 | spl42_93), inference(avatar_split_clause, [], [f455, f1064, f1456])).
fof(f455, plain, ((e20 = op2(e22, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e22, e20))) | ~ sP23), inference(nnf_transformation, [], [f45])).
fof(f1460, plain, (~ spl42_150 | ~ spl42_87), inference(avatar_split_clause, [], [f617, f1038, f1456])).
fof(f617, plain, (~ (e22 = op2(e22, e22)) | ~ sP23), inference(duplicate_literal_removal, [], [f458])).
fof(f458, plain, (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1454, plain, (~ spl42_149 | spl42_90), inference(avatar_split_clause, [], [f450, f1051, f1447])).
fof(f450, plain, ((e21 = op2(e22, e21)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e22, e21))) | ~ sP24), inference(nnf_transformation, [], [f46])).
fof(f1445, plain, (~ spl42_148 | spl42_87), inference(avatar_split_clause, [], [f445, f1038, f1438])).
fof(f445, plain, ((e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(nnf_transformation, [], [f47])).
fof(f1442, plain, (~ spl42_148 | ~ spl42_87), inference(avatar_split_clause, [], [f619, f1038, f1438])).
fof(f619, plain, (~ (e22 = op2(e22, e22)) | ~ sP25), inference(duplicate_literal_removal, [], [f448])).
fof(f448, plain, (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f1436, plain, (~ spl42_147 | spl42_84), inference(avatar_split_clause, [], [f440, f1025, f1429])).
fof(f440, plain, ((e23 = op2(e22, e23)) | ~ sP26), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e22, e23))) | ~ sP26), inference(nnf_transformation, [], [f48])).
fof(f1427, plain, (~ spl42_146 | spl42_77), inference(avatar_split_clause, [], [f435, f996, f1420])).
fof(f435, plain, ((e20 = op2(e23, e20)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f83, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e23, e20))) | ~ sP27), inference(nnf_transformation, [], [f49])).
fof(f1418, plain, (~ spl42_145 | spl42_74), inference(avatar_split_clause, [], [f430, f983, f1411])).
fof(f430, plain, ((e21 = op2(e23, e21)) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f82, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e23, e21))) | ~ sP28), inference(nnf_transformation, [], [f50])).
fof(f1414, plain, (~ spl42_145 | ~ spl42_68), inference(avatar_split_clause, [], [f622, f957, f1411])).
fof(f622, plain, (~ (e23 = op2(e23, e23)) | ~ sP28), inference(duplicate_literal_removal, [], [f434])).
fof(f434, plain, (~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23)) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f1409, plain, (~ spl42_144 | spl42_71), inference(avatar_split_clause, [], [f425, f970, f1402])).
fof(f425, plain, ((e22 = op2(e23, e22)) | ~ sP29), inference(cnf_transformation, [], [f81])).
fof(f81, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e23, e22))) | ~ sP29), inference(nnf_transformation, [], [f51])).
fof(f1405, plain, (~ spl42_144 | ~ spl42_68), inference(avatar_split_clause, [], [f623, f957, f1402])).
fof(f623, plain, (~ (e23 = op2(e23, e23)) | ~ sP29), inference(duplicate_literal_removal, [], [f429])).
fof(f429, plain, (~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23)) | ~ sP29), inference(cnf_transformation, [], [f81])).
fof(f1400, plain, (spl42_143 | spl42_142 | spl42_141 | spl42_140 | spl42_139 | spl42_138 | spl42_137 | spl42_136 | spl42_135 | spl42_134 | spl42_133 | spl42_132 | spl42_131 | spl42_130 | spl42_129 | spl42_4), inference(avatar_split_clause, [], [f403, f653, f1249, f1258, f1267, f1276, f1285, f1294, f1303, f1312, f1321, f1330, f1339, f1348, f1357, f1366, f1375])).
fof(f1375, plain, (spl42_143 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl42_143])])).
fof(f1366, plain, (spl42_142 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl42_142])])).
fof(f1357, plain, (spl42_141 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl42_141])])).
fof(f1348, plain, (spl42_140 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl42_140])])).
fof(f1339, plain, (spl42_139 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl42_139])])).
fof(f1330, plain, (spl42_138 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl42_138])])).
fof(f1321, plain, (spl42_137 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl42_137])])).
fof(f1312, plain, (spl42_136 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl42_136])])).
fof(f1303, plain, (spl42_135 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl42_135])])).
fof(f1294, plain, (spl42_134 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl42_134])])).
fof(f1285, plain, (spl42_133 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl42_133])])).
fof(f1276, plain, (spl42_132 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl42_132])])).
fof(f1267, plain, (spl42_131 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl42_131])])).
fof(f1258, plain, (spl42_130 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl42_130])])).
fof(f1249, plain, (spl42_129 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl42_129])])).
fof(f403, plain, ((e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & ((e13 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e10))) & (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f10, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e11, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e11, e12))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e11, e13))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e12, e10))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e12, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e12, e13))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e13, e10))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e13, e11))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e13, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f10, plain, (((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & ((e13 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e10))) & (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e13, e12))) | ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e13, e11))) | ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e13, e10))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e12, e13))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e12, e11))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e12, e10))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e11, e13))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e11, e12))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e11, e10))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, e13))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e10, e12))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e10, e11))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax10)).
fof(f1395, plain, (spl42_61 | spl42_42 | spl42_23 | spl42_4), inference(avatar_split_clause, [], [f408, f653, f734, f815, f896])).
fof(f408, plain, ((e13 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f36])).
fof(f1392, plain, (~ spl42_64 | spl42_49), inference(avatar_split_clause, [], [f412, f845, f908])).
fof(f908, plain, (spl42_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl42_64])])).
fof(f412, plain, ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f36])).
fof(f1389, plain, (~ spl42_44 | spl42_34), inference(avatar_split_clause, [], [f416, f781, f823])).
fof(f823, plain, (spl42_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_44])])).
fof(f416, plain, ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))), inference(cnf_transformation, [], [f36])).
fof(f1388, plain, (~ spl42_21 | spl42_31), inference(avatar_split_clause, [], [f417, f768, f726])).
fof(f417, plain, ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f36])).
fof(f1386, plain, (~ spl42_24 | spl42_19), inference(avatar_split_clause, [], [f420, f717, f738])).
fof(f738, plain, (spl42_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_24])])).
fof(f420, plain, ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))), inference(cnf_transformation, [], [f36])).
fof(f1385, plain, (~ spl42_1 | spl42_16), inference(avatar_split_clause, [], [f421, f704, f641])).
fof(f421, plain, ((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))), inference(cnf_transformation, [], [f36])).
fof(f1384, plain, (~ spl42_2 | spl42_12), inference(avatar_split_clause, [], [f422, f687, f645])).
fof(f645, plain, (spl42_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_2])])).
fof(f422, plain, ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))), inference(cnf_transformation, [], [f36])).
fof(f1383, plain, (~ spl42_3 | spl42_8), inference(avatar_split_clause, [], [f423, f670, f649])).
fof(f649, plain, (spl42_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_3])])).
fof(f423, plain, ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))), inference(cnf_transformation, [], [f36])).
fof(f1378, plain, (~ spl42_143 | ~ spl42_1 | ~ spl42_16), inference(avatar_split_clause, [], [f402, f704, f641, f1375])).
fof(f402, plain, (~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1369, plain, (~ spl42_142 | ~ spl42_1 | ~ spl42_16), inference(avatar_split_clause, [], [f397, f704, f641, f1366])).
fof(f397, plain, (~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f79, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1360, plain, (~ spl42_141 | ~ spl42_1 | ~ spl42_16), inference(avatar_split_clause, [], [f392, f704, f641, f1357])).
fof(f392, plain, (~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13)) | ~ sP2), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1355, plain, (~ spl42_140 | spl42_52), inference(avatar_split_clause, [], [f383, f857, f1348])).
fof(f383, plain, ((e13 = op1(e10, e13)) | ~ sP3), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, e13))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1346, plain, (~ spl42_139 | spl42_45), inference(avatar_split_clause, [], [f378, f828, f1339])).
fof(f378, plain, ((e10 = op1(e11, e10)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e11, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f1345, plain, (~ spl42_139 | ~ spl42_62 | ~ spl42_57), inference(avatar_split_clause, [], [f379, f879, f900, f1339])).
fof(f379, plain, (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1336, plain, (~ spl42_138 | ~ spl42_62 | ~ spl42_57), inference(avatar_split_clause, [], [f374, f879, f900, f1330])).
fof(f374, plain, (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f75, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f1335, plain, (~ spl42_138 | ~ spl42_42), inference(avatar_split_clause, [], [f630, f815, f1330])).
fof(f630, plain, (~ (e11 = op1(e11, e11)) | ~ sP5), inference(duplicate_literal_removal, [], [f375])).
fof(f375, plain, (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1328, plain, (~ spl42_137 | spl42_39), inference(avatar_split_clause, [], [f368, f802, f1321])).
fof(f368, plain, ((e12 = op1(e11, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f74, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e11, e12))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f1327, plain, (~ spl42_137 | ~ spl42_62 | ~ spl42_57), inference(avatar_split_clause, [], [f369, f879, f900, f1321])).
fof(f369, plain, (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1318, plain, (~ spl42_136 | ~ spl42_62 | ~ spl42_57), inference(avatar_split_clause, [], [f364, f879, f900, f1312])).
fof(f364, plain, (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f73, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e11, e13))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f1317, plain, (~ spl42_136 | ~ spl42_42), inference(avatar_split_clause, [], [f632, f815, f1312])).
fof(f632, plain, (~ (e11 = op1(e11, e11)) | ~ sP7), inference(duplicate_literal_removal, [], [f365])).
fof(f365, plain, (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11)) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f1310, plain, (~ spl42_135 | spl42_29), inference(avatar_split_clause, [], [f358, f760, f1303])).
fof(f358, plain, ((e10 = op1(e12, e10)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f72, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e12, e10))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f1307, plain, (~ spl42_135 | ~ spl42_23), inference(avatar_split_clause, [], [f633, f734, f1303])).
fof(f633, plain, (~ (e12 = op1(e12, e12)) | ~ sP8), inference(duplicate_literal_removal, [], [f361])).
fof(f361, plain, (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1301, plain, (~ spl42_134 | spl42_26), inference(avatar_split_clause, [], [f353, f747, f1294])).
fof(f353, plain, ((e11 = op1(e12, e11)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f71, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e12, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f1292, plain, (~ spl42_133 | spl42_23), inference(avatar_split_clause, [], [f348, f734, f1285])).
fof(f348, plain, ((e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f1289, plain, (~ spl42_133 | ~ spl42_23), inference(avatar_split_clause, [], [f635, f734, f1285])).
fof(f635, plain, (~ (e12 = op1(e12, e12)) | ~ sP10), inference(duplicate_literal_removal, [], [f351])).
fof(f351, plain, (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f1283, plain, (~ spl42_132 | spl42_20), inference(avatar_split_clause, [], [f343, f721, f1276])).
fof(f343, plain, ((e13 = op1(e12, e13)) | ~ sP11), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e12, e13))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f1274, plain, (~ spl42_131 | spl42_13), inference(avatar_split_clause, [], [f338, f692, f1267])).
fof(f338, plain, ((e10 = op1(e13, e10)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f68, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e13, e10))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f1265, plain, (~ spl42_130 | spl42_10), inference(avatar_split_clause, [], [f333, f679, f1258])).
fof(f333, plain, ((e11 = op1(e13, e11)) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f67, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e13, e11))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f1256, plain, (~ spl42_129 | spl42_7), inference(avatar_split_clause, [], [f328, f666, f1249])).
fof(f328, plain, ((e12 = op1(e13, e12)) | ~ sP14), inference(cnf_transformation, [], [f66])).
fof(f66, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e13, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f1246, plain, (spl42_125 | spl42_109 | spl42_93 | spl42_77), inference(avatar_split_clause, [], [f173, f996, f1064, f1132, f1200])).
fof(f173, plain, ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax4)).
fof(f1245, plain, (spl42_126 | spl42_122 | spl42_118 | spl42_114), inference(avatar_split_clause, [], [f174, f1153, f1170, f1187, f1204])).
fof(f174, plain, ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1244, plain, (spl42_126 | spl42_110 | spl42_94 | spl42_78), inference(avatar_split_clause, [], [f175, f1000, f1068, f1136, f1204])).
fof(f175, plain, ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1242, plain, (spl42_127 | spl42_111 | spl42_95 | spl42_79), inference(avatar_split_clause, [], [f177, f1004, f1072, f1140, f1208])).
fof(f177, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1237, plain, (spl42_110 | spl42_106 | spl42_102 | spl42_98), inference(avatar_split_clause, [], [f182, f1085, f1102, f1119, f1136])).
fof(f182, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1230, plain, (spl42_117 | spl42_101 | spl42_85 | spl42_69), inference(avatar_split_clause, [], [f189, f962, f1030, f1098, f1166])).
fof(f189, plain, ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1228, plain, (spl42_118 | spl42_102 | spl42_86 | spl42_70), inference(avatar_split_clause, [], [f191, f966, f1034, f1102, f1170])).
fof(f191, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1225, plain, (spl42_96 | spl42_92 | spl42_88 | spl42_84), inference(avatar_split_clause, [], [f194, f1025, f1042, f1059, f1076])).
fof(f194, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1224, plain, (spl42_120 | spl42_104 | spl42_88 | spl42_72), inference(avatar_split_clause, [], [f195, f974, f1042, f1110, f1178])).
fof(f195, plain, ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1221, plain, (spl42_78 | spl42_74 | spl42_70 | spl42_66), inference(avatar_split_clause, [], [f198, f949, f966, f983, f1000])).
fof(f198, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1219, plain, (spl42_79 | spl42_75 | spl42_71 | spl42_67), inference(avatar_split_clause, [], [f200, f953, f970, f987, f1004])).
fof(f200, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1198, plain, (spl42_121 | spl42_122 | spl42_123 | spl42_124), inference(avatar_split_clause, [], [f157, f1195, f1191, f1187, f1183])).
fof(f157, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax3)).
fof(f1147, plain, (spl42_109 | spl42_110 | spl42_111 | spl42_112), inference(avatar_split_clause, [], [f160, f1144, f1140, f1136, f1132])).
fof(f160, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1130, plain, (spl42_105 | spl42_106 | spl42_107 | spl42_108), inference(avatar_split_clause, [], [f161, f1127, f1123, f1119, f1115])).
fof(f161, plain, ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))), inference(cnf_transformation, [], [f3])).
fof(f1113, plain, (spl42_101 | spl42_102 | spl42_103 | spl42_104), inference(avatar_split_clause, [], [f162, f1110, f1106, f1102, f1098])).
fof(f162, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f1096, plain, (spl42_97 | spl42_98 | spl42_99 | spl42_100), inference(avatar_split_clause, [], [f163, f1093, f1089, f1085, f1081])).
fof(f163, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1045, plain, (spl42_85 | spl42_86 | spl42_87 | spl42_88), inference(avatar_split_clause, [], [f166, f1042, f1038, f1034, f1030])).
fof(f166, plain, ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f3])).
fof(f977, plain, (spl42_69 | spl42_70 | spl42_71 | spl42_72), inference(avatar_split_clause, [], [f170, f974, f970, f966, f962])).
fof(f170, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f941, plain, (spl42_62 | spl42_58 | spl42_54 | spl42_50), inference(avatar_split_clause, [], [f126, f849, f866, f883, f900])).
fof(f126, plain, ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax2)).
fof(f940, plain, (spl42_62 | spl42_46 | spl42_30 | spl42_14), inference(avatar_split_clause, [], [f127, f696, f764, f832, f900])).
fof(f127, plain, ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f938, plain, (spl42_63 | spl42_47 | spl42_31 | spl42_15), inference(avatar_split_clause, [], [f129, f700, f768, f836, f904])).
fof(f129, plain, ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f934, plain, (spl42_57 | spl42_41 | spl42_25 | spl42_9), inference(avatar_split_clause, [], [f133, f675, f743, f811, f879])).
fof(f133, plain, ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f931, plain, (spl42_47 | spl42_43 | spl42_39 | spl42_35), inference(avatar_split_clause, [], [f136, f785, f802, f819, f836])).
fof(f136, plain, ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f928, plain, (spl42_60 | spl42_44 | spl42_28 | spl42_12), inference(avatar_split_clause, [], [f139, f687, f755, f823, f891])).
fof(f139, plain, ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f926, plain, (spl42_53 | spl42_37 | spl42_21 | spl42_5), inference(avatar_split_clause, [], [f141, f658, f726, f794, f862])).
fof(f141, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f920, plain, (spl42_56 | spl42_40 | spl42_24 | spl42_8), inference(avatar_split_clause, [], [f147, f670, f738, f806, f874])).
fof(f147, plain, ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f917, plain, (spl42_14 | spl42_10 | spl42_6 | spl42_2), inference(avatar_split_clause, [], [f150, f645, f662, f679, f696])).
fof(f150, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f915, plain, (spl42_15 | spl42_11 | spl42_7 | spl42_3), inference(avatar_split_clause, [], [f152, f649, f666, f683, f700])).
fof(f152, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f912, plain, (spl42_52 | spl42_36 | spl42_20 | spl42_4), inference(avatar_split_clause, [], [f155, f653, f721, f789, f857])).
fof(f155, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f911, plain, (spl42_61 | spl42_62 | spl42_63 | spl42_64), inference(avatar_split_clause, [], [f108, f908, f904, f900, f896])).
fof(f108, plain, ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG123+1.p', ax1)).
fof(f894, plain, (spl42_57 | spl42_58 | spl42_59 | spl42_60), inference(avatar_split_clause, [], [f109, f891, f887, f883, f879])).
fof(f109, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f843, plain, (spl42_45 | spl42_46 | spl42_47 | spl42_48), inference(avatar_split_clause, [], [f112, f840, f836, f832, f828])).
fof(f112, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f826, plain, (spl42_41 | spl42_42 | spl42_43 | spl42_44), inference(avatar_split_clause, [], [f113, f823, f819, f815, f811])).
fof(f113, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f809, plain, (spl42_37 | spl42_38 | spl42_39 | spl42_40), inference(avatar_split_clause, [], [f114, f806, f802, f798, f794])).
fof(f114, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f775, plain, (spl42_29 | spl42_30 | spl42_31 | spl42_32), inference(avatar_split_clause, [], [f116, f772, f768, f764, f760])).
fof(f116, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f741, plain, (spl42_21 | spl42_22 | spl42_23 | spl42_24), inference(avatar_split_clause, [], [f118, f738, f734, f730, f726])).
fof(f118, plain, ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f1])).