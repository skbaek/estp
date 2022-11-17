fof(f2771, plain, $false, inference(avatar_sat_refutation, [], [f401, f418, f435, f452, f469, f486, f503, f520, f537, f554, f571, f588, f605, f622, f639, f656, f657, f658, f659, f660, f661, f662, f663, f664, f665, f666, f667, f669, f670, f671, f672, f673, f674, f675, f676, f677, f679, f680, f682, f683, f684, f685, f686, f687, f688, f693, f694, f695, f696, f697, f702, f704, f705, f706, f711, f712, f714, f715, f720, f721, f722, f723, f724, f729, f730, f731, f733, f738, f739, f740, f741, f742, f747, f748, f749, f750, f751, f756, f757, f758, f759, f760, f765, f766, f767, f768, f769, f774, f775, f776, f778, f783, f784, f785, f786, f787, f792, f793, f794, f795, f796, f801, f802, f803, f804, f805, f810, f811, f813, f814, f819, f820, f821, f823, f828, f829, f830, f835, f836, f837, f842, f843, f844, f849, f851, f857, f858, f863, f864, f865, f870, f871, f872, f877, f878, f879, f884, f885, f886, f892, f893, f898, f900, f905, f906, f907, f912, f913, f914, f919, f921, f927, f928, f929, f930, f931, f933, f935, f936, f945, f978, f987, f992, f1002, f1011, f1012, f1032, f1037, f1038, f1044, f1049, f1050, f1067, f1068, f1069, f1070, f1078, f1081, f1089, f1098, f1107, f1108, f1111, f1120, f1121, f1123, f1131, f1138, f1143, f1144, f1150, f1152, f1159, f1160, f1166, f1169, f1172, f1175, f1181, f1184, f1187, f1191, f1194, f1199, f1202, f1205, f1209, f1214, f1219, f1225, f1234, f1236, f1237, f1238, f1240, f1247, f1251, f1253, f1257, f1259, f1265, f1270, f1274, f1279, f1283, f1290, f1297, f1304, f1307, f1312, f1317, f1328, f1332, f1339, f1343, f1348, f1349, f1352, f1361, f1362, f1384, f1390, f1392, f1393, f1406, f1415, f1433, f1441, f1447, f1448, f1453, f1457, f1490, f1495, f1498, f1501, f1530, f1552, f1555, f1557, f1595, f1603, f1608, f1617, f1636, f1646, f1663, f1674, f1684, f1707, f1716, f1734, f1739, f1746, f1751, f1754, f1767, f1769, f1770, f1773, f1775, f1780, f1795, f1796, f1802, f1810, f1840, f1845, f1873, f1882, f1886, f1892, f1902, f1906, f1925, f1940, f1942, f1946, f1954, f1955, f1965, f1971, f1981, f1987, f2014, f2036, f2045, f2059, f2062, f2075, f2085, f2087, f2093, f2094, f2098, f2107, f2119, f2148, f2152, f2161, f2163, f2166, f2182, f2201, f2209, f2211, f2213, f2219, f2222, f2227, f2234, f2236, f2242, f2250, f2264, f2270, f2273, f2278, f2286, f2288, f2291, f2301, f2315, f2321, f2337, f2339, f2349, f2362, f2363, f2366, f2373, f2374, f2380, f2395, f2403, f2408, f2411, f2422, f2440, f2442, f2446, f2466, f2475, f2476, f2482, f2484, f2491, f2494, f2495, f2500, f2502, f2520, f2525, f2538, f2540, f2542, f2544, f2546, f2548, f2550, f2552, f2571, f2583, f2584, f2589, f2595, f2613, f2619, f2625, f2633, f2638, f2655, f2657, f2680, f2681, f2689, f2690, f2698, f2702, f2707, f2717, f2723, f2729, f2760, f2770])).
fof(f2770, plain, (~ spl30_2 | ~ spl30_25 | spl30_101 | ~ spl30_103), inference(avatar_contradiction_clause, [], [f2769])).
fof(f2769, plain, ($false | (~ spl30_2 | ~ spl30_25 | spl30_101 | ~ spl30_103)), inference(subsumption_resolution, [], [f2767, f490])).
fof(f490, plain, ((e0 = op(e2, e1)) | ~ spl30_25), inference(avatar_component_clause, [], [f488])).
fof(f488, plain, (spl30_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_25])])).
fof(f2767, plain, (~ (e0 = op(e2, e1)) | (~ spl30_2 | spl30_101 | ~ spl30_103)), inference(backward_demodulation, [], [f2668, f392])).
fof(f392, plain, ((e1 = op(e3, e3)) | ~ spl30_2), inference(avatar_component_clause, [], [f390])).
fof(f390, plain, (spl30_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_2])])).
fof(f2668, plain, (~ (e0 = op(e2, op(e3, e3))) | (spl30_101 | ~ spl30_103)), inference(backward_demodulation, [], [f968, f976])).
fof(f976, plain, ((e2 = op(e3, op(e3, e3))) | ~ spl30_103), inference(avatar_component_clause, [], [f975])).
fof(f975, plain, (spl30_103 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_103])])).
fof(f968, plain, (~ (e0 = op(op(e3, op(e3, e3)), op(e3, e3))) | spl30_101), inference(avatar_component_clause, [], [f966])).
fof(f966, plain, (spl30_101 <=> (e0 = op(op(e3, op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_101])])).
fof(f2760, plain, (~ spl30_3 | ~ spl30_11), inference(avatar_split_clause, [], [f2758, f428, f394])).
fof(f394, plain, (spl30_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_3])])).
fof(f428, plain, (spl30_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_11])])).
fof(f2758, plain, (~ (e2 = op(e3, e3)) | ~ spl30_11), inference(backward_demodulation, [], [f209, f430])).
fof(f430, plain, ((e2 = op(e3, e1)) | ~ spl30_11), inference(avatar_component_clause, [], [f428])).
fof(f209, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax3)).
fof(f2729, plain, (~ spl30_23 | ~ spl30_31), inference(avatar_split_clause, [], [f2726, f513, f479])).
fof(f479, plain, (spl30_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_23])])).
fof(f513, plain, (spl30_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_31])])).
fof(f2726, plain, (~ (e2 = op(e2, e2)) | ~ spl30_31), inference(backward_demodulation, [], [f200, f515])).
fof(f515, plain, ((e2 = op(e2, e0)) | ~ spl30_31), inference(avatar_component_clause, [], [f513])).
fof(f200, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2723, plain, (~ spl30_23 | ~ spl30_39), inference(avatar_split_clause, [], [f2720, f547, f479])).
fof(f547, plain, (spl30_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_39])])).
fof(f2720, plain, (~ (e2 = op(e2, e2)) | ~ spl30_39), inference(backward_demodulation, [], [f178, f549])).
fof(f549, plain, ((e2 = op(e1, e2)) | ~ spl30_39), inference(avatar_component_clause, [], [f547])).
fof(f178, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2717, plain, (~ spl30_10 | ~ spl30_42), inference(avatar_split_clause, [], [f2710, f560, f424])).
fof(f424, plain, (spl30_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_10])])).
fof(f560, plain, (spl30_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_42])])).
fof(f2710, plain, (~ (e1 = op(e3, e1)) | ~ spl30_42), inference(backward_demodulation, [], [f173, f562])).
fof(f562, plain, ((e1 = op(e1, e1)) | ~ spl30_42), inference(avatar_component_clause, [], [f560])).
fof(f173, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2707, plain, (~ spl30_16 | ~ spl30_48), inference(avatar_split_clause, [], [f2704, f585, f449])).
fof(f449, plain, (spl30_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_16])])).
fof(f585, plain, (spl30_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_48])])).
fof(f2704, plain, (~ (e3 = op(e3, e0)) | ~ spl30_48), inference(backward_demodulation, [], [f167, f587])).
fof(f587, plain, ((e3 = op(e1, e0)) | ~ spl30_48), inference(avatar_component_clause, [], [f585])).
fof(f167, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2702, plain, (~ spl30_3 | ~ spl30_51), inference(avatar_split_clause, [], [f2700, f598, f394])).
fof(f598, plain, (spl30_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_51])])).
fof(f2700, plain, (~ (e2 = op(e3, e3)) | ~ spl30_51), inference(backward_demodulation, [], [f183, f600])).
fof(f600, plain, ((e2 = op(e0, e3)) | ~ spl30_51), inference(avatar_component_clause, [], [f598])).
fof(f183, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2698, plain, (~ spl30_5 | ~ spl30_53), inference(avatar_split_clause, [], [f2693, f607, f403])).
fof(f403, plain, (spl30_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_5])])).
fof(f607, plain, (spl30_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_53])])).
fof(f2693, plain, (~ (e0 = op(e3, e2)) | ~ spl30_53), inference(backward_demodulation, [], [f177, f609])).
fof(f609, plain, ((e0 = op(e0, e2)) | ~ spl30_53), inference(avatar_component_clause, [], [f607])).
fof(f177, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2690, plain, (~ spl30_52 | ~ spl30_60), inference(avatar_split_clause, [], [f2688, f636, f602])).
fof(f602, plain, (spl30_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_52])])).
fof(f636, plain, (spl30_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_60])])).
fof(f2688, plain, (~ (e3 = op(e0, e3)) | ~ spl30_60), inference(backward_demodulation, [], [f191, f638])).
fof(f638, plain, ((e3 = op(e0, e1)) | ~ spl30_60), inference(avatar_component_clause, [], [f636])).
fof(f191, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2689, plain, (~ spl30_28 | ~ spl30_60), inference(avatar_split_clause, [], [f2685, f636, f500])).
fof(f500, plain, (spl30_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_28])])).
fof(f2685, plain, (~ (e3 = op(e2, e1)) | ~ spl30_60), inference(backward_demodulation, [], [f170, f638])).
fof(f170, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2681, plain, (~ spl30_54 | ~ spl30_62), inference(avatar_split_clause, [], [f2673, f645, f611])).
fof(f611, plain, (spl30_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_54])])).
fof(f645, plain, (spl30_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl30_62])])).
fof(f2673, plain, (~ (e1 = op(e0, e2)) | ~ spl30_62), inference(backward_demodulation, [], [f188, f647])).
fof(f647, plain, ((op(e0, e0) = e1) | ~ spl30_62), inference(avatar_component_clause, [], [f645])).
fof(f188, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2680, plain, (~ spl30_46 | ~ spl30_62), inference(avatar_split_clause, [], [f2669, f645, f577])).
fof(f577, plain, (spl30_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_46])])).
fof(f2669, plain, (~ (e1 = op(e1, e0)) | ~ spl30_62), inference(backward_demodulation, [], [f163, f647])).
fof(f163, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2657, plain, (~ spl30_3 | ~ spl30_5 | spl30_110), inference(avatar_contradiction_clause, [], [f2656])).
fof(f2656, plain, ($false | (~ spl30_3 | ~ spl30_5 | spl30_110)), inference(subsumption_resolution, [], [f2648, f405])).
fof(f405, plain, ((e0 = op(e3, e2)) | ~ spl30_5), inference(avatar_component_clause, [], [f403])).
fof(f2648, plain, (~ (e0 = op(e3, e2)) | (~ spl30_3 | spl30_110)), inference(backward_demodulation, [], [f1010, f396])).
fof(f396, plain, ((e2 = op(e3, e3)) | ~ spl30_3), inference(avatar_component_clause, [], [f394])).
fof(f1010, plain, (~ (e0 = op(e3, op(e3, e3))) | spl30_110), inference(avatar_component_clause, [], [f1008])).
fof(f1008, plain, (spl30_110 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_110])])).
fof(f2655, plain, (~ spl30_3 | ~ spl30_5 | ~ spl30_54 | spl30_109), inference(avatar_contradiction_clause, [], [f2654])).
fof(f2654, plain, ($false | (~ spl30_3 | ~ spl30_5 | ~ spl30_54 | spl30_109)), inference(subsumption_resolution, [], [f2653, f613])).
fof(f613, plain, ((e1 = op(e0, e2)) | ~ spl30_54), inference(avatar_component_clause, [], [f611])).
fof(f2653, plain, (~ (e1 = op(e0, e2)) | (~ spl30_3 | ~ spl30_5 | spl30_109)), inference(forward_demodulation, [], [f2647, f405])).
fof(f2647, plain, (~ (e1 = op(op(e3, e2), e2)) | (~ spl30_3 | spl30_109)), inference(backward_demodulation, [], [f1006, f396])).
fof(f1006, plain, (~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3))) | spl30_109), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl30_109 <=> (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_109])])).
fof(f2638, plain, (~ spl30_2 | ~ spl30_10), inference(avatar_split_clause, [], [f2635, f424, f390])).
fof(f2635, plain, (~ (e1 = op(e3, e3)) | ~ spl30_10), inference(backward_demodulation, [], [f209, f426])).
fof(f426, plain, ((e1 = op(e3, e1)) | ~ spl30_10), inference(avatar_component_clause, [], [f424])).
fof(f2633, plain, (~ spl30_2 | ~ spl30_18), inference(avatar_split_clause, [], [f2632, f458, f390])).
fof(f458, plain, (spl30_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_18])])).
fof(f2632, plain, (~ (e1 = op(e3, e3)) | ~ spl30_18), inference(backward_demodulation, [], [f186, f460])).
fof(f460, plain, ((e1 = op(e2, e3)) | ~ spl30_18), inference(avatar_component_clause, [], [f458])).
fof(f186, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2625, plain, (~ spl30_7 | ~ spl30_23), inference(avatar_split_clause, [], [f2620, f479, f411])).
fof(f411, plain, (spl30_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_7])])).
fof(f2620, plain, (~ (e2 = op(e3, e2)) | ~ spl30_23), inference(backward_demodulation, [], [f180, f481])).
fof(f481, plain, ((e2 = op(e2, e2)) | ~ spl30_23), inference(avatar_component_clause, [], [f479])).
fof(f180, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2619, plain, (~ spl30_17 | ~ spl30_29), inference(avatar_split_clause, [], [f2617, f505, f454])).
fof(f454, plain, (spl30_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_17])])).
fof(f505, plain, (spl30_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_29])])).
fof(f2617, plain, (~ (e0 = op(e2, e3)) | ~ spl30_29), inference(backward_demodulation, [], [f201, f507])).
fof(f507, plain, ((e0 = op(e2, e0)) | ~ spl30_29), inference(avatar_component_clause, [], [f505])).
fof(f201, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2613, plain, (~ spl30_17 | ~ spl30_33), inference(avatar_split_clause, [], [f2608, f522, f454])).
fof(f522, plain, (spl30_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_33])])).
fof(f2608, plain, (~ (e0 = op(e2, e3)) | ~ spl30_33), inference(backward_demodulation, [], [f184, f524])).
fof(f524, plain, ((e0 = op(e1, e3)) | ~ spl30_33), inference(avatar_component_clause, [], [f522])).
fof(f184, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2595, plain, (~ spl30_42 | ~ spl30_46), inference(avatar_split_clause, [], [f2594, f577, f560])).
fof(f2594, plain, (~ (e1 = op(e1, e1)) | ~ spl30_46), inference(backward_demodulation, [], [f193, f579])).
fof(f579, plain, ((e1 = op(e1, e0)) | ~ spl30_46), inference(avatar_component_clause, [], [f577])).
fof(f193, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2589, plain, (~ spl30_22 | ~ spl30_54), inference(avatar_split_clause, [], [f2585, f611, f475])).
fof(f475, plain, (spl30_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_22])])).
fof(f2585, plain, (~ (e1 = op(e2, e2)) | ~ spl30_54), inference(backward_demodulation, [], [f176, f613])).
fof(f176, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2584, plain, (~ spl30_53 | ~ spl30_57), inference(avatar_split_clause, [], [f2579, f624, f607])).
fof(f624, plain, (spl30_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_57])])).
fof(f2579, plain, (~ (e0 = op(e0, e2)) | ~ spl30_57), inference(backward_demodulation, [], [f190, f626])).
fof(f626, plain, ((e0 = op(e0, e1)) | ~ spl30_57), inference(avatar_component_clause, [], [f624])).
fof(f190, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2583, plain, (~ spl30_9 | ~ spl30_57), inference(avatar_split_clause, [], [f2578, f624, f420])).
fof(f420, plain, (spl30_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_9])])).
fof(f2578, plain, (~ (e0 = op(e3, e1)) | ~ spl30_57), inference(backward_demodulation, [], [f171, f626])).
fof(f171, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2571, plain, (~ spl30_31 | ~ spl30_63), inference(avatar_split_clause, [], [f2564, f649, f513])).
fof(f649, plain, (spl30_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl30_63])])).
fof(f2564, plain, (~ (e2 = op(e2, e0)) | ~ spl30_63), inference(backward_demodulation, [], [f164, f651])).
fof(f651, plain, ((op(e0, e0) = e2) | ~ spl30_63), inference(avatar_component_clause, [], [f649])).
fof(f164, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2552, plain, (~ spl30_44 | ~ spl30_40), inference(avatar_split_clause, [], [f2551, f551, f568])).
fof(f568, plain, (spl30_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_44])])).
fof(f551, plain, (spl30_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_40])])).
fof(f2551, plain, (~ (e3 = op(e1, e1)) | ~ spl30_40), inference(forward_demodulation, [], [f196, f553])).
fof(f553, plain, ((e3 = op(e1, e2)) | ~ spl30_40), inference(avatar_component_clause, [], [f551])).
fof(f196, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2550, plain, (~ spl30_32 | ~ spl30_28), inference(avatar_split_clause, [], [f2549, f500, f517])).
fof(f517, plain, (spl30_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_32])])).
fof(f2549, plain, (~ (e3 = op(e2, e0)) | ~ spl30_28), inference(forward_demodulation, [], [f199, f502])).
fof(f502, plain, ((e3 = op(e2, e1)) | ~ spl30_28), inference(avatar_component_clause, [], [f500])).
fof(f199, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2548, plain, (~ spl30_20 | ~ spl30_28), inference(avatar_split_clause, [], [f2547, f500, f466])).
fof(f466, plain, (spl30_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_20])])).
fof(f2547, plain, (~ (e3 = op(e2, e3)) | ~ spl30_28), inference(forward_demodulation, [], [f203, f502])).
fof(f203, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2546, plain, (~ spl30_20 | ~ spl30_52), inference(avatar_split_clause, [], [f2545, f602, f466])).
fof(f2545, plain, (~ (e3 = op(e2, e3)) | ~ spl30_52), inference(forward_demodulation, [], [f182, f604])).
fof(f604, plain, ((e3 = op(e0, e3)) | ~ spl30_52), inference(avatar_component_clause, [], [f602])).
fof(f182, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2544, plain, (~ spl30_12 | ~ spl30_16), inference(avatar_split_clause, [], [f2543, f449, f432])).
fof(f432, plain, (spl30_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_12])])).
fof(f2543, plain, (~ (e3 = op(e3, e1)) | ~ spl30_16), inference(forward_demodulation, [], [f205, f451])).
fof(f451, plain, ((e3 = op(e3, e0)) | ~ spl30_16), inference(avatar_component_clause, [], [f449])).
fof(f205, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2542, plain, (~ spl30_12 | ~ spl30_28), inference(avatar_split_clause, [], [f2541, f500, f432])).
fof(f2541, plain, (~ (e3 = op(e3, e1)) | ~ spl30_28), inference(forward_demodulation, [], [f174, f502])).
fof(f174, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2540, plain, (~ spl30_8 | ~ spl30_40), inference(avatar_split_clause, [], [f2539, f551, f415])).
fof(f415, plain, (spl30_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_8])])).
fof(f2539, plain, (~ (e3 = op(e3, e2)) | ~ spl30_40), inference(forward_demodulation, [], [f179, f553])).
fof(f179, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2538, plain, (~ spl30_4 | ~ spl30_52), inference(avatar_split_clause, [], [f2537, f602, f398])).
fof(f398, plain, (spl30_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_4])])).
fof(f2537, plain, (~ (e3 = op(e3, e3)) | ~ spl30_52), inference(forward_demodulation, [], [f183, f604])).
fof(f2525, plain, (~ spl30_2 | ~ spl30_9 | spl30_110), inference(avatar_contradiction_clause, [], [f2524])).
fof(f2524, plain, ($false | (~ spl30_2 | ~ spl30_9 | spl30_110)), inference(subsumption_resolution, [], [f2523, f422])).
fof(f422, plain, ((e0 = op(e3, e1)) | ~ spl30_9), inference(avatar_component_clause, [], [f420])).
fof(f2523, plain, (~ (e0 = op(e3, e1)) | (~ spl30_2 | spl30_110)), inference(forward_demodulation, [], [f1010, f392])).
fof(f2520, plain, (~ spl30_28 | ~ spl30_59 | ~ spl30_62 | spl30_116), inference(avatar_contradiction_clause, [], [f2519])).
fof(f2519, plain, ($false | (~ spl30_28 | ~ spl30_59 | ~ spl30_62 | spl30_116)), inference(subsumption_resolution, [], [f2518, f502])).
fof(f2518, plain, (~ (e3 = op(e2, e1)) | (~ spl30_59 | ~ spl30_62 | spl30_116)), inference(forward_demodulation, [], [f2517, f634])).
fof(f634, plain, ((e2 = op(e0, e1)) | ~ spl30_59), inference(avatar_component_clause, [], [f632])).
fof(f632, plain, (spl30_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_59])])).
fof(f2517, plain, (~ (e3 = op(op(e0, e1), e1)) | (~ spl30_62 | spl30_116)), inference(forward_demodulation, [], [f1042, f647])).
fof(f1042, plain, (~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0))) | spl30_116), inference(avatar_component_clause, [], [f1040])).
fof(f1040, plain, (spl30_116 <=> (e3 = op(op(e0, op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_116])])).
fof(f2502, plain, (~ spl30_24 | ~ spl30_28), inference(avatar_split_clause, [], [f2501, f500, f483])).
fof(f483, plain, (spl30_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_24])])).
fof(f2501, plain, (~ (e3 = op(e2, e2)) | ~ spl30_28), inference(backward_demodulation, [], [f202, f502])).
fof(f202, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2500, plain, (~ spl30_27 | ~ spl30_31), inference(avatar_split_clause, [], [f2499, f513, f496])).
fof(f496, plain, (spl30_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_27])])).
fof(f2499, plain, (~ (e2 = op(e2, e1)) | ~ spl30_31), inference(backward_demodulation, [], [f199, f515])).
fof(f2495, plain, (~ spl30_24 | ~ spl30_40), inference(avatar_split_clause, [], [f2493, f551, f483])).
fof(f2493, plain, (~ (e3 = op(e2, e2)) | ~ spl30_40), inference(backward_demodulation, [], [f178, f553])).
fof(f2494, plain, (~ spl30_36 | ~ spl30_40), inference(avatar_split_clause, [], [f2492, f551, f534])).
fof(f534, plain, (spl30_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_36])])).
fof(f2492, plain, (~ (e3 = op(e1, e3)) | ~ spl30_40), inference(backward_demodulation, [], [f198, f553])).
fof(f198, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2491, plain, (~ spl30_37 | ~ spl30_45), inference(avatar_split_clause, [], [f2488, f573, f539])).
fof(f539, plain, (spl30_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_37])])).
fof(f573, plain, (spl30_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_45])])).
fof(f2488, plain, (~ (e0 = op(e1, e2)) | ~ spl30_45), inference(backward_demodulation, [], [f194, f575])).
fof(f575, plain, ((e0 = op(e1, e0)) | ~ spl30_45), inference(avatar_component_clause, [], [f573])).
fof(f194, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2484, plain, (~ spl30_36 | ~ spl30_52), inference(avatar_split_clause, [], [f2483, f602, f534])).
fof(f2483, plain, (~ (e3 = op(e1, e3)) | ~ spl30_52), inference(backward_demodulation, [], [f181, f604])).
fof(f181, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2482, plain, (~ spl30_37 | ~ spl30_53), inference(avatar_split_clause, [], [f2477, f607, f539])).
fof(f2477, plain, (~ (e0 = op(e1, e2)) | ~ spl30_53), inference(backward_demodulation, [], [f175, f609])).
fof(f175, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2476, plain, (~ spl30_51 | ~ spl30_59), inference(avatar_split_clause, [], [f2473, f632, f598])).
fof(f2473, plain, (~ (e2 = op(e0, e3)) | ~ spl30_59), inference(backward_demodulation, [], [f191, f634])).
fof(f2475, plain, (~ spl30_27 | ~ spl30_59), inference(avatar_split_clause, [], [f2471, f632, f496])).
fof(f2471, plain, (~ (e2 = op(e2, e1)) | ~ spl30_59), inference(backward_demodulation, [], [f170, f634])).
fof(f2466, plain, (~ spl30_30 | ~ spl30_62), inference(avatar_split_clause, [], [f2457, f645, f509])).
fof(f509, plain, (spl30_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_30])])).
fof(f2457, plain, (~ (e1 = op(e2, e0)) | ~ spl30_62), inference(backward_demodulation, [], [f164, f647])).
fof(f2446, plain, (~ spl30_59 | ~ spl30_2 | ~ spl30_9 | spl30_115), inference(avatar_split_clause, [], [f2434, f1034, f420, f390, f632])).
fof(f1034, plain, (spl30_115 <=> (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_115])])).
fof(f2434, plain, (~ (e2 = op(e0, e1)) | (~ spl30_2 | ~ spl30_9 | spl30_115)), inference(forward_demodulation, [], [f2429, f422])).
fof(f2429, plain, (~ (e2 = op(op(e3, e1), e1)) | (~ spl30_2 | spl30_115)), inference(backward_demodulation, [], [f1036, f392])).
fof(f1036, plain, (~ (e2 = op(op(e3, op(e3, e3)), op(e3, e3))) | spl30_115), inference(avatar_component_clause, [], [f1034])).
fof(f2442, plain, (~ spl30_39 | ~ spl30_7), inference(avatar_split_clause, [], [f2441, f411, f547])).
fof(f2441, plain, (~ (e2 = op(e1, e2)) | ~ spl30_7), inference(forward_demodulation, [], [f179, f413])).
fof(f413, plain, ((e2 = op(e3, e2)) | ~ spl30_7), inference(avatar_component_clause, [], [f411])).
fof(f2440, plain, (~ spl30_38 | ~ spl30_42), inference(avatar_split_clause, [], [f2439, f560, f543])).
fof(f543, plain, (spl30_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_38])])).
fof(f2439, plain, (~ (e1 = op(e1, e2)) | ~ spl30_42), inference(forward_demodulation, [], [f196, f562])).
fof(f2422, plain, (~ spl30_3 | ~ spl30_7), inference(avatar_split_clause, [], [f2421, f411, f394])).
fof(f2421, plain, (~ (e2 = op(e3, e3)) | ~ spl30_7), inference(backward_demodulation, [], [f210, f413])).
fof(f210, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2411, plain, (~ spl30_23 | ~ spl30_27), inference(avatar_split_clause, [], [f2410, f496, f479])).
fof(f2410, plain, (~ (e2 = op(e2, e2)) | ~ spl30_27), inference(backward_demodulation, [], [f202, f498])).
fof(f498, plain, ((e2 = op(e2, e1)) | ~ spl30_27), inference(avatar_component_clause, [], [f496])).
fof(f2408, plain, (~ spl30_26 | ~ spl30_30), inference(avatar_split_clause, [], [f2406, f509, f492])).
fof(f492, plain, (spl30_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_26])])).
fof(f2406, plain, (~ (e1 = op(e2, e1)) | ~ spl30_30), inference(backward_demodulation, [], [f199, f511])).
fof(f511, plain, ((e1 = op(e2, e0)) | ~ spl30_30), inference(avatar_component_clause, [], [f509])).
fof(f2403, plain, (~ spl30_37 | ~ spl30_38), inference(avatar_contradiction_clause, [], [f2402])).
fof(f2402, plain, ($false | (~ spl30_37 | ~ spl30_38)), inference(subsumption_resolution, [], [f2401, f211])).
fof(f211, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax4)).
fof(f2401, plain, ((e0 = e1) | (~ spl30_37 | ~ spl30_38)), inference(backward_demodulation, [], [f545, f541])).
fof(f541, plain, ((e0 = op(e1, e2)) | ~ spl30_37), inference(avatar_component_clause, [], [f539])).
fof(f545, plain, ((e1 = op(e1, e2)) | ~ spl30_38), inference(avatar_component_clause, [], [f543])).
fof(f2395, plain, (~ spl30_26 | ~ spl30_42), inference(avatar_split_clause, [], [f2388, f560, f492])).
fof(f2388, plain, (~ (e1 = op(e2, e1)) | ~ spl30_42), inference(backward_demodulation, [], [f172, f562])).
fof(f172, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2380, plain, (~ spl30_50 | ~ spl30_54), inference(avatar_split_clause, [], [f2377, f611, f594])).
fof(f594, plain, (spl30_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_50])])).
fof(f2377, plain, (~ (e1 = op(e0, e3)) | ~ spl30_54), inference(backward_demodulation, [], [f192, f613])).
fof(f192, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2374, plain, (~ spl30_56 | ~ spl30_60), inference(avatar_split_clause, [], [f2371, f636, f619])).
fof(f619, plain, (spl30_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_56])])).
fof(f2371, plain, (~ (e3 = op(e0, e2)) | ~ spl30_60), inference(backward_demodulation, [], [f190, f638])).
fof(f2373, plain, (~ spl30_44 | ~ spl30_60), inference(avatar_split_clause, [], [f2369, f636, f568])).
fof(f2369, plain, (~ (e3 = op(e1, e1)) | ~ spl30_60), inference(backward_demodulation, [], [f169, f638])).
fof(f169, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2366, plain, (~ spl30_61 | ~ spl30_106), inference(avatar_contradiction_clause, [], [f2365])).
fof(f2365, plain, ($false | (~ spl30_61 | ~ spl30_106)), inference(subsumption_resolution, [], [f2364, f213])).
fof(f213, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f2364, plain, ((e0 = e3) | (~ spl30_61 | ~ spl30_106)), inference(forward_demodulation, [], [f2360, f643])).
fof(f643, plain, ((e0 = op(e0, e0)) | ~ spl30_61), inference(avatar_component_clause, [], [f641])).
fof(f641, plain, (spl30_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_61])])).
fof(f2360, plain, ((op(e0, e0) = e3) | (~ spl30_61 | ~ spl30_106)), inference(backward_demodulation, [], [f990, f643])).
fof(f990, plain, ((e3 = op(e0, op(e0, e0))) | ~ spl30_106), inference(avatar_component_clause, [], [f989])).
fof(f989, plain, (spl30_106 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_106])])).
fof(f2363, plain, (~ spl30_49 | ~ spl30_61), inference(avatar_split_clause, [], [f2356, f641, f590])).
fof(f590, plain, (spl30_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_49])])).
fof(f2356, plain, (~ (e0 = op(e0, e3)) | ~ spl30_61), inference(backward_demodulation, [], [f189, f643])).
fof(f189, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2362, plain, (~ spl30_45 | ~ spl30_61), inference(avatar_split_clause, [], [f2352, f641, f573])).
fof(f2352, plain, (~ (e0 = op(e1, e0)) | ~ spl30_61), inference(backward_demodulation, [], [f163, f643])).
fof(f2349, plain, (~ spl30_54 | ~ spl30_38), inference(avatar_split_clause, [], [f2348, f543, f611])).
fof(f2348, plain, (~ (e1 = op(e0, e2)) | ~ spl30_38), inference(forward_demodulation, [], [f175, f545])).
fof(f2339, plain, (~ spl30_32 | ~ spl30_16), inference(avatar_split_clause, [], [f2338, f449, f517])).
fof(f2338, plain, (~ (e3 = op(e2, e0)) | ~ spl30_16), inference(forward_demodulation, [], [f168, f451])).
fof(f168, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2337, plain, (~ spl30_64 | ~ spl30_16), inference(avatar_split_clause, [], [f2336, f449, f653])).
fof(f653, plain, (spl30_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl30_64])])).
fof(f2336, plain, (~ (op(e0, e0) = e3) | ~ spl30_16), inference(forward_demodulation, [], [f165, f451])).
fof(f165, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2321, plain, (~ spl30_5 | ~ spl30_9), inference(avatar_split_clause, [], [f2316, f420, f403])).
fof(f2316, plain, (~ (e0 = op(e3, e2)) | ~ spl30_9), inference(backward_demodulation, [], [f208, f422])).
fof(f208, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2315, plain, (~ spl30_14 | ~ spl30_16), inference(avatar_contradiction_clause, [], [f2314])).
fof(f2314, plain, ($false | (~ spl30_14 | ~ spl30_16)), inference(subsumption_resolution, [], [f2313, f215])).
fof(f215, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2313, plain, ((e1 = e3) | (~ spl30_14 | ~ spl30_16)), inference(forward_demodulation, [], [f451, f443])).
fof(f443, plain, ((e1 = op(e3, e0)) | ~ spl30_14), inference(avatar_component_clause, [], [f441])).
fof(f441, plain, (spl30_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_14])])).
fof(f2301, plain, (~ spl30_22 | ~ spl30_26), inference(avatar_split_clause, [], [f2299, f492, f475])).
fof(f2299, plain, (~ (e1 = op(e2, e2)) | ~ spl30_26), inference(backward_demodulation, [], [f202, f494])).
fof(f494, plain, ((e1 = op(e2, e1)) | ~ spl30_26), inference(avatar_component_clause, [], [f492])).
fof(f2291, plain, (~ spl30_17 | ~ spl30_44 | spl30_95 | ~ spl30_96), inference(avatar_contradiction_clause, [], [f2290])).
fof(f2290, plain, ($false | (~ spl30_17 | ~ spl30_44 | spl30_95 | ~ spl30_96)), inference(subsumption_resolution, [], [f2284, f456])).
fof(f456, plain, ((e0 = op(e2, e3)) | ~ spl30_17), inference(avatar_component_clause, [], [f454])).
fof(f2284, plain, (~ (e0 = op(e2, e3)) | (~ spl30_44 | spl30_95 | ~ spl30_96)), inference(backward_demodulation, [], [f2257, f570])).
fof(f570, plain, ((e3 = op(e1, e1)) | ~ spl30_44), inference(avatar_component_clause, [], [f568])).
fof(f2257, plain, (~ (e0 = op(e2, op(e1, e1))) | (spl30_95 | ~ spl30_96)), inference(forward_demodulation, [], [f940, f943])).
fof(f943, plain, ((e2 = op(e1, op(e1, e1))) | ~ spl30_96), inference(avatar_component_clause, [], [f942])).
fof(f942, plain, (spl30_96 <=> (e2 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_96])])).
fof(f940, plain, (~ (e0 = op(op(e1, op(e1, e1)), op(e1, e1))) | spl30_95), inference(avatar_component_clause, [], [f938])).
fof(f938, plain, (spl30_95 <=> (e0 = op(op(e1, op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_95])])).
fof(f2288, plain, (spl30_35 | ~ spl30_44 | ~ spl30_96), inference(avatar_contradiction_clause, [], [f2287])).
fof(f2287, plain, ($false | (spl30_35 | ~ spl30_44 | ~ spl30_96)), inference(subsumption_resolution, [], [f2281, f531])).
fof(f531, plain, (~ (e2 = op(e1, e3)) | spl30_35), inference(avatar_component_clause, [], [f530])).
fof(f530, plain, (spl30_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_35])])).
fof(f2281, plain, ((e2 = op(e1, e3)) | (~ spl30_44 | ~ spl30_96)), inference(backward_demodulation, [], [f943, f570])).
fof(f2286, plain, (~ spl30_12 | ~ spl30_44), inference(avatar_split_clause, [], [f2280, f568, f432])).
fof(f2280, plain, (~ (e3 = op(e3, e1)) | ~ spl30_44), inference(backward_demodulation, [], [f173, f570])).
fof(f2278, plain, (~ spl30_41 | ~ spl30_45), inference(avatar_split_clause, [], [f2274, f573, f556])).
fof(f556, plain, (spl30_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_41])])).
fof(f2274, plain, (~ (e0 = op(e1, e1)) | ~ spl30_45), inference(backward_demodulation, [], [f193, f575])).
fof(f2273, plain, (~ spl30_49 | ~ spl30_50), inference(avatar_contradiction_clause, [], [f2272])).
fof(f2272, plain, ($false | (~ spl30_49 | ~ spl30_50)), inference(subsumption_resolution, [], [f2271, f211])).
fof(f2271, plain, ((e0 = e1) | (~ spl30_49 | ~ spl30_50)), inference(backward_demodulation, [], [f596, f592])).
fof(f592, plain, ((e0 = op(e0, e3)) | ~ spl30_49), inference(avatar_component_clause, [], [f590])).
fof(f596, plain, ((e1 = op(e0, e3)) | ~ spl30_50), inference(avatar_component_clause, [], [f594])).
fof(f2270, plain, (~ spl30_2 | ~ spl30_50), inference(avatar_split_clause, [], [f2269, f594, f390])).
fof(f2269, plain, (~ (e1 = op(e3, e3)) | ~ spl30_50), inference(backward_demodulation, [], [f183, f596])).
fof(f2264, plain, (~ spl30_43 | ~ spl30_59), inference(avatar_split_clause, [], [f2259, f632, f564])).
fof(f564, plain, (spl30_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_43])])).
fof(f2259, plain, (~ (e2 = op(e1, e1)) | ~ spl30_59), inference(backward_demodulation, [], [f169, f634])).
fof(f2250, plain, (~ spl30_49 | ~ spl30_17), inference(avatar_split_clause, [], [f2249, f454, f590])).
fof(f2249, plain, (~ (e0 = op(e0, e3)) | ~ spl30_17), inference(forward_demodulation, [], [f182, f456])).
fof(f2242, plain, (~ spl30_44 | ~ spl30_36), inference(avatar_split_clause, [], [f2241, f534, f568])).
fof(f2241, plain, (~ (e3 = op(e1, e1)) | ~ spl30_36), inference(forward_demodulation, [], [f197, f536])).
fof(f536, plain, ((e3 = op(e1, e3)) | ~ spl30_36), inference(avatar_component_clause, [], [f534])).
fof(f197, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2236, plain, (~ spl30_24 | ~ spl30_32), inference(avatar_split_clause, [], [f2235, f517, f483])).
fof(f2235, plain, (~ (e3 = op(e2, e2)) | ~ spl30_32), inference(forward_demodulation, [], [f200, f519])).
fof(f519, plain, ((e3 = op(e2, e0)) | ~ spl30_32), inference(avatar_component_clause, [], [f517])).
fof(f2234, plain, (~ spl30_22 | ~ spl30_38), inference(avatar_split_clause, [], [f2233, f543, f475])).
fof(f2233, plain, (~ (e1 = op(e2, e2)) | ~ spl30_38), inference(forward_demodulation, [], [f178, f545])).
fof(f2227, plain, (~ spl30_2 | ~ spl30_14), inference(avatar_split_clause, [], [f2226, f441, f390])).
fof(f2226, plain, (~ (e1 = op(e3, e3)) | ~ spl30_14), inference(backward_demodulation, [], [f207, f443])).
fof(f207, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2222, plain, (~ spl30_4 | ~ spl30_36), inference(avatar_split_clause, [], [f2221, f534, f398])).
fof(f2221, plain, (~ (e3 = op(e3, e3)) | ~ spl30_36), inference(forward_demodulation, [], [f185, f536])).
fof(f185, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2219, plain, (~ spl30_13 | ~ spl30_14), inference(avatar_contradiction_clause, [], [f2218])).
fof(f2218, plain, ($false | (~ spl30_13 | ~ spl30_14)), inference(subsumption_resolution, [], [f2217, f211])).
fof(f2217, plain, ((e0 = e1) | (~ spl30_13 | ~ spl30_14)), inference(forward_demodulation, [], [f443, f439])).
fof(f439, plain, ((e0 = op(e3, e0)) | ~ spl30_13), inference(avatar_component_clause, [], [f437])).
fof(f437, plain, (spl30_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_13])])).
fof(f2213, plain, (~ spl30_20 | ~ spl30_32), inference(avatar_split_clause, [], [f2212, f517, f466])).
fof(f2212, plain, (~ (e3 = op(e2, e3)) | ~ spl30_32), inference(backward_demodulation, [], [f201, f519])).
fof(f2211, plain, (~ spl30_20 | ~ spl30_36), inference(avatar_split_clause, [], [f2210, f534, f466])).
fof(f2210, plain, (~ (e3 = op(e2, e3)) | ~ spl30_36), inference(backward_demodulation, [], [f184, f536])).
fof(f2209, plain, (~ spl30_34 | ~ spl30_38), inference(avatar_split_clause, [], [f2208, f543, f526])).
fof(f526, plain, (spl30_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_34])])).
fof(f2208, plain, (~ (e1 = op(e1, e3)) | ~ spl30_38), inference(backward_demodulation, [], [f198, f545])).
fof(f2201, plain, (~ spl30_37 | ~ spl30_41), inference(avatar_split_clause, [], [f2192, f556, f539])).
fof(f2192, plain, (~ (e0 = op(e1, e2)) | ~ spl30_41), inference(backward_demodulation, [], [f196, f558])).
fof(f558, plain, ((e0 = op(e1, e1)) | ~ spl30_41), inference(avatar_component_clause, [], [f556])).
fof(f2182, plain, (~ spl30_53 | ~ spl30_56), inference(avatar_contradiction_clause, [], [f2181])).
fof(f2181, plain, ($false | (~ spl30_53 | ~ spl30_56)), inference(subsumption_resolution, [], [f2179, f213])).
fof(f2179, plain, ((e0 = e3) | (~ spl30_53 | ~ spl30_56)), inference(backward_demodulation, [], [f621, f609])).
fof(f621, plain, ((e3 = op(e0, e2)) | ~ spl30_56), inference(avatar_component_clause, [], [f619])).
fof(f2166, plain, (~ spl30_51 | ~ spl30_63), inference(avatar_split_clause, [], [f2165, f649, f598])).
fof(f2165, plain, (~ (e2 = op(e0, e3)) | ~ spl30_63), inference(forward_demodulation, [], [f189, f651])).
fof(f2163, plain, (~ spl30_47 | ~ spl30_63), inference(avatar_split_clause, [], [f2162, f649, f581])).
fof(f581, plain, (spl30_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_47])])).
fof(f2162, plain, (~ (e2 = op(e1, e0)) | ~ spl30_63), inference(forward_demodulation, [], [f163, f651])).
fof(f2161, plain, (~ spl30_43 | ~ spl30_27), inference(avatar_split_clause, [], [f2160, f496, f564])).
fof(f2160, plain, (~ (e2 = op(e1, e1)) | ~ spl30_27), inference(forward_demodulation, [], [f172, f498])).
fof(f2152, plain, (~ spl30_40 | ~ spl30_56), inference(avatar_split_clause, [], [f2151, f619, f551])).
fof(f2151, plain, (~ (e3 = op(e1, e2)) | ~ spl30_56), inference(forward_demodulation, [], [f175, f621])).
fof(f2148, plain, (~ spl30_34 | ~ spl30_2), inference(avatar_split_clause, [], [f2147, f390, f526])).
fof(f2147, plain, (~ (e1 = op(e1, e3)) | ~ spl30_2), inference(forward_demodulation, [], [f185, f392])).
fof(f2119, plain, (~ spl30_2 | ~ spl30_3), inference(avatar_contradiction_clause, [], [f2118])).
fof(f2118, plain, ($false | (~ spl30_2 | ~ spl30_3)), inference(subsumption_resolution, [], [f2117, f214])).
fof(f214, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2117, plain, ((e1 = e2) | (~ spl30_2 | ~ spl30_3)), inference(backward_demodulation, [], [f396, f392])).
fof(f2107, plain, (~ spl30_6 | ~ spl30_22), inference(avatar_split_clause, [], [f2099, f475, f407])).
fof(f407, plain, (spl30_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_6])])).
fof(f2099, plain, (~ (e1 = op(e3, e2)) | ~ spl30_22), inference(backward_demodulation, [], [f180, f477])).
fof(f477, plain, ((e1 = op(e2, e2)) | ~ spl30_22), inference(avatar_component_clause, [], [f475])).
fof(f2098, plain, (~ spl30_25 | ~ spl30_27), inference(avatar_contradiction_clause, [], [f2097])).
fof(f2097, plain, ($false | (~ spl30_25 | ~ spl30_27)), inference(subsumption_resolution, [], [f2096, f212])).
fof(f212, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2096, plain, ((e0 = e2) | (~ spl30_25 | ~ spl30_27)), inference(forward_demodulation, [], [f498, f490])).
fof(f2094, plain, (~ spl30_24 | ~ spl30_56), inference(avatar_split_clause, [], [f2092, f619, f483])).
fof(f2092, plain, (~ (e3 = op(e2, e2)) | ~ spl30_56), inference(backward_demodulation, [], [f176, f621])).
fof(f2093, plain, (~ spl30_6 | ~ spl30_56 | ~ spl30_63 | spl30_104), inference(avatar_split_clause, [], [f2090, f980, f649, f619, f407])).
fof(f980, plain, (spl30_104 <=> (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_104])])).
fof(f2090, plain, (~ (e1 = op(e3, e2)) | (~ spl30_56 | ~ spl30_63 | spl30_104)), inference(backward_demodulation, [], [f2081, f621])).
fof(f2081, plain, (~ (e1 = op(op(e0, e2), e2)) | (~ spl30_63 | spl30_104)), inference(backward_demodulation, [], [f982, f651])).
fof(f982, plain, (~ (e1 = op(op(e0, op(e0, e0)), op(e0, e0))) | spl30_104), inference(avatar_component_clause, [], [f980])).
fof(f2087, plain, (~ spl30_56 | ~ spl30_63 | spl30_106), inference(avatar_split_clause, [], [f2083, f989, f649, f619])).
fof(f2083, plain, (~ (e3 = op(e0, e2)) | (~ spl30_63 | spl30_106)), inference(backward_demodulation, [], [f991, f651])).
fof(f991, plain, (~ (e3 = op(e0, op(e0, e0))) | spl30_106), inference(avatar_component_clause, [], [f989])).
fof(f2085, plain, (~ spl30_55 | ~ spl30_63), inference(avatar_split_clause, [], [f2079, f649, f615])).
fof(f615, plain, (spl30_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_55])])).
fof(f2079, plain, (~ (e2 = op(e0, e2)) | ~ spl30_63), inference(backward_demodulation, [], [f188, f651])).
fof(f2075, plain, (~ spl30_56 | ~ spl30_37 | ~ spl30_43 | spl30_117), inference(avatar_split_clause, [], [f2074, f1046, f564, f539, f619])).
fof(f1046, plain, (spl30_117 <=> (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_117])])).
fof(f2074, plain, (~ (e3 = op(e0, e2)) | (~ spl30_37 | ~ spl30_43 | spl30_117)), inference(forward_demodulation, [], [f2073, f541])).
fof(f2073, plain, (~ (e3 = op(op(e1, e2), e2)) | (~ spl30_43 | spl30_117)), inference(forward_demodulation, [], [f1048, f566])).
fof(f566, plain, ((e2 = op(e1, e1)) | ~ spl30_43), inference(avatar_component_clause, [], [f564])).
fof(f1048, plain, (~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1))) | spl30_117), inference(avatar_component_clause, [], [f1046])).
fof(f2062, plain, (~ spl30_64 | ~ spl30_48), inference(avatar_split_clause, [], [f2061, f585, f653])).
fof(f2061, plain, (~ (op(e0, e0) = e3) | ~ spl30_48), inference(forward_demodulation, [], [f163, f587])).
fof(f2059, plain, (~ spl30_62 | ~ spl30_58), inference(avatar_split_clause, [], [f2058, f628, f645])).
fof(f628, plain, (spl30_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_58])])).
fof(f2058, plain, (~ (op(e0, e0) = e1) | ~ spl30_58), inference(forward_demodulation, [], [f187, f630])).
fof(f630, plain, ((e1 = op(e0, e1)) | ~ spl30_58), inference(avatar_component_clause, [], [f628])).
fof(f187, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2045, plain, (~ spl30_19 | ~ spl30_3), inference(avatar_split_clause, [], [f2044, f394, f462])).
fof(f462, plain, (spl30_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_19])])).
fof(f2044, plain, (~ (e2 = op(e2, e3)) | ~ spl30_3), inference(forward_demodulation, [], [f186, f396])).
fof(f2036, plain, (~ spl30_8 | ~ spl30_12), inference(avatar_split_clause, [], [f2035, f432, f415])).
fof(f2035, plain, (~ (e3 = op(e3, e2)) | ~ spl30_12), inference(forward_demodulation, [], [f208, f434])).
fof(f434, plain, ((e3 = op(e3, e1)) | ~ spl30_12), inference(avatar_component_clause, [], [f432])).
fof(f2014, plain, (~ spl30_11 | ~ spl30_15), inference(avatar_split_clause, [], [f2012, f445, f428])).
fof(f445, plain, (spl30_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_15])])).
fof(f2012, plain, (~ (e2 = op(e3, e1)) | ~ spl30_15), inference(backward_demodulation, [], [f205, f447])).
fof(f447, plain, ((e2 = op(e3, e0)) | ~ spl30_15), inference(avatar_component_clause, [], [f445])).
fof(f1987, plain, (~ spl30_37 | ~ spl30_43 | spl30_114), inference(avatar_contradiction_clause, [], [f1986])).
fof(f1986, plain, ($false | (~ spl30_37 | ~ spl30_43 | spl30_114)), inference(subsumption_resolution, [], [f1977, f541])).
fof(f1977, plain, (~ (e0 = op(e1, e2)) | (~ spl30_43 | spl30_114)), inference(backward_demodulation, [], [f1030, f566])).
fof(f1030, plain, (~ (e0 = op(e1, op(e1, e1))) | spl30_114), inference(avatar_component_clause, [], [f1028])).
fof(f1028, plain, (spl30_114 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_114])])).
fof(f1981, plain, (~ spl30_11 | ~ spl30_43), inference(avatar_split_clause, [], [f1973, f564, f428])).
fof(f1973, plain, (~ (e2 = op(e3, e1)) | ~ spl30_43), inference(backward_demodulation, [], [f173, f566])).
fof(f1971, plain, (~ spl30_44 | ~ spl30_48), inference(avatar_split_clause, [], [f1967, f585, f568])).
fof(f1967, plain, (~ (e3 = op(e1, e1)) | ~ spl30_48), inference(backward_demodulation, [], [f193, f587])).
fof(f1965, plain, (~ spl30_49 | ~ spl30_52), inference(avatar_contradiction_clause, [], [f1964])).
fof(f1964, plain, ($false | (~ spl30_49 | ~ spl30_52)), inference(subsumption_resolution, [], [f1962, f213])).
fof(f1962, plain, ((e0 = e3) | (~ spl30_49 | ~ spl30_52)), inference(backward_demodulation, [], [f604, f592])).
fof(f1955, plain, (~ spl30_50 | ~ spl30_58), inference(avatar_split_clause, [], [f1950, f628, f594])).
fof(f1950, plain, (~ (e1 = op(e0, e3)) | ~ spl30_58), inference(backward_demodulation, [], [f191, f630])).
fof(f1954, plain, (~ spl30_26 | ~ spl30_58), inference(avatar_split_clause, [], [f1948, f628, f492])).
fof(f1948, plain, (~ (e1 = op(e2, e1)) | ~ spl30_58), inference(backward_demodulation, [], [f170, f630])).
fof(f1946, plain, (~ spl30_59 | ~ spl30_55), inference(avatar_split_clause, [], [f1945, f615, f632])).
fof(f1945, plain, (~ (e2 = op(e0, e1)) | ~ spl30_55), inference(forward_demodulation, [], [f190, f617])).
fof(f617, plain, ((e2 = op(e0, e2)) | ~ spl30_55), inference(avatar_component_clause, [], [f615])).
fof(f1942, plain, (~ spl30_59 | ~ spl30_62 | spl30_105), inference(avatar_split_clause, [], [f1870, f984, f645, f632])).
fof(f984, plain, (spl30_105 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_105])])).
fof(f1870, plain, (~ (e2 = op(e0, e1)) | (~ spl30_62 | spl30_105)), inference(backward_demodulation, [], [f986, f647])).
fof(f986, plain, (~ (e2 = op(e0, op(e0, e0))) | spl30_105), inference(avatar_component_clause, [], [f984])).
fof(f1940, plain, (~ spl30_50 | ~ spl30_62), inference(avatar_split_clause, [], [f1939, f645, f594])).
fof(f1939, plain, (~ (e1 = op(e0, e3)) | ~ spl30_62), inference(forward_demodulation, [], [f189, f647])).
fof(f1925, plain, (~ spl30_20 | ~ spl30_24), inference(avatar_split_clause, [], [f1924, f483, f466])).
fof(f1924, plain, (~ (e3 = op(e2, e3)) | ~ spl30_24), inference(forward_demodulation, [], [f204, f485])).
fof(f485, plain, ((e3 = op(e2, e2)) | ~ spl30_24), inference(avatar_component_clause, [], [f483])).
fof(f204, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1906, plain, (~ spl30_2 | ~ spl30_6), inference(avatar_split_clause, [], [f1904, f407, f390])).
fof(f1904, plain, (~ (e1 = op(e3, e3)) | ~ spl30_6), inference(backward_demodulation, [], [f210, f409])).
fof(f409, plain, ((e1 = op(e3, e2)) | ~ spl30_6), inference(avatar_component_clause, [], [f407])).
fof(f1902, plain, (~ spl30_8 | ~ spl30_16), inference(avatar_split_clause, [], [f1900, f449, f415])).
fof(f1900, plain, (~ (e3 = op(e3, e2)) | ~ spl30_16), inference(backward_demodulation, [], [f206, f451])).
fof(f206, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1892, plain, (~ spl30_8 | ~ spl30_24), inference(avatar_split_clause, [], [f1887, f483, f415])).
fof(f1887, plain, (~ (e3 = op(e3, e2)) | ~ spl30_24), inference(backward_demodulation, [], [f180, f485])).
fof(f1886, plain, (~ spl30_29 | ~ spl30_31), inference(avatar_contradiction_clause, [], [f1885])).
fof(f1885, plain, ($false | (~ spl30_29 | ~ spl30_31)), inference(subsumption_resolution, [], [f1883, f212])).
fof(f1883, plain, ((e0 = e2) | (~ spl30_29 | ~ spl30_31)), inference(backward_demodulation, [], [f515, f507])).
fof(f1882, plain, (~ spl30_57 | ~ spl30_60), inference(avatar_contradiction_clause, [], [f1881])).
fof(f1881, plain, ($false | (~ spl30_57 | ~ spl30_60)), inference(subsumption_resolution, [], [f1880, f213])).
fof(f1880, plain, ((e0 = e3) | (~ spl30_57 | ~ spl30_60)), inference(forward_demodulation, [], [f638, f626])).
fof(f1873, plain, (~ spl30_14 | ~ spl30_62), inference(avatar_split_clause, [], [f1866, f645, f441])).
fof(f1866, plain, (~ (e1 = op(e3, e0)) | ~ spl30_62), inference(backward_demodulation, [], [f165, f647])).
fof(f1845, plain, (~ spl30_1 | ~ spl30_13), inference(avatar_split_clause, [], [f1842, f437, f386])).
fof(f386, plain, (spl30_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_1])])).
fof(f1842, plain, (~ (e0 = op(e3, e3)) | ~ spl30_13), inference(backward_demodulation, [], [f207, f439])).
fof(f1840, plain, (~ spl30_17 | ~ spl30_20), inference(avatar_contradiction_clause, [], [f1839])).
fof(f1839, plain, ($false | (~ spl30_17 | ~ spl30_20)), inference(subsumption_resolution, [], [f1837, f213])).
fof(f1837, plain, ((e0 = e3) | (~ spl30_17 | ~ spl30_20)), inference(backward_demodulation, [], [f468, f456])).
fof(f468, plain, ((e3 = op(e2, e3)) | ~ spl30_20), inference(avatar_component_clause, [], [f466])).
fof(f1810, plain, (~ spl30_18 | ~ spl30_50), inference(avatar_split_clause, [], [f1803, f594, f458])).
fof(f1803, plain, (~ (e1 = op(e2, e3)) | ~ spl30_50), inference(backward_demodulation, [], [f182, f596])).
fof(f1802, plain, (~ spl30_23 | ~ spl30_55), inference(avatar_split_clause, [], [f1801, f615, f479])).
fof(f1801, plain, (~ (e2 = op(e2, e2)) | ~ spl30_55), inference(backward_demodulation, [], [f176, f617])).
fof(f1796, plain, (~ spl30_52 | ~ spl30_64), inference(avatar_split_clause, [], [f1789, f653, f602])).
fof(f1789, plain, (~ (e3 = op(e0, e3)) | ~ spl30_64), inference(backward_demodulation, [], [f189, f655])).
fof(f655, plain, ((op(e0, e0) = e3) | ~ spl30_64), inference(avatar_component_clause, [], [f653])).
fof(f1795, plain, (~ spl30_56 | ~ spl30_64), inference(avatar_split_clause, [], [f1788, f653, f619])).
fof(f1788, plain, (~ (e3 = op(e0, e2)) | ~ spl30_64), inference(backward_demodulation, [], [f188, f655])).
fof(f1780, plain, (~ spl30_30 | ~ spl30_46), inference(avatar_split_clause, [], [f1779, f577, f509])).
fof(f1779, plain, (~ (e1 = op(e2, e0)) | ~ spl30_46), inference(forward_demodulation, [], [f166, f579])).
fof(f166, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1775, plain, (~ spl30_18 | ~ spl30_26), inference(avatar_split_clause, [], [f1774, f492, f458])).
fof(f1774, plain, (~ (e1 = op(e2, e3)) | ~ spl30_26), inference(forward_demodulation, [], [f203, f494])).
fof(f1773, plain, (~ spl30_19 | ~ spl30_35), inference(avatar_split_clause, [], [f1772, f530, f462])).
fof(f1772, plain, (~ (e2 = op(e2, e3)) | ~ spl30_35), inference(forward_demodulation, [], [f184, f532])).
fof(f532, plain, ((e2 = op(e1, e3)) | ~ spl30_35), inference(avatar_component_clause, [], [f530])).
fof(f1770, plain, (~ spl30_20 | ~ spl30_35 | ~ spl30_44 | spl30_117), inference(avatar_split_clause, [], [f1678, f1046, f568, f530, f466])).
fof(f1678, plain, (~ (e3 = op(e2, e3)) | (~ spl30_35 | ~ spl30_44 | spl30_117)), inference(forward_demodulation, [], [f1672, f532])).
fof(f1672, plain, (~ (e3 = op(op(e1, e3), e3)) | (~ spl30_44 | spl30_117)), inference(backward_demodulation, [], [f1048, f570])).
fof(f1769, plain, (~ spl30_17 | ~ spl30_35 | ~ spl30_44 | spl30_95), inference(avatar_split_clause, [], [f1727, f938, f568, f530, f454])).
fof(f1727, plain, (~ (e0 = op(e2, e3)) | (~ spl30_35 | ~ spl30_44 | spl30_95)), inference(forward_demodulation, [], [f1726, f532])).
fof(f1726, plain, (~ (e0 = op(op(e1, e3), e3)) | (~ spl30_44 | spl30_95)), inference(forward_demodulation, [], [f940, f570])).
fof(f1767, plain, (~ spl30_14 | ~ spl30_46), inference(avatar_split_clause, [], [f1586, f577, f441])).
fof(f1586, plain, (~ (e1 = op(e3, e0)) | ~ spl30_46), inference(forward_demodulation, [], [f167, f579])).
fof(f1754, plain, (~ spl30_23 | ~ spl30_24), inference(avatar_contradiction_clause, [], [f1753])).
fof(f1753, plain, ($false | (~ spl30_23 | ~ spl30_24)), inference(subsumption_resolution, [], [f1752, f216])).
fof(f216, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1752, plain, ((e2 = e3) | (~ spl30_23 | ~ spl30_24)), inference(forward_demodulation, [], [f485, f481])).
fof(f1751, plain, (~ spl30_10 | ~ spl30_26), inference(avatar_split_clause, [], [f1749, f492, f424])).
fof(f1749, plain, (~ (e1 = op(e3, e1)) | ~ spl30_26), inference(backward_demodulation, [], [f174, f494])).
fof(f1746, plain, (~ spl30_8 | ~ spl30_56), inference(avatar_split_clause, [], [f1743, f619, f415])).
fof(f1743, plain, (~ (e3 = op(e3, e2)) | ~ spl30_56), inference(backward_demodulation, [], [f177, f621])).
fof(f1739, plain, (~ spl30_57 | ~ spl30_58), inference(avatar_contradiction_clause, [], [f1738])).
fof(f1738, plain, ($false | (~ spl30_57 | ~ spl30_58)), inference(subsumption_resolution, [], [f1737, f211])).
fof(f1737, plain, ((e0 = e1) | (~ spl30_57 | ~ spl30_58)), inference(forward_demodulation, [], [f630, f626])).
fof(f1734, plain, (~ spl30_15 | ~ spl30_63), inference(avatar_split_clause, [], [f1728, f649, f445])).
fof(f1728, plain, (~ (e2 = op(e3, e0)) | ~ spl30_63), inference(backward_demodulation, [], [f165, f651])).
fof(f1716, plain, (~ spl30_56 | ~ spl30_52), inference(avatar_split_clause, [], [f1715, f602, f619])).
fof(f1715, plain, (~ (e3 = op(e0, e2)) | ~ spl30_52), inference(forward_demodulation, [], [f192, f604])).
fof(f1707, plain, (~ spl30_15 | ~ spl30_1 | spl30_103), inference(avatar_split_clause, [], [f1694, f975, f386, f445])).
fof(f1694, plain, (~ (e2 = op(e3, e0)) | (~ spl30_1 | spl30_103)), inference(backward_demodulation, [], [f977, f388])).
fof(f388, plain, ((e0 = op(e3, e3)) | ~ spl30_1), inference(avatar_component_clause, [], [f386])).
fof(f977, plain, (~ (e2 = op(e3, op(e3, e3))) | spl30_103), inference(avatar_component_clause, [], [f975])).
fof(f1684, plain, (~ spl30_37 | ~ spl30_40), inference(avatar_contradiction_clause, [], [f1683])).
fof(f1683, plain, ($false | (~ spl30_37 | ~ spl30_40)), inference(subsumption_resolution, [], [f1682, f213])).
fof(f1682, plain, ((e0 = e3) | (~ spl30_37 | ~ spl30_40)), inference(backward_demodulation, [], [f553, f541])).
fof(f1674, plain, (~ spl30_35 | ~ spl30_44 | spl30_96), inference(avatar_contradiction_clause, [], [f1673])).
fof(f1673, plain, ($false | (~ spl30_35 | ~ spl30_44 | spl30_96)), inference(subsumption_resolution, [], [f1669, f532])).
fof(f1669, plain, (~ (e2 = op(e1, e3)) | (~ spl30_44 | spl30_96)), inference(backward_demodulation, [], [f944, f570])).
fof(f944, plain, (~ (e2 = op(e1, op(e1, e1))) | spl30_96), inference(avatar_component_clause, [], [f942])).
fof(f1663, plain, (~ spl30_57 | ~ spl30_59), inference(avatar_contradiction_clause, [], [f1662])).
fof(f1662, plain, ($false | (~ spl30_57 | ~ spl30_59)), inference(subsumption_resolution, [], [f1661, f212])).
fof(f1661, plain, ((e0 = e2) | (~ spl30_57 | ~ spl30_59)), inference(forward_demodulation, [], [f634, f626])).
fof(f1646, plain, (~ spl30_51 | ~ spl30_35), inference(avatar_split_clause, [], [f1645, f530, f598])).
fof(f1645, plain, (~ (e2 = op(e0, e3)) | ~ spl30_35), inference(forward_demodulation, [], [f181, f532])).
fof(f1636, plain, (~ spl30_34 | ~ spl30_18), inference(avatar_split_clause, [], [f1635, f458, f526])).
fof(f1635, plain, (~ (e1 = op(e1, e3)) | ~ spl30_18), inference(forward_demodulation, [], [f184, f460])).
fof(f1617, plain, (~ spl30_5 | ~ spl30_8), inference(avatar_contradiction_clause, [], [f1616])).
fof(f1616, plain, ($false | (~ spl30_5 | ~ spl30_8)), inference(subsumption_resolution, [], [f1615, f213])).
fof(f1615, plain, ((e0 = e3) | (~ spl30_5 | ~ spl30_8)), inference(forward_demodulation, [], [f417, f405])).
fof(f417, plain, ((e3 = op(e3, e2)) | ~ spl30_8), inference(avatar_component_clause, [], [f415])).
fof(f1608, plain, (~ spl30_18 | ~ spl30_20), inference(avatar_contradiction_clause, [], [f1607])).
fof(f1607, plain, ($false | (~ spl30_18 | ~ spl30_20)), inference(subsumption_resolution, [], [f1606, f215])).
fof(f1606, plain, ((e1 = e3) | (~ spl30_18 | ~ spl30_20)), inference(backward_demodulation, [], [f468, f460])).
fof(f1603, plain, (~ spl30_33 | ~ spl30_35), inference(avatar_contradiction_clause, [], [f1602])).
fof(f1602, plain, ($false | (~ spl30_33 | ~ spl30_35)), inference(subsumption_resolution, [], [f1601, f212])).
fof(f1601, plain, ((e0 = e2) | (~ spl30_33 | ~ spl30_35)), inference(forward_demodulation, [], [f532, f524])).
fof(f1595, plain, (~ spl30_18 | ~ spl30_51 | ~ spl30_64 | spl30_104), inference(avatar_split_clause, [], [f1594, f980, f653, f598, f458])).
fof(f1594, plain, (~ (e1 = op(e2, e3)) | (~ spl30_51 | ~ spl30_64 | spl30_104)), inference(forward_demodulation, [], [f1593, f600])).
fof(f1593, plain, (~ (e1 = op(op(e0, e3), e3)) | (~ spl30_64 | spl30_104)), inference(forward_demodulation, [], [f982, f655])).
fof(f1557, plain, (~ spl30_11 | ~ spl30_2 | spl30_103), inference(avatar_split_clause, [], [f1556, f975, f390, f428])).
fof(f1556, plain, (~ (e2 = op(e3, e1)) | (~ spl30_2 | spl30_103)), inference(forward_demodulation, [], [f977, f392])).
fof(f1555, plain, (~ spl30_1 | ~ spl30_2), inference(avatar_contradiction_clause, [], [f1554])).
fof(f1554, plain, ($false | (~ spl30_1 | ~ spl30_2)), inference(subsumption_resolution, [], [f1553, f211])).
fof(f1553, plain, ((e0 = e1) | (~ spl30_1 | ~ spl30_2)), inference(forward_demodulation, [], [f392, f388])).
fof(f1552, plain, (~ spl30_5 | ~ spl30_7), inference(avatar_contradiction_clause, [], [f1551])).
fof(f1551, plain, ($false | (~ spl30_5 | ~ spl30_7)), inference(subsumption_resolution, [], [f1550, f212])).
fof(f1550, plain, ((e0 = e2) | (~ spl30_5 | ~ spl30_7)), inference(backward_demodulation, [], [f413, f405])).
fof(f1530, plain, (~ spl30_46 | ~ spl30_47), inference(avatar_contradiction_clause, [], [f1529])).
fof(f1529, plain, ($false | (~ spl30_46 | ~ spl30_47)), inference(subsumption_resolution, [], [f1528, f214])).
fof(f1528, plain, ((e1 = e2) | (~ spl30_46 | ~ spl30_47)), inference(backward_demodulation, [], [f583, f579])).
fof(f583, plain, ((e2 = op(e1, e0)) | ~ spl30_47), inference(avatar_component_clause, [], [f581])).
fof(f1501, plain, (~ spl30_51 | ~ spl30_64 | spl30_105), inference(avatar_contradiction_clause, [], [f1500])).
fof(f1500, plain, ($false | (~ spl30_51 | ~ spl30_64 | spl30_105)), inference(subsumption_resolution, [], [f1499, f600])).
fof(f1499, plain, (~ (e2 = op(e0, e3)) | (~ spl30_64 | spl30_105)), inference(forward_demodulation, [], [f986, f655])).
fof(f1498, plain, (~ spl30_58 | ~ spl30_42), inference(avatar_split_clause, [], [f1497, f560, f628])).
fof(f1497, plain, (~ (e1 = op(e0, e1)) | ~ spl30_42), inference(forward_demodulation, [], [f169, f562])).
fof(f1495, plain, (~ spl30_58 | ~ spl30_54), inference(avatar_split_clause, [], [f1494, f611, f628])).
fof(f1494, plain, (~ (e1 = op(e0, e1)) | ~ spl30_54), inference(forward_demodulation, [], [f190, f613])).
fof(f1490, plain, (~ spl30_1 | ~ spl30_33), inference(avatar_contradiction_clause, [], [f1489])).
fof(f1489, plain, ($false | (~ spl30_1 | ~ spl30_33)), inference(subsumption_resolution, [], [f1488, f524])).
fof(f1488, plain, (~ (e0 = op(e1, e3)) | ~ spl30_1), inference(forward_demodulation, [], [f185, f388])).
fof(f1457, plain, (~ spl30_15 | ~ spl30_47), inference(avatar_split_clause, [], [f1456, f581, f445])).
fof(f1456, plain, (~ (e2 = op(e3, e0)) | ~ spl30_47), inference(forward_demodulation, [], [f167, f583])).
fof(f1453, plain, (~ spl30_9 | ~ spl30_1), inference(avatar_split_clause, [], [f1452, f386, f420])).
fof(f1452, plain, (~ (e0 = op(e3, e1)) | ~ spl30_1), inference(forward_demodulation, [], [f209, f388])).
fof(f1448, plain, (~ spl30_5 | ~ spl30_1), inference(avatar_split_clause, [], [f1305, f386, f403])).
fof(f1305, plain, (~ (e0 = op(e3, e2)) | ~ spl30_1), inference(forward_demodulation, [], [f210, f388])).
fof(f1447, plain, (~ spl30_9 | ~ spl30_12), inference(avatar_contradiction_clause, [], [f1446])).
fof(f1446, plain, ($false | (~ spl30_9 | ~ spl30_12)), inference(subsumption_resolution, [], [f1444, f213])).
fof(f1444, plain, ((e0 = e3) | (~ spl30_9 | ~ spl30_12)), inference(backward_demodulation, [], [f434, f422])).
fof(f1441, plain, (~ spl30_21 | ~ spl30_23), inference(avatar_contradiction_clause, [], [f1440])).
fof(f1440, plain, ($false | (~ spl30_21 | ~ spl30_23)), inference(subsumption_resolution, [], [f1439, f212])).
fof(f1439, plain, ((e0 = e2) | (~ spl30_21 | ~ spl30_23)), inference(forward_demodulation, [], [f481, f473])).
fof(f473, plain, ((e0 = op(e2, e2)) | ~ spl30_21), inference(avatar_component_clause, [], [f471])).
fof(f471, plain, (spl30_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_21])])).
fof(f1433, plain, (~ spl30_33 | ~ spl30_34), inference(avatar_contradiction_clause, [], [f1432])).
fof(f1432, plain, ($false | (~ spl30_33 | ~ spl30_34)), inference(subsumption_resolution, [], [f1431, f211])).
fof(f1431, plain, ((e0 = e1) | (~ spl30_33 | ~ spl30_34)), inference(backward_demodulation, [], [f528, f524])).
fof(f528, plain, ((e1 = op(e1, e3)) | ~ spl30_34), inference(avatar_component_clause, [], [f526])).
fof(f1415, plain, (~ spl30_61 | ~ spl30_64), inference(avatar_contradiction_clause, [], [f1414])).
fof(f1414, plain, ($false | (~ spl30_61 | ~ spl30_64)), inference(subsumption_resolution, [], [f1413, f213])).
fof(f1413, plain, ((e0 = e3) | (~ spl30_61 | ~ spl30_64)), inference(forward_demodulation, [], [f655, f643])).
fof(f1406, plain, (~ spl30_1 | ~ spl30_14 | ~ spl30_47 | spl30_115), inference(avatar_contradiction_clause, [], [f1405])).
fof(f1405, plain, ($false | (~ spl30_1 | ~ spl30_14 | ~ spl30_47 | spl30_115)), inference(subsumption_resolution, [], [f1404, f583])).
fof(f1404, plain, (~ (e2 = op(e1, e0)) | (~ spl30_1 | ~ spl30_14 | spl30_115)), inference(forward_demodulation, [], [f1403, f443])).
fof(f1403, plain, (~ (e2 = op(op(e3, e0), e0)) | (~ spl30_1 | spl30_115)), inference(forward_demodulation, [], [f1036, f388])).
fof(f1393, plain, (~ spl30_55 | ~ spl30_7), inference(avatar_split_clause, [], [f1299, f411, f615])).
fof(f1299, plain, (~ (e2 = op(e0, e2)) | ~ spl30_7), inference(forward_demodulation, [], [f177, f413])).
fof(f1392, plain, (~ spl30_55 | ~ spl30_51), inference(avatar_split_clause, [], [f1391, f598, f615])).
fof(f1391, plain, (~ (e2 = op(e0, e2)) | ~ spl30_51), inference(forward_demodulation, [], [f192, f600])).
fof(f1390, plain, (~ spl30_31 | ~ spl30_47), inference(avatar_split_clause, [], [f1389, f581, f513])).
fof(f1389, plain, (~ (e2 = op(e2, e0)) | ~ spl30_47), inference(forward_demodulation, [], [f166, f583])).
fof(f1384, plain, (~ spl30_42 | ~ spl30_34), inference(avatar_split_clause, [], [f1383, f526, f560])).
fof(f1383, plain, (~ (e1 = op(e1, e1)) | ~ spl30_34), inference(forward_demodulation, [], [f197, f528])).
fof(f1362, plain, (~ spl30_15 | ~ spl30_31), inference(avatar_split_clause, [], [f1358, f513, f445])).
fof(f1358, plain, (~ (e2 = op(e3, e0)) | ~ spl30_31), inference(backward_demodulation, [], [f168, f515])).
fof(f1361, plain, (~ spl30_19 | ~ spl30_31), inference(avatar_split_clause, [], [f1357, f513, f462])).
fof(f1357, plain, (~ (e2 = op(e2, e3)) | ~ spl30_31), inference(backward_demodulation, [], [f201, f515])).
fof(f1352, plain, (~ spl30_47 | ~ spl30_48), inference(avatar_contradiction_clause, [], [f1351])).
fof(f1351, plain, ($false | (~ spl30_47 | ~ spl30_48)), inference(subsumption_resolution, [], [f1350, f216])).
fof(f1350, plain, ((e2 = e3) | (~ spl30_47 | ~ spl30_48)), inference(backward_demodulation, [], [f587, f583])).
fof(f1349, plain, (~ spl30_32 | ~ spl30_48), inference(avatar_split_clause, [], [f1346, f585, f517])).
fof(f1346, plain, (~ (e3 = op(e2, e0)) | ~ spl30_48), inference(backward_demodulation, [], [f166, f587])).
fof(f1348, plain, (~ spl30_15 | ~ spl30_41 | ~ spl30_48 | spl30_113), inference(avatar_split_clause, [], [f1344, f1024, f585, f556, f445])).
fof(f1024, plain, (spl30_113 <=> (e2 = op(op(e1, op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_113])])).
fof(f1344, plain, (~ (e2 = op(e3, e0)) | (~ spl30_41 | ~ spl30_48 | spl30_113)), inference(backward_demodulation, [], [f1330, f587])).
fof(f1330, plain, (~ (e2 = op(op(e1, e0), e0)) | (~ spl30_41 | spl30_113)), inference(forward_demodulation, [], [f1026, f558])).
fof(f1026, plain, (~ (e2 = op(op(e1, op(e1, e1)), op(e1, e1))) | spl30_113), inference(avatar_component_clause, [], [f1024])).
fof(f1343, plain, (~ spl30_19 | ~ spl30_51), inference(avatar_split_clause, [], [f1342, f598, f462])).
fof(f1342, plain, (~ (e2 = op(e2, e3)) | ~ spl30_51), inference(backward_demodulation, [], [f182, f600])).
fof(f1339, plain, (~ spl30_58 | ~ spl30_60), inference(avatar_contradiction_clause, [], [f1338])).
fof(f1338, plain, ($false | (~ spl30_58 | ~ spl30_60)), inference(subsumption_resolution, [], [f1337, f215])).
fof(f1337, plain, ((e1 = e3) | (~ spl30_58 | ~ spl30_60)), inference(backward_demodulation, [], [f638, f630])).
fof(f1332, plain, (~ spl30_32 | ~ spl30_21 | spl30_100), inference(avatar_split_clause, [], [f1331, f961, f471, f517])).
fof(f961, plain, (spl30_100 <=> (e3 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_100])])).
fof(f1331, plain, (~ (e3 = op(e2, e0)) | (~ spl30_21 | spl30_100)), inference(forward_demodulation, [], [f963, f473])).
fof(f963, plain, (~ (e3 = op(e2, op(e2, e2))) | spl30_100), inference(avatar_component_clause, [], [f961])).
fof(f1328, plain, (~ spl30_50 | ~ spl30_34), inference(avatar_split_clause, [], [f1327, f526, f594])).
fof(f1327, plain, (~ (e1 = op(e0, e3)) | ~ spl30_34), inference(forward_demodulation, [], [f181, f528])).
fof(f1317, plain, (~ spl30_19 | ~ spl30_27), inference(avatar_split_clause, [], [f1316, f496, f462])).
fof(f1316, plain, (~ (e2 = op(e2, e3)) | ~ spl30_27), inference(backward_demodulation, [], [f203, f498])).
fof(f1312, plain, (~ spl30_14 | ~ spl30_1 | spl30_102), inference(avatar_split_clause, [], [f1292, f970, f386, f441])).
fof(f970, plain, (spl30_102 <=> (e1 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_102])])).
fof(f1292, plain, (~ (e1 = op(e3, e0)) | (~ spl30_1 | spl30_102)), inference(backward_demodulation, [], [f972, f388])).
fof(f972, plain, (~ (e1 = op(e3, op(e3, e3))) | spl30_102), inference(avatar_component_clause, [], [f970])).
fof(f1307, plain, (~ spl30_12 | ~ spl30_60), inference(avatar_split_clause, [], [f1306, f636, f432])).
fof(f1306, plain, (~ (e3 = op(e3, e1)) | ~ spl30_60), inference(forward_demodulation, [], [f171, f638])).
fof(f1304, plain, (~ spl30_15 | ~ spl30_7), inference(avatar_split_clause, [], [f1303, f411, f445])).
fof(f1303, plain, (~ (e2 = op(e3, e0)) | ~ spl30_7), inference(forward_demodulation, [], [f206, f413])).
fof(f1297, plain, (~ spl30_30 | ~ spl30_1 | ~ spl30_15 | spl30_109), inference(avatar_split_clause, [], [f1296, f1004, f445, f386, f509])).
fof(f1296, plain, (~ (e1 = op(e2, e0)) | (~ spl30_1 | ~ spl30_15 | spl30_109)), inference(forward_demodulation, [], [f1293, f447])).
fof(f1293, plain, (~ (e1 = op(op(e3, e0), e0)) | (~ spl30_1 | spl30_109)), inference(backward_demodulation, [], [f1006, f388])).
fof(f1290, plain, (~ spl30_6 | ~ spl30_7), inference(avatar_contradiction_clause, [], [f1289])).
fof(f1289, plain, ($false | (~ spl30_6 | ~ spl30_7)), inference(subsumption_resolution, [], [f1288, f214])).
fof(f1288, plain, ((e1 = e2) | (~ spl30_6 | ~ spl30_7)), inference(forward_demodulation, [], [f413, f409])).
fof(f1283, plain, (~ spl30_26 | ~ spl30_27), inference(avatar_contradiction_clause, [], [f1282])).
fof(f1282, plain, ($false | (~ spl30_26 | ~ spl30_27)), inference(subsumption_resolution, [], [f1281, f214])).
fof(f1281, plain, ((e1 = e2) | (~ spl30_26 | ~ spl30_27)), inference(backward_demodulation, [], [f498, f494])).
fof(f1279, plain, (~ spl30_14 | ~ spl30_21 | ~ spl30_32 | spl30_107), inference(avatar_split_clause, [], [f1276, f994, f517, f471, f441])).
fof(f994, plain, (spl30_107 <=> (e1 = op(op(e2, op(e2, e2)), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_107])])).
fof(f1276, plain, (~ (e1 = op(e3, e0)) | (~ spl30_21 | ~ spl30_32 | spl30_107)), inference(backward_demodulation, [], [f1157, f519])).
fof(f1157, plain, (~ (e1 = op(op(e2, e0), e0)) | (~ spl30_21 | spl30_107)), inference(backward_demodulation, [], [f996, f473])).
fof(f996, plain, (~ (e1 = op(op(e2, op(e2, e2)), op(e2, e2))) | spl30_107), inference(avatar_component_clause, [], [f994])).
fof(f1274, plain, (~ spl30_34 | ~ spl30_35), inference(avatar_contradiction_clause, [], [f1273])).
fof(f1273, plain, ($false | (~ spl30_34 | ~ spl30_35)), inference(subsumption_resolution, [], [f1272, f214])).
fof(f1272, plain, ((e1 = e2) | (~ spl30_34 | ~ spl30_35)), inference(forward_demodulation, [], [f532, f528])).
fof(f1270, plain, (~ spl30_38 | ~ spl30_46), inference(avatar_split_clause, [], [f1268, f577, f543])).
fof(f1268, plain, (~ (e1 = op(e1, e2)) | ~ spl30_46), inference(backward_demodulation, [], [f194, f579])).
fof(f1265, plain, (~ spl30_39 | ~ spl30_55), inference(avatar_split_clause, [], [f1264, f615, f547])).
fof(f1264, plain, (~ (e2 = op(e1, e2)) | ~ spl30_55), inference(backward_demodulation, [], [f175, f617])).
fof(f1259, plain, (~ spl30_57 | ~ spl30_41), inference(avatar_split_clause, [], [f1258, f556, f624])).
fof(f1258, plain, (~ (e0 = op(e0, e1)) | ~ spl30_41), inference(forward_demodulation, [], [f169, f558])).
fof(f1257, plain, (~ spl30_58 | ~ spl30_10), inference(avatar_split_clause, [], [f1256, f424, f628])).
fof(f1256, plain, (~ (e1 = op(e0, e1)) | ~ spl30_10), inference(forward_demodulation, [], [f171, f426])).
fof(f1253, plain, (~ spl30_53 | ~ spl30_21), inference(avatar_split_clause, [], [f1252, f471, f607])).
fof(f1252, plain, (~ (e0 = op(e0, e2)) | ~ spl30_21), inference(forward_demodulation, [], [f176, f473])).
fof(f1251, plain, (~ spl30_54 | ~ spl30_6), inference(avatar_split_clause, [], [f1250, f407, f611])).
fof(f1250, plain, (~ (e1 = op(e0, e2)) | ~ spl30_6), inference(forward_demodulation, [], [f177, f409])).
fof(f1247, plain, (~ spl30_50 | ~ spl30_52), inference(avatar_contradiction_clause, [], [f1246])).
fof(f1246, plain, ($false | (~ spl30_50 | ~ spl30_52)), inference(subsumption_resolution, [], [f1245, f215])).
fof(f1245, plain, ((e1 = e3) | (~ spl30_50 | ~ spl30_52)), inference(forward_demodulation, [], [f604, f596])).
fof(f1240, plain, (~ spl30_46 | ~ spl30_34), inference(avatar_split_clause, [], [f1239, f526, f577])).
fof(f1239, plain, (~ (e1 = op(e1, e0)) | ~ spl30_34), inference(forward_demodulation, [], [f195, f528])).
fof(f195, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1238, plain, (~ spl30_47 | ~ spl30_41 | spl30_96), inference(avatar_split_clause, [], [f1118, f942, f556, f581])).
fof(f1118, plain, (~ (e2 = op(e1, e0)) | (~ spl30_41 | spl30_96)), inference(backward_demodulation, [], [f944, f558])).
fof(f1237, plain, (~ spl30_48 | ~ spl30_41 | spl30_97), inference(avatar_split_clause, [], [f1119, f947, f556, f585])).
fof(f947, plain, (spl30_97 <=> (e3 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_97])])).
fof(f1119, plain, (~ (e3 = op(e1, e0)) | (~ spl30_41 | spl30_97)), inference(backward_demodulation, [], [f949, f558])).
fof(f949, plain, (~ (e3 = op(e1, op(e1, e1))) | spl30_97), inference(avatar_component_clause, [], [f947])).
fof(f1236, plain, (~ spl30_37 | ~ spl30_21), inference(avatar_split_clause, [], [f1235, f471, f539])).
fof(f1235, plain, (~ (e0 = op(e1, e2)) | ~ spl30_21), inference(forward_demodulation, [], [f178, f473])).
fof(f1234, plain, (~ spl30_38 | ~ spl30_6), inference(avatar_split_clause, [], [f1233, f407, f543])).
fof(f1233, plain, (~ (e1 = op(e1, e2)) | ~ spl30_6), inference(forward_demodulation, [], [f179, f409])).
fof(f1225, plain, (~ spl30_29 | ~ spl30_21), inference(avatar_split_clause, [], [f1224, f471, f505])).
fof(f1224, plain, (~ (e0 = op(e2, e0)) | ~ spl30_21), inference(forward_demodulation, [], [f200, f473])).
fof(f1219, plain, (~ spl30_25 | ~ spl30_21), inference(avatar_split_clause, [], [f1218, f471, f488])).
fof(f1218, plain, (~ (e0 = op(e2, e1)) | ~ spl30_21), inference(forward_demodulation, [], [f202, f473])).
fof(f1214, plain, (~ spl30_14 | ~ spl30_10), inference(avatar_split_clause, [], [f1213, f424, f441])).
fof(f1213, plain, (~ (e1 = op(e3, e0)) | ~ spl30_10), inference(forward_demodulation, [], [f205, f426])).
fof(f1209, plain, (~ spl30_10 | ~ spl30_12), inference(avatar_contradiction_clause, [], [f1208])).
fof(f1208, plain, ($false | (~ spl30_10 | ~ spl30_12)), inference(subsumption_resolution, [], [f1207, f215])).
fof(f1207, plain, ((e1 = e3) | (~ spl30_10 | ~ spl30_12)), inference(forward_demodulation, [], [f434, f426])).
fof(f1205, plain, (~ spl30_5 | ~ spl30_6), inference(avatar_contradiction_clause, [], [f1204])).
fof(f1204, plain, ($false | (~ spl30_5 | ~ spl30_6)), inference(subsumption_resolution, [], [f1203, f211])).
fof(f1203, plain, ((e0 = e1) | (~ spl30_5 | ~ spl30_6)), inference(backward_demodulation, [], [f409, f405])).
fof(f1202, plain, (~ spl30_6 | ~ spl30_8), inference(avatar_contradiction_clause, [], [f1201])).
fof(f1201, plain, ($false | (~ spl30_6 | ~ spl30_8)), inference(subsumption_resolution, [], [f1200, f215])).
fof(f1200, plain, ((e1 = e3) | (~ spl30_6 | ~ spl30_8)), inference(backward_demodulation, [], [f417, f409])).
fof(f1199, plain, (~ spl30_4 | ~ spl30_8), inference(avatar_split_clause, [], [f1198, f415, f398])).
fof(f1198, plain, (~ (e3 = op(e3, e3)) | ~ spl30_8), inference(backward_demodulation, [], [f210, f417])).
fof(f1194, plain, (~ spl30_11 | ~ spl30_12), inference(avatar_contradiction_clause, [], [f1193])).
fof(f1193, plain, ($false | (~ spl30_11 | ~ spl30_12)), inference(subsumption_resolution, [], [f1192, f216])).
fof(f1192, plain, ((e2 = e3) | (~ spl30_11 | ~ spl30_12)), inference(backward_demodulation, [], [f434, f430])).
fof(f1191, plain, (~ spl30_4 | ~ spl30_12), inference(avatar_split_clause, [], [f1189, f432, f398])).
fof(f1189, plain, (~ (e3 = op(e3, e3)) | ~ spl30_12), inference(backward_demodulation, [], [f209, f434])).
fof(f1187, plain, (~ spl30_14 | ~ spl30_15), inference(avatar_contradiction_clause, [], [f1186])).
fof(f1186, plain, ($false | (~ spl30_14 | ~ spl30_15)), inference(subsumption_resolution, [], [f1185, f214])).
fof(f1185, plain, ((e1 = e2) | (~ spl30_14 | ~ spl30_15)), inference(backward_demodulation, [], [f447, f443])).
fof(f1184, plain, (~ spl30_15 | ~ spl30_16), inference(avatar_contradiction_clause, [], [f1183])).
fof(f1183, plain, ($false | (~ spl30_15 | ~ spl30_16)), inference(subsumption_resolution, [], [f1182, f216])).
fof(f1182, plain, ((e2 = e3) | (~ spl30_15 | ~ spl30_16)), inference(backward_demodulation, [], [f451, f447])).
fof(f1181, plain, (~ spl30_4 | ~ spl30_16), inference(avatar_split_clause, [], [f1178, f449, f398])).
fof(f1178, plain, (~ (e3 = op(e3, e3)) | ~ spl30_16), inference(backward_demodulation, [], [f207, f451])).
fof(f1175, plain, (~ spl30_17 | ~ spl30_18), inference(avatar_contradiction_clause, [], [f1174])).
fof(f1174, plain, ($false | (~ spl30_17 | ~ spl30_18)), inference(subsumption_resolution, [], [f1173, f211])).
fof(f1173, plain, ((e0 = e1) | (~ spl30_17 | ~ spl30_18)), inference(backward_demodulation, [], [f460, f456])).
fof(f1172, plain, (~ spl30_18 | ~ spl30_19), inference(avatar_contradiction_clause, [], [f1171])).
fof(f1171, plain, ($false | (~ spl30_18 | ~ spl30_19)), inference(subsumption_resolution, [], [f1170, f214])).
fof(f1170, plain, ((e1 = e2) | (~ spl30_18 | ~ spl30_19)), inference(backward_demodulation, [], [f464, f460])).
fof(f464, plain, ((e2 = op(e2, e3)) | ~ spl30_19), inference(avatar_component_clause, [], [f462])).
fof(f1169, plain, (~ spl30_19 | ~ spl30_20), inference(avatar_contradiction_clause, [], [f1168])).
fof(f1168, plain, ($false | (~ spl30_19 | ~ spl30_20)), inference(subsumption_resolution, [], [f1167, f216])).
fof(f1167, plain, ((e2 = e3) | (~ spl30_19 | ~ spl30_20)), inference(backward_demodulation, [], [f468, f464])).
fof(f1166, plain, (~ spl30_4 | ~ spl30_20), inference(avatar_split_clause, [], [f1165, f466, f398])).
fof(f1165, plain, (~ (e3 = op(e3, e3)) | ~ spl30_20), inference(backward_demodulation, [], [f186, f468])).
fof(f1160, plain, (~ spl30_17 | ~ spl30_21), inference(avatar_split_clause, [], [f1154, f471, f454])).
fof(f1154, plain, (~ (e0 = op(e2, e3)) | ~ spl30_21), inference(backward_demodulation, [], [f204, f473])).
fof(f1159, plain, (~ spl30_5 | ~ spl30_21), inference(avatar_split_clause, [], [f1153, f471, f403])).
fof(f1153, plain, (~ (e0 = op(e3, e2)) | ~ spl30_21), inference(backward_demodulation, [], [f180, f473])).
fof(f1152, plain, (~ spl30_17 | ~ spl30_25), inference(avatar_split_clause, [], [f1149, f488, f454])).
fof(f1149, plain, (~ (e0 = op(e2, e3)) | ~ spl30_25), inference(backward_demodulation, [], [f203, f490])).
fof(f1150, plain, (~ spl30_9 | ~ spl30_25), inference(avatar_split_clause, [], [f1147, f488, f420])).
fof(f1147, plain, (~ (e0 = op(e3, e1)) | ~ spl30_25), inference(backward_demodulation, [], [f174, f490])).
fof(f1144, plain, (~ spl30_25 | ~ spl30_29), inference(avatar_split_clause, [], [f1140, f505, f488])).
fof(f1140, plain, (~ (e0 = op(e2, e1)) | ~ spl30_29), inference(backward_demodulation, [], [f199, f507])).
fof(f1143, plain, (~ spl30_13 | ~ spl30_29), inference(avatar_split_clause, [], [f1139, f505, f437])).
fof(f1139, plain, (~ (e0 = op(e3, e0)) | ~ spl30_29), inference(backward_demodulation, [], [f168, f507])).
fof(f1138, plain, (~ spl30_34 | ~ spl30_36), inference(avatar_contradiction_clause, [], [f1137])).
fof(f1137, plain, ($false | (~ spl30_34 | ~ spl30_36)), inference(subsumption_resolution, [], [f1136, f215])).
fof(f1136, plain, ((e1 = e3) | (~ spl30_34 | ~ spl30_36)), inference(backward_demodulation, [], [f536, f528])).
fof(f1131, plain, (~ spl30_35 | ~ spl30_39), inference(avatar_split_clause, [], [f1128, f547, f530])).
fof(f1128, plain, (~ (e2 = op(e1, e3)) | ~ spl30_39), inference(backward_demodulation, [], [f198, f549])).
fof(f1123, plain, (~ spl30_33 | ~ spl30_41), inference(avatar_split_clause, [], [f1115, f556, f522])).
fof(f1115, plain, (~ (e0 = op(e1, e3)) | ~ spl30_41), inference(backward_demodulation, [], [f197, f558])).
fof(f1121, plain, (~ spl30_9 | ~ spl30_41), inference(avatar_split_clause, [], [f1113, f556, f420])).
fof(f1113, plain, (~ (e0 = op(e3, e1)) | ~ spl30_41), inference(backward_demodulation, [], [f173, f558])).
fof(f1120, plain, (~ spl30_25 | ~ spl30_41), inference(avatar_split_clause, [], [f1112, f556, f488])).
fof(f1112, plain, (~ (e0 = op(e2, e1)) | ~ spl30_41), inference(backward_demodulation, [], [f172, f558])).
fof(f1111, plain, (~ spl30_33 | ~ spl30_45), inference(avatar_split_clause, [], [f1106, f573, f522])).
fof(f1106, plain, (~ (e0 = op(e1, e3)) | ~ spl30_45), inference(backward_demodulation, [], [f195, f575])).
fof(f1108, plain, (~ spl30_13 | ~ spl30_45), inference(avatar_split_clause, [], [f1103, f573, f437])).
fof(f1103, plain, (~ (e0 = op(e3, e0)) | ~ spl30_45), inference(backward_demodulation, [], [f167, f575])).
fof(f1107, plain, (~ spl30_29 | ~ spl30_45), inference(avatar_split_clause, [], [f1102, f573, f505])).
fof(f1102, plain, (~ (e0 = op(e2, e0)) | ~ spl30_45), inference(backward_demodulation, [], [f166, f575])).
fof(f1098, plain, (~ spl30_51 | ~ spl30_52), inference(avatar_contradiction_clause, [], [f1097])).
fof(f1097, plain, ($false | (~ spl30_51 | ~ spl30_52)), inference(subsumption_resolution, [], [f1096, f216])).
fof(f1096, plain, ((e2 = e3) | (~ spl30_51 | ~ spl30_52)), inference(backward_demodulation, [], [f604, f600])).
fof(f1089, plain, (~ spl30_49 | ~ spl30_53), inference(avatar_split_clause, [], [f1085, f607, f590])).
fof(f1085, plain, (~ (e0 = op(e0, e3)) | ~ spl30_53), inference(backward_demodulation, [], [f192, f609])).
fof(f1081, plain, (~ spl30_49 | ~ spl30_57), inference(avatar_split_clause, [], [f1076, f624, f590])).
fof(f1076, plain, (~ (e0 = op(e0, e3)) | ~ spl30_57), inference(backward_demodulation, [], [f191, f626])).
fof(f1078, plain, (~ spl30_25 | ~ spl30_57), inference(avatar_split_clause, [], [f1073, f624, f488])).
fof(f1073, plain, (~ (e0 = op(e2, e1)) | ~ spl30_57), inference(backward_demodulation, [], [f170, f626])).
fof(f1070, plain, (~ spl30_53 | ~ spl30_61), inference(avatar_split_clause, [], [f1061, f641, f607])).
fof(f1061, plain, (~ (e0 = op(e0, e2)) | ~ spl30_61), inference(backward_demodulation, [], [f188, f643])).
fof(f1069, plain, (~ spl30_57 | ~ spl30_61), inference(avatar_split_clause, [], [f1060, f641, f624])).
fof(f1060, plain, (~ (e0 = op(e0, e1)) | ~ spl30_61), inference(backward_demodulation, [], [f187, f643])).
fof(f1068, plain, (~ spl30_13 | ~ spl30_61), inference(avatar_split_clause, [], [f1059, f641, f437])).
fof(f1059, plain, (~ (e0 = op(e3, e0)) | ~ spl30_61), inference(backward_demodulation, [], [f165, f643])).
fof(f1067, plain, (~ spl30_29 | ~ spl30_61), inference(avatar_split_clause, [], [f1058, f641, f505])).
fof(f1058, plain, (~ (e0 = op(e2, e0)) | ~ spl30_61), inference(backward_demodulation, [], [f164, f643])).
fof(f1050, plain, (~ spl30_117 | ~ spl30_96 | ~ spl30_41), inference(avatar_split_clause, [], [f382, f556, f942, f1046])).
fof(f382, plain, (~ (e0 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e0 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e0 = op(e1, e1)) & (e2 = op(e1, op(e1, e1))) & (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax27)).
fof(f1049, plain, (~ spl30_117 | ~ spl30_114 | ~ spl30_43), inference(avatar_split_clause, [], [f381, f564, f1028, f1046])).
fof(f381, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e2 = op(e1, e1)) & (e0 = op(e1, op(e1, e1))) & (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax26)).
fof(f1044, plain, (~ spl30_116 | ~ spl30_105 | ~ spl30_62), inference(avatar_split_clause, [], [f380, f645, f984, f1040])).
fof(f380, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, e0))) | ~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0)))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, e0))) | ~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0)))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((op(e0, e0) = e1) & (e2 = op(e0, op(e0, e0))) & (e3 = op(op(e0, op(e0, e0)), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax25)).
fof(f1038, plain, (~ spl30_115 | ~ spl30_102 | ~ spl30_1), inference(avatar_split_clause, [], [f378, f386, f970, f1034])).
fof(f378, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e0 = op(e3, e3)) & (e1 = op(e3, op(e3, e3))) & (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax23)).
fof(f1037, plain, (~ spl30_115 | ~ spl30_110 | ~ spl30_2), inference(avatar_split_clause, [], [f377, f390, f1008, f1034])).
fof(f377, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e1 = op(e3, e3)) & (e0 = op(e3, op(e3, e3))) & (e2 = op(op(e3, op(e3, e3)), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax22)).
fof(f1032, plain, (~ spl30_113 | ~ spl30_97 | ~ spl30_41), inference(avatar_split_clause, [], [f376, f556, f947, f1024])).
fof(f376, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, e1))) | ~ (e2 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, e1))) | ~ (e2 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e1, e1)) & (e3 = op(e1, op(e1, e1))) & (e2 = op(op(e1, op(e1, e1)), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax21)).
fof(f1012, plain, (~ spl30_109 | ~ spl30_103 | ~ spl30_1), inference(avatar_split_clause, [], [f372, f386, f975, f1004])).
fof(f372, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e0 = op(e3, e3)) & (e2 = op(e3, op(e3, e3))) & (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax17)).
fof(f1011, plain, (~ spl30_109 | ~ spl30_110 | ~ spl30_3), inference(avatar_split_clause, [], [f371, f394, f1008, f1004])).
fof(f371, plain, (~ (e2 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e2 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e2 = op(e3, e3)) & (e0 = op(e3, op(e3, e3))) & (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax16)).
fof(f1002, plain, (~ spl30_107 | ~ spl30_100 | ~ spl30_21), inference(avatar_split_clause, [], [f370, f471, f961, f994])).
fof(f370, plain, (~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, e2))) | ~ (e1 = op(op(e2, op(e2, e2)), op(e2, e2)))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, e2))) | ~ (e1 = op(op(e2, op(e2, e2)), op(e2, e2)))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e2, e2)) & (e3 = op(e2, op(e2, e2))) & (e1 = op(op(e2, op(e2, e2)), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax15)).
fof(f992, plain, (~ spl30_104 | ~ spl30_106 | ~ spl30_63), inference(avatar_split_clause, [], [f368, f649, f989, f980])).
fof(f368, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((op(e0, e0) = e2) & (e3 = op(e0, op(e0, e0))) & (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax13)).
fof(f987, plain, (~ spl30_104 | ~ spl30_105 | ~ spl30_64), inference(avatar_split_clause, [], [f367, f653, f984, f980])).
fof(f367, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((op(e0, e0) = e3) & (e2 = op(e0, op(e0, e0))) & (e1 = op(op(e0, op(e0, e0)), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax12)).
fof(f978, plain, (~ spl30_101 | ~ spl30_103 | ~ spl30_2), inference(avatar_split_clause, [], [f366, f390, f975, f966])).
fof(f366, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(op(e3, op(e3, e3)), op(e3, e3)))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e1 = op(e3, e3)) & (e2 = op(e3, op(e3, e3))) & (e0 = op(op(e3, op(e3, e3)), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax11)).
fof(f945, plain, (~ spl30_95 | ~ spl30_96 | ~ spl30_44), inference(avatar_split_clause, [], [f361, f568, f942, f938])).
fof(f361, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(op(e1, op(e1, e1)), op(e1, e1)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e1, e1)) & (e2 = op(e1, op(e1, e1))) & (e0 = op(op(e1, op(e1, e1)), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax6)).
fof(f936, plain, (spl30_94 | spl30_93 | spl30_92 | spl30_91 | spl30_90 | spl30_89 | spl30_88 | spl30_87 | spl30_86 | spl30_85 | spl30_84 | spl30_83 | spl30_82 | spl30_81 | spl30_80 | spl30_4), inference(avatar_split_clause, [], [f352, f398, f825, f832, f839, f846, f853, f860, f867, f874, f881, f888, f895, f902, f909, f916, f923])).
fof(f923, plain, (spl30_94 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl30_94])])).
fof(f916, plain, (spl30_93 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl30_93])])).
fof(f909, plain, (spl30_92 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl30_92])])).
fof(f902, plain, (spl30_91 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl30_91])])).
fof(f895, plain, (spl30_90 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl30_90])])).
fof(f888, plain, (spl30_89 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl30_89])])).
fof(f881, plain, (spl30_88 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl30_88])])).
fof(f874, plain, (spl30_87 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl30_87])])).
fof(f867, plain, (spl30_86 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl30_86])])).
fof(f860, plain, (spl30_85 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl30_85])])).
fof(f853, plain, (spl30_84 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl30_84])])).
fof(f846, plain, (spl30_83 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl30_83])])).
fof(f839, plain, (spl30_82 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl30_82])])).
fof(f832, plain, (spl30_81 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl30_81])])).
fof(f825, plain, (spl30_80 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl30_80])])).
fof(f352, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15) & (((e3 = op(e3, e3)) & ~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f5, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, (((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((e1 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((e1 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((e2 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((e2 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, (((e3 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> ((e3 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, (((e0 = op(e0, e1)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> ((e0 = op(e0, e1)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, (((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> ((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, (((e2 = op(e2, e1)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> ((e2 = op(e2, e1)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, (((e3 = op(e3, e1)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> ((e3 = op(e3, e1)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, (((e0 = op(e0, e2)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> ((e0 = op(e0, e2)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, (((e1 = op(e1, e2)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> ((e1 = op(e1, e2)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, (((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> ((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, (((e3 = op(e3, e2)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> ((e3 = op(e3, e2)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, (((e0 = op(e0, e3)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> ((e0 = op(e0, e3)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, (((e1 = op(e1, e3)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> ((e1 = op(e1, e3)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, (((e2 = op(e2, e3)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> ((e2 = op(e2, e3)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f69, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP15), inference(usedef, [], [e69])).
fof(e69, plain, (sP15 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f70, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP16), inference(usedef, [], [e70])).
fof(e70, plain, (sP16 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f71, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP17), inference(usedef, [], [e71])).
fof(e71, plain, (sP17 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f72, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP18), inference(usedef, [], [e72])).
fof(e72, plain, (sP18 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f73, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP19), inference(usedef, [], [e73])).
fof(e73, plain, (sP19 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f74, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP20), inference(usedef, [], [e74])).
fof(e74, plain, (sP20 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f75, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP21), inference(usedef, [], [e75])).
fof(e75, plain, (sP21 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f76, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP22), inference(usedef, [], [e76])).
fof(e76, plain, (sP22 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f77, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP23), inference(usedef, [], [e77])).
fof(e77, plain, (sP23 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f78, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP24), inference(usedef, [], [e78])).
fof(e78, plain, (sP24 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f79, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP25), inference(usedef, [], [e79])).
fof(e79, plain, (sP25 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f80, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP26), inference(usedef, [], [e80])).
fof(e80, plain, (sP26 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f81, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP27), inference(usedef, [], [e81])).
fof(e81, plain, (sP27 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f82, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP28), inference(usedef, [], [e82])).
fof(e82, plain, (sP28 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f83, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP29), inference(usedef, [], [e83])).
fof(e83, plain, (sP29 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f5, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & ~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ((e2 = op(e2, e3)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2))) | ((e1 = op(e1, e3)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1))) | ((e0 = op(e0, e3)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0))) | ((e3 = op(e3, e2)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3))) | ((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ((e1 = op(e1, e2)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1))) | ((e0 = op(e0, e2)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0))) | ((e3 = op(e3, e1)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3))) | ((e2 = op(e2, e1)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2))) | ((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ((e0 = op(e0, e1)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0))) | ((e3 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3))) | ((e2 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2))) | ((e1 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1))) | ((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax5)).
fof(f935, plain, (spl30_94 | spl30_93 | spl30_92 | spl30_91 | spl30_90 | spl30_89 | spl30_88 | spl30_87 | spl30_86 | spl30_85 | spl30_84 | spl30_83 | spl30_82 | spl30_81 | spl30_80 | ~ spl30_4), inference(avatar_split_clause, [], [f353, f398, f825, f832, f839, f846, f853, f860, f867, f874, f881, f888, f895, f902, f909, f916, f923])).
fof(f353, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f84])).
fof(f933, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | spl30_4), inference(avatar_split_clause, [], [f355, f398, f690, f699, f708, f717, f726, f735, f744, f753, f762, f771, f780, f789, f798, f807, f816])).
fof(f816, plain, (spl30_79 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl30_79])])).
fof(f807, plain, (spl30_78 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl30_78])])).
fof(f798, plain, (spl30_77 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl30_77])])).
fof(f789, plain, (spl30_76 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl30_76])])).
fof(f780, plain, (spl30_75 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl30_75])])).
fof(f771, plain, (spl30_74 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl30_74])])).
fof(f762, plain, (spl30_73 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl30_73])])).
fof(f753, plain, (spl30_72 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl30_72])])).
fof(f744, plain, (spl30_71 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl30_71])])).
fof(f735, plain, (spl30_70 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl30_70])])).
fof(f726, plain, (spl30_69 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl30_69])])).
fof(f717, plain, (spl30_68 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl30_68])])).
fof(f708, plain, (spl30_67 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl30_67])])).
fof(f699, plain, (spl30_66 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl30_66])])).
fof(f690, plain, (spl30_65 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl30_65])])).
fof(f355, plain, ((e3 = op(e3, e3)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f931, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f357, f590, f653, f690, f699, f708, f717, f726, f735, f744, f753, f762, f771, f780, f789, f798, f807, f816])).
fof(f357, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f930, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f358, f526, f568, f690, f699, f708, f717, f726, f735, f744, f753, f762, f771, f780, f789, f798, f807, f816])).
fof(f358, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f929, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f359, f462, f483, f690, f699, f708, f717, f726, f735, f744, f753, f762, f771, f780, f789, f798, f807, f816])).
fof(f359, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f928, plain, (~ spl30_94 | spl30_61), inference(avatar_split_clause, [], [f349, f641, f923])).
fof(f349, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f114])).
fof(f114, plain, (((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f927, plain, (~ spl30_94 | ~ spl30_61), inference(avatar_split_clause, [], [f350, f641, f923])).
fof(f350, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f114])).
fof(f921, plain, (~ spl30_93 | spl30_57), inference(avatar_split_clause, [], [f346, f624, f916])).
fof(f346, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f113])).
fof(f113, plain, (((e1 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f919, plain, (~ spl30_93 | spl30_46), inference(avatar_split_clause, [], [f348, f577, f916])).
fof(f348, plain, ((e1 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f113])).
fof(f914, plain, (~ spl30_92 | spl30_53), inference(avatar_split_clause, [], [f343, f607, f909])).
fof(f343, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f112, plain, (((e2 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f913, plain, (~ spl30_92 | ~ spl30_63), inference(avatar_split_clause, [], [f344, f649, f909])).
fof(f344, plain, (~ (op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f912, plain, (~ spl30_92 | spl30_31), inference(avatar_split_clause, [], [f345, f513, f909])).
fof(f345, plain, ((e2 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f907, plain, (~ spl30_91 | spl30_49), inference(avatar_split_clause, [], [f340, f590, f902])).
fof(f340, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f111, plain, (((e3 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f906, plain, (~ spl30_91 | ~ spl30_64), inference(avatar_split_clause, [], [f341, f653, f902])).
fof(f341, plain, (~ (op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f905, plain, (~ spl30_91 | spl30_16), inference(avatar_split_clause, [], [f342, f449, f902])).
fof(f342, plain, ((e3 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f900, plain, (~ spl30_90 | spl30_46), inference(avatar_split_clause, [], [f337, f577, f895])).
fof(f337, plain, ((e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f110])).
fof(f110, plain, (((e0 = op(e0, e1)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f898, plain, (~ spl30_90 | spl30_57), inference(avatar_split_clause, [], [f339, f624, f895])).
fof(f339, plain, ((e0 = op(e0, e1)) | ~ sP4), inference(cnf_transformation, [], [f110])).
fof(f893, plain, (~ spl30_89 | spl30_42), inference(avatar_split_clause, [], [f334, f560, f888])).
fof(f334, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f109])).
fof(f109, plain, (((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f892, plain, (~ spl30_89 | ~ spl30_42), inference(avatar_split_clause, [], [f335, f560, f888])).
fof(f335, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f109])).
fof(f886, plain, (~ spl30_88 | spl30_38), inference(avatar_split_clause, [], [f331, f543, f881])).
fof(f331, plain, ((e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f108, plain, (((e2 = op(e2, e1)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f885, plain, (~ spl30_88 | ~ spl30_43), inference(avatar_split_clause, [], [f332, f564, f881])).
fof(f332, plain, (~ (e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f884, plain, (~ spl30_88 | spl30_27), inference(avatar_split_clause, [], [f333, f496, f881])).
fof(f333, plain, ((e2 = op(e2, e1)) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f879, plain, (~ spl30_87 | spl30_34), inference(avatar_split_clause, [], [f328, f526, f874])).
fof(f328, plain, ((e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f107, plain, (((e3 = op(e3, e1)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f878, plain, (~ spl30_87 | ~ spl30_44), inference(avatar_split_clause, [], [f329, f568, f874])).
fof(f329, plain, (~ (e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f877, plain, (~ spl30_87 | spl30_12), inference(avatar_split_clause, [], [f330, f432, f874])).
fof(f330, plain, ((e3 = op(e3, e1)) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f872, plain, (~ spl30_86 | spl30_31), inference(avatar_split_clause, [], [f325, f513, f867])).
fof(f325, plain, ((e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f106, plain, (((e0 = op(e0, e2)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f871, plain, (~ spl30_86 | ~ spl30_21), inference(avatar_split_clause, [], [f326, f471, f867])).
fof(f326, plain, (~ (e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f870, plain, (~ spl30_86 | spl30_53), inference(avatar_split_clause, [], [f327, f607, f867])).
fof(f327, plain, ((e0 = op(e0, e2)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f865, plain, (~ spl30_85 | spl30_27), inference(avatar_split_clause, [], [f322, f496, f860])).
fof(f322, plain, ((e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f105, plain, (((e1 = op(e1, e2)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f864, plain, (~ spl30_85 | ~ spl30_22), inference(avatar_split_clause, [], [f323, f475, f860])).
fof(f323, plain, (~ (e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f863, plain, (~ spl30_85 | spl30_38), inference(avatar_split_clause, [], [f324, f543, f860])).
fof(f324, plain, ((e1 = op(e1, e2)) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f858, plain, (~ spl30_84 | spl30_23), inference(avatar_split_clause, [], [f319, f479, f853])).
fof(f319, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f104])).
fof(f104, plain, (((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f857, plain, (~ spl30_84 | ~ spl30_23), inference(avatar_split_clause, [], [f320, f479, f853])).
fof(f320, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f104])).
fof(f851, plain, (~ spl30_83 | spl30_19), inference(avatar_split_clause, [], [f316, f462, f846])).
fof(f316, plain, ((e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f103])).
fof(f103, plain, (((e3 = op(e3, e2)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f849, plain, (~ spl30_83 | spl30_8), inference(avatar_split_clause, [], [f318, f415, f846])).
fof(f318, plain, ((e3 = op(e3, e2)) | ~ sP11), inference(cnf_transformation, [], [f103])).
fof(f844, plain, (~ spl30_82 | spl30_16), inference(avatar_split_clause, [], [f313, f449, f839])).
fof(f313, plain, ((e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f102, plain, (((e0 = op(e0, e3)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f843, plain, (~ spl30_82 | ~ spl30_1), inference(avatar_split_clause, [], [f314, f386, f839])).
fof(f314, plain, (~ (e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f842, plain, (~ spl30_82 | spl30_49), inference(avatar_split_clause, [], [f315, f590, f839])).
fof(f315, plain, ((e0 = op(e0, e3)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f837, plain, (~ spl30_81 | spl30_12), inference(avatar_split_clause, [], [f310, f432, f832])).
fof(f310, plain, ((e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f101, plain, (((e1 = op(e1, e3)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f836, plain, (~ spl30_81 | ~ spl30_2), inference(avatar_split_clause, [], [f311, f390, f832])).
fof(f311, plain, (~ (e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f835, plain, (~ spl30_81 | spl30_34), inference(avatar_split_clause, [], [f312, f526, f832])).
fof(f312, plain, ((e1 = op(e1, e3)) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f830, plain, (~ spl30_80 | spl30_8), inference(avatar_split_clause, [], [f307, f415, f825])).
fof(f307, plain, ((e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (((e2 = op(e2, e3)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f829, plain, (~ spl30_80 | ~ spl30_3), inference(avatar_split_clause, [], [f308, f394, f825])).
fof(f308, plain, (~ (e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f828, plain, (~ spl30_80 | spl30_19), inference(avatar_split_clause, [], [f309, f462, f825])).
fof(f309, plain, ((e2 = op(e2, e3)) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f823, plain, (~ spl30_79 | spl30_61), inference(avatar_split_clause, [], [f301, f641, f816])).
fof(f301, plain, ((e0 = op(e0, e0)) | ~ sP15), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP15), inference(nnf_transformation, [], [f69])).
fof(f821, plain, (~ spl30_79 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f304, f577, f556, f816])).
fof(f304, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP15), inference(cnf_transformation, [], [f99])).
fof(f820, plain, (~ spl30_79 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f305, f513, f471, f816])).
fof(f305, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP15), inference(cnf_transformation, [], [f99])).
fof(f819, plain, (~ spl30_79 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f306, f449, f386, f816])).
fof(f306, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP15), inference(cnf_transformation, [], [f99])).
fof(f814, plain, (~ spl30_78 | spl30_57), inference(avatar_split_clause, [], [f295, f624, f807])).
fof(f295, plain, ((e0 = op(e0, e1)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP16), inference(nnf_transformation, [], [f70])).
fof(f813, plain, (~ spl30_78 | spl30_45), inference(avatar_split_clause, [], [f296, f573, f807])).
fof(f296, plain, ((e0 = op(e1, e0)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f811, plain, (~ spl30_78 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f299, f513, f471, f807])).
fof(f299, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f810, plain, (~ spl30_78 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f300, f449, f386, f807])).
fof(f300, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f805, plain, (~ spl30_77 | spl30_53), inference(avatar_split_clause, [], [f289, f607, f798])).
fof(f289, plain, ((e0 = op(e0, e2)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP17), inference(nnf_transformation, [], [f71])).
fof(f804, plain, (~ spl30_77 | spl30_29), inference(avatar_split_clause, [], [f290, f505, f798])).
fof(f290, plain, ((e0 = op(e2, e0)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f803, plain, (~ spl30_77 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f292, f577, f556, f798])).
fof(f292, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f802, plain, (~ spl30_77 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f293, f513, f471, f798])).
fof(f293, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f801, plain, (~ spl30_77 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f294, f449, f386, f798])).
fof(f294, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f796, plain, (~ spl30_76 | spl30_49), inference(avatar_split_clause, [], [f283, f590, f789])).
fof(f283, plain, ((e0 = op(e0, e3)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP18), inference(nnf_transformation, [], [f72])).
fof(f795, plain, (~ spl30_76 | spl30_13), inference(avatar_split_clause, [], [f284, f437, f789])).
fof(f284, plain, ((e0 = op(e3, e0)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f794, plain, (~ spl30_76 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f286, f577, f556, f789])).
fof(f286, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f793, plain, (~ spl30_76 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f287, f513, f471, f789])).
fof(f287, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f792, plain, (~ spl30_76 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f288, f449, f386, f789])).
fof(f288, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f787, plain, (~ spl30_75 | spl30_46), inference(avatar_split_clause, [], [f277, f577, f780])).
fof(f277, plain, ((e1 = op(e1, e0)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP19), inference(nnf_transformation, [], [f73])).
fof(f786, plain, (~ spl30_75 | spl30_58), inference(avatar_split_clause, [], [f278, f628, f780])).
fof(f278, plain, ((e1 = op(e0, e1)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f785, plain, (~ spl30_75 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f279, f624, f645, f780])).
fof(f279, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f784, plain, (~ spl30_75 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f281, f496, f475, f780])).
fof(f281, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f783, plain, (~ spl30_75 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f282, f432, f390, f780])).
fof(f282, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f778, plain, (~ spl30_74 | spl30_42), inference(avatar_split_clause, [], [f271, f560, f771])).
fof(f271, plain, ((e1 = op(e1, e1)) | ~ sP20), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP20), inference(nnf_transformation, [], [f74])).
fof(f776, plain, (~ spl30_74 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f273, f624, f645, f771])).
fof(f273, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP20), inference(cnf_transformation, [], [f94])).
fof(f775, plain, (~ spl30_74 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f275, f496, f475, f771])).
fof(f275, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP20), inference(cnf_transformation, [], [f94])).
fof(f774, plain, (~ spl30_74 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f276, f432, f390, f771])).
fof(f276, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP20), inference(cnf_transformation, [], [f94])).
fof(f769, plain, (~ spl30_73 | spl30_38), inference(avatar_split_clause, [], [f265, f543, f762])).
fof(f265, plain, ((e1 = op(e1, e2)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP21), inference(nnf_transformation, [], [f75])).
fof(f768, plain, (~ spl30_73 | spl30_26), inference(avatar_split_clause, [], [f266, f492, f762])).
fof(f266, plain, ((e1 = op(e2, e1)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f767, plain, (~ spl30_73 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f267, f624, f645, f762])).
fof(f267, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f766, plain, (~ spl30_73 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f269, f496, f475, f762])).
fof(f269, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f765, plain, (~ spl30_73 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f270, f432, f390, f762])).
fof(f270, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f760, plain, (~ spl30_72 | spl30_34), inference(avatar_split_clause, [], [f259, f526, f753])).
fof(f259, plain, ((e1 = op(e1, e3)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP22), inference(nnf_transformation, [], [f76])).
fof(f759, plain, (~ spl30_72 | spl30_10), inference(avatar_split_clause, [], [f260, f424, f753])).
fof(f260, plain, ((e1 = op(e3, e1)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f758, plain, (~ spl30_72 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f261, f624, f645, f753])).
fof(f261, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f757, plain, (~ spl30_72 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f263, f496, f475, f753])).
fof(f263, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f756, plain, (~ spl30_72 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f264, f432, f390, f753])).
fof(f264, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f751, plain, (~ spl30_71 | spl30_31), inference(avatar_split_clause, [], [f253, f513, f744])).
fof(f253, plain, ((e2 = op(e2, e0)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP23), inference(nnf_transformation, [], [f77])).
fof(f750, plain, (~ spl30_71 | spl30_55), inference(avatar_split_clause, [], [f254, f615, f744])).
fof(f254, plain, ((e2 = op(e0, e2)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f749, plain, (~ spl30_71 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f255, f607, f649, f744])).
fof(f255, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f748, plain, (~ spl30_71 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f256, f543, f564, f744])).
fof(f256, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f747, plain, (~ spl30_71 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f258, f415, f394, f744])).
fof(f258, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f742, plain, (~ spl30_70 | spl30_27), inference(avatar_split_clause, [], [f247, f496, f735])).
fof(f247, plain, ((e2 = op(e2, e1)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP24), inference(nnf_transformation, [], [f78])).
fof(f741, plain, (~ spl30_70 | spl30_39), inference(avatar_split_clause, [], [f248, f547, f735])).
fof(f248, plain, ((e2 = op(e1, e2)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f740, plain, (~ spl30_70 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f249, f607, f649, f735])).
fof(f249, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f739, plain, (~ spl30_70 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f250, f543, f564, f735])).
fof(f250, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f738, plain, (~ spl30_70 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f252, f415, f394, f735])).
fof(f252, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f733, plain, (~ spl30_69 | spl30_23), inference(avatar_split_clause, [], [f241, f479, f726])).
fof(f241, plain, ((e2 = op(e2, e2)) | ~ sP25), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP25), inference(nnf_transformation, [], [f79])).
fof(f731, plain, (~ spl30_69 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f243, f607, f649, f726])).
fof(f243, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP25), inference(cnf_transformation, [], [f89])).
fof(f730, plain, (~ spl30_69 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f244, f543, f564, f726])).
fof(f244, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP25), inference(cnf_transformation, [], [f89])).
fof(f729, plain, (~ spl30_69 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f246, f415, f394, f726])).
fof(f246, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP25), inference(cnf_transformation, [], [f89])).
fof(f724, plain, (~ spl30_68 | spl30_19), inference(avatar_split_clause, [], [f235, f462, f717])).
fof(f235, plain, ((e2 = op(e2, e3)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP26), inference(nnf_transformation, [], [f80])).
fof(f723, plain, (~ spl30_68 | spl30_7), inference(avatar_split_clause, [], [f236, f411, f717])).
fof(f236, plain, ((e2 = op(e3, e2)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f722, plain, (~ spl30_68 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f237, f607, f649, f717])).
fof(f237, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f721, plain, (~ spl30_68 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f238, f543, f564, f717])).
fof(f238, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f720, plain, (~ spl30_68 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f240, f415, f394, f717])).
fof(f240, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f715, plain, (~ spl30_67 | spl30_16), inference(avatar_split_clause, [], [f229, f449, f708])).
fof(f229, plain, ((e3 = op(e3, e0)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP27), inference(nnf_transformation, [], [f81])).
fof(f714, plain, (~ spl30_67 | spl30_52), inference(avatar_split_clause, [], [f230, f602, f708])).
fof(f230, plain, ((e3 = op(e0, e3)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f712, plain, (~ spl30_67 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f232, f526, f568, f708])).
fof(f232, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f711, plain, (~ spl30_67 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f233, f462, f483, f708])).
fof(f233, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f706, plain, (~ spl30_66 | spl30_12), inference(avatar_split_clause, [], [f223, f432, f699])).
fof(f223, plain, ((e3 = op(e3, e1)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP28), inference(nnf_transformation, [], [f82])).
fof(f705, plain, (~ spl30_66 | spl30_36), inference(avatar_split_clause, [], [f224, f534, f699])).
fof(f224, plain, ((e3 = op(e1, e3)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f704, plain, (~ spl30_66 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f225, f590, f653, f699])).
fof(f225, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f702, plain, (~ spl30_66 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f227, f462, f483, f699])).
fof(f227, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f697, plain, (~ spl30_65 | spl30_8), inference(avatar_split_clause, [], [f217, f415, f690])).
fof(f217, plain, ((e3 = op(e3, e2)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP29), inference(nnf_transformation, [], [f83])).
fof(f696, plain, (~ spl30_65 | spl30_20), inference(avatar_split_clause, [], [f218, f466, f690])).
fof(f218, plain, ((e3 = op(e2, e3)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f695, plain, (~ spl30_65 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f219, f590, f653, f690])).
fof(f219, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f694, plain, (~ spl30_65 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f220, f526, f568, f690])).
fof(f220, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f693, plain, (~ spl30_65 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f221, f462, f483, f690])).
fof(f221, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f688, plain, (spl30_61 | spl30_57 | spl30_53 | spl30_49), inference(avatar_split_clause, [], [f131, f590, f607, f624, f641])).
fof(f131, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax2)).
fof(f687, plain, (spl30_61 | spl30_45 | spl30_29 | spl30_13), inference(avatar_split_clause, [], [f132, f437, f505, f573, f641])).
fof(f132, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f686, plain, (spl30_62 | spl30_58 | spl30_54 | spl30_50), inference(avatar_split_clause, [], [f133, f594, f611, f628, f645])).
fof(f133, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f685, plain, (spl30_62 | spl30_46 | spl30_30 | spl30_14), inference(avatar_split_clause, [], [f134, f441, f509, f577, f645])).
fof(f134, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f684, plain, (spl30_63 | spl30_59 | spl30_55 | spl30_51), inference(avatar_split_clause, [], [f135, f598, f615, f632, f649])).
fof(f135, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f683, plain, (spl30_63 | spl30_47 | spl30_31 | spl30_15), inference(avatar_split_clause, [], [f136, f445, f513, f581, f649])).
fof(f136, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f682, plain, (spl30_64 | spl30_60 | spl30_56 | spl30_52), inference(avatar_split_clause, [], [f137, f602, f619, f636, f653])).
fof(f137, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f680, plain, (spl30_45 | spl30_41 | spl30_37 | spl30_33), inference(avatar_split_clause, [], [f139, f522, f539, f556, f573])).
fof(f139, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f679, plain, (spl30_57 | spl30_41 | spl30_25 | spl30_9), inference(avatar_split_clause, [], [f140, f420, f488, f556, f624])).
fof(f140, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f677, plain, (spl30_58 | spl30_42 | spl30_26 | spl30_10), inference(avatar_split_clause, [], [f142, f424, f492, f560, f628])).
fof(f142, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f676, plain, (spl30_47 | spl30_43 | spl30_39 | spl30_35), inference(avatar_split_clause, [], [f143, f530, f547, f564, f581])).
fof(f143, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f675, plain, (spl30_59 | spl30_43 | spl30_27 | spl30_11), inference(avatar_split_clause, [], [f144, f428, f496, f564, f632])).
fof(f144, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f674, plain, (spl30_48 | spl30_44 | spl30_40 | spl30_36), inference(avatar_split_clause, [], [f145, f534, f551, f568, f585])).
fof(f145, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f673, plain, (spl30_60 | spl30_44 | spl30_28 | spl30_12), inference(avatar_split_clause, [], [f146, f432, f500, f568, f636])).
fof(f146, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f672, plain, (spl30_29 | spl30_25 | spl30_21 | spl30_17), inference(avatar_split_clause, [], [f147, f454, f471, f488, f505])).
fof(f147, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f671, plain, (spl30_53 | spl30_37 | spl30_21 | spl30_5), inference(avatar_split_clause, [], [f148, f403, f471, f539, f607])).
fof(f148, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f670, plain, (spl30_30 | spl30_26 | spl30_22 | spl30_18), inference(avatar_split_clause, [], [f149, f458, f475, f492, f509])).
fof(f149, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f669, plain, (spl30_54 | spl30_38 | spl30_22 | spl30_6), inference(avatar_split_clause, [], [f150, f407, f475, f543, f611])).
fof(f150, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f667, plain, (spl30_55 | spl30_39 | spl30_23 | spl30_7), inference(avatar_split_clause, [], [f152, f411, f479, f547, f615])).
fof(f152, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f666, plain, (spl30_32 | spl30_28 | spl30_24 | spl30_20), inference(avatar_split_clause, [], [f153, f466, f483, f500, f517])).
fof(f153, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f665, plain, (spl30_56 | spl30_40 | spl30_24 | spl30_8), inference(avatar_split_clause, [], [f154, f415, f483, f551, f619])).
fof(f154, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f664, plain, (spl30_13 | spl30_9 | spl30_5 | spl30_1), inference(avatar_split_clause, [], [f155, f386, f403, f420, f437])).
fof(f155, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f663, plain, (spl30_49 | spl30_33 | spl30_17 | spl30_1), inference(avatar_split_clause, [], [f156, f386, f454, f522, f590])).
fof(f156, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f662, plain, (spl30_14 | spl30_10 | spl30_6 | spl30_2), inference(avatar_split_clause, [], [f157, f390, f407, f424, f441])).
fof(f157, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f661, plain, (spl30_50 | spl30_34 | spl30_18 | spl30_2), inference(avatar_split_clause, [], [f158, f390, f458, f526, f594])).
fof(f158, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f660, plain, (spl30_15 | spl30_11 | spl30_7 | spl30_3), inference(avatar_split_clause, [], [f159, f394, f411, f428, f445])).
fof(f159, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f659, plain, (spl30_51 | spl30_35 | spl30_19 | spl30_3), inference(avatar_split_clause, [], [f160, f394, f462, f530, f598])).
fof(f160, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f658, plain, (spl30_16 | spl30_12 | spl30_8 | spl30_4), inference(avatar_split_clause, [], [f161, f398, f415, f432, f449])).
fof(f161, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f657, plain, (spl30_52 | spl30_36 | spl30_20 | spl30_4), inference(avatar_split_clause, [], [f162, f398, f466, f534, f602])).
fof(f162, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f656, plain, (spl30_61 | spl30_62 | spl30_63 | spl30_64), inference(avatar_split_clause, [], [f115, f653, f649, f645, f641])).
fof(f115, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG158+1.p', ax1)).
fof(f639, plain, (spl30_57 | spl30_58 | spl30_59 | spl30_60), inference(avatar_split_clause, [], [f116, f636, f632, f628, f624])).
fof(f116, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f622, plain, (spl30_53 | spl30_54 | spl30_55 | spl30_56), inference(avatar_split_clause, [], [f117, f619, f615, f611, f607])).
fof(f117, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f605, plain, (spl30_49 | spl30_50 | spl30_51 | spl30_52), inference(avatar_split_clause, [], [f118, f602, f598, f594, f590])).
fof(f118, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f588, plain, (spl30_45 | spl30_46 | spl30_47 | spl30_48), inference(avatar_split_clause, [], [f119, f585, f581, f577, f573])).
fof(f119, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f571, plain, (spl30_41 | spl30_42 | spl30_43 | spl30_44), inference(avatar_split_clause, [], [f120, f568, f564, f560, f556])).
fof(f120, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f554, plain, (spl30_37 | spl30_38 | spl30_39 | spl30_40), inference(avatar_split_clause, [], [f121, f551, f547, f543, f539])).
fof(f121, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f537, plain, (spl30_33 | spl30_34 | spl30_35 | spl30_36), inference(avatar_split_clause, [], [f122, f534, f530, f526, f522])).
fof(f122, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f520, plain, (spl30_29 | spl30_30 | spl30_31 | spl30_32), inference(avatar_split_clause, [], [f123, f517, f513, f509, f505])).
fof(f123, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f503, plain, (spl30_25 | spl30_26 | spl30_27 | spl30_28), inference(avatar_split_clause, [], [f124, f500, f496, f492, f488])).
fof(f124, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f486, plain, (spl30_21 | spl30_22 | spl30_23 | spl30_24), inference(avatar_split_clause, [], [f125, f483, f479, f475, f471])).
fof(f125, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f469, plain, (spl30_17 | spl30_18 | spl30_19 | spl30_20), inference(avatar_split_clause, [], [f126, f466, f462, f458, f454])).
fof(f126, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f452, plain, (spl30_13 | spl30_14 | spl30_15 | spl30_16), inference(avatar_split_clause, [], [f127, f449, f445, f441, f437])).
fof(f127, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f435, plain, (spl30_9 | spl30_10 | spl30_11 | spl30_12), inference(avatar_split_clause, [], [f128, f432, f428, f424, f420])).
fof(f128, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f418, plain, (spl30_5 | spl30_6 | spl30_7 | spl30_8), inference(avatar_split_clause, [], [f129, f415, f411, f407, f403])).
fof(f129, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f401, plain, (spl30_1 | spl30_2 | spl30_3 | spl30_4), inference(avatar_split_clause, [], [f130, f398, f394, f390, f386])).
fof(f130, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).