fof(f2895, plain, $false, inference(avatar_sat_refutation, [], [f449, f466, f483, f500, f517, f534, f551, f568, f585, f602, f619, f636, f653, f670, f687, f704, f705, f706, f707, f708, f709, f710, f711, f712, f713, f714, f715, f716, f717, f718, f719, f720, f721, f722, f723, f724, f725, f726, f727, f728, f729, f730, f731, f732, f733, f734, f735, f736, f742, f743, f744, f745, f746, f752, f753, f754, f755, f756, f762, f763, f764, f765, f766, f771, f773, f774, f775, f776, f782, f786, f791, f792, f793, f794, f795, f796, f801, f802, f803, f804, f805, f806, f811, f812, f813, f814, f815, f816, f821, f822, f824, f825, f826, f833, f836, f841, f842, f843, f844, f845, f846, f851, f852, f853, f854, f855, f856, f861, f862, f863, f865, f866, f871, f872, f873, f875, f876, f884, f886, f891, f892, f893, f894, f899, f900, f901, f902, f907, f908, f909, f910, f915, f916, f917, f918, f923, f924, f925, f926, f931, f932, f933, f934, f939, f940, f941, f942, f947, f948, f949, f950, f955, f956, f957, f958, f963, f964, f965, f966, f971, f972, f973, f974, f979, f980, f981, f982, f987, f988, f989, f990, f995, f996, f997, f998, f1003, f1004, f1005, f1006, f1007, f1008, f1009, f1010, f1012, f1013, f1014, f1015, f1016, f1034, f1053, f1058, f1063, f1068, f1077, f1082, f1087, f1092, f1097, f1107, f1113, f1123, f1128, f1133, f1136, f1146, f1148, f1149, f1159, f1168, f1169, f1175, f1178, f1181, f1188, f1189, f1190, f1192, f1202, f1205, f1211, f1213, f1220, f1223, f1229, f1230, f1236, f1251, f1254, f1257, f1260, f1266, f1269, f1272, f1279, f1282, f1284, f1287, f1290, f1293, f1311, f1315, f1318, f1324, f1328, f1332, f1338, f1349, f1353, f1357, f1363, f1364, f1369, f1381, f1384, f1393, f1409, f1414, f1418, f1422, f1429, f1431, f1433, f1436, f1450, f1456, f1459, f1471, f1474, f1489, f1496, f1500, f1508, f1517, f1525, f1528, f1547, f1558, f1564, f1568, f1571, f1582, f1606, f1616, f1619, f1624, f1631, f1668, f1672, f1683, f1699, f1707, f1712, f1717, f1730, f1741, f1750, f1753, f1759, f1787, f1788, f1794, f1798, f1799, f1804, f1814, f1817, f1827, f1833, f1837, f1841, f1847, f1852, f1857, f1861, f1863, f1875, f1876, f1877, f1878, f1884, f1885, f1888, f1889, f1892, f1897, f1904, f1905, f1907, f1908, f1914, f1918, f1921, f1931, f1934, f1938, f1946, f1955, f1960, f1961, f1969, f1973, f1978, f1986, f1992, f1999, f2005, f2020, f2022, f2024, f2026, f2031, f2032, f2044, f2050, f2058, f2068, f2071, f2081, f2089, f2094, f2095, f2099, f2111, f2113, f2115, f2134, f2136, f2138, f2157, f2159, f2170, f2185, f2190, f2193, f2207, f2210, f2212, f2232, f2244, f2248, f2253, f2263, f2264, f2274, f2277, f2281, f2289, f2320, f2323, f2327, f2329, f2332, f2338, f2347, f2357, f2360, f2366, f2370, f2384, f2385, f2395, f2396, f2401, f2408, f2416, f2417, f2420, f2425, f2430, f2446, f2453, f2476, f2477, f2478, f2486, f2487, f2492, f2497, f2524, f2527, f2542, f2544, f2561, f2573, f2574, f2576, f2577, f2586, f2587, f2595, f2614, f2625, f2631, f2633, f2637, f2639, f2641, f2651, f2664, f2668, f2675, f2680, f2686, f2687, f2690, f2693, f2703, f2705, f2711, f2714, f2721, f2723, f2731, f2738, f2747, f2758, f2759, f2766, f2773, f2779, f2780, f2784, f2790, f2797, f2802, f2806, f2808, f2811, f2814, f2824, f2826, f2828, f2830, f2832, f2851, f2852, f2859, f2863, f2866, f2871, f2875, f2879, f2884, f2889, f2894])).
fof(f2894, plain, (~ spl30_1 | ~ spl30_5), inference(avatar_split_clause, [], [f2893, f451, f434])).
fof(f434, plain, (spl30_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_1])])).
fof(f451, plain, (spl30_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_5])])).
fof(f2893, plain, (~ (e0 = op(e3, e3)) | ~ spl30_5), inference(backward_demodulation, [], [f210, f453])).
fof(f453, plain, ((e0 = op(e3, e2)) | ~ spl30_5), inference(avatar_component_clause, [], [f451])).
fof(f210, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax3)).
fof(f2889, plain, (~ spl30_6 | ~ spl30_10), inference(avatar_split_clause, [], [f2885, f472, f455])).
fof(f455, plain, (spl30_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_6])])).
fof(f472, plain, (spl30_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_10])])).
fof(f2885, plain, (~ (e1 = op(e3, e2)) | ~ spl30_10), inference(backward_demodulation, [], [f208, f474])).
fof(f474, plain, ((e1 = op(e3, e1)) | ~ spl30_10), inference(avatar_component_clause, [], [f472])).
fof(f208, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2884, plain, (~ spl30_1 | ~ spl30_17), inference(avatar_split_clause, [], [f2881, f502, f434])).
fof(f502, plain, (spl30_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_17])])).
fof(f2881, plain, (~ (e0 = op(e3, e3)) | ~ spl30_17), inference(backward_demodulation, [], [f186, f504])).
fof(f504, plain, ((e0 = op(e2, e3)) | ~ spl30_17), inference(avatar_component_clause, [], [f502])).
fof(f186, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2879, plain, (~ spl30_22 | ~ spl30_24), inference(avatar_contradiction_clause, [], [f2878])).
fof(f2878, plain, ($false | (~ spl30_22 | ~ spl30_24)), inference(subsumption_resolution, [], [f2877, f215])).
fof(f215, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax4)).
fof(f2877, plain, ((e1 = e3) | (~ spl30_22 | ~ spl30_24)), inference(backward_demodulation, [], [f533, f525])).
fof(f525, plain, ((e1 = op(e2, e2)) | ~ spl30_22), inference(avatar_component_clause, [], [f523])).
fof(f523, plain, (spl30_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_22])])).
fof(f533, plain, ((e3 = op(e2, e2)) | ~ spl30_24), inference(avatar_component_clause, [], [f531])).
fof(f531, plain, (spl30_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_24])])).
fof(f2875, plain, (~ spl30_18 | ~ spl30_30), inference(avatar_split_clause, [], [f2874, f557, f506])).
fof(f506, plain, (spl30_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_18])])).
fof(f557, plain, (spl30_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_30])])).
fof(f2874, plain, (~ (e1 = op(e2, e3)) | ~ spl30_30), inference(backward_demodulation, [], [f201, f559])).
fof(f559, plain, ((e1 = op(e2, e0)) | ~ spl30_30), inference(avatar_component_clause, [], [f557])).
fof(f201, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2871, plain, (~ spl30_18 | ~ spl30_34), inference(avatar_split_clause, [], [f2867, f574, f506])).
fof(f574, plain, (spl30_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_34])])).
fof(f2867, plain, (~ (e1 = op(e2, e3)) | ~ spl30_34), inference(backward_demodulation, [], [f184, f576])).
fof(f576, plain, ((e1 = op(e1, e3)) | ~ spl30_34), inference(avatar_component_clause, [], [f574])).
fof(f184, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2866, plain, (~ spl30_36 | ~ spl30_48), inference(avatar_split_clause, [], [f2865, f633, f582])).
fof(f582, plain, (spl30_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_36])])).
fof(f633, plain, (spl30_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_48])])).
fof(f2865, plain, (~ (e3 = op(e1, e3)) | ~ spl30_48), inference(backward_demodulation, [], [f195, f635])).
fof(f635, plain, ((e3 = op(e1, e0)) | ~ spl30_48), inference(avatar_component_clause, [], [f633])).
fof(f195, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2863, plain, (~ spl30_6 | ~ spl30_54), inference(avatar_split_clause, [], [f2860, f659, f455])).
fof(f659, plain, (spl30_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_54])])).
fof(f2860, plain, (~ (e1 = op(e3, e2)) | ~ spl30_54), inference(backward_demodulation, [], [f177, f661])).
fof(f661, plain, ((e1 = op(e0, e2)) | ~ spl30_54), inference(avatar_component_clause, [], [f659])).
fof(f177, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2859, plain, (~ spl30_12 | ~ spl30_60), inference(avatar_split_clause, [], [f2857, f684, f480])).
fof(f480, plain, (spl30_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_12])])).
fof(f684, plain, (spl30_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_60])])).
fof(f2857, plain, (~ (e3 = op(e3, e1)) | ~ spl30_60), inference(backward_demodulation, [], [f171, f686])).
fof(f686, plain, ((e3 = op(e0, e1)) | ~ spl30_60), inference(avatar_component_clause, [], [f684])).
fof(f171, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2852, plain, (~ spl30_53 | ~ spl30_61), inference(avatar_split_clause, [], [f2843, f689, f655])).
fof(f655, plain, (spl30_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_53])])).
fof(f689, plain, (spl30_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_61])])).
fof(f2843, plain, (~ (e0 = op(e0, e2)) | ~ spl30_61), inference(backward_demodulation, [], [f188, f691])).
fof(f691, plain, ((e0 = op(e0, e0)) | ~ spl30_61), inference(avatar_component_clause, [], [f689])).
fof(f188, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2851, plain, (~ spl30_29 | ~ spl30_61), inference(avatar_split_clause, [], [f2841, f689, f553])).
fof(f553, plain, (spl30_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_29])])).
fof(f2841, plain, (~ (e0 = op(e2, e0)) | ~ spl30_61), inference(backward_demodulation, [], [f164, f691])).
fof(f164, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2832, plain, (~ spl30_18 | ~ spl30_24 | spl30_103), inference(avatar_split_clause, [], [f2831, f1055, f531, f506])).
fof(f1055, plain, (spl30_103 <=> (e1 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_103])])).
fof(f2831, plain, (~ (e1 = op(e2, e3)) | (~ spl30_24 | spl30_103)), inference(forward_demodulation, [], [f1057, f533])).
fof(f1057, plain, (~ (e1 = op(e2, op(e2, e2))) | spl30_103), inference(avatar_component_clause, [], [f1055])).
fof(f2830, plain, (spl30_17 | ~ spl30_24 | ~ spl30_110), inference(avatar_split_clause, [], [f2829, f1089, f531, f502])).
fof(f1089, plain, (spl30_110 <=> (e0 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_110])])).
fof(f2829, plain, ((e0 = op(e2, e3)) | (~ spl30_24 | ~ spl30_110)), inference(forward_demodulation, [], [f1090, f533])).
fof(f1090, plain, ((e0 = op(e2, op(e2, e2))) | ~ spl30_110), inference(avatar_component_clause, [], [f1089])).
fof(f2828, plain, (~ spl30_63 | ~ spl30_41 | spl30_111), inference(avatar_split_clause, [], [f2827, f1094, f604, f697])).
fof(f697, plain, (spl30_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl30_63])])).
fof(f604, plain, (spl30_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_41])])).
fof(f1094, plain, (spl30_111 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_111])])).
fof(f2827, plain, (~ (op(e0, e0) = e2) | (~ spl30_41 | spl30_111)), inference(forward_demodulation, [], [f1096, f606])).
fof(f606, plain, ((e0 = op(e1, e1)) | ~ spl30_41), inference(avatar_component_clause, [], [f604])).
fof(f1096, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl30_111), inference(avatar_component_clause, [], [f1094])).
fof(f2826, plain, (~ spl30_64 | ~ spl30_41 | spl30_116), inference(avatar_split_clause, [], [f2825, f1120, f604, f701])).
fof(f701, plain, (spl30_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl30_64])])).
fof(f1120, plain, (spl30_116 <=> (e3 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_116])])).
fof(f2825, plain, (~ (op(e0, e0) = e3) | (~ spl30_41 | spl30_116)), inference(forward_demodulation, [], [f1122, f606])).
fof(f1122, plain, (~ (e3 = op(op(e1, e1), op(e1, e1))) | spl30_116), inference(avatar_component_clause, [], [f1120])).
fof(f2824, plain, (~ spl30_63 | ~ spl30_15), inference(avatar_split_clause, [], [f2823, f493, f697])).
fof(f493, plain, (spl30_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_15])])).
fof(f2823, plain, (~ (op(e0, e0) = e2) | ~ spl30_15), inference(forward_demodulation, [], [f165, f495])).
fof(f495, plain, ((e2 = op(e3, e0)) | ~ spl30_15), inference(avatar_component_clause, [], [f493])).
fof(f165, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2814, plain, (~ spl30_32 | ~ spl30_24), inference(avatar_split_clause, [], [f2813, f531, f565])).
fof(f565, plain, (spl30_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_32])])).
fof(f2813, plain, (~ (e3 = op(e2, e0)) | ~ spl30_24), inference(forward_demodulation, [], [f200, f533])).
fof(f200, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2811, plain, (~ spl30_20 | ~ spl30_24), inference(avatar_split_clause, [], [f2810, f531, f514])).
fof(f514, plain, (spl30_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_20])])).
fof(f2810, plain, (~ (e3 = op(e2, e3)) | ~ spl30_24), inference(forward_demodulation, [], [f204, f533])).
fof(f204, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2808, plain, (~ spl30_3 | ~ spl30_15), inference(avatar_split_clause, [], [f2807, f493, f442])).
fof(f442, plain, (spl30_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_3])])).
fof(f2807, plain, (~ (e2 = op(e3, e3)) | ~ spl30_15), inference(forward_demodulation, [], [f207, f495])).
fof(f207, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2806, plain, (~ spl30_4 | ~ spl30_24 | spl30_117), inference(avatar_split_clause, [], [f2795, f1125, f531, f446])).
fof(f446, plain, (spl30_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_4])])).
fof(f1125, plain, (spl30_117 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_117])])).
fof(f2795, plain, (~ (e3 = op(e3, e3)) | (~ spl30_24 | spl30_117)), inference(backward_demodulation, [], [f1127, f533])).
fof(f1127, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl30_117), inference(avatar_component_clause, [], [f1125])).
fof(f2802, plain, (~ spl30_7 | ~ spl30_15), inference(avatar_split_clause, [], [f2801, f493, f459])).
fof(f459, plain, (spl30_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_7])])).
fof(f2801, plain, (~ (e2 = op(e3, e2)) | ~ spl30_15), inference(backward_demodulation, [], [f206, f495])).
fof(f206, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2797, plain, (~ spl30_1 | ~ spl30_24 | spl30_95), inference(avatar_contradiction_clause, [], [f2796])).
fof(f2796, plain, ($false | (~ spl30_1 | ~ spl30_24 | spl30_95)), inference(subsumption_resolution, [], [f2792, f436])).
fof(f436, plain, ((e0 = op(e3, e3)) | ~ spl30_1), inference(avatar_component_clause, [], [f434])).
fof(f2792, plain, (~ (e0 = op(e3, e3)) | (~ spl30_24 | spl30_95)), inference(backward_demodulation, [], [f1020, f533])).
fof(f1020, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl30_95), inference(avatar_component_clause, [], [f1018])).
fof(f1018, plain, (spl30_95 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_95])])).
fof(f2790, plain, (~ spl30_21 | ~ spl30_29), inference(avatar_split_clause, [], [f2786, f553, f519])).
fof(f519, plain, (spl30_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_21])])).
fof(f2786, plain, (~ (e0 = op(e2, e2)) | ~ spl30_29), inference(backward_demodulation, [], [f200, f555])).
fof(f555, plain, ((e0 = op(e2, e0)) | ~ spl30_29), inference(avatar_component_clause, [], [f553])).
fof(f2784, plain, (~ spl30_7 | ~ spl30_39), inference(avatar_split_clause, [], [f2782, f595, f459])).
fof(f595, plain, (spl30_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_39])])).
fof(f2782, plain, (~ (e2 = op(e3, e2)) | ~ spl30_39), inference(backward_demodulation, [], [f179, f597])).
fof(f597, plain, ((e2 = op(e1, e2)) | ~ spl30_39), inference(avatar_component_clause, [], [f595])).
fof(f179, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2780, plain, (~ spl30_38 | ~ spl30_46), inference(avatar_split_clause, [], [f2776, f625, f591])).
fof(f591, plain, (spl30_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_38])])).
fof(f625, plain, (spl30_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_46])])).
fof(f2776, plain, (~ (e1 = op(e1, e2)) | ~ spl30_46), inference(backward_demodulation, [], [f194, f627])).
fof(f627, plain, ((e1 = op(e1, e0)) | ~ spl30_46), inference(avatar_component_clause, [], [f625])).
fof(f194, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2779, plain, (~ spl30_14 | ~ spl30_46), inference(avatar_split_clause, [], [f2775, f625, f489])).
fof(f489, plain, (spl30_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_14])])).
fof(f2775, plain, (~ (e1 = op(e3, e0)) | ~ spl30_46), inference(backward_demodulation, [], [f167, f627])).
fof(f167, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2773, plain, (~ spl30_21 | ~ spl30_53), inference(avatar_split_clause, [], [f2768, f655, f519])).
fof(f2768, plain, (~ (e0 = op(e2, e2)) | ~ spl30_53), inference(backward_demodulation, [], [f176, f657])).
fof(f657, plain, ((e0 = op(e0, e2)) | ~ spl30_53), inference(avatar_component_clause, [], [f655])).
fof(f176, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2766, plain, (~ spl30_63 | ~ spl30_64), inference(avatar_contradiction_clause, [], [f2765])).
fof(f2765, plain, ($false | (~ spl30_63 | ~ spl30_64)), inference(subsumption_resolution, [], [f2764, f216])).
fof(f216, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2764, plain, ((e2 = e3) | (~ spl30_63 | ~ spl30_64)), inference(backward_demodulation, [], [f703, f699])).
fof(f699, plain, ((op(e0, e0) = e2) | ~ spl30_63), inference(avatar_component_clause, [], [f697])).
fof(f703, plain, ((op(e0, e0) = e3) | ~ spl30_64), inference(avatar_component_clause, [], [f701])).
fof(f2759, plain, (~ spl30_56 | ~ spl30_64), inference(avatar_split_clause, [], [f2752, f701, f667])).
fof(f667, plain, (spl30_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_56])])).
fof(f2752, plain, (~ (e3 = op(e0, e2)) | ~ spl30_64), inference(backward_demodulation, [], [f188, f703])).
fof(f2758, plain, (~ spl30_32 | ~ spl30_64), inference(avatar_split_clause, [], [f2750, f701, f565])).
fof(f2750, plain, (~ (e3 = op(e2, e0)) | ~ spl30_64), inference(backward_demodulation, [], [f164, f703])).
fof(f2747, plain, (~ spl30_47 | ~ spl30_41 | spl30_102), inference(avatar_split_clause, [], [f2746, f1050, f604, f629])).
fof(f629, plain, (spl30_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_47])])).
fof(f1050, plain, (spl30_102 <=> (e2 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_102])])).
fof(f2746, plain, (~ (e2 = op(e1, e0)) | (~ spl30_41 | spl30_102)), inference(forward_demodulation, [], [f1052, f606])).
fof(f1052, plain, (~ (e2 = op(e1, op(e1, e1))) | spl30_102), inference(avatar_component_clause, [], [f1050])).
fof(f2738, plain, (~ spl30_63 | ~ spl30_51), inference(avatar_split_clause, [], [f2737, f646, f697])).
fof(f646, plain, (spl30_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_51])])).
fof(f2737, plain, (~ (op(e0, e0) = e2) | ~ spl30_51), inference(forward_demodulation, [], [f189, f648])).
fof(f648, plain, ((e2 = op(e0, e3)) | ~ spl30_51), inference(avatar_component_clause, [], [f646])).
fof(f189, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2731, plain, (~ spl30_16 | ~ spl30_12), inference(avatar_split_clause, [], [f2730, f480, f497])).
fof(f497, plain, (spl30_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_16])])).
fof(f2730, plain, (~ (e3 = op(e3, e0)) | ~ spl30_12), inference(forward_demodulation, [], [f205, f482])).
fof(f482, plain, ((e3 = op(e3, e1)) | ~ spl30_12), inference(avatar_component_clause, [], [f480])).
fof(f205, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2723, plain, (~ spl30_15 | ~ spl30_1 | spl30_98), inference(avatar_split_clause, [], [f2722, f1031, f434, f493])).
fof(f1031, plain, (spl30_98 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_98])])).
fof(f2722, plain, (~ (e2 = op(e3, e0)) | (~ spl30_1 | spl30_98)), inference(forward_demodulation, [], [f1033, f436])).
fof(f1033, plain, (~ (e2 = op(e3, op(e3, e3))) | spl30_98), inference(avatar_component_clause, [], [f1031])).
fof(f2721, plain, (~ spl30_1 | ~ spl30_2), inference(avatar_contradiction_clause, [], [f2720])).
fof(f2720, plain, ($false | (~ spl30_1 | ~ spl30_2)), inference(subsumption_resolution, [], [f2719, f211])).
fof(f211, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f2719, plain, ((e0 = e1) | (~ spl30_1 | ~ spl30_2)), inference(backward_demodulation, [], [f440, f436])).
fof(f440, plain, ((e1 = op(e3, e3)) | ~ spl30_2), inference(avatar_component_clause, [], [f438])).
fof(f438, plain, (spl30_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_2])])).
fof(f2714, plain, (~ spl30_3 | ~ spl30_7), inference(avatar_split_clause, [], [f2712, f459, f442])).
fof(f2712, plain, (~ (e2 = op(e3, e3)) | ~ spl30_7), inference(backward_demodulation, [], [f210, f461])).
fof(f461, plain, ((e2 = op(e3, e2)) | ~ spl30_7), inference(avatar_component_clause, [], [f459])).
fof(f2711, plain, (~ spl30_2 | ~ spl30_18), inference(avatar_split_clause, [], [f2708, f506, f438])).
fof(f2708, plain, (~ (e1 = op(e3, e3)) | ~ spl30_18), inference(backward_demodulation, [], [f186, f508])).
fof(f508, plain, ((e1 = op(e2, e3)) | ~ spl30_18), inference(avatar_component_clause, [], [f506])).
fof(f2705, plain, (~ spl30_21 | ~ spl30_61 | spl30_95), inference(avatar_contradiction_clause, [], [f2704])).
fof(f2704, plain, ($false | (~ spl30_21 | ~ spl30_61 | spl30_95)), inference(subsumption_resolution, [], [f2699, f691])).
fof(f2699, plain, (~ (e0 = op(e0, e0)) | (~ spl30_21 | spl30_95)), inference(backward_demodulation, [], [f1020, f521])).
fof(f521, plain, ((e0 = op(e2, e2)) | ~ spl30_21), inference(avatar_component_clause, [], [f519])).
fof(f2703, plain, (~ spl30_5 | ~ spl30_21), inference(avatar_split_clause, [], [f2694, f519, f451])).
fof(f2694, plain, (~ (e0 = op(e3, e2)) | ~ spl30_21), inference(backward_demodulation, [], [f180, f521])).
fof(f180, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2693, plain, (~ spl30_20 | ~ spl30_32), inference(avatar_split_clause, [], [f2691, f565, f514])).
fof(f2691, plain, (~ (e3 = op(e2, e3)) | ~ spl30_32), inference(backward_demodulation, [], [f201, f567])).
fof(f567, plain, ((e3 = op(e2, e0)) | ~ spl30_32), inference(avatar_component_clause, [], [f565])).
fof(f2690, plain, (~ spl30_20 | ~ spl30_36), inference(avatar_split_clause, [], [f2688, f582, f514])).
fof(f2688, plain, (~ (e3 = op(e2, e3)) | ~ spl30_36), inference(backward_demodulation, [], [f184, f584])).
fof(f584, plain, ((e3 = op(e1, e3)) | ~ spl30_36), inference(avatar_component_clause, [], [f582])).
fof(f2687, plain, (~ spl30_34 | ~ spl30_38), inference(avatar_split_clause, [], [f2683, f591, f574])).
fof(f2683, plain, (~ (e1 = op(e1, e3)) | ~ spl30_38), inference(backward_demodulation, [], [f198, f593])).
fof(f593, plain, ((e1 = op(e1, e2)) | ~ spl30_38), inference(avatar_component_clause, [], [f591])).
fof(f198, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2686, plain, (~ spl30_22 | ~ spl30_38), inference(avatar_split_clause, [], [f2681, f591, f523])).
fof(f2681, plain, (~ (e1 = op(e2, e2)) | ~ spl30_38), inference(backward_demodulation, [], [f178, f593])).
fof(f178, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2680, plain, (~ spl30_39 | ~ spl30_47), inference(avatar_split_clause, [], [f2677, f629, f595])).
fof(f2677, plain, (~ (e2 = op(e1, e2)) | ~ spl30_47), inference(backward_demodulation, [], [f194, f631])).
fof(f631, plain, ((e2 = op(e1, e0)) | ~ spl30_47), inference(avatar_component_clause, [], [f629])).
fof(f2675, plain, (~ spl30_3 | ~ spl30_51), inference(avatar_split_clause, [], [f2674, f646, f442])).
fof(f2674, plain, (~ (e2 = op(e3, e3)) | ~ spl30_51), inference(backward_demodulation, [], [f183, f648])).
fof(f183, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2668, plain, (~ spl30_61 | ~ spl30_107), inference(avatar_contradiction_clause, [], [f2667])).
fof(f2667, plain, ($false | (~ spl30_61 | ~ spl30_107)), inference(subsumption_resolution, [], [f2666, f213])).
fof(f213, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f2666, plain, ((e0 = e3) | (~ spl30_61 | ~ spl30_107)), inference(forward_demodulation, [], [f2660, f691])).
fof(f2660, plain, ((op(e0, e0) = e3) | (~ spl30_61 | ~ spl30_107)), inference(backward_demodulation, [], [f1075, f691])).
fof(f1075, plain, ((e3 = op(e0, op(e0, e0))) | ~ spl30_107), inference(avatar_component_clause, [], [f1074])).
fof(f1074, plain, (spl30_107 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_107])])).
fof(f2664, plain, (~ spl30_49 | ~ spl30_61), inference(avatar_split_clause, [], [f2655, f689, f638])).
fof(f638, plain, (spl30_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_49])])).
fof(f2655, plain, (~ (e0 = op(e0, e3)) | ~ spl30_61), inference(backward_demodulation, [], [f189, f691])).
fof(f2651, plain, (spl30_47 | ~ spl30_41 | ~ spl30_102), inference(avatar_split_clause, [], [f2650, f1050, f604, f629])).
fof(f2650, plain, ((e2 = op(e1, e0)) | (~ spl30_41 | ~ spl30_102)), inference(forward_demodulation, [], [f1051, f606])).
fof(f1051, plain, ((e2 = op(e1, op(e1, e1))) | ~ spl30_102), inference(avatar_component_clause, [], [f1050])).
fof(f2641, plain, (~ spl30_23 | ~ spl30_27), inference(avatar_split_clause, [], [f2640, f544, f527])).
fof(f527, plain, (spl30_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_23])])).
fof(f544, plain, (spl30_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_27])])).
fof(f2640, plain, (~ (e2 = op(e2, e2)) | ~ spl30_27), inference(forward_demodulation, [], [f202, f546])).
fof(f546, plain, ((e2 = op(e2, e1)) | ~ spl30_27), inference(avatar_component_clause, [], [f544])).
fof(f202, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2639, plain, (~ spl30_24 | ~ spl30_56), inference(avatar_split_clause, [], [f2638, f667, f531])).
fof(f2638, plain, (~ (e3 = op(e2, e2)) | ~ spl30_56), inference(forward_demodulation, [], [f176, f669])).
fof(f669, plain, ((e3 = op(e0, e2)) | ~ spl30_56), inference(avatar_component_clause, [], [f667])).
fof(f2637, plain, (~ spl30_6 | ~ spl30_14), inference(avatar_split_clause, [], [f2636, f489, f455])).
fof(f2636, plain, (~ (e1 = op(e3, e2)) | ~ spl30_14), inference(forward_demodulation, [], [f206, f491])).
fof(f491, plain, ((e1 = op(e3, e0)) | ~ spl30_14), inference(avatar_component_clause, [], [f489])).
fof(f2633, plain, (~ spl30_2 | ~ spl30_14), inference(avatar_split_clause, [], [f2632, f489, f438])).
fof(f2632, plain, (~ (e1 = op(e3, e3)) | ~ spl30_14), inference(forward_demodulation, [], [f207, f491])).
fof(f2631, plain, (~ spl30_4 | ~ spl30_12), inference(avatar_split_clause, [], [f2630, f480, f446])).
fof(f2630, plain, (~ (e3 = op(e3, e3)) | ~ spl30_12), inference(forward_demodulation, [], [f209, f482])).
fof(f209, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2625, plain, (~ spl30_17 | ~ spl30_29), inference(avatar_split_clause, [], [f2621, f553, f502])).
fof(f2621, plain, (~ (e0 = op(e2, e3)) | ~ spl30_29), inference(backward_demodulation, [], [f201, f555])).
fof(f2614, plain, (~ spl30_41 | ~ spl30_48 | spl30_100), inference(avatar_contradiction_clause, [], [f2613])).
fof(f2613, plain, ($false | (~ spl30_41 | ~ spl30_48 | spl30_100)), inference(subsumption_resolution, [], [f2609, f635])).
fof(f2609, plain, (~ (e3 = op(e1, e0)) | (~ spl30_41 | spl30_100)), inference(backward_demodulation, [], [f1042, f606])).
fof(f1042, plain, (~ (e3 = op(e1, op(e1, e1))) | spl30_100), inference(avatar_component_clause, [], [f1040])).
fof(f1040, plain, (spl30_100 <=> (e3 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_100])])).
fof(f2595, plain, (~ spl30_17 | ~ spl30_49), inference(avatar_split_clause, [], [f2591, f638, f502])).
fof(f2591, plain, (~ (e0 = op(e2, e3)) | ~ spl30_49), inference(backward_demodulation, [], [f182, f640])).
fof(f640, plain, ((e0 = op(e0, e3)) | ~ spl30_49), inference(avatar_component_clause, [], [f638])).
fof(f182, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2587, plain, (~ spl30_50 | ~ spl30_58), inference(avatar_split_clause, [], [f2583, f676, f642])).
fof(f642, plain, (spl30_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_50])])).
fof(f676, plain, (spl30_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_58])])).
fof(f2583, plain, (~ (e1 = op(e0, e3)) | ~ spl30_58), inference(backward_demodulation, [], [f191, f678])).
fof(f678, plain, ((e1 = op(e0, e1)) | ~ spl30_58), inference(avatar_component_clause, [], [f676])).
fof(f191, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2586, plain, (~ spl30_10 | ~ spl30_58), inference(avatar_split_clause, [], [f2581, f676, f472])).
fof(f2581, plain, (~ (e1 = op(e3, e1)) | ~ spl30_58), inference(backward_demodulation, [], [f171, f678])).
fof(f2577, plain, (spl30_56 | ~ spl30_63 | ~ spl30_107), inference(avatar_split_clause, [], [f2570, f1074, f697, f667])).
fof(f2570, plain, ((e3 = op(e0, e2)) | (~ spl30_63 | ~ spl30_107)), inference(backward_demodulation, [], [f1075, f699])).
fof(f2576, plain, (~ spl30_22 | ~ spl30_63 | spl30_106), inference(avatar_contradiction_clause, [], [f2575])).
fof(f2575, plain, ($false | (~ spl30_22 | ~ spl30_63 | spl30_106)), inference(subsumption_resolution, [], [f2569, f525])).
fof(f2569, plain, (~ (e1 = op(e2, e2)) | (~ spl30_63 | spl30_106)), inference(backward_demodulation, [], [f1072, f699])).
fof(f1072, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl30_106), inference(avatar_component_clause, [], [f1070])).
fof(f1070, plain, (spl30_106 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_106])])).
fof(f2574, plain, (~ spl30_55 | ~ spl30_63), inference(avatar_split_clause, [], [f2566, f697, f663])).
fof(f663, plain, (spl30_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_55])])).
fof(f2566, plain, (~ (e2 = op(e0, e2)) | ~ spl30_63), inference(backward_demodulation, [], [f188, f699])).
fof(f2573, plain, (~ spl30_31 | ~ spl30_63), inference(avatar_split_clause, [], [f2564, f697, f561])).
fof(f561, plain, (spl30_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_31])])).
fof(f2564, plain, (~ (e2 = op(e2, e0)) | ~ spl30_63), inference(backward_demodulation, [], [f164, f699])).
fof(f2561, plain, (~ spl30_3 | ~ spl30_5 | spl30_108), inference(avatar_contradiction_clause, [], [f2560])).
fof(f2560, plain, ($false | (~ spl30_3 | ~ spl30_5 | spl30_108)), inference(subsumption_resolution, [], [f2559, f453])).
fof(f2559, plain, (~ (e0 = op(e3, e2)) | (~ spl30_3 | spl30_108)), inference(forward_demodulation, [], [f1081, f444])).
fof(f444, plain, ((e2 = op(e3, e3)) | ~ spl30_3), inference(avatar_component_clause, [], [f442])).
fof(f1081, plain, (~ (e0 = op(e3, op(e3, e3))) | spl30_108), inference(avatar_component_clause, [], [f1079])).
fof(f1079, plain, (spl30_108 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_108])])).
fof(f2544, plain, (~ spl30_10 | ~ spl30_14), inference(avatar_split_clause, [], [f2543, f489, f472])).
fof(f2543, plain, (~ (e1 = op(e3, e1)) | ~ spl30_14), inference(forward_demodulation, [], [f205, f491])).
fof(f2542, plain, (~ spl30_5 | ~ spl30_8), inference(avatar_contradiction_clause, [], [f2541])).
fof(f2541, plain, ($false | (~ spl30_5 | ~ spl30_8)), inference(subsumption_resolution, [], [f2540, f213])).
fof(f2540, plain, ((e0 = e3) | (~ spl30_5 | ~ spl30_8)), inference(forward_demodulation, [], [f465, f453])).
fof(f465, plain, ((e3 = op(e3, e2)) | ~ spl30_8), inference(avatar_component_clause, [], [f463])).
fof(f463, plain, (spl30_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_8])])).
fof(f2527, plain, (~ spl30_20 | ~ spl30_28), inference(avatar_split_clause, [], [f2526, f548, f514])).
fof(f548, plain, (spl30_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_28])])).
fof(f2526, plain, (~ (e3 = op(e2, e3)) | ~ spl30_28), inference(backward_demodulation, [], [f203, f550])).
fof(f550, plain, ((e3 = op(e2, e1)) | ~ spl30_28), inference(avatar_component_clause, [], [f548])).
fof(f203, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2524, plain, (~ spl30_23 | ~ spl30_31), inference(avatar_split_clause, [], [f2521, f561, f527])).
fof(f2521, plain, (~ (e2 = op(e2, e2)) | ~ spl30_31), inference(backward_demodulation, [], [f200, f563])).
fof(f563, plain, ((e2 = op(e2, e0)) | ~ spl30_31), inference(avatar_component_clause, [], [f561])).
fof(f2497, plain, (~ spl30_34 | ~ spl30_50), inference(avatar_split_clause, [], [f2493, f642, f574])).
fof(f2493, plain, (~ (e1 = op(e1, e3)) | ~ spl30_50), inference(backward_demodulation, [], [f181, f644])).
fof(f644, plain, ((e1 = op(e0, e3)) | ~ spl30_50), inference(avatar_component_clause, [], [f642])).
fof(f181, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2492, plain, (~ spl30_23 | ~ spl30_55), inference(avatar_split_clause, [], [f2489, f663, f527])).
fof(f2489, plain, (~ (e2 = op(e2, e2)) | ~ spl30_55), inference(backward_demodulation, [], [f176, f665])).
fof(f665, plain, ((e2 = op(e0, e2)) | ~ spl30_55), inference(avatar_component_clause, [], [f663])).
fof(f2487, plain, (~ spl30_49 | ~ spl30_57), inference(avatar_split_clause, [], [f2482, f672, f638])).
fof(f672, plain, (spl30_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_57])])).
fof(f2482, plain, (~ (e0 = op(e0, e3)) | ~ spl30_57), inference(backward_demodulation, [], [f191, f674])).
fof(f674, plain, ((e0 = op(e0, e1)) | ~ spl30_57), inference(avatar_component_clause, [], [f672])).
fof(f2486, plain, (~ spl30_25 | ~ spl30_57), inference(avatar_split_clause, [], [f2480, f672, f536])).
fof(f536, plain, (spl30_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_25])])).
fof(f2480, plain, (~ (e0 = op(e2, e1)) | ~ spl30_57), inference(backward_demodulation, [], [f170, f674])).
fof(f170, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2478, plain, (~ spl30_50 | ~ spl30_64 | spl30_114), inference(avatar_split_clause, [], [f2475, f1110, f701, f642])).
fof(f1110, plain, (spl30_114 <=> (e1 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_114])])).
fof(f2475, plain, (~ (e1 = op(e0, e3)) | (~ spl30_64 | spl30_114)), inference(backward_demodulation, [], [f1112, f703])).
fof(f1112, plain, (~ (e1 = op(e0, op(e0, e0))) | spl30_114), inference(avatar_component_clause, [], [f1110])).
fof(f2477, plain, (~ spl30_60 | ~ spl30_64), inference(avatar_split_clause, [], [f2468, f701, f684])).
fof(f2468, plain, (~ (e3 = op(e0, e1)) | ~ spl30_64), inference(backward_demodulation, [], [f187, f703])).
fof(f187, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2476, plain, (~ spl30_16 | ~ spl30_64), inference(avatar_split_clause, [], [f2467, f701, f497])).
fof(f2467, plain, (~ (e3 = op(e3, e0)) | ~ spl30_64), inference(backward_demodulation, [], [f165, f703])).
fof(f2453, plain, (~ spl30_42 | ~ spl30_10), inference(avatar_split_clause, [], [f2452, f472, f608])).
fof(f608, plain, (spl30_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_42])])).
fof(f2452, plain, (~ (e1 = op(e1, e1)) | ~ spl30_10), inference(forward_demodulation, [], [f173, f474])).
fof(f173, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2446, plain, (~ spl30_22 | ~ spl30_3 | spl30_105), inference(avatar_split_clause, [], [f2437, f1065, f442, f523])).
fof(f1065, plain, (spl30_105 <=> (e1 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_105])])).
fof(f2437, plain, (~ (e1 = op(e2, e2)) | (~ spl30_3 | spl30_105)), inference(backward_demodulation, [], [f1067, f444])).
fof(f1067, plain, (~ (e1 = op(op(e3, e3), op(e3, e3))) | spl30_105), inference(avatar_component_clause, [], [f1065])).
fof(f2430, plain, (~ spl30_2 | ~ spl30_10), inference(avatar_split_clause, [], [f2427, f472, f438])).
fof(f2427, plain, (~ (e1 = op(e3, e3)) | ~ spl30_10), inference(backward_demodulation, [], [f209, f474])).
fof(f2425, plain, (~ spl30_8 | ~ spl30_16), inference(avatar_split_clause, [], [f2422, f497, f463])).
fof(f2422, plain, (~ (e3 = op(e3, e2)) | ~ spl30_16), inference(backward_demodulation, [], [f206, f499])).
fof(f499, plain, ((e3 = op(e3, e0)) | ~ spl30_16), inference(avatar_component_clause, [], [f497])).
fof(f2420, plain, (~ spl30_23 | ~ spl30_117), inference(avatar_contradiction_clause, [], [f2419])).
fof(f2419, plain, ($false | (~ spl30_23 | ~ spl30_117)), inference(subsumption_resolution, [], [f2418, f216])).
fof(f2418, plain, ((e2 = e3) | (~ spl30_23 | ~ spl30_117)), inference(forward_demodulation, [], [f2415, f529])).
fof(f529, plain, ((e2 = op(e2, e2)) | ~ spl30_23), inference(avatar_component_clause, [], [f527])).
fof(f2415, plain, ((e3 = op(e2, e2)) | (~ spl30_23 | ~ spl30_117)), inference(backward_demodulation, [], [f1126, f529])).
fof(f1126, plain, ((e3 = op(op(e2, e2), op(e2, e2))) | ~ spl30_117), inference(avatar_component_clause, [], [f1125])).
fof(f2417, plain, (~ spl30_21 | ~ spl30_23 | spl30_110), inference(avatar_split_clause, [], [f2414, f1089, f527, f519])).
fof(f2414, plain, (~ (e0 = op(e2, e2)) | (~ spl30_23 | spl30_110)), inference(backward_demodulation, [], [f1091, f529])).
fof(f1091, plain, (~ (e0 = op(e2, op(e2, e2))) | spl30_110), inference(avatar_component_clause, [], [f1089])).
fof(f2416, plain, (~ spl30_19 | ~ spl30_23), inference(avatar_split_clause, [], [f2410, f527, f510])).
fof(f510, plain, (spl30_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_19])])).
fof(f2410, plain, (~ (e2 = op(e2, e3)) | ~ spl30_23), inference(backward_demodulation, [], [f204, f529])).
fof(f2408, plain, (~ spl30_21 | ~ spl30_25), inference(avatar_split_clause, [], [f2403, f536, f519])).
fof(f2403, plain, (~ (e0 = op(e2, e2)) | ~ spl30_25), inference(backward_demodulation, [], [f202, f538])).
fof(f538, plain, ((e0 = op(e2, e1)) | ~ spl30_25), inference(avatar_component_clause, [], [f536])).
fof(f2401, plain, (~ spl30_2 | ~ spl30_34), inference(avatar_split_clause, [], [f2399, f574, f438])).
fof(f2399, plain, (~ (e1 = op(e3, e3)) | ~ spl30_34), inference(backward_demodulation, [], [f185, f576])).
fof(f185, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2396, plain, (~ spl30_36 | ~ spl30_40), inference(avatar_split_clause, [], [f2394, f599, f582])).
fof(f599, plain, (spl30_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_40])])).
fof(f2394, plain, (~ (e3 = op(e1, e3)) | ~ spl30_40), inference(backward_demodulation, [], [f198, f601])).
fof(f601, plain, ((e3 = op(e1, e2)) | ~ spl30_40), inference(avatar_component_clause, [], [f599])).
fof(f2395, plain, (~ spl30_8 | ~ spl30_40), inference(avatar_split_clause, [], [f2393, f599, f463])).
fof(f2393, plain, (~ (e3 = op(e3, e2)) | ~ spl30_40), inference(backward_demodulation, [], [f179, f601])).
fof(f2385, plain, (~ spl30_34 | ~ spl30_42), inference(avatar_split_clause, [], [f2374, f608, f574])).
fof(f2374, plain, (~ (e1 = op(e1, e3)) | ~ spl30_42), inference(backward_demodulation, [], [f197, f610])).
fof(f610, plain, ((e1 = op(e1, e1)) | ~ spl30_42), inference(avatar_component_clause, [], [f608])).
fof(f197, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2384, plain, (~ spl30_38 | ~ spl30_42), inference(avatar_split_clause, [], [f2373, f608, f591])).
fof(f2373, plain, (~ (e1 = op(e1, e2)) | ~ spl30_42), inference(backward_demodulation, [], [f196, f610])).
fof(f196, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2370, plain, (~ spl30_45 | ~ spl30_47), inference(avatar_contradiction_clause, [], [f2369])).
fof(f2369, plain, ($false | (~ spl30_45 | ~ spl30_47)), inference(subsumption_resolution, [], [f2368, f212])).
fof(f212, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2368, plain, ((e0 = e2) | (~ spl30_45 | ~ spl30_47)), inference(backward_demodulation, [], [f631, f623])).
fof(f623, plain, ((e0 = op(e1, e0)) | ~ spl30_45), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl30_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_45])])).
fof(f2366, plain, (~ spl30_38 | ~ spl30_54), inference(avatar_split_clause, [], [f2361, f659, f591])).
fof(f2361, plain, (~ (e1 = op(e1, e2)) | ~ spl30_54), inference(backward_demodulation, [], [f175, f661])).
fof(f175, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2360, plain, (~ spl30_59 | ~ spl30_60), inference(avatar_contradiction_clause, [], [f2359])).
fof(f2359, plain, ($false | (~ spl30_59 | ~ spl30_60)), inference(subsumption_resolution, [], [f2358, f216])).
fof(f2358, plain, ((e2 = e3) | (~ spl30_59 | ~ spl30_60)), inference(backward_demodulation, [], [f686, f682])).
fof(f682, plain, ((e2 = op(e0, e1)) | ~ spl30_59), inference(avatar_component_clause, [], [f680])).
fof(f680, plain, (spl30_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_59])])).
fof(f2357, plain, (~ spl30_28 | ~ spl30_60), inference(avatar_split_clause, [], [f2354, f684, f548])).
fof(f2354, plain, (~ (e3 = op(e2, e1)) | ~ spl30_60), inference(backward_demodulation, [], [f170, f686])).
fof(f2347, plain, (~ spl30_59 | ~ spl30_63), inference(avatar_split_clause, [], [f2340, f697, f680])).
fof(f2340, plain, (~ (e2 = op(e0, e1)) | ~ spl30_63), inference(backward_demodulation, [], [f187, f699])).
fof(f2338, plain, (~ spl30_63 | ~ spl30_47), inference(avatar_split_clause, [], [f2337, f629, f697])).
fof(f2337, plain, (~ (op(e0, e0) = e2) | ~ spl30_47), inference(forward_demodulation, [], [f163, f631])).
fof(f163, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2332, plain, (~ spl30_43 | ~ spl30_47), inference(avatar_split_clause, [], [f2331, f629, f612])).
fof(f612, plain, (spl30_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_43])])).
fof(f2331, plain, (~ (e2 = op(e1, e1)) | ~ spl30_47), inference(forward_demodulation, [], [f193, f631])).
fof(f193, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2329, plain, (~ spl30_14 | ~ spl30_30), inference(avatar_split_clause, [], [f2328, f557, f489])).
fof(f2328, plain, (~ (e1 = op(e3, e0)) | ~ spl30_30), inference(forward_demodulation, [], [f168, f559])).
fof(f168, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2327, plain, (~ spl30_15 | ~ spl30_47), inference(avatar_split_clause, [], [f2326, f629, f493])).
fof(f2326, plain, (~ (e2 = op(e3, e0)) | ~ spl30_47), inference(forward_demodulation, [], [f167, f631])).
fof(f2323, plain, (~ spl30_2 | ~ spl30_11 | spl30_98), inference(avatar_contradiction_clause, [], [f2322])).
fof(f2322, plain, ($false | (~ spl30_2 | ~ spl30_11 | spl30_98)), inference(subsumption_resolution, [], [f2321, f478])).
fof(f478, plain, ((e2 = op(e3, e1)) | ~ spl30_11), inference(avatar_component_clause, [], [f476])).
fof(f476, plain, (spl30_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_11])])).
fof(f2321, plain, (~ (e2 = op(e3, e1)) | (~ spl30_2 | spl30_98)), inference(forward_demodulation, [], [f1033, f440])).
fof(f2320, plain, (~ spl30_21 | ~ spl30_30 | spl30_103), inference(avatar_contradiction_clause, [], [f2319])).
fof(f2319, plain, ($false | (~ spl30_21 | ~ spl30_30 | spl30_103)), inference(subsumption_resolution, [], [f2318, f559])).
fof(f2318, plain, (~ (e1 = op(e2, e0)) | (~ spl30_21 | spl30_103)), inference(forward_demodulation, [], [f1057, f521])).
fof(f2289, plain, (~ spl30_2 | ~ spl30_41 | spl30_97), inference(avatar_contradiction_clause, [], [f2288])).
fof(f2288, plain, ($false | (~ spl30_2 | ~ spl30_41 | spl30_97)), inference(subsumption_resolution, [], [f2285, f606])).
fof(f2285, plain, (~ (e0 = op(e1, e1)) | (~ spl30_2 | spl30_97)), inference(backward_demodulation, [], [f1029, f440])).
fof(f1029, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl30_97), inference(avatar_component_clause, [], [f1027])).
fof(f1027, plain, (spl30_97 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl30_97])])).
fof(f2281, plain, (~ spl30_3 | ~ spl30_11), inference(avatar_split_clause, [], [f2280, f476, f442])).
fof(f2280, plain, (~ (e2 = op(e3, e3)) | ~ spl30_11), inference(backward_demodulation, [], [f209, f478])).
fof(f2277, plain, (~ spl30_3 | ~ spl30_19), inference(avatar_split_clause, [], [f2276, f510, f442])).
fof(f2276, plain, (~ (e2 = op(e3, e3)) | ~ spl30_19), inference(backward_demodulation, [], [f186, f512])).
fof(f512, plain, ((e2 = op(e2, e3)) | ~ spl30_19), inference(avatar_component_clause, [], [f510])).
fof(f2274, plain, (~ spl30_21 | ~ spl30_64 | spl30_117), inference(avatar_contradiction_clause, [], [f2273])).
fof(f2273, plain, ($false | (~ spl30_21 | ~ spl30_64 | spl30_117)), inference(subsumption_resolution, [], [f2272, f703])).
fof(f2272, plain, (~ (op(e0, e0) = e3) | (~ spl30_21 | spl30_117)), inference(backward_demodulation, [], [f1127, f521])).
fof(f2264, plain, (~ spl30_24 | ~ spl30_28), inference(avatar_split_clause, [], [f2262, f548, f531])).
fof(f2262, plain, (~ (e3 = op(e2, e2)) | ~ spl30_28), inference(backward_demodulation, [], [f202, f550])).
fof(f2263, plain, (~ spl30_12 | ~ spl30_28), inference(avatar_split_clause, [], [f2260, f548, f480])).
fof(f2260, plain, (~ (e3 = op(e3, e1)) | ~ spl30_28), inference(backward_demodulation, [], [f174, f550])).
fof(f174, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2253, plain, (~ spl30_6 | ~ spl30_38), inference(avatar_split_clause, [], [f2251, f591, f455])).
fof(f2251, plain, (~ (e1 = op(e3, e2)) | ~ spl30_38), inference(backward_demodulation, [], [f179, f593])).
fof(f2248, plain, (~ spl30_41 | ~ spl30_42), inference(avatar_contradiction_clause, [], [f2247])).
fof(f2247, plain, ($false | (~ spl30_41 | ~ spl30_42)), inference(subsumption_resolution, [], [f2246, f211])).
fof(f2246, plain, ((e0 = e1) | (~ spl30_41 | ~ spl30_42)), inference(backward_demodulation, [], [f610, f606])).
fof(f2244, plain, (~ spl30_31 | ~ spl30_47), inference(avatar_split_clause, [], [f2243, f629, f561])).
fof(f2243, plain, (~ (e2 = op(e2, e0)) | ~ spl30_47), inference(backward_demodulation, [], [f166, f631])).
fof(f166, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2232, plain, (~ spl30_3 | ~ spl30_64 | spl30_113), inference(avatar_split_clause, [], [f2231, f1104, f701, f442])).
fof(f1104, plain, (spl30_113 <=> (e2 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_113])])).
fof(f2231, plain, (~ (e2 = op(e3, e3)) | (~ spl30_64 | spl30_113)), inference(forward_demodulation, [], [f1106, f703])).
fof(f1106, plain, (~ (e2 = op(op(e0, e0), op(e0, e0))) | spl30_113), inference(avatar_component_clause, [], [f1104])).
fof(f2212, plain, (~ spl30_46 | ~ spl30_42), inference(avatar_split_clause, [], [f2211, f608, f625])).
fof(f2211, plain, (~ (e1 = op(e1, e0)) | ~ spl30_42), inference(forward_demodulation, [], [f193, f610])).
fof(f2210, plain, (~ spl30_42 | ~ spl30_58), inference(avatar_contradiction_clause, [], [f2209])).
fof(f2209, plain, ($false | (~ spl30_42 | ~ spl30_58)), inference(subsumption_resolution, [], [f2208, f678])).
fof(f2208, plain, (~ (e1 = op(e0, e1)) | ~ spl30_42), inference(forward_demodulation, [], [f169, f610])).
fof(f169, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2207, plain, (spl30_41 | ~ spl30_42 | ~ spl30_115), inference(avatar_split_clause, [], [f2206, f1115, f608, f604])).
fof(f1115, plain, (spl30_115 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_115])])).
fof(f2206, plain, ((e0 = op(e1, e1)) | (~ spl30_42 | ~ spl30_115)), inference(forward_demodulation, [], [f1116, f610])).
fof(f1116, plain, ((e0 = op(e1, op(e1, e1))) | ~ spl30_115), inference(avatar_component_clause, [], [f1115])).
fof(f2193, plain, (~ spl30_19 | ~ spl30_31), inference(avatar_split_clause, [], [f2192, f561, f510])).
fof(f2192, plain, (~ (e2 = op(e2, e3)) | ~ spl30_31), inference(backward_demodulation, [], [f201, f563])).
fof(f2190, plain, (~ spl30_5 | ~ spl30_37), inference(avatar_split_clause, [], [f2189, f587, f451])).
fof(f587, plain, (spl30_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl30_37])])).
fof(f2189, plain, (~ (e0 = op(e3, e2)) | ~ spl30_37), inference(backward_demodulation, [], [f179, f589])).
fof(f589, plain, ((e0 = op(e1, e2)) | ~ spl30_37), inference(avatar_component_clause, [], [f587])).
fof(f2185, plain, (~ spl30_42 | ~ spl30_43), inference(avatar_contradiction_clause, [], [f2184])).
fof(f2184, plain, ($false | (~ spl30_42 | ~ spl30_43)), inference(subsumption_resolution, [], [f2183, f214])).
fof(f214, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2183, plain, ((e1 = e2) | (~ spl30_42 | ~ spl30_43)), inference(backward_demodulation, [], [f614, f610])).
fof(f614, plain, ((e2 = op(e1, e1)) | ~ spl30_43), inference(avatar_component_clause, [], [f612])).
fof(f2170, plain, (~ spl30_2 | ~ spl30_64 | spl30_106), inference(avatar_split_clause, [], [f2163, f1070, f701, f438])).
fof(f2163, plain, (~ (e1 = op(e3, e3)) | (~ spl30_64 | spl30_106)), inference(backward_demodulation, [], [f1072, f703])).
fof(f2159, plain, (spl30_37 | ~ spl30_43 | ~ spl30_115), inference(avatar_split_clause, [], [f2158, f1115, f612, f587])).
fof(f2158, plain, ((e0 = op(e1, e2)) | (~ spl30_43 | ~ spl30_115)), inference(forward_demodulation, [], [f1116, f614])).
fof(f2157, plain, (~ spl30_24 | ~ spl30_43 | spl30_116), inference(avatar_contradiction_clause, [], [f2156])).
fof(f2156, plain, ($false | (~ spl30_24 | ~ spl30_43 | spl30_116)), inference(subsumption_resolution, [], [f2155, f533])).
fof(f2155, plain, (~ (e3 = op(e2, e2)) | (~ spl30_43 | spl30_116)), inference(forward_demodulation, [], [f1122, f614])).
fof(f2138, plain, (~ spl30_39 | ~ spl30_43), inference(avatar_split_clause, [], [f2137, f612, f595])).
fof(f2137, plain, (~ (e2 = op(e1, e2)) | ~ spl30_43), inference(forward_demodulation, [], [f196, f614])).
fof(f2136, plain, (~ spl30_39 | ~ spl30_55), inference(avatar_split_clause, [], [f2135, f663, f595])).
fof(f2135, plain, (~ (e2 = op(e1, e2)) | ~ spl30_55), inference(forward_demodulation, [], [f175, f665])).
fof(f2134, plain, (~ spl30_40 | ~ spl30_24), inference(avatar_split_clause, [], [f2133, f531, f599])).
fof(f2133, plain, (~ (e3 = op(e1, e2)) | ~ spl30_24), inference(forward_demodulation, [], [f178, f533])).
fof(f2115, plain, (~ spl30_5 | ~ spl30_13), inference(avatar_split_clause, [], [f2114, f485, f451])).
fof(f485, plain, (spl30_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl30_13])])).
fof(f2114, plain, (~ (e0 = op(e3, e2)) | ~ spl30_13), inference(forward_demodulation, [], [f206, f487])).
fof(f487, plain, ((e0 = op(e3, e0)) | ~ spl30_13), inference(avatar_component_clause, [], [f485])).
fof(f2113, plain, (~ spl30_8 | ~ spl30_24), inference(avatar_split_clause, [], [f2112, f531, f463])).
fof(f2112, plain, (~ (e3 = op(e3, e2)) | ~ spl30_24), inference(forward_demodulation, [], [f180, f533])).
fof(f2111, plain, (~ spl30_7 | ~ spl30_55), inference(avatar_split_clause, [], [f2110, f663, f459])).
fof(f2110, plain, (~ (e2 = op(e3, e2)) | ~ spl30_55), inference(forward_demodulation, [], [f177, f665])).
fof(f2099, plain, (~ spl30_35 | ~ spl30_3), inference(avatar_split_clause, [], [f2098, f442, f578])).
fof(f578, plain, (spl30_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_35])])).
fof(f2098, plain, (~ (e2 = op(e1, e3)) | ~ spl30_3), inference(forward_demodulation, [], [f185, f444])).
fof(f2095, plain, (~ spl30_2 | ~ spl30_24 | spl30_104), inference(avatar_split_clause, [], [f2075, f1060, f531, f438])).
fof(f1060, plain, (spl30_104 <=> (e1 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_104])])).
fof(f2075, plain, (~ (e1 = op(e3, e3)) | (~ spl30_24 | spl30_104)), inference(backward_demodulation, [], [f1062, f533])).
fof(f1062, plain, (~ (e1 = op(op(e2, e2), op(e2, e2))) | spl30_104), inference(avatar_component_clause, [], [f1060])).
fof(f2094, plain, (~ spl30_2 | ~ spl30_3), inference(avatar_contradiction_clause, [], [f2093])).
fof(f2093, plain, ($false | (~ spl30_2 | ~ spl30_3)), inference(subsumption_resolution, [], [f2092, f214])).
fof(f2092, plain, ((e1 = e2) | (~ spl30_2 | ~ spl30_3)), inference(forward_demodulation, [], [f444, f440])).
fof(f2089, plain, (~ spl30_13 | ~ spl30_16), inference(avatar_contradiction_clause, [], [f2088])).
fof(f2088, plain, ($false | (~ spl30_13 | ~ spl30_16)), inference(subsumption_resolution, [], [f2086, f213])).
fof(f2086, plain, ((e0 = e3) | (~ spl30_13 | ~ spl30_16)), inference(backward_demodulation, [], [f499, f487])).
fof(f2081, plain, (~ spl30_17 | ~ spl30_24 | spl30_110), inference(avatar_split_clause, [], [f2076, f1089, f531, f502])).
fof(f2076, plain, (~ (e0 = op(e2, e3)) | (~ spl30_24 | spl30_110)), inference(backward_demodulation, [], [f1091, f533])).
fof(f2071, plain, (~ spl30_25 | ~ spl30_26), inference(avatar_contradiction_clause, [], [f2070])).
fof(f2070, plain, ($false | (~ spl30_25 | ~ spl30_26)), inference(subsumption_resolution, [], [f2069, f211])).
fof(f2069, plain, ((e0 = e1) | (~ spl30_25 | ~ spl30_26)), inference(backward_demodulation, [], [f542, f538])).
fof(f542, plain, ((e1 = op(e2, e1)) | ~ spl30_26), inference(avatar_component_clause, [], [f540])).
fof(f540, plain, (spl30_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_26])])).
fof(f2068, plain, (~ spl30_33 | ~ spl30_36), inference(avatar_contradiction_clause, [], [f2067])).
fof(f2067, plain, ($false | (~ spl30_33 | ~ spl30_36)), inference(subsumption_resolution, [], [f2065, f213])).
fof(f2065, plain, ((e0 = e3) | (~ spl30_33 | ~ spl30_36)), inference(backward_demodulation, [], [f584, f572])).
fof(f572, plain, ((e0 = op(e1, e3)) | ~ spl30_33), inference(avatar_component_clause, [], [f570])).
fof(f570, plain, (spl30_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_33])])).
fof(f2058, plain, (~ spl30_35 | ~ spl30_43), inference(avatar_split_clause, [], [f2053, f612, f578])).
fof(f2053, plain, (~ (e2 = op(e1, e3)) | ~ spl30_43), inference(backward_demodulation, [], [f197, f614])).
fof(f2050, plain, (~ spl30_35 | ~ spl30_47), inference(avatar_split_clause, [], [f2047, f629, f578])).
fof(f2047, plain, (~ (e2 = op(e1, e3)) | ~ spl30_47), inference(backward_demodulation, [], [f195, f631])).
fof(f2044, plain, (~ spl30_33 | ~ spl30_49), inference(avatar_split_clause, [], [f2042, f638, f570])).
fof(f2042, plain, (~ (e0 = op(e1, e3)) | ~ spl30_49), inference(backward_demodulation, [], [f181, f640])).
fof(f2032, plain, (~ spl30_56 | ~ spl30_60), inference(avatar_split_clause, [], [f2028, f684, f667])).
fof(f2028, plain, (~ (e3 = op(e0, e2)) | ~ spl30_60), inference(backward_demodulation, [], [f190, f686])).
fof(f190, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2031, plain, (~ spl30_44 | ~ spl30_60), inference(avatar_split_clause, [], [f2027, f684, f616])).
fof(f616, plain, (spl30_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_44])])).
fof(f2027, plain, (~ (e3 = op(e1, e1)) | ~ spl30_60), inference(backward_demodulation, [], [f169, f686])).
fof(f2026, plain, (spl30_43 | ~ spl30_62 | ~ spl30_113), inference(avatar_split_clause, [], [f2025, f1104, f693, f612])).
fof(f693, plain, (spl30_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl30_62])])).
fof(f2025, plain, ((e2 = op(e1, e1)) | (~ spl30_62 | ~ spl30_113)), inference(forward_demodulation, [], [f1105, f695])).
fof(f695, plain, ((op(e0, e0) = e1) | ~ spl30_62), inference(avatar_component_clause, [], [f693])).
fof(f1105, plain, ((e2 = op(op(e0, e0), op(e0, e0))) | ~ spl30_113), inference(avatar_component_clause, [], [f1104])).
fof(f2024, plain, (~ spl30_60 | ~ spl30_62 | spl30_107), inference(avatar_split_clause, [], [f1872, f1074, f693, f684])).
fof(f1872, plain, (~ (e3 = op(e0, e1)) | (~ spl30_62 | spl30_107)), inference(backward_demodulation, [], [f1076, f695])).
fof(f1076, plain, (~ (e3 = op(e0, op(e0, e0))) | spl30_107), inference(avatar_component_clause, [], [f1074])).
fof(f2022, plain, (~ spl30_59 | ~ spl30_62 | spl30_109), inference(avatar_split_clause, [], [f1964, f1084, f693, f680])).
fof(f1084, plain, (spl30_109 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_109])])).
fof(f1964, plain, (~ (e2 = op(e0, e1)) | (~ spl30_62 | spl30_109)), inference(forward_demodulation, [], [f1086, f695])).
fof(f1086, plain, (~ (e2 = op(e0, op(e0, e0))) | spl30_109), inference(avatar_component_clause, [], [f1084])).
fof(f2020, plain, (~ spl30_50 | ~ spl30_2), inference(avatar_split_clause, [], [f2019, f438, f642])).
fof(f2019, plain, (~ (e1 = op(e0, e3)) | ~ spl30_2), inference(forward_demodulation, [], [f183, f440])).
fof(f2005, plain, (~ spl30_16 | ~ spl30_32), inference(avatar_split_clause, [], [f2004, f565, f497])).
fof(f2004, plain, (~ (e3 = op(e3, e0)) | ~ spl30_32), inference(forward_demodulation, [], [f168, f567])).
fof(f1999, plain, (~ spl30_10 | ~ spl30_26), inference(avatar_split_clause, [], [f1998, f540, f472])).
fof(f1998, plain, (~ (e1 = op(e3, e1)) | ~ spl30_26), inference(forward_demodulation, [], [f174, f542])).
fof(f1992, plain, (~ spl30_17 | ~ spl30_20), inference(avatar_contradiction_clause, [], [f1991])).
fof(f1991, plain, ($false | (~ spl30_17 | ~ spl30_20)), inference(subsumption_resolution, [], [f1989, f213])).
fof(f1989, plain, ((e0 = e3) | (~ spl30_17 | ~ spl30_20)), inference(backward_demodulation, [], [f516, f504])).
fof(f516, plain, ((e3 = op(e2, e3)) | ~ spl30_20), inference(avatar_component_clause, [], [f514])).
fof(f1986, plain, (~ spl30_18 | ~ spl30_26), inference(avatar_split_clause, [], [f1984, f540, f506])).
fof(f1984, plain, (~ (e1 = op(e2, e3)) | ~ spl30_26), inference(backward_demodulation, [], [f203, f542])).
fof(f1978, plain, (~ spl30_18 | ~ spl30_50), inference(avatar_split_clause, [], [f1975, f642, f506])).
fof(f1975, plain, (~ (e1 = op(e2, e3)) | ~ spl30_50), inference(backward_demodulation, [], [f182, f644])).
fof(f1973, plain, (~ spl30_57 | ~ spl30_59), inference(avatar_contradiction_clause, [], [f1972])).
fof(f1972, plain, ($false | (~ spl30_57 | ~ spl30_59)), inference(subsumption_resolution, [], [f1971, f212])).
fof(f1971, plain, ((e0 = e2) | (~ spl30_57 | ~ spl30_59)), inference(backward_demodulation, [], [f682, f674])).
fof(f1969, plain, (~ spl30_1 | ~ spl30_44 | spl30_99), inference(avatar_split_clause, [], [f1968, f1036, f616, f434])).
fof(f1036, plain, (spl30_99 <=> (e0 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_99])])).
fof(f1968, plain, (~ (e0 = op(e3, e3)) | (~ spl30_44 | spl30_99)), inference(forward_demodulation, [], [f1038, f618])).
fof(f618, plain, ((e3 = op(e1, e1)) | ~ spl30_44), inference(avatar_component_clause, [], [f616])).
fof(f1038, plain, (~ (e0 = op(op(e1, e1), op(e1, e1))) | spl30_99), inference(avatar_component_clause, [], [f1036])).
fof(f1961, plain, (~ spl30_50 | ~ spl30_62), inference(avatar_split_clause, [], [f1868, f693, f642])).
fof(f1868, plain, (~ (e1 = op(e0, e3)) | ~ spl30_62), inference(backward_demodulation, [], [f189, f695])).
fof(f1960, plain, (~ spl30_48 | ~ spl30_44), inference(avatar_split_clause, [], [f1959, f616, f633])).
fof(f1959, plain, (~ (e3 = op(e1, e0)) | ~ spl30_44), inference(forward_demodulation, [], [f193, f618])).
fof(f1955, plain, (~ spl30_48 | ~ spl30_32), inference(avatar_split_clause, [], [f1954, f565, f633])).
fof(f1954, plain, (~ (e3 = op(e1, e0)) | ~ spl30_32), inference(forward_demodulation, [], [f166, f567])).
fof(f1946, plain, (~ spl30_12 | ~ spl30_44), inference(avatar_contradiction_clause, [], [f1945])).
fof(f1945, plain, ($false | (~ spl30_12 | ~ spl30_44)), inference(subsumption_resolution, [], [f1944, f618])).
fof(f1944, plain, (~ (e3 = op(e1, e1)) | ~ spl30_12), inference(forward_demodulation, [], [f173, f482])).
fof(f1938, plain, (spl30_4 | ~ spl30_44 | ~ spl30_116), inference(avatar_split_clause, [], [f1903, f1120, f616, f446])).
fof(f1903, plain, ((e3 = op(e3, e3)) | (~ spl30_44 | ~ spl30_116)), inference(backward_demodulation, [], [f1121, f618])).
fof(f1121, plain, ((e3 = op(op(e1, e1), op(e1, e1))) | ~ spl30_116), inference(avatar_component_clause, [], [f1120])).
fof(f1934, plain, (~ spl30_10 | ~ spl30_12), inference(avatar_contradiction_clause, [], [f1933])).
fof(f1933, plain, ($false | (~ spl30_10 | ~ spl30_12)), inference(subsumption_resolution, [], [f1932, f215])).
fof(f1932, plain, ((e1 = e3) | (~ spl30_10 | ~ spl30_12)), inference(forward_demodulation, [], [f482, f474])).
fof(f1931, plain, (~ spl30_13 | ~ spl30_15), inference(avatar_contradiction_clause, [], [f1930])).
fof(f1930, plain, ($false | (~ spl30_13 | ~ spl30_15)), inference(subsumption_resolution, [], [f1929, f212])).
fof(f1929, plain, ((e0 = e2) | (~ spl30_13 | ~ spl30_15)), inference(backward_demodulation, [], [f495, f487])).
fof(f1921, plain, (~ spl30_28 | ~ spl30_32), inference(avatar_split_clause, [], [f1919, f565, f548])).
fof(f1919, plain, (~ (e3 = op(e2, e1)) | ~ spl30_32), inference(backward_demodulation, [], [f199, f567])).
fof(f199, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1918, plain, (~ spl30_33 | ~ spl30_35), inference(avatar_contradiction_clause, [], [f1917])).
fof(f1917, plain, ($false | (~ spl30_33 | ~ spl30_35)), inference(subsumption_resolution, [], [f1916, f212])).
fof(f1916, plain, ((e0 = e2) | (~ spl30_33 | ~ spl30_35)), inference(backward_demodulation, [], [f580, f572])).
fof(f580, plain, ((e2 = op(e1, e3)) | ~ spl30_35), inference(avatar_component_clause, [], [f578])).
fof(f1914, plain, (~ spl30_19 | ~ spl30_35), inference(avatar_split_clause, [], [f1913, f578, f510])).
fof(f1913, plain, (~ (e2 = op(e2, e3)) | ~ spl30_35), inference(backward_demodulation, [], [f184, f580])).
fof(f1908, plain, (~ spl30_33 | ~ spl30_44 | spl30_115), inference(avatar_split_clause, [], [f1902, f1115, f616, f570])).
fof(f1902, plain, (~ (e0 = op(e1, e3)) | (~ spl30_44 | spl30_115)), inference(backward_demodulation, [], [f1117, f618])).
fof(f1117, plain, (~ (e0 = op(e1, op(e1, e1))) | spl30_115), inference(avatar_component_clause, [], [f1115])).
fof(f1907, plain, (~ spl30_35 | ~ spl30_44 | spl30_102), inference(avatar_split_clause, [], [f1901, f1050, f616, f578])).
fof(f1901, plain, (~ (e2 = op(e1, e3)) | (~ spl30_44 | spl30_102)), inference(backward_demodulation, [], [f1052, f618])).
fof(f1905, plain, (~ spl30_36 | ~ spl30_44), inference(avatar_split_clause, [], [f1899, f616, f582])).
fof(f1899, plain, (~ (e3 = op(e1, e3)) | ~ spl30_44), inference(backward_demodulation, [], [f197, f618])).
fof(f1904, plain, (~ spl30_28 | ~ spl30_44), inference(avatar_split_clause, [], [f1898, f616, f548])).
fof(f1898, plain, (~ (e3 = op(e2, e1)) | ~ spl30_44), inference(backward_demodulation, [], [f172, f618])).
fof(f172, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1897, plain, (~ spl30_49 | ~ spl30_52), inference(avatar_contradiction_clause, [], [f1896])).
fof(f1896, plain, ($false | (~ spl30_49 | ~ spl30_52)), inference(subsumption_resolution, [], [f1894, f213])).
fof(f1894, plain, ((e0 = e3) | (~ spl30_49 | ~ spl30_52)), inference(backward_demodulation, [], [f652, f640])).
fof(f652, plain, ((e3 = op(e0, e3)) | ~ spl30_52), inference(avatar_component_clause, [], [f650])).
fof(f650, plain, (spl30_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl30_52])])).
fof(f1892, plain, (~ spl30_36 | ~ spl30_52), inference(avatar_split_clause, [], [f1890, f650, f582])).
fof(f1890, plain, (~ (e3 = op(e1, e3)) | ~ spl30_52), inference(backward_demodulation, [], [f181, f652])).
fof(f1889, plain, (~ spl30_8 | ~ spl30_56), inference(avatar_split_clause, [], [f1887, f667, f463])).
fof(f1887, plain, (~ (e3 = op(e3, e2)) | ~ spl30_56), inference(backward_demodulation, [], [f177, f669])).
fof(f1888, plain, (~ spl30_52 | ~ spl30_56), inference(avatar_split_clause, [], [f1886, f667, f650])).
fof(f1886, plain, (~ (e3 = op(e0, e3)) | ~ spl30_56), inference(backward_demodulation, [], [f192, f669])).
fof(f192, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1885, plain, (~ spl30_51 | ~ spl30_59), inference(avatar_split_clause, [], [f1882, f680, f646])).
fof(f1882, plain, (~ (e2 = op(e0, e3)) | ~ spl30_59), inference(backward_demodulation, [], [f191, f682])).
fof(f1884, plain, (~ spl30_43 | ~ spl30_59), inference(avatar_split_clause, [], [f1879, f680, f612])).
fof(f1879, plain, (~ (e2 = op(e1, e1)) | ~ spl30_59), inference(backward_demodulation, [], [f169, f682])).
fof(f1878, plain, (~ spl30_44 | ~ spl30_62 | spl30_118), inference(avatar_split_clause, [], [f1874, f1130, f693, f616])).
fof(f1130, plain, (spl30_118 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl30_118])])).
fof(f1874, plain, (~ (e3 = op(e1, e1)) | (~ spl30_62 | spl30_118)), inference(backward_demodulation, [], [f1132, f695])).
fof(f1132, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl30_118), inference(avatar_component_clause, [], [f1130])).
fof(f1877, plain, (~ spl30_43 | ~ spl30_62 | spl30_113), inference(avatar_split_clause, [], [f1873, f1104, f693, f612])).
fof(f1873, plain, (~ (e2 = op(e1, e1)) | (~ spl30_62 | spl30_113)), inference(backward_demodulation, [], [f1106, f695])).
fof(f1876, plain, (~ spl30_54 | ~ spl30_62), inference(avatar_split_clause, [], [f1867, f693, f659])).
fof(f1867, plain, (~ (e1 = op(e0, e2)) | ~ spl30_62), inference(backward_demodulation, [], [f188, f695])).
fof(f1875, plain, (~ spl30_30 | ~ spl30_62), inference(avatar_split_clause, [], [f1865, f693, f557])).
fof(f1865, plain, (~ (e1 = op(e2, e0)) | ~ spl30_62), inference(backward_demodulation, [], [f164, f695])).
fof(f1863, plain, (~ spl30_64 | ~ spl30_48), inference(avatar_split_clause, [], [f1862, f633, f701])).
fof(f1862, plain, (~ (op(e0, e0) = e3) | ~ spl30_48), inference(forward_demodulation, [], [f163, f635])).
fof(f1861, plain, (~ spl30_62 | ~ spl30_1 | spl30_105), inference(avatar_split_clause, [], [f1649, f1065, f434, f693])).
fof(f1649, plain, (~ (op(e0, e0) = e1) | (~ spl30_1 | spl30_105)), inference(backward_demodulation, [], [f1067, f436])).
fof(f1857, plain, (~ spl30_49 | ~ spl30_1), inference(avatar_split_clause, [], [f1670, f434, f638])).
fof(f1670, plain, (~ (e0 = op(e0, e3)) | ~ spl30_1), inference(forward_demodulation, [], [f183, f436])).
fof(f1852, plain, (~ spl30_33 | ~ spl30_1), inference(avatar_split_clause, [], [f1666, f434, f570])).
fof(f1666, plain, (~ (e0 = op(e1, e3)) | ~ spl30_1), inference(forward_demodulation, [], [f185, f436])).
fof(f1847, plain, (~ spl30_13 | ~ spl30_1), inference(avatar_split_clause, [], [f1696, f434, f485])).
fof(f1696, plain, (~ (e0 = op(e3, e0)) | ~ spl30_1), inference(forward_demodulation, [], [f207, f436])).
fof(f1841, plain, (~ spl30_11 | ~ spl30_15), inference(avatar_split_clause, [], [f1840, f493, f476])).
fof(f1840, plain, (~ (e2 = op(e3, e1)) | ~ spl30_15), inference(backward_demodulation, [], [f205, f495])).
fof(f1837, plain, (~ spl30_29 | ~ spl30_30), inference(avatar_contradiction_clause, [], [f1836])).
fof(f1836, plain, ($false | (~ spl30_29 | ~ spl30_30)), inference(subsumption_resolution, [], [f1835, f211])).
fof(f1835, plain, ((e0 = e1) | (~ spl30_29 | ~ spl30_30)), inference(forward_demodulation, [], [f559, f555])).
fof(f1833, plain, (~ spl30_34 | ~ spl30_36), inference(avatar_contradiction_clause, [], [f1832])).
fof(f1832, plain, ($false | (~ spl30_34 | ~ spl30_36)), inference(subsumption_resolution, [], [f1831, f215])).
fof(f1831, plain, ((e1 = e3) | (~ spl30_34 | ~ spl30_36)), inference(backward_demodulation, [], [f584, f576])).
fof(f1827, plain, (~ spl30_37 | ~ spl30_38), inference(avatar_contradiction_clause, [], [f1826])).
fof(f1826, plain, ($false | (~ spl30_37 | ~ spl30_38)), inference(subsumption_resolution, [], [f1825, f211])).
fof(f1825, plain, ((e0 = e1) | (~ spl30_37 | ~ spl30_38)), inference(backward_demodulation, [], [f593, f589])).
fof(f1817, plain, (~ spl30_37 | ~ spl30_43 | spl30_115), inference(avatar_split_clause, [], [f1812, f1115, f612, f587])).
fof(f1812, plain, (~ (e0 = op(e1, e2)) | (~ spl30_43 | spl30_115)), inference(backward_demodulation, [], [f1117, f614])).
fof(f1814, plain, (~ spl30_11 | ~ spl30_43), inference(avatar_split_clause, [], [f1806, f612, f476])).
fof(f1806, plain, (~ (e2 = op(e3, e1)) | ~ spl30_43), inference(backward_demodulation, [], [f173, f614])).
fof(f1804, plain, (~ spl30_40 | ~ spl30_48), inference(avatar_split_clause, [], [f1802, f633, f599])).
fof(f1802, plain, (~ (e3 = op(e1, e2)) | ~ spl30_48), inference(backward_demodulation, [], [f194, f635])).
fof(f1799, plain, (~ spl30_19 | ~ spl30_51), inference(avatar_split_clause, [], [f1797, f646, f510])).
fof(f1797, plain, (~ (e2 = op(e2, e3)) | ~ spl30_51), inference(backward_demodulation, [], [f182, f648])).
fof(f1798, plain, (~ spl30_35 | ~ spl30_51), inference(avatar_split_clause, [], [f1796, f646, f578])).
fof(f1796, plain, (~ (e2 = op(e1, e3)) | ~ spl30_51), inference(backward_demodulation, [], [f181, f648])).
fof(f1794, plain, (~ spl30_50 | ~ spl30_54), inference(avatar_split_clause, [], [f1791, f659, f642])).
fof(f1791, plain, (~ (e1 = op(e0, e3)) | ~ spl30_54), inference(backward_demodulation, [], [f192, f661])).
fof(f1788, plain, (~ spl30_53 | ~ spl30_57), inference(avatar_split_clause, [], [f1782, f672, f655])).
fof(f1782, plain, (~ (e0 = op(e0, e2)) | ~ spl30_57), inference(backward_demodulation, [], [f190, f674])).
fof(f1787, plain, (~ spl30_41 | ~ spl30_57), inference(avatar_split_clause, [], [f1780, f672, f604])).
fof(f1780, plain, (~ (e0 = op(e1, e1)) | ~ spl30_57), inference(backward_demodulation, [], [f169, f674])).
fof(f1759, plain, (~ spl30_12 | ~ spl30_8), inference(avatar_split_clause, [], [f1758, f463, f480])).
fof(f1758, plain, (~ (e3 = op(e3, e1)) | ~ spl30_8), inference(forward_demodulation, [], [f208, f465])).
fof(f1753, plain, (~ spl30_22 | ~ spl30_23), inference(avatar_contradiction_clause, [], [f1752])).
fof(f1752, plain, ($false | (~ spl30_22 | ~ spl30_23)), inference(subsumption_resolution, [], [f1751, f214])).
fof(f1751, plain, ((e1 = e2) | (~ spl30_22 | ~ spl30_23)), inference(backward_demodulation, [], [f529, f525])).
fof(f1750, plain, (~ spl30_29 | ~ spl30_31), inference(avatar_contradiction_clause, [], [f1749])).
fof(f1749, plain, ($false | (~ spl30_29 | ~ spl30_31)), inference(subsumption_resolution, [], [f1748, f212])).
fof(f1748, plain, ((e0 = e2) | (~ spl30_29 | ~ spl30_31)), inference(forward_demodulation, [], [f563, f555])).
fof(f1741, plain, (~ spl30_53 | ~ spl30_55), inference(avatar_contradiction_clause, [], [f1740])).
fof(f1740, plain, ($false | (~ spl30_53 | ~ spl30_55)), inference(subsumption_resolution, [], [f1739, f212])).
fof(f1739, plain, ((e0 = e2) | (~ spl30_53 | ~ spl30_55)), inference(forward_demodulation, [], [f665, f657])).
fof(f1730, plain, (~ spl30_52 | ~ spl30_20), inference(avatar_split_clause, [], [f1729, f514, f650])).
fof(f1729, plain, (~ (e3 = op(e0, e3)) | ~ spl30_20), inference(forward_demodulation, [], [f182, f516])).
fof(f1717, plain, (~ spl30_7 | ~ spl30_11), inference(avatar_split_clause, [], [f1716, f476, f459])).
fof(f1716, plain, (~ (e2 = op(e3, e2)) | ~ spl30_11), inference(forward_demodulation, [], [f208, f478])).
fof(f1712, plain, (~ spl30_18 | ~ spl30_20), inference(avatar_contradiction_clause, [], [f1711])).
fof(f1711, plain, ($false | (~ spl30_18 | ~ spl30_20)), inference(subsumption_resolution, [], [f1710, f215])).
fof(f1710, plain, ((e1 = e3) | (~ spl30_18 | ~ spl30_20)), inference(forward_demodulation, [], [f516, f508])).
fof(f1707, plain, (~ spl30_46 | ~ spl30_47), inference(avatar_contradiction_clause, [], [f1706])).
fof(f1706, plain, ($false | (~ spl30_46 | ~ spl30_47)), inference(subsumption_resolution, [], [f1705, f214])).
fof(f1705, plain, ((e1 = e2) | (~ spl30_46 | ~ spl30_47)), inference(forward_demodulation, [], [f631, f627])).
fof(f1699, plain, (~ spl30_34 | ~ spl30_46), inference(avatar_split_clause, [], [f1578, f625, f574])).
fof(f1578, plain, (~ (e1 = op(e1, e3)) | ~ spl30_46), inference(forward_demodulation, [], [f195, f627])).
fof(f1683, plain, (~ spl30_58 | ~ spl30_59), inference(avatar_contradiction_clause, [], [f1682])).
fof(f1682, plain, ($false | (~ spl30_58 | ~ spl30_59)), inference(subsumption_resolution, [], [f1681, f214])).
fof(f1681, plain, ((e1 = e2) | (~ spl30_58 | ~ spl30_59)), inference(forward_demodulation, [], [f682, f678])).
fof(f1672, plain, (~ spl30_52 | ~ spl30_64), inference(avatar_split_clause, [], [f1590, f701, f650])).
fof(f1590, plain, (~ (e3 = op(e0, e3)) | ~ spl30_64), inference(backward_demodulation, [], [f189, f703])).
fof(f1668, plain, (~ spl30_39 | ~ spl30_23), inference(avatar_split_clause, [], [f1667, f527, f595])).
fof(f1667, plain, (~ (e2 = op(e1, e2)) | ~ spl30_23), inference(forward_demodulation, [], [f178, f529])).
fof(f1631, plain, (~ spl30_7 | ~ spl30_23), inference(avatar_split_clause, [], [f1625, f527, f459])).
fof(f1625, plain, (~ (e2 = op(e3, e2)) | ~ spl30_23), inference(backward_demodulation, [], [f180, f529])).
fof(f1624, plain, (~ spl30_29 | ~ spl30_32), inference(avatar_contradiction_clause, [], [f1623])).
fof(f1623, plain, ($false | (~ spl30_29 | ~ spl30_32)), inference(subsumption_resolution, [], [f1621, f213])).
fof(f1621, plain, ((e0 = e3) | (~ spl30_29 | ~ spl30_32)), inference(backward_demodulation, [], [f567, f555])).
fof(f1619, plain, (~ spl30_4 | ~ spl30_36), inference(avatar_split_clause, [], [f1618, f582, f446])).
fof(f1618, plain, (~ (e3 = op(e3, e3)) | ~ spl30_36), inference(backward_demodulation, [], [f185, f584])).
fof(f1616, plain, (~ spl30_35 | ~ spl30_39), inference(avatar_split_clause, [], [f1613, f595, f578])).
fof(f1613, plain, (~ (e2 = op(e1, e3)) | ~ spl30_39), inference(backward_demodulation, [], [f198, f597])).
fof(f1606, plain, (~ spl30_53 | ~ spl30_56), inference(avatar_contradiction_clause, [], [f1605])).
fof(f1605, plain, ($false | (~ spl30_53 | ~ spl30_56)), inference(subsumption_resolution, [], [f1604, f213])).
fof(f1604, plain, ((e0 = e3) | (~ spl30_53 | ~ spl30_56)), inference(backward_demodulation, [], [f669, f657])).
fof(f1582, plain, (~ spl30_40 | ~ spl30_56), inference(avatar_split_clause, [], [f1581, f667, f599])).
fof(f1581, plain, (~ (e3 = op(e1, e2)) | ~ spl30_56), inference(backward_demodulation, [], [f175, f669])).
fof(f1571, plain, (~ spl30_26 | ~ spl30_27), inference(avatar_contradiction_clause, [], [f1570])).
fof(f1570, plain, ($false | (~ spl30_26 | ~ spl30_27)), inference(subsumption_resolution, [], [f1569, f214])).
fof(f1569, plain, ((e1 = e2) | (~ spl30_26 | ~ spl30_27)), inference(backward_demodulation, [], [f546, f542])).
fof(f1568, plain, (~ spl30_38 | ~ spl30_40), inference(avatar_contradiction_clause, [], [f1567])).
fof(f1567, plain, ($false | (~ spl30_38 | ~ spl30_40)), inference(subsumption_resolution, [], [f1566, f215])).
fof(f1566, plain, ((e1 = e3) | (~ spl30_38 | ~ spl30_40)), inference(backward_demodulation, [], [f601, f593])).
fof(f1564, plain, (~ spl30_54 | ~ spl30_56), inference(avatar_contradiction_clause, [], [f1563])).
fof(f1563, plain, ($false | (~ spl30_54 | ~ spl30_56)), inference(subsumption_resolution, [], [f1562, f215])).
fof(f1562, plain, ((e1 = e3) | (~ spl30_54 | ~ spl30_56)), inference(backward_demodulation, [], [f669, f661])).
fof(f1558, plain, (~ spl30_56 | ~ spl30_63 | spl30_107), inference(avatar_contradiction_clause, [], [f1557])).
fof(f1557, plain, ($false | (~ spl30_56 | ~ spl30_63 | spl30_107)), inference(subsumption_resolution, [], [f1553, f669])).
fof(f1553, plain, (~ (e3 = op(e0, e2)) | (~ spl30_63 | spl30_107)), inference(backward_demodulation, [], [f1076, f699])).
fof(f1547, plain, (~ spl30_59 | ~ spl30_27), inference(avatar_split_clause, [], [f1546, f544, f680])).
fof(f1546, plain, (~ (e2 = op(e0, e1)) | ~ spl30_27), inference(forward_demodulation, [], [f170, f546])).
fof(f1528, plain, (~ spl30_32 | ~ spl30_21 | spl30_96), inference(avatar_split_clause, [], [f1527, f1022, f519, f565])).
fof(f1022, plain, (spl30_96 <=> (e3 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl30_96])])).
fof(f1527, plain, (~ (e3 = op(e2, e0)) | (~ spl30_21 | spl30_96)), inference(forward_demodulation, [], [f1024, f521])).
fof(f1024, plain, (~ (e3 = op(e2, op(e2, e2))) | spl30_96), inference(avatar_component_clause, [], [f1022])).
fof(f1525, plain, (~ spl30_62 | ~ spl30_21 | spl30_104), inference(avatar_split_clause, [], [f1524, f1060, f519, f693])).
fof(f1524, plain, (~ (op(e0, e0) = e1) | (~ spl30_21 | spl30_104)), inference(forward_demodulation, [], [f1062, f521])).
fof(f1517, plain, (~ spl30_3 | ~ spl30_4), inference(avatar_contradiction_clause, [], [f1516])).
fof(f1516, plain, ($false | (~ spl30_3 | ~ spl30_4)), inference(subsumption_resolution, [], [f1515, f216])).
fof(f1515, plain, ((e2 = e3) | (~ spl30_3 | ~ spl30_4)), inference(backward_demodulation, [], [f448, f444])).
fof(f448, plain, ((e3 = op(e3, e3)) | ~ spl30_4), inference(avatar_component_clause, [], [f446])).
fof(f1508, plain, (~ spl30_9 | ~ spl30_13), inference(avatar_split_clause, [], [f1505, f485, f468])).
fof(f468, plain, (spl30_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl30_9])])).
fof(f1505, plain, (~ (e0 = op(e3, e1)) | ~ spl30_13), inference(backward_demodulation, [], [f205, f487])).
fof(f1500, plain, (~ spl30_21 | ~ spl30_22), inference(avatar_contradiction_clause, [], [f1499])).
fof(f1499, plain, ($false | (~ spl30_21 | ~ spl30_22)), inference(subsumption_resolution, [], [f1498, f211])).
fof(f1498, plain, ((e0 = e1) | (~ spl30_21 | ~ spl30_22)), inference(backward_demodulation, [], [f525, f521])).
fof(f1496, plain, (~ spl30_31 | ~ spl30_32), inference(avatar_contradiction_clause, [], [f1495])).
fof(f1495, plain, ($false | (~ spl30_31 | ~ spl30_32)), inference(subsumption_resolution, [], [f1494, f216])).
fof(f1494, plain, ((e2 = e3) | (~ spl30_31 | ~ spl30_32)), inference(backward_demodulation, [], [f567, f563])).
fof(f1489, plain, (~ spl30_9 | ~ spl30_41), inference(avatar_split_clause, [], [f1481, f604, f468])).
fof(f1481, plain, (~ (e0 = op(e3, e1)) | ~ spl30_41), inference(backward_demodulation, [], [f173, f606])).
fof(f1474, plain, (~ spl30_55 | ~ spl30_56), inference(avatar_contradiction_clause, [], [f1473])).
fof(f1473, plain, ($false | (~ spl30_55 | ~ spl30_56)), inference(subsumption_resolution, [], [f1472, f216])).
fof(f1472, plain, ((e2 = e3) | (~ spl30_55 | ~ spl30_56)), inference(backward_demodulation, [], [f669, f665])).
fof(f1471, plain, (~ spl30_58 | ~ spl30_60), inference(avatar_contradiction_clause, [], [f1470])).
fof(f1470, plain, ($false | (~ spl30_58 | ~ spl30_60)), inference(subsumption_resolution, [], [f1469, f215])).
fof(f1469, plain, ((e1 = e3) | (~ spl30_58 | ~ spl30_60)), inference(forward_demodulation, [], [f686, f678])).
fof(f1459, plain, (~ spl30_62 | ~ spl30_46), inference(avatar_split_clause, [], [f1458, f625, f693])).
fof(f1458, plain, (~ (op(e0, e0) = e1) | ~ spl30_46), inference(forward_demodulation, [], [f163, f627])).
fof(f1456, plain, (~ spl30_62 | ~ spl30_58), inference(avatar_split_clause, [], [f1455, f676, f693])).
fof(f1455, plain, (~ (op(e0, e0) = e1) | ~ spl30_58), inference(forward_demodulation, [], [f187, f678])).
fof(f1450, plain, (~ spl30_43 | ~ spl30_27), inference(avatar_split_clause, [], [f1449, f544, f612])).
fof(f1449, plain, (~ (e2 = op(e1, e1)) | ~ spl30_27), inference(forward_demodulation, [], [f172, f546])).
fof(f1436, plain, (~ spl30_39 | ~ spl30_40), inference(avatar_contradiction_clause, [], [f1435])).
fof(f1435, plain, ($false | (~ spl30_39 | ~ spl30_40)), inference(subsumption_resolution, [], [f1434, f216])).
fof(f1434, plain, ((e2 = e3) | (~ spl30_39 | ~ spl30_40)), inference(forward_demodulation, [], [f601, f597])).
fof(f1433, plain, (~ spl30_26 | ~ spl30_22), inference(avatar_split_clause, [], [f1432, f523, f540])).
fof(f1432, plain, (~ (e1 = op(e2, e1)) | ~ spl30_22), inference(forward_demodulation, [], [f202, f525])).
fof(f1431, plain, (~ spl30_18 | ~ spl30_22), inference(avatar_split_clause, [], [f1430, f523, f506])).
fof(f1430, plain, (~ (e1 = op(e2, e3)) | ~ spl30_22), inference(forward_demodulation, [], [f204, f525])).
fof(f1429, plain, (~ spl30_19 | ~ spl30_27), inference(avatar_split_clause, [], [f1428, f544, f510])).
fof(f1428, plain, (~ (e2 = op(e2, e3)) | ~ spl30_27), inference(forward_demodulation, [], [f203, f546])).
fof(f1422, plain, (~ spl30_11 | ~ spl30_27), inference(avatar_split_clause, [], [f1421, f544, f476])).
fof(f1421, plain, (~ (e2 = op(e3, e1)) | ~ spl30_27), inference(forward_demodulation, [], [f174, f546])).
fof(f1418, plain, (~ spl30_6 | ~ spl30_22), inference(avatar_split_clause, [], [f1417, f523, f455])).
fof(f1417, plain, (~ (e1 = op(e3, e2)) | ~ spl30_22), inference(forward_demodulation, [], [f180, f525])).
fof(f1414, plain, (~ spl30_2 | ~ spl30_4), inference(avatar_contradiction_clause, [], [f1413])).
fof(f1413, plain, ($false | (~ spl30_2 | ~ spl30_4)), inference(subsumption_resolution, [], [f1412, f215])).
fof(f1412, plain, ((e1 = e3) | (~ spl30_2 | ~ spl30_4)), inference(backward_demodulation, [], [f448, f440])).
fof(f1409, plain, (~ spl30_9 | ~ spl30_11), inference(avatar_contradiction_clause, [], [f1408])).
fof(f1408, plain, ($false | (~ spl30_9 | ~ spl30_11)), inference(subsumption_resolution, [], [f1407, f212])).
fof(f1407, plain, ((e0 = e2) | (~ spl30_9 | ~ spl30_11)), inference(backward_demodulation, [], [f478, f470])).
fof(f470, plain, ((e0 = op(e3, e1)) | ~ spl30_9), inference(avatar_component_clause, [], [f468])).
fof(f1393, plain, (~ spl30_27 | ~ spl30_28), inference(avatar_contradiction_clause, [], [f1392])).
fof(f1392, plain, ($false | (~ spl30_27 | ~ spl30_28)), inference(subsumption_resolution, [], [f1391, f216])).
fof(f1391, plain, ((e2 = e3) | (~ spl30_27 | ~ spl30_28)), inference(backward_demodulation, [], [f550, f546])).
fof(f1384, plain, (~ spl30_37 | ~ spl30_39), inference(avatar_contradiction_clause, [], [f1383])).
fof(f1383, plain, ($false | (~ spl30_37 | ~ spl30_39)), inference(subsumption_resolution, [], [f1382, f212])).
fof(f1382, plain, ((e0 = e2) | (~ spl30_37 | ~ spl30_39)), inference(forward_demodulation, [], [f597, f589])).
fof(f1381, plain, (~ spl30_37 | ~ spl30_40), inference(avatar_contradiction_clause, [], [f1380])).
fof(f1380, plain, ($false | (~ spl30_37 | ~ spl30_40)), inference(subsumption_resolution, [], [f1379, f213])).
fof(f1379, plain, ((e0 = e3) | (~ spl30_37 | ~ spl30_40)), inference(forward_demodulation, [], [f601, f589])).
fof(f1369, plain, (~ spl30_30 | ~ spl30_46), inference(avatar_split_clause, [], [f1367, f625, f557])).
fof(f1367, plain, (~ (e1 = op(e2, e0)) | ~ spl30_46), inference(backward_demodulation, [], [f166, f627])).
fof(f1364, plain, (~ spl30_54 | ~ spl30_58), inference(avatar_split_clause, [], [f1362, f676, f659])).
fof(f1362, plain, (~ (e1 = op(e0, e2)) | ~ spl30_58), inference(backward_demodulation, [], [f190, f678])).
fof(f1363, plain, (~ spl30_26 | ~ spl30_58), inference(avatar_split_clause, [], [f1361, f676, f540])).
fof(f1361, plain, (~ (e1 = op(e2, e1)) | ~ spl30_58), inference(backward_demodulation, [], [f170, f678])).
fof(f1357, plain, (~ spl30_59 | ~ spl30_11), inference(avatar_split_clause, [], [f1356, f476, f680])).
fof(f1356, plain, (~ (e2 = op(e0, e1)) | ~ spl30_11), inference(forward_demodulation, [], [f171, f478])).
fof(f1353, plain, (~ spl30_53 | ~ spl30_37), inference(avatar_split_clause, [], [f1352, f587, f655])).
fof(f1352, plain, (~ (e0 = op(e0, e2)) | ~ spl30_37), inference(forward_demodulation, [], [f175, f589])).
fof(f1349, plain, (~ spl30_55 | ~ spl30_51), inference(avatar_split_clause, [], [f1348, f646, f663])).
fof(f1348, plain, (~ (e2 = op(e0, e2)) | ~ spl30_51), inference(forward_demodulation, [], [f192, f648])).
fof(f1338, plain, (~ spl30_45 | ~ spl30_37), inference(avatar_split_clause, [], [f1337, f587, f621])).
fof(f1337, plain, (~ (e0 = op(e1, e0)) | ~ spl30_37), inference(forward_demodulation, [], [f194, f589])).
fof(f1332, plain, (~ spl30_41 | ~ spl30_37), inference(avatar_split_clause, [], [f1331, f587, f604])).
fof(f1331, plain, (~ (e0 = op(e1, e1)) | ~ spl30_37), inference(forward_demodulation, [], [f196, f589])).
fof(f1328, plain, (~ spl30_33 | ~ spl30_17), inference(avatar_split_clause, [], [f1327, f502, f570])).
fof(f1327, plain, (~ (e0 = op(e1, e3)) | ~ spl30_17), inference(forward_demodulation, [], [f184, f504])).
fof(f1324, plain, (~ spl30_31 | ~ spl30_15), inference(avatar_split_clause, [], [f1323, f493, f561])).
fof(f1323, plain, (~ (e2 = op(e2, e0)) | ~ spl30_15), inference(forward_demodulation, [], [f168, f495])).
fof(f1318, plain, (~ spl30_25 | ~ spl30_17), inference(avatar_split_clause, [], [f1317, f502, f536])).
fof(f1317, plain, (~ (e0 = op(e2, e1)) | ~ spl30_17), inference(forward_demodulation, [], [f203, f504])).
fof(f1315, plain, (~ spl30_21 | ~ spl30_17), inference(avatar_split_clause, [], [f1314, f502, f519])).
fof(f1314, plain, (~ (e0 = op(e2, e2)) | ~ spl30_17), inference(forward_demodulation, [], [f204, f504])).
fof(f1311, plain, (~ spl30_17 | ~ spl30_19), inference(avatar_contradiction_clause, [], [f1310])).
fof(f1310, plain, ($false | (~ spl30_17 | ~ spl30_19)), inference(subsumption_resolution, [], [f1309, f212])).
fof(f1309, plain, ((e0 = e2) | (~ spl30_17 | ~ spl30_19)), inference(forward_demodulation, [], [f512, f504])).
fof(f1293, plain, (~ spl30_5 | ~ spl30_6), inference(avatar_contradiction_clause, [], [f1292])).
fof(f1292, plain, ($false | (~ spl30_5 | ~ spl30_6)), inference(subsumption_resolution, [], [f1291, f211])).
fof(f1291, plain, ((e0 = e1) | (~ spl30_5 | ~ spl30_6)), inference(backward_demodulation, [], [f457, f453])).
fof(f457, plain, ((e1 = op(e3, e2)) | ~ spl30_6), inference(avatar_component_clause, [], [f455])).
fof(f1290, plain, (~ spl30_6 | ~ spl30_7), inference(avatar_contradiction_clause, [], [f1289])).
fof(f1289, plain, ($false | (~ spl30_6 | ~ spl30_7)), inference(subsumption_resolution, [], [f1288, f214])).
fof(f1288, plain, ((e1 = e2) | (~ spl30_6 | ~ spl30_7)), inference(backward_demodulation, [], [f461, f457])).
fof(f1287, plain, (~ spl30_7 | ~ spl30_8), inference(avatar_contradiction_clause, [], [f1286])).
fof(f1286, plain, ($false | (~ spl30_7 | ~ spl30_8)), inference(subsumption_resolution, [], [f1285, f216])).
fof(f1285, plain, ((e2 = e3) | (~ spl30_7 | ~ spl30_8)), inference(backward_demodulation, [], [f465, f461])).
fof(f1284, plain, (~ spl30_4 | ~ spl30_8), inference(avatar_split_clause, [], [f1283, f463, f446])).
fof(f1283, plain, (~ (e3 = op(e3, e3)) | ~ spl30_8), inference(backward_demodulation, [], [f210, f465])).
fof(f1282, plain, (~ spl30_10 | ~ spl30_11), inference(avatar_contradiction_clause, [], [f1281])).
fof(f1281, plain, ($false | (~ spl30_10 | ~ spl30_11)), inference(subsumption_resolution, [], [f1280, f214])).
fof(f1280, plain, ((e1 = e2) | (~ spl30_10 | ~ spl30_11)), inference(backward_demodulation, [], [f478, f474])).
fof(f1279, plain, (~ spl30_11 | ~ spl30_12), inference(avatar_contradiction_clause, [], [f1278])).
fof(f1278, plain, ($false | (~ spl30_11 | ~ spl30_12)), inference(subsumption_resolution, [], [f1277, f216])).
fof(f1277, plain, ((e2 = e3) | (~ spl30_11 | ~ spl30_12)), inference(backward_demodulation, [], [f482, f478])).
fof(f1272, plain, (~ spl30_14 | ~ spl30_15), inference(avatar_contradiction_clause, [], [f1271])).
fof(f1271, plain, ($false | (~ spl30_14 | ~ spl30_15)), inference(subsumption_resolution, [], [f1270, f214])).
fof(f1270, plain, ((e1 = e2) | (~ spl30_14 | ~ spl30_15)), inference(backward_demodulation, [], [f495, f491])).
fof(f1269, plain, (~ spl30_15 | ~ spl30_16), inference(avatar_contradiction_clause, [], [f1268])).
fof(f1268, plain, ($false | (~ spl30_15 | ~ spl30_16)), inference(subsumption_resolution, [], [f1267, f216])).
fof(f1267, plain, ((e2 = e3) | (~ spl30_15 | ~ spl30_16)), inference(backward_demodulation, [], [f499, f495])).
fof(f1266, plain, (~ spl30_4 | ~ spl30_16), inference(avatar_split_clause, [], [f1263, f497, f446])).
fof(f1263, plain, (~ (e3 = op(e3, e3)) | ~ spl30_16), inference(backward_demodulation, [], [f207, f499])).
fof(f1260, plain, (~ spl30_17 | ~ spl30_18), inference(avatar_contradiction_clause, [], [f1259])).
fof(f1259, plain, ($false | (~ spl30_17 | ~ spl30_18)), inference(subsumption_resolution, [], [f1258, f211])).
fof(f1258, plain, ((e0 = e1) | (~ spl30_17 | ~ spl30_18)), inference(backward_demodulation, [], [f508, f504])).
fof(f1257, plain, (~ spl30_18 | ~ spl30_19), inference(avatar_contradiction_clause, [], [f1256])).
fof(f1256, plain, ($false | (~ spl30_18 | ~ spl30_19)), inference(subsumption_resolution, [], [f1255, f214])).
fof(f1255, plain, ((e1 = e2) | (~ spl30_18 | ~ spl30_19)), inference(backward_demodulation, [], [f512, f508])).
fof(f1254, plain, (~ spl30_19 | ~ spl30_20), inference(avatar_contradiction_clause, [], [f1253])).
fof(f1253, plain, ($false | (~ spl30_19 | ~ spl30_20)), inference(subsumption_resolution, [], [f1252, f216])).
fof(f1252, plain, ((e2 = e3) | (~ spl30_19 | ~ spl30_20)), inference(backward_demodulation, [], [f516, f512])).
fof(f1251, plain, (~ spl30_4 | ~ spl30_20), inference(avatar_split_clause, [], [f1250, f514, f446])).
fof(f1250, plain, (~ (e3 = op(e3, e3)) | ~ spl30_20), inference(backward_demodulation, [], [f186, f516])).
fof(f1236, plain, (~ spl30_9 | ~ spl30_25), inference(avatar_split_clause, [], [f1233, f536, f468])).
fof(f1233, plain, (~ (e0 = op(e3, e1)) | ~ spl30_25), inference(backward_demodulation, [], [f174, f538])).
fof(f1230, plain, (~ spl30_25 | ~ spl30_29), inference(avatar_split_clause, [], [f1226, f553, f536])).
fof(f1226, plain, (~ (e0 = op(e2, e1)) | ~ spl30_29), inference(backward_demodulation, [], [f199, f555])).
fof(f1229, plain, (~ spl30_13 | ~ spl30_29), inference(avatar_split_clause, [], [f1225, f553, f485])).
fof(f1225, plain, (~ (e0 = op(e3, e0)) | ~ spl30_29), inference(backward_demodulation, [], [f168, f555])).
fof(f1223, plain, (~ spl30_34 | ~ spl30_35), inference(avatar_contradiction_clause, [], [f1222])).
fof(f1222, plain, ($false | (~ spl30_34 | ~ spl30_35)), inference(subsumption_resolution, [], [f1221, f214])).
fof(f1221, plain, ((e1 = e2) | (~ spl30_34 | ~ spl30_35)), inference(backward_demodulation, [], [f580, f576])).
fof(f1220, plain, (~ spl30_35 | ~ spl30_36), inference(avatar_contradiction_clause, [], [f1219])).
fof(f1219, plain, ($false | (~ spl30_35 | ~ spl30_36)), inference(subsumption_resolution, [], [f1218, f216])).
fof(f1218, plain, ((e2 = e3) | (~ spl30_35 | ~ spl30_36)), inference(backward_demodulation, [], [f584, f580])).
fof(f1213, plain, (~ spl30_33 | ~ spl30_37), inference(avatar_split_clause, [], [f1210, f587, f570])).
fof(f1210, plain, (~ (e0 = op(e1, e3)) | ~ spl30_37), inference(backward_demodulation, [], [f198, f589])).
fof(f1211, plain, (~ spl30_21 | ~ spl30_37), inference(avatar_split_clause, [], [f1208, f587, f519])).
fof(f1208, plain, (~ (e0 = op(e2, e2)) | ~ spl30_37), inference(backward_demodulation, [], [f178, f589])).
fof(f1205, plain, (~ spl30_33 | ~ spl30_41), inference(avatar_split_clause, [], [f1196, f604, f570])).
fof(f1196, plain, (~ (e0 = op(e1, e3)) | ~ spl30_41), inference(backward_demodulation, [], [f197, f606])).
fof(f1202, plain, (~ spl30_25 | ~ spl30_41), inference(avatar_split_clause, [], [f1193, f604, f536])).
fof(f1193, plain, (~ (e0 = op(e2, e1)) | ~ spl30_41), inference(backward_demodulation, [], [f172, f606])).
fof(f1192, plain, (~ spl30_33 | ~ spl30_45), inference(avatar_split_clause, [], [f1187, f621, f570])).
fof(f1187, plain, (~ (e0 = op(e1, e3)) | ~ spl30_45), inference(backward_demodulation, [], [f195, f623])).
fof(f1190, plain, (~ spl30_41 | ~ spl30_45), inference(avatar_split_clause, [], [f1185, f621, f604])).
fof(f1185, plain, (~ (e0 = op(e1, e1)) | ~ spl30_45), inference(backward_demodulation, [], [f193, f623])).
fof(f1189, plain, (~ spl30_13 | ~ spl30_45), inference(avatar_split_clause, [], [f1184, f621, f485])).
fof(f1184, plain, (~ (e0 = op(e3, e0)) | ~ spl30_45), inference(backward_demodulation, [], [f167, f623])).
fof(f1188, plain, (~ spl30_29 | ~ spl30_45), inference(avatar_split_clause, [], [f1183, f621, f553])).
fof(f1183, plain, (~ (e0 = op(e2, e0)) | ~ spl30_45), inference(backward_demodulation, [], [f166, f623])).
fof(f1181, plain, (~ spl30_50 | ~ spl30_51), inference(avatar_contradiction_clause, [], [f1180])).
fof(f1180, plain, ($false | (~ spl30_50 | ~ spl30_51)), inference(subsumption_resolution, [], [f1179, f214])).
fof(f1179, plain, ((e1 = e2) | (~ spl30_50 | ~ spl30_51)), inference(backward_demodulation, [], [f648, f644])).
fof(f1178, plain, (~ spl30_51 | ~ spl30_52), inference(avatar_contradiction_clause, [], [f1177])).
fof(f1177, plain, ($false | (~ spl30_51 | ~ spl30_52)), inference(subsumption_resolution, [], [f1176, f216])).
fof(f1176, plain, ((e2 = e3) | (~ spl30_51 | ~ spl30_52)), inference(backward_demodulation, [], [f652, f648])).
fof(f1175, plain, (~ spl30_4 | ~ spl30_52), inference(avatar_split_clause, [], [f1172, f650, f446])).
fof(f1172, plain, (~ (e3 = op(e3, e3)) | ~ spl30_52), inference(backward_demodulation, [], [f183, f652])).
fof(f1169, plain, (~ spl30_49 | ~ spl30_53), inference(avatar_split_clause, [], [f1165, f655, f638])).
fof(f1165, plain, (~ (e0 = op(e0, e3)) | ~ spl30_53), inference(backward_demodulation, [], [f192, f657])).
fof(f1168, plain, (~ spl30_5 | ~ spl30_53), inference(avatar_split_clause, [], [f1164, f655, f451])).
fof(f1164, plain, (~ (e0 = op(e3, e2)) | ~ spl30_53), inference(backward_demodulation, [], [f177, f657])).
fof(f1159, plain, (~ spl30_9 | ~ spl30_57), inference(avatar_split_clause, [], [f1154, f672, f468])).
fof(f1154, plain, (~ (e0 = op(e3, e1)) | ~ spl30_57), inference(backward_demodulation, [], [f171, f674])).
fof(f1149, plain, (~ spl30_57 | ~ spl30_61), inference(avatar_split_clause, [], [f1140, f689, f672])).
fof(f1140, plain, (~ (e0 = op(e0, e1)) | ~ spl30_61), inference(backward_demodulation, [], [f187, f691])).
fof(f1148, plain, (~ spl30_13 | ~ spl30_61), inference(avatar_split_clause, [], [f1139, f689, f485])).
fof(f1139, plain, (~ (e0 = op(e3, e0)) | ~ spl30_61), inference(backward_demodulation, [], [f165, f691])).
fof(f1146, plain, (~ spl30_45 | ~ spl30_61), inference(avatar_split_clause, [], [f1137, f689, f621])).
fof(f1137, plain, (~ (e0 = op(e1, e0)) | ~ spl30_61), inference(backward_demodulation, [], [f163, f691])).
fof(f1136, plain, (~ spl30_116 | ~ spl30_43 | ~ spl30_115), inference(avatar_split_clause, [], [f416, f1115, f612, f1120])).
fof(f416, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(e1, op(e1, e1))) & (e2 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax29)).
fof(f1133, plain, (~ spl30_118 | ~ spl30_62 | ~ spl30_109), inference(avatar_split_clause, [], [f413, f1084, f693, f1130])).
fof(f413, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e2 = op(e0, op(e0, e0))) & (op(e0, e0) = e1) & (e3 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax26)).
fof(f1128, plain, (~ spl30_117 | ~ spl30_21 | ~ spl30_103), inference(avatar_split_clause, [], [f412, f1055, f519, f1125])).
fof(f412, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e1 = op(e2, op(e2, e2))) & (e0 = op(e2, e2)) & (e3 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax25)).
fof(f1123, plain, (~ spl30_116 | ~ spl30_41 | ~ spl30_102), inference(avatar_split_clause, [], [f411, f1050, f604, f1120])).
fof(f411, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e2 = op(e1, op(e1, e1))) & (e0 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax24)).
fof(f1113, plain, (~ spl30_113 | ~ spl30_64 | ~ spl30_114), inference(avatar_split_clause, [], [f409, f1110, f701, f1104])).
fof(f409, plain, (~ (e1 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e1 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e1 = op(e0, op(e0, e0))) & (op(e0, e0) = e3) & (e2 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax22)).
fof(f1107, plain, (~ spl30_113 | ~ spl30_62 | ~ spl30_107), inference(avatar_split_clause, [], [f407, f1074, f693, f1104])).
fof(f407, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e3 = op(e0, op(e0, e0))) & (op(e0, e0) = e1) & (e2 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax20)).
fof(f1097, plain, (~ spl30_111 | ~ spl30_41 | ~ spl30_100), inference(avatar_split_clause, [], [f405, f1040, f604, f1094])).
fof(f405, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e1, op(e1, e1))) & (e0 = op(e1, e1)) & (e2 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax18)).
fof(f1092, plain, (~ spl30_104 | ~ spl30_24 | ~ spl30_110), inference(avatar_split_clause, [], [f404, f1089, f531, f1060])).
fof(f404, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e0 = op(e2, op(e2, e2))) & (e3 = op(e2, e2)) & (e1 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax17)).
fof(f1087, plain, (~ spl30_106 | ~ spl30_64 | ~ spl30_109), inference(avatar_split_clause, [], [f403, f1084, f701, f1070])).
fof(f403, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e2 = op(e0, op(e0, e0))) & (op(e0, e0) = e3) & (e1 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax16)).
fof(f1082, plain, (~ spl30_105 | ~ spl30_3 | ~ spl30_108), inference(avatar_split_clause, [], [f402, f1079, f442, f1065])).
fof(f402, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e3, op(e3, e3))) & (e2 = op(e3, e3)) & (e1 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax15)).
fof(f1077, plain, (~ spl30_106 | ~ spl30_63 | ~ spl30_107), inference(avatar_split_clause, [], [f401, f1074, f697, f1070])).
fof(f401, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e3 = op(e0, op(e0, e0))) & (op(e0, e0) = e2) & (e1 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax14)).
fof(f1068, plain, (~ spl30_105 | ~ spl30_1 | ~ spl30_98), inference(avatar_split_clause, [], [f400, f1031, f434, f1065])).
fof(f400, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e2 = op(e3, op(e3, e3))) & (e0 = op(e3, e3)) & (e1 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax13)).
fof(f1063, plain, (~ spl30_104 | ~ spl30_21 | ~ spl30_96), inference(avatar_split_clause, [], [f399, f1022, f519, f1060])).
fof(f399, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e3 = op(e2, op(e2, e2))) & (e0 = op(e2, e2)) & (e1 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax12)).
fof(f1058, plain, (~ spl30_95 | ~ spl30_24 | ~ spl30_103), inference(avatar_split_clause, [], [f398, f1055, f531, f1018])).
fof(f398, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e1 = op(e2, op(e2, e2))) & (e3 = op(e2, e2)) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax11)).
fof(f1053, plain, (~ spl30_99 | ~ spl30_44 | ~ spl30_102), inference(avatar_split_clause, [], [f397, f1050, f616, f1036])).
fof(f397, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e2 = op(e1, op(e1, e1))) & (e3 = op(e1, e1)) & (e0 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax10)).
fof(f1034, plain, (~ spl30_97 | ~ spl30_2 | ~ spl30_98), inference(avatar_split_clause, [], [f394, f1031, f438, f1027])).
fof(f394, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e2 = op(e3, op(e3, e3))) & (e1 = op(e3, e3)) & (e0 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax7)).
fof(f1016, plain, (spl30_94 | spl30_93 | spl30_92 | spl30_91 | spl30_90 | spl30_89 | spl30_88 | spl30_87 | spl30_86 | spl30_85 | spl30_84 | spl30_83 | spl30_82 | spl30_81 | spl30_80 | spl30_4), inference(avatar_split_clause, [], [f382, f446, f888, f896, f904, f912, f920, f928, f936, f944, f952, f960, f968, f976, f984, f992, f1000])).
fof(f1000, plain, (spl30_94 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl30_94])])).
fof(f992, plain, (spl30_93 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl30_93])])).
fof(f984, plain, (spl30_92 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl30_92])])).
fof(f976, plain, (spl30_91 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl30_91])])).
fof(f968, plain, (spl30_90 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl30_90])])).
fof(f960, plain, (spl30_89 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl30_89])])).
fof(f952, plain, (spl30_88 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl30_88])])).
fof(f944, plain, (spl30_87 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl30_87])])).
fof(f936, plain, (spl30_86 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl30_86])])).
fof(f928, plain, (spl30_85 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl30_85])])).
fof(f920, plain, (spl30_84 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl30_84])])).
fof(f912, plain, (spl30_83 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl30_83])])).
fof(f904, plain, (spl30_82 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl30_82])])).
fof(f896, plain, (spl30_81 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl30_81])])).
fof(f888, plain, (spl30_80 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl30_80])])).
fof(f382, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15) & ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f5, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e1))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e1))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e1))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e2))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e2))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e2))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e3))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e3))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e3))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f69, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP15), inference(usedef, [], [e69])).
fof(e69, plain, (sP15 <=> ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f70, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP16), inference(usedef, [], [e70])).
fof(e70, plain, (sP16 <=> ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f71, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP17), inference(usedef, [], [e71])).
fof(e71, plain, (sP17 <=> ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f72, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP18), inference(usedef, [], [e72])).
fof(e72, plain, (sP18 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f73, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP19), inference(usedef, [], [e73])).
fof(e73, plain, (sP19 <=> ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f74, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP20), inference(usedef, [], [e74])).
fof(e74, plain, (sP20 <=> ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f75, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP21), inference(usedef, [], [e75])).
fof(e75, plain, (sP21 <=> ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f76, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP22), inference(usedef, [], [e76])).
fof(e76, plain, (sP22 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f77, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP23), inference(usedef, [], [e77])).
fof(e77, plain, (sP23 <=> ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f78, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP24), inference(usedef, [], [e78])).
fof(e78, plain, (sP24 <=> ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f79, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP25), inference(usedef, [], [e79])).
fof(e79, plain, (sP25 <=> ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f80, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP26), inference(usedef, [], [e80])).
fof(e80, plain, (sP26 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f81, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP27), inference(usedef, [], [e81])).
fof(e81, plain, (sP27 <=> ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f82, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP28), inference(usedef, [], [e82])).
fof(e82, plain, (sP28 <=> ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f83, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP29), inference(usedef, [], [e83])).
fof(e83, plain, (sP29 <=> ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f5, plain, ((((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e3))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e3))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e3))) | (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e2))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e2))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e2))) | (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e1))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e1))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e1))) | (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (op(e0, e0) = e3)) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (op(e0, e0) = e2)) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (op(e0, e0) = e1)) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax5)).
fof(f1015, plain, (spl30_94 | spl30_93 | spl30_92 | spl30_91 | spl30_90 | spl30_89 | spl30_88 | spl30_87 | spl30_86 | spl30_85 | spl30_84 | spl30_83 | spl30_82 | spl30_81 | spl30_80 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f383, f638, f701, f888, f896, f904, f912, f920, f928, f936, f944, f952, f960, f968, f976, f984, f992, f1000])).
fof(f383, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f84])).
fof(f1014, plain, (spl30_94 | spl30_93 | spl30_92 | spl30_91 | spl30_90 | spl30_89 | spl30_88 | spl30_87 | spl30_86 | spl30_85 | spl30_84 | spl30_83 | spl30_82 | spl30_81 | spl30_80 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f384, f574, f616, f888, f896, f904, f912, f920, f928, f936, f944, f952, f960, f968, f976, f984, f992, f1000])).
fof(f384, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f84])).
fof(f1013, plain, (spl30_94 | spl30_93 | spl30_92 | spl30_91 | spl30_90 | spl30_89 | spl30_88 | spl30_87 | spl30_86 | spl30_85 | spl30_84 | spl30_83 | spl30_82 | spl30_81 | spl30_80 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f385, f510, f531, f888, f896, f904, f912, f920, f928, f936, f944, f952, f960, f968, f976, f984, f992, f1000])).
fof(f385, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f84])).
fof(f1012, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | spl30_4), inference(avatar_split_clause, [], [f387, f446, f738, f748, f758, f768, f778, f788, f798, f808, f818, f828, f838, f848, f858, f868, f878])).
fof(f878, plain, (spl30_79 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl30_79])])).
fof(f868, plain, (spl30_78 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl30_78])])).
fof(f858, plain, (spl30_77 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl30_77])])).
fof(f848, plain, (spl30_76 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl30_76])])).
fof(f838, plain, (spl30_75 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl30_75])])).
fof(f828, plain, (spl30_74 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl30_74])])).
fof(f818, plain, (spl30_73 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl30_73])])).
fof(f808, plain, (spl30_72 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl30_72])])).
fof(f798, plain, (spl30_71 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl30_71])])).
fof(f788, plain, (spl30_70 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl30_70])])).
fof(f778, plain, (spl30_69 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl30_69])])).
fof(f768, plain, (spl30_68 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl30_68])])).
fof(f758, plain, (spl30_67 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl30_67])])).
fof(f748, plain, (spl30_66 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl30_66])])).
fof(f738, plain, (spl30_65 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl30_65])])).
fof(f387, plain, ((e3 = op(e3, e3)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f1010, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_16 | ~ spl30_1), inference(avatar_split_clause, [], [f389, f434, f497, f738, f748, f758, f768, f778, f788, f798, f808, f818, f828, f838, f848, f858, f868, f878])).
fof(f389, plain, (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f1009, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_12 | ~ spl30_2), inference(avatar_split_clause, [], [f390, f438, f480, f738, f748, f758, f768, f778, f788, f798, f808, f818, f828, f838, f848, f858, f868, f878])).
fof(f390, plain, (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f1008, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_8 | ~ spl30_3), inference(avatar_split_clause, [], [f391, f442, f463, f738, f748, f758, f768, f778, f788, f798, f808, f818, f828, f838, f848, f858, f868, f878])).
fof(f391, plain, (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f1007, plain, (spl30_79 | spl30_78 | spl30_77 | spl30_76 | spl30_75 | spl30_74 | spl30_73 | spl30_72 | spl30_71 | spl30_70 | spl30_69 | spl30_68 | spl30_67 | spl30_66 | spl30_65 | ~ spl30_4), inference(avatar_split_clause, [], [f417, f446, f738, f748, f758, f768, f778, f788, f798, f808, f818, f828, f838, f848, f858, f868, f878])).
fof(f417, plain, (~ (e3 = op(e3, e3)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(duplicate_literal_removal, [], [f392])).
fof(f392, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f84])).
fof(f1006, plain, (~ spl30_94 | spl30_61), inference(avatar_split_clause, [], [f377, f689, f1000])).
fof(f377, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f114])).
fof(f114, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f1005, plain, (~ spl30_94 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f379, f625, f604, f1000])).
fof(f379, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f114])).
fof(f1004, plain, (~ spl30_94 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f380, f561, f519, f1000])).
fof(f380, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f114])).
fof(f1003, plain, (~ spl30_94 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f381, f497, f434, f1000])).
fof(f381, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f114])).
fof(f998, plain, (~ spl30_93 | spl30_62), inference(avatar_split_clause, [], [f372, f693, f992])).
fof(f372, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f997, plain, (~ spl30_93 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f373, f672, f693, f992])).
fof(f373, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f113])).
fof(f996, plain, (~ spl30_93 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f375, f544, f523, f992])).
fof(f375, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f113])).
fof(f995, plain, (~ spl30_93 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f376, f480, f438, f992])).
fof(f376, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f113])).
fof(f990, plain, (~ spl30_92 | spl30_63), inference(avatar_split_clause, [], [f367, f697, f984])).
fof(f367, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f989, plain, (~ spl30_92 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f368, f655, f697, f984])).
fof(f368, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f988, plain, (~ spl30_92 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f369, f591, f612, f984])).
fof(f369, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f987, plain, (~ spl30_92 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f371, f463, f442, f984])).
fof(f371, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f112])).
fof(f982, plain, (~ spl30_91 | spl30_64), inference(avatar_split_clause, [], [f362, f701, f976])).
fof(f362, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f981, plain, (~ spl30_91 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f363, f638, f701, f976])).
fof(f363, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f980, plain, (~ spl30_91 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f364, f574, f616, f976])).
fof(f364, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f979, plain, (~ spl30_91 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f365, f510, f531, f976])).
fof(f365, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP3), inference(cnf_transformation, [], [f111])).
fof(f974, plain, (~ spl30_90 | spl30_41), inference(avatar_split_clause, [], [f357, f604, f968])).
fof(f357, plain, ((e0 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e1, e1))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f973, plain, (~ spl30_90 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f359, f625, f604, f968])).
fof(f359, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f110])).
fof(f972, plain, (~ spl30_90 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f360, f561, f519, f968])).
fof(f360, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP4), inference(cnf_transformation, [], [f110])).
fof(f971, plain, (~ spl30_90 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f361, f497, f434, f968])).
fof(f361, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP4), inference(cnf_transformation, [], [f110])).
fof(f966, plain, (~ spl30_89 | spl30_42), inference(avatar_split_clause, [], [f352, f608, f960])).
fof(f352, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f965, plain, (~ spl30_89 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f353, f672, f693, f960])).
fof(f353, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP5), inference(cnf_transformation, [], [f109])).
fof(f964, plain, (~ spl30_89 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f355, f544, f523, f960])).
fof(f355, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP5), inference(cnf_transformation, [], [f109])).
fof(f963, plain, (~ spl30_89 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f356, f480, f438, f960])).
fof(f356, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP5), inference(cnf_transformation, [], [f109])).
fof(f958, plain, (~ spl30_88 | spl30_43), inference(avatar_split_clause, [], [f347, f612, f952])).
fof(f347, plain, ((e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f957, plain, (~ spl30_88 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f348, f655, f697, f952])).
fof(f348, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f956, plain, (~ spl30_88 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f349, f591, f612, f952])).
fof(f349, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f955, plain, (~ spl30_88 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f351, f463, f442, f952])).
fof(f351, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP6), inference(cnf_transformation, [], [f108])).
fof(f950, plain, (~ spl30_87 | spl30_44), inference(avatar_split_clause, [], [f342, f616, f944])).
fof(f342, plain, ((e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e1, e1))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f949, plain, (~ spl30_87 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f343, f638, f701, f944])).
fof(f343, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f948, plain, (~ spl30_87 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f344, f574, f616, f944])).
fof(f344, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f947, plain, (~ spl30_87 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f345, f510, f531, f944])).
fof(f345, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP7), inference(cnf_transformation, [], [f107])).
fof(f942, plain, (~ spl30_86 | spl30_21), inference(avatar_split_clause, [], [f337, f519, f936])).
fof(f337, plain, ((e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e2, e2))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f941, plain, (~ spl30_86 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f339, f625, f604, f936])).
fof(f339, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f940, plain, (~ spl30_86 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f340, f561, f519, f936])).
fof(f340, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f939, plain, (~ spl30_86 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f341, f497, f434, f936])).
fof(f341, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP8), inference(cnf_transformation, [], [f106])).
fof(f934, plain, (~ spl30_85 | spl30_22), inference(avatar_split_clause, [], [f332, f523, f928])).
fof(f332, plain, ((e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e2, e2))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f933, plain, (~ spl30_85 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f333, f672, f693, f928])).
fof(f333, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f932, plain, (~ spl30_85 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f335, f544, f523, f928])).
fof(f335, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f931, plain, (~ spl30_85 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f336, f480, f438, f928])).
fof(f336, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP9), inference(cnf_transformation, [], [f105])).
fof(f926, plain, (~ spl30_84 | spl30_23), inference(avatar_split_clause, [], [f327, f527, f920])).
fof(f327, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f925, plain, (~ spl30_84 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f328, f655, f697, f920])).
fof(f328, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP10), inference(cnf_transformation, [], [f104])).
fof(f924, plain, (~ spl30_84 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f329, f591, f612, f920])).
fof(f329, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP10), inference(cnf_transformation, [], [f104])).
fof(f923, plain, (~ spl30_84 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f331, f463, f442, f920])).
fof(f331, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP10), inference(cnf_transformation, [], [f104])).
fof(f918, plain, (~ spl30_83 | spl30_24), inference(avatar_split_clause, [], [f322, f531, f912])).
fof(f322, plain, ((e3 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f103])).
fof(f103, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e2, e2))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f917, plain, (~ spl30_83 | ~ spl30_64 | spl30_49), inference(avatar_split_clause, [], [f323, f638, f701, f912])).
fof(f323, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP11), inference(cnf_transformation, [], [f103])).
fof(f916, plain, (~ spl30_83 | ~ spl30_44 | spl30_34), inference(avatar_split_clause, [], [f324, f574, f616, f912])).
fof(f324, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP11), inference(cnf_transformation, [], [f103])).
fof(f915, plain, (~ spl30_83 | ~ spl30_24 | spl30_19), inference(avatar_split_clause, [], [f325, f510, f531, f912])).
fof(f325, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f103])).
fof(f910, plain, (~ spl30_82 | spl30_1), inference(avatar_split_clause, [], [f317, f434, f904])).
fof(f317, plain, ((e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e3, e3))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f909, plain, (~ spl30_82 | ~ spl30_41 | spl30_46), inference(avatar_split_clause, [], [f319, f625, f604, f904])).
fof(f319, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f908, plain, (~ spl30_82 | ~ spl30_21 | spl30_31), inference(avatar_split_clause, [], [f320, f561, f519, f904])).
fof(f320, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f907, plain, (~ spl30_82 | ~ spl30_1 | spl30_16), inference(avatar_split_clause, [], [f321, f497, f434, f904])).
fof(f321, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f102])).
fof(f902, plain, (~ spl30_81 | spl30_2), inference(avatar_split_clause, [], [f312, f438, f896])).
fof(f312, plain, ((e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e3, e3))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f901, plain, (~ spl30_81 | ~ spl30_62 | spl30_57), inference(avatar_split_clause, [], [f313, f672, f693, f896])).
fof(f313, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f900, plain, (~ spl30_81 | ~ spl30_22 | spl30_27), inference(avatar_split_clause, [], [f315, f544, f523, f896])).
fof(f315, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f899, plain, (~ spl30_81 | ~ spl30_2 | spl30_12), inference(avatar_split_clause, [], [f316, f480, f438, f896])).
fof(f316, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f101])).
fof(f894, plain, (~ spl30_80 | spl30_3), inference(avatar_split_clause, [], [f307, f442, f888])).
fof(f307, plain, ((e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e3, e3))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f893, plain, (~ spl30_80 | ~ spl30_63 | spl30_53), inference(avatar_split_clause, [], [f308, f655, f697, f888])).
fof(f308, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f892, plain, (~ spl30_80 | ~ spl30_43 | spl30_38), inference(avatar_split_clause, [], [f309, f591, f612, f888])).
fof(f309, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f891, plain, (~ spl30_80 | ~ spl30_3 | spl30_8), inference(avatar_split_clause, [], [f311, f463, f442, f888])).
fof(f311, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f100])).
fof(f886, plain, (~ spl30_79 | spl30_61), inference(avatar_split_clause, [], [f301, f689, f878])).
fof(f301, plain, ((e0 = op(e0, e0)) | ~ sP15), inference(cnf_transformation, [], [f99])).
fof(f99, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP15), inference(nnf_transformation, [], [f69])).
fof(f884, plain, (~ spl30_79 | ~ spl30_61), inference(avatar_split_clause, [], [f418, f689, f878])).
fof(f418, plain, (~ (e0 = op(e0, e0)) | ~ sP15), inference(duplicate_literal_removal, [], [f303])).
fof(f303, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP15), inference(cnf_transformation, [], [f99])).
fof(f876, plain, (~ spl30_78 | spl30_57), inference(avatar_split_clause, [], [f295, f672, f868])).
fof(f295, plain, ((e0 = op(e0, e1)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f98, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP16), inference(nnf_transformation, [], [f70])).
fof(f875, plain, (~ spl30_78 | spl30_45), inference(avatar_split_clause, [], [f296, f621, f868])).
fof(f296, plain, ((e0 = op(e1, e0)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f873, plain, (~ spl30_78 | ~ spl30_42), inference(avatar_split_clause, [], [f419, f608, f868])).
fof(f419, plain, (~ (e1 = op(e1, e1)) | ~ sP16), inference(duplicate_literal_removal, [], [f298])).
fof(f298, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f872, plain, (~ spl30_78 | ~ spl30_38 | ~ spl30_43), inference(avatar_split_clause, [], [f299, f612, f591, f868])).
fof(f299, plain, (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f871, plain, (~ spl30_78 | ~ spl30_34 | ~ spl30_44), inference(avatar_split_clause, [], [f300, f616, f574, f868])).
fof(f300, plain, (~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3)) | ~ sP16), inference(cnf_transformation, [], [f98])).
fof(f866, plain, (~ spl30_77 | spl30_53), inference(avatar_split_clause, [], [f289, f655, f858])).
fof(f289, plain, ((e0 = op(e0, e2)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f97, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP17), inference(nnf_transformation, [], [f71])).
fof(f865, plain, (~ spl30_77 | spl30_29), inference(avatar_split_clause, [], [f290, f553, f858])).
fof(f290, plain, ((e0 = op(e2, e0)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f863, plain, (~ spl30_77 | ~ spl30_27 | ~ spl30_22), inference(avatar_split_clause, [], [f292, f523, f544, f858])).
fof(f292, plain, (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f862, plain, (~ spl30_77 | ~ spl30_23), inference(avatar_split_clause, [], [f420, f527, f858])).
fof(f420, plain, (~ (e2 = op(e2, e2)) | ~ sP17), inference(duplicate_literal_removal, [], [f293])).
fof(f293, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f861, plain, (~ spl30_77 | ~ spl30_19 | ~ spl30_24), inference(avatar_split_clause, [], [f294, f531, f510, f858])).
fof(f294, plain, (~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3)) | ~ sP17), inference(cnf_transformation, [], [f97])).
fof(f856, plain, (~ spl30_76 | spl30_49), inference(avatar_split_clause, [], [f283, f638, f848])).
fof(f283, plain, ((e0 = op(e0, e3)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f96, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP18), inference(nnf_transformation, [], [f72])).
fof(f855, plain, (~ spl30_76 | spl30_13), inference(avatar_split_clause, [], [f284, f485, f848])).
fof(f284, plain, ((e0 = op(e3, e0)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f854, plain, (~ spl30_76 | ~ spl30_16 | ~ spl30_1), inference(avatar_split_clause, [], [f285, f434, f497, f848])).
fof(f285, plain, (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f853, plain, (~ spl30_76 | ~ spl30_12 | ~ spl30_2), inference(avatar_split_clause, [], [f286, f438, f480, f848])).
fof(f286, plain, (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f852, plain, (~ spl30_76 | ~ spl30_8 | ~ spl30_3), inference(avatar_split_clause, [], [f287, f442, f463, f848])).
fof(f287, plain, (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f851, plain, (~ spl30_76 | ~ spl30_4), inference(avatar_split_clause, [], [f421, f446, f848])).
fof(f421, plain, (~ (e3 = op(e3, e3)) | ~ sP18), inference(duplicate_literal_removal, [], [f288])).
fof(f288, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f96])).
fof(f846, plain, (~ spl30_75 | spl30_46), inference(avatar_split_clause, [], [f277, f625, f838])).
fof(f277, plain, ((e1 = op(e1, e0)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f95, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP19), inference(nnf_transformation, [], [f73])).
fof(f845, plain, (~ spl30_75 | spl30_58), inference(avatar_split_clause, [], [f278, f676, f838])).
fof(f278, plain, ((e1 = op(e0, e1)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f844, plain, (~ spl30_75 | ~ spl30_61), inference(avatar_split_clause, [], [f422, f689, f838])).
fof(f422, plain, (~ (e0 = op(e0, e0)) | ~ sP19), inference(duplicate_literal_removal, [], [f279])).
fof(f279, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f843, plain, (~ spl30_75 | ~ spl30_57 | ~ spl30_62), inference(avatar_split_clause, [], [f280, f693, f672, f838])).
fof(f280, plain, (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f842, plain, (~ spl30_75 | ~ spl30_53 | ~ spl30_63), inference(avatar_split_clause, [], [f281, f697, f655, f838])).
fof(f281, plain, (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f841, plain, (~ spl30_75 | ~ spl30_49 | ~ spl30_64), inference(avatar_split_clause, [], [f282, f701, f638, f838])).
fof(f282, plain, (~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3)) | ~ sP19), inference(cnf_transformation, [], [f95])).
fof(f836, plain, (~ spl30_74 | spl30_42), inference(avatar_split_clause, [], [f271, f608, f828])).
fof(f271, plain, ((e1 = op(e1, e1)) | ~ sP20), inference(cnf_transformation, [], [f94])).
fof(f94, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP20), inference(nnf_transformation, [], [f74])).
fof(f833, plain, (~ spl30_74 | ~ spl30_42), inference(avatar_split_clause, [], [f423, f608, f828])).
fof(f423, plain, (~ (e1 = op(e1, e1)) | ~ sP20), inference(duplicate_literal_removal, [], [f274])).
fof(f274, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP20), inference(cnf_transformation, [], [f94])).
fof(f826, plain, (~ spl30_73 | spl30_38), inference(avatar_split_clause, [], [f265, f591, f818])).
fof(f265, plain, ((e1 = op(e1, e2)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f93, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP21), inference(nnf_transformation, [], [f75])).
fof(f825, plain, (~ spl30_73 | spl30_26), inference(avatar_split_clause, [], [f266, f540, f818])).
fof(f266, plain, ((e1 = op(e2, e1)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f824, plain, (~ spl30_73 | ~ spl30_31 | ~ spl30_21), inference(avatar_split_clause, [], [f267, f519, f561, f818])).
fof(f267, plain, (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f822, plain, (~ spl30_73 | ~ spl30_23), inference(avatar_split_clause, [], [f424, f527, f818])).
fof(f424, plain, (~ (e2 = op(e2, e2)) | ~ sP21), inference(duplicate_literal_removal, [], [f269])).
fof(f269, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f821, plain, (~ spl30_73 | ~ spl30_19 | ~ spl30_24), inference(avatar_split_clause, [], [f270, f531, f510, f818])).
fof(f270, plain, (~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3)) | ~ sP21), inference(cnf_transformation, [], [f93])).
fof(f816, plain, (~ spl30_72 | spl30_34), inference(avatar_split_clause, [], [f259, f574, f808])).
fof(f259, plain, ((e1 = op(e1, e3)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f92, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP22), inference(nnf_transformation, [], [f76])).
fof(f815, plain, (~ spl30_72 | spl30_10), inference(avatar_split_clause, [], [f260, f472, f808])).
fof(f260, plain, ((e1 = op(e3, e1)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f814, plain, (~ spl30_72 | ~ spl30_16 | ~ spl30_1), inference(avatar_split_clause, [], [f261, f434, f497, f808])).
fof(f261, plain, (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f813, plain, (~ spl30_72 | ~ spl30_12 | ~ spl30_2), inference(avatar_split_clause, [], [f262, f438, f480, f808])).
fof(f262, plain, (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f812, plain, (~ spl30_72 | ~ spl30_8 | ~ spl30_3), inference(avatar_split_clause, [], [f263, f442, f463, f808])).
fof(f263, plain, (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f811, plain, (~ spl30_72 | ~ spl30_4), inference(avatar_split_clause, [], [f425, f446, f808])).
fof(f425, plain, (~ (e3 = op(e3, e3)) | ~ sP22), inference(duplicate_literal_removal, [], [f264])).
fof(f264, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP22), inference(cnf_transformation, [], [f92])).
fof(f806, plain, (~ spl30_71 | spl30_31), inference(avatar_split_clause, [], [f253, f561, f798])).
fof(f253, plain, ((e2 = op(e2, e0)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f91, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP23), inference(nnf_transformation, [], [f77])).
fof(f805, plain, (~ spl30_71 | spl30_55), inference(avatar_split_clause, [], [f254, f663, f798])).
fof(f254, plain, ((e2 = op(e0, e2)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f804, plain, (~ spl30_71 | ~ spl30_61), inference(avatar_split_clause, [], [f426, f689, f798])).
fof(f426, plain, (~ (e0 = op(e0, e0)) | ~ sP23), inference(duplicate_literal_removal, [], [f255])).
fof(f255, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f803, plain, (~ spl30_71 | ~ spl30_57 | ~ spl30_62), inference(avatar_split_clause, [], [f256, f693, f672, f798])).
fof(f256, plain, (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f802, plain, (~ spl30_71 | ~ spl30_53 | ~ spl30_63), inference(avatar_split_clause, [], [f257, f697, f655, f798])).
fof(f257, plain, (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f801, plain, (~ spl30_71 | ~ spl30_49 | ~ spl30_64), inference(avatar_split_clause, [], [f258, f701, f638, f798])).
fof(f258, plain, (~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3)) | ~ sP23), inference(cnf_transformation, [], [f91])).
fof(f796, plain, (~ spl30_70 | spl30_27), inference(avatar_split_clause, [], [f247, f544, f788])).
fof(f247, plain, ((e2 = op(e2, e1)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f90, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP24), inference(nnf_transformation, [], [f78])).
fof(f795, plain, (~ spl30_70 | spl30_39), inference(avatar_split_clause, [], [f248, f595, f788])).
fof(f248, plain, ((e2 = op(e1, e2)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f794, plain, (~ spl30_70 | ~ spl30_46 | ~ spl30_41), inference(avatar_split_clause, [], [f249, f604, f625, f788])).
fof(f249, plain, (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f793, plain, (~ spl30_70 | ~ spl30_42), inference(avatar_split_clause, [], [f427, f608, f788])).
fof(f427, plain, (~ (e1 = op(e1, e1)) | ~ sP24), inference(duplicate_literal_removal, [], [f250])).
fof(f250, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f792, plain, (~ spl30_70 | ~ spl30_38 | ~ spl30_43), inference(avatar_split_clause, [], [f251, f612, f591, f788])).
fof(f251, plain, (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f791, plain, (~ spl30_70 | ~ spl30_34 | ~ spl30_44), inference(avatar_split_clause, [], [f252, f616, f574, f788])).
fof(f252, plain, (~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3)) | ~ sP24), inference(cnf_transformation, [], [f90])).
fof(f786, plain, (~ spl30_69 | spl30_23), inference(avatar_split_clause, [], [f241, f527, f778])).
fof(f241, plain, ((e2 = op(e2, e2)) | ~ sP25), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP25), inference(nnf_transformation, [], [f79])).
fof(f782, plain, (~ spl30_69 | ~ spl30_23), inference(avatar_split_clause, [], [f428, f527, f778])).
fof(f428, plain, (~ (e2 = op(e2, e2)) | ~ sP25), inference(duplicate_literal_removal, [], [f245])).
fof(f245, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP25), inference(cnf_transformation, [], [f89])).
fof(f776, plain, (~ spl30_68 | spl30_19), inference(avatar_split_clause, [], [f235, f510, f768])).
fof(f235, plain, ((e2 = op(e2, e3)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f88, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP26), inference(nnf_transformation, [], [f80])).
fof(f775, plain, (~ spl30_68 | spl30_7), inference(avatar_split_clause, [], [f236, f459, f768])).
fof(f236, plain, ((e2 = op(e3, e2)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f774, plain, (~ spl30_68 | ~ spl30_16 | ~ spl30_1), inference(avatar_split_clause, [], [f237, f434, f497, f768])).
fof(f237, plain, (~ (e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f773, plain, (~ spl30_68 | ~ spl30_12 | ~ spl30_2), inference(avatar_split_clause, [], [f238, f438, f480, f768])).
fof(f238, plain, (~ (e1 = op(e3, e3)) | ~ (e3 = op(e3, e1)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f771, plain, (~ spl30_68 | ~ spl30_4), inference(avatar_split_clause, [], [f429, f446, f768])).
fof(f429, plain, (~ (e3 = op(e3, e3)) | ~ sP26), inference(duplicate_literal_removal, [], [f240])).
fof(f240, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP26), inference(cnf_transformation, [], [f88])).
fof(f766, plain, (~ spl30_67 | spl30_16), inference(avatar_split_clause, [], [f229, f497, f758])).
fof(f229, plain, ((e3 = op(e3, e0)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((~ (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP27), inference(nnf_transformation, [], [f81])).
fof(f765, plain, (~ spl30_67 | spl30_52), inference(avatar_split_clause, [], [f230, f650, f758])).
fof(f230, plain, ((e3 = op(e0, e3)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f764, plain, (~ spl30_67 | ~ spl30_61), inference(avatar_split_clause, [], [f430, f689, f758])).
fof(f430, plain, (~ (e0 = op(e0, e0)) | ~ sP27), inference(duplicate_literal_removal, [], [f231])).
fof(f231, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f763, plain, (~ spl30_67 | ~ spl30_57 | ~ spl30_62), inference(avatar_split_clause, [], [f232, f693, f672, f758])).
fof(f232, plain, (~ (op(e0, e0) = e1) | ~ (e0 = op(e0, e1)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f762, plain, (~ spl30_67 | ~ spl30_53 | ~ spl30_63), inference(avatar_split_clause, [], [f233, f697, f655, f758])).
fof(f233, plain, (~ (op(e0, e0) = e2) | ~ (e0 = op(e0, e2)) | ~ sP27), inference(cnf_transformation, [], [f87])).
fof(f756, plain, (~ spl30_66 | spl30_12), inference(avatar_split_clause, [], [f223, f480, f748])).
fof(f223, plain, ((e3 = op(e3, e1)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (((~ (e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP28), inference(nnf_transformation, [], [f82])).
fof(f755, plain, (~ spl30_66 | spl30_36), inference(avatar_split_clause, [], [f224, f582, f748])).
fof(f224, plain, ((e3 = op(e1, e3)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f754, plain, (~ spl30_66 | ~ spl30_46 | ~ spl30_41), inference(avatar_split_clause, [], [f225, f604, f625, f748])).
fof(f225, plain, (~ (e0 = op(e1, e1)) | ~ (e1 = op(e1, e0)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f753, plain, (~ spl30_66 | ~ spl30_42), inference(avatar_split_clause, [], [f431, f608, f748])).
fof(f431, plain, (~ (e1 = op(e1, e1)) | ~ sP28), inference(duplicate_literal_removal, [], [f226])).
fof(f226, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f752, plain, (~ spl30_66 | ~ spl30_38 | ~ spl30_43), inference(avatar_split_clause, [], [f227, f612, f591, f748])).
fof(f227, plain, (~ (e2 = op(e1, e1)) | ~ (e1 = op(e1, e2)) | ~ sP28), inference(cnf_transformation, [], [f86])).
fof(f746, plain, (~ spl30_65 | spl30_8), inference(avatar_split_clause, [], [f217, f463, f738])).
fof(f217, plain, ((e3 = op(e3, e2)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (((~ (e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP29), inference(nnf_transformation, [], [f83])).
fof(f745, plain, (~ spl30_65 | spl30_20), inference(avatar_split_clause, [], [f218, f514, f738])).
fof(f218, plain, ((e3 = op(e2, e3)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f744, plain, (~ spl30_65 | ~ spl30_31 | ~ spl30_21), inference(avatar_split_clause, [], [f219, f519, f561, f738])).
fof(f219, plain, (~ (e0 = op(e2, e2)) | ~ (e2 = op(e2, e0)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f743, plain, (~ spl30_65 | ~ spl30_27 | ~ spl30_22), inference(avatar_split_clause, [], [f220, f523, f544, f738])).
fof(f220, plain, (~ (e1 = op(e2, e2)) | ~ (e2 = op(e2, e1)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f742, plain, (~ spl30_65 | ~ spl30_23), inference(avatar_split_clause, [], [f432, f527, f738])).
fof(f432, plain, (~ (e2 = op(e2, e2)) | ~ sP29), inference(duplicate_literal_removal, [], [f221])).
fof(f221, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP29), inference(cnf_transformation, [], [f85])).
fof(f736, plain, (spl30_61 | spl30_57 | spl30_53 | spl30_49), inference(avatar_split_clause, [], [f131, f638, f655, f672, f689])).
fof(f131, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax2)).
fof(f735, plain, (spl30_61 | spl30_45 | spl30_29 | spl30_13), inference(avatar_split_clause, [], [f132, f485, f553, f621, f689])).
fof(f132, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f734, plain, (spl30_62 | spl30_58 | spl30_54 | spl30_50), inference(avatar_split_clause, [], [f133, f642, f659, f676, f693])).
fof(f133, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f733, plain, (spl30_62 | spl30_46 | spl30_30 | spl30_14), inference(avatar_split_clause, [], [f134, f489, f557, f625, f693])).
fof(f134, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f732, plain, (spl30_63 | spl30_59 | spl30_55 | spl30_51), inference(avatar_split_clause, [], [f135, f646, f663, f680, f697])).
fof(f135, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f731, plain, (spl30_63 | spl30_47 | spl30_31 | spl30_15), inference(avatar_split_clause, [], [f136, f493, f561, f629, f697])).
fof(f136, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f730, plain, (spl30_64 | spl30_60 | spl30_56 | spl30_52), inference(avatar_split_clause, [], [f137, f650, f667, f684, f701])).
fof(f137, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f729, plain, (spl30_64 | spl30_48 | spl30_32 | spl30_16), inference(avatar_split_clause, [], [f138, f497, f565, f633, f701])).
fof(f138, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f728, plain, (spl30_45 | spl30_41 | spl30_37 | spl30_33), inference(avatar_split_clause, [], [f139, f570, f587, f604, f621])).
fof(f139, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f727, plain, (spl30_57 | spl30_41 | spl30_25 | spl30_9), inference(avatar_split_clause, [], [f140, f468, f536, f604, f672])).
fof(f140, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f726, plain, (spl30_46 | spl30_42 | spl30_38 | spl30_34), inference(avatar_split_clause, [], [f141, f574, f591, f608, f625])).
fof(f141, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f725, plain, (spl30_58 | spl30_42 | spl30_26 | spl30_10), inference(avatar_split_clause, [], [f142, f472, f540, f608, f676])).
fof(f142, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f724, plain, (spl30_47 | spl30_43 | spl30_39 | spl30_35), inference(avatar_split_clause, [], [f143, f578, f595, f612, f629])).
fof(f143, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f723, plain, (spl30_59 | spl30_43 | spl30_27 | spl30_11), inference(avatar_split_clause, [], [f144, f476, f544, f612, f680])).
fof(f144, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f722, plain, (spl30_48 | spl30_44 | spl30_40 | spl30_36), inference(avatar_split_clause, [], [f145, f582, f599, f616, f633])).
fof(f145, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f721, plain, (spl30_60 | spl30_44 | spl30_28 | spl30_12), inference(avatar_split_clause, [], [f146, f480, f548, f616, f684])).
fof(f146, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f720, plain, (spl30_29 | spl30_25 | spl30_21 | spl30_17), inference(avatar_split_clause, [], [f147, f502, f519, f536, f553])).
fof(f147, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f719, plain, (spl30_53 | spl30_37 | spl30_21 | spl30_5), inference(avatar_split_clause, [], [f148, f451, f519, f587, f655])).
fof(f148, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f718, plain, (spl30_30 | spl30_26 | spl30_22 | spl30_18), inference(avatar_split_clause, [], [f149, f506, f523, f540, f557])).
fof(f149, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f717, plain, (spl30_54 | spl30_38 | spl30_22 | spl30_6), inference(avatar_split_clause, [], [f150, f455, f523, f591, f659])).
fof(f150, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f716, plain, (spl30_31 | spl30_27 | spl30_23 | spl30_19), inference(avatar_split_clause, [], [f151, f510, f527, f544, f561])).
fof(f151, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f715, plain, (spl30_55 | spl30_39 | spl30_23 | spl30_7), inference(avatar_split_clause, [], [f152, f459, f527, f595, f663])).
fof(f152, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f714, plain, (spl30_32 | spl30_28 | spl30_24 | spl30_20), inference(avatar_split_clause, [], [f153, f514, f531, f548, f565])).
fof(f153, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f713, plain, (spl30_56 | spl30_40 | spl30_24 | spl30_8), inference(avatar_split_clause, [], [f154, f463, f531, f599, f667])).
fof(f154, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f712, plain, (spl30_13 | spl30_9 | spl30_5 | spl30_1), inference(avatar_split_clause, [], [f155, f434, f451, f468, f485])).
fof(f155, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f711, plain, (spl30_49 | spl30_33 | spl30_17 | spl30_1), inference(avatar_split_clause, [], [f156, f434, f502, f570, f638])).
fof(f156, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f710, plain, (spl30_14 | spl30_10 | spl30_6 | spl30_2), inference(avatar_split_clause, [], [f157, f438, f455, f472, f489])).
fof(f157, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f709, plain, (spl30_50 | spl30_34 | spl30_18 | spl30_2), inference(avatar_split_clause, [], [f158, f438, f506, f574, f642])).
fof(f158, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f708, plain, (spl30_15 | spl30_11 | spl30_7 | spl30_3), inference(avatar_split_clause, [], [f159, f442, f459, f476, f493])).
fof(f159, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f707, plain, (spl30_51 | spl30_35 | spl30_19 | spl30_3), inference(avatar_split_clause, [], [f160, f442, f510, f578, f646])).
fof(f160, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f706, plain, (spl30_16 | spl30_12 | spl30_8 | spl30_4), inference(avatar_split_clause, [], [f161, f446, f463, f480, f497])).
fof(f161, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f705, plain, (spl30_52 | spl30_36 | spl30_20 | spl30_4), inference(avatar_split_clause, [], [f162, f446, f514, f582, f650])).
fof(f162, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f704, plain, (spl30_61 | spl30_62 | spl30_63 | spl30_64), inference(avatar_split_clause, [], [f115, f701, f697, f693, f689])).
fof(f115, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG153+1.p', ax1)).
fof(f687, plain, (spl30_57 | spl30_58 | spl30_59 | spl30_60), inference(avatar_split_clause, [], [f116, f684, f680, f676, f672])).
fof(f116, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f670, plain, (spl30_53 | spl30_54 | spl30_55 | spl30_56), inference(avatar_split_clause, [], [f117, f667, f663, f659, f655])).
fof(f117, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f653, plain, (spl30_49 | spl30_50 | spl30_51 | spl30_52), inference(avatar_split_clause, [], [f118, f650, f646, f642, f638])).
fof(f118, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f636, plain, (spl30_45 | spl30_46 | spl30_47 | spl30_48), inference(avatar_split_clause, [], [f119, f633, f629, f625, f621])).
fof(f119, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f619, plain, (spl30_41 | spl30_42 | spl30_43 | spl30_44), inference(avatar_split_clause, [], [f120, f616, f612, f608, f604])).
fof(f120, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f602, plain, (spl30_37 | spl30_38 | spl30_39 | spl30_40), inference(avatar_split_clause, [], [f121, f599, f595, f591, f587])).
fof(f121, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f585, plain, (spl30_33 | spl30_34 | spl30_35 | spl30_36), inference(avatar_split_clause, [], [f122, f582, f578, f574, f570])).
fof(f122, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f568, plain, (spl30_29 | spl30_30 | spl30_31 | spl30_32), inference(avatar_split_clause, [], [f123, f565, f561, f557, f553])).
fof(f123, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f551, plain, (spl30_25 | spl30_26 | spl30_27 | spl30_28), inference(avatar_split_clause, [], [f124, f548, f544, f540, f536])).
fof(f124, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f534, plain, (spl30_21 | spl30_22 | spl30_23 | spl30_24), inference(avatar_split_clause, [], [f125, f531, f527, f523, f519])).
fof(f125, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f517, plain, (spl30_17 | spl30_18 | spl30_19 | spl30_20), inference(avatar_split_clause, [], [f126, f514, f510, f506, f502])).
fof(f126, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f500, plain, (spl30_13 | spl30_14 | spl30_15 | spl30_16), inference(avatar_split_clause, [], [f127, f497, f493, f489, f485])).
fof(f127, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f483, plain, (spl30_9 | spl30_10 | spl30_11 | spl30_12), inference(avatar_split_clause, [], [f128, f480, f476, f472, f468])).
fof(f128, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f466, plain, (spl30_5 | spl30_6 | spl30_7 | spl30_8), inference(avatar_split_clause, [], [f129, f463, f459, f455, f451])).
fof(f129, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f449, plain, (spl30_1 | spl30_2 | spl30_3 | spl30_4), inference(avatar_split_clause, [], [f130, f446, f442, f438, f434])).
fof(f130, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).