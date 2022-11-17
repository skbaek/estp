fof(f2972, plain, $false, inference(avatar_sat_refutation, [], [f349, f366, f383, f400, f417, f434, f451, f468, f485, f502, f519, f536, f553, f570, f587, f604, f605, f607, f608, f610, f612, f614, f616, f617, f618, f619, f620, f621, f622, f624, f626, f627, f628, f629, f631, f632, f633, f634, f635, f636, f641, f642, f643, f644, f649, f650, f651, f652, f657, f658, f659, f660, f665, f666, f669, f674, f676, f678, f683, f686, f687, f692, f693, f696, f702, f705, f711, f712, f714, f720, f722, f723, f728, f730, f731, f732, f738, f739, f741, f748, f750, f756, f757, f758, f759, f764, f767, f768, f774, f776, f777, f784, f785, f786, f791, f794, f795, f796, f797, f798, f799, f800, f804, f813, f822, f831, f836, f841, f855, f864, f873, f874, f879, f880, f894, f903, f904, f911, f923, f924, f935, f938, f945, f947, f948, f954, f957, f961, f977, f999, f1044, f1051, f1056, f1064, f1067, f1070, f1084, f1090, f1120, f1124, f1129, f1147, f1183, f1204, f1213, f1219, f1224, f1234, f1238, f1261, f1295, f1296, f1303, f1325, f1338, f1381, f1385, f1391, f1394, f1398, f1410, f1418, f1432, f1447, f1460, f1469, f1476, f1485, f1488, f1537, f1559, f1568, f1583, f1599, f1607, f1644, f1668, f1675, f1683, f1691, f1703, f1710, f1715, f1738, f1746, f1755, f1784, f1793, f1802, f1810, f1817, f1831, f1838, f1858, f1862, f1866, f1882, f1900, f1907, f1912, f1922, f1923, f1932, f1963, f1984, f1993, f1999, f2004, f2015, f2027, f2057, f2058, f2078, f2079, f2089, f2092, f2096, f2107, f2112, f2116, f2130, f2167, f2172, f2180, f2182, f2184, f2199, f2253, f2258, f2267, f2271, f2289, f2294, f2311, f2327, f2333, f2342, f2352, f2367, f2368, f2375, f2383, f2418, f2432, f2451, f2456, f2464, f2472, f2480, f2484, f2499, f2503, f2515, f2517, f2522, f2528, f2551, f2554, f2561, f2564, f2572, f2574, f2580, f2596, f2597, f2607, f2618, f2626, f2634, f2635, f2646, f2649, f2653, f2660, f2712, f2717, f2721, f2733, f2736, f2746, f2760, f2766, f2771, f2775, f2787, f2800, f2801, f2807, f2818, f2823, f2844, f2848, f2850, f2852, f2860, f2862, f2864, f2866, f2881, f2899, f2900, f2905, f2906, f2911, f2920, f2930, f2931, f2934, f2940, f2952, f2957, f2965])).
fof(f2965, plain, (~ spl18_1 | ~ spl18_13), inference(avatar_split_clause, [], [f2963, f385, f334])).
fof(f334, plain, (spl18_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f385, plain, (spl18_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_13])])).
fof(f2963, plain, (~ (e0 = op(e3, e3)) | ~ spl18_13), inference(backward_demodulation, [], [f183, f387])).
fof(f387, plain, ((e0 = op(e3, e0)) | ~ spl18_13), inference(avatar_component_clause, [], [f385])).
fof(f183, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax3)).
fof(f2957, plain, (~ spl18_29 | ~ spl18_32), inference(avatar_contradiction_clause, [], [f2956])).
fof(f2956, plain, ($false | (~ spl18_29 | ~ spl18_32)), inference(subsumption_resolution, [], [f2955, f189])).
fof(f189, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax4)).
fof(f2955, plain, ((e0 = e3) | (~ spl18_29 | ~ spl18_32)), inference(backward_demodulation, [], [f467, f455])).
fof(f455, plain, ((e0 = op(e2, e0)) | ~ spl18_29), inference(avatar_component_clause, [], [f453])).
fof(f453, plain, (spl18_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_29])])).
fof(f467, plain, ((e3 = op(e2, e0)) | ~ spl18_32), inference(avatar_component_clause, [], [f465])).
fof(f465, plain, (spl18_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_32])])).
fof(f2952, plain, (~ spl18_20 | ~ spl18_32), inference(avatar_split_clause, [], [f2951, f465, f414])).
fof(f414, plain, (spl18_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_20])])).
fof(f2951, plain, (~ (e3 = op(e2, e3)) | ~ spl18_32), inference(backward_demodulation, [], [f177, f467])).
fof(f177, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2940, plain, (~ spl18_36 | ~ spl18_44), inference(avatar_split_clause, [], [f2936, f516, f482])).
fof(f482, plain, (spl18_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_36])])).
fof(f516, plain, (spl18_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_44])])).
fof(f2936, plain, (~ (e3 = op(e1, e3)) | ~ spl18_44), inference(backward_demodulation, [], [f173, f518])).
fof(f518, plain, ((e3 = op(e1, e1)) | ~ spl18_44), inference(avatar_component_clause, [], [f516])).
fof(f173, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2934, plain, (~ spl18_46 | ~ spl18_47), inference(avatar_contradiction_clause, [], [f2933])).
fof(f2933, plain, ($false | (~ spl18_46 | ~ spl18_47)), inference(subsumption_resolution, [], [f2932, f190])).
fof(f190, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2932, plain, ((e1 = e2) | (~ spl18_46 | ~ spl18_47)), inference(backward_demodulation, [], [f531, f527])).
fof(f527, plain, ((e1 = op(e1, e0)) | ~ spl18_46), inference(avatar_component_clause, [], [f525])).
fof(f525, plain, (spl18_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_46])])).
fof(f531, plain, ((e2 = op(e1, e0)) | ~ spl18_47), inference(avatar_component_clause, [], [f529])).
fof(f529, plain, (spl18_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_47])])).
fof(f2931, plain, (~ spl18_15 | ~ spl18_47), inference(avatar_split_clause, [], [f2928, f529, f393])).
fof(f393, plain, (spl18_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f2928, plain, (~ (e2 = op(e3, e0)) | ~ spl18_47), inference(backward_demodulation, [], [f143, f531])).
fof(f143, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2930, plain, (~ spl18_43 | ~ spl18_47), inference(avatar_split_clause, [], [f2925, f529, f512])).
fof(f512, plain, (spl18_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_43])])).
fof(f2925, plain, (~ (e2 = op(e1, e1)) | ~ spl18_47), inference(backward_demodulation, [], [f169, f531])).
fof(f169, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2920, plain, (~ spl18_1 | ~ spl18_49), inference(avatar_split_clause, [], [f2914, f538, f334])).
fof(f538, plain, (spl18_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_49])])).
fof(f2914, plain, (~ (e0 = op(e3, e3)) | ~ spl18_49), inference(backward_demodulation, [], [f159, f540])).
fof(f540, plain, ((e0 = op(e0, e3)) | ~ spl18_49), inference(avatar_component_clause, [], [f538])).
fof(f159, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2911, plain, (~ spl18_38 | ~ spl18_54), inference(avatar_split_clause, [], [f2910, f559, f491])).
fof(f491, plain, (spl18_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_38])])).
fof(f559, plain, (spl18_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_54])])).
fof(f2910, plain, (~ (e1 = op(e1, e2)) | ~ spl18_54), inference(backward_demodulation, [], [f151, f561])).
fof(f561, plain, ((e1 = op(e0, e2)) | ~ spl18_54), inference(avatar_component_clause, [], [f559])).
fof(f151, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2906, plain, (~ spl18_51 | ~ spl18_59), inference(avatar_split_clause, [], [f2903, f580, f546])).
fof(f546, plain, (spl18_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f580, plain, (spl18_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_59])])).
fof(f2903, plain, (~ (e2 = op(e0, e3)) | ~ spl18_59), inference(backward_demodulation, [], [f167, f582])).
fof(f582, plain, ((e2 = op(e0, e1)) | ~ spl18_59), inference(avatar_component_clause, [], [f580])).
fof(f167, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2905, plain, (~ spl18_43 | ~ spl18_59), inference(avatar_split_clause, [], [f2901, f580, f512])).
fof(f2901, plain, (~ (e2 = op(e1, e1)) | ~ spl18_59), inference(backward_demodulation, [], [f145, f582])).
fof(f145, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2900, plain, (~ spl18_51 | ~ spl18_64 | spl18_102), inference(avatar_split_clause, [], [f2896, f896, f601, f546])).
fof(f601, plain, (spl18_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl18_64])])).
fof(f896, plain, (spl18_102 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_102])])).
fof(f2896, plain, (~ (e2 = op(e0, e3)) | (~ spl18_64 | spl18_102)), inference(backward_demodulation, [], [f898, f603])).
fof(f603, plain, ((op(e0, e0) = e3) | ~ spl18_64), inference(avatar_component_clause, [], [f601])).
fof(f898, plain, (~ (e2 = op(e0, op(e0, e0))) | spl18_102), inference(avatar_component_clause, [], [f896])).
fof(f2899, plain, (~ spl18_60 | ~ spl18_64), inference(avatar_split_clause, [], [f2890, f601, f584])).
fof(f584, plain, (spl18_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_60])])).
fof(f2890, plain, (~ (e3 = op(e0, e1)) | ~ spl18_64), inference(backward_demodulation, [], [f163, f603])).
fof(f163, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2881, plain, (~ spl18_29 | ~ spl18_25), inference(avatar_split_clause, [], [f2676, f436, f453])).
fof(f436, plain, (spl18_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_25])])).
fof(f2676, plain, (~ (e0 = op(e2, e0)) | ~ spl18_25), inference(forward_demodulation, [], [f175, f438])).
fof(f438, plain, ((e0 = op(e2, e1)) | ~ spl18_25), inference(avatar_component_clause, [], [f436])).
fof(f175, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2866, plain, (~ spl18_31 | ~ spl18_23), inference(avatar_split_clause, [], [f2865, f427, f461])).
fof(f461, plain, (spl18_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_31])])).
fof(f427, plain, (spl18_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_23])])).
fof(f2865, plain, (~ (e2 = op(e2, e0)) | ~ spl18_23), inference(forward_demodulation, [], [f176, f429])).
fof(f429, plain, ((e2 = op(e2, e2)) | ~ spl18_23), inference(avatar_component_clause, [], [f427])).
fof(f176, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2864, plain, (~ spl18_39 | ~ spl18_23), inference(avatar_split_clause, [], [f2863, f427, f495])).
fof(f495, plain, (spl18_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_39])])).
fof(f2863, plain, (~ (e2 = op(e1, e2)) | ~ spl18_23), inference(forward_demodulation, [], [f154, f429])).
fof(f154, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2862, plain, (~ spl18_55 | ~ spl18_23), inference(avatar_split_clause, [], [f2861, f427, f563])).
fof(f563, plain, (spl18_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f2861, plain, (~ (e2 = op(e0, e2)) | ~ spl18_23), inference(forward_demodulation, [], [f152, f429])).
fof(f152, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2860, plain, (~ spl18_16 | ~ spl18_8), inference(avatar_split_clause, [], [f2859, f363, f397])).
fof(f397, plain, (spl18_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f363, plain, (spl18_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_8])])).
fof(f2859, plain, (~ (e3 = op(e3, e0)) | ~ spl18_8), inference(forward_demodulation, [], [f182, f365])).
fof(f365, plain, ((e3 = op(e3, e2)) | ~ spl18_8), inference(avatar_component_clause, [], [f363])).
fof(f182, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2852, plain, (~ spl18_4 | ~ spl18_8), inference(avatar_split_clause, [], [f2851, f363, f346])).
fof(f346, plain, (spl18_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_4])])).
fof(f2851, plain, (~ (e3 = op(e3, e3)) | ~ spl18_8), inference(backward_demodulation, [], [f186, f365])).
fof(f186, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2850, plain, (~ spl18_4 | ~ spl18_20), inference(avatar_split_clause, [], [f2849, f414, f346])).
fof(f2849, plain, (~ (e3 = op(e3, e3)) | ~ spl18_20), inference(backward_demodulation, [], [f162, f416])).
fof(f416, plain, ((e3 = op(e2, e3)) | ~ spl18_20), inference(avatar_component_clause, [], [f414])).
fof(f162, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2848, plain, (~ spl18_22 | ~ spl18_23), inference(avatar_contradiction_clause, [], [f2847])).
fof(f2847, plain, ($false | (~ spl18_22 | ~ spl18_23)), inference(subsumption_resolution, [], [f2846, f190])).
fof(f2846, plain, ((e1 = e2) | (~ spl18_22 | ~ spl18_23)), inference(forward_demodulation, [], [f429, f425])).
fof(f425, plain, ((e1 = op(e2, e2)) | ~ spl18_22), inference(avatar_component_clause, [], [f423])).
fof(f423, plain, (spl18_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_22])])).
fof(f2844, plain, (~ spl18_19 | ~ spl18_31), inference(avatar_split_clause, [], [f2842, f461, f410])).
fof(f410, plain, (spl18_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_19])])).
fof(f2842, plain, (~ (e2 = op(e2, e3)) | ~ spl18_31), inference(backward_demodulation, [], [f177, f463])).
fof(f463, plain, ((e2 = op(e2, e0)) | ~ spl18_31), inference(avatar_component_clause, [], [f461])).
fof(f2823, plain, (~ spl18_45 | ~ spl18_48), inference(avatar_contradiction_clause, [], [f2822])).
fof(f2822, plain, ($false | (~ spl18_45 | ~ spl18_48)), inference(subsumption_resolution, [], [f2821, f189])).
fof(f2821, plain, ((e0 = e3) | (~ spl18_45 | ~ spl18_48)), inference(backward_demodulation, [], [f535, f523])).
fof(f523, plain, ((e0 = op(e1, e0)) | ~ spl18_45), inference(avatar_component_clause, [], [f521])).
fof(f521, plain, (spl18_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_45])])).
fof(f535, plain, ((e3 = op(e1, e0)) | ~ spl18_48), inference(avatar_component_clause, [], [f533])).
fof(f533, plain, (spl18_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_48])])).
fof(f2818, plain, (~ spl18_19 | ~ spl18_51), inference(avatar_split_clause, [], [f2815, f546, f410])).
fof(f2815, plain, (~ (e2 = op(e2, e3)) | ~ spl18_51), inference(backward_demodulation, [], [f158, f548])).
fof(f548, plain, ((e2 = op(e0, e3)) | ~ spl18_51), inference(avatar_component_clause, [], [f546])).
fof(f158, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2807, plain, (~ spl18_56 | ~ spl18_60), inference(avatar_split_clause, [], [f2804, f584, f567])).
fof(f567, plain, (spl18_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f2804, plain, (~ (e3 = op(e0, e2)) | ~ spl18_60), inference(backward_demodulation, [], [f166, f586])).
fof(f586, plain, ((e3 = op(e0, e1)) | ~ spl18_60), inference(avatar_component_clause, [], [f584])).
fof(f166, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2801, plain, (~ spl18_50 | ~ spl18_62), inference(avatar_split_clause, [], [f2795, f593, f542])).
fof(f542, plain, (spl18_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_50])])).
fof(f593, plain, (spl18_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl18_62])])).
fof(f2795, plain, (~ (e1 = op(e0, e3)) | ~ spl18_62), inference(backward_demodulation, [], [f165, f595])).
fof(f595, plain, ((op(e0, e0) = e1) | ~ spl18_62), inference(avatar_component_clause, [], [f593])).
fof(f165, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2800, plain, (~ spl18_30 | ~ spl18_62), inference(avatar_split_clause, [], [f2792, f593, f457])).
fof(f457, plain, (spl18_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_30])])).
fof(f2792, plain, (~ (e1 = op(e2, e0)) | ~ spl18_62), inference(backward_demodulation, [], [f140, f595])).
fof(f140, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2787, plain, (~ spl18_51 | spl18_97 | ~ spl18_106), inference(avatar_split_clause, [], [f2786, f918, f870, f546])).
fof(f870, plain, (spl18_97 <=> (e2 = op(e0, op(e0, op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl18_97])])).
fof(f918, plain, (spl18_106 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_106])])).
fof(f2786, plain, (~ (e2 = op(e0, e3)) | (spl18_97 | ~ spl18_106)), inference(forward_demodulation, [], [f872, f919])).
fof(f919, plain, ((e3 = op(e0, op(e0, e0))) | ~ spl18_106), inference(avatar_component_clause, [], [f918])).
fof(f872, plain, (~ (e2 = op(e0, op(e0, op(e0, e0)))) | spl18_97), inference(avatar_component_clause, [], [f870])).
fof(f2775, plain, (~ spl18_22 | ~ spl18_38), inference(avatar_contradiction_clause, [], [f2774])).
fof(f2774, plain, ($false | (~ spl18_22 | ~ spl18_38)), inference(subsumption_resolution, [], [f2773, f493])).
fof(f493, plain, ((e1 = op(e1, e2)) | ~ spl18_38), inference(avatar_component_clause, [], [f491])).
fof(f2773, plain, (~ (e1 = op(e1, e2)) | ~ spl18_22), inference(forward_demodulation, [], [f154, f425])).
fof(f2771, plain, (~ spl18_34 | ~ spl18_38), inference(avatar_split_clause, [], [f2770, f491, f474])).
fof(f474, plain, (spl18_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_34])])).
fof(f2770, plain, (~ (e1 = op(e1, e3)) | ~ spl18_38), inference(forward_demodulation, [], [f174, f493])).
fof(f174, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2766, plain, (~ spl18_30 | ~ spl18_22), inference(avatar_split_clause, [], [f2765, f423, f457])).
fof(f2765, plain, (~ (e1 = op(e2, e0)) | ~ spl18_22), inference(forward_demodulation, [], [f176, f425])).
fof(f2760, plain, (~ spl18_6 | ~ spl18_38), inference(avatar_split_clause, [], [f2759, f491, f355])).
fof(f355, plain, (spl18_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_6])])).
fof(f2759, plain, (~ (e1 = op(e3, e2)) | ~ spl18_38), inference(forward_demodulation, [], [f155, f493])).
fof(f155, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2746, plain, (~ spl18_18 | ~ spl18_22), inference(avatar_split_clause, [], [f2738, f423, f406])).
fof(f406, plain, (spl18_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_18])])).
fof(f2738, plain, (~ (e1 = op(e2, e3)) | ~ spl18_22), inference(backward_demodulation, [], [f180, f425])).
fof(f180, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2736, plain, (~ spl18_25 | ~ spl18_28), inference(avatar_contradiction_clause, [], [f2735])).
fof(f2735, plain, ($false | (~ spl18_25 | ~ spl18_28)), inference(subsumption_resolution, [], [f2734, f189])).
fof(f2734, plain, ((e0 = e3) | (~ spl18_25 | ~ spl18_28)), inference(forward_demodulation, [], [f450, f438])).
fof(f450, plain, ((e3 = op(e2, e1)) | ~ spl18_28), inference(avatar_component_clause, [], [f448])).
fof(f448, plain, (spl18_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_28])])).
fof(f2733, plain, (~ spl18_18 | ~ spl18_30), inference(avatar_split_clause, [], [f2729, f457, f406])).
fof(f2729, plain, (~ (e1 = op(e2, e3)) | ~ spl18_30), inference(backward_demodulation, [], [f177, f459])).
fof(f459, plain, ((e1 = op(e2, e0)) | ~ spl18_30), inference(avatar_component_clause, [], [f457])).
fof(f2721, plain, (~ spl18_40 | ~ spl18_44), inference(avatar_split_clause, [], [f2719, f516, f499])).
fof(f499, plain, (spl18_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_40])])).
fof(f2719, plain, (~ (e3 = op(e1, e2)) | ~ spl18_44), inference(backward_demodulation, [], [f172, f518])).
fof(f172, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2717, plain, (~ spl18_44 | ~ spl18_48), inference(avatar_split_clause, [], [f2714, f533, f516])).
fof(f2714, plain, (~ (e3 = op(e1, e1)) | ~ spl18_48), inference(backward_demodulation, [], [f169, f535])).
fof(f2712, plain, (~ spl18_18 | ~ spl18_50), inference(avatar_split_clause, [], [f2711, f542, f406])).
fof(f2711, plain, (~ (e1 = op(e2, e3)) | ~ spl18_50), inference(backward_demodulation, [], [f158, f544])).
fof(f544, plain, ((e1 = op(e0, e3)) | ~ spl18_50), inference(avatar_component_clause, [], [f542])).
fof(f2660, plain, (~ spl18_7 | ~ spl18_23), inference(avatar_split_clause, [], [f2654, f427, f359])).
fof(f359, plain, (spl18_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_7])])).
fof(f2654, plain, (~ (e2 = op(e3, e2)) | ~ spl18_23), inference(backward_demodulation, [], [f156, f429])).
fof(f156, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2653, plain, (~ spl18_25 | ~ spl18_27), inference(avatar_contradiction_clause, [], [f2652])).
fof(f2652, plain, ($false | (~ spl18_25 | ~ spl18_27)), inference(subsumption_resolution, [], [f2651, f188])).
fof(f188, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2651, plain, ((e0 = e2) | (~ spl18_25 | ~ spl18_27)), inference(backward_demodulation, [], [f446, f438])).
fof(f446, plain, ((e2 = op(e2, e1)) | ~ spl18_27), inference(avatar_component_clause, [], [f444])).
fof(f444, plain, (spl18_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_27])])).
fof(f2649, plain, (~ spl18_23 | ~ spl18_27), inference(avatar_split_clause, [], [f2647, f444, f427])).
fof(f2647, plain, (~ (e2 = op(e2, e2)) | ~ spl18_27), inference(backward_demodulation, [], [f178, f446])).
fof(f178, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2646, plain, (~ spl18_28 | ~ spl18_32), inference(avatar_split_clause, [], [f2644, f465, f448])).
fof(f2644, plain, (~ (e3 = op(e2, e1)) | ~ spl18_32), inference(backward_demodulation, [], [f175, f467])).
fof(f2635, plain, (~ spl18_35 | ~ spl18_43), inference(avatar_split_clause, [], [f2629, f512, f478])).
fof(f478, plain, (spl18_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_35])])).
fof(f2629, plain, (~ (e2 = op(e1, e3)) | ~ spl18_43), inference(backward_demodulation, [], [f173, f514])).
fof(f514, plain, ((e2 = op(e1, e1)) | ~ spl18_43), inference(avatar_component_clause, [], [f512])).
fof(f2634, plain, (~ spl18_27 | ~ spl18_43), inference(avatar_split_clause, [], [f2627, f512, f444])).
fof(f2627, plain, (~ (e2 = op(e2, e1)) | ~ spl18_43), inference(backward_demodulation, [], [f148, f514])).
fof(f148, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2626, plain, (~ spl18_38 | ~ spl18_46), inference(avatar_split_clause, [], [f2622, f525, f491])).
fof(f2622, plain, (~ (e1 = op(e1, e2)) | ~ spl18_46), inference(backward_demodulation, [], [f170, f527])).
fof(f170, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2618, plain, (~ spl18_35 | ~ spl18_51), inference(avatar_split_clause, [], [f2616, f546, f478])).
fof(f2616, plain, (~ (e2 = op(e1, e3)) | ~ spl18_51), inference(backward_demodulation, [], [f157, f548])).
fof(f157, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2607, plain, (~ spl18_28 | ~ spl18_60), inference(avatar_split_clause, [], [f2604, f584, f448])).
fof(f2604, plain, (~ (e3 = op(e2, e1)) | ~ spl18_60), inference(backward_demodulation, [], [f146, f586])).
fof(f146, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2597, plain, (~ spl18_49 | ~ spl18_61), inference(avatar_split_clause, [], [f2590, f589, f538])).
fof(f589, plain, (spl18_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_61])])).
fof(f2590, plain, (~ (e0 = op(e0, e3)) | ~ spl18_61), inference(backward_demodulation, [], [f165, f591])).
fof(f591, plain, ((e0 = op(e0, e0)) | ~ spl18_61), inference(avatar_component_clause, [], [f589])).
fof(f2596, plain, (~ spl18_13 | ~ spl18_61), inference(avatar_split_clause, [], [f2587, f589, f385])).
fof(f2587, plain, (~ (e0 = op(e3, e0)) | ~ spl18_61), inference(backward_demodulation, [], [f141, f591])).
fof(f141, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2580, plain, (~ spl18_33 | spl18_100 | ~ spl18_104), inference(avatar_split_clause, [], [f2579, f908, f886, f470])).
fof(f470, plain, (spl18_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_33])])).
fof(f886, plain, (spl18_100 <=> (e0 = op(e1, op(e1, op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl18_100])])).
fof(f908, plain, (spl18_104 <=> (e3 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl18_104])])).
fof(f2579, plain, (~ (e0 = op(e1, e3)) | (spl18_100 | ~ spl18_104)), inference(forward_demodulation, [], [f888, f909])).
fof(f909, plain, ((e3 = op(e1, op(e1, e1))) | ~ spl18_104), inference(avatar_component_clause, [], [f908])).
fof(f888, plain, (~ (e0 = op(e1, op(e1, op(e1, e1)))) | spl18_100), inference(avatar_component_clause, [], [f886])).
fof(f2574, plain, (~ spl18_52 | ~ spl18_4), inference(avatar_split_clause, [], [f2573, f346, f550])).
fof(f550, plain, (spl18_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_52])])).
fof(f2573, plain, (~ (e3 = op(e0, e3)) | ~ spl18_4), inference(forward_demodulation, [], [f159, f348])).
fof(f348, plain, ((e3 = op(e3, e3)) | ~ spl18_4), inference(avatar_component_clause, [], [f346])).
fof(f2572, plain, (~ spl18_42 | ~ spl18_10), inference(avatar_split_clause, [], [f2571, f372, f508])).
fof(f508, plain, (spl18_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_42])])).
fof(f372, plain, (spl18_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_10])])).
fof(f2571, plain, (~ (e1 = op(e1, e1)) | ~ spl18_10), inference(forward_demodulation, [], [f149, f374])).
fof(f374, plain, ((e1 = op(e3, e1)) | ~ spl18_10), inference(avatar_component_clause, [], [f372])).
fof(f149, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2564, plain, (~ spl18_14 | ~ spl18_10), inference(avatar_split_clause, [], [f2563, f372, f389])).
fof(f389, plain, (spl18_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_14])])).
fof(f2563, plain, (~ (e1 = op(e3, e0)) | ~ spl18_10), inference(forward_demodulation, [], [f181, f374])).
fof(f181, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2561, plain, (~ spl18_6 | ~ spl18_10), inference(avatar_split_clause, [], [f2560, f372, f355])).
fof(f2560, plain, (~ (e1 = op(e3, e2)) | ~ spl18_10), inference(backward_demodulation, [], [f184, f374])).
fof(f184, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2554, plain, (~ spl18_20 | ~ spl18_28), inference(avatar_split_clause, [], [f2553, f448, f414])).
fof(f2553, plain, (~ (e3 = op(e2, e3)) | ~ spl18_28), inference(backward_demodulation, [], [f179, f450])).
fof(f179, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2551, plain, (~ spl18_27 | ~ spl18_31), inference(avatar_split_clause, [], [f2547, f461, f444])).
fof(f2547, plain, (~ (e2 = op(e2, e1)) | ~ spl18_31), inference(backward_demodulation, [], [f175, f463])).
fof(f2528, plain, (~ spl18_30 | ~ spl18_21 | spl18_92), inference(avatar_split_clause, [], [f2527, f848, f419, f457])).
fof(f419, plain, (spl18_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_21])])).
fof(f848, plain, (spl18_92 <=> (e1 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl18_92])])).
fof(f2527, plain, (~ (e1 = op(e2, e0)) | (~ spl18_21 | spl18_92)), inference(forward_demodulation, [], [f850, f421])).
fof(f421, plain, ((e0 = op(e2, e2)) | ~ spl18_21), inference(avatar_component_clause, [], [f419])).
fof(f850, plain, (~ (e1 = op(e2, op(e2, e2))) | spl18_92), inference(avatar_component_clause, [], [f848])).
fof(f2522, plain, (~ spl18_41 | ~ spl18_48 | spl18_104), inference(avatar_contradiction_clause, [], [f2521])).
fof(f2521, plain, ($false | (~ spl18_41 | ~ spl18_48 | spl18_104)), inference(subsumption_resolution, [], [f2520, f535])).
fof(f2520, plain, (~ (e3 = op(e1, e0)) | (~ spl18_41 | spl18_104)), inference(forward_demodulation, [], [f910, f506])).
fof(f506, plain, ((e0 = op(e1, e1)) | ~ spl18_41), inference(avatar_component_clause, [], [f504])).
fof(f504, plain, (spl18_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_41])])).
fof(f910, plain, (~ (e3 = op(e1, op(e1, e1))) | spl18_104), inference(avatar_component_clause, [], [f908])).
fof(f2517, plain, (~ spl18_58 | ~ spl18_62), inference(avatar_split_clause, [], [f2516, f593, f576])).
fof(f576, plain, (spl18_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_58])])).
fof(f2516, plain, (~ (e1 = op(e0, e1)) | ~ spl18_62), inference(forward_demodulation, [], [f163, f595])).
fof(f2515, plain, (~ spl18_60 | ~ spl18_62 | spl18_106), inference(avatar_split_clause, [], [f2415, f918, f593, f584])).
fof(f2415, plain, (~ (e3 = op(e0, e1)) | (~ spl18_62 | spl18_106)), inference(backward_demodulation, [], [f920, f595])).
fof(f920, plain, (~ (e3 = op(e0, op(e0, e0))) | spl18_106), inference(avatar_component_clause, [], [f918])).
fof(f2503, plain, (~ spl18_12 | ~ spl18_4), inference(avatar_split_clause, [], [f2502, f346, f380])).
fof(f380, plain, (spl18_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_12])])).
fof(f2502, plain, (~ (e3 = op(e3, e1)) | ~ spl18_4), inference(forward_demodulation, [], [f185, f348])).
fof(f185, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2499, plain, (~ spl18_9 | ~ spl18_41), inference(avatar_split_clause, [], [f2498, f504, f368])).
fof(f368, plain, (spl18_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_9])])).
fof(f2498, plain, (~ (e0 = op(e3, e1)) | ~ spl18_41), inference(forward_demodulation, [], [f149, f506])).
fof(f2484, plain, (~ spl18_13 | ~ spl18_15), inference(avatar_contradiction_clause, [], [f2483])).
fof(f2483, plain, ($false | (~ spl18_13 | ~ spl18_15)), inference(subsumption_resolution, [], [f2482, f188])).
fof(f2482, plain, ((e0 = e2) | (~ spl18_13 | ~ spl18_15)), inference(backward_demodulation, [], [f395, f387])).
fof(f395, plain, ((e2 = op(e3, e0)) | ~ spl18_15), inference(avatar_component_clause, [], [f393])).
fof(f2480, plain, (~ spl18_3 | ~ spl18_15), inference(avatar_split_clause, [], [f2479, f393, f342])).
fof(f342, plain, (spl18_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_3])])).
fof(f2479, plain, (~ (e2 = op(e3, e3)) | ~ spl18_15), inference(backward_demodulation, [], [f183, f395])).
fof(f2472, plain, (~ spl18_14 | ~ spl18_30), inference(avatar_split_clause, [], [f2465, f457, f389])).
fof(f2465, plain, (~ (e1 = op(e3, e0)) | ~ spl18_30), inference(backward_demodulation, [], [f144, f459])).
fof(f144, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2464, plain, (~ spl18_34 | ~ spl18_35), inference(avatar_contradiction_clause, [], [f2463])).
fof(f2463, plain, ($false | (~ spl18_34 | ~ spl18_35)), inference(subsumption_resolution, [], [f2462, f190])).
fof(f2462, plain, ((e1 = e2) | (~ spl18_34 | ~ spl18_35)), inference(backward_demodulation, [], [f480, f476])).
fof(f476, plain, ((e1 = op(e1, e3)) | ~ spl18_34), inference(avatar_component_clause, [], [f474])).
fof(f480, plain, ((e2 = op(e1, e3)) | ~ spl18_35), inference(avatar_component_clause, [], [f478])).
fof(f2456, plain, (~ spl18_8 | ~ spl18_40), inference(avatar_split_clause, [], [f2454, f499, f363])).
fof(f2454, plain, (~ (e3 = op(e3, e2)) | ~ spl18_40), inference(backward_demodulation, [], [f155, f501])).
fof(f501, plain, ((e3 = op(e1, e2)) | ~ spl18_40), inference(avatar_component_clause, [], [f499])).
fof(f2451, plain, (~ spl18_35 | ~ spl18_41 | ~ spl18_48 | spl18_88), inference(avatar_split_clause, [], [f2450, f828, f533, f504, f478])).
fof(f828, plain, (spl18_88 <=> (e2 = op(e1, op(e1, op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl18_88])])).
fof(f2450, plain, (~ (e2 = op(e1, e3)) | (~ spl18_41 | ~ spl18_48 | spl18_88)), inference(forward_demodulation, [], [f2447, f535])).
fof(f2447, plain, (~ (e2 = op(e1, op(e1, e0))) | (~ spl18_41 | spl18_88)), inference(backward_demodulation, [], [f830, f506])).
fof(f830, plain, (~ (e2 = op(e1, op(e1, op(e1, e1)))) | spl18_88), inference(avatar_component_clause, [], [f828])).
fof(f2432, plain, (~ spl18_39 | ~ spl18_55), inference(avatar_split_clause, [], [f2429, f563, f495])).
fof(f2429, plain, (~ (e2 = op(e1, e2)) | ~ spl18_55), inference(backward_demodulation, [], [f151, f565])).
fof(f565, plain, ((e2 = op(e0, e2)) | ~ spl18_55), inference(avatar_component_clause, [], [f563])).
fof(f2418, plain, (~ spl18_54 | ~ spl18_62), inference(avatar_split_clause, [], [f2409, f593, f559])).
fof(f2409, plain, (~ (e1 = op(e0, e2)) | ~ spl18_62), inference(backward_demodulation, [], [f164, f595])).
fof(f164, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2383, plain, (~ spl18_6 | ~ spl18_14), inference(avatar_split_clause, [], [f2380, f389, f355])).
fof(f2380, plain, (~ (e1 = op(e3, e2)) | ~ spl18_14), inference(backward_demodulation, [], [f182, f391])).
fof(f391, plain, ((e1 = op(e3, e0)) | ~ spl18_14), inference(avatar_component_clause, [], [f389])).
fof(f2375, plain, (~ spl18_17 | ~ spl18_21), inference(avatar_split_clause, [], [f2370, f419, f402])).
fof(f402, plain, (spl18_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_17])])).
fof(f2370, plain, (~ (e0 = op(e2, e3)) | ~ spl18_21), inference(backward_demodulation, [], [f180, f421])).
fof(f2368, plain, (~ spl18_24 | ~ spl18_32), inference(avatar_split_clause, [], [f2365, f465, f431])).
fof(f431, plain, (spl18_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_24])])).
fof(f2365, plain, (~ (e3 = op(e2, e2)) | ~ spl18_32), inference(backward_demodulation, [], [f176, f467])).
fof(f2367, plain, (~ spl18_16 | ~ spl18_32), inference(avatar_split_clause, [], [f2364, f465, f397])).
fof(f2364, plain, (~ (e3 = op(e3, e0)) | ~ spl18_32), inference(backward_demodulation, [], [f144, f467])).
fof(f2352, plain, (~ spl18_34 | ~ spl18_42), inference(avatar_split_clause, [], [f2344, f508, f474])).
fof(f2344, plain, (~ (e1 = op(e1, e3)) | ~ spl18_42), inference(backward_demodulation, [], [f173, f510])).
fof(f510, plain, ((e1 = op(e1, e1)) | ~ spl18_42), inference(avatar_component_clause, [], [f508])).
fof(f2342, plain, (~ spl18_37 | ~ spl18_45), inference(avatar_split_clause, [], [f2337, f521, f487])).
fof(f487, plain, (spl18_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_37])])).
fof(f2337, plain, (~ (e0 = op(e1, e2)) | ~ spl18_45), inference(backward_demodulation, [], [f170, f523])).
fof(f2333, plain, (~ spl18_17 | ~ spl18_49), inference(avatar_split_clause, [], [f2329, f538, f402])).
fof(f2329, plain, (~ (e0 = op(e2, e3)) | ~ spl18_49), inference(backward_demodulation, [], [f158, f540])).
fof(f2327, plain, (~ spl18_6 | ~ spl18_54), inference(avatar_split_clause, [], [f2320, f559, f355])).
fof(f2320, plain, (~ (e1 = op(e3, e2)) | ~ spl18_54), inference(backward_demodulation, [], [f153, f561])).
fof(f153, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2311, plain, (~ spl18_54 | ~ spl18_63 | spl18_96), inference(avatar_split_clause, [], [f2303, f866, f597, f559])).
fof(f597, plain, (spl18_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f866, plain, (spl18_96 <=> (e1 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_96])])).
fof(f2303, plain, (~ (e1 = op(e0, e2)) | (~ spl18_63 | spl18_96)), inference(backward_demodulation, [], [f868, f599])).
fof(f599, plain, ((op(e0, e0) = e2) | ~ spl18_63), inference(avatar_component_clause, [], [f597])).
fof(f868, plain, (~ (e1 = op(e0, op(e0, e0))) | spl18_96), inference(avatar_component_clause, [], [f866])).
fof(f2294, plain, (~ spl18_6 | ~ spl18_3 | spl18_94), inference(avatar_split_clause, [], [f2293, f857, f342, f355])).
fof(f857, plain, (spl18_94 <=> (e1 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_94])])).
fof(f2293, plain, (~ (e1 = op(e3, e2)) | (~ spl18_3 | spl18_94)), inference(forward_demodulation, [], [f859, f344])).
fof(f344, plain, ((e2 = op(e3, e3)) | ~ spl18_3), inference(avatar_component_clause, [], [f342])).
fof(f859, plain, (~ (e1 = op(e3, op(e3, e3))) | spl18_94), inference(avatar_component_clause, [], [f857])).
fof(f2289, plain, (~ spl18_30 | ~ spl18_83 | spl18_84), inference(avatar_split_clause, [], [f2288, f810, f806, f457])).
fof(f806, plain, (spl18_83 <=> (e0 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl18_83])])).
fof(f810, plain, (spl18_84 <=> (e1 = op(e2, op(e2, op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl18_84])])).
fof(f2288, plain, (~ (e1 = op(e2, e0)) | (~ spl18_83 | spl18_84)), inference(forward_demodulation, [], [f812, f807])).
fof(f807, plain, ((e0 = op(e2, op(e2, e2))) | ~ spl18_83), inference(avatar_component_clause, [], [f806])).
fof(f812, plain, (~ (e1 = op(e2, op(e2, op(e2, e2)))) | spl18_84), inference(avatar_component_clause, [], [f810])).
fof(f2271, plain, (~ spl18_1 | ~ spl18_9), inference(avatar_split_clause, [], [f2270, f368, f334])).
fof(f2270, plain, (~ (e0 = op(e3, e3)) | ~ spl18_9), inference(backward_demodulation, [], [f185, f370])).
fof(f370, plain, ((e0 = op(e3, e1)) | ~ spl18_9), inference(avatar_component_clause, [], [f368])).
fof(f2267, plain, (~ spl18_1 | ~ spl18_17), inference(avatar_split_clause, [], [f2263, f402, f334])).
fof(f2263, plain, (~ (e0 = op(e3, e3)) | ~ spl18_17), inference(backward_demodulation, [], [f162, f404])).
fof(f404, plain, ((e0 = op(e2, e3)) | ~ spl18_17), inference(avatar_component_clause, [], [f402])).
fof(f2258, plain, (~ spl18_11 | ~ spl18_27), inference(avatar_split_clause, [], [f2254, f444, f376])).
fof(f376, plain, (spl18_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_11])])).
fof(f2254, plain, (~ (e2 = op(e3, e1)) | ~ spl18_27), inference(backward_demodulation, [], [f150, f446])).
fof(f150, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2253, plain, (~ spl18_26 | ~ spl18_30), inference(avatar_split_clause, [], [f2248, f457, f440])).
fof(f440, plain, (spl18_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_26])])).
fof(f2248, plain, (~ (e1 = op(e2, e1)) | ~ spl18_30), inference(backward_demodulation, [], [f175, f459])).
fof(f2199, plain, (~ spl18_9 | ~ spl18_94 | spl18_95), inference(avatar_split_clause, [], [f2198, f861, f857, f368])).
fof(f861, plain, (spl18_95 <=> (e0 = op(e3, op(e3, op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_95])])).
fof(f2198, plain, (~ (e0 = op(e3, e1)) | (~ spl18_94 | spl18_95)), inference(forward_demodulation, [], [f863, f858])).
fof(f858, plain, ((e1 = op(e3, op(e3, e3))) | ~ spl18_94), inference(avatar_component_clause, [], [f857])).
fof(f863, plain, (~ (e0 = op(e3, op(e3, op(e3, e3)))) | spl18_95), inference(avatar_component_clause, [], [f861])).
fof(f2184, plain, (~ spl18_2 | ~ spl18_6), inference(avatar_split_clause, [], [f2183, f355, f338])).
fof(f338, plain, (spl18_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f2183, plain, (~ (e1 = op(e3, e3)) | ~ spl18_6), inference(forward_demodulation, [], [f186, f357])).
fof(f357, plain, ((e1 = op(e3, e2)) | ~ spl18_6), inference(avatar_component_clause, [], [f355])).
fof(f2182, plain, (~ spl18_11 | spl18_89 | ~ spl18_94), inference(avatar_split_clause, [], [f2181, f857, f833, f376])).
fof(f833, plain, (spl18_89 <=> (e2 = op(e3, op(e3, op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_89])])).
fof(f2181, plain, (~ (e2 = op(e3, e1)) | (spl18_89 | ~ spl18_94)), inference(backward_demodulation, [], [f835, f858])).
fof(f835, plain, (~ (e2 = op(e3, op(e3, op(e3, e3)))) | spl18_89), inference(avatar_component_clause, [], [f833])).
fof(f2180, plain, (~ spl18_4 | ~ spl18_16), inference(avatar_split_clause, [], [f2179, f397, f346])).
fof(f2179, plain, (~ (e3 = op(e3, e3)) | ~ spl18_16), inference(forward_demodulation, [], [f183, f399])).
fof(f399, plain, ((e3 = op(e3, e0)) | ~ spl18_16), inference(avatar_component_clause, [], [f397])).
fof(f2172, plain, (~ spl18_38 | ~ spl18_40), inference(avatar_contradiction_clause, [], [f2171])).
fof(f2171, plain, ($false | (~ spl18_38 | ~ spl18_40)), inference(subsumption_resolution, [], [f2170, f191])).
fof(f191, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2170, plain, ((e1 = e3) | (~ spl18_38 | ~ spl18_40)), inference(backward_demodulation, [], [f501, f493])).
fof(f2167, plain, (~ spl18_34 | ~ spl18_50), inference(avatar_split_clause, [], [f2164, f542, f474])).
fof(f2164, plain, (~ (e1 = op(e1, e3)) | ~ spl18_50), inference(backward_demodulation, [], [f157, f544])).
fof(f2130, plain, (~ spl18_15 | ~ spl18_1 | spl18_101), inference(avatar_split_clause, [], [f2012, f891, f334, f393])).
fof(f891, plain, (spl18_101 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_101])])).
fof(f2012, plain, (~ (e2 = op(e3, e0)) | (~ spl18_1 | spl18_101)), inference(backward_demodulation, [], [f893, f336])).
fof(f336, plain, ((e0 = op(e3, e3)) | ~ spl18_1), inference(avatar_component_clause, [], [f334])).
fof(f893, plain, (~ (e2 = op(e3, op(e3, e3))) | spl18_101), inference(avatar_component_clause, [], [f891])).
fof(f2116, plain, (~ spl18_14 | ~ spl18_16), inference(avatar_contradiction_clause, [], [f2115])).
fof(f2115, plain, ($false | (~ spl18_14 | ~ spl18_16)), inference(subsumption_resolution, [], [f2114, f191])).
fof(f2114, plain, ((e1 = e3) | (~ spl18_14 | ~ spl18_16)), inference(forward_demodulation, [], [f399, f391])).
fof(f2112, plain, (~ spl18_10 | ~ spl18_26), inference(avatar_split_clause, [], [f2108, f440, f372])).
fof(f2108, plain, (~ (e1 = op(e3, e1)) | ~ spl18_26), inference(backward_demodulation, [], [f150, f442])).
fof(f442, plain, ((e1 = op(e2, e1)) | ~ spl18_26), inference(avatar_component_clause, [], [f440])).
fof(f2107, plain, (~ spl18_18 | ~ spl18_34), inference(avatar_split_clause, [], [f2104, f474, f406])).
fof(f2104, plain, (~ (e1 = op(e2, e3)) | ~ spl18_34), inference(backward_demodulation, [], [f160, f476])).
fof(f160, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2096, plain, (~ spl18_41 | ~ spl18_43), inference(avatar_contradiction_clause, [], [f2095])).
fof(f2095, plain, ($false | (~ spl18_41 | ~ spl18_43)), inference(subsumption_resolution, [], [f2094, f188])).
fof(f2094, plain, ((e0 = e2) | (~ spl18_41 | ~ spl18_43)), inference(backward_demodulation, [], [f514, f506])).
fof(f2092, plain, (~ spl18_40 | ~ spl18_43 | spl18_104), inference(avatar_split_clause, [], [f2088, f908, f512, f499])).
fof(f2088, plain, (~ (e3 = op(e1, e2)) | (~ spl18_43 | spl18_104)), inference(backward_demodulation, [], [f910, f514])).
fof(f2089, plain, (~ spl18_11 | ~ spl18_43), inference(avatar_split_clause, [], [f2081, f512, f376])).
fof(f2081, plain, (~ (e2 = op(e3, e1)) | ~ spl18_43), inference(backward_demodulation, [], [f149, f514])).
fof(f2079, plain, (~ spl18_34 | ~ spl18_46), inference(avatar_split_clause, [], [f2075, f525, f474])).
fof(f2075, plain, (~ (e1 = op(e1, e3)) | ~ spl18_46), inference(backward_demodulation, [], [f171, f527])).
fof(f171, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2078, plain, (~ spl18_42 | ~ spl18_46), inference(avatar_split_clause, [], [f2073, f525, f508])).
fof(f2073, plain, (~ (e1 = op(e1, e1)) | ~ spl18_46), inference(backward_demodulation, [], [f169, f527])).
fof(f2058, plain, (~ spl18_57 | ~ spl18_61), inference(avatar_split_clause, [], [f2047, f589, f572])).
fof(f572, plain, (spl18_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_57])])).
fof(f2047, plain, (~ (e0 = op(e0, e1)) | ~ spl18_61), inference(backward_demodulation, [], [f163, f591])).
fof(f2057, plain, (~ spl18_45 | ~ spl18_61), inference(avatar_split_clause, [], [f2046, f589, f521])).
fof(f2046, plain, (~ (e0 = op(e1, e0)) | ~ spl18_61), inference(backward_demodulation, [], [f139, f591])).
fof(f139, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2027, plain, (~ spl18_46 | ~ spl18_14), inference(avatar_split_clause, [], [f2026, f389, f525])).
fof(f2026, plain, (~ (e1 = op(e1, e0)) | ~ spl18_14), inference(forward_demodulation, [], [f143, f391])).
fof(f2015, plain, (~ spl18_1 | ~ spl18_14 | spl18_94), inference(avatar_contradiction_clause, [], [f2014])).
fof(f2014, plain, ($false | (~ spl18_1 | ~ spl18_14 | spl18_94)), inference(subsumption_resolution, [], [f2010, f391])).
fof(f2010, plain, (~ (e1 = op(e3, e0)) | (~ spl18_1 | spl18_94)), inference(backward_demodulation, [], [f859, f336])).
fof(f2004, plain, (~ spl18_2 | ~ spl18_14), inference(avatar_split_clause, [], [f2001, f389, f338])).
fof(f2001, plain, (~ (e1 = op(e3, e3)) | ~ spl18_14), inference(backward_demodulation, [], [f183, f391])).
fof(f1999, plain, (~ spl18_2 | ~ spl18_18), inference(avatar_split_clause, [], [f1996, f406, f338])).
fof(f1996, plain, (~ (e1 = op(e3, e3)) | ~ spl18_18), inference(backward_demodulation, [], [f162, f408])).
fof(f408, plain, ((e1 = op(e2, e3)) | ~ spl18_18), inference(avatar_component_clause, [], [f406])).
fof(f1993, plain, (~ spl18_5 | ~ spl18_21), inference(avatar_split_clause, [], [f1985, f419, f351])).
fof(f351, plain, (spl18_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_5])])).
fof(f1985, plain, (~ (e0 = op(e3, e2)) | ~ spl18_21), inference(backward_demodulation, [], [f156, f421])).
fof(f1984, plain, (~ spl18_24 | ~ spl18_28), inference(avatar_split_clause, [], [f1982, f448, f431])).
fof(f1982, plain, (~ (e3 = op(e2, e2)) | ~ spl18_28), inference(backward_demodulation, [], [f178, f450])).
fof(f1963, plain, (~ spl18_38 | ~ spl18_42), inference(avatar_split_clause, [], [f1955, f508, f491])).
fof(f1955, plain, (~ (e1 = op(e1, e2)) | ~ spl18_42), inference(backward_demodulation, [], [f172, f510])).
fof(f1932, plain, (~ spl18_25 | ~ spl18_57), inference(avatar_split_clause, [], [f1926, f572, f436])).
fof(f1926, plain, (~ (e0 = op(e2, e1)) | ~ spl18_57), inference(backward_demodulation, [], [f146, f574])).
fof(f574, plain, ((e0 = op(e0, e1)) | ~ spl18_57), inference(avatar_component_clause, [], [f572])).
fof(f1923, plain, (~ spl18_52 | ~ spl18_64), inference(avatar_split_clause, [], [f1920, f601, f550])).
fof(f1920, plain, (~ (e3 = op(e0, e3)) | ~ spl18_64), inference(backward_demodulation, [], [f165, f603])).
fof(f1922, plain, (~ spl18_16 | ~ spl18_64), inference(avatar_split_clause, [], [f1917, f601, f397])).
fof(f1917, plain, (~ (e3 = op(e3, e0)) | ~ spl18_64), inference(backward_demodulation, [], [f141, f603])).
fof(f1912, plain, (~ spl18_54 | ~ spl18_102 | spl18_103), inference(avatar_split_clause, [], [f1908, f900, f896, f559])).
fof(f900, plain, (spl18_103 <=> (e1 = op(e0, op(e0, op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl18_103])])).
fof(f1908, plain, (~ (e1 = op(e0, e2)) | (~ spl18_102 | spl18_103)), inference(backward_demodulation, [], [f902, f897])).
fof(f897, plain, ((e2 = op(e0, op(e0, e0))) | ~ spl18_102), inference(avatar_component_clause, [], [f896])).
fof(f902, plain, (~ (e1 = op(e0, op(e0, op(e0, e0)))) | spl18_103), inference(avatar_component_clause, [], [f900])).
fof(f1907, plain, (~ spl18_59 | ~ spl18_11), inference(avatar_split_clause, [], [f1906, f376, f580])).
fof(f1906, plain, (~ (e2 = op(e0, e1)) | ~ spl18_11), inference(forward_demodulation, [], [f147, f378])).
fof(f378, plain, ((e2 = op(e3, e1)) | ~ spl18_11), inference(avatar_component_clause, [], [f376])).
fof(f147, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1900, plain, (~ spl18_33 | ~ spl18_44 | spl18_87), inference(avatar_contradiction_clause, [], [f1899])).
fof(f1899, plain, ($false | (~ spl18_33 | ~ spl18_44 | spl18_87)), inference(subsumption_resolution, [], [f1898, f472])).
fof(f472, plain, ((e0 = op(e1, e3)) | ~ spl18_33), inference(avatar_component_clause, [], [f470])).
fof(f1898, plain, (~ (e0 = op(e1, e3)) | (~ spl18_44 | spl18_87)), inference(forward_demodulation, [], [f826, f518])).
fof(f826, plain, (~ (e0 = op(e1, op(e1, e1))) | spl18_87), inference(avatar_component_clause, [], [f824])).
fof(f824, plain, (spl18_87 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl18_87])])).
fof(f1882, plain, (~ spl18_2 | ~ spl18_11 | spl18_101), inference(avatar_contradiction_clause, [], [f1881])).
fof(f1881, plain, ($false | (~ spl18_2 | ~ spl18_11 | spl18_101)), inference(subsumption_resolution, [], [f1880, f378])).
fof(f1880, plain, (~ (e2 = op(e3, e1)) | (~ spl18_2 | spl18_101)), inference(forward_demodulation, [], [f893, f340])).
fof(f340, plain, ((e1 = op(e3, e3)) | ~ spl18_2), inference(avatar_component_clause, [], [f338])).
fof(f1866, plain, (~ spl18_3 | ~ spl18_11), inference(avatar_split_clause, [], [f1864, f376, f342])).
fof(f1864, plain, (~ (e2 = op(e3, e3)) | ~ spl18_11), inference(backward_demodulation, [], [f185, f378])).
fof(f1862, plain, (~ spl18_12 | ~ spl18_16), inference(avatar_split_clause, [], [f1859, f397, f380])).
fof(f1859, plain, (~ (e3 = op(e3, e1)) | ~ spl18_16), inference(backward_demodulation, [], [f181, f399])).
fof(f1858, plain, (~ spl18_3 | ~ spl18_19), inference(avatar_split_clause, [], [f1854, f410, f342])).
fof(f1854, plain, (~ (e2 = op(e3, e3)) | ~ spl18_19), inference(backward_demodulation, [], [f162, f412])).
fof(f412, plain, ((e2 = op(e2, e3)) | ~ spl18_19), inference(avatar_component_clause, [], [f410])).
fof(f1838, plain, (~ spl18_17 | ~ spl18_33), inference(avatar_split_clause, [], [f1835, f470, f402])).
fof(f1835, plain, (~ (e0 = op(e2, e3)) | ~ spl18_33), inference(backward_demodulation, [], [f160, f472])).
fof(f1831, plain, (~ spl18_12 | ~ spl18_44), inference(avatar_split_clause, [], [f1828, f516, f380])).
fof(f1828, plain, (~ (e3 = op(e3, e1)) | ~ spl18_44), inference(backward_demodulation, [], [f149, f518])).
fof(f1817, plain, (~ spl18_47 | ~ spl18_87 | spl18_88), inference(avatar_split_clause, [], [f1816, f828, f824, f529])).
fof(f1816, plain, (~ (e2 = op(e1, e0)) | (~ spl18_87 | spl18_88)), inference(forward_demodulation, [], [f830, f825])).
fof(f825, plain, ((e0 = op(e1, op(e1, e1))) | ~ spl18_87), inference(avatar_component_clause, [], [f824])).
fof(f1810, plain, (~ spl18_48 | ~ spl18_87 | spl18_90), inference(avatar_split_clause, [], [f1757, f838, f824, f533])).
fof(f838, plain, (spl18_90 <=> (e3 = op(e1, op(e1, op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl18_90])])).
fof(f1757, plain, (~ (e3 = op(e1, e0)) | (~ spl18_87 | spl18_90)), inference(backward_demodulation, [], [f840, f825])).
fof(f840, plain, (~ (e3 = op(e1, op(e1, op(e1, e1)))) | spl18_90), inference(avatar_component_clause, [], [f838])).
fof(f1802, plain, (~ spl18_15 | ~ spl18_85 | spl18_89), inference(avatar_split_clause, [], [f1801, f833, f815, f393])).
fof(f815, plain, (spl18_85 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f1801, plain, (~ (e2 = op(e3, e0)) | (~ spl18_85 | spl18_89)), inference(forward_demodulation, [], [f835, f816])).
fof(f816, plain, ((e0 = op(e3, op(e3, e3))) | ~ spl18_85), inference(avatar_component_clause, [], [f815])).
fof(f1793, plain, (~ spl18_9 | ~ spl18_12), inference(avatar_contradiction_clause, [], [f1792])).
fof(f1792, plain, ($false | (~ spl18_9 | ~ spl18_12)), inference(subsumption_resolution, [], [f1791, f189])).
fof(f1791, plain, ((e0 = e3) | (~ spl18_9 | ~ spl18_12)), inference(backward_demodulation, [], [f382, f370])).
fof(f382, plain, ((e3 = op(e3, e1)) | ~ spl18_12), inference(avatar_component_clause, [], [f380])).
fof(f1784, plain, (~ spl18_37 | ~ spl18_38), inference(avatar_contradiction_clause, [], [f1783])).
fof(f1783, plain, ($false | (~ spl18_37 | ~ spl18_38)), inference(subsumption_resolution, [], [f1782, f187])).
fof(f187, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1782, plain, ((e0 = e1) | (~ spl18_37 | ~ spl18_38)), inference(backward_demodulation, [], [f493, f489])).
fof(f489, plain, ((e0 = op(e1, e2)) | ~ spl18_37), inference(avatar_component_clause, [], [f487])).
fof(f1755, plain, (~ spl18_25 | ~ spl18_18 | ~ spl18_24 | spl18_93), inference(avatar_split_clause, [], [f1754, f852, f431, f406, f436])).
fof(f852, plain, (spl18_93 <=> (e0 = op(e2, op(e2, op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl18_93])])).
fof(f1754, plain, (~ (e0 = op(e2, e1)) | (~ spl18_18 | ~ spl18_24 | spl18_93)), inference(forward_demodulation, [], [f1753, f408])).
fof(f1753, plain, (~ (e0 = op(e2, op(e2, e3))) | (~ spl18_24 | spl18_93)), inference(forward_demodulation, [], [f854, f433])).
fof(f433, plain, ((e3 = op(e2, e2)) | ~ spl18_24), inference(avatar_component_clause, [], [f431])).
fof(f854, plain, (~ (e0 = op(e2, op(e2, op(e2, e2)))) | spl18_93), inference(avatar_component_clause, [], [f852])).
fof(f1746, plain, (~ spl18_42 | ~ spl18_26), inference(avatar_split_clause, [], [f1745, f440, f508])).
fof(f1745, plain, (~ (e1 = op(e1, e1)) | ~ spl18_26), inference(forward_demodulation, [], [f148, f442])).
fof(f1738, plain, (~ spl18_17 | ~ spl18_24 | spl18_83), inference(avatar_split_clause, [], [f1477, f806, f431, f402])).
fof(f1477, plain, (~ (e0 = op(e2, e3)) | (~ spl18_24 | spl18_83)), inference(backward_demodulation, [], [f808, f433])).
fof(f808, plain, (~ (e0 = op(e2, op(e2, e2))) | spl18_83), inference(avatar_component_clause, [], [f806])).
fof(f1715, plain, (~ spl18_46 | ~ spl18_48), inference(avatar_contradiction_clause, [], [f1714])).
fof(f1714, plain, ($false | (~ spl18_46 | ~ spl18_48)), inference(subsumption_resolution, [], [f1713, f191])).
fof(f1713, plain, ((e1 = e3) | (~ spl18_46 | ~ spl18_48)), inference(backward_demodulation, [], [f535, f527])).
fof(f1710, plain, (~ spl18_18 | ~ spl18_24 | spl18_92), inference(avatar_contradiction_clause, [], [f1709])).
fof(f1709, plain, ($false | (~ spl18_18 | ~ spl18_24 | spl18_92)), inference(subsumption_resolution, [], [f1708, f408])).
fof(f1708, plain, (~ (e1 = op(e2, e3)) | (~ spl18_24 | spl18_92)), inference(forward_demodulation, [], [f850, f433])).
fof(f1703, plain, (~ spl18_3 | ~ spl18_5 | ~ spl18_14 | spl18_86), inference(avatar_contradiction_clause, [], [f1702])).
fof(f1702, plain, ($false | (~ spl18_3 | ~ spl18_5 | ~ spl18_14 | spl18_86)), inference(subsumption_resolution, [], [f1701, f391])).
fof(f1701, plain, (~ (e1 = op(e3, e0)) | (~ spl18_3 | ~ spl18_5 | spl18_86)), inference(forward_demodulation, [], [f1700, f353])).
fof(f353, plain, ((e0 = op(e3, e2)) | ~ spl18_5), inference(avatar_component_clause, [], [f351])).
fof(f1700, plain, (~ (e1 = op(e3, op(e3, e2))) | (~ spl18_3 | spl18_86)), inference(forward_demodulation, [], [f821, f344])).
fof(f821, plain, (~ (e1 = op(e3, op(e3, op(e3, e3)))) | spl18_86), inference(avatar_component_clause, [], [f819])).
fof(f819, plain, (spl18_86 <=> (e1 = op(e3, op(e3, op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_86])])).
fof(f1691, plain, (~ spl18_41 | ~ spl18_25), inference(avatar_split_clause, [], [f1510, f436, f504])).
fof(f1510, plain, (~ (e0 = op(e1, e1)) | ~ spl18_25), inference(forward_demodulation, [], [f148, f438])).
fof(f1683, plain, (~ spl18_41 | ~ spl18_33), inference(avatar_split_clause, [], [f1682, f470, f504])).
fof(f1682, plain, (~ (e0 = op(e1, e1)) | ~ spl18_33), inference(forward_demodulation, [], [f173, f472])).
fof(f1675, plain, (~ spl18_3 | ~ spl18_5 | spl18_85), inference(avatar_contradiction_clause, [], [f1674])).
fof(f1674, plain, ($false | (~ spl18_3 | ~ spl18_5 | spl18_85)), inference(subsumption_resolution, [], [f1670, f353])).
fof(f1670, plain, (~ (e0 = op(e3, e2)) | (~ spl18_3 | spl18_85)), inference(backward_demodulation, [], [f817, f344])).
fof(f817, plain, (~ (e0 = op(e3, op(e3, e3))) | spl18_85), inference(avatar_component_clause, [], [f815])).
fof(f1668, plain, (~ spl18_1 | ~ spl18_5), inference(avatar_split_clause, [], [f1667, f351, f334])).
fof(f1667, plain, (~ (e0 = op(e3, e3)) | ~ spl18_5), inference(backward_demodulation, [], [f186, f353])).
fof(f1644, plain, (~ spl18_33 | ~ spl18_34), inference(avatar_contradiction_clause, [], [f1643])).
fof(f1643, plain, ($false | (~ spl18_33 | ~ spl18_34)), inference(subsumption_resolution, [], [f1642, f187])).
fof(f1642, plain, ((e0 = e1) | (~ spl18_33 | ~ spl18_34)), inference(backward_demodulation, [], [f476, f472])).
fof(f1607, plain, (~ spl18_56 | ~ spl18_52), inference(avatar_split_clause, [], [f1606, f550, f567])).
fof(f1606, plain, (~ (e3 = op(e0, e2)) | ~ spl18_52), inference(forward_demodulation, [], [f168, f552])).
fof(f552, plain, ((e3 = op(e0, e3)) | ~ spl18_52), inference(avatar_component_clause, [], [f550])).
fof(f168, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1599, plain, (~ spl18_37 | ~ spl18_43 | spl18_87), inference(avatar_split_clause, [], [f1598, f824, f512, f487])).
fof(f1598, plain, (~ (e0 = op(e1, e2)) | (~ spl18_43 | spl18_87)), inference(forward_demodulation, [], [f826, f514])).
fof(f1583, plain, (~ spl18_8 | ~ spl18_12), inference(avatar_split_clause, [], [f1582, f380, f363])).
fof(f1582, plain, (~ (e3 = op(e3, e2)) | ~ spl18_12), inference(forward_demodulation, [], [f184, f382])).
fof(f1568, plain, (~ spl18_2 | ~ spl18_34), inference(avatar_split_clause, [], [f1564, f474, f338])).
fof(f1564, plain, (~ (e1 = op(e3, e3)) | ~ spl18_34), inference(backward_demodulation, [], [f161, f476])).
fof(f161, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1559, plain, (~ spl18_43 | ~ spl18_44), inference(avatar_contradiction_clause, [], [f1558])).
fof(f1558, plain, ($false | (~ spl18_43 | ~ spl18_44)), inference(subsumption_resolution, [], [f1557, f192])).
fof(f192, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1557, plain, ((e2 = e3) | (~ spl18_43 | ~ spl18_44)), inference(backward_demodulation, [], [f518, f514])).
fof(f1537, plain, (~ spl18_42 | ~ spl18_58), inference(avatar_split_clause, [], [f1535, f576, f508])).
fof(f1535, plain, (~ (e1 = op(e1, e1)) | ~ spl18_58), inference(backward_demodulation, [], [f145, f578])).
fof(f578, plain, ((e1 = op(e0, e1)) | ~ spl18_58), inference(avatar_component_clause, [], [f576])).
fof(f1488, plain, (~ spl18_15 | ~ spl18_16), inference(avatar_contradiction_clause, [], [f1487])).
fof(f1487, plain, ($false | (~ spl18_15 | ~ spl18_16)), inference(subsumption_resolution, [], [f1486, f192])).
fof(f1486, plain, ((e2 = e3) | (~ spl18_15 | ~ spl18_16)), inference(forward_demodulation, [], [f399, f395])).
fof(f1485, plain, (~ spl18_19 | ~ spl18_20), inference(avatar_contradiction_clause, [], [f1484])).
fof(f1484, plain, ($false | (~ spl18_19 | ~ spl18_20)), inference(subsumption_resolution, [], [f1483, f192])).
fof(f1483, plain, ((e2 = e3) | (~ spl18_19 | ~ spl18_20)), inference(forward_demodulation, [], [f416, f412])).
fof(f1476, plain, (~ spl18_21 | ~ spl18_25), inference(avatar_split_clause, [], [f1473, f436, f419])).
fof(f1473, plain, (~ (e0 = op(e2, e2)) | ~ spl18_25), inference(backward_demodulation, [], [f178, f438])).
fof(f1469, plain, (~ spl18_39 | ~ spl18_40), inference(avatar_contradiction_clause, [], [f1468])).
fof(f1468, plain, ($false | (~ spl18_39 | ~ spl18_40)), inference(subsumption_resolution, [], [f1467, f192])).
fof(f1467, plain, ((e2 = e3) | (~ spl18_39 | ~ spl18_40)), inference(backward_demodulation, [], [f501, f497])).
fof(f497, plain, ((e2 = op(e1, e2)) | ~ spl18_39), inference(avatar_component_clause, [], [f495])).
fof(f1460, plain, (~ spl18_49 | ~ spl18_50), inference(avatar_contradiction_clause, [], [f1459])).
fof(f1459, plain, ($false | (~ spl18_49 | ~ spl18_50)), inference(subsumption_resolution, [], [f1458, f187])).
fof(f1458, plain, ((e0 = e1) | (~ spl18_49 | ~ spl18_50)), inference(backward_demodulation, [], [f544, f540])).
fof(f1447, plain, (~ spl18_59 | ~ spl18_63), inference(avatar_split_clause, [], [f1442, f597, f580])).
fof(f1442, plain, (~ (e2 = op(e0, e1)) | ~ spl18_63), inference(backward_demodulation, [], [f163, f599])).
fof(f1432, plain, (~ spl18_59 | ~ spl18_96 | spl18_97), inference(avatar_split_clause, [], [f1431, f870, f866, f580])).
fof(f1431, plain, (~ (e2 = op(e0, e1)) | (~ spl18_96 | spl18_97)), inference(forward_demodulation, [], [f872, f867])).
fof(f867, plain, ((e1 = op(e0, op(e0, e0))) | ~ spl18_96), inference(avatar_component_clause, [], [f866])).
fof(f1418, plain, (~ spl18_35 | ~ spl18_19), inference(avatar_split_clause, [], [f1345, f410, f478])).
fof(f1345, plain, (~ (e2 = op(e1, e3)) | ~ spl18_19), inference(forward_demodulation, [], [f160, f412])).
fof(f1410, plain, (~ spl18_23 | ~ spl18_19), inference(avatar_split_clause, [], [f1409, f410, f427])).
fof(f1409, plain, (~ (e2 = op(e2, e2)) | ~ spl18_19), inference(forward_demodulation, [], [f180, f412])).
fof(f1398, plain, (~ spl18_2 | ~ spl18_50), inference(avatar_split_clause, [], [f1397, f542, f338])).
fof(f1397, plain, (~ (e1 = op(e3, e3)) | ~ spl18_50), inference(forward_demodulation, [], [f159, f544])).
fof(f1394, plain, (~ spl18_6 | spl18_86 | ~ spl18_101), inference(avatar_contradiction_clause, [], [f1393])).
fof(f1393, plain, ($false | (~ spl18_6 | spl18_86 | ~ spl18_101)), inference(subsumption_resolution, [], [f1392, f357])).
fof(f1392, plain, (~ (e1 = op(e3, e2)) | (spl18_86 | ~ spl18_101)), inference(backward_demodulation, [], [f821, f892])).
fof(f892, plain, ((e2 = op(e3, op(e3, e3))) | ~ spl18_101), inference(avatar_component_clause, [], [f891])).
fof(f1391, plain, (~ spl18_11 | ~ spl18_15), inference(avatar_split_clause, [], [f1388, f393, f376])).
fof(f1388, plain, (~ (e2 = op(e3, e1)) | ~ spl18_15), inference(backward_demodulation, [], [f181, f395])).
fof(f1385, plain, (~ spl18_29 | ~ spl18_30), inference(avatar_contradiction_clause, [], [f1384])).
fof(f1384, plain, ($false | (~ spl18_29 | ~ spl18_30)), inference(subsumption_resolution, [], [f1383, f187])).
fof(f1383, plain, ((e0 = e1) | (~ spl18_29 | ~ spl18_30)), inference(backward_demodulation, [], [f459, f455])).
fof(f1381, plain, (~ spl18_42 | ~ spl18_43), inference(avatar_contradiction_clause, [], [f1380])).
fof(f1380, plain, ($false | (~ spl18_42 | ~ spl18_43)), inference(subsumption_resolution, [], [f1379, f190])).
fof(f1379, plain, ((e1 = e2) | (~ spl18_42 | ~ spl18_43)), inference(forward_demodulation, [], [f514, f510])).
fof(f1338, plain, (~ spl18_29 | ~ spl18_21), inference(avatar_split_clause, [], [f1186, f419, f453])).
fof(f1186, plain, (~ (e0 = op(e2, e0)) | ~ spl18_21), inference(forward_demodulation, [], [f176, f421])).
fof(f1325, plain, (~ spl18_13 | ~ spl18_16), inference(avatar_contradiction_clause, [], [f1324])).
fof(f1324, plain, ($false | (~ spl18_13 | ~ spl18_16)), inference(subsumption_resolution, [], [f1322, f189])).
fof(f1322, plain, ((e0 = e3) | (~ spl18_13 | ~ spl18_16)), inference(backward_demodulation, [], [f399, f387])).
fof(f1303, plain, (~ spl18_49 | ~ spl18_57), inference(avatar_split_clause, [], [f1302, f572, f538])).
fof(f1302, plain, (~ (e0 = op(e0, e3)) | ~ spl18_57), inference(backward_demodulation, [], [f167, f574])).
fof(f1296, plain, (~ spl18_50 | ~ spl18_64 | spl18_96), inference(avatar_split_clause, [], [f1289, f866, f601, f542])).
fof(f1289, plain, (~ (e1 = op(e0, e3)) | (~ spl18_64 | spl18_96)), inference(backward_demodulation, [], [f868, f603])).
fof(f1295, plain, (~ spl18_56 | ~ spl18_64), inference(avatar_split_clause, [], [f1287, f601, f567])).
fof(f1287, plain, (~ (e3 = op(e0, e2)) | ~ spl18_64), inference(backward_demodulation, [], [f164, f603])).
fof(f1261, plain, (~ spl18_21 | ~ spl18_28 | ~ spl18_30 | spl18_91), inference(avatar_contradiction_clause, [], [f1260])).
fof(f1260, plain, ($false | (~ spl18_21 | ~ spl18_28 | ~ spl18_30 | spl18_91)), inference(subsumption_resolution, [], [f1259, f450])).
fof(f1259, plain, (~ (e3 = op(e2, e1)) | (~ spl18_21 | ~ spl18_30 | spl18_91)), inference(forward_demodulation, [], [f1135, f459])).
fof(f1135, plain, (~ (e3 = op(e2, op(e2, e0))) | (~ spl18_21 | spl18_91)), inference(backward_demodulation, [], [f845, f421])).
fof(f845, plain, (~ (e3 = op(e2, op(e2, op(e2, e2)))) | spl18_91), inference(avatar_component_clause, [], [f843])).
fof(f843, plain, (spl18_91 <=> (e3 = op(e2, op(e2, op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl18_91])])).
fof(f1238, plain, (~ spl18_6 | ~ spl18_8), inference(avatar_contradiction_clause, [], [f1237])).
fof(f1237, plain, ($false | (~ spl18_6 | ~ spl18_8)), inference(subsumption_resolution, [], [f1235, f191])).
fof(f1235, plain, ((e1 = e3) | (~ spl18_6 | ~ spl18_8)), inference(backward_demodulation, [], [f365, f357])).
fof(f1234, plain, (~ spl18_9 | ~ spl18_11), inference(avatar_contradiction_clause, [], [f1233])).
fof(f1233, plain, ($false | (~ spl18_9 | ~ spl18_11)), inference(subsumption_resolution, [], [f1232, f188])).
fof(f1232, plain, ((e0 = e2) | (~ spl18_9 | ~ spl18_11)), inference(backward_demodulation, [], [f378, f370])).
fof(f1224, plain, (~ spl18_30 | ~ spl18_31), inference(avatar_contradiction_clause, [], [f1223])).
fof(f1223, plain, ($false | (~ spl18_30 | ~ spl18_31)), inference(subsumption_resolution, [], [f1222, f190])).
fof(f1222, plain, ((e1 = e2) | (~ spl18_30 | ~ spl18_31)), inference(backward_demodulation, [], [f463, f459])).
fof(f1219, plain, (~ spl18_49 | ~ spl18_51), inference(avatar_contradiction_clause, [], [f1218])).
fof(f1218, plain, ($false | (~ spl18_49 | ~ spl18_51)), inference(subsumption_resolution, [], [f1217, f188])).
fof(f1217, plain, ((e0 = e2) | (~ spl18_49 | ~ spl18_51)), inference(backward_demodulation, [], [f548, f540])).
fof(f1213, plain, (~ spl18_54 | ~ spl18_60 | ~ spl18_63 | spl18_98), inference(avatar_contradiction_clause, [], [f1212])).
fof(f1212, plain, ($false | (~ spl18_54 | ~ spl18_60 | ~ spl18_63 | spl18_98)), inference(subsumption_resolution, [], [f1211, f586])).
fof(f1211, plain, (~ (e3 = op(e0, e1)) | (~ spl18_54 | ~ spl18_63 | spl18_98)), inference(forward_demodulation, [], [f1207, f561])).
fof(f1207, plain, (~ (e3 = op(e0, op(e0, e2))) | (~ spl18_63 | spl18_98)), inference(backward_demodulation, [], [f878, f599])).
fof(f878, plain, (~ (e3 = op(e0, op(e0, op(e0, e0)))) | spl18_98), inference(avatar_component_clause, [], [f876])).
fof(f876, plain, (spl18_98 <=> (e3 = op(e0, op(e0, op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl18_98])])).
fof(f1204, plain, (~ spl18_63 | ~ spl18_51), inference(avatar_split_clause, [], [f1101, f546, f597])).
fof(f1101, plain, (~ (op(e0, e0) = e2) | ~ spl18_51), inference(forward_demodulation, [], [f165, f548])).
fof(f1183, plain, (~ spl18_63 | ~ spl18_31), inference(avatar_split_clause, [], [f1182, f461, f597])).
fof(f1182, plain, (~ (op(e0, e0) = e2) | ~ spl18_31), inference(forward_demodulation, [], [f140, f463])).
fof(f1147, plain, (~ spl18_18 | ~ spl18_19), inference(avatar_contradiction_clause, [], [f1146])).
fof(f1146, plain, ($false | (~ spl18_18 | ~ spl18_19)), inference(subsumption_resolution, [], [f1145, f190])).
fof(f1145, plain, ((e1 = e2) | (~ spl18_18 | ~ spl18_19)), inference(backward_demodulation, [], [f412, f408])).
fof(f1129, plain, (~ spl18_31 | ~ spl18_32), inference(avatar_contradiction_clause, [], [f1128])).
fof(f1128, plain, ($false | (~ spl18_31 | ~ spl18_32)), inference(subsumption_resolution, [], [f1127, f192])).
fof(f1127, plain, ((e2 = e3) | (~ spl18_31 | ~ spl18_32)), inference(backward_demodulation, [], [f467, f463])).
fof(f1124, plain, (~ spl18_22 | ~ spl18_54), inference(avatar_split_clause, [], [f1122, f559, f423])).
fof(f1122, plain, (~ (e1 = op(e2, e2)) | ~ spl18_54), inference(backward_demodulation, [], [f152, f561])).
fof(f1120, plain, (~ spl18_57 | ~ spl18_60), inference(avatar_contradiction_clause, [], [f1119])).
fof(f1119, plain, ($false | (~ spl18_57 | ~ spl18_60)), inference(subsumption_resolution, [], [f1118, f189])).
fof(f1118, plain, ((e0 = e3) | (~ spl18_57 | ~ spl18_60)), inference(forward_demodulation, [], [f586, f574])).
fof(f1090, plain, (~ spl18_37 | ~ spl18_5), inference(avatar_split_clause, [], [f1089, f351, f487])).
fof(f1089, plain, (~ (e0 = op(e1, e2)) | ~ spl18_5), inference(forward_demodulation, [], [f155, f353])).
fof(f1084, plain, (~ spl18_27 | ~ spl18_19), inference(avatar_split_clause, [], [f1083, f410, f444])).
fof(f1083, plain, (~ (e2 = op(e2, e1)) | ~ spl18_19), inference(forward_demodulation, [], [f179, f412])).
fof(f1070, plain, (~ spl18_5 | ~ spl18_8), inference(avatar_contradiction_clause, [], [f1069])).
fof(f1069, plain, ($false | (~ spl18_5 | ~ spl18_8)), inference(subsumption_resolution, [], [f1068, f189])).
fof(f1068, plain, ((e0 = e3) | (~ spl18_5 | ~ spl18_8)), inference(forward_demodulation, [], [f365, f353])).
fof(f1067, plain, (~ spl18_2 | ~ spl18_5 | ~ spl18_11 | spl18_95), inference(avatar_contradiction_clause, [], [f1066])).
fof(f1066, plain, ($false | (~ spl18_2 | ~ spl18_5 | ~ spl18_11 | spl18_95)), inference(subsumption_resolution, [], [f1065, f353])).
fof(f1065, plain, (~ (e0 = op(e3, e2)) | (~ spl18_2 | ~ spl18_11 | spl18_95)), inference(forward_demodulation, [], [f1063, f378])).
fof(f1063, plain, (~ (e0 = op(e3, op(e3, e1))) | (~ spl18_2 | spl18_95)), inference(backward_demodulation, [], [f863, f340])).
fof(f1064, plain, (~ spl18_9 | ~ spl18_2 | spl18_85), inference(avatar_split_clause, [], [f1062, f815, f338, f368])).
fof(f1062, plain, (~ (e0 = op(e3, e1)) | (~ spl18_2 | spl18_85)), inference(backward_demodulation, [], [f817, f340])).
fof(f1056, plain, (~ spl18_7 | ~ spl18_8), inference(avatar_contradiction_clause, [], [f1055])).
fof(f1055, plain, ($false | (~ spl18_7 | ~ spl18_8)), inference(subsumption_resolution, [], [f1054, f192])).
fof(f1054, plain, ((e2 = e3) | (~ spl18_7 | ~ spl18_8)), inference(backward_demodulation, [], [f365, f361])).
fof(f361, plain, ((e2 = op(e3, e2)) | ~ spl18_7), inference(avatar_component_clause, [], [f359])).
fof(f1051, plain, (~ spl18_11 | ~ spl18_12), inference(avatar_contradiction_clause, [], [f1050])).
fof(f1050, plain, ($false | (~ spl18_11 | ~ spl18_12)), inference(subsumption_resolution, [], [f1049, f192])).
fof(f1049, plain, ((e2 = e3) | (~ spl18_11 | ~ spl18_12)), inference(backward_demodulation, [], [f382, f378])).
fof(f1044, plain, (~ spl18_14 | ~ spl18_15), inference(avatar_contradiction_clause, [], [f1043])).
fof(f1043, plain, ($false | (~ spl18_14 | ~ spl18_15)), inference(subsumption_resolution, [], [f1042, f190])).
fof(f1042, plain, ((e1 = e2) | (~ spl18_14 | ~ spl18_15)), inference(backward_demodulation, [], [f395, f391])).
fof(f999, plain, (~ spl18_4 | ~ spl18_36), inference(avatar_split_clause, [], [f997, f482, f346])).
fof(f997, plain, (~ (e3 = op(e3, e3)) | ~ spl18_36), inference(backward_demodulation, [], [f161, f484])).
fof(f484, plain, ((e3 = op(e1, e3)) | ~ spl18_36), inference(avatar_component_clause, [], [f482])).
fof(f977, plain, (~ spl18_41 | ~ spl18_45), inference(avatar_split_clause, [], [f972, f521, f504])).
fof(f972, plain, (~ (e0 = op(e1, e1)) | ~ spl18_45), inference(backward_demodulation, [], [f169, f523])).
fof(f961, plain, (~ spl18_36 | ~ spl18_52), inference(avatar_split_clause, [], [f958, f550, f482])).
fof(f958, plain, (~ (e3 = op(e1, e3)) | ~ spl18_52), inference(backward_demodulation, [], [f157, f552])).
fof(f957, plain, (~ spl18_49 | ~ spl18_53), inference(avatar_split_clause, [], [f953, f555, f538])).
fof(f555, plain, (spl18_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_53])])).
fof(f953, plain, (~ (e0 = op(e0, e3)) | ~ spl18_53), inference(backward_demodulation, [], [f168, f557])).
fof(f557, plain, ((e0 = op(e0, e2)) | ~ spl18_53), inference(avatar_component_clause, [], [f555])).
fof(f954, plain, (~ spl18_37 | ~ spl18_53), inference(avatar_split_clause, [], [f950, f555, f487])).
fof(f950, plain, (~ (e0 = op(e1, e2)) | ~ spl18_53), inference(backward_demodulation, [], [f151, f557])).
fof(f948, plain, (~ spl18_53 | ~ spl18_57), inference(avatar_split_clause, [], [f943, f572, f555])).
fof(f943, plain, (~ (e0 = op(e0, e2)) | ~ spl18_57), inference(backward_demodulation, [], [f166, f574])).
fof(f947, plain, (~ spl18_9 | ~ spl18_57), inference(avatar_split_clause, [], [f942, f572, f368])).
fof(f942, plain, (~ (e0 = op(e3, e1)) | ~ spl18_57), inference(backward_demodulation, [], [f147, f574])).
fof(f945, plain, (~ spl18_41 | ~ spl18_57), inference(avatar_split_clause, [], [f940, f572, f504])).
fof(f940, plain, (~ (e0 = op(e1, e1)) | ~ spl18_57), inference(backward_demodulation, [], [f145, f574])).
fof(f938, plain, (~ spl18_53 | ~ spl18_61), inference(avatar_split_clause, [], [f929, f589, f555])).
fof(f929, plain, (~ (e0 = op(e0, e2)) | ~ spl18_61), inference(backward_demodulation, [], [f164, f591])).
fof(f935, plain, (~ spl18_29 | ~ spl18_61), inference(avatar_split_clause, [], [f926, f589, f453])).
fof(f926, plain, (~ (e0 = op(e2, e0)) | ~ spl18_61), inference(backward_demodulation, [], [f140, f591])).
fof(f924, plain, (~ spl18_104 | ~ spl18_88 | ~ spl18_41), inference(avatar_split_clause, [], [f316, f504, f828, f908])).
fof(f316, plain, (~ (e0 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1)))) | ~ (e3 = op(e1, op(e1, e1)))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1)))) | ~ (e3 = op(e1, op(e1, e1)))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(e1, e1)) & (e2 = op(e1, op(e1, op(e1, e1)))) & (e3 = op(e1, op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax29)).
fof(f923, plain, (~ spl18_106 | ~ spl18_97 | ~ spl18_62), inference(avatar_split_clause, [], [f315, f593, f870, f918])).
fof(f315, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, op(e0, e0)))) | ~ (e3 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, op(e0, e0)))) | ~ (e3 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((op(e0, e0) = e1) & (e2 = op(e0, op(e0, op(e0, e0)))) & (e3 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax28)).
fof(f911, plain, (~ spl18_104 | ~ spl18_100 | ~ spl18_43), inference(avatar_split_clause, [], [f311, f512, f886, f908])).
fof(f311, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, op(e1, e1)))) | ~ (e3 = op(e1, op(e1, e1)))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, op(e1, e1)))) | ~ (e3 = op(e1, op(e1, e1)))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e2 = op(e1, e1)) & (e0 = op(e1, op(e1, op(e1, e1)))) & (e3 = op(e1, op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax24)).
fof(f904, plain, (~ spl18_101 | ~ spl18_86 | ~ spl18_1), inference(avatar_split_clause, [], [f308, f334, f819, f891])).
fof(f308, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3)))) | ~ (e2 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3)))) | ~ (e2 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e3, e3)) & (e1 = op(e3, op(e3, op(e3, e3)))) & (e2 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax21)).
fof(f903, plain, (~ spl18_102 | ~ spl18_103 | ~ spl18_64), inference(avatar_split_clause, [], [f307, f601, f900, f896])).
fof(f307, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(e0, op(e0, op(e0, e0)))) | ~ (e2 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(e0, op(e0, op(e0, e0)))) | ~ (e2 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((op(e0, e0) = e3) & (e1 = op(e0, op(e0, op(e0, e0)))) & (e2 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax20)).
fof(f894, plain, (~ spl18_101 | ~ spl18_95 | ~ spl18_2), inference(avatar_split_clause, [], [f306, f338, f861, f891])).
fof(f306, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3)))) | ~ (e2 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3)))) | ~ (e2 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e3, e3)) & (e0 = op(e3, op(e3, op(e3, e3)))) & (e2 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax19)).
fof(f880, plain, (~ spl18_92 | ~ spl18_91 | ~ spl18_21), inference(avatar_split_clause, [], [f304, f419, f843, f848])).
fof(f304, plain, (~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, op(e2, e2)))) | ~ (e1 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, op(e2, e2)))) | ~ (e1 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e0 = op(e2, e2)) & (e3 = op(e2, op(e2, op(e2, e2)))) & (e1 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax17)).
fof(f879, plain, (~ spl18_96 | ~ spl18_98 | ~ spl18_63), inference(avatar_split_clause, [], [f303, f597, f876, f866])).
fof(f303, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(e0, op(e0, op(e0, e0)))) | ~ (e1 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(e0, op(e0, op(e0, e0)))) | ~ (e1 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((op(e0, e0) = e2) & (e3 = op(e0, op(e0, op(e0, e0)))) & (e1 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax16)).
fof(f874, plain, (~ spl18_94 | ~ spl18_89 | ~ spl18_1), inference(avatar_split_clause, [], [f302, f334, f833, f857])).
fof(f302, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3)))) | ~ (e1 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3)))) | ~ (e1 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e3, e3)) & (e2 = op(e3, op(e3, op(e3, e3)))) & (e1 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax15)).
fof(f873, plain, (~ spl18_96 | ~ spl18_97 | ~ spl18_64), inference(avatar_split_clause, [], [f301, f601, f870, f866])).
fof(f301, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(e0, op(e0, op(e0, e0)))) | ~ (e1 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(e0, op(e0, op(e0, e0)))) | ~ (e1 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((op(e0, e0) = e3) & (e2 = op(e0, op(e0, op(e0, e0)))) & (e1 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax14)).
fof(f864, plain, (~ spl18_94 | ~ spl18_95 | ~ spl18_3), inference(avatar_split_clause, [], [f300, f342, f861, f857])).
fof(f300, plain, (~ (e2 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3)))) | ~ (e1 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e2 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3)))) | ~ (e1 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e2 = op(e3, e3)) & (e0 = op(e3, op(e3, op(e3, e3)))) & (e1 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax13)).
fof(f855, plain, (~ spl18_92 | ~ spl18_93 | ~ spl18_24), inference(avatar_split_clause, [], [f299, f431, f852, f848])).
fof(f299, plain, (~ (e3 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, op(e2, e2)))) | ~ (e1 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e3 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, op(e2, e2)))) | ~ (e1 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e3 = op(e2, e2)) & (e0 = op(e2, op(e2, op(e2, e2)))) & (e1 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax12)).
fof(f841, plain, (~ spl18_87 | ~ spl18_90 | ~ spl18_43), inference(avatar_split_clause, [], [f297, f512, f838, f824])).
fof(f297, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, op(e1, e1)))) | ~ (e0 = op(e1, op(e1, e1)))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, op(e1, e1)))) | ~ (e0 = op(e1, op(e1, e1)))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e2 = op(e1, e1)) & (e3 = op(e1, op(e1, op(e1, e1)))) & (e0 = op(e1, op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax10)).
fof(f836, plain, (~ spl18_85 | ~ spl18_89 | ~ spl18_2), inference(avatar_split_clause, [], [f296, f338, f833, f815])).
fof(f296, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3)))) | ~ (e0 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3)))) | ~ (e0 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(e3, e3)) & (e2 = op(e3, op(e3, op(e3, e3)))) & (e0 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax9)).
fof(f831, plain, (~ spl18_87 | ~ spl18_88 | ~ spl18_44), inference(avatar_split_clause, [], [f295, f516, f828, f824])).
fof(f295, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1)))) | ~ (e0 = op(e1, op(e1, e1)))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1)))) | ~ (e0 = op(e1, op(e1, e1)))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e3 = op(e1, e1)) & (e2 = op(e1, op(e1, op(e1, e1)))) & (e0 = op(e1, op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax8)).
fof(f822, plain, (~ spl18_85 | ~ spl18_86 | ~ spl18_3), inference(avatar_split_clause, [], [f294, f342, f819, f815])).
fof(f294, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3)))) | ~ (e0 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3)))) | ~ (e0 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e2 = op(e3, e3)) & (e1 = op(e3, op(e3, op(e3, e3)))) & (e0 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax7)).
fof(f813, plain, (~ spl18_83 | ~ spl18_84 | ~ spl18_24), inference(avatar_split_clause, [], [f293, f431, f810, f806])).
fof(f293, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(e2, op(e2, op(e2, e2)))) | ~ (e0 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(e2, op(e2, op(e2, e2)))) | ~ (e0 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e2, e2)) & (e1 = op(e2, op(e2, op(e2, e2)))) & (e0 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax6)).
fof(f804, plain, (spl18_82 | spl18_81 | spl18_80 | spl18_79 | spl18_78 | spl18_77 | spl18_76 | spl18_75 | spl18_74 | spl18_73 | spl18_72 | spl18_71 | spl18_70 | spl18_69 | spl18_68 | spl18_4), inference(avatar_split_clause, [], [f283, f346, f662, f671, f680, f689, f698, f707, f716, f725, f734, f743, f752, f761, f770, f779, f788])).
fof(f788, plain, (spl18_82 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl18_82])])).
fof(f779, plain, (spl18_81 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl18_81])])).
fof(f770, plain, (spl18_80 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl18_80])])).
fof(f761, plain, (spl18_79 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl18_79])])).
fof(f752, plain, (spl18_78 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl18_78])])).
fof(f743, plain, (spl18_77 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl18_77])])).
fof(f734, plain, (spl18_76 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl18_76])])).
fof(f725, plain, (spl18_75 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl18_75])])).
fof(f716, plain, (spl18_74 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl18_74])])).
fof(f707, plain, (spl18_73 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f698, plain, (spl18_72 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl18_72])])).
fof(f689, plain, (spl18_71 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl18_71])])).
fof(f680, plain, (spl18_70 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl18_70])])).
fof(f671, plain, (spl18_69 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl18_69])])).
fof(f662, plain, (spl18_68 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl18_68])])).
fof(f283, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f72])).
fof(f72, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | sP17 | sP16 | sP15) & (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f5, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e1, e0))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e1, e2))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e1, e3))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e2, e0))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e2, e1))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e2, e3))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e3, e0))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e3, e1))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e3, e2))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f69, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP15), inference(usedef, [], [e69])).
fof(e69, plain, (sP15 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f70, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP16), inference(usedef, [], [e70])).
fof(e70, plain, (sP16 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f71, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP17), inference(usedef, [], [e71])).
fof(e71, plain, (sP17 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f5, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)))) & (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e3, e2))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e3, e1))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e3, e0))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e2, e3))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e2, e1))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e2, e0))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e1, e3))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e1, e2))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e1, e0))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e0, e3))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e0, e2))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e0, e1))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax5)).
fof(f800, plain, (spl18_82 | spl18_81 | spl18_80 | spl18_79 | spl18_78 | spl18_77 | spl18_76 | spl18_75 | spl18_74 | spl18_73 | spl18_72 | spl18_71 | spl18_70 | spl18_69 | spl18_68 | ~ spl18_4), inference(avatar_split_clause, [], [f317, f346, f662, f671, f680, f689, f698, f707, f716, f725, f734, f743, f752, f761, f770, f779, f788])).
fof(f317, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(duplicate_literal_removal, [], [f287])).
fof(f287, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f72])).
fof(f799, plain, (spl18_67 | spl18_66 | spl18_65 | spl18_4), inference(avatar_split_clause, [], [f288, f346, f638, f646, f654])).
fof(f654, plain, (spl18_67 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl18_67])])).
fof(f646, plain, (spl18_66 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl18_66])])).
fof(f638, plain, (spl18_65 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl18_65])])).
fof(f288, plain, ((e3 = op(e3, e3)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f798, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_64 | spl18_49), inference(avatar_split_clause, [], [f289, f538, f601, f638, f646, f654])).
fof(f289, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f797, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_44 | spl18_34), inference(avatar_split_clause, [], [f290, f474, f516, f638, f646, f654])).
fof(f290, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f796, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_24 | spl18_19), inference(avatar_split_clause, [], [f291, f410, f431, f638, f646, f654])).
fof(f291, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f795, plain, (~ spl18_82 | spl18_61), inference(avatar_split_clause, [], [f278, f589, f788])).
fof(f278, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f90])).
fof(f90, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f794, plain, (~ spl18_82 | ~ spl18_61), inference(avatar_split_clause, [], [f318, f589, f788])).
fof(f318, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(duplicate_literal_removal, [], [f279])).
fof(f279, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f90])).
fof(f791, plain, (~ spl18_82 | ~ spl18_1 | ~ spl18_16), inference(avatar_split_clause, [], [f282, f397, f334, f788])).
fof(f282, plain, (~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f90])).
fof(f786, plain, (~ spl18_81 | spl18_57), inference(avatar_split_clause, [], [f273, f572, f779])).
fof(f273, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f785, plain, (~ spl18_81 | ~ spl18_62 | ~ spl18_57), inference(avatar_split_clause, [], [f274, f572, f593, f779])).
fof(f274, plain, (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f89])).
fof(f784, plain, (~ spl18_81 | ~ spl18_42), inference(avatar_split_clause, [], [f319, f508, f779])).
fof(f319, plain, (~ (e1 = op(e1, e1)) | ~ sP1), inference(duplicate_literal_removal, [], [f275])).
fof(f275, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f89])).
fof(f777, plain, (~ spl18_80 | spl18_53), inference(avatar_split_clause, [], [f268, f555, f770])).
fof(f268, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f88])).
fof(f88, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f776, plain, (~ spl18_80 | ~ spl18_63 | ~ spl18_53), inference(avatar_split_clause, [], [f269, f555, f597, f770])).
fof(f269, plain, (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f88])).
fof(f774, plain, (~ spl18_80 | ~ spl18_23), inference(avatar_split_clause, [], [f320, f427, f770])).
fof(f320, plain, (~ (e2 = op(e2, e2)) | ~ sP2), inference(duplicate_literal_removal, [], [f271])).
fof(f271, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f88])).
fof(f768, plain, (~ spl18_79 | spl18_49), inference(avatar_split_clause, [], [f263, f538, f761])).
fof(f263, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f767, plain, (~ spl18_79 | ~ spl18_64 | ~ spl18_49), inference(avatar_split_clause, [], [f264, f538, f601, f761])).
fof(f264, plain, (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f87])).
fof(f764, plain, (~ spl18_79 | ~ spl18_4), inference(avatar_split_clause, [], [f321, f346, f761])).
fof(f321, plain, (~ (e3 = op(e3, e3)) | ~ sP3), inference(duplicate_literal_removal, [], [f267])).
fof(f267, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f87])).
fof(f759, plain, (~ spl18_78 | spl18_46), inference(avatar_split_clause, [], [f258, f525, f752])).
fof(f258, plain, ((e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e1, e0))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f758, plain, (~ spl18_78 | ~ spl18_61), inference(avatar_split_clause, [], [f322, f589, f752])).
fof(f322, plain, (~ (e0 = op(e0, e0)) | ~ sP4), inference(duplicate_literal_removal, [], [f259])).
fof(f259, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f757, plain, (~ spl18_78 | ~ spl18_41 | ~ spl18_46), inference(avatar_split_clause, [], [f260, f525, f504, f752])).
fof(f260, plain, (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f756, plain, (~ spl18_78 | ~ spl18_21 | ~ spl18_31), inference(avatar_split_clause, [], [f261, f461, f419, f752])).
fof(f261, plain, (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f750, plain, (~ spl18_77 | spl18_42), inference(avatar_split_clause, [], [f253, f508, f743])).
fof(f253, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f748, plain, (~ spl18_77 | ~ spl18_42), inference(avatar_split_clause, [], [f323, f508, f743])).
fof(f323, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(duplicate_literal_removal, [], [f255])).
fof(f255, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f85])).
fof(f741, plain, (~ spl18_76 | spl18_38), inference(avatar_split_clause, [], [f248, f491, f734])).
fof(f248, plain, ((e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e1, e2))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f739, plain, (~ spl18_76 | ~ spl18_43 | ~ spl18_38), inference(avatar_split_clause, [], [f250, f491, f512, f734])).
fof(f250, plain, (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f84])).
fof(f738, plain, (~ spl18_76 | ~ spl18_23), inference(avatar_split_clause, [], [f324, f427, f734])).
fof(f324, plain, (~ (e2 = op(e2, e2)) | ~ sP6), inference(duplicate_literal_removal, [], [f251])).
fof(f251, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP6), inference(cnf_transformation, [], [f84])).
fof(f732, plain, (~ spl18_75 | spl18_34), inference(avatar_split_clause, [], [f243, f474, f725])).
fof(f243, plain, ((e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f83, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e1, e3))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f731, plain, (~ spl18_75 | ~ spl18_64 | ~ spl18_49), inference(avatar_split_clause, [], [f244, f538, f601, f725])).
fof(f244, plain, (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f730, plain, (~ spl18_75 | ~ spl18_44 | ~ spl18_34), inference(avatar_split_clause, [], [f245, f474, f516, f725])).
fof(f245, plain, (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f728, plain, (~ spl18_75 | ~ spl18_4), inference(avatar_split_clause, [], [f325, f346, f725])).
fof(f325, plain, (~ (e3 = op(e3, e3)) | ~ sP7), inference(duplicate_literal_removal, [], [f247])).
fof(f247, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f723, plain, (~ spl18_74 | spl18_31), inference(avatar_split_clause, [], [f238, f461, f716])).
fof(f238, plain, ((e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f82, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e2, e0))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f722, plain, (~ spl18_74 | ~ spl18_61), inference(avatar_split_clause, [], [f326, f589, f716])).
fof(f326, plain, (~ (e0 = op(e0, e0)) | ~ sP8), inference(duplicate_literal_removal, [], [f239])).
fof(f239, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f720, plain, (~ spl18_74 | ~ spl18_21 | ~ spl18_31), inference(avatar_split_clause, [], [f241, f461, f419, f716])).
fof(f241, plain, (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f714, plain, (~ spl18_73 | spl18_27), inference(avatar_split_clause, [], [f233, f444, f707])).
fof(f233, plain, ((e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f81, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e2, e1))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f712, plain, (~ spl18_73 | ~ spl18_42), inference(avatar_split_clause, [], [f327, f508, f707])).
fof(f327, plain, (~ (e1 = op(e1, e1)) | ~ sP9), inference(duplicate_literal_removal, [], [f235])).
fof(f235, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f711, plain, (~ spl18_73 | ~ spl18_22 | ~ spl18_27), inference(avatar_split_clause, [], [f236, f444, f423, f707])).
fof(f236, plain, (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f705, plain, (~ spl18_72 | spl18_23), inference(avatar_split_clause, [], [f228, f427, f698])).
fof(f228, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f80])).
fof(f80, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f702, plain, (~ spl18_72 | ~ spl18_23), inference(avatar_split_clause, [], [f328, f427, f698])).
fof(f328, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(duplicate_literal_removal, [], [f231])).
fof(f231, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f80])).
fof(f696, plain, (~ spl18_71 | spl18_19), inference(avatar_split_clause, [], [f223, f410, f689])).
fof(f223, plain, ((e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f79, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e2, e3))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f693, plain, (~ spl18_71 | ~ spl18_24 | ~ spl18_19), inference(avatar_split_clause, [], [f226, f410, f431, f689])).
fof(f226, plain, (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f692, plain, (~ spl18_71 | ~ spl18_4), inference(avatar_split_clause, [], [f329, f346, f689])).
fof(f329, plain, (~ (e3 = op(e3, e3)) | ~ sP11), inference(duplicate_literal_removal, [], [f227])).
fof(f227, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f687, plain, (~ spl18_70 | spl18_16), inference(avatar_split_clause, [], [f218, f397, f680])).
fof(f218, plain, ((e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e3, e0))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f686, plain, (~ spl18_70 | ~ spl18_61), inference(avatar_split_clause, [], [f330, f589, f680])).
fof(f330, plain, (~ (e0 = op(e0, e0)) | ~ sP12), inference(duplicate_literal_removal, [], [f219])).
fof(f219, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP12), inference(cnf_transformation, [], [f78])).
fof(f683, plain, (~ spl18_70 | ~ spl18_1 | ~ spl18_16), inference(avatar_split_clause, [], [f222, f397, f334, f680])).
fof(f222, plain, (~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f78])).
fof(f678, plain, (~ spl18_69 | spl18_12), inference(avatar_split_clause, [], [f213, f380, f671])).
fof(f213, plain, ((e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e3, e1))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f676, plain, (~ spl18_69 | ~ spl18_42), inference(avatar_split_clause, [], [f331, f508, f671])).
fof(f331, plain, (~ (e1 = op(e1, e1)) | ~ sP13), inference(duplicate_literal_removal, [], [f215])).
fof(f215, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP13), inference(cnf_transformation, [], [f77])).
fof(f674, plain, (~ spl18_69 | ~ spl18_2 | ~ spl18_12), inference(avatar_split_clause, [], [f217, f380, f338, f671])).
fof(f217, plain, (~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f77])).
fof(f669, plain, (~ spl18_68 | spl18_8), inference(avatar_split_clause, [], [f208, f363, f662])).
fof(f208, plain, ((e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e3, e2))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f666, plain, (~ spl18_68 | ~ spl18_23), inference(avatar_split_clause, [], [f332, f427, f662])).
fof(f332, plain, (~ (e2 = op(e2, e2)) | ~ sP14), inference(duplicate_literal_removal, [], [f211])).
fof(f211, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f76])).
fof(f665, plain, (~ spl18_68 | ~ spl18_3 | ~ spl18_8), inference(avatar_split_clause, [], [f212, f363, f342, f662])).
fof(f212, plain, (~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f76])).
fof(f660, plain, (~ spl18_67 | spl18_61), inference(avatar_split_clause, [], [f203, f589, f654])).
fof(f203, plain, ((e0 = op(e0, e0)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP15), inference(nnf_transformation, [], [f69])).
fof(f659, plain, (~ spl18_67 | ~ spl18_41 | spl18_46), inference(avatar_split_clause, [], [f205, f525, f504, f654])).
fof(f205, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f658, plain, (~ spl18_67 | ~ spl18_21 | spl18_31), inference(avatar_split_clause, [], [f206, f461, f419, f654])).
fof(f206, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f657, plain, (~ spl18_67 | ~ spl18_1 | spl18_16), inference(avatar_split_clause, [], [f207, f397, f334, f654])).
fof(f207, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f652, plain, (~ spl18_66 | spl18_42), inference(avatar_split_clause, [], [f198, f508, f646])).
fof(f198, plain, ((e1 = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP16), inference(nnf_transformation, [], [f70])).
fof(f651, plain, (~ spl18_66 | ~ spl18_62 | spl18_57), inference(avatar_split_clause, [], [f199, f572, f593, f646])).
fof(f199, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f650, plain, (~ spl18_66 | ~ spl18_22 | spl18_27), inference(avatar_split_clause, [], [f201, f444, f423, f646])).
fof(f201, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f649, plain, (~ spl18_66 | ~ spl18_2 | spl18_12), inference(avatar_split_clause, [], [f202, f380, f338, f646])).
fof(f202, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f644, plain, (~ spl18_65 | spl18_23), inference(avatar_split_clause, [], [f193, f427, f638])).
fof(f193, plain, ((e2 = op(e2, e2)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP17), inference(nnf_transformation, [], [f71])).
fof(f643, plain, (~ spl18_65 | ~ spl18_63 | spl18_53), inference(avatar_split_clause, [], [f194, f555, f597, f638])).
fof(f194, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f642, plain, (~ spl18_65 | ~ spl18_43 | spl18_38), inference(avatar_split_clause, [], [f195, f491, f512, f638])).
fof(f195, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f641, plain, (~ spl18_65 | ~ spl18_3 | spl18_8), inference(avatar_split_clause, [], [f197, f363, f342, f638])).
fof(f197, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f636, plain, (spl18_61 | spl18_57 | spl18_53 | spl18_49), inference(avatar_split_clause, [], [f107, f538, f555, f572, f589])).
fof(f107, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax2)).
fof(f635, plain, (spl18_61 | spl18_45 | spl18_29 | spl18_13), inference(avatar_split_clause, [], [f108, f385, f453, f521, f589])).
fof(f108, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f634, plain, (spl18_62 | spl18_58 | spl18_54 | spl18_50), inference(avatar_split_clause, [], [f109, f542, f559, f576, f593])).
fof(f109, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f633, plain, (spl18_62 | spl18_46 | spl18_30 | spl18_14), inference(avatar_split_clause, [], [f110, f389, f457, f525, f593])).
fof(f110, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f632, plain, (spl18_63 | spl18_59 | spl18_55 | spl18_51), inference(avatar_split_clause, [], [f111, f546, f563, f580, f597])).
fof(f111, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f631, plain, (spl18_63 | spl18_47 | spl18_31 | spl18_15), inference(avatar_split_clause, [], [f112, f393, f461, f529, f597])).
fof(f112, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f629, plain, (spl18_64 | spl18_48 | spl18_32 | spl18_16), inference(avatar_split_clause, [], [f114, f397, f465, f533, f601])).
fof(f114, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f628, plain, (spl18_45 | spl18_41 | spl18_37 | spl18_33), inference(avatar_split_clause, [], [f115, f470, f487, f504, f521])).
fof(f115, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f627, plain, (spl18_57 | spl18_41 | spl18_25 | spl18_9), inference(avatar_split_clause, [], [f116, f368, f436, f504, f572])).
fof(f116, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f626, plain, (spl18_46 | spl18_42 | spl18_38 | spl18_34), inference(avatar_split_clause, [], [f117, f474, f491, f508, f525])).
fof(f117, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f624, plain, (spl18_47 | spl18_43 | spl18_39 | spl18_35), inference(avatar_split_clause, [], [f119, f478, f495, f512, f529])).
fof(f119, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f622, plain, (spl18_48 | spl18_44 | spl18_40 | spl18_36), inference(avatar_split_clause, [], [f121, f482, f499, f516, f533])).
fof(f121, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f621, plain, (spl18_60 | spl18_44 | spl18_28 | spl18_12), inference(avatar_split_clause, [], [f122, f380, f448, f516, f584])).
fof(f122, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f620, plain, (spl18_29 | spl18_25 | spl18_21 | spl18_17), inference(avatar_split_clause, [], [f123, f402, f419, f436, f453])).
fof(f123, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f619, plain, (spl18_53 | spl18_37 | spl18_21 | spl18_5), inference(avatar_split_clause, [], [f124, f351, f419, f487, f555])).
fof(f124, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f618, plain, (spl18_30 | spl18_26 | spl18_22 | spl18_18), inference(avatar_split_clause, [], [f125, f406, f423, f440, f457])).
fof(f125, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f617, plain, (spl18_54 | spl18_38 | spl18_22 | spl18_6), inference(avatar_split_clause, [], [f126, f355, f423, f491, f559])).
fof(f126, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f616, plain, (spl18_31 | spl18_27 | spl18_23 | spl18_19), inference(avatar_split_clause, [], [f127, f410, f427, f444, f461])).
fof(f127, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f614, plain, (spl18_32 | spl18_28 | spl18_24 | spl18_20), inference(avatar_split_clause, [], [f129, f414, f431, f448, f465])).
fof(f129, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f612, plain, (spl18_13 | spl18_9 | spl18_5 | spl18_1), inference(avatar_split_clause, [], [f131, f334, f351, f368, f385])).
fof(f131, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f610, plain, (spl18_14 | spl18_10 | spl18_6 | spl18_2), inference(avatar_split_clause, [], [f133, f338, f355, f372, f389])).
fof(f133, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f608, plain, (spl18_15 | spl18_11 | spl18_7 | spl18_3), inference(avatar_split_clause, [], [f135, f342, f359, f376, f393])).
fof(f135, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f607, plain, (spl18_51 | spl18_35 | spl18_19 | spl18_3), inference(avatar_split_clause, [], [f136, f342, f410, f478, f546])).
fof(f136, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f605, plain, (spl18_52 | spl18_36 | spl18_20 | spl18_4), inference(avatar_split_clause, [], [f138, f346, f414, f482, f550])).
fof(f138, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f604, plain, (spl18_61 | spl18_62 | spl18_63 | spl18_64), inference(avatar_split_clause, [], [f91, f601, f597, f593, f589])).
fof(f91, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG152+1.p', ax1)).
fof(f587, plain, (spl18_57 | spl18_58 | spl18_59 | spl18_60), inference(avatar_split_clause, [], [f92, f584, f580, f576, f572])).
fof(f92, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f570, plain, (spl18_53 | spl18_54 | spl18_55 | spl18_56), inference(avatar_split_clause, [], [f93, f567, f563, f559, f555])).
fof(f93, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f553, plain, (spl18_49 | spl18_50 | spl18_51 | spl18_52), inference(avatar_split_clause, [], [f94, f550, f546, f542, f538])).
fof(f94, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f536, plain, (spl18_45 | spl18_46 | spl18_47 | spl18_48), inference(avatar_split_clause, [], [f95, f533, f529, f525, f521])).
fof(f95, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f519, plain, (spl18_41 | spl18_42 | spl18_43 | spl18_44), inference(avatar_split_clause, [], [f96, f516, f512, f508, f504])).
fof(f96, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f502, plain, (spl18_37 | spl18_38 | spl18_39 | spl18_40), inference(avatar_split_clause, [], [f97, f499, f495, f491, f487])).
fof(f97, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f485, plain, (spl18_33 | spl18_34 | spl18_35 | spl18_36), inference(avatar_split_clause, [], [f98, f482, f478, f474, f470])).
fof(f98, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f468, plain, (spl18_29 | spl18_30 | spl18_31 | spl18_32), inference(avatar_split_clause, [], [f99, f465, f461, f457, f453])).
fof(f99, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f451, plain, (spl18_25 | spl18_26 | spl18_27 | spl18_28), inference(avatar_split_clause, [], [f100, f448, f444, f440, f436])).
fof(f100, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f434, plain, (spl18_21 | spl18_22 | spl18_23 | spl18_24), inference(avatar_split_clause, [], [f101, f431, f427, f423, f419])).
fof(f101, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f417, plain, (spl18_17 | spl18_18 | spl18_19 | spl18_20), inference(avatar_split_clause, [], [f102, f414, f410, f406, f402])).
fof(f102, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f400, plain, (spl18_13 | spl18_14 | spl18_15 | spl18_16), inference(avatar_split_clause, [], [f103, f397, f393, f389, f385])).
fof(f103, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f383, plain, (spl18_9 | spl18_10 | spl18_11 | spl18_12), inference(avatar_split_clause, [], [f104, f380, f376, f372, f368])).
fof(f104, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f366, plain, (spl18_5 | spl18_6 | spl18_7 | spl18_8), inference(avatar_split_clause, [], [f105, f363, f359, f355, f351])).
fof(f105, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f349, plain, (spl18_1 | spl18_2 | spl18_3 | spl18_4), inference(avatar_split_clause, [], [f106, f346, f342, f338, f334])).
fof(f106, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).