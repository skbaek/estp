fof(f3111, plain, $false, inference(avatar_sat_refutation, [], [f274, f291, f308, f325, f342, f359, f376, f393, f410, f427, f444, f461, f478, f495, f512, f529, f530, f531, f532, f533, f534, f535, f536, f537, f538, f539, f540, f541, f542, f543, f544, f545, f546, f547, f548, f549, f550, f551, f552, f553, f554, f555, f556, f557, f558, f559, f560, f561, f566, f567, f568, f569, f571, f572, f573, f574, f575, f576, f577, f582, f583, f584, f585, f586, f587, f588, f589, f590, f591, f592, f593, f598, f599, f600, f601, f602, f603, f604, f605, f606, f607, f608, f609, f614, f615, f616, f618, f619, f621, f622, f623, f624, f625, f626, f627, f628, f629, f630, f639, f644, f653, f654, f659, f660, f669, f674, f683, f684, f689, f690, f699, f704, f713, f714, f719, f720, f729, f734, f743, f744, f749, f750, f761, f762, f765, f766, f767, f775, f776, f785, f803, f806, f816, f819, f826, f838, f844, f846, f850, f851, f868, f871, f885, f896, f899, f909, f911, f914, f916, f929, f931, f941, f976, f1043, f1050, f1059, f1063, f1066, f1073, f1083, f1110, f1114, f1125, f1126, f1163, f1181, f1184, f1188, f1200, f1203, f1249, f1259, f1284, f1300, f1309, f1314, f1322, f1354, f1364, f1378, f1403, f1406, f1413, f1419, f1456, f1485, f1487, f1530, f1534, f1539, f1574, f1578, f1596, f1614, f1623, f1631, f1663, f1678, f1687, f1730, f1736, f1769, f1770, f1813, f1871, f1873, f1876, f1880, f1882, f1891, f1904, f1905, f1907, f1933, f1942, f1949, f1953, f1954, f2006, f2013, f2036, f2048, f2051, f2053, f2070, f2076, f2079, f2092, f2113, f2128, f2131, f2151, f2162, f2167, f2172, f2181, f2182, f2211, f2220, f2222, f2228, f2247, f2277, f2285, f2289, f2292, f2302, f2310, f2313, f2316, f2324, f2332, f2364, f2365, f2370, f2375, f2376, f2397, f2403, f2417, f2425, f2432, f2459, f2460, f2470, f2472, f2480, f2483, f2492, f2495, f2503, f2508, f2514, f2518, f2524, f2535, f2546, f2557, f2559, f2568, f2578, f2588, f2598, f2608, f2616, f2641, f2681, f2700, f2701, f2709, f2710, f2723, f2759, f2769, f2775, f2787, f2790, f2798, f2806, f2814, f2815, f2821, f2826, f2827, f2833, f2840, f2852, f2855, f2859, f2866, f2870, f2873, f2878, f2879, f2883, f2888, f2889, f2890, f2896, f2904, f2912, f2913, f2924, f2931, f2937, f2939, f2949, f2958, f2968, f2969, f2977, f2978, f2984, f2985, f2989, f2993, f3002, f3004, f3019, f3028, f3031, f3033, f3036, f3045, f3047, f3052, f3059, f3060, f3067, f3073, f3078, f3079, f3085, f3092, f3101, f3110])).
fof(f3110, plain, (spl4_1 | ~ spl4_3 | ~ spl4_20 | ~ spl4_87), inference(avatar_split_clause, [], [f3109, f722, f339, f267, f259])).
fof(f259, plain, (spl4_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_1])])).
fof(f267, plain, (spl4_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_3])])).
fof(f339, plain, (spl4_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_20])])).
fof(f722, plain, (spl4_87 <=> (e0 = op(op(op(e3, e3), e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_87])])).
fof(f3109, plain, ((e0 = op(e3, e3)) | (~ spl4_3 | ~ spl4_20 | ~ spl4_87)), inference(forward_demodulation, [], [f3106, f341])).
fof(f341, plain, ((e3 = op(e2, e3)) | ~ spl4_20), inference(avatar_component_clause, [], [f339])).
fof(f3106, plain, ((e0 = op(op(e2, e3), e3)) | (~ spl4_3 | ~ spl4_87)), inference(backward_demodulation, [], [f723, f269])).
fof(f269, plain, ((e2 = op(e3, e3)) | ~ spl4_3), inference(avatar_component_clause, [], [f267])).
fof(f723, plain, ((e0 = op(op(op(e3, e3), e3), e3)) | ~ spl4_87), inference(avatar_component_clause, [], [f722])).
fof(f3101, plain, (~ spl4_1 | ~ spl4_9), inference(avatar_split_clause, [], [f3094, f293, f259])).
fof(f293, plain, (spl4_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_9])])).
fof(f3094, plain, (~ (e0 = op(e3, e3)) | ~ spl4_9), inference(backward_demodulation, [], [f157, f295])).
fof(f295, plain, ((e0 = op(e3, e1)) | ~ spl4_9), inference(avatar_component_clause, [], [f293])).
fof(f157, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax3)).
fof(f3092, plain, (~ spl4_6 | ~ spl4_14), inference(avatar_split_clause, [], [f3088, f314, f280])).
fof(f280, plain, (spl4_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_6])])).
fof(f314, plain, (spl4_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_14])])).
fof(f3088, plain, (~ (e1 = op(e3, e2)) | ~ spl4_14), inference(backward_demodulation, [], [f154, f316])).
fof(f316, plain, ((e1 = op(e3, e0)) | ~ spl4_14), inference(avatar_component_clause, [], [f314])).
fof(f154, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3085, plain, (~ spl4_6 | ~ spl4_22), inference(avatar_split_clause, [], [f3080, f348, f280])).
fof(f348, plain, (spl4_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_22])])).
fof(f3080, plain, (~ (e1 = op(e3, e2)) | ~ spl4_22), inference(backward_demodulation, [], [f128, f350])).
fof(f350, plain, ((e1 = op(e2, e2)) | ~ spl4_22), inference(avatar_component_clause, [], [f348])).
fof(f128, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3079, plain, (~ spl4_19 | ~ spl4_27), inference(avatar_split_clause, [], [f3076, f369, f335])).
fof(f335, plain, (spl4_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_19])])).
fof(f369, plain, (spl4_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_27])])).
fof(f3076, plain, (~ (e2 = op(e2, e3)) | ~ spl4_27), inference(backward_demodulation, [], [f151, f371])).
fof(f371, plain, ((e2 = op(e2, e1)) | ~ spl4_27), inference(avatar_component_clause, [], [f369])).
fof(f151, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3078, plain, (~ spl4_11 | ~ spl4_27), inference(avatar_split_clause, [], [f3074, f369, f301])).
fof(f301, plain, (spl4_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_11])])).
fof(f3074, plain, (~ (e2 = op(e3, e1)) | ~ spl4_27), inference(backward_demodulation, [], [f122, f371])).
fof(f122, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3073, plain, (~ spl4_1 | ~ spl4_49), inference(avatar_split_clause, [], [f3070, f463, f259])).
fof(f463, plain, (spl4_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_49])])).
fof(f3070, plain, (~ (e0 = op(e3, e3)) | ~ spl4_49), inference(backward_demodulation, [], [f131, f465])).
fof(f465, plain, ((e0 = op(e0, e3)) | ~ spl4_49), inference(avatar_component_clause, [], [f463])).
fof(f131, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3067, plain, (~ spl4_26 | ~ spl4_58), inference(avatar_split_clause, [], [f3066, f501, f365])).
fof(f365, plain, (spl4_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_26])])).
fof(f501, plain, (spl4_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_58])])).
fof(f3066, plain, (~ (e1 = op(e2, e1)) | ~ spl4_58), inference(backward_demodulation, [], [f118, f503])).
fof(f503, plain, ((e1 = op(e0, e1)) | ~ spl4_58), inference(avatar_component_clause, [], [f501])).
fof(f118, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3060, plain, (~ spl4_52 | ~ spl4_64), inference(avatar_split_clause, [], [f3057, f526, f475])).
fof(f475, plain, (spl4_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_52])])).
fof(f526, plain, (spl4_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl4_64])])).
fof(f3057, plain, (~ (e3 = op(e0, e3)) | ~ spl4_64), inference(backward_demodulation, [], [f137, f528])).
fof(f528, plain, ((op(e0, e0) = e3) | ~ spl4_64), inference(avatar_component_clause, [], [f526])).
fof(f137, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3059, plain, (~ spl4_16 | ~ spl4_64), inference(avatar_split_clause, [], [f3055, f526, f322])).
fof(f322, plain, (spl4_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_16])])).
fof(f3055, plain, (~ (e3 = op(e3, e0)) | ~ spl4_64), inference(backward_demodulation, [], [f113, f528])).
fof(f113, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3052, plain, (~ spl4_47 | spl4_72 | ~ spl4_73), inference(avatar_contradiction_clause, [], [f3051])).
fof(f3051, plain, ($false | (~ spl4_47 | spl4_72 | ~ spl4_73)), inference(subsumption_resolution, [], [f3048, f456])).
fof(f456, plain, ((e2 = op(e1, e0)) | ~ spl4_47), inference(avatar_component_clause, [], [f454])).
fof(f454, plain, (spl4_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_47])])).
fof(f3048, plain, (~ (e2 = op(e1, e0)) | (spl4_72 | ~ spl4_73)), inference(backward_demodulation, [], [f648, f651])).
fof(f651, plain, ((e1 = op(op(e0, e0), e0)) | ~ spl4_73), inference(avatar_component_clause, [], [f650])).
fof(f650, plain, (spl4_73 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_73])])).
fof(f648, plain, (~ (e2 = op(op(op(e0, e0), e0), e0)) | spl4_72), inference(avatar_component_clause, [], [f646])).
fof(f646, plain, (spl4_72 <=> (e2 = op(op(op(e0, e0), e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_72])])).
fof(f3047, plain, (~ spl4_11 | ~ spl4_44 | spl4_76), inference(avatar_split_clause, [], [f3046, f666, f441, f301])).
fof(f441, plain, (spl4_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_44])])).
fof(f666, plain, (spl4_76 <=> (e2 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_76])])).
fof(f3046, plain, (~ (e2 = op(e3, e1)) | (~ spl4_44 | spl4_76)), inference(forward_demodulation, [], [f668, f443])).
fof(f443, plain, ((e3 = op(e1, e1)) | ~ spl4_44), inference(avatar_component_clause, [], [f441])).
fof(f668, plain, (~ (e2 = op(op(e1, e1), e1)) | spl4_76), inference(avatar_component_clause, [], [f666])).
fof(f3045, plain, (spl4_9 | ~ spl4_44 | ~ spl4_79), inference(avatar_split_clause, [], [f3044, f680, f441, f293])).
fof(f680, plain, (spl4_79 <=> (e0 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_79])])).
fof(f3044, plain, ((e0 = op(e3, e1)) | (~ spl4_44 | ~ spl4_79)), inference(forward_demodulation, [], [f681, f443])).
fof(f681, plain, ((e0 = op(op(e1, e1), e1)) | ~ spl4_79), inference(avatar_component_clause, [], [f680])).
fof(f3036, plain, (~ spl4_63 | ~ spl4_55), inference(avatar_split_clause, [], [f3035, f488, f522])).
fof(f522, plain, (spl4_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl4_63])])).
fof(f488, plain, (spl4_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_55])])).
fof(f3035, plain, (~ (op(e0, e0) = e2) | ~ spl4_55), inference(forward_demodulation, [], [f136, f490])).
fof(f490, plain, ((e2 = op(e0, e2)) | ~ spl4_55), inference(avatar_component_clause, [], [f488])).
fof(f136, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3033, plain, (~ spl4_59 | ~ spl4_55), inference(avatar_split_clause, [], [f3032, f488, f505])).
fof(f505, plain, (spl4_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_59])])).
fof(f3032, plain, (~ (e2 = op(e0, e1)) | ~ spl4_55), inference(forward_demodulation, [], [f138, f490])).
fof(f138, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3031, plain, (~ spl4_60 | ~ spl4_44), inference(avatar_split_clause, [], [f3030, f441, f509])).
fof(f509, plain, (spl4_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_60])])).
fof(f3030, plain, (~ (e3 = op(e0, e1)) | ~ spl4_44), inference(forward_demodulation, [], [f117, f443])).
fof(f117, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3028, plain, (~ spl4_28 | ~ spl4_44), inference(avatar_split_clause, [], [f3027, f441, f373])).
fof(f373, plain, (spl4_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_28])])).
fof(f3027, plain, (~ (e3 = op(e2, e1)) | ~ spl4_44), inference(forward_demodulation, [], [f120, f443])).
fof(f120, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3019, plain, (~ spl4_6 | ~ spl4_24 | spl4_82), inference(avatar_contradiction_clause, [], [f3018])).
fof(f3018, plain, ($false | (~ spl4_6 | ~ spl4_24 | spl4_82)), inference(subsumption_resolution, [], [f3017, f282])).
fof(f282, plain, ((e1 = op(e3, e2)) | ~ spl4_6), inference(avatar_component_clause, [], [f280])).
fof(f3017, plain, (~ (e1 = op(e3, e2)) | (~ spl4_24 | spl4_82)), inference(forward_demodulation, [], [f698, f358])).
fof(f358, plain, ((e3 = op(e2, e2)) | ~ spl4_24), inference(avatar_component_clause, [], [f356])).
fof(f356, plain, (spl4_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_24])])).
fof(f698, plain, (~ (e1 = op(op(e2, e2), e2)) | spl4_82), inference(avatar_component_clause, [], [f696])).
fof(f696, plain, (spl4_82 <=> (e1 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_82])])).
fof(f3004, plain, (~ spl4_6 | ~ spl4_24 | ~ spl4_37 | spl4_81), inference(avatar_contradiction_clause, [], [f3003])).
fof(f3003, plain, ($false | (~ spl4_6 | ~ spl4_24 | ~ spl4_37 | spl4_81)), inference(subsumption_resolution, [], [f3000, f414])).
fof(f414, plain, ((e0 = op(e1, e2)) | ~ spl4_37), inference(avatar_component_clause, [], [f412])).
fof(f412, plain, (spl4_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_37])])).
fof(f3000, plain, (~ (e0 = op(e1, e2)) | (~ spl4_6 | ~ spl4_24 | spl4_81)), inference(backward_demodulation, [], [f2945, f282])).
fof(f2945, plain, (~ (e0 = op(op(e3, e2), e2)) | (~ spl4_24 | spl4_81)), inference(forward_demodulation, [], [f694, f358])).
fof(f694, plain, (~ (e0 = op(op(op(e2, e2), e2), e2)) | spl4_81), inference(avatar_component_clause, [], [f692])).
fof(f692, plain, (spl4_81 <=> (e0 = op(op(op(e2, e2), e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_81])])).
fof(f3002, plain, (~ spl4_2 | ~ spl4_6), inference(avatar_split_clause, [], [f2997, f280, f263])).
fof(f263, plain, (spl4_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_2])])).
fof(f2997, plain, (~ (e1 = op(e3, e3)) | ~ spl4_6), inference(backward_demodulation, [], [f158, f282])).
fof(f158, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2993, plain, (~ spl4_12 | ~ spl4_16), inference(avatar_split_clause, [], [f2990, f322, f305])).
fof(f305, plain, (spl4_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_12])])).
fof(f2990, plain, (~ (e3 = op(e3, e1)) | ~ spl4_16), inference(backward_demodulation, [], [f153, f324])).
fof(f324, plain, ((e3 = op(e3, e0)) | ~ spl4_16), inference(avatar_component_clause, [], [f322])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2989, plain, (~ spl4_2 | ~ spl4_34), inference(avatar_split_clause, [], [f2987, f399, f263])).
fof(f399, plain, (spl4_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_34])])).
fof(f2987, plain, (~ (e1 = op(e3, e3)) | ~ spl4_34), inference(backward_demodulation, [], [f133, f401])).
fof(f401, plain, ((e1 = op(e1, e3)) | ~ spl4_34), inference(avatar_component_clause, [], [f399])).
fof(f133, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2985, plain, (~ spl4_33 | ~ spl4_37), inference(avatar_split_clause, [], [f2981, f412, f395])).
fof(f395, plain, (spl4_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_33])])).
fof(f2981, plain, (~ (e0 = op(e1, e3)) | ~ spl4_37), inference(backward_demodulation, [], [f146, f414])).
fof(f146, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2984, plain, (~ spl4_5 | ~ spl4_37), inference(avatar_split_clause, [], [f2980, f412, f276])).
fof(f276, plain, (spl4_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_5])])).
fof(f2980, plain, (~ (e0 = op(e3, e2)) | ~ spl4_37), inference(backward_demodulation, [], [f127, f414])).
fof(f127, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2978, plain, (spl4_11 | ~ spl4_44 | ~ spl4_76), inference(avatar_split_clause, [], [f2975, f666, f441, f301])).
fof(f2975, plain, ((e2 = op(e3, e1)) | (~ spl4_44 | ~ spl4_76)), inference(backward_demodulation, [], [f667, f443])).
fof(f667, plain, ((e2 = op(op(e1, e1), e1)) | ~ spl4_76), inference(avatar_component_clause, [], [f666])).
fof(f2977, plain, (~ spl4_12 | ~ spl4_44), inference(avatar_split_clause, [], [f2972, f441, f305])).
fof(f2972, plain, (~ (e3 = op(e3, e1)) | ~ spl4_44), inference(backward_demodulation, [], [f121, f443])).
fof(f121, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2969, plain, (~ spl4_43 | ~ spl4_47), inference(avatar_split_clause, [], [f2962, f454, f437])).
fof(f437, plain, (spl4_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_43])])).
fof(f2962, plain, (~ (e2 = op(e1, e1)) | ~ spl4_47), inference(backward_demodulation, [], [f141, f456])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2968, plain, (~ spl4_15 | ~ spl4_47), inference(avatar_split_clause, [], [f2961, f454, f318])).
fof(f318, plain, (spl4_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_15])])).
fof(f2961, plain, (~ (e2 = op(e3, e0)) | ~ spl4_47), inference(backward_demodulation, [], [f115, f456])).
fof(f115, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2958, plain, (~ spl4_48 | ~ spl4_62 | spl4_71), inference(avatar_split_clause, [], [f2957, f641, f518, f458])).
fof(f458, plain, (spl4_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_48])])).
fof(f518, plain, (spl4_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl4_62])])).
fof(f641, plain, (spl4_71 <=> (e3 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_71])])).
fof(f2957, plain, (~ (e3 = op(e1, e0)) | (~ spl4_62 | spl4_71)), inference(forward_demodulation, [], [f643, f520])).
fof(f520, plain, ((op(e0, e0) = e1) | ~ spl4_62), inference(avatar_component_clause, [], [f518])).
fof(f643, plain, (~ (e3 = op(op(e0, e0), e0)) | spl4_71), inference(avatar_component_clause, [], [f641])).
fof(f2949, plain, (~ spl4_5 | ~ spl4_24 | spl4_85), inference(avatar_split_clause, [], [f2948, f710, f356, f276])).
fof(f710, plain, (spl4_85 <=> (e0 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_85])])).
fof(f2948, plain, (~ (e0 = op(e3, e2)) | (~ spl4_24 | spl4_85)), inference(forward_demodulation, [], [f712, f358])).
fof(f712, plain, (~ (e0 = op(op(e2, e2), e2)) | spl4_85), inference(avatar_component_clause, [], [f710])).
fof(f2939, plain, (~ spl4_39 | ~ spl4_55), inference(avatar_split_clause, [], [f2938, f488, f420])).
fof(f420, plain, (spl4_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_39])])).
fof(f2938, plain, (~ (e2 = op(e1, e2)) | ~ spl4_55), inference(forward_demodulation, [], [f123, f490])).
fof(f123, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2937, plain, (~ spl4_40 | ~ spl4_24), inference(avatar_split_clause, [], [f2936, f356, f424])).
fof(f424, plain, (spl4_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_40])])).
fof(f2936, plain, (~ (e3 = op(e1, e2)) | ~ spl4_24), inference(forward_demodulation, [], [f126, f358])).
fof(f126, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2931, plain, (~ spl4_1 | ~ spl4_5), inference(avatar_split_clause, [], [f2927, f276, f259])).
fof(f2927, plain, (~ (e0 = op(e3, e3)) | ~ spl4_5), inference(backward_demodulation, [], [f158, f278])).
fof(f278, plain, ((e0 = op(e3, e2)) | ~ spl4_5), inference(avatar_component_clause, [], [f276])).
fof(f2924, plain, (~ spl4_7 | ~ spl4_15), inference(avatar_split_clause, [], [f2921, f318, f284])).
fof(f284, plain, (spl4_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_7])])).
fof(f2921, plain, (~ (e2 = op(e3, e2)) | ~ spl4_15), inference(backward_demodulation, [], [f154, f320])).
fof(f320, plain, ((e2 = op(e3, e0)) | ~ spl4_15), inference(avatar_component_clause, [], [f318])).
fof(f2913, plain, (~ spl4_18 | ~ spl4_26), inference(avatar_split_clause, [], [f2907, f365, f331])).
fof(f331, plain, (spl4_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_18])])).
fof(f2907, plain, (~ (e1 = op(e2, e3)) | ~ spl4_26), inference(backward_demodulation, [], [f151, f367])).
fof(f367, plain, ((e1 = op(e2, e1)) | ~ spl4_26), inference(avatar_component_clause, [], [f365])).
fof(f2912, plain, (~ spl4_10 | ~ spl4_26), inference(avatar_split_clause, [], [f2905, f365, f297])).
fof(f297, plain, (spl4_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_10])])).
fof(f2905, plain, (~ (e1 = op(e3, e1)) | ~ spl4_26), inference(backward_demodulation, [], [f122, f367])).
fof(f2904, plain, (~ spl4_21 | ~ spl4_29), inference(avatar_split_clause, [], [f2899, f378, f344])).
fof(f344, plain, (spl4_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_21])])).
fof(f378, plain, (spl4_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_29])])).
fof(f2899, plain, (~ (e0 = op(e2, e2)) | ~ spl4_29), inference(backward_demodulation, [], [f148, f380])).
fof(f380, plain, ((e0 = op(e2, e0)) | ~ spl4_29), inference(avatar_component_clause, [], [f378])).
fof(f148, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2896, plain, (~ spl4_1 | ~ spl4_33), inference(avatar_split_clause, [], [f2892, f395, f259])).
fof(f2892, plain, (~ (e0 = op(e3, e3)) | ~ spl4_33), inference(backward_demodulation, [], [f133, f397])).
fof(f397, plain, ((e0 = op(e1, e3)) | ~ spl4_33), inference(avatar_component_clause, [], [f395])).
fof(f2890, plain, (~ spl4_15 | ~ spl4_48 | ~ spl4_62 | spl4_72), inference(avatar_split_clause, [], [f2886, f646, f518, f458, f318])).
fof(f2886, plain, (~ (e2 = op(e3, e0)) | (~ spl4_48 | ~ spl4_62 | spl4_72)), inference(backward_demodulation, [], [f2738, f460])).
fof(f460, plain, ((e3 = op(e1, e0)) | ~ spl4_48), inference(avatar_component_clause, [], [f458])).
fof(f2738, plain, (~ (e2 = op(op(e1, e0), e0)) | (~ spl4_62 | spl4_72)), inference(forward_demodulation, [], [f648, f520])).
fof(f2889, plain, (~ spl4_36 | ~ spl4_48), inference(avatar_split_clause, [], [f2885, f458, f407])).
fof(f407, plain, (spl4_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_36])])).
fof(f2885, plain, (~ (e3 = op(e1, e3)) | ~ spl4_48), inference(backward_demodulation, [], [f143, f460])).
fof(f143, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2888, plain, (~ spl4_16 | ~ spl4_48), inference(avatar_split_clause, [], [f2884, f458, f322])).
fof(f2884, plain, (~ (e3 = op(e3, e0)) | ~ spl4_48), inference(backward_demodulation, [], [f115, f460])).
fof(f2883, plain, (~ spl4_36 | ~ spl4_52), inference(avatar_split_clause, [], [f2882, f475, f407])).
fof(f2882, plain, (~ (e3 = op(e1, e3)) | ~ spl4_52), inference(backward_demodulation, [], [f129, f477])).
fof(f477, plain, ((e3 = op(e0, e3)) | ~ spl4_52), inference(avatar_component_clause, [], [f475])).
fof(f129, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2879, plain, (~ spl4_51 | ~ spl4_55), inference(avatar_split_clause, [], [f2876, f488, f471])).
fof(f471, plain, (spl4_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_51])])).
fof(f2876, plain, (~ (e2 = op(e0, e3)) | ~ spl4_55), inference(backward_demodulation, [], [f140, f490])).
fof(f140, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2878, plain, (~ spl4_7 | ~ spl4_55), inference(avatar_split_clause, [], [f2875, f488, f284])).
fof(f2875, plain, (~ (e2 = op(e3, e2)) | ~ spl4_55), inference(backward_demodulation, [], [f125, f490])).
fof(f125, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2873, plain, (~ spl4_28 | ~ spl4_43 | spl4_77), inference(avatar_split_clause, [], [f2872, f671, f437, f373])).
fof(f671, plain, (spl4_77 <=> (e3 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_77])])).
fof(f2872, plain, (~ (e3 = op(e2, e1)) | (~ spl4_43 | spl4_77)), inference(forward_demodulation, [], [f673, f439])).
fof(f439, plain, ((e2 = op(e1, e1)) | ~ spl4_43), inference(avatar_component_clause, [], [f437])).
fof(f673, plain, (~ (e3 = op(op(e1, e1), e1)) | spl4_77), inference(avatar_component_clause, [], [f671])).
fof(f2870, plain, (~ spl4_56 | ~ spl4_85 | spl4_86), inference(avatar_split_clause, [], [f2868, f716, f710, f492])).
fof(f492, plain, (spl4_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_56])])).
fof(f716, plain, (spl4_86 <=> (e3 = op(op(op(e2, e2), e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_86])])).
fof(f2868, plain, (~ (e3 = op(e0, e2)) | (~ spl4_85 | spl4_86)), inference(backward_demodulation, [], [f718, f711])).
fof(f711, plain, ((e0 = op(op(e2, e2), e2)) | ~ spl4_85), inference(avatar_component_clause, [], [f710])).
fof(f718, plain, (~ (e3 = op(op(op(e2, e2), e2), e2)) | spl4_86), inference(avatar_component_clause, [], [f716])).
fof(f2866, plain, (~ spl4_51 | ~ spl4_91 | spl4_92), inference(avatar_split_clause, [], [f2865, f746, f740, f471])).
fof(f740, plain, (spl4_91 <=> (e0 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_91])])).
fof(f746, plain, (spl4_92 <=> (e2 = op(op(op(e3, e3), e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_92])])).
fof(f2865, plain, (~ (e2 = op(e0, e3)) | (~ spl4_91 | spl4_92)), inference(backward_demodulation, [], [f748, f741])).
fof(f741, plain, ((e0 = op(op(e3, e3), e3)) | ~ spl4_91), inference(avatar_component_clause, [], [f740])).
fof(f748, plain, (~ (e2 = op(op(op(e3, e3), e3), e3)) | spl4_92), inference(avatar_component_clause, [], [f746])).
fof(f2859, plain, (~ spl4_22 | ~ spl4_38), inference(avatar_split_clause, [], [f2858, f416, f348])).
fof(f416, plain, (spl4_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_38])])).
fof(f2858, plain, (~ (e1 = op(e2, e2)) | ~ spl4_38), inference(forward_demodulation, [], [f126, f418])).
fof(f418, plain, ((e1 = op(e1, e2)) | ~ spl4_38), inference(avatar_component_clause, [], [f416])).
fof(f2855, plain, (~ spl4_1 | ~ spl4_18 | ~ spl4_51 | spl4_90), inference(avatar_contradiction_clause, [], [f2854])).
fof(f2854, plain, ($false | (~ spl4_1 | ~ spl4_18 | ~ spl4_51 | spl4_90)), inference(subsumption_resolution, [], [f2853, f333])).
fof(f333, plain, ((e1 = op(e2, e3)) | ~ spl4_18), inference(avatar_component_clause, [], [f331])).
fof(f2853, plain, (~ (e1 = op(e2, e3)) | (~ spl4_1 | ~ spl4_51 | spl4_90)), inference(forward_demodulation, [], [f2847, f473])).
fof(f473, plain, ((e2 = op(e0, e3)) | ~ spl4_51), inference(avatar_component_clause, [], [f471])).
fof(f2847, plain, (~ (e1 = op(op(e0, e3), e3)) | (~ spl4_1 | spl4_90)), inference(backward_demodulation, [], [f738, f261])).
fof(f261, plain, ((e0 = op(e3, e3)) | ~ spl4_1), inference(avatar_component_clause, [], [f259])).
fof(f738, plain, (~ (e1 = op(op(op(e3, e3), e3), e3)) | spl4_90), inference(avatar_component_clause, [], [f736])).
fof(f736, plain, (spl4_90 <=> (e1 = op(op(op(e3, e3), e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_90])])).
fof(f2852, plain, (~ spl4_1 | ~ spl4_51 | spl4_89), inference(avatar_contradiction_clause, [], [f2851])).
fof(f2851, plain, ($false | (~ spl4_1 | ~ spl4_51 | spl4_89)), inference(subsumption_resolution, [], [f2846, f473])).
fof(f2846, plain, (~ (e2 = op(e0, e3)) | (~ spl4_1 | spl4_89)), inference(backward_demodulation, [], [f733, f261])).
fof(f733, plain, (~ (e2 = op(op(e3, e3), e3)) | spl4_89), inference(avatar_component_clause, [], [f731])).
fof(f731, plain, (spl4_89 <=> (e2 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_89])])).
fof(f2840, plain, (~ spl4_2 | ~ spl4_10), inference(avatar_split_clause, [], [f2838, f297, f263])).
fof(f2838, plain, (~ (e1 = op(e3, e3)) | ~ spl4_10), inference(backward_demodulation, [], [f157, f299])).
fof(f299, plain, ((e1 = op(e3, e1)) | ~ spl4_10), inference(avatar_component_clause, [], [f297])).
fof(f2833, plain, (~ spl4_2 | ~ spl4_18), inference(avatar_split_clause, [], [f2831, f331, f263])).
fof(f2831, plain, (~ (e1 = op(e3, e3)) | ~ spl4_18), inference(backward_demodulation, [], [f134, f333])).
fof(f134, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2827, plain, (~ spl4_20 | ~ spl4_28), inference(avatar_split_clause, [], [f2823, f373, f339])).
fof(f2823, plain, (~ (e3 = op(e2, e3)) | ~ spl4_28), inference(backward_demodulation, [], [f151, f375])).
fof(f375, plain, ((e3 = op(e2, e1)) | ~ spl4_28), inference(avatar_component_clause, [], [f373])).
fof(f2826, plain, (~ spl4_12 | ~ spl4_28), inference(avatar_split_clause, [], [f2822, f373, f305])).
fof(f2822, plain, (~ (e3 = op(e3, e1)) | ~ spl4_28), inference(backward_demodulation, [], [f122, f375])).
fof(f2821, plain, (~ spl4_20 | ~ spl4_36), inference(avatar_split_clause, [], [f2820, f407, f339])).
fof(f2820, plain, (~ (e3 = op(e2, e3)) | ~ spl4_36), inference(backward_demodulation, [], [f132, f409])).
fof(f409, plain, ((e3 = op(e1, e3)) | ~ spl4_36), inference(avatar_component_clause, [], [f407])).
fof(f132, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2815, plain, (~ spl4_33 | ~ spl4_45), inference(avatar_split_clause, [], [f2808, f446, f395])).
fof(f446, plain, (spl4_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_45])])).
fof(f2808, plain, (~ (e0 = op(e1, e3)) | ~ spl4_45), inference(backward_demodulation, [], [f143, f448])).
fof(f448, plain, ((e0 = op(e1, e0)) | ~ spl4_45), inference(avatar_component_clause, [], [f446])).
fof(f2814, plain, (~ spl4_13 | ~ spl4_45), inference(avatar_split_clause, [], [f2807, f446, f310])).
fof(f310, plain, (spl4_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_13])])).
fof(f2807, plain, (~ (e0 = op(e3, e0)) | ~ spl4_45), inference(backward_demodulation, [], [f115, f448])).
fof(f2806, plain, (spl4_28 | ~ spl4_43 | ~ spl4_77), inference(avatar_split_clause, [], [f2805, f671, f437, f373])).
fof(f2805, plain, ((e3 = op(e2, e1)) | (~ spl4_43 | ~ spl4_77)), inference(forward_demodulation, [], [f672, f439])).
fof(f672, plain, ((e3 = op(op(e1, e1), e1)) | ~ spl4_77), inference(avatar_component_clause, [], [f671])).
fof(f2798, plain, (~ spl4_35 | ~ spl4_51), inference(avatar_split_clause, [], [f2797, f471, f403])).
fof(f403, plain, (spl4_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_35])])).
fof(f2797, plain, (~ (e2 = op(e1, e3)) | ~ spl4_51), inference(forward_demodulation, [], [f129, f473])).
fof(f2790, plain, (~ spl4_3 | ~ spl4_7), inference(avatar_split_clause, [], [f2788, f284, f267])).
fof(f2788, plain, (~ (e2 = op(e3, e3)) | ~ spl4_7), inference(backward_demodulation, [], [f158, f286])).
fof(f286, plain, ((e2 = op(e3, e2)) | ~ spl4_7), inference(avatar_component_clause, [], [f284])).
fof(f2787, plain, (~ spl4_8 | ~ spl4_12), inference(avatar_split_clause, [], [f2785, f305, f288])).
fof(f288, plain, (spl4_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_8])])).
fof(f2785, plain, (~ (e3 = op(e3, e2)) | ~ spl4_12), inference(backward_demodulation, [], [f156, f307])).
fof(f307, plain, ((e3 = op(e3, e1)) | ~ spl4_12), inference(avatar_component_clause, [], [f305])).
fof(f156, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2775, plain, (~ spl4_3 | ~ spl4_51), inference(avatar_split_clause, [], [f2774, f471, f267])).
fof(f2774, plain, (~ (e2 = op(e3, e3)) | ~ spl4_51), inference(backward_demodulation, [], [f131, f473])).
fof(f2769, plain, (~ spl4_52 | ~ spl4_56), inference(avatar_split_clause, [], [f2766, f492, f475])).
fof(f2766, plain, (~ (e3 = op(e0, e3)) | ~ spl4_56), inference(backward_demodulation, [], [f140, f494])).
fof(f494, plain, ((e3 = op(e0, e2)) | ~ spl4_56), inference(avatar_component_clause, [], [f492])).
fof(f2759, plain, (spl4_56 | ~ spl4_21 | ~ spl4_83), inference(avatar_split_clause, [], [f2758, f701, f344, f492])).
fof(f701, plain, (spl4_83 <=> (e3 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_83])])).
fof(f2758, plain, ((e3 = op(e0, e2)) | (~ spl4_21 | ~ spl4_83)), inference(forward_demodulation, [], [f702, f346])).
fof(f346, plain, ((e0 = op(e2, e2)) | ~ spl4_21), inference(avatar_component_clause, [], [f344])).
fof(f702, plain, ((e3 = op(op(e2, e2), e2)) | ~ spl4_83), inference(avatar_component_clause, [], [f701])).
fof(f2723, plain, (~ spl4_9 | ~ spl4_13), inference(avatar_split_clause, [], [f2720, f310, f293])).
fof(f2720, plain, (~ (e0 = op(e3, e1)) | ~ spl4_13), inference(backward_demodulation, [], [f153, f312])).
fof(f312, plain, ((e0 = op(e3, e0)) | ~ spl4_13), inference(avatar_component_clause, [], [f310])).
fof(f2710, plain, (~ spl4_49 | ~ spl4_57), inference(avatar_split_clause, [], [f2708, f497, f463])).
fof(f497, plain, (spl4_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_57])])).
fof(f2708, plain, (~ (e0 = op(e0, e3)) | ~ spl4_57), inference(backward_demodulation, [], [f139, f499])).
fof(f499, plain, ((e0 = op(e0, e1)) | ~ spl4_57), inference(avatar_component_clause, [], [f497])).
fof(f139, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2709, plain, (~ spl4_9 | ~ spl4_57), inference(avatar_split_clause, [], [f2705, f497, f293])).
fof(f2705, plain, (~ (e0 = op(e3, e1)) | ~ spl4_57), inference(backward_demodulation, [], [f119, f499])).
fof(f119, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2701, plain, (~ spl4_58 | ~ spl4_62), inference(avatar_split_clause, [], [f2695, f518, f501])).
fof(f2695, plain, (~ (e1 = op(e0, e1)) | ~ spl4_62), inference(backward_demodulation, [], [f135, f520])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2700, plain, (~ spl4_14 | ~ spl4_62), inference(avatar_split_clause, [], [f2694, f518, f314])).
fof(f2694, plain, (~ (e1 = op(e3, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f113, f520])).
fof(f2681, plain, (~ spl4_33 | ~ spl4_3 | ~ spl4_18 | spl4_87), inference(avatar_split_clause, [], [f2680, f722, f331, f267, f395])).
fof(f2680, plain, (~ (e0 = op(e1, e3)) | (~ spl4_3 | ~ spl4_18 | spl4_87)), inference(forward_demodulation, [], [f2679, f333])).
fof(f2679, plain, (~ (e0 = op(op(e2, e3), e3)) | (~ spl4_3 | spl4_87)), inference(forward_demodulation, [], [f724, f269])).
fof(f724, plain, (~ (e0 = op(op(op(e3, e3), e3), e3)) | spl4_87), inference(avatar_component_clause, [], [f722])).
fof(f2641, plain, (~ spl4_2 | ~ spl4_14), inference(avatar_split_clause, [], [f2635, f314, f263])).
fof(f2635, plain, (~ (e1 = op(e3, e3)) | ~ spl4_14), inference(backward_demodulation, [], [f155, f316])).
fof(f155, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2616, plain, (~ spl4_35 | ~ spl4_43), inference(avatar_split_clause, [], [f2612, f437, f403])).
fof(f2612, plain, (~ (e2 = op(e1, e3)) | ~ spl4_43), inference(backward_demodulation, [], [f145, f439])).
fof(f145, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2608, plain, (~ spl4_41 | ~ spl4_45), inference(avatar_split_clause, [], [f2601, f446, f429])).
fof(f429, plain, (spl4_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_41])])).
fof(f2601, plain, (~ (e0 = op(e1, e1)) | ~ spl4_45), inference(backward_demodulation, [], [f141, f448])).
fof(f2598, plain, (~ spl4_21 | ~ spl4_55 | spl4_81), inference(avatar_contradiction_clause, [], [f2597])).
fof(f2597, plain, ($false | (~ spl4_21 | ~ spl4_55 | spl4_81)), inference(subsumption_resolution, [], [f2593, f346])).
fof(f2593, plain, (~ (e0 = op(e2, e2)) | (~ spl4_21 | ~ spl4_55 | spl4_81)), inference(backward_demodulation, [], [f2547, f490])).
fof(f2547, plain, (~ (e0 = op(op(e0, e2), e2)) | (~ spl4_21 | spl4_81)), inference(forward_demodulation, [], [f694, f346])).
fof(f2588, plain, (~ spl4_54 | ~ spl4_58), inference(avatar_split_clause, [], [f2584, f501, f484])).
fof(f484, plain, (spl4_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_54])])).
fof(f2584, plain, (~ (e1 = op(e0, e2)) | ~ spl4_58), inference(backward_demodulation, [], [f138, f503])).
fof(f2578, plain, (~ spl4_9 | spl4_75 | ~ spl4_77), inference(avatar_split_clause, [], [f2576, f671, f662, f293])).
fof(f662, plain, (spl4_75 <=> (e0 = op(op(op(e1, e1), e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_75])])).
fof(f2576, plain, (~ (e0 = op(e3, e1)) | (spl4_75 | ~ spl4_77)), inference(backward_demodulation, [], [f664, f672])).
fof(f664, plain, (~ (e0 = op(op(op(e1, e1), e1), e1)) | spl4_75), inference(avatar_component_clause, [], [f662])).
fof(f2568, plain, (~ spl4_56 | ~ spl4_21 | spl4_83), inference(avatar_split_clause, [], [f2549, f701, f344, f492])).
fof(f2549, plain, (~ (e3 = op(e0, e2)) | (~ spl4_21 | spl4_83)), inference(forward_demodulation, [], [f703, f346])).
fof(f703, plain, (~ (e3 = op(op(e2, e2), e2)) | spl4_83), inference(avatar_component_clause, [], [f701])).
fof(f2559, plain, (~ spl4_18 | ~ spl4_89 | spl4_90), inference(avatar_split_clause, [], [f2558, f736, f731, f331])).
fof(f2558, plain, (~ (e1 = op(e2, e3)) | (~ spl4_89 | spl4_90)), inference(backward_demodulation, [], [f738, f732])).
fof(f732, plain, ((e2 = op(op(e3, e3), e3)) | ~ spl4_89), inference(avatar_component_clause, [], [f731])).
fof(f2557, plain, (~ spl4_21 | ~ spl4_54 | spl4_82), inference(avatar_contradiction_clause, [], [f2556])).
fof(f2556, plain, ($false | (~ spl4_21 | ~ spl4_54 | spl4_82)), inference(subsumption_resolution, [], [f2555, f486])).
fof(f486, plain, ((e1 = op(e0, e2)) | ~ spl4_54), inference(avatar_component_clause, [], [f484])).
fof(f2555, plain, (~ (e1 = op(e0, e2)) | (~ spl4_21 | spl4_82)), inference(forward_demodulation, [], [f698, f346])).
fof(f2546, plain, (~ spl4_21 | ~ spl4_40 | ~ spl4_54 | spl4_86), inference(avatar_contradiction_clause, [], [f2545])).
fof(f2545, plain, ($false | (~ spl4_21 | ~ spl4_40 | ~ spl4_54 | spl4_86)), inference(subsumption_resolution, [], [f2544, f426])).
fof(f426, plain, ((e3 = op(e1, e2)) | ~ spl4_40), inference(avatar_component_clause, [], [f424])).
fof(f2544, plain, (~ (e3 = op(e1, e2)) | (~ spl4_21 | ~ spl4_54 | spl4_86)), inference(forward_demodulation, [], [f2543, f486])).
fof(f2543, plain, (~ (e3 = op(op(e0, e2), e2)) | (~ spl4_21 | spl4_86)), inference(forward_demodulation, [], [f718, f346])).
fof(f2535, plain, (~ spl4_5 | ~ spl4_13), inference(avatar_split_clause, [], [f2528, f310, f276])).
fof(f2528, plain, (~ (e0 = op(e3, e2)) | ~ spl4_13), inference(backward_demodulation, [], [f154, f312])).
fof(f2524, plain, (~ spl4_5 | ~ spl4_21), inference(avatar_split_clause, [], [f2519, f344, f276])).
fof(f2519, plain, (~ (e0 = op(e3, e2)) | ~ spl4_21), inference(backward_demodulation, [], [f128, f346])).
fof(f2518, plain, (~ spl4_22 | ~ spl4_26), inference(avatar_split_clause, [], [f2515, f365, f348])).
fof(f2515, plain, (~ (e1 = op(e2, e2)) | ~ spl4_26), inference(backward_demodulation, [], [f150, f367])).
fof(f150, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2514, plain, (~ spl4_27 | ~ spl4_31), inference(avatar_split_clause, [], [f2512, f386, f369])).
fof(f386, plain, (spl4_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_31])])).
fof(f2512, plain, (~ (e2 = op(e2, e1)) | ~ spl4_31), inference(backward_demodulation, [], [f147, f388])).
fof(f388, plain, ((e2 = op(e2, e0)) | ~ spl4_31), inference(avatar_component_clause, [], [f386])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2508, plain, (~ spl4_22 | ~ spl4_54), inference(avatar_split_clause, [], [f2506, f484, f348])).
fof(f2506, plain, (~ (e1 = op(e2, e2)) | ~ spl4_54), inference(backward_demodulation, [], [f124, f486])).
fof(f124, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2503, plain, (~ spl4_27 | ~ spl4_59), inference(avatar_split_clause, [], [f2499, f505, f369])).
fof(f2499, plain, (~ (e2 = op(e2, e1)) | ~ spl4_59), inference(backward_demodulation, [], [f118, f507])).
fof(f507, plain, ((e2 = op(e0, e1)) | ~ spl4_59), inference(avatar_component_clause, [], [f505])).
fof(f2495, plain, (~ spl4_15 | ~ spl4_64 | spl4_70), inference(avatar_split_clause, [], [f2494, f636, f526, f318])).
fof(f636, plain, (spl4_70 <=> (e2 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_70])])).
fof(f2494, plain, (~ (e2 = op(e3, e0)) | (~ spl4_64 | spl4_70)), inference(forward_demodulation, [], [f638, f528])).
fof(f638, plain, (~ (e2 = op(op(e0, e0), e0)) | spl4_70), inference(avatar_component_clause, [], [f636])).
fof(f2492, plain, (spl4_59 | ~ spl4_41 | ~ spl4_76), inference(avatar_split_clause, [], [f2491, f666, f429, f505])).
fof(f2491, plain, ((e2 = op(e0, e1)) | (~ spl4_41 | ~ spl4_76)), inference(forward_demodulation, [], [f667, f431])).
fof(f431, plain, ((e0 = op(e1, e1)) | ~ spl4_41), inference(avatar_component_clause, [], [f429])).
fof(f2483, plain, (~ spl4_56 | ~ spl4_40), inference(avatar_split_clause, [], [f2482, f424, f492])).
fof(f2482, plain, (~ (e3 = op(e0, e2)) | ~ spl4_40), inference(forward_demodulation, [], [f123, f426])).
fof(f2480, plain, (~ spl4_32 | ~ spl4_64), inference(avatar_split_clause, [], [f2479, f526, f390])).
fof(f390, plain, (spl4_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_32])])).
fof(f2479, plain, (~ (e3 = op(e2, e0)) | ~ spl4_64), inference(forward_demodulation, [], [f112, f528])).
fof(f112, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2472, plain, (~ spl4_8 | ~ spl4_40), inference(avatar_split_clause, [], [f2471, f424, f288])).
fof(f2471, plain, (~ (e3 = op(e3, e2)) | ~ spl4_40), inference(forward_demodulation, [], [f127, f426])).
fof(f2470, plain, (~ spl4_5 | spl4_81 | ~ spl4_83), inference(avatar_split_clause, [], [f2427, f701, f692, f276])).
fof(f2427, plain, (~ (e0 = op(e3, e2)) | (spl4_81 | ~ spl4_83)), inference(backward_demodulation, [], [f694, f702])).
fof(f2460, plain, (~ spl4_19 | ~ spl4_35), inference(avatar_split_clause, [], [f2457, f403, f335])).
fof(f2457, plain, (~ (e2 = op(e2, e3)) | ~ spl4_35), inference(backward_demodulation, [], [f132, f405])).
fof(f405, plain, ((e2 = op(e1, e3)) | ~ spl4_35), inference(avatar_component_clause, [], [f403])).
fof(f2459, plain, (~ spl4_2 | spl4_18 | ~ spl4_35 | ~ spl4_90), inference(avatar_contradiction_clause, [], [f2458])).
fof(f2458, plain, ($false | (~ spl4_2 | spl4_18 | ~ spl4_35 | ~ spl4_90)), inference(subsumption_resolution, [], [f2456, f332])).
fof(f332, plain, (~ (e1 = op(e2, e3)) | spl4_18), inference(avatar_component_clause, [], [f331])).
fof(f2456, plain, ((e1 = op(e2, e3)) | (~ spl4_2 | ~ spl4_35 | ~ spl4_90)), inference(backward_demodulation, [], [f2420, f405])).
fof(f2420, plain, ((e1 = op(op(e1, e3), e3)) | (~ spl4_2 | ~ spl4_90)), inference(forward_demodulation, [], [f737, f265])).
fof(f265, plain, ((e1 = op(e3, e3)) | ~ spl4_2), inference(avatar_component_clause, [], [f263])).
fof(f737, plain, ((e1 = op(op(op(e3, e3), e3), e3)) | ~ spl4_90), inference(avatar_component_clause, [], [f736])).
fof(f2432, plain, (~ spl4_59 | ~ spl4_41 | spl4_76), inference(avatar_split_clause, [], [f2431, f666, f429, f505])).
fof(f2431, plain, (~ (e2 = op(e0, e1)) | (~ spl4_41 | spl4_76)), inference(forward_demodulation, [], [f668, f431])).
fof(f2425, plain, (spl4_35 | ~ spl4_2 | ~ spl4_89), inference(avatar_split_clause, [], [f2424, f731, f263, f403])).
fof(f2424, plain, ((e2 = op(e1, e3)) | (~ spl4_2 | ~ spl4_89)), inference(forward_demodulation, [], [f732, f265])).
fof(f2417, plain, (~ spl4_60 | ~ spl4_12), inference(avatar_split_clause, [], [f2416, f305, f509])).
fof(f2416, plain, (~ (e3 = op(e0, e1)) | ~ spl4_12), inference(forward_demodulation, [], [f119, f307])).
fof(f2403, plain, (~ spl4_2 | spl4_34 | ~ spl4_88), inference(avatar_contradiction_clause, [], [f2402])).
fof(f2402, plain, ($false | (~ spl4_2 | spl4_34 | ~ spl4_88)), inference(subsumption_resolution, [], [f2400, f400])).
fof(f400, plain, (~ (e1 = op(e1, e3)) | spl4_34), inference(avatar_component_clause, [], [f399])).
fof(f2400, plain, ((e1 = op(e1, e3)) | (~ spl4_2 | ~ spl4_88)), inference(backward_demodulation, [], [f727, f265])).
fof(f727, plain, ((e1 = op(op(e3, e3), e3)) | ~ spl4_88), inference(avatar_component_clause, [], [f726])).
fof(f726, plain, (spl4_88 <=> (e1 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_88])])).
fof(f2397, plain, (~ spl4_5 | ~ spl4_24 | ~ spl4_54 | spl4_84), inference(avatar_contradiction_clause, [], [f2396])).
fof(f2396, plain, ($false | (~ spl4_5 | ~ spl4_24 | ~ spl4_54 | spl4_84)), inference(subsumption_resolution, [], [f2394, f486])).
fof(f2394, plain, (~ (e1 = op(e0, e2)) | (~ spl4_5 | ~ spl4_24 | spl4_84)), inference(backward_demodulation, [], [f2340, f278])).
fof(f2340, plain, (~ (e1 = op(op(e3, e2), e2)) | (~ spl4_24 | spl4_84)), inference(forward_demodulation, [], [f708, f358])).
fof(f708, plain, (~ (e1 = op(op(op(e2, e2), e2), e2)) | spl4_84), inference(avatar_component_clause, [], [f706])).
fof(f706, plain, (spl4_84 <=> (e1 = op(op(op(e2, e2), e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_84])])).
fof(f2376, plain, (~ spl4_50 | ~ spl4_54), inference(avatar_split_clause, [], [f2374, f484, f467])).
fof(f467, plain, (spl4_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_50])])).
fof(f2374, plain, (~ (e1 = op(e0, e3)) | ~ spl4_54), inference(backward_demodulation, [], [f140, f486])).
fof(f2375, plain, (~ spl4_6 | ~ spl4_54), inference(avatar_split_clause, [], [f2373, f484, f280])).
fof(f2373, plain, (~ (e1 = op(e3, e2)) | ~ spl4_54), inference(backward_demodulation, [], [f125, f486])).
fof(f2370, plain, (~ spl4_11 | ~ spl4_59), inference(avatar_split_clause, [], [f2367, f505, f301])).
fof(f2367, plain, (~ (e2 = op(e3, e1)) | ~ spl4_59), inference(backward_demodulation, [], [f119, f507])).
fof(f2365, plain, (spl4_15 | ~ spl4_64 | ~ spl4_70), inference(avatar_split_clause, [], [f2361, f636, f526, f318])).
fof(f2361, plain, ((e2 = op(e3, e0)) | (~ spl4_64 | ~ spl4_70)), inference(backward_demodulation, [], [f637, f528])).
fof(f637, plain, ((e2 = op(op(e0, e0), e0)) | ~ spl4_70), inference(avatar_component_clause, [], [f636])).
fof(f2364, plain, (~ spl4_60 | ~ spl4_64), inference(avatar_split_clause, [], [f2358, f526, f509])).
fof(f2358, plain, (~ (e3 = op(e0, e1)) | ~ spl4_64), inference(backward_demodulation, [], [f135, f528])).
fof(f2332, plain, (~ spl4_56 | ~ spl4_24), inference(avatar_split_clause, [], [f2331, f356, f492])).
fof(f2331, plain, (~ (e3 = op(e0, e2)) | ~ spl4_24), inference(forward_demodulation, [], [f124, f358])).
fof(f2324, plain, (~ spl4_3 | ~ spl4_19), inference(avatar_split_clause, [], [f2323, f335, f267])).
fof(f2323, plain, (~ (e2 = op(e3, e3)) | ~ spl4_19), inference(forward_demodulation, [], [f134, f337])).
fof(f337, plain, ((e2 = op(e2, e3)) | ~ spl4_19), inference(avatar_component_clause, [], [f335])).
fof(f2316, plain, (~ spl4_7 | ~ spl4_11), inference(avatar_split_clause, [], [f2315, f301, f284])).
fof(f2315, plain, (~ (e2 = op(e3, e2)) | ~ spl4_11), inference(backward_demodulation, [], [f156, f303])).
fof(f303, plain, ((e2 = op(e3, e1)) | ~ spl4_11), inference(avatar_component_clause, [], [f301])).
fof(f2313, plain, (~ spl4_7 | ~ spl4_39), inference(avatar_split_clause, [], [f2311, f420, f284])).
fof(f2311, plain, (~ (e2 = op(e3, e2)) | ~ spl4_39), inference(backward_demodulation, [], [f127, f422])).
fof(f422, plain, ((e2 = op(e1, e2)) | ~ spl4_39), inference(avatar_component_clause, [], [f420])).
fof(f2310, plain, (~ spl4_9 | ~ spl4_41), inference(avatar_split_clause, [], [f2303, f429, f293])).
fof(f2303, plain, (~ (e0 = op(e3, e1)) | ~ spl4_41), inference(backward_demodulation, [], [f121, f431])).
fof(f2302, plain, (~ spl4_11 | ~ spl4_77 | spl4_78), inference(avatar_split_clause, [], [f2300, f676, f671, f301])).
fof(f676, plain, (spl4_78 <=> (e2 = op(op(op(e1, e1), e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_78])])).
fof(f2300, plain, (~ (e2 = op(e3, e1)) | (~ spl4_77 | spl4_78)), inference(backward_demodulation, [], [f678, f672])).
fof(f678, plain, (~ (e2 = op(op(op(e1, e1), e1), e1)) | spl4_78), inference(avatar_component_clause, [], [f676])).
fof(f2292, plain, (~ spl4_8 | ~ spl4_24), inference(avatar_split_clause, [], [f2291, f356, f288])).
fof(f2291, plain, (~ (e3 = op(e3, e2)) | ~ spl4_24), inference(forward_demodulation, [], [f128, f358])).
fof(f2289, plain, (~ spl4_6 | ~ spl4_83 | spl4_84), inference(avatar_split_clause, [], [f2215, f706, f701, f280])).
fof(f2215, plain, (~ (e1 = op(e3, e2)) | (~ spl4_83 | spl4_84)), inference(forward_demodulation, [], [f708, f702])).
fof(f2285, plain, (~ spl4_14 | ~ spl4_16), inference(avatar_contradiction_clause, [], [f2284])).
fof(f2284, plain, ($false | (~ spl4_14 | ~ spl4_16)), inference(subsumption_resolution, [], [f2282, f163])).
fof(f163, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax4)).
fof(f2282, plain, ((e1 = e3) | (~ spl4_14 | ~ spl4_16)), inference(backward_demodulation, [], [f324, f316])).
fof(f2277, plain, (~ spl4_20 | ~ spl4_24), inference(avatar_split_clause, [], [f2274, f356, f339])).
fof(f2274, plain, (~ (e3 = op(e2, e3)) | ~ spl4_24), inference(backward_demodulation, [], [f152, f358])).
fof(f152, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2247, plain, (~ spl4_27 | ~ spl4_43), inference(avatar_split_clause, [], [f2238, f437, f369])).
fof(f2238, plain, (~ (e2 = op(e2, e1)) | ~ spl4_43), inference(backward_demodulation, [], [f120, f439])).
fof(f2228, plain, (~ spl4_38 | ~ spl4_46), inference(avatar_split_clause, [], [f2227, f450, f416])).
fof(f450, plain, (spl4_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_46])])).
fof(f2227, plain, (~ (e1 = op(e1, e2)) | ~ spl4_46), inference(forward_demodulation, [], [f142, f452])).
fof(f452, plain, ((e1 = op(e1, e0)) | ~ spl4_46), inference(avatar_component_clause, [], [f450])).
fof(f142, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2222, plain, (~ spl4_35 | ~ spl4_1 | ~ spl4_50 | spl4_92), inference(avatar_split_clause, [], [f2201, f746, f467, f259, f403])).
fof(f2201, plain, (~ (e2 = op(e1, e3)) | (~ spl4_1 | ~ spl4_50 | spl4_92)), inference(forward_demodulation, [], [f2200, f469])).
fof(f469, plain, ((e1 = op(e0, e3)) | ~ spl4_50), inference(avatar_component_clause, [], [f467])).
fof(f2200, plain, (~ (e2 = op(op(e0, e3), e3)) | (~ spl4_1 | spl4_92)), inference(forward_demodulation, [], [f748, f261])).
fof(f2220, plain, (~ spl4_28 | ~ spl4_60), inference(avatar_split_clause, [], [f2219, f509, f373])).
fof(f2219, plain, (~ (e3 = op(e2, e1)) | ~ spl4_60), inference(forward_demodulation, [], [f118, f511])).
fof(f511, plain, ((e3 = op(e0, e1)) | ~ spl4_60), inference(avatar_component_clause, [], [f509])).
fof(f2211, plain, (~ spl4_14 | ~ spl4_46), inference(avatar_split_clause, [], [f2147, f450, f314])).
fof(f2147, plain, (~ (e1 = op(e3, e0)) | ~ spl4_46), inference(forward_demodulation, [], [f115, f452])).
fof(f2182, plain, (~ spl4_17 | ~ spl4_29), inference(avatar_split_clause, [], [f2175, f378, f327])).
fof(f327, plain, (spl4_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_17])])).
fof(f2175, plain, (~ (e0 = op(e2, e3)) | ~ spl4_29), inference(backward_demodulation, [], [f149, f380])).
fof(f149, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2181, plain, (~ spl4_13 | ~ spl4_29), inference(avatar_split_clause, [], [f2174, f378, f310])).
fof(f2174, plain, (~ (e0 = op(e3, e0)) | ~ spl4_29), inference(backward_demodulation, [], [f116, f380])).
fof(f116, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2172, plain, (~ spl4_2 | ~ spl4_50), inference(avatar_split_clause, [], [f2169, f467, f263])).
fof(f2169, plain, (~ (e1 = op(e3, e3)) | ~ spl4_50), inference(backward_demodulation, [], [f131, f469])).
fof(f2167, plain, (~ spl4_52 | ~ spl4_60), inference(avatar_split_clause, [], [f2165, f509, f475])).
fof(f2165, plain, (~ (e3 = op(e0, e3)) | ~ spl4_60), inference(backward_demodulation, [], [f139, f511])).
fof(f2162, plain, (~ spl4_32 | ~ spl4_63 | spl4_71), inference(avatar_split_clause, [], [f2161, f641, f522, f390])).
fof(f2161, plain, (~ (e3 = op(e2, e0)) | (~ spl4_63 | spl4_71)), inference(forward_demodulation, [], [f643, f524])).
fof(f524, plain, ((op(e0, e0) = e2) | ~ spl4_63), inference(avatar_component_clause, [], [f522])).
fof(f2151, plain, (~ spl4_60 | ~ spl4_41 | spl4_77), inference(avatar_split_clause, [], [f2088, f671, f429, f509])).
fof(f2088, plain, (~ (e3 = op(e0, e1)) | (~ spl4_41 | spl4_77)), inference(backward_demodulation, [], [f673, f431])).
fof(f2131, plain, (~ spl4_15 | ~ spl4_63), inference(avatar_split_clause, [], [f2130, f522, f318])).
fof(f2130, plain, (~ (e2 = op(e3, e0)) | ~ spl4_63), inference(forward_demodulation, [], [f113, f524])).
fof(f2128, plain, (~ spl4_2 | ~ spl4_35 | spl4_89), inference(avatar_contradiction_clause, [], [f2127])).
fof(f2127, plain, ($false | (~ spl4_2 | ~ spl4_35 | spl4_89)), inference(subsumption_resolution, [], [f2124, f405])).
fof(f2124, plain, (~ (e2 = op(e1, e3)) | (~ spl4_2 | spl4_89)), inference(backward_demodulation, [], [f733, f265])).
fof(f2113, plain, (~ spl4_3 | ~ spl4_35), inference(avatar_split_clause, [], [f2112, f403, f267])).
fof(f2112, plain, (~ (e2 = op(e3, e3)) | ~ spl4_35), inference(backward_demodulation, [], [f133, f405])).
fof(f2092, plain, (~ spl4_37 | ~ spl4_41), inference(avatar_split_clause, [], [f2081, f429, f412])).
fof(f2081, plain, (~ (e0 = op(e1, e2)) | ~ spl4_41), inference(backward_demodulation, [], [f144, f431])).
fof(f144, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2079, plain, (~ spl4_46 | ~ spl4_47), inference(avatar_contradiction_clause, [], [f2078])).
fof(f2078, plain, ($false | (~ spl4_46 | ~ spl4_47)), inference(subsumption_resolution, [], [f2077, f162])).
fof(f162, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2077, plain, ((e1 = e2) | (~ spl4_46 | ~ spl4_47)), inference(backward_demodulation, [], [f456, f452])).
fof(f2076, plain, (~ spl4_37 | ~ spl4_53), inference(avatar_split_clause, [], [f2073, f480, f412])).
fof(f480, plain, (spl4_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_53])])).
fof(f2073, plain, (~ (e0 = op(e1, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f123, f482])).
fof(f482, plain, ((e0 = op(e0, e2)) | ~ spl4_53), inference(avatar_component_clause, [], [f480])).
fof(f2070, plain, (~ spl4_10 | ~ spl4_58), inference(avatar_split_clause, [], [f2067, f501, f297])).
fof(f2067, plain, (~ (e1 = op(e3, e1)) | ~ spl4_58), inference(backward_demodulation, [], [f119, f503])).
fof(f2053, plain, (spl4_40 | ~ spl4_22 | ~ spl4_83), inference(avatar_split_clause, [], [f2052, f701, f348, f424])).
fof(f2052, plain, ((e3 = op(e1, e2)) | (~ spl4_22 | ~ spl4_83)), inference(forward_demodulation, [], [f702, f350])).
fof(f2051, plain, (~ spl4_37 | ~ spl4_22 | spl4_85), inference(avatar_split_clause, [], [f2050, f710, f348, f412])).
fof(f2050, plain, (~ (e0 = op(e1, e2)) | (~ spl4_22 | spl4_85)), inference(forward_demodulation, [], [f712, f350])).
fof(f2048, plain, (~ spl4_63 | ~ spl4_47), inference(avatar_split_clause, [], [f2047, f454, f522])).
fof(f2047, plain, (~ (op(e0, e0) = e2) | ~ spl4_47), inference(forward_demodulation, [], [f111, f456])).
fof(f111, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2036, plain, (~ spl4_32 | ~ spl4_47 | ~ spl4_62 | spl4_74), inference(avatar_contradiction_clause, [], [f2035])).
fof(f2035, plain, ($false | (~ spl4_32 | ~ spl4_47 | ~ spl4_62 | spl4_74)), inference(subsumption_resolution, [], [f2034, f392])).
fof(f392, plain, ((e3 = op(e2, e0)) | ~ spl4_32), inference(avatar_component_clause, [], [f390])).
fof(f2034, plain, (~ (e3 = op(e2, e0)) | (~ spl4_47 | ~ spl4_62 | spl4_74)), inference(forward_demodulation, [], [f2033, f456])).
fof(f2033, plain, (~ (e3 = op(op(e1, e0), e0)) | (~ spl4_62 | spl4_74)), inference(forward_demodulation, [], [f658, f520])).
fof(f658, plain, (~ (e3 = op(op(op(e0, e0), e0), e0)) | spl4_74), inference(avatar_component_clause, [], [f656])).
fof(f656, plain, (spl4_74 <=> (e3 = op(op(op(e0, e0), e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_74])])).
fof(f2013, plain, (~ spl4_39 | ~ spl4_47), inference(avatar_split_clause, [], [f2011, f454, f420])).
fof(f2011, plain, (~ (e2 = op(e1, e2)) | ~ spl4_47), inference(backward_demodulation, [], [f142, f456])).
fof(f2006, plain, (~ spl4_53 | ~ spl4_57), inference(avatar_split_clause, [], [f2004, f497, f480])).
fof(f2004, plain, (~ (e0 = op(e0, e2)) | ~ spl4_57), inference(backward_demodulation, [], [f138, f499])).
fof(f1954, plain, (~ spl4_40 | ~ spl4_22 | spl4_83), inference(avatar_split_clause, [], [f1917, f701, f348, f424])).
fof(f1917, plain, (~ (e3 = op(e1, e2)) | (~ spl4_22 | spl4_83)), inference(backward_demodulation, [], [f703, f350])).
fof(f1953, plain, (~ spl4_16 | ~ spl4_8), inference(avatar_split_clause, [], [f1952, f288, f322])).
fof(f1952, plain, (~ (e3 = op(e3, e0)) | ~ spl4_8), inference(forward_demodulation, [], [f154, f290])).
fof(f290, plain, ((e3 = op(e3, e2)) | ~ spl4_8), inference(avatar_component_clause, [], [f288])).
fof(f1949, plain, (~ spl4_11 | ~ spl4_3), inference(avatar_split_clause, [], [f1948, f267, f301])).
fof(f1948, plain, (~ (e2 = op(e3, e1)) | ~ spl4_3), inference(forward_demodulation, [], [f157, f269])).
fof(f1942, plain, (~ spl4_9 | ~ spl4_11), inference(avatar_contradiction_clause, [], [f1941])).
fof(f1941, plain, ($false | (~ spl4_9 | ~ spl4_11)), inference(subsumption_resolution, [], [f1940, f160])).
fof(f160, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f1940, plain, ((e0 = e2) | (~ spl4_9 | ~ spl4_11)), inference(backward_demodulation, [], [f303, f295])).
fof(f1933, plain, (~ spl4_1 | ~ spl4_17), inference(avatar_split_clause, [], [f1931, f327, f259])).
fof(f1931, plain, (~ (e0 = op(e3, e3)) | ~ spl4_17), inference(backward_demodulation, [], [f134, f329])).
fof(f329, plain, ((e0 = op(e2, e3)) | ~ spl4_17), inference(avatar_component_clause, [], [f327])).
fof(f1907, plain, (~ spl4_14 | ~ spl4_32 | ~ spl4_63 | spl4_69), inference(avatar_split_clause, [], [f1903, f632, f522, f390, f314])).
fof(f632, plain, (spl4_69 <=> (e1 = op(op(op(e0, e0), e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_69])])).
fof(f1903, plain, (~ (e1 = op(e3, e0)) | (~ spl4_32 | ~ spl4_63 | spl4_69)), inference(backward_demodulation, [], [f1896, f392])).
fof(f1896, plain, (~ (e1 = op(op(e2, e0), e0)) | (~ spl4_63 | spl4_69)), inference(forward_demodulation, [], [f634, f524])).
fof(f634, plain, (~ (e1 = op(op(op(e0, e0), e0), e0)) | spl4_69), inference(avatar_component_clause, [], [f632])).
fof(f1905, plain, (~ spl4_24 | ~ spl4_32), inference(avatar_split_clause, [], [f1899, f390, f356])).
fof(f1899, plain, (~ (e3 = op(e2, e2)) | ~ spl4_32), inference(backward_demodulation, [], [f148, f392])).
fof(f1904, plain, (~ spl4_16 | ~ spl4_32), inference(avatar_split_clause, [], [f1897, f390, f322])).
fof(f1897, plain, (~ (e3 = op(e3, e0)) | ~ spl4_32), inference(backward_demodulation, [], [f116, f392])).
fof(f1891, plain, (~ spl4_31 | ~ spl4_63), inference(avatar_split_clause, [], [f1890, f522, f386])).
fof(f1890, plain, (~ (e2 = op(e2, e0)) | ~ spl4_63), inference(forward_demodulation, [], [f112, f524])).
fof(f1882, plain, (~ spl4_28 | ~ spl4_76 | spl4_80), inference(avatar_split_clause, [], [f1818, f686, f666, f373])).
fof(f686, plain, (spl4_80 <=> (e3 = op(op(op(e1, e1), e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_80])])).
fof(f1818, plain, (~ (e3 = op(e2, e1)) | (~ spl4_76 | spl4_80)), inference(backward_demodulation, [], [f688, f667])).
fof(f688, plain, (~ (e3 = op(op(op(e1, e1), e1), e1)) | spl4_80), inference(avatar_component_clause, [], [f686])).
fof(f1880, plain, (~ spl4_25 | spl4_75 | ~ spl4_76), inference(avatar_split_clause, [], [f1823, f666, f662, f361])).
fof(f361, plain, (spl4_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_25])])).
fof(f1823, plain, (~ (e0 = op(e2, e1)) | (spl4_75 | ~ spl4_76)), inference(forward_demodulation, [], [f664, f667])).
fof(f1876, plain, (~ spl4_20 | ~ spl4_52), inference(avatar_split_clause, [], [f1875, f475, f339])).
fof(f1875, plain, (~ (e3 = op(e2, e3)) | ~ spl4_52), inference(forward_demodulation, [], [f130, f477])).
fof(f130, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1873, plain, (~ spl4_9 | ~ spl4_44 | spl4_79), inference(avatar_split_clause, [], [f1839, f680, f441, f293])).
fof(f1839, plain, (~ (e0 = op(e3, e1)) | (~ spl4_44 | spl4_79)), inference(backward_demodulation, [], [f682, f443])).
fof(f682, plain, (~ (e0 = op(op(e1, e1), e1)) | spl4_79), inference(avatar_component_clause, [], [f680])).
fof(f1871, plain, (~ spl4_1 | spl4_49 | ~ spl4_91), inference(avatar_contradiction_clause, [], [f1870])).
fof(f1870, plain, ($false | (~ spl4_1 | spl4_49 | ~ spl4_91)), inference(subsumption_resolution, [], [f1869, f464])).
fof(f464, plain, (~ (e0 = op(e0, e3)) | spl4_49), inference(avatar_component_clause, [], [f463])).
fof(f1869, plain, ((e0 = op(e0, e3)) | (~ spl4_1 | ~ spl4_91)), inference(backward_demodulation, [], [f741, f261])).
fof(f1813, plain, (~ spl4_48 | ~ spl4_30 | ~ spl4_63 | spl4_74), inference(avatar_split_clause, [], [f1797, f656, f522, f382, f458])).
fof(f382, plain, (spl4_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_30])])).
fof(f1797, plain, (~ (e3 = op(e1, e0)) | (~ spl4_30 | ~ spl4_63 | spl4_74)), inference(forward_demodulation, [], [f1796, f384])).
fof(f384, plain, ((e1 = op(e2, e0)) | ~ spl4_30), inference(avatar_component_clause, [], [f382])).
fof(f1796, plain, (~ (e3 = op(op(e2, e0), e0)) | (~ spl4_63 | spl4_74)), inference(forward_demodulation, [], [f658, f524])).
fof(f1770, plain, (~ spl4_49 | ~ spl4_53), inference(avatar_split_clause, [], [f1767, f480, f463])).
fof(f1767, plain, (~ (e0 = op(e0, e3)) | ~ spl4_53), inference(backward_demodulation, [], [f140, f482])).
fof(f1769, plain, (~ spl4_5 | ~ spl4_53), inference(avatar_split_clause, [], [f1765, f480, f276])).
fof(f1765, plain, (~ (e0 = op(e3, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f125, f482])).
fof(f1736, plain, (~ spl4_50 | ~ spl4_58), inference(avatar_split_clause, [], [f1735, f501, f467])).
fof(f1735, plain, (~ (e1 = op(e0, e3)) | ~ spl4_58), inference(forward_demodulation, [], [f139, f503])).
fof(f1730, plain, (~ spl4_15 | ~ spl4_30 | ~ spl4_64 | spl4_69), inference(avatar_contradiction_clause, [], [f1729])).
fof(f1729, plain, ($false | (~ spl4_15 | ~ spl4_30 | ~ spl4_64 | spl4_69)), inference(subsumption_resolution, [], [f1728, f384])).
fof(f1728, plain, (~ (e1 = op(e2, e0)) | (~ spl4_15 | ~ spl4_64 | spl4_69)), inference(forward_demodulation, [], [f1727, f320])).
fof(f1727, plain, (~ (e1 = op(op(e3, e0), e0)) | (~ spl4_64 | spl4_69)), inference(forward_demodulation, [], [f634, f528])).
fof(f1687, plain, (~ spl4_2 | ~ spl4_36 | spl4_90), inference(avatar_contradiction_clause, [], [f1686])).
fof(f1686, plain, ($false | (~ spl4_2 | ~ spl4_36 | spl4_90)), inference(subsumption_resolution, [], [f1685, f265])).
fof(f1685, plain, (~ (e1 = op(e3, e3)) | (~ spl4_2 | ~ spl4_36 | spl4_90)), inference(forward_demodulation, [], [f1684, f409])).
fof(f1684, plain, (~ (e1 = op(op(e1, e3), e3)) | (~ spl4_2 | spl4_90)), inference(forward_demodulation, [], [f738, f265])).
fof(f1678, plain, (~ spl4_54 | ~ spl4_38), inference(avatar_split_clause, [], [f1677, f416, f484])).
fof(f1677, plain, (~ (e1 = op(e0, e2)) | ~ spl4_38), inference(forward_demodulation, [], [f123, f418])).
fof(f1663, plain, (~ spl4_14 | ~ spl4_30), inference(avatar_split_clause, [], [f1662, f382, f314])).
fof(f1662, plain, (~ (e1 = op(e3, e0)) | ~ spl4_30), inference(forward_demodulation, [], [f116, f384])).
fof(f1631, plain, (~ spl4_22 | ~ spl4_30), inference(avatar_split_clause, [], [f1627, f382, f348])).
fof(f1627, plain, (~ (e1 = op(e2, e2)) | ~ spl4_30), inference(backward_demodulation, [], [f148, f384])).
fof(f1623, plain, (~ spl4_37 | ~ spl4_38), inference(avatar_contradiction_clause, [], [f1622])).
fof(f1622, plain, ($false | (~ spl4_37 | ~ spl4_38)), inference(subsumption_resolution, [], [f1621, f159])).
fof(f159, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1621, plain, ((e0 = e1) | (~ spl4_37 | ~ spl4_38)), inference(forward_demodulation, [], [f418, f414])).
fof(f1614, plain, (~ spl4_31 | ~ spl4_47), inference(avatar_split_clause, [], [f1611, f454, f386])).
fof(f1611, plain, (~ (e2 = op(e2, e0)) | ~ spl4_47), inference(backward_demodulation, [], [f114, f456])).
fof(f114, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1596, plain, (~ spl4_59 | spl4_78 | ~ spl4_79), inference(avatar_split_clause, [], [f1595, f680, f676, f505])).
fof(f1595, plain, (~ (e2 = op(e0, e1)) | (spl4_78 | ~ spl4_79)), inference(backward_demodulation, [], [f678, f681])).
fof(f1578, plain, (~ spl4_1 | ~ spl4_13), inference(avatar_split_clause, [], [f1577, f310, f259])).
fof(f1577, plain, (~ (e0 = op(e3, e3)) | ~ spl4_13), inference(forward_demodulation, [], [f155, f312])).
fof(f1574, plain, (~ spl4_1 | ~ spl4_3), inference(avatar_contradiction_clause, [], [f1573])).
fof(f1573, plain, ($false | (~ spl4_1 | ~ spl4_3)), inference(subsumption_resolution, [], [f1572, f160])).
fof(f1572, plain, ((e0 = e2) | (~ spl4_1 | ~ spl4_3)), inference(forward_demodulation, [], [f269, f261])).
fof(f1539, plain, (~ spl4_19 | ~ spl4_31), inference(avatar_split_clause, [], [f1537, f386, f335])).
fof(f1537, plain, (~ (e2 = op(e2, e3)) | ~ spl4_31), inference(backward_demodulation, [], [f149, f388])).
fof(f1534, plain, (~ spl4_37 | ~ spl4_39), inference(avatar_contradiction_clause, [], [f1533])).
fof(f1533, plain, ($false | (~ spl4_37 | ~ spl4_39)), inference(subsumption_resolution, [], [f1532, f160])).
fof(f1532, plain, ((e0 = e2) | (~ spl4_37 | ~ spl4_39)), inference(backward_demodulation, [], [f422, f414])).
fof(f1530, plain, (~ spl4_30 | ~ spl4_46), inference(avatar_split_clause, [], [f1529, f450, f382])).
fof(f1529, plain, (~ (e1 = op(e2, e0)) | ~ spl4_46), inference(backward_demodulation, [], [f114, f452])).
fof(f1487, plain, (~ spl4_46 | ~ spl4_34), inference(avatar_split_clause, [], [f1486, f399, f450])).
fof(f1486, plain, (~ (e1 = op(e1, e0)) | ~ spl4_34), inference(forward_demodulation, [], [f143, f401])).
fof(f1485, plain, (~ spl4_48 | ~ spl4_44), inference(avatar_split_clause, [], [f1484, f441, f458])).
fof(f1484, plain, (~ (e3 = op(e1, e0)) | ~ spl4_44), inference(forward_demodulation, [], [f141, f443])).
fof(f1456, plain, (~ spl4_59 | ~ spl4_63), inference(avatar_split_clause, [], [f1452, f522, f505])).
fof(f1452, plain, (~ (e2 = op(e0, e1)) | ~ spl4_63), inference(backward_demodulation, [], [f135, f524])).
fof(f1419, plain, (~ spl4_36 | ~ spl4_44), inference(avatar_split_clause, [], [f1417, f441, f407])).
fof(f1417, plain, (~ (e3 = op(e1, e3)) | ~ spl4_44), inference(backward_demodulation, [], [f145, f443])).
fof(f1413, plain, (~ spl4_1 | ~ spl4_52 | spl4_87), inference(avatar_contradiction_clause, [], [f1412])).
fof(f1412, plain, ($false | (~ spl4_1 | ~ spl4_52 | spl4_87)), inference(subsumption_resolution, [], [f1408, f261])).
fof(f1408, plain, (~ (e0 = op(e3, e3)) | (~ spl4_1 | ~ spl4_52 | spl4_87)), inference(backward_demodulation, [], [f1348, f477])).
fof(f1348, plain, (~ (e0 = op(op(e0, e3), e3)) | (~ spl4_1 | spl4_87)), inference(backward_demodulation, [], [f724, f261])).
fof(f1406, plain, (~ spl4_53 | ~ spl4_54), inference(avatar_contradiction_clause, [], [f1405])).
fof(f1405, plain, ($false | (~ spl4_53 | ~ spl4_54)), inference(subsumption_resolution, [], [f1404, f159])).
fof(f1404, plain, ((e0 = e1) | (~ spl4_53 | ~ spl4_54)), inference(forward_demodulation, [], [f486, f482])).
fof(f1403, plain, (~ spl4_43 | ~ spl4_59), inference(avatar_split_clause, [], [f1402, f505, f437])).
fof(f1402, plain, (~ (e2 = op(e1, e1)) | ~ spl4_59), inference(backward_demodulation, [], [f117, f507])).
fof(f1378, plain, (spl4_50 | ~ spl4_1 | ~ spl4_88), inference(avatar_split_clause, [], [f1358, f726, f259, f467])).
fof(f1358, plain, ((e1 = op(e0, e3)) | (~ spl4_1 | ~ spl4_88)), inference(forward_demodulation, [], [f727, f261])).
fof(f1364, plain, (~ spl4_25 | ~ spl4_43 | ~ spl4_60 | spl4_80), inference(avatar_contradiction_clause, [], [f1363])).
fof(f1363, plain, ($false | (~ spl4_25 | ~ spl4_43 | ~ spl4_60 | spl4_80)), inference(subsumption_resolution, [], [f1362, f511])).
fof(f1362, plain, (~ (e3 = op(e0, e1)) | (~ spl4_25 | ~ spl4_43 | spl4_80)), inference(forward_demodulation, [], [f1361, f363])).
fof(f363, plain, ((e0 = op(e2, e1)) | ~ spl4_25), inference(avatar_component_clause, [], [f361])).
fof(f1361, plain, (~ (e3 = op(op(e2, e1), e1)) | (~ spl4_43 | spl4_80)), inference(forward_demodulation, [], [f688, f439])).
fof(f1354, plain, (~ spl4_1 | ~ spl4_50 | spl4_88), inference(avatar_contradiction_clause, [], [f1353])).
fof(f1353, plain, ($false | (~ spl4_1 | ~ spl4_50 | spl4_88)), inference(subsumption_resolution, [], [f1349, f469])).
fof(f1349, plain, (~ (e1 = op(e0, e3)) | (~ spl4_1 | spl4_88)), inference(backward_demodulation, [], [f728, f261])).
fof(f728, plain, (~ (e1 = op(op(e3, e3), e3)) | spl4_88), inference(avatar_component_clause, [], [f726])).
fof(f1322, plain, (~ spl4_34 | ~ spl4_38), inference(avatar_split_clause, [], [f1318, f416, f399])).
fof(f1318, plain, (~ (e1 = op(e1, e3)) | ~ spl4_38), inference(backward_demodulation, [], [f146, f418])).
fof(f1314, plain, (~ spl4_25 | ~ spl4_43 | spl4_79), inference(avatar_contradiction_clause, [], [f1313])).
fof(f1313, plain, ($false | (~ spl4_25 | ~ spl4_43 | spl4_79)), inference(subsumption_resolution, [], [f1307, f363])).
fof(f1307, plain, (~ (e0 = op(e2, e1)) | (~ spl4_43 | spl4_79)), inference(backward_demodulation, [], [f682, f439])).
fof(f1309, plain, (~ spl4_39 | ~ spl4_43), inference(avatar_split_clause, [], [f1301, f437, f420])).
fof(f1301, plain, (~ (e2 = op(e1, e2)) | ~ spl4_43), inference(backward_demodulation, [], [f144, f439])).
fof(f1300, plain, (~ spl4_34 | ~ spl4_50), inference(avatar_split_clause, [], [f1295, f467, f399])).
fof(f1295, plain, (~ (e1 = op(e1, e3)) | ~ spl4_50), inference(backward_demodulation, [], [f129, f469])).
fof(f1284, plain, (~ spl4_56 | ~ spl4_60), inference(avatar_split_clause, [], [f1281, f509, f492])).
fof(f1281, plain, (~ (e3 = op(e0, e2)) | ~ spl4_60), inference(backward_demodulation, [], [f138, f511])).
fof(f1259, plain, (~ spl4_25 | ~ spl4_27), inference(avatar_contradiction_clause, [], [f1258])).
fof(f1258, plain, ($false | (~ spl4_25 | ~ spl4_27)), inference(subsumption_resolution, [], [f1257, f160])).
fof(f1257, plain, ((e0 = e2) | (~ spl4_25 | ~ spl4_27)), inference(forward_demodulation, [], [f371, f363])).
fof(f1249, plain, (~ spl4_30 | ~ spl4_63 | spl4_73), inference(avatar_contradiction_clause, [], [f1248])).
fof(f1248, plain, ($false | (~ spl4_30 | ~ spl4_63 | spl4_73)), inference(subsumption_resolution, [], [f1243, f384])).
fof(f1243, plain, (~ (e1 = op(e2, e0)) | (~ spl4_63 | spl4_73)), inference(backward_demodulation, [], [f652, f524])).
fof(f652, plain, (~ (e1 = op(op(e0, e0), e0)) | spl4_73), inference(avatar_component_clause, [], [f650])).
fof(f1203, plain, (~ spl4_25 | ~ spl4_28), inference(avatar_contradiction_clause, [], [f1202])).
fof(f1202, plain, ($false | (~ spl4_25 | ~ spl4_28)), inference(subsumption_resolution, [], [f1201, f161])).
fof(f161, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f1201, plain, ((e0 = e3) | (~ spl4_25 | ~ spl4_28)), inference(forward_demodulation, [], [f375, f363])).
fof(f1200, plain, (~ spl4_30 | ~ spl4_31), inference(avatar_contradiction_clause, [], [f1199])).
fof(f1199, plain, ($false | (~ spl4_30 | ~ spl4_31)), inference(subsumption_resolution, [], [f1198, f162])).
fof(f1198, plain, ((e1 = e2) | (~ spl4_30 | ~ spl4_31)), inference(backward_demodulation, [], [f388, f384])).
fof(f1188, plain, (~ spl4_54 | ~ spl4_56), inference(avatar_contradiction_clause, [], [f1187])).
fof(f1187, plain, ($false | (~ spl4_54 | ~ spl4_56)), inference(subsumption_resolution, [], [f1186, f163])).
fof(f1186, plain, ((e1 = e3) | (~ spl4_54 | ~ spl4_56)), inference(backward_demodulation, [], [f494, f486])).
fof(f1184, plain, (~ spl4_14 | ~ spl4_64 | spl4_73), inference(avatar_split_clause, [], [f1179, f650, f526, f314])).
fof(f1179, plain, (~ (e1 = op(e3, e0)) | (~ spl4_64 | spl4_73)), inference(backward_demodulation, [], [f652, f528])).
fof(f1181, plain, (~ spl4_48 | ~ spl4_64), inference(avatar_split_clause, [], [f1175, f526, f458])).
fof(f1175, plain, (~ (e3 = op(e1, e0)) | ~ spl4_64), inference(backward_demodulation, [], [f111, f528])).
fof(f1163, plain, (~ spl4_64 | ~ spl4_56), inference(avatar_split_clause, [], [f1162, f492, f526])).
fof(f1162, plain, (~ (op(e0, e0) = e3) | ~ spl4_56), inference(forward_demodulation, [], [f136, f494])).
fof(f1126, plain, (~ spl4_18 | ~ spl4_3 | spl4_88), inference(avatar_split_clause, [], [f1117, f726, f267, f331])).
fof(f1117, plain, (~ (e1 = op(e2, e3)) | (~ spl4_3 | spl4_88)), inference(backward_demodulation, [], [f728, f269])).
fof(f1125, plain, (~ spl4_15 | ~ spl4_3), inference(avatar_split_clause, [], [f1124, f267, f318])).
fof(f1124, plain, (~ (e2 = op(e3, e0)) | ~ spl4_3), inference(forward_demodulation, [], [f155, f269])).
fof(f1114, plain, (~ spl4_5 | ~ spl4_8), inference(avatar_contradiction_clause, [], [f1113])).
fof(f1113, plain, ($false | (~ spl4_5 | ~ spl4_8)), inference(subsumption_resolution, [], [f1112, f161])).
fof(f1112, plain, ((e0 = e3) | (~ spl4_5 | ~ spl4_8)), inference(forward_demodulation, [], [f290, f278])).
fof(f1110, plain, (~ spl4_13 | ~ spl4_14), inference(avatar_contradiction_clause, [], [f1109])).
fof(f1109, plain, ($false | (~ spl4_13 | ~ spl4_14)), inference(subsumption_resolution, [], [f1108, f159])).
fof(f1108, plain, ((e0 = e1) | (~ spl4_13 | ~ spl4_14)), inference(backward_demodulation, [], [f316, f312])).
fof(f1083, plain, (~ spl4_33 | ~ spl4_34), inference(avatar_contradiction_clause, [], [f1082])).
fof(f1082, plain, ($false | (~ spl4_33 | ~ spl4_34)), inference(subsumption_resolution, [], [f1081, f159])).
fof(f1081, plain, ((e0 = e1) | (~ spl4_33 | ~ spl4_34)), inference(backward_demodulation, [], [f401, f397])).
fof(f1073, plain, (~ spl4_43 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1072])).
fof(f1072, plain, ($false | (~ spl4_43 | ~ spl4_44)), inference(subsumption_resolution, [], [f1071, f164])).
fof(f164, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1071, plain, ((e2 = e3) | (~ spl4_43 | ~ spl4_44)), inference(forward_demodulation, [], [f443, f439])).
fof(f1066, plain, (~ spl4_49 | ~ spl4_50), inference(avatar_contradiction_clause, [], [f1065])).
fof(f1065, plain, ($false | (~ spl4_49 | ~ spl4_50)), inference(subsumption_resolution, [], [f1064, f159])).
fof(f1064, plain, ((e0 = e1) | (~ spl4_49 | ~ spl4_50)), inference(backward_demodulation, [], [f469, f465])).
fof(f1063, plain, (~ spl4_50 | ~ spl4_51), inference(avatar_contradiction_clause, [], [f1062])).
fof(f1062, plain, ($false | (~ spl4_50 | ~ spl4_51)), inference(subsumption_resolution, [], [f1061, f162])).
fof(f1061, plain, ((e1 = e2) | (~ spl4_50 | ~ spl4_51)), inference(backward_demodulation, [], [f473, f469])).
fof(f1059, plain, (~ spl4_19 | ~ spl4_51), inference(avatar_split_clause, [], [f1056, f471, f335])).
fof(f1056, plain, (~ (e2 = op(e2, e3)) | ~ spl4_51), inference(backward_demodulation, [], [f130, f473])).
fof(f1050, plain, (~ spl4_51 | ~ spl4_59), inference(avatar_split_clause, [], [f1048, f505, f471])).
fof(f1048, plain, (~ (e2 = op(e0, e3)) | ~ spl4_59), inference(backward_demodulation, [], [f139, f507])).
fof(f1043, plain, (~ spl4_51 | ~ spl4_63), inference(avatar_split_clause, [], [f1035, f522, f471])).
fof(f1035, plain, (~ (e2 = op(e0, e3)) | ~ spl4_63), inference(backward_demodulation, [], [f137, f524])).
fof(f976, plain, (~ spl4_38 | ~ spl4_40), inference(avatar_contradiction_clause, [], [f975])).
fof(f975, plain, ($false | (~ spl4_38 | ~ spl4_40)), inference(subsumption_resolution, [], [f974, f163])).
fof(f974, plain, ((e1 = e3) | (~ spl4_38 | ~ spl4_40)), inference(backward_demodulation, [], [f426, f418])).
fof(f941, plain, (~ spl4_2 | ~ spl4_17 | ~ spl4_35 | spl4_87), inference(avatar_contradiction_clause, [], [f940])).
fof(f940, plain, ($false | (~ spl4_2 | ~ spl4_17 | ~ spl4_35 | spl4_87)), inference(subsumption_resolution, [], [f938, f329])).
fof(f938, plain, (~ (e0 = op(e2, e3)) | (~ spl4_2 | ~ spl4_35 | spl4_87)), inference(backward_demodulation, [], [f920, f405])).
fof(f920, plain, (~ (e0 = op(op(e1, e3), e3)) | (~ spl4_2 | spl4_87)), inference(forward_demodulation, [], [f724, f265])).
fof(f931, plain, (~ spl4_21 | ~ spl4_17), inference(avatar_split_clause, [], [f930, f327, f344])).
fof(f930, plain, (~ (e0 = op(e2, e2)) | ~ spl4_17), inference(forward_demodulation, [], [f152, f329])).
fof(f929, plain, (~ spl4_17 | ~ spl4_19), inference(avatar_contradiction_clause, [], [f928])).
fof(f928, plain, ($false | (~ spl4_17 | ~ spl4_19)), inference(subsumption_resolution, [], [f927, f160])).
fof(f927, plain, ((e0 = e2) | (~ spl4_17 | ~ spl4_19)), inference(forward_demodulation, [], [f337, f329])).
fof(f916, plain, (~ spl4_33 | ~ spl4_2 | spl4_91), inference(avatar_split_clause, [], [f915, f740, f263, f395])).
fof(f915, plain, (~ (e0 = op(e1, e3)) | (~ spl4_2 | spl4_91)), inference(forward_demodulation, [], [f742, f265])).
fof(f742, plain, (~ (e0 = op(op(e3, e3), e3)) | spl4_91), inference(avatar_component_clause, [], [f740])).
fof(f914, plain, (~ spl4_2 | ~ spl4_3), inference(avatar_contradiction_clause, [], [f913])).
fof(f913, plain, ($false | (~ spl4_2 | ~ spl4_3)), inference(subsumption_resolution, [], [f912, f162])).
fof(f912, plain, ((e1 = e2) | (~ spl4_2 | ~ spl4_3)), inference(backward_demodulation, [], [f269, f265])).
fof(f911, plain, (~ spl4_3 | ~ spl4_17 | spl4_91), inference(avatar_contradiction_clause, [], [f910])).
fof(f910, plain, ($false | (~ spl4_3 | ~ spl4_17 | spl4_91)), inference(subsumption_resolution, [], [f904, f329])).
fof(f904, plain, (~ (e0 = op(e2, e3)) | (~ spl4_3 | spl4_91)), inference(backward_demodulation, [], [f742, f269])).
fof(f909, plain, (~ spl4_50 | ~ spl4_3 | ~ spl4_17 | spl4_90), inference(avatar_split_clause, [], [f908, f736, f327, f267, f467])).
fof(f908, plain, (~ (e1 = op(e0, e3)) | (~ spl4_3 | ~ spl4_17 | spl4_90)), inference(forward_demodulation, [], [f903, f329])).
fof(f903, plain, (~ (e1 = op(op(e2, e3), e3)) | (~ spl4_3 | spl4_90)), inference(backward_demodulation, [], [f738, f269])).
fof(f899, plain, (~ spl4_5 | ~ spl4_6), inference(avatar_contradiction_clause, [], [f898])).
fof(f898, plain, ($false | (~ spl4_5 | ~ spl4_6)), inference(subsumption_resolution, [], [f897, f159])).
fof(f897, plain, ((e0 = e1) | (~ spl4_5 | ~ spl4_6)), inference(backward_demodulation, [], [f282, f278])).
fof(f896, plain, (~ spl4_6 | ~ spl4_7), inference(avatar_contradiction_clause, [], [f895])).
fof(f895, plain, ($false | (~ spl4_6 | ~ spl4_7)), inference(subsumption_resolution, [], [f894, f162])).
fof(f894, plain, ((e1 = e2) | (~ spl4_6 | ~ spl4_7)), inference(backward_demodulation, [], [f286, f282])).
fof(f885, plain, (~ spl4_11 | ~ spl4_12), inference(avatar_contradiction_clause, [], [f884])).
fof(f884, plain, ($false | (~ spl4_11 | ~ spl4_12)), inference(subsumption_resolution, [], [f883, f164])).
fof(f883, plain, ((e2 = e3) | (~ spl4_11 | ~ spl4_12)), inference(backward_demodulation, [], [f307, f303])).
fof(f871, plain, (~ spl4_17 | ~ spl4_18), inference(avatar_contradiction_clause, [], [f870])).
fof(f870, plain, ($false | (~ spl4_17 | ~ spl4_18)), inference(subsumption_resolution, [], [f869, f159])).
fof(f869, plain, ((e0 = e1) | (~ spl4_17 | ~ spl4_18)), inference(backward_demodulation, [], [f333, f329])).
fof(f868, plain, (~ spl4_18 | ~ spl4_19), inference(avatar_contradiction_clause, [], [f867])).
fof(f867, plain, ($false | (~ spl4_18 | ~ spl4_19)), inference(subsumption_resolution, [], [f866, f162])).
fof(f866, plain, ((e1 = e2) | (~ spl4_18 | ~ spl4_19)), inference(backward_demodulation, [], [f337, f333])).
fof(f851, plain, (~ spl4_21 | ~ spl4_25), inference(avatar_split_clause, [], [f848, f361, f344])).
fof(f848, plain, (~ (e0 = op(e2, e2)) | ~ spl4_25), inference(backward_demodulation, [], [f150, f363])).
fof(f850, plain, (~ spl4_9 | ~ spl4_25), inference(avatar_split_clause, [], [f847, f361, f293])).
fof(f847, plain, (~ (e0 = op(e3, e1)) | ~ spl4_25), inference(backward_demodulation, [], [f122, f363])).
fof(f846, plain, (~ spl4_20 | ~ spl4_32), inference(avatar_split_clause, [], [f842, f390, f339])).
fof(f842, plain, (~ (e3 = op(e2, e3)) | ~ spl4_32), inference(backward_demodulation, [], [f149, f392])).
fof(f844, plain, (~ spl4_28 | ~ spl4_32), inference(avatar_split_clause, [], [f840, f390, f373])).
fof(f840, plain, (~ (e3 = op(e2, e1)) | ~ spl4_32), inference(backward_demodulation, [], [f147, f392])).
fof(f838, plain, (~ spl4_34 | ~ spl4_35), inference(avatar_contradiction_clause, [], [f837])).
fof(f837, plain, ($false | (~ spl4_34 | ~ spl4_35)), inference(subsumption_resolution, [], [f836, f162])).
fof(f836, plain, ((e1 = e2) | (~ spl4_34 | ~ spl4_35)), inference(backward_demodulation, [], [f405, f401])).
fof(f826, plain, (~ spl4_21 | ~ spl4_37), inference(avatar_split_clause, [], [f823, f412, f344])).
fof(f823, plain, (~ (e0 = op(e2, e2)) | ~ spl4_37), inference(backward_demodulation, [], [f126, f414])).
fof(f819, plain, (~ spl4_33 | ~ spl4_41), inference(avatar_split_clause, [], [f811, f429, f395])).
fof(f811, plain, (~ (e0 = op(e1, e3)) | ~ spl4_41), inference(backward_demodulation, [], [f145, f431])).
fof(f816, plain, (~ spl4_25 | ~ spl4_41), inference(avatar_split_clause, [], [f808, f429, f361])).
fof(f808, plain, (~ (e0 = op(e2, e1)) | ~ spl4_41), inference(backward_demodulation, [], [f120, f431])).
fof(f806, plain, (~ spl4_37 | ~ spl4_45), inference(avatar_split_clause, [], [f801, f446, f412])).
fof(f801, plain, (~ (e0 = op(e1, e2)) | ~ spl4_45), inference(backward_demodulation, [], [f142, f448])).
fof(f803, plain, (~ spl4_29 | ~ spl4_45), inference(avatar_split_clause, [], [f798, f446, f378])).
fof(f798, plain, (~ (e0 = op(e2, e0)) | ~ spl4_45), inference(backward_demodulation, [], [f114, f448])).
fof(f785, plain, (~ spl4_21 | ~ spl4_53), inference(avatar_split_clause, [], [f781, f480, f344])).
fof(f781, plain, (~ (e0 = op(e2, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f124, f482])).
fof(f776, plain, (~ spl4_25 | ~ spl4_57), inference(avatar_split_clause, [], [f771, f497, f361])).
fof(f771, plain, (~ (e0 = op(e2, e1)) | ~ spl4_57), inference(backward_demodulation, [], [f118, f499])).
fof(f775, plain, (~ spl4_41 | ~ spl4_57), inference(avatar_split_clause, [], [f770, f497, f429])).
fof(f770, plain, (~ (e0 = op(e1, e1)) | ~ spl4_57), inference(backward_demodulation, [], [f117, f499])).
fof(f767, plain, (~ spl4_47 | ~ spl4_62 | spl4_70), inference(avatar_split_clause, [], [f759, f636, f518, f454])).
fof(f759, plain, (~ (e2 = op(e1, e0)) | (~ spl4_62 | spl4_70)), inference(backward_demodulation, [], [f638, f520])).
fof(f766, plain, (~ spl4_50 | ~ spl4_62), inference(avatar_split_clause, [], [f756, f518, f467])).
fof(f756, plain, (~ (e1 = op(e0, e3)) | ~ spl4_62), inference(backward_demodulation, [], [f137, f520])).
fof(f765, plain, (~ spl4_54 | ~ spl4_62), inference(avatar_split_clause, [], [f755, f518, f484])).
fof(f755, plain, (~ (e1 = op(e0, e2)) | ~ spl4_62), inference(backward_demodulation, [], [f136, f520])).
fof(f762, plain, (~ spl4_30 | ~ spl4_62), inference(avatar_split_clause, [], [f752, f518, f382])).
fof(f752, plain, (~ (e1 = op(e2, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f112, f520])).
fof(f761, plain, (~ spl4_46 | ~ spl4_62), inference(avatar_split_clause, [], [f751, f518, f450])).
fof(f751, plain, (~ (e1 = op(e1, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f111, f520])).
fof(f750, plain, (~ spl4_92 | ~ spl4_88 | ~ spl4_1), inference(avatar_split_clause, [], [f257, f259, f726, f746])).
fof(f257, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(e3, e3)) & (e1 = op(op(e3, e3), e3)) & (e2 = op(op(op(e3, e3), e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax29)).
fof(f749, plain, (~ spl4_92 | ~ spl4_91 | ~ spl4_2), inference(avatar_split_clause, [], [f256, f263, f740, f746])).
fof(f256, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e1 = op(e3, e3)) & (e0 = op(op(e3, e3), e3)) & (e2 = op(op(op(e3, e3), e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax28)).
fof(f744, plain, (~ spl4_90 | ~ spl4_89 | ~ spl4_1), inference(avatar_split_clause, [], [f255, f259, f731, f736])).
fof(f255, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e0 = op(e3, e3)) & (e2 = op(op(e3, e3), e3)) & (e1 = op(op(op(e3, e3), e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax27)).
fof(f743, plain, (~ spl4_90 | ~ spl4_91 | ~ spl4_3), inference(avatar_split_clause, [], [f254, f267, f740, f736])).
fof(f254, plain, (~ (e2 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e2 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e2 = op(e3, e3)) & (e0 = op(op(e3, e3), e3)) & (e1 = op(op(op(e3, e3), e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax26)).
fof(f734, plain, (~ spl4_87 | ~ spl4_89 | ~ spl4_2), inference(avatar_split_clause, [], [f253, f263, f731, f722])).
fof(f253, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e1 = op(e3, e3)) & (e2 = op(op(e3, e3), e3)) & (e0 = op(op(op(e3, e3), e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax25)).
fof(f729, plain, (~ spl4_87 | ~ spl4_88 | ~ spl4_3), inference(avatar_split_clause, [], [f252, f267, f726, f722])).
fof(f252, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e2 = op(e3, e3)) & (e1 = op(op(e3, e3), e3)) & (e0 = op(op(op(e3, e3), e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax24)).
fof(f720, plain, (~ spl4_86 | ~ spl4_82 | ~ spl4_21), inference(avatar_split_clause, [], [f251, f344, f696, f716])).
fof(f251, plain, (~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e0 = op(e2, e2)) & (e1 = op(op(e2, e2), e2)) & (e3 = op(op(op(e2, e2), e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax23)).
fof(f719, plain, (~ spl4_86 | ~ spl4_85 | ~ spl4_22), inference(avatar_split_clause, [], [f250, f348, f710, f716])).
fof(f250, plain, (~ (e1 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e1 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e1 = op(e2, e2)) & (e0 = op(op(e2, e2), e2)) & (e3 = op(op(op(e2, e2), e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax22)).
fof(f714, plain, (~ spl4_84 | ~ spl4_83 | ~ spl4_21), inference(avatar_split_clause, [], [f249, f344, f701, f706])).
fof(f249, plain, (~ (e0 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2)) | ~ (e1 = op(op(op(e2, e2), e2), e2))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2)) | ~ (e1 = op(op(op(e2, e2), e2), e2))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e2, e2)) & (e3 = op(op(e2, e2), e2)) & (e1 = op(op(op(e2, e2), e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax21)).
fof(f713, plain, (~ spl4_84 | ~ spl4_85 | ~ spl4_24), inference(avatar_split_clause, [], [f248, f356, f710, f706])).
fof(f248, plain, (~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), e2)) | ~ (e1 = op(op(op(e2, e2), e2), e2))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), e2)) | ~ (e1 = op(op(op(e2, e2), e2), e2))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e3 = op(e2, e2)) & (e0 = op(op(e2, e2), e2)) & (e1 = op(op(op(e2, e2), e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax20)).
fof(f704, plain, (~ spl4_81 | ~ spl4_83 | ~ spl4_22), inference(avatar_split_clause, [], [f247, f348, f701, f692])).
fof(f247, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2)) | ~ (e0 = op(op(op(e2, e2), e2), e2))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2)) | ~ (e0 = op(op(op(e2, e2), e2), e2))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e2, e2)) & (e3 = op(op(e2, e2), e2)) & (e0 = op(op(op(e2, e2), e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax19)).
fof(f699, plain, (~ spl4_81 | ~ spl4_82 | ~ spl4_24), inference(avatar_split_clause, [], [f246, f356, f696, f692])).
fof(f246, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e0 = op(op(op(e2, e2), e2), e2))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e0 = op(op(op(e2, e2), e2), e2))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e2, e2)) & (e1 = op(op(e2, e2), e2)) & (e0 = op(op(op(e2, e2), e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax18)).
fof(f690, plain, (~ spl4_80 | ~ spl4_76 | ~ spl4_41), inference(avatar_split_clause, [], [f245, f429, f666, f686])).
fof(f245, plain, (~ (e0 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), e1)) | ~ (e3 = op(op(op(e1, e1), e1), e1))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e0 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), e1)) | ~ (e3 = op(op(op(e1, e1), e1), e1))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e0 = op(e1, e1)) & (e2 = op(op(e1, e1), e1)) & (e3 = op(op(op(e1, e1), e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax17)).
fof(f689, plain, (~ spl4_80 | ~ spl4_79 | ~ spl4_43), inference(avatar_split_clause, [], [f244, f437, f680, f686])).
fof(f244, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1)) | ~ (e3 = op(op(op(e1, e1), e1), e1))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1)) | ~ (e3 = op(op(op(e1, e1), e1), e1))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e2 = op(e1, e1)) & (e0 = op(op(e1, e1), e1)) & (e3 = op(op(op(e1, e1), e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax16)).
fof(f684, plain, (~ spl4_78 | ~ spl4_77 | ~ spl4_41), inference(avatar_split_clause, [], [f243, f429, f671, f676])).
fof(f243, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1)) | ~ (e2 = op(op(op(e1, e1), e1), e1))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1)) | ~ (e2 = op(op(op(e1, e1), e1), e1))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e1, e1)) & (e3 = op(op(e1, e1), e1)) & (e2 = op(op(op(e1, e1), e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax15)).
fof(f683, plain, (~ spl4_78 | ~ spl4_79 | ~ spl4_44), inference(avatar_split_clause, [], [f242, f441, f680, f676])).
fof(f242, plain, (~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1)) | ~ (e2 = op(op(op(e1, e1), e1), e1))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1)) | ~ (e2 = op(op(op(e1, e1), e1), e1))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e3 = op(e1, e1)) & (e0 = op(op(e1, e1), e1)) & (e2 = op(op(op(e1, e1), e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax14)).
fof(f674, plain, (~ spl4_75 | ~ spl4_77 | ~ spl4_43), inference(avatar_split_clause, [], [f241, f437, f671, f662])).
fof(f241, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1)) | ~ (e0 = op(op(op(e1, e1), e1), e1))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1)) | ~ (e0 = op(op(op(e1, e1), e1), e1))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e2 = op(e1, e1)) & (e3 = op(op(e1, e1), e1)) & (e0 = op(op(op(e1, e1), e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax13)).
fof(f669, plain, (~ spl4_75 | ~ spl4_76 | ~ spl4_44), inference(avatar_split_clause, [], [f240, f441, f666, f662])).
fof(f240, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), e1)) | ~ (e0 = op(op(op(e1, e1), e1), e1))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), e1)) | ~ (e0 = op(op(op(e1, e1), e1), e1))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e3 = op(e1, e1)) & (e2 = op(op(e1, e1), e1)) & (e0 = op(op(op(e1, e1), e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax12)).
fof(f660, plain, (~ spl4_74 | ~ spl4_70 | ~ spl4_62), inference(avatar_split_clause, [], [f239, f518, f636, f656])).
fof(f239, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((op(e0, e0) = e1) & (e2 = op(op(e0, e0), e0)) & (e3 = op(op(op(e0, e0), e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax11)).
fof(f659, plain, (~ spl4_74 | ~ spl4_73 | ~ spl4_63), inference(avatar_split_clause, [], [f238, f522, f650, f656])).
fof(f238, plain, (~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((op(e0, e0) = e2) & (e1 = op(op(e0, e0), e0)) & (e3 = op(op(op(e0, e0), e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax10)).
fof(f654, plain, (~ spl4_72 | ~ spl4_71 | ~ spl4_62), inference(avatar_split_clause, [], [f237, f518, f641, f646])).
fof(f237, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), e0)) | ~ (e2 = op(op(op(e0, e0), e0), e0))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), e0)) | ~ (e2 = op(op(op(e0, e0), e0), e0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((op(e0, e0) = e1) & (e3 = op(op(e0, e0), e0)) & (e2 = op(op(op(e0, e0), e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax9)).
fof(f653, plain, (~ spl4_72 | ~ spl4_73 | ~ spl4_64), inference(avatar_split_clause, [], [f236, f526, f650, f646])).
fof(f236, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), e0)) | ~ (e2 = op(op(op(e0, e0), e0), e0))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), e0)) | ~ (e2 = op(op(op(e0, e0), e0), e0))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((op(e0, e0) = e3) & (e1 = op(op(e0, e0), e0)) & (e2 = op(op(op(e0, e0), e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax8)).
fof(f644, plain, (~ spl4_69 | ~ spl4_71 | ~ spl4_63), inference(avatar_split_clause, [], [f235, f522, f641, f632])).
fof(f235, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), e0)) | ~ (e1 = op(op(op(e0, e0), e0), e0))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), e0)) | ~ (e1 = op(op(op(e0, e0), e0), e0))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((op(e0, e0) = e2) & (e3 = op(op(e0, e0), e0)) & (e1 = op(op(op(e0, e0), e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax7)).
fof(f639, plain, (~ spl4_69 | ~ spl4_70 | ~ spl4_64), inference(avatar_split_clause, [], [f234, f526, f636, f632])).
fof(f234, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), e0)) | ~ (e1 = op(op(op(e0, e0), e0), e0))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), e0)) | ~ (e1 = op(op(op(e0, e0), e0), e0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((op(e0, e0) = e3) & (e2 = op(op(e0, e0), e0)) & (e1 = op(op(op(e0, e0), e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax6)).
fof(f630, plain, ~ spl4_61, inference(avatar_split_clause, [], [f229, f514])).
fof(f514, plain, (spl4_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_61])])).
fof(f229, plain, ~ (e0 = op(e0, e0)), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((sP3 | sP2 | sP1 | sP0) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0))), inference(definition_folding, [], [f5, e57, e56, e55, e54])).
fof(f54, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e3, e0)) | ~ (e0 = op(e3, e2))) & ((e1 = op(e3, e0)) | ~ (e0 = op(e3, e1))) & ((e0 = op(e3, e0)) | ~ (e0 = op(e3, e0))) & ((e3 = op(e2, e0)) | ~ (e0 = op(e2, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e2, e0)) | ~ (e0 = op(e2, e1))) & ((e0 = op(e2, e0)) | ~ (e0 = op(e2, e0))) & ((e3 = op(e1, e0)) | ~ (e0 = op(e1, e3))) & ((e2 = op(e1, e0)) | ~ (e0 = op(e1, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e1, e0)) | ~ (e0 = op(e1, e0))) & ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e3, e0)) | ~ (e0 = op(e3, e2))) & ((e1 = op(e3, e0)) | ~ (e0 = op(e3, e1))) & ((e0 = op(e3, e0)) | ~ (e0 = op(e3, e0))) & ((e3 = op(e2, e0)) | ~ (e0 = op(e2, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e2, e0)) | ~ (e0 = op(e2, e1))) & ((e0 = op(e2, e0)) | ~ (e0 = op(e2, e0))) & ((e3 = op(e1, e0)) | ~ (e0 = op(e1, e3))) & ((e2 = op(e1, e0)) | ~ (e0 = op(e1, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e1, e0)) | ~ (e0 = op(e1, e0))) & ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e3, e1)) | ~ (e1 = op(e3, e2))) & ((e1 = op(e3, e1)) | ~ (e1 = op(e3, e1))) & ((e0 = op(e3, e1)) | ~ (e1 = op(e3, e0))) & ((e3 = op(e2, e1)) | ~ (e1 = op(e2, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e2, e1)) | ~ (e1 = op(e2, e1))) & ((e0 = op(e2, e1)) | ~ (e1 = op(e2, e0))) & ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e3 = op(e0, e1)) | ~ (e1 = op(e0, e3))) & ((e2 = op(e0, e1)) | ~ (e1 = op(e0, e2))) & ((e1 = op(e0, e1)) | ~ (e1 = op(e0, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e3, e1)) | ~ (e1 = op(e3, e2))) & ((e1 = op(e3, e1)) | ~ (e1 = op(e3, e1))) & ((e0 = op(e3, e1)) | ~ (e1 = op(e3, e0))) & ((e3 = op(e2, e1)) | ~ (e1 = op(e2, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e2, e1)) | ~ (e1 = op(e2, e1))) & ((e0 = op(e2, e1)) | ~ (e1 = op(e2, e0))) & ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e3 = op(e0, e1)) | ~ (e1 = op(e0, e3))) & ((e2 = op(e0, e1)) | ~ (e1 = op(e0, e2))) & ((e1 = op(e0, e1)) | ~ (e1 = op(e0, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e3, e2)) | ~ (e2 = op(e3, e2))) & ((e1 = op(e3, e2)) | ~ (e2 = op(e3, e1))) & ((e0 = op(e3, e2)) | ~ (e2 = op(e3, e0))) & ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e3 = op(e1, e2)) | ~ (e2 = op(e1, e3))) & ((e2 = op(e1, e2)) | ~ (e2 = op(e1, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e1, e2)) | ~ (e2 = op(e1, e0))) & ((e3 = op(e0, e2)) | ~ (e2 = op(e0, e3))) & ((e2 = op(e0, e2)) | ~ (e2 = op(e0, e2))) & ((e1 = op(e0, e2)) | ~ (e2 = op(e0, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e3, e2)) | ~ (e2 = op(e3, e2))) & ((e1 = op(e3, e2)) | ~ (e2 = op(e3, e1))) & ((e0 = op(e3, e2)) | ~ (e2 = op(e3, e0))) & ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e3 = op(e1, e2)) | ~ (e2 = op(e1, e3))) & ((e2 = op(e1, e2)) | ~ (e2 = op(e1, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e1, e2)) | ~ (e2 = op(e1, e0))) & ((e3 = op(e0, e2)) | ~ (e2 = op(e0, e3))) & ((e2 = op(e0, e2)) | ~ (e2 = op(e0, e2))) & ((e1 = op(e0, e2)) | ~ (e2 = op(e0, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e3 = op(e2, e3)) | ~ (e3 = op(e2, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e2, e3)) | ~ (e3 = op(e2, e1))) & ((e0 = op(e2, e3)) | ~ (e3 = op(e2, e0))) & ((e3 = op(e1, e3)) | ~ (e3 = op(e1, e3))) & ((e2 = op(e1, e3)) | ~ (e3 = op(e1, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e1, e3)) | ~ (e3 = op(e1, e0))) & ((e3 = op(e0, e3)) | ~ (e3 = op(e0, e3))) & ((e2 = op(e0, e3)) | ~ (e3 = op(e0, e2))) & ((e1 = op(e0, e3)) | ~ (e3 = op(e0, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e3 = op(e2, e3)) | ~ (e3 = op(e2, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e2, e3)) | ~ (e3 = op(e2, e1))) & ((e0 = op(e2, e3)) | ~ (e3 = op(e2, e0))) & ((e3 = op(e1, e3)) | ~ (e3 = op(e1, e3))) & ((e2 = op(e1, e3)) | ~ (e3 = op(e1, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e1, e3)) | ~ (e3 = op(e1, e0))) & ((e3 = op(e0, e3)) | ~ (e3 = op(e0, e3))) & ((e2 = op(e0, e3)) | ~ (e3 = op(e0, e2))) & ((e1 = op(e0, e3)) | ~ (e3 = op(e0, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f5, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e3 = op(e2, e3)) | ~ (e3 = op(e2, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e2, e3)) | ~ (e3 = op(e2, e1))) & ((e0 = op(e2, e3)) | ~ (e3 = op(e2, e0))) & ((e3 = op(e1, e3)) | ~ (e3 = op(e1, e3))) & ((e2 = op(e1, e3)) | ~ (e3 = op(e1, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e1, e3)) | ~ (e3 = op(e1, e0))) & ((e3 = op(e0, e3)) | ~ (e3 = op(e0, e3))) & ((e2 = op(e0, e3)) | ~ (e3 = op(e0, e2))) & ((e1 = op(e0, e3)) | ~ (e3 = op(e0, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3))) | (((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e3, e2)) | ~ (e2 = op(e3, e2))) & ((e1 = op(e3, e2)) | ~ (e2 = op(e3, e1))) & ((e0 = op(e3, e2)) | ~ (e2 = op(e3, e0))) & ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e3 = op(e1, e2)) | ~ (e2 = op(e1, e3))) & ((e2 = op(e1, e2)) | ~ (e2 = op(e1, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e1, e2)) | ~ (e2 = op(e1, e0))) & ((e3 = op(e0, e2)) | ~ (e2 = op(e0, e3))) & ((e2 = op(e0, e2)) | ~ (e2 = op(e0, e2))) & ((e1 = op(e0, e2)) | ~ (e2 = op(e0, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2))) | (((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e3, e1)) | ~ (e1 = op(e3, e2))) & ((e1 = op(e3, e1)) | ~ (e1 = op(e3, e1))) & ((e0 = op(e3, e1)) | ~ (e1 = op(e3, e0))) & ((e3 = op(e2, e1)) | ~ (e1 = op(e2, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e2, e1)) | ~ (e1 = op(e2, e1))) & ((e0 = op(e2, e1)) | ~ (e1 = op(e2, e0))) & ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e3 = op(e0, e1)) | ~ (e1 = op(e0, e3))) & ((e2 = op(e0, e1)) | ~ (e1 = op(e0, e2))) & ((e1 = op(e0, e1)) | ~ (e1 = op(e0, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1))) | (((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e3, e0)) | ~ (e0 = op(e3, e2))) & ((e1 = op(e3, e0)) | ~ (e0 = op(e3, e1))) & ((e0 = op(e3, e0)) | ~ (e0 = op(e3, e0))) & ((e3 = op(e2, e0)) | ~ (e0 = op(e2, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e2, e0)) | ~ (e0 = op(e2, e1))) & ((e0 = op(e2, e0)) | ~ (e0 = op(e2, e0))) & ((e3 = op(e1, e0)) | ~ (e0 = op(e1, e3))) & ((e2 = op(e1, e0)) | ~ (e0 = op(e1, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e1, e0)) | ~ (e0 = op(e1, e0))) & ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))))) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax5)).
fof(f629, plain, ~ spl4_42, inference(avatar_split_clause, [], [f230, f433])).
fof(f433, plain, (spl4_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_42])])).
fof(f230, plain, ~ (e1 = op(e1, e1)), inference(cnf_transformation, [], [f58])).
fof(f628, plain, ~ spl4_23, inference(avatar_split_clause, [], [f231, f352])).
fof(f352, plain, (spl4_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_23])])).
fof(f231, plain, ~ (e2 = op(e2, e2)), inference(cnf_transformation, [], [f58])).
fof(f627, plain, ~ spl4_4, inference(avatar_split_clause, [], [f232, f271])).
fof(f271, plain, (spl4_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_4])])).
fof(f232, plain, ~ (e3 = op(e3, e3)), inference(cnf_transformation, [], [f58])).
fof(f626, plain, (spl4_68 | spl4_67 | spl4_66 | spl4_65), inference(avatar_split_clause, [], [f233, f563, f579, f595, f611])).
fof(f611, plain, (spl4_68 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl4_68])])).
fof(f595, plain, (spl4_67 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl4_67])])).
fof(f579, plain, (spl4_66 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl4_66])])).
fof(f563, plain, (spl4_65 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl4_65])])).
fof(f233, plain, (sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f58])).
fof(f625, plain, (~ spl4_68 | ~ spl4_57 | spl4_62), inference(avatar_split_clause, [], [f214, f518, f497, f611])).
fof(f214, plain, ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e3, e0)) | ~ (e0 = op(e3, e2))) & ((e1 = op(e3, e0)) | ~ (e0 = op(e3, e1))) & ((e0 = op(e3, e0)) | ~ (e0 = op(e3, e0))) & ((e3 = op(e2, e0)) | ~ (e0 = op(e2, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e2, e0)) | ~ (e0 = op(e2, e1))) & ((e0 = op(e2, e0)) | ~ (e0 = op(e2, e0))) & ((e3 = op(e1, e0)) | ~ (e0 = op(e1, e3))) & ((e2 = op(e1, e0)) | ~ (e0 = op(e1, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e1, e0)) | ~ (e0 = op(e1, e0))) & ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f624, plain, (~ spl4_68 | ~ spl4_53 | spl4_63), inference(avatar_split_clause, [], [f215, f522, f480, f611])).
fof(f215, plain, ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f623, plain, (~ spl4_68 | ~ spl4_49 | spl4_64), inference(avatar_split_clause, [], [f216, f526, f463, f611])).
fof(f216, plain, ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f622, plain, (~ spl4_68 | ~ spl4_41 | spl4_46), inference(avatar_split_clause, [], [f218, f450, f429, f611])).
fof(f218, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f621, plain, (~ spl4_68 | ~ spl4_37 | spl4_47), inference(avatar_split_clause, [], [f219, f454, f412, f611])).
fof(f219, plain, ((e2 = op(e1, e0)) | ~ (e0 = op(e1, e2)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f619, plain, (~ spl4_68 | ~ spl4_25 | spl4_30), inference(avatar_split_clause, [], [f222, f382, f361, f611])).
fof(f222, plain, ((e1 = op(e2, e0)) | ~ (e0 = op(e2, e1)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f618, plain, (~ spl4_68 | ~ spl4_21 | spl4_31), inference(avatar_split_clause, [], [f223, f386, f344, f611])).
fof(f223, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f616, plain, (~ spl4_68 | ~ spl4_9 | spl4_14), inference(avatar_split_clause, [], [f226, f314, f293, f611])).
fof(f226, plain, ((e1 = op(e3, e0)) | ~ (e0 = op(e3, e1)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f615, plain, (~ spl4_68 | ~ spl4_5 | spl4_15), inference(avatar_split_clause, [], [f227, f318, f276, f611])).
fof(f227, plain, ((e2 = op(e3, e0)) | ~ (e0 = op(e3, e2)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f614, plain, (~ spl4_68 | ~ spl4_1 | spl4_16), inference(avatar_split_clause, [], [f228, f322, f259, f611])).
fof(f228, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f609, plain, (~ spl4_67 | ~ spl4_62 | spl4_57), inference(avatar_split_clause, [], [f197, f497, f518, f595])).
fof(f197, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e3, e1)) | ~ (e1 = op(e3, e2))) & ((e1 = op(e3, e1)) | ~ (e1 = op(e3, e1))) & ((e0 = op(e3, e1)) | ~ (e1 = op(e3, e0))) & ((e3 = op(e2, e1)) | ~ (e1 = op(e2, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e2, e1)) | ~ (e1 = op(e2, e1))) & ((e0 = op(e2, e1)) | ~ (e1 = op(e2, e0))) & ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e3 = op(e0, e1)) | ~ (e1 = op(e0, e3))) & ((e2 = op(e0, e1)) | ~ (e1 = op(e0, e2))) & ((e1 = op(e0, e1)) | ~ (e1 = op(e0, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f608, plain, (~ spl4_67 | ~ spl4_54 | spl4_59), inference(avatar_split_clause, [], [f199, f505, f484, f595])).
fof(f199, plain, ((e2 = op(e0, e1)) | ~ (e1 = op(e0, e2)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f607, plain, (~ spl4_67 | ~ spl4_50 | spl4_60), inference(avatar_split_clause, [], [f200, f509, f467, f595])).
fof(f200, plain, ((e3 = op(e0, e1)) | ~ (e1 = op(e0, e3)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f606, plain, (~ spl4_67 | ~ spl4_46 | spl4_41), inference(avatar_split_clause, [], [f201, f429, f450, f595])).
fof(f201, plain, ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f605, plain, (~ spl4_67 | ~ spl4_38 | spl4_43), inference(avatar_split_clause, [], [f203, f437, f416, f595])).
fof(f203, plain, ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f604, plain, (~ spl4_67 | ~ spl4_34 | spl4_44), inference(avatar_split_clause, [], [f204, f441, f399, f595])).
fof(f204, plain, ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f603, plain, (~ spl4_67 | ~ spl4_30 | spl4_25), inference(avatar_split_clause, [], [f205, f361, f382, f595])).
fof(f205, plain, ((e0 = op(e2, e1)) | ~ (e1 = op(e2, e0)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f602, plain, (~ spl4_67 | ~ spl4_22 | spl4_27), inference(avatar_split_clause, [], [f207, f369, f348, f595])).
fof(f207, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f601, plain, (~ spl4_67 | ~ spl4_18 | spl4_28), inference(avatar_split_clause, [], [f208, f373, f331, f595])).
fof(f208, plain, ((e3 = op(e2, e1)) | ~ (e1 = op(e2, e3)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f600, plain, (~ spl4_67 | ~ spl4_14 | spl4_9), inference(avatar_split_clause, [], [f209, f293, f314, f595])).
fof(f209, plain, ((e0 = op(e3, e1)) | ~ (e1 = op(e3, e0)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f599, plain, (~ spl4_67 | ~ spl4_6 | spl4_11), inference(avatar_split_clause, [], [f211, f301, f280, f595])).
fof(f211, plain, ((e2 = op(e3, e1)) | ~ (e1 = op(e3, e2)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f598, plain, (~ spl4_67 | ~ spl4_2 | spl4_12), inference(avatar_split_clause, [], [f212, f305, f263, f595])).
fof(f212, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f593, plain, (~ spl4_66 | ~ spl4_63 | spl4_53), inference(avatar_split_clause, [], [f181, f480, f522, f579])).
fof(f181, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e3, e2)) | ~ (e2 = op(e3, e2))) & ((e1 = op(e3, e2)) | ~ (e2 = op(e3, e1))) & ((e0 = op(e3, e2)) | ~ (e2 = op(e3, e0))) & ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e3 = op(e1, e2)) | ~ (e2 = op(e1, e3))) & ((e2 = op(e1, e2)) | ~ (e2 = op(e1, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e1, e2)) | ~ (e2 = op(e1, e0))) & ((e3 = op(e0, e2)) | ~ (e2 = op(e0, e3))) & ((e2 = op(e0, e2)) | ~ (e2 = op(e0, e2))) & ((e1 = op(e0, e2)) | ~ (e2 = op(e0, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f592, plain, (~ spl4_66 | ~ spl4_59 | spl4_54), inference(avatar_split_clause, [], [f182, f484, f505, f579])).
fof(f182, plain, ((e1 = op(e0, e2)) | ~ (e2 = op(e0, e1)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f591, plain, (~ spl4_66 | ~ spl4_51 | spl4_56), inference(avatar_split_clause, [], [f184, f492, f471, f579])).
fof(f184, plain, ((e3 = op(e0, e2)) | ~ (e2 = op(e0, e3)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f590, plain, (~ spl4_66 | ~ spl4_47 | spl4_37), inference(avatar_split_clause, [], [f185, f412, f454, f579])).
fof(f185, plain, ((e0 = op(e1, e2)) | ~ (e2 = op(e1, e0)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f589, plain, (~ spl4_66 | ~ spl4_43 | spl4_38), inference(avatar_split_clause, [], [f186, f416, f437, f579])).
fof(f186, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f588, plain, (~ spl4_66 | ~ spl4_35 | spl4_40), inference(avatar_split_clause, [], [f188, f424, f403, f579])).
fof(f188, plain, ((e3 = op(e1, e2)) | ~ (e2 = op(e1, e3)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f587, plain, (~ spl4_66 | ~ spl4_31 | spl4_21), inference(avatar_split_clause, [], [f189, f344, f386, f579])).
fof(f189, plain, ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f586, plain, (~ spl4_66 | ~ spl4_27 | spl4_22), inference(avatar_split_clause, [], [f190, f348, f369, f579])).
fof(f190, plain, ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f585, plain, (~ spl4_66 | ~ spl4_19 | spl4_24), inference(avatar_split_clause, [], [f192, f356, f335, f579])).
fof(f192, plain, ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f584, plain, (~ spl4_66 | ~ spl4_15 | spl4_5), inference(avatar_split_clause, [], [f193, f276, f318, f579])).
fof(f193, plain, ((e0 = op(e3, e2)) | ~ (e2 = op(e3, e0)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f583, plain, (~ spl4_66 | ~ spl4_11 | spl4_6), inference(avatar_split_clause, [], [f194, f280, f301, f579])).
fof(f194, plain, ((e1 = op(e3, e2)) | ~ (e2 = op(e3, e1)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f582, plain, (~ spl4_66 | ~ spl4_3 | spl4_8), inference(avatar_split_clause, [], [f196, f288, f267, f579])).
fof(f196, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f577, plain, (~ spl4_65 | ~ spl4_64 | spl4_49), inference(avatar_split_clause, [], [f165, f463, f526, f563])).
fof(f165, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e3 = op(e2, e3)) | ~ (e3 = op(e2, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e2, e3)) | ~ (e3 = op(e2, e1))) & ((e0 = op(e2, e3)) | ~ (e3 = op(e2, e0))) & ((e3 = op(e1, e3)) | ~ (e3 = op(e1, e3))) & ((e2 = op(e1, e3)) | ~ (e3 = op(e1, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e1, e3)) | ~ (e3 = op(e1, e0))) & ((e3 = op(e0, e3)) | ~ (e3 = op(e0, e3))) & ((e2 = op(e0, e3)) | ~ (e3 = op(e0, e2))) & ((e1 = op(e0, e3)) | ~ (e3 = op(e0, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f576, plain, (~ spl4_65 | ~ spl4_60 | spl4_50), inference(avatar_split_clause, [], [f166, f467, f509, f563])).
fof(f166, plain, ((e1 = op(e0, e3)) | ~ (e3 = op(e0, e1)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f575, plain, (~ spl4_65 | ~ spl4_56 | spl4_51), inference(avatar_split_clause, [], [f167, f471, f492, f563])).
fof(f167, plain, ((e2 = op(e0, e3)) | ~ (e3 = op(e0, e2)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f574, plain, (~ spl4_65 | ~ spl4_48 | spl4_33), inference(avatar_split_clause, [], [f169, f395, f458, f563])).
fof(f169, plain, ((e0 = op(e1, e3)) | ~ (e3 = op(e1, e0)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f573, plain, (~ spl4_65 | ~ spl4_44 | spl4_34), inference(avatar_split_clause, [], [f170, f399, f441, f563])).
fof(f170, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f572, plain, (~ spl4_65 | ~ spl4_40 | spl4_35), inference(avatar_split_clause, [], [f171, f403, f424, f563])).
fof(f171, plain, ((e2 = op(e1, e3)) | ~ (e3 = op(e1, e2)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f571, plain, (~ spl4_65 | ~ spl4_32 | spl4_17), inference(avatar_split_clause, [], [f173, f327, f390, f563])).
fof(f173, plain, ((e0 = op(e2, e3)) | ~ (e3 = op(e2, e0)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f569, plain, (~ spl4_65 | ~ spl4_24 | spl4_19), inference(avatar_split_clause, [], [f175, f335, f356, f563])).
fof(f175, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f568, plain, (~ spl4_65 | ~ spl4_16 | spl4_1), inference(avatar_split_clause, [], [f177, f259, f322, f563])).
fof(f177, plain, ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f567, plain, (~ spl4_65 | ~ spl4_12 | spl4_2), inference(avatar_split_clause, [], [f178, f263, f305, f563])).
fof(f178, plain, ((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f566, plain, (~ spl4_65 | ~ spl4_8 | spl4_3), inference(avatar_split_clause, [], [f179, f267, f288, f563])).
fof(f179, plain, ((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f561, plain, (spl4_61 | spl4_57 | spl4_53 | spl4_49), inference(avatar_split_clause, [], [f79, f463, f480, f497, f514])).
fof(f79, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax2)).
fof(f560, plain, (spl4_61 | spl4_45 | spl4_29 | spl4_13), inference(avatar_split_clause, [], [f80, f310, f378, f446, f514])).
fof(f80, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f559, plain, (spl4_62 | spl4_58 | spl4_54 | spl4_50), inference(avatar_split_clause, [], [f81, f467, f484, f501, f518])).
fof(f81, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f558, plain, (spl4_62 | spl4_46 | spl4_30 | spl4_14), inference(avatar_split_clause, [], [f82, f314, f382, f450, f518])).
fof(f82, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f557, plain, (spl4_63 | spl4_59 | spl4_55 | spl4_51), inference(avatar_split_clause, [], [f83, f471, f488, f505, f522])).
fof(f83, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f556, plain, (spl4_63 | spl4_47 | spl4_31 | spl4_15), inference(avatar_split_clause, [], [f84, f318, f386, f454, f522])).
fof(f84, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f555, plain, (spl4_64 | spl4_60 | spl4_56 | spl4_52), inference(avatar_split_clause, [], [f85, f475, f492, f509, f526])).
fof(f85, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f554, plain, (spl4_64 | spl4_48 | spl4_32 | spl4_16), inference(avatar_split_clause, [], [f86, f322, f390, f458, f526])).
fof(f86, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f553, plain, (spl4_45 | spl4_41 | spl4_37 | spl4_33), inference(avatar_split_clause, [], [f87, f395, f412, f429, f446])).
fof(f87, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f552, plain, (spl4_57 | spl4_41 | spl4_25 | spl4_9), inference(avatar_split_clause, [], [f88, f293, f361, f429, f497])).
fof(f88, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f551, plain, (spl4_46 | spl4_42 | spl4_38 | spl4_34), inference(avatar_split_clause, [], [f89, f399, f416, f433, f450])).
fof(f89, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f550, plain, (spl4_58 | spl4_42 | spl4_26 | spl4_10), inference(avatar_split_clause, [], [f90, f297, f365, f433, f501])).
fof(f90, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f549, plain, (spl4_47 | spl4_43 | spl4_39 | spl4_35), inference(avatar_split_clause, [], [f91, f403, f420, f437, f454])).
fof(f91, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f548, plain, (spl4_59 | spl4_43 | spl4_27 | spl4_11), inference(avatar_split_clause, [], [f92, f301, f369, f437, f505])).
fof(f92, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f547, plain, (spl4_48 | spl4_44 | spl4_40 | spl4_36), inference(avatar_split_clause, [], [f93, f407, f424, f441, f458])).
fof(f93, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f546, plain, (spl4_60 | spl4_44 | spl4_28 | spl4_12), inference(avatar_split_clause, [], [f94, f305, f373, f441, f509])).
fof(f94, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f545, plain, (spl4_29 | spl4_25 | spl4_21 | spl4_17), inference(avatar_split_clause, [], [f95, f327, f344, f361, f378])).
fof(f95, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f544, plain, (spl4_53 | spl4_37 | spl4_21 | spl4_5), inference(avatar_split_clause, [], [f96, f276, f344, f412, f480])).
fof(f96, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f543, plain, (spl4_30 | spl4_26 | spl4_22 | spl4_18), inference(avatar_split_clause, [], [f97, f331, f348, f365, f382])).
fof(f97, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f542, plain, (spl4_54 | spl4_38 | spl4_22 | spl4_6), inference(avatar_split_clause, [], [f98, f280, f348, f416, f484])).
fof(f98, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f541, plain, (spl4_31 | spl4_27 | spl4_23 | spl4_19), inference(avatar_split_clause, [], [f99, f335, f352, f369, f386])).
fof(f99, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f540, plain, (spl4_55 | spl4_39 | spl4_23 | spl4_7), inference(avatar_split_clause, [], [f100, f284, f352, f420, f488])).
fof(f100, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f539, plain, (spl4_32 | spl4_28 | spl4_24 | spl4_20), inference(avatar_split_clause, [], [f101, f339, f356, f373, f390])).
fof(f101, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f538, plain, (spl4_56 | spl4_40 | spl4_24 | spl4_8), inference(avatar_split_clause, [], [f102, f288, f356, f424, f492])).
fof(f102, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f537, plain, (spl4_13 | spl4_9 | spl4_5 | spl4_1), inference(avatar_split_clause, [], [f103, f259, f276, f293, f310])).
fof(f103, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f536, plain, (spl4_49 | spl4_33 | spl4_17 | spl4_1), inference(avatar_split_clause, [], [f104, f259, f327, f395, f463])).
fof(f104, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f535, plain, (spl4_14 | spl4_10 | spl4_6 | spl4_2), inference(avatar_split_clause, [], [f105, f263, f280, f297, f314])).
fof(f105, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f534, plain, (spl4_50 | spl4_34 | spl4_18 | spl4_2), inference(avatar_split_clause, [], [f106, f263, f331, f399, f467])).
fof(f106, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f533, plain, (spl4_15 | spl4_11 | spl4_7 | spl4_3), inference(avatar_split_clause, [], [f107, f267, f284, f301, f318])).
fof(f107, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f532, plain, (spl4_51 | spl4_35 | spl4_19 | spl4_3), inference(avatar_split_clause, [], [f108, f267, f335, f403, f471])).
fof(f108, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f531, plain, (spl4_16 | spl4_12 | spl4_8 | spl4_4), inference(avatar_split_clause, [], [f109, f271, f288, f305, f322])).
fof(f109, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f530, plain, (spl4_52 | spl4_36 | spl4_20 | spl4_4), inference(avatar_split_clause, [], [f110, f271, f339, f407, f475])).
fof(f110, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f529, plain, (spl4_61 | spl4_62 | spl4_63 | spl4_64), inference(avatar_split_clause, [], [f63, f526, f522, f518, f514])).
fof(f63, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG149+1.p', ax1)).
fof(f512, plain, (spl4_57 | spl4_58 | spl4_59 | spl4_60), inference(avatar_split_clause, [], [f64, f509, f505, f501, f497])).
fof(f64, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f495, plain, (spl4_53 | spl4_54 | spl4_55 | spl4_56), inference(avatar_split_clause, [], [f65, f492, f488, f484, f480])).
fof(f65, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f478, plain, (spl4_49 | spl4_50 | spl4_51 | spl4_52), inference(avatar_split_clause, [], [f66, f475, f471, f467, f463])).
fof(f66, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f461, plain, (spl4_45 | spl4_46 | spl4_47 | spl4_48), inference(avatar_split_clause, [], [f67, f458, f454, f450, f446])).
fof(f67, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f444, plain, (spl4_41 | spl4_42 | spl4_43 | spl4_44), inference(avatar_split_clause, [], [f68, f441, f437, f433, f429])).
fof(f68, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f427, plain, (spl4_37 | spl4_38 | spl4_39 | spl4_40), inference(avatar_split_clause, [], [f69, f424, f420, f416, f412])).
fof(f69, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f410, plain, (spl4_33 | spl4_34 | spl4_35 | spl4_36), inference(avatar_split_clause, [], [f70, f407, f403, f399, f395])).
fof(f70, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f393, plain, (spl4_29 | spl4_30 | spl4_31 | spl4_32), inference(avatar_split_clause, [], [f71, f390, f386, f382, f378])).
fof(f71, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f376, plain, (spl4_25 | spl4_26 | spl4_27 | spl4_28), inference(avatar_split_clause, [], [f72, f373, f369, f365, f361])).
fof(f72, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f359, plain, (spl4_21 | spl4_22 | spl4_23 | spl4_24), inference(avatar_split_clause, [], [f73, f356, f352, f348, f344])).
fof(f73, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f342, plain, (spl4_17 | spl4_18 | spl4_19 | spl4_20), inference(avatar_split_clause, [], [f74, f339, f335, f331, f327])).
fof(f74, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f325, plain, (spl4_13 | spl4_14 | spl4_15 | spl4_16), inference(avatar_split_clause, [], [f75, f322, f318, f314, f310])).
fof(f75, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f308, plain, (spl4_9 | spl4_10 | spl4_11 | spl4_12), inference(avatar_split_clause, [], [f76, f305, f301, f297, f293])).
fof(f76, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f291, plain, (spl4_5 | spl4_6 | spl4_7 | spl4_8), inference(avatar_split_clause, [], [f77, f288, f284, f280, f276])).
fof(f77, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f274, plain, (spl4_1 | spl4_2 | spl4_3 | spl4_4), inference(avatar_split_clause, [], [f78, f271, f267, f263, f259])).
fof(f78, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).