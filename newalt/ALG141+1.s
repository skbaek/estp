fof(f3334, plain, $false, inference(avatar_sat_refutation, [], [f236, f253, f270, f287, f304, f321, f338, f355, f372, f406, f423, f440, f457, f474, f480, f482, f486, f487, f488, f489, f490, f495, f498, f502, f504, f506, f515, f520, f525, f530, f539, f544, f549, f554, f563, f568, f573, f578, f583, f588, f593, f598, f607, f616, f664, f674, f685, f690, f700, f705, f711, f716, f728, f729, f733, f743, f787, f794, f812, f814, f821, f836, f889, f899, f938, f940, f979, f989, f992, f1003, f1014, f1040, f1062, f1090, f1106, f1131, f1138, f1160, f1184, f1212, f1213, f1226, f1246, f1257, f1263, f1271, f1283, f1302, f1305, f1322, f1323, f1361, f1365, f1417, f1424, f1432, f1443, f1456, f1457, f1480, f1487, f1496, f1497, f1533, f1543, f1547, f1570, f1573, f1592, f1622, f1627, f1631, f1633, f1668, f1677, f1700, f1723, f1734, f1747, f1760, f1768, f1778, f1787, f1803, f1821, f1823, f1863, f1870, f1912, f1914, f1916, f1926, f1934, f1941, f1951, f1961, f1997, f2003, f2004, f2015, f2020, f2023, f2032, f2033, f2035, f2037, f2041, f2053, f2074, f2101, f2113, f2153, f2163, f2164, f2177, f2184, f2194, f2204, f2233, f2234, f2235, f2247, f2310, f2393, f2412, f2428, f2441, f2445, f2461, f2462, f2497, f2498, f2506, f2509, f2523, f2524, f2531, f2535, f2556, f2561, f2567, f2573, f2581, f2590, f2652, f2659, f2681, f2716, f2721, f2740, f2741, f2749, f2755, f2814, f2818, f2834, f2838, f2850, f2857, f2864, f2882, f2894, f2909, f2926, f2927, f2932, f2954, f2956, f2972, f2984, f2993, f2994, f3000, f3009, f3042, f3047, f3056, f3071, f3090, f3111, f3128, f3129, f3139, f3147, f3155, f3164, f3210, f3251, f3256, f3258, f3265, f3266, f3274, f3283, f3298, f3299, f3301, f3308])).
fof(f3308, plain, (spl3_23 | ~ spl3_39 | ~ spl3_72), inference(avatar_contradiction_clause, [], [f3307])).
fof(f3307, plain, ($false | (spl3_23 | ~ spl3_39 | ~ spl3_72)), inference(subsumption_resolution, [], [f3304, f298])).
fof(f298, plain, (~ (e2 = op(e2, e2)) | spl3_23), inference(avatar_component_clause, [], [f297])).
fof(f297, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f3304, plain, ((e2 = op(e2, e2)) | (~ spl3_39 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f367])).
fof(f367, plain, ((e2 = op(e1, e2)) | ~ spl3_39), inference(avatar_component_clause, [], [f365])).
fof(f365, plain, (spl3_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_39])])).
fof(f543, plain, ((e2 = op(op(e1, e2), op(e1, e2))) | ~ spl3_72), inference(avatar_component_clause, [], [f541])).
fof(f541, plain, (spl3_72 <=> (e2 = op(op(e1, e2), op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_72])])).
fof(f3301, plain, (~ spl3_41 | spl3_62 | ~ spl3_73), inference(avatar_contradiction_clause, [], [f3300])).
fof(f3300, plain, ($false | (~ spl3_41 | spl3_62 | ~ spl3_73)), inference(subsumption_resolution, [], [f3294, f464])).
fof(f464, plain, (~ (op(e0, e0) = e1) | spl3_62), inference(avatar_component_clause, [], [f463])).
fof(f463, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f3294, plain, ((op(e0, e0) = e1) | (~ spl3_41 | ~ spl3_73)), inference(backward_demodulation, [], [f548, f376])).
fof(f376, plain, ((e0 = op(e1, e1)) | ~ spl3_41), inference(avatar_component_clause, [], [f374])).
fof(f374, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f548, plain, ((e1 = op(op(e1, e1), op(e1, e1))) | ~ spl3_73), inference(avatar_component_clause, [], [f546])).
fof(f546, plain, (spl3_73 <=> (e1 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_73])])).
fof(f3299, plain, (~ spl3_37 | ~ spl3_41), inference(avatar_split_clause, [], [f3290, f374, f357])).
fof(f357, plain, (spl3_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_37])])).
fof(f3290, plain, (~ (e0 = op(e1, e2)) | ~ spl3_41), inference(backward_demodulation, [], [f142, f376])).
fof(f142, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax3)).
fof(f3298, plain, (~ spl3_9 | ~ spl3_41), inference(avatar_split_clause, [], [f3289, f374, f238])).
fof(f238, plain, (spl3_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_9])])).
fof(f3289, plain, (~ (e0 = op(e3, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f119, f376])).
fof(f119, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3283, plain, (~ spl3_37 | ~ spl3_53), inference(avatar_split_clause, [], [f3276, f425, f357])).
fof(f425, plain, (spl3_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_53])])).
fof(f3276, plain, (~ (e0 = op(e1, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f121, f427])).
fof(f427, plain, ((e0 = op(e0, e2)) | ~ spl3_53), inference(avatar_component_clause, [], [f425])).
fof(f121, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3274, plain, (~ spl3_43 | ~ spl3_59), inference(avatar_split_clause, [], [f3268, f450, f382])).
fof(f382, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f450, plain, (spl3_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_59])])).
fof(f3268, plain, (~ (e2 = op(e1, e1)) | ~ spl3_59), inference(backward_demodulation, [], [f115, f452])).
fof(f452, plain, ((e2 = op(e0, e1)) | ~ spl3_59), inference(avatar_component_clause, [], [f450])).
fof(f115, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3266, plain, (~ spl3_60 | ~ spl3_64), inference(avatar_split_clause, [], [f3260, f471, f454])).
fof(f454, plain, (spl3_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_60])])).
fof(f471, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f3260, plain, (~ (e3 = op(e0, e1)) | ~ spl3_64), inference(backward_demodulation, [], [f133, f473])).
fof(f473, plain, ((op(e0, e0) = e3) | ~ spl3_64), inference(avatar_component_clause, [], [f471])).
fof(f133, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f3265, plain, (~ spl3_16 | ~ spl3_64), inference(avatar_split_clause, [], [f3259, f471, f267])).
fof(f267, plain, (spl3_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_16])])).
fof(f3259, plain, (~ (e3 = op(e3, e0)) | ~ spl3_64), inference(backward_demodulation, [], [f111, f473])).
fof(f111, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3258, plain, (spl3_64 | ~ spl3_17 | ~ spl3_66), inference(avatar_split_clause, [], [f3257, f512, f272, f471])).
fof(f272, plain, (spl3_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_17])])).
fof(f512, plain, (spl3_66 <=> (e3 = op(op(e2, e3), op(e2, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_66])])).
fof(f3257, plain, ((op(e0, e0) = e3) | (~ spl3_17 | ~ spl3_66)), inference(forward_demodulation, [], [f514, f274])).
fof(f274, plain, ((e0 = op(e2, e3)) | ~ spl3_17), inference(avatar_component_clause, [], [f272])).
fof(f514, plain, ((e3 = op(op(e2, e3), op(e2, e3))) | ~ spl3_66), inference(avatar_component_clause, [], [f512])).
fof(f3256, plain, (spl3_3 | ~ spl3_24 | ~ spl3_67), inference(avatar_split_clause, [], [f3255, f517, f301, f212])).
fof(f212, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f301, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f517, plain, (spl3_67 <=> (e2 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_67])])).
fof(f3255, plain, ((e2 = op(e3, e3)) | (~ spl3_24 | ~ spl3_67)), inference(forward_demodulation, [], [f519, f303])).
fof(f303, plain, ((e3 = op(e2, e2)) | ~ spl3_24), inference(avatar_component_clause, [], [f301])).
fof(f519, plain, ((e2 = op(op(e2, e2), op(e2, e2))) | ~ spl3_67), inference(avatar_component_clause, [], [f517])).
fof(f3251, plain, (spl3_41 | ~ spl3_46 | ~ spl3_74), inference(avatar_split_clause, [], [f3250, f551, f395, f374])).
fof(f395, plain, (spl3_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_46])])).
fof(f551, plain, (spl3_74 <=> (e0 = op(op(e1, e0), op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_74])])).
fof(f3250, plain, ((e0 = op(e1, e1)) | (~ spl3_46 | ~ spl3_74)), inference(forward_demodulation, [], [f553, f397])).
fof(f397, plain, ((e1 = op(e1, e0)) | ~ spl3_46), inference(avatar_component_clause, [], [f395])).
fof(f553, plain, ((e0 = op(op(e1, e0), op(e1, e0))) | ~ spl3_74), inference(avatar_component_clause, [], [f551])).
fof(f3210, plain, (~ spl3_2 | ~ spl3_9 | ~ spl3_51 | spl3_85), inference(avatar_contradiction_clause, [], [f3209])).
fof(f3209, plain, ($false | (~ spl3_2 | ~ spl3_9 | ~ spl3_51 | spl3_85)), inference(subsumption_resolution, [], [f3208, f418])).
fof(f418, plain, ((e2 = op(e0, e3)) | ~ spl3_51), inference(avatar_component_clause, [], [f416])).
fof(f416, plain, (spl3_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_51])])).
fof(f3208, plain, (~ (e2 = op(e0, e3)) | (~ spl3_2 | ~ spl3_9 | spl3_85)), inference(forward_demodulation, [], [f3207, f240])).
fof(f240, plain, ((e0 = op(e3, e1)) | ~ spl3_9), inference(avatar_component_clause, [], [f238])).
fof(f3207, plain, (~ (e2 = op(op(e3, e1), e3)) | (~ spl3_2 | spl3_85)), inference(forward_demodulation, [], [f606, f210])).
fof(f210, plain, ((e1 = op(e3, e3)) | ~ spl3_2), inference(avatar_component_clause, [], [f208])).
fof(f208, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f606, plain, (~ (e2 = op(op(e3, op(e3, e3)), e3)) | spl3_85), inference(avatar_component_clause, [], [f604])).
fof(f604, plain, (spl3_85 <=> (e2 = op(op(e3, op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_85])])).
fof(f3164, plain, (~ spl3_11 | ~ spl3_43), inference(avatar_split_clause, [], [f3159, f382, f246])).
fof(f246, plain, (spl3_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_11])])).
fof(f3159, plain, (~ (e2 = op(e3, e1)) | ~ spl3_43), inference(backward_demodulation, [], [f119, f384])).
fof(f384, plain, ((e2 = op(e1, e1)) | ~ spl3_43), inference(avatar_component_clause, [], [f382])).
fof(f3155, plain, (~ spl3_30 | ~ spl3_46), inference(avatar_split_clause, [], [f3148, f395, f327])).
fof(f327, plain, (spl3_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_30])])).
fof(f3148, plain, (~ (e1 = op(e2, e0)) | ~ spl3_46), inference(backward_demodulation, [], [f112, f397])).
fof(f112, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3147, plain, (spl3_43 | ~ spl3_54 | ~ spl3_77), inference(avatar_split_clause, [], [f3145, f565, f429, f382])).
fof(f429, plain, (spl3_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_54])])).
fof(f565, plain, (spl3_77 <=> (e2 = op(op(e0, e2), op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_77])])).
fof(f3145, plain, ((e2 = op(e1, e1)) | (~ spl3_54 | ~ spl3_77)), inference(backward_demodulation, [], [f567, f431])).
fof(f431, plain, ((e1 = op(e0, e2)) | ~ spl3_54), inference(avatar_component_clause, [], [f429])).
fof(f567, plain, ((e2 = op(op(e0, e2), op(e0, e2))) | ~ spl3_77), inference(avatar_component_clause, [], [f565])).
fof(f3139, plain, (~ spl3_28 | ~ spl3_60), inference(avatar_split_clause, [], [f3135, f454, f318])).
fof(f318, plain, (spl3_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_28])])).
fof(f3135, plain, (~ (e3 = op(e2, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f116, f456])).
fof(f456, plain, ((e3 = op(e0, e1)) | ~ spl3_60), inference(avatar_component_clause, [], [f454])).
fof(f116, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3129, plain, (~ spl3_53 | ~ spl3_61), inference(avatar_split_clause, [], [f3120, f459, f425])).
fof(f459, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f3120, plain, (~ (e0 = op(e0, e2)) | ~ spl3_61), inference(backward_demodulation, [], [f134, f461])).
fof(f461, plain, ((e0 = op(e0, e0)) | ~ spl3_61), inference(avatar_component_clause, [], [f459])).
fof(f134, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3128, plain, (~ spl3_13 | ~ spl3_61), inference(avatar_split_clause, [], [f3118, f459, f255])).
fof(f255, plain, (spl3_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_13])])).
fof(f3118, plain, (~ (e0 = op(e3, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f111, f461])).
fof(f3111, plain, (spl3_9 | ~ spl3_2 | ~ spl3_84), inference(avatar_split_clause, [], [f3110, f600, f208, f238])).
fof(f600, plain, (spl3_84 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_84])])).
fof(f3110, plain, ((e0 = op(e3, e1)) | (~ spl3_2 | ~ spl3_84)), inference(forward_demodulation, [], [f601, f210])).
fof(f601, plain, ((e0 = op(e3, op(e3, e3))) | ~ spl3_84), inference(avatar_component_clause, [], [f600])).
fof(f3090, plain, (~ spl3_11 | ~ spl3_2 | spl3_100), inference(avatar_split_clause, [], [f3084, f676, f208, f246])).
fof(f676, plain, (spl3_100 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_100])])).
fof(f3084, plain, (~ (e2 = op(e3, e1)) | (~ spl3_2 | spl3_100)), inference(backward_demodulation, [], [f678, f210])).
fof(f678, plain, (~ (e2 = op(e3, op(e3, e3))) | spl3_100), inference(avatar_component_clause, [], [f676])).
fof(f3071, plain, (~ spl3_1 | ~ spl3_13), inference(avatar_split_clause, [], [f3065, f255, f204])).
fof(f204, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f3065, plain, (~ (e0 = op(e3, e3)) | ~ spl3_13), inference(backward_demodulation, [], [f153, f257])).
fof(f257, plain, ((e0 = op(e3, e0)) | ~ spl3_13), inference(avatar_component_clause, [], [f255])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3056, plain, (~ spl3_23 | ~ spl3_105), inference(avatar_contradiction_clause, [], [f3055])).
fof(f3055, plain, ($false | (~ spl3_23 | ~ spl3_105)), inference(subsumption_resolution, [], [f3054, f162])).
fof(f162, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax4)).
fof(f3054, plain, ((e2 = e3) | (~ spl3_23 | ~ spl3_105)), inference(forward_demodulation, [], [f3052, f299])).
fof(f299, plain, ((e2 = op(e2, e2)) | ~ spl3_23), inference(avatar_component_clause, [], [f297])).
fof(f3052, plain, ((e3 = op(e2, e2)) | (~ spl3_23 | ~ spl3_105)), inference(backward_demodulation, [], [f703, f299])).
fof(f703, plain, ((e3 = op(e2, op(e2, e2))) | ~ spl3_105), inference(avatar_component_clause, [], [f702])).
fof(f702, plain, (spl3_105 <=> (e3 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_105])])).
fof(f3047, plain, (spl3_2 | ~ spl3_28 | ~ spl3_68), inference(avatar_split_clause, [], [f3046, f522, f318, f208])).
fof(f522, plain, (spl3_68 <=> (e1 = op(op(e2, e1), op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_68])])).
fof(f3046, plain, ((e1 = op(e3, e3)) | (~ spl3_28 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f320])).
fof(f320, plain, ((e3 = op(e2, e1)) | ~ spl3_28), inference(avatar_component_clause, [], [f318])).
fof(f524, plain, ((e1 = op(op(e2, e1), op(e2, e1))) | ~ spl3_68), inference(avatar_component_clause, [], [f522])).
fof(f3042, plain, (~ spl3_26 | ~ spl3_30), inference(avatar_split_clause, [], [f3036, f327, f310])).
fof(f310, plain, (spl3_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_26])])).
fof(f3036, plain, (~ (e1 = op(e2, e1)) | ~ spl3_30), inference(backward_demodulation, [], [f145, f329])).
fof(f329, plain, ((e1 = op(e2, e0)) | ~ spl3_30), inference(avatar_component_clause, [], [f327])).
fof(f145, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3009, plain, (~ spl3_43 | ~ spl3_47), inference(avatar_split_clause, [], [f3003, f399, f382])).
fof(f399, plain, (spl3_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_47])])).
fof(f3003, plain, (~ (e2 = op(e1, e1)) | ~ spl3_47), inference(backward_demodulation, [], [f139, f401])).
fof(f401, plain, ((e2 = op(e1, e0)) | ~ spl3_47), inference(avatar_component_clause, [], [f399])).
fof(f139, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3000, plain, (~ spl3_19 | ~ spl3_51), inference(avatar_split_clause, [], [f2996, f416, f280])).
fof(f280, plain, (spl3_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_19])])).
fof(f2996, plain, (~ (e2 = op(e2, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f128, f418])).
fof(f128, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2994, plain, (~ spl3_49 | ~ spl3_53), inference(avatar_split_clause, [], [f2988, f425, f408])).
fof(f408, plain, (spl3_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_49])])).
fof(f2988, plain, (~ (e0 = op(e0, e3)) | ~ spl3_53), inference(backward_demodulation, [], [f138, f427])).
fof(f138, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2993, plain, (~ spl3_21 | ~ spl3_53), inference(avatar_split_clause, [], [f2986, f425, f289])).
fof(f289, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f2986, plain, (~ (e0 = op(e2, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f122, f427])).
fof(f122, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2984, plain, (~ spl3_26 | ~ spl3_58), inference(avatar_split_clause, [], [f2977, f446, f310])).
fof(f446, plain, (spl3_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_58])])).
fof(f2977, plain, (~ (e1 = op(e2, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f116, f448])).
fof(f448, plain, ((e1 = op(e0, e1)) | ~ spl3_58), inference(avatar_component_clause, [], [f446])).
fof(f2972, plain, (~ spl3_32 | ~ spl3_64), inference(avatar_split_clause, [], [f2964, f471, f335])).
fof(f335, plain, (spl3_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_32])])).
fof(f2964, plain, (~ (e3 = op(e2, e0)) | ~ spl3_64), inference(backward_demodulation, [], [f110, f473])).
fof(f110, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2956, plain, (~ spl3_30 | ~ spl3_102 | spl3_104), inference(avatar_split_clause, [], [f2955, f697, f687, f327])).
fof(f687, plain, (spl3_102 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_102])])).
fof(f697, plain, (spl3_104 <=> (e1 = op(op(e0, op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_104])])).
fof(f2955, plain, (~ (e1 = op(e2, e0)) | (~ spl3_102 | spl3_104)), inference(backward_demodulation, [], [f699, f688])).
fof(f688, plain, ((e2 = op(e0, op(e0, e0))) | ~ spl3_102), inference(avatar_component_clause, [], [f687])).
fof(f699, plain, (~ (e1 = op(op(e0, op(e0, e0)), e0)) | spl3_104), inference(avatar_component_clause, [], [f697])).
fof(f2954, plain, (~ spl3_6 | spl3_91 | ~ spl3_105), inference(avatar_split_clause, [], [f2953, f702, f632, f225])).
fof(f225, plain, (spl3_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_6])])).
fof(f632, plain, (spl3_91 <=> (e1 = op(op(e2, op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_91])])).
fof(f2953, plain, (~ (e1 = op(e3, e2)) | (spl3_91 | ~ spl3_105)), inference(backward_demodulation, [], [f634, f703])).
fof(f634, plain, (~ (e1 = op(op(e2, op(e2, e2)), e2)) | spl3_91), inference(avatar_component_clause, [], [f632])).
fof(f2932, plain, (~ spl3_39 | ~ spl3_55), inference(avatar_split_clause, [], [f2929, f433, f365])).
fof(f433, plain, (spl3_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_55])])).
fof(f2929, plain, (~ (e2 = op(e1, e2)) | ~ spl3_55), inference(backward_demodulation, [], [f121, f435])).
fof(f435, plain, ((e2 = op(e0, e2)) | ~ spl3_55), inference(avatar_component_clause, [], [f433])).
fof(f2927, plain, (~ spl3_56 | ~ spl3_60), inference(avatar_split_clause, [], [f2922, f454, f437])).
fof(f437, plain, (spl3_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_56])])).
fof(f2922, plain, (~ (e3 = op(e0, e2)) | ~ spl3_60), inference(backward_demodulation, [], [f136, f456])).
fof(f136, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2926, plain, (~ spl3_44 | ~ spl3_60), inference(avatar_split_clause, [], [f2921, f454, f386])).
fof(f386, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f2921, plain, (~ (e3 = op(e1, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f115, f456])).
fof(f2909, plain, (~ spl3_60 | ~ spl3_62 | spl3_107), inference(avatar_split_clause, [], [f2841, f713, f463, f454])).
fof(f713, plain, (spl3_107 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_107])])).
fof(f2841, plain, (~ (e3 = op(e0, e1)) | (~ spl3_62 | spl3_107)), inference(forward_demodulation, [], [f715, f465])).
fof(f465, plain, ((op(e0, e0) = e1) | ~ spl3_62), inference(avatar_component_clause, [], [f463])).
fof(f715, plain, (~ (e3 = op(e0, op(e0, e0))) | spl3_107), inference(avatar_component_clause, [], [f713])).
fof(f2894, plain, (~ spl3_28 | ~ spl3_32), inference(avatar_split_clause, [], [f2892, f335, f318])).
fof(f2892, plain, (~ (e3 = op(e2, e1)) | ~ spl3_32), inference(backward_demodulation, [], [f145, f337])).
fof(f337, plain, ((e3 = op(e2, e0)) | ~ spl3_32), inference(avatar_component_clause, [], [f335])).
fof(f2882, plain, (~ spl3_28 | ~ spl3_44), inference(avatar_split_clause, [], [f2874, f386, f318])).
fof(f2874, plain, (~ (e3 = op(e2, e1)) | ~ spl3_44), inference(backward_demodulation, [], [f118, f388])).
fof(f388, plain, ((e3 = op(e1, e1)) | ~ spl3_44), inference(avatar_component_clause, [], [f386])).
fof(f118, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2864, plain, (~ spl3_33 | ~ spl3_49), inference(avatar_split_clause, [], [f2860, f408, f340])).
fof(f340, plain, (spl3_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_33])])).
fof(f2860, plain, (~ (e0 = op(e1, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f127, f410])).
fof(f410, plain, ((e0 = op(e0, e3)) | ~ spl3_49), inference(avatar_component_clause, [], [f408])).
fof(f127, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2857, plain, (~ spl3_32 | ~ spl3_59 | ~ spl3_62 | spl3_97), inference(avatar_split_clause, [], [f2856, f661, f463, f450, f335])).
fof(f661, plain, (spl3_97 <=> (e3 = op(op(e0, op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_97])])).
fof(f2856, plain, (~ (e3 = op(e2, e0)) | (~ spl3_59 | ~ spl3_62 | spl3_97)), inference(forward_demodulation, [], [f2855, f452])).
fof(f2855, plain, (~ (e3 = op(op(e0, e1), e0)) | (~ spl3_62 | spl3_97)), inference(forward_demodulation, [], [f663, f465])).
fof(f663, plain, (~ (e3 = op(op(e0, op(e0, e0)), e0)) | spl3_97), inference(avatar_component_clause, [], [f661])).
fof(f2850, plain, (~ spl3_30 | ~ spl3_62), inference(avatar_split_clause, [], [f2849, f463, f327])).
fof(f2849, plain, (~ (e1 = op(e2, e0)) | ~ spl3_62), inference(forward_demodulation, [], [f110, f465])).
fof(f2838, plain, (~ spl3_50 | ~ spl3_62), inference(avatar_split_clause, [], [f2836, f463, f412])).
fof(f412, plain, (spl3_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_50])])).
fof(f2836, plain, (~ (e1 = op(e0, e3)) | ~ spl3_62), inference(backward_demodulation, [], [f135, f465])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2834, plain, (spl3_62 | ~ spl3_9 | ~ spl3_82), inference(avatar_split_clause, [], [f2804, f590, f238, f463])).
fof(f590, plain, (spl3_82 <=> (e1 = op(op(e3, e1), op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_82])])).
fof(f2804, plain, ((op(e0, e0) = e1) | (~ spl3_9 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f240])).
fof(f592, plain, ((e1 = op(op(e3, e1), op(e3, e1))) | ~ spl3_82), inference(avatar_component_clause, [], [f590])).
fof(f2818, plain, (~ spl3_32 | ~ spl3_21 | spl3_105), inference(avatar_split_clause, [], [f2785, f702, f289, f335])).
fof(f2785, plain, (~ (e3 = op(e2, e0)) | (~ spl3_21 | spl3_105)), inference(backward_demodulation, [], [f704, f291])).
fof(f291, plain, ((e0 = op(e2, e2)) | ~ spl3_21), inference(avatar_component_clause, [], [f289])).
fof(f704, plain, (~ (e3 = op(e2, op(e2, e2))) | spl3_105), inference(avatar_component_clause, [], [f702])).
fof(f2814, plain, (~ spl3_6 | ~ spl3_42 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f2813])).
fof(f2813, plain, ($false | (~ spl3_6 | ~ spl3_42 | ~ spl3_81)), inference(subsumption_resolution, [], [f2812, f160])).
fof(f160, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2812, plain, ((e1 = e2) | (~ spl3_6 | ~ spl3_42 | ~ spl3_81)), inference(forward_demodulation, [], [f2811, f380])).
fof(f380, plain, ((e1 = op(e1, e1)) | ~ spl3_42), inference(avatar_component_clause, [], [f378])).
fof(f378, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f2811, plain, ((e2 = op(e1, e1)) | (~ spl3_6 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f227])).
fof(f227, plain, ((e1 = op(e3, e2)) | ~ spl3_6), inference(avatar_component_clause, [], [f225])).
fof(f587, plain, ((e2 = op(op(e3, e2), op(e3, e2))) | ~ spl3_81), inference(avatar_component_clause, [], [f585])).
fof(f585, plain, (spl3_81 <=> (e2 = op(op(e3, e2), op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_81])])).
fof(f2755, plain, (~ spl3_23 | ~ spl3_39), inference(avatar_split_clause, [], [f2750, f365, f297])).
fof(f2750, plain, (~ (e2 = op(e2, e2)) | ~ spl3_39), inference(backward_demodulation, [], [f124, f367])).
fof(f124, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2749, plain, (~ spl3_41 | ~ spl3_42 | spl3_103), inference(avatar_split_clause, [], [f2748, f692, f378, f374])).
fof(f692, plain, (spl3_103 <=> (e0 = op(op(e1, op(e1, e1)), e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_103])])).
fof(f2748, plain, (~ (e0 = op(e1, e1)) | (~ spl3_42 | spl3_103)), inference(forward_demodulation, [], [f2739, f380])).
fof(f2739, plain, (~ (e0 = op(op(e1, e1), e1)) | (~ spl3_42 | spl3_103)), inference(backward_demodulation, [], [f694, f380])).
fof(f694, plain, (~ (e0 = op(op(e1, op(e1, e1)), e1)) | spl3_103), inference(avatar_component_clause, [], [f692])).
fof(f2741, plain, (~ spl3_38 | ~ spl3_42), inference(avatar_split_clause, [], [f2730, f378, f361])).
fof(f361, plain, (spl3_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_38])])).
fof(f2730, plain, (~ (e1 = op(e1, e2)) | ~ spl3_42), inference(backward_demodulation, [], [f142, f380])).
fof(f2740, plain, (~ spl3_26 | ~ spl3_42), inference(avatar_split_clause, [], [f2728, f378, f310])).
fof(f2728, plain, (~ (e1 = op(e2, e1)) | ~ spl3_42), inference(backward_demodulation, [], [f118, f380])).
fof(f2721, plain, (~ spl3_61 | ~ spl3_62), inference(avatar_contradiction_clause, [], [f2720])).
fof(f2720, plain, ($false | (~ spl3_61 | ~ spl3_62)), inference(subsumption_resolution, [], [f2719, f157])).
fof(f157, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f2719, plain, ((e0 = e1) | (~ spl3_61 | ~ spl3_62)), inference(forward_demodulation, [], [f465, f461])).
fof(f2716, plain, (~ spl3_61 | ~ spl3_102), inference(avatar_contradiction_clause, [], [f2715])).
fof(f2715, plain, ($false | (~ spl3_61 | ~ spl3_102)), inference(subsumption_resolution, [], [f2714, f158])).
fof(f158, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2714, plain, ((e0 = e2) | (~ spl3_61 | ~ spl3_102)), inference(forward_demodulation, [], [f2713, f461])).
fof(f2713, plain, ((op(e0, e0) = e2) | (~ spl3_61 | ~ spl3_102)), inference(forward_demodulation, [], [f688, f461])).
fof(f2681, plain, (~ spl3_24 | ~ spl3_56), inference(avatar_split_clause, [], [f2680, f437, f301])).
fof(f2680, plain, (~ (e3 = op(e2, e2)) | ~ spl3_56), inference(forward_demodulation, [], [f122, f439])).
fof(f439, plain, ((e3 = op(e0, e2)) | ~ spl3_56), inference(avatar_component_clause, [], [f437])).
fof(f2659, plain, (~ spl3_11 | spl3_22 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f2658])).
fof(f2658, plain, ($false | (~ spl3_11 | spl3_22 | ~ spl3_82)), inference(subsumption_resolution, [], [f2656, f294])).
fof(f294, plain, (~ (e1 = op(e2, e2)) | spl3_22), inference(avatar_component_clause, [], [f293])).
fof(f293, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f2656, plain, ((e1 = op(e2, e2)) | (~ spl3_11 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f248])).
fof(f248, plain, ((e2 = op(e3, e1)) | ~ spl3_11), inference(avatar_component_clause, [], [f246])).
fof(f2652, plain, (~ spl3_10 | ~ spl3_14), inference(avatar_split_clause, [], [f2646, f259, f242])).
fof(f242, plain, (spl3_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_10])])).
fof(f259, plain, (spl3_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_14])])).
fof(f2646, plain, (~ (e1 = op(e3, e1)) | ~ spl3_14), inference(backward_demodulation, [], [f151, f261])).
fof(f261, plain, ((e1 = op(e3, e0)) | ~ spl3_14), inference(avatar_component_clause, [], [f259])).
fof(f151, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2590, plain, (~ spl3_52 | ~ spl3_60), inference(avatar_split_clause, [], [f2586, f454, f420])).
fof(f420, plain, (spl3_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_52])])).
fof(f2586, plain, (~ (e3 = op(e0, e3)) | ~ spl3_60), inference(backward_demodulation, [], [f137, f456])).
fof(f137, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2581, plain, (~ spl3_61 | ~ spl3_63), inference(avatar_contradiction_clause, [], [f2580])).
fof(f2580, plain, ($false | (~ spl3_61 | ~ spl3_63)), inference(subsumption_resolution, [], [f2579, f158])).
fof(f2579, plain, ((e0 = e2) | (~ spl3_61 | ~ spl3_63)), inference(backward_demodulation, [], [f469, f461])).
fof(f469, plain, ((op(e0, e0) = e2) | ~ spl3_63), inference(avatar_component_clause, [], [f467])).
fof(f467, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f2573, plain, (~ spl3_54 | ~ spl3_63 | spl3_96), inference(avatar_split_clause, [], [f2572, f657, f467, f429])).
fof(f657, plain, (spl3_96 <=> (e1 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_96])])).
fof(f2572, plain, (~ (e1 = op(e0, e2)) | (~ spl3_63 | spl3_96)), inference(forward_demodulation, [], [f659, f469])).
fof(f659, plain, (~ (e1 = op(e0, op(e0, e0))) | spl3_96), inference(avatar_component_clause, [], [f657])).
fof(f2567, plain, (~ spl3_55 | ~ spl3_63), inference(avatar_split_clause, [], [f2566, f467, f433])).
fof(f2566, plain, (~ (e2 = op(e0, e2)) | ~ spl3_63), inference(forward_demodulation, [], [f134, f469])).
fof(f2561, plain, (~ spl3_36 | ~ spl3_48), inference(avatar_split_clause, [], [f2560, f403, f352])).
fof(f352, plain, (spl3_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_36])])).
fof(f403, plain, (spl3_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_48])])).
fof(f2560, plain, (~ (e3 = op(e1, e3)) | ~ spl3_48), inference(forward_demodulation, [], [f141, f405])).
fof(f405, plain, ((e3 = op(e1, e0)) | ~ spl3_48), inference(avatar_component_clause, [], [f403])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2556, plain, (~ spl3_31 | ~ spl3_63), inference(avatar_split_clause, [], [f2555, f467, f331])).
fof(f331, plain, (spl3_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_31])])).
fof(f2555, plain, (~ (e2 = op(e2, e0)) | ~ spl3_63), inference(forward_demodulation, [], [f110, f469])).
fof(f2535, plain, (~ spl3_16 | ~ spl3_48), inference(avatar_split_clause, [], [f2532, f403, f267])).
fof(f2532, plain, (~ (e3 = op(e3, e0)) | ~ spl3_48), inference(backward_demodulation, [], [f113, f405])).
fof(f113, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2531, plain, (~ spl3_9 | ~ spl3_57), inference(avatar_split_clause, [], [f2526, f442, f238])).
fof(f442, plain, (spl3_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_57])])).
fof(f2526, plain, (~ (e0 = op(e3, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f117, f444])).
fof(f444, plain, ((e0 = op(e0, e1)) | ~ spl3_57), inference(avatar_component_clause, [], [f442])).
fof(f117, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2524, plain, (~ spl3_59 | ~ spl3_63), inference(avatar_split_clause, [], [f2519, f467, f450])).
fof(f2519, plain, (~ (e2 = op(e0, e1)) | ~ spl3_63), inference(backward_demodulation, [], [f133, f469])).
fof(f2523, plain, (~ spl3_47 | ~ spl3_63), inference(avatar_split_clause, [], [f2517, f467, f399])).
fof(f2517, plain, (~ (e2 = op(e1, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f109, f469])).
fof(f109, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2509, plain, (~ spl3_30 | spl3_41 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f2508])).
fof(f2508, plain, ($false | (~ spl3_30 | spl3_41 | ~ spl3_69)), inference(subsumption_resolution, [], [f2507, f375])).
fof(f375, plain, (~ (e0 = op(e1, e1)) | spl3_41), inference(avatar_component_clause, [], [f374])).
fof(f2507, plain, ((e0 = op(e1, e1)) | (~ spl3_30 | ~ spl3_69)), inference(forward_demodulation, [], [f529, f329])).
fof(f529, plain, ((e0 = op(op(e2, e0), op(e2, e0))) | ~ spl3_69), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl3_69 <=> (e0 = op(op(e2, e0), op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f2506, plain, (spl3_63 | ~ spl3_37 | ~ spl3_72), inference(avatar_split_clause, [], [f2505, f541, f357, f467])).
fof(f2505, plain, ((op(e0, e0) = e2) | (~ spl3_37 | ~ spl3_72)), inference(forward_demodulation, [], [f543, f359])).
fof(f359, plain, ((e0 = op(e1, e2)) | ~ spl3_37), inference(avatar_component_clause, [], [f357])).
fof(f2498, plain, (~ spl3_47 | ~ spl3_96 | spl3_99), inference(avatar_split_clause, [], [f2494, f671, f657, f399])).
fof(f671, plain, (spl3_99 <=> (e2 = op(op(e0, op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_99])])).
fof(f2494, plain, (~ (e2 = op(e1, e0)) | (~ spl3_96 | spl3_99)), inference(backward_demodulation, [], [f673, f658])).
fof(f658, plain, ((e1 = op(e0, op(e0, e0))) | ~ spl3_96), inference(avatar_component_clause, [], [f657])).
fof(f673, plain, (~ (e2 = op(op(e0, op(e0, e0)), e0)) | spl3_99), inference(avatar_component_clause, [], [f671])).
fof(f2497, plain, (~ spl3_48 | ~ spl3_96 | spl3_97), inference(avatar_split_clause, [], [f2493, f661, f657, f403])).
fof(f2493, plain, (~ (e3 = op(e1, e0)) | (~ spl3_96 | spl3_97)), inference(backward_demodulation, [], [f663, f658])).
fof(f2462, plain, (~ spl3_24 | ~ spl3_28), inference(avatar_split_clause, [], [f2459, f318, f301])).
fof(f2459, plain, (~ (e3 = op(e2, e2)) | ~ spl3_28), inference(backward_demodulation, [], [f148, f320])).
fof(f148, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2461, plain, (~ spl3_12 | ~ spl3_28), inference(avatar_split_clause, [], [f2458, f318, f250])).
fof(f250, plain, (spl3_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_12])])).
fof(f2458, plain, (~ (e3 = op(e3, e1)) | ~ spl3_28), inference(backward_demodulation, [], [f120, f320])).
fof(f120, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2445, plain, (spl3_41 | ~ spl3_42 | ~ spl3_103), inference(avatar_contradiction_clause, [], [f2444])).
fof(f2444, plain, ($false | (spl3_41 | ~ spl3_42 | ~ spl3_103)), inference(subsumption_resolution, [], [f2443, f375])).
fof(f2443, plain, ((e0 = op(e1, e1)) | (~ spl3_42 | ~ spl3_103)), inference(forward_demodulation, [], [f2437, f380])).
fof(f2437, plain, ((e0 = op(op(e1, e1), e1)) | (~ spl3_42 | ~ spl3_103)), inference(backward_demodulation, [], [f693, f380])).
fof(f693, plain, ((e0 = op(op(e1, op(e1, e1)), e1)) | ~ spl3_103), inference(avatar_component_clause, [], [f692])).
fof(f2441, plain, (~ spl3_42 | spl3_73), inference(avatar_contradiction_clause, [], [f2440])).
fof(f2440, plain, ($false | (~ spl3_42 | spl3_73)), inference(subsumption_resolution, [], [f2434, f380])).
fof(f2434, plain, (~ (e1 = op(e1, e1)) | (~ spl3_42 | spl3_73)), inference(backward_demodulation, [], [f547, f380])).
fof(f547, plain, (~ (e1 = op(op(e1, e1), op(e1, e1))) | spl3_73), inference(avatar_component_clause, [], [f546])).
fof(f2428, plain, (~ spl3_31 | ~ spl3_47), inference(avatar_split_clause, [], [f2423, f399, f331])).
fof(f2423, plain, (~ (e2 = op(e2, e0)) | ~ spl3_47), inference(backward_demodulation, [], [f112, f401])).
fof(f2412, plain, (spl3_22 | ~ spl3_59 | ~ spl3_78), inference(avatar_contradiction_clause, [], [f2411])).
fof(f2411, plain, ($false | (spl3_22 | ~ spl3_59 | ~ spl3_78)), inference(subsumption_resolution, [], [f2408, f294])).
fof(f2408, plain, ((e1 = op(e2, e2)) | (~ spl3_59 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f452])).
fof(f572, plain, ((e1 = op(op(e0, e1), op(e0, e1))) | ~ spl3_78), inference(avatar_component_clause, [], [f570])).
fof(f570, plain, (spl3_78 <=> (e1 = op(op(e0, e1), op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_78])])).
fof(f2393, plain, (~ spl3_57 | ~ spl3_61), inference(avatar_split_clause, [], [f2380, f459, f442])).
fof(f2380, plain, (~ (e0 = op(e0, e1)) | ~ spl3_61), inference(backward_demodulation, [], [f133, f461])).
fof(f2310, plain, (~ spl3_22 | ~ spl3_26), inference(avatar_split_clause, [], [f2305, f310, f293])).
fof(f2305, plain, (~ (e1 = op(e2, e2)) | ~ spl3_26), inference(backward_demodulation, [], [f148, f312])).
fof(f312, plain, ((e1 = op(e2, e1)) | ~ spl3_26), inference(avatar_component_clause, [], [f310])).
fof(f2247, plain, (~ spl3_41 | ~ spl3_57), inference(avatar_split_clause, [], [f2236, f442, f374])).
fof(f2236, plain, (~ (e0 = op(e1, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f115, f444])).
fof(f2235, plain, (spl3_41 | ~ spl3_62 | ~ spl3_79), inference(avatar_split_clause, [], [f2228, f575, f463, f374])).
fof(f575, plain, (spl3_79 <=> (e0 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_79])])).
fof(f2228, plain, ((e0 = op(e1, e1)) | (~ spl3_62 | ~ spl3_79)), inference(backward_demodulation, [], [f577, f465])).
fof(f577, plain, ((e0 = op(op(e0, e0), op(e0, e0))) | ~ spl3_79), inference(avatar_component_clause, [], [f575])).
fof(f2234, plain, (~ spl3_58 | ~ spl3_62), inference(avatar_split_clause, [], [f2224, f463, f446])).
fof(f2224, plain, (~ (e1 = op(e0, e1)) | ~ spl3_62), inference(backward_demodulation, [], [f133, f465])).
fof(f2233, plain, (~ spl3_46 | ~ spl3_62), inference(avatar_split_clause, [], [f2221, f463, f395])).
fof(f2221, plain, (~ (e1 = op(e1, e0)) | ~ spl3_62), inference(backward_demodulation, [], [f109, f465])).
fof(f2204, plain, (~ spl3_2 | ~ spl3_41 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f2203])).
fof(f2203, plain, ($false | (~ spl3_2 | ~ spl3_41 | ~ spl3_80)), inference(subsumption_resolution, [], [f2202, f159])).
fof(f159, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f2202, plain, ((e0 = e3) | (~ spl3_2 | ~ spl3_41 | ~ spl3_80)), inference(forward_demodulation, [], [f2197, f376])).
fof(f2197, plain, ((e3 = op(e1, e1)) | (~ spl3_2 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f210])).
fof(f582, plain, ((e3 = op(op(e3, e3), op(e3, e3))) | ~ spl3_80), inference(avatar_component_clause, [], [f580])).
fof(f580, plain, (spl3_80 <=> (e3 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_80])])).
fof(f2194, plain, (~ spl3_1 | ~ spl3_5), inference(avatar_split_clause, [], [f2189, f221, f204])).
fof(f221, plain, (spl3_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_5])])).
fof(f2189, plain, (~ (e0 = op(e3, e3)) | ~ spl3_5), inference(backward_demodulation, [], [f156, f223])).
fof(f223, plain, ((e0 = op(e3, e2)) | ~ spl3_5), inference(avatar_component_clause, [], [f221])).
fof(f156, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2184, plain, (spl3_1 | ~ spl3_16 | ~ spl3_83), inference(avatar_split_clause, [], [f2182, f595, f267, f204])).
fof(f595, plain, (spl3_83 <=> (e0 = op(op(e3, e0), op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_83])])).
fof(f2182, plain, ((e0 = op(e3, e3)) | (~ spl3_16 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f269])).
fof(f269, plain, ((e3 = op(e3, e0)) | ~ spl3_16), inference(avatar_component_clause, [], [f267])).
fof(f597, plain, ((e0 = op(op(e3, e0), op(e3, e0))) | ~ spl3_83), inference(avatar_component_clause, [], [f595])).
fof(f2177, plain, (~ spl3_1 | ~ spl3_49), inference(avatar_split_clause, [], [f2172, f408, f204])).
fof(f2172, plain, (~ (e0 = op(e3, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f129, f410])).
fof(f129, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2164, plain, (~ spl3_50 | ~ spl3_58), inference(avatar_split_clause, [], [f2159, f446, f412])).
fof(f2159, plain, (~ (e1 = op(e0, e3)) | ~ spl3_58), inference(backward_demodulation, [], [f137, f448])).
fof(f2163, plain, (~ spl3_10 | ~ spl3_58), inference(avatar_split_clause, [], [f2157, f446, f242])).
fof(f2157, plain, (~ (e1 = op(e3, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f117, f448])).
fof(f2153, plain, (~ spl3_15 | ~ spl3_63), inference(avatar_split_clause, [], [f2146, f467, f263])).
fof(f263, plain, (spl3_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_15])])).
fof(f2146, plain, (~ (e2 = op(e3, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f111, f469])).
fof(f2113, plain, (~ spl3_5 | ~ spl3_22 | ~ spl3_28 | spl3_98), inference(avatar_split_clause, [], [f2099, f666, f318, f293, f221])).
fof(f666, plain, (spl3_98 <=> (e0 = op(op(e2, op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_98])])).
fof(f2099, plain, (~ (e0 = op(e3, e2)) | (~ spl3_22 | ~ spl3_28 | spl3_98)), inference(backward_demodulation, [], [f1467, f320])).
fof(f1467, plain, (~ (e0 = op(op(e2, e1), e2)) | (~ spl3_22 | spl3_98)), inference(forward_demodulation, [], [f668, f295])).
fof(f295, plain, ((e1 = op(e2, e2)) | ~ spl3_22), inference(avatar_component_clause, [], [f293])).
fof(f668, plain, (~ (e0 = op(op(e2, op(e2, e2)), e2)) | spl3_98), inference(avatar_component_clause, [], [f666])).
fof(f2101, plain, (~ spl3_20 | ~ spl3_28), inference(avatar_split_clause, [], [f2097, f318, f284])).
fof(f284, plain, (spl3_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_20])])).
fof(f2097, plain, (~ (e3 = op(e2, e3)) | ~ spl3_28), inference(backward_demodulation, [], [f149, f320])).
fof(f149, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2074, plain, (~ spl3_25 | ~ spl3_41), inference(avatar_split_clause, [], [f2063, f374, f306])).
fof(f306, plain, (spl3_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_25])])).
fof(f2063, plain, (~ (e0 = op(e2, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f118, f376])).
fof(f2053, plain, (spl3_44 | ~ spl3_50 | ~ spl3_76), inference(avatar_split_clause, [], [f2048, f560, f412, f386])).
fof(f560, plain, (spl3_76 <=> (e3 = op(op(e0, e3), op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_76])])).
fof(f2048, plain, ((e3 = op(e1, e1)) | (~ spl3_50 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f414])).
fof(f414, plain, ((e1 = op(e0, e3)) | ~ spl3_50), inference(avatar_component_clause, [], [f412])).
fof(f562, plain, ((e3 = op(op(e0, e3), op(e0, e3))) | ~ spl3_76), inference(avatar_component_clause, [], [f560])).
fof(f2041, plain, (~ spl3_25 | ~ spl3_22 | spl3_86), inference(avatar_split_clause, [], [f2040, f609, f293, f306])).
fof(f609, plain, (spl3_86 <=> (e0 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_86])])).
fof(f2040, plain, (~ (e0 = op(e2, e1)) | (~ spl3_22 | spl3_86)), inference(forward_demodulation, [], [f611, f295])).
fof(f611, plain, (~ (e0 = op(e2, op(e2, e2))) | spl3_86), inference(avatar_component_clause, [], [f609])).
fof(f2037, plain, (~ spl3_51 | ~ spl3_64 | spl3_102), inference(avatar_split_clause, [], [f2036, f687, f471, f416])).
fof(f2036, plain, (~ (e2 = op(e0, e3)) | (~ spl3_64 | spl3_102)), inference(forward_demodulation, [], [f689, f473])).
fof(f689, plain, (~ (e2 = op(e0, op(e0, e0))) | spl3_102), inference(avatar_component_clause, [], [f687])).
fof(f2035, plain, (spl3_28 | ~ spl3_22 | ~ spl3_105), inference(avatar_split_clause, [], [f2034, f702, f293, f318])).
fof(f2034, plain, ((e3 = op(e2, e1)) | (~ spl3_22 | ~ spl3_105)), inference(forward_demodulation, [], [f703, f295])).
fof(f2033, plain, (spl3_63 | ~ spl3_53 | ~ spl3_77), inference(avatar_split_clause, [], [f1944, f565, f425, f467])).
fof(f1944, plain, ((op(e0, e0) = e2) | (~ spl3_53 | ~ spl3_77)), inference(backward_demodulation, [], [f567, f427])).
fof(f2032, plain, (spl3_64 | ~ spl3_1 | ~ spl3_80), inference(avatar_split_clause, [], [f2008, f580, f204, f471])).
fof(f2008, plain, ((op(e0, e0) = e3) | (~ spl3_1 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f206])).
fof(f206, plain, ((e0 = op(e3, e3)) | ~ spl3_1), inference(avatar_component_clause, [], [f204])).
fof(f2023, plain, (~ spl3_31 | ~ spl3_15), inference(avatar_split_clause, [], [f2022, f263, f331])).
fof(f2022, plain, (~ (e2 = op(e2, e0)) | ~ spl3_15), inference(forward_demodulation, [], [f114, f265])).
fof(f265, plain, ((e2 = op(e3, e0)) | ~ spl3_15), inference(avatar_component_clause, [], [f263])).
fof(f114, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2020, plain, (~ spl3_27 | ~ spl3_59), inference(avatar_split_clause, [], [f2019, f450, f314])).
fof(f314, plain, (spl3_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_27])])).
fof(f2019, plain, (~ (e2 = op(e2, e1)) | ~ spl3_59), inference(forward_demodulation, [], [f116, f452])).
fof(f2015, plain, (~ spl3_1 | ~ spl3_62 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f2014])).
fof(f2014, plain, ($false | (~ spl3_1 | ~ spl3_62 | ~ spl3_80)), inference(subsumption_resolution, [], [f2013, f161])).
fof(f161, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2013, plain, ((e1 = e3) | (~ spl3_1 | ~ spl3_62 | ~ spl3_80)), inference(forward_demodulation, [], [f2008, f465])).
fof(f2004, plain, (~ spl3_2 | ~ spl3_10), inference(avatar_split_clause, [], [f2001, f242, f208])).
fof(f2001, plain, (~ (e1 = op(e3, e3)) | ~ spl3_10), inference(backward_demodulation, [], [f155, f244])).
fof(f244, plain, ((e1 = op(e3, e1)) | ~ spl3_10), inference(avatar_component_clause, [], [f242])).
fof(f155, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2003, plain, (~ spl3_10 | spl3_42 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f2002])).
fof(f2002, plain, ($false | (~ spl3_10 | spl3_42 | ~ spl3_82)), inference(subsumption_resolution, [], [f1999, f379])).
fof(f379, plain, (~ (e1 = op(e1, e1)) | spl3_42), inference(avatar_component_clause, [], [f378])).
fof(f1999, plain, ((e1 = op(e1, e1)) | (~ spl3_10 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f244])).
fof(f1997, plain, (~ spl3_15 | spl3_21 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1996])).
fof(f1996, plain, ($false | (~ spl3_15 | spl3_21 | ~ spl3_83)), inference(subsumption_resolution, [], [f1994, f290])).
fof(f290, plain, (~ (e0 = op(e2, e2)) | spl3_21), inference(avatar_component_clause, [], [f289])).
fof(f1994, plain, ((e0 = op(e2, e2)) | (~ spl3_15 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f265])).
fof(f1961, plain, (~ spl3_35 | ~ spl3_51), inference(avatar_split_clause, [], [f1954, f416, f348])).
fof(f348, plain, (spl3_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_35])])).
fof(f1954, plain, (~ (e2 = op(e1, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f127, f418])).
fof(f1951, plain, (~ spl3_53 | ~ spl3_62 | ~ spl3_77), inference(avatar_contradiction_clause, [], [f1950])).
fof(f1950, plain, ($false | (~ spl3_53 | ~ spl3_62 | ~ spl3_77)), inference(subsumption_resolution, [], [f1949, f160])).
fof(f1949, plain, ((e1 = e2) | (~ spl3_53 | ~ spl3_62 | ~ spl3_77)), inference(forward_demodulation, [], [f1944, f465])).
fof(f1941, plain, (~ spl3_55 | ~ spl3_59), inference(avatar_split_clause, [], [f1936, f450, f433])).
fof(f1936, plain, (~ (e2 = op(e0, e2)) | ~ spl3_59), inference(backward_demodulation, [], [f136, f452])).
fof(f1934, plain, (~ spl3_62 | ~ spl3_64), inference(avatar_contradiction_clause, [], [f1933])).
fof(f1933, plain, ($false | (~ spl3_62 | ~ spl3_64)), inference(subsumption_resolution, [], [f1932, f161])).
fof(f1932, plain, ((e1 = e3) | (~ spl3_62 | ~ spl3_64)), inference(backward_demodulation, [], [f473, f465])).
fof(f1926, plain, (spl3_1 | ~ spl3_64 | ~ spl3_79), inference(avatar_split_clause, [], [f1925, f575, f471, f204])).
fof(f1925, plain, ((e0 = op(e3, e3)) | (~ spl3_64 | ~ spl3_79)), inference(forward_demodulation, [], [f577, f473])).
fof(f1916, plain, (~ spl3_56 | ~ spl3_64), inference(avatar_split_clause, [], [f1915, f471, f437])).
fof(f1915, plain, (~ (e3 = op(e0, e2)) | ~ spl3_64), inference(forward_demodulation, [], [f134, f473])).
fof(f1914, plain, (~ spl3_56 | ~ spl3_22 | ~ spl3_25 | spl3_87), inference(avatar_split_clause, [], [f1878, f613, f306, f293, f437])).
fof(f613, plain, (spl3_87 <=> (e3 = op(op(e2, op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_87])])).
fof(f1878, plain, (~ (e3 = op(e0, e2)) | (~ spl3_22 | ~ spl3_25 | spl3_87)), inference(backward_demodulation, [], [f1646, f308])).
fof(f308, plain, ((e0 = op(e2, e1)) | ~ spl3_25), inference(avatar_component_clause, [], [f306])).
fof(f1646, plain, (~ (e3 = op(op(e2, e1), e2)) | (~ spl3_22 | spl3_87)), inference(forward_demodulation, [], [f615, f295])).
fof(f615, plain, (~ (e3 = op(op(e2, op(e2, e2)), e2)) | spl3_87), inference(avatar_component_clause, [], [f613])).
fof(f1912, plain, (~ spl3_52 | ~ spl3_64), inference(avatar_split_clause, [], [f1911, f471, f420])).
fof(f1911, plain, (~ (e3 = op(e0, e3)) | ~ spl3_64), inference(forward_demodulation, [], [f135, f473])).
fof(f1870, plain, (~ spl3_27 | ~ spl3_31), inference(avatar_split_clause, [], [f1866, f331, f314])).
fof(f1866, plain, (~ (e2 = op(e2, e1)) | ~ spl3_31), inference(backward_demodulation, [], [f145, f333])).
fof(f333, plain, ((e2 = op(e2, e0)) | ~ spl3_31), inference(avatar_component_clause, [], [f331])).
fof(f1863, plain, (~ spl3_22 | ~ spl3_35 | ~ spl3_71), inference(avatar_contradiction_clause, [], [f1862])).
fof(f1862, plain, ($false | (~ spl3_22 | ~ spl3_35 | ~ spl3_71)), inference(subsumption_resolution, [], [f1861, f161])).
fof(f1861, plain, ((e1 = e3) | (~ spl3_22 | ~ spl3_35 | ~ spl3_71)), inference(forward_demodulation, [], [f1857, f295])).
fof(f1857, plain, ((e3 = op(e2, e2)) | (~ spl3_35 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f350])).
fof(f350, plain, ((e2 = op(e1, e3)) | ~ spl3_35), inference(avatar_component_clause, [], [f348])).
fof(f538, plain, ((e3 = op(op(e1, e3), op(e1, e3))) | ~ spl3_71), inference(avatar_component_clause, [], [f536])).
fof(f536, plain, (spl3_71 <=> (e3 = op(op(e1, e3), op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f1823, plain, (spl3_64 | ~ spl3_49 | ~ spl3_76), inference(avatar_split_clause, [], [f1822, f560, f408, f471])).
fof(f1822, plain, ((op(e0, e0) = e3) | (~ spl3_49 | ~ spl3_76)), inference(forward_demodulation, [], [f562, f410])).
fof(f1821, plain, (~ spl3_13 | spl3_61 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1820])).
fof(f1820, plain, ($false | (~ spl3_13 | spl3_61 | ~ spl3_83)), inference(subsumption_resolution, [], [f1819, f460])).
fof(f460, plain, (~ (e0 = op(e0, e0)) | spl3_61), inference(avatar_component_clause, [], [f459])).
fof(f1819, plain, ((e0 = op(e0, e0)) | (~ spl3_13 | ~ spl3_83)), inference(forward_demodulation, [], [f597, f257])).
fof(f1803, plain, (spl3_43 | ~ spl3_22 | ~ spl3_67), inference(avatar_split_clause, [], [f1748, f517, f293, f382])).
fof(f1748, plain, ((e2 = op(e1, e1)) | (~ spl3_22 | ~ spl3_67)), inference(forward_demodulation, [], [f519, f295])).
fof(f1787, plain, (spl3_3 | ~ spl3_8 | ~ spl3_81), inference(avatar_split_clause, [], [f1783, f585, f233, f212])).
fof(f233, plain, (spl3_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_8])])).
fof(f1783, plain, ((e2 = op(e3, e3)) | (~ spl3_8 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f235])).
fof(f235, plain, ((e3 = op(e3, e2)) | ~ spl3_8), inference(avatar_component_clause, [], [f233])).
fof(f1778, plain, (~ spl3_17 | ~ spl3_62 | ~ spl3_66), inference(avatar_contradiction_clause, [], [f1777])).
fof(f1777, plain, ($false | (~ spl3_17 | ~ spl3_62 | ~ spl3_66)), inference(subsumption_resolution, [], [f1776, f161])).
fof(f1776, plain, ((e1 = e3) | (~ spl3_17 | ~ spl3_62 | ~ spl3_66)), inference(forward_demodulation, [], [f1773, f465])).
fof(f1773, plain, ((op(e0, e0) = e3) | (~ spl3_17 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f274])).
fof(f1768, plain, (~ spl3_37 | ~ spl3_62 | ~ spl3_72), inference(avatar_contradiction_clause, [], [f1767])).
fof(f1767, plain, ($false | (~ spl3_37 | ~ spl3_62 | ~ spl3_72)), inference(subsumption_resolution, [], [f1766, f160])).
fof(f1766, plain, ((e1 = e2) | (~ spl3_37 | ~ spl3_62 | ~ spl3_72)), inference(forward_demodulation, [], [f1762, f465])).
fof(f1762, plain, ((op(e0, e0) = e2) | (~ spl3_37 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f359])).
fof(f1760, plain, (~ spl3_40 | ~ spl3_44), inference(avatar_split_clause, [], [f1754, f386, f369])).
fof(f369, plain, (spl3_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_40])])).
fof(f1754, plain, (~ (e3 = op(e1, e2)) | ~ spl3_44), inference(backward_demodulation, [], [f142, f388])).
fof(f1747, plain, (spl3_1 | ~ spl3_32 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f1746])).
fof(f1746, plain, ($false | (spl3_1 | ~ spl3_32 | ~ spl3_69)), inference(subsumption_resolution, [], [f1745, f205])).
fof(f205, plain, (~ (e0 = op(e3, e3)) | spl3_1), inference(avatar_component_clause, [], [f204])).
fof(f1745, plain, ((e0 = op(e3, e3)) | (~ spl3_32 | ~ spl3_69)), inference(forward_demodulation, [], [f529, f337])).
fof(f1734, plain, (~ spl3_20 | ~ spl3_32), inference(avatar_split_clause, [], [f1733, f335, f284])).
fof(f1733, plain, (~ (e3 = op(e2, e3)) | ~ spl3_32), inference(forward_demodulation, [], [f147, f337])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1723, plain, (~ spl3_6 | ~ spl3_41 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1722])).
fof(f1722, plain, ($false | (~ spl3_6 | ~ spl3_41 | ~ spl3_81)), inference(subsumption_resolution, [], [f1721, f158])).
fof(f1721, plain, ((e0 = e2) | (~ spl3_6 | ~ spl3_41 | ~ spl3_81)), inference(forward_demodulation, [], [f1718, f376])).
fof(f1718, plain, ((e2 = op(e1, e1)) | (~ spl3_6 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f227])).
fof(f1700, plain, (~ spl3_21 | ~ spl3_22), inference(avatar_contradiction_clause, [], [f1699])).
fof(f1699, plain, ($false | (~ spl3_21 | ~ spl3_22)), inference(subsumption_resolution, [], [f1698, f157])).
fof(f1698, plain, ((e0 = e1) | (~ spl3_21 | ~ spl3_22)), inference(backward_demodulation, [], [f295, f291])).
fof(f1677, plain, (spl3_21 | ~ spl3_47 | ~ spl3_74), inference(avatar_split_clause, [], [f1674, f551, f399, f289])).
fof(f1674, plain, ((e0 = op(e2, e2)) | (~ spl3_47 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f401])).
fof(f1668, plain, (~ spl3_12 | ~ spl3_60), inference(avatar_split_clause, [], [f1664, f454, f250])).
fof(f1664, plain, (~ (e3 = op(e3, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f117, f456])).
fof(f1633, plain, (spl3_44 | ~ spl3_34 | ~ spl3_71), inference(avatar_split_clause, [], [f1607, f536, f344, f386])).
fof(f344, plain, (spl3_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_34])])).
fof(f1607, plain, ((e3 = op(e1, e1)) | (~ spl3_34 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f346])).
fof(f346, plain, ((e1 = op(e1, e3)) | ~ spl3_34), inference(avatar_component_clause, [], [f344])).
fof(f1631, plain, (~ spl3_15 | ~ spl3_3), inference(avatar_split_clause, [], [f1630, f212, f263])).
fof(f1630, plain, (~ (e2 = op(e3, e0)) | ~ spl3_3), inference(forward_demodulation, [], [f153, f214])).
fof(f214, plain, ((e2 = op(e3, e3)) | ~ spl3_3), inference(avatar_component_clause, [], [f212])).
fof(f1627, plain, (~ spl3_7 | ~ spl3_55), inference(avatar_split_clause, [], [f1626, f433, f229])).
fof(f229, plain, (spl3_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f1626, plain, (~ (e2 = op(e3, e2)) | ~ spl3_55), inference(forward_demodulation, [], [f123, f435])).
fof(f123, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1622, plain, (~ spl3_25 | spl3_62 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f1621])).
fof(f1621, plain, ($false | (~ spl3_25 | spl3_62 | ~ spl3_68)), inference(subsumption_resolution, [], [f1619, f464])).
fof(f1619, plain, ((op(e0, e0) = e1) | (~ spl3_25 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f308])).
fof(f1592, plain, (~ spl3_45 | spl3_61 | ~ spl3_74), inference(avatar_contradiction_clause, [], [f1591])).
fof(f1591, plain, ($false | (~ spl3_45 | spl3_61 | ~ spl3_74)), inference(subsumption_resolution, [], [f1588, f460])).
fof(f1588, plain, ((e0 = op(e0, e0)) | (~ spl3_45 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f393])).
fof(f393, plain, ((e0 = op(e1, e0)) | ~ spl3_45), inference(avatar_component_clause, [], [f391])).
fof(f391, plain, (spl3_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_45])])).
fof(f1573, plain, (spl3_42 | ~ spl3_58 | ~ spl3_78), inference(avatar_split_clause, [], [f1569, f570, f446, f378])).
fof(f1569, plain, ((e1 = op(e1, e1)) | (~ spl3_58 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f448])).
fof(f1570, plain, (~ spl3_42 | ~ spl3_58), inference(avatar_split_clause, [], [f1563, f446, f378])).
fof(f1563, plain, (~ (e1 = op(e1, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f115, f448])).
fof(f1547, plain, (~ spl3_36 | ~ spl3_40), inference(avatar_split_clause, [], [f1546, f369, f352])).
fof(f1546, plain, (~ (e3 = op(e1, e3)) | ~ spl3_40), inference(forward_demodulation, [], [f144, f371])).
fof(f371, plain, ((e3 = op(e1, e2)) | ~ spl3_40), inference(avatar_component_clause, [], [f369])).
fof(f144, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1543, plain, (~ spl3_30 | ~ spl3_22), inference(avatar_split_clause, [], [f1542, f293, f327])).
fof(f1542, plain, (~ (e1 = op(e2, e0)) | ~ spl3_22), inference(forward_demodulation, [], [f146, f295])).
fof(f146, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1533, plain, (~ spl3_5 | spl3_63 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1532])).
fof(f1532, plain, ($false | (~ spl3_5 | spl3_63 | ~ spl3_81)), inference(subsumption_resolution, [], [f1531, f468])).
fof(f468, plain, (~ (op(e0, e0) = e2) | spl3_63), inference(avatar_component_clause, [], [f467])).
fof(f1531, plain, ((op(e0, e0) = e2) | (~ spl3_5 | ~ spl3_81)), inference(forward_demodulation, [], [f587, f223])).
fof(f1497, plain, (~ spl3_53 | ~ spl3_57), inference(avatar_split_clause, [], [f1491, f442, f425])).
fof(f1491, plain, (~ (e0 = op(e0, e2)) | ~ spl3_57), inference(backward_demodulation, [], [f136, f444])).
fof(f1496, plain, (~ spl3_57 | spl3_62 | ~ spl3_78), inference(avatar_contradiction_clause, [], [f1495])).
fof(f1495, plain, ($false | (~ spl3_57 | spl3_62 | ~ spl3_78)), inference(subsumption_resolution, [], [f1490, f464])).
fof(f1490, plain, ((op(e0, e0) = e1) | (~ spl3_57 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f444])).
fof(f1487, plain, (~ spl3_48 | ~ spl3_64), inference(avatar_split_clause, [], [f1483, f471, f403])).
fof(f1483, plain, (~ (e3 = op(e1, e0)) | ~ spl3_64), inference(backward_demodulation, [], [f109, f473])).
fof(f1480, plain, (spl3_64 | ~ spl3_33 | ~ spl3_71), inference(avatar_split_clause, [], [f1479, f536, f340, f471])).
fof(f1479, plain, ((op(e0, e0) = e3) | (~ spl3_33 | ~ spl3_71)), inference(forward_demodulation, [], [f538, f342])).
fof(f342, plain, ((e0 = op(e1, e3)) | ~ spl3_33), inference(avatar_component_clause, [], [f340])).
fof(f1457, plain, (~ spl3_54 | ~ spl3_22), inference(avatar_split_clause, [], [f1379, f293, f429])).
fof(f1379, plain, (~ (e1 = op(e0, e2)) | ~ spl3_22), inference(forward_demodulation, [], [f122, f295])).
fof(f1456, plain, (~ spl3_54 | ~ spl3_50), inference(avatar_split_clause, [], [f1455, f412, f429])).
fof(f1455, plain, (~ (e1 = op(e0, e2)) | ~ spl3_50), inference(forward_demodulation, [], [f138, f414])).
fof(f1443, plain, (~ spl3_11 | ~ spl3_27), inference(avatar_split_clause, [], [f1442, f314, f246])).
fof(f1442, plain, (~ (e2 = op(e3, e1)) | ~ spl3_27), inference(forward_demodulation, [], [f120, f316])).
fof(f316, plain, ((e2 = op(e2, e1)) | ~ spl3_27), inference(avatar_component_clause, [], [f314])).
fof(f1432, plain, (~ spl3_42 | ~ spl3_50 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f1431])).
fof(f1431, plain, ($false | (~ spl3_42 | ~ spl3_50 | ~ spl3_76)), inference(subsumption_resolution, [], [f1430, f161])).
fof(f1430, plain, ((e1 = e3) | (~ spl3_42 | ~ spl3_50 | ~ spl3_76)), inference(forward_demodulation, [], [f1427, f380])).
fof(f1427, plain, ((e3 = op(e1, e1)) | (~ spl3_50 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f414])).
fof(f1424, plain, (spl3_2 | ~ spl3_60 | ~ spl3_78), inference(avatar_contradiction_clause, [], [f1423])).
fof(f1423, plain, ($false | (spl3_2 | ~ spl3_60 | ~ spl3_78)), inference(subsumption_resolution, [], [f1421, f209])).
fof(f209, plain, (~ (e1 = op(e3, e3)) | spl3_2), inference(avatar_component_clause, [], [f208])).
fof(f1421, plain, ((e1 = op(e3, e3)) | (~ spl3_60 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f456])).
fof(f1417, plain, (spl3_21 | ~ spl3_63 | ~ spl3_79), inference(avatar_contradiction_clause, [], [f1416])).
fof(f1416, plain, ($false | (spl3_21 | ~ spl3_63 | ~ spl3_79)), inference(subsumption_resolution, [], [f1415, f290])).
fof(f1415, plain, ((e0 = op(e2, e2)) | (~ spl3_63 | ~ spl3_79)), inference(forward_demodulation, [], [f577, f469])).
fof(f1365, plain, (~ spl3_1 | ~ spl3_3), inference(avatar_contradiction_clause, [], [f1364])).
fof(f1364, plain, ($false | (~ spl3_1 | ~ spl3_3)), inference(subsumption_resolution, [], [f1363, f158])).
fof(f1363, plain, ((e0 = e2) | (~ spl3_1 | ~ spl3_3)), inference(backward_demodulation, [], [f214, f206])).
fof(f1361, plain, (~ spl3_7 | ~ spl3_22 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1360])).
fof(f1360, plain, ($false | (~ spl3_7 | ~ spl3_22 | ~ spl3_81)), inference(subsumption_resolution, [], [f1359, f160])).
fof(f1359, plain, ((e1 = e2) | (~ spl3_7 | ~ spl3_22 | ~ spl3_81)), inference(forward_demodulation, [], [f1355, f295])).
fof(f1355, plain, ((e2 = op(e2, e2)) | (~ spl3_7 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f231])).
fof(f231, plain, ((e2 = op(e3, e2)) | ~ spl3_7), inference(avatar_component_clause, [], [f229])).
fof(f1323, plain, (~ spl3_5 | ~ spl3_53), inference(avatar_split_clause, [], [f1320, f425, f221])).
fof(f1320, plain, (~ (e0 = op(e3, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f123, f427])).
fof(f1322, plain, (~ spl3_53 | ~ spl3_63 | spl3_99), inference(avatar_contradiction_clause, [], [f1321])).
fof(f1321, plain, ($false | (~ spl3_53 | ~ spl3_63 | spl3_99)), inference(subsumption_resolution, [], [f1318, f469])).
fof(f1318, plain, (~ (op(e0, e0) = e2) | (~ spl3_53 | ~ spl3_63 | spl3_99)), inference(backward_demodulation, [], [f1197, f427])).
fof(f1197, plain, (~ (e2 = op(op(e0, e2), e0)) | (~ spl3_63 | spl3_99)), inference(backward_demodulation, [], [f673, f469])).
fof(f1305, plain, (spl3_2 | ~ spl3_12 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f1304])).
fof(f1304, plain, ($false | (spl3_2 | ~ spl3_12 | ~ spl3_82)), inference(subsumption_resolution, [], [f1303, f209])).
fof(f1303, plain, ((e1 = op(e3, e3)) | (~ spl3_12 | ~ spl3_82)), inference(forward_demodulation, [], [f592, f252])).
fof(f252, plain, ((e3 = op(e3, e1)) | ~ spl3_12), inference(avatar_component_clause, [], [f250])).
fof(f1302, plain, (~ spl3_14 | spl3_41 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1301])).
fof(f1301, plain, ($false | (~ spl3_14 | spl3_41 | ~ spl3_83)), inference(subsumption_resolution, [], [f1300, f375])).
fof(f1300, plain, ((e0 = op(e1, e1)) | (~ spl3_14 | ~ spl3_83)), inference(forward_demodulation, [], [f597, f261])).
fof(f1283, plain, (spl3_43 | ~ spl3_38 | ~ spl3_72), inference(avatar_split_clause, [], [f1230, f541, f361, f382])).
fof(f1230, plain, ((e2 = op(e1, e1)) | (~ spl3_38 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f363])).
fof(f363, plain, ((e1 = op(e1, e2)) | ~ spl3_38), inference(avatar_component_clause, [], [f361])).
fof(f1271, plain, (spl3_22 | ~ spl3_27 | ~ spl3_68), inference(avatar_split_clause, [], [f1236, f522, f314, f293])).
fof(f1236, plain, ((e1 = op(e2, e2)) | (~ spl3_27 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f316])).
fof(f1263, plain, (~ spl3_6 | ~ spl3_14), inference(avatar_split_clause, [], [f1262, f259, f225])).
fof(f1262, plain, (~ (e1 = op(e3, e2)) | ~ spl3_14), inference(forward_demodulation, [], [f152, f261])).
fof(f152, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1257, plain, (spl3_1 | ~ spl3_48 | ~ spl3_74), inference(avatar_split_clause, [], [f1211, f551, f403, f204])).
fof(f1211, plain, ((e0 = op(e3, e3)) | (~ spl3_48 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f405])).
fof(f1246, plain, (~ spl3_2 | ~ spl3_14), inference(avatar_split_clause, [], [f1245, f259, f208])).
fof(f1245, plain, (~ (e1 = op(e3, e3)) | ~ spl3_14), inference(backward_demodulation, [], [f153, f261])).
fof(f1226, plain, (spl3_22 | ~ spl3_43 | ~ spl3_73), inference(avatar_contradiction_clause, [], [f1225])).
fof(f1225, plain, ($false | (spl3_22 | ~ spl3_43 | ~ spl3_73)), inference(subsumption_resolution, [], [f1219, f294])).
fof(f1219, plain, ((e1 = op(e2, e2)) | (~ spl3_43 | ~ spl3_73)), inference(backward_demodulation, [], [f548, f384])).
fof(f1213, plain, (~ spl3_40 | ~ spl3_48), inference(avatar_split_clause, [], [f1210, f403, f369])).
fof(f1210, plain, (~ (e3 = op(e1, e2)) | ~ spl3_48), inference(backward_demodulation, [], [f140, f405])).
fof(f140, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1212, plain, (~ spl3_44 | ~ spl3_48), inference(avatar_split_clause, [], [f1209, f403, f386])).
fof(f1209, plain, (~ (e3 = op(e1, e1)) | ~ spl3_48), inference(backward_demodulation, [], [f139, f405])).
fof(f1184, plain, (spl3_44 | ~ spl3_18 | ~ spl3_66), inference(avatar_split_clause, [], [f1156, f512, f276, f386])).
fof(f276, plain, (spl3_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_18])])).
fof(f1156, plain, ((e3 = op(e1, e1)) | (~ spl3_18 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f278])).
fof(f278, plain, ((e1 = op(e2, e3)) | ~ spl3_18), inference(avatar_component_clause, [], [f276])).
fof(f1160, plain, (~ spl3_18 | ~ spl3_42 | ~ spl3_66), inference(avatar_contradiction_clause, [], [f1159])).
fof(f1159, plain, ($false | (~ spl3_18 | ~ spl3_42 | ~ spl3_66)), inference(subsumption_resolution, [], [f1158, f161])).
fof(f1158, plain, ((e1 = e3) | (~ spl3_18 | ~ spl3_42 | ~ spl3_66)), inference(forward_demodulation, [], [f1156, f380])).
fof(f1138, plain, (~ spl3_57 | ~ spl3_59), inference(avatar_contradiction_clause, [], [f1137])).
fof(f1137, plain, ($false | (~ spl3_57 | ~ spl3_59)), inference(subsumption_resolution, [], [f1136, f158])).
fof(f1136, plain, ((e0 = e2) | (~ spl3_57 | ~ spl3_59)), inference(backward_demodulation, [], [f452, f444])).
fof(f1131, plain, (~ spl3_59 | ~ spl3_62 | spl3_102), inference(avatar_contradiction_clause, [], [f1130])).
fof(f1130, plain, ($false | (~ spl3_59 | ~ spl3_62 | spl3_102)), inference(subsumption_resolution, [], [f1126, f452])).
fof(f1126, plain, (~ (e2 = op(e0, e1)) | (~ spl3_62 | spl3_102)), inference(backward_demodulation, [], [f689, f465])).
fof(f1106, plain, (~ spl3_62 | ~ spl3_54), inference(avatar_split_clause, [], [f1105, f429, f463])).
fof(f1105, plain, (~ (op(e0, e0) = e1) | ~ spl3_54), inference(forward_demodulation, [], [f134, f431])).
fof(f1090, plain, (spl3_42 | ~ spl3_26 | ~ spl3_68), inference(avatar_split_clause, [], [f1089, f522, f310, f378])).
fof(f1089, plain, ((e1 = op(e1, e1)) | (~ spl3_26 | ~ spl3_68)), inference(forward_demodulation, [], [f524, f312])).
fof(f1062, plain, (~ spl3_29 | spl3_61 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f1061])).
fof(f1061, plain, ($false | (~ spl3_29 | spl3_61 | ~ spl3_69)), inference(subsumption_resolution, [], [f1059, f460])).
fof(f1059, plain, ((e0 = op(e0, e0)) | (~ spl3_29 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f325])).
fof(f325, plain, ((e0 = op(e2, e0)) | ~ spl3_29), inference(avatar_component_clause, [], [f323])).
fof(f323, plain, (spl3_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_29])])).
fof(f1040, plain, (~ spl3_50 | ~ spl3_64 | spl3_96), inference(avatar_split_clause, [], [f1034, f657, f471, f412])).
fof(f1034, plain, (~ (e1 = op(e0, e3)) | (~ spl3_64 | spl3_96)), inference(backward_demodulation, [], [f659, f473])).
fof(f1014, plain, (spl3_24 | ~ spl3_19 | ~ spl3_66), inference(avatar_split_clause, [], [f1010, f512, f280, f301])).
fof(f1010, plain, ((e3 = op(e2, e2)) | (~ spl3_19 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f282])).
fof(f282, plain, ((e2 = op(e2, e3)) | ~ spl3_19), inference(avatar_component_clause, [], [f280])).
fof(f1003, plain, (~ spl3_14 | ~ spl3_46), inference(avatar_split_clause, [], [f1002, f395, f259])).
fof(f1002, plain, (~ (e1 = op(e3, e0)) | ~ spl3_46), inference(forward_demodulation, [], [f113, f397])).
fof(f992, plain, (~ spl3_22 | ~ spl3_28 | spl3_105), inference(avatar_contradiction_clause, [], [f991])).
fof(f991, plain, ($false | (~ spl3_22 | ~ spl3_28 | spl3_105)), inference(subsumption_resolution, [], [f986, f320])).
fof(f986, plain, (~ (e3 = op(e2, e1)) | (~ spl3_22 | spl3_105)), inference(backward_demodulation, [], [f704, f295])).
fof(f989, plain, (~ spl3_22 | ~ spl3_41 | ~ spl3_67), inference(avatar_contradiction_clause, [], [f988])).
fof(f988, plain, ($false | (~ spl3_22 | ~ spl3_41 | ~ spl3_67)), inference(subsumption_resolution, [], [f987, f158])).
fof(f987, plain, ((e0 = e2) | (~ spl3_22 | ~ spl3_41 | ~ spl3_67)), inference(forward_demodulation, [], [f983, f376])).
fof(f983, plain, ((e2 = op(e1, e1)) | (~ spl3_22 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f295])).
fof(f979, plain, (spl3_21 | ~ spl3_31 | ~ spl3_69), inference(avatar_split_clause, [], [f976, f527, f331, f289])).
fof(f976, plain, ((e0 = op(e2, e2)) | (~ spl3_31 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f333])).
fof(f940, plain, (~ spl3_45 | ~ spl3_41), inference(avatar_split_clause, [], [f939, f374, f391])).
fof(f939, plain, (~ (e0 = op(e1, e0)) | ~ spl3_41), inference(forward_demodulation, [], [f139, f376])).
fof(f938, plain, (~ spl3_47 | ~ spl3_35), inference(avatar_split_clause, [], [f937, f348, f399])).
fof(f937, plain, (~ (e2 = op(e1, e0)) | ~ spl3_35), inference(forward_demodulation, [], [f141, f350])).
fof(f899, plain, (~ spl3_2 | ~ spl3_11 | ~ spl3_17 | spl3_95), inference(avatar_contradiction_clause, [], [f898])).
fof(f898, plain, ($false | (~ spl3_2 | ~ spl3_11 | ~ spl3_17 | spl3_95)), inference(subsumption_resolution, [], [f897, f274])).
fof(f897, plain, (~ (e0 = op(e2, e3)) | (~ spl3_2 | ~ spl3_11 | spl3_95)), inference(backward_demodulation, [], [f888, f248])).
fof(f888, plain, (~ (e0 = op(op(e3, e1), e3)) | (~ spl3_2 | spl3_95)), inference(backward_demodulation, [], [f654, f210])).
fof(f654, plain, (~ (e0 = op(op(e3, op(e3, e3)), e3)) | spl3_95), inference(avatar_component_clause, [], [f652])).
fof(f652, plain, (spl3_95 <=> (e0 = op(op(e3, op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_95])])).
fof(f889, plain, (~ spl3_9 | ~ spl3_2 | spl3_84), inference(avatar_split_clause, [], [f887, f600, f208, f238])).
fof(f887, plain, (~ (e0 = op(e3, e1)) | (~ spl3_2 | spl3_84)), inference(backward_demodulation, [], [f602, f210])).
fof(f602, plain, (~ (e0 = op(e3, op(e3, e3))) | spl3_84), inference(avatar_component_clause, [], [f600])).
fof(f836, plain, (~ spl3_21 | ~ spl3_61 | ~ spl3_67), inference(avatar_contradiction_clause, [], [f835])).
fof(f835, plain, ($false | (~ spl3_21 | ~ spl3_61 | ~ spl3_67)), inference(subsumption_resolution, [], [f834, f158])).
fof(f834, plain, ((e0 = e2) | (~ spl3_21 | ~ spl3_61 | ~ spl3_67)), inference(forward_demodulation, [], [f828, f461])).
fof(f828, plain, ((op(e0, e0) = e2) | (~ spl3_21 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f291])).
fof(f821, plain, (~ spl3_21 | ~ spl3_25), inference(avatar_split_clause, [], [f817, f306, f289])).
fof(f817, plain, (~ (e0 = op(e2, e2)) | ~ spl3_25), inference(backward_demodulation, [], [f148, f308])).
fof(f814, plain, (~ spl3_21 | ~ spl3_29), inference(avatar_split_clause, [], [f809, f323, f289])).
fof(f809, plain, (~ (e0 = op(e2, e2)) | ~ spl3_29), inference(backward_demodulation, [], [f146, f325])).
fof(f812, plain, (~ spl3_13 | ~ spl3_29), inference(avatar_split_clause, [], [f807, f323, f255])).
fof(f807, plain, (~ (e0 = op(e3, e0)) | ~ spl3_29), inference(backward_demodulation, [], [f114, f325])).
fof(f794, plain, (~ spl3_21 | ~ spl3_37), inference(avatar_split_clause, [], [f791, f357, f289])).
fof(f791, plain, (~ (e0 = op(e2, e2)) | ~ spl3_37), inference(backward_demodulation, [], [f124, f359])).
fof(f787, plain, (~ spl3_33 | ~ spl3_41), inference(avatar_split_clause, [], [f780, f374, f340])).
fof(f780, plain, (~ (e0 = op(e1, e3)) | ~ spl3_41), inference(backward_demodulation, [], [f143, f376])).
fof(f143, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f743, plain, (~ spl3_25 | ~ spl3_57), inference(avatar_split_clause, [], [f738, f442, f306])).
fof(f738, plain, (~ (e0 = op(e2, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f116, f444])).
fof(f733, plain, (~ spl3_49 | ~ spl3_61), inference(avatar_split_clause, [], [f724, f459, f408])).
fof(f724, plain, (~ (e0 = op(e0, e3)) | ~ spl3_61), inference(backward_demodulation, [], [f135, f461])).
fof(f729, plain, (~ spl3_29 | ~ spl3_61), inference(avatar_split_clause, [], [f720, f459, f323])).
fof(f720, plain, (~ (e0 = op(e2, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f110, f461])).
fof(f728, plain, (~ spl3_45 | ~ spl3_61), inference(avatar_split_clause, [], [f719, f459, f391])).
fof(f719, plain, (~ (e0 = op(e1, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f109, f461])).
fof(f716, plain, (~ spl3_107 | ~ spl3_62 | ~ spl3_99), inference(avatar_split_clause, [], [f200, f671, f463, f713])).
fof(f200, plain, (~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1) | ~ (e3 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1) | ~ (e3 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e2 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e1) & (e3 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax27)).
fof(f711, plain, (~ spl3_105 | ~ spl3_22 | ~ spl3_98), inference(avatar_split_clause, [], [f199, f666, f293, f702])).
fof(f199, plain, (~ (e0 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(op(e2, op(e2, e2)), e2)) & (e1 = op(e2, e2)) & (e3 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax26)).
fof(f705, plain, (~ spl3_105 | ~ spl3_21 | ~ spl3_91), inference(avatar_split_clause, [], [f197, f632, f289, f702])).
fof(f197, plain, (~ (e1 = op(op(e2, op(e2, e2)), e2)) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e1 = op(op(e2, op(e2, e2)), e2)) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e1 = op(op(e2, op(e2, e2)), e2)) & (e0 = op(e2, e2)) & (e3 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax24)).
fof(f700, plain, (~ spl3_102 | ~ spl3_64 | ~ spl3_104), inference(avatar_split_clause, [], [f196, f697, f471, f687])).
fof(f196, plain, (~ (e1 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3) | ~ (e2 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e1 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3) | ~ (e2 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e1 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e3) & (e2 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax23)).
fof(f690, plain, (~ spl3_102 | ~ spl3_62 | ~ spl3_97), inference(avatar_split_clause, [], [f194, f661, f463, f687])).
fof(f194, plain, (~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e3 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e1) & (e2 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax21)).
fof(f685, plain, (~ spl3_100 | ~ spl3_2 | ~ spl3_95), inference(avatar_split_clause, [], [f193, f652, f208, f676])).
fof(f193, plain, (~ (e0 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e0 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e0 = op(op(e3, op(e3, e3)), e3)) & (e1 = op(e3, e3)) & (e2 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax20)).
fof(f674, plain, (~ spl3_96 | ~ spl3_64 | ~ spl3_99), inference(avatar_split_clause, [], [f190, f671, f471, f657])).
fof(f190, plain, (~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3) | ~ (e1 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3) | ~ (e1 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e2 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e3) & (e1 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax17)).
fof(f664, plain, (~ spl3_96 | ~ spl3_63 | ~ spl3_97), inference(avatar_split_clause, [], [f188, f661, f467, f657])).
fof(f188, plain, (~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e2) | ~ (e1 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e2) | ~ (e1 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e3 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e2) & (e1 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax15)).
fof(f616, plain, (~ spl3_86 | ~ spl3_22 | ~ spl3_87), inference(avatar_split_clause, [], [f180, f613, f293, f609])).
fof(f180, plain, (~ (e3 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e3 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e3 = op(op(e2, op(e2, e2)), e2)) & (e1 = op(e2, e2)) & (e0 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax7)).
fof(f607, plain, (~ spl3_84 | ~ spl3_2 | ~ spl3_85), inference(avatar_split_clause, [], [f179, f604, f208, f600])).
fof(f179, plain, (~ (e2 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(op(e3, op(e3, e3)), e3)) & (e1 = op(e3, e3)) & (e0 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax6)).
fof(f598, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_83), inference(avatar_split_clause, [], [f175, f595, f508, f532, f556])).
fof(f556, plain, (spl3_75 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_75])])).
fof(f532, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f508, plain, (spl3_65 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f175, plain, ((e0 = op(op(e3, e0), op(e3, e0))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f57, plain, (((e3 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e3, e2), op(e3, e2))) & (e1 = op(op(e3, e1), op(e3, e1))) & (e0 = op(op(e3, e0), op(e3, e0)))) | sP2 | sP1 | sP0), inference(definition_folding, [], [f5, e56, e55, e54])).
fof(f54, plain, (((e3 = op(op(e0, e3), op(e0, e3))) & (e2 = op(op(e0, e2), op(e0, e2))) & (e1 = op(op(e0, e1), op(e0, e1))) & (e0 = op(op(e0, e0), op(e0, e0)))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((e3 = op(op(e0, e3), op(e0, e3))) & (e2 = op(op(e0, e2), op(e0, e2))) & (e1 = op(op(e0, e1), op(e0, e1))) & (e0 = op(op(e0, e0), op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((e3 = op(op(e1, e3), op(e1, e3))) & (e2 = op(op(e1, e2), op(e1, e2))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e0), op(e1, e0)))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((e3 = op(op(e1, e3), op(e1, e3))) & (e2 = op(op(e1, e2), op(e1, e2))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e0), op(e1, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((e3 = op(op(e2, e3), op(e2, e3))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e2, e1), op(e2, e1))) & (e0 = op(op(e2, e0), op(e2, e0)))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((e3 = op(op(e2, e3), op(e2, e3))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e2, e1), op(e2, e1))) & (e0 = op(op(e2, e0), op(e2, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f5, plain, (((e3 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e3, e2), op(e3, e2))) & (e1 = op(op(e3, e1), op(e3, e1))) & (e0 = op(op(e3, e0), op(e3, e0)))) | ((e3 = op(op(e2, e3), op(e2, e3))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e2, e1), op(e2, e1))) & (e0 = op(op(e2, e0), op(e2, e0)))) | ((e3 = op(op(e1, e3), op(e1, e3))) & (e2 = op(op(e1, e2), op(e1, e2))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e0), op(e1, e0)))) | ((e3 = op(op(e0, e3), op(e0, e3))) & (e2 = op(op(e0, e2), op(e0, e2))) & (e1 = op(op(e0, e1), op(e0, e1))) & (e0 = op(op(e0, e0), op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax5)).
fof(f593, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_82), inference(avatar_split_clause, [], [f176, f590, f508, f532, f556])).
fof(f176, plain, ((e1 = op(op(e3, e1), op(e3, e1))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f588, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_81), inference(avatar_split_clause, [], [f177, f585, f508, f532, f556])).
fof(f177, plain, ((e2 = op(op(e3, e2), op(e3, e2))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f583, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_80), inference(avatar_split_clause, [], [f178, f580, f508, f532, f556])).
fof(f178, plain, ((e3 = op(op(e3, e3), op(e3, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f578, plain, (~ spl3_75 | spl3_79), inference(avatar_split_clause, [], [f171, f575, f556])).
fof(f171, plain, ((e0 = op(op(e0, e0), op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (((e3 = op(op(e0, e3), op(e0, e3))) & (e2 = op(op(e0, e2), op(e0, e2))) & (e1 = op(op(e0, e1), op(e0, e1))) & (e0 = op(op(e0, e0), op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f573, plain, (~ spl3_75 | spl3_78), inference(avatar_split_clause, [], [f172, f570, f556])).
fof(f172, plain, ((e1 = op(op(e0, e1), op(e0, e1))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f568, plain, (~ spl3_75 | spl3_77), inference(avatar_split_clause, [], [f173, f565, f556])).
fof(f173, plain, ((e2 = op(op(e0, e2), op(e0, e2))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f563, plain, (~ spl3_75 | spl3_76), inference(avatar_split_clause, [], [f174, f560, f556])).
fof(f174, plain, ((e3 = op(op(e0, e3), op(e0, e3))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f554, plain, (~ spl3_70 | spl3_74), inference(avatar_split_clause, [], [f167, f551, f532])).
fof(f167, plain, ((e0 = op(op(e1, e0), op(e1, e0))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f59, plain, (((e3 = op(op(e1, e3), op(e1, e3))) & (e2 = op(op(e1, e2), op(e1, e2))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e0), op(e1, e0)))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f549, plain, (~ spl3_70 | spl3_73), inference(avatar_split_clause, [], [f168, f546, f532])).
fof(f168, plain, ((e1 = op(op(e1, e1), op(e1, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f544, plain, (~ spl3_70 | spl3_72), inference(avatar_split_clause, [], [f169, f541, f532])).
fof(f169, plain, ((e2 = op(op(e1, e2), op(e1, e2))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f539, plain, (~ spl3_70 | spl3_71), inference(avatar_split_clause, [], [f170, f536, f532])).
fof(f170, plain, ((e3 = op(op(e1, e3), op(e1, e3))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f530, plain, (~ spl3_65 | spl3_69), inference(avatar_split_clause, [], [f163, f527, f508])).
fof(f163, plain, ((e0 = op(op(e2, e0), op(e2, e0))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((e3 = op(op(e2, e3), op(e2, e3))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e2, e1), op(e2, e1))) & (e0 = op(op(e2, e0), op(e2, e0)))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f525, plain, (~ spl3_65 | spl3_68), inference(avatar_split_clause, [], [f164, f522, f508])).
fof(f164, plain, ((e1 = op(op(e2, e1), op(e2, e1))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f520, plain, (~ spl3_65 | spl3_67), inference(avatar_split_clause, [], [f165, f517, f508])).
fof(f165, plain, ((e2 = op(op(e2, e2), op(e2, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f515, plain, (~ spl3_65 | spl3_66), inference(avatar_split_clause, [], [f166, f512, f508])).
fof(f166, plain, ((e3 = op(op(e2, e3), op(e2, e3))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f506, plain, (spl3_61 | spl3_57 | spl3_53 | spl3_49), inference(avatar_split_clause, [], [f77, f408, f425, f442, f459])).
fof(f77, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax2)).
fof(f504, plain, (spl3_62 | spl3_58 | spl3_54 | spl3_50), inference(avatar_split_clause, [], [f79, f412, f429, f446, f463])).
fof(f79, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f502, plain, (spl3_63 | spl3_59 | spl3_55 | spl3_51), inference(avatar_split_clause, [], [f81, f416, f433, f450, f467])).
fof(f81, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f498, plain, (spl3_45 | spl3_41 | spl3_37 | spl3_33), inference(avatar_split_clause, [], [f85, f340, f357, f374, f391])).
fof(f85, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f495, plain, (spl3_58 | spl3_42 | spl3_26 | spl3_10), inference(avatar_split_clause, [], [f88, f242, f310, f378, f446])).
fof(f88, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f490, plain, (spl3_29 | spl3_25 | spl3_21 | spl3_17), inference(avatar_split_clause, [], [f93, f272, f289, f306, f323])).
fof(f93, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f489, plain, (spl3_53 | spl3_37 | spl3_21 | spl3_5), inference(avatar_split_clause, [], [f94, f221, f289, f357, f425])).
fof(f94, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f488, plain, (spl3_30 | spl3_26 | spl3_22 | spl3_18), inference(avatar_split_clause, [], [f95, f276, f293, f310, f327])).
fof(f95, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f487, plain, (spl3_54 | spl3_38 | spl3_22 | spl3_6), inference(avatar_split_clause, [], [f96, f225, f293, f361, f429])).
fof(f96, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f486, plain, (spl3_31 | spl3_27 | spl3_23 | spl3_19), inference(avatar_split_clause, [], [f97, f280, f297, f314, f331])).
fof(f97, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f482, plain, (spl3_13 | spl3_9 | spl3_5 | spl3_1), inference(avatar_split_clause, [], [f101, f204, f221, f238, f255])).
fof(f101, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f480, plain, (spl3_14 | spl3_10 | spl3_6 | spl3_2), inference(avatar_split_clause, [], [f103, f208, f225, f242, f259])).
fof(f103, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f474, plain, (spl3_61 | spl3_62 | spl3_63 | spl3_64), inference(avatar_split_clause, [], [f61, f471, f467, f463, f459])).
fof(f61, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG141+1.p', ax1)).
fof(f457, plain, (spl3_57 | spl3_58 | spl3_59 | spl3_60), inference(avatar_split_clause, [], [f62, f454, f450, f446, f442])).
fof(f62, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f440, plain, (spl3_53 | spl3_54 | spl3_55 | spl3_56), inference(avatar_split_clause, [], [f63, f437, f433, f429, f425])).
fof(f63, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f423, plain, (spl3_49 | spl3_50 | spl3_51 | spl3_52), inference(avatar_split_clause, [], [f64, f420, f416, f412, f408])).
fof(f64, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f406, plain, (spl3_45 | spl3_46 | spl3_47 | spl3_48), inference(avatar_split_clause, [], [f65, f403, f399, f395, f391])).
fof(f65, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f372, plain, (spl3_37 | spl3_38 | spl3_39 | spl3_40), inference(avatar_split_clause, [], [f67, f369, f365, f361, f357])).
fof(f67, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f355, plain, (spl3_33 | spl3_34 | spl3_35 | spl3_36), inference(avatar_split_clause, [], [f68, f352, f348, f344, f340])).
fof(f68, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f338, plain, (spl3_29 | spl3_30 | spl3_31 | spl3_32), inference(avatar_split_clause, [], [f69, f335, f331, f327, f323])).
fof(f69, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f321, plain, (spl3_25 | spl3_26 | spl3_27 | spl3_28), inference(avatar_split_clause, [], [f70, f318, f314, f310, f306])).
fof(f70, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f304, plain, (spl3_21 | spl3_22 | spl3_23 | spl3_24), inference(avatar_split_clause, [], [f71, f301, f297, f293, f289])).
fof(f71, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f287, plain, (spl3_17 | spl3_18 | spl3_19 | spl3_20), inference(avatar_split_clause, [], [f72, f284, f280, f276, f272])).
fof(f72, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f270, plain, (spl3_13 | spl3_14 | spl3_15 | spl3_16), inference(avatar_split_clause, [], [f73, f267, f263, f259, f255])).
fof(f73, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f253, plain, (spl3_9 | spl3_10 | spl3_11 | spl3_12), inference(avatar_split_clause, [], [f74, f250, f246, f242, f238])).
fof(f74, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f236, plain, (spl3_5 | spl3_6 | spl3_7 | spl3_8), inference(avatar_split_clause, [], [f75, f233, f229, f225, f221])).
fof(f75, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).