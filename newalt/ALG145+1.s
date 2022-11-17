fof(f3378, plain, $false, inference(avatar_sat_refutation, [], [f339, f356, f373, f390, f407, f424, f441, f458, f475, f492, f509, f526, f543, f560, f577, f594, f595, f596, f597, f598, f599, f600, f601, f602, f603, f604, f605, f606, f608, f609, f610, f611, f612, f613, f614, f616, f617, f618, f619, f620, f621, f622, f623, f624, f625, f626, f635, f640, f645, f650, f651, f652, f657, f658, f659, f660, f661, f662, f668, f669, f670, f671, f672, f681, f686, f691, f696, f697, f698, f704, f708, f713, f715, f716, f717, f718, f723, f725, f726, f727, f728, f737, f742, f747, f752, f753, f754, f759, f760, f761, f762, f763, f764, f771, f774, f779, f780, f782, f783, f784, f793, f798, f803, f808, f809, f810, f815, f817, f819, f820, f825, f826, f829, f830, f836, f838, f840, f841, f843, f846, f847, f848, f849, f850, f851, f852, f853, f854, f855, f856, f857, f858, f876, f885, f895, f927, f928, f933, f943, f948, f957, f958, f959, f960, f965, f977, f988, f990, f992, f993, f1003, f1006, f1023, f1026, f1032, f1036, f1044, f1053, f1058, f1059, f1062, f1065, f1070, f1071, f1077, f1078, f1095, f1098, f1101, f1108, f1112, f1115, f1120, f1124, f1127, f1131, f1134, f1140, f1146, f1152, f1155, f1167, f1180, f1183, f1186, f1188, f1202, f1204, f1210, f1214, f1216, f1221, f1223, f1224, f1234, f1242, f1245, f1252, f1261, f1294, f1297, f1311, f1314, f1322, f1325, f1343, f1347, f1350, f1363, f1372, f1381, f1391, f1399, f1426, f1443, f1445, f1446, f1452, f1458, f1469, f1477, f1480, f1490, f1492, f1496, f1499, f1503, f1508, f1526, f1538, f1557, f1565, f1570, f1579, f1592, f1594, f1622, f1626, f1654, f1655, f1660, f1682, f1697, f1707, f1725, f1727, f1734, f1737, f1740, f1744, f1774, f1783, f1789, f1812, f1850, f1862, f1868, f1880, f1885, f1891, f1892, f1894, f1898, f1939, f1942, f1945, f1954, f1966, f1972, f1979, f1983, f1993, f1995, f2020, f2027, f2034, f2035, f2069, f2087, f2089, f2091, f2100, f2107, f2128, f2136, f2142, f2163, f2170, f2172, f2184, f2188, f2211, f2236, f2237, f2258, f2259, f2272, f2292, f2294, f2297, f2303, f2319, f2320, f2327, f2332, f2348, f2372, f2380, f2383, f2385, f2396, f2397, f2408, f2418, f2436, f2447, f2449, f2462, f2463, f2465, f2477, f2479, f2488, f2490, f2494, f2501, f2514, f2524, f2533, f2539, f2554, f2558, f2576, f2577, f2584, f2598, f2599, f2600, f2614, f2618, f2627, f2628, f2632, f2638, f2654, f2659, f2660, f2673, f2676, f2697, f2711, f2719, f2734, f2739, f2740, f2746, f2751, f2755, f2758, f2763, f2770, f2772, f2788, f2806, f2818, f2819, f2821, f2854, f2858, f2869, f2882, f2886, f2888, f2900, f2901, f2902, f2911, f2912, f2918, f2923, f2933, f2939, f2945, f2950, f2954, f2960, f2962, f2981, f2984, f2986, f2991, f2995, f2997, f3009, f3030, f3038, f3043, f3052, f3077, f3079, f3086, f3098, f3099, f3100, f3101, f3105, f3107, f3111, f3138, f3140, f3142, f3156, f3157, f3159, f3164, f3166, f3176, f3180, f3182, f3186, f3191, f3205, f3206, f3207, f3208, f3209, f3215, f3218, f3224, f3238, f3255, f3257, f3259, f3261, f3263, f3264, f3266, f3270, f3272, f3274, f3276, f3280, f3282, f3284, f3298, f3299, f3305, f3313, f3314, f3316, f3323, f3324, f3329, f3337, f3338, f3347, f3348, f3361, f3370])).
fof(f3370, plain, (~ spl15_4 | ~ spl15_8), inference(avatar_split_clause, [], [f3368, f353, f336])).
fof(f336, plain, (spl15_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f353, plain, (spl15_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f3368, plain, (~ (e3 = op(e3, e3)) | ~ spl15_8), inference(backward_demodulation, [], [f180, f355])).
fof(f355, plain, ((e3 = op(e3, e2)) | ~ spl15_8), inference(avatar_component_clause, [], [f353])).
fof(f180, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax3)).
fof(f3361, plain, (~ spl15_7 | ~ spl15_15), inference(avatar_split_clause, [], [f3357, f383, f349])).
fof(f349, plain, (spl15_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f383, plain, (spl15_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f3357, plain, (~ (e2 = op(e3, e2)) | ~ spl15_15), inference(backward_demodulation, [], [f176, f385])).
fof(f385, plain, ((e2 = op(e3, e0)) | ~ spl15_15), inference(avatar_component_clause, [], [f383])).
fof(f176, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3348, plain, (~ spl15_19 | ~ spl15_23), inference(avatar_split_clause, [], [f3340, f417, f400])).
fof(f400, plain, (spl15_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_19])])).
fof(f417, plain, (spl15_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_23])])).
fof(f3340, plain, (~ (e2 = op(e2, e3)) | ~ spl15_23), inference(backward_demodulation, [], [f174, f419])).
fof(f419, plain, ((e2 = op(e2, e2)) | ~ spl15_23), inference(avatar_component_clause, [], [f417])).
fof(f174, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3347, plain, (~ spl15_7 | ~ spl15_23), inference(avatar_split_clause, [], [f3339, f417, f349])).
fof(f3339, plain, (~ (e2 = op(e3, e2)) | ~ spl15_23), inference(backward_demodulation, [], [f150, f419])).
fof(f150, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3338, plain, (~ spl15_22 | ~ spl15_26), inference(avatar_split_clause, [], [f3335, f430, f413])).
fof(f413, plain, (spl15_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_22])])).
fof(f430, plain, (spl15_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_26])])).
fof(f3335, plain, (~ (e1 = op(e2, e2)) | ~ spl15_26), inference(backward_demodulation, [], [f172, f432])).
fof(f432, plain, ((e1 = op(e2, e1)) | ~ spl15_26), inference(avatar_component_clause, [], [f430])).
fof(f172, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3337, plain, (~ spl15_10 | ~ spl15_26), inference(avatar_split_clause, [], [f3331, f430, f362])).
fof(f362, plain, (spl15_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f3331, plain, (~ (e1 = op(e3, e1)) | ~ spl15_26), inference(backward_demodulation, [], [f144, f432])).
fof(f144, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3329, plain, (~ spl15_19 | ~ spl15_35), inference(avatar_split_clause, [], [f3325, f468, f400])).
fof(f468, plain, (spl15_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_35])])).
fof(f3325, plain, (~ (e2 = op(e2, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f154, f470])).
fof(f470, plain, ((e2 = op(e1, e3)) | ~ spl15_35), inference(avatar_component_clause, [], [f468])).
fof(f154, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3324, plain, (~ spl15_34 | ~ spl15_38), inference(avatar_split_clause, [], [f3319, f481, f464])).
fof(f464, plain, (spl15_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_34])])).
fof(f481, plain, (spl15_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_38])])).
fof(f3319, plain, (~ (e1 = op(e1, e3)) | ~ spl15_38), inference(backward_demodulation, [], [f168, f483])).
fof(f483, plain, ((e1 = op(e1, e2)) | ~ spl15_38), inference(avatar_component_clause, [], [f481])).
fof(f168, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3323, plain, (~ spl15_22 | ~ spl15_38), inference(avatar_split_clause, [], [f3317, f481, f413])).
fof(f3317, plain, (~ (e1 = op(e2, e2)) | ~ spl15_38), inference(backward_demodulation, [], [f148, f483])).
fof(f148, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3316, plain, (~ spl15_45 | spl15_84), inference(avatar_contradiction_clause, [], [f3315])).
fof(f3315, plain, ($false | (~ spl15_45 | spl15_84)), inference(subsumption_resolution, [], [f3312, f513])).
fof(f513, plain, ((e0 = op(e1, e0)) | ~ spl15_45), inference(avatar_component_clause, [], [f511])).
fof(f511, plain, (spl15_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_45])])).
fof(f3312, plain, (~ (e0 = op(e1, e0)) | (~ spl15_45 | spl15_84)), inference(backward_demodulation, [], [f751, f513])).
fof(f751, plain, (~ (e0 = op(e1, op(e1, e0))) | spl15_84), inference(avatar_component_clause, [], [f749])).
fof(f749, plain, (spl15_84 <=> (e0 = op(e1, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_84])])).
fof(f3314, plain, (~ spl15_37 | ~ spl15_45), inference(avatar_split_clause, [], [f3307, f511, f477])).
fof(f477, plain, (spl15_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_37])])).
fof(f3307, plain, (~ (e0 = op(e1, e2)) | ~ spl15_45), inference(backward_demodulation, [], [f164, f513])).
fof(f164, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3313, plain, (~ spl15_13 | ~ spl15_45), inference(avatar_split_clause, [], [f3306, f511, f375])).
fof(f375, plain, (spl15_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f3306, plain, (~ (e0 = op(e3, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f137, f513])).
fof(f137, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3305, plain, (~ spl15_4 | ~ spl15_52), inference(avatar_split_clause, [], [f3303, f540, f336])).
fof(f540, plain, (spl15_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_52])])).
fof(f3303, plain, (~ (e3 = op(e3, e3)) | ~ spl15_52), inference(backward_demodulation, [], [f153, f542])).
fof(f542, plain, ((e3 = op(e0, e3)) | ~ spl15_52), inference(avatar_component_clause, [], [f540])).
fof(f153, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3299, plain, (~ spl15_49 | ~ spl15_53), inference(avatar_split_clause, [], [f3293, f545, f528])).
fof(f528, plain, (spl15_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_49])])).
fof(f545, plain, (spl15_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_53])])).
fof(f3293, plain, (~ (e0 = op(e0, e3)) | ~ spl15_53), inference(backward_demodulation, [], [f162, f547])).
fof(f547, plain, ((e0 = op(e0, e2)) | ~ spl15_53), inference(avatar_component_clause, [], [f545])).
fof(f162, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3298, plain, (~ spl15_37 | ~ spl15_53), inference(avatar_split_clause, [], [f3290, f545, f477])).
fof(f3290, plain, (~ (e0 = op(e1, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f145, f547])).
fof(f145, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3284, plain, (~ spl15_47 | ~ spl15_62 | spl15_115), inference(avatar_split_clause, [], [f3283, f950, f583, f519])).
fof(f519, plain, (spl15_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_47])])).
fof(f583, plain, (spl15_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl15_62])])).
fof(f950, plain, (spl15_115 <=> (e2 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_115])])).
fof(f3283, plain, (~ (e2 = op(e1, e0)) | (~ spl15_62 | spl15_115)), inference(forward_demodulation, [], [f952, f585])).
fof(f585, plain, ((op(e0, e0) = e1) | ~ spl15_62), inference(avatar_component_clause, [], [f583])).
fof(f952, plain, (~ (e2 = op(op(e0, e0), e0)) | spl15_115), inference(avatar_component_clause, [], [f950])).
fof(f3282, plain, (~ spl15_51 | ~ spl15_59), inference(avatar_split_clause, [], [f3281, f570, f536])).
fof(f536, plain, (spl15_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_51])])).
fof(f570, plain, (spl15_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_59])])).
fof(f3281, plain, (~ (e2 = op(e0, e3)) | ~ spl15_59), inference(forward_demodulation, [], [f161, f572])).
fof(f572, plain, ((e2 = op(e0, e1)) | ~ spl15_59), inference(avatar_component_clause, [], [f570])).
fof(f161, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3280, plain, (~ spl15_50 | ~ spl15_62), inference(avatar_split_clause, [], [f3279, f583, f532])).
fof(f532, plain, (spl15_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_50])])).
fof(f3279, plain, (~ (e1 = op(e0, e3)) | ~ spl15_62), inference(forward_demodulation, [], [f159, f585])).
fof(f159, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3276, plain, (~ spl15_40 | ~ spl15_44), inference(avatar_split_clause, [], [f3275, f506, f489])).
fof(f489, plain, (spl15_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_40])])).
fof(f506, plain, (spl15_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_44])])).
fof(f3275, plain, (~ (e3 = op(e1, e2)) | ~ spl15_44), inference(forward_demodulation, [], [f166, f508])).
fof(f508, plain, ((e3 = op(e1, e1)) | ~ spl15_44), inference(avatar_component_clause, [], [f506])).
fof(f166, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3274, plain, (~ spl15_36 | ~ spl15_44), inference(avatar_split_clause, [], [f3273, f506, f472])).
fof(f472, plain, (spl15_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_36])])).
fof(f3273, plain, (~ (e3 = op(e1, e3)) | ~ spl15_44), inference(forward_demodulation, [], [f167, f508])).
fof(f167, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3272, plain, (~ spl15_28 | ~ spl15_44), inference(avatar_split_clause, [], [f3271, f506, f438])).
fof(f438, plain, (spl15_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_28])])).
fof(f3271, plain, (~ (e3 = op(e2, e1)) | ~ spl15_44), inference(forward_demodulation, [], [f142, f508])).
fof(f142, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3270, plain, (~ spl15_27 | ~ spl15_59), inference(avatar_split_clause, [], [f3269, f570, f434])).
fof(f434, plain, (spl15_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_27])])).
fof(f3269, plain, (~ (e2 = op(e2, e1)) | ~ spl15_59), inference(forward_demodulation, [], [f140, f572])).
fof(f140, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3266, plain, (~ spl15_20 | ~ spl15_32), inference(avatar_split_clause, [], [f3265, f455, f404])).
fof(f404, plain, (spl15_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f455, plain, (spl15_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_32])])).
fof(f3265, plain, (~ (e3 = op(e2, e3)) | ~ spl15_32), inference(forward_demodulation, [], [f171, f457])).
fof(f457, plain, ((e3 = op(e2, e0)) | ~ spl15_32), inference(avatar_component_clause, [], [f455])).
fof(f171, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3264, plain, (spl15_17 | ~ spl15_32 | ~ spl15_76), inference(avatar_split_clause, [], [f3237, f693, f455, f392])).
fof(f392, plain, (spl15_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_17])])).
fof(f693, plain, (spl15_76 <=> (e0 = op(e2, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_76])])).
fof(f3237, plain, ((e0 = op(e2, e3)) | (~ spl15_32 | ~ spl15_76)), inference(backward_demodulation, [], [f694, f457])).
fof(f694, plain, ((e0 = op(e2, op(e2, e0))) | ~ spl15_76), inference(avatar_component_clause, [], [f693])).
fof(f3263, plain, (~ spl15_14 | ~ spl15_62), inference(avatar_split_clause, [], [f3262, f583, f379])).
fof(f379, plain, (spl15_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f3262, plain, (~ (e1 = op(e3, e0)) | ~ spl15_62), inference(forward_demodulation, [], [f135, f585])).
fof(f135, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3261, plain, (~ spl15_16 | ~ spl15_32), inference(avatar_split_clause, [], [f3260, f455, f387])).
fof(f387, plain, (spl15_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f3260, plain, (~ (e3 = op(e3, e0)) | ~ spl15_32), inference(forward_demodulation, [], [f138, f457])).
fof(f138, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3259, plain, (~ spl15_12 | ~ spl15_44), inference(avatar_split_clause, [], [f3258, f506, f370])).
fof(f370, plain, (spl15_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f3258, plain, (~ (e3 = op(e3, e1)) | ~ spl15_44), inference(forward_demodulation, [], [f143, f508])).
fof(f143, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3257, plain, (~ spl15_11 | ~ spl15_59), inference(avatar_split_clause, [], [f3256, f570, f366])).
fof(f366, plain, (spl15_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f3256, plain, (~ (e2 = op(e3, e1)) | ~ spl15_59), inference(forward_demodulation, [], [f141, f572])).
fof(f141, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3255, plain, (~ spl15_11 | ~ spl15_44 | spl15_112), inference(avatar_split_clause, [], [f3229, f936, f506, f366])).
fof(f936, plain, (spl15_112 <=> (e2 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_112])])).
fof(f3229, plain, (~ (e2 = op(e3, e1)) | (~ spl15_44 | spl15_112)), inference(backward_demodulation, [], [f938, f508])).
fof(f938, plain, (~ (e2 = op(op(e1, e1), e1)) | spl15_112), inference(avatar_component_clause, [], [f936])).
fof(f3238, plain, (~ spl15_24 | ~ spl15_32), inference(avatar_split_clause, [], [f3236, f455, f421])).
fof(f421, plain, (spl15_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_24])])).
fof(f3236, plain, (~ (e3 = op(e2, e2)) | ~ spl15_32), inference(backward_demodulation, [], [f170, f457])).
fof(f170, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3224, plain, (~ spl15_43 | ~ spl15_47), inference(avatar_split_clause, [], [f3221, f519, f502])).
fof(f502, plain, (spl15_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_43])])).
fof(f3221, plain, (~ (e2 = op(e1, e1)) | ~ spl15_47), inference(backward_demodulation, [], [f163, f521])).
fof(f521, plain, ((e2 = op(e1, e0)) | ~ spl15_47), inference(avatar_component_clause, [], [f519])).
fof(f163, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3218, plain, (~ spl15_24 | ~ spl15_56), inference(avatar_split_clause, [], [f3216, f557, f421])).
fof(f557, plain, (spl15_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_56])])).
fof(f3216, plain, (~ (e3 = op(e2, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f146, f559])).
fof(f559, plain, ((e3 = op(e0, e2)) | ~ spl15_56), inference(avatar_component_clause, [], [f557])).
fof(f146, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3215, plain, (~ spl15_43 | ~ spl15_59), inference(avatar_split_clause, [], [f3212, f570, f502])).
fof(f3212, plain, (~ (e2 = op(e1, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f139, f572])).
fof(f139, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3209, plain, (~ spl15_48 | ~ spl15_62 | spl15_119), inference(avatar_split_clause, [], [f3204, f972, f583, f523])).
fof(f523, plain, (spl15_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_48])])).
fof(f972, plain, (spl15_119 <=> (e3 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_119])])).
fof(f3204, plain, (~ (e3 = op(e1, e0)) | (~ spl15_62 | spl15_119)), inference(backward_demodulation, [], [f974, f585])).
fof(f974, plain, (~ (e3 = op(op(e0, e0), e0)) | spl15_119), inference(avatar_component_clause, [], [f972])).
fof(f3208, plain, (~ spl15_44 | ~ spl15_62 | spl15_111), inference(avatar_split_clause, [], [f3202, f930, f583, f506])).
fof(f930, plain, (spl15_111 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_111])])).
fof(f3202, plain, (~ (e3 = op(e1, e1)) | (~ spl15_62 | spl15_111)), inference(backward_demodulation, [], [f932, f585])).
fof(f932, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl15_111), inference(avatar_component_clause, [], [f930])).
fof(f3207, plain, (~ spl15_43 | ~ spl15_62 | spl15_110), inference(avatar_split_clause, [], [f3201, f924, f583, f502])).
fof(f924, plain, (spl15_110 <=> (e2 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_110])])).
fof(f3201, plain, (~ (e2 = op(e1, e1)) | (~ spl15_62 | spl15_110)), inference(backward_demodulation, [], [f926, f585])).
fof(f926, plain, (~ (e2 = op(op(e0, e0), op(e0, e0))) | spl15_110), inference(avatar_component_clause, [], [f924])).
fof(f3206, plain, (~ spl15_54 | ~ spl15_62), inference(avatar_split_clause, [], [f3197, f583, f549])).
fof(f549, plain, (spl15_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_54])])).
fof(f3197, plain, (~ (e1 = op(e0, e2)) | ~ spl15_62), inference(backward_demodulation, [], [f158, f585])).
fof(f158, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3205, plain, (~ spl15_30 | ~ spl15_62), inference(avatar_split_clause, [], [f3195, f583, f447])).
fof(f447, plain, (spl15_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_30])])).
fof(f3195, plain, (~ (e1 = op(e2, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f134, f585])).
fof(f134, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3191, plain, (spl15_47 | ~ spl15_37 | ~ spl15_82), inference(avatar_split_clause, [], [f3190, f739, f477, f519])).
fof(f739, plain, (spl15_82 <=> (e2 = op(e1, op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_82])])).
fof(f3190, plain, ((e2 = op(e1, e0)) | (~ spl15_37 | ~ spl15_82)), inference(forward_demodulation, [], [f740, f479])).
fof(f479, plain, ((e0 = op(e1, e2)) | ~ spl15_37), inference(avatar_component_clause, [], [f477])).
fof(f740, plain, ((e2 = op(e1, op(e1, e2))) | ~ spl15_82), inference(avatar_component_clause, [], [f739])).
fof(f3186, plain, (~ spl15_55 | ~ spl15_7), inference(avatar_split_clause, [], [f3185, f349, f553])).
fof(f553, plain, (spl15_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_55])])).
fof(f3185, plain, (~ (e2 = op(e0, e2)) | ~ spl15_7), inference(forward_demodulation, [], [f147, f351])).
fof(f351, plain, ((e2 = op(e3, e2)) | ~ spl15_7), inference(avatar_component_clause, [], [f349])).
fof(f147, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3182, plain, (~ spl15_31 | ~ spl15_19), inference(avatar_split_clause, [], [f3181, f400, f451])).
fof(f451, plain, (spl15_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_31])])).
fof(f3181, plain, (~ (e2 = op(e2, e0)) | ~ spl15_19), inference(forward_demodulation, [], [f171, f402])).
fof(f402, plain, ((e2 = op(e2, e3)) | ~ spl15_19), inference(avatar_component_clause, [], [f400])).
fof(f3180, plain, (spl15_30 | ~ spl15_25 | ~ spl15_75), inference(avatar_split_clause, [], [f3144, f688, f426, f447])).
fof(f426, plain, (spl15_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_25])])).
fof(f688, plain, (spl15_75 <=> (e1 = op(e2, op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_75])])).
fof(f3144, plain, ((e1 = op(e2, e0)) | (~ spl15_25 | ~ spl15_75)), inference(forward_demodulation, [], [f689, f428])).
fof(f428, plain, ((e0 = op(e2, e1)) | ~ spl15_25), inference(avatar_component_clause, [], [f426])).
fof(f689, plain, ((e1 = op(e2, op(e2, e1))) | ~ spl15_75), inference(avatar_component_clause, [], [f688])).
fof(f3176, plain, (~ spl15_24 | ~ spl15_43 | spl15_103), inference(avatar_contradiction_clause, [], [f3175])).
fof(f3175, plain, ($false | (~ spl15_24 | ~ spl15_43 | spl15_103)), inference(subsumption_resolution, [], [f3171, f423])).
fof(f423, plain, ((e3 = op(e2, e2)) | ~ spl15_24), inference(avatar_component_clause, [], [f421])).
fof(f3171, plain, (~ (e3 = op(e2, e2)) | (~ spl15_43 | spl15_103)), inference(backward_demodulation, [], [f894, f504])).
fof(f504, plain, ((e2 = op(e1, e1)) | ~ spl15_43), inference(avatar_component_clause, [], [f502])).
fof(f894, plain, (~ (e3 = op(op(e1, e1), op(e1, e1))) | spl15_103), inference(avatar_component_clause, [], [f892])).
fof(f892, plain, (spl15_103 <=> (e3 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_103])])).
fof(f3166, plain, (~ spl15_44 | ~ spl15_48), inference(avatar_split_clause, [], [f3165, f523, f506])).
fof(f3165, plain, (~ (e3 = op(e1, e1)) | ~ spl15_48), inference(backward_demodulation, [], [f163, f525])).
fof(f525, plain, ((e3 = op(e1, e0)) | ~ spl15_48), inference(avatar_component_clause, [], [f523])).
fof(f3164, plain, (~ spl15_44 | ~ spl15_60), inference(avatar_split_clause, [], [f3162, f574, f506])).
fof(f574, plain, (spl15_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_60])])).
fof(f3162, plain, (~ (e3 = op(e1, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f139, f576])).
fof(f576, plain, ((e3 = op(e0, e1)) | ~ spl15_60), inference(avatar_component_clause, [], [f574])).
fof(f3159, plain, (~ spl15_30 | ~ spl15_63 | spl15_109), inference(avatar_contradiction_clause, [], [f3158])).
fof(f3158, plain, ($false | (~ spl15_30 | ~ spl15_63 | spl15_109)), inference(subsumption_resolution, [], [f3151, f449])).
fof(f449, plain, ((e1 = op(e2, e0)) | ~ spl15_30), inference(avatar_component_clause, [], [f447])).
fof(f3151, plain, (~ (e1 = op(e2, e0)) | (~ spl15_63 | spl15_109)), inference(backward_demodulation, [], [f922, f589])).
fof(f589, plain, ((op(e0, e0) = e2) | ~ spl15_63), inference(avatar_component_clause, [], [f587])).
fof(f587, plain, (spl15_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl15_63])])).
fof(f922, plain, (~ (e1 = op(op(e0, e0), e0)) | spl15_109), inference(avatar_component_clause, [], [f920])).
fof(f920, plain, (spl15_109 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_109])])).
fof(f3157, plain, (~ spl15_59 | ~ spl15_63), inference(avatar_split_clause, [], [f3148, f587, f570])).
fof(f3148, plain, (~ (e2 = op(e0, e1)) | ~ spl15_63), inference(backward_demodulation, [], [f157, f589])).
fof(f157, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f3156, plain, (~ spl15_47 | ~ spl15_63), inference(avatar_split_clause, [], [f3147, f587, f519])).
fof(f3147, plain, (~ (e2 = op(e1, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f133, f589])).
fof(f133, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f3142, plain, (~ spl15_44 | ~ spl15_34 | spl15_81), inference(avatar_split_clause, [], [f3141, f734, f464, f506])).
fof(f734, plain, (spl15_81 <=> (e3 = op(e1, op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_81])])).
fof(f3141, plain, (~ (e3 = op(e1, e1)) | (~ spl15_34 | spl15_81)), inference(forward_demodulation, [], [f736, f466])).
fof(f466, plain, ((e1 = op(e1, e3)) | ~ spl15_34), inference(avatar_component_clause, [], [f464])).
fof(f736, plain, (~ (e3 = op(e1, op(e1, e3))) | spl15_81), inference(avatar_component_clause, [], [f734])).
fof(f3140, plain, (~ spl15_47 | ~ spl15_37 | spl15_82), inference(avatar_split_clause, [], [f3139, f739, f477, f519])).
fof(f3139, plain, (~ (e2 = op(e1, e0)) | (~ spl15_37 | spl15_82)), inference(forward_demodulation, [], [f741, f479])).
fof(f741, plain, (~ (e2 = op(e1, op(e1, e2))) | spl15_82), inference(avatar_component_clause, [], [f739])).
fof(f3138, plain, (~ spl15_64 | ~ spl15_49 | spl15_89), inference(avatar_split_clause, [], [f3137, f790, f528, f591])).
fof(f591, plain, (spl15_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl15_64])])).
fof(f790, plain, (spl15_89 <=> (e3 = op(e0, op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_89])])).
fof(f3137, plain, (~ (op(e0, e0) = e3) | (~ spl15_49 | spl15_89)), inference(forward_demodulation, [], [f792, f530])).
fof(f530, plain, ((e0 = op(e0, e3)) | ~ spl15_49), inference(avatar_component_clause, [], [f528])).
fof(f792, plain, (~ (e3 = op(e0, op(e0, e3))) | spl15_89), inference(avatar_component_clause, [], [f790])).
fof(f3111, plain, (~ spl15_3 | ~ spl15_7), inference(avatar_split_clause, [], [f3108, f349, f332])).
fof(f332, plain, (spl15_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f3108, plain, (~ (e2 = op(e3, e3)) | ~ spl15_7), inference(backward_demodulation, [], [f180, f351])).
fof(f3107, plain, (~ spl15_19 | ~ spl15_24 | spl15_73), inference(avatar_contradiction_clause, [], [f3106])).
fof(f3106, plain, ($false | (~ spl15_19 | ~ spl15_24 | spl15_73)), inference(subsumption_resolution, [], [f3104, f423])).
fof(f3104, plain, (~ (e3 = op(e2, e2)) | (~ spl15_19 | spl15_73)), inference(backward_demodulation, [], [f680, f402])).
fof(f680, plain, (~ (e3 = op(e2, op(e2, e3))) | spl15_73), inference(avatar_component_clause, [], [f678])).
fof(f678, plain, (spl15_73 <=> (e3 = op(e2, op(e2, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_73])])).
fof(f3105, plain, (~ spl15_3 | ~ spl15_19), inference(avatar_split_clause, [], [f3102, f400, f332])).
fof(f3102, plain, (~ (e2 = op(e3, e3)) | ~ spl15_19), inference(backward_demodulation, [], [f156, f402])).
fof(f156, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3101, plain, (~ spl15_4 | ~ spl15_24 | spl15_104), inference(avatar_split_clause, [], [f3095, f897, f421, f336])).
fof(f897, plain, (spl15_104 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_104])])).
fof(f3095, plain, (~ (e3 = op(e3, e3)) | (~ spl15_24 | spl15_104)), inference(backward_demodulation, [], [f899, f423])).
fof(f899, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl15_104), inference(avatar_component_clause, [], [f897])).
fof(f3100, plain, (~ spl15_19 | ~ spl15_24 | spl15_74), inference(avatar_split_clause, [], [f3092, f683, f421, f400])).
fof(f683, plain, (spl15_74 <=> (e2 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_74])])).
fof(f3092, plain, (~ (e2 = op(e2, e3)) | (~ spl15_24 | spl15_74)), inference(backward_demodulation, [], [f685, f423])).
fof(f685, plain, (~ (e2 = op(e2, op(e2, e2))) | spl15_74), inference(avatar_component_clause, [], [f683])).
fof(f3099, plain, (~ spl15_20 | ~ spl15_24), inference(avatar_split_clause, [], [f3091, f421, f404])).
fof(f3091, plain, (~ (e3 = op(e2, e3)) | ~ spl15_24), inference(backward_demodulation, [], [f174, f423])).
fof(f3098, plain, (~ spl15_8 | ~ spl15_24), inference(avatar_split_clause, [], [f3090, f421, f353])).
fof(f3090, plain, (~ (e3 = op(e3, e2)) | ~ spl15_24), inference(backward_demodulation, [], [f150, f423])).
fof(f3086, plain, (~ spl15_25 | ~ spl15_30 | spl15_76), inference(avatar_contradiction_clause, [], [f3085])).
fof(f3085, plain, ($false | (~ spl15_25 | ~ spl15_30 | spl15_76)), inference(subsumption_resolution, [], [f3084, f428])).
fof(f3084, plain, (~ (e0 = op(e2, e1)) | (~ spl15_30 | spl15_76)), inference(forward_demodulation, [], [f695, f449])).
fof(f695, plain, (~ (e0 = op(e2, op(e2, e0))) | spl15_76), inference(avatar_component_clause, [], [f693])).
fof(f3079, plain, (~ spl15_3 | ~ spl15_44 | spl15_101), inference(avatar_split_clause, [], [f3078, f882, f506, f332])).
fof(f882, plain, (spl15_101 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_101])])).
fof(f3078, plain, (~ (e2 = op(e3, e3)) | (~ spl15_44 | spl15_101)), inference(forward_demodulation, [], [f884, f508])).
fof(f884, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl15_101), inference(avatar_component_clause, [], [f882])).
fof(f3077, plain, (~ spl15_3 | ~ spl15_64 | spl15_110), inference(avatar_split_clause, [], [f3076, f924, f591, f332])).
fof(f3076, plain, (~ (e2 = op(e3, e3)) | (~ spl15_64 | spl15_110)), inference(forward_demodulation, [], [f926, f593])).
fof(f593, plain, ((op(e0, e0) = e3) | ~ spl15_64), inference(avatar_component_clause, [], [f591])).
fof(f3052, plain, (~ spl15_2 | ~ spl15_10), inference(avatar_split_clause, [], [f3048, f362, f328])).
fof(f328, plain, (spl15_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_2])])).
fof(f3048, plain, (~ (e1 = op(e3, e3)) | ~ spl15_10), inference(backward_demodulation, [], [f179, f364])).
fof(f364, plain, ((e1 = op(e3, e1)) | ~ spl15_10), inference(avatar_component_clause, [], [f362])).
fof(f179, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3043, plain, (~ spl15_2 | ~ spl15_34), inference(avatar_split_clause, [], [f3040, f464, f328])).
fof(f3040, plain, (~ (e1 = op(e3, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f155, f466])).
fof(f155, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3038, plain, (~ spl15_33 | ~ spl15_37), inference(avatar_split_clause, [], [f3037, f477, f460])).
fof(f460, plain, (spl15_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_33])])).
fof(f3037, plain, (~ (e0 = op(e1, e3)) | ~ spl15_37), inference(backward_demodulation, [], [f168, f479])).
fof(f3030, plain, (~ spl15_33 | ~ spl15_49), inference(avatar_split_clause, [], [f3024, f528, f460])).
fof(f3024, plain, (~ (e0 = op(e1, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f151, f530])).
fof(f151, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3009, plain, (spl15_37 | ~ spl15_47 | ~ spl15_84), inference(avatar_split_clause, [], [f3008, f749, f519, f477])).
fof(f3008, plain, ((e0 = op(e1, e2)) | (~ spl15_47 | ~ spl15_84)), inference(forward_demodulation, [], [f750, f521])).
fof(f750, plain, ((e0 = op(e1, op(e1, e0))) | ~ spl15_84), inference(avatar_component_clause, [], [f749])).
fof(f2997, plain, (spl15_3 | ~ spl15_64 | ~ spl15_110), inference(avatar_split_clause, [], [f2996, f924, f591, f332])).
fof(f2996, plain, ((e2 = op(e3, e3)) | (~ spl15_64 | ~ spl15_110)), inference(forward_demodulation, [], [f925, f593])).
fof(f925, plain, ((e2 = op(op(e0, e0), op(e0, e0))) | ~ spl15_110), inference(avatar_component_clause, [], [f924])).
fof(f2995, plain, (~ spl15_2 | ~ spl15_64 | spl15_116), inference(avatar_split_clause, [], [f2994, f954, f591, f328])).
fof(f954, plain, (spl15_116 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_116])])).
fof(f2994, plain, (~ (e1 = op(e3, e3)) | (~ spl15_64 | spl15_116)), inference(forward_demodulation, [], [f956, f593])).
fof(f956, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl15_116), inference(avatar_component_clause, [], [f954])).
fof(f2991, plain, (~ spl15_49 | ~ spl15_64 | spl15_92), inference(avatar_split_clause, [], [f2895, f805, f591, f528])).
fof(f805, plain, (spl15_92 <=> (e0 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_92])])).
fof(f2895, plain, (~ (e0 = op(e0, e3)) | (~ spl15_64 | spl15_92)), inference(backward_demodulation, [], [f807, f593])).
fof(f807, plain, (~ (e0 = op(e0, op(e0, e0))) | spl15_92), inference(avatar_component_clause, [], [f805])).
fof(f2986, plain, (~ spl15_39 | ~ spl15_47), inference(avatar_split_clause, [], [f2985, f519, f485])).
fof(f485, plain, (spl15_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_39])])).
fof(f2985, plain, (~ (e2 = op(e1, e2)) | ~ spl15_47), inference(forward_demodulation, [], [f164, f521])).
fof(f2984, plain, (~ spl15_38 | ~ spl15_54), inference(avatar_split_clause, [], [f2983, f549, f481])).
fof(f2983, plain, (~ (e1 = op(e1, e2)) | ~ spl15_54), inference(forward_demodulation, [], [f145, f551])).
fof(f551, plain, ((e1 = op(e0, e2)) | ~ spl15_54), inference(avatar_component_clause, [], [f549])).
fof(f2981, plain, (~ spl15_26 | ~ spl15_30), inference(avatar_split_clause, [], [f2980, f447, f430])).
fof(f2980, plain, (~ (e1 = op(e2, e1)) | ~ spl15_30), inference(forward_demodulation, [], [f169, f449])).
fof(f169, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2962, plain, (~ spl15_13 | spl15_69), inference(avatar_contradiction_clause, [], [f2961])).
fof(f2961, plain, ($false | (~ spl15_13 | spl15_69)), inference(subsumption_resolution, [], [f2959, f377])).
fof(f377, plain, ((e0 = op(e3, e0)) | ~ spl15_13), inference(avatar_component_clause, [], [f375])).
fof(f2959, plain, (~ (e0 = op(e3, e0)) | (~ spl15_13 | spl15_69)), inference(backward_demodulation, [], [f649, f377])).
fof(f649, plain, (~ (e0 = op(e3, op(e3, e0))) | spl15_69), inference(avatar_component_clause, [], [f647])).
fof(f647, plain, (spl15_69 <=> (e0 = op(e3, op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_69])])).
fof(f2960, plain, (~ spl15_1 | ~ spl15_13), inference(avatar_split_clause, [], [f2955, f375, f324])).
fof(f324, plain, (spl15_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f2955, plain, (~ (e0 = op(e3, e3)) | ~ spl15_13), inference(backward_demodulation, [], [f177, f377])).
fof(f177, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2954, plain, (~ spl15_21 | ~ spl15_23), inference(avatar_contradiction_clause, [], [f2953])).
fof(f2953, plain, ($false | (~ spl15_21 | ~ spl15_23)), inference(subsumption_resolution, [], [f2952, f182])).
fof(f182, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax4)).
fof(f2952, plain, ((e0 = e2) | (~ spl15_21 | ~ spl15_23)), inference(backward_demodulation, [], [f419, f411])).
fof(f411, plain, ((e0 = op(e2, e2)) | ~ spl15_21), inference(avatar_component_clause, [], [f409])).
fof(f409, plain, (spl15_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f2950, plain, (~ spl15_14 | ~ spl15_30), inference(avatar_split_clause, [], [f2947, f447, f379])).
fof(f2947, plain, (~ (e1 = op(e3, e0)) | ~ spl15_30), inference(backward_demodulation, [], [f138, f449])).
fof(f2945, plain, (~ spl15_1 | ~ spl15_33), inference(avatar_split_clause, [], [f2940, f460, f324])).
fof(f2940, plain, (~ (e0 = op(e3, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f155, f462])).
fof(f462, plain, ((e0 = op(e1, e3)) | ~ spl15_33), inference(avatar_component_clause, [], [f460])).
fof(f2939, plain, (~ spl15_1 | ~ spl15_44 | spl15_113), inference(avatar_split_clause, [], [f2938, f940, f506, f324])).
fof(f940, plain, (spl15_113 <=> (e0 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_113])])).
fof(f2938, plain, (~ (e0 = op(e3, e3)) | (~ spl15_44 | spl15_113)), inference(backward_demodulation, [], [f942, f508])).
fof(f942, plain, (~ (e0 = op(op(e1, e1), op(e1, e1))) | spl15_113), inference(avatar_component_clause, [], [f940])).
fof(f2933, plain, (~ spl15_35 | ~ spl15_47), inference(avatar_split_clause, [], [f2930, f519, f468])).
fof(f2930, plain, (~ (e2 = op(e1, e3)) | ~ spl15_47), inference(backward_demodulation, [], [f165, f521])).
fof(f165, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2923, plain, (~ spl15_35 | ~ spl15_51), inference(avatar_split_clause, [], [f2919, f536, f468])).
fof(f2919, plain, (~ (e2 = op(e1, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f151, f538])).
fof(f538, plain, ((e2 = op(e0, e3)) | ~ spl15_51), inference(avatar_component_clause, [], [f536])).
fof(f2918, plain, (~ spl15_50 | ~ spl15_54), inference(avatar_split_clause, [], [f2915, f549, f532])).
fof(f2915, plain, (~ (e1 = op(e0, e3)) | ~ spl15_54), inference(backward_demodulation, [], [f162, f551])).
fof(f2912, plain, (~ spl15_53 | ~ spl15_57), inference(avatar_split_clause, [], [f2905, f562, f545])).
fof(f562, plain, (spl15_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_57])])).
fof(f2905, plain, (~ (e0 = op(e0, e2)) | ~ spl15_57), inference(backward_demodulation, [], [f160, f564])).
fof(f564, plain, ((e0 = op(e0, e1)) | ~ spl15_57), inference(avatar_component_clause, [], [f562])).
fof(f160, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2911, plain, (~ spl15_41 | ~ spl15_57), inference(avatar_split_clause, [], [f2904, f562, f494])).
fof(f494, plain, (spl15_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_41])])).
fof(f2904, plain, (~ (e0 = op(e1, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f139, f564])).
fof(f2902, plain, (~ spl15_14 | ~ spl15_64 | spl15_109), inference(avatar_split_clause, [], [f2896, f920, f591, f379])).
fof(f2896, plain, (~ (e1 = op(e3, e0)) | (~ spl15_64 | spl15_109)), inference(backward_demodulation, [], [f922, f593])).
fof(f2901, plain, (~ spl15_60 | ~ spl15_64), inference(avatar_split_clause, [], [f2892, f591, f574])).
fof(f2892, plain, (~ (e3 = op(e0, e1)) | ~ spl15_64), inference(backward_demodulation, [], [f157, f593])).
fof(f2900, plain, (~ spl15_48 | ~ spl15_64), inference(avatar_split_clause, [], [f2889, f591, f523])).
fof(f2889, plain, (~ (e3 = op(e1, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f133, f593])).
fof(f2888, plain, (spl15_21 | ~ spl15_23 | ~ spl15_96), inference(avatar_split_clause, [], [f2887, f860, f417, f409])).
fof(f860, plain, (spl15_96 <=> (e0 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_96])])).
fof(f2887, plain, ((e0 = op(e2, e2)) | (~ spl15_23 | ~ spl15_96)), inference(forward_demodulation, [], [f861, f419])).
fof(f861, plain, ((e0 = op(op(e2, e2), e2)) | ~ spl15_96), inference(avatar_component_clause, [], [f860])).
fof(f2886, plain, (~ spl15_23 | ~ spl15_104), inference(avatar_contradiction_clause, [], [f2885])).
fof(f2885, plain, ($false | (~ spl15_23 | ~ spl15_104)), inference(subsumption_resolution, [], [f2884, f186])).
fof(f186, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2884, plain, ((e2 = e3) | (~ spl15_23 | ~ spl15_104)), inference(forward_demodulation, [], [f2883, f419])).
fof(f2883, plain, ((e3 = op(e2, e2)) | (~ spl15_23 | ~ spl15_104)), inference(forward_demodulation, [], [f898, f419])).
fof(f898, plain, ((e3 = op(op(e2, e2), op(e2, e2))) | ~ spl15_104), inference(avatar_component_clause, [], [f897])).
fof(f2882, plain, (spl15_21 | ~ spl15_23 | ~ spl15_106), inference(avatar_split_clause, [], [f2881, f906, f417, f409])).
fof(f906, plain, (spl15_106 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_106])])).
fof(f2881, plain, ((e0 = op(e2, e2)) | (~ spl15_23 | ~ spl15_106)), inference(forward_demodulation, [], [f907, f419])).
fof(f907, plain, ((e0 = op(op(e2, e2), op(e2, e2))) | ~ spl15_106), inference(avatar_component_clause, [], [f906])).
fof(f2869, plain, (~ spl15_43 | ~ spl15_38 | spl15_82), inference(avatar_split_clause, [], [f2829, f739, f481, f502])).
fof(f2829, plain, (~ (e2 = op(e1, e1)) | (~ spl15_38 | spl15_82)), inference(backward_demodulation, [], [f741, f483])).
fof(f2858, plain, (~ spl15_7 | ~ spl15_11), inference(avatar_split_clause, [], [f2855, f366, f349])).
fof(f2855, plain, (~ (e2 = op(e3, e2)) | ~ spl15_11), inference(backward_demodulation, [], [f178, f368])).
fof(f368, plain, ((e2 = op(e3, e1)) | ~ spl15_11), inference(avatar_component_clause, [], [f366])).
fof(f178, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2854, plain, (~ spl15_9 | ~ spl15_14 | spl15_69), inference(avatar_split_clause, [], [f2853, f647, f379, f358])).
fof(f358, plain, (spl15_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f2853, plain, (~ (e0 = op(e3, e1)) | (~ spl15_14 | spl15_69)), inference(backward_demodulation, [], [f649, f381])).
fof(f381, plain, ((e1 = op(e3, e0)) | ~ spl15_14), inference(avatar_component_clause, [], [f379])).
fof(f2821, plain, (~ spl15_41 | spl15_57 | ~ spl15_100), inference(avatar_contradiction_clause, [], [f2820])).
fof(f2820, plain, ($false | (~ spl15_41 | spl15_57 | ~ spl15_100)), inference(subsumption_resolution, [], [f2815, f563])).
fof(f563, plain, (~ (e0 = op(e0, e1)) | spl15_57), inference(avatar_component_clause, [], [f562])).
fof(f2815, plain, ((e0 = op(e0, e1)) | (~ spl15_41 | ~ spl15_100)), inference(backward_demodulation, [], [f879, f496])).
fof(f496, plain, ((e0 = op(e1, e1)) | ~ spl15_41), inference(avatar_component_clause, [], [f494])).
fof(f879, plain, ((e0 = op(op(e1, e1), e1)) | ~ spl15_100), inference(avatar_component_clause, [], [f878])).
fof(f878, plain, (spl15_100 <=> (e0 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_100])])).
fof(f2819, plain, (~ spl15_37 | ~ spl15_41), inference(avatar_split_clause, [], [f2810, f494, f477])).
fof(f2810, plain, (~ (e0 = op(e1, e2)) | ~ spl15_41), inference(backward_demodulation, [], [f166, f496])).
fof(f2818, plain, (~ spl15_9 | ~ spl15_41), inference(avatar_split_clause, [], [f2809, f494, f358])).
fof(f2809, plain, (~ (e0 = op(e3, e1)) | ~ spl15_41), inference(backward_demodulation, [], [f143, f496])).
fof(f2806, plain, (~ spl15_16 | ~ spl15_48), inference(avatar_split_clause, [], [f2802, f523, f387])).
fof(f2802, plain, (~ (e3 = op(e3, e0)) | ~ spl15_48), inference(backward_demodulation, [], [f137, f525])).
fof(f2788, plain, (~ spl15_56 | ~ spl15_60), inference(avatar_split_clause, [], [f2784, f574, f557])).
fof(f2784, plain, (~ (e3 = op(e0, e2)) | ~ spl15_60), inference(backward_demodulation, [], [f160, f576])).
fof(f2772, plain, (~ spl15_49 | ~ spl15_1), inference(avatar_split_clause, [], [f2771, f324, f528])).
fof(f2771, plain, (~ (e0 = op(e0, e3)) | ~ spl15_1), inference(forward_demodulation, [], [f153, f326])).
fof(f326, plain, ((e0 = op(e3, e3)) | ~ spl15_1), inference(avatar_component_clause, [], [f324])).
fof(f2770, plain, (~ spl15_51 | ~ spl15_63), inference(avatar_split_clause, [], [f2769, f587, f536])).
fof(f2769, plain, (~ (e2 = op(e0, e3)) | ~ spl15_63), inference(forward_demodulation, [], [f159, f589])).
fof(f2763, plain, (spl15_16 | ~ spl15_1 | ~ spl15_66), inference(avatar_split_clause, [], [f2762, f632, f324, f387])).
fof(f632, plain, (spl15_66 <=> (e3 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_66])])).
fof(f2762, plain, ((e3 = op(e3, e0)) | (~ spl15_1 | ~ spl15_66)), inference(forward_demodulation, [], [f633, f326])).
fof(f633, plain, ((e3 = op(e3, op(e3, e3))) | ~ spl15_66), inference(avatar_component_clause, [], [f632])).
fof(f2758, plain, (~ spl15_9 | ~ spl15_1), inference(avatar_split_clause, [], [f2757, f324, f358])).
fof(f2757, plain, (~ (e0 = op(e3, e1)) | ~ spl15_1), inference(forward_demodulation, [], [f179, f326])).
fof(f2755, plain, (~ spl15_50 | ~ spl15_1 | spl15_107), inference(avatar_split_clause, [], [f2754, f911, f324, f532])).
fof(f911, plain, (spl15_107 <=> (e1 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_107])])).
fof(f2754, plain, (~ (e1 = op(e0, e3)) | (~ spl15_1 | spl15_107)), inference(forward_demodulation, [], [f913, f326])).
fof(f913, plain, (~ (e1 = op(op(e3, e3), e3)) | spl15_107), inference(avatar_component_clause, [], [f911])).
fof(f2751, plain, (~ spl15_1 | ~ spl15_2), inference(avatar_contradiction_clause, [], [f2750])).
fof(f2750, plain, ($false | (~ spl15_1 | ~ spl15_2)), inference(subsumption_resolution, [], [f2749, f181])).
fof(f181, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f2749, plain, ((e0 = e1) | (~ spl15_1 | ~ spl15_2)), inference(backward_demodulation, [], [f330, f326])).
fof(f330, plain, ((e1 = op(e3, e3)) | ~ spl15_2), inference(avatar_component_clause, [], [f328])).
fof(f2746, plain, (~ spl15_5 | ~ spl15_9), inference(avatar_split_clause, [], [f2741, f358, f341])).
fof(f341, plain, (spl15_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f2741, plain, (~ (e0 = op(e3, e2)) | ~ spl15_9), inference(backward_demodulation, [], [f178, f360])).
fof(f360, plain, ((e0 = op(e3, e1)) | ~ spl15_9), inference(avatar_component_clause, [], [f358])).
fof(f2740, plain, (spl15_1 | ~ spl15_16 | ~ spl15_69), inference(avatar_split_clause, [], [f2738, f647, f387, f324])).
fof(f2738, plain, ((e0 = op(e3, e3)) | (~ spl15_16 | ~ spl15_69)), inference(backward_demodulation, [], [f648, f389])).
fof(f389, plain, ((e3 = op(e3, e0)) | ~ spl15_16), inference(avatar_component_clause, [], [f387])).
fof(f648, plain, ((e0 = op(e3, op(e3, e0))) | ~ spl15_69), inference(avatar_component_clause, [], [f647])).
fof(f2739, plain, (~ spl15_12 | ~ spl15_16), inference(avatar_split_clause, [], [f2736, f387, f370])).
fof(f2736, plain, (~ (e3 = op(e3, e1)) | ~ spl15_16), inference(backward_demodulation, [], [f175, f389])).
fof(f175, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2734, plain, (~ spl15_5 | ~ spl15_37), inference(avatar_split_clause, [], [f2729, f477, f341])).
fof(f2729, plain, (~ (e0 = op(e3, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f149, f479])).
fof(f149, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2719, plain, (~ spl15_40 | ~ spl15_56), inference(avatar_split_clause, [], [f2717, f557, f489])).
fof(f2717, plain, (~ (e3 = op(e1, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f145, f559])).
fof(f2711, plain, (~ spl15_15 | ~ spl15_63), inference(avatar_split_clause, [], [f2705, f587, f383])).
fof(f2705, plain, (~ (e2 = op(e3, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f135, f589])).
fof(f2697, plain, (~ spl15_41 | ~ spl15_2 | spl15_108), inference(avatar_split_clause, [], [f2696, f915, f328, f494])).
fof(f915, plain, (spl15_108 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_108])])).
fof(f2696, plain, (~ (e0 = op(e1, e1)) | (~ spl15_2 | spl15_108)), inference(forward_demodulation, [], [f917, f330])).
fof(f917, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl15_108), inference(avatar_component_clause, [], [f915])).
fof(f2676, plain, (spl15_12 | ~ spl15_2 | ~ spl15_66), inference(avatar_split_clause, [], [f2668, f632, f328, f370])).
fof(f2668, plain, ((e3 = op(e3, e1)) | (~ spl15_2 | ~ spl15_66)), inference(backward_demodulation, [], [f633, f330])).
fof(f2673, plain, (~ spl15_2 | ~ spl15_35 | spl15_114), inference(avatar_contradiction_clause, [], [f2672])).
fof(f2672, plain, ($false | (~ spl15_2 | ~ spl15_35 | spl15_114)), inference(subsumption_resolution, [], [f2671, f470])).
fof(f2671, plain, (~ (e2 = op(e1, e3)) | (~ spl15_2 | spl15_114)), inference(backward_demodulation, [], [f947, f330])).
fof(f947, plain, (~ (e2 = op(op(e3, e3), e3)) | spl15_114), inference(avatar_component_clause, [], [f945])).
fof(f945, plain, (spl15_114 <=> (e2 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_114])])).
fof(f2660, plain, (spl15_2 | ~ spl15_12 | ~ spl15_68), inference(avatar_split_clause, [], [f2658, f642, f370, f328])).
fof(f642, plain, (spl15_68 <=> (e1 = op(e3, op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_68])])).
fof(f2658, plain, ((e1 = op(e3, e3)) | (~ spl15_12 | ~ spl15_68)), inference(backward_demodulation, [], [f643, f372])).
fof(f372, plain, ((e3 = op(e3, e1)) | ~ spl15_12), inference(avatar_component_clause, [], [f370])).
fof(f643, plain, ((e1 = op(e3, op(e3, e1))) | ~ spl15_68), inference(avatar_component_clause, [], [f642])).
fof(f2659, plain, (~ spl15_8 | ~ spl15_12), inference(avatar_split_clause, [], [f2656, f370, f353])).
fof(f2656, plain, (~ (e3 = op(e3, e2)) | ~ spl15_12), inference(backward_demodulation, [], [f178, f372])).
fof(f2654, plain, (~ spl15_3 | ~ spl15_15), inference(avatar_split_clause, [], [f2651, f383, f332])).
fof(f2651, plain, (~ (e2 = op(e3, e3)) | ~ spl15_15), inference(backward_demodulation, [], [f177, f385])).
fof(f2638, plain, (~ spl15_23 | ~ spl15_27), inference(avatar_split_clause, [], [f2636, f434, f417])).
fof(f2636, plain, (~ (e2 = op(e2, e2)) | ~ spl15_27), inference(backward_demodulation, [], [f172, f436])).
fof(f436, plain, ((e2 = op(e2, e1)) | ~ spl15_27), inference(avatar_component_clause, [], [f434])).
fof(f2632, plain, (~ spl15_3 | ~ spl15_35), inference(avatar_split_clause, [], [f2629, f468, f332])).
fof(f2629, plain, (~ (e2 = op(e3, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f155, f470])).
fof(f2628, plain, (~ spl15_35 | ~ spl15_40 | spl15_82), inference(avatar_split_clause, [], [f2626, f739, f489, f468])).
fof(f2626, plain, (~ (e2 = op(e1, e3)) | (~ spl15_40 | spl15_82)), inference(backward_demodulation, [], [f741, f491])).
fof(f491, plain, ((e3 = op(e1, e2)) | ~ spl15_40), inference(avatar_component_clause, [], [f489])).
fof(f2627, plain, (~ spl15_8 | ~ spl15_40), inference(avatar_split_clause, [], [f2624, f489, f353])).
fof(f2624, plain, (~ (e3 = op(e3, e2)) | ~ spl15_40), inference(backward_demodulation, [], [f149, f491])).
fof(f2618, plain, (~ spl15_41 | ~ spl15_64 | spl15_103), inference(avatar_contradiction_clause, [], [f2617])).
fof(f2617, plain, ($false | (~ spl15_41 | ~ spl15_64 | spl15_103)), inference(subsumption_resolution, [], [f2610, f593])).
fof(f2610, plain, (~ (op(e0, e0) = e3) | (~ spl15_41 | spl15_103)), inference(backward_demodulation, [], [f894, f496])).
fof(f2614, plain, (~ spl15_33 | ~ spl15_41), inference(avatar_split_clause, [], [f2604, f494, f460])).
fof(f2604, plain, (~ (e0 = op(e1, e3)) | ~ spl15_41), inference(backward_demodulation, [], [f167, f496])).
fof(f2600, plain, (~ spl15_41 | ~ spl15_46 | spl15_84), inference(avatar_split_clause, [], [f2597, f749, f515, f494])).
fof(f515, plain, (spl15_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_46])])).
fof(f2597, plain, (~ (e0 = op(e1, e1)) | (~ spl15_46 | spl15_84)), inference(backward_demodulation, [], [f751, f517])).
fof(f517, plain, ((e1 = op(e1, e0)) | ~ spl15_46), inference(avatar_component_clause, [], [f515])).
fof(f2599, plain, (~ spl15_38 | ~ spl15_46), inference(avatar_split_clause, [], [f2593, f515, f481])).
fof(f2593, plain, (~ (e1 = op(e1, e2)) | ~ spl15_46), inference(backward_demodulation, [], [f164, f517])).
fof(f2598, plain, (~ spl15_14 | ~ spl15_46), inference(avatar_split_clause, [], [f2591, f515, f379])).
fof(f2591, plain, (~ (e1 = op(e3, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f137, f517])).
fof(f2584, plain, (~ spl15_23 | ~ spl15_55), inference(avatar_split_clause, [], [f2579, f553, f417])).
fof(f2579, plain, (~ (e2 = op(e2, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f146, f555])).
fof(f555, plain, ((e2 = op(e0, e2)) | ~ spl15_55), inference(avatar_component_clause, [], [f553])).
fof(f2577, plain, (~ spl15_50 | ~ spl15_58), inference(avatar_split_clause, [], [f2572, f566, f532])).
fof(f566, plain, (spl15_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_58])])).
fof(f2572, plain, (~ (e1 = op(e0, e3)) | ~ spl15_58), inference(backward_demodulation, [], [f161, f568])).
fof(f568, plain, ((e1 = op(e0, e1)) | ~ spl15_58), inference(avatar_component_clause, [], [f566])).
fof(f2576, plain, (~ spl15_26 | ~ spl15_58), inference(avatar_split_clause, [], [f2569, f566, f430])).
fof(f2569, plain, (~ (e1 = op(e2, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f140, f568])).
fof(f2558, plain, (~ spl15_4 | ~ spl15_20), inference(avatar_split_clause, [], [f2557, f404, f336])).
fof(f2557, plain, (~ (e3 = op(e3, e3)) | ~ spl15_20), inference(forward_demodulation, [], [f156, f406])).
fof(f406, plain, ((e3 = op(e2, e3)) | ~ spl15_20), inference(avatar_component_clause, [], [f404])).
fof(f2554, plain, (~ spl15_6 | ~ spl15_14), inference(avatar_split_clause, [], [f2550, f379, f345])).
fof(f345, plain, (spl15_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_6])])).
fof(f2550, plain, (~ (e1 = op(e3, e2)) | ~ spl15_14), inference(backward_demodulation, [], [f176, f381])).
fof(f2539, plain, (~ spl15_18 | ~ spl15_26), inference(avatar_split_clause, [], [f2535, f430, f396])).
fof(f396, plain, (spl15_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f2535, plain, (~ (e1 = op(e2, e3)) | ~ spl15_26), inference(backward_demodulation, [], [f173, f432])).
fof(f173, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2533, plain, (~ spl15_21 | ~ spl15_29), inference(avatar_split_clause, [], [f2527, f443, f409])).
fof(f443, plain, (spl15_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_29])])).
fof(f2527, plain, (~ (e0 = op(e2, e2)) | ~ spl15_29), inference(backward_demodulation, [], [f170, f445])).
fof(f445, plain, ((e0 = op(e2, e0)) | ~ spl15_29), inference(avatar_component_clause, [], [f443])).
fof(f2524, plain, (~ spl15_6 | ~ spl15_38), inference(avatar_split_clause, [], [f2520, f481, f345])).
fof(f2520, plain, (~ (e1 = op(e3, e2)) | ~ spl15_38), inference(backward_demodulation, [], [f149, f483])).
fof(f2514, plain, (~ spl15_9 | ~ spl15_44 | spl15_100), inference(avatar_contradiction_clause, [], [f2513])).
fof(f2513, plain, ($false | (~ spl15_9 | ~ spl15_44 | spl15_100)), inference(subsumption_resolution, [], [f2506, f360])).
fof(f2506, plain, (~ (e0 = op(e3, e1)) | (~ spl15_44 | spl15_100)), inference(backward_demodulation, [], [f880, f508])).
fof(f880, plain, (~ (e0 = op(op(e1, e1), e1)) | spl15_100), inference(avatar_component_clause, [], [f878])).
fof(f2501, plain, (~ spl15_31 | ~ spl15_47), inference(avatar_split_clause, [], [f2495, f519, f451])).
fof(f2495, plain, (~ (e2 = op(e2, e0)) | ~ spl15_47), inference(backward_demodulation, [], [f136, f521])).
fof(f136, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2494, plain, (~ spl15_18 | ~ spl15_50), inference(avatar_split_clause, [], [f2491, f532, f396])).
fof(f2491, plain, (~ (e1 = op(e2, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f152, f534])).
fof(f534, plain, ((e1 = op(e0, e3)) | ~ spl15_50), inference(avatar_component_clause, [], [f532])).
fof(f152, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2490, plain, (~ spl15_53 | spl15_63 | ~ spl15_90), inference(avatar_contradiction_clause, [], [f2489])).
fof(f2489, plain, ($false | (~ spl15_53 | spl15_63 | ~ spl15_90)), inference(subsumption_resolution, [], [f2487, f588])).
fof(f588, plain, (~ (op(e0, e0) = e2) | spl15_63), inference(avatar_component_clause, [], [f587])).
fof(f2487, plain, ((op(e0, e0) = e2) | (~ spl15_53 | ~ spl15_90)), inference(backward_demodulation, [], [f796, f547])).
fof(f796, plain, ((e2 = op(e0, op(e0, e2))) | ~ spl15_90), inference(avatar_component_clause, [], [f795])).
fof(f795, plain, (spl15_90 <=> (e2 = op(e0, op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_90])])).
fof(f2488, plain, (~ spl15_21 | ~ spl15_53), inference(avatar_split_clause, [], [f2481, f545, f409])).
fof(f2481, plain, (~ (e0 = op(e2, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f146, f547])).
fof(f2479, plain, (spl15_54 | ~ spl15_59 | ~ spl15_91), inference(avatar_contradiction_clause, [], [f2478])).
fof(f2478, plain, ($false | (spl15_54 | ~ spl15_59 | ~ spl15_91)), inference(subsumption_resolution, [], [f2475, f550])).
fof(f550, plain, (~ (e1 = op(e0, e2)) | spl15_54), inference(avatar_component_clause, [], [f549])).
fof(f2475, plain, ((e1 = op(e0, e2)) | (~ spl15_59 | ~ spl15_91)), inference(backward_demodulation, [], [f801, f572])).
fof(f801, plain, ((e1 = op(e0, op(e0, e1))) | ~ spl15_91), inference(avatar_component_clause, [], [f800])).
fof(f800, plain, (spl15_91 <=> (e1 = op(e0, op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_91])])).
fof(f2477, plain, (~ spl15_55 | ~ spl15_59), inference(avatar_split_clause, [], [f2472, f570, f553])).
fof(f2472, plain, (~ (e2 = op(e0, e2)) | ~ spl15_59), inference(backward_demodulation, [], [f160, f572])).
fof(f2465, plain, (spl15_49 | ~ spl15_64 | ~ spl15_92), inference(avatar_contradiction_clause, [], [f2464])).
fof(f2464, plain, ($false | (spl15_49 | ~ spl15_64 | ~ spl15_92)), inference(subsumption_resolution, [], [f2456, f529])).
fof(f529, plain, (~ (e0 = op(e0, e3)) | spl15_49), inference(avatar_component_clause, [], [f528])).
fof(f2456, plain, ((e0 = op(e0, e3)) | (~ spl15_64 | ~ spl15_92)), inference(backward_demodulation, [], [f806, f593])).
fof(f806, plain, ((e0 = op(e0, op(e0, e0))) | ~ spl15_92), inference(avatar_component_clause, [], [f805])).
fof(f2463, plain, (~ spl15_52 | ~ spl15_64), inference(avatar_split_clause, [], [f2455, f591, f540])).
fof(f2455, plain, (~ (e3 = op(e0, e3)) | ~ spl15_64), inference(backward_demodulation, [], [f159, f593])).
fof(f2462, plain, (~ spl15_16 | ~ spl15_64), inference(avatar_split_clause, [], [f2452, f591, f387])).
fof(f2452, plain, (~ (e3 = op(e3, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f135, f593])).
fof(f2449, plain, (spl15_8 | ~ spl15_3 | ~ spl15_66), inference(avatar_split_clause, [], [f2448, f632, f332, f353])).
fof(f2448, plain, ((e3 = op(e3, e2)) | (~ spl15_3 | ~ spl15_66)), inference(forward_demodulation, [], [f633, f334])).
fof(f334, plain, ((e2 = op(e3, e3)) | ~ spl15_3), inference(avatar_component_clause, [], [f332])).
fof(f2447, plain, (spl15_14 | ~ spl15_9 | ~ spl15_68), inference(avatar_split_clause, [], [f2446, f642, f358, f379])).
fof(f2446, plain, ((e1 = op(e3, e0)) | (~ spl15_9 | ~ spl15_68)), inference(forward_demodulation, [], [f643, f360])).
fof(f2436, plain, (~ spl15_48 | ~ spl15_33 | spl15_81), inference(avatar_split_clause, [], [f2350, f734, f460, f523])).
fof(f2350, plain, (~ (e3 = op(e1, e0)) | (~ spl15_33 | spl15_81)), inference(forward_demodulation, [], [f736, f462])).
fof(f2418, plain, (~ spl15_5 | ~ spl15_21), inference(avatar_split_clause, [], [f2410, f409, f341])).
fof(f2410, plain, (~ (e0 = op(e3, e2)) | ~ spl15_21), inference(backward_demodulation, [], [f150, f411])).
fof(f2408, plain, (~ spl15_20 | ~ spl15_28), inference(avatar_split_clause, [], [f2406, f438, f404])).
fof(f2406, plain, (~ (e3 = op(e2, e3)) | ~ spl15_28), inference(backward_demodulation, [], [f173, f440])).
fof(f440, plain, ((e3 = op(e2, e1)) | ~ spl15_28), inference(avatar_component_clause, [], [f438])).
fof(f2397, plain, (~ spl15_21 | ~ spl15_43 | spl15_113), inference(avatar_split_clause, [], [f2394, f940, f502, f409])).
fof(f2394, plain, (~ (e0 = op(e2, e2)) | (~ spl15_43 | spl15_113)), inference(backward_demodulation, [], [f942, f504])).
fof(f2396, plain, (~ spl15_39 | ~ spl15_43), inference(avatar_split_clause, [], [f2388, f502, f485])).
fof(f2388, plain, (~ (e2 = op(e1, e2)) | ~ spl15_43), inference(backward_demodulation, [], [f166, f504])).
fof(f2385, plain, (~ spl15_52 | spl15_89), inference(avatar_contradiction_clause, [], [f2384])).
fof(f2384, plain, ($false | (~ spl15_52 | spl15_89)), inference(subsumption_resolution, [], [f2382, f542])).
fof(f2382, plain, (~ (e3 = op(e0, e3)) | (~ spl15_52 | spl15_89)), inference(backward_demodulation, [], [f792, f542])).
fof(f2383, plain, (~ spl15_20 | ~ spl15_52), inference(avatar_split_clause, [], [f2381, f540, f404])).
fof(f2381, plain, (~ (e3 = op(e2, e3)) | ~ spl15_52), inference(backward_demodulation, [], [f152, f542])).
fof(f2380, plain, (~ spl15_39 | ~ spl15_55), inference(avatar_split_clause, [], [f2374, f553, f485])).
fof(f2374, plain, (~ (e2 = op(e1, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f145, f555])).
fof(f2372, plain, (~ spl15_10 | ~ spl15_58), inference(avatar_split_clause, [], [f2366, f566, f362])).
fof(f2366, plain, (~ (e1 = op(e3, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f141, f568])).
fof(f2348, plain, (~ spl15_21 | ~ spl15_31 | spl15_76), inference(avatar_split_clause, [], [f2302, f693, f451, f409])).
fof(f2302, plain, (~ (e0 = op(e2, e2)) | (~ spl15_31 | spl15_76)), inference(backward_demodulation, [], [f695, f453])).
fof(f453, plain, ((e2 = op(e2, e0)) | ~ spl15_31), inference(avatar_component_clause, [], [f451])).
fof(f2332, plain, (~ spl15_1 | ~ spl15_5), inference(avatar_split_clause, [], [f2328, f341, f324])).
fof(f2328, plain, (~ (e0 = op(e3, e3)) | ~ spl15_5), inference(backward_demodulation, [], [f180, f343])).
fof(f343, plain, ((e0 = op(e3, e2)) | ~ spl15_5), inference(avatar_component_clause, [], [f341])).
fof(f2327, plain, (~ spl15_10 | spl15_68), inference(avatar_contradiction_clause, [], [f2326])).
fof(f2326, plain, ($false | (~ spl15_10 | spl15_68)), inference(subsumption_resolution, [], [f2325, f364])).
fof(f2325, plain, (~ (e1 = op(e3, e1)) | (~ spl15_10 | spl15_68)), inference(backward_demodulation, [], [f644, f364])).
fof(f644, plain, (~ (e1 = op(e3, op(e3, e1))) | spl15_68), inference(avatar_component_clause, [], [f642])).
fof(f2320, plain, (~ spl15_1 | ~ spl15_16 | spl15_69), inference(avatar_split_clause, [], [f2318, f647, f387, f324])).
fof(f2318, plain, (~ (e0 = op(e3, e3)) | (~ spl15_16 | spl15_69)), inference(backward_demodulation, [], [f649, f389])).
fof(f2319, plain, (~ spl15_8 | ~ spl15_16), inference(avatar_split_clause, [], [f2316, f387, f353])).
fof(f2316, plain, (~ (e3 = op(e3, e2)) | ~ spl15_16), inference(backward_demodulation, [], [f176, f389])).
fof(f2303, plain, (~ spl15_23 | ~ spl15_31), inference(avatar_split_clause, [], [f2300, f451, f417])).
fof(f2300, plain, (~ (e2 = op(e2, e2)) | ~ spl15_31), inference(backward_demodulation, [], [f170, f453])).
fof(f2297, plain, (~ spl15_33 | ~ spl15_34), inference(avatar_contradiction_clause, [], [f2296])).
fof(f2296, plain, ($false | (~ spl15_33 | ~ spl15_34)), inference(subsumption_resolution, [], [f2295, f181])).
fof(f2295, plain, ((e0 = e1) | (~ spl15_33 | ~ spl15_34)), inference(backward_demodulation, [], [f466, f462])).
fof(f2294, plain, (~ spl15_39 | spl15_82), inference(avatar_contradiction_clause, [], [f2293])).
fof(f2293, plain, ($false | (~ spl15_39 | spl15_82)), inference(subsumption_resolution, [], [f2291, f487])).
fof(f487, plain, ((e2 = op(e1, e2)) | ~ spl15_39), inference(avatar_component_clause, [], [f485])).
fof(f2291, plain, (~ (e2 = op(e1, e2)) | (~ spl15_39 | spl15_82)), inference(backward_demodulation, [], [f741, f487])).
fof(f2292, plain, (~ spl15_23 | ~ spl15_39), inference(avatar_split_clause, [], [f2288, f485, f417])).
fof(f2288, plain, (~ (e2 = op(e2, e2)) | ~ spl15_39), inference(backward_demodulation, [], [f148, f487])).
fof(f2272, plain, (~ spl15_42 | ~ spl15_46), inference(avatar_split_clause, [], [f2267, f515, f498])).
fof(f498, plain, (spl15_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_42])])).
fof(f2267, plain, (~ (e1 = op(e1, e1)) | ~ spl15_46), inference(backward_demodulation, [], [f163, f517])).
fof(f2259, plain, (~ spl15_51 | ~ spl15_56 | spl15_90), inference(avatar_split_clause, [], [f2256, f795, f557, f536])).
fof(f2256, plain, (~ (e2 = op(e0, e3)) | (~ spl15_56 | spl15_90)), inference(backward_demodulation, [], [f797, f559])).
fof(f797, plain, (~ (e2 = op(e0, op(e0, e2))) | spl15_90), inference(avatar_component_clause, [], [f795])).
fof(f2258, plain, (~ spl15_8 | ~ spl15_56), inference(avatar_split_clause, [], [f2254, f557, f353])).
fof(f2254, plain, (~ (e3 = op(e3, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f147, f559])).
fof(f2237, plain, (~ spl15_57 | ~ spl15_61), inference(avatar_split_clause, [], [f2223, f579, f562])).
fof(f579, plain, (spl15_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_61])])).
fof(f2223, plain, (~ (e0 = op(e0, e1)) | ~ spl15_61), inference(backward_demodulation, [], [f157, f581])).
fof(f581, plain, ((e0 = op(e0, e0)) | ~ spl15_61), inference(avatar_component_clause, [], [f579])).
fof(f2236, plain, (~ spl15_29 | ~ spl15_61), inference(avatar_split_clause, [], [f2221, f579, f443])).
fof(f2221, plain, (~ (e0 = op(e2, e0)) | ~ spl15_61), inference(backward_demodulation, [], [f134, f581])).
fof(f2211, plain, (~ spl15_46 | ~ spl15_34), inference(avatar_split_clause, [], [f2102, f464, f515])).
fof(f2102, plain, (~ (e1 = op(e1, e0)) | ~ spl15_34), inference(forward_demodulation, [], [f165, f466])).
fof(f2188, plain, (~ spl15_18 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f2187])).
fof(f2187, plain, ($false | (~ spl15_18 | ~ spl15_20)), inference(subsumption_resolution, [], [f2186, f185])).
fof(f185, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2186, plain, ((e1 = e3) | (~ spl15_18 | ~ spl15_20)), inference(backward_demodulation, [], [f406, f398])).
fof(f398, plain, ((e1 = op(e2, e3)) | ~ spl15_18), inference(avatar_component_clause, [], [f396])).
fof(f2184, plain, (~ spl15_20 | spl15_73), inference(avatar_contradiction_clause, [], [f2183])).
fof(f2183, plain, ($false | (~ spl15_20 | spl15_73)), inference(subsumption_resolution, [], [f2181, f406])).
fof(f2181, plain, (~ (e3 = op(e2, e3)) | (~ spl15_20 | spl15_73)), inference(backward_demodulation, [], [f680, f406])).
fof(f2172, plain, (~ spl15_29 | spl15_76), inference(avatar_contradiction_clause, [], [f2171])).
fof(f2171, plain, ($false | (~ spl15_29 | spl15_76)), inference(subsumption_resolution, [], [f2169, f445])).
fof(f2169, plain, (~ (e0 = op(e2, e0)) | (~ spl15_29 | spl15_76)), inference(backward_demodulation, [], [f695, f445])).
fof(f2170, plain, (~ spl15_17 | ~ spl15_29), inference(avatar_split_clause, [], [f2165, f443, f392])).
fof(f2165, plain, (~ (e0 = op(e2, e3)) | ~ spl15_29), inference(backward_demodulation, [], [f171, f445])).
fof(f2163, plain, (~ spl15_37 | ~ spl15_40), inference(avatar_contradiction_clause, [], [f2162])).
fof(f2162, plain, ($false | (~ spl15_37 | ~ spl15_40)), inference(subsumption_resolution, [], [f2161, f183])).
fof(f183, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f2161, plain, ((e0 = e3) | (~ spl15_37 | ~ spl15_40)), inference(forward_demodulation, [], [f491, f479])).
fof(f2142, plain, (~ spl15_22 | ~ spl15_54), inference(avatar_split_clause, [], [f2137, f549, f413])).
fof(f2137, plain, (~ (e1 = op(e2, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f146, f551])).
fof(f2136, plain, (~ spl15_9 | ~ spl15_57), inference(avatar_split_clause, [], [f2132, f562, f358])).
fof(f2132, plain, (~ (e0 = op(e3, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f141, f564])).
fof(f2128, plain, (~ spl15_56 | ~ spl15_64), inference(avatar_split_clause, [], [f2121, f591, f557])).
fof(f2121, plain, (~ (e3 = op(e0, e2)) | ~ spl15_64), inference(backward_demodulation, [], [f158, f593])).
fof(f2107, plain, (~ spl15_60 | ~ spl15_28), inference(avatar_split_clause, [], [f2106, f438, f574])).
fof(f2106, plain, (~ (e3 = op(e0, e1)) | ~ spl15_28), inference(forward_demodulation, [], [f140, f440])).
fof(f2100, plain, (~ spl15_42 | ~ spl15_34), inference(avatar_split_clause, [], [f2099, f464, f498])).
fof(f2099, plain, (~ (e1 = op(e1, e1)) | ~ spl15_34), inference(forward_demodulation, [], [f167, f466])).
fof(f2091, plain, (~ spl15_18 | ~ spl15_34), inference(avatar_split_clause, [], [f2090, f464, f396])).
fof(f2090, plain, (~ (e1 = op(e2, e3)) | ~ spl15_34), inference(forward_demodulation, [], [f154, f466])).
fof(f2089, plain, (~ spl15_19 | ~ spl15_51), inference(avatar_split_clause, [], [f2088, f536, f400])).
fof(f2088, plain, (~ (e2 = op(e2, e3)) | ~ spl15_51), inference(forward_demodulation, [], [f152, f538])).
fof(f2087, plain, (~ spl15_18 | ~ spl15_28 | spl15_75), inference(avatar_split_clause, [], [f2058, f688, f438, f396])).
fof(f2058, plain, (~ (e1 = op(e2, e3)) | (~ spl15_28 | spl15_75)), inference(backward_demodulation, [], [f690, f440])).
fof(f690, plain, (~ (e1 = op(e2, op(e2, e1))) | spl15_75), inference(avatar_component_clause, [], [f688])).
fof(f2069, plain, (~ spl15_17 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f2068])).
fof(f2068, plain, ($false | (~ spl15_17 | ~ spl15_19)), inference(subsumption_resolution, [], [f2067, f182])).
fof(f2067, plain, ((e0 = e2) | (~ spl15_17 | ~ spl15_19)), inference(forward_demodulation, [], [f402, f394])).
fof(f394, plain, ((e0 = op(e2, e3)) | ~ spl15_17), inference(avatar_component_clause, [], [f392])).
fof(f2035, plain, (~ spl15_32 | ~ spl15_48), inference(avatar_split_clause, [], [f2032, f523, f455])).
fof(f2032, plain, (~ (e3 = op(e2, e0)) | ~ spl15_48), inference(backward_demodulation, [], [f136, f525])).
fof(f2034, plain, (~ spl15_36 | ~ spl15_48), inference(avatar_split_clause, [], [f2031, f523, f472])).
fof(f2031, plain, (~ (e3 = op(e1, e3)) | ~ spl15_48), inference(backward_demodulation, [], [f165, f525])).
fof(f2027, plain, (~ spl15_3 | ~ spl15_51), inference(avatar_split_clause, [], [f2024, f536, f332])).
fof(f2024, plain, (~ (e2 = op(e3, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f153, f538])).
fof(f2020, plain, (~ spl15_58 | spl15_91), inference(avatar_contradiction_clause, [], [f2019])).
fof(f2019, plain, ($false | (~ spl15_58 | spl15_91)), inference(subsumption_resolution, [], [f2015, f568])).
fof(f2015, plain, (~ (e1 = op(e0, e1)) | (~ spl15_58 | spl15_91)), inference(backward_demodulation, [], [f802, f568])).
fof(f802, plain, (~ (e1 = op(e0, op(e0, e1))) | spl15_91), inference(avatar_component_clause, [], [f800])).
fof(f1995, plain, (~ spl15_28 | ~ spl15_43 | spl15_117), inference(avatar_split_clause, [], [f1929, f962, f502, f438])).
fof(f962, plain, (spl15_117 <=> (e3 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_117])])).
fof(f1929, plain, (~ (e3 = op(e2, e1)) | (~ spl15_43 | spl15_117)), inference(forward_demodulation, [], [f964, f504])).
fof(f964, plain, (~ (e3 = op(op(e1, e1), e1)) | spl15_117), inference(avatar_component_clause, [], [f962])).
fof(f1993, plain, (~ spl15_5 | ~ spl15_14 | ~ spl15_67), inference(avatar_contradiction_clause, [], [f1992])).
fof(f1992, plain, ($false | (~ spl15_5 | ~ spl15_14 | ~ spl15_67)), inference(subsumption_resolution, [], [f1991, f184])).
fof(f184, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1991, plain, ((e1 = e2) | (~ spl15_5 | ~ spl15_14 | ~ spl15_67)), inference(forward_demodulation, [], [f1988, f381])).
fof(f1988, plain, ((e2 = op(e3, e0)) | (~ spl15_5 | ~ spl15_67)), inference(backward_demodulation, [], [f638, f343])).
fof(f638, plain, ((e2 = op(e3, op(e3, e2))) | ~ spl15_67), inference(avatar_component_clause, [], [f637])).
fof(f637, plain, (spl15_67 <=> (e2 = op(e3, op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_67])])).
fof(f1983, plain, (~ spl15_14 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f1982])).
fof(f1982, plain, ($false | (~ spl15_14 | ~ spl15_16)), inference(subsumption_resolution, [], [f1981, f185])).
fof(f1981, plain, ((e1 = e3) | (~ spl15_14 | ~ spl15_16)), inference(forward_demodulation, [], [f389, f381])).
fof(f1979, plain, (~ spl15_23 | spl15_74), inference(avatar_contradiction_clause, [], [f1978])).
fof(f1978, plain, ($false | (~ spl15_23 | spl15_74)), inference(subsumption_resolution, [], [f1975, f419])).
fof(f1975, plain, (~ (e2 = op(e2, e2)) | (~ spl15_23 | spl15_74)), inference(backward_demodulation, [], [f685, f419])).
fof(f1972, plain, (~ spl15_17 | ~ spl15_32 | spl15_76), inference(avatar_contradiction_clause, [], [f1971])).
fof(f1971, plain, ($false | (~ spl15_17 | ~ spl15_32 | spl15_76)), inference(subsumption_resolution, [], [f1969, f394])).
fof(f1969, plain, (~ (e0 = op(e2, e3)) | (~ spl15_32 | spl15_76)), inference(backward_demodulation, [], [f695, f457])).
fof(f1966, plain, (~ spl15_41 | ~ spl15_43), inference(avatar_contradiction_clause, [], [f1965])).
fof(f1965, plain, ($false | (~ spl15_41 | ~ spl15_43)), inference(subsumption_resolution, [], [f1964, f182])).
fof(f1964, plain, ((e0 = e2) | (~ spl15_41 | ~ spl15_43)), inference(backward_demodulation, [], [f504, f496])).
fof(f1954, plain, (~ spl15_55 | ~ spl15_56), inference(avatar_contradiction_clause, [], [f1953])).
fof(f1953, plain, ($false | (~ spl15_55 | ~ spl15_56)), inference(subsumption_resolution, [], [f1952, f186])).
fof(f1952, plain, ((e2 = e3) | (~ spl15_55 | ~ spl15_56)), inference(forward_demodulation, [], [f559, f555])).
fof(f1945, plain, (~ spl15_55 | spl15_90), inference(avatar_contradiction_clause, [], [f1944])).
fof(f1944, plain, ($false | (~ spl15_55 | spl15_90)), inference(subsumption_resolution, [], [f1943, f555])).
fof(f1943, plain, (~ (e2 = op(e0, e2)) | (~ spl15_55 | spl15_90)), inference(forward_demodulation, [], [f797, f555])).
fof(f1942, plain, (~ spl15_50 | ~ spl15_60 | spl15_91), inference(avatar_contradiction_clause, [], [f1941])).
fof(f1941, plain, ($false | (~ spl15_50 | ~ spl15_60 | spl15_91)), inference(subsumption_resolution, [], [f1940, f534])).
fof(f1940, plain, (~ (e1 = op(e0, e3)) | (~ spl15_60 | spl15_91)), inference(forward_demodulation, [], [f802, f576])).
fof(f1939, plain, (~ spl15_61 | spl15_92), inference(avatar_contradiction_clause, [], [f1938])).
fof(f1938, plain, ($false | (~ spl15_61 | spl15_92)), inference(subsumption_resolution, [], [f1937, f581])).
fof(f1937, plain, (~ (e0 = op(e0, e0)) | (~ spl15_61 | spl15_92)), inference(forward_demodulation, [], [f807, f581])).
fof(f1898, plain, (~ spl15_11 | ~ spl15_43), inference(avatar_split_clause, [], [f1897, f502, f366])).
fof(f1897, plain, (~ (e2 = op(e3, e1)) | ~ spl15_43), inference(forward_demodulation, [], [f143, f504])).
fof(f1894, plain, (~ spl15_10 | ~ spl15_14), inference(avatar_split_clause, [], [f1893, f379, f362])).
fof(f1893, plain, (~ (e1 = op(e3, e1)) | ~ spl15_14), inference(forward_demodulation, [], [f175, f381])).
fof(f1892, plain, (~ spl15_11 | ~ spl15_3), inference(avatar_split_clause, [], [f1798, f332, f366])).
fof(f1798, plain, (~ (e2 = op(e3, e1)) | ~ spl15_3), inference(forward_demodulation, [], [f179, f334])).
fof(f1891, plain, (spl15_9 | ~ spl15_14 | ~ spl15_69), inference(avatar_split_clause, [], [f1855, f647, f379, f358])).
fof(f1855, plain, ((e0 = op(e3, e1)) | (~ spl15_14 | ~ spl15_69)), inference(forward_demodulation, [], [f648, f381])).
fof(f1885, plain, (~ spl15_37 | ~ spl15_38), inference(avatar_contradiction_clause, [], [f1884])).
fof(f1884, plain, ($false | (~ spl15_37 | ~ spl15_38)), inference(subsumption_resolution, [], [f1883, f181])).
fof(f1883, plain, ((e0 = e1) | (~ spl15_37 | ~ spl15_38)), inference(forward_demodulation, [], [f483, f479])).
fof(f1880, plain, (~ spl15_35 | ~ spl15_43), inference(avatar_split_clause, [], [f1873, f502, f468])).
fof(f1873, plain, (~ (e2 = op(e1, e3)) | ~ spl15_43), inference(backward_demodulation, [], [f167, f504])).
fof(f1868, plain, (~ spl15_50 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f1867])).
fof(f1867, plain, ($false | (~ spl15_50 | ~ spl15_52)), inference(subsumption_resolution, [], [f1866, f185])).
fof(f1866, plain, ((e1 = e3) | (~ spl15_50 | ~ spl15_52)), inference(forward_demodulation, [], [f542, f534])).
fof(f1862, plain, (~ spl15_59 | ~ spl15_60), inference(avatar_contradiction_clause, [], [f1861])).
fof(f1861, plain, ($false | (~ spl15_59 | ~ spl15_60)), inference(subsumption_resolution, [], [f1860, f186])).
fof(f1860, plain, ((e2 = e3) | (~ spl15_59 | ~ spl15_60)), inference(forward_demodulation, [], [f576, f572])).
fof(f1850, plain, (~ spl15_26 | spl15_75), inference(avatar_contradiction_clause, [], [f1849])).
fof(f1849, plain, ($false | (~ spl15_26 | spl15_75)), inference(subsumption_resolution, [], [f1848, f432])).
fof(f1848, plain, (~ (e1 = op(e2, e1)) | (~ spl15_26 | spl15_75)), inference(forward_demodulation, [], [f690, f432])).
fof(f1812, plain, (~ spl15_42 | ~ spl15_26), inference(avatar_split_clause, [], [f1811, f430, f498])).
fof(f1811, plain, (~ (e1 = op(e1, e1)) | ~ spl15_26), inference(forward_demodulation, [], [f142, f432])).
fof(f1789, plain, (~ spl15_9 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f1788])).
fof(f1788, plain, ($false | (~ spl15_9 | ~ spl15_12)), inference(subsumption_resolution, [], [f1786, f183])).
fof(f1786, plain, ((e0 = e3) | (~ spl15_9 | ~ spl15_12)), inference(backward_demodulation, [], [f372, f360])).
fof(f1783, plain, (~ spl15_5 | ~ spl15_24 | spl15_96), inference(avatar_split_clause, [], [f1780, f860, f421, f341])).
fof(f1780, plain, (~ (e0 = op(e3, e2)) | (~ spl15_24 | spl15_96)), inference(backward_demodulation, [], [f862, f423])).
fof(f862, plain, (~ (e0 = op(op(e2, e2), e2)) | spl15_96), inference(avatar_component_clause, [], [f860])).
fof(f1774, plain, (~ spl15_27 | ~ spl15_31), inference(avatar_split_clause, [], [f1773, f451, f434])).
fof(f1773, plain, (~ (e2 = op(e2, e1)) | ~ spl15_31), inference(backward_demodulation, [], [f169, f453])).
fof(f1744, plain, (~ spl15_61 | ~ spl15_63), inference(avatar_contradiction_clause, [], [f1743])).
fof(f1743, plain, ($false | (~ spl15_61 | ~ spl15_63)), inference(subsumption_resolution, [], [f1742, f182])).
fof(f1742, plain, ((e0 = e2) | (~ spl15_61 | ~ spl15_63)), inference(backward_demodulation, [], [f589, f581])).
fof(f1740, plain, (~ spl15_63 | ~ spl15_64), inference(avatar_contradiction_clause, [], [f1739])).
fof(f1739, plain, ($false | (~ spl15_63 | ~ spl15_64)), inference(subsumption_resolution, [], [f1738, f186])).
fof(f1738, plain, ((e2 = e3) | (~ spl15_63 | ~ spl15_64)), inference(forward_demodulation, [], [f593, f589])).
fof(f1737, plain, (~ spl15_36 | spl15_81), inference(avatar_contradiction_clause, [], [f1736])).
fof(f1736, plain, ($false | (~ spl15_36 | spl15_81)), inference(subsumption_resolution, [], [f1735, f474])).
fof(f474, plain, ((e3 = op(e1, e3)) | ~ spl15_36), inference(avatar_component_clause, [], [f472])).
fof(f1735, plain, (~ (e3 = op(e1, e3)) | (~ spl15_36 | spl15_81)), inference(forward_demodulation, [], [f736, f474])).
fof(f1734, plain, (~ spl15_42 | spl15_83), inference(avatar_contradiction_clause, [], [f1733])).
fof(f1733, plain, ($false | (~ spl15_42 | spl15_83)), inference(subsumption_resolution, [], [f1732, f500])).
fof(f500, plain, ((e1 = op(e1, e1)) | ~ spl15_42), inference(avatar_component_clause, [], [f498])).
fof(f1732, plain, (~ (e1 = op(e1, e1)) | (~ spl15_42 | spl15_83)), inference(forward_demodulation, [], [f746, f500])).
fof(f746, plain, (~ (e1 = op(e1, op(e1, e1))) | spl15_83), inference(avatar_component_clause, [], [f744])).
fof(f744, plain, (spl15_83 <=> (e1 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_83])])).
fof(f1727, plain, (~ spl15_24 | ~ spl15_63 | spl15_111), inference(avatar_split_clause, [], [f1726, f930, f587, f421])).
fof(f1726, plain, (~ (e3 = op(e2, e2)) | (~ spl15_63 | spl15_111)), inference(forward_demodulation, [], [f932, f589])).
fof(f1725, plain, (~ spl15_58 | ~ spl15_42), inference(avatar_split_clause, [], [f1724, f498, f566])).
fof(f1724, plain, (~ (e1 = op(e0, e1)) | ~ spl15_42), inference(forward_demodulation, [], [f139, f500])).
fof(f1707, plain, (~ spl15_31 | ~ spl15_63), inference(avatar_split_clause, [], [f1706, f587, f451])).
fof(f1706, plain, (~ (e2 = op(e2, e0)) | ~ spl15_63), inference(forward_demodulation, [], [f134, f589])).
fof(f1697, plain, (~ spl15_22 | ~ spl15_3 | spl15_99), inference(avatar_split_clause, [], [f1689, f873, f332, f413])).
fof(f873, plain, (spl15_99 <=> (e1 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_99])])).
fof(f1689, plain, (~ (e1 = op(e2, e2)) | (~ spl15_3 | spl15_99)), inference(forward_demodulation, [], [f875, f334])).
fof(f875, plain, (~ (e1 = op(op(e3, e3), op(e3, e3))) | spl15_99), inference(avatar_component_clause, [], [f873])).
fof(f1682, plain, (~ spl15_3 | ~ spl15_17 | spl15_98), inference(avatar_contradiction_clause, [], [f1681])).
fof(f1681, plain, ($false | (~ spl15_3 | ~ spl15_17 | spl15_98)), inference(subsumption_resolution, [], [f1676, f394])).
fof(f1676, plain, (~ (e0 = op(e2, e3)) | (~ spl15_3 | spl15_98)), inference(backward_demodulation, [], [f871, f334])).
fof(f871, plain, (~ (e0 = op(op(e3, e3), e3)) | spl15_98), inference(avatar_component_clause, [], [f869])).
fof(f869, plain, (spl15_98 <=> (e0 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_98])])).
fof(f1660, plain, (~ spl15_1 | ~ spl15_17), inference(avatar_split_clause, [], [f1658, f392, f324])).
fof(f1658, plain, (~ (e0 = op(e3, e3)) | ~ spl15_17), inference(backward_demodulation, [], [f156, f394])).
fof(f1655, plain, (~ spl15_18 | ~ spl15_22), inference(avatar_split_clause, [], [f1652, f413, f396])).
fof(f1652, plain, (~ (e1 = op(e2, e3)) | ~ spl15_22), inference(backward_demodulation, [], [f174, f415])).
fof(f415, plain, ((e1 = op(e2, e2)) | ~ spl15_22), inference(avatar_component_clause, [], [f413])).
fof(f1654, plain, (~ spl15_6 | ~ spl15_22), inference(avatar_split_clause, [], [f1650, f413, f345])).
fof(f1650, plain, (~ (e1 = op(e3, e2)) | ~ spl15_22), inference(backward_demodulation, [], [f150, f415])).
fof(f1626, plain, (~ spl15_52 | ~ spl15_36), inference(avatar_split_clause, [], [f1625, f472, f540])).
fof(f1625, plain, (~ (e3 = op(e0, e3)) | ~ spl15_36), inference(forward_demodulation, [], [f151, f474])).
fof(f1622, plain, (~ spl15_19 | ~ spl15_27), inference(avatar_split_clause, [], [f1621, f434, f400])).
fof(f1621, plain, (~ (e2 = op(e2, e3)) | ~ spl15_27), inference(forward_demodulation, [], [f173, f436])).
fof(f1594, plain, (spl15_62 | ~ spl15_1 | ~ spl15_99), inference(avatar_split_clause, [], [f1593, f873, f324, f583])).
fof(f1593, plain, ((op(e0, e0) = e1) | (~ spl15_1 | ~ spl15_99)), inference(forward_demodulation, [], [f874, f326])).
fof(f874, plain, ((e1 = op(op(e3, e3), op(e3, e3))) | ~ spl15_99), inference(avatar_component_clause, [], [f873])).
fof(f1592, plain, (~ spl15_1 | ~ spl15_51 | spl15_114), inference(avatar_contradiction_clause, [], [f1591])).
fof(f1591, plain, ($false | (~ spl15_1 | ~ spl15_51 | spl15_114)), inference(subsumption_resolution, [], [f1590, f538])).
fof(f1590, plain, (~ (e2 = op(e0, e3)) | (~ spl15_1 | spl15_114)), inference(forward_demodulation, [], [f947, f326])).
fof(f1579, plain, (~ spl15_7 | spl15_67), inference(avatar_contradiction_clause, [], [f1578])).
fof(f1578, plain, ($false | (~ spl15_7 | spl15_67)), inference(subsumption_resolution, [], [f1576, f351])).
fof(f1576, plain, (~ (e2 = op(e3, e2)) | (~ spl15_7 | spl15_67)), inference(backward_demodulation, [], [f639, f351])).
fof(f639, plain, (~ (e2 = op(e3, op(e3, e2))) | spl15_67), inference(avatar_component_clause, [], [f637])).
fof(f1570, plain, (~ spl15_21 | ~ spl15_24), inference(avatar_contradiction_clause, [], [f1569])).
fof(f1569, plain, ($false | (~ spl15_21 | ~ spl15_24)), inference(subsumption_resolution, [], [f1568, f183])).
fof(f1568, plain, ((e0 = e3) | (~ spl15_21 | ~ spl15_24)), inference(backward_demodulation, [], [f423, f411])).
fof(f1565, plain, (~ spl15_1 | ~ spl15_24 | spl15_106), inference(avatar_contradiction_clause, [], [f1564])).
fof(f1564, plain, ($false | (~ spl15_1 | ~ spl15_24 | spl15_106)), inference(subsumption_resolution, [], [f1561, f326])).
fof(f1561, plain, (~ (e0 = op(e3, e3)) | (~ spl15_24 | spl15_106)), inference(backward_demodulation, [], [f908, f423])).
fof(f908, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl15_106), inference(avatar_component_clause, [], [f906])).
fof(f1557, plain, (~ spl15_15 | ~ spl15_31), inference(avatar_split_clause, [], [f1556, f451, f383])).
fof(f1556, plain, (~ (e2 = op(e3, e0)) | ~ spl15_31), inference(backward_demodulation, [], [f138, f453])).
fof(f1538, plain, (~ spl15_6 | ~ spl15_54), inference(avatar_split_clause, [], [f1535, f549, f345])).
fof(f1535, plain, (~ (e1 = op(e3, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f147, f551])).
fof(f1526, plain, (~ spl15_60 | ~ spl15_12), inference(avatar_split_clause, [], [f1525, f370, f574])).
fof(f1525, plain, (~ (e3 = op(e0, e1)) | ~ spl15_12), inference(forward_demodulation, [], [f141, f372])).
fof(f1508, plain, (~ spl15_32 | ~ spl15_64), inference(avatar_split_clause, [], [f1507, f591, f455])).
fof(f1507, plain, (~ (e3 = op(e2, e0)) | ~ spl15_64), inference(forward_demodulation, [], [f134, f593])).
fof(f1503, plain, (~ spl15_43 | ~ spl15_27), inference(avatar_split_clause, [], [f1502, f434, f502])).
fof(f1502, plain, (~ (e2 = op(e1, e1)) | ~ spl15_27), inference(forward_demodulation, [], [f142, f436])).
fof(f1499, plain, (~ spl15_16 | ~ spl15_1 | spl15_66), inference(avatar_split_clause, [], [f1408, f632, f324, f387])).
fof(f1408, plain, (~ (e3 = op(e3, e0)) | (~ spl15_1 | spl15_66)), inference(backward_demodulation, [], [f634, f326])).
fof(f634, plain, (~ (e3 = op(e3, op(e3, e3))) | spl15_66), inference(avatar_component_clause, [], [f632])).
fof(f1496, plain, (~ spl15_15 | ~ spl15_64 | spl15_115), inference(avatar_split_clause, [], [f1460, f950, f591, f383])).
fof(f1460, plain, (~ (e2 = op(e3, e0)) | (~ spl15_64 | spl15_115)), inference(backward_demodulation, [], [f952, f593])).
fof(f1492, plain, (~ spl15_7 | ~ spl15_39), inference(avatar_split_clause, [], [f1491, f485, f349])).
fof(f1491, plain, (~ (e2 = op(e3, e2)) | ~ spl15_39), inference(forward_demodulation, [], [f149, f487])).
fof(f1490, plain, (~ spl15_13 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1489])).
fof(f1489, plain, ($false | (~ spl15_13 | ~ spl15_15)), inference(subsumption_resolution, [], [f1488, f182])).
fof(f1488, plain, ((e0 = e2) | (~ spl15_13 | ~ spl15_15)), inference(backward_demodulation, [], [f385, f377])).
fof(f1480, plain, (~ spl15_25 | ~ spl15_27), inference(avatar_contradiction_clause, [], [f1479])).
fof(f1479, plain, ($false | (~ spl15_25 | ~ spl15_27)), inference(subsumption_resolution, [], [f1478, f182])).
fof(f1478, plain, ((e0 = e2) | (~ spl15_25 | ~ spl15_27)), inference(forward_demodulation, [], [f436, f428])).
fof(f1477, plain, (~ spl15_33 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f1476])).
fof(f1476, plain, ($false | (~ spl15_33 | ~ spl15_36)), inference(subsumption_resolution, [], [f1475, f183])).
fof(f1475, plain, ((e0 = e3) | (~ spl15_33 | ~ spl15_36)), inference(backward_demodulation, [], [f474, f462])).
fof(f1469, plain, (~ spl15_53 | ~ spl15_56), inference(avatar_contradiction_clause, [], [f1468])).
fof(f1468, plain, ($false | (~ spl15_53 | ~ spl15_56)), inference(subsumption_resolution, [], [f1467, f183])).
fof(f1467, plain, ((e0 = e3) | (~ spl15_53 | ~ spl15_56)), inference(backward_demodulation, [], [f559, f547])).
fof(f1458, plain, (~ spl15_62 | ~ spl15_46), inference(avatar_split_clause, [], [f1457, f515, f583])).
fof(f1457, plain, (~ (op(e0, e0) = e1) | ~ spl15_46), inference(forward_demodulation, [], [f133, f517])).
fof(f1452, plain, (~ spl15_62 | ~ spl15_58), inference(avatar_split_clause, [], [f1451, f566, f583])).
fof(f1451, plain, (~ (op(e0, e0) = e1) | ~ spl15_58), inference(forward_demodulation, [], [f157, f568])).
fof(f1446, plain, (~ spl15_62 | ~ spl15_1 | spl15_99), inference(avatar_split_clause, [], [f1409, f873, f324, f583])).
fof(f1409, plain, (~ (op(e0, e0) = e1) | (~ spl15_1 | spl15_99)), inference(backward_demodulation, [], [f875, f326])).
fof(f1445, plain, (~ spl15_63 | ~ spl15_1 | spl15_102), inference(avatar_split_clause, [], [f1410, f887, f324, f587])).
fof(f887, plain, (spl15_102 <=> (e2 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_102])])).
fof(f1410, plain, (~ (op(e0, e0) = e2) | (~ spl15_1 | spl15_102)), inference(backward_demodulation, [], [f889, f326])).
fof(f889, plain, (~ (e2 = op(op(e3, e3), op(e3, e3))) | spl15_102), inference(avatar_component_clause, [], [f887])).
fof(f1443, plain, (~ spl15_43 | ~ spl15_44), inference(avatar_contradiction_clause, [], [f1442])).
fof(f1442, plain, ($false | (~ spl15_43 | ~ spl15_44)), inference(subsumption_resolution, [], [f1441, f186])).
fof(f1441, plain, ((e2 = e3) | (~ spl15_43 | ~ spl15_44)), inference(forward_demodulation, [], [f508, f504])).
fof(f1426, plain, (~ spl15_25 | ~ spl15_43 | spl15_100), inference(avatar_contradiction_clause, [], [f1425])).
fof(f1425, plain, ($false | (~ spl15_25 | ~ spl15_43 | spl15_100)), inference(subsumption_resolution, [], [f1424, f428])).
fof(f1424, plain, (~ (e0 = op(e2, e1)) | (~ spl15_43 | spl15_100)), inference(forward_demodulation, [], [f880, f504])).
fof(f1399, plain, (~ spl15_5 | ~ spl15_15 | spl15_69), inference(avatar_split_clause, [], [f1395, f647, f383, f341])).
fof(f1395, plain, (~ (e0 = op(e3, e2)) | (~ spl15_15 | spl15_69)), inference(backward_demodulation, [], [f649, f385])).
fof(f1391, plain, (~ spl15_17 | ~ spl15_25), inference(avatar_split_clause, [], [f1389, f426, f392])).
fof(f1389, plain, (~ (e0 = op(e2, e3)) | ~ spl15_25), inference(backward_demodulation, [], [f173, f428])).
fof(f1381, plain, (~ spl15_42 | ~ spl15_43), inference(avatar_contradiction_clause, [], [f1380])).
fof(f1380, plain, ($false | (~ spl15_42 | ~ spl15_43)), inference(subsumption_resolution, [], [f1379, f184])).
fof(f1379, plain, ((e1 = e2) | (~ spl15_42 | ~ spl15_43)), inference(forward_demodulation, [], [f504, f500])).
fof(f1372, plain, (~ spl15_41 | ~ spl15_42 | spl15_100), inference(avatar_split_clause, [], [f1371, f878, f498, f494])).
fof(f1371, plain, (~ (e0 = op(e1, e1)) | (~ spl15_42 | spl15_100)), inference(forward_demodulation, [], [f880, f500])).
fof(f1363, plain, (~ spl15_38 | ~ spl15_42), inference(avatar_split_clause, [], [f1362, f498, f481])).
fof(f1362, plain, (~ (e1 = op(e1, e2)) | ~ spl15_42), inference(forward_demodulation, [], [f166, f500])).
fof(f1350, plain, (~ spl15_42 | ~ spl15_44), inference(avatar_contradiction_clause, [], [f1349])).
fof(f1349, plain, ($false | (~ spl15_42 | ~ spl15_44)), inference(subsumption_resolution, [], [f1348, f185])).
fof(f1348, plain, ((e1 = e3) | (~ spl15_42 | ~ spl15_44)), inference(forward_demodulation, [], [f508, f500])).
fof(f1347, plain, (~ spl15_40 | ~ spl15_36), inference(avatar_split_clause, [], [f1346, f472, f489])).
fof(f1346, plain, (~ (e3 = op(e1, e2)) | ~ spl15_36), inference(forward_demodulation, [], [f168, f474])).
fof(f1343, plain, (~ spl15_28 | ~ spl15_12), inference(avatar_split_clause, [], [f1342, f370, f438])).
fof(f1342, plain, (~ (e3 = op(e2, e1)) | ~ spl15_12), inference(forward_demodulation, [], [f144, f372])).
fof(f1325, plain, (~ spl15_2 | ~ spl15_12 | spl15_68), inference(avatar_split_clause, [], [f1323, f642, f370, f328])).
fof(f1323, plain, (~ (e1 = op(e3, e3)) | (~ spl15_12 | spl15_68)), inference(backward_demodulation, [], [f644, f372])).
fof(f1322, plain, (~ spl15_2 | ~ spl15_14), inference(avatar_split_clause, [], [f1321, f379, f328])).
fof(f1321, plain, (~ (e1 = op(e3, e3)) | ~ spl15_14), inference(backward_demodulation, [], [f177, f381])).
fof(f1314, plain, (~ spl15_11 | ~ spl15_27), inference(avatar_split_clause, [], [f1313, f434, f366])).
fof(f1313, plain, (~ (e2 = op(e3, e1)) | ~ spl15_27), inference(backward_demodulation, [], [f144, f436])).
fof(f1311, plain, (~ spl15_30 | ~ spl15_32), inference(avatar_contradiction_clause, [], [f1310])).
fof(f1310, plain, ($false | (~ spl15_30 | ~ spl15_32)), inference(subsumption_resolution, [], [f1309, f185])).
fof(f1309, plain, ((e1 = e3) | (~ spl15_30 | ~ spl15_32)), inference(backward_demodulation, [], [f457, f449])).
fof(f1297, plain, (~ spl15_41 | ~ spl15_44), inference(avatar_contradiction_clause, [], [f1296])).
fof(f1296, plain, ($false | (~ spl15_41 | ~ spl15_44)), inference(subsumption_resolution, [], [f1295, f183])).
fof(f1295, plain, ((e0 = e3) | (~ spl15_41 | ~ spl15_44)), inference(forward_demodulation, [], [f508, f496])).
fof(f1294, plain, (~ spl15_15 | ~ spl15_47), inference(avatar_split_clause, [], [f1291, f519, f383])).
fof(f1291, plain, (~ (e2 = op(e3, e0)) | ~ spl15_47), inference(backward_demodulation, [], [f137, f521])).
fof(f1261, plain, (~ spl15_22 | ~ spl15_23), inference(avatar_contradiction_clause, [], [f1260])).
fof(f1260, plain, ($false | (~ spl15_22 | ~ spl15_23)), inference(subsumption_resolution, [], [f1259, f184])).
fof(f1259, plain, ((e1 = e2) | (~ spl15_22 | ~ spl15_23)), inference(backward_demodulation, [], [f419, f415])).
fof(f1252, plain, (~ spl15_24 | ~ spl15_28), inference(avatar_split_clause, [], [f1251, f438, f421])).
fof(f1251, plain, (~ (e3 = op(e2, e2)) | ~ spl15_28), inference(backward_demodulation, [], [f172, f440])).
fof(f1245, plain, (~ spl15_24 | ~ spl15_40), inference(avatar_split_clause, [], [f1244, f489, f421])).
fof(f1244, plain, (~ (e3 = op(e2, e2)) | ~ spl15_40), inference(backward_demodulation, [], [f148, f491])).
fof(f1242, plain, (~ spl15_30 | ~ spl15_46), inference(avatar_split_clause, [], [f1240, f515, f447])).
fof(f1240, plain, (~ (e1 = op(e2, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f136, f517])).
fof(f1234, plain, (~ spl15_54 | ~ spl15_58), inference(avatar_split_clause, [], [f1232, f566, f549])).
fof(f1232, plain, (~ (e1 = op(e0, e2)) | ~ spl15_58), inference(backward_demodulation, [], [f160, f568])).
fof(f1224, plain, (~ spl15_59 | ~ spl15_41 | spl15_112), inference(avatar_split_clause, [], [f1043, f936, f494, f570])).
fof(f1043, plain, (~ (e2 = op(e0, e1)) | (~ spl15_41 | spl15_112)), inference(backward_demodulation, [], [f938, f496])).
fof(f1223, plain, (~ spl15_53 | ~ spl15_5), inference(avatar_split_clause, [], [f1222, f341, f545])).
fof(f1222, plain, (~ (e0 = op(e0, e2)) | ~ spl15_5), inference(forward_demodulation, [], [f147, f343])).
fof(f1221, plain, (~ spl15_55 | ~ spl15_51), inference(avatar_split_clause, [], [f1220, f536, f553])).
fof(f1220, plain, (~ (e2 = op(e0, e2)) | ~ spl15_51), inference(forward_demodulation, [], [f162, f538])).
fof(f1216, plain, (~ spl15_49 | ~ spl15_17), inference(avatar_split_clause, [], [f1215, f392, f528])).
fof(f1215, plain, (~ (e0 = op(e0, e3)) | ~ spl15_17), inference(forward_demodulation, [], [f152, f394])).
fof(f1214, plain, (~ spl15_50 | ~ spl15_2), inference(avatar_split_clause, [], [f1213, f328, f532])).
fof(f1213, plain, (~ (e1 = op(e0, e3)) | ~ spl15_2), inference(forward_demodulation, [], [f153, f330])).
fof(f1210, plain, (~ spl15_45 | ~ spl15_41), inference(avatar_split_clause, [], [f1209, f494, f511])).
fof(f1209, plain, (~ (e0 = op(e1, e0)) | ~ spl15_41), inference(forward_demodulation, [], [f163, f496])).
fof(f1204, plain, (~ spl15_39 | ~ spl15_35), inference(avatar_split_clause, [], [f1203, f468, f485])).
fof(f1203, plain, (~ (e2 = op(e1, e2)) | ~ spl15_35), inference(forward_demodulation, [], [f168, f470])).
fof(f1202, plain, (~ spl15_33 | ~ spl15_17), inference(avatar_split_clause, [], [f1201, f392, f460])).
fof(f1201, plain, (~ (e0 = op(e1, e3)) | ~ spl15_17), inference(forward_demodulation, [], [f154, f394])).
fof(f1188, plain, (~ spl15_21 | ~ spl15_17), inference(avatar_split_clause, [], [f1187, f392, f409])).
fof(f1187, plain, (~ (e0 = op(e2, e2)) | ~ spl15_17), inference(forward_demodulation, [], [f174, f394])).
fof(f1186, plain, (~ spl15_17 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f1185])).
fof(f1185, plain, ($false | (~ spl15_17 | ~ spl15_20)), inference(subsumption_resolution, [], [f1184, f183])).
fof(f1184, plain, ((e0 = e3) | (~ spl15_17 | ~ spl15_20)), inference(forward_demodulation, [], [f406, f394])).
fof(f1183, plain, (~ spl15_11 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1182])).
fof(f1182, plain, ($false | (~ spl15_11 | ~ spl15_15)), inference(subsumption_resolution, [], [f1181, f385])).
fof(f1181, plain, (~ (e2 = op(e3, e0)) | ~ spl15_11), inference(forward_demodulation, [], [f175, f368])).
fof(f1180, plain, (~ spl15_13 | ~ spl15_5), inference(avatar_split_clause, [], [f1179, f341, f375])).
fof(f1179, plain, (~ (e0 = op(e3, e0)) | ~ spl15_5), inference(forward_demodulation, [], [f176, f343])).
fof(f1167, plain, (~ spl15_5 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1166])).
fof(f1166, plain, ($false | (~ spl15_5 | ~ spl15_8)), inference(subsumption_resolution, [], [f1165, f183])).
fof(f1165, plain, ((e0 = e3) | (~ spl15_5 | ~ spl15_8)), inference(forward_demodulation, [], [f355, f343])).
fof(f1155, plain, (~ spl15_2 | ~ spl15_3), inference(avatar_contradiction_clause, [], [f1154])).
fof(f1154, plain, ($false | (~ spl15_2 | ~ spl15_3)), inference(subsumption_resolution, [], [f1153, f184])).
fof(f1153, plain, ((e1 = e2) | (~ spl15_2 | ~ spl15_3)), inference(backward_demodulation, [], [f334, f330])).
fof(f1152, plain, (~ spl15_3 | ~ spl15_4), inference(avatar_contradiction_clause, [], [f1151])).
fof(f1151, plain, ($false | (~ spl15_3 | ~ spl15_4)), inference(subsumption_resolution, [], [f1150, f186])).
fof(f1150, plain, ((e2 = e3) | (~ spl15_3 | ~ spl15_4)), inference(backward_demodulation, [], [f338, f334])).
fof(f338, plain, ((e3 = op(e3, e3)) | ~ spl15_4), inference(avatar_component_clause, [], [f336])).
fof(f1146, plain, (~ spl15_4 | spl15_66), inference(avatar_contradiction_clause, [], [f1145])).
fof(f1145, plain, ($false | (~ spl15_4 | spl15_66)), inference(subsumption_resolution, [], [f1141, f338])).
fof(f1141, plain, (~ (e3 = op(e3, e3)) | (~ spl15_4 | spl15_66)), inference(backward_demodulation, [], [f634, f338])).
fof(f1140, plain, (~ spl15_5 | ~ spl15_6), inference(avatar_contradiction_clause, [], [f1139])).
fof(f1139, plain, ($false | (~ spl15_5 | ~ spl15_6)), inference(subsumption_resolution, [], [f1138, f181])).
fof(f1138, plain, ((e0 = e1) | (~ spl15_5 | ~ spl15_6)), inference(backward_demodulation, [], [f347, f343])).
fof(f347, plain, ((e1 = op(e3, e2)) | ~ spl15_6), inference(avatar_component_clause, [], [f345])).
fof(f1134, plain, (~ spl15_7 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1133])).
fof(f1133, plain, ($false | (~ spl15_7 | ~ spl15_8)), inference(subsumption_resolution, [], [f1132, f186])).
fof(f1132, plain, ((e2 = e3) | (~ spl15_7 | ~ spl15_8)), inference(backward_demodulation, [], [f355, f351])).
fof(f1131, plain, (~ spl15_3 | ~ spl15_8 | spl15_67), inference(avatar_split_clause, [], [f1129, f637, f353, f332])).
fof(f1129, plain, (~ (e2 = op(e3, e3)) | (~ spl15_8 | spl15_67)), inference(backward_demodulation, [], [f639, f355])).
fof(f1127, plain, (~ spl15_10 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1126])).
fof(f1126, plain, ($false | (~ spl15_10 | ~ spl15_11)), inference(subsumption_resolution, [], [f1125, f184])).
fof(f1125, plain, ((e1 = e2) | (~ spl15_10 | ~ spl15_11)), inference(backward_demodulation, [], [f368, f364])).
fof(f1124, plain, (~ spl15_11 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f1123])).
fof(f1123, plain, ($false | (~ spl15_11 | ~ spl15_12)), inference(subsumption_resolution, [], [f1122, f186])).
fof(f1122, plain, ((e2 = e3) | (~ spl15_11 | ~ spl15_12)), inference(backward_demodulation, [], [f372, f368])).
fof(f1120, plain, (~ spl15_4 | ~ spl15_12), inference(avatar_split_clause, [], [f1117, f370, f336])).
fof(f1117, plain, (~ (e3 = op(e3, e3)) | ~ spl15_12), inference(backward_demodulation, [], [f179, f372])).
fof(f1115, plain, (~ spl15_14 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1114])).
fof(f1114, plain, ($false | (~ spl15_14 | ~ spl15_15)), inference(subsumption_resolution, [], [f1113, f184])).
fof(f1113, plain, ((e1 = e2) | (~ spl15_14 | ~ spl15_15)), inference(backward_demodulation, [], [f385, f381])).
fof(f1112, plain, (~ spl15_15 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f1111])).
fof(f1111, plain, ($false | (~ spl15_15 | ~ spl15_16)), inference(subsumption_resolution, [], [f1110, f186])).
fof(f1110, plain, ((e2 = e3) | (~ spl15_15 | ~ spl15_16)), inference(backward_demodulation, [], [f389, f385])).
fof(f1108, plain, (~ spl15_4 | ~ spl15_16), inference(avatar_split_clause, [], [f1104, f387, f336])).
fof(f1104, plain, (~ (e3 = op(e3, e3)) | ~ spl15_16), inference(backward_demodulation, [], [f177, f389])).
fof(f1101, plain, (~ spl15_17 | ~ spl15_18), inference(avatar_contradiction_clause, [], [f1100])).
fof(f1100, plain, ($false | (~ spl15_17 | ~ spl15_18)), inference(subsumption_resolution, [], [f1099, f181])).
fof(f1099, plain, ((e0 = e1) | (~ spl15_17 | ~ spl15_18)), inference(backward_demodulation, [], [f398, f394])).
fof(f1098, plain, (~ spl15_18 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f1097])).
fof(f1097, plain, ($false | (~ spl15_18 | ~ spl15_19)), inference(subsumption_resolution, [], [f1096, f184])).
fof(f1096, plain, ((e1 = e2) | (~ spl15_18 | ~ spl15_19)), inference(backward_demodulation, [], [f402, f398])).
fof(f1095, plain, (~ spl15_19 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f1094])).
fof(f1094, plain, ($false | (~ spl15_19 | ~ spl15_20)), inference(subsumption_resolution, [], [f1093, f186])).
fof(f1093, plain, ((e2 = e3) | (~ spl15_19 | ~ spl15_20)), inference(backward_demodulation, [], [f406, f402])).
fof(f1078, plain, (~ spl15_21 | ~ spl15_25), inference(avatar_split_clause, [], [f1075, f426, f409])).
fof(f1075, plain, (~ (e0 = op(e2, e2)) | ~ spl15_25), inference(backward_demodulation, [], [f172, f428])).
fof(f1077, plain, (~ spl15_9 | ~ spl15_25), inference(avatar_split_clause, [], [f1074, f426, f358])).
fof(f1074, plain, (~ (e0 = op(e3, e1)) | ~ spl15_25), inference(backward_demodulation, [], [f144, f428])).
fof(f1071, plain, (~ spl15_25 | ~ spl15_29), inference(avatar_split_clause, [], [f1067, f443, f426])).
fof(f1067, plain, (~ (e0 = op(e2, e1)) | ~ spl15_29), inference(backward_demodulation, [], [f169, f445])).
fof(f1070, plain, (~ spl15_13 | ~ spl15_29), inference(avatar_split_clause, [], [f1066, f443, f375])).
fof(f1066, plain, (~ (e0 = op(e3, e0)) | ~ spl15_29), inference(backward_demodulation, [], [f138, f445])).
fof(f1065, plain, (~ spl15_34 | ~ spl15_35), inference(avatar_contradiction_clause, [], [f1064])).
fof(f1064, plain, ($false | (~ spl15_34 | ~ spl15_35)), inference(subsumption_resolution, [], [f1063, f184])).
fof(f1063, plain, ((e1 = e2) | (~ spl15_34 | ~ spl15_35)), inference(backward_demodulation, [], [f470, f466])).
fof(f1062, plain, (~ spl15_35 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f1061])).
fof(f1061, plain, ($false | (~ spl15_35 | ~ spl15_36)), inference(subsumption_resolution, [], [f1060, f186])).
fof(f1060, plain, ((e2 = e3) | (~ spl15_35 | ~ spl15_36)), inference(backward_demodulation, [], [f474, f470])).
fof(f1059, plain, (~ spl15_4 | ~ spl15_36), inference(avatar_split_clause, [], [f1057, f472, f336])).
fof(f1057, plain, (~ (e3 = op(e3, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f155, f474])).
fof(f1058, plain, (~ spl15_20 | ~ spl15_36), inference(avatar_split_clause, [], [f1056, f472, f404])).
fof(f1056, plain, (~ (e3 = op(e2, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f154, f474])).
fof(f1053, plain, (~ spl15_21 | ~ spl15_37), inference(avatar_split_clause, [], [f1050, f477, f409])).
fof(f1050, plain, (~ (e0 = op(e2, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f148, f479])).
fof(f1044, plain, (~ spl15_25 | ~ spl15_41), inference(avatar_split_clause, [], [f1037, f494, f426])).
fof(f1037, plain, (~ (e0 = op(e2, e1)) | ~ spl15_41), inference(backward_demodulation, [], [f142, f496])).
fof(f1036, plain, (~ spl15_33 | ~ spl15_45), inference(avatar_split_clause, [], [f1031, f511, f460])).
fof(f1031, plain, (~ (e0 = op(e1, e3)) | ~ spl15_45), inference(backward_demodulation, [], [f165, f513])).
fof(f1032, plain, (~ spl15_29 | ~ spl15_45), inference(avatar_split_clause, [], [f1027, f511, f443])).
fof(f1027, plain, (~ (e0 = op(e2, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f136, f513])).
fof(f1026, plain, (~ spl15_50 | ~ spl15_51), inference(avatar_contradiction_clause, [], [f1025])).
fof(f1025, plain, ($false | (~ spl15_50 | ~ spl15_51)), inference(subsumption_resolution, [], [f1024, f184])).
fof(f1024, plain, ((e1 = e2) | (~ spl15_50 | ~ spl15_51)), inference(backward_demodulation, [], [f538, f534])).
fof(f1023, plain, (~ spl15_51 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f1022])).
fof(f1022, plain, ($false | (~ spl15_51 | ~ spl15_52)), inference(subsumption_resolution, [], [f1021, f186])).
fof(f1021, plain, ((e2 = e3) | (~ spl15_51 | ~ spl15_52)), inference(backward_demodulation, [], [f542, f538])).
fof(f1006, plain, (~ spl15_49 | ~ spl15_57), inference(avatar_split_clause, [], [f1001, f562, f528])).
fof(f1001, plain, (~ (e0 = op(e0, e3)) | ~ spl15_57), inference(backward_demodulation, [], [f161, f564])).
fof(f1003, plain, (~ spl15_25 | ~ spl15_57), inference(avatar_split_clause, [], [f998, f562, f426])).
fof(f998, plain, (~ (e0 = op(e2, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f140, f564])).
fof(f993, plain, (~ spl15_49 | ~ spl15_61), inference(avatar_split_clause, [], [f984, f579, f528])).
fof(f984, plain, (~ (e0 = op(e0, e3)) | ~ spl15_61), inference(backward_demodulation, [], [f159, f581])).
fof(f992, plain, (~ spl15_53 | ~ spl15_61), inference(avatar_split_clause, [], [f983, f579, f545])).
fof(f983, plain, (~ (e0 = op(e0, e2)) | ~ spl15_61), inference(backward_demodulation, [], [f158, f581])).
fof(f990, plain, (~ spl15_13 | ~ spl15_61), inference(avatar_split_clause, [], [f981, f579, f375])).
fof(f981, plain, (~ (e0 = op(e3, e0)) | ~ spl15_61), inference(backward_demodulation, [], [f135, f581])).
fof(f988, plain, (~ spl15_45 | ~ spl15_61), inference(avatar_split_clause, [], [f979, f579, f511])).
fof(f979, plain, (~ (e0 = op(e1, e0)) | ~ spl15_61), inference(backward_demodulation, [], [f133, f581])).
fof(f977, plain, (~ spl15_119 | ~ spl15_110 | ~ spl15_62), inference(avatar_split_clause, [], [f321, f583, f924, f972])).
fof(f321, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (e3 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (e3 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((op(e0, e0) = e1) & (e2 = op(op(e0, e0), op(e0, e0))) & (e3 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax28)).
fof(f965, plain, (~ spl15_117 | ~ spl15_113 | ~ spl15_43), inference(avatar_split_clause, [], [f317, f502, f940, f962])).
fof(f317, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e2 = op(e1, e1)) & (e0 = op(op(e1, e1), op(e1, e1))) & (e3 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax24)).
fof(f960, plain, (~ spl15_112 | ~ spl15_103 | ~ spl15_41), inference(avatar_split_clause, [], [f316, f494, f892, f936])).
fof(f316, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e0 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e2 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax23)).
fof(f959, plain, (~ spl15_115 | ~ spl15_111 | ~ spl15_62), inference(avatar_split_clause, [], [f315, f583, f930, f950])).
fof(f315, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((op(e0, e0) = e1) & (e3 = op(op(e0, e0), op(e0, e0))) & (e2 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax22)).
fof(f958, plain, (~ spl15_114 | ~ spl15_99 | ~ spl15_1), inference(avatar_split_clause, [], [f314, f324, f873, f945])).
fof(f314, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e3, e3)) & (e1 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax21)).
fof(f957, plain, (~ spl15_115 | ~ spl15_116 | ~ spl15_64), inference(avatar_split_clause, [], [f313, f591, f954, f950])).
fof(f313, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((op(e0, e0) = e3) & (e1 = op(op(e0, e0), op(e0, e0))) & (e2 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax20)).
fof(f948, plain, (~ spl15_114 | ~ spl15_108 | ~ spl15_2), inference(avatar_split_clause, [], [f312, f328, f915, f945])).
fof(f312, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e3, e3)) & (e0 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax19)).
fof(f943, plain, (~ spl15_112 | ~ spl15_113 | ~ spl15_44), inference(avatar_split_clause, [], [f311, f506, f940, f936])).
fof(f311, plain, (~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e1, e1)) & (e0 = op(op(e1, e1), op(e1, e1))) & (e2 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax18)).
fof(f933, plain, (~ spl15_109 | ~ spl15_111 | ~ spl15_63), inference(avatar_split_clause, [], [f309, f587, f930, f920])).
fof(f309, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e1 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e1 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((op(e0, e0) = e2) & (e3 = op(op(e0, e0), op(e0, e0))) & (e1 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax16)).
fof(f928, plain, (~ spl15_107 | ~ spl15_102 | ~ spl15_1), inference(avatar_split_clause, [], [f308, f324, f887, f911])).
fof(f308, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e3, e3)) & (e2 = op(op(e3, e3), op(e3, e3))) & (e1 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax15)).
fof(f927, plain, (~ spl15_109 | ~ spl15_110 | ~ spl15_64), inference(avatar_split_clause, [], [f307, f591, f924, f920])).
fof(f307, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (e1 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (e1 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((op(e0, e0) = e3) & (e2 = op(op(e0, e0), op(e0, e0))) & (e1 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax14)).
fof(f895, plain, (~ spl15_100 | ~ spl15_103 | ~ spl15_43), inference(avatar_split_clause, [], [f303, f502, f892, f878])).
fof(f303, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e2 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax10)).
fof(f885, plain, (~ spl15_100 | ~ spl15_101 | ~ spl15_44), inference(avatar_split_clause, [], [f301, f506, f882, f878])).
fof(f301, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e3 = op(e1, e1)) & (e2 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax8)).
fof(f876, plain, (~ spl15_98 | ~ spl15_99 | ~ spl15_3), inference(avatar_split_clause, [], [f300, f332, f873, f869])).
fof(f300, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e2 = op(e3, e3)) & (e1 = op(op(e3, e3), op(e3, e3))) & (e0 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax7)).
fof(f858, plain, (~ spl15_57 | spl15_62 | spl15_42 | spl15_22 | spl15_2), inference(avatar_split_clause, [], [f278, f328, f413, f498, f583, f562])).
fof(f278, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e3 = op(e3, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e3 = op(e3, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e3 = op(e3, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e3 = op(e3, e0))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e2 = op(e2, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e2 = op(e2, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e2 = op(e2, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e2 = op(e2, e0))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e1 = op(e1, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e1 = op(e1, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e1 = op(e1, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e1 = op(e1, e0))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f5, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, ((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> (~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, ((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> (~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, ((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> (~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f5, plain, (((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | (~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | (~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | (~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | (~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e3 = op(e3, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e3 = op(e3, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e3 = op(e3, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e3 = op(e3, e0))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e2 = op(e2, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e2 = op(e2, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e2 = op(e2, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e2 = op(e2, e0))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e1 = op(e1, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e1 = op(e1, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e1 = op(e1, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e1 = op(e1, e0))) & ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax5)).
fof(f857, plain, (~ spl15_53 | spl15_63 | spl15_43 | spl15_23 | spl15_3), inference(avatar_split_clause, [], [f279, f332, f417, f502, f587, f545])).
fof(f279, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e0 = op(e0, e2))), inference(cnf_transformation, [], [f69])).
fof(f856, plain, (~ spl15_49 | spl15_64 | spl15_44 | spl15_24 | spl15_4), inference(avatar_split_clause, [], [f280, f336, f421, f506, f591, f528])).
fof(f280, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e0 = op(e0, e3))), inference(cnf_transformation, [], [f69])).
fof(f855, plain, (~ spl15_46 | spl15_61 | spl15_41 | spl15_21 | spl15_1), inference(avatar_split_clause, [], [f281, f324, f409, f494, f579, f515])).
fof(f281, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e1 = op(e1, e0))), inference(cnf_transformation, [], [f69])).
fof(f854, plain, (~ spl15_38 | spl15_63 | spl15_43 | spl15_23 | spl15_3), inference(avatar_split_clause, [], [f283, f332, f417, f502, f587, f481])).
fof(f283, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e1 = op(e1, e2))), inference(cnf_transformation, [], [f69])).
fof(f853, plain, (~ spl15_34 | spl15_64 | spl15_44 | spl15_24 | spl15_4), inference(avatar_split_clause, [], [f284, f336, f421, f506, f591, f464])).
fof(f284, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e1 = op(e1, e3))), inference(cnf_transformation, [], [f69])).
fof(f852, plain, (~ spl15_31 | spl15_61 | spl15_41 | spl15_21 | spl15_1), inference(avatar_split_clause, [], [f285, f324, f409, f494, f579, f451])).
fof(f285, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e2 = op(e2, e0))), inference(cnf_transformation, [], [f69])).
fof(f851, plain, (~ spl15_27 | spl15_62 | spl15_42 | spl15_22 | spl15_2), inference(avatar_split_clause, [], [f286, f328, f413, f498, f583, f434])).
fof(f286, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e2 = op(e2, e1))), inference(cnf_transformation, [], [f69])).
fof(f850, plain, (~ spl15_19 | spl15_64 | spl15_44 | spl15_24 | spl15_4), inference(avatar_split_clause, [], [f288, f336, f421, f506, f591, f400])).
fof(f288, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3) | ~ (e2 = op(e2, e3))), inference(cnf_transformation, [], [f69])).
fof(f849, plain, (~ spl15_16 | spl15_61 | spl15_41 | spl15_21 | spl15_1), inference(avatar_split_clause, [], [f289, f324, f409, f494, f579, f387])).
fof(f289, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)) | ~ (e3 = op(e3, e0))), inference(cnf_transformation, [], [f69])).
fof(f848, plain, (~ spl15_12 | spl15_62 | spl15_42 | spl15_22 | spl15_2), inference(avatar_split_clause, [], [f290, f328, f413, f498, f583, f370])).
fof(f290, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1) | ~ (e3 = op(e3, e1))), inference(cnf_transformation, [], [f69])).
fof(f847, plain, (~ spl15_8 | spl15_63 | spl15_43 | spl15_23 | spl15_3), inference(avatar_split_clause, [], [f291, f332, f417, f502, f587, f353])).
fof(f291, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2) | ~ (e3 = op(e3, e2))), inference(cnf_transformation, [], [f69])).
fof(f846, plain, (spl15_95 | spl15_94 | spl15_93 | spl15_88 | spl15_87 | spl15_86 | spl15_85 | spl15_80 | spl15_79 | spl15_78 | spl15_77 | spl15_72 | spl15_71 | spl15_70 | spl15_65 | spl15_4), inference(avatar_split_clause, [], [f293, f336, f628, f654, f664, f674, f700, f710, f720, f730, f756, f766, f776, f786, f812, f822, f832])).
fof(f832, plain, (spl15_95 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl15_95])])).
fof(f822, plain, (spl15_94 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl15_94])])).
fof(f812, plain, (spl15_93 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl15_93])])).
fof(f786, plain, (spl15_88 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl15_88])])).
fof(f776, plain, (spl15_87 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_87])])).
fof(f766, plain, (spl15_86 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_86])])).
fof(f756, plain, (spl15_85 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_85])])).
fof(f730, plain, (spl15_80 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_80])])).
fof(f720, plain, (spl15_79 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_79])])).
fof(f710, plain, (spl15_78 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl15_78])])).
fof(f700, plain, (spl15_77 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl15_77])])).
fof(f674, plain, (spl15_72 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl15_72])])).
fof(f664, plain, (spl15_71 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl15_71])])).
fof(f654, plain, (spl15_70 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl15_70])])).
fof(f628, plain, (spl15_65 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl15_65])])).
fof(f293, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f843, plain, (spl15_95 | spl15_94 | spl15_93 | spl15_88 | spl15_87 | spl15_86 | spl15_85 | spl15_80 | spl15_79 | spl15_78 | spl15_77 | spl15_72 | spl15_71 | spl15_70 | spl15_65 | ~ spl15_68), inference(avatar_split_clause, [], [f296, f642, f628, f654, f664, f674, f700, f710, f720, f730, f756, f766, f776, f786, f812, f822, f832])).
fof(f296, plain, (~ (e1 = op(e3, op(e3, e1))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f841, plain, (spl15_95 | spl15_94 | spl15_93 | spl15_88 | spl15_87 | spl15_86 | spl15_85 | spl15_80 | spl15_79 | spl15_78 | spl15_77 | spl15_72 | spl15_71 | spl15_70 | spl15_65 | ~ spl15_66), inference(avatar_split_clause, [], [f298, f632, f628, f654, f664, f674, f700, f710, f720, f730, f756, f766, f776, f786, f812, f822, f832])).
fof(f298, plain, (~ (e3 = op(e3, op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f840, plain, (~ spl15_95 | spl15_61), inference(avatar_split_clause, [], [f271, f579, f832])).
fof(f271, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f838, plain, (~ spl15_95 | ~ spl15_92), inference(avatar_split_clause, [], [f273, f805, f832])).
fof(f273, plain, (~ (e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f836, plain, (~ spl15_95 | ~ spl15_90), inference(avatar_split_clause, [], [f275, f795, f832])).
fof(f275, plain, (~ (e2 = op(e0, op(e0, e2))) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f830, plain, (~ spl15_94 | spl15_57), inference(avatar_split_clause, [], [f265, f562, f822])).
fof(f265, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f829, plain, (~ spl15_94 | spl15_45), inference(avatar_split_clause, [], [f266, f511, f822])).
fof(f266, plain, ((e0 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f826, plain, (~ spl15_94 | ~ spl15_90), inference(avatar_split_clause, [], [f269, f795, f822])).
fof(f269, plain, (~ (e2 = op(e0, op(e0, e2))) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f825, plain, (~ spl15_94 | ~ spl15_89), inference(avatar_split_clause, [], [f270, f790, f822])).
fof(f270, plain, (~ (e3 = op(e0, op(e0, e3))) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f820, plain, (~ spl15_93 | spl15_53), inference(avatar_split_clause, [], [f259, f545, f812])).
fof(f259, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f819, plain, (~ spl15_93 | spl15_29), inference(avatar_split_clause, [], [f260, f443, f812])).
fof(f260, plain, ((e0 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f817, plain, (~ spl15_93 | ~ spl15_91), inference(avatar_split_clause, [], [f262, f800, f812])).
fof(f262, plain, (~ (e1 = op(e0, op(e0, e1))) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f815, plain, (~ spl15_93 | ~ spl15_89), inference(avatar_split_clause, [], [f264, f790, f812])).
fof(f264, plain, (~ (e3 = op(e0, op(e0, e3))) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f810, plain, (~ spl15_88 | spl15_49), inference(avatar_split_clause, [], [f253, f528, f786])).
fof(f253, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e3 = op(e0, op(e0, e3))) & ~ (e2 = op(e0, op(e0, e2))) & ~ (e1 = op(e0, op(e0, e1))) & ~ (e0 = op(e0, op(e0, e0))) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f809, plain, (~ spl15_88 | spl15_13), inference(avatar_split_clause, [], [f254, f375, f786])).
fof(f254, plain, ((e0 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f808, plain, (~ spl15_88 | ~ spl15_92), inference(avatar_split_clause, [], [f255, f805, f786])).
fof(f255, plain, (~ (e0 = op(e0, op(e0, e0))) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f803, plain, (~ spl15_88 | ~ spl15_91), inference(avatar_split_clause, [], [f256, f800, f786])).
fof(f256, plain, (~ (e1 = op(e0, op(e0, e1))) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f798, plain, (~ spl15_88 | ~ spl15_90), inference(avatar_split_clause, [], [f257, f795, f786])).
fof(f257, plain, (~ (e2 = op(e0, op(e0, e2))) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f793, plain, (~ spl15_88 | ~ spl15_89), inference(avatar_split_clause, [], [f258, f790, f786])).
fof(f258, plain, (~ (e3 = op(e0, op(e0, e3))) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f784, plain, (~ spl15_87 | spl15_46), inference(avatar_split_clause, [], [f247, f515, f776])).
fof(f247, plain, ((e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f783, plain, (~ spl15_87 | spl15_58), inference(avatar_split_clause, [], [f248, f566, f776])).
fof(f248, plain, ((e1 = op(e0, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f782, plain, (~ spl15_87 | ~ spl15_84), inference(avatar_split_clause, [], [f249, f749, f776])).
fof(f249, plain, (~ (e0 = op(e1, op(e1, e0))) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f780, plain, (~ spl15_87 | ~ spl15_82), inference(avatar_split_clause, [], [f251, f739, f776])).
fof(f251, plain, (~ (e2 = op(e1, op(e1, e2))) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f779, plain, (~ spl15_87 | ~ spl15_81), inference(avatar_split_clause, [], [f252, f734, f776])).
fof(f252, plain, (~ (e3 = op(e1, op(e1, e3))) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f774, plain, (~ spl15_86 | spl15_42), inference(avatar_split_clause, [], [f241, f498, f766])).
fof(f241, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f771, plain, (~ spl15_86 | ~ spl15_83), inference(avatar_split_clause, [], [f244, f744, f766])).
fof(f244, plain, (~ (e1 = op(e1, op(e1, e1))) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f764, plain, (~ spl15_85 | spl15_38), inference(avatar_split_clause, [], [f235, f481, f756])).
fof(f235, plain, ((e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f763, plain, (~ spl15_85 | spl15_26), inference(avatar_split_clause, [], [f236, f430, f756])).
fof(f236, plain, ((e1 = op(e2, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f762, plain, (~ spl15_85 | ~ spl15_84), inference(avatar_split_clause, [], [f237, f749, f756])).
fof(f237, plain, (~ (e0 = op(e1, op(e1, e0))) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f761, plain, (~ spl15_85 | ~ spl15_83), inference(avatar_split_clause, [], [f238, f744, f756])).
fof(f238, plain, (~ (e1 = op(e1, op(e1, e1))) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f760, plain, (~ spl15_85 | ~ spl15_82), inference(avatar_split_clause, [], [f239, f739, f756])).
fof(f239, plain, (~ (e2 = op(e1, op(e1, e2))) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f759, plain, (~ spl15_85 | ~ spl15_81), inference(avatar_split_clause, [], [f240, f734, f756])).
fof(f240, plain, (~ (e3 = op(e1, op(e1, e3))) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f754, plain, (~ spl15_80 | spl15_34), inference(avatar_split_clause, [], [f229, f464, f730])).
fof(f229, plain, ((e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e3 = op(e1, op(e1, e3))) & ~ (e2 = op(e1, op(e1, e2))) & ~ (e1 = op(e1, op(e1, e1))) & ~ (e0 = op(e1, op(e1, e0))) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f753, plain, (~ spl15_80 | spl15_10), inference(avatar_split_clause, [], [f230, f362, f730])).
fof(f230, plain, ((e1 = op(e3, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f752, plain, (~ spl15_80 | ~ spl15_84), inference(avatar_split_clause, [], [f231, f749, f730])).
fof(f231, plain, (~ (e0 = op(e1, op(e1, e0))) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f747, plain, (~ spl15_80 | ~ spl15_83), inference(avatar_split_clause, [], [f232, f744, f730])).
fof(f232, plain, (~ (e1 = op(e1, op(e1, e1))) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f742, plain, (~ spl15_80 | ~ spl15_82), inference(avatar_split_clause, [], [f233, f739, f730])).
fof(f233, plain, (~ (e2 = op(e1, op(e1, e2))) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f737, plain, (~ spl15_80 | ~ spl15_81), inference(avatar_split_clause, [], [f234, f734, f730])).
fof(f234, plain, (~ (e3 = op(e1, op(e1, e3))) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f728, plain, (~ spl15_79 | spl15_31), inference(avatar_split_clause, [], [f223, f451, f720])).
fof(f223, plain, ((e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f727, plain, (~ spl15_79 | spl15_55), inference(avatar_split_clause, [], [f224, f553, f720])).
fof(f224, plain, ((e2 = op(e0, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f726, plain, (~ spl15_79 | ~ spl15_76), inference(avatar_split_clause, [], [f225, f693, f720])).
fof(f225, plain, (~ (e0 = op(e2, op(e2, e0))) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f725, plain, (~ spl15_79 | ~ spl15_75), inference(avatar_split_clause, [], [f226, f688, f720])).
fof(f226, plain, (~ (e1 = op(e2, op(e2, e1))) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f723, plain, (~ spl15_79 | ~ spl15_73), inference(avatar_split_clause, [], [f228, f678, f720])).
fof(f228, plain, (~ (e3 = op(e2, op(e2, e3))) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f718, plain, (~ spl15_78 | spl15_27), inference(avatar_split_clause, [], [f217, f434, f710])).
fof(f217, plain, ((e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f717, plain, (~ spl15_78 | spl15_39), inference(avatar_split_clause, [], [f218, f485, f710])).
fof(f218, plain, ((e2 = op(e1, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f716, plain, (~ spl15_78 | ~ spl15_76), inference(avatar_split_clause, [], [f219, f693, f710])).
fof(f219, plain, (~ (e0 = op(e2, op(e2, e0))) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f715, plain, (~ spl15_78 | ~ spl15_75), inference(avatar_split_clause, [], [f220, f688, f710])).
fof(f220, plain, (~ (e1 = op(e2, op(e2, e1))) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f713, plain, (~ spl15_78 | ~ spl15_73), inference(avatar_split_clause, [], [f222, f678, f710])).
fof(f222, plain, (~ (e3 = op(e2, op(e2, e3))) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f708, plain, (~ spl15_77 | spl15_23), inference(avatar_split_clause, [], [f211, f417, f700])).
fof(f211, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f704, plain, (~ spl15_77 | ~ spl15_74), inference(avatar_split_clause, [], [f215, f683, f700])).
fof(f215, plain, (~ (e2 = op(e2, op(e2, e2))) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f698, plain, (~ spl15_72 | spl15_19), inference(avatar_split_clause, [], [f205, f400, f674])).
fof(f205, plain, ((e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e3 = op(e2, op(e2, e3))) & ~ (e2 = op(e2, op(e2, e2))) & ~ (e1 = op(e2, op(e2, e1))) & ~ (e0 = op(e2, op(e2, e0))) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f697, plain, (~ spl15_72 | spl15_7), inference(avatar_split_clause, [], [f206, f349, f674])).
fof(f206, plain, ((e2 = op(e3, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f696, plain, (~ spl15_72 | ~ spl15_76), inference(avatar_split_clause, [], [f207, f693, f674])).
fof(f207, plain, (~ (e0 = op(e2, op(e2, e0))) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f691, plain, (~ spl15_72 | ~ spl15_75), inference(avatar_split_clause, [], [f208, f688, f674])).
fof(f208, plain, (~ (e1 = op(e2, op(e2, e1))) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f686, plain, (~ spl15_72 | ~ spl15_74), inference(avatar_split_clause, [], [f209, f683, f674])).
fof(f209, plain, (~ (e2 = op(e2, op(e2, e2))) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f681, plain, (~ spl15_72 | ~ spl15_73), inference(avatar_split_clause, [], [f210, f678, f674])).
fof(f210, plain, (~ (e3 = op(e2, op(e2, e3))) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f672, plain, (~ spl15_71 | spl15_16), inference(avatar_split_clause, [], [f199, f387, f664])).
fof(f199, plain, ((e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f671, plain, (~ spl15_71 | spl15_52), inference(avatar_split_clause, [], [f200, f540, f664])).
fof(f200, plain, ((e3 = op(e0, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f670, plain, (~ spl15_71 | ~ spl15_69), inference(avatar_split_clause, [], [f201, f647, f664])).
fof(f201, plain, (~ (e0 = op(e3, op(e3, e0))) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f669, plain, (~ spl15_71 | ~ spl15_68), inference(avatar_split_clause, [], [f202, f642, f664])).
fof(f202, plain, (~ (e1 = op(e3, op(e3, e1))) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f668, plain, (~ spl15_71 | ~ spl15_67), inference(avatar_split_clause, [], [f203, f637, f664])).
fof(f203, plain, (~ (e2 = op(e3, op(e3, e2))) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f662, plain, (~ spl15_70 | spl15_12), inference(avatar_split_clause, [], [f193, f370, f654])).
fof(f193, plain, ((e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f661, plain, (~ spl15_70 | spl15_36), inference(avatar_split_clause, [], [f194, f472, f654])).
fof(f194, plain, ((e3 = op(e1, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f660, plain, (~ spl15_70 | ~ spl15_69), inference(avatar_split_clause, [], [f195, f647, f654])).
fof(f195, plain, (~ (e0 = op(e3, op(e3, e0))) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f659, plain, (~ spl15_70 | ~ spl15_68), inference(avatar_split_clause, [], [f196, f642, f654])).
fof(f196, plain, (~ (e1 = op(e3, op(e3, e1))) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f658, plain, (~ spl15_70 | ~ spl15_67), inference(avatar_split_clause, [], [f197, f637, f654])).
fof(f197, plain, (~ (e2 = op(e3, op(e3, e2))) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f657, plain, (~ spl15_70 | ~ spl15_66), inference(avatar_split_clause, [], [f198, f632, f654])).
fof(f198, plain, (~ (e3 = op(e3, op(e3, e3))) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f652, plain, (~ spl15_65 | spl15_8), inference(avatar_split_clause, [], [f187, f353, f628])).
fof(f187, plain, ((e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e3 = op(e3, op(e3, e3))) & ~ (e2 = op(e3, op(e3, e2))) & ~ (e1 = op(e3, op(e3, e1))) & ~ (e0 = op(e3, op(e3, e0))) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f651, plain, (~ spl15_65 | spl15_20), inference(avatar_split_clause, [], [f188, f404, f628])).
fof(f188, plain, ((e3 = op(e2, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f650, plain, (~ spl15_65 | ~ spl15_69), inference(avatar_split_clause, [], [f189, f647, f628])).
fof(f189, plain, (~ (e0 = op(e3, op(e3, e0))) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f645, plain, (~ spl15_65 | ~ spl15_68), inference(avatar_split_clause, [], [f190, f642, f628])).
fof(f190, plain, (~ (e1 = op(e3, op(e3, e1))) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f640, plain, (~ spl15_65 | ~ spl15_67), inference(avatar_split_clause, [], [f191, f637, f628])).
fof(f191, plain, (~ (e2 = op(e3, op(e3, e2))) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f635, plain, (~ spl15_65 | ~ spl15_66), inference(avatar_split_clause, [], [f192, f632, f628])).
fof(f192, plain, (~ (e3 = op(e3, op(e3, e3))) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f626, plain, (spl15_61 | spl15_57 | spl15_53 | spl15_49), inference(avatar_split_clause, [], [f101, f528, f545, f562, f579])).
fof(f101, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax2)).
fof(f625, plain, (spl15_61 | spl15_45 | spl15_29 | spl15_13), inference(avatar_split_clause, [], [f102, f375, f443, f511, f579])).
fof(f102, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f624, plain, (spl15_62 | spl15_58 | spl15_54 | spl15_50), inference(avatar_split_clause, [], [f103, f532, f549, f566, f583])).
fof(f103, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f623, plain, (spl15_62 | spl15_46 | spl15_30 | spl15_14), inference(avatar_split_clause, [], [f104, f379, f447, f515, f583])).
fof(f104, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f622, plain, (spl15_63 | spl15_59 | spl15_55 | spl15_51), inference(avatar_split_clause, [], [f105, f536, f553, f570, f587])).
fof(f105, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f621, plain, (spl15_63 | spl15_47 | spl15_31 | spl15_15), inference(avatar_split_clause, [], [f106, f383, f451, f519, f587])).
fof(f106, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f620, plain, (spl15_64 | spl15_60 | spl15_56 | spl15_52), inference(avatar_split_clause, [], [f107, f540, f557, f574, f591])).
fof(f107, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f619, plain, (spl15_64 | spl15_48 | spl15_32 | spl15_16), inference(avatar_split_clause, [], [f108, f387, f455, f523, f591])).
fof(f108, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f618, plain, (spl15_45 | spl15_41 | spl15_37 | spl15_33), inference(avatar_split_clause, [], [f109, f460, f477, f494, f511])).
fof(f109, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f617, plain, (spl15_57 | spl15_41 | spl15_25 | spl15_9), inference(avatar_split_clause, [], [f110, f358, f426, f494, f562])).
fof(f110, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f616, plain, (spl15_46 | spl15_42 | spl15_38 | spl15_34), inference(avatar_split_clause, [], [f111, f464, f481, f498, f515])).
fof(f111, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f614, plain, (spl15_47 | spl15_43 | spl15_39 | spl15_35), inference(avatar_split_clause, [], [f113, f468, f485, f502, f519])).
fof(f113, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f613, plain, (spl15_59 | spl15_43 | spl15_27 | spl15_11), inference(avatar_split_clause, [], [f114, f366, f434, f502, f570])).
fof(f114, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f612, plain, (spl15_48 | spl15_44 | spl15_40 | spl15_36), inference(avatar_split_clause, [], [f115, f472, f489, f506, f523])).
fof(f115, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f611, plain, (spl15_60 | spl15_44 | spl15_28 | spl15_12), inference(avatar_split_clause, [], [f116, f370, f438, f506, f574])).
fof(f116, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f610, plain, (spl15_29 | spl15_25 | spl15_21 | spl15_17), inference(avatar_split_clause, [], [f117, f392, f409, f426, f443])).
fof(f117, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f609, plain, (spl15_53 | spl15_37 | spl15_21 | spl15_5), inference(avatar_split_clause, [], [f118, f341, f409, f477, f545])).
fof(f118, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f608, plain, (spl15_30 | spl15_26 | spl15_22 | spl15_18), inference(avatar_split_clause, [], [f119, f396, f413, f430, f447])).
fof(f119, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f606, plain, (spl15_31 | spl15_27 | spl15_23 | spl15_19), inference(avatar_split_clause, [], [f121, f400, f417, f434, f451])).
fof(f121, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f605, plain, (spl15_55 | spl15_39 | spl15_23 | spl15_7), inference(avatar_split_clause, [], [f122, f349, f417, f485, f553])).
fof(f122, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f604, plain, (spl15_32 | spl15_28 | spl15_24 | spl15_20), inference(avatar_split_clause, [], [f123, f404, f421, f438, f455])).
fof(f123, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f603, plain, (spl15_56 | spl15_40 | spl15_24 | spl15_8), inference(avatar_split_clause, [], [f124, f353, f421, f489, f557])).
fof(f124, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f602, plain, (spl15_13 | spl15_9 | spl15_5 | spl15_1), inference(avatar_split_clause, [], [f125, f324, f341, f358, f375])).
fof(f125, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f601, plain, (spl15_49 | spl15_33 | spl15_17 | spl15_1), inference(avatar_split_clause, [], [f126, f324, f392, f460, f528])).
fof(f126, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f600, plain, (spl15_14 | spl15_10 | spl15_6 | spl15_2), inference(avatar_split_clause, [], [f127, f328, f345, f362, f379])).
fof(f127, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f599, plain, (spl15_50 | spl15_34 | spl15_18 | spl15_2), inference(avatar_split_clause, [], [f128, f328, f396, f464, f532])).
fof(f128, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f598, plain, (spl15_15 | spl15_11 | spl15_7 | spl15_3), inference(avatar_split_clause, [], [f129, f332, f349, f366, f383])).
fof(f129, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f597, plain, (spl15_51 | spl15_35 | spl15_19 | spl15_3), inference(avatar_split_clause, [], [f130, f332, f400, f468, f536])).
fof(f130, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f596, plain, (spl15_16 | spl15_12 | spl15_8 | spl15_4), inference(avatar_split_clause, [], [f131, f336, f353, f370, f387])).
fof(f131, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f595, plain, (spl15_52 | spl15_36 | spl15_20 | spl15_4), inference(avatar_split_clause, [], [f132, f336, f404, f472, f540])).
fof(f132, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f594, plain, (spl15_61 | spl15_62 | spl15_63 | spl15_64), inference(avatar_split_clause, [], [f85, f591, f587, f583, f579])).
fof(f85, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG145+1.p', ax1)).
fof(f577, plain, (spl15_57 | spl15_58 | spl15_59 | spl15_60), inference(avatar_split_clause, [], [f86, f574, f570, f566, f562])).
fof(f86, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f560, plain, (spl15_53 | spl15_54 | spl15_55 | spl15_56), inference(avatar_split_clause, [], [f87, f557, f553, f549, f545])).
fof(f87, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f543, plain, (spl15_49 | spl15_50 | spl15_51 | spl15_52), inference(avatar_split_clause, [], [f88, f540, f536, f532, f528])).
fof(f88, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f526, plain, (spl15_45 | spl15_46 | spl15_47 | spl15_48), inference(avatar_split_clause, [], [f89, f523, f519, f515, f511])).
fof(f89, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f509, plain, (spl15_41 | spl15_42 | spl15_43 | spl15_44), inference(avatar_split_clause, [], [f90, f506, f502, f498, f494])).
fof(f90, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f492, plain, (spl15_37 | spl15_38 | spl15_39 | spl15_40), inference(avatar_split_clause, [], [f91, f489, f485, f481, f477])).
fof(f91, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f475, plain, (spl15_33 | spl15_34 | spl15_35 | spl15_36), inference(avatar_split_clause, [], [f92, f472, f468, f464, f460])).
fof(f92, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f458, plain, (spl15_29 | spl15_30 | spl15_31 | spl15_32), inference(avatar_split_clause, [], [f93, f455, f451, f447, f443])).
fof(f93, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f441, plain, (spl15_25 | spl15_26 | spl15_27 | spl15_28), inference(avatar_split_clause, [], [f94, f438, f434, f430, f426])).
fof(f94, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f424, plain, (spl15_21 | spl15_22 | spl15_23 | spl15_24), inference(avatar_split_clause, [], [f95, f421, f417, f413, f409])).
fof(f95, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f407, plain, (spl15_17 | spl15_18 | spl15_19 | spl15_20), inference(avatar_split_clause, [], [f96, f404, f400, f396, f392])).
fof(f96, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f390, plain, (spl15_13 | spl15_14 | spl15_15 | spl15_16), inference(avatar_split_clause, [], [f97, f387, f383, f379, f375])).
fof(f97, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f373, plain, (spl15_9 | spl15_10 | spl15_11 | spl15_12), inference(avatar_split_clause, [], [f98, f370, f366, f362, f358])).
fof(f98, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f356, plain, (spl15_5 | spl15_6 | spl15_7 | spl15_8), inference(avatar_split_clause, [], [f99, f353, f349, f345, f341])).
fof(f99, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f339, plain, (spl15_1 | spl15_2 | spl15_3 | spl15_4), inference(avatar_split_clause, [], [f100, f336, f332, f328, f324])).
fof(f100, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).