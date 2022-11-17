fof(f3609, plain, $false, inference(avatar_sat_refutation, [], [f313, f330, f347, f364, f381, f398, f415, f432, f449, f466, f483, f500, f517, f534, f551, f568, f569, f570, f571, f572, f573, f574, f575, f576, f577, f578, f579, f580, f581, f582, f583, f584, f585, f586, f587, f588, f590, f591, f592, f593, f594, f595, f596, f597, f598, f599, f600, f605, f606, f607, f608, f609, f610, f615, f616, f617, f618, f619, f620, f625, f626, f627, f628, f629, f630, f635, f636, f637, f642, f643, f644, f649, f650, f651, f656, f657, f658, f664, f665, f670, f671, f672, f677, f678, f679, f684, f685, f686, f691, f692, f693, f699, f700, f705, f706, f707, f712, f713, f714, f719, f720, f721, f726, f727, f728, f734, f735, f736, f737, f738, f739, f740, f741, f743, f744, f753, f762, f767, f772, f773, f774, f783, f792, f797, f802, f803, f804, f813, f822, f827, f832, f833, f834, f843, f852, f857, f862, f863, f864, f874, f877, f890, f892, f900, f904, f912, f942, f949, f953, f977, f987, f996, f1000, f1012, f1020, f1035, f1073, f1081, f1083, f1094, f1097, f1112, f1119, f1126, f1131, f1136, f1144, f1150, f1152, f1180, f1194, f1201, f1216, f1221, f1224, f1247, f1254, f1261, f1264, f1284, f1289, f1308, f1322, f1338, f1352, f1354, f1362, f1403, f1409, f1429, f1432, f1440, f1484, f1503, f1511, f1543, f1544, f1551, f1563, f1578, f1582, f1600, f1605, f1616, f1621, f1637, f1654, f1657, f1662, f1673, f1683, f1704, f1724, f1742, f1743, f1750, f1753, f1757, f1761, f1781, f1784, f1814, f1818, f1836, f1841, f1854, f1869, f1875, f1880, f1881, f1882, f1891, f1907, f1910, f1922, f1938, f1961, f1967, f1969, f2013, f2019, f2032, f2036, f2045, f2065, f2071, f2074, f2083, f2091, f2139, f2144, f2172, f2190, f2197, f2204, f2231, f2255, f2261, f2267, f2279, f2294, f2298, f2316, f2320, f2335, f2346, f2352, f2361, f2385, f2391, f2400, f2401, f2410, f2422, f2430, f2434, f2443, f2483, f2494, f2501, f2518, f2543, f2562, f2570, f2598, f2641, f2647, f2648, f2657, f2671, f2684, f2691, f2699, f2701, f2728, f2731, f2739, f2740, f2748, f2759, f2761, f2792, f2798, f2802, f2809, f2816, f2836, f2853, f2868, f2873, f2891, f2899, f2903, f2914, f2915, f2921, f2924, f2928, f2934, f2946, f2954, f2965, f2967, f2969, f2983, f3004, f3005, f3010, f3013, f3022, f3024, f3030, f3038, f3048, f3050, f3053, f3054, f3057, f3062, f3064, f3070, f3077, f3080, f3086, f3091, f3092, f3101, f3118, f3119, f3127, f3131, f3138, f3144, f3155, f3166, f3176, f3178, f3180, f3181, f3202, f3213, f3214, f3227, f3234, f3242, f3253, f3257, f3258, f3266, f3274, f3279, f3286, f3304, f3306, f3308, f3310, f3320, f3321, f3329, f3332, f3336, f3337, f3363, f3372, f3376, f3378, f3380, f3395, f3406, f3407, f3412, f3418, f3425, f3426, f3431, f3441, f3450, f3453, f3457, f3470, f3476, f3490, f3491, f3492, f3493, f3502, f3509, f3510, f3515, f3522, f3523, f3534, f3546, f3549, f3557, f3565, f3576, f3581, f3592])).
fof(f3592, plain, (~ spl18_18 | ~ spl18_24 | ~ spl18_37 | spl18_99), inference(avatar_contradiction_clause, [], [f3591])).
fof(f3591, plain, ($false | (~ spl18_18 | ~ spl18_24 | ~ spl18_37 | spl18_99)), inference(subsumption_resolution, [], [f3590, f453])).
fof(f453, plain, ((e0 = op(e1, e2)) | ~ spl18_37), inference(avatar_component_clause, [], [f451])).
fof(f451, plain, (spl18_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_37])])).
fof(f3590, plain, (~ (e0 = op(e1, e2)) | (~ spl18_18 | ~ spl18_24 | spl18_99)), inference(forward_demodulation, [], [f3589, f372])).
fof(f372, plain, ((e1 = op(e2, e3)) | ~ spl18_18), inference(avatar_component_clause, [], [f370])).
fof(f370, plain, (spl18_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_18])])).
fof(f3589, plain, (~ (e0 = op(op(e2, e3), e2)) | (~ spl18_24 | spl18_99)), inference(forward_demodulation, [], [f826, f397])).
fof(f397, plain, ((e3 = op(e2, e2)) | ~ spl18_24), inference(avatar_component_clause, [], [f395])).
fof(f395, plain, (spl18_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_24])])).
fof(f826, plain, (~ (e0 = op(op(e2, op(e2, e2)), e2)) | spl18_99), inference(avatar_component_clause, [], [f824])).
fof(f824, plain, (spl18_99 <=> (e0 = op(op(e2, op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_99])])).
fof(f3581, plain, (~ spl18_3 | ~ spl18_7), inference(avatar_split_clause, [], [f3579, f323, f306])).
fof(f306, plain, (spl18_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_3])])).
fof(f323, plain, (spl18_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_7])])).
fof(f3579, plain, (~ (e2 = op(e3, e3)) | ~ spl18_7), inference(backward_demodulation, [], [f186, f325])).
fof(f325, plain, ((e2 = op(e3, e2)) | ~ spl18_7), inference(avatar_component_clause, [], [f323])).
fof(f186, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax3)).
fof(f3576, plain, (~ spl18_10 | ~ spl18_14), inference(avatar_split_clause, [], [f3571, f353, f336])).
fof(f336, plain, (spl18_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_10])])).
fof(f353, plain, (spl18_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_14])])).
fof(f3571, plain, (~ (e1 = op(e3, e1)) | ~ spl18_14), inference(backward_demodulation, [], [f181, f355])).
fof(f355, plain, ((e1 = op(e3, e0)) | ~ spl18_14), inference(avatar_component_clause, [], [f353])).
fof(f181, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3565, plain, (~ spl18_20 | ~ spl18_24), inference(avatar_split_clause, [], [f3563, f395, f378])).
fof(f378, plain, (spl18_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_20])])).
fof(f3563, plain, (~ (e3 = op(e2, e3)) | ~ spl18_24), inference(backward_demodulation, [], [f180, f397])).
fof(f180, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3557, plain, (~ spl18_25 | ~ spl18_29), inference(avatar_split_clause, [], [f3551, f417, f400])).
fof(f400, plain, (spl18_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_25])])).
fof(f417, plain, (spl18_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_29])])).
fof(f3551, plain, (~ (e0 = op(e2, e1)) | ~ spl18_29), inference(backward_demodulation, [], [f175, f419])).
fof(f419, plain, ((e0 = op(e2, e0)) | ~ spl18_29), inference(avatar_component_clause, [], [f417])).
fof(f175, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3549, plain, (~ spl18_20 | ~ spl18_36), inference(avatar_split_clause, [], [f3547, f446, f378])).
fof(f446, plain, (spl18_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_36])])).
fof(f3547, plain, (~ (e3 = op(e2, e3)) | ~ spl18_36), inference(backward_demodulation, [], [f160, f448])).
fof(f448, plain, ((e3 = op(e1, e3)) | ~ spl18_36), inference(avatar_component_clause, [], [f446])).
fof(f160, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3546, plain, (~ spl18_33 | ~ spl18_37), inference(avatar_split_clause, [], [f3541, f451, f434])).
fof(f434, plain, (spl18_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_33])])).
fof(f3541, plain, (~ (e0 = op(e1, e3)) | ~ spl18_37), inference(backward_demodulation, [], [f174, f453])).
fof(f174, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3534, plain, (~ spl18_10 | ~ spl18_42), inference(avatar_split_clause, [], [f3525, f472, f336])).
fof(f472, plain, (spl18_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_42])])).
fof(f3525, plain, (~ (e1 = op(e3, e1)) | ~ spl18_42), inference(backward_demodulation, [], [f149, f474])).
fof(f474, plain, ((e1 = op(e1, e1)) | ~ spl18_42), inference(avatar_component_clause, [], [f472])).
fof(f149, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3523, plain, (~ spl18_43 | ~ spl18_47), inference(avatar_split_clause, [], [f3518, f493, f476])).
fof(f476, plain, (spl18_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_43])])).
fof(f493, plain, (spl18_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_47])])).
fof(f3518, plain, (~ (e2 = op(e1, e1)) | ~ spl18_47), inference(backward_demodulation, [], [f169, f495])).
fof(f495, plain, ((e2 = op(e1, e0)) | ~ spl18_47), inference(avatar_component_clause, [], [f493])).
fof(f169, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3522, plain, (~ spl18_31 | ~ spl18_47), inference(avatar_split_clause, [], [f3516, f493, f425])).
fof(f425, plain, (spl18_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_31])])).
fof(f3516, plain, (~ (e2 = op(e2, e0)) | ~ spl18_47), inference(backward_demodulation, [], [f142, f495])).
fof(f142, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3515, plain, (~ spl18_3 | ~ spl18_51), inference(avatar_split_clause, [], [f3513, f510, f306])).
fof(f510, plain, (spl18_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f3513, plain, (~ (e2 = op(e3, e3)) | ~ spl18_51), inference(backward_demodulation, [], [f159, f512])).
fof(f512, plain, ((e2 = op(e0, e3)) | ~ spl18_51), inference(avatar_component_clause, [], [f510])).
fof(f159, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3510, plain, (~ spl18_50 | ~ spl18_54), inference(avatar_split_clause, [], [f3506, f523, f506])).
fof(f506, plain, (spl18_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_50])])).
fof(f523, plain, (spl18_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_54])])).
fof(f3506, plain, (~ (e1 = op(e0, e3)) | ~ spl18_54), inference(backward_demodulation, [], [f168, f525])).
fof(f525, plain, ((e1 = op(e0, e2)) | ~ spl18_54), inference(avatar_component_clause, [], [f523])).
fof(f168, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3509, plain, (~ spl18_22 | ~ spl18_54), inference(avatar_split_clause, [], [f3504, f523, f387])).
fof(f387, plain, (spl18_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_22])])).
fof(f3504, plain, (~ (e1 = op(e2, e2)) | ~ spl18_54), inference(backward_demodulation, [], [f152, f525])).
fof(f152, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3502, plain, (~ spl18_25 | ~ spl18_57), inference(avatar_split_clause, [], [f3495, f536, f400])).
fof(f536, plain, (spl18_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_57])])).
fof(f3495, plain, (~ (e0 = op(e2, e1)) | ~ spl18_57), inference(backward_demodulation, [], [f146, f538])).
fof(f538, plain, ((e0 = op(e0, e1)) | ~ spl18_57), inference(avatar_component_clause, [], [f536])).
fof(f146, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3493, plain, (~ spl18_50 | ~ spl18_64 | spl18_88), inference(avatar_split_clause, [], [f3489, f769, f565, f506])).
fof(f565, plain, (spl18_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl18_64])])).
fof(f769, plain, (spl18_88 <=> (e1 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_88])])).
fof(f3489, plain, (~ (e1 = op(e0, e3)) | (~ spl18_64 | spl18_88)), inference(backward_demodulation, [], [f771, f567])).
fof(f567, plain, ((op(e0, e0) = e3) | ~ spl18_64), inference(avatar_component_clause, [], [f565])).
fof(f771, plain, (~ (e1 = op(e0, op(e0, e0))) | spl18_88), inference(avatar_component_clause, [], [f769])).
fof(f3492, plain, (spl18_51 | ~ spl18_64 | ~ spl18_86), inference(avatar_split_clause, [], [f3488, f759, f565, f510])).
fof(f759, plain, (spl18_86 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_86])])).
fof(f3488, plain, ((e2 = op(e0, e3)) | (~ spl18_64 | ~ spl18_86)), inference(backward_demodulation, [], [f760, f567])).
fof(f760, plain, ((e2 = op(e0, op(e0, e0))) | ~ spl18_86), inference(avatar_component_clause, [], [f759])).
fof(f3491, plain, (~ spl18_60 | ~ spl18_64), inference(avatar_split_clause, [], [f3485, f565, f548])).
fof(f548, plain, (spl18_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_60])])).
fof(f3485, plain, (~ (e3 = op(e0, e1)) | ~ spl18_64), inference(backward_demodulation, [], [f163, f567])).
fof(f163, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f3490, plain, (~ spl18_16 | ~ spl18_64), inference(avatar_split_clause, [], [f3484, f565, f361])).
fof(f361, plain, (spl18_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f3484, plain, (~ (e3 = op(e3, e0)) | ~ spl18_64), inference(backward_demodulation, [], [f141, f567])).
fof(f141, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3476, plain, (~ spl18_51 | spl18_103 | ~ spl18_106), inference(avatar_split_clause, [], [f3475, f859, f845, f510])).
fof(f845, plain, (spl18_103 <=> (e2 = op(op(e3, op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_103])])).
fof(f859, plain, (spl18_106 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_106])])).
fof(f3475, plain, (~ (e2 = op(e0, e3)) | (spl18_103 | ~ spl18_106)), inference(forward_demodulation, [], [f847, f860])).
fof(f860, plain, ((e0 = op(e3, op(e3, e3))) | ~ spl18_106), inference(avatar_component_clause, [], [f859])).
fof(f847, plain, (~ (e2 = op(op(e3, op(e3, e3)), e3)) | spl18_103), inference(avatar_component_clause, [], [f845])).
fof(f3470, plain, (~ spl18_54 | spl18_95 | ~ spl18_100), inference(avatar_split_clause, [], [f3469, f829, f806, f523])).
fof(f806, plain, (spl18_95 <=> (e1 = op(op(e2, op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_95])])).
fof(f829, plain, (spl18_100 <=> (e0 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl18_100])])).
fof(f3469, plain, (~ (e1 = op(e0, e2)) | (spl18_95 | ~ spl18_100)), inference(forward_demodulation, [], [f808, f830])).
fof(f830, plain, ((e0 = op(e2, op(e2, e2))) | ~ spl18_100), inference(avatar_component_clause, [], [f829])).
fof(f808, plain, (~ (e1 = op(op(e2, op(e2, e2)), e2)) | spl18_95), inference(avatar_component_clause, [], [f806])).
fof(f3457, plain, (~ spl18_4 | ~ spl18_16), inference(avatar_split_clause, [], [f3456, f361, f310])).
fof(f310, plain, (spl18_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_4])])).
fof(f3456, plain, (~ (e3 = op(e3, e3)) | ~ spl18_16), inference(backward_demodulation, [], [f183, f363])).
fof(f363, plain, ((e3 = op(e3, e0)) | ~ spl18_16), inference(avatar_component_clause, [], [f361])).
fof(f183, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3453, plain, (~ spl18_4 | ~ spl18_20), inference(avatar_split_clause, [], [f3452, f378, f310])).
fof(f3452, plain, (~ (e3 = op(e3, e3)) | ~ spl18_20), inference(backward_demodulation, [], [f162, f380])).
fof(f380, plain, ((e3 = op(e2, e3)) | ~ spl18_20), inference(avatar_component_clause, [], [f378])).
fof(f162, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3450, plain, (~ spl18_9 | ~ spl18_25), inference(avatar_split_clause, [], [f3442, f400, f332])).
fof(f332, plain, (spl18_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_9])])).
fof(f3442, plain, (~ (e0 = op(e3, e1)) | ~ spl18_25), inference(backward_demodulation, [], [f150, f402])).
fof(f402, plain, ((e0 = op(e2, e1)) | ~ spl18_25), inference(avatar_component_clause, [], [f400])).
fof(f150, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3441, plain, (~ spl18_19 | ~ spl18_31), inference(avatar_split_clause, [], [f3439, f425, f374])).
fof(f374, plain, (spl18_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_19])])).
fof(f3439, plain, (~ (e2 = op(e2, e3)) | ~ spl18_31), inference(backward_demodulation, [], [f177, f427])).
fof(f427, plain, ((e2 = op(e2, e0)) | ~ spl18_31), inference(avatar_component_clause, [], [f425])).
fof(f177, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3431, plain, (~ spl18_9 | ~ spl18_40 | ~ spl18_43 | spl18_93), inference(avatar_split_clause, [], [f3429, f794, f476, f463, f332])).
fof(f463, plain, (spl18_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_40])])).
fof(f794, plain, (spl18_93 <=> (e0 = op(op(e1, op(e1, e1)), e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_93])])).
fof(f3429, plain, (~ (e0 = op(e3, e1)) | (~ spl18_40 | ~ spl18_43 | spl18_93)), inference(backward_demodulation, [], [f3190, f465])).
fof(f465, plain, ((e3 = op(e1, e2)) | ~ spl18_40), inference(avatar_component_clause, [], [f463])).
fof(f3190, plain, (~ (e0 = op(op(e1, e2), e1)) | (~ spl18_43 | spl18_93)), inference(forward_demodulation, [], [f796, f478])).
fof(f478, plain, ((e2 = op(e1, e1)) | ~ spl18_43), inference(avatar_component_clause, [], [f476])).
fof(f796, plain, (~ (e0 = op(op(e1, op(e1, e1)), e1)) | spl18_93), inference(avatar_component_clause, [], [f794])).
fof(f3426, plain, (~ spl18_34 | ~ spl18_46), inference(avatar_split_clause, [], [f3422, f489, f438])).
fof(f438, plain, (spl18_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_34])])).
fof(f489, plain, (spl18_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_46])])).
fof(f3422, plain, (~ (e1 = op(e1, e3)) | ~ spl18_46), inference(backward_demodulation, [], [f171, f491])).
fof(f491, plain, ((e1 = op(e1, e0)) | ~ spl18_46), inference(avatar_component_clause, [], [f489])).
fof(f171, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3425, plain, (~ spl18_14 | ~ spl18_46), inference(avatar_split_clause, [], [f3420, f489, f353])).
fof(f3420, plain, (~ (e1 = op(e3, e0)) | ~ spl18_46), inference(backward_demodulation, [], [f143, f491])).
fof(f143, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3418, plain, (~ spl18_34 | ~ spl18_50), inference(avatar_split_clause, [], [f3413, f506, f438])).
fof(f3413, plain, (~ (e1 = op(e1, e3)) | ~ spl18_50), inference(backward_demodulation, [], [f157, f508])).
fof(f508, plain, ((e1 = op(e0, e3)) | ~ spl18_50), inference(avatar_component_clause, [], [f506])).
fof(f157, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3412, plain, (~ spl18_7 | ~ spl18_55), inference(avatar_split_clause, [], [f3409, f527, f323])).
fof(f527, plain, (spl18_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f3409, plain, (~ (e2 = op(e3, e2)) | ~ spl18_55), inference(backward_demodulation, [], [f153, f529])).
fof(f529, plain, ((e2 = op(e0, e2)) | ~ spl18_55), inference(avatar_component_clause, [], [f527])).
fof(f153, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3407, plain, (~ spl18_56 | ~ spl18_60), inference(avatar_split_clause, [], [f3404, f548, f531])).
fof(f531, plain, (spl18_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f3404, plain, (~ (e3 = op(e0, e2)) | ~ spl18_60), inference(backward_demodulation, [], [f166, f550])).
fof(f550, plain, ((e3 = op(e0, e1)) | ~ spl18_60), inference(avatar_component_clause, [], [f548])).
fof(f166, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3406, plain, (~ spl18_28 | ~ spl18_60), inference(avatar_split_clause, [], [f3402, f548, f412])).
fof(f412, plain, (spl18_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_28])])).
fof(f3402, plain, (~ (e3 = op(e2, e1)) | ~ spl18_60), inference(backward_demodulation, [], [f146, f550])).
fof(f3395, plain, (~ spl18_49 | ~ spl18_61), inference(avatar_split_clause, [], [f3386, f553, f502])).
fof(f502, plain, (spl18_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_49])])).
fof(f553, plain, (spl18_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_61])])).
fof(f3386, plain, (~ (e0 = op(e0, e3)) | ~ spl18_61), inference(backward_demodulation, [], [f165, f555])).
fof(f555, plain, ((e0 = op(e0, e0)) | ~ spl18_61), inference(avatar_component_clause, [], [f553])).
fof(f165, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3380, plain, (spl18_40 | ~ spl18_43 | ~ spl18_90), inference(avatar_split_clause, [], [f3379, f780, f476, f463])).
fof(f780, plain, (spl18_90 <=> (e3 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl18_90])])).
fof(f3379, plain, ((e3 = op(e1, e2)) | (~ spl18_43 | ~ spl18_90)), inference(forward_demodulation, [], [f781, f478])).
fof(f781, plain, ((e3 = op(e1, op(e1, e1))) | ~ spl18_90), inference(avatar_component_clause, [], [f780])).
fof(f3378, plain, (~ spl18_37 | ~ spl18_43 | spl18_94), inference(avatar_split_clause, [], [f3377, f799, f476, f451])).
fof(f799, plain, (spl18_94 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl18_94])])).
fof(f3377, plain, (~ (e0 = op(e1, e2)) | (~ spl18_43 | spl18_94)), inference(forward_demodulation, [], [f801, f478])).
fof(f801, plain, (~ (e0 = op(e1, op(e1, e1))) | spl18_94), inference(avatar_component_clause, [], [f799])).
fof(f3376, plain, (~ spl18_28 | ~ spl18_22 | spl18_96), inference(avatar_split_clause, [], [f3375, f810, f387, f412])).
fof(f810, plain, (spl18_96 <=> (e3 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl18_96])])).
fof(f3375, plain, (~ (e3 = op(e2, e1)) | (~ spl18_22 | spl18_96)), inference(forward_demodulation, [], [f812, f389])).
fof(f389, plain, ((e1 = op(e2, e2)) | ~ spl18_22), inference(avatar_component_clause, [], [f387])).
fof(f812, plain, (~ (e3 = op(e2, op(e2, e2))) | spl18_96), inference(avatar_component_clause, [], [f810])).
fof(f3372, plain, (~ spl18_50 | spl18_101 | ~ spl18_106), inference(avatar_split_clause, [], [f3371, f859, f836, f506])).
fof(f836, plain, (spl18_101 <=> (e1 = op(op(e3, op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_101])])).
fof(f3371, plain, (~ (e1 = op(e0, e3)) | (spl18_101 | ~ spl18_106)), inference(forward_demodulation, [], [f838, f860])).
fof(f838, plain, (~ (e1 = op(op(e3, op(e3, e3)), e3)) | spl18_101), inference(avatar_component_clause, [], [f836])).
fof(f3363, plain, (~ spl18_5 | ~ spl18_9), inference(avatar_split_clause, [], [f3360, f332, f315])).
fof(f315, plain, (spl18_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_5])])).
fof(f3360, plain, (~ (e0 = op(e3, e2)) | ~ spl18_9), inference(backward_demodulation, [], [f184, f334])).
fof(f334, plain, ((e0 = op(e3, e1)) | ~ spl18_9), inference(avatar_component_clause, [], [f332])).
fof(f184, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3337, plain, (~ spl18_40 | ~ spl18_48), inference(avatar_split_clause, [], [f3335, f497, f463])).
fof(f497, plain, (spl18_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_48])])).
fof(f3335, plain, (~ (e3 = op(e1, e2)) | ~ spl18_48), inference(backward_demodulation, [], [f170, f499])).
fof(f499, plain, ((e3 = op(e1, e0)) | ~ spl18_48), inference(avatar_component_clause, [], [f497])).
fof(f170, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3336, plain, (~ spl18_32 | ~ spl18_48), inference(avatar_split_clause, [], [f3333, f497, f429])).
fof(f429, plain, (spl18_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_32])])).
fof(f3333, plain, (~ (e3 = op(e2, e0)) | ~ spl18_48), inference(backward_demodulation, [], [f142, f499])).
fof(f3332, plain, (~ spl18_40 | ~ spl18_56), inference(avatar_split_clause, [], [f3330, f531, f463])).
fof(f3330, plain, (~ (e3 = op(e1, e2)) | ~ spl18_56), inference(backward_demodulation, [], [f151, f533])).
fof(f533, plain, ((e3 = op(e0, e2)) | ~ spl18_56), inference(avatar_component_clause, [], [f531])).
fof(f151, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3329, plain, (~ spl18_10 | ~ spl18_58), inference(avatar_split_clause, [], [f3325, f540, f336])).
fof(f540, plain, (spl18_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_58])])).
fof(f3325, plain, (~ (e1 = op(e3, e1)) | ~ spl18_58), inference(backward_demodulation, [], [f147, f542])).
fof(f542, plain, ((e1 = op(e0, e1)) | ~ spl18_58), inference(avatar_component_clause, [], [f540])).
fof(f147, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3321, plain, (~ spl18_55 | ~ spl18_63), inference(avatar_split_clause, [], [f3315, f561, f527])).
fof(f561, plain, (spl18_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f3315, plain, (~ (e2 = op(e0, e2)) | ~ spl18_63), inference(backward_demodulation, [], [f164, f563])).
fof(f563, plain, ((op(e0, e0) = e2) | ~ spl18_63), inference(avatar_component_clause, [], [f561])).
fof(f164, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3320, plain, (~ spl18_15 | ~ spl18_63), inference(avatar_split_clause, [], [f3313, f561, f357])).
fof(f357, plain, (spl18_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f3313, plain, (~ (e2 = op(e3, e0)) | ~ spl18_63), inference(backward_demodulation, [], [f141, f563])).
fof(f3310, plain, (~ spl18_14 | ~ spl18_84 | spl18_87), inference(avatar_split_clause, [], [f3309, f764, f750, f353])).
fof(f750, plain, (spl18_84 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl18_84])])).
fof(f764, plain, (spl18_87 <=> (e1 = op(op(e0, op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_87])])).
fof(f3309, plain, (~ (e1 = op(e3, e0)) | (~ spl18_84 | spl18_87)), inference(forward_demodulation, [], [f766, f751])).
fof(f751, plain, ((e3 = op(e0, op(e0, e0))) | ~ spl18_84), inference(avatar_component_clause, [], [f750])).
fof(f766, plain, (~ (e1 = op(op(e0, op(e0, e0)), e0)) | spl18_87), inference(avatar_component_clause, [], [f764])).
fof(f3308, plain, (~ spl18_40 | ~ spl18_43 | spl18_90), inference(avatar_split_clause, [], [f3307, f780, f476, f463])).
fof(f3307, plain, (~ (e3 = op(e1, e2)) | (~ spl18_43 | spl18_90)), inference(forward_demodulation, [], [f782, f478])).
fof(f782, plain, (~ (e3 = op(e1, op(e1, e1))) | spl18_90), inference(avatar_component_clause, [], [f780])).
fof(f3306, plain, (spl18_37 | ~ spl18_43 | ~ spl18_94), inference(avatar_split_clause, [], [f3305, f799, f476, f451])).
fof(f3305, plain, ((e0 = op(e1, e2)) | (~ spl18_43 | ~ spl18_94)), inference(forward_demodulation, [], [f800, f478])).
fof(f800, plain, ((e0 = op(e1, op(e1, e1))) | ~ spl18_94), inference(avatar_component_clause, [], [f799])).
fof(f3304, plain, (~ spl18_25 | ~ spl18_22 | spl18_100), inference(avatar_split_clause, [], [f3303, f829, f387, f400])).
fof(f3303, plain, (~ (e0 = op(e2, e1)) | (~ spl18_22 | spl18_100)), inference(forward_demodulation, [], [f831, f389])).
fof(f831, plain, (~ (e0 = op(e2, op(e2, e2))) | spl18_100), inference(avatar_component_clause, [], [f829])).
fof(f3286, plain, (spl18_1 | ~ spl18_4 | ~ spl18_106), inference(avatar_contradiction_clause, [], [f3285])).
fof(f3285, plain, ($false | (spl18_1 | ~ spl18_4 | ~ spl18_106)), inference(subsumption_resolution, [], [f3284, f299])).
fof(f299, plain, (~ (e0 = op(e3, e3)) | spl18_1), inference(avatar_component_clause, [], [f298])).
fof(f298, plain, (spl18_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f3284, plain, ((e0 = op(e3, e3)) | (~ spl18_4 | ~ spl18_106)), inference(backward_demodulation, [], [f860, f312])).
fof(f312, plain, ((e3 = op(e3, e3)) | ~ spl18_4), inference(avatar_component_clause, [], [f310])).
fof(f3279, plain, (~ spl18_2 | ~ spl18_10), inference(avatar_split_clause, [], [f3276, f336, f302])).
fof(f302, plain, (spl18_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f3276, plain, (~ (e1 = op(e3, e3)) | ~ spl18_10), inference(backward_demodulation, [], [f185, f338])).
fof(f338, plain, ((e1 = op(e3, e1)) | ~ spl18_10), inference(avatar_component_clause, [], [f336])).
fof(f185, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3274, plain, (~ spl18_7 | ~ spl18_15), inference(avatar_split_clause, [], [f3271, f357, f323])).
fof(f3271, plain, (~ (e2 = op(e3, e2)) | ~ spl18_15), inference(backward_demodulation, [], [f182, f359])).
fof(f359, plain, ((e2 = op(e3, e0)) | ~ spl18_15), inference(avatar_component_clause, [], [f357])).
fof(f182, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3266, plain, (~ spl18_17 | ~ spl18_25), inference(avatar_split_clause, [], [f3260, f400, f366])).
fof(f366, plain, (spl18_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_17])])).
fof(f3260, plain, (~ (e0 = op(e2, e3)) | ~ spl18_25), inference(backward_demodulation, [], [f179, f402])).
fof(f179, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3258, plain, (~ spl18_28 | ~ spl18_32), inference(avatar_split_clause, [], [f3255, f429, f412])).
fof(f3255, plain, (~ (e3 = op(e2, e1)) | ~ spl18_32), inference(backward_demodulation, [], [f175, f431])).
fof(f431, plain, ((e3 = op(e2, e0)) | ~ spl18_32), inference(avatar_component_clause, [], [f429])).
fof(f3257, plain, (~ spl18_16 | ~ spl18_32), inference(avatar_split_clause, [], [f3254, f429, f361])).
fof(f3254, plain, (~ (e3 = op(e3, e0)) | ~ spl18_32), inference(backward_demodulation, [], [f144, f431])).
fof(f144, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3253, plain, (~ spl18_2 | ~ spl18_34), inference(avatar_split_clause, [], [f3250, f438, f302])).
fof(f3250, plain, (~ (e1 = op(e3, e3)) | ~ spl18_34), inference(backward_demodulation, [], [f161, f440])).
fof(f440, plain, ((e1 = op(e1, e3)) | ~ spl18_34), inference(avatar_component_clause, [], [f438])).
fof(f161, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3242, plain, (~ spl18_37 | ~ spl18_45), inference(avatar_split_clause, [], [f3237, f485, f451])).
fof(f485, plain, (spl18_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_45])])).
fof(f3237, plain, (~ (e0 = op(e1, e2)) | ~ spl18_45), inference(backward_demodulation, [], [f170, f487])).
fof(f487, plain, ((e0 = op(e1, e0)) | ~ spl18_45), inference(avatar_component_clause, [], [f485])).
fof(f3234, plain, (~ spl18_17 | ~ spl18_49), inference(avatar_split_clause, [], [f3229, f502, f366])).
fof(f3229, plain, (~ (e0 = op(e2, e3)) | ~ spl18_49), inference(backward_demodulation, [], [f158, f504])).
fof(f504, plain, ((e0 = op(e0, e3)) | ~ spl18_49), inference(avatar_component_clause, [], [f502])).
fof(f158, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3227, plain, (~ spl18_51 | ~ spl18_55), inference(avatar_split_clause, [], [f3224, f527, f510])).
fof(f3224, plain, (~ (e2 = op(e0, e3)) | ~ spl18_55), inference(backward_demodulation, [], [f168, f529])).
fof(f3214, plain, (~ spl18_58 | ~ spl18_62), inference(avatar_split_clause, [], [f3206, f557, f540])).
fof(f557, plain, (spl18_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl18_62])])).
fof(f3206, plain, (~ (e1 = op(e0, e1)) | ~ spl18_62), inference(backward_demodulation, [], [f163, f559])).
fof(f559, plain, ((op(e0, e0) = e1) | ~ spl18_62), inference(avatar_component_clause, [], [f557])).
fof(f3213, plain, (~ spl18_46 | ~ spl18_62), inference(avatar_split_clause, [], [f3203, f557, f489])).
fof(f3203, plain, (~ (e1 = op(e1, e0)) | ~ spl18_62), inference(backward_demodulation, [], [f139, f559])).
fof(f139, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f3202, plain, (~ spl18_15 | spl18_83 | ~ spl18_84), inference(avatar_split_clause, [], [f3201, f750, f746, f357])).
fof(f746, plain, (spl18_83 <=> (e2 = op(op(e0, op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_83])])).
fof(f3201, plain, (~ (e2 = op(e3, e0)) | (spl18_83 | ~ spl18_84)), inference(forward_demodulation, [], [f748, f751])).
fof(f748, plain, (~ (e2 = op(op(e0, op(e0, e0)), e0)) | spl18_83), inference(avatar_component_clause, [], [f746])).
fof(f3181, plain, (spl18_28 | ~ spl18_22 | ~ spl18_96), inference(avatar_split_clause, [], [f3161, f810, f387, f412])).
fof(f3161, plain, ((e3 = op(e2, e1)) | (~ spl18_22 | ~ spl18_96)), inference(backward_demodulation, [], [f811, f389])).
fof(f811, plain, ((e3 = op(e2, op(e2, e2))) | ~ spl18_96), inference(avatar_component_clause, [], [f810])).
fof(f3180, plain, (~ spl18_18 | ~ spl18_22), inference(avatar_split_clause, [], [f3179, f387, f370])).
fof(f3179, plain, (~ (e1 = op(e2, e3)) | ~ spl18_22), inference(forward_demodulation, [], [f180, f389])).
fof(f3178, plain, (~ spl18_5 | ~ spl18_96 | spl18_99), inference(avatar_split_clause, [], [f3103, f824, f810, f315])).
fof(f3103, plain, (~ (e0 = op(e3, e2)) | (~ spl18_96 | spl18_99)), inference(backward_demodulation, [], [f826, f811])).
fof(f3176, plain, (~ spl18_2 | ~ spl18_9 | ~ spl18_51 | spl18_103), inference(avatar_contradiction_clause, [], [f3175])).
fof(f3175, plain, ($false | (~ spl18_2 | ~ spl18_9 | ~ spl18_51 | spl18_103)), inference(subsumption_resolution, [], [f3174, f512])).
fof(f3174, plain, (~ (e2 = op(e0, e3)) | (~ spl18_2 | ~ spl18_9 | spl18_103)), inference(forward_demodulation, [], [f3173, f334])).
fof(f3173, plain, (~ (e2 = op(op(e3, e1), e3)) | (~ spl18_2 | spl18_103)), inference(forward_demodulation, [], [f847, f304])).
fof(f304, plain, ((e1 = op(e3, e3)) | ~ spl18_2), inference(avatar_component_clause, [], [f302])).
fof(f3166, plain, (~ spl18_8 | ~ spl18_16), inference(avatar_split_clause, [], [f3165, f361, f327])).
fof(f327, plain, (spl18_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_8])])).
fof(f3165, plain, (~ (e3 = op(e3, e2)) | ~ spl18_16), inference(backward_demodulation, [], [f182, f363])).
fof(f3155, plain, (~ spl18_23 | ~ spl18_31), inference(avatar_split_clause, [], [f3153, f425, f391])).
fof(f391, plain, (spl18_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_23])])).
fof(f3153, plain, (~ (e2 = op(e2, e2)) | ~ spl18_31), inference(backward_demodulation, [], [f176, f427])).
fof(f176, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3144, plain, (~ spl18_11 | ~ spl18_43), inference(avatar_split_clause, [], [f3140, f476, f340])).
fof(f340, plain, (spl18_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_11])])).
fof(f3140, plain, (~ (e2 = op(e3, e1)) | ~ spl18_43), inference(backward_demodulation, [], [f149, f478])).
fof(f3138, plain, (~ spl18_38 | ~ spl18_46), inference(avatar_split_clause, [], [f3135, f489, f455])).
fof(f455, plain, (spl18_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_38])])).
fof(f3135, plain, (~ (e1 = op(e1, e2)) | ~ spl18_46), inference(backward_demodulation, [], [f170, f491])).
fof(f3131, plain, (~ spl18_8 | ~ spl18_56), inference(avatar_split_clause, [], [f3130, f531, f327])).
fof(f3130, plain, (~ (e3 = op(e3, e2)) | ~ spl18_56), inference(backward_demodulation, [], [f153, f533])).
fof(f3127, plain, (~ spl18_26 | ~ spl18_58), inference(avatar_split_clause, [], [f3122, f540, f404])).
fof(f404, plain, (spl18_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_26])])).
fof(f3122, plain, (~ (e1 = op(e2, e1)) | ~ spl18_58), inference(backward_demodulation, [], [f146, f542])).
fof(f3119, plain, (~ spl18_53 | ~ spl18_61), inference(avatar_split_clause, [], [f3113, f553, f519])).
fof(f519, plain, (spl18_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_53])])).
fof(f3113, plain, (~ (e0 = op(e0, e2)) | ~ spl18_61), inference(backward_demodulation, [], [f164, f555])).
fof(f3118, plain, (~ spl18_13 | ~ spl18_61), inference(avatar_split_clause, [], [f3111, f553, f349])).
fof(f349, plain, (spl18_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_13])])).
fof(f3111, plain, (~ (e0 = op(e3, e0)) | ~ spl18_61), inference(backward_demodulation, [], [f141, f555])).
fof(f3101, plain, (~ spl18_63 | ~ spl18_51), inference(avatar_split_clause, [], [f3100, f510, f561])).
fof(f3100, plain, (~ (op(e0, e0) = e2) | ~ spl18_51), inference(forward_demodulation, [], [f165, f512])).
fof(f3092, plain, (~ spl18_9 | ~ spl18_2 | spl18_106), inference(avatar_split_clause, [], [f2961, f859, f302, f332])).
fof(f2961, plain, (~ (e0 = op(e3, e1)) | (~ spl18_2 | spl18_106)), inference(backward_demodulation, [], [f861, f304])).
fof(f861, plain, (~ (e0 = op(e3, op(e3, e3))) | spl18_106), inference(avatar_component_clause, [], [f859])).
fof(f3091, plain, (~ spl18_2 | ~ spl18_11 | ~ spl18_17 | spl18_105), inference(avatar_contradiction_clause, [], [f3090])).
fof(f3090, plain, ($false | (~ spl18_2 | ~ spl18_11 | ~ spl18_17 | spl18_105)), inference(subsumption_resolution, [], [f3085, f368])).
fof(f368, plain, ((e0 = op(e2, e3)) | ~ spl18_17), inference(avatar_component_clause, [], [f366])).
fof(f3085, plain, (~ (e0 = op(e2, e3)) | (~ spl18_2 | ~ spl18_11 | spl18_105)), inference(backward_demodulation, [], [f2960, f342])).
fof(f342, plain, ((e2 = op(e3, e1)) | ~ spl18_11), inference(avatar_component_clause, [], [f340])).
fof(f2960, plain, (~ (e0 = op(op(e3, e1), e3)) | (~ spl18_2 | spl18_105)), inference(backward_demodulation, [], [f856, f304])).
fof(f856, plain, (~ (e0 = op(op(e3, op(e3, e3)), e3)) | spl18_105), inference(avatar_component_clause, [], [f854])).
fof(f854, plain, (spl18_105 <=> (e0 = op(op(e3, op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_105])])).
fof(f3086, plain, (~ spl18_7 | ~ spl18_11), inference(avatar_split_clause, [], [f3082, f340, f323])).
fof(f3082, plain, (~ (e2 = op(e3, e2)) | ~ spl18_11), inference(backward_demodulation, [], [f184, f342])).
fof(f3080, plain, (~ spl18_22 | ~ spl18_23 | spl18_98), inference(avatar_split_clause, [], [f3074, f819, f391, f387])).
fof(f819, plain, (spl18_98 <=> (e1 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl18_98])])).
fof(f3074, plain, (~ (e1 = op(e2, e2)) | (~ spl18_23 | spl18_98)), inference(backward_demodulation, [], [f821, f393])).
fof(f393, plain, ((e2 = op(e2, e2)) | ~ spl18_23), inference(avatar_component_clause, [], [f391])).
fof(f821, plain, (~ (e1 = op(e2, op(e2, e2))) | spl18_98), inference(avatar_component_clause, [], [f819])).
fof(f3077, plain, (~ spl18_7 | ~ spl18_23), inference(avatar_split_clause, [], [f3071, f391, f323])).
fof(f3071, plain, (~ (e2 = op(e3, e2)) | ~ spl18_23), inference(backward_demodulation, [], [f156, f393])).
fof(f156, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3070, plain, (~ spl18_22 | ~ spl18_38), inference(avatar_split_clause, [], [f3065, f455, f387])).
fof(f3065, plain, (~ (e1 = op(e2, e2)) | ~ spl18_38), inference(backward_demodulation, [], [f154, f457])).
fof(f457, plain, ((e1 = op(e1, e2)) | ~ spl18_38), inference(avatar_component_clause, [], [f455])).
fof(f154, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3064, plain, (spl18_25 | ~ spl18_41 | ~ spl18_47 | ~ spl18_93), inference(avatar_contradiction_clause, [], [f3063])).
fof(f3063, plain, ($false | (spl18_25 | ~ spl18_41 | ~ spl18_47 | ~ spl18_93)), inference(subsumption_resolution, [], [f3061, f401])).
fof(f401, plain, (~ (e0 = op(e2, e1)) | spl18_25), inference(avatar_component_clause, [], [f400])).
fof(f3061, plain, ((e0 = op(e2, e1)) | (~ spl18_41 | ~ spl18_47 | ~ spl18_93)), inference(backward_demodulation, [], [f2999, f495])).
fof(f2999, plain, ((e0 = op(op(e1, e0), e1)) | (~ spl18_41 | ~ spl18_93)), inference(forward_demodulation, [], [f795, f470])).
fof(f470, plain, ((e0 = op(e1, e1)) | ~ spl18_41), inference(avatar_component_clause, [], [f468])).
fof(f468, plain, (spl18_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_41])])).
fof(f795, plain, ((e0 = op(op(e1, op(e1, e1)), e1)) | ~ spl18_93), inference(avatar_component_clause, [], [f794])).
fof(f3062, plain, (~ spl18_35 | ~ spl18_47), inference(avatar_split_clause, [], [f3059, f493, f442])).
fof(f442, plain, (spl18_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_35])])).
fof(f3059, plain, (~ (e2 = op(e1, e3)) | ~ spl18_47), inference(backward_demodulation, [], [f171, f495])).
fof(f3057, plain, (~ spl18_35 | ~ spl18_51), inference(avatar_split_clause, [], [f3055, f510, f442])).
fof(f3055, plain, (~ (e2 = op(e1, e3)) | ~ spl18_51), inference(backward_demodulation, [], [f157, f512])).
fof(f3054, plain, (~ spl18_52 | ~ spl18_60), inference(avatar_split_clause, [], [f3052, f548, f514])).
fof(f514, plain, (spl18_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl18_52])])).
fof(f3052, plain, (~ (e3 = op(e0, e3)) | ~ spl18_60), inference(backward_demodulation, [], [f167, f550])).
fof(f167, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3053, plain, (~ spl18_12 | ~ spl18_60), inference(avatar_split_clause, [], [f3051, f548, f344])).
fof(f344, plain, (spl18_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_12])])).
fof(f3051, plain, (~ (e3 = op(e3, e1)) | ~ spl18_60), inference(backward_demodulation, [], [f147, f550])).
fof(f3050, plain, (spl18_47 | ~ spl18_41 | ~ spl18_92), inference(avatar_split_clause, [], [f3049, f789, f468, f493])).
fof(f789, plain, (spl18_92 <=> (e2 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl18_92])])).
fof(f3049, plain, ((e2 = op(e1, e0)) | (~ spl18_41 | ~ spl18_92)), inference(forward_demodulation, [], [f790, f470])).
fof(f790, plain, ((e2 = op(e1, op(e1, e1))) | ~ spl18_92), inference(avatar_component_clause, [], [f789])).
fof(f3048, plain, (~ spl18_60 | ~ spl18_62 | spl18_84), inference(avatar_split_clause, [], [f2981, f750, f557, f548])).
fof(f2981, plain, (~ (e3 = op(e0, e1)) | (~ spl18_62 | spl18_84)), inference(forward_demodulation, [], [f752, f559])).
fof(f752, plain, (~ (e3 = op(e0, op(e0, e0))) | spl18_84), inference(avatar_component_clause, [], [f750])).
fof(f3038, plain, (~ spl18_48 | ~ spl18_41 | spl18_90), inference(avatar_split_clause, [], [f2911, f780, f468, f497])).
fof(f2911, plain, (~ (e3 = op(e1, e0)) | (~ spl18_41 | spl18_90)), inference(backward_demodulation, [], [f782, f470])).
fof(f3030, plain, (~ spl18_22 | ~ spl18_26), inference(avatar_split_clause, [], [f3029, f404, f387])).
fof(f3029, plain, (~ (e1 = op(e2, e2)) | ~ spl18_26), inference(forward_demodulation, [], [f178, f406])).
fof(f406, plain, ((e1 = op(e2, e1)) | ~ spl18_26), inference(avatar_component_clause, [], [f404])).
fof(f178, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3024, plain, (~ spl18_11 | ~ spl18_2 | spl18_102), inference(avatar_split_clause, [], [f2959, f840, f302, f340])).
fof(f840, plain, (spl18_102 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_102])])).
fof(f2959, plain, (~ (e2 = op(e3, e1)) | (~ spl18_2 | spl18_102)), inference(backward_demodulation, [], [f842, f304])).
fof(f842, plain, (~ (e2 = op(e3, op(e3, e3))) | spl18_102), inference(avatar_component_clause, [], [f840])).
fof(f3022, plain, (~ spl18_5 | ~ spl18_13), inference(avatar_split_clause, [], [f3018, f349, f315])).
fof(f3018, plain, (~ (e0 = op(e3, e2)) | ~ spl18_13), inference(backward_demodulation, [], [f182, f351])).
fof(f351, plain, ((e0 = op(e3, e0)) | ~ spl18_13), inference(avatar_component_clause, [], [f349])).
fof(f3013, plain, (~ spl18_31 | ~ spl18_32), inference(avatar_contradiction_clause, [], [f3012])).
fof(f3012, plain, ($false | (~ spl18_31 | ~ spl18_32)), inference(subsumption_resolution, [], [f3011, f192])).
fof(f192, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax4)).
fof(f3011, plain, ((e2 = e3) | (~ spl18_31 | ~ spl18_32)), inference(backward_demodulation, [], [f431, f427])).
fof(f3010, plain, (~ spl18_5 | ~ spl18_53), inference(avatar_split_clause, [], [f3006, f519, f315])).
fof(f3006, plain, (~ (e0 = op(e3, e2)) | ~ spl18_53), inference(backward_demodulation, [], [f153, f521])).
fof(f521, plain, ((e0 = op(e0, e2)) | ~ spl18_53), inference(avatar_component_clause, [], [f519])).
fof(f3005, plain, (~ spl18_55 | ~ spl18_59), inference(avatar_split_clause, [], [f3002, f544, f527])).
fof(f544, plain, (spl18_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_59])])).
fof(f3002, plain, (~ (e2 = op(e0, e2)) | ~ spl18_59), inference(backward_demodulation, [], [f166, f546])).
fof(f546, plain, ((e2 = op(e0, e1)) | ~ spl18_59), inference(avatar_component_clause, [], [f544])).
fof(f3004, plain, (~ spl18_27 | ~ spl18_59), inference(avatar_split_clause, [], [f3001, f544, f408])).
fof(f408, plain, (spl18_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_27])])).
fof(f3001, plain, (~ (e2 = op(e2, e1)) | ~ spl18_59), inference(backward_demodulation, [], [f146, f546])).
fof(f2983, plain, (~ spl18_59 | ~ spl18_62 | spl18_86), inference(avatar_split_clause, [], [f2982, f759, f557, f544])).
fof(f2982, plain, (~ (e2 = op(e0, e1)) | (~ spl18_62 | spl18_86)), inference(forward_demodulation, [], [f761, f559])).
fof(f761, plain, (~ (e2 = op(e0, op(e0, e0))) | spl18_86), inference(avatar_component_clause, [], [f759])).
fof(f2969, plain, (~ spl18_8 | ~ spl18_12), inference(avatar_split_clause, [], [f2968, f344, f327])).
fof(f2968, plain, (~ (e3 = op(e3, e2)) | ~ spl18_12), inference(forward_demodulation, [], [f184, f346])).
fof(f346, plain, ((e3 = op(e3, e1)) | ~ spl18_12), inference(avatar_component_clause, [], [f344])).
fof(f2967, plain, (~ spl18_6 | ~ spl18_22), inference(avatar_split_clause, [], [f2966, f387, f319])).
fof(f319, plain, (spl18_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_6])])).
fof(f2966, plain, (~ (e1 = op(e3, e2)) | ~ spl18_22), inference(forward_demodulation, [], [f156, f389])).
fof(f2965, plain, (~ spl18_8 | ~ spl18_40), inference(avatar_split_clause, [], [f2964, f463, f327])).
fof(f2964, plain, (~ (e3 = op(e3, e2)) | ~ spl18_40), inference(forward_demodulation, [], [f155, f465])).
fof(f155, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2954, plain, (~ spl18_3 | ~ spl18_15), inference(avatar_split_clause, [], [f2952, f357, f306])).
fof(f2952, plain, (~ (e2 = op(e3, e3)) | ~ spl18_15), inference(backward_demodulation, [], [f183, f359])).
fof(f2946, plain, (~ spl18_22 | spl18_26 | ~ spl18_98), inference(avatar_contradiction_clause, [], [f2945])).
fof(f2945, plain, ($false | (~ spl18_22 | spl18_26 | ~ spl18_98)), inference(subsumption_resolution, [], [f2942, f405])).
fof(f405, plain, (~ (e1 = op(e2, e1)) | spl18_26), inference(avatar_component_clause, [], [f404])).
fof(f2942, plain, ((e1 = op(e2, e1)) | (~ spl18_22 | ~ spl18_98)), inference(backward_demodulation, [], [f820, f389])).
fof(f820, plain, ((e1 = op(e2, op(e2, e2))) | ~ spl18_98), inference(avatar_component_clause, [], [f819])).
fof(f2934, plain, (~ spl18_24 | ~ spl18_32), inference(avatar_split_clause, [], [f2931, f429, f395])).
fof(f2931, plain, (~ (e3 = op(e2, e2)) | ~ spl18_32), inference(backward_demodulation, [], [f176, f431])).
fof(f2928, plain, (~ spl18_3 | ~ spl18_35), inference(avatar_split_clause, [], [f2926, f442, f306])).
fof(f2926, plain, (~ (e2 = op(e3, e3)) | ~ spl18_35), inference(backward_demodulation, [], [f161, f444])).
fof(f444, plain, ((e2 = op(e1, e3)) | ~ spl18_35), inference(avatar_component_clause, [], [f442])).
fof(f2924, plain, (~ spl18_24 | ~ spl18_40), inference(avatar_split_clause, [], [f2922, f463, f395])).
fof(f2922, plain, (~ (e3 = op(e2, e2)) | ~ spl18_40), inference(backward_demodulation, [], [f154, f465])).
fof(f2921, plain, (~ spl18_41 | ~ spl18_46 | spl18_93), inference(avatar_contradiction_clause, [], [f2920])).
fof(f2920, plain, ($false | (~ spl18_41 | ~ spl18_46 | spl18_93)), inference(subsumption_resolution, [], [f2919, f470])).
fof(f2919, plain, (~ (e0 = op(e1, e1)) | (~ spl18_41 | ~ spl18_46 | spl18_93)), inference(forward_demodulation, [], [f2913, f491])).
fof(f2913, plain, (~ (e0 = op(op(e1, e0), e1)) | (~ spl18_41 | spl18_93)), inference(backward_demodulation, [], [f796, f470])).
fof(f2915, plain, (~ spl18_33 | ~ spl18_41), inference(avatar_split_clause, [], [f2907, f468, f434])).
fof(f2907, plain, (~ (e0 = op(e1, e3)) | ~ spl18_41), inference(backward_demodulation, [], [f173, f470])).
fof(f173, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2914, plain, (~ spl18_25 | ~ spl18_41), inference(avatar_split_clause, [], [f2904, f468, f400])).
fof(f2904, plain, (~ (e0 = op(e2, e1)) | ~ spl18_41), inference(backward_demodulation, [], [f148, f470])).
fof(f148, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2903, plain, (~ spl18_39 | ~ spl18_55), inference(avatar_split_clause, [], [f2900, f527, f459])).
fof(f459, plain, (spl18_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_39])])).
fof(f2900, plain, (~ (e2 = op(e1, e2)) | ~ spl18_55), inference(backward_demodulation, [], [f151, f529])).
fof(f2899, plain, (~ spl18_54 | ~ spl18_58), inference(avatar_split_clause, [], [f2895, f540, f523])).
fof(f2895, plain, (~ (e1 = op(e0, e2)) | ~ spl18_58), inference(backward_demodulation, [], [f166, f542])).
fof(f2891, plain, (~ spl18_61 | ~ spl18_62), inference(avatar_contradiction_clause, [], [f2890])).
fof(f2890, plain, ($false | (~ spl18_61 | ~ spl18_62)), inference(subsumption_resolution, [], [f2889, f187])).
fof(f187, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f2889, plain, ((e0 = e1) | (~ spl18_61 | ~ spl18_62)), inference(forward_demodulation, [], [f559, f555])).
fof(f2873, plain, (~ spl18_33 | ~ spl18_44 | spl18_94), inference(avatar_contradiction_clause, [], [f2872])).
fof(f2872, plain, ($false | (~ spl18_33 | ~ spl18_44 | spl18_94)), inference(subsumption_resolution, [], [f2871, f436])).
fof(f436, plain, ((e0 = op(e1, e3)) | ~ spl18_33), inference(avatar_component_clause, [], [f434])).
fof(f2871, plain, (~ (e0 = op(e1, e3)) | (~ spl18_44 | spl18_94)), inference(forward_demodulation, [], [f801, f482])).
fof(f482, plain, ((e3 = op(e1, e1)) | ~ spl18_44), inference(avatar_component_clause, [], [f480])).
fof(f480, plain, (spl18_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_44])])).
fof(f2868, plain, (~ spl18_33 | ~ spl18_44 | ~ spl18_59 | spl18_89), inference(avatar_contradiction_clause, [], [f2867])).
fof(f2867, plain, ($false | (~ spl18_33 | ~ spl18_44 | ~ spl18_59 | spl18_89)), inference(subsumption_resolution, [], [f2866, f546])).
fof(f2866, plain, (~ (e2 = op(e0, e1)) | (~ spl18_33 | ~ spl18_44 | spl18_89)), inference(forward_demodulation, [], [f2865, f436])).
fof(f2865, plain, (~ (e2 = op(op(e1, e3), e1)) | (~ spl18_44 | spl18_89)), inference(forward_demodulation, [], [f778, f482])).
fof(f778, plain, (~ (e2 = op(op(e1, op(e1, e1)), e1)) | spl18_89), inference(avatar_component_clause, [], [f776])).
fof(f776, plain, (spl18_89 <=> (e2 = op(op(e1, op(e1, e1)), e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_89])])).
fof(f2853, plain, (~ spl18_6 | ~ spl18_10), inference(avatar_split_clause, [], [f2849, f336, f319])).
fof(f2849, plain, (~ (e1 = op(e3, e2)) | ~ spl18_10), inference(backward_demodulation, [], [f184, f338])).
fof(f2836, plain, (~ spl18_21 | ~ spl18_25), inference(avatar_split_clause, [], [f2831, f400, f383])).
fof(f383, plain, (spl18_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_21])])).
fof(f2831, plain, (~ (e0 = op(e2, e2)) | ~ spl18_25), inference(backward_demodulation, [], [f178, f402])).
fof(f2816, plain, (~ spl18_28 | ~ spl18_44), inference(avatar_split_clause, [], [f2811, f480, f412])).
fof(f2811, plain, (~ (e3 = op(e2, e1)) | ~ spl18_44), inference(backward_demodulation, [], [f148, f482])).
fof(f2809, plain, (~ spl18_30 | ~ spl18_46), inference(avatar_split_clause, [], [f2803, f489, f421])).
fof(f421, plain, (spl18_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_30])])).
fof(f2803, plain, (~ (e1 = op(e2, e0)) | ~ spl18_46), inference(backward_demodulation, [], [f142, f491])).
fof(f2802, plain, (~ spl18_4 | ~ spl18_52), inference(avatar_split_clause, [], [f2801, f514, f310])).
fof(f2801, plain, (~ (e3 = op(e3, e3)) | ~ spl18_52), inference(backward_demodulation, [], [f159, f516])).
fof(f516, plain, ((e3 = op(e0, e3)) | ~ spl18_52), inference(avatar_component_clause, [], [f514])).
fof(f2798, plain, (~ spl18_6 | ~ spl18_54), inference(avatar_split_clause, [], [f2794, f523, f319])).
fof(f2794, plain, (~ (e1 = op(e3, e2)) | ~ spl18_54), inference(backward_demodulation, [], [f153, f525])).
fof(f2792, plain, (~ spl18_11 | ~ spl18_59), inference(avatar_split_clause, [], [f2788, f544, f340])).
fof(f2788, plain, (~ (e2 = op(e3, e1)) | ~ spl18_59), inference(backward_demodulation, [], [f147, f546])).
fof(f2761, plain, (~ spl18_35 | ~ spl18_39), inference(avatar_split_clause, [], [f2760, f459, f442])).
fof(f2760, plain, (~ (e2 = op(e1, e3)) | ~ spl18_39), inference(forward_demodulation, [], [f174, f461])).
fof(f461, plain, ((e2 = op(e1, e2)) | ~ spl18_39), inference(avatar_component_clause, [], [f459])).
fof(f2759, plain, (~ spl18_23 | ~ spl18_39), inference(avatar_split_clause, [], [f2758, f459, f391])).
fof(f2758, plain, (~ (e2 = op(e2, e2)) | ~ spl18_39), inference(forward_demodulation, [], [f154, f461])).
fof(f2748, plain, (~ spl18_9 | ~ spl18_13), inference(avatar_split_clause, [], [f2746, f349, f332])).
fof(f2746, plain, (~ (e0 = op(e3, e1)) | ~ spl18_13), inference(backward_demodulation, [], [f181, f351])).
fof(f2740, plain, (~ spl18_26 | ~ spl18_30), inference(avatar_split_clause, [], [f2733, f421, f404])).
fof(f2733, plain, (~ (e1 = op(e2, e1)) | ~ spl18_30), inference(backward_demodulation, [], [f175, f423])).
fof(f423, plain, ((e1 = op(e2, e0)) | ~ spl18_30), inference(avatar_component_clause, [], [f421])).
fof(f2739, plain, (~ spl18_14 | ~ spl18_30), inference(avatar_split_clause, [], [f2732, f421, f353])).
fof(f2732, plain, (~ (e1 = op(e3, e0)) | ~ spl18_30), inference(backward_demodulation, [], [f144, f423])).
fof(f2731, plain, (~ spl18_7 | ~ spl18_39), inference(avatar_split_clause, [], [f2729, f459, f323])).
fof(f2729, plain, (~ (e2 = op(e3, e2)) | ~ spl18_39), inference(backward_demodulation, [], [f155, f461])).
fof(f2728, plain, (~ spl18_9 | ~ spl18_41), inference(avatar_split_clause, [], [f2721, f468, f332])).
fof(f2721, plain, (~ (e0 = op(e3, e1)) | ~ spl18_41), inference(backward_demodulation, [], [f149, f470])).
fof(f2701, plain, (~ spl18_54 | ~ spl18_63 | spl18_88), inference(avatar_split_clause, [], [f2700, f769, f561, f523])).
fof(f2700, plain, (~ (e1 = op(e0, e2)) | (~ spl18_63 | spl18_88)), inference(forward_demodulation, [], [f771, f563])).
fof(f2699, plain, (~ spl18_32 | ~ spl18_21 | spl18_96), inference(avatar_split_clause, [], [f2698, f810, f383, f429])).
fof(f2698, plain, (~ (e3 = op(e2, e0)) | (~ spl18_21 | spl18_96)), inference(forward_demodulation, [], [f812, f385])).
fof(f385, plain, ((e0 = op(e2, e2)) | ~ spl18_21), inference(avatar_component_clause, [], [f383])).
fof(f2691, plain, (~ spl18_56 | ~ spl18_63 | spl18_84), inference(avatar_split_clause, [], [f2552, f750, f561, f531])).
fof(f2552, plain, (~ (e3 = op(e0, e2)) | (~ spl18_63 | spl18_84)), inference(backward_demodulation, [], [f752, f563])).
fof(f2684, plain, (~ spl18_11 | spl18_89 | ~ spl18_90), inference(avatar_split_clause, [], [f2681, f780, f776, f340])).
fof(f2681, plain, (~ (e2 = op(e3, e1)) | (spl18_89 | ~ spl18_90)), inference(backward_demodulation, [], [f778, f781])).
fof(f2671, plain, (~ spl18_8 | ~ spl18_4), inference(avatar_split_clause, [], [f2670, f310, f327])).
fof(f2670, plain, (~ (e3 = op(e3, e2)) | ~ spl18_4), inference(forward_demodulation, [], [f186, f312])).
fof(f2657, plain, (~ spl18_2 | ~ spl18_14), inference(avatar_split_clause, [], [f2656, f353, f302])).
fof(f2656, plain, (~ (e1 = op(e3, e3)) | ~ spl18_14), inference(backward_demodulation, [], [f183, f355])).
fof(f2648, plain, (~ spl18_33 | ~ spl18_45), inference(avatar_split_clause, [], [f2644, f485, f434])).
fof(f2644, plain, (~ (e0 = op(e1, e3)) | ~ spl18_45), inference(backward_demodulation, [], [f171, f487])).
fof(f2647, plain, (~ spl18_13 | ~ spl18_45), inference(avatar_split_clause, [], [f2643, f485, f349])).
fof(f2643, plain, (~ (e0 = op(e3, e0)) | ~ spl18_45), inference(backward_demodulation, [], [f143, f487])).
fof(f2641, plain, (~ spl18_33 | ~ spl18_49), inference(avatar_split_clause, [], [f2636, f502, f434])).
fof(f2636, plain, (~ (e0 = op(e1, e3)) | ~ spl18_49), inference(backward_demodulation, [], [f157, f504])).
fof(f2598, plain, (~ spl18_20 | ~ spl18_32), inference(avatar_split_clause, [], [f2597, f429, f378])).
fof(f2597, plain, (~ (e3 = op(e2, e3)) | ~ spl18_32), inference(backward_demodulation, [], [f177, f431])).
fof(f2570, plain, (~ spl18_53 | ~ spl18_57), inference(avatar_split_clause, [], [f2565, f536, f519])).
fof(f2565, plain, (~ (e0 = op(e0, e2)) | ~ spl18_57), inference(backward_demodulation, [], [f166, f538])).
fof(f2562, plain, (~ spl18_62 | ~ spl18_63), inference(avatar_contradiction_clause, [], [f2561])).
fof(f2561, plain, ($false | (~ spl18_62 | ~ spl18_63)), inference(subsumption_resolution, [], [f2560, f190])).
fof(f190, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2560, plain, ((e1 = e2) | (~ spl18_62 | ~ spl18_63)), inference(backward_demodulation, [], [f563, f559])).
fof(f2543, plain, (~ spl18_6 | spl18_95 | ~ spl18_96), inference(avatar_split_clause, [], [f2541, f810, f806, f319])).
fof(f2541, plain, (~ (e1 = op(e3, e2)) | (spl18_95 | ~ spl18_96)), inference(backward_demodulation, [], [f808, f811])).
fof(f2518, plain, (~ spl18_6 | ~ spl18_2), inference(avatar_split_clause, [], [f2517, f302, f319])).
fof(f2517, plain, (~ (e1 = op(e3, e2)) | ~ spl18_2), inference(forward_demodulation, [], [f186, f304])).
fof(f2501, plain, (~ spl18_1 | ~ spl18_2), inference(avatar_contradiction_clause, [], [f2500])).
fof(f2500, plain, ($false | (~ spl18_1 | ~ spl18_2)), inference(subsumption_resolution, [], [f2499, f187])).
fof(f2499, plain, ((e0 = e1) | (~ spl18_1 | ~ spl18_2)), inference(forward_demodulation, [], [f304, f300])).
fof(f300, plain, ((e0 = op(e3, e3)) | ~ spl18_1), inference(avatar_component_clause, [], [f298])).
fof(f2494, plain, (~ spl18_11 | ~ spl18_15), inference(avatar_split_clause, [], [f2488, f357, f340])).
fof(f2488, plain, (~ (e2 = op(e3, e1)) | ~ spl18_15), inference(backward_demodulation, [], [f181, f359])).
fof(f2483, plain, (~ spl18_19 | ~ spl18_23), inference(avatar_split_clause, [], [f2477, f391, f374])).
fof(f2477, plain, (~ (e2 = op(e2, e3)) | ~ spl18_23), inference(backward_demodulation, [], [f180, f393])).
fof(f2443, plain, (~ spl18_41 | ~ spl18_45), inference(avatar_split_clause, [], [f2437, f485, f468])).
fof(f2437, plain, (~ (e0 = op(e1, e1)) | ~ spl18_45), inference(backward_demodulation, [], [f169, f487])).
fof(f2434, plain, (~ spl18_19 | ~ spl18_51), inference(avatar_split_clause, [], [f2432, f510, f374])).
fof(f2432, plain, (~ (e2 = op(e2, e3)) | ~ spl18_51), inference(backward_demodulation, [], [f158, f512])).
fof(f2430, plain, (~ spl18_21 | ~ spl18_53), inference(avatar_split_clause, [], [f2424, f519, f383])).
fof(f2424, plain, (~ (e0 = op(e2, e2)) | ~ spl18_53), inference(backward_demodulation, [], [f152, f521])).
fof(f2422, plain, (~ spl18_50 | ~ spl18_58), inference(avatar_split_clause, [], [f2418, f540, f506])).
fof(f2418, plain, (~ (e1 = op(e0, e3)) | ~ spl18_58), inference(backward_demodulation, [], [f167, f542])).
fof(f2410, plain, (~ spl18_32 | ~ spl18_64), inference(avatar_split_clause, [], [f2403, f565, f429])).
fof(f2403, plain, (~ (e3 = op(e2, e0)) | ~ spl18_64), inference(backward_demodulation, [], [f140, f567])).
fof(f140, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2401, plain, (~ spl18_30 | ~ spl18_86 | spl18_87), inference(avatar_split_clause, [], [f2399, f764, f759, f421])).
fof(f2399, plain, (~ (e1 = op(e2, e0)) | (~ spl18_86 | spl18_87)), inference(backward_demodulation, [], [f766, f760])).
fof(f2400, plain, (~ spl18_32 | spl18_85 | ~ spl18_86), inference(avatar_split_clause, [], [f2398, f759, f755, f429])).
fof(f755, plain, (spl18_85 <=> (e3 = op(op(e0, op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f2398, plain, (~ (e3 = op(e2, e0)) | (spl18_85 | ~ spl18_86)), inference(backward_demodulation, [], [f757, f760])).
fof(f757, plain, (~ (e3 = op(op(e0, op(e0, e0)), e0)) | spl18_85), inference(avatar_component_clause, [], [f755])).
fof(f2391, plain, (~ spl18_14 | ~ spl18_1 | spl18_104), inference(avatar_split_clause, [], [f2390, f849, f298, f353])).
fof(f849, plain, (spl18_104 <=> (e1 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_104])])).
fof(f2390, plain, (~ (e1 = op(e3, e0)) | (~ spl18_1 | spl18_104)), inference(forward_demodulation, [], [f851, f300])).
fof(f851, plain, (~ (e1 = op(e3, op(e3, e3))) | spl18_104), inference(avatar_component_clause, [], [f849])).
fof(f2385, plain, (~ spl18_25 | ~ spl18_92 | spl18_93), inference(avatar_split_clause, [], [f2384, f794, f789, f400])).
fof(f2384, plain, (~ (e0 = op(e2, e1)) | (~ spl18_92 | spl18_93)), inference(forward_demodulation, [], [f796, f790])).
fof(f2361, plain, (~ spl18_18 | ~ spl18_26), inference(avatar_split_clause, [], [f2358, f404, f370])).
fof(f2358, plain, (~ (e1 = op(e2, e3)) | ~ spl18_26), inference(backward_demodulation, [], [f179, f406])).
fof(f2352, plain, (~ spl18_18 | ~ spl18_50), inference(avatar_split_clause, [], [f2349, f506, f370])).
fof(f2349, plain, (~ (e1 = op(e2, e3)) | ~ spl18_50), inference(backward_demodulation, [], [f158, f508])).
fof(f2346, plain, (~ spl18_23 | ~ spl18_55), inference(avatar_split_clause, [], [f2340, f527, f391])).
fof(f2340, plain, (~ (e2 = op(e2, e2)) | ~ spl18_55), inference(backward_demodulation, [], [f152, f529])).
fof(f2335, plain, (~ spl18_61 | ~ spl18_63), inference(avatar_contradiction_clause, [], [f2334])).
fof(f2334, plain, ($false | (~ spl18_61 | ~ spl18_63)), inference(subsumption_resolution, [], [f2333, f188])).
fof(f188, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2333, plain, ((e0 = e2) | (~ spl18_61 | ~ spl18_63)), inference(backward_demodulation, [], [f563, f555])).
fof(f2320, plain, (~ spl18_28 | ~ spl18_41 | ~ spl18_47 | spl18_91), inference(avatar_split_clause, [], [f2319, f785, f493, f468, f412])).
fof(f785, plain, (spl18_91 <=> (e3 = op(op(e1, op(e1, e1)), e1))), introduced(avatar_definition, [new_symbols(naming, [spl18_91])])).
fof(f2319, plain, (~ (e3 = op(e2, e1)) | (~ spl18_41 | ~ spl18_47 | spl18_91)), inference(forward_demodulation, [], [f2318, f495])).
fof(f2318, plain, (~ (e3 = op(op(e1, e0), e1)) | (~ spl18_41 | spl18_91)), inference(forward_demodulation, [], [f787, f470])).
fof(f787, plain, (~ (e3 = op(op(e1, op(e1, e1)), e1)) | spl18_91), inference(avatar_component_clause, [], [f785])).
fof(f2316, plain, (~ spl18_63 | ~ spl18_47), inference(avatar_split_clause, [], [f2315, f493, f561])).
fof(f2315, plain, (~ (op(e0, e0) = e2) | ~ spl18_47), inference(forward_demodulation, [], [f139, f495])).
fof(f2298, plain, (~ spl18_3 | ~ spl18_11), inference(avatar_split_clause, [], [f2296, f340, f306])).
fof(f2296, plain, (~ (e2 = op(e3, e3)) | ~ spl18_11), inference(backward_demodulation, [], [f185, f342])).
fof(f2294, plain, (~ spl18_6 | ~ spl18_14), inference(avatar_split_clause, [], [f2290, f353, f319])).
fof(f2290, plain, (~ (e1 = op(e3, e2)) | ~ spl18_14), inference(backward_demodulation, [], [f182, f355])).
fof(f2279, plain, (~ spl18_24 | ~ spl18_28), inference(avatar_split_clause, [], [f2277, f412, f395])).
fof(f2277, plain, (~ (e3 = op(e2, e2)) | ~ spl18_28), inference(backward_demodulation, [], [f178, f414])).
fof(f414, plain, ((e3 = op(e2, e1)) | ~ spl18_28), inference(avatar_component_clause, [], [f412])).
fof(f2267, plain, (~ spl18_6 | ~ spl18_38), inference(avatar_split_clause, [], [f2264, f455, f319])).
fof(f2264, plain, (~ (e1 = op(e3, e2)) | ~ spl18_38), inference(backward_demodulation, [], [f155, f457])).
fof(f2261, plain, (~ spl18_41 | ~ spl18_47 | spl18_92), inference(avatar_contradiction_clause, [], [f2260])).
fof(f2260, plain, ($false | (~ spl18_41 | ~ spl18_47 | spl18_92)), inference(subsumption_resolution, [], [f2252, f495])).
fof(f2252, plain, (~ (e2 = op(e1, e0)) | (~ spl18_41 | spl18_92)), inference(backward_demodulation, [], [f791, f470])).
fof(f791, plain, (~ (e2 = op(e1, op(e1, e1))) | spl18_92), inference(avatar_component_clause, [], [f789])).
fof(f2255, plain, (~ spl18_37 | ~ spl18_41), inference(avatar_split_clause, [], [f2245, f468, f451])).
fof(f2245, plain, (~ (e0 = op(e1, e2)) | ~ spl18_41), inference(backward_demodulation, [], [f172, f470])).
fof(f172, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2231, plain, (~ spl18_37 | ~ spl18_53), inference(avatar_split_clause, [], [f2224, f519, f451])).
fof(f2224, plain, (~ (e0 = op(e1, e2)) | ~ spl18_53), inference(backward_demodulation, [], [f151, f521])).
fof(f2204, plain, (~ spl18_31 | spl18_83 | ~ spl18_86), inference(avatar_split_clause, [], [f2203, f759, f746, f425])).
fof(f2203, plain, (~ (e2 = op(e2, e0)) | (spl18_83 | ~ spl18_86)), inference(forward_demodulation, [], [f748, f760])).
fof(f2197, plain, (~ spl18_44 | ~ spl18_36), inference(avatar_split_clause, [], [f2196, f446, f480])).
fof(f2196, plain, (~ (e3 = op(e1, e1)) | ~ spl18_36), inference(forward_demodulation, [], [f173, f448])).
fof(f2190, plain, (~ spl18_56 | spl18_97 | ~ spl18_100), inference(avatar_split_clause, [], [f2189, f829, f815, f531])).
fof(f815, plain, (spl18_97 <=> (e3 = op(op(e2, op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl18_97])])).
fof(f2189, plain, (~ (e3 = op(e0, e2)) | (spl18_97 | ~ spl18_100)), inference(forward_demodulation, [], [f817, f830])).
fof(f817, plain, (~ (e3 = op(op(e2, op(e2, e2)), e2)) | spl18_97), inference(avatar_component_clause, [], [f815])).
fof(f2172, plain, (~ spl18_37 | ~ spl18_43 | ~ spl18_60 | spl18_91), inference(avatar_contradiction_clause, [], [f2171])).
fof(f2171, plain, ($false | (~ spl18_37 | ~ spl18_43 | ~ spl18_60 | spl18_91)), inference(subsumption_resolution, [], [f2170, f550])).
fof(f2170, plain, (~ (e3 = op(e0, e1)) | (~ spl18_37 | ~ spl18_43 | spl18_91)), inference(forward_demodulation, [], [f2169, f453])).
fof(f2169, plain, (~ (e3 = op(op(e1, e2), e1)) | (~ spl18_43 | spl18_91)), inference(forward_demodulation, [], [f787, f478])).
fof(f2144, plain, (~ spl18_27 | ~ spl18_31), inference(avatar_split_clause, [], [f2141, f425, f408])).
fof(f2141, plain, (~ (e2 = op(e2, e1)) | ~ spl18_31), inference(backward_demodulation, [], [f175, f427])).
fof(f2139, plain, (~ spl18_4 | ~ spl18_36), inference(avatar_split_clause, [], [f2137, f446, f310])).
fof(f2137, plain, (~ (e3 = op(e3, e3)) | ~ spl18_36), inference(backward_demodulation, [], [f161, f448])).
fof(f2091, plain, (~ spl18_33 | ~ spl18_104 | spl18_105), inference(avatar_split_clause, [], [f2090, f854, f849, f434])).
fof(f2090, plain, (~ (e0 = op(e1, e3)) | (~ spl18_104 | spl18_105)), inference(backward_demodulation, [], [f856, f850])).
fof(f850, plain, ((e1 = op(e3, op(e3, e3))) | ~ spl18_104), inference(avatar_component_clause, [], [f849])).
fof(f2083, plain, (~ spl18_56 | ~ spl18_24), inference(avatar_split_clause, [], [f2082, f395, f531])).
fof(f2082, plain, (~ (e3 = op(e0, e2)) | ~ spl18_24), inference(forward_demodulation, [], [f152, f397])).
fof(f2074, plain, (~ spl18_39 | ~ spl18_43), inference(avatar_split_clause, [], [f2073, f476, f459])).
fof(f2073, plain, (~ (e2 = op(e1, e2)) | ~ spl18_43), inference(forward_demodulation, [], [f172, f478])).
fof(f2071, plain, (~ spl18_35 | ~ spl18_43), inference(avatar_split_clause, [], [f2070, f476, f442])).
fof(f2070, plain, (~ (e2 = op(e1, e3)) | ~ spl18_43), inference(forward_demodulation, [], [f173, f478])).
fof(f2065, plain, (~ spl18_17 | ~ spl18_24 | spl18_100), inference(avatar_split_clause, [], [f1950, f829, f395, f366])).
fof(f1950, plain, (~ (e0 = op(e2, e3)) | (~ spl18_24 | spl18_100)), inference(forward_demodulation, [], [f831, f397])).
fof(f2045, plain, (~ spl18_9 | ~ spl18_10), inference(avatar_contradiction_clause, [], [f2044])).
fof(f2044, plain, ($false | (~ spl18_9 | ~ spl18_10)), inference(subsumption_resolution, [], [f2043, f187])).
fof(f2043, plain, ((e0 = e1) | (~ spl18_9 | ~ spl18_10)), inference(backward_demodulation, [], [f338, f334])).
fof(f2036, plain, (~ spl18_22 | ~ spl18_24), inference(avatar_contradiction_clause, [], [f2035])).
fof(f2035, plain, ($false | (~ spl18_22 | ~ spl18_24)), inference(subsumption_resolution, [], [f2034, f191])).
fof(f191, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2034, plain, ((e1 = e3) | (~ spl18_22 | ~ spl18_24)), inference(backward_demodulation, [], [f397, f389])).
fof(f2032, plain, (~ spl18_11 | ~ spl18_27), inference(avatar_split_clause, [], [f2031, f408, f340])).
fof(f2031, plain, (~ (e2 = op(e3, e1)) | ~ spl18_27), inference(backward_demodulation, [], [f150, f410])).
fof(f410, plain, ((e2 = op(e2, e1)) | ~ spl18_27), inference(avatar_component_clause, [], [f408])).
fof(f2019, plain, (~ spl18_27 | ~ spl18_43), inference(avatar_split_clause, [], [f2014, f476, f408])).
fof(f2014, plain, (~ (e2 = op(e2, e1)) | ~ spl18_43), inference(backward_demodulation, [], [f148, f478])).
fof(f2013, plain, (~ spl18_46 | ~ spl18_48), inference(avatar_contradiction_clause, [], [f2012])).
fof(f2012, plain, ($false | (~ spl18_46 | ~ spl18_48)), inference(subsumption_resolution, [], [f2011, f191])).
fof(f2011, plain, ((e1 = e3) | (~ spl18_46 | ~ spl18_48)), inference(backward_demodulation, [], [f499, f491])).
fof(f1969, plain, (~ spl18_29 | ~ spl18_13), inference(avatar_split_clause, [], [f1968, f349, f417])).
fof(f1968, plain, (~ (e0 = op(e2, e0)) | ~ spl18_13), inference(forward_demodulation, [], [f144, f351])).
fof(f1967, plain, (~ spl18_30 | ~ spl18_18), inference(avatar_split_clause, [], [f1966, f370, f421])).
fof(f1966, plain, (~ (e1 = op(e2, e0)) | ~ spl18_18), inference(forward_demodulation, [], [f177, f372])).
fof(f1961, plain, (~ spl18_31 | ~ spl18_63), inference(avatar_split_clause, [], [f1960, f561, f425])).
fof(f1960, plain, (~ (e2 = op(e2, e0)) | ~ spl18_63), inference(forward_demodulation, [], [f140, f563])).
fof(f1938, plain, (~ spl18_3 | ~ spl18_6 | spl18_104), inference(avatar_contradiction_clause, [], [f1937])).
fof(f1937, plain, ($false | (~ spl18_3 | ~ spl18_6 | spl18_104)), inference(subsumption_resolution, [], [f1932, f321])).
fof(f321, plain, ((e1 = op(e3, e2)) | ~ spl18_6), inference(avatar_component_clause, [], [f319])).
fof(f1932, plain, (~ (e1 = op(e3, e2)) | (~ spl18_3 | spl18_104)), inference(backward_demodulation, [], [f851, f308])).
fof(f308, plain, ((e2 = op(e3, e3)) | ~ spl18_3), inference(avatar_component_clause, [], [f306])).
fof(f1922, plain, (~ spl18_13 | ~ spl18_16), inference(avatar_contradiction_clause, [], [f1921])).
fof(f1921, plain, ($false | (~ spl18_13 | ~ spl18_16)), inference(subsumption_resolution, [], [f1920, f189])).
fof(f189, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f1920, plain, ((e0 = e3) | (~ spl18_13 | ~ spl18_16)), inference(backward_demodulation, [], [f363, f351])).
fof(f1910, plain, (~ spl18_23 | ~ spl18_24), inference(avatar_contradiction_clause, [], [f1909])).
fof(f1909, plain, ($false | (~ spl18_23 | ~ spl18_24)), inference(subsumption_resolution, [], [f1908, f192])).
fof(f1908, plain, ((e2 = e3) | (~ spl18_23 | ~ spl18_24)), inference(forward_demodulation, [], [f397, f393])).
fof(f1907, plain, (~ spl18_33 | ~ spl18_36), inference(avatar_contradiction_clause, [], [f1906])).
fof(f1906, plain, ($false | (~ spl18_33 | ~ spl18_36)), inference(subsumption_resolution, [], [f1905, f189])).
fof(f1905, plain, ((e0 = e3) | (~ spl18_33 | ~ spl18_36)), inference(backward_demodulation, [], [f448, f436])).
fof(f1891, plain, (~ spl18_38 | ~ spl18_42), inference(avatar_split_clause, [], [f1884, f472, f455])).
fof(f1884, plain, (~ (e1 = op(e1, e2)) | ~ spl18_42), inference(backward_demodulation, [], [f172, f474])).
fof(f1882, plain, (~ spl18_36 | ~ spl18_48), inference(avatar_split_clause, [], [f1879, f497, f446])).
fof(f1879, plain, (~ (e3 = op(e1, e3)) | ~ spl18_48), inference(backward_demodulation, [], [f171, f499])).
fof(f1881, plain, (~ spl18_44 | ~ spl18_48), inference(avatar_split_clause, [], [f1877, f497, f480])).
fof(f1877, plain, (~ (e3 = op(e1, e1)) | ~ spl18_48), inference(backward_demodulation, [], [f169, f499])).
fof(f1880, plain, (~ spl18_16 | ~ spl18_48), inference(avatar_split_clause, [], [f1876, f497, f361])).
fof(f1876, plain, (~ (e3 = op(e3, e0)) | ~ spl18_48), inference(backward_demodulation, [], [f143, f499])).
fof(f1875, plain, (~ spl18_44 | ~ spl18_60), inference(avatar_split_clause, [], [f1874, f548, f480])).
fof(f1874, plain, (~ (e3 = op(e1, e1)) | ~ spl18_60), inference(backward_demodulation, [], [f145, f550])).
fof(f145, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1869, plain, (~ spl18_59 | ~ spl18_63), inference(avatar_split_clause, [], [f1865, f561, f544])).
fof(f1865, plain, (~ (e2 = op(e0, e1)) | ~ spl18_63), inference(backward_demodulation, [], [f163, f563])).
fof(f1854, plain, (~ spl18_48 | spl18_85 | ~ spl18_88), inference(avatar_split_clause, [], [f1853, f769, f755, f497])).
fof(f1853, plain, (~ (e3 = op(e1, e0)) | (spl18_85 | ~ spl18_88)), inference(forward_demodulation, [], [f757, f770])).
fof(f770, plain, ((e1 = op(e0, op(e0, e0))) | ~ spl18_88), inference(avatar_component_clause, [], [f769])).
fof(f1841, plain, (~ spl18_18 | spl18_101 | ~ spl18_102), inference(avatar_split_clause, [], [f1631, f840, f836, f370])).
fof(f1631, plain, (~ (e1 = op(e2, e3)) | (spl18_101 | ~ spl18_102)), inference(backward_demodulation, [], [f838, f841])).
fof(f841, plain, ((e2 = op(e3, op(e3, e3))) | ~ spl18_102), inference(avatar_component_clause, [], [f840])).
fof(f1836, plain, (~ spl18_35 | ~ spl18_44 | spl18_92), inference(avatar_contradiction_clause, [], [f1835])).
fof(f1835, plain, ($false | (~ spl18_35 | ~ spl18_44 | spl18_92)), inference(subsumption_resolution, [], [f1834, f444])).
fof(f1834, plain, (~ (e2 = op(e1, e3)) | (~ spl18_44 | spl18_92)), inference(forward_demodulation, [], [f791, f482])).
fof(f1818, plain, (~ spl18_25 | ~ spl18_35 | ~ spl18_44 | spl18_93), inference(avatar_contradiction_clause, [], [f1817])).
fof(f1817, plain, ($false | (~ spl18_25 | ~ spl18_35 | ~ spl18_44 | spl18_93)), inference(subsumption_resolution, [], [f1813, f402])).
fof(f1813, plain, (~ (e0 = op(e2, e1)) | (~ spl18_35 | ~ spl18_44 | spl18_93)), inference(backward_demodulation, [], [f1801, f444])).
fof(f1801, plain, (~ (e0 = op(op(e1, e3), e1)) | (~ spl18_44 | spl18_93)), inference(backward_demodulation, [], [f796, f482])).
fof(f1814, plain, (~ spl18_19 | ~ spl18_35), inference(avatar_split_clause, [], [f1809, f442, f374])).
fof(f1809, plain, (~ (e2 = op(e2, e3)) | ~ spl18_35), inference(backward_demodulation, [], [f160, f444])).
fof(f1784, plain, (~ spl18_24 | ~ spl18_8), inference(avatar_split_clause, [], [f1783, f327, f395])).
fof(f1783, plain, (~ (e3 = op(e2, e2)) | ~ spl18_8), inference(forward_demodulation, [], [f156, f329])).
fof(f329, plain, ((e3 = op(e3, e2)) | ~ spl18_8), inference(avatar_component_clause, [], [f327])).
fof(f1781, plain, (~ spl18_22 | ~ spl18_30), inference(avatar_split_clause, [], [f1780, f421, f387])).
fof(f1780, plain, (~ (e1 = op(e2, e2)) | ~ spl18_30), inference(forward_demodulation, [], [f176, f423])).
fof(f1761, plain, (~ spl18_15 | ~ spl18_16), inference(avatar_contradiction_clause, [], [f1760])).
fof(f1760, plain, ($false | (~ spl18_15 | ~ spl18_16)), inference(subsumption_resolution, [], [f1759, f192])).
fof(f1759, plain, ((e2 = e3) | (~ spl18_15 | ~ spl18_16)), inference(backward_demodulation, [], [f363, f359])).
fof(f1757, plain, (~ spl18_12 | ~ spl18_16), inference(avatar_split_clause, [], [f1754, f361, f344])).
fof(f1754, plain, (~ (e3 = op(e3, e1)) | ~ spl18_16), inference(backward_demodulation, [], [f181, f363])).
fof(f1753, plain, (~ spl18_3 | ~ spl18_19), inference(avatar_split_clause, [], [f1752, f374, f306])).
fof(f1752, plain, (~ (e2 = op(e3, e3)) | ~ spl18_19), inference(backward_demodulation, [], [f162, f376])).
fof(f376, plain, ((e2 = op(e2, e3)) | ~ spl18_19), inference(avatar_component_clause, [], [f374])).
fof(f1750, plain, (~ spl18_25 | ~ spl18_28), inference(avatar_contradiction_clause, [], [f1749])).
fof(f1749, plain, ($false | (~ spl18_25 | ~ spl18_28)), inference(subsumption_resolution, [], [f1748, f189])).
fof(f1748, plain, ((e0 = e3) | (~ spl18_25 | ~ spl18_28)), inference(backward_demodulation, [], [f414, f402])).
fof(f1743, plain, (~ spl18_20 | ~ spl18_28), inference(avatar_split_clause, [], [f1740, f412, f378])).
fof(f1740, plain, (~ (e3 = op(e2, e3)) | ~ spl18_28), inference(backward_demodulation, [], [f179, f414])).
fof(f1742, plain, (~ spl18_12 | ~ spl18_28), inference(avatar_split_clause, [], [f1739, f412, f344])).
fof(f1739, plain, (~ (e3 = op(e3, e1)) | ~ spl18_28), inference(backward_demodulation, [], [f150, f414])).
fof(f1724, plain, (~ spl18_29 | ~ spl18_45), inference(avatar_split_clause, [], [f1717, f485, f417])).
fof(f1717, plain, (~ (e0 = op(e2, e0)) | ~ spl18_45), inference(backward_demodulation, [], [f142, f487])).
fof(f1704, plain, (~ spl18_47 | ~ spl18_50 | ~ spl18_64 | spl18_83), inference(avatar_split_clause, [], [f1703, f746, f565, f506, f493])).
fof(f1703, plain, (~ (e2 = op(e1, e0)) | (~ spl18_50 | ~ spl18_64 | spl18_83)), inference(forward_demodulation, [], [f1702, f508])).
fof(f1702, plain, (~ (e2 = op(op(e0, e3), e0)) | (~ spl18_64 | spl18_83)), inference(forward_demodulation, [], [f748, f567])).
fof(f1683, plain, (~ spl18_3 | ~ spl18_5 | spl18_106), inference(avatar_contradiction_clause, [], [f1682])).
fof(f1682, plain, ($false | (~ spl18_3 | ~ spl18_5 | spl18_106)), inference(subsumption_resolution, [], [f1677, f317])).
fof(f317, plain, ((e0 = op(e3, e2)) | ~ spl18_5), inference(avatar_component_clause, [], [f315])).
fof(f1677, plain, (~ (e0 = op(e3, e2)) | (~ spl18_3 | spl18_106)), inference(backward_demodulation, [], [f861, f308])).
fof(f1673, plain, (~ spl18_1 | ~ spl18_5), inference(avatar_split_clause, [], [f1672, f315, f298])).
fof(f1672, plain, (~ (e0 = op(e3, e3)) | ~ spl18_5), inference(backward_demodulation, [], [f186, f317])).
fof(f1662, plain, (~ spl18_1 | ~ spl18_33), inference(avatar_split_clause, [], [f1660, f434, f298])).
fof(f1660, plain, (~ (e0 = op(e3, e3)) | ~ spl18_33), inference(backward_demodulation, [], [f161, f436])).
fof(f1657, plain, (~ spl18_42 | ~ spl18_43), inference(avatar_contradiction_clause, [], [f1656])).
fof(f1656, plain, ($false | (~ spl18_42 | ~ spl18_43)), inference(subsumption_resolution, [], [f1655, f190])).
fof(f1655, plain, ((e1 = e2) | (~ spl18_42 | ~ spl18_43)), inference(forward_demodulation, [], [f478, f474])).
fof(f1654, plain, (~ spl18_15 | ~ spl18_47), inference(avatar_split_clause, [], [f1652, f493, f357])).
fof(f1652, plain, (~ (e2 = op(e3, e0)) | ~ spl18_47), inference(backward_demodulation, [], [f143, f495])).
fof(f1637, plain, (~ spl18_2 | ~ spl18_50), inference(avatar_split_clause, [], [f1636, f506, f302])).
fof(f1636, plain, (~ (e1 = op(e3, e3)) | ~ spl18_50), inference(forward_demodulation, [], [f159, f508])).
fof(f1621, plain, (~ spl18_21 | ~ spl18_29), inference(avatar_split_clause, [], [f1620, f417, f383])).
fof(f1620, plain, (~ (e0 = op(e2, e2)) | ~ spl18_29), inference(backward_demodulation, [], [f176, f419])).
fof(f1616, plain, (~ spl18_37 | ~ spl18_40), inference(avatar_contradiction_clause, [], [f1615])).
fof(f1615, plain, ($false | (~ spl18_37 | ~ spl18_40)), inference(subsumption_resolution, [], [f1613, f189])).
fof(f1613, plain, ((e0 = e3) | (~ spl18_37 | ~ spl18_40)), inference(backward_demodulation, [], [f465, f453])).
fof(f1605, plain, (~ spl18_46 | ~ spl18_42), inference(avatar_split_clause, [], [f1604, f472, f489])).
fof(f1604, plain, (~ (e1 = op(e1, e0)) | ~ spl18_42), inference(forward_demodulation, [], [f169, f474])).
fof(f1600, plain, (~ spl18_31 | ~ spl18_15), inference(avatar_split_clause, [], [f1599, f357, f425])).
fof(f1599, plain, (~ (e2 = op(e2, e0)) | ~ spl18_15), inference(forward_demodulation, [], [f144, f359])).
fof(f1582, plain, (~ spl18_21 | ~ spl18_30 | ~ spl18_40 | spl18_97), inference(avatar_contradiction_clause, [], [f1581])).
fof(f1581, plain, ($false | (~ spl18_21 | ~ spl18_30 | ~ spl18_40 | spl18_97)), inference(subsumption_resolution, [], [f1580, f465])).
fof(f1580, plain, (~ (e3 = op(e1, e2)) | (~ spl18_21 | ~ spl18_30 | spl18_97)), inference(forward_demodulation, [], [f1579, f423])).
fof(f1579, plain, (~ (e3 = op(op(e2, e0), e2)) | (~ spl18_21 | spl18_97)), inference(forward_demodulation, [], [f817, f385])).
fof(f1578, plain, (~ spl18_21 | ~ spl18_30 | spl18_98), inference(avatar_contradiction_clause, [], [f1577])).
fof(f1577, plain, ($false | (~ spl18_21 | ~ spl18_30 | spl18_98)), inference(subsumption_resolution, [], [f1576, f423])).
fof(f1576, plain, (~ (e1 = op(e2, e0)) | (~ spl18_21 | spl18_98)), inference(forward_demodulation, [], [f821, f385])).
fof(f1563, plain, (~ spl18_21 | ~ spl18_23), inference(avatar_contradiction_clause, [], [f1562])).
fof(f1562, plain, ($false | (~ spl18_21 | ~ spl18_23)), inference(subsumption_resolution, [], [f1561, f188])).
fof(f1561, plain, ((e0 = e2) | (~ spl18_21 | ~ spl18_23)), inference(backward_demodulation, [], [f393, f385])).
fof(f1551, plain, (~ spl18_41 | ~ spl18_42 | spl18_93), inference(avatar_split_clause, [], [f1550, f794, f472, f468])).
fof(f1550, plain, (~ (e0 = op(e1, e1)) | (~ spl18_42 | spl18_93)), inference(forward_demodulation, [], [f1542, f474])).
fof(f1542, plain, (~ (e0 = op(op(e1, e1), e1)) | (~ spl18_42 | spl18_93)), inference(backward_demodulation, [], [f796, f474])).
fof(f1544, plain, (~ spl18_34 | ~ spl18_42), inference(avatar_split_clause, [], [f1536, f472, f438])).
fof(f1536, plain, (~ (e1 = op(e1, e3)) | ~ spl18_42), inference(backward_demodulation, [], [f173, f474])).
fof(f1543, plain, (~ spl18_26 | ~ spl18_42), inference(avatar_split_clause, [], [f1535, f472, f404])).
fof(f1535, plain, (~ (e1 = op(e2, e1)) | ~ spl18_42), inference(backward_demodulation, [], [f148, f474])).
fof(f1511, plain, (~ spl18_41 | ~ spl18_57), inference(avatar_split_clause, [], [f1504, f536, f468])).
fof(f1504, plain, (~ (e0 = op(e1, e1)) | ~ spl18_57), inference(backward_demodulation, [], [f145, f538])).
fof(f1503, plain, (~ spl18_51 | ~ spl18_64 | spl18_86), inference(avatar_split_clause, [], [f1502, f759, f565, f510])).
fof(f1502, plain, (~ (e2 = op(e0, e3)) | (~ spl18_64 | spl18_86)), inference(forward_demodulation, [], [f761, f567])).
fof(f1484, plain, (~ spl18_15 | ~ spl18_1 | spl18_102), inference(avatar_split_clause, [], [f1375, f840, f298, f357])).
fof(f1375, plain, (~ (e2 = op(e3, e0)) | (~ spl18_1 | spl18_102)), inference(forward_demodulation, [], [f842, f300])).
fof(f1440, plain, (~ spl18_53 | ~ spl18_54), inference(avatar_contradiction_clause, [], [f1439])).
fof(f1439, plain, ($false | (~ spl18_53 | ~ spl18_54)), inference(subsumption_resolution, [], [f1438, f187])).
fof(f1438, plain, ((e0 = e1) | (~ spl18_53 | ~ spl18_54)), inference(backward_demodulation, [], [f525, f521])).
fof(f1432, plain, (~ spl18_63 | ~ spl18_64), inference(avatar_contradiction_clause, [], [f1431])).
fof(f1431, plain, ($false | (~ spl18_63 | ~ spl18_64)), inference(subsumption_resolution, [], [f1430, f192])).
fof(f1430, plain, ((e2 = e3) | (~ spl18_63 | ~ spl18_64)), inference(backward_demodulation, [], [f567, f563])).
fof(f1429, plain, (~ spl18_59 | ~ spl18_51), inference(avatar_split_clause, [], [f1428, f510, f544])).
fof(f1428, plain, (~ (e2 = op(e0, e1)) | ~ spl18_51), inference(forward_demodulation, [], [f167, f512])).
fof(f1409, plain, (~ spl18_1 | ~ spl18_3), inference(avatar_contradiction_clause, [], [f1408])).
fof(f1408, plain, ($false | (~ spl18_1 | ~ spl18_3)), inference(subsumption_resolution, [], [f1407, f188])).
fof(f1407, plain, ((e0 = e2) | (~ spl18_1 | ~ spl18_3)), inference(forward_demodulation, [], [f308, f300])).
fof(f1403, plain, (~ spl18_18 | ~ spl18_20), inference(avatar_contradiction_clause, [], [f1402])).
fof(f1402, plain, ($false | (~ spl18_18 | ~ spl18_20)), inference(subsumption_resolution, [], [f1401, f191])).
fof(f1401, plain, ((e1 = e3) | (~ spl18_18 | ~ spl18_20)), inference(forward_demodulation, [], [f380, f372])).
fof(f1362, plain, (~ spl18_52 | ~ spl18_64), inference(avatar_split_clause, [], [f1361, f565, f514])).
fof(f1361, plain, (~ (e3 = op(e0, e3)) | ~ spl18_64), inference(forward_demodulation, [], [f165, f567])).
fof(f1354, plain, (~ spl18_34 | ~ spl18_18), inference(avatar_split_clause, [], [f1353, f370, f438])).
fof(f1353, plain, (~ (e1 = op(e1, e3)) | ~ spl18_18), inference(forward_demodulation, [], [f160, f372])).
fof(f1352, plain, (~ spl18_35 | ~ spl18_1 | ~ spl18_14 | spl18_103), inference(avatar_split_clause, [], [f1316, f845, f353, f298, f442])).
fof(f1316, plain, (~ (e2 = op(e1, e3)) | (~ spl18_1 | ~ spl18_14 | spl18_103)), inference(forward_demodulation, [], [f1315, f355])).
fof(f1315, plain, (~ (e2 = op(op(e3, e0), e3)) | (~ spl18_1 | spl18_103)), inference(forward_demodulation, [], [f847, f300])).
fof(f1338, plain, (~ spl18_26 | ~ spl18_28), inference(avatar_contradiction_clause, [], [f1337])).
fof(f1337, plain, ($false | (~ spl18_26 | ~ spl18_28)), inference(subsumption_resolution, [], [f1336, f191])).
fof(f1336, plain, ((e1 = e3) | (~ spl18_26 | ~ spl18_28)), inference(forward_demodulation, [], [f414, f406])).
fof(f1322, plain, (~ spl18_57 | ~ spl18_59), inference(avatar_contradiction_clause, [], [f1321])).
fof(f1321, plain, ($false | (~ spl18_57 | ~ spl18_59)), inference(subsumption_resolution, [], [f1320, f188])).
fof(f1320, plain, ((e0 = e2) | (~ spl18_57 | ~ spl18_59)), inference(backward_demodulation, [], [f546, f538])).
fof(f1308, plain, (~ spl18_49 | ~ spl18_1), inference(avatar_split_clause, [], [f1145, f298, f502])).
fof(f1145, plain, (~ (e0 = op(e0, e3)) | ~ spl18_1), inference(forward_demodulation, [], [f159, f300])).
fof(f1289, plain, (~ spl18_44 | ~ spl18_12), inference(avatar_split_clause, [], [f1288, f344, f480])).
fof(f1288, plain, (~ (e3 = op(e1, e1)) | ~ spl18_12), inference(forward_demodulation, [], [f149, f346])).
fof(f1284, plain, (~ spl18_17 | ~ spl18_21), inference(avatar_split_clause, [], [f1283, f383, f366])).
fof(f1283, plain, (~ (e0 = op(e2, e3)) | ~ spl18_21), inference(forward_demodulation, [], [f180, f385])).
fof(f1264, plain, (~ spl18_17 | ~ spl18_1), inference(avatar_split_clause, [], [f1176, f298, f366])).
fof(f1176, plain, (~ (e0 = op(e2, e3)) | ~ spl18_1), inference(forward_demodulation, [], [f162, f300])).
fof(f1261, plain, (~ spl18_9 | ~ spl18_12), inference(avatar_contradiction_clause, [], [f1260])).
fof(f1260, plain, ($false | (~ spl18_9 | ~ spl18_12)), inference(subsumption_resolution, [], [f1258, f189])).
fof(f1258, plain, ((e0 = e3) | (~ spl18_9 | ~ spl18_12)), inference(backward_demodulation, [], [f346, f334])).
fof(f1254, plain, (~ spl18_21 | ~ spl18_24), inference(avatar_contradiction_clause, [], [f1253])).
fof(f1253, plain, ($false | (~ spl18_21 | ~ spl18_24)), inference(subsumption_resolution, [], [f1252, f189])).
fof(f1252, plain, ((e0 = e3) | (~ spl18_21 | ~ spl18_24)), inference(backward_demodulation, [], [f397, f385])).
fof(f1247, plain, (~ spl18_34 | ~ spl18_35), inference(avatar_contradiction_clause, [], [f1246])).
fof(f1246, plain, ($false | (~ spl18_34 | ~ spl18_35)), inference(subsumption_resolution, [], [f1245, f190])).
fof(f1245, plain, ((e1 = e2) | (~ spl18_34 | ~ spl18_35)), inference(backward_demodulation, [], [f444, f440])).
fof(f1224, plain, (~ spl18_49 | ~ spl18_52), inference(avatar_contradiction_clause, [], [f1223])).
fof(f1223, plain, ($false | (~ spl18_49 | ~ spl18_52)), inference(subsumption_resolution, [], [f1222, f189])).
fof(f1222, plain, ((e0 = e3) | (~ spl18_49 | ~ spl18_52)), inference(forward_demodulation, [], [f516, f504])).
fof(f1221, plain, (~ spl18_54 | ~ spl18_55), inference(avatar_contradiction_clause, [], [f1220])).
fof(f1220, plain, ($false | (~ spl18_54 | ~ spl18_55)), inference(subsumption_resolution, [], [f1219, f190])).
fof(f1219, plain, ((e1 = e2) | (~ spl18_54 | ~ spl18_55)), inference(backward_demodulation, [], [f529, f525])).
fof(f1216, plain, (~ spl18_43 | ~ spl18_59), inference(avatar_split_clause, [], [f1213, f544, f476])).
fof(f1213, plain, (~ (e2 = op(e1, e1)) | ~ spl18_59), inference(backward_demodulation, [], [f145, f546])).
fof(f1201, plain, (~ spl18_49 | ~ spl18_64 | spl18_85), inference(avatar_contradiction_clause, [], [f1200])).
fof(f1200, plain, ($false | (~ spl18_49 | ~ spl18_64 | spl18_85)), inference(subsumption_resolution, [], [f1199, f567])).
fof(f1199, plain, (~ (op(e0, e0) = e3) | (~ spl18_49 | ~ spl18_64 | spl18_85)), inference(forward_demodulation, [], [f1092, f504])).
fof(f1092, plain, (~ (e3 = op(op(e0, e3), e0)) | (~ spl18_64 | spl18_85)), inference(backward_demodulation, [], [f757, f567])).
fof(f1194, plain, (~ spl18_44 | ~ spl18_40), inference(avatar_split_clause, [], [f1193, f463, f480])).
fof(f1193, plain, (~ (e3 = op(e1, e1)) | ~ spl18_40), inference(forward_demodulation, [], [f172, f465])).
fof(f1180, plain, (~ spl18_54 | ~ spl18_17 | ~ spl18_24 | spl18_95), inference(avatar_split_clause, [], [f1179, f806, f395, f366, f523])).
fof(f1179, plain, (~ (e1 = op(e0, e2)) | (~ spl18_17 | ~ spl18_24 | spl18_95)), inference(forward_demodulation, [], [f970, f368])).
fof(f970, plain, (~ (e1 = op(op(e2, e3), e2)) | (~ spl18_24 | spl18_95)), inference(backward_demodulation, [], [f808, f397])).
fof(f1152, plain, (~ spl18_9 | ~ spl18_1), inference(avatar_split_clause, [], [f1151, f298, f332])).
fof(f1151, plain, (~ (e0 = op(e3, e1)) | ~ spl18_1), inference(forward_demodulation, [], [f185, f300])).
fof(f1150, plain, (~ spl18_13 | ~ spl18_1), inference(avatar_split_clause, [], [f1149, f298, f349])).
fof(f1149, plain, (~ (e0 = op(e3, e0)) | ~ spl18_1), inference(forward_demodulation, [], [f183, f300])).
fof(f1144, plain, (~ spl18_1 | ~ spl18_4), inference(avatar_contradiction_clause, [], [f1143])).
fof(f1143, plain, ($false | (~ spl18_1 | ~ spl18_4)), inference(subsumption_resolution, [], [f1141, f189])).
fof(f1141, plain, ((e0 = e3) | (~ spl18_1 | ~ spl18_4)), inference(backward_demodulation, [], [f312, f300])).
fof(f1136, plain, (~ spl18_13 | ~ spl18_14), inference(avatar_contradiction_clause, [], [f1135])).
fof(f1135, plain, ($false | (~ spl18_13 | ~ spl18_14)), inference(subsumption_resolution, [], [f1134, f187])).
fof(f1134, plain, ((e0 = e1) | (~ spl18_13 | ~ spl18_14)), inference(backward_demodulation, [], [f355, f351])).
fof(f1131, plain, (~ spl18_17 | ~ spl18_19), inference(avatar_contradiction_clause, [], [f1130])).
fof(f1130, plain, ($false | (~ spl18_17 | ~ spl18_19)), inference(subsumption_resolution, [], [f1129, f188])).
fof(f1129, plain, ((e0 = e2) | (~ spl18_17 | ~ spl18_19)), inference(backward_demodulation, [], [f376, f368])).
fof(f1126, plain, (~ spl18_30 | ~ spl18_31), inference(avatar_contradiction_clause, [], [f1125])).
fof(f1125, plain, ($false | (~ spl18_30 | ~ spl18_31)), inference(subsumption_resolution, [], [f1124, f190])).
fof(f1124, plain, ((e1 = e2) | (~ spl18_30 | ~ spl18_31)), inference(backward_demodulation, [], [f427, f423])).
fof(f1119, plain, (~ spl18_38 | ~ spl18_40), inference(avatar_contradiction_clause, [], [f1118])).
fof(f1118, plain, ($false | (~ spl18_38 | ~ spl18_40)), inference(subsumption_resolution, [], [f1117, f191])).
fof(f1117, plain, ((e1 = e3) | (~ spl18_38 | ~ spl18_40)), inference(forward_demodulation, [], [f465, f457])).
fof(f1112, plain, (~ spl18_49 | ~ spl18_50), inference(avatar_contradiction_clause, [], [f1111])).
fof(f1111, plain, ($false | (~ spl18_49 | ~ spl18_50)), inference(subsumption_resolution, [], [f1110, f187])).
fof(f1110, plain, ((e0 = e1) | (~ spl18_49 | ~ spl18_50)), inference(backward_demodulation, [], [f508, f504])).
fof(f1097, plain, (~ spl18_56 | ~ spl18_64), inference(avatar_split_clause, [], [f1089, f565, f531])).
fof(f1089, plain, (~ (e3 = op(e0, e2)) | ~ spl18_64), inference(backward_demodulation, [], [f164, f567])).
fof(f1094, plain, (~ spl18_48 | ~ spl18_64), inference(avatar_split_clause, [], [f1086, f565, f497])).
fof(f1086, plain, (~ (e3 = op(e1, e0)) | ~ spl18_64), inference(backward_demodulation, [], [f139, f567])).
fof(f1083, plain, (~ spl18_62 | ~ spl18_14), inference(avatar_split_clause, [], [f1082, f353, f557])).
fof(f1082, plain, (~ (op(e0, e0) = e1) | ~ spl18_14), inference(forward_demodulation, [], [f141, f355])).
fof(f1081, plain, (~ spl18_62 | ~ spl18_50), inference(avatar_split_clause, [], [f1080, f506, f557])).
fof(f1080, plain, (~ (op(e0, e0) = e1) | ~ spl18_50), inference(forward_demodulation, [], [f165, f508])).
fof(f1073, plain, (~ spl18_54 | ~ spl18_38), inference(avatar_split_clause, [], [f1072, f455, f523])).
fof(f1072, plain, (~ (e1 = op(e0, e2)) | ~ spl18_38), inference(forward_demodulation, [], [f151, f457])).
fof(f1035, plain, (~ spl18_27 | ~ spl18_19), inference(avatar_split_clause, [], [f1034, f374, f408])).
fof(f1034, plain, (~ (e2 = op(e2, e1)) | ~ spl18_19), inference(forward_demodulation, [], [f179, f376])).
fof(f1020, plain, (~ spl18_3 | ~ spl18_4), inference(avatar_contradiction_clause, [], [f1019])).
fof(f1019, plain, ($false | (~ spl18_3 | ~ spl18_4)), inference(subsumption_resolution, [], [f1018, f192])).
fof(f1018, plain, ((e2 = e3) | (~ spl18_3 | ~ spl18_4)), inference(backward_demodulation, [], [f312, f308])).
fof(f1012, plain, (~ spl18_5 | ~ spl18_7), inference(avatar_contradiction_clause, [], [f1011])).
fof(f1011, plain, ($false | (~ spl18_5 | ~ spl18_7)), inference(subsumption_resolution, [], [f1010, f188])).
fof(f1010, plain, ((e0 = e2) | (~ spl18_5 | ~ spl18_7)), inference(backward_demodulation, [], [f325, f317])).
fof(f1000, plain, (~ spl18_4 | ~ spl18_12), inference(avatar_split_clause, [], [f998, f344, f310])).
fof(f998, plain, (~ (e3 = op(e3, e3)) | ~ spl18_12), inference(backward_demodulation, [], [f185, f346])).
fof(f996, plain, (~ spl18_14 | ~ spl18_15), inference(avatar_contradiction_clause, [], [f995])).
fof(f995, plain, ($false | (~ spl18_14 | ~ spl18_15)), inference(subsumption_resolution, [], [f994, f190])).
fof(f994, plain, ((e1 = e2) | (~ spl18_14 | ~ spl18_15)), inference(backward_demodulation, [], [f359, f355])).
fof(f987, plain, (~ spl18_18 | ~ spl18_19), inference(avatar_contradiction_clause, [], [f986])).
fof(f986, plain, ($false | (~ spl18_18 | ~ spl18_19)), inference(subsumption_resolution, [], [f985, f190])).
fof(f985, plain, ((e1 = e2) | (~ spl18_18 | ~ spl18_19)), inference(backward_demodulation, [], [f376, f372])).
fof(f977, plain, (~ spl18_18 | ~ spl18_24 | spl18_98), inference(avatar_split_clause, [], [f973, f819, f395, f370])).
fof(f973, plain, (~ (e1 = op(e2, e3)) | (~ spl18_24 | spl18_98)), inference(backward_demodulation, [], [f821, f397])).
fof(f953, plain, (~ spl18_33 | ~ spl18_35), inference(avatar_contradiction_clause, [], [f952])).
fof(f952, plain, ($false | (~ spl18_33 | ~ spl18_35)), inference(subsumption_resolution, [], [f951, f188])).
fof(f951, plain, ((e0 = e2) | (~ spl18_33 | ~ spl18_35)), inference(backward_demodulation, [], [f444, f436])).
fof(f949, plain, (~ spl18_35 | ~ spl18_36), inference(avatar_contradiction_clause, [], [f948])).
fof(f948, plain, ($false | (~ spl18_35 | ~ spl18_36)), inference(subsumption_resolution, [], [f947, f192])).
fof(f947, plain, ((e2 = e3) | (~ spl18_35 | ~ spl18_36)), inference(backward_demodulation, [], [f448, f444])).
fof(f942, plain, (~ spl18_34 | ~ spl18_38), inference(avatar_split_clause, [], [f939, f455, f438])).
fof(f939, plain, (~ (e1 = op(e1, e3)) | ~ spl18_38), inference(backward_demodulation, [], [f174, f457])).
fof(f912, plain, (~ spl18_50 | ~ spl18_51), inference(avatar_contradiction_clause, [], [f911])).
fof(f911, plain, ($false | (~ spl18_50 | ~ spl18_51)), inference(subsumption_resolution, [], [f910, f190])).
fof(f910, plain, ((e1 = e2) | (~ spl18_50 | ~ spl18_51)), inference(backward_demodulation, [], [f512, f508])).
fof(f904, plain, (~ spl18_36 | ~ spl18_52), inference(avatar_split_clause, [], [f901, f514, f446])).
fof(f901, plain, (~ (e3 = op(e1, e3)) | ~ spl18_52), inference(backward_demodulation, [], [f157, f516])).
fof(f900, plain, (~ spl18_49 | ~ spl18_53), inference(avatar_split_clause, [], [f896, f519, f502])).
fof(f896, plain, (~ (e0 = op(e0, e3)) | ~ spl18_53), inference(backward_demodulation, [], [f168, f521])).
fof(f892, plain, (~ spl18_49 | ~ spl18_57), inference(avatar_split_clause, [], [f887, f536, f502])).
fof(f887, plain, (~ (e0 = op(e0, e3)) | ~ spl18_57), inference(backward_demodulation, [], [f167, f538])).
fof(f890, plain, (~ spl18_9 | ~ spl18_57), inference(avatar_split_clause, [], [f885, f536, f332])).
fof(f885, plain, (~ (e0 = op(e3, e1)) | ~ spl18_57), inference(backward_demodulation, [], [f147, f538])).
fof(f877, plain, (~ spl18_57 | ~ spl18_61), inference(avatar_split_clause, [], [f868, f553, f536])).
fof(f868, plain, (~ (e0 = op(e0, e1)) | ~ spl18_61), inference(backward_demodulation, [], [f163, f555])).
fof(f874, plain, (~ spl18_45 | ~ spl18_61), inference(avatar_split_clause, [], [f865, f553, f485])).
fof(f865, plain, (~ (e0 = op(e1, e0)) | ~ spl18_61), inference(backward_demodulation, [], [f139, f555])).
fof(f864, plain, (~ spl18_3 | ~ spl18_101 | ~ spl18_106), inference(avatar_split_clause, [], [f296, f859, f836, f306])).
fof(f296, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), e3)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), e3)) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(e3, op(e3, e3))) & (e1 = op(op(e3, op(e3, e3)), e3)) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax29)).
fof(f863, plain, (~ spl18_3 | ~ spl18_105 | ~ spl18_104), inference(avatar_split_clause, [], [f295, f849, f854, f306])).
fof(f295, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e0 = op(op(e3, op(e3, e3)), e3)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e0 = op(op(e3, op(e3, e3)), e3)) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e1 = op(e3, op(e3, e3))) & (e0 = op(op(e3, op(e3, e3)), e3)) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax28)).
fof(f862, plain, (~ spl18_2 | ~ spl18_103 | ~ spl18_106), inference(avatar_split_clause, [], [f294, f859, f845, f302])).
fof(f294, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e0 = op(e3, op(e3, e3))) & (e2 = op(op(e3, op(e3, e3)), e3)) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax27)).
fof(f857, plain, (~ spl18_2 | ~ spl18_105 | ~ spl18_102), inference(avatar_split_clause, [], [f293, f840, f854, f302])).
fof(f293, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(op(e3, op(e3, e3)), e3)) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e2 = op(e3, op(e3, e3))) & (e0 = op(op(e3, op(e3, e3)), e3)) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax26)).
fof(f852, plain, (~ spl18_1 | ~ spl18_103 | ~ spl18_104), inference(avatar_split_clause, [], [f292, f849, f845, f298])).
fof(f292, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), e3)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e2 = op(op(e3, op(e3, e3)), e3)) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e1 = op(e3, op(e3, e3))) & (e2 = op(op(e3, op(e3, e3)), e3)) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax25)).
fof(f843, plain, (~ spl18_1 | ~ spl18_101 | ~ spl18_102), inference(avatar_split_clause, [], [f291, f840, f836, f298])).
fof(f291, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), e3)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(op(e3, op(e3, e3)), e3)) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e2 = op(e3, op(e3, e3))) & (e1 = op(op(e3, op(e3, e3)), e3)) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax24)).
fof(f834, plain, (~ spl18_24 | ~ spl18_95 | ~ spl18_100), inference(avatar_split_clause, [], [f290, f829, f806, f395])).
fof(f290, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e1 = op(op(e2, op(e2, e2)), e2)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e1 = op(op(e2, op(e2, e2)), e2)) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e0 = op(e2, op(e2, e2))) & (e1 = op(op(e2, op(e2, e2)), e2)) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax23)).
fof(f833, plain, (~ spl18_24 | ~ spl18_99 | ~ spl18_98), inference(avatar_split_clause, [], [f289, f819, f824, f395])).
fof(f289, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e0 = op(op(e2, op(e2, e2)), e2)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e0 = op(op(e2, op(e2, e2)), e2)) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e1 = op(e2, op(e2, e2))) & (e0 = op(op(e2, op(e2, e2)), e2)) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax22)).
fof(f832, plain, (~ spl18_22 | ~ spl18_97 | ~ spl18_100), inference(avatar_split_clause, [], [f288, f829, f815, f387])).
fof(f288, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e3 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e3 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e2, op(e2, e2))) & (e3 = op(op(e2, op(e2, e2)), e2)) & (e1 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax21)).
fof(f827, plain, (~ spl18_22 | ~ spl18_99 | ~ spl18_96), inference(avatar_split_clause, [], [f287, f810, f824, f387])).
fof(f287, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e0 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e0 = op(op(e2, op(e2, e2)), e2)) | ~ (e1 = op(e2, e2))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e3 = op(e2, op(e2, e2))) & (e0 = op(op(e2, op(e2, e2)), e2)) & (e1 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax20)).
fof(f822, plain, (~ spl18_21 | ~ spl18_97 | ~ spl18_98), inference(avatar_split_clause, [], [f286, f819, f815, f383])).
fof(f286, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e3 = op(op(e2, op(e2, e2)), e2)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e3 = op(op(e2, op(e2, e2)), e2)) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e2, op(e2, e2))) & (e3 = op(op(e2, op(e2, e2)), e2)) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax19)).
fof(f813, plain, (~ spl18_21 | ~ spl18_95 | ~ spl18_96), inference(avatar_split_clause, [], [f285, f810, f806, f383])).
fof(f285, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e1 = op(op(e2, op(e2, e2)), e2)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e1 = op(op(e2, op(e2, e2)), e2)) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e2, op(e2, e2))) & (e1 = op(op(e2, op(e2, e2)), e2)) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax18)).
fof(f804, plain, (~ spl18_44 | ~ spl18_89 | ~ spl18_94), inference(avatar_split_clause, [], [f284, f799, f776, f480])).
fof(f284, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(op(e1, op(e1, e1)), e1)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(op(e1, op(e1, e1)), e1)) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e0 = op(e1, op(e1, e1))) & (e2 = op(op(e1, op(e1, e1)), e1)) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax17)).
fof(f803, plain, (~ spl18_44 | ~ spl18_93 | ~ spl18_92), inference(avatar_split_clause, [], [f283, f789, f794, f480])).
fof(f283, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(op(e1, op(e1, e1)), e1)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(op(e1, op(e1, e1)), e1)) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e2 = op(e1, op(e1, e1))) & (e0 = op(op(e1, op(e1, e1)), e1)) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax16)).
fof(f802, plain, (~ spl18_43 | ~ spl18_91 | ~ spl18_94), inference(avatar_split_clause, [], [f282, f799, f785, f476])).
fof(f282, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), e1)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), e1)) | ~ (e2 = op(e1, e1))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e1, op(e1, e1))) & (e3 = op(op(e1, op(e1, e1)), e1)) & (e2 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax15)).
fof(f797, plain, (~ spl18_43 | ~ spl18_93 | ~ spl18_90), inference(avatar_split_clause, [], [f281, f780, f794, f476])).
fof(f281, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e0 = op(op(e1, op(e1, e1)), e1)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e0 = op(op(e1, op(e1, e1)), e1)) | ~ (e2 = op(e1, e1))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e3 = op(e1, op(e1, e1))) & (e0 = op(op(e1, op(e1, e1)), e1)) & (e2 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax14)).
fof(f792, plain, (~ spl18_41 | ~ spl18_91 | ~ spl18_92), inference(avatar_split_clause, [], [f280, f789, f785, f468])).
fof(f280, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), e1)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(op(e1, op(e1, e1)), e1)) | ~ (e0 = op(e1, e1))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e2 = op(e1, op(e1, e1))) & (e3 = op(op(e1, op(e1, e1)), e1)) & (e0 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax13)).
fof(f783, plain, (~ spl18_41 | ~ spl18_89 | ~ spl18_90), inference(avatar_split_clause, [], [f279, f780, f776, f468])).
fof(f279, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e2 = op(op(e1, op(e1, e1)), e1)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e2 = op(op(e1, op(e1, e1)), e1)) | ~ (e0 = op(e1, e1))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e3 = op(e1, op(e1, e1))) & (e2 = op(op(e1, op(e1, e1)), e1)) & (e0 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax12)).
fof(f774, plain, (~ spl18_64 | ~ spl18_83 | ~ spl18_88), inference(avatar_split_clause, [], [f278, f769, f746, f565])).
fof(f278, plain, (~ (e1 = op(e0, op(e0, e0))) | ~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e1 = op(e0, op(e0, e0))) | ~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e1 = op(e0, op(e0, e0))) & (e2 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e3)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax11)).
fof(f773, plain, (~ spl18_64 | ~ spl18_87 | ~ spl18_86), inference(avatar_split_clause, [], [f277, f759, f764, f565])).
fof(f277, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e3)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e2 = op(e0, op(e0, e0))) & (e1 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e3)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax10)).
fof(f772, plain, (~ spl18_63 | ~ spl18_85 | ~ spl18_88), inference(avatar_split_clause, [], [f276, f769, f755, f561])).
fof(f276, plain, (~ (e1 = op(e0, op(e0, e0))) | ~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(e0, op(e0, e0))) | ~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(e0, op(e0, e0))) & (e3 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax9)).
fof(f767, plain, (~ spl18_63 | ~ spl18_87 | ~ spl18_84), inference(avatar_split_clause, [], [f275, f750, f764, f561])).
fof(f275, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (e1 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e3 = op(e0, op(e0, e0))) & (e1 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax8)).
fof(f762, plain, (~ spl18_62 | ~ spl18_85 | ~ spl18_86), inference(avatar_split_clause, [], [f274, f759, f755, f557])).
fof(f274, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (e3 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e2 = op(e0, op(e0, e0))) & (e3 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax7)).
fof(f753, plain, (~ spl18_62 | ~ spl18_83 | ~ spl18_84), inference(avatar_split_clause, [], [f273, f750, f746, f557])).
fof(f273, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (e2 = op(op(e0, op(e0, e0)), e0)) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e0, op(e0, e0))) & (e2 = op(op(e0, op(e0, e0)), e0)) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax6)).
fof(f744, plain, (spl18_82 | spl18_81 | spl18_80 | spl18_79 | spl18_78 | spl18_77 | spl18_76 | spl18_75 | spl18_74 | spl18_73 | spl18_72 | spl18_71 | spl18_70 | spl18_69 | spl18_68 | spl18_4), inference(avatar_split_clause, [], [f262, f310, f632, f639, f646, f653, f660, f667, f674, f681, f688, f695, f702, f709, f716, f723, f730])).
fof(f730, plain, (spl18_82 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl18_82])])).
fof(f723, plain, (spl18_81 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl18_81])])).
fof(f716, plain, (spl18_80 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl18_80])])).
fof(f709, plain, (spl18_79 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl18_79])])).
fof(f702, plain, (spl18_78 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl18_78])])).
fof(f695, plain, (spl18_77 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl18_77])])).
fof(f688, plain, (spl18_76 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl18_76])])).
fof(f681, plain, (spl18_75 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl18_75])])).
fof(f674, plain, (spl18_74 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl18_74])])).
fof(f667, plain, (spl18_73 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f660, plain, (spl18_72 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl18_72])])).
fof(f653, plain, (spl18_71 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl18_71])])).
fof(f646, plain, (spl18_70 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl18_70])])).
fof(f639, plain, (spl18_69 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl18_69])])).
fof(f632, plain, (spl18_68 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl18_68])])).
fof(f262, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f72])).
fof(f72, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3))) | sP17 | sP16 | sP15) & (((e3 = op(e3, e3)) & ~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f5, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, (((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((e0 = op(e1, e1)) & ~ (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((e0 = op(e1, e1)) & ~ (e0 = op(e0, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((e0 = op(e2, e2)) & ~ (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((e0 = op(e2, e2)) & ~ (e0 = op(e0, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, (((e0 = op(e3, e3)) & ~ (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> ((e0 = op(e3, e3)) & ~ (e0 = op(e0, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, (((op(e0, e0) = e1) & ~ (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> ((op(e0, e0) = e1) & ~ (e1 = op(e1, e0)) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, (((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> ((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, (((e1 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> ((e1 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, (((e1 = op(e3, e3)) & ~ (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> ((e1 = op(e3, e3)) & ~ (e1 = op(e1, e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, (((op(e0, e0) = e2) & ~ (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> ((op(e0, e0) = e2) & ~ (e2 = op(e2, e0)) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, (((e2 = op(e1, e1)) & ~ (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> ((e2 = op(e1, e1)) & ~ (e2 = op(e2, e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, (((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> ((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, (((e2 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> ((e2 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, (((op(e0, e0) = e3) & ~ (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> ((op(e0, e0) = e3) & ~ (e3 = op(e3, e0)) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, (((e3 = op(e1, e1)) & ~ (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> ((e3 = op(e1, e1)) & ~ (e3 = op(e3, e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, (((e3 = op(e2, e2)) & ~ (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> ((e3 = op(e2, e2)) & ~ (e3 = op(e3, e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f69, plain, ((((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))) | ~ sP15), inference(usedef, [], [e69])).
fof(e69, plain, (sP15 <=> (((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f70, plain, ((((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1))) | ~ sP16), inference(usedef, [], [e70])).
fof(e70, plain, (sP16 <=> (((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f71, plain, ((((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2))) | ~ sP17), inference(usedef, [], [e71])).
fof(e71, plain, (sP17 <=> (((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f5, plain, (((((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3))) & ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3))) & ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3))) | (((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2))) | (((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1))) | (((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))))) & (((e3 = op(e3, e3)) & ~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ((e3 = op(e2, e2)) & ~ (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | ((e3 = op(e1, e1)) & ~ (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | ((op(e0, e0) = e3) & ~ (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | ((e2 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | ((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ((e2 = op(e1, e1)) & ~ (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | ((op(e0, e0) = e2) & ~ (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | ((e1 = op(e3, e3)) & ~ (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | ((e1 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | ((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ((op(e0, e0) = e1) & ~ (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | ((e0 = op(e3, e3)) & ~ (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ((e0 = op(e2, e2)) & ~ (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ((e0 = op(e1, e1)) & ~ (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax5)).
fof(f743, plain, (spl18_82 | spl18_81 | spl18_80 | spl18_79 | spl18_78 | spl18_77 | spl18_76 | spl18_75 | spl18_74 | spl18_73 | spl18_72 | spl18_71 | spl18_70 | spl18_69 | spl18_68 | ~ spl18_4), inference(avatar_split_clause, [], [f263, f310, f632, f639, f646, f653, f660, f667, f674, f681, f688, f695, f702, f709, f716, f723, f730])).
fof(f263, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f72])).
fof(f741, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_64 | spl18_49), inference(avatar_split_clause, [], [f265, f502, f565, f602, f612, f622])).
fof(f622, plain, (spl18_67 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl18_67])])).
fof(f612, plain, (spl18_66 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl18_66])])).
fof(f602, plain, (spl18_65 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl18_65])])).
fof(f265, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f740, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_44 | spl18_34), inference(avatar_split_clause, [], [f266, f438, f480, f602, f612, f622])).
fof(f266, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f739, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_24 | spl18_19), inference(avatar_split_clause, [], [f267, f374, f395, f602, f612, f622])).
fof(f267, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f738, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_49 | spl18_64), inference(avatar_split_clause, [], [f269, f565, f502, f602, f612, f622])).
fof(f269, plain, ((op(e0, e0) = e3) | ~ (e0 = op(e0, e3)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f737, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_34 | spl18_44), inference(avatar_split_clause, [], [f270, f480, f438, f602, f612, f622])).
fof(f270, plain, ((e3 = op(e1, e1)) | ~ (e1 = op(e1, e3)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f736, plain, (spl18_67 | spl18_66 | spl18_65 | ~ spl18_19 | spl18_24), inference(avatar_split_clause, [], [f271, f395, f374, f602, f612, f622])).
fof(f271, plain, ((e3 = op(e2, e2)) | ~ (e2 = op(e2, e3)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f72])).
fof(f735, plain, (~ spl18_82 | spl18_61), inference(avatar_split_clause, [], [f259, f553, f730])).
fof(f259, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f90])).
fof(f90, plain, (((e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f734, plain, (~ spl18_82 | ~ spl18_61), inference(avatar_split_clause, [], [f260, f553, f730])).
fof(f260, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f90])).
fof(f728, plain, (~ spl18_81 | spl18_62), inference(avatar_split_clause, [], [f256, f557, f723])).
fof(f256, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (((e0 = op(e1, e1)) & ~ (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f727, plain, (~ spl18_81 | ~ spl18_57), inference(avatar_split_clause, [], [f257, f536, f723])).
fof(f257, plain, (~ (e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f89])).
fof(f726, plain, (~ spl18_81 | spl18_41), inference(avatar_split_clause, [], [f258, f468, f723])).
fof(f258, plain, ((e0 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f89])).
fof(f721, plain, (~ spl18_80 | spl18_63), inference(avatar_split_clause, [], [f253, f561, f716])).
fof(f253, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f88])).
fof(f88, plain, (((e0 = op(e2, e2)) & ~ (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f720, plain, (~ spl18_80 | ~ spl18_53), inference(avatar_split_clause, [], [f254, f519, f716])).
fof(f254, plain, (~ (e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f88])).
fof(f719, plain, (~ spl18_80 | spl18_21), inference(avatar_split_clause, [], [f255, f383, f716])).
fof(f255, plain, ((e0 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f88])).
fof(f714, plain, (~ spl18_79 | spl18_64), inference(avatar_split_clause, [], [f250, f565, f709])).
fof(f250, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((e0 = op(e3, e3)) & ~ (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f713, plain, (~ spl18_79 | ~ spl18_49), inference(avatar_split_clause, [], [f251, f502, f709])).
fof(f251, plain, (~ (e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f87])).
fof(f712, plain, (~ spl18_79 | spl18_1), inference(avatar_split_clause, [], [f252, f298, f709])).
fof(f252, plain, ((e0 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f87])).
fof(f707, plain, (~ spl18_78 | spl18_41), inference(avatar_split_clause, [], [f247, f468, f702])).
fof(f247, plain, ((e0 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (((op(e0, e0) = e1) & ~ (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f706, plain, (~ spl18_78 | ~ spl18_46), inference(avatar_split_clause, [], [f248, f489, f702])).
fof(f248, plain, (~ (e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f705, plain, (~ spl18_78 | spl18_62), inference(avatar_split_clause, [], [f249, f557, f702])).
fof(f249, plain, ((op(e0, e0) = e1) | ~ sP4), inference(cnf_transformation, [], [f86])).
fof(f700, plain, (~ spl18_77 | spl18_42), inference(avatar_split_clause, [], [f244, f472, f695])).
fof(f244, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (((e1 = op(e1, e1)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f699, plain, (~ spl18_77 | ~ spl18_42), inference(avatar_split_clause, [], [f245, f472, f695])).
fof(f245, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f85])).
fof(f693, plain, (~ spl18_76 | spl18_43), inference(avatar_split_clause, [], [f241, f476, f688])).
fof(f241, plain, ((e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (((e1 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f692, plain, (~ spl18_76 | ~ spl18_38), inference(avatar_split_clause, [], [f242, f455, f688])).
fof(f242, plain, (~ (e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f84])).
fof(f691, plain, (~ spl18_76 | spl18_22), inference(avatar_split_clause, [], [f243, f387, f688])).
fof(f243, plain, ((e1 = op(e2, e2)) | ~ sP6), inference(cnf_transformation, [], [f84])).
fof(f686, plain, (~ spl18_75 | spl18_44), inference(avatar_split_clause, [], [f238, f480, f681])).
fof(f238, plain, ((e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f83, plain, (((e1 = op(e3, e3)) & ~ (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f685, plain, (~ spl18_75 | ~ spl18_34), inference(avatar_split_clause, [], [f239, f438, f681])).
fof(f239, plain, (~ (e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f684, plain, (~ spl18_75 | spl18_2), inference(avatar_split_clause, [], [f240, f302, f681])).
fof(f240, plain, ((e1 = op(e3, e3)) | ~ sP7), inference(cnf_transformation, [], [f83])).
fof(f679, plain, (~ spl18_74 | spl18_21), inference(avatar_split_clause, [], [f235, f383, f674])).
fof(f235, plain, ((e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f82, plain, (((op(e0, e0) = e2) & ~ (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f678, plain, (~ spl18_74 | ~ spl18_31), inference(avatar_split_clause, [], [f236, f425, f674])).
fof(f236, plain, (~ (e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f677, plain, (~ spl18_74 | spl18_63), inference(avatar_split_clause, [], [f237, f561, f674])).
fof(f237, plain, ((op(e0, e0) = e2) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f672, plain, (~ spl18_73 | spl18_22), inference(avatar_split_clause, [], [f232, f387, f667])).
fof(f232, plain, ((e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f81, plain, (((e2 = op(e1, e1)) & ~ (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f671, plain, (~ spl18_73 | ~ spl18_27), inference(avatar_split_clause, [], [f233, f408, f667])).
fof(f233, plain, (~ (e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f670, plain, (~ spl18_73 | spl18_43), inference(avatar_split_clause, [], [f234, f476, f667])).
fof(f234, plain, ((e2 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f665, plain, (~ spl18_72 | spl18_23), inference(avatar_split_clause, [], [f229, f391, f660])).
fof(f229, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f80])).
fof(f80, plain, (((e2 = op(e2, e2)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f664, plain, (~ spl18_72 | ~ spl18_23), inference(avatar_split_clause, [], [f230, f391, f660])).
fof(f230, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f80])).
fof(f658, plain, (~ spl18_71 | spl18_24), inference(avatar_split_clause, [], [f226, f395, f653])).
fof(f226, plain, ((e3 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f79, plain, (((e2 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f657, plain, (~ spl18_71 | ~ spl18_19), inference(avatar_split_clause, [], [f227, f374, f653])).
fof(f227, plain, (~ (e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f656, plain, (~ spl18_71 | spl18_3), inference(avatar_split_clause, [], [f228, f306, f653])).
fof(f228, plain, ((e2 = op(e3, e3)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f651, plain, (~ spl18_70 | spl18_1), inference(avatar_split_clause, [], [f223, f298, f646])).
fof(f223, plain, ((e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((op(e0, e0) = e3) & ~ (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f650, plain, (~ spl18_70 | ~ spl18_16), inference(avatar_split_clause, [], [f224, f361, f646])).
fof(f224, plain, (~ (e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f78])).
fof(f649, plain, (~ spl18_70 | spl18_64), inference(avatar_split_clause, [], [f225, f565, f646])).
fof(f225, plain, ((op(e0, e0) = e3) | ~ sP12), inference(cnf_transformation, [], [f78])).
fof(f644, plain, (~ spl18_69 | spl18_2), inference(avatar_split_clause, [], [f220, f302, f639])).
fof(f220, plain, ((e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((e3 = op(e1, e1)) & ~ (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f643, plain, (~ spl18_69 | ~ spl18_12), inference(avatar_split_clause, [], [f221, f344, f639])).
fof(f221, plain, (~ (e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f77])).
fof(f642, plain, (~ spl18_69 | spl18_44), inference(avatar_split_clause, [], [f222, f480, f639])).
fof(f222, plain, ((e3 = op(e1, e1)) | ~ sP13), inference(cnf_transformation, [], [f77])).
fof(f637, plain, (~ spl18_68 | spl18_3), inference(avatar_split_clause, [], [f217, f306, f632])).
fof(f217, plain, ((e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (((e3 = op(e2, e2)) & ~ (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f636, plain, (~ spl18_68 | ~ spl18_8), inference(avatar_split_clause, [], [f218, f327, f632])).
fof(f218, plain, (~ (e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f76])).
fof(f635, plain, (~ spl18_68 | spl18_24), inference(avatar_split_clause, [], [f219, f395, f632])).
fof(f219, plain, ((e3 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f76])).
fof(f630, plain, (~ spl18_67 | ~ spl18_41 | spl18_46), inference(avatar_split_clause, [], [f210, f489, f468, f622])).
fof(f210, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0))) & ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0))) & ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))) | ~ sP15), inference(nnf_transformation, [], [f69])).
fof(f629, plain, (~ spl18_67 | ~ spl18_21 | spl18_31), inference(avatar_split_clause, [], [f211, f425, f383, f622])).
fof(f211, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f628, plain, (~ spl18_67 | ~ spl18_1 | spl18_16), inference(avatar_split_clause, [], [f212, f361, f298, f622])).
fof(f212, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f627, plain, (~ spl18_67 | ~ spl18_46 | spl18_41), inference(avatar_split_clause, [], [f214, f468, f489, f622])).
fof(f214, plain, ((e0 = op(e1, e1)) | ~ (e1 = op(e1, e0)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f626, plain, (~ spl18_67 | ~ spl18_31 | spl18_21), inference(avatar_split_clause, [], [f215, f383, f425, f622])).
fof(f215, plain, ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f625, plain, (~ spl18_67 | ~ spl18_16 | spl18_1), inference(avatar_split_clause, [], [f216, f298, f361, f622])).
fof(f216, plain, ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP15), inference(cnf_transformation, [], [f75])).
fof(f620, plain, (~ spl18_66 | ~ spl18_62 | spl18_57), inference(avatar_split_clause, [], [f201, f536, f557, f612])).
fof(f201, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1))) & ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1))) | ~ sP16), inference(nnf_transformation, [], [f70])).
fof(f619, plain, (~ spl18_66 | ~ spl18_22 | spl18_27), inference(avatar_split_clause, [], [f203, f408, f387, f612])).
fof(f203, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f618, plain, (~ spl18_66 | ~ spl18_2 | spl18_12), inference(avatar_split_clause, [], [f204, f344, f302, f612])).
fof(f204, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f617, plain, (~ spl18_66 | ~ spl18_57 | spl18_62), inference(avatar_split_clause, [], [f205, f557, f536, f612])).
fof(f205, plain, ((op(e0, e0) = e1) | ~ (e0 = op(e0, e1)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f616, plain, (~ spl18_66 | ~ spl18_27 | spl18_22), inference(avatar_split_clause, [], [f207, f387, f408, f612])).
fof(f207, plain, ((e1 = op(e2, e2)) | ~ (e2 = op(e2, e1)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f615, plain, (~ spl18_66 | ~ spl18_12 | spl18_2), inference(avatar_split_clause, [], [f208, f302, f344, f612])).
fof(f208, plain, ((e1 = op(e3, e3)) | ~ (e3 = op(e3, e1)) | ~ sP16), inference(cnf_transformation, [], [f74])).
fof(f610, plain, (~ spl18_65 | ~ spl18_63 | spl18_53), inference(avatar_split_clause, [], [f193, f519, f561, f602])).
fof(f193, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2))) & ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2))) | ~ sP17), inference(nnf_transformation, [], [f71])).
fof(f609, plain, (~ spl18_65 | ~ spl18_43 | spl18_38), inference(avatar_split_clause, [], [f194, f455, f476, f602])).
fof(f194, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f608, plain, (~ spl18_65 | ~ spl18_3 | spl18_8), inference(avatar_split_clause, [], [f196, f327, f306, f602])).
fof(f196, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f607, plain, (~ spl18_65 | ~ spl18_53 | spl18_63), inference(avatar_split_clause, [], [f197, f561, f519, f602])).
fof(f197, plain, ((op(e0, e0) = e2) | ~ (e0 = op(e0, e2)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f606, plain, (~ spl18_65 | ~ spl18_38 | spl18_43), inference(avatar_split_clause, [], [f198, f476, f455, f602])).
fof(f198, plain, ((e2 = op(e1, e1)) | ~ (e1 = op(e1, e2)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f605, plain, (~ spl18_65 | ~ spl18_8 | spl18_3), inference(avatar_split_clause, [], [f200, f306, f327, f602])).
fof(f200, plain, ((e2 = op(e3, e3)) | ~ (e3 = op(e3, e2)) | ~ sP17), inference(cnf_transformation, [], [f73])).
fof(f600, plain, (spl18_61 | spl18_57 | spl18_53 | spl18_49), inference(avatar_split_clause, [], [f107, f502, f519, f536, f553])).
fof(f107, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax2)).
fof(f599, plain, (spl18_61 | spl18_45 | spl18_29 | spl18_13), inference(avatar_split_clause, [], [f108, f349, f417, f485, f553])).
fof(f108, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f598, plain, (spl18_62 | spl18_58 | spl18_54 | spl18_50), inference(avatar_split_clause, [], [f109, f506, f523, f540, f557])).
fof(f109, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f597, plain, (spl18_62 | spl18_46 | spl18_30 | spl18_14), inference(avatar_split_clause, [], [f110, f353, f421, f489, f557])).
fof(f110, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f596, plain, (spl18_63 | spl18_59 | spl18_55 | spl18_51), inference(avatar_split_clause, [], [f111, f510, f527, f544, f561])).
fof(f111, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f595, plain, (spl18_63 | spl18_47 | spl18_31 | spl18_15), inference(avatar_split_clause, [], [f112, f357, f425, f493, f561])).
fof(f112, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f594, plain, (spl18_64 | spl18_60 | spl18_56 | spl18_52), inference(avatar_split_clause, [], [f113, f514, f531, f548, f565])).
fof(f113, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f593, plain, (spl18_64 | spl18_48 | spl18_32 | spl18_16), inference(avatar_split_clause, [], [f114, f361, f429, f497, f565])).
fof(f114, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f592, plain, (spl18_45 | spl18_41 | spl18_37 | spl18_33), inference(avatar_split_clause, [], [f115, f434, f451, f468, f485])).
fof(f115, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f591, plain, (spl18_57 | spl18_41 | spl18_25 | spl18_9), inference(avatar_split_clause, [], [f116, f332, f400, f468, f536])).
fof(f116, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f590, plain, (spl18_46 | spl18_42 | spl18_38 | spl18_34), inference(avatar_split_clause, [], [f117, f438, f455, f472, f489])).
fof(f117, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f588, plain, (spl18_47 | spl18_43 | spl18_39 | spl18_35), inference(avatar_split_clause, [], [f119, f442, f459, f476, f493])).
fof(f119, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f587, plain, (spl18_59 | spl18_43 | spl18_27 | spl18_11), inference(avatar_split_clause, [], [f120, f340, f408, f476, f544])).
fof(f120, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f586, plain, (spl18_48 | spl18_44 | spl18_40 | spl18_36), inference(avatar_split_clause, [], [f121, f446, f463, f480, f497])).
fof(f121, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f585, plain, (spl18_60 | spl18_44 | spl18_28 | spl18_12), inference(avatar_split_clause, [], [f122, f344, f412, f480, f548])).
fof(f122, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f584, plain, (spl18_29 | spl18_25 | spl18_21 | spl18_17), inference(avatar_split_clause, [], [f123, f366, f383, f400, f417])).
fof(f123, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f583, plain, (spl18_53 | spl18_37 | spl18_21 | spl18_5), inference(avatar_split_clause, [], [f124, f315, f383, f451, f519])).
fof(f124, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f582, plain, (spl18_30 | spl18_26 | spl18_22 | spl18_18), inference(avatar_split_clause, [], [f125, f370, f387, f404, f421])).
fof(f125, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f581, plain, (spl18_54 | spl18_38 | spl18_22 | spl18_6), inference(avatar_split_clause, [], [f126, f319, f387, f455, f523])).
fof(f126, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f580, plain, (spl18_31 | spl18_27 | spl18_23 | spl18_19), inference(avatar_split_clause, [], [f127, f374, f391, f408, f425])).
fof(f127, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f579, plain, (spl18_55 | spl18_39 | spl18_23 | spl18_7), inference(avatar_split_clause, [], [f128, f323, f391, f459, f527])).
fof(f128, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f578, plain, (spl18_32 | spl18_28 | spl18_24 | spl18_20), inference(avatar_split_clause, [], [f129, f378, f395, f412, f429])).
fof(f129, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f577, plain, (spl18_56 | spl18_40 | spl18_24 | spl18_8), inference(avatar_split_clause, [], [f130, f327, f395, f463, f531])).
fof(f130, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f576, plain, (spl18_13 | spl18_9 | spl18_5 | spl18_1), inference(avatar_split_clause, [], [f131, f298, f315, f332, f349])).
fof(f131, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f575, plain, (spl18_49 | spl18_33 | spl18_17 | spl18_1), inference(avatar_split_clause, [], [f132, f298, f366, f434, f502])).
fof(f132, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f574, plain, (spl18_14 | spl18_10 | spl18_6 | spl18_2), inference(avatar_split_clause, [], [f133, f302, f319, f336, f353])).
fof(f133, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f573, plain, (spl18_50 | spl18_34 | spl18_18 | spl18_2), inference(avatar_split_clause, [], [f134, f302, f370, f438, f506])).
fof(f134, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f572, plain, (spl18_15 | spl18_11 | spl18_7 | spl18_3), inference(avatar_split_clause, [], [f135, f306, f323, f340, f357])).
fof(f135, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f571, plain, (spl18_51 | spl18_35 | spl18_19 | spl18_3), inference(avatar_split_clause, [], [f136, f306, f374, f442, f510])).
fof(f136, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f570, plain, (spl18_16 | spl18_12 | spl18_8 | spl18_4), inference(avatar_split_clause, [], [f137, f310, f327, f344, f361])).
fof(f137, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f569, plain, (spl18_52 | spl18_36 | spl18_20 | spl18_4), inference(avatar_split_clause, [], [f138, f310, f378, f446, f514])).
fof(f138, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f568, plain, (spl18_61 | spl18_62 | spl18_63 | spl18_64), inference(avatar_split_clause, [], [f91, f565, f561, f557, f553])).
fof(f91, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG161+1.p', ax1)).
fof(f551, plain, (spl18_57 | spl18_58 | spl18_59 | spl18_60), inference(avatar_split_clause, [], [f92, f548, f544, f540, f536])).
fof(f92, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f534, plain, (spl18_53 | spl18_54 | spl18_55 | spl18_56), inference(avatar_split_clause, [], [f93, f531, f527, f523, f519])).
fof(f93, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f517, plain, (spl18_49 | spl18_50 | spl18_51 | spl18_52), inference(avatar_split_clause, [], [f94, f514, f510, f506, f502])).
fof(f94, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f500, plain, (spl18_45 | spl18_46 | spl18_47 | spl18_48), inference(avatar_split_clause, [], [f95, f497, f493, f489, f485])).
fof(f95, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f483, plain, (spl18_41 | spl18_42 | spl18_43 | spl18_44), inference(avatar_split_clause, [], [f96, f480, f476, f472, f468])).
fof(f96, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f466, plain, (spl18_37 | spl18_38 | spl18_39 | spl18_40), inference(avatar_split_clause, [], [f97, f463, f459, f455, f451])).
fof(f97, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f449, plain, (spl18_33 | spl18_34 | spl18_35 | spl18_36), inference(avatar_split_clause, [], [f98, f446, f442, f438, f434])).
fof(f98, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f432, plain, (spl18_29 | spl18_30 | spl18_31 | spl18_32), inference(avatar_split_clause, [], [f99, f429, f425, f421, f417])).
fof(f99, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f415, plain, (spl18_25 | spl18_26 | spl18_27 | spl18_28), inference(avatar_split_clause, [], [f100, f412, f408, f404, f400])).
fof(f100, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f398, plain, (spl18_21 | spl18_22 | spl18_23 | spl18_24), inference(avatar_split_clause, [], [f101, f395, f391, f387, f383])).
fof(f101, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f381, plain, (spl18_17 | spl18_18 | spl18_19 | spl18_20), inference(avatar_split_clause, [], [f102, f378, f374, f370, f366])).
fof(f102, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f364, plain, (spl18_13 | spl18_14 | spl18_15 | spl18_16), inference(avatar_split_clause, [], [f103, f361, f357, f353, f349])).
fof(f103, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f347, plain, (spl18_9 | spl18_10 | spl18_11 | spl18_12), inference(avatar_split_clause, [], [f104, f344, f340, f336, f332])).
fof(f104, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f330, plain, (spl18_5 | spl18_6 | spl18_7 | spl18_8), inference(avatar_split_clause, [], [f105, f327, f323, f319, f315])).
fof(f105, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f313, plain, (spl18_1 | spl18_2 | spl18_3 | spl18_4), inference(avatar_split_clause, [], [f106, f310, f306, f302, f298])).
fof(f106, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).