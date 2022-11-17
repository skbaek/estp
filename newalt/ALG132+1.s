fof(f3249, plain, $false, inference(avatar_sat_refutation, [], [f219, f236, f253, f270, f304, f338, f355, f372, f389, f406, f423, f440, f457, f474, f477, f478, f482, f484, f487, f488, f490, f491, f494, f495, f498, f500, f501, f503, f504, f505, f506, f515, f520, f525, f530, f539, f544, f549, f554, f563, f568, f573, f578, f583, f588, f593, f598, f625, f634, f671, f685, f695, f701, f713, f718, f730, f731, f732, f734, f744, f746, f753, f755, f776, f785, f796, f822, f827, f844, f847, f858, f862, f865, f898, f910, f920, f928, f932, f939, f948, f952, f970, f981, f986, f990, f1004, f1007, f1009, f1011, f1053, f1061, f1082, f1097, f1101, f1132, f1152, f1155, f1190, f1278, f1314, f1347, f1350, f1396, f1427, f1446, f1493, f1496, f1517, f1527, f1529, f1531, f1534, f1541, f1550, f1563, f1570, f1575, f1612, f1613, f1626, f1633, f1646, f1654, f1659, f1671, f1674, f1680, f1681, f1694, f1702, f1720, f1730, f1732, f1741, f1745, f1765, f1775, f1777, f1782, f1809, f1817, f1832, f1854, f1856, f1885, f1892, f1893, f1895, f1924, f1936, f1967, f1972, f1980, f1990, f1999, f2009, f2020, f2045, f2064, f2077, f2084, f2120, f2157, f2160, f2163, f2166, f2188, f2203, f2210, f2235, f2236, f2247, f2262, f2297, f2342, f2347, f2363, f2372, f2387, f2391, f2396, f2408, f2420, f2428, f2435, f2444, f2453, f2454, f2473, f2485, f2492, f2510, f2521, f2535, f2553, f2566, f2567, f2577, f2611, f2621, f2662, f2664, f2667, f2670, f2706, f2707, f2714, f2715, f2724, f2729, f2730, f2735, f2750, f2757, f2791, f2797, f2809, f2835, f2836, f2837, f2838, f2847, f2861, f2884, f2892, f2905, f2906, f2938, f2976, f2993, f2994, f3001, f3010, f3011, f3021, f3023, f3032, f3036, f3043, f3078, f3094, f3096, f3107, f3126, f3162, f3178, f3179, f3180, f3183, f3188, f3193, f3211, f3216])).
fof(f3216, plain, (~ spl3_23 | ~ spl3_31), inference(avatar_split_clause, [], [f3212, f331, f297])).
fof(f297, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f331, plain, (spl3_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_31])])).
fof(f3212, plain, (~ (e2 = op(e2, e2)) | ~ spl3_31), inference(backward_demodulation, [], [f146, f333])).
fof(f333, plain, ((e2 = op(e2, e0)) | ~ spl3_31), inference(avatar_component_clause, [], [f331])).
fof(f146, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax3)).
fof(f3211, plain, (spl3_8 | ~ spl3_35 | ~ spl3_71), inference(avatar_split_clause, [], [f3209, f536, f348, f233])).
fof(f233, plain, (spl3_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_8])])).
fof(f348, plain, (spl3_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_35])])).
fof(f536, plain, (spl3_71 <=> (e3 = op(e3, op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f3209, plain, ((e3 = op(e3, e2)) | (~ spl3_35 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f350])).
fof(f350, plain, ((e2 = op(e1, e3)) | ~ spl3_35), inference(avatar_component_clause, [], [f348])).
fof(f538, plain, ((e3 = op(e3, op(e1, e3))) | ~ spl3_71), inference(avatar_component_clause, [], [f536])).
fof(f3193, plain, (~ spl3_23 | ~ spl3_55), inference(avatar_split_clause, [], [f3190, f433, f297])).
fof(f433, plain, (spl3_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_55])])).
fof(f3190, plain, (~ (e2 = op(e2, e2)) | ~ spl3_55), inference(backward_demodulation, [], [f122, f435])).
fof(f435, plain, ((e2 = op(e0, e2)) | ~ spl3_55), inference(avatar_component_clause, [], [f433])).
fof(f122, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3188, plain, (~ spl3_12 | ~ spl3_60), inference(avatar_split_clause, [], [f3185, f454, f250])).
fof(f250, plain, (spl3_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_12])])).
fof(f454, plain, (spl3_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_60])])).
fof(f3185, plain, (~ (e3 = op(e3, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f117, f456])).
fof(f456, plain, ((e3 = op(e0, e1)) | ~ spl3_60), inference(avatar_component_clause, [], [f454])).
fof(f117, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3183, plain, (~ spl3_11 | ~ spl3_62 | ~ spl3_101 | spl3_102), inference(avatar_split_clause, [], [f3177, f682, f678, f463, f246])).
fof(f246, plain, (spl3_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_11])])).
fof(f463, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f678, plain, (spl3_101 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_101])])).
fof(f682, plain, (spl3_102 <=> (e2 = op(op(e0, op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_102])])).
fof(f3177, plain, (~ (e2 = op(e3, e1)) | (~ spl3_62 | ~ spl3_101 | spl3_102)), inference(backward_demodulation, [], [f3147, f465])).
fof(f465, plain, ((op(e0, e0) = e1) | ~ spl3_62), inference(avatar_component_clause, [], [f463])).
fof(f3147, plain, (~ (e2 = op(e3, op(e0, e0))) | (~ spl3_101 | spl3_102)), inference(forward_demodulation, [], [f684, f679])).
fof(f679, plain, ((e3 = op(e0, op(e0, e0))) | ~ spl3_101), inference(avatar_component_clause, [], [f678])).
fof(f684, plain, (~ (e2 = op(op(e0, op(e0, e0)), op(e0, e0))) | spl3_102), inference(avatar_component_clause, [], [f682])).
fof(f3180, plain, (spl3_60 | ~ spl3_62 | ~ spl3_101), inference(avatar_split_clause, [], [f3174, f678, f463, f454])).
fof(f3174, plain, ((e3 = op(e0, e1)) | (~ spl3_62 | ~ spl3_101)), inference(backward_demodulation, [], [f679, f465])).
fof(f3179, plain, (~ spl3_58 | ~ spl3_62), inference(avatar_split_clause, [], [f3168, f463, f446])).
fof(f446, plain, (spl3_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_58])])).
fof(f3168, plain, (~ (e1 = op(e0, e1)) | ~ spl3_62), inference(backward_demodulation, [], [f133, f465])).
fof(f133, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f3178, plain, (~ spl3_46 | ~ spl3_62), inference(avatar_split_clause, [], [f3166, f463, f395])).
fof(f395, plain, (spl3_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_46])])).
fof(f3166, plain, (~ (e1 = op(e1, e0)) | ~ spl3_62), inference(backward_demodulation, [], [f109, f465])).
fof(f109, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f3162, plain, (spl3_31 | ~ spl3_37 | ~ spl3_72), inference(avatar_split_clause, [], [f3161, f541, f357, f331])).
fof(f357, plain, (spl3_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_37])])).
fof(f541, plain, (spl3_72 <=> (e2 = op(e2, op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_72])])).
fof(f3161, plain, ((e2 = op(e2, e0)) | (~ spl3_37 | ~ spl3_72)), inference(forward_demodulation, [], [f543, f359])).
fof(f359, plain, ((e0 = op(e1, e2)) | ~ spl3_37), inference(avatar_component_clause, [], [f357])).
fof(f543, plain, ((e2 = op(e2, op(e1, e2))) | ~ spl3_72), inference(avatar_component_clause, [], [f541])).
fof(f3126, plain, (~ spl3_8 | ~ spl3_56), inference(avatar_split_clause, [], [f3122, f437, f233])).
fof(f437, plain, (spl3_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_56])])).
fof(f3122, plain, (~ (e3 = op(e3, e2)) | ~ spl3_56), inference(backward_demodulation, [], [f123, f439])).
fof(f439, plain, ((e3 = op(e0, e2)) | ~ spl3_56), inference(avatar_component_clause, [], [f437])).
fof(f123, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3107, plain, (~ spl3_54 | ~ spl3_63 | spl3_105), inference(avatar_split_clause, [], [f3106, f698, f467, f429])).
fof(f429, plain, (spl3_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_54])])).
fof(f467, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f698, plain, (spl3_105 <=> (e1 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_105])])).
fof(f3106, plain, (~ (e1 = op(e0, e2)) | (~ spl3_63 | spl3_105)), inference(forward_demodulation, [], [f700, f469])).
fof(f469, plain, ((op(e0, e0) = e2) | ~ spl3_63), inference(avatar_component_clause, [], [f467])).
fof(f700, plain, (~ (e1 = op(e0, op(e0, e0))) | spl3_105), inference(avatar_component_clause, [], [f698])).
fof(f3096, plain, (~ spl3_56 | ~ spl3_43 | spl3_89 | ~ spl3_104), inference(avatar_split_clause, [], [f3019, f692, f622, f382, f437])).
fof(f382, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f622, plain, (spl3_89 <=> (e3 = op(op(e1, op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_89])])).
fof(f692, plain, (spl3_104 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_104])])).
fof(f3019, plain, (~ (e3 = op(e0, e2)) | (~ spl3_43 | spl3_89 | ~ spl3_104)), inference(backward_demodulation, [], [f2957, f384])).
fof(f384, plain, ((e2 = op(e1, e1)) | ~ spl3_43), inference(avatar_component_clause, [], [f382])).
fof(f2957, plain, (~ (e3 = op(e0, op(e1, e1))) | (spl3_89 | ~ spl3_104)), inference(backward_demodulation, [], [f624, f693])).
fof(f693, plain, ((e0 = op(e1, op(e1, e1))) | ~ spl3_104), inference(avatar_component_clause, [], [f692])).
fof(f624, plain, (~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1))) | spl3_89), inference(avatar_component_clause, [], [f622])).
fof(f3094, plain, (spl3_12 | ~ spl3_18 | ~ spl3_66), inference(avatar_split_clause, [], [f3061, f512, f276, f250])).
fof(f276, plain, (spl3_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_18])])).
fof(f512, plain, (spl3_66 <=> (e3 = op(e3, op(e2, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_66])])).
fof(f3061, plain, ((e3 = op(e3, e1)) | (~ spl3_18 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f278])).
fof(f278, plain, ((e1 = op(e2, e3)) | ~ spl3_18), inference(avatar_component_clause, [], [f276])).
fof(f514, plain, ((e3 = op(e3, op(e2, e3))) | ~ spl3_66), inference(avatar_component_clause, [], [f512])).
fof(f3078, plain, (~ spl3_4 | ~ spl3_8), inference(avatar_split_clause, [], [f3077, f233, f216])).
fof(f216, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f3077, plain, (~ (e3 = op(e3, e3)) | ~ spl3_8), inference(backward_demodulation, [], [f156, f235])).
fof(f235, plain, ((e3 = op(e3, e2)) | ~ spl3_8), inference(avatar_component_clause, [], [f233])).
fof(f156, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3043, plain, (~ spl3_24 | ~ spl3_32), inference(avatar_split_clause, [], [f3040, f335, f301])).
fof(f301, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f335, plain, (spl3_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_32])])).
fof(f3040, plain, (~ (e3 = op(e2, e2)) | ~ spl3_32), inference(backward_demodulation, [], [f146, f337])).
fof(f337, plain, ((e3 = op(e2, e0)) | ~ spl3_32), inference(avatar_component_clause, [], [f335])).
fof(f3036, plain, (~ spl3_4 | ~ spl3_36), inference(avatar_split_clause, [], [f3034, f352, f216])).
fof(f352, plain, (spl3_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_36])])).
fof(f3034, plain, (~ (e3 = op(e3, e3)) | ~ spl3_36), inference(backward_demodulation, [], [f131, f354])).
fof(f354, plain, ((e3 = op(e1, e3)) | ~ spl3_36), inference(avatar_component_clause, [], [f352])).
fof(f131, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3032, plain, (~ spl3_5 | ~ spl3_37), inference(avatar_split_clause, [], [f3027, f357, f221])).
fof(f221, plain, (spl3_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_5])])).
fof(f3027, plain, (~ (e0 = op(e3, e2)) | ~ spl3_37), inference(backward_demodulation, [], [f125, f359])).
fof(f125, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3023, plain, (spl3_37 | ~ spl3_43 | ~ spl3_104), inference(avatar_split_clause, [], [f3017, f692, f382, f357])).
fof(f3017, plain, ((e0 = op(e1, e2)) | (~ spl3_43 | ~ spl3_104)), inference(backward_demodulation, [], [f693, f384])).
fof(f3021, plain, (~ spl3_11 | ~ spl3_43), inference(avatar_split_clause, [], [f3013, f382, f246])).
fof(f3013, plain, (~ (e2 = op(e3, e1)) | ~ spl3_43), inference(backward_demodulation, [], [f119, f384])).
fof(f119, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3011, plain, (~ spl3_38 | ~ spl3_46), inference(avatar_split_clause, [], [f3005, f395, f361])).
fof(f361, plain, (spl3_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_38])])).
fof(f3005, plain, (~ (e1 = op(e1, e2)) | ~ spl3_46), inference(backward_demodulation, [], [f140, f397])).
fof(f397, plain, ((e1 = op(e1, e0)) | ~ spl3_46), inference(avatar_component_clause, [], [f395])).
fof(f140, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3010, plain, (~ spl3_14 | ~ spl3_46), inference(avatar_split_clause, [], [f3003, f395, f259])).
fof(f259, plain, (spl3_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_14])])).
fof(f3003, plain, (~ (e1 = op(e3, e0)) | ~ spl3_46), inference(backward_demodulation, [], [f113, f397])).
fof(f113, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3001, plain, (~ spl3_17 | ~ spl3_49), inference(avatar_split_clause, [], [f2996, f408, f272])).
fof(f272, plain, (spl3_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_17])])).
fof(f408, plain, (spl3_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_49])])).
fof(f2996, plain, (~ (e0 = op(e2, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f128, f410])).
fof(f410, plain, ((e0 = op(e0, e3)) | ~ spl3_49), inference(avatar_component_clause, [], [f408])).
fof(f128, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2994, plain, (~ spl3_50 | ~ spl3_54), inference(avatar_split_clause, [], [f2989, f429, f412])).
fof(f412, plain, (spl3_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_50])])).
fof(f2989, plain, (~ (e1 = op(e0, e3)) | ~ spl3_54), inference(backward_demodulation, [], [f138, f431])).
fof(f431, plain, ((e1 = op(e0, e2)) | ~ spl3_54), inference(avatar_component_clause, [], [f429])).
fof(f138, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2993, plain, (~ spl3_38 | ~ spl3_54), inference(avatar_split_clause, [], [f2986, f429, f361])).
fof(f2986, plain, (~ (e1 = op(e1, e2)) | ~ spl3_54), inference(backward_demodulation, [], [f121, f431])).
fof(f121, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2976, plain, (~ spl3_31 | ~ spl3_63), inference(avatar_split_clause, [], [f2967, f467, f331])).
fof(f2967, plain, (~ (e2 = op(e2, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f110, f469])).
fof(f110, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2938, plain, (~ spl3_10 | ~ spl3_14), inference(avatar_split_clause, [], [f2932, f259, f242])).
fof(f242, plain, (spl3_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_10])])).
fof(f2932, plain, (~ (e1 = op(e3, e1)) | ~ spl3_14), inference(backward_demodulation, [], [f151, f261])).
fof(f261, plain, ((e1 = op(e3, e0)) | ~ spl3_14), inference(avatar_component_clause, [], [f259])).
fof(f151, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2906, plain, (~ spl3_27 | ~ spl3_31), inference(avatar_split_clause, [], [f2900, f331, f314])).
fof(f314, plain, (spl3_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_27])])).
fof(f2900, plain, (~ (e2 = op(e2, e1)) | ~ spl3_31), inference(backward_demodulation, [], [f145, f333])).
fof(f145, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2905, plain, (~ spl3_15 | ~ spl3_31), inference(avatar_split_clause, [], [f2899, f331, f263])).
fof(f263, plain, (spl3_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_15])])).
fof(f2899, plain, (~ (e2 = op(e3, e0)) | ~ spl3_31), inference(backward_demodulation, [], [f114, f333])).
fof(f114, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2892, plain, (~ spl3_34 | ~ spl3_38), inference(avatar_split_clause, [], [f2888, f361, f344])).
fof(f344, plain, (spl3_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_34])])).
fof(f2888, plain, (~ (e1 = op(e1, e3)) | ~ spl3_38), inference(backward_demodulation, [], [f144, f363])).
fof(f363, plain, ((e1 = op(e1, e2)) | ~ spl3_38), inference(avatar_component_clause, [], [f361])).
fof(f144, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2884, plain, (~ spl3_17 | ~ spl3_44 | ~ spl3_88 | spl3_107), inference(avatar_split_clause, [], [f2879, f709, f618, f386, f272])).
fof(f386, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f618, plain, (spl3_88 <=> (e2 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_88])])).
fof(f709, plain, (spl3_107 <=> (e0 = op(op(e1, op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_107])])).
fof(f2879, plain, (~ (e0 = op(e2, e3)) | (~ spl3_44 | ~ spl3_88 | spl3_107)), inference(backward_demodulation, [], [f2817, f388])).
fof(f388, plain, ((e3 = op(e1, e1)) | ~ spl3_44), inference(avatar_component_clause, [], [f386])).
fof(f2817, plain, (~ (e0 = op(e2, op(e1, e1))) | (~ spl3_88 | spl3_107)), inference(backward_demodulation, [], [f711, f619])).
fof(f619, plain, ((e2 = op(e1, op(e1, e1))) | ~ spl3_88), inference(avatar_component_clause, [], [f618])).
fof(f711, plain, (~ (e0 = op(op(e1, op(e1, e1)), op(e1, e1))) | spl3_107), inference(avatar_component_clause, [], [f709])).
fof(f2861, plain, (~ spl3_34 | ~ spl3_50), inference(avatar_split_clause, [], [f2855, f412, f344])).
fof(f2855, plain, (~ (e1 = op(e1, e3)) | ~ spl3_50), inference(backward_demodulation, [], [f127, f414])).
fof(f414, plain, ((e1 = op(e0, e3)) | ~ spl3_50), inference(avatar_component_clause, [], [f412])).
fof(f127, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2847, plain, (~ spl3_41 | ~ spl3_57), inference(avatar_split_clause, [], [f2839, f442, f374])).
fof(f374, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f442, plain, (spl3_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_57])])).
fof(f2839, plain, (~ (e0 = op(e1, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f115, f444])).
fof(f444, plain, ((e0 = op(e0, e1)) | ~ spl3_57), inference(avatar_component_clause, [], [f442])).
fof(f115, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2838, plain, (~ spl3_50 | ~ spl3_64 | spl3_105), inference(avatar_split_clause, [], [f2834, f698, f471, f412])).
fof(f471, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f2834, plain, (~ (e1 = op(e0, e3)) | (~ spl3_64 | spl3_105)), inference(backward_demodulation, [], [f700, f473])).
fof(f473, plain, ((op(e0, e0) = e3) | ~ spl3_64), inference(avatar_component_clause, [], [f471])).
fof(f2837, plain, (~ spl3_51 | ~ spl3_64 | spl3_98), inference(avatar_split_clause, [], [f2832, f664, f471, f416])).
fof(f416, plain, (spl3_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_51])])).
fof(f664, plain, (spl3_98 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_98])])).
fof(f2832, plain, (~ (e2 = op(e0, e3)) | (~ spl3_64 | spl3_98)), inference(backward_demodulation, [], [f666, f473])).
fof(f666, plain, (~ (e2 = op(e0, op(e0, e0))) | spl3_98), inference(avatar_component_clause, [], [f664])).
fof(f2836, plain, (~ spl3_60 | ~ spl3_64), inference(avatar_split_clause, [], [f2828, f471, f454])).
fof(f2828, plain, (~ (e3 = op(e0, e1)) | ~ spl3_64), inference(backward_demodulation, [], [f133, f473])).
fof(f2835, plain, (~ spl3_48 | ~ spl3_64), inference(avatar_split_clause, [], [f2825, f471, f403])).
fof(f403, plain, (spl3_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_48])])).
fof(f2825, plain, (~ (e3 = op(e1, e0)) | ~ spl3_64), inference(backward_demodulation, [], [f109, f473])).
fof(f2809, plain, (~ spl3_1 | ~ spl3_30 | ~ spl3_90 | spl3_91), inference(avatar_contradiction_clause, [], [f2808])).
fof(f2808, plain, ($false | (~ spl3_1 | ~ spl3_30 | ~ spl3_90 | spl3_91)), inference(subsumption_resolution, [], [f2805, f329])).
fof(f329, plain, ((e1 = op(e2, e0)) | ~ spl3_30), inference(avatar_component_clause, [], [f327])).
fof(f327, plain, (spl3_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_30])])).
fof(f2805, plain, (~ (e1 = op(e2, e0)) | (~ spl3_1 | ~ spl3_90 | spl3_91)), inference(backward_demodulation, [], [f2687, f206])).
fof(f206, plain, ((e0 = op(e3, e3)) | ~ spl3_1), inference(avatar_component_clause, [], [f204])).
fof(f204, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f2687, plain, (~ (e1 = op(e2, op(e3, e3))) | (~ spl3_90 | spl3_91)), inference(backward_demodulation, [], [f633, f628])).
fof(f628, plain, ((e2 = op(e3, op(e3, e3))) | ~ spl3_90), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl3_90 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_90])])).
fof(f633, plain, (~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3))) | spl3_91), inference(avatar_component_clause, [], [f631])).
fof(f631, plain, (spl3_91 <=> (e1 = op(op(e3, op(e3, e3)), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_91])])).
fof(f2797, plain, (~ spl3_6 | ~ spl3_10), inference(avatar_split_clause, [], [f2792, f242, f225])).
fof(f225, plain, (spl3_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_6])])).
fof(f2792, plain, (~ (e1 = op(e3, e2)) | ~ spl3_10), inference(backward_demodulation, [], [f154, f244])).
fof(f244, plain, ((e1 = op(e3, e1)) | ~ spl3_10), inference(avatar_component_clause, [], [f242])).
fof(f154, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2791, plain, (~ spl3_3 | ~ spl3_15), inference(avatar_split_clause, [], [f2788, f263, f212])).
fof(f212, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f2788, plain, (~ (e2 = op(e3, e3)) | ~ spl3_15), inference(backward_demodulation, [], [f153, f265])).
fof(f265, plain, ((e2 = op(e3, e0)) | ~ spl3_15), inference(avatar_component_clause, [], [f263])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2757, plain, (~ spl3_23 | ~ spl3_39), inference(avatar_split_clause, [], [f2753, f365, f297])).
fof(f365, plain, (spl3_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_39])])).
fof(f2753, plain, (~ (e2 = op(e2, e2)) | ~ spl3_39), inference(backward_demodulation, [], [f124, f367])).
fof(f367, plain, ((e2 = op(e1, e2)) | ~ spl3_39), inference(avatar_component_clause, [], [f365])).
fof(f124, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2750, plain, (~ spl3_9 | ~ spl3_41), inference(avatar_split_clause, [], [f2738, f374, f238])).
fof(f238, plain, (spl3_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_9])])).
fof(f2738, plain, (~ (e0 = op(e3, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f119, f376])).
fof(f376, plain, ((e0 = op(e1, e1)) | ~ spl3_41), inference(avatar_component_clause, [], [f374])).
fof(f2735, plain, (~ spl3_16 | ~ spl3_48), inference(avatar_split_clause, [], [f2731, f403, f267])).
fof(f267, plain, (spl3_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_16])])).
fof(f2731, plain, (~ (e3 = op(e3, e0)) | ~ spl3_48), inference(backward_demodulation, [], [f113, f405])).
fof(f405, plain, ((e3 = op(e1, e0)) | ~ spl3_48), inference(avatar_component_clause, [], [f403])).
fof(f2730, plain, (spl3_8 | ~ spl3_51 | ~ spl3_76), inference(avatar_split_clause, [], [f2728, f560, f416, f233])).
fof(f560, plain, (spl3_76 <=> (e3 = op(e3, op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_76])])).
fof(f2728, plain, ((e3 = op(e3, e2)) | (~ spl3_51 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f418])).
fof(f418, plain, ((e2 = op(e0, e3)) | ~ spl3_51), inference(avatar_component_clause, [], [f416])).
fof(f562, plain, ((e3 = op(e3, op(e0, e3))) | ~ spl3_76), inference(avatar_component_clause, [], [f560])).
fof(f2729, plain, (~ spl3_3 | ~ spl3_51), inference(avatar_split_clause, [], [f2726, f416, f212])).
fof(f2726, plain, (~ (e2 = op(e3, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f129, f418])).
fof(f129, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2724, plain, (spl3_27 | ~ spl3_54 | ~ spl3_77), inference(avatar_split_clause, [], [f2722, f565, f429, f314])).
fof(f565, plain, (spl3_77 <=> (e2 = op(e2, op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_77])])).
fof(f2722, plain, ((e2 = op(e2, e1)) | (~ spl3_54 | ~ spl3_77)), inference(backward_demodulation, [], [f567, f431])).
fof(f567, plain, ((e2 = op(e2, op(e0, e2))) | ~ spl3_77), inference(avatar_component_clause, [], [f565])).
fof(f2715, plain, (~ spl3_52 | ~ spl3_60), inference(avatar_split_clause, [], [f2712, f454, f420])).
fof(f420, plain, (spl3_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_52])])).
fof(f2712, plain, (~ (e3 = op(e0, e3)) | ~ spl3_60), inference(backward_demodulation, [], [f137, f456])).
fof(f137, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2714, plain, (~ spl3_28 | ~ spl3_60), inference(avatar_split_clause, [], [f2709, f454, f318])).
fof(f318, plain, (spl3_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_28])])).
fof(f2709, plain, (~ (e3 = op(e2, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f116, f456])).
fof(f116, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2707, plain, (~ spl3_53 | ~ spl3_61), inference(avatar_split_clause, [], [f2700, f459, f425])).
fof(f425, plain, (spl3_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_53])])).
fof(f459, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f2700, plain, (~ (e0 = op(e0, e2)) | ~ spl3_61), inference(backward_demodulation, [], [f134, f461])).
fof(f461, plain, ((e0 = op(e0, e0)) | ~ spl3_61), inference(avatar_component_clause, [], [f459])).
fof(f134, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2706, plain, (~ spl3_45 | ~ spl3_61), inference(avatar_split_clause, [], [f2697, f459, f391])).
fof(f391, plain, (spl3_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_45])])).
fof(f2697, plain, (~ (e0 = op(e1, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f109, f461])).
fof(f2670, plain, (~ spl3_12 | ~ spl3_16), inference(avatar_split_clause, [], [f2668, f267, f250])).
fof(f2668, plain, (~ (e3 = op(e3, e1)) | ~ spl3_16), inference(backward_demodulation, [], [f151, f269])).
fof(f269, plain, ((e3 = op(e3, e0)) | ~ spl3_16), inference(avatar_component_clause, [], [f267])).
fof(f2667, plain, (~ spl3_12 | ~ spl3_28), inference(avatar_split_clause, [], [f2665, f318, f250])).
fof(f2665, plain, (~ (e3 = op(e3, e1)) | ~ spl3_28), inference(backward_demodulation, [], [f120, f320])).
fof(f320, plain, ((e3 = op(e2, e1)) | ~ spl3_28), inference(avatar_component_clause, [], [f318])).
fof(f120, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2664, plain, (~ spl3_30 | spl3_57 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f2663])).
fof(f2663, plain, ($false | (~ spl3_30 | spl3_57 | ~ spl3_69)), inference(subsumption_resolution, [], [f2661, f443])).
fof(f443, plain, (~ (e0 = op(e0, e1)) | spl3_57), inference(avatar_component_clause, [], [f442])).
fof(f2661, plain, ((e0 = op(e0, e1)) | (~ spl3_30 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f329])).
fof(f529, plain, ((e0 = op(e0, op(e2, e0))) | ~ spl3_69), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl3_69 <=> (e0 = op(e0, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f2662, plain, (~ spl3_26 | ~ spl3_30), inference(avatar_split_clause, [], [f2658, f327, f310])).
fof(f310, plain, (spl3_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_26])])).
fof(f2658, plain, (~ (e1 = op(e2, e1)) | ~ spl3_30), inference(backward_demodulation, [], [f145, f329])).
fof(f2621, plain, (~ spl3_47 | ~ spl3_63), inference(avatar_split_clause, [], [f2612, f467, f399])).
fof(f399, plain, (spl3_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_47])])).
fof(f2612, plain, (~ (e2 = op(e1, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f109, f469])).
fof(f2611, plain, (spl3_16 | ~ spl3_17 | ~ spl3_66), inference(avatar_split_clause, [], [f2610, f512, f272, f267])).
fof(f2610, plain, ((e3 = op(e3, e0)) | (~ spl3_17 | ~ spl3_66)), inference(forward_demodulation, [], [f514, f274])).
fof(f274, plain, ((e0 = op(e2, e3)) | ~ spl3_17), inference(avatar_component_clause, [], [f272])).
fof(f2577, plain, (~ spl3_16 | ~ spl3_32), inference(avatar_split_clause, [], [f2574, f335, f267])).
fof(f2574, plain, (~ (e3 = op(e3, e0)) | ~ spl3_32), inference(backward_demodulation, [], [f114, f337])).
fof(f2567, plain, (~ spl3_32 | ~ spl3_41 | ~ spl3_88 | spl3_89), inference(avatar_split_clause, [], [f2561, f622, f618, f374, f335])).
fof(f2561, plain, (~ (e3 = op(e2, e0)) | (~ spl3_41 | ~ spl3_88 | spl3_89)), inference(backward_demodulation, [], [f2512, f376])).
fof(f2512, plain, (~ (e3 = op(e2, op(e1, e1))) | (~ spl3_88 | spl3_89)), inference(backward_demodulation, [], [f624, f619])).
fof(f2566, plain, (~ spl3_41 | spl3_46 | ~ spl3_73), inference(avatar_contradiction_clause, [], [f2565])).
fof(f2565, plain, ($false | (~ spl3_41 | spl3_46 | ~ spl3_73)), inference(subsumption_resolution, [], [f2559, f396])).
fof(f396, plain, (~ (e1 = op(e1, e0)) | spl3_46), inference(avatar_component_clause, [], [f395])).
fof(f2559, plain, ((e1 = op(e1, e0)) | (~ spl3_41 | ~ spl3_73)), inference(backward_demodulation, [], [f548, f376])).
fof(f548, plain, ((e1 = op(e1, op(e1, e1))) | ~ spl3_73), inference(avatar_component_clause, [], [f546])).
fof(f546, plain, (spl3_73 <=> (e1 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_73])])).
fof(f2553, plain, (~ spl3_43 | ~ spl3_47), inference(avatar_split_clause, [], [f2549, f399, f382])).
fof(f2549, plain, (~ (e2 = op(e1, e1)) | ~ spl3_47), inference(backward_demodulation, [], [f139, f401])).
fof(f401, plain, ((e2 = op(e1, e0)) | ~ spl3_47), inference(avatar_component_clause, [], [f399])).
fof(f139, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2535, plain, (spl3_58 | ~ spl3_62 | ~ spl3_105), inference(avatar_contradiction_clause, [], [f2534])).
fof(f2534, plain, ($false | (spl3_58 | ~ spl3_62 | ~ spl3_105)), inference(subsumption_resolution, [], [f2530, f447])).
fof(f447, plain, (~ (e1 = op(e0, e1)) | spl3_58), inference(avatar_component_clause, [], [f446])).
fof(f2530, plain, ((e1 = op(e0, e1)) | (~ spl3_62 | ~ spl3_105)), inference(backward_demodulation, [], [f699, f465])).
fof(f699, plain, ((e1 = op(e0, op(e0, e0))) | ~ spl3_105), inference(avatar_component_clause, [], [f698])).
fof(f2521, plain, (spl3_19 | ~ spl3_40 | ~ spl3_72), inference(avatar_contradiction_clause, [], [f2520])).
fof(f2520, plain, ($false | (spl3_19 | ~ spl3_40 | ~ spl3_72)), inference(subsumption_resolution, [], [f2519, f281])).
fof(f281, plain, (~ (e2 = op(e2, e3)) | spl3_19), inference(avatar_component_clause, [], [f280])).
fof(f280, plain, (spl3_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_19])])).
fof(f2519, plain, ((e2 = op(e2, e3)) | (~ spl3_40 | ~ spl3_72)), inference(forward_demodulation, [], [f543, f371])).
fof(f371, plain, ((e3 = op(e1, e2)) | ~ spl3_40), inference(avatar_component_clause, [], [f369])).
fof(f369, plain, (spl3_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_40])])).
fof(f2510, plain, (~ spl3_98 | ~ spl3_105), inference(avatar_contradiction_clause, [], [f2509])).
fof(f2509, plain, ($false | (~ spl3_98 | ~ spl3_105)), inference(subsumption_resolution, [], [f2508, f160])).
fof(f160, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax4)).
fof(f2508, plain, ((e1 = e2) | (~ spl3_98 | ~ spl3_105)), inference(forward_demodulation, [], [f665, f699])).
fof(f665, plain, ((e2 = op(e0, op(e0, e0))) | ~ spl3_98), inference(avatar_component_clause, [], [f664])).
fof(f2492, plain, (spl3_31 | ~ spl3_53 | ~ spl3_77), inference(avatar_split_clause, [], [f2406, f565, f425, f331])).
fof(f2406, plain, ((e2 = op(e2, e0)) | (~ spl3_53 | ~ spl3_77)), inference(backward_demodulation, [], [f567, f427])).
fof(f427, plain, ((e0 = op(e0, e2)) | ~ spl3_53), inference(avatar_component_clause, [], [f425])).
fof(f2485, plain, (~ spl3_3 | ~ spl3_6 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f2484])).
fof(f2484, plain, ($false | (~ spl3_3 | ~ spl3_6 | ~ spl3_80)), inference(subsumption_resolution, [], [f2483, f161])).
fof(f161, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2483, plain, ((e1 = e3) | (~ spl3_3 | ~ spl3_6 | ~ spl3_80)), inference(forward_demodulation, [], [f2480, f227])).
fof(f227, plain, ((e1 = op(e3, e2)) | ~ spl3_6), inference(avatar_component_clause, [], [f225])).
fof(f2480, plain, ((e3 = op(e3, e2)) | (~ spl3_3 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f214])).
fof(f214, plain, ((e2 = op(e3, e3)) | ~ spl3_3), inference(avatar_component_clause, [], [f212])).
fof(f582, plain, ((e3 = op(e3, op(e3, e3))) | ~ spl3_80), inference(avatar_component_clause, [], [f580])).
fof(f580, plain, (spl3_80 <=> (e3 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_80])])).
fof(f2473, plain, (~ spl3_1 | ~ spl3_9), inference(avatar_split_clause, [], [f2468, f238, f204])).
fof(f2468, plain, (~ (e0 = op(e3, e3)) | ~ spl3_9), inference(backward_demodulation, [], [f155, f240])).
fof(f240, plain, ((e0 = op(e3, e1)) | ~ spl3_9), inference(avatar_component_clause, [], [f238])).
fof(f155, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2454, plain, (~ spl3_19 | ~ spl3_23), inference(avatar_split_clause, [], [f2446, f297, f280])).
fof(f2446, plain, (~ (e2 = op(e2, e3)) | ~ spl3_23), inference(backward_demodulation, [], [f150, f299])).
fof(f299, plain, ((e2 = op(e2, e2)) | ~ spl3_23), inference(avatar_component_clause, [], [f297])).
fof(f150, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2453, plain, (~ spl3_7 | ~ spl3_23), inference(avatar_split_clause, [], [f2445, f297, f229])).
fof(f229, plain, (spl3_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f2445, plain, (~ (e2 = op(e3, e2)) | ~ spl3_23), inference(backward_demodulation, [], [f126, f299])).
fof(f126, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2444, plain, (spl3_16 | ~ spl3_33 | ~ spl3_71), inference(avatar_split_clause, [], [f2442, f536, f340, f267])).
fof(f340, plain, (spl3_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_33])])).
fof(f2442, plain, ((e3 = op(e3, e0)) | (~ spl3_33 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f342])).
fof(f342, plain, ((e0 = op(e1, e3)) | ~ spl3_33), inference(avatar_component_clause, [], [f340])).
fof(f2435, plain, (~ spl3_24 | ~ spl3_40), inference(avatar_split_clause, [], [f2429, f369, f301])).
fof(f2429, plain, (~ (e3 = op(e2, e2)) | ~ spl3_40), inference(backward_demodulation, [], [f124, f371])).
fof(f2428, plain, (~ spl3_34 | ~ spl3_46), inference(avatar_split_clause, [], [f2423, f395, f344])).
fof(f2423, plain, (~ (e1 = op(e1, e3)) | ~ spl3_46), inference(backward_demodulation, [], [f141, f397])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2420, plain, (spl3_12 | ~ spl3_50 | ~ spl3_76), inference(avatar_split_clause, [], [f2418, f560, f412, f250])).
fof(f2418, plain, ((e3 = op(e3, e1)) | (~ spl3_50 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f414])).
fof(f2408, plain, (~ spl3_37 | ~ spl3_53), inference(avatar_split_clause, [], [f2399, f425, f357])).
fof(f2399, plain, (~ (e0 = op(e1, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f121, f427])).
fof(f2396, plain, (spl3_34 | ~ spl3_60 | ~ spl3_78), inference(avatar_split_clause, [], [f2392, f570, f454, f344])).
fof(f570, plain, (spl3_78 <=> (e1 = op(e1, op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_78])])).
fof(f2392, plain, ((e1 = op(e1, e3)) | (~ spl3_60 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f456])).
fof(f572, plain, ((e1 = op(e1, op(e0, e1))) | ~ spl3_78), inference(avatar_component_clause, [], [f570])).
fof(f2391, plain, (spl3_53 | ~ spl3_63 | ~ spl3_79), inference(avatar_split_clause, [], [f2390, f575, f467, f425])).
fof(f575, plain, (spl3_79 <=> (e0 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_79])])).
fof(f2390, plain, ((e0 = op(e0, e2)) | (~ spl3_63 | ~ spl3_79)), inference(forward_demodulation, [], [f577, f469])).
fof(f577, plain, ((e0 = op(e0, op(e0, e0))) | ~ spl3_79), inference(avatar_component_clause, [], [f575])).
fof(f2387, plain, (~ spl3_37 | ~ spl3_43 | spl3_104), inference(avatar_split_clause, [], [f2386, f692, f382, f357])).
fof(f2386, plain, (~ (e0 = op(e1, e2)) | (~ spl3_43 | spl3_104)), inference(forward_demodulation, [], [f694, f384])).
fof(f694, plain, (~ (e0 = op(e1, op(e1, e1))) | spl3_104), inference(avatar_component_clause, [], [f692])).
fof(f2372, plain, (~ spl3_1 | ~ spl3_14 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f2371])).
fof(f2371, plain, ($false | (~ spl3_1 | ~ spl3_14 | ~ spl3_80)), inference(subsumption_resolution, [], [f2370, f161])).
fof(f2370, plain, ((e1 = e3) | (~ spl3_1 | ~ spl3_14 | ~ spl3_80)), inference(forward_demodulation, [], [f2367, f261])).
fof(f2367, plain, ((e3 = op(e3, e0)) | (~ spl3_1 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f206])).
fof(f2363, plain, (~ spl3_7 | spl3_23 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f2362])).
fof(f2362, plain, ($false | (~ spl3_7 | spl3_23 | ~ spl3_81)), inference(subsumption_resolution, [], [f2360, f298])).
fof(f298, plain, (~ (e2 = op(e2, e2)) | spl3_23), inference(avatar_component_clause, [], [f297])).
fof(f2360, plain, ((e2 = op(e2, e2)) | (~ spl3_7 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f231])).
fof(f231, plain, ((e2 = op(e3, e2)) | ~ spl3_7), inference(avatar_component_clause, [], [f229])).
fof(f587, plain, ((e2 = op(e2, op(e3, e2))) | ~ spl3_81), inference(avatar_component_clause, [], [f585])).
fof(f585, plain, (spl3_81 <=> (e2 = op(e2, op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_81])])).
fof(f2347, plain, (~ spl3_3 | ~ spl3_19), inference(avatar_split_clause, [], [f2344, f280, f212])).
fof(f2344, plain, (~ (e2 = op(e3, e3)) | ~ spl3_19), inference(backward_demodulation, [], [f132, f282])).
fof(f282, plain, ((e2 = op(e2, e3)) | ~ spl3_19), inference(avatar_component_clause, [], [f280])).
fof(f132, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2342, plain, (spl3_19 | ~ spl3_24 | ~ spl3_67), inference(avatar_split_clause, [], [f2339, f517, f301, f280])).
fof(f517, plain, (spl3_67 <=> (e2 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_67])])).
fof(f2339, plain, ((e2 = op(e2, e3)) | (~ spl3_24 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f303])).
fof(f303, plain, ((e3 = op(e2, e2)) | ~ spl3_24), inference(avatar_component_clause, [], [f301])).
fof(f519, plain, ((e2 = op(e2, op(e2, e2))) | ~ spl3_67), inference(avatar_component_clause, [], [f517])).
fof(f2297, plain, (~ spl3_3 | ~ spl3_5 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f2296])).
fof(f2296, plain, ($false | (~ spl3_3 | ~ spl3_5 | ~ spl3_80)), inference(subsumption_resolution, [], [f2295, f159])).
fof(f159, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f2295, plain, ((e0 = e3) | (~ spl3_3 | ~ spl3_5 | ~ spl3_80)), inference(forward_demodulation, [], [f2292, f223])).
fof(f223, plain, ((e0 = op(e3, e2)) | ~ spl3_5), inference(avatar_component_clause, [], [f221])).
fof(f2292, plain, ((e3 = op(e3, e2)) | (~ spl3_3 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f214])).
fof(f2262, plain, (~ spl3_2 | ~ spl3_34), inference(avatar_split_clause, [], [f2258, f344, f208])).
fof(f208, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f2258, plain, (~ (e1 = op(e3, e3)) | ~ spl3_34), inference(backward_demodulation, [], [f131, f346])).
fof(f346, plain, ((e1 = op(e1, e3)) | ~ spl3_34), inference(avatar_component_clause, [], [f344])).
fof(f2247, plain, (~ spl3_39 | ~ spl3_43), inference(avatar_split_clause, [], [f2239, f382, f365])).
fof(f2239, plain, (~ (e2 = op(e1, e2)) | ~ spl3_43), inference(backward_demodulation, [], [f142, f384])).
fof(f142, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2236, plain, (~ spl3_33 | ~ spl3_45), inference(avatar_split_clause, [], [f2230, f391, f340])).
fof(f2230, plain, (~ (e0 = op(e1, e3)) | ~ spl3_45), inference(backward_demodulation, [], [f141, f393])).
fof(f393, plain, ((e0 = op(e1, e0)) | ~ spl3_45), inference(avatar_component_clause, [], [f391])).
fof(f2235, plain, (~ spl3_29 | ~ spl3_45), inference(avatar_split_clause, [], [f2227, f391, f323])).
fof(f323, plain, (spl3_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_29])])).
fof(f2227, plain, (~ (e0 = op(e2, e0)) | ~ spl3_45), inference(backward_demodulation, [], [f112, f393])).
fof(f112, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2210, plain, (~ spl3_40 | ~ spl3_54 | ~ spl3_63 | spl3_99), inference(avatar_split_clause, [], [f2092, f668, f467, f429, f369])).
fof(f668, plain, (spl3_99 <=> (e3 = op(op(e0, op(e0, e0)), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_99])])).
fof(f2092, plain, (~ (e3 = op(e1, e2)) | (~ spl3_54 | ~ spl3_63 | spl3_99)), inference(backward_demodulation, [], [f2074, f431])).
fof(f2074, plain, (~ (e3 = op(op(e0, e2), e2)) | (~ spl3_63 | spl3_99)), inference(forward_demodulation, [], [f670, f469])).
fof(f670, plain, (~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0))) | spl3_99), inference(avatar_component_clause, [], [f668])).
fof(f2203, plain, (spl3_31 | ~ spl3_5 | ~ spl3_81), inference(avatar_split_clause, [], [f2182, f585, f221, f331])).
fof(f2182, plain, ((e2 = op(e2, e0)) | (~ spl3_5 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f223])).
fof(f2188, plain, (~ spl3_5 | ~ spl3_29 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f2187])).
fof(f2187, plain, ($false | (~ spl3_5 | ~ spl3_29 | ~ spl3_81)), inference(subsumption_resolution, [], [f2186, f158])).
fof(f158, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2186, plain, ((e0 = e2) | (~ spl3_5 | ~ spl3_29 | ~ spl3_81)), inference(forward_demodulation, [], [f2182, f325])).
fof(f325, plain, ((e0 = op(e2, e0)) | ~ spl3_29), inference(avatar_component_clause, [], [f323])).
fof(f2166, plain, (~ spl3_11 | spl3_38 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f2165])).
fof(f2165, plain, ($false | (~ spl3_11 | spl3_38 | ~ spl3_82)), inference(subsumption_resolution, [], [f2164, f362])).
fof(f362, plain, (~ (e1 = op(e1, e2)) | spl3_38), inference(avatar_component_clause, [], [f361])).
fof(f2164, plain, ((e1 = op(e1, e2)) | (~ spl3_11 | ~ spl3_82)), inference(forward_demodulation, [], [f592, f248])).
fof(f248, plain, ((e2 = op(e3, e1)) | ~ spl3_11), inference(avatar_component_clause, [], [f246])).
fof(f592, plain, ((e1 = op(e1, op(e3, e1))) | ~ spl3_82), inference(avatar_component_clause, [], [f590])).
fof(f590, plain, (spl3_82 <=> (e1 = op(e1, op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_82])])).
fof(f2163, plain, (~ spl3_16 | spl3_49 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f2162])).
fof(f2162, plain, ($false | (~ spl3_16 | spl3_49 | ~ spl3_83)), inference(subsumption_resolution, [], [f2161, f409])).
fof(f409, plain, (~ (e0 = op(e0, e3)) | spl3_49), inference(avatar_component_clause, [], [f408])).
fof(f2161, plain, ((e0 = op(e0, e3)) | (~ spl3_16 | ~ spl3_83)), inference(forward_demodulation, [], [f597, f269])).
fof(f597, plain, ((e0 = op(e0, op(e3, e0))) | ~ spl3_83), inference(avatar_component_clause, [], [f595])).
fof(f595, plain, (spl3_83 <=> (e0 = op(e0, op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_83])])).
fof(f2160, plain, (~ spl3_80 | ~ spl3_90), inference(avatar_contradiction_clause, [], [f2159])).
fof(f2159, plain, ($false | (~ spl3_80 | ~ spl3_90)), inference(subsumption_resolution, [], [f2158, f162])).
fof(f162, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2158, plain, ((e2 = e3) | (~ spl3_80 | ~ spl3_90)), inference(backward_demodulation, [], [f582, f628])).
fof(f2157, plain, (spl3_33 | ~ spl3_44 | ~ spl3_104), inference(avatar_split_clause, [], [f2156, f692, f386, f340])).
fof(f2156, plain, ((e0 = op(e1, e3)) | (~ spl3_44 | ~ spl3_104)), inference(forward_demodulation, [], [f693, f388])).
fof(f2120, plain, (~ spl3_26 | spl3_42 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f2119])).
fof(f2119, plain, ($false | (~ spl3_26 | spl3_42 | ~ spl3_68)), inference(subsumption_resolution, [], [f2115, f379])).
fof(f379, plain, (~ (e1 = op(e1, e1)) | spl3_42), inference(avatar_component_clause, [], [f378])).
fof(f378, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f2115, plain, ((e1 = op(e1, e1)) | (~ spl3_26 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f312])).
fof(f312, plain, ((e1 = op(e2, e1)) | ~ spl3_26), inference(avatar_component_clause, [], [f310])).
fof(f524, plain, ((e1 = op(e1, op(e2, e1))) | ~ spl3_68), inference(avatar_component_clause, [], [f522])).
fof(f522, plain, (spl3_68 <=> (e1 = op(e1, op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_68])])).
fof(f2084, plain, (~ spl3_29 | spl3_61 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f2083])).
fof(f2083, plain, ($false | (~ spl3_29 | spl3_61 | ~ spl3_69)), inference(subsumption_resolution, [], [f2082, f460])).
fof(f460, plain, (~ (e0 = op(e0, e0)) | spl3_61), inference(avatar_component_clause, [], [f459])).
fof(f2082, plain, ((e0 = op(e0, e0)) | (~ spl3_29 | ~ spl3_69)), inference(forward_demodulation, [], [f529, f325])).
fof(f2077, plain, (~ spl3_35 | ~ spl3_44 | spl3_88), inference(avatar_split_clause, [], [f2076, f618, f386, f348])).
fof(f2076, plain, (~ (e2 = op(e1, e3)) | (~ spl3_44 | spl3_88)), inference(forward_demodulation, [], [f620, f388])).
fof(f620, plain, (~ (e2 = op(e1, op(e1, e1))) | spl3_88), inference(avatar_component_clause, [], [f618])).
fof(f2064, plain, (~ spl3_28 | ~ spl3_44), inference(avatar_split_clause, [], [f2063, f386, f318])).
fof(f2063, plain, (~ (e3 = op(e2, e1)) | ~ spl3_44), inference(forward_demodulation, [], [f118, f388])).
fof(f118, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2045, plain, (~ spl3_4 | ~ spl3_16), inference(avatar_split_clause, [], [f2043, f267, f216])).
fof(f2043, plain, (~ (e3 = op(e3, e3)) | ~ spl3_16), inference(backward_demodulation, [], [f153, f269])).
fof(f2020, plain, (~ spl3_29 | ~ spl3_37 | ~ spl3_72), inference(avatar_contradiction_clause, [], [f2019])).
fof(f2019, plain, ($false | (~ spl3_29 | ~ spl3_37 | ~ spl3_72)), inference(subsumption_resolution, [], [f2018, f158])).
fof(f2018, plain, ((e0 = e2) | (~ spl3_29 | ~ spl3_37 | ~ spl3_72)), inference(forward_demodulation, [], [f2015, f325])).
fof(f2015, plain, ((e2 = op(e2, e0)) | (~ spl3_37 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f359])).
fof(f2009, plain, (~ spl3_10 | ~ spl3_50 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f2008])).
fof(f2008, plain, ($false | (~ spl3_10 | ~ spl3_50 | ~ spl3_76)), inference(subsumption_resolution, [], [f2007, f161])).
fof(f2007, plain, ((e1 = e3) | (~ spl3_10 | ~ spl3_50 | ~ spl3_76)), inference(forward_demodulation, [], [f2005, f244])).
fof(f2005, plain, ((e3 = op(e3, e1)) | (~ spl3_50 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f414])).
fof(f1999, plain, (spl3_19 | ~ spl3_56 | ~ spl3_77), inference(avatar_contradiction_clause, [], [f1998])).
fof(f1998, plain, ($false | (spl3_19 | ~ spl3_56 | ~ spl3_77)), inference(subsumption_resolution, [], [f1997, f281])).
fof(f1997, plain, ((e2 = op(e2, e3)) | (~ spl3_56 | ~ spl3_77)), inference(backward_demodulation, [], [f567, f439])).
fof(f1990, plain, (~ spl3_15 | ~ spl3_63), inference(avatar_split_clause, [], [f1985, f467, f263])).
fof(f1985, plain, (~ (e2 = op(e3, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f111, f469])).
fof(f111, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f1980, plain, (~ spl3_33 | ~ spl3_44 | spl3_104), inference(avatar_split_clause, [], [f1979, f692, f386, f340])).
fof(f1979, plain, (~ (e0 = op(e1, e3)) | (~ spl3_44 | spl3_104)), inference(forward_demodulation, [], [f694, f388])).
fof(f1972, plain, (~ spl3_56 | ~ spl3_24), inference(avatar_split_clause, [], [f1971, f301, f437])).
fof(f1971, plain, (~ (e3 = op(e0, e2)) | ~ spl3_24), inference(forward_demodulation, [], [f122, f303])).
fof(f1967, plain, (~ spl3_19 | ~ spl3_27), inference(avatar_split_clause, [], [f1966, f314, f280])).
fof(f1966, plain, (~ (e2 = op(e2, e3)) | ~ spl3_27), inference(forward_demodulation, [], [f149, f316])).
fof(f316, plain, ((e2 = op(e2, e1)) | ~ spl3_27), inference(avatar_component_clause, [], [f314])).
fof(f149, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1936, plain, (~ spl3_33 | ~ spl3_44 | ~ spl3_51 | spl3_93), inference(avatar_contradiction_clause, [], [f1935])).
fof(f1935, plain, ($false | (~ spl3_33 | ~ spl3_44 | ~ spl3_51 | spl3_93)), inference(subsumption_resolution, [], [f1931, f418])).
fof(f1931, plain, (~ (e2 = op(e0, e3)) | (~ spl3_33 | ~ spl3_44 | spl3_93)), inference(backward_demodulation, [], [f1874, f342])).
fof(f1874, plain, (~ (e2 = op(op(e1, e3), e3)) | (~ spl3_44 | spl3_93)), inference(forward_demodulation, [], [f642, f388])).
fof(f642, plain, (~ (e2 = op(op(e1, op(e1, e1)), op(e1, e1))) | spl3_93), inference(avatar_component_clause, [], [f640])).
fof(f640, plain, (spl3_93 <=> (e2 = op(op(e1, op(e1, e1)), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_93])])).
fof(f1924, plain, (spl3_23 | ~ spl3_39 | ~ spl3_72), inference(avatar_contradiction_clause, [], [f1923])).
fof(f1923, plain, ($false | (spl3_23 | ~ spl3_39 | ~ spl3_72)), inference(subsumption_resolution, [], [f1921, f298])).
fof(f1921, plain, ((e2 = op(e2, e2)) | (~ spl3_39 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f367])).
fof(f1895, plain, (spl3_49 | ~ spl3_64 | ~ spl3_79), inference(avatar_contradiction_clause, [], [f1894])).
fof(f1894, plain, ($false | (spl3_49 | ~ spl3_64 | ~ spl3_79)), inference(subsumption_resolution, [], [f1891, f409])).
fof(f1891, plain, ((e0 = op(e0, e3)) | (~ spl3_64 | ~ spl3_79)), inference(backward_demodulation, [], [f577, f473])).
fof(f1893, plain, (~ spl3_52 | ~ spl3_64), inference(avatar_split_clause, [], [f1890, f471, f420])).
fof(f1890, plain, (~ (e3 = op(e0, e3)) | ~ spl3_64), inference(backward_demodulation, [], [f135, f473])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1892, plain, (~ spl3_32 | ~ spl3_64), inference(avatar_split_clause, [], [f1888, f471, f335])).
fof(f1888, plain, (~ (e3 = op(e2, e0)) | ~ spl3_64), inference(backward_demodulation, [], [f110, f473])).
fof(f1885, plain, (spl3_46 | ~ spl3_57 | ~ spl3_78), inference(avatar_split_clause, [], [f1884, f570, f442, f395])).
fof(f1884, plain, ((e1 = op(e1, e0)) | (~ spl3_57 | ~ spl3_78)), inference(forward_demodulation, [], [f572, f444])).
fof(f1856, plain, (~ spl3_1 | ~ spl3_15 | spl3_90), inference(avatar_contradiction_clause, [], [f1855])).
fof(f1855, plain, ($false | (~ spl3_1 | ~ spl3_15 | spl3_90)), inference(subsumption_resolution, [], [f1850, f265])).
fof(f1850, plain, (~ (e2 = op(e3, e0)) | (~ spl3_1 | spl3_90)), inference(backward_demodulation, [], [f629, f206])).
fof(f629, plain, (~ (e2 = op(e3, op(e3, e3))) | spl3_90), inference(avatar_component_clause, [], [f627])).
fof(f1854, plain, (~ spl3_1 | ~ spl3_15 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f1853])).
fof(f1853, plain, ($false | (~ spl3_1 | ~ spl3_15 | ~ spl3_80)), inference(subsumption_resolution, [], [f1852, f162])).
fof(f1852, plain, ((e2 = e3) | (~ spl3_1 | ~ spl3_15 | ~ spl3_80)), inference(forward_demodulation, [], [f1849, f265])).
fof(f1849, plain, ((e3 = op(e3, e0)) | (~ spl3_1 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f206])).
fof(f1832, plain, (~ spl3_45 | spl3_61 | ~ spl3_74), inference(avatar_contradiction_clause, [], [f1831])).
fof(f1831, plain, ($false | (~ spl3_45 | spl3_61 | ~ spl3_74)), inference(subsumption_resolution, [], [f1828, f460])).
fof(f1828, plain, ((e0 = op(e0, e0)) | (~ spl3_45 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f393])).
fof(f553, plain, ((e0 = op(e0, op(e1, e0))) | ~ spl3_74), inference(avatar_component_clause, [], [f551])).
fof(f551, plain, (spl3_74 <=> (e0 = op(e0, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_74])])).
fof(f1817, plain, (~ spl3_9 | ~ spl3_57), inference(avatar_split_clause, [], [f1810, f442, f238])).
fof(f1810, plain, (~ (e0 = op(e3, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f117, f444])).
fof(f1809, plain, (~ spl3_59 | ~ spl3_62 | spl3_98), inference(avatar_split_clause, [], [f1803, f664, f463, f450])).
fof(f450, plain, (spl3_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_59])])).
fof(f1803, plain, (~ (e2 = op(e0, e1)) | (~ spl3_62 | spl3_98)), inference(backward_demodulation, [], [f666, f465])).
fof(f1782, plain, (spl3_34 | ~ spl3_44 | ~ spl3_73), inference(avatar_contradiction_clause, [], [f1781])).
fof(f1781, plain, ($false | (spl3_34 | ~ spl3_44 | ~ spl3_73)), inference(subsumption_resolution, [], [f1780, f345])).
fof(f345, plain, (~ (e1 = op(e1, e3)) | spl3_34), inference(avatar_component_clause, [], [f344])).
fof(f1780, plain, ((e1 = op(e1, e3)) | (~ spl3_44 | ~ spl3_73)), inference(forward_demodulation, [], [f548, f388])).
fof(f1777, plain, (~ spl3_43 | ~ spl3_27), inference(avatar_split_clause, [], [f1776, f314, f382])).
fof(f1776, plain, (~ (e2 = op(e1, e1)) | ~ spl3_27), inference(forward_demodulation, [], [f118, f316])).
fof(f1775, plain, (~ spl3_48 | ~ spl3_44), inference(avatar_split_clause, [], [f1774, f386, f403])).
fof(f1774, plain, (~ (e3 = op(e1, e0)) | ~ spl3_44), inference(forward_demodulation, [], [f139, f388])).
fof(f1765, plain, (~ spl3_11 | ~ spl3_27), inference(avatar_split_clause, [], [f1764, f314, f246])).
fof(f1764, plain, (~ (e2 = op(e3, e1)) | ~ spl3_27), inference(forward_demodulation, [], [f120, f316])).
fof(f1745, plain, (~ spl3_42 | ~ spl3_44), inference(avatar_contradiction_clause, [], [f1744])).
fof(f1744, plain, ($false | (~ spl3_42 | ~ spl3_44)), inference(subsumption_resolution, [], [f1743, f161])).
fof(f1743, plain, ((e1 = e3) | (~ spl3_42 | ~ spl3_44)), inference(forward_demodulation, [], [f388, f380])).
fof(f380, plain, ((e1 = op(e1, e1)) | ~ spl3_42), inference(avatar_component_clause, [], [f378])).
fof(f1741, plain, (~ spl3_46 | spl3_57 | ~ spl3_74), inference(avatar_contradiction_clause, [], [f1740])).
fof(f1740, plain, ($false | (~ spl3_46 | spl3_57 | ~ spl3_74)), inference(subsumption_resolution, [], [f1737, f443])).
fof(f1737, plain, ((e0 = op(e0, e1)) | (~ spl3_46 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f397])).
fof(f1732, plain, (~ spl3_46 | ~ spl3_42), inference(avatar_split_clause, [], [f1731, f378, f395])).
fof(f1731, plain, (~ (e1 = op(e1, e0)) | ~ spl3_42), inference(forward_demodulation, [], [f139, f380])).
fof(f1730, plain, (~ spl3_47 | ~ spl3_39), inference(avatar_split_clause, [], [f1729, f365, f399])).
fof(f1729, plain, (~ (e2 = op(e1, e0)) | ~ spl3_39), inference(forward_demodulation, [], [f140, f367])).
fof(f1720, plain, (spl3_31 | ~ spl3_21 | ~ spl3_67), inference(avatar_split_clause, [], [f1507, f517, f289, f331])).
fof(f289, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f1507, plain, ((e2 = op(e2, e0)) | (~ spl3_21 | ~ spl3_67)), inference(forward_demodulation, [], [f519, f291])).
fof(f291, plain, ((e0 = op(e2, e2)) | ~ spl3_21), inference(avatar_component_clause, [], [f289])).
fof(f1702, plain, (~ spl3_38 | ~ spl3_42), inference(avatar_split_clause, [], [f1697, f378, f361])).
fof(f1697, plain, (~ (e1 = op(e1, e2)) | ~ spl3_42), inference(backward_demodulation, [], [f142, f380])).
fof(f1694, plain, (~ spl3_47 | spl3_53 | ~ spl3_74), inference(avatar_contradiction_clause, [], [f1693])).
fof(f1693, plain, ($false | (~ spl3_47 | spl3_53 | ~ spl3_74)), inference(subsumption_resolution, [], [f1690, f426])).
fof(f426, plain, (~ (e0 = op(e0, e2)) | spl3_53), inference(avatar_component_clause, [], [f425])).
fof(f1690, plain, ((e0 = op(e0, e2)) | (~ spl3_47 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f401])).
fof(f1681, plain, (spl3_38 | ~ spl3_59 | ~ spl3_78), inference(avatar_split_clause, [], [f1678, f570, f450, f361])).
fof(f1678, plain, ((e1 = op(e1, e2)) | (~ spl3_59 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f452])).
fof(f452, plain, ((e2 = op(e0, e1)) | ~ spl3_59), inference(avatar_component_clause, [], [f450])).
fof(f1680, plain, (~ spl3_55 | ~ spl3_59), inference(avatar_split_clause, [], [f1676, f450, f433])).
fof(f1676, plain, (~ (e2 = op(e0, e2)) | ~ spl3_59), inference(backward_demodulation, [], [f136, f452])).
fof(f136, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f1674, plain, (~ spl3_61 | ~ spl3_64), inference(avatar_contradiction_clause, [], [f1673])).
fof(f1673, plain, ($false | (~ spl3_61 | ~ spl3_64)), inference(subsumption_resolution, [], [f1672, f159])).
fof(f1672, plain, ((e0 = e3) | (~ spl3_61 | ~ spl3_64)), inference(forward_demodulation, [], [f473, f461])).
fof(f1671, plain, (~ spl3_14 | ~ spl3_33 | ~ spl3_71), inference(avatar_contradiction_clause, [], [f1670])).
fof(f1670, plain, ($false | (~ spl3_14 | ~ spl3_33 | ~ spl3_71)), inference(subsumption_resolution, [], [f1669, f161])).
fof(f1669, plain, ((e1 = e3) | (~ spl3_14 | ~ spl3_33 | ~ spl3_71)), inference(forward_demodulation, [], [f1668, f261])).
fof(f1668, plain, ((e3 = op(e3, e0)) | (~ spl3_33 | ~ spl3_71)), inference(forward_demodulation, [], [f538, f342])).
fof(f1659, plain, (~ spl3_47 | ~ spl3_31), inference(avatar_split_clause, [], [f1658, f331, f399])).
fof(f1658, plain, (~ (e2 = op(e1, e0)) | ~ spl3_31), inference(forward_demodulation, [], [f112, f333])).
fof(f1654, plain, (~ spl3_40 | ~ spl3_8), inference(avatar_split_clause, [], [f1653, f233, f369])).
fof(f1653, plain, (~ (e3 = op(e1, e2)) | ~ spl3_8), inference(forward_demodulation, [], [f125, f235])).
fof(f1646, plain, (~ spl3_14 | spl3_57 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1645])).
fof(f1645, plain, ($false | (~ spl3_14 | spl3_57 | ~ spl3_83)), inference(subsumption_resolution, [], [f1642, f443])).
fof(f1642, plain, ((e0 = op(e0, e1)) | (~ spl3_14 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f261])).
fof(f1633, plain, (spl3_38 | ~ spl3_43 | ~ spl3_73), inference(avatar_split_clause, [], [f1630, f546, f382, f361])).
fof(f1630, plain, ((e1 = op(e1, e2)) | (~ spl3_43 | ~ spl3_73)), inference(backward_demodulation, [], [f548, f384])).
fof(f1626, plain, (spl3_4 | ~ spl3_52 | ~ spl3_76), inference(avatar_split_clause, [], [f1624, f560, f420, f216])).
fof(f1624, plain, ((e3 = op(e3, e3)) | (~ spl3_52 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f422])).
fof(f422, plain, ((e3 = op(e0, e3)) | ~ spl3_52), inference(avatar_component_clause, [], [f420])).
fof(f1613, plain, (spl3_42 | ~ spl3_58 | ~ spl3_78), inference(avatar_split_clause, [], [f1611, f570, f446, f378])).
fof(f1611, plain, ((e1 = op(e1, e1)) | (~ spl3_58 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f448])).
fof(f448, plain, ((e1 = op(e0, e1)) | ~ spl3_58), inference(avatar_component_clause, [], [f446])).
fof(f1612, plain, (~ spl3_42 | ~ spl3_58), inference(avatar_split_clause, [], [f1606, f446, f378])).
fof(f1606, plain, (~ (e1 = op(e1, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f115, f448])).
fof(f1575, plain, (~ spl3_6 | spl3_27 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1574])).
fof(f1574, plain, ($false | (~ spl3_6 | spl3_27 | ~ spl3_81)), inference(subsumption_resolution, [], [f1573, f315])).
fof(f315, plain, (~ (e2 = op(e2, e1)) | spl3_27), inference(avatar_component_clause, [], [f314])).
fof(f1573, plain, ((e2 = op(e2, e1)) | (~ spl3_6 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f227])).
fof(f1570, plain, (~ spl3_9 | spl3_46 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f1569])).
fof(f1569, plain, ($false | (~ spl3_9 | spl3_46 | ~ spl3_82)), inference(subsumption_resolution, [], [f1568, f396])).
fof(f1568, plain, ((e1 = op(e1, e0)) | (~ spl3_9 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f240])).
fof(f1563, plain, (~ spl3_15 | spl3_53 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1562])).
fof(f1562, plain, ($false | (~ spl3_15 | spl3_53 | ~ spl3_83)), inference(subsumption_resolution, [], [f1560, f426])).
fof(f1560, plain, ((e0 = op(e0, e2)) | (~ spl3_15 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f265])).
fof(f1550, plain, (~ spl3_10 | ~ spl3_42), inference(avatar_split_clause, [], [f1544, f378, f242])).
fof(f1544, plain, (~ (e1 = op(e3, e1)) | ~ spl3_42), inference(backward_demodulation, [], [f119, f380])).
fof(f1541, plain, (~ spl3_28 | ~ spl3_59 | ~ spl3_62 | spl3_99), inference(avatar_contradiction_clause, [], [f1540])).
fof(f1540, plain, ($false | (~ spl3_28 | ~ spl3_59 | ~ spl3_62 | spl3_99)), inference(subsumption_resolution, [], [f1536, f320])).
fof(f1536, plain, (~ (e3 = op(e2, e1)) | (~ spl3_59 | ~ spl3_62 | spl3_99)), inference(backward_demodulation, [], [f1273, f452])).
fof(f1273, plain, (~ (e3 = op(op(e0, e1), e1)) | (~ spl3_62 | spl3_99)), inference(backward_demodulation, [], [f670, f465])).
fof(f1534, plain, (~ spl3_61 | ~ spl3_62), inference(avatar_contradiction_clause, [], [f1533])).
fof(f1533, plain, ($false | (~ spl3_61 | ~ spl3_62)), inference(subsumption_resolution, [], [f1532, f157])).
fof(f157, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1532, plain, ((e0 = e1) | (~ spl3_61 | ~ spl3_62)), inference(backward_demodulation, [], [f465, f461])).
fof(f1531, plain, (spl3_59 | ~ spl3_62 | ~ spl3_98), inference(avatar_split_clause, [], [f1530, f664, f463, f450])).
fof(f1530, plain, ((e2 = op(e0, e1)) | (~ spl3_62 | ~ spl3_98)), inference(forward_demodulation, [], [f665, f465])).
fof(f1529, plain, (~ spl3_59 | ~ spl3_51), inference(avatar_split_clause, [], [f1528, f416, f450])).
fof(f1528, plain, (~ (e2 = op(e0, e1)) | ~ spl3_51), inference(forward_demodulation, [], [f137, f418])).
fof(f1527, plain, (spl3_57 | ~ spl3_62 | ~ spl3_79), inference(avatar_split_clause, [], [f1475, f575, f463, f442])).
fof(f1475, plain, ((e0 = op(e0, e1)) | (~ spl3_62 | ~ spl3_79)), inference(forward_demodulation, [], [f577, f465])).
fof(f1517, plain, (~ spl3_12 | ~ spl3_4), inference(avatar_split_clause, [], [f1516, f216, f250])).
fof(f1516, plain, (~ (e3 = op(e3, e1)) | ~ spl3_4), inference(forward_demodulation, [], [f155, f218])).
fof(f218, plain, ((e3 = op(e3, e3)) | ~ spl3_4), inference(avatar_component_clause, [], [f216])).
fof(f1496, plain, (~ spl3_10 | spl3_42 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f1495])).
fof(f1495, plain, ($false | (~ spl3_10 | spl3_42 | ~ spl3_82)), inference(subsumption_resolution, [], [f1494, f379])).
fof(f1494, plain, ((e1 = op(e1, e1)) | (~ spl3_10 | ~ spl3_82)), inference(forward_demodulation, [], [f592, f244])).
fof(f1493, plain, (~ spl3_13 | spl3_61 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1492])).
fof(f1492, plain, ($false | (~ spl3_13 | spl3_61 | ~ spl3_83)), inference(subsumption_resolution, [], [f1491, f460])).
fof(f1491, plain, ((e0 = op(e0, e0)) | (~ spl3_13 | ~ spl3_83)), inference(forward_demodulation, [], [f597, f257])).
fof(f257, plain, ((e0 = op(e3, e0)) | ~ spl3_13), inference(avatar_component_clause, [], [f255])).
fof(f255, plain, (spl3_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_13])])).
fof(f1446, plain, (spl3_27 | ~ spl3_38 | ~ spl3_72), inference(avatar_contradiction_clause, [], [f1445])).
fof(f1445, plain, ($false | (spl3_27 | ~ spl3_38 | ~ spl3_72)), inference(subsumption_resolution, [], [f1443, f315])).
fof(f1443, plain, ((e2 = op(e2, e1)) | (~ spl3_38 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f363])).
fof(f1427, plain, (~ spl3_49 | ~ spl3_57), inference(avatar_split_clause, [], [f1424, f442, f408])).
fof(f1424, plain, (~ (e0 = op(e0, e3)) | ~ spl3_57), inference(backward_demodulation, [], [f137, f444])).
fof(f1396, plain, (spl3_49 | ~ spl3_48 | ~ spl3_74), inference(avatar_split_clause, [], [f1158, f551, f403, f408])).
fof(f1158, plain, ((e0 = op(e0, e3)) | (~ spl3_48 | ~ spl3_74)), inference(forward_demodulation, [], [f553, f405])).
fof(f1350, plain, (spl3_41 | ~ spl3_42 | ~ spl3_104), inference(avatar_contradiction_clause, [], [f1349])).
fof(f1349, plain, ($false | (spl3_41 | ~ spl3_42 | ~ spl3_104)), inference(subsumption_resolution, [], [f1346, f375])).
fof(f375, plain, (~ (e0 = op(e1, e1)) | spl3_41), inference(avatar_component_clause, [], [f374])).
fof(f1346, plain, ((e0 = op(e1, e1)) | (~ spl3_42 | ~ spl3_104)), inference(backward_demodulation, [], [f693, f380])).
fof(f1347, plain, (~ spl3_34 | ~ spl3_42), inference(avatar_split_clause, [], [f1343, f378, f344])).
fof(f1343, plain, (~ (e1 = op(e1, e3)) | ~ spl3_42), inference(backward_demodulation, [], [f143, f380])).
fof(f143, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1314, plain, (~ spl3_73 | ~ spl3_104), inference(avatar_contradiction_clause, [], [f1313])).
fof(f1313, plain, ($false | (~ spl3_73 | ~ spl3_104)), inference(subsumption_resolution, [], [f1312, f157])).
fof(f1312, plain, ((e0 = e1) | (~ spl3_73 | ~ spl3_104)), inference(forward_demodulation, [], [f548, f693])).
fof(f1278, plain, (~ spl3_60 | ~ spl3_62 | spl3_101), inference(avatar_split_clause, [], [f1274, f678, f463, f454])).
fof(f1274, plain, (~ (e3 = op(e0, e1)) | (~ spl3_62 | spl3_101)), inference(backward_demodulation, [], [f680, f465])).
fof(f680, plain, (~ (e3 = op(e0, op(e0, e0))) | spl3_101), inference(avatar_component_clause, [], [f678])).
fof(f1190, plain, (~ spl3_19 | ~ spl3_31), inference(avatar_split_clause, [], [f1188, f331, f280])).
fof(f1188, plain, (~ (e2 = op(e2, e3)) | ~ spl3_31), inference(backward_demodulation, [], [f147, f333])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1155, plain, (~ spl3_33 | ~ spl3_49), inference(avatar_split_clause, [], [f1154, f408, f340])).
fof(f1154, plain, (~ (e0 = op(e1, e3)) | ~ spl3_49), inference(forward_demodulation, [], [f127, f410])).
fof(f1152, plain, (~ spl3_1 | ~ spl3_49), inference(avatar_split_clause, [], [f1151, f408, f204])).
fof(f1151, plain, (~ (e0 = op(e3, e3)) | ~ spl3_49), inference(forward_demodulation, [], [f129, f410])).
fof(f1132, plain, (~ spl3_25 | spl3_46 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f1131])).
fof(f1131, plain, ($false | (~ spl3_25 | spl3_46 | ~ spl3_68)), inference(subsumption_resolution, [], [f1130, f396])).
fof(f1130, plain, ((e1 = op(e1, e0)) | (~ spl3_25 | ~ spl3_68)), inference(forward_demodulation, [], [f524, f308])).
fof(f308, plain, ((e0 = op(e2, e1)) | ~ spl3_25), inference(avatar_component_clause, [], [f306])).
fof(f306, plain, (spl3_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_25])])).
fof(f1101, plain, (~ spl3_25 | ~ spl3_27), inference(avatar_contradiction_clause, [], [f1100])).
fof(f1100, plain, ($false | (~ spl3_25 | ~ spl3_27)), inference(subsumption_resolution, [], [f1099, f158])).
fof(f1099, plain, ((e0 = e2) | (~ spl3_25 | ~ spl3_27)), inference(backward_demodulation, [], [f316, f308])).
fof(f1097, plain, (~ spl3_33 | ~ spl3_35), inference(avatar_contradiction_clause, [], [f1096])).
fof(f1096, plain, ($false | (~ spl3_33 | ~ spl3_35)), inference(subsumption_resolution, [], [f1095, f158])).
fof(f1095, plain, ((e0 = e2) | (~ spl3_33 | ~ spl3_35)), inference(backward_demodulation, [], [f350, f342])).
fof(f1082, plain, (~ spl3_49 | ~ spl3_52), inference(avatar_contradiction_clause, [], [f1081])).
fof(f1081, plain, ($false | (~ spl3_49 | ~ spl3_52)), inference(subsumption_resolution, [], [f1079, f159])).
fof(f1079, plain, ((e0 = e3) | (~ spl3_49 | ~ spl3_52)), inference(backward_demodulation, [], [f422, f410])).
fof(f1061, plain, (spl3_49 | ~ spl3_32 | ~ spl3_69), inference(avatar_split_clause, [], [f1016, f527, f335, f408])).
fof(f1016, plain, ((e0 = op(e0, e3)) | (~ spl3_32 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f337])).
fof(f1053, plain, (~ spl3_44 | ~ spl3_12), inference(avatar_split_clause, [], [f1052, f250, f386])).
fof(f1052, plain, (~ (e3 = op(e1, e1)) | ~ spl3_12), inference(forward_demodulation, [], [f119, f252])).
fof(f252, plain, ((e3 = op(e3, e1)) | ~ spl3_12), inference(avatar_component_clause, [], [f250])).
fof(f1011, plain, (~ spl3_50 | ~ spl3_58), inference(avatar_split_clause, [], [f1010, f446, f412])).
fof(f1010, plain, (~ (e1 = op(e0, e3)) | ~ spl3_58), inference(forward_demodulation, [], [f137, f448])).
fof(f1009, plain, (~ spl3_30 | ~ spl3_22), inference(avatar_split_clause, [], [f1008, f293, f327])).
fof(f293, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f1008, plain, (~ (e1 = op(e2, e0)) | ~ spl3_22), inference(forward_demodulation, [], [f146, f295])).
fof(f295, plain, ((e1 = op(e2, e2)) | ~ spl3_22), inference(avatar_component_clause, [], [f293])).
fof(f1007, plain, (~ spl3_26 | ~ spl3_22), inference(avatar_split_clause, [], [f1006, f293, f310])).
fof(f1006, plain, (~ (e1 = op(e2, e1)) | ~ spl3_22), inference(forward_demodulation, [], [f148, f295])).
fof(f148, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1004, plain, (~ spl3_14 | ~ spl3_2), inference(avatar_split_clause, [], [f1003, f208, f259])).
fof(f1003, plain, (~ (e1 = op(e3, e0)) | ~ spl3_2), inference(forward_demodulation, [], [f153, f210])).
fof(f210, plain, ((e1 = op(e3, e3)) | ~ spl3_2), inference(avatar_component_clause, [], [f208])).
fof(f990, plain, (spl3_27 | ~ spl3_22 | ~ spl3_67), inference(avatar_split_clause, [], [f987, f517, f293, f314])).
fof(f987, plain, ((e2 = op(e2, e1)) | (~ spl3_22 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f295])).
fof(f986, plain, (~ spl3_28 | spl3_34 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f985])).
fof(f985, plain, ($false | (~ spl3_28 | spl3_34 | ~ spl3_68)), inference(subsumption_resolution, [], [f983, f345])).
fof(f983, plain, ((e1 = op(e1, e3)) | (~ spl3_28 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f320])).
fof(f981, plain, (~ spl3_31 | spl3_53 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f980])).
fof(f980, plain, ($false | (~ spl3_31 | spl3_53 | ~ spl3_69)), inference(subsumption_resolution, [], [f977, f426])).
fof(f977, plain, ((e0 = op(e0, e2)) | (~ spl3_31 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f333])).
fof(f970, plain, (~ spl3_30 | ~ spl3_46), inference(avatar_split_clause, [], [f968, f395, f327])).
fof(f968, plain, (~ (e1 = op(e2, e0)) | ~ spl3_46), inference(backward_demodulation, [], [f112, f397])).
fof(f952, plain, (~ spl3_53 | ~ spl3_5), inference(avatar_split_clause, [], [f951, f221, f425])).
fof(f951, plain, (~ (e0 = op(e0, e2)) | ~ spl3_5), inference(forward_demodulation, [], [f123, f223])).
fof(f948, plain, (~ spl3_50 | ~ spl3_52), inference(avatar_contradiction_clause, [], [f947])).
fof(f947, plain, ($false | (~ spl3_50 | ~ spl3_52)), inference(subsumption_resolution, [], [f946, f161])).
fof(f946, plain, ((e1 = e3) | (~ spl3_50 | ~ spl3_52)), inference(forward_demodulation, [], [f422, f414])).
fof(f939, plain, (~ spl3_47 | ~ spl3_41 | spl3_88), inference(avatar_split_clause, [], [f782, f618, f374, f399])).
fof(f782, plain, (~ (e2 = op(e1, e0)) | (~ spl3_41 | spl3_88)), inference(backward_demodulation, [], [f620, f376])).
fof(f932, plain, (~ spl3_33 | ~ spl3_17), inference(avatar_split_clause, [], [f931, f272, f340])).
fof(f931, plain, (~ (e0 = op(e1, e3)) | ~ spl3_17), inference(forward_demodulation, [], [f130, f274])).
fof(f130, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f928, plain, (~ spl3_30 | ~ spl3_14), inference(avatar_split_clause, [], [f927, f259, f327])).
fof(f927, plain, (~ (e1 = op(e2, e0)) | ~ spl3_14), inference(forward_demodulation, [], [f114, f261])).
fof(f920, plain, (~ spl3_21 | ~ spl3_5), inference(avatar_split_clause, [], [f919, f221, f289])).
fof(f919, plain, (~ (e0 = op(e2, e2)) | ~ spl3_5), inference(forward_demodulation, [], [f126, f223])).
fof(f910, plain, (~ spl3_17 | ~ spl3_19), inference(avatar_contradiction_clause, [], [f909])).
fof(f909, plain, ($false | (~ spl3_17 | ~ spl3_19)), inference(subsumption_resolution, [], [f908, f158])).
fof(f908, plain, ((e0 = e2) | (~ spl3_17 | ~ spl3_19)), inference(forward_demodulation, [], [f282, f274])).
fof(f898, plain, (~ spl3_11 | ~ spl3_2 | spl3_90), inference(avatar_split_clause, [], [f889, f627, f208, f246])).
fof(f889, plain, (~ (e2 = op(e3, e1)) | (~ spl3_2 | spl3_90)), inference(backward_demodulation, [], [f629, f210])).
fof(f865, plain, (~ spl3_14 | ~ spl3_15), inference(avatar_contradiction_clause, [], [f864])).
fof(f864, plain, ($false | (~ spl3_14 | ~ spl3_15)), inference(subsumption_resolution, [], [f863, f160])).
fof(f863, plain, ((e1 = e2) | (~ spl3_14 | ~ spl3_15)), inference(backward_demodulation, [], [f265, f261])).
fof(f862, plain, (~ spl3_15 | ~ spl3_16), inference(avatar_contradiction_clause, [], [f861])).
fof(f861, plain, ($false | (~ spl3_15 | ~ spl3_16)), inference(subsumption_resolution, [], [f860, f162])).
fof(f860, plain, ((e2 = e3) | (~ spl3_15 | ~ spl3_16)), inference(backward_demodulation, [], [f269, f265])).
fof(f858, plain, (~ spl3_8 | ~ spl3_16), inference(avatar_split_clause, [], [f855, f267, f233])).
fof(f855, plain, (~ (e3 = op(e3, e2)) | ~ spl3_16), inference(backward_demodulation, [], [f152, f269])).
fof(f152, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f847, plain, (~ spl3_19 | ~ spl3_20), inference(avatar_contradiction_clause, [], [f846])).
fof(f846, plain, ($false | (~ spl3_19 | ~ spl3_20)), inference(subsumption_resolution, [], [f845, f162])).
fof(f845, plain, ((e2 = e3) | (~ spl3_19 | ~ spl3_20)), inference(backward_demodulation, [], [f286, f282])).
fof(f286, plain, ((e3 = op(e2, e3)) | ~ spl3_20), inference(avatar_component_clause, [], [f284])).
fof(f284, plain, (spl3_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_20])])).
fof(f844, plain, (spl3_4 | ~ spl3_20 | ~ spl3_66), inference(avatar_split_clause, [], [f842, f512, f284, f216])).
fof(f842, plain, ((e3 = op(e3, e3)) | (~ spl3_20 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f286])).
fof(f827, plain, (~ spl3_25 | ~ spl3_45 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f826])).
fof(f826, plain, ($false | (~ spl3_25 | ~ spl3_45 | ~ spl3_68)), inference(subsumption_resolution, [], [f825, f157])).
fof(f825, plain, ((e0 = e1) | (~ spl3_25 | ~ spl3_45 | ~ spl3_68)), inference(forward_demodulation, [], [f821, f393])).
fof(f821, plain, ((e1 = op(e1, e0)) | (~ spl3_25 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f308])).
fof(f822, plain, (~ spl3_9 | ~ spl3_25), inference(avatar_split_clause, [], [f818, f306, f238])).
fof(f818, plain, (~ (e0 = op(e3, e1)) | ~ spl3_25), inference(backward_demodulation, [], [f120, f308])).
fof(f796, plain, (~ spl3_21 | ~ spl3_37), inference(avatar_split_clause, [], [f793, f357, f289])).
fof(f793, plain, (~ (e0 = op(e2, e2)) | ~ spl3_37), inference(backward_demodulation, [], [f124, f359])).
fof(f785, plain, (~ spl3_25 | ~ spl3_41), inference(avatar_split_clause, [], [f778, f374, f306])).
fof(f778, plain, (~ (e0 = op(e2, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f118, f376])).
fof(f776, plain, (~ spl3_37 | ~ spl3_45), inference(avatar_split_clause, [], [f771, f391, f357])).
fof(f771, plain, (~ (e0 = op(e1, e2)) | ~ spl3_45), inference(backward_demodulation, [], [f140, f393])).
fof(f755, plain, (~ spl3_49 | ~ spl3_53), inference(avatar_split_clause, [], [f751, f425, f408])).
fof(f751, plain, (~ (e0 = op(e0, e3)) | ~ spl3_53), inference(backward_demodulation, [], [f138, f427])).
fof(f753, plain, (~ spl3_21 | ~ spl3_53), inference(avatar_split_clause, [], [f749, f425, f289])).
fof(f749, plain, (~ (e0 = op(e2, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f122, f427])).
fof(f746, plain, (~ spl3_53 | ~ spl3_57), inference(avatar_split_clause, [], [f741, f442, f425])).
fof(f741, plain, (~ (e0 = op(e0, e2)) | ~ spl3_57), inference(backward_demodulation, [], [f136, f444])).
fof(f744, plain, (~ spl3_25 | ~ spl3_57), inference(avatar_split_clause, [], [f739, f442, f306])).
fof(f739, plain, (~ (e0 = op(e2, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f116, f444])).
fof(f734, plain, (~ spl3_49 | ~ spl3_61), inference(avatar_split_clause, [], [f725, f459, f408])).
fof(f725, plain, (~ (e0 = op(e0, e3)) | ~ spl3_61), inference(backward_demodulation, [], [f135, f461])).
fof(f732, plain, (~ spl3_57 | ~ spl3_61), inference(avatar_split_clause, [], [f723, f459, f442])).
fof(f723, plain, (~ (e0 = op(e0, e1)) | ~ spl3_61), inference(backward_demodulation, [], [f133, f461])).
fof(f731, plain, (~ spl3_13 | ~ spl3_61), inference(avatar_split_clause, [], [f722, f459, f255])).
fof(f722, plain, (~ (e0 = op(e3, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f111, f461])).
fof(f730, plain, (~ spl3_29 | ~ spl3_61), inference(avatar_split_clause, [], [f721, f459, f323])).
fof(f721, plain, (~ (e0 = op(e2, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f110, f461])).
fof(f718, plain, (~ spl3_44 | ~ spl3_88 | ~ spl3_107), inference(avatar_split_clause, [], [f202, f709, f618, f386])).
fof(f202, plain, (~ (e0 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(op(e1, op(e1, e1)), op(e1, e1))) & (e2 = op(e1, op(e1, e1))) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax29)).
fof(f713, plain, (~ spl3_44 | ~ spl3_104 | ~ spl3_93), inference(avatar_split_clause, [], [f197, f640, f692, f386])).
fof(f197, plain, (~ (e2 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e2 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e2 = op(op(e1, op(e1, e1)), op(e1, e1))) & (e0 = op(e1, op(e1, e1))) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax24)).
fof(f701, plain, (~ spl3_63 | ~ spl3_105 | ~ spl3_99), inference(avatar_split_clause, [], [f193, f668, f698, f467])).
fof(f193, plain, (~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0))) | ~ (e1 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0))) | ~ (e1 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e3 = op(op(e0, op(e0, e0)), op(e0, e0))) & (e1 = op(e0, op(e0, e0))) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax20)).
fof(f695, plain, (~ spl3_43 | ~ spl3_104 | ~ spl3_89), inference(avatar_split_clause, [], [f191, f622, f692, f382])).
fof(f191, plain, (~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(op(e1, op(e1, e1)), op(e1, e1))) & (e0 = op(e1, op(e1, e1))) & (e2 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax18)).
fof(f685, plain, (~ spl3_62 | ~ spl3_101 | ~ spl3_102), inference(avatar_split_clause, [], [f189, f682, f678, f463])).
fof(f189, plain, (~ (e2 = op(op(e0, op(e0, e0)), op(e0, e0))) | ~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e2 = op(op(e0, op(e0, e0)), op(e0, e0))) | ~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e2 = op(op(e0, op(e0, e0)), op(e0, e0))) & (e3 = op(e0, op(e0, e0))) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax16)).
fof(f671, plain, (~ spl3_62 | ~ spl3_98 | ~ spl3_99), inference(avatar_split_clause, [], [f187, f668, f664, f463])).
fof(f187, plain, (~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0))) | ~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e3 = op(op(e0, op(e0, e0)), op(e0, e0))) | ~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e3 = op(op(e0, op(e0, e0)), op(e0, e0))) & (e2 = op(e0, op(e0, e0))) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax14)).
fof(f634, plain, (~ spl3_1 | ~ spl3_90 | ~ spl3_91), inference(avatar_split_clause, [], [f182, f631, f627, f204])).
fof(f182, plain, (~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3))) | ~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(op(e3, op(e3, e3)), op(e3, e3))) | ~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(op(e3, op(e3, e3)), op(e3, e3))) & (e2 = op(e3, op(e3, e3))) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax9)).
fof(f625, plain, (~ spl3_41 | ~ spl3_88 | ~ spl3_89), inference(avatar_split_clause, [], [f181, f622, f618, f374])).
fof(f181, plain, (~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e3 = op(op(e1, op(e1, e1)), op(e1, e1))) | ~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e3 = op(op(e1, op(e1, e1)), op(e1, e1))) & (e2 = op(e1, op(e1, e1))) & (e0 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax8)).
fof(f598, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_83), inference(avatar_split_clause, [], [f175, f595, f508, f532, f556])).
fof(f556, plain, (spl3_75 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_75])])).
fof(f532, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f508, plain, (spl3_65 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f175, plain, ((e0 = op(e0, op(e3, e0))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f57, plain, (((e3 = op(e3, op(e3, e3))) & (e2 = op(e2, op(e3, e2))) & (e1 = op(e1, op(e3, e1))) & (e0 = op(e0, op(e3, e0)))) | sP2 | sP1 | sP0), inference(definition_folding, [], [f5, e56, e55, e54])).
fof(f54, plain, (((e3 = op(e3, op(e0, e3))) & (e2 = op(e2, op(e0, e2))) & (e1 = op(e1, op(e0, e1))) & (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((e3 = op(e3, op(e0, e3))) & (e2 = op(e2, op(e0, e2))) & (e1 = op(e1, op(e0, e1))) & (e0 = op(e0, op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((e3 = op(e3, op(e1, e3))) & (e2 = op(e2, op(e1, e2))) & (e1 = op(e1, op(e1, e1))) & (e0 = op(e0, op(e1, e0)))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((e3 = op(e3, op(e1, e3))) & (e2 = op(e2, op(e1, e2))) & (e1 = op(e1, op(e1, e1))) & (e0 = op(e0, op(e1, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((e3 = op(e3, op(e2, e3))) & (e2 = op(e2, op(e2, e2))) & (e1 = op(e1, op(e2, e1))) & (e0 = op(e0, op(e2, e0)))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((e3 = op(e3, op(e2, e3))) & (e2 = op(e2, op(e2, e2))) & (e1 = op(e1, op(e2, e1))) & (e0 = op(e0, op(e2, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f5, plain, (((e3 = op(e3, op(e3, e3))) & (e2 = op(e2, op(e3, e2))) & (e1 = op(e1, op(e3, e1))) & (e0 = op(e0, op(e3, e0)))) | ((e3 = op(e3, op(e2, e3))) & (e2 = op(e2, op(e2, e2))) & (e1 = op(e1, op(e2, e1))) & (e0 = op(e0, op(e2, e0)))) | ((e3 = op(e3, op(e1, e3))) & (e2 = op(e2, op(e1, e2))) & (e1 = op(e1, op(e1, e1))) & (e0 = op(e0, op(e1, e0)))) | ((e3 = op(e3, op(e0, e3))) & (e2 = op(e2, op(e0, e2))) & (e1 = op(e1, op(e0, e1))) & (e0 = op(e0, op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax5)).
fof(f593, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_82), inference(avatar_split_clause, [], [f176, f590, f508, f532, f556])).
fof(f176, plain, ((e1 = op(e1, op(e3, e1))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f588, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_81), inference(avatar_split_clause, [], [f177, f585, f508, f532, f556])).
fof(f177, plain, ((e2 = op(e2, op(e3, e2))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f583, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_80), inference(avatar_split_clause, [], [f178, f580, f508, f532, f556])).
fof(f178, plain, ((e3 = op(e3, op(e3, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f578, plain, (~ spl3_75 | spl3_79), inference(avatar_split_clause, [], [f171, f575, f556])).
fof(f171, plain, ((e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (((e3 = op(e3, op(e0, e3))) & (e2 = op(e2, op(e0, e2))) & (e1 = op(e1, op(e0, e1))) & (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f573, plain, (~ spl3_75 | spl3_78), inference(avatar_split_clause, [], [f172, f570, f556])).
fof(f172, plain, ((e1 = op(e1, op(e0, e1))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f568, plain, (~ spl3_75 | spl3_77), inference(avatar_split_clause, [], [f173, f565, f556])).
fof(f173, plain, ((e2 = op(e2, op(e0, e2))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f563, plain, (~ spl3_75 | spl3_76), inference(avatar_split_clause, [], [f174, f560, f556])).
fof(f174, plain, ((e3 = op(e3, op(e0, e3))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f554, plain, (~ spl3_70 | spl3_74), inference(avatar_split_clause, [], [f167, f551, f532])).
fof(f167, plain, ((e0 = op(e0, op(e1, e0))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f59, plain, (((e3 = op(e3, op(e1, e3))) & (e2 = op(e2, op(e1, e2))) & (e1 = op(e1, op(e1, e1))) & (e0 = op(e0, op(e1, e0)))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f549, plain, (~ spl3_70 | spl3_73), inference(avatar_split_clause, [], [f168, f546, f532])).
fof(f168, plain, ((e1 = op(e1, op(e1, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f544, plain, (~ spl3_70 | spl3_72), inference(avatar_split_clause, [], [f169, f541, f532])).
fof(f169, plain, ((e2 = op(e2, op(e1, e2))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f539, plain, (~ spl3_70 | spl3_71), inference(avatar_split_clause, [], [f170, f536, f532])).
fof(f170, plain, ((e3 = op(e3, op(e1, e3))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f530, plain, (~ spl3_65 | spl3_69), inference(avatar_split_clause, [], [f163, f527, f508])).
fof(f163, plain, ((e0 = op(e0, op(e2, e0))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((e3 = op(e3, op(e2, e3))) & (e2 = op(e2, op(e2, e2))) & (e1 = op(e1, op(e2, e1))) & (e0 = op(e0, op(e2, e0)))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f525, plain, (~ spl3_65 | spl3_68), inference(avatar_split_clause, [], [f164, f522, f508])).
fof(f164, plain, ((e1 = op(e1, op(e2, e1))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f520, plain, (~ spl3_65 | spl3_67), inference(avatar_split_clause, [], [f165, f517, f508])).
fof(f165, plain, ((e2 = op(e2, op(e2, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f515, plain, (~ spl3_65 | spl3_66), inference(avatar_split_clause, [], [f166, f512, f508])).
fof(f166, plain, ((e3 = op(e3, op(e2, e3))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f506, plain, (spl3_61 | spl3_57 | spl3_53 | spl3_49), inference(avatar_split_clause, [], [f77, f408, f425, f442, f459])).
fof(f77, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax2)).
fof(f505, plain, (spl3_61 | spl3_45 | spl3_29 | spl3_13), inference(avatar_split_clause, [], [f78, f255, f323, f391, f459])).
fof(f78, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f504, plain, (spl3_62 | spl3_58 | spl3_54 | spl3_50), inference(avatar_split_clause, [], [f79, f412, f429, f446, f463])).
fof(f79, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f503, plain, (spl3_62 | spl3_46 | spl3_30 | spl3_14), inference(avatar_split_clause, [], [f80, f259, f327, f395, f463])).
fof(f80, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f501, plain, (spl3_63 | spl3_47 | spl3_31 | spl3_15), inference(avatar_split_clause, [], [f82, f263, f331, f399, f467])).
fof(f82, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f500, plain, (spl3_64 | spl3_60 | spl3_56 | spl3_52), inference(avatar_split_clause, [], [f83, f420, f437, f454, f471])).
fof(f83, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f498, plain, (spl3_45 | spl3_41 | spl3_37 | spl3_33), inference(avatar_split_clause, [], [f85, f340, f357, f374, f391])).
fof(f85, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f495, plain, (spl3_58 | spl3_42 | spl3_26 | spl3_10), inference(avatar_split_clause, [], [f88, f242, f310, f378, f446])).
fof(f88, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f494, plain, (spl3_47 | spl3_43 | spl3_39 | spl3_35), inference(avatar_split_clause, [], [f89, f348, f365, f382, f399])).
fof(f89, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f491, plain, (spl3_60 | spl3_44 | spl3_28 | spl3_12), inference(avatar_split_clause, [], [f92, f250, f318, f386, f454])).
fof(f92, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f490, plain, (spl3_29 | spl3_25 | spl3_21 | spl3_17), inference(avatar_split_clause, [], [f93, f272, f289, f306, f323])).
fof(f93, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f488, plain, (spl3_30 | spl3_26 | spl3_22 | spl3_18), inference(avatar_split_clause, [], [f95, f276, f293, f310, f327])).
fof(f95, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f487, plain, (spl3_54 | spl3_38 | spl3_22 | spl3_6), inference(avatar_split_clause, [], [f96, f225, f293, f361, f429])).
fof(f96, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f484, plain, (spl3_32 | spl3_28 | spl3_24 | spl3_20), inference(avatar_split_clause, [], [f99, f284, f301, f318, f335])).
fof(f99, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f482, plain, (spl3_13 | spl3_9 | spl3_5 | spl3_1), inference(avatar_split_clause, [], [f101, f204, f221, f238, f255])).
fof(f101, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f478, plain, (spl3_15 | spl3_11 | spl3_7 | spl3_3), inference(avatar_split_clause, [], [f105, f212, f229, f246, f263])).
fof(f105, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f477, plain, (spl3_51 | spl3_35 | spl3_19 | spl3_3), inference(avatar_split_clause, [], [f106, f212, f280, f348, f416])).
fof(f106, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f474, plain, (spl3_61 | spl3_62 | spl3_63 | spl3_64), inference(avatar_split_clause, [], [f61, f471, f467, f463, f459])).
fof(f61, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG132+1.p', ax1)).
fof(f457, plain, (spl3_57 | spl3_58 | spl3_59 | spl3_60), inference(avatar_split_clause, [], [f62, f454, f450, f446, f442])).
fof(f62, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f440, plain, (spl3_53 | spl3_54 | spl3_55 | spl3_56), inference(avatar_split_clause, [], [f63, f437, f433, f429, f425])).
fof(f63, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f423, plain, (spl3_49 | spl3_50 | spl3_51 | spl3_52), inference(avatar_split_clause, [], [f64, f420, f416, f412, f408])).
fof(f64, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f406, plain, (spl3_45 | spl3_46 | spl3_47 | spl3_48), inference(avatar_split_clause, [], [f65, f403, f399, f395, f391])).
fof(f65, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f389, plain, (spl3_41 | spl3_42 | spl3_43 | spl3_44), inference(avatar_split_clause, [], [f66, f386, f382, f378, f374])).
fof(f66, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f372, plain, (spl3_37 | spl3_38 | spl3_39 | spl3_40), inference(avatar_split_clause, [], [f67, f369, f365, f361, f357])).
fof(f67, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f355, plain, (spl3_33 | spl3_34 | spl3_35 | spl3_36), inference(avatar_split_clause, [], [f68, f352, f348, f344, f340])).
fof(f68, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f338, plain, (spl3_29 | spl3_30 | spl3_31 | spl3_32), inference(avatar_split_clause, [], [f69, f335, f331, f327, f323])).
fof(f69, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f304, plain, (spl3_21 | spl3_22 | spl3_23 | spl3_24), inference(avatar_split_clause, [], [f71, f301, f297, f293, f289])).
fof(f71, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f270, plain, (spl3_13 | spl3_14 | spl3_15 | spl3_16), inference(avatar_split_clause, [], [f73, f267, f263, f259, f255])).
fof(f73, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f253, plain, (spl3_9 | spl3_10 | spl3_11 | spl3_12), inference(avatar_split_clause, [], [f74, f250, f246, f242, f238])).
fof(f74, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f236, plain, (spl3_5 | spl3_6 | spl3_7 | spl3_8), inference(avatar_split_clause, [], [f75, f233, f229, f225, f221])).
fof(f75, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f219, plain, (spl3_1 | spl3_2 | spl3_3 | spl3_4), inference(avatar_split_clause, [], [f76, f216, f212, f208, f204])).
fof(f76, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).