fof(f3959, plain, $false, inference(avatar_sat_refutation, [], [f327, f344, f361, f378, f395, f412, f429, f446, f463, f480, f497, f514, f531, f548, f565, f582, f583, f584, f585, f586, f587, f588, f589, f590, f591, f592, f593, f594, f595, f596, f597, f598, f599, f600, f601, f602, f603, f604, f605, f606, f607, f608, f609, f610, f611, f612, f613, f614, f619, f620, f621, f622, f623, f624, f629, f630, f631, f632, f633, f634, f639, f640, f641, f642, f643, f644, f649, f650, f651, f652, f653, f654, f663, f664, f669, f670, f671, f672, f673, f674, f679, f680, f681, f682, f683, f684, f689, f690, f691, f692, f693, f694, f699, f700, f701, f702, f703, f704, f712, f713, f714, f719, f720, f721, f722, f723, f724, f729, f730, f731, f732, f733, f734, f739, f740, f741, f742, f743, f744, f749, f750, f751, f752, f753, f754, f763, f764, f767, f768, f769, f770, f787, f804, f821, f838, f847, f856, f861, f870, f875, f880, f885, f890, f895, f909, f914, f919, f924, f925, f930, f935, f945, f950, f951, f956, f957, f974, f986, f996, f1003, f1020, f1039, f1051, f1056, f1076, f1077, f1080, f1083, f1086, f1095, f1102, f1105, f1110, f1113, f1125, f1138, f1142, f1151, f1165, f1197, f1207, f1218, f1234, f1243, f1253, f1282, f1309, f1315, f1331, f1334, f1340, f1343, f1351, f1352, f1380, f1382, f1391, f1398, f1399, f1402, f1411, f1417, f1420, f1422, f1467, f1471, f1483, f1490, f1491, f1524, f1535, f1542, f1556, f1576, f1581, f1589, f1599, f1608, f1617, f1623, f1624, f1634, f1644, f1668, f1671, f1675, f1676, f1685, f1691, f1697, f1707, f1731, f1739, f1742, f1762, f1763, f1780, f1786, f1797, f1807, f1808, f1811, f1820, f1821, f1822, f1834, f1849, f1852, f1859, f1861, f1864, f1867, f1884, f1902, f1903, f1905, f1929, f1949, f1951, f1981, f1982, f1995, f1996, f2024, f2048, f2089, f2096, f2107, f2136, f2162, f2170, f2171, f2180, f2198, f2202, f2203, f2222, f2226, f2236, f2253, f2258, f2260, f2267, f2289, f2291, f2303, f2315, f2316, f2317, f2320, f2340, f2347, f2348, f2350, f2357, f2360, f2364, f2373, f2386, f2393, f2409, f2422, f2434, f2439, f2445, f2454, f2460, f2469, f2505, f2511, f2521, f2523, f2543, f2569, f2571, f2575, f2593, f2594, f2597, f2606, f2613, f2615, f2621, f2623, f2625, f2627, f2646, f2654, f2655, f2668, f2669, f2670, f2690, f2703, f2708, f2717, f2721, f2724, f2728, f2731, f2743, f2760, f2761, f2763, f2783, f2784, f2809, f2811, f2822, f2825, f2837, f2867, f2873, f2903, f2904, f2932, f2933, f2939, f2941, f2961, f2965, f2969, f2971, f2972, f2974, f2984, f3003, f3012, f3020, f3021, f3034, f3046, f3106, f3115, f3119, f3120, f3125, f3126, f3138, f3140, f3142, f3162, f3169, f3199, f3208, f3210, f3214, f3240, f3241, f3243, f3249, f3257, f3263, f3275, f3280, f3287, f3292, f3303, f3304, f3306, f3317, f3324, f3341, f3345, f3358, f3359, f3362, f3371, f3380, f3381, f3390, f3399, f3405, f3406, f3408, f3414, f3422, f3424, f3430, f3443, f3454, f3461, f3479, f3494, f3504, f3505, f3515, f3522, f3523, f3532, f3533, f3546, f3557, f3565, f3567, f3573, f3582, f3583, f3584, f3595, f3601, f3609, f3614, f3657, f3666, f3673, f3698, f3701, f3715, f3725, f3727, f3736, f3737, f3750, f3751, f3757, f3764, f3776, f3777, f3787, f3788, f3791, f3795, f3813, f3820, f3822, f3851, f3852, f3854, f3868, f3869, f3880, f3881, f3887, f3898, f3905, f3906, f3908, f3914, f3921, f3930, f3937, f3947, f3949, f3953])).
fof(f3953, plain, (~ spl15_3 | ~ spl15_11), inference(avatar_split_clause, [], [f3950, f354, f320])).
fof(f320, plain, (spl15_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f354, plain, (spl15_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f3950, plain, (~ (e2 = op(e3, e3)) | ~ spl15_11), inference(backward_demodulation, [], [f179, f356])).
fof(f356, plain, ((e2 = op(e3, e1)) | ~ spl15_11), inference(avatar_component_clause, [], [f354])).
fof(f179, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax3)).
fof(f3949, plain, (~ spl15_14 | spl15_34 | ~ spl15_95), inference(avatar_contradiction_clause, [], [f3948])).
fof(f3948, plain, ($false | (~ spl15_14 | spl15_34 | ~ spl15_95)), inference(subsumption_resolution, [], [f3946, f453])).
fof(f453, plain, (~ (e1 = op(e1, e3)) | spl15_34), inference(avatar_component_clause, [], [f452])).
fof(f452, plain, (spl15_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_34])])).
fof(f3946, plain, ((e1 = op(e1, e3)) | (~ spl15_14 | ~ spl15_95)), inference(backward_demodulation, [], [f837, f369])).
fof(f369, plain, ((e1 = op(e3, e0)) | ~ spl15_14), inference(avatar_component_clause, [], [f367])).
fof(f367, plain, (spl15_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f837, plain, ((op(e3, e0) = op(op(e3, e0), e3)) | ~ spl15_95), inference(avatar_component_clause, [], [f835])).
fof(f835, plain, (spl15_95 <=> (op(e3, e0) = op(op(e3, e0), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_95])])).
fof(f3947, plain, (~ spl15_10 | ~ spl15_14), inference(avatar_split_clause, [], [f3942, f367, f350])).
fof(f350, plain, (spl15_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f3942, plain, (~ (e1 = op(e3, e1)) | ~ spl15_14), inference(backward_demodulation, [], [f175, f369])).
fof(f175, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3937, plain, (~ spl15_17 | ~ spl15_24 | spl15_110), inference(avatar_split_clause, [], [f3934, f906, f409, f380])).
fof(f380, plain, (spl15_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_17])])).
fof(f409, plain, (spl15_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_24])])).
fof(f906, plain, (spl15_110 <=> (e0 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_110])])).
fof(f3934, plain, (~ (e0 = op(e2, e3)) | (~ spl15_24 | spl15_110)), inference(backward_demodulation, [], [f908, f411])).
fof(f411, plain, ((e3 = op(e2, e2)) | ~ spl15_24), inference(avatar_component_clause, [], [f409])).
fof(f908, plain, (~ (e0 = op(e2, op(e2, e2))) | spl15_110), inference(avatar_component_clause, [], [f906])).
fof(f3930, plain, (~ spl15_17 | ~ spl15_25), inference(avatar_split_clause, [], [f3925, f414, f380])).
fof(f414, plain, (spl15_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_25])])).
fof(f3925, plain, (~ (e0 = op(e2, e3)) | ~ spl15_25), inference(backward_demodulation, [], [f173, f416])).
fof(f416, plain, ((e0 = op(e2, e1)) | ~ spl15_25), inference(avatar_component_clause, [], [f414])).
fof(f173, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3921, plain, (~ spl15_23 | ~ spl15_31), inference(avatar_split_clause, [], [f3917, f439, f405])).
fof(f405, plain, (spl15_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_23])])).
fof(f439, plain, (spl15_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_31])])).
fof(f3917, plain, (~ (e2 = op(e2, e2)) | ~ spl15_31), inference(backward_demodulation, [], [f170, f441])).
fof(f441, plain, ((e2 = op(e2, e0)) | ~ spl15_31), inference(avatar_component_clause, [], [f439])).
fof(f170, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3914, plain, (~ spl15_3 | ~ spl15_35), inference(avatar_split_clause, [], [f3911, f456, f320])).
fof(f456, plain, (spl15_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_35])])).
fof(f3911, plain, (~ (e2 = op(e3, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f155, f458])).
fof(f458, plain, ((e2 = op(e1, e3)) | ~ spl15_35), inference(avatar_component_clause, [], [f456])).
fof(f155, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3908, plain, (spl15_12 | ~ spl15_44 | ~ spl15_89), inference(avatar_contradiction_clause, [], [f3907])).
fof(f3907, plain, ($false | (spl15_12 | ~ spl15_44 | ~ spl15_89)), inference(subsumption_resolution, [], [f3902, f359])).
fof(f359, plain, (~ (e3 = op(e3, e1)) | spl15_12), inference(avatar_component_clause, [], [f358])).
fof(f358, plain, (spl15_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f3902, plain, ((e3 = op(e3, e1)) | (~ spl15_44 | ~ spl15_89)), inference(backward_demodulation, [], [f812, f496])).
fof(f496, plain, ((e3 = op(e1, e1)) | ~ spl15_44), inference(avatar_component_clause, [], [f494])).
fof(f494, plain, (spl15_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_44])])).
fof(f812, plain, ((op(e1, e1) = op(op(e1, e1), e1)) | ~ spl15_89), inference(avatar_component_clause, [], [f810])).
fof(f810, plain, (spl15_89 <=> (op(e1, e1) = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_89])])).
fof(f3906, plain, (~ spl15_36 | ~ spl15_44), inference(avatar_split_clause, [], [f3901, f494, f460])).
fof(f460, plain, (spl15_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_36])])).
fof(f3901, plain, (~ (e3 = op(e1, e3)) | ~ spl15_44), inference(backward_demodulation, [], [f167, f496])).
fof(f167, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3905, plain, (~ spl15_28 | ~ spl15_44), inference(avatar_split_clause, [], [f3899, f494, f426])).
fof(f426, plain, (spl15_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_28])])).
fof(f3899, plain, (~ (e3 = op(e2, e1)) | ~ spl15_44), inference(backward_demodulation, [], [f142, f496])).
fof(f142, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3898, plain, (~ spl15_17 | ~ spl15_49), inference(avatar_split_clause, [], [f3890, f516, f380])).
fof(f516, plain, (spl15_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_49])])).
fof(f3890, plain, (~ (e0 = op(e2, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f152, f518])).
fof(f518, plain, ((e0 = op(e0, e3)) | ~ spl15_49), inference(avatar_component_clause, [], [f516])).
fof(f152, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3887, plain, (~ spl15_23 | ~ spl15_55), inference(avatar_split_clause, [], [f3883, f541, f405])).
fof(f541, plain, (spl15_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_55])])).
fof(f3883, plain, (~ (e2 = op(e2, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f146, f543])).
fof(f543, plain, ((e2 = op(e0, e2)) | ~ spl15_55), inference(avatar_component_clause, [], [f541])).
fof(f146, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3881, plain, (~ spl15_50 | ~ spl15_58), inference(avatar_split_clause, [], [f3876, f554, f520])).
fof(f520, plain, (spl15_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_50])])).
fof(f554, plain, (spl15_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_58])])).
fof(f3876, plain, (~ (e1 = op(e0, e3)) | ~ spl15_58), inference(backward_demodulation, [], [f161, f556])).
fof(f556, plain, ((e1 = op(e0, e1)) | ~ spl15_58), inference(avatar_component_clause, [], [f554])).
fof(f161, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3880, plain, (~ spl15_10 | ~ spl15_58), inference(avatar_split_clause, [], [f3874, f554, f350])).
fof(f3874, plain, (~ (e1 = op(e3, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f141, f556])).
fof(f141, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3869, plain, (~ spl15_56 | ~ spl15_64), inference(avatar_split_clause, [], [f3860, f579, f545])).
fof(f545, plain, (spl15_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_56])])).
fof(f579, plain, (spl15_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl15_64])])).
fof(f3860, plain, (~ (e3 = op(e0, e2)) | ~ spl15_64), inference(backward_demodulation, [], [f158, f581])).
fof(f581, plain, ((op(e0, e0) = e3) | ~ spl15_64), inference(avatar_component_clause, [], [f579])).
fof(f158, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3868, plain, (~ spl15_16 | ~ spl15_64), inference(avatar_split_clause, [], [f3858, f579, f375])).
fof(f375, plain, (spl15_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f3858, plain, (~ (e3 = op(e3, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f135, f581])).
fof(f135, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3854, plain, (~ spl15_57 | ~ spl15_45 | spl15_93), inference(avatar_split_clause, [], [f3853, f827, f499, f550])).
fof(f550, plain, (spl15_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_57])])).
fof(f499, plain, (spl15_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_45])])).
fof(f827, plain, (spl15_93 <=> (op(e1, e0) = op(op(e1, e0), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_93])])).
fof(f3853, plain, (~ (e0 = op(e0, e1)) | (~ spl15_45 | spl15_93)), inference(forward_demodulation, [], [f828, f501])).
fof(f501, plain, ((e0 = op(e1, e0)) | ~ spl15_45), inference(avatar_component_clause, [], [f499])).
fof(f828, plain, (~ (op(e1, e0) = op(op(e1, e0), e1)) | spl15_93), inference(avatar_component_clause, [], [f827])).
fof(f3852, plain, (~ spl15_28 | ~ spl15_103 | spl15_117), inference(avatar_split_clause, [], [f3850, f942, f872, f426])).
fof(f872, plain, (spl15_103 <=> (e1 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_103])])).
fof(f942, plain, (spl15_117 <=> (e3 = op(e2, op(e2, op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl15_117])])).
fof(f3850, plain, (~ (e3 = op(e2, e1)) | (~ spl15_103 | spl15_117)), inference(backward_demodulation, [], [f944, f873])).
fof(f873, plain, ((e1 = op(e2, op(e2, e2))) | ~ spl15_103), inference(avatar_component_clause, [], [f872])).
fof(f944, plain, (~ (e3 = op(e2, op(e2, op(e2, e2)))) | spl15_117), inference(avatar_component_clause, [], [f942])).
fof(f3851, plain, (~ spl15_25 | spl15_98 | ~ spl15_103), inference(avatar_split_clause, [], [f3847, f872, f849, f414])).
fof(f849, plain, (spl15_98 <=> (e0 = op(e2, op(e2, op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl15_98])])).
fof(f3847, plain, (~ (e0 = op(e2, e1)) | (spl15_98 | ~ spl15_103)), inference(backward_demodulation, [], [f851, f873])).
fof(f851, plain, (~ (e0 = op(e2, op(e2, op(e2, e2)))) | spl15_98), inference(avatar_component_clause, [], [f849])).
fof(f3822, plain, (~ spl15_10 | spl15_34 | ~ spl15_91), inference(avatar_contradiction_clause, [], [f3821])).
fof(f3821, plain, ($false | (~ spl15_10 | spl15_34 | ~ spl15_91)), inference(subsumption_resolution, [], [f3819, f453])).
fof(f3819, plain, ((e1 = op(e1, e3)) | (~ spl15_10 | ~ spl15_91)), inference(backward_demodulation, [], [f820, f352])).
fof(f352, plain, ((e1 = op(e3, e1)) | ~ spl15_10), inference(avatar_component_clause, [], [f350])).
fof(f820, plain, ((op(e3, e1) = op(op(e3, e1), e3)) | ~ spl15_91), inference(avatar_component_clause, [], [f818])).
fof(f818, plain, (spl15_91 <=> (op(e3, e1) = op(op(e3, e1), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_91])])).
fof(f3820, plain, (~ spl15_6 | ~ spl15_10), inference(avatar_split_clause, [], [f3815, f350, f333])).
fof(f333, plain, (spl15_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_6])])).
fof(f3815, plain, (~ (e1 = op(e3, e2)) | ~ spl15_10), inference(backward_demodulation, [], [f178, f352])).
fof(f178, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3813, plain, (~ spl15_4 | ~ spl15_16), inference(avatar_split_clause, [], [f3811, f375, f324])).
fof(f324, plain, (spl15_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f3811, plain, (~ (e3 = op(e3, e3)) | ~ spl15_16), inference(backward_demodulation, [], [f177, f377])).
fof(f377, plain, ((e3 = op(e3, e0)) | ~ spl15_16), inference(avatar_component_clause, [], [f375])).
fof(f177, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3795, plain, (~ spl15_24 | ~ spl15_28), inference(avatar_split_clause, [], [f3793, f426, f409])).
fof(f3793, plain, (~ (e3 = op(e2, e2)) | ~ spl15_28), inference(backward_demodulation, [], [f172, f428])).
fof(f428, plain, ((e3 = op(e2, e1)) | ~ spl15_28), inference(avatar_component_clause, [], [f426])).
fof(f172, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3791, plain, (~ spl15_4 | ~ spl15_36), inference(avatar_split_clause, [], [f3789, f460, f324])).
fof(f3789, plain, (~ (e3 = op(e3, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f155, f462])).
fof(f462, plain, ((e3 = op(e1, e3)) | ~ spl15_36), inference(avatar_component_clause, [], [f460])).
fof(f3788, plain, (~ spl15_34 | ~ spl15_38), inference(avatar_split_clause, [], [f3781, f469, f452])).
fof(f469, plain, (spl15_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_38])])).
fof(f3781, plain, (~ (e1 = op(e1, e3)) | ~ spl15_38), inference(backward_demodulation, [], [f168, f471])).
fof(f471, plain, ((e1 = op(e1, e2)) | ~ spl15_38), inference(avatar_component_clause, [], [f469])).
fof(f168, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3787, plain, (~ spl15_6 | ~ spl15_38), inference(avatar_split_clause, [], [f3780, f469, f333])).
fof(f3780, plain, (~ (e1 = op(e3, e2)) | ~ spl15_38), inference(backward_demodulation, [], [f149, f471])).
fof(f149, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3777, plain, (~ spl15_39 | ~ spl15_43), inference(avatar_split_clause, [], [f3768, f490, f473])).
fof(f473, plain, (spl15_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_39])])).
fof(f490, plain, (spl15_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_43])])).
fof(f3768, plain, (~ (e2 = op(e1, e2)) | ~ spl15_43), inference(backward_demodulation, [], [f166, f492])).
fof(f492, plain, ((e2 = op(e1, e1)) | ~ spl15_43), inference(avatar_component_clause, [], [f490])).
fof(f166, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3776, plain, (~ spl15_27 | ~ spl15_43), inference(avatar_split_clause, [], [f3766, f490, f422])).
fof(f422, plain, (spl15_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_27])])).
fof(f3766, plain, (~ (e2 = op(e2, e1)) | ~ spl15_43), inference(backward_demodulation, [], [f142, f492])).
fof(f3764, plain, (~ spl15_34 | ~ spl15_50), inference(avatar_split_clause, [], [f3759, f520, f452])).
fof(f3759, plain, (~ (e1 = op(e1, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f151, f522])).
fof(f522, plain, ((e1 = op(e0, e3)) | ~ spl15_50), inference(avatar_component_clause, [], [f520])).
fof(f151, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3757, plain, (~ spl15_24 | ~ spl15_56), inference(avatar_split_clause, [], [f3753, f545, f409])).
fof(f3753, plain, (~ (e3 = op(e2, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f146, f547])).
fof(f547, plain, ((e3 = op(e0, e2)) | ~ spl15_56), inference(avatar_component_clause, [], [f545])).
fof(f3751, plain, (~ spl15_53 | ~ spl15_57), inference(avatar_split_clause, [], [f3744, f550, f533])).
fof(f533, plain, (spl15_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_53])])).
fof(f3744, plain, (~ (e0 = op(e0, e2)) | ~ spl15_57), inference(backward_demodulation, [], [f160, f552])).
fof(f552, plain, ((e0 = op(e0, e1)) | ~ spl15_57), inference(avatar_component_clause, [], [f550])).
fof(f160, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3750, plain, (~ spl15_9 | ~ spl15_57), inference(avatar_split_clause, [], [f3743, f550, f346])).
fof(f346, plain, (spl15_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f3743, plain, (~ (e0 = op(e3, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f141, f552])).
fof(f3737, plain, (~ spl15_51 | ~ spl15_63), inference(avatar_split_clause, [], [f3731, f575, f524])).
fof(f524, plain, (spl15_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_51])])).
fof(f575, plain, (spl15_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl15_63])])).
fof(f3731, plain, (~ (e2 = op(e0, e3)) | ~ spl15_63), inference(backward_demodulation, [], [f159, f577])).
fof(f577, plain, ((op(e0, e0) = e2) | ~ spl15_63), inference(avatar_component_clause, [], [f575])).
fof(f159, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3736, plain, (~ spl15_15 | ~ spl15_63), inference(avatar_split_clause, [], [f3728, f575, f371])).
fof(f371, plain, (spl15_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f3728, plain, (~ (e2 = op(e3, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f135, f577])).
fof(f3727, plain, (spl15_53 | ~ spl15_17 | ~ spl15_82), inference(avatar_split_clause, [], [f3726, f780, f380, f533])).
fof(f780, plain, (spl15_82 <=> (op(e2, e3) = op(op(e2, e3), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_82])])).
fof(f3726, plain, ((e0 = op(e0, e2)) | (~ spl15_17 | ~ spl15_82)), inference(forward_demodulation, [], [f782, f382])).
fof(f382, plain, ((e0 = op(e2, e3)) | ~ spl15_17), inference(avatar_component_clause, [], [f380])).
fof(f782, plain, ((op(e2, e3) = op(op(e2, e3), e2)) | ~ spl15_82), inference(avatar_component_clause, [], [f780])).
fof(f3725, plain, (spl15_57 | ~ spl15_45 | ~ spl15_93), inference(avatar_split_clause, [], [f3724, f827, f499, f550])).
fof(f3724, plain, ((e0 = op(e0, e1)) | (~ spl15_45 | ~ spl15_93)), inference(forward_demodulation, [], [f829, f501])).
fof(f829, plain, ((op(e1, e0) = op(op(e1, e0), e1)) | ~ spl15_93), inference(avatar_component_clause, [], [f827])).
fof(f3715, plain, (~ spl15_38 | ~ spl15_30 | spl15_94), inference(avatar_split_clause, [], [f3645, f831, f435, f469])).
fof(f435, plain, (spl15_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_30])])).
fof(f831, plain, (spl15_94 <=> (op(e2, e0) = op(op(e2, e0), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_94])])).
fof(f3645, plain, (~ (e1 = op(e1, e2)) | (~ spl15_30 | spl15_94)), inference(forward_demodulation, [], [f832, f437])).
fof(f437, plain, ((e1 = op(e2, e0)) | ~ spl15_30), inference(avatar_component_clause, [], [f435])).
fof(f832, plain, (~ (op(e2, e0) = op(op(e2, e0), e2)) | spl15_94), inference(avatar_component_clause, [], [f831])).
fof(f3701, plain, (~ spl15_15 | ~ spl15_17 | ~ spl15_95), inference(avatar_contradiction_clause, [], [f3700])).
fof(f3700, plain, ($false | (~ spl15_15 | ~ spl15_17 | ~ spl15_95)), inference(subsumption_resolution, [], [f3699, f182])).
fof(f182, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax4)).
fof(f3699, plain, ((e0 = e2) | (~ spl15_15 | ~ spl15_17 | ~ spl15_95)), inference(forward_demodulation, [], [f3697, f382])).
fof(f3697, plain, ((e2 = op(e2, e3)) | (~ spl15_15 | ~ spl15_95)), inference(backward_demodulation, [], [f837, f373])).
fof(f373, plain, ((e2 = op(e3, e0)) | ~ spl15_15), inference(avatar_component_clause, [], [f371])).
fof(f3698, plain, (~ spl15_3 | ~ spl15_15), inference(avatar_split_clause, [], [f3695, f371, f320])).
fof(f3695, plain, (~ (e2 = op(e3, e3)) | ~ spl15_15), inference(backward_demodulation, [], [f177, f373])).
fof(f3673, plain, (~ spl15_3 | ~ spl15_51), inference(avatar_split_clause, [], [f3670, f524, f320])).
fof(f3670, plain, (~ (e2 = op(e3, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f153, f526])).
fof(f526, plain, ((e2 = op(e0, e3)) | ~ spl15_51), inference(avatar_component_clause, [], [f524])).
fof(f153, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3666, plain, (~ spl15_42 | ~ spl15_58), inference(avatar_split_clause, [], [f3661, f554, f486])).
fof(f486, plain, (spl15_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_42])])).
fof(f3661, plain, (~ (e1 = op(e1, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f139, f556])).
fof(f139, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3657, plain, (~ spl15_60 | ~ spl15_64), inference(avatar_split_clause, [], [f3651, f579, f562])).
fof(f562, plain, (spl15_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_60])])).
fof(f3651, plain, (~ (e3 = op(e0, e1)) | ~ spl15_64), inference(backward_demodulation, [], [f157, f581])).
fof(f157, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f3614, plain, (~ spl15_2 | ~ spl15_6), inference(avatar_split_clause, [], [f3610, f333, f316])).
fof(f316, plain, (spl15_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_2])])).
fof(f3610, plain, (~ (e1 = op(e3, e3)) | ~ spl15_6), inference(backward_demodulation, [], [f180, f335])).
fof(f335, plain, ((e1 = op(e3, e2)) | ~ spl15_6), inference(avatar_component_clause, [], [f333])).
fof(f180, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3609, plain, (~ spl15_9 | spl15_49 | ~ spl15_91), inference(avatar_contradiction_clause, [], [f3608])).
fof(f3608, plain, ($false | (~ spl15_9 | spl15_49 | ~ spl15_91)), inference(subsumption_resolution, [], [f3607, f517])).
fof(f517, plain, (~ (e0 = op(e0, e3)) | spl15_49), inference(avatar_component_clause, [], [f516])).
fof(f3607, plain, ((e0 = op(e0, e3)) | (~ spl15_9 | ~ spl15_91)), inference(backward_demodulation, [], [f820, f348])).
fof(f348, plain, ((e0 = op(e3, e1)) | ~ spl15_9), inference(avatar_component_clause, [], [f346])).
fof(f3601, plain, (~ spl15_8 | ~ spl15_16), inference(avatar_split_clause, [], [f3598, f375, f341])).
fof(f341, plain, (spl15_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f3598, plain, (~ (e3 = op(e3, e2)) | ~ spl15_16), inference(backward_demodulation, [], [f176, f377])).
fof(f176, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3595, plain, (~ spl15_17 | ~ spl15_24 | ~ spl15_30 | spl15_106), inference(avatar_contradiction_clause, [], [f3594])).
fof(f3594, plain, ($false | (~ spl15_17 | ~ spl15_24 | ~ spl15_30 | spl15_106)), inference(subsumption_resolution, [], [f3592, f437])).
fof(f3592, plain, (~ (e1 = op(e2, e0)) | (~ spl15_17 | ~ spl15_24 | spl15_106)), inference(backward_demodulation, [], [f3580, f382])).
fof(f3580, plain, (~ (e1 = op(e2, op(e2, e3))) | (~ spl15_24 | spl15_106)), inference(backward_demodulation, [], [f889, f411])).
fof(f889, plain, (~ (e1 = op(e2, op(e2, op(e2, e2)))) | spl15_106), inference(avatar_component_clause, [], [f887])).
fof(f887, plain, (spl15_106 <=> (e1 = op(e2, op(e2, op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl15_106])])).
fof(f3584, plain, (spl15_8 | ~ spl15_24 | ~ spl15_86), inference(avatar_split_clause, [], [f3576, f797, f409, f341])).
fof(f797, plain, (spl15_86 <=> (op(e2, e2) = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_86])])).
fof(f3576, plain, ((e3 = op(e3, e2)) | (~ spl15_24 | ~ spl15_86)), inference(backward_demodulation, [], [f799, f411])).
fof(f799, plain, ((op(e2, e2) = op(op(e2, e2), e2)) | ~ spl15_86), inference(avatar_component_clause, [], [f797])).
fof(f3583, plain, (~ spl15_20 | ~ spl15_24), inference(avatar_split_clause, [], [f3575, f409, f392])).
fof(f392, plain, (spl15_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f3575, plain, (~ (e3 = op(e2, e3)) | ~ spl15_24), inference(backward_demodulation, [], [f174, f411])).
fof(f174, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3582, plain, (~ spl15_8 | ~ spl15_24), inference(avatar_split_clause, [], [f3574, f409, f341])).
fof(f3574, plain, (~ (e3 = op(e3, e2)) | ~ spl15_24), inference(backward_demodulation, [], [f150, f411])).
fof(f150, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3573, plain, (~ spl15_11 | ~ spl15_27), inference(avatar_split_clause, [], [f3568, f422, f354])).
fof(f3568, plain, (~ (e2 = op(e3, e1)) | ~ spl15_27), inference(backward_demodulation, [], [f144, f424])).
fof(f424, plain, ((e2 = op(e2, e1)) | ~ spl15_27), inference(avatar_component_clause, [], [f422])).
fof(f144, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3567, plain, (~ spl15_30 | spl15_38 | ~ spl15_94), inference(avatar_contradiction_clause, [], [f3566])).
fof(f3566, plain, ($false | (~ spl15_30 | spl15_38 | ~ spl15_94)), inference(subsumption_resolution, [], [f3564, f470])).
fof(f470, plain, (~ (e1 = op(e1, e2)) | spl15_38), inference(avatar_component_clause, [], [f469])).
fof(f3564, plain, ((e1 = op(e1, e2)) | (~ spl15_30 | ~ spl15_94)), inference(backward_demodulation, [], [f833, f437])).
fof(f833, plain, ((op(e2, e0) = op(op(e2, e0), e2)) | ~ spl15_94), inference(avatar_component_clause, [], [f831])).
fof(f3565, plain, (~ spl15_26 | ~ spl15_30), inference(avatar_split_clause, [], [f3559, f435, f418])).
fof(f418, plain, (spl15_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_26])])).
fof(f3559, plain, (~ (e1 = op(e2, e1)) | ~ spl15_30), inference(backward_demodulation, [], [f169, f437])).
fof(f169, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3557, plain, (~ spl15_20 | ~ spl15_36), inference(avatar_split_clause, [], [f3554, f460, f392])).
fof(f3554, plain, (~ (e3 = op(e2, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f154, f462])).
fof(f154, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3546, plain, (~ spl15_26 | ~ spl15_42), inference(avatar_split_clause, [], [f3534, f486, f418])).
fof(f3534, plain, (~ (e1 = op(e2, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f142, f488])).
fof(f488, plain, ((e1 = op(e1, e1)) | ~ spl15_42), inference(avatar_component_clause, [], [f486])).
fof(f3533, plain, (~ spl15_33 | ~ spl15_45), inference(avatar_split_clause, [], [f3527, f499, f448])).
fof(f448, plain, (spl15_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_33])])).
fof(f3527, plain, (~ (e0 = op(e1, e3)) | ~ spl15_45), inference(backward_demodulation, [], [f165, f501])).
fof(f165, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3532, plain, (~ spl15_13 | ~ spl15_45), inference(avatar_split_clause, [], [f3525, f499, f363])).
fof(f363, plain, (spl15_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f3525, plain, (~ (e0 = op(e3, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f137, f501])).
fof(f137, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3523, plain, (~ spl15_46 | ~ spl15_50 | spl15_80), inference(avatar_split_clause, [], [f3521, f772, f520, f503])).
fof(f503, plain, (spl15_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_46])])).
fof(f772, plain, (spl15_80 <=> (op(e0, e3) = op(op(e0, e3), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_80])])).
fof(f3521, plain, (~ (e1 = op(e1, e0)) | (~ spl15_50 | spl15_80)), inference(backward_demodulation, [], [f773, f522])).
fof(f773, plain, (~ (op(e0, e3) = op(op(e0, e3), e0)) | spl15_80), inference(avatar_component_clause, [], [f772])).
fof(f3522, plain, (~ spl15_2 | ~ spl15_50), inference(avatar_split_clause, [], [f3518, f520, f316])).
fof(f3518, plain, (~ (e1 = op(e3, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f153, f522])).
fof(f3515, plain, (~ spl15_21 | ~ spl15_53), inference(avatar_split_clause, [], [f3506, f533, f397])).
fof(f397, plain, (spl15_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f3506, plain, (~ (e0 = op(e2, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f146, f535])).
fof(f535, plain, ((e0 = op(e0, e2)) | ~ spl15_53), inference(avatar_component_clause, [], [f533])).
fof(f3505, plain, (spl15_16 | ~ spl15_60 | ~ spl15_88), inference(avatar_split_clause, [], [f3503, f806, f562, f375])).
fof(f806, plain, (spl15_88 <=> (op(e0, e1) = op(op(e0, e1), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_88])])).
fof(f3503, plain, ((e3 = op(e3, e0)) | (~ spl15_60 | ~ spl15_88)), inference(backward_demodulation, [], [f808, f564])).
fof(f564, plain, ((e3 = op(e0, e1)) | ~ spl15_60), inference(avatar_component_clause, [], [f562])).
fof(f808, plain, ((op(e0, e1) = op(op(e0, e1), e0)) | ~ spl15_88), inference(avatar_component_clause, [], [f806])).
fof(f3504, plain, (~ spl15_44 | ~ spl15_60), inference(avatar_split_clause, [], [f3498, f562, f494])).
fof(f3498, plain, (~ (e3 = op(e1, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f139, f564])).
fof(f3494, plain, (~ spl15_31 | ~ spl15_63), inference(avatar_split_clause, [], [f3482, f575, f439])).
fof(f3482, plain, (~ (e2 = op(e2, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f134, f577])).
fof(f134, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3479, plain, (~ spl15_9 | spl15_96 | ~ spl15_100), inference(avatar_split_clause, [], [f3477, f858, f840, f346])).
fof(f840, plain, (spl15_96 <=> (e0 = op(e3, op(e3, op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl15_96])])).
fof(f858, plain, (spl15_100 <=> (e1 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_100])])).
fof(f3477, plain, (~ (e0 = op(e3, e1)) | (spl15_96 | ~ spl15_100)), inference(backward_demodulation, [], [f842, f859])).
fof(f859, plain, ((e1 = op(e3, op(e3, e3))) | ~ spl15_100), inference(avatar_component_clause, [], [f858])).
fof(f842, plain, (~ (e0 = op(e3, op(e3, op(e3, e3)))) | spl15_96), inference(avatar_component_clause, [], [f840])).
fof(f3461, plain, (~ spl15_11 | spl15_19 | ~ spl15_91), inference(avatar_contradiction_clause, [], [f3460])).
fof(f3460, plain, ($false | (~ spl15_11 | spl15_19 | ~ spl15_91)), inference(subsumption_resolution, [], [f3458, f389])).
fof(f389, plain, (~ (e2 = op(e2, e3)) | spl15_19), inference(avatar_component_clause, [], [f388])).
fof(f388, plain, (spl15_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_19])])).
fof(f3458, plain, ((e2 = op(e2, e3)) | (~ spl15_11 | ~ spl15_91)), inference(backward_demodulation, [], [f820, f356])).
fof(f3454, plain, (spl15_8 | ~ spl15_20 | ~ spl15_82), inference(avatar_split_clause, [], [f3453, f780, f392, f341])).
fof(f3453, plain, ((e3 = op(e3, e2)) | (~ spl15_20 | ~ spl15_82)), inference(backward_demodulation, [], [f782, f394])).
fof(f394, plain, ((e3 = op(e2, e3)) | ~ spl15_20), inference(avatar_component_clause, [], [f392])).
fof(f3443, plain, (~ spl15_17 | ~ spl15_21), inference(avatar_split_clause, [], [f3432, f397, f380])).
fof(f3432, plain, (~ (e0 = op(e2, e3)) | ~ spl15_21), inference(backward_demodulation, [], [f174, f399])).
fof(f399, plain, ((e0 = op(e2, e2)) | ~ spl15_21), inference(avatar_component_clause, [], [f397])).
fof(f3430, plain, (spl15_23 | ~ spl15_31 | ~ spl15_94), inference(avatar_split_clause, [], [f3428, f831, f439, f405])).
fof(f3428, plain, ((e2 = op(e2, e2)) | (~ spl15_31 | ~ spl15_94)), inference(backward_demodulation, [], [f833, f441])).
fof(f3424, plain, (~ spl15_33 | ~ spl15_57 | spl15_81), inference(avatar_contradiction_clause, [], [f3423])).
fof(f3423, plain, ($false | (~ spl15_33 | ~ spl15_57 | spl15_81)), inference(subsumption_resolution, [], [f3421, f552])).
fof(f3421, plain, (~ (e0 = op(e0, e1)) | (~ spl15_33 | spl15_81)), inference(backward_demodulation, [], [f777, f450])).
fof(f450, plain, ((e0 = op(e1, e3)) | ~ spl15_33), inference(avatar_component_clause, [], [f448])).
fof(f777, plain, (~ (op(e1, e3) = op(op(e1, e3), e1)) | spl15_81), inference(avatar_component_clause, [], [f776])).
fof(f776, plain, (spl15_81 <=> (op(e1, e3) = op(op(e1, e3), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_81])])).
fof(f3422, plain, (~ spl15_17 | ~ spl15_33), inference(avatar_split_clause, [], [f3416, f448, f380])).
fof(f3416, plain, (~ (e0 = op(e2, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f154, f450])).
fof(f3414, plain, (~ spl15_23 | ~ spl15_39), inference(avatar_split_clause, [], [f3409, f473, f405])).
fof(f3409, plain, (~ (e2 = op(e2, e2)) | ~ spl15_39), inference(backward_demodulation, [], [f148, f475])).
fof(f475, plain, ((e2 = op(e1, e2)) | ~ spl15_39), inference(avatar_component_clause, [], [f473])).
fof(f148, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3408, plain, (spl15_33 | ~ spl15_44 | ~ spl15_115), inference(avatar_split_clause, [], [f3404, f932, f494, f448])).
fof(f932, plain, (spl15_115 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_115])])).
fof(f3404, plain, ((e0 = op(e1, e3)) | (~ spl15_44 | ~ spl15_115)), inference(backward_demodulation, [], [f933, f496])).
fof(f933, plain, ((e0 = op(e1, op(e1, e1))) | ~ spl15_115), inference(avatar_component_clause, [], [f932])).
fof(f3406, plain, (~ spl15_40 | ~ spl15_44), inference(avatar_split_clause, [], [f3401, f494, f477])).
fof(f477, plain, (spl15_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_40])])).
fof(f3401, plain, (~ (e3 = op(e1, e2)) | ~ spl15_44), inference(backward_demodulation, [], [f166, f496])).
fof(f3405, plain, (~ spl15_12 | ~ spl15_44), inference(avatar_split_clause, [], [f3400, f494, f358])).
fof(f3400, plain, (~ (e3 = op(e3, e1)) | ~ spl15_44), inference(backward_demodulation, [], [f143, f496])).
fof(f143, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3399, plain, (~ spl15_34 | ~ spl15_46), inference(avatar_split_clause, [], [f3395, f503, f452])).
fof(f3395, plain, (~ (e1 = op(e1, e3)) | ~ spl15_46), inference(backward_demodulation, [], [f165, f505])).
fof(f505, plain, ((e1 = op(e1, e0)) | ~ spl15_46), inference(avatar_component_clause, [], [f503])).
fof(f3390, plain, (~ spl15_51 | ~ spl15_54 | ~ spl15_64 | spl15_108), inference(avatar_contradiction_clause, [], [f3389])).
fof(f3389, plain, ($false | (~ spl15_51 | ~ spl15_54 | ~ spl15_64 | spl15_108)), inference(subsumption_resolution, [], [f3386, f539])).
fof(f539, plain, ((e1 = op(e0, e2)) | ~ spl15_54), inference(avatar_component_clause, [], [f537])).
fof(f537, plain, (spl15_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_54])])).
fof(f3386, plain, (~ (e1 = op(e0, e2)) | (~ spl15_51 | ~ spl15_64 | spl15_108)), inference(backward_demodulation, [], [f3353, f526])).
fof(f3353, plain, (~ (e1 = op(e0, op(e0, e3))) | (~ spl15_64 | spl15_108)), inference(backward_demodulation, [], [f899, f581])).
fof(f899, plain, (~ (e1 = op(e0, op(e0, op(e0, e0)))) | spl15_108), inference(avatar_component_clause, [], [f897])).
fof(f897, plain, (spl15_108 <=> (e1 = op(e0, op(e0, op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl15_108])])).
fof(f3381, plain, (spl15_46 | ~ spl15_54 | ~ spl15_84), inference(avatar_split_clause, [], [f3379, f789, f537, f503])).
fof(f789, plain, (spl15_84 <=> (op(e0, e2) = op(op(e0, e2), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_84])])).
fof(f3379, plain, ((e1 = op(e1, e0)) | (~ spl15_54 | ~ spl15_84)), inference(backward_demodulation, [], [f791, f539])).
fof(f791, plain, ((op(e0, e2) = op(op(e0, e2), e0)) | ~ spl15_84), inference(avatar_component_clause, [], [f789])).
fof(f3380, plain, (~ spl15_6 | ~ spl15_54), inference(avatar_split_clause, [], [f3375, f537, f333])).
fof(f3375, plain, (~ (e1 = op(e3, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f147, f539])).
fof(f147, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3371, plain, (~ spl15_41 | ~ spl15_57), inference(avatar_split_clause, [], [f3363, f550, f482])).
fof(f482, plain, (spl15_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_41])])).
fof(f3363, plain, (~ (e0 = op(e1, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f139, f552])).
fof(f3362, plain, (~ spl15_51 | ~ spl15_64 | spl15_111), inference(avatar_split_clause, [], [f3355, f911, f579, f524])).
fof(f911, plain, (spl15_111 <=> (e2 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_111])])).
fof(f3355, plain, (~ (e2 = op(e0, e3)) | (~ spl15_64 | spl15_111)), inference(backward_demodulation, [], [f913, f581])).
fof(f913, plain, (~ (e2 = op(e0, op(e0, e0))) | spl15_111), inference(avatar_component_clause, [], [f911])).
fof(f3359, plain, (~ spl15_52 | ~ spl15_64), inference(avatar_split_clause, [], [f3351, f579, f528])).
fof(f528, plain, (spl15_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_52])])).
fof(f3351, plain, (~ (e3 = op(e0, e3)) | ~ spl15_64), inference(backward_demodulation, [], [f159, f581])).
fof(f3358, plain, (~ spl15_32 | ~ spl15_64), inference(avatar_split_clause, [], [f3348, f579, f443])).
fof(f443, plain, (spl15_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_32])])).
fof(f3348, plain, (~ (e3 = op(e2, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f134, f581])).
fof(f3345, plain, (~ spl15_6 | ~ spl15_97 | spl15_105), inference(avatar_split_clause, [], [f3343, f882, f844, f333])).
fof(f844, plain, (spl15_97 <=> (e2 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_97])])).
fof(f882, plain, (spl15_105 <=> (e1 = op(e3, op(e3, op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl15_105])])).
fof(f3343, plain, (~ (e1 = op(e3, e2)) | (~ spl15_97 | spl15_105)), inference(backward_demodulation, [], [f884, f845])).
fof(f845, plain, ((e2 = op(e3, op(e3, e3))) | ~ spl15_97), inference(avatar_component_clause, [], [f844])).
fof(f884, plain, (~ (e1 = op(e3, op(e3, op(e3, e3)))) | spl15_105), inference(avatar_component_clause, [], [f882])).
fof(f3341, plain, (~ spl15_47 | spl15_113 | ~ spl15_115), inference(avatar_split_clause, [], [f3339, f932, f921, f507])).
fof(f507, plain, (spl15_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_47])])).
fof(f921, plain, (spl15_113 <=> (e2 = op(e1, op(e1, op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl15_113])])).
fof(f3339, plain, (~ (e2 = op(e1, e0)) | (spl15_113 | ~ spl15_115)), inference(backward_demodulation, [], [f923, f933])).
fof(f923, plain, (~ (e2 = op(e1, op(e1, op(e1, e1)))) | spl15_113), inference(avatar_component_clause, [], [f921])).
fof(f3324, plain, (~ spl15_1 | ~ spl15_13), inference(avatar_split_clause, [], [f3319, f363, f312])).
fof(f312, plain, (spl15_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f3319, plain, (~ (e0 = op(e3, e3)) | ~ spl15_13), inference(backward_demodulation, [], [f177, f365])).
fof(f365, plain, ((e0 = op(e3, e0)) | ~ spl15_13), inference(avatar_component_clause, [], [f363])).
fof(f3317, plain, (~ spl15_1 | ~ spl15_17), inference(avatar_split_clause, [], [f3312, f380, f312])).
fof(f3312, plain, (~ (e0 = op(e3, e3)) | ~ spl15_17), inference(backward_demodulation, [], [f156, f382])).
fof(f156, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3306, plain, (~ spl15_23 | spl15_86), inference(avatar_contradiction_clause, [], [f3305])).
fof(f3305, plain, ($false | (~ spl15_23 | spl15_86)), inference(subsumption_resolution, [], [f3296, f407])).
fof(f407, plain, ((e2 = op(e2, e2)) | ~ spl15_23), inference(avatar_component_clause, [], [f405])).
fof(f3296, plain, (~ (e2 = op(e2, e2)) | (~ spl15_23 | spl15_86)), inference(backward_demodulation, [], [f798, f407])).
fof(f798, plain, (~ (op(e2, e2) = op(op(e2, e2), e2)) | spl15_86), inference(avatar_component_clause, [], [f797])).
fof(f3304, plain, (~ spl15_19 | ~ spl15_23), inference(avatar_split_clause, [], [f3294, f405, f388])).
fof(f3294, plain, (~ (e2 = op(e2, e3)) | ~ spl15_23), inference(backward_demodulation, [], [f174, f407])).
fof(f3303, plain, (~ spl15_7 | ~ spl15_23), inference(avatar_split_clause, [], [f3293, f405, f337])).
fof(f337, plain, (spl15_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f3293, plain, (~ (e2 = op(e3, e2)) | ~ spl15_23), inference(backward_demodulation, [], [f150, f407])).
fof(f3292, plain, (~ spl15_24 | ~ spl15_32), inference(avatar_split_clause, [], [f3289, f443, f409])).
fof(f3289, plain, (~ (e3 = op(e2, e2)) | ~ spl15_32), inference(backward_demodulation, [], [f170, f445])).
fof(f445, plain, ((e3 = op(e2, e0)) | ~ spl15_32), inference(avatar_component_clause, [], [f443])).
fof(f3287, plain, (~ spl15_24 | ~ spl15_40), inference(avatar_split_clause, [], [f3284, f477, f409])).
fof(f3284, plain, (~ (e3 = op(e2, e2)) | ~ spl15_40), inference(backward_demodulation, [], [f148, f479])).
fof(f479, plain, ((e3 = op(e1, e2)) | ~ spl15_40), inference(avatar_component_clause, [], [f477])).
fof(f3280, plain, (~ spl15_41 | ~ spl15_47 | spl15_104), inference(avatar_contradiction_clause, [], [f3279])).
fof(f3279, plain, ($false | (~ spl15_41 | ~ spl15_47 | spl15_104)), inference(subsumption_resolution, [], [f3271, f509])).
fof(f509, plain, ((e2 = op(e1, e0)) | ~ spl15_47), inference(avatar_component_clause, [], [f507])).
fof(f3271, plain, (~ (e2 = op(e1, e0)) | (~ spl15_41 | spl15_104)), inference(backward_demodulation, [], [f879, f484])).
fof(f484, plain, ((e0 = op(e1, e1)) | ~ spl15_41), inference(avatar_component_clause, [], [f482])).
fof(f879, plain, (~ (e2 = op(e1, op(e1, e1))) | spl15_104), inference(avatar_component_clause, [], [f877])).
fof(f877, plain, (spl15_104 <=> (e2 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_104])])).
fof(f3275, plain, (~ spl15_37 | ~ spl15_41), inference(avatar_split_clause, [], [f3264, f482, f465])).
fof(f465, plain, (spl15_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_37])])).
fof(f3264, plain, (~ (e0 = op(e1, e2)) | ~ spl15_41), inference(backward_demodulation, [], [f166, f484])).
fof(f3263, plain, (~ spl15_43 | ~ spl15_47), inference(avatar_split_clause, [], [f3260, f507, f490])).
fof(f3260, plain, (~ (e2 = op(e1, e1)) | ~ spl15_47), inference(backward_demodulation, [], [f163, f509])).
fof(f163, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3257, plain, (~ spl15_37 | ~ spl15_53), inference(avatar_split_clause, [], [f3250, f533, f465])).
fof(f3250, plain, (~ (e0 = op(e1, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f145, f535])).
fof(f145, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3249, plain, (~ spl15_43 | ~ spl15_59), inference(avatar_split_clause, [], [f3245, f558, f490])).
fof(f558, plain, (spl15_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_59])])).
fof(f3245, plain, (~ (e2 = op(e1, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f139, f560])).
fof(f560, plain, ((e2 = op(e0, e1)) | ~ spl15_59), inference(avatar_component_clause, [], [f558])).
fof(f3243, plain, (spl15_46 | ~ spl15_62 | ~ spl15_92), inference(avatar_contradiction_clause, [], [f3242])).
fof(f3242, plain, ($false | (spl15_46 | ~ spl15_62 | ~ spl15_92)), inference(subsumption_resolution, [], [f3237, f504])).
fof(f504, plain, (~ (e1 = op(e1, e0)) | spl15_46), inference(avatar_component_clause, [], [f503])).
fof(f3237, plain, ((e1 = op(e1, e0)) | (~ spl15_62 | ~ spl15_92)), inference(backward_demodulation, [], [f825, f573])).
fof(f573, plain, ((op(e0, e0) = e1) | ~ spl15_62), inference(avatar_component_clause, [], [f571])).
fof(f571, plain, (spl15_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl15_62])])).
fof(f825, plain, ((op(e0, e0) = op(op(e0, e0), e0)) | ~ spl15_92), inference(avatar_component_clause, [], [f823])).
fof(f823, plain, (spl15_92 <=> (op(e0, e0) = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_92])])).
fof(f3241, plain, (~ spl15_54 | ~ spl15_62), inference(avatar_split_clause, [], [f3234, f571, f537])).
fof(f3234, plain, (~ (e1 = op(e0, e2)) | ~ spl15_62), inference(backward_demodulation, [], [f158, f573])).
fof(f3240, plain, (~ spl15_14 | ~ spl15_62), inference(avatar_split_clause, [], [f3232, f571, f367])).
fof(f3232, plain, (~ (e1 = op(e3, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f135, f573])).
fof(f3214, plain, (spl15_38 | ~ spl15_26 | ~ spl15_90), inference(avatar_split_clause, [], [f3168, f814, f418, f469])).
fof(f814, plain, (spl15_90 <=> (op(e2, e1) = op(op(e2, e1), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_90])])).
fof(f3168, plain, ((e1 = op(e1, e2)) | (~ spl15_26 | ~ spl15_90)), inference(backward_demodulation, [], [f816, f420])).
fof(f420, plain, ((e1 = op(e2, e1)) | ~ spl15_26), inference(avatar_component_clause, [], [f418])).
fof(f816, plain, ((op(e2, e1) = op(op(e2, e1), e2)) | ~ spl15_90), inference(avatar_component_clause, [], [f814])).
fof(f3210, plain, (~ spl15_4 | ~ spl15_12), inference(avatar_split_clause, [], [f3209, f358, f324])).
fof(f3209, plain, (~ (e3 = op(e3, e3)) | ~ spl15_12), inference(forward_demodulation, [], [f179, f360])).
fof(f360, plain, ((e3 = op(e3, e1)) | ~ spl15_12), inference(avatar_component_clause, [], [f358])).
fof(f3208, plain, (~ spl15_4 | ~ spl15_52), inference(avatar_split_clause, [], [f3207, f528, f324])).
fof(f3207, plain, (~ (e3 = op(e3, e3)) | ~ spl15_52), inference(forward_demodulation, [], [f153, f530])).
fof(f530, plain, ((e3 = op(e0, e3)) | ~ spl15_52), inference(avatar_component_clause, [], [f528])).
fof(f3199, plain, (~ spl15_6 | ~ spl15_14), inference(avatar_split_clause, [], [f3194, f367, f333])).
fof(f3194, plain, (~ (e1 = op(e3, e2)) | ~ spl15_14), inference(backward_demodulation, [], [f176, f369])).
fof(f3169, plain, (~ spl15_22 | ~ spl15_26), inference(avatar_split_clause, [], [f3164, f418, f401])).
fof(f401, plain, (spl15_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_22])])).
fof(f3164, plain, (~ (e1 = op(e2, e2)) | ~ spl15_26), inference(backward_demodulation, [], [f172, f420])).
fof(f3162, plain, (~ spl15_25 | ~ spl15_29), inference(avatar_split_clause, [], [f3155, f431, f414])).
fof(f431, plain, (spl15_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_29])])).
fof(f3155, plain, (~ (e0 = op(e2, e1)) | ~ spl15_29), inference(backward_demodulation, [], [f169, f433])).
fof(f433, plain, ((e0 = op(e2, e0)) | ~ spl15_29), inference(avatar_component_clause, [], [f431])).
fof(f3142, plain, (~ spl15_37 | ~ spl15_43 | spl15_115), inference(avatar_split_clause, [], [f3136, f932, f490, f465])).
fof(f3136, plain, (~ (e0 = op(e1, e2)) | (~ spl15_43 | spl15_115)), inference(backward_demodulation, [], [f934, f492])).
fof(f934, plain, (~ (e0 = op(e1, op(e1, e1))) | spl15_115), inference(avatar_component_clause, [], [f932])).
fof(f3140, plain, (~ spl15_40 | ~ spl15_43 | spl15_102), inference(avatar_split_clause, [], [f3133, f867, f490, f477])).
fof(f867, plain, (spl15_102 <=> (e3 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_102])])).
fof(f3133, plain, (~ (e3 = op(e1, e2)) | (~ spl15_43 | spl15_102)), inference(backward_demodulation, [], [f869, f492])).
fof(f869, plain, (~ (e3 = op(e1, op(e1, e1))) | spl15_102), inference(avatar_component_clause, [], [f867])).
fof(f3138, plain, (~ spl15_11 | ~ spl15_43), inference(avatar_split_clause, [], [f3128, f490, f354])).
fof(f3128, plain, (~ (e2 = op(e3, e1)) | ~ spl15_43), inference(backward_demodulation, [], [f143, f492])).
fof(f3126, plain, (~ spl15_40 | ~ spl15_48), inference(avatar_split_clause, [], [f3124, f511, f477])).
fof(f511, plain, (spl15_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_48])])).
fof(f3124, plain, (~ (e3 = op(e1, e2)) | ~ spl15_48), inference(backward_demodulation, [], [f164, f513])).
fof(f513, plain, ((e3 = op(e1, e0)) | ~ spl15_48), inference(avatar_component_clause, [], [f511])).
fof(f164, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3125, plain, (~ spl15_16 | ~ spl15_48), inference(avatar_split_clause, [], [f3122, f511, f375])).
fof(f3122, plain, (~ (e3 = op(e3, e0)) | ~ spl15_48), inference(backward_demodulation, [], [f137, f513])).
fof(f3120, plain, (~ spl15_16 | ~ spl15_52 | spl15_80), inference(avatar_split_clause, [], [f3118, f772, f528, f375])).
fof(f3118, plain, (~ (e3 = op(e3, e0)) | (~ spl15_52 | spl15_80)), inference(backward_demodulation, [], [f773, f530])).
fof(f3119, plain, (~ spl15_20 | ~ spl15_52), inference(avatar_split_clause, [], [f3117, f528, f392])).
fof(f3117, plain, (~ (e3 = op(e2, e3)) | ~ spl15_52), inference(backward_demodulation, [], [f152, f530])).
fof(f3115, plain, (~ spl15_22 | ~ spl15_54), inference(avatar_split_clause, [], [f3109, f537, f401])).
fof(f3109, plain, (~ (e1 = op(e2, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f146, f539])).
fof(f3106, plain, (~ spl15_25 | ~ spl15_57), inference(avatar_split_clause, [], [f3098, f550, f414])).
fof(f3098, plain, (~ (e0 = op(e2, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f140, f552])).
fof(f140, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3046, plain, (~ spl15_12 | ~ spl15_16), inference(avatar_split_clause, [], [f3042, f375, f358])).
fof(f3042, plain, (~ (e3 = op(e3, e1)) | ~ spl15_16), inference(backward_demodulation, [], [f175, f377])).
fof(f3034, plain, (~ spl15_42 | spl15_89), inference(avatar_contradiction_clause, [], [f3033])).
fof(f3033, plain, ($false | (~ spl15_42 | spl15_89)), inference(subsumption_resolution, [], [f3029, f488])).
fof(f3029, plain, (~ (e1 = op(e1, e1)) | (~ spl15_42 | spl15_89)), inference(backward_demodulation, [], [f811, f488])).
fof(f811, plain, (~ (op(e1, e1) = op(op(e1, e1), e1)) | spl15_89), inference(avatar_component_clause, [], [f810])).
fof(f3021, plain, (~ spl15_49 | ~ spl15_53), inference(avatar_split_clause, [], [f3015, f533, f516])).
fof(f3015, plain, (~ (e0 = op(e0, e3)) | ~ spl15_53), inference(backward_demodulation, [], [f162, f535])).
fof(f162, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3020, plain, (~ spl15_5 | ~ spl15_53), inference(avatar_split_clause, [], [f3014, f533, f329])).
fof(f329, plain, (spl15_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f3014, plain, (~ (e0 = op(e3, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f147, f535])).
fof(f3012, plain, (~ spl15_12 | ~ spl15_60), inference(avatar_split_clause, [], [f3008, f562, f358])).
fof(f3008, plain, (~ (e3 = op(e3, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f141, f564])).
fof(f3003, plain, (~ spl15_58 | ~ spl15_62), inference(avatar_split_clause, [], [f2993, f571, f554])).
fof(f2993, plain, (~ (e1 = op(e0, e1)) | ~ spl15_62), inference(backward_demodulation, [], [f157, f573])).
fof(f2984, plain, (~ spl15_51 | ~ spl15_109 | spl15_114), inference(avatar_split_clause, [], [f2982, f927, f901, f524])).
fof(f901, plain, (spl15_109 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_109])])).
fof(f927, plain, (spl15_114 <=> (e2 = op(e0, op(e0, op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl15_114])])).
fof(f2982, plain, (~ (e2 = op(e0, e3)) | (~ spl15_109 | spl15_114)), inference(backward_demodulation, [], [f929, f902])).
fof(f902, plain, ((e3 = op(e0, op(e0, e0))) | ~ spl15_109), inference(avatar_component_clause, [], [f901])).
fof(f929, plain, (~ (e2 = op(e0, op(e0, op(e0, e0)))) | spl15_114), inference(avatar_component_clause, [], [f927])).
fof(f2974, plain, (~ spl15_42 | ~ spl15_34), inference(avatar_split_clause, [], [f2973, f452, f486])).
fof(f2973, plain, (~ (e1 = op(e1, e1)) | ~ spl15_34), inference(forward_demodulation, [], [f167, f454])).
fof(f454, plain, ((e1 = op(e1, e3)) | ~ spl15_34), inference(avatar_component_clause, [], [f452])).
fof(f2972, plain, (~ spl15_6 | ~ spl15_22), inference(avatar_split_clause, [], [f2351, f401, f333])).
fof(f2351, plain, (~ (e1 = op(e3, e2)) | ~ spl15_22), inference(backward_demodulation, [], [f150, f403])).
fof(f403, plain, ((e1 = op(e2, e2)) | ~ spl15_22), inference(avatar_component_clause, [], [f401])).
fof(f2971, plain, (~ spl15_8 | ~ spl15_40), inference(avatar_split_clause, [], [f2970, f477, f341])).
fof(f2970, plain, (~ (e3 = op(e3, e2)) | ~ spl15_40), inference(forward_demodulation, [], [f149, f479])).
fof(f2969, plain, (~ spl15_8 | ~ spl15_20 | spl15_82), inference(avatar_split_clause, [], [f2954, f780, f392, f341])).
fof(f2954, plain, (~ (e3 = op(e3, e2)) | (~ spl15_20 | spl15_82)), inference(backward_demodulation, [], [f781, f394])).
fof(f781, plain, (~ (op(e2, e3) = op(op(e2, e3), e2)) | spl15_82), inference(avatar_component_clause, [], [f780])).
fof(f2965, plain, (spl15_4 | ~ spl15_12 | ~ spl15_91), inference(avatar_contradiction_clause, [], [f2964])).
fof(f2964, plain, ($false | (spl15_4 | ~ spl15_12 | ~ spl15_91)), inference(subsumption_resolution, [], [f2963, f325])).
fof(f325, plain, (~ (e3 = op(e3, e3)) | spl15_4), inference(avatar_component_clause, [], [f324])).
fof(f2963, plain, ((e3 = op(e3, e3)) | (~ spl15_12 | ~ spl15_91)), inference(backward_demodulation, [], [f820, f360])).
fof(f2961, plain, (~ spl15_2 | ~ spl15_14), inference(avatar_split_clause, [], [f2957, f367, f316])).
fof(f2957, plain, (~ (e1 = op(e3, e3)) | ~ spl15_14), inference(backward_demodulation, [], [f177, f369])).
fof(f2941, plain, (~ spl15_34 | spl15_42 | ~ spl15_81), inference(avatar_contradiction_clause, [], [f2940])).
fof(f2940, plain, ($false | (~ spl15_34 | spl15_42 | ~ spl15_81)), inference(subsumption_resolution, [], [f2938, f487])).
fof(f487, plain, (~ (e1 = op(e1, e1)) | spl15_42), inference(avatar_component_clause, [], [f486])).
fof(f2938, plain, ((e1 = op(e1, e1)) | (~ spl15_34 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f454])).
fof(f778, plain, ((op(e1, e3) = op(op(e1, e3), e1)) | ~ spl15_81), inference(avatar_component_clause, [], [f776])).
fof(f2939, plain, (~ spl15_2 | ~ spl15_34), inference(avatar_split_clause, [], [f2935, f452, f316])).
fof(f2935, plain, (~ (e1 = op(e3, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f155, f454])).
fof(f2933, plain, (~ spl15_36 | ~ spl15_40), inference(avatar_split_clause, [], [f2931, f477, f460])).
fof(f2931, plain, (~ (e3 = op(e1, e3)) | ~ spl15_40), inference(backward_demodulation, [], [f168, f479])).
fof(f2932, plain, (spl15_12 | ~ spl15_40 | ~ spl15_85), inference(avatar_split_clause, [], [f2930, f793, f477, f358])).
fof(f793, plain, (spl15_85 <=> (op(e1, e2) = op(op(e1, e2), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_85])])).
fof(f2930, plain, ((e3 = op(e3, e1)) | (~ spl15_40 | ~ spl15_85)), inference(backward_demodulation, [], [f795, f479])).
fof(f795, plain, ((op(e1, e2) = op(op(e1, e2), e1)) | ~ spl15_85), inference(avatar_component_clause, [], [f793])).
fof(f2904, plain, (~ spl15_51 | ~ spl15_55), inference(avatar_split_clause, [], [f2900, f541, f524])).
fof(f2900, plain, (~ (e2 = op(e0, e3)) | ~ spl15_55), inference(backward_demodulation, [], [f162, f543])).
fof(f2903, plain, (~ spl15_39 | ~ spl15_55), inference(avatar_split_clause, [], [f2899, f541, f473])).
fof(f2899, plain, (~ (e2 = op(e1, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f145, f543])).
fof(f2873, plain, (~ spl15_28 | ~ spl15_22 | spl15_99), inference(avatar_split_clause, [], [f2872, f853, f401, f426])).
fof(f853, plain, (spl15_99 <=> (e3 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_99])])).
fof(f2872, plain, (~ (e3 = op(e2, e1)) | (~ spl15_22 | spl15_99)), inference(forward_demodulation, [], [f855, f403])).
fof(f855, plain, (~ (e3 = op(e2, op(e2, e2))) | spl15_99), inference(avatar_component_clause, [], [f853])).
fof(f2867, plain, (~ spl15_14 | spl15_105 | ~ spl15_107), inference(avatar_split_clause, [], [f2866, f892, f882, f367])).
fof(f892, plain, (spl15_107 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_107])])).
fof(f2866, plain, (~ (e1 = op(e3, e0)) | (spl15_105 | ~ spl15_107)), inference(forward_demodulation, [], [f884, f893])).
fof(f893, plain, ((e0 = op(e3, op(e3, e3))) | ~ spl15_107), inference(avatar_component_clause, [], [f892])).
fof(f2837, plain, (spl15_4 | ~ spl15_16 | ~ spl15_95), inference(avatar_contradiction_clause, [], [f2836])).
fof(f2836, plain, ($false | (spl15_4 | ~ spl15_16 | ~ spl15_95)), inference(subsumption_resolution, [], [f2834, f325])).
fof(f2834, plain, ((e3 = op(e3, e3)) | (~ spl15_16 | ~ spl15_95)), inference(backward_demodulation, [], [f837, f377])).
fof(f2825, plain, (~ spl15_17 | ~ spl15_22 | ~ spl15_28 | spl15_98), inference(avatar_split_clause, [], [f2821, f849, f426, f401, f380])).
fof(f2821, plain, (~ (e0 = op(e2, e3)) | (~ spl15_22 | ~ spl15_28 | spl15_98)), inference(backward_demodulation, [], [f2740, f428])).
fof(f2740, plain, (~ (e0 = op(e2, op(e2, e1))) | (~ spl15_22 | spl15_98)), inference(forward_demodulation, [], [f851, f403])).
fof(f2822, plain, (~ spl15_20 | ~ spl15_28), inference(avatar_split_clause, [], [f2817, f426, f392])).
fof(f2817, plain, (~ (e3 = op(e2, e3)) | ~ spl15_28), inference(backward_demodulation, [], [f173, f428])).
fof(f2811, plain, (spl15_41 | ~ spl15_42 | ~ spl15_115), inference(avatar_contradiction_clause, [], [f2810])).
fof(f2810, plain, ($false | (spl15_41 | ~ spl15_42 | ~ spl15_115)), inference(subsumption_resolution, [], [f2808, f483])).
fof(f483, plain, (~ (e0 = op(e1, e1)) | spl15_41), inference(avatar_component_clause, [], [f482])).
fof(f2808, plain, ((e0 = op(e1, e1)) | (~ spl15_42 | ~ spl15_115)), inference(backward_demodulation, [], [f933, f488])).
fof(f2809, plain, (~ spl15_10 | ~ spl15_42), inference(avatar_split_clause, [], [f2803, f486, f350])).
fof(f2803, plain, (~ (e1 = op(e3, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f143, f488])).
fof(f2784, plain, (spl15_16 | ~ spl15_56 | ~ spl15_84), inference(avatar_split_clause, [], [f2782, f789, f545, f375])).
fof(f2782, plain, ((e3 = op(e3, e0)) | (~ spl15_56 | ~ spl15_84)), inference(backward_demodulation, [], [f791, f547])).
fof(f2783, plain, (~ spl15_8 | ~ spl15_56), inference(avatar_split_clause, [], [f2780, f545, f341])).
fof(f2780, plain, (~ (e3 = op(e3, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f147, f547])).
fof(f2763, plain, (~ spl15_59 | ~ spl15_62 | spl15_111), inference(avatar_split_clause, [], [f2758, f911, f571, f558])).
fof(f2758, plain, (~ (e2 = op(e0, e1)) | (~ spl15_62 | spl15_111)), inference(backward_demodulation, [], [f913, f573])).
fof(f2761, plain, (~ spl15_50 | ~ spl15_62), inference(avatar_split_clause, [], [f2753, f571, f520])).
fof(f2753, plain, (~ (e1 = op(e0, e3)) | ~ spl15_62), inference(backward_demodulation, [], [f159, f573])).
fof(f2760, plain, (~ spl15_46 | ~ spl15_62), inference(avatar_split_clause, [], [f2749, f571, f503])).
fof(f2749, plain, (~ (e1 = op(e1, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f133, f573])).
fof(f133, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2743, plain, (~ spl15_5 | spl15_96 | ~ spl15_97), inference(avatar_split_clause, [], [f2741, f844, f840, f329])).
fof(f2741, plain, (~ (e0 = op(e3, e2)) | (spl15_96 | ~ spl15_97)), inference(backward_demodulation, [], [f842, f845])).
fof(f2731, plain, (~ spl15_47 | ~ spl15_31), inference(avatar_split_clause, [], [f2730, f439, f507])).
fof(f2730, plain, (~ (e2 = op(e1, e0)) | ~ spl15_31), inference(forward_demodulation, [], [f136, f441])).
fof(f136, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2728, plain, (~ spl15_45 | spl15_101 | ~ spl15_115), inference(avatar_split_clause, [], [f2727, f932, f863, f499])).
fof(f863, plain, (spl15_101 <=> (e0 = op(e1, op(e1, op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl15_101])])).
fof(f2727, plain, (~ (e0 = op(e1, e0)) | (spl15_101 | ~ spl15_115)), inference(forward_demodulation, [], [f865, f933])).
fof(f865, plain, (~ (e0 = op(e1, op(e1, op(e1, e1)))) | spl15_101), inference(avatar_component_clause, [], [f863])).
fof(f2724, plain, (~ spl15_48 | ~ spl15_115 | spl15_118), inference(avatar_split_clause, [], [f2723, f947, f932, f511])).
fof(f947, plain, (spl15_118 <=> (e3 = op(e1, op(e1, op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl15_118])])).
fof(f2723, plain, (~ (e3 = op(e1, e0)) | (~ spl15_115 | spl15_118)), inference(forward_demodulation, [], [f949, f933])).
fof(f949, plain, (~ (e3 = op(e1, op(e1, op(e1, e1)))) | spl15_118), inference(avatar_component_clause, [], [f947])).
fof(f2721, plain, (~ spl15_15 | ~ spl15_31), inference(avatar_split_clause, [], [f2720, f439, f371])).
fof(f2720, plain, (~ (e2 = op(e3, e0)) | ~ spl15_31), inference(forward_demodulation, [], [f138, f441])).
fof(f138, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2717, plain, (~ spl15_3 | spl15_19 | ~ spl15_83), inference(avatar_contradiction_clause, [], [f2716])).
fof(f2716, plain, ($false | (~ spl15_3 | spl15_19 | ~ spl15_83)), inference(subsumption_resolution, [], [f2710, f389])).
fof(f2710, plain, ((e2 = op(e2, e3)) | (~ spl15_3 | ~ spl15_83)), inference(backward_demodulation, [], [f786, f322])).
fof(f322, plain, ((e2 = op(e3, e3)) | ~ spl15_3), inference(avatar_component_clause, [], [f320])).
fof(f786, plain, ((op(e3, e3) = op(op(e3, e3), e3)) | ~ spl15_83), inference(avatar_component_clause, [], [f784])).
fof(f784, plain, (spl15_83 <=> (op(e3, e3) = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_83])])).
fof(f2708, plain, (~ spl15_4 | ~ spl15_8), inference(avatar_split_clause, [], [f2706, f341, f324])).
fof(f2706, plain, (~ (e3 = op(e3, e3)) | ~ spl15_8), inference(backward_demodulation, [], [f180, f343])).
fof(f343, plain, ((e3 = op(e3, e2)) | ~ spl15_8), inference(avatar_component_clause, [], [f341])).
fof(f2703, plain, (~ spl15_4 | ~ spl15_20), inference(avatar_split_clause, [], [f2701, f392, f324])).
fof(f2701, plain, (~ (e3 = op(e3, e3)) | ~ spl15_20), inference(backward_demodulation, [], [f156, f394])).
fof(f2690, plain, (~ spl15_27 | ~ spl15_31), inference(avatar_split_clause, [], [f2684, f439, f422])).
fof(f2684, plain, (~ (e2 = op(e2, e1)) | ~ spl15_31), inference(backward_demodulation, [], [f169, f441])).
fof(f2670, plain, (spl15_27 | ~ spl15_39 | ~ spl15_85), inference(avatar_split_clause, [], [f2667, f793, f473, f422])).
fof(f2667, plain, ((e2 = op(e2, e1)) | (~ spl15_39 | ~ spl15_85)), inference(backward_demodulation, [], [f795, f475])).
fof(f2669, plain, (~ spl15_35 | ~ spl15_39), inference(avatar_split_clause, [], [f2665, f473, f456])).
fof(f2665, plain, (~ (e2 = op(e1, e3)) | ~ spl15_39), inference(backward_demodulation, [], [f168, f475])).
fof(f2668, plain, (~ spl15_7 | ~ spl15_39), inference(avatar_split_clause, [], [f2664, f473, f337])).
fof(f2664, plain, (~ (e2 = op(e3, e2)) | ~ spl15_39), inference(backward_demodulation, [], [f149, f475])).
fof(f2655, plain, (~ spl15_55 | ~ spl15_59), inference(avatar_split_clause, [], [f2651, f558, f541])).
fof(f2651, plain, (~ (e2 = op(e0, e2)) | ~ spl15_59), inference(backward_demodulation, [], [f160, f560])).
fof(f2654, plain, (~ spl15_27 | ~ spl15_59), inference(avatar_split_clause, [], [f2650, f558, f422])).
fof(f2650, plain, (~ (e2 = op(e2, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f140, f560])).
fof(f2646, plain, (~ spl15_35 | ~ spl15_44 | spl15_104), inference(avatar_split_clause, [], [f2645, f877, f494, f456])).
fof(f2645, plain, (~ (e2 = op(e1, e3)) | (~ spl15_44 | spl15_104)), inference(forward_demodulation, [], [f879, f496])).
fof(f2627, plain, (~ spl15_30 | ~ spl15_46), inference(avatar_split_clause, [], [f2626, f503, f435])).
fof(f2626, plain, (~ (e1 = op(e2, e0)) | ~ spl15_46), inference(forward_demodulation, [], [f136, f505])).
fof(f2625, plain, (~ spl15_29 | ~ spl15_13), inference(avatar_split_clause, [], [f2624, f363, f431])).
fof(f2624, plain, (~ (e0 = op(e2, e0)) | ~ spl15_13), inference(forward_demodulation, [], [f138, f365])).
fof(f2623, plain, (~ spl15_18 | ~ spl15_50), inference(avatar_split_clause, [], [f2622, f520, f384])).
fof(f384, plain, (spl15_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f2622, plain, (~ (e1 = op(e2, e3)) | ~ spl15_50), inference(forward_demodulation, [], [f152, f522])).
fof(f2621, plain, (~ spl15_18 | ~ spl15_22), inference(avatar_split_clause, [], [f2620, f401, f384])).
fof(f2620, plain, (~ (e1 = op(e2, e3)) | ~ spl15_22), inference(forward_demodulation, [], [f174, f403])).
fof(f2615, plain, (~ spl15_13 | spl15_49 | ~ spl15_95), inference(avatar_contradiction_clause, [], [f2614])).
fof(f2614, plain, ($false | (~ spl15_13 | spl15_49 | ~ spl15_95)), inference(subsumption_resolution, [], [f2612, f517])).
fof(f2612, plain, ((e0 = op(e0, e3)) | (~ spl15_13 | ~ spl15_95)), inference(backward_demodulation, [], [f837, f365])).
fof(f2613, plain, (~ spl15_5 | ~ spl15_13), inference(avatar_split_clause, [], [f2608, f363, f329])).
fof(f2608, plain, (~ (e0 = op(e3, e2)) | ~ spl15_13), inference(backward_demodulation, [], [f176, f365])).
fof(f2606, plain, (~ spl15_22 | ~ spl15_24), inference(avatar_contradiction_clause, [], [f2605])).
fof(f2605, plain, ($false | (~ spl15_22 | ~ spl15_24)), inference(subsumption_resolution, [], [f2604, f185])).
fof(f185, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2604, plain, ((e1 = e3) | (~ spl15_22 | ~ spl15_24)), inference(forward_demodulation, [], [f411, f403])).
fof(f2597, plain, (spl15_27 | ~ spl15_35 | ~ spl15_81), inference(avatar_split_clause, [], [f2596, f776, f456, f422])).
fof(f2596, plain, ((e2 = op(e2, e1)) | (~ spl15_35 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f458])).
fof(f2594, plain, (~ spl15_33 | ~ spl15_37), inference(avatar_split_clause, [], [f2588, f465, f448])).
fof(f2588, plain, (~ (e0 = op(e1, e3)) | ~ spl15_37), inference(backward_demodulation, [], [f168, f467])).
fof(f467, plain, ((e0 = op(e1, e2)) | ~ spl15_37), inference(avatar_component_clause, [], [f465])).
fof(f2593, plain, (~ spl15_5 | ~ spl15_37), inference(avatar_split_clause, [], [f2587, f465, f329])).
fof(f2587, plain, (~ (e0 = op(e3, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f149, f467])).
fof(f2575, plain, (~ spl15_63 | ~ spl15_64), inference(avatar_contradiction_clause, [], [f2574])).
fof(f2574, plain, ($false | (~ spl15_63 | ~ spl15_64)), inference(subsumption_resolution, [], [f2573, f186])).
fof(f186, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2573, plain, ((e2 = e3) | (~ spl15_63 | ~ spl15_64)), inference(backward_demodulation, [], [f581, f577])).
fof(f2571, plain, (~ spl15_37 | spl15_101 | ~ spl15_104), inference(avatar_split_clause, [], [f2570, f877, f863, f465])).
fof(f2570, plain, (~ (e0 = op(e1, e2)) | (spl15_101 | ~ spl15_104)), inference(forward_demodulation, [], [f865, f878])).
fof(f878, plain, ((e2 = op(e1, op(e1, e1))) | ~ spl15_104), inference(avatar_component_clause, [], [f877])).
fof(f2569, plain, (~ spl15_40 | ~ spl15_104 | spl15_118), inference(avatar_split_clause, [], [f2567, f947, f877, f477])).
fof(f2567, plain, (~ (e3 = op(e1, e2)) | (~ spl15_104 | spl15_118)), inference(backward_demodulation, [], [f949, f878])).
fof(f2543, plain, (~ spl15_17 | ~ spl15_29), inference(avatar_contradiction_clause, [], [f2542])).
fof(f2542, plain, ($false | (~ spl15_17 | ~ spl15_29)), inference(subsumption_resolution, [], [f2541, f433])).
fof(f2541, plain, (~ (e0 = op(e2, e0)) | ~ spl15_17), inference(forward_demodulation, [], [f171, f382])).
fof(f171, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2523, plain, (~ spl15_7 | ~ spl15_55), inference(avatar_split_clause, [], [f2522, f541, f337])).
fof(f2522, plain, (~ (e2 = op(e3, e2)) | ~ spl15_55), inference(forward_demodulation, [], [f147, f543])).
fof(f2521, plain, (~ spl15_17 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f2520])).
fof(f2520, plain, ($false | (~ spl15_17 | ~ spl15_19)), inference(subsumption_resolution, [], [f2519, f182])).
fof(f2519, plain, ((e0 = e2) | (~ spl15_17 | ~ spl15_19)), inference(backward_demodulation, [], [f390, f382])).
fof(f390, plain, ((e2 = op(e2, e3)) | ~ spl15_19), inference(avatar_component_clause, [], [f388])).
fof(f2511, plain, (spl15_27 | ~ spl15_43 | ~ spl15_89), inference(avatar_contradiction_clause, [], [f2510])).
fof(f2510, plain, ($false | (spl15_27 | ~ spl15_43 | ~ spl15_89)), inference(subsumption_resolution, [], [f2509, f423])).
fof(f423, plain, (~ (e2 = op(e2, e1)) | spl15_27), inference(avatar_component_clause, [], [f422])).
fof(f2509, plain, ((e2 = op(e2, e1)) | (~ spl15_43 | ~ spl15_89)), inference(forward_demodulation, [], [f812, f492])).
fof(f2505, plain, (~ spl15_29 | spl15_53 | ~ spl15_94), inference(avatar_contradiction_clause, [], [f2504])).
fof(f2504, plain, ($false | (~ spl15_29 | spl15_53 | ~ spl15_94)), inference(subsumption_resolution, [], [f2503, f534])).
fof(f534, plain, (~ (e0 = op(e0, e2)) | spl15_53), inference(avatar_component_clause, [], [f533])).
fof(f2503, plain, ((e0 = op(e0, e2)) | (~ spl15_29 | ~ spl15_94)), inference(forward_demodulation, [], [f833, f433])).
fof(f2469, plain, (~ spl15_4 | spl15_83), inference(avatar_contradiction_clause, [], [f2468])).
fof(f2468, plain, ($false | (~ spl15_4 | spl15_83)), inference(subsumption_resolution, [], [f2464, f326])).
fof(f326, plain, ((e3 = op(e3, e3)) | ~ spl15_4), inference(avatar_component_clause, [], [f324])).
fof(f2464, plain, (~ (e3 = op(e3, e3)) | (~ spl15_4 | spl15_83)), inference(backward_demodulation, [], [f785, f326])).
fof(f785, plain, (~ (op(e3, e3) = op(op(e3, e3), e3)) | spl15_83), inference(avatar_component_clause, [], [f784])).
fof(f2460, plain, (~ spl15_2 | ~ spl15_10), inference(avatar_split_clause, [], [f2456, f350, f316])).
fof(f2456, plain, (~ (e1 = op(e3, e3)) | ~ spl15_10), inference(backward_demodulation, [], [f179, f352])).
fof(f2454, plain, (~ spl15_12 | ~ spl15_28), inference(avatar_split_clause, [], [f2451, f426, f358])).
fof(f2451, plain, (~ (e3 = op(e3, e1)) | ~ spl15_28), inference(backward_demodulation, [], [f144, f428])).
fof(f2445, plain, (~ spl15_33 | ~ spl15_40 | ~ spl15_43 | spl15_101), inference(avatar_contradiction_clause, [], [f2444])).
fof(f2444, plain, ($false | (~ spl15_33 | ~ spl15_40 | ~ spl15_43 | spl15_101)), inference(subsumption_resolution, [], [f2441, f450])).
fof(f2441, plain, (~ (e0 = op(e1, e3)) | (~ spl15_40 | ~ spl15_43 | spl15_101)), inference(backward_demodulation, [], [f2176, f479])).
fof(f2176, plain, (~ (e0 = op(e1, op(e1, e2))) | (~ spl15_43 | spl15_101)), inference(backward_demodulation, [], [f865, f492])).
fof(f2439, plain, (spl15_42 | ~ spl15_46 | ~ spl15_93), inference(avatar_split_clause, [], [f2436, f827, f503, f486])).
fof(f2436, plain, ((e1 = op(e1, e1)) | (~ spl15_46 | ~ spl15_93)), inference(backward_demodulation, [], [f829, f505])).
fof(f2434, plain, (spl15_46 | ~ spl15_50 | ~ spl15_80), inference(avatar_split_clause, [], [f2432, f772, f520, f503])).
fof(f2432, plain, ((e1 = op(e1, e0)) | (~ spl15_50 | ~ spl15_80)), inference(backward_demodulation, [], [f774, f522])).
fof(f774, plain, ((op(e0, e3) = op(op(e0, e3), e0)) | ~ spl15_80), inference(avatar_component_clause, [], [f772])).
fof(f2422, plain, (~ spl15_49 | ~ spl15_57), inference(avatar_split_clause, [], [f2416, f550, f516])).
fof(f2416, plain, (~ (e0 = op(e0, e3)) | ~ spl15_57), inference(backward_demodulation, [], [f161, f552])).
fof(f2409, plain, (~ spl15_48 | ~ spl15_64), inference(avatar_split_clause, [], [f2403, f579, f511])).
fof(f2403, plain, (~ (e3 = op(e1, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f133, f581])).
fof(f2393, plain, (~ spl15_56 | ~ spl15_111 | spl15_119), inference(avatar_split_clause, [], [f2392, f953, f911, f545])).
fof(f953, plain, (spl15_119 <=> (e3 = op(e0, op(e0, op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl15_119])])).
fof(f2392, plain, (~ (e3 = op(e0, e2)) | (~ spl15_111 | spl15_119)), inference(forward_demodulation, [], [f955, f912])).
fof(f912, plain, ((e2 = op(e0, op(e0, e0))) | ~ spl15_111), inference(avatar_component_clause, [], [f911])).
fof(f955, plain, (~ (e3 = op(e0, op(e0, op(e0, e0)))) | spl15_119), inference(avatar_component_clause, [], [f953])).
fof(f2386, plain, (~ spl15_49 | ~ spl15_33), inference(avatar_split_clause, [], [f2385, f448, f516])).
fof(f2385, plain, (~ (e0 = op(e0, e3)) | ~ spl15_33), inference(forward_demodulation, [], [f151, f450])).
fof(f2373, plain, (~ spl15_5 | ~ spl15_49 | spl15_87), inference(avatar_contradiction_clause, [], [f2372])).
fof(f2372, plain, ($false | (~ spl15_5 | ~ spl15_49 | spl15_87)), inference(subsumption_resolution, [], [f2371, f518])).
fof(f2371, plain, (~ (e0 = op(e0, e3)) | (~ spl15_5 | spl15_87)), inference(backward_demodulation, [], [f802, f331])).
fof(f331, plain, ((e0 = op(e3, e2)) | ~ spl15_5), inference(avatar_component_clause, [], [f329])).
fof(f802, plain, (~ (op(e3, e2) = op(op(e3, e2), e3)) | spl15_87), inference(avatar_component_clause, [], [f801])).
fof(f801, plain, (spl15_87 <=> (op(e3, e2) = op(op(e3, e2), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_87])])).
fof(f2364, plain, (~ spl15_8 | ~ spl15_12), inference(avatar_split_clause, [], [f2361, f358, f341])).
fof(f2361, plain, (~ (e3 = op(e3, e2)) | ~ spl15_12), inference(backward_demodulation, [], [f178, f360])).
fof(f2360, plain, (~ spl15_19 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f2359])).
fof(f2359, plain, ($false | (~ spl15_19 | ~ spl15_20)), inference(subsumption_resolution, [], [f2358, f186])).
fof(f2358, plain, ((e2 = e3) | (~ spl15_19 | ~ spl15_20)), inference(forward_demodulation, [], [f394, f390])).
fof(f2357, plain, (~ spl15_22 | spl15_38 | ~ spl15_86), inference(avatar_contradiction_clause, [], [f2356])).
fof(f2356, plain, ($false | (~ spl15_22 | spl15_38 | ~ spl15_86)), inference(subsumption_resolution, [], [f2354, f470])).
fof(f2354, plain, ((e1 = op(e1, e2)) | (~ spl15_22 | ~ spl15_86)), inference(backward_demodulation, [], [f799, f403])).
fof(f2350, plain, (~ spl15_25 | spl15_53 | ~ spl15_90), inference(avatar_contradiction_clause, [], [f2349])).
fof(f2349, plain, ($false | (~ spl15_25 | spl15_53 | ~ spl15_90)), inference(subsumption_resolution, [], [f2346, f534])).
fof(f2346, plain, ((e0 = op(e0, e2)) | (~ spl15_25 | ~ spl15_90)), inference(backward_demodulation, [], [f816, f416])).
fof(f2348, plain, (~ spl15_21 | ~ spl15_25), inference(avatar_split_clause, [], [f2342, f414, f397])).
fof(f2342, plain, (~ (e0 = op(e2, e2)) | ~ spl15_25), inference(backward_demodulation, [], [f172, f416])).
fof(f2347, plain, (~ spl15_9 | ~ spl15_25), inference(avatar_split_clause, [], [f2341, f414, f346])).
fof(f2341, plain, (~ (e0 = op(e3, e1)) | ~ spl15_25), inference(backward_demodulation, [], [f144, f416])).
fof(f2340, plain, (~ spl15_21 | ~ spl15_29), inference(avatar_split_clause, [], [f2335, f431, f397])).
fof(f2335, plain, (~ (e0 = op(e2, e2)) | ~ spl15_29), inference(backward_demodulation, [], [f170, f433])).
fof(f2320, plain, (~ spl15_42 | ~ spl15_43), inference(avatar_contradiction_clause, [], [f2319])).
fof(f2319, plain, ($false | (~ spl15_42 | ~ spl15_43)), inference(subsumption_resolution, [], [f2318, f184])).
fof(f184, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2318, plain, ((e1 = e2) | (~ spl15_42 | ~ spl15_43)), inference(backward_demodulation, [], [f492, f488])).
fof(f2317, plain, (~ spl15_36 | ~ spl15_48), inference(avatar_split_clause, [], [f2314, f511, f460])).
fof(f2314, plain, (~ (e3 = op(e1, e3)) | ~ spl15_48), inference(backward_demodulation, [], [f165, f513])).
fof(f2316, plain, (~ spl15_32 | ~ spl15_48), inference(avatar_split_clause, [], [f2312, f511, f443])).
fof(f2312, plain, (~ (e3 = op(e2, e0)) | ~ spl15_48), inference(backward_demodulation, [], [f136, f513])).
fof(f2315, plain, (spl15_12 | ~ spl15_48 | ~ spl15_93), inference(avatar_split_clause, [], [f2311, f827, f511, f358])).
fof(f2311, plain, ((e3 = op(e3, e1)) | (~ spl15_48 | ~ spl15_93)), inference(backward_demodulation, [], [f829, f513])).
fof(f2303, plain, (~ spl15_56 | ~ spl15_59 | ~ spl15_62 | spl15_119), inference(avatar_split_clause, [], [f2300, f953, f571, f558, f545])).
fof(f2300, plain, (~ (e3 = op(e0, e2)) | (~ spl15_59 | ~ spl15_62 | spl15_119)), inference(backward_demodulation, [], [f2237, f560])).
fof(f2237, plain, (~ (e3 = op(e0, op(e0, e1))) | (~ spl15_62 | spl15_119)), inference(forward_demodulation, [], [f955, f573])).
fof(f2291, plain, (~ spl15_9 | ~ spl15_2 | spl15_107), inference(avatar_split_clause, [], [f2290, f892, f316, f346])).
fof(f2290, plain, (~ (e0 = op(e3, e1)) | (~ spl15_2 | spl15_107)), inference(forward_demodulation, [], [f894, f318])).
fof(f318, plain, ((e1 = op(e3, e3)) | ~ spl15_2), inference(avatar_component_clause, [], [f316])).
fof(f894, plain, (~ (e0 = op(e3, op(e3, e3))) | spl15_107), inference(avatar_component_clause, [], [f892])).
fof(f2289, plain, (~ spl15_32 | ~ spl15_110 | spl15_117), inference(avatar_split_clause, [], [f2285, f942, f906, f443])).
fof(f2285, plain, (~ (e3 = op(e2, e0)) | (~ spl15_110 | spl15_117)), inference(backward_demodulation, [], [f944, f907])).
fof(f907, plain, ((e0 = op(e2, op(e2, e2))) | ~ spl15_110), inference(avatar_component_clause, [], [f906])).
fof(f2267, plain, (~ spl15_27 | ~ spl15_19), inference(avatar_split_clause, [], [f2266, f388, f422])).
fof(f2266, plain, (~ (e2 = op(e2, e1)) | ~ spl15_19), inference(forward_demodulation, [], [f173, f390])).
fof(f2260, plain, (~ spl15_11 | ~ spl15_2 | spl15_97), inference(avatar_split_clause, [], [f2246, f844, f316, f354])).
fof(f2246, plain, (~ (e2 = op(e3, e1)) | (~ spl15_2 | spl15_97)), inference(forward_demodulation, [], [f846, f318])).
fof(f846, plain, (~ (e2 = op(e3, op(e3, e3))) | spl15_97), inference(avatar_component_clause, [], [f844])).
fof(f2258, plain, (~ spl15_49 | spl15_61 | ~ spl15_80), inference(avatar_contradiction_clause, [], [f2257])).
fof(f2257, plain, ($false | (~ spl15_49 | spl15_61 | ~ spl15_80)), inference(subsumption_resolution, [], [f2256, f568])).
fof(f568, plain, (~ (e0 = op(e0, e0)) | spl15_61), inference(avatar_component_clause, [], [f567])).
fof(f567, plain, (spl15_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_61])])).
fof(f2256, plain, ((e0 = op(e0, e0)) | (~ spl15_49 | ~ spl15_80)), inference(forward_demodulation, [], [f774, f518])).
fof(f2253, plain, (~ spl15_21 | spl15_53 | ~ spl15_86), inference(avatar_contradiction_clause, [], [f2252])).
fof(f2252, plain, ($false | (~ spl15_21 | spl15_53 | ~ spl15_86)), inference(subsumption_resolution, [], [f2251, f534])).
fof(f2251, plain, ((e0 = op(e0, e2)) | (~ spl15_21 | ~ spl15_86)), inference(forward_demodulation, [], [f799, f399])).
fof(f2236, plain, (~ spl15_2 | ~ spl15_9 | ~ spl15_15 | spl15_112), inference(avatar_contradiction_clause, [], [f2235])).
fof(f2235, plain, ($false | (~ spl15_2 | ~ spl15_9 | ~ spl15_15 | spl15_112)), inference(subsumption_resolution, [], [f2234, f373])).
fof(f2234, plain, (~ (e2 = op(e3, e0)) | (~ spl15_2 | ~ spl15_9 | spl15_112)), inference(forward_demodulation, [], [f2233, f348])).
fof(f2233, plain, (~ (e2 = op(e3, op(e3, e1))) | (~ spl15_2 | spl15_112)), inference(forward_demodulation, [], [f918, f318])).
fof(f918, plain, (~ (e2 = op(e3, op(e3, op(e3, e3)))) | spl15_112), inference(avatar_component_clause, [], [f916])).
fof(f916, plain, (spl15_112 <=> (e2 = op(e3, op(e3, op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl15_112])])).
fof(f2226, plain, (spl15_4 | ~ spl15_8 | ~ spl15_87), inference(avatar_contradiction_clause, [], [f2225])).
fof(f2225, plain, ($false | (spl15_4 | ~ spl15_8 | ~ spl15_87)), inference(subsumption_resolution, [], [f2224, f325])).
fof(f2224, plain, ((e3 = op(e3, e3)) | (~ spl15_8 | ~ spl15_87)), inference(backward_demodulation, [], [f803, f343])).
fof(f803, plain, ((op(e3, e2) = op(op(e3, e2), e3)) | ~ spl15_87), inference(avatar_component_clause, [], [f801])).
fof(f2222, plain, (~ spl15_1 | ~ spl15_9), inference(avatar_split_clause, [], [f2217, f346, f312])).
fof(f2217, plain, (~ (e0 = op(e3, e3)) | ~ spl15_9), inference(backward_demodulation, [], [f179, f348])).
fof(f2203, plain, (spl15_8 | ~ spl15_32 | ~ spl15_94), inference(avatar_split_clause, [], [f2201, f831, f443, f341])).
fof(f2201, plain, ((e3 = op(e3, e2)) | (~ spl15_32 | ~ spl15_94)), inference(backward_demodulation, [], [f833, f445])).
fof(f2202, plain, (~ spl15_20 | ~ spl15_32), inference(avatar_split_clause, [], [f2200, f443, f392])).
fof(f2200, plain, (~ (e3 = op(e2, e3)) | ~ spl15_32), inference(backward_demodulation, [], [f171, f445])).
fof(f2198, plain, (spl15_12 | ~ spl15_36 | ~ spl15_81), inference(avatar_split_clause, [], [f2196, f776, f460, f358])).
fof(f2196, plain, ((e3 = op(e3, e1)) | (~ spl15_36 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f462])).
fof(f2180, plain, (~ spl15_35 | ~ spl15_43), inference(avatar_split_clause, [], [f2174, f490, f456])).
fof(f2174, plain, (~ (e2 = op(e1, e3)) | ~ spl15_43), inference(backward_demodulation, [], [f167, f492])).
fof(f2171, plain, (~ spl15_41 | ~ spl15_45), inference(avatar_split_clause, [], [f2164, f499, f482])).
fof(f2164, plain, (~ (e0 = op(e1, e1)) | ~ spl15_45), inference(backward_demodulation, [], [f163, f501])).
fof(f2170, plain, (~ spl15_29 | ~ spl15_45), inference(avatar_split_clause, [], [f2163, f499, f431])).
fof(f2163, plain, (~ (e0 = op(e2, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f136, f501])).
fof(f2162, plain, (~ spl15_1 | ~ spl15_49), inference(avatar_split_clause, [], [f2158, f516, f312])).
fof(f2158, plain, (~ (e0 = op(e3, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f153, f518])).
fof(f2136, plain, (~ spl15_60 | ~ spl15_62 | spl15_109), inference(avatar_split_clause, [], [f2131, f901, f571, f562])).
fof(f2131, plain, (~ (e3 = op(e0, e1)) | (~ spl15_62 | spl15_109)), inference(backward_demodulation, [], [f903, f573])).
fof(f903, plain, (~ (e3 = op(e0, op(e0, e0))) | spl15_109), inference(avatar_component_clause, [], [f901])).
fof(f2107, plain, (~ spl15_38 | ~ spl15_26 | spl15_90), inference(avatar_split_clause, [], [f2076, f814, f418, f469])).
fof(f2076, plain, (~ (e1 = op(e1, e2)) | (~ spl15_26 | spl15_90)), inference(backward_demodulation, [], [f815, f420])).
fof(f815, plain, (~ (op(e2, e1) = op(op(e2, e1), e2)) | spl15_90), inference(avatar_component_clause, [], [f814])).
fof(f2096, plain, (~ spl15_6 | spl15_34 | ~ spl15_87), inference(avatar_contradiction_clause, [], [f2095])).
fof(f2095, plain, ($false | (~ spl15_6 | spl15_34 | ~ spl15_87)), inference(subsumption_resolution, [], [f2094, f453])).
fof(f2094, plain, ((e1 = op(e1, e3)) | (~ spl15_6 | ~ spl15_87)), inference(backward_demodulation, [], [f803, f335])).
fof(f2089, plain, (~ spl15_7 | ~ spl15_15), inference(avatar_split_clause, [], [f2084, f371, f337])).
fof(f2084, plain, (~ (e2 = op(e3, e2)) | ~ spl15_15), inference(backward_demodulation, [], [f176, f373])).
fof(f2048, plain, (~ spl15_41 | spl15_57 | ~ spl15_89), inference(avatar_contradiction_clause, [], [f2047])).
fof(f2047, plain, ($false | (~ spl15_41 | spl15_57 | ~ spl15_89)), inference(subsumption_resolution, [], [f2046, f551])).
fof(f551, plain, (~ (e0 = op(e0, e1)) | spl15_57), inference(avatar_component_clause, [], [f550])).
fof(f2046, plain, ((e0 = op(e0, e1)) | (~ spl15_41 | ~ spl15_89)), inference(forward_demodulation, [], [f812, f484])).
fof(f2024, plain, (~ spl15_10 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f2023])).
fof(f2023, plain, ($false | (~ spl15_10 | ~ spl15_12)), inference(subsumption_resolution, [], [f2021, f185])).
fof(f2021, plain, ((e1 = e3) | (~ spl15_10 | ~ spl15_12)), inference(backward_demodulation, [], [f360, f352])).
fof(f1996, plain, (~ spl15_14 | ~ spl15_46), inference(avatar_split_clause, [], [f1992, f503, f367])).
fof(f1992, plain, (~ (e1 = op(e3, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f137, f505])).
fof(f1995, plain, (~ spl15_38 | ~ spl15_46), inference(avatar_split_clause, [], [f1991, f503, f469])).
fof(f1991, plain, (~ (e1 = op(e1, e2)) | ~ spl15_46), inference(backward_demodulation, [], [f164, f505])).
fof(f1982, plain, (~ spl15_52 | ~ spl15_60), inference(avatar_split_clause, [], [f1980, f562, f528])).
fof(f1980, plain, (~ (e3 = op(e0, e3)) | ~ spl15_60), inference(backward_demodulation, [], [f161, f564])).
fof(f1981, plain, (~ spl15_16 | ~ spl15_60 | spl15_88), inference(avatar_split_clause, [], [f1979, f806, f562, f375])).
fof(f1979, plain, (~ (e3 = op(e3, e0)) | (~ spl15_60 | spl15_88)), inference(backward_demodulation, [], [f807, f564])).
fof(f807, plain, (~ (op(e0, e1) = op(op(e0, e1), e0)) | spl15_88), inference(avatar_component_clause, [], [f806])).
fof(f1951, plain, (~ spl15_19 | ~ spl15_35), inference(avatar_split_clause, [], [f1950, f456, f388])).
fof(f1950, plain, (~ (e2 = op(e2, e3)) | ~ spl15_35), inference(forward_demodulation, [], [f154, f458])).
fof(f1949, plain, (spl15_19 | ~ spl15_7 | ~ spl15_87), inference(avatar_split_clause, [], [f1917, f801, f337, f388])).
fof(f1917, plain, ((e2 = op(e2, e3)) | (~ spl15_7 | ~ spl15_87)), inference(backward_demodulation, [], [f803, f339])).
fof(f339, plain, ((e2 = op(e3, e2)) | ~ spl15_7), inference(avatar_component_clause, [], [f337])).
fof(f1929, plain, (~ spl15_1 | ~ spl15_3), inference(avatar_contradiction_clause, [], [f1928])).
fof(f1928, plain, ($false | (~ spl15_1 | ~ spl15_3)), inference(subsumption_resolution, [], [f1927, f182])).
fof(f1927, plain, ((e0 = e2) | (~ spl15_1 | ~ spl15_3)), inference(backward_demodulation, [], [f322, f314])).
fof(f314, plain, ((e0 = op(e3, e3)) | ~ spl15_1), inference(avatar_component_clause, [], [f312])).
fof(f1905, plain, (~ spl15_41 | ~ spl15_48 | spl15_102), inference(avatar_contradiction_clause, [], [f1904])).
fof(f1904, plain, ($false | (~ spl15_41 | ~ spl15_48 | spl15_102)), inference(subsumption_resolution, [], [f1898, f513])).
fof(f1898, plain, (~ (e3 = op(e1, e0)) | (~ spl15_41 | spl15_102)), inference(backward_demodulation, [], [f869, f484])).
fof(f1903, plain, (~ spl15_33 | ~ spl15_41), inference(avatar_split_clause, [], [f1894, f482, f448])).
fof(f1894, plain, (~ (e0 = op(e1, e3)) | ~ spl15_41), inference(backward_demodulation, [], [f167, f484])).
fof(f1902, plain, (~ spl15_9 | ~ spl15_41), inference(avatar_split_clause, [], [f1893, f482, f346])).
fof(f1893, plain, (~ (e0 = op(e3, e1)) | ~ spl15_41), inference(backward_demodulation, [], [f143, f484])).
fof(f1884, plain, (~ spl15_3 | spl15_6 | ~ spl15_100), inference(avatar_contradiction_clause, [], [f1883])).
fof(f1883, plain, ($false | (~ spl15_3 | spl15_6 | ~ spl15_100)), inference(subsumption_resolution, [], [f1882, f334])).
fof(f334, plain, (~ (e1 = op(e3, e2)) | spl15_6), inference(avatar_component_clause, [], [f333])).
fof(f1882, plain, ((e1 = op(e3, e2)) | (~ spl15_3 | ~ spl15_100)), inference(forward_demodulation, [], [f859, f322])).
fof(f1867, plain, (spl15_23 | ~ spl15_27 | ~ spl15_90), inference(avatar_split_clause, [], [f1866, f814, f422, f405])).
fof(f1866, plain, ((e2 = op(e2, e2)) | (~ spl15_27 | ~ spl15_90)), inference(forward_demodulation, [], [f816, f424])).
fof(f1864, plain, (~ spl15_23 | ~ spl15_27), inference(avatar_split_clause, [], [f1863, f422, f405])).
fof(f1863, plain, (~ (e2 = op(e2, e2)) | ~ spl15_27), inference(forward_demodulation, [], [f172, f424])).
fof(f1861, plain, (~ spl15_7 | ~ spl15_3), inference(avatar_split_clause, [], [f1765, f320, f337])).
fof(f1765, plain, (~ (e2 = op(e3, e2)) | ~ spl15_3), inference(forward_demodulation, [], [f180, f322])).
fof(f1859, plain, (~ spl15_5 | ~ spl15_9), inference(avatar_split_clause, [], [f1857, f346, f329])).
fof(f1857, plain, (~ (e0 = op(e3, e2)) | ~ spl15_9), inference(backward_demodulation, [], [f178, f348])).
fof(f1852, plain, (~ spl15_27 | ~ spl15_28), inference(avatar_contradiction_clause, [], [f1851])).
fof(f1851, plain, ($false | (~ spl15_27 | ~ spl15_28)), inference(subsumption_resolution, [], [f1850, f186])).
fof(f1850, plain, ((e2 = e3) | (~ spl15_27 | ~ spl15_28)), inference(backward_demodulation, [], [f428, f424])).
fof(f1849, plain, (spl15_8 | ~ spl15_28 | ~ spl15_90), inference(avatar_split_clause, [], [f1847, f814, f426, f341])).
fof(f1847, plain, ((e3 = op(e3, e2)) | (~ spl15_28 | ~ spl15_90)), inference(backward_demodulation, [], [f816, f428])).
fof(f1834, plain, (~ spl15_44 | ~ spl15_48), inference(avatar_split_clause, [], [f1833, f511, f494])).
fof(f1833, plain, (~ (e3 = op(e1, e1)) | ~ spl15_48), inference(backward_demodulation, [], [f163, f513])).
fof(f1822, plain, (~ spl15_46 | ~ spl15_58 | spl15_88), inference(avatar_split_clause, [], [f1819, f806, f554, f503])).
fof(f1819, plain, (~ (e1 = op(e1, e0)) | (~ spl15_58 | spl15_88)), inference(backward_demodulation, [], [f807, f556])).
fof(f1821, plain, (~ spl15_54 | ~ spl15_58), inference(avatar_split_clause, [], [f1816, f554, f537])).
fof(f1816, plain, (~ (e1 = op(e0, e2)) | ~ spl15_58), inference(backward_demodulation, [], [f160, f556])).
fof(f1820, plain, (~ spl15_26 | ~ spl15_58), inference(avatar_split_clause, [], [f1814, f554, f418])).
fof(f1814, plain, (~ (e1 = op(e2, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f140, f556])).
fof(f1811, plain, (~ spl15_29 | ~ spl15_63 | ~ spl15_92), inference(avatar_contradiction_clause, [], [f1810])).
fof(f1810, plain, ($false | (~ spl15_29 | ~ spl15_63 | ~ spl15_92)), inference(subsumption_resolution, [], [f1809, f182])).
fof(f1809, plain, ((e0 = e2) | (~ spl15_29 | ~ spl15_63 | ~ spl15_92)), inference(forward_demodulation, [], [f1802, f433])).
fof(f1802, plain, ((e2 = op(e2, e0)) | (~ spl15_63 | ~ spl15_92)), inference(backward_demodulation, [], [f825, f577])).
fof(f1808, plain, (~ spl15_59 | ~ spl15_63), inference(avatar_split_clause, [], [f1799, f575, f558])).
fof(f1799, plain, (~ (e2 = op(e0, e1)) | ~ spl15_63), inference(backward_demodulation, [], [f157, f577])).
fof(f1807, plain, (~ spl15_47 | ~ spl15_63), inference(avatar_split_clause, [], [f1798, f575, f507])).
fof(f1798, plain, (~ (e2 = op(e1, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f133, f577])).
fof(f1797, plain, (~ spl15_38 | spl15_42 | ~ spl15_85), inference(avatar_contradiction_clause, [], [f1796])).
fof(f1796, plain, ($false | (~ spl15_38 | spl15_42 | ~ spl15_85)), inference(subsumption_resolution, [], [f1795, f487])).
fof(f1795, plain, ((e1 = op(e1, e1)) | (~ spl15_38 | ~ spl15_85)), inference(forward_demodulation, [], [f795, f471])).
fof(f1786, plain, (~ spl15_61 | ~ spl15_29), inference(avatar_split_clause, [], [f1785, f431, f567])).
fof(f1785, plain, (~ (e0 = op(e0, e0)) | ~ spl15_29), inference(forward_demodulation, [], [f134, f433])).
fof(f1780, plain, (~ spl15_26 | ~ spl15_18), inference(avatar_split_clause, [], [f1779, f384, f418])).
fof(f1779, plain, (~ (e1 = op(e2, e1)) | ~ spl15_18), inference(forward_demodulation, [], [f173, f386])).
fof(f386, plain, ((e1 = op(e2, e3)) | ~ spl15_18), inference(avatar_component_clause, [], [f384])).
fof(f1763, plain, (~ spl15_6 | ~ spl15_3 | spl15_100), inference(avatar_split_clause, [], [f1750, f858, f320, f333])).
fof(f1750, plain, (~ (e1 = op(e3, e2)) | (~ spl15_3 | spl15_100)), inference(backward_demodulation, [], [f860, f322])).
fof(f860, plain, (~ (e1 = op(e3, op(e3, e3))) | spl15_100), inference(avatar_component_clause, [], [f858])).
fof(f1762, plain, (~ spl15_5 | ~ spl15_3 | spl15_107), inference(avatar_split_clause, [], [f1751, f892, f320, f329])).
fof(f1751, plain, (~ (e0 = op(e3, e2)) | (~ spl15_3 | spl15_107)), inference(backward_demodulation, [], [f894, f322])).
fof(f1742, plain, (~ spl15_14 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f1741])).
fof(f1741, plain, ($false | (~ spl15_14 | ~ spl15_16)), inference(subsumption_resolution, [], [f1740, f185])).
fof(f1740, plain, ((e1 = e3) | (~ spl15_14 | ~ spl15_16)), inference(forward_demodulation, [], [f377, f369])).
fof(f1739, plain, (~ spl15_2 | ~ spl15_18), inference(avatar_split_clause, [], [f1735, f384, f316])).
fof(f1735, plain, (~ (e1 = op(e3, e3)) | ~ spl15_18), inference(backward_demodulation, [], [f156, f386])).
fof(f1731, plain, (~ spl15_18 | ~ spl15_24 | spl15_103), inference(avatar_split_clause, [], [f1726, f872, f409, f384])).
fof(f1726, plain, (~ (e1 = op(e2, e3)) | (~ spl15_24 | spl15_103)), inference(backward_demodulation, [], [f874, f411])).
fof(f874, plain, (~ (e1 = op(e2, op(e2, e2))) | spl15_103), inference(avatar_component_clause, [], [f872])).
fof(f1707, plain, (~ spl15_46 | ~ spl15_47), inference(avatar_contradiction_clause, [], [f1706])).
fof(f1706, plain, ($false | (~ spl15_46 | ~ spl15_47)), inference(subsumption_resolution, [], [f1705, f184])).
fof(f1705, plain, ((e1 = e2) | (~ spl15_46 | ~ spl15_47)), inference(backward_demodulation, [], [f509, f505])).
fof(f1697, plain, (~ spl15_57 | ~ spl15_59), inference(avatar_contradiction_clause, [], [f1696])).
fof(f1696, plain, ($false | (~ spl15_57 | ~ spl15_59)), inference(subsumption_resolution, [], [f1695, f182])).
fof(f1695, plain, ((e0 = e2) | (~ spl15_57 | ~ spl15_59)), inference(backward_demodulation, [], [f560, f552])).
fof(f1691, plain, (~ spl15_33 | ~ spl15_44 | spl15_115), inference(avatar_contradiction_clause, [], [f1690])).
fof(f1690, plain, ($false | (~ spl15_33 | ~ spl15_44 | spl15_115)), inference(subsumption_resolution, [], [f1689, f450])).
fof(f1689, plain, (~ (e0 = op(e1, e3)) | (~ spl15_44 | spl15_115)), inference(forward_demodulation, [], [f934, f496])).
fof(f1685, plain, (~ spl15_11 | ~ spl15_59), inference(avatar_contradiction_clause, [], [f1684])).
fof(f1684, plain, ($false | (~ spl15_11 | ~ spl15_59)), inference(subsumption_resolution, [], [f1683, f560])).
fof(f1683, plain, (~ (e2 = op(e0, e1)) | ~ spl15_11), inference(forward_demodulation, [], [f141, f356])).
fof(f1676, plain, (~ spl15_57 | ~ spl15_61), inference(avatar_split_clause, [], [f1305, f567, f550])).
fof(f1305, plain, (~ (e0 = op(e0, e1)) | ~ spl15_61), inference(forward_demodulation, [], [f157, f569])).
fof(f569, plain, ((e0 = op(e0, e0)) | ~ spl15_61), inference(avatar_component_clause, [], [f567])).
fof(f1675, plain, (~ spl15_54 | ~ spl15_38), inference(avatar_split_clause, [], [f1579, f469, f537])).
fof(f1579, plain, (~ (e1 = op(e0, e2)) | ~ spl15_38), inference(forward_demodulation, [], [f145, f471])).
fof(f1671, plain, (~ spl15_22 | ~ spl15_38), inference(avatar_split_clause, [], [f1670, f469, f401])).
fof(f1670, plain, (~ (e1 = op(e2, e2)) | ~ spl15_38), inference(forward_demodulation, [], [f148, f471])).
fof(f1668, plain, (~ spl15_18 | ~ spl15_99 | spl15_106), inference(avatar_split_clause, [], [f1667, f887, f853, f384])).
fof(f1667, plain, (~ (e1 = op(e2, e3)) | (~ spl15_99 | spl15_106)), inference(forward_demodulation, [], [f889, f854])).
fof(f854, plain, ((e3 = op(e2, op(e2, e2))) | ~ spl15_99), inference(avatar_component_clause, [], [f853])).
fof(f1644, plain, (~ spl15_9 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1643])).
fof(f1643, plain, ($false | (~ spl15_9 | ~ spl15_11)), inference(subsumption_resolution, [], [f1642, f182])).
fof(f1642, plain, ((e0 = e2) | (~ spl15_9 | ~ spl15_11)), inference(backward_demodulation, [], [f356, f348])).
fof(f1634, plain, (~ spl15_21 | ~ spl15_22), inference(avatar_contradiction_clause, [], [f1633])).
fof(f1633, plain, ($false | (~ spl15_21 | ~ spl15_22)), inference(subsumption_resolution, [], [f1632, f181])).
fof(f181, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1632, plain, ((e0 = e1) | (~ spl15_21 | ~ spl15_22)), inference(forward_demodulation, [], [f403, f399])).
fof(f1624, plain, (~ spl15_18 | ~ spl15_21 | ~ spl15_32 | spl15_106), inference(avatar_split_clause, [], [f1621, f887, f443, f397, f384])).
fof(f1621, plain, (~ (e1 = op(e2, e3)) | (~ spl15_21 | ~ spl15_32 | spl15_106)), inference(backward_demodulation, [], [f1582, f445])).
fof(f1582, plain, (~ (e1 = op(e2, op(e2, e0))) | (~ spl15_21 | spl15_106)), inference(forward_demodulation, [], [f889, f399])).
fof(f1623, plain, (~ spl15_28 | ~ spl15_32), inference(avatar_split_clause, [], [f1618, f443, f426])).
fof(f1618, plain, (~ (e3 = op(e2, e1)) | ~ spl15_32), inference(backward_demodulation, [], [f169, f445])).
fof(f1617, plain, (~ spl15_1 | ~ spl15_33), inference(avatar_split_clause, [], [f1613, f448, f312])).
fof(f1613, plain, (~ (e0 = op(e3, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f155, f450])).
fof(f1608, plain, (~ spl15_38 | ~ spl15_39), inference(avatar_contradiction_clause, [], [f1607])).
fof(f1607, plain, ($false | (~ spl15_38 | ~ spl15_39)), inference(subsumption_resolution, [], [f1606, f184])).
fof(f1606, plain, ((e1 = e2) | (~ spl15_38 | ~ spl15_39)), inference(forward_demodulation, [], [f475, f471])).
fof(f1599, plain, (~ spl15_35 | ~ spl15_47), inference(avatar_split_clause, [], [f1596, f507, f456])).
fof(f1596, plain, (~ (e2 = op(e1, e3)) | ~ spl15_47), inference(backward_demodulation, [], [f165, f509])).
fof(f1589, plain, (~ spl15_58 | ~ spl15_59), inference(avatar_contradiction_clause, [], [f1588])).
fof(f1588, plain, ($false | (~ spl15_58 | ~ spl15_59)), inference(subsumption_resolution, [], [f1587, f184])).
fof(f1587, plain, ((e1 = e2) | (~ spl15_58 | ~ spl15_59)), inference(forward_demodulation, [], [f560, f556])).
fof(f1581, plain, (~ spl15_56 | ~ spl15_52), inference(avatar_split_clause, [], [f1452, f528, f545])).
fof(f1452, plain, (~ (e3 = op(e0, e2)) | ~ spl15_52), inference(forward_demodulation, [], [f162, f530])).
fof(f1576, plain, (~ spl15_42 | ~ spl15_38), inference(avatar_split_clause, [], [f1575, f469, f486])).
fof(f1575, plain, (~ (e1 = op(e1, e1)) | ~ spl15_38), inference(forward_demodulation, [], [f166, f471])).
fof(f1556, plain, (~ spl15_19 | ~ spl15_31), inference(avatar_split_clause, [], [f1554, f439, f388])).
fof(f1554, plain, (~ (e2 = op(e2, e3)) | ~ spl15_31), inference(backward_demodulation, [], [f171, f441])).
fof(f1542, plain, (spl15_31 | ~ spl15_55 | ~ spl15_84), inference(avatar_split_clause, [], [f1539, f789, f541, f439])).
fof(f1539, plain, ((e2 = op(e2, e0)) | (~ spl15_55 | ~ spl15_84)), inference(backward_demodulation, [], [f791, f543])).
fof(f1535, plain, (~ spl15_14 | ~ spl15_1 | spl15_100), inference(avatar_split_clause, [], [f1534, f858, f312, f367])).
fof(f1534, plain, (~ (e1 = op(e3, e0)) | (~ spl15_1 | spl15_100)), inference(forward_demodulation, [], [f860, f314])).
fof(f1524, plain, (~ spl15_35 | ~ spl15_41 | ~ spl15_48 | spl15_113), inference(avatar_split_clause, [], [f1523, f921, f511, f482, f456])).
fof(f1523, plain, (~ (e2 = op(e1, e3)) | (~ spl15_41 | ~ spl15_48 | spl15_113)), inference(forward_demodulation, [], [f1522, f513])).
fof(f1522, plain, (~ (e2 = op(e1, op(e1, e0))) | (~ spl15_41 | spl15_113)), inference(forward_demodulation, [], [f923, f484])).
fof(f1491, plain, (~ spl15_22 | ~ spl15_30), inference(avatar_split_clause, [], [f1489, f435, f401])).
fof(f1489, plain, (~ (e1 = op(e2, e2)) | ~ spl15_30), inference(backward_demodulation, [], [f170, f437])).
fof(f1490, plain, (~ spl15_14 | ~ spl15_30), inference(avatar_split_clause, [], [f1487, f435, f367])).
fof(f1487, plain, (~ (e1 = op(e3, e0)) | ~ spl15_30), inference(backward_demodulation, [], [f138, f437])).
fof(f1483, plain, (~ spl15_41 | ~ spl15_44), inference(avatar_contradiction_clause, [], [f1482])).
fof(f1482, plain, ($false | (~ spl15_41 | ~ spl15_44)), inference(subsumption_resolution, [], [f1481, f183])).
fof(f183, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f1481, plain, ((e0 = e3) | (~ spl15_41 | ~ spl15_44)), inference(backward_demodulation, [], [f496, f484])).
fof(f1471, plain, (~ spl15_61 | ~ spl15_63), inference(avatar_contradiction_clause, [], [f1470])).
fof(f1470, plain, ($false | (~ spl15_61 | ~ spl15_63)), inference(subsumption_resolution, [], [f1469, f182])).
fof(f1469, plain, ((e0 = e2) | (~ spl15_61 | ~ spl15_63)), inference(forward_demodulation, [], [f577, f569])).
fof(f1467, plain, (~ spl15_1 | spl15_49 | ~ spl15_83), inference(avatar_contradiction_clause, [], [f1466])).
fof(f1466, plain, ($false | (~ spl15_1 | spl15_49 | ~ spl15_83)), inference(subsumption_resolution, [], [f1465, f517])).
fof(f1465, plain, ((e0 = op(e0, e3)) | (~ spl15_1 | ~ spl15_83)), inference(forward_demodulation, [], [f786, f314])).
fof(f1422, plain, (~ spl15_15 | ~ spl15_11), inference(avatar_split_clause, [], [f1421, f354, f371])).
fof(f1421, plain, (~ (e2 = op(e3, e0)) | ~ spl15_11), inference(forward_demodulation, [], [f175, f356])).
fof(f1420, plain, (~ spl15_13 | ~ spl15_61), inference(avatar_split_clause, [], [f1353, f567, f363])).
fof(f1353, plain, (~ (e0 = op(e3, e0)) | ~ spl15_61), inference(forward_demodulation, [], [f135, f569])).
fof(f1417, plain, (~ spl15_11 | ~ spl15_1 | ~ spl15_14 | spl15_112), inference(avatar_split_clause, [], [f1415, f916, f367, f312, f354])).
fof(f1415, plain, (~ (e2 = op(e3, e1)) | (~ spl15_1 | ~ spl15_14 | spl15_112)), inference(backward_demodulation, [], [f1260, f369])).
fof(f1260, plain, (~ (e2 = op(e3, op(e3, e0))) | (~ spl15_1 | spl15_112)), inference(forward_demodulation, [], [f918, f314])).
fof(f1411, plain, (~ spl15_25 | ~ spl15_28), inference(avatar_contradiction_clause, [], [f1410])).
fof(f1410, plain, ($false | (~ spl15_25 | ~ spl15_28)), inference(subsumption_resolution, [], [f1409, f183])).
fof(f1409, plain, ((e0 = e3) | (~ spl15_25 | ~ spl15_28)), inference(forward_demodulation, [], [f428, f416])).
fof(f1402, plain, (~ spl15_47 | ~ spl15_48), inference(avatar_contradiction_clause, [], [f1401])).
fof(f1401, plain, ($false | (~ spl15_47 | ~ spl15_48)), inference(subsumption_resolution, [], [f1400, f186])).
fof(f1400, plain, ((e2 = e3) | (~ spl15_47 | ~ spl15_48)), inference(forward_demodulation, [], [f513, f509])).
fof(f1399, plain, (~ spl15_36 | ~ spl15_52), inference(avatar_split_clause, [], [f1396, f528, f460])).
fof(f1396, plain, (~ (e3 = op(e1, e3)) | ~ spl15_52), inference(backward_demodulation, [], [f151, f530])).
fof(f1398, plain, (spl15_16 | ~ spl15_52 | ~ spl15_80), inference(avatar_contradiction_clause, [], [f1397])).
fof(f1397, plain, ($false | (spl15_16 | ~ spl15_52 | ~ spl15_80)), inference(subsumption_resolution, [], [f1395, f376])).
fof(f376, plain, (~ (e3 = op(e3, e0)) | spl15_16), inference(avatar_component_clause, [], [f375])).
fof(f1395, plain, ((e3 = op(e3, e0)) | (~ spl15_52 | ~ spl15_80)), inference(backward_demodulation, [], [f774, f530])).
fof(f1391, plain, (spl15_46 | ~ spl15_58 | ~ spl15_88), inference(avatar_split_clause, [], [f1388, f806, f554, f503])).
fof(f1388, plain, ((e1 = op(e1, e0)) | (~ spl15_58 | ~ spl15_88)), inference(backward_demodulation, [], [f808, f556])).
fof(f1382, plain, (~ spl15_49 | ~ spl15_61), inference(avatar_split_clause, [], [f1381, f567, f516])).
fof(f1381, plain, (~ (e0 = op(e0, e3)) | ~ spl15_61), inference(forward_demodulation, [], [f159, f569])).
fof(f1380, plain, (~ spl15_51 | ~ spl15_19), inference(avatar_split_clause, [], [f1379, f388, f524])).
fof(f1379, plain, (~ (e2 = op(e0, e3)) | ~ spl15_19), inference(forward_demodulation, [], [f152, f390])).
fof(f1352, plain, (~ spl15_15 | ~ spl15_1 | spl15_97), inference(avatar_split_clause, [], [f1263, f844, f312, f371])).
fof(f1263, plain, (~ (e2 = op(e3, e0)) | (~ spl15_1 | spl15_97)), inference(forward_demodulation, [], [f846, f314])).
fof(f1351, plain, (~ spl15_15 | ~ spl15_47), inference(avatar_split_clause, [], [f1350, f507, f371])).
fof(f1350, plain, (~ (e2 = op(e3, e0)) | ~ spl15_47), inference(forward_demodulation, [], [f137, f509])).
fof(f1343, plain, (~ spl15_30 | ~ spl15_31), inference(avatar_contradiction_clause, [], [f1342])).
fof(f1342, plain, ($false | (~ spl15_30 | ~ spl15_31)), inference(subsumption_resolution, [], [f1341, f184])).
fof(f1341, plain, ((e1 = e2) | (~ spl15_30 | ~ spl15_31)), inference(backward_demodulation, [], [f441, f437])).
fof(f1340, plain, (~ spl15_31 | ~ spl15_32), inference(avatar_contradiction_clause, [], [f1339])).
fof(f1339, plain, ($false | (~ spl15_31 | ~ spl15_32)), inference(subsumption_resolution, [], [f1338, f186])).
fof(f1338, plain, ((e2 = e3) | (~ spl15_31 | ~ spl15_32)), inference(backward_demodulation, [], [f445, f441])).
fof(f1334, plain, (~ spl15_37 | ~ spl15_38), inference(avatar_contradiction_clause, [], [f1333])).
fof(f1333, plain, ($false | (~ spl15_37 | ~ spl15_38)), inference(subsumption_resolution, [], [f1332, f181])).
fof(f1332, plain, ((e0 = e1) | (~ spl15_37 | ~ spl15_38)), inference(backward_demodulation, [], [f471, f467])).
fof(f1331, plain, (~ spl15_38 | ~ spl15_40), inference(avatar_contradiction_clause, [], [f1330])).
fof(f1330, plain, ($false | (~ spl15_38 | ~ spl15_40)), inference(subsumption_resolution, [], [f1329, f185])).
fof(f1329, plain, ((e1 = e3) | (~ spl15_38 | ~ spl15_40)), inference(backward_demodulation, [], [f479, f471])).
fof(f1315, plain, (spl15_31 | ~ spl15_59 | ~ spl15_88), inference(avatar_split_clause, [], [f1314, f806, f558, f439])).
fof(f1314, plain, ((e2 = op(e2, e0)) | (~ spl15_59 | ~ spl15_88)), inference(backward_demodulation, [], [f808, f560])).
fof(f1309, plain, (~ spl15_22 | ~ spl15_25 | ~ spl15_32 | spl15_117), inference(avatar_contradiction_clause, [], [f1308])).
fof(f1308, plain, ($false | (~ spl15_22 | ~ spl15_25 | ~ spl15_32 | spl15_117)), inference(subsumption_resolution, [], [f1307, f445])).
fof(f1307, plain, (~ (e3 = op(e2, e0)) | (~ spl15_22 | ~ spl15_25 | spl15_117)), inference(forward_demodulation, [], [f1306, f416])).
fof(f1306, plain, (~ (e3 = op(e2, op(e2, e1))) | (~ spl15_22 | spl15_117)), inference(forward_demodulation, [], [f944, f403])).
fof(f1282, plain, (~ spl15_45 | ~ spl15_61), inference(avatar_split_clause, [], [f1281, f567, f499])).
fof(f1281, plain, (~ (e0 = op(e1, e0)) | ~ spl15_61), inference(forward_demodulation, [], [f133, f569])).
fof(f1253, plain, (~ spl15_1 | ~ spl15_2), inference(avatar_contradiction_clause, [], [f1252])).
fof(f1252, plain, ($false | (~ spl15_1 | ~ spl15_2)), inference(subsumption_resolution, [], [f1251, f181])).
fof(f1251, plain, ((e0 = e1) | (~ spl15_1 | ~ spl15_2)), inference(backward_demodulation, [], [f318, f314])).
fof(f1243, plain, (~ spl15_22 | ~ spl15_25 | spl15_110), inference(avatar_contradiction_clause, [], [f1242])).
fof(f1242, plain, ($false | (~ spl15_22 | ~ spl15_25 | spl15_110)), inference(subsumption_resolution, [], [f1239, f416])).
fof(f1239, plain, (~ (e0 = op(e2, e1)) | (~ spl15_22 | spl15_110)), inference(backward_demodulation, [], [f908, f403])).
fof(f1234, plain, (~ spl15_25 | ~ spl15_27), inference(avatar_contradiction_clause, [], [f1233])).
fof(f1233, plain, ($false | (~ spl15_25 | ~ spl15_27)), inference(subsumption_resolution, [], [f1232, f182])).
fof(f1232, plain, ((e0 = e2) | (~ spl15_25 | ~ spl15_27)), inference(forward_demodulation, [], [f424, f416])).
fof(f1218, plain, (~ spl15_50 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f1217])).
fof(f1217, plain, ($false | (~ spl15_50 | ~ spl15_52)), inference(subsumption_resolution, [], [f1216, f185])).
fof(f1216, plain, ((e1 = e3) | (~ spl15_50 | ~ spl15_52)), inference(forward_demodulation, [], [f530, f522])).
fof(f1207, plain, (~ spl15_56 | ~ spl15_60), inference(avatar_split_clause, [], [f1201, f562, f545])).
fof(f1201, plain, (~ (e3 = op(e0, e2)) | ~ spl15_60), inference(backward_demodulation, [], [f160, f564])).
fof(f1197, plain, (~ spl15_61 | ~ spl15_62), inference(avatar_contradiction_clause, [], [f1196])).
fof(f1196, plain, ($false | (~ spl15_61 | ~ spl15_62)), inference(subsumption_resolution, [], [f1195, f181])).
fof(f1195, plain, ((e0 = e1) | (~ spl15_61 | ~ spl15_62)), inference(backward_demodulation, [], [f573, f569])).
fof(f1165, plain, (~ spl15_54 | ~ spl15_50), inference(avatar_split_clause, [], [f1164, f520, f537])).
fof(f1164, plain, (~ (e1 = op(e0, e2)) | ~ spl15_50), inference(forward_demodulation, [], [f162, f522])).
fof(f1151, plain, (~ spl15_41 | ~ spl15_25), inference(avatar_split_clause, [], [f1150, f414, f482])).
fof(f1150, plain, (~ (e0 = op(e1, e1)) | ~ spl15_25), inference(forward_demodulation, [], [f142, f416])).
fof(f1142, plain, (~ spl15_34 | ~ spl15_18), inference(avatar_split_clause, [], [f1141, f384, f452])).
fof(f1141, plain, (~ (e1 = op(e1, e3)) | ~ spl15_18), inference(forward_demodulation, [], [f154, f386])).
fof(f1138, plain, (~ spl15_21 | ~ spl15_5), inference(avatar_split_clause, [], [f1137, f329, f397])).
fof(f1137, plain, (~ (e0 = op(e2, e2)) | ~ spl15_5), inference(forward_demodulation, [], [f150, f331])).
fof(f1125, plain, (~ spl15_5 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1124])).
fof(f1124, plain, ($false | (~ spl15_5 | ~ spl15_8)), inference(subsumption_resolution, [], [f1123, f183])).
fof(f1123, plain, ((e0 = e3) | (~ spl15_5 | ~ spl15_8)), inference(forward_demodulation, [], [f343, f331])).
fof(f1113, plain, (~ spl15_6 | ~ spl15_7), inference(avatar_contradiction_clause, [], [f1112])).
fof(f1112, plain, ($false | (~ spl15_6 | ~ spl15_7)), inference(subsumption_resolution, [], [f1111, f184])).
fof(f1111, plain, ((e1 = e2) | (~ spl15_6 | ~ spl15_7)), inference(backward_demodulation, [], [f339, f335])).
fof(f1110, plain, (~ spl15_7 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1109])).
fof(f1109, plain, ($false | (~ spl15_7 | ~ spl15_8)), inference(subsumption_resolution, [], [f1108, f186])).
fof(f1108, plain, ((e2 = e3) | (~ spl15_7 | ~ spl15_8)), inference(backward_demodulation, [], [f343, f339])).
fof(f1105, plain, (~ spl15_10 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1104])).
fof(f1104, plain, ($false | (~ spl15_10 | ~ spl15_11)), inference(subsumption_resolution, [], [f1103, f184])).
fof(f1103, plain, ((e1 = e2) | (~ spl15_10 | ~ spl15_11)), inference(backward_demodulation, [], [f356, f352])).
fof(f1102, plain, (~ spl15_11 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f1101])).
fof(f1101, plain, ($false | (~ spl15_11 | ~ spl15_12)), inference(subsumption_resolution, [], [f1100, f186])).
fof(f1100, plain, ((e2 = e3) | (~ spl15_11 | ~ spl15_12)), inference(backward_demodulation, [], [f360, f356])).
fof(f1095, plain, (~ spl15_14 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1094])).
fof(f1094, plain, ($false | (~ spl15_14 | ~ spl15_15)), inference(subsumption_resolution, [], [f1093, f184])).
fof(f1093, plain, ((e1 = e2) | (~ spl15_14 | ~ spl15_15)), inference(backward_demodulation, [], [f373, f369])).
fof(f1086, plain, (~ spl15_17 | ~ spl15_18), inference(avatar_contradiction_clause, [], [f1085])).
fof(f1085, plain, ($false | (~ spl15_17 | ~ spl15_18)), inference(subsumption_resolution, [], [f1084, f181])).
fof(f1084, plain, ((e0 = e1) | (~ spl15_17 | ~ spl15_18)), inference(backward_demodulation, [], [f386, f382])).
fof(f1083, plain, (~ spl15_18 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f1082])).
fof(f1082, plain, ($false | (~ spl15_18 | ~ spl15_19)), inference(subsumption_resolution, [], [f1081, f184])).
fof(f1081, plain, ((e1 = e2) | (~ spl15_18 | ~ spl15_19)), inference(backward_demodulation, [], [f390, f386])).
fof(f1080, plain, (~ spl15_3 | ~ spl15_19), inference(avatar_split_clause, [], [f1079, f388, f320])).
fof(f1079, plain, (~ (e2 = op(e3, e3)) | ~ spl15_19), inference(backward_demodulation, [], [f156, f390])).
fof(f1077, plain, (~ spl15_30 | ~ spl15_21 | spl15_103), inference(avatar_split_clause, [], [f1071, f872, f397, f435])).
fof(f1071, plain, (~ (e1 = op(e2, e0)) | (~ spl15_21 | spl15_103)), inference(backward_demodulation, [], [f874, f399])).
fof(f1076, plain, (~ spl15_21 | ~ spl15_32 | spl15_99), inference(avatar_contradiction_clause, [], [f1075])).
fof(f1075, plain, ($false | (~ spl15_21 | ~ spl15_32 | spl15_99)), inference(subsumption_resolution, [], [f1070, f445])).
fof(f1070, plain, (~ (e3 = op(e2, e0)) | (~ spl15_21 | spl15_99)), inference(backward_demodulation, [], [f855, f399])).
fof(f1056, plain, (~ spl15_16 | ~ spl15_32), inference(avatar_split_clause, [], [f1052, f443, f375])).
fof(f1052, plain, (~ (e3 = op(e3, e0)) | ~ spl15_32), inference(backward_demodulation, [], [f138, f445])).
fof(f1051, plain, (~ spl15_34 | ~ spl15_35), inference(avatar_contradiction_clause, [], [f1050])).
fof(f1050, plain, ($false | (~ spl15_34 | ~ spl15_35)), inference(subsumption_resolution, [], [f1049, f184])).
fof(f1049, plain, ((e1 = e2) | (~ spl15_34 | ~ spl15_35)), inference(backward_demodulation, [], [f458, f454])).
fof(f1039, plain, (~ spl15_21 | ~ spl15_37), inference(avatar_split_clause, [], [f1036, f465, f397])).
fof(f1036, plain, (~ (e0 = op(e2, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f148, f467])).
fof(f1020, plain, (~ spl15_37 | ~ spl15_45), inference(avatar_split_clause, [], [f1015, f499, f465])).
fof(f1015, plain, (~ (e0 = op(e1, e2)) | ~ spl15_45), inference(backward_demodulation, [], [f164, f501])).
fof(f1003, plain, (~ spl15_35 | ~ spl15_51), inference(avatar_split_clause, [], [f997, f524, f456])).
fof(f997, plain, (~ (e2 = op(e1, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f151, f526])).
fof(f996, plain, (spl15_61 | ~ spl15_53 | ~ spl15_84), inference(avatar_split_clause, [], [f991, f789, f533, f567])).
fof(f991, plain, ((e0 = op(e0, e0)) | (~ spl15_53 | ~ spl15_84)), inference(backward_demodulation, [], [f791, f535])).
fof(f986, plain, (spl15_61 | ~ spl15_57 | ~ spl15_88), inference(avatar_split_clause, [], [f980, f806, f550, f567])).
fof(f980, plain, ((e0 = op(e0, e0)) | (~ spl15_57 | ~ spl15_88)), inference(backward_demodulation, [], [f808, f552])).
fof(f974, plain, (spl15_16 | ~ spl15_64 | ~ spl15_92), inference(avatar_split_clause, [], [f965, f823, f579, f375])).
fof(f965, plain, ((e3 = op(e3, e0)) | (~ spl15_64 | ~ spl15_92)), inference(backward_demodulation, [], [f825, f581])).
fof(f957, plain, (~ spl15_118 | ~ spl15_43 | ~ spl15_115), inference(avatar_split_clause, [], [f309, f932, f490, f947])).
fof(f309, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, op(e1, e1))))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, op(e1, e1))))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e0 = op(e1, op(e1, e1))) & (e2 = op(e1, e1)) & (e3 = op(e1, op(e1, op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax28)).
fof(f956, plain, (~ spl15_119 | ~ spl15_62 | ~ spl15_111), inference(avatar_split_clause, [], [f308, f911, f571, f953])).
fof(f308, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e3 = op(e0, op(e0, op(e0, e0))))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e3 = op(e0, op(e0, op(e0, e0))))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e2 = op(e0, op(e0, e0))) & (op(e0, e0) = e1) & (e3 = op(e0, op(e0, op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax27)).
fof(f951, plain, (~ spl15_117 | ~ spl15_22 | ~ spl15_110), inference(avatar_split_clause, [], [f307, f906, f401, f942])).
fof(f307, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, op(e2, e2))))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, op(e2, e2))))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(e2, op(e2, e2))) & (e1 = op(e2, e2)) & (e3 = op(e2, op(e2, op(e2, e2))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax26)).
fof(f950, plain, (~ spl15_118 | ~ spl15_41 | ~ spl15_104), inference(avatar_split_clause, [], [f306, f877, f482, f947])).
fof(f306, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, op(e1, e1))))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e3 = op(e1, op(e1, op(e1, e1))))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e2 = op(e1, op(e1, e1))) & (e0 = op(e1, e1)) & (e3 = op(e1, op(e1, op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax25)).
fof(f945, plain, (~ spl15_117 | ~ spl15_21 | ~ spl15_103), inference(avatar_split_clause, [], [f305, f872, f397, f942])).
fof(f305, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, op(e2, e2))))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(e2, op(e2, op(e2, e2))))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e1 = op(e2, op(e2, e2))) & (e0 = op(e2, e2)) & (e3 = op(e2, op(e2, op(e2, e2))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax24)).
fof(f935, plain, (~ spl15_113 | ~ spl15_44 | ~ spl15_115), inference(avatar_split_clause, [], [f303, f932, f494, f921])).
fof(f303, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1))))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e0 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1))))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e0 = op(e1, op(e1, e1))) & (e3 = op(e1, e1)) & (e2 = op(e1, op(e1, op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax22)).
fof(f930, plain, (~ spl15_114 | ~ spl15_62 | ~ spl15_109), inference(avatar_split_clause, [], [f302, f901, f571, f927])).
fof(f302, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, op(e0, e0))))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e3 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(e0, op(e0, op(e0, e0))))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e3 = op(e0, op(e0, e0))) & (op(e0, e0) = e1) & (e2 = op(e0, op(e0, op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax21)).
fof(f925, plain, (~ spl15_112 | ~ spl15_2 | ~ spl15_107), inference(avatar_split_clause, [], [f301, f892, f316, f916])).
fof(f301, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3))))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3))))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e0 = op(e3, op(e3, e3))) & (e1 = op(e3, e3)) & (e2 = op(e3, op(e3, op(e3, e3))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax20)).
fof(f924, plain, (~ spl15_113 | ~ spl15_41 | ~ spl15_102), inference(avatar_split_clause, [], [f300, f867, f482, f921])).
fof(f300, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1))))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e0 = op(e1, e1)) | ~ (e2 = op(e1, op(e1, op(e1, e1))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e3 = op(e1, op(e1, e1))) & (e0 = op(e1, e1)) & (e2 = op(e1, op(e1, op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax19)).
fof(f919, plain, (~ spl15_112 | ~ spl15_1 | ~ spl15_100), inference(avatar_split_clause, [], [f299, f858, f312, f916])).
fof(f299, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3))))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3)) | ~ (e2 = op(e3, op(e3, op(e3, e3))))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e1 = op(e3, op(e3, e3))) & (e0 = op(e3, e3)) & (e2 = op(e3, op(e3, op(e3, e3))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax18)).
fof(f914, plain, (~ spl15_108 | ~ spl15_64 | ~ spl15_111), inference(avatar_split_clause, [], [f298, f911, f579, f897])).
fof(f298, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e3) | ~ (e1 = op(e0, op(e0, op(e0, e0))))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e2 = op(e0, op(e0, e0))) | ~ (op(e0, e0) = e3) | ~ (e1 = op(e0, op(e0, op(e0, e0))))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e2 = op(e0, op(e0, e0))) & (op(e0, e0) = e3) & (e1 = op(e0, op(e0, op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax17)).
fof(f909, plain, (~ spl15_106 | ~ spl15_24 | ~ spl15_110), inference(avatar_split_clause, [], [f297, f906, f409, f887])).
fof(f297, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(e2, op(e2, op(e2, e2))))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e0 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(e2, op(e2, op(e2, e2))))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e0 = op(e2, op(e2, e2))) & (e3 = op(e2, e2)) & (e1 = op(e2, op(e2, op(e2, e2))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax16)).
fof(f895, plain, (~ spl15_105 | ~ spl15_3 | ~ spl15_107), inference(avatar_split_clause, [], [f295, f892, f320, f882])).
fof(f295, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3))))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e0 = op(e3, op(e3, e3))) | ~ (e2 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3))))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e0 = op(e3, op(e3, e3))) & (e2 = op(e3, e3)) & (e1 = op(e3, op(e3, op(e3, e3))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax14)).
fof(f890, plain, (~ spl15_106 | ~ spl15_21 | ~ spl15_99), inference(avatar_split_clause, [], [f294, f853, f397, f887])).
fof(f294, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(e2, op(e2, op(e2, e2))))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(e2, op(e2, op(e2, e2))))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e3 = op(e2, op(e2, e2))) & (e0 = op(e2, e2)) & (e1 = op(e2, op(e2, op(e2, e2))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax13)).
fof(f885, plain, (~ spl15_105 | ~ spl15_1 | ~ spl15_97), inference(avatar_split_clause, [], [f293, f844, f312, f882])).
fof(f293, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3))))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e0 = op(e3, e3)) | ~ (e1 = op(e3, op(e3, op(e3, e3))))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e2 = op(e3, op(e3, e3))) & (e0 = op(e3, e3)) & (e1 = op(e3, op(e3, op(e3, e3))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax12)).
fof(f880, plain, (~ spl15_101 | ~ spl15_44 | ~ spl15_104), inference(avatar_split_clause, [], [f292, f877, f494, f863])).
fof(f292, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, op(e1, e1))))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e2 = op(e1, op(e1, e1))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, op(e1, e1))))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e2 = op(e1, op(e1, e1))) & (e3 = op(e1, e1)) & (e0 = op(e1, op(e1, op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax11)).
fof(f875, plain, (~ spl15_98 | ~ spl15_24 | ~ spl15_103), inference(avatar_split_clause, [], [f291, f872, f409, f849])).
fof(f291, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, op(e2, e2))))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e1 = op(e2, op(e2, e2))) | ~ (e3 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, op(e2, e2))))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e1 = op(e2, op(e2, e2))) & (e3 = op(e2, e2)) & (e0 = op(e2, op(e2, op(e2, e2))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax10)).
fof(f870, plain, (~ spl15_101 | ~ spl15_43 | ~ spl15_102), inference(avatar_split_clause, [], [f290, f867, f490, f863])).
fof(f290, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, op(e1, e1))))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e3 = op(e1, op(e1, e1))) | ~ (e2 = op(e1, e1)) | ~ (e0 = op(e1, op(e1, op(e1, e1))))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e3 = op(e1, op(e1, e1))) & (e2 = op(e1, e1)) & (e0 = op(e1, op(e1, op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax9)).
fof(f861, plain, (~ spl15_96 | ~ spl15_3 | ~ spl15_100), inference(avatar_split_clause, [], [f289, f858, f320, f840])).
fof(f289, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e2 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3))))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e1 = op(e3, op(e3, e3))) | ~ (e2 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3))))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e1 = op(e3, op(e3, e3))) & (e2 = op(e3, e3)) & (e0 = op(e3, op(e3, op(e3, e3))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax8)).
fof(f856, plain, (~ spl15_98 | ~ spl15_22 | ~ spl15_99), inference(avatar_split_clause, [], [f288, f853, f401, f849])).
fof(f288, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e1 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, op(e2, e2))))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e3 = op(e2, op(e2, e2))) | ~ (e1 = op(e2, e2)) | ~ (e0 = op(e2, op(e2, op(e2, e2))))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e3 = op(e2, op(e2, e2))) & (e1 = op(e2, e2)) & (e0 = op(e2, op(e2, op(e2, e2))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax7)).
fof(f847, plain, (~ spl15_96 | ~ spl15_2 | ~ spl15_97), inference(avatar_split_clause, [], [f287, f844, f316, f840])).
fof(f287, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3))))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(e3, op(e3, e3))) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(e3, op(e3, op(e3, e3))))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(e3, op(e3, e3))) & (e1 = op(e3, e3)) & (e0 = op(e3, op(e3, op(e3, e3))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax6)).
fof(f838, plain, (spl15_92 | spl15_93 | spl15_94 | spl15_95), inference(avatar_split_clause, [], [f277, f835, f831, f827, f823])).
fof(f277, plain, ((op(e3, e0) = op(op(e3, e0), e3)) | (op(e2, e0) = op(op(e2, e0), e2)) | (op(e1, e0) = op(op(e1, e0), e1)) | (op(e0, e0) = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((op(e3, e3) = op(op(e3, e3), e3)) | (op(e2, e3) = op(op(e2, e3), e2)) | (op(e1, e3) = op(op(e1, e3), e1)) | (op(e0, e3) = op(op(e0, e3), e0))) & ((op(e3, e2) = op(op(e3, e2), e3)) | (op(e2, e2) = op(op(e2, e2), e2)) | (op(e1, e2) = op(op(e1, e2), e1)) | (op(e0, e2) = op(op(e0, e2), e0))) & ((op(e3, e1) = op(op(e3, e1), e3)) | (op(e2, e1) = op(op(e2, e1), e2)) | (op(e1, e1) = op(op(e1, e1), e1)) | (op(e0, e1) = op(op(e0, e1), e0))) & ((op(e3, e0) = op(op(e3, e0), e3)) | (op(e2, e0) = op(op(e2, e0), e2)) | (op(e1, e0) = op(op(e1, e0), e1)) | (op(e0, e0) = op(op(e0, e0), e0)))), inference(definition_folding, [], [f5, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f5, plain, (((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0))) | (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0))) | (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0))) | (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((op(e3, e3) = op(op(e3, e3), e3)) | (op(e2, e3) = op(op(e2, e3), e2)) | (op(e1, e3) = op(op(e1, e3), e1)) | (op(e0, e3) = op(op(e0, e3), e0))) & ((op(e3, e2) = op(op(e3, e2), e3)) | (op(e2, e2) = op(op(e2, e2), e2)) | (op(e1, e2) = op(op(e1, e2), e1)) | (op(e0, e2) = op(op(e0, e2), e0))) & ((op(e3, e1) = op(op(e3, e1), e3)) | (op(e2, e1) = op(op(e2, e1), e2)) | (op(e1, e1) = op(op(e1, e1), e1)) | (op(e0, e1) = op(op(e0, e1), e0))) & ((op(e3, e0) = op(op(e3, e0), e3)) | (op(e2, e0) = op(op(e2, e0), e2)) | (op(e1, e0) = op(op(e1, e0), e1)) | (op(e0, e0) = op(op(e0, e0), e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax5)).
fof(f821, plain, (spl15_88 | spl15_89 | spl15_90 | spl15_91), inference(avatar_split_clause, [], [f278, f818, f814, f810, f806])).
fof(f278, plain, ((op(e3, e1) = op(op(e3, e1), e3)) | (op(e2, e1) = op(op(e2, e1), e2)) | (op(e1, e1) = op(op(e1, e1), e1)) | (op(e0, e1) = op(op(e0, e1), e0))), inference(cnf_transformation, [], [f69])).
fof(f804, plain, (spl15_84 | spl15_85 | spl15_86 | spl15_87), inference(avatar_split_clause, [], [f279, f801, f797, f793, f789])).
fof(f279, plain, ((op(e3, e2) = op(op(e3, e2), e3)) | (op(e2, e2) = op(op(e2, e2), e2)) | (op(e1, e2) = op(op(e1, e2), e1)) | (op(e0, e2) = op(op(e0, e2), e0))), inference(cnf_transformation, [], [f69])).
fof(f787, plain, (spl15_80 | spl15_81 | spl15_82 | spl15_83), inference(avatar_split_clause, [], [f280, f784, f780, f776, f772])).
fof(f280, plain, ((op(e3, e3) = op(op(e3, e3), e3)) | (op(e2, e3) = op(op(e2, e3), e2)) | (op(e1, e3) = op(op(e1, e3), e1)) | (op(e0, e3) = op(op(e0, e3), e0))), inference(cnf_transformation, [], [f69])).
fof(f770, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | spl15_4), inference(avatar_split_clause, [], [f281, f324, f616, f626, f636, f646, f656, f666, f676, f686, f696, f706, f716, f726, f736, f746, f756])).
fof(f756, plain, (spl15_79 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl15_79])])).
fof(f746, plain, (spl15_78 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl15_78])])).
fof(f736, plain, (spl15_77 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl15_77])])).
fof(f726, plain, (spl15_76 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl15_76])])).
fof(f716, plain, (spl15_75 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_75])])).
fof(f706, plain, (spl15_74 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_74])])).
fof(f696, plain, (spl15_73 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_73])])).
fof(f686, plain, (spl15_72 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_72])])).
fof(f676, plain, (spl15_71 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_71])])).
fof(f666, plain, (spl15_70 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl15_70])])).
fof(f656, plain, (spl15_69 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl15_69])])).
fof(f646, plain, (spl15_68 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl15_68])])).
fof(f636, plain, (spl15_67 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl15_67])])).
fof(f626, plain, (spl15_66 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl15_66])])).
fof(f616, plain, (spl15_65 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl15_65])])).
fof(f281, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f769, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_4), inference(avatar_split_clause, [], [f282, f324, f616, f626, f636, f646, f656, f666, f676, f686, f696, f706, f716, f726, f736, f746, f756])).
fof(f282, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f768, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_13), inference(avatar_split_clause, [], [f283, f363, f616, f626, f636, f646, f656, f666, f676, f686, f696, f706, f716, f726, f736, f746, f756])).
fof(f283, plain, (~ (e0 = op(e3, e0)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f767, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_10), inference(avatar_split_clause, [], [f284, f350, f616, f626, f636, f646, f656, f666, f676, f686, f696, f706, f716, f726, f736, f746, f756])).
fof(f284, plain, (~ (e1 = op(e3, e1)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f764, plain, (~ spl15_79 | spl15_61), inference(avatar_split_clause, [], [f271, f567, f756])).
fof(f271, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f763, plain, (~ spl15_79 | ~ spl15_61), inference(avatar_split_clause, [], [f272, f567, f756])).
fof(f272, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f754, plain, (~ spl15_78 | spl15_57), inference(avatar_split_clause, [], [f265, f550, f746])).
fof(f265, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (op(e0, e0) = e1) & (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f753, plain, (~ spl15_78 | ~ spl15_62), inference(avatar_split_clause, [], [f266, f571, f746])).
fof(f266, plain, (~ (op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f752, plain, (~ spl15_78 | ~ spl15_45), inference(avatar_split_clause, [], [f267, f499, f746])).
fof(f267, plain, (~ (e0 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f751, plain, (~ spl15_78 | ~ spl15_42), inference(avatar_split_clause, [], [f268, f486, f746])).
fof(f268, plain, (~ (e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f750, plain, (~ spl15_78 | ~ spl15_39), inference(avatar_split_clause, [], [f269, f473, f746])).
fof(f269, plain, (~ (e2 = op(e1, e2)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f749, plain, (~ spl15_78 | ~ spl15_36), inference(avatar_split_clause, [], [f270, f460, f746])).
fof(f270, plain, (~ (e3 = op(e1, e3)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f744, plain, (~ spl15_77 | spl15_53), inference(avatar_split_clause, [], [f259, f533, f736])).
fof(f259, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (op(e0, e0) = e2) & (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f743, plain, (~ spl15_77 | ~ spl15_63), inference(avatar_split_clause, [], [f260, f575, f736])).
fof(f260, plain, (~ (op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f742, plain, (~ spl15_77 | ~ spl15_29), inference(avatar_split_clause, [], [f261, f431, f736])).
fof(f261, plain, (~ (e0 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f741, plain, (~ spl15_77 | ~ spl15_26), inference(avatar_split_clause, [], [f262, f418, f736])).
fof(f262, plain, (~ (e1 = op(e2, e1)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f740, plain, (~ spl15_77 | ~ spl15_23), inference(avatar_split_clause, [], [f263, f405, f736])).
fof(f263, plain, (~ (e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f739, plain, (~ spl15_77 | ~ spl15_20), inference(avatar_split_clause, [], [f264, f392, f736])).
fof(f264, plain, (~ (e3 = op(e2, e3)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f734, plain, (~ spl15_76 | spl15_49), inference(avatar_split_clause, [], [f253, f516, f726])).
fof(f253, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (op(e0, e0) = e3) & (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f733, plain, (~ spl15_76 | ~ spl15_64), inference(avatar_split_clause, [], [f254, f579, f726])).
fof(f254, plain, (~ (op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f732, plain, (~ spl15_76 | ~ spl15_13), inference(avatar_split_clause, [], [f255, f363, f726])).
fof(f255, plain, (~ (e0 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f731, plain, (~ spl15_76 | ~ spl15_10), inference(avatar_split_clause, [], [f256, f350, f726])).
fof(f256, plain, (~ (e1 = op(e3, e1)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f730, plain, (~ spl15_76 | ~ spl15_7), inference(avatar_split_clause, [], [f257, f337, f726])).
fof(f257, plain, (~ (e2 = op(e3, e2)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f729, plain, (~ spl15_76 | ~ spl15_4), inference(avatar_split_clause, [], [f258, f324, f726])).
fof(f258, plain, (~ (e3 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f724, plain, (~ spl15_75 | spl15_46), inference(avatar_split_clause, [], [f247, f503, f716])).
fof(f247, plain, ((e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e1, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f723, plain, (~ spl15_75 | ~ spl15_41), inference(avatar_split_clause, [], [f248, f482, f716])).
fof(f248, plain, (~ (e0 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f722, plain, (~ spl15_75 | ~ spl15_61), inference(avatar_split_clause, [], [f249, f567, f716])).
fof(f249, plain, (~ (e0 = op(e0, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f721, plain, (~ spl15_75 | ~ spl15_58), inference(avatar_split_clause, [], [f250, f554, f716])).
fof(f250, plain, (~ (e1 = op(e0, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f720, plain, (~ spl15_75 | ~ spl15_55), inference(avatar_split_clause, [], [f251, f541, f716])).
fof(f251, plain, (~ (e2 = op(e0, e2)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f719, plain, (~ spl15_75 | ~ spl15_52), inference(avatar_split_clause, [], [f252, f528, f716])).
fof(f252, plain, (~ (e3 = op(e0, e3)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f714, plain, (~ spl15_74 | spl15_42), inference(avatar_split_clause, [], [f241, f486, f706])).
fof(f241, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f713, plain, (~ spl15_74 | ~ spl15_42), inference(avatar_split_clause, [], [f242, f486, f706])).
fof(f242, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f712, plain, (~ spl15_74 | ~ spl15_45), inference(avatar_split_clause, [], [f243, f499, f706])).
fof(f243, plain, (~ (e0 = op(e1, e0)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f704, plain, (~ spl15_73 | spl15_38), inference(avatar_split_clause, [], [f235, f469, f696])).
fof(f235, plain, ((e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e1, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f703, plain, (~ spl15_73 | ~ spl15_43), inference(avatar_split_clause, [], [f236, f490, f696])).
fof(f236, plain, (~ (e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f702, plain, (~ spl15_73 | ~ spl15_29), inference(avatar_split_clause, [], [f237, f431, f696])).
fof(f237, plain, (~ (e0 = op(e2, e0)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f701, plain, (~ spl15_73 | ~ spl15_26), inference(avatar_split_clause, [], [f238, f418, f696])).
fof(f238, plain, (~ (e1 = op(e2, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f700, plain, (~ spl15_73 | ~ spl15_23), inference(avatar_split_clause, [], [f239, f405, f696])).
fof(f239, plain, (~ (e2 = op(e2, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f699, plain, (~ spl15_73 | ~ spl15_20), inference(avatar_split_clause, [], [f240, f392, f696])).
fof(f240, plain, (~ (e3 = op(e2, e3)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f694, plain, (~ spl15_72 | spl15_34), inference(avatar_split_clause, [], [f229, f452, f686])).
fof(f229, plain, ((e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e1, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f693, plain, (~ spl15_72 | ~ spl15_44), inference(avatar_split_clause, [], [f230, f494, f686])).
fof(f230, plain, (~ (e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f692, plain, (~ spl15_72 | ~ spl15_13), inference(avatar_split_clause, [], [f231, f363, f686])).
fof(f231, plain, (~ (e0 = op(e3, e0)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f691, plain, (~ spl15_72 | ~ spl15_10), inference(avatar_split_clause, [], [f232, f350, f686])).
fof(f232, plain, (~ (e1 = op(e3, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f690, plain, (~ spl15_72 | ~ spl15_7), inference(avatar_split_clause, [], [f233, f337, f686])).
fof(f233, plain, (~ (e2 = op(e3, e2)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f689, plain, (~ spl15_72 | ~ spl15_4), inference(avatar_split_clause, [], [f234, f324, f686])).
fof(f234, plain, (~ (e3 = op(e3, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f684, plain, (~ spl15_71 | spl15_31), inference(avatar_split_clause, [], [f223, f439, f676])).
fof(f223, plain, ((e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e2, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f683, plain, (~ spl15_71 | ~ spl15_21), inference(avatar_split_clause, [], [f224, f397, f676])).
fof(f224, plain, (~ (e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f682, plain, (~ spl15_71 | ~ spl15_61), inference(avatar_split_clause, [], [f225, f567, f676])).
fof(f225, plain, (~ (e0 = op(e0, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f681, plain, (~ spl15_71 | ~ spl15_58), inference(avatar_split_clause, [], [f226, f554, f676])).
fof(f226, plain, (~ (e1 = op(e0, e1)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f680, plain, (~ spl15_71 | ~ spl15_55), inference(avatar_split_clause, [], [f227, f541, f676])).
fof(f227, plain, (~ (e2 = op(e0, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f679, plain, (~ spl15_71 | ~ spl15_52), inference(avatar_split_clause, [], [f228, f528, f676])).
fof(f228, plain, (~ (e3 = op(e0, e3)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f674, plain, (~ spl15_70 | spl15_27), inference(avatar_split_clause, [], [f217, f422, f666])).
fof(f217, plain, ((e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e2, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f673, plain, (~ spl15_70 | ~ spl15_22), inference(avatar_split_clause, [], [f218, f401, f666])).
fof(f218, plain, (~ (e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f672, plain, (~ spl15_70 | ~ spl15_45), inference(avatar_split_clause, [], [f219, f499, f666])).
fof(f219, plain, (~ (e0 = op(e1, e0)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f671, plain, (~ spl15_70 | ~ spl15_42), inference(avatar_split_clause, [], [f220, f486, f666])).
fof(f220, plain, (~ (e1 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f670, plain, (~ spl15_70 | ~ spl15_39), inference(avatar_split_clause, [], [f221, f473, f666])).
fof(f221, plain, (~ (e2 = op(e1, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f669, plain, (~ spl15_70 | ~ spl15_36), inference(avatar_split_clause, [], [f222, f460, f666])).
fof(f222, plain, (~ (e3 = op(e1, e3)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f664, plain, (~ spl15_69 | spl15_23), inference(avatar_split_clause, [], [f211, f405, f656])).
fof(f211, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f663, plain, (~ spl15_69 | ~ spl15_23), inference(avatar_split_clause, [], [f212, f405, f656])).
fof(f212, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f654, plain, (~ spl15_68 | spl15_19), inference(avatar_split_clause, [], [f205, f388, f646])).
fof(f205, plain, ((e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & ~ (e3 = op(e2, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f653, plain, (~ spl15_68 | ~ spl15_24), inference(avatar_split_clause, [], [f206, f409, f646])).
fof(f206, plain, (~ (e3 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f652, plain, (~ spl15_68 | ~ spl15_13), inference(avatar_split_clause, [], [f207, f363, f646])).
fof(f207, plain, (~ (e0 = op(e3, e0)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f651, plain, (~ spl15_68 | ~ spl15_10), inference(avatar_split_clause, [], [f208, f350, f646])).
fof(f208, plain, (~ (e1 = op(e3, e1)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f650, plain, (~ spl15_68 | ~ spl15_7), inference(avatar_split_clause, [], [f209, f337, f646])).
fof(f209, plain, (~ (e2 = op(e3, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f649, plain, (~ spl15_68 | ~ spl15_4), inference(avatar_split_clause, [], [f210, f324, f646])).
fof(f210, plain, (~ (e3 = op(e3, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f644, plain, (~ spl15_67 | spl15_16), inference(avatar_split_clause, [], [f199, f375, f636])).
fof(f199, plain, ((e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & ~ (e0 = op(e3, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f643, plain, (~ spl15_67 | ~ spl15_1), inference(avatar_split_clause, [], [f200, f312, f636])).
fof(f200, plain, (~ (e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f642, plain, (~ spl15_67 | ~ spl15_61), inference(avatar_split_clause, [], [f201, f567, f636])).
fof(f201, plain, (~ (e0 = op(e0, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f641, plain, (~ spl15_67 | ~ spl15_58), inference(avatar_split_clause, [], [f202, f554, f636])).
fof(f202, plain, (~ (e1 = op(e0, e1)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f640, plain, (~ spl15_67 | ~ spl15_55), inference(avatar_split_clause, [], [f203, f541, f636])).
fof(f203, plain, (~ (e2 = op(e0, e2)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f639, plain, (~ spl15_67 | ~ spl15_52), inference(avatar_split_clause, [], [f204, f528, f636])).
fof(f204, plain, (~ (e3 = op(e0, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f634, plain, (~ spl15_66 | spl15_12), inference(avatar_split_clause, [], [f193, f358, f626])).
fof(f193, plain, ((e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & ~ (e1 = op(e3, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f633, plain, (~ spl15_66 | ~ spl15_2), inference(avatar_split_clause, [], [f194, f316, f626])).
fof(f194, plain, (~ (e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f632, plain, (~ spl15_66 | ~ spl15_45), inference(avatar_split_clause, [], [f195, f499, f626])).
fof(f195, plain, (~ (e0 = op(e1, e0)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f631, plain, (~ spl15_66 | ~ spl15_42), inference(avatar_split_clause, [], [f196, f486, f626])).
fof(f196, plain, (~ (e1 = op(e1, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f630, plain, (~ spl15_66 | ~ spl15_39), inference(avatar_split_clause, [], [f197, f473, f626])).
fof(f197, plain, (~ (e2 = op(e1, e2)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f629, plain, (~ spl15_66 | ~ spl15_36), inference(avatar_split_clause, [], [f198, f460, f626])).
fof(f198, plain, (~ (e3 = op(e1, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f624, plain, (~ spl15_65 | spl15_8), inference(avatar_split_clause, [], [f187, f341, f616])).
fof(f187, plain, ((e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & ~ (e2 = op(e3, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f623, plain, (~ spl15_65 | ~ spl15_3), inference(avatar_split_clause, [], [f188, f320, f616])).
fof(f188, plain, (~ (e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f622, plain, (~ spl15_65 | ~ spl15_29), inference(avatar_split_clause, [], [f189, f431, f616])).
fof(f189, plain, (~ (e0 = op(e2, e0)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f621, plain, (~ spl15_65 | ~ spl15_26), inference(avatar_split_clause, [], [f190, f418, f616])).
fof(f190, plain, (~ (e1 = op(e2, e1)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f620, plain, (~ spl15_65 | ~ spl15_23), inference(avatar_split_clause, [], [f191, f405, f616])).
fof(f191, plain, (~ (e2 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f619, plain, (~ spl15_65 | ~ spl15_20), inference(avatar_split_clause, [], [f192, f392, f616])).
fof(f192, plain, (~ (e3 = op(e2, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f614, plain, (spl15_61 | spl15_57 | spl15_53 | spl15_49), inference(avatar_split_clause, [], [f101, f516, f533, f550, f567])).
fof(f101, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax2)).
fof(f613, plain, (spl15_61 | spl15_45 | spl15_29 | spl15_13), inference(avatar_split_clause, [], [f102, f363, f431, f499, f567])).
fof(f102, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f612, plain, (spl15_62 | spl15_58 | spl15_54 | spl15_50), inference(avatar_split_clause, [], [f103, f520, f537, f554, f571])).
fof(f103, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f611, plain, (spl15_62 | spl15_46 | spl15_30 | spl15_14), inference(avatar_split_clause, [], [f104, f367, f435, f503, f571])).
fof(f104, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f610, plain, (spl15_63 | spl15_59 | spl15_55 | spl15_51), inference(avatar_split_clause, [], [f105, f524, f541, f558, f575])).
fof(f105, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f609, plain, (spl15_63 | spl15_47 | spl15_31 | spl15_15), inference(avatar_split_clause, [], [f106, f371, f439, f507, f575])).
fof(f106, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f608, plain, (spl15_64 | spl15_60 | spl15_56 | spl15_52), inference(avatar_split_clause, [], [f107, f528, f545, f562, f579])).
fof(f107, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f607, plain, (spl15_64 | spl15_48 | spl15_32 | spl15_16), inference(avatar_split_clause, [], [f108, f375, f443, f511, f579])).
fof(f108, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f606, plain, (spl15_45 | spl15_41 | spl15_37 | spl15_33), inference(avatar_split_clause, [], [f109, f448, f465, f482, f499])).
fof(f109, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f605, plain, (spl15_57 | spl15_41 | spl15_25 | spl15_9), inference(avatar_split_clause, [], [f110, f346, f414, f482, f550])).
fof(f110, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f604, plain, (spl15_46 | spl15_42 | spl15_38 | spl15_34), inference(avatar_split_clause, [], [f111, f452, f469, f486, f503])).
fof(f111, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f603, plain, (spl15_58 | spl15_42 | spl15_26 | spl15_10), inference(avatar_split_clause, [], [f112, f350, f418, f486, f554])).
fof(f112, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f602, plain, (spl15_47 | spl15_43 | spl15_39 | spl15_35), inference(avatar_split_clause, [], [f113, f456, f473, f490, f507])).
fof(f113, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f601, plain, (spl15_59 | spl15_43 | spl15_27 | spl15_11), inference(avatar_split_clause, [], [f114, f354, f422, f490, f558])).
fof(f114, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f600, plain, (spl15_48 | spl15_44 | spl15_40 | spl15_36), inference(avatar_split_clause, [], [f115, f460, f477, f494, f511])).
fof(f115, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f599, plain, (spl15_60 | spl15_44 | spl15_28 | spl15_12), inference(avatar_split_clause, [], [f116, f358, f426, f494, f562])).
fof(f116, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f598, plain, (spl15_29 | spl15_25 | spl15_21 | spl15_17), inference(avatar_split_clause, [], [f117, f380, f397, f414, f431])).
fof(f117, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f597, plain, (spl15_53 | spl15_37 | spl15_21 | spl15_5), inference(avatar_split_clause, [], [f118, f329, f397, f465, f533])).
fof(f118, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f596, plain, (spl15_30 | spl15_26 | spl15_22 | spl15_18), inference(avatar_split_clause, [], [f119, f384, f401, f418, f435])).
fof(f119, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f595, plain, (spl15_54 | spl15_38 | spl15_22 | spl15_6), inference(avatar_split_clause, [], [f120, f333, f401, f469, f537])).
fof(f120, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f594, plain, (spl15_31 | spl15_27 | spl15_23 | spl15_19), inference(avatar_split_clause, [], [f121, f388, f405, f422, f439])).
fof(f121, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f593, plain, (spl15_55 | spl15_39 | spl15_23 | spl15_7), inference(avatar_split_clause, [], [f122, f337, f405, f473, f541])).
fof(f122, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f592, plain, (spl15_32 | spl15_28 | spl15_24 | spl15_20), inference(avatar_split_clause, [], [f123, f392, f409, f426, f443])).
fof(f123, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f591, plain, (spl15_56 | spl15_40 | spl15_24 | spl15_8), inference(avatar_split_clause, [], [f124, f341, f409, f477, f545])).
fof(f124, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f590, plain, (spl15_13 | spl15_9 | spl15_5 | spl15_1), inference(avatar_split_clause, [], [f125, f312, f329, f346, f363])).
fof(f125, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f589, plain, (spl15_49 | spl15_33 | spl15_17 | spl15_1), inference(avatar_split_clause, [], [f126, f312, f380, f448, f516])).
fof(f126, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f588, plain, (spl15_14 | spl15_10 | spl15_6 | spl15_2), inference(avatar_split_clause, [], [f127, f316, f333, f350, f367])).
fof(f127, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f587, plain, (spl15_50 | spl15_34 | spl15_18 | spl15_2), inference(avatar_split_clause, [], [f128, f316, f384, f452, f520])).
fof(f128, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f586, plain, (spl15_15 | spl15_11 | spl15_7 | spl15_3), inference(avatar_split_clause, [], [f129, f320, f337, f354, f371])).
fof(f129, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f585, plain, (spl15_51 | spl15_35 | spl15_19 | spl15_3), inference(avatar_split_clause, [], [f130, f320, f388, f456, f524])).
fof(f130, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f584, plain, (spl15_16 | spl15_12 | spl15_8 | spl15_4), inference(avatar_split_clause, [], [f131, f324, f341, f358, f375])).
fof(f131, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f583, plain, (spl15_52 | spl15_36 | spl15_20 | spl15_4), inference(avatar_split_clause, [], [f132, f324, f392, f460, f528])).
fof(f132, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f582, plain, (spl15_61 | spl15_62 | spl15_63 | spl15_64), inference(avatar_split_clause, [], [f85, f579, f575, f571, f567])).
fof(f85, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG147+1.p', ax1)).
fof(f565, plain, (spl15_57 | spl15_58 | spl15_59 | spl15_60), inference(avatar_split_clause, [], [f86, f562, f558, f554, f550])).
fof(f86, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f548, plain, (spl15_53 | spl15_54 | spl15_55 | spl15_56), inference(avatar_split_clause, [], [f87, f545, f541, f537, f533])).
fof(f87, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f531, plain, (spl15_49 | spl15_50 | spl15_51 | spl15_52), inference(avatar_split_clause, [], [f88, f528, f524, f520, f516])).
fof(f88, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f514, plain, (spl15_45 | spl15_46 | spl15_47 | spl15_48), inference(avatar_split_clause, [], [f89, f511, f507, f503, f499])).
fof(f89, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f497, plain, (spl15_41 | spl15_42 | spl15_43 | spl15_44), inference(avatar_split_clause, [], [f90, f494, f490, f486, f482])).
fof(f90, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f480, plain, (spl15_37 | spl15_38 | spl15_39 | spl15_40), inference(avatar_split_clause, [], [f91, f477, f473, f469, f465])).
fof(f91, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f463, plain, (spl15_33 | spl15_34 | spl15_35 | spl15_36), inference(avatar_split_clause, [], [f92, f460, f456, f452, f448])).
fof(f92, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f446, plain, (spl15_29 | spl15_30 | spl15_31 | spl15_32), inference(avatar_split_clause, [], [f93, f443, f439, f435, f431])).
fof(f93, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f429, plain, (spl15_25 | spl15_26 | spl15_27 | spl15_28), inference(avatar_split_clause, [], [f94, f426, f422, f418, f414])).
fof(f94, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f412, plain, (spl15_21 | spl15_22 | spl15_23 | spl15_24), inference(avatar_split_clause, [], [f95, f409, f405, f401, f397])).
fof(f95, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f395, plain, (spl15_17 | spl15_18 | spl15_19 | spl15_20), inference(avatar_split_clause, [], [f96, f392, f388, f384, f380])).
fof(f96, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f378, plain, (spl15_13 | spl15_14 | spl15_15 | spl15_16), inference(avatar_split_clause, [], [f97, f375, f371, f367, f363])).
fof(f97, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f361, plain, (spl15_9 | spl15_10 | spl15_11 | spl15_12), inference(avatar_split_clause, [], [f98, f358, f354, f350, f346])).
fof(f98, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f344, plain, (spl15_5 | spl15_6 | spl15_7 | spl15_8), inference(avatar_split_clause, [], [f99, f341, f337, f333, f329])).
fof(f99, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f327, plain, (spl15_1 | spl15_2 | spl15_3 | spl15_4), inference(avatar_split_clause, [], [f100, f324, f320, f316, f312])).
fof(f100, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).