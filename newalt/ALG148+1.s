fof(f2634, plain, $false, inference(avatar_sat_refutation, [], [f340, f357, f374, f408, f425, f442, f459, f476, f493, f510, f527, f544, f561, f578, f595, f596, f597, f598, f600, f601, f602, f603, f605, f608, f609, f610, f611, f613, f614, f615, f618, f619, f620, f621, f622, f623, f624, f625, f627, f632, f633, f634, f636, f641, f643, f644, f645, f650, f653, f654, f660, f661, f662, f663, f669, f672, f678, f679, f681, f687, f689, f690, f695, f697, f698, f699, f704, f706, f707, f708, f715, f717, f724, f725, f726, f731, f732, f733, f734, f735, f741, f743, f744, f749, f750, f751, f752, f753, f761, f762, f763, f764, f765, f766, f767, f768, f769, f770, f771, f772, f773, f774, f775, f776, f779, f780, f789, f807, f825, f839, f849, f863, f872, f873, f879, f894, f896, f897, f899, f914, f916, f931, f934, f939, f947, f948, f949, f957, f964, f966, f970, f973, f982, f988, f996, f999, f1003, f1009, f1011, f1014, f1017, f1020, f1026, f1029, f1078, f1079, f1090, f1107, f1110, f1113, f1118, f1127, f1140, f1151, f1155, f1165, f1171, f1179, f1187, f1202, f1235, f1243, f1251, f1255, f1269, f1278, f1282, f1314, f1339, f1343, f1362, f1367, f1376, f1377, f1398, f1414, f1421, f1424, f1442, f1455, f1460, f1464, f1490, f1511, f1516, f1519, f1530, f1532, f1539, f1567, f1568, f1591, f1594, f1599, f1607, f1609, f1623, f1635, f1640, f1650, f1654, f1655, f1661, f1662, f1677, f1685, f1690, f1722, f1725, f1728, f1734, f1736, f1752, f1767, f1768, f1782, f1784, f1805, f1817, f1828, f1829, f1840, f1844, f1853, f1888, f1891, f1897, f1918, f1928, f1933, f1937, f1957, f1959, f1967, f2001, f2005, f2009, f2014, f2022, f2037, f2043, f2051, f2060, f2061, f2067, f2068, f2075, f2076, f2084, f2094, f2104, f2107, f2112, f2134, f2139, f2141, f2150, f2151, f2163, f2173, f2177, f2183, f2188, f2196, f2203, f2215, f2227, f2247, f2256, f2275, f2282, f2289, f2293, f2299, f2302, f2307, f2313, f2321, f2323, f2328, f2335, f2340, f2347, f2348, f2370, f2387, f2388, f2412, f2421, f2429, f2442, f2450, f2454, f2461, f2466, f2470, f2477, f2498, f2501, f2503, f2506, f2520, f2521, f2525, f2528, f2532, f2539, f2544, f2568, f2572, f2574, f2577, f2588, f2597, f2598, f2611, f2620])).
fof(f2620, plain, (~ spl15_6 | ~ spl15_14), inference(avatar_split_clause, [], [f2614, f380, f346])).
fof(f346, plain, (spl15_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_6])])).
fof(f380, plain, (spl15_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f2614, plain, (~ (e1 = op(e3, e2)) | ~ spl15_14), inference(backward_demodulation, [], [f176, f382])).
fof(f382, plain, ((e1 = op(e3, e0)) | ~ spl15_14), inference(avatar_component_clause, [], [f380])).
fof(f176, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax3)).
fof(f2611, plain, (~ spl15_20 | ~ spl15_24), inference(avatar_split_clause, [], [f2605, f422, f405])).
fof(f405, plain, (spl15_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f422, plain, (spl15_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_24])])).
fof(f2605, plain, (~ (e3 = op(e2, e3)) | ~ spl15_24), inference(backward_demodulation, [], [f174, f424])).
fof(f424, plain, ((e3 = op(e2, e2)) | ~ spl15_24), inference(avatar_component_clause, [], [f422])).
fof(f174, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2598, plain, (~ spl15_25 | ~ spl15_29), inference(avatar_split_clause, [], [f2591, f444, f427])).
fof(f427, plain, (spl15_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_25])])).
fof(f444, plain, (spl15_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_29])])).
fof(f2591, plain, (~ (e0 = op(e2, e1)) | ~ spl15_29), inference(backward_demodulation, [], [f169, f446])).
fof(f446, plain, ((e0 = op(e2, e0)) | ~ spl15_29), inference(avatar_component_clause, [], [f444])).
fof(f169, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2597, plain, (~ spl15_13 | ~ spl15_29), inference(avatar_split_clause, [], [f2590, f444, f376])).
fof(f376, plain, (spl15_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f2590, plain, (~ (e0 = op(e3, e0)) | ~ spl15_29), inference(backward_demodulation, [], [f138, f446])).
fof(f138, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2588, plain, (~ spl15_6 | ~ spl15_54), inference(avatar_split_clause, [], [f2586, f550, f346])).
fof(f550, plain, (spl15_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_54])])).
fof(f2586, plain, (~ (e1 = op(e3, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f147, f552])).
fof(f552, plain, ((e1 = op(e0, e2)) | ~ spl15_54), inference(avatar_component_clause, [], [f550])).
fof(f147, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2577, plain, (~ spl15_9 | ~ spl15_44 | spl15_102), inference(avatar_split_clause, [], [f2576, f886, f507, f359])).
fof(f359, plain, (spl15_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f507, plain, (spl15_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_44])])).
fof(f886, plain, (spl15_102 <=> (e0 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_102])])).
fof(f2576, plain, (~ (e0 = op(e3, e1)) | (~ spl15_44 | spl15_102)), inference(forward_demodulation, [], [f888, f509])).
fof(f509, plain, ((e3 = op(e1, e1)) | ~ spl15_44), inference(avatar_component_clause, [], [f507])).
fof(f888, plain, (~ (e0 = op(op(e1, e1), e1)) | spl15_102), inference(avatar_component_clause, [], [f886])).
fof(f2574, plain, (~ spl15_31 | ~ spl15_47), inference(avatar_split_clause, [], [f2573, f520, f452])).
fof(f452, plain, (spl15_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_31])])).
fof(f520, plain, (spl15_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_47])])).
fof(f2573, plain, (~ (e2 = op(e2, e0)) | ~ spl15_47), inference(forward_demodulation, [], [f136, f522])).
fof(f522, plain, ((e2 = op(e1, e0)) | ~ spl15_47), inference(avatar_component_clause, [], [f520])).
fof(f136, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2572, plain, (~ spl15_32 | ~ spl15_64), inference(avatar_split_clause, [], [f2571, f592, f456])).
fof(f456, plain, (spl15_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_32])])).
fof(f592, plain, (spl15_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl15_64])])).
fof(f2571, plain, (~ (e3 = op(e2, e0)) | ~ spl15_64), inference(forward_demodulation, [], [f134, f594])).
fof(f594, plain, ((op(e0, e0) = e3) | ~ spl15_64), inference(avatar_component_clause, [], [f592])).
fof(f134, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2568, plain, (~ spl15_11 | ~ spl15_25 | ~ spl15_44 | spl15_100), inference(avatar_contradiction_clause, [], [f2567])).
fof(f2567, plain, ($false | (~ spl15_11 | ~ spl15_25 | ~ spl15_44 | spl15_100)), inference(subsumption_resolution, [], [f2562, f429])).
fof(f429, plain, ((e0 = op(e2, e1)) | ~ spl15_25), inference(avatar_component_clause, [], [f427])).
fof(f2562, plain, (~ (e0 = op(e2, e1)) | (~ spl15_11 | ~ spl15_44 | spl15_100)), inference(backward_demodulation, [], [f2513, f369])).
fof(f369, plain, ((e2 = op(e3, e1)) | ~ spl15_11), inference(avatar_component_clause, [], [f367])).
fof(f367, plain, (spl15_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f2513, plain, (~ (e0 = op(op(e3, e1), e1)) | (~ spl15_44 | spl15_100)), inference(forward_demodulation, [], [f877, f509])).
fof(f877, plain, (~ (e0 = op(op(op(e1, e1), e1), e1)) | spl15_100), inference(avatar_component_clause, [], [f875])).
fof(f875, plain, (spl15_100 <=> (e0 = op(op(op(e1, e1), e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_100])])).
fof(f2544, plain, (~ spl15_19 | ~ spl15_23), inference(avatar_split_clause, [], [f2541, f418, f401])).
fof(f401, plain, (spl15_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_19])])).
fof(f418, plain, (spl15_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_23])])).
fof(f2541, plain, (~ (e2 = op(e2, e3)) | ~ spl15_23), inference(backward_demodulation, [], [f174, f420])).
fof(f420, plain, ((e2 = op(e2, e2)) | ~ spl15_23), inference(avatar_component_clause, [], [f418])).
fof(f2539, plain, (~ spl15_5 | ~ spl15_37), inference(avatar_split_clause, [], [f2535, f478, f342])).
fof(f342, plain, (spl15_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f478, plain, (spl15_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_37])])).
fof(f2535, plain, (~ (e0 = op(e3, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f149, f480])).
fof(f480, plain, ((e0 = op(e1, e2)) | ~ spl15_37), inference(avatar_component_clause, [], [f478])).
fof(f149, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2532, plain, (~ spl15_39 | ~ spl15_47), inference(avatar_split_clause, [], [f2529, f520, f486])).
fof(f486, plain, (spl15_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_39])])).
fof(f2529, plain, (~ (e2 = op(e1, e2)) | ~ spl15_47), inference(backward_demodulation, [], [f164, f522])).
fof(f164, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2528, plain, (~ spl15_49 | ~ spl15_51), inference(avatar_contradiction_clause, [], [f2527])).
fof(f2527, plain, ($false | (~ spl15_49 | ~ spl15_51)), inference(subsumption_resolution, [], [f2526, f182])).
fof(f182, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax4)).
fof(f2526, plain, ((e0 = e2) | (~ spl15_49 | ~ spl15_51)), inference(forward_demodulation, [], [f539, f531])).
fof(f531, plain, ((e0 = op(e0, e3)) | ~ spl15_49), inference(avatar_component_clause, [], [f529])).
fof(f529, plain, (spl15_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_49])])).
fof(f539, plain, ((e2 = op(e0, e3)) | ~ spl15_51), inference(avatar_component_clause, [], [f537])).
fof(f537, plain, (spl15_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_51])])).
fof(f2525, plain, (~ spl15_24 | ~ spl15_56), inference(avatar_split_clause, [], [f2523, f558, f422])).
fof(f558, plain, (spl15_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_56])])).
fof(f2523, plain, (~ (e3 = op(e2, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f146, f560])).
fof(f560, plain, ((e3 = op(e0, e2)) | ~ spl15_56), inference(avatar_component_clause, [], [f558])).
fof(f146, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2521, plain, (~ spl15_54 | ~ spl15_58), inference(avatar_split_clause, [], [f2517, f567, f550])).
fof(f567, plain, (spl15_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_58])])).
fof(f2517, plain, (~ (e1 = op(e0, e2)) | ~ spl15_58), inference(backward_demodulation, [], [f160, f569])).
fof(f569, plain, ((e1 = op(e0, e1)) | ~ spl15_58), inference(avatar_component_clause, [], [f567])).
fof(f160, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2520, plain, (~ spl15_10 | ~ spl15_58), inference(avatar_split_clause, [], [f2516, f567, f363])).
fof(f363, plain, (spl15_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f2516, plain, (~ (e1 = op(e3, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f141, f569])).
fof(f141, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2506, plain, (~ spl15_56 | ~ spl15_64), inference(avatar_split_clause, [], [f2415, f592, f558])).
fof(f2415, plain, (~ (e3 = op(e0, e2)) | ~ spl15_64), inference(backward_demodulation, [], [f158, f594])).
fof(f158, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2503, plain, (~ spl15_46 | ~ spl15_30), inference(avatar_split_clause, [], [f2400, f448, f516])).
fof(f516, plain, (spl15_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_46])])).
fof(f448, plain, (spl15_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_30])])).
fof(f2400, plain, (~ (e1 = op(e1, e0)) | ~ spl15_30), inference(forward_demodulation, [], [f136, f450])).
fof(f450, plain, ((e1 = op(e2, e0)) | ~ spl15_30), inference(avatar_component_clause, [], [f448])).
fof(f2501, plain, (~ spl15_20 | ~ spl15_4), inference(avatar_split_clause, [], [f2500, f337, f405])).
fof(f337, plain, (spl15_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f2500, plain, (~ (e3 = op(e2, e3)) | ~ spl15_4), inference(forward_demodulation, [], [f156, f339])).
fof(f339, plain, ((e3 = op(e3, e3)) | ~ spl15_4), inference(avatar_component_clause, [], [f337])).
fof(f156, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2498, plain, (~ spl15_11 | ~ spl15_44 | spl15_91), inference(avatar_split_clause, [], [f2448, f831, f507, f367])).
fof(f831, plain, (spl15_91 <=> (e2 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_91])])).
fof(f2448, plain, (~ (e2 = op(e3, e1)) | (~ spl15_44 | spl15_91)), inference(backward_demodulation, [], [f833, f509])).
fof(f833, plain, (~ (e2 = op(op(e1, e1), e1)) | spl15_91), inference(avatar_component_clause, [], [f831])).
fof(f2477, plain, (~ spl15_3 | ~ spl15_15), inference(avatar_split_clause, [], [f2473, f384, f333])).
fof(f333, plain, (spl15_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f384, plain, (spl15_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f2473, plain, (~ (e2 = op(e3, e3)) | ~ spl15_15), inference(backward_demodulation, [], [f177, f386])).
fof(f386, plain, ((e2 = op(e3, e0)) | ~ spl15_15), inference(avatar_component_clause, [], [f384])).
fof(f177, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2470, plain, (~ spl15_3 | ~ spl15_19), inference(avatar_split_clause, [], [f2469, f401, f333])).
fof(f2469, plain, (~ (e2 = op(e3, e3)) | ~ spl15_19), inference(backward_demodulation, [], [f156, f403])).
fof(f403, plain, ((e2 = op(e2, e3)) | ~ spl15_19), inference(avatar_component_clause, [], [f401])).
fof(f2466, plain, (~ spl15_8 | ~ spl15_24), inference(avatar_split_clause, [], [f2463, f422, f354])).
fof(f354, plain, (spl15_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f2463, plain, (~ (e3 = op(e3, e2)) | ~ spl15_24), inference(backward_demodulation, [], [f150, f424])).
fof(f150, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2461, plain, (~ spl15_9 | ~ spl15_25), inference(avatar_split_clause, [], [f2455, f427, f359])).
fof(f2455, plain, (~ (e0 = op(e3, e1)) | ~ spl15_25), inference(backward_demodulation, [], [f144, f429])).
fof(f144, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2454, plain, (~ spl15_23 | ~ spl15_39), inference(avatar_split_clause, [], [f2451, f486, f418])).
fof(f2451, plain, (~ (e2 = op(e2, e2)) | ~ spl15_39), inference(backward_demodulation, [], [f148, f488])).
fof(f488, plain, ((e2 = op(e1, e2)) | ~ spl15_39), inference(avatar_component_clause, [], [f486])).
fof(f148, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2450, plain, (~ spl15_28 | ~ spl15_44), inference(avatar_split_clause, [], [f2444, f507, f439])).
fof(f439, plain, (spl15_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_28])])).
fof(f2444, plain, (~ (e3 = op(e2, e1)) | ~ spl15_44), inference(backward_demodulation, [], [f142, f509])).
fof(f142, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2442, plain, (~ spl15_17 | ~ spl15_49), inference(avatar_split_clause, [], [f2437, f529, f393])).
fof(f393, plain, (spl15_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_17])])).
fof(f2437, plain, (~ (e0 = op(e2, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f152, f531])).
fof(f152, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2429, plain, (~ spl15_43 | ~ spl15_59), inference(avatar_split_clause, [], [f2423, f571, f503])).
fof(f503, plain, (spl15_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_43])])).
fof(f571, plain, (spl15_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_59])])).
fof(f2423, plain, (~ (e2 = op(e1, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f139, f573])).
fof(f573, plain, ((e2 = op(e0, e1)) | ~ spl15_59), inference(avatar_component_clause, [], [f571])).
fof(f139, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2421, plain, (~ spl15_52 | ~ spl15_64), inference(avatar_split_clause, [], [f2416, f592, f541])).
fof(f541, plain, (spl15_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_52])])).
fof(f2416, plain, (~ (e3 = op(e0, e3)) | ~ spl15_64), inference(backward_demodulation, [], [f159, f594])).
fof(f159, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2412, plain, (~ spl15_54 | spl15_82 | ~ spl15_97), inference(avatar_split_clause, [], [f2411, f860, f791, f550])).
fof(f791, plain, (spl15_82 <=> (e1 = op(op(op(e2, e2), e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_82])])).
fof(f860, plain, (spl15_97 <=> (e0 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_97])])).
fof(f2411, plain, (~ (e1 = op(e0, e2)) | (spl15_82 | ~ spl15_97)), inference(forward_demodulation, [], [f793, f861])).
fof(f861, plain, ((e0 = op(op(e2, e2), e2)) | ~ spl15_97), inference(avatar_component_clause, [], [f860])).
fof(f793, plain, (~ (e1 = op(op(op(e2, e2), e2), e2)) | spl15_82), inference(avatar_component_clause, [], [f791])).
fof(f2388, plain, (~ spl15_21 | ~ spl15_23 | spl15_97), inference(avatar_split_clause, [], [f2385, f860, f418, f410])).
fof(f410, plain, (spl15_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f2385, plain, (~ (e0 = op(e2, e2)) | (~ spl15_23 | spl15_97)), inference(backward_demodulation, [], [f862, f420])).
fof(f862, plain, (~ (e0 = op(op(e2, e2), e2)) | spl15_97), inference(avatar_component_clause, [], [f860])).
fof(f2387, plain, (spl15_22 | ~ spl15_23 | ~ spl15_89), inference(avatar_contradiction_clause, [], [f2386])).
fof(f2386, plain, ($false | (spl15_22 | ~ spl15_23 | ~ spl15_89)), inference(subsumption_resolution, [], [f2384, f415])).
fof(f415, plain, (~ (e1 = op(e2, e2)) | spl15_22), inference(avatar_component_clause, [], [f414])).
fof(f414, plain, (spl15_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_22])])).
fof(f2384, plain, ((e1 = op(e2, e2)) | (~ spl15_23 | ~ spl15_89)), inference(backward_demodulation, [], [f823, f420])).
fof(f823, plain, ((e1 = op(op(e2, e2), e2)) | ~ spl15_89), inference(avatar_component_clause, [], [f822])).
fof(f822, plain, (spl15_89 <=> (e1 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_89])])).
fof(f2370, plain, (~ spl15_39 | ~ spl15_43), inference(avatar_split_clause, [], [f2366, f503, f486])).
fof(f2366, plain, (~ (e2 = op(e1, e2)) | ~ spl15_43), inference(backward_demodulation, [], [f166, f505])).
fof(f505, plain, ((e2 = op(e1, e1)) | ~ spl15_43), inference(avatar_component_clause, [], [f503])).
fof(f166, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2348, plain, (~ spl15_59 | ~ spl15_63), inference(avatar_split_clause, [], [f2343, f588, f571])).
fof(f588, plain, (spl15_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl15_63])])).
fof(f2343, plain, (~ (e2 = op(e0, e1)) | ~ spl15_63), inference(backward_demodulation, [], [f157, f590])).
fof(f590, plain, ((op(e0, e0) = e2) | ~ spl15_63), inference(avatar_component_clause, [], [f588])).
fof(f157, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2347, plain, (~ spl15_31 | ~ spl15_63), inference(avatar_split_clause, [], [f2342, f588, f452])).
fof(f2342, plain, (~ (e2 = op(e2, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f134, f590])).
fof(f2340, plain, (spl15_17 | ~ spl15_3 | ~ spl15_94), inference(avatar_split_clause, [], [f2339, f846, f333, f393])).
fof(f846, plain, (spl15_94 <=> (e0 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_94])])).
fof(f2339, plain, ((e0 = op(e2, e3)) | (~ spl15_3 | ~ spl15_94)), inference(forward_demodulation, [], [f847, f335])).
fof(f335, plain, ((e2 = op(e3, e3)) | ~ spl15_3), inference(avatar_component_clause, [], [f333])).
fof(f847, plain, ((e0 = op(op(e3, e3), e3)) | ~ spl15_94), inference(avatar_component_clause, [], [f846])).
fof(f2335, plain, (~ spl15_48 | spl15_98 | ~ spl15_103), inference(avatar_contradiction_clause, [], [f2334])).
fof(f2334, plain, ($false | (~ spl15_48 | spl15_98 | ~ spl15_103)), inference(subsumption_resolution, [], [f2332, f526])).
fof(f526, plain, ((e3 = op(e1, e0)) | ~ spl15_48), inference(avatar_component_clause, [], [f524])).
fof(f524, plain, (spl15_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_48])])).
fof(f2332, plain, (~ (e3 = op(e1, e0)) | (spl15_98 | ~ spl15_103)), inference(backward_demodulation, [], [f867, f892])).
fof(f892, plain, ((e1 = op(op(e0, e0), e0)) | ~ spl15_103), inference(avatar_component_clause, [], [f891])).
fof(f891, plain, (spl15_103 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_103])])).
fof(f867, plain, (~ (e3 = op(op(op(e0, e0), e0), e0)) | spl15_98), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl15_98 <=> (e3 = op(op(op(e0, e0), e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_98])])).
fof(f2328, plain, (~ spl15_62 | ~ spl15_14), inference(avatar_split_clause, [], [f2327, f380, f584])).
fof(f584, plain, (spl15_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl15_62])])).
fof(f2327, plain, (~ (op(e0, e0) = e1) | ~ spl15_14), inference(forward_demodulation, [], [f135, f382])).
fof(f135, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2323, plain, (~ spl15_38 | ~ spl15_34), inference(avatar_split_clause, [], [f2322, f465, f482])).
fof(f482, plain, (spl15_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_38])])).
fof(f465, plain, (spl15_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_34])])).
fof(f2322, plain, (~ (e1 = op(e1, e2)) | ~ spl15_34), inference(forward_demodulation, [], [f168, f467])).
fof(f467, plain, ((e1 = op(e1, e3)) | ~ spl15_34), inference(avatar_component_clause, [], [f465])).
fof(f168, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2321, plain, (~ spl15_40 | ~ spl15_48), inference(avatar_split_clause, [], [f2320, f524, f490])).
fof(f490, plain, (spl15_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_40])])).
fof(f2320, plain, (~ (e3 = op(e1, e2)) | ~ spl15_48), inference(forward_demodulation, [], [f164, f526])).
fof(f2313, plain, (~ spl15_18 | ~ spl15_34), inference(avatar_split_clause, [], [f2312, f465, f397])).
fof(f397, plain, (spl15_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f2312, plain, (~ (e1 = op(e2, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f154, f467])).
fof(f154, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2307, plain, (~ spl15_32 | ~ spl15_48), inference(avatar_split_clause, [], [f2306, f524, f456])).
fof(f2306, plain, (~ (e3 = op(e2, e0)) | ~ spl15_48), inference(forward_demodulation, [], [f136, f526])).
fof(f2302, plain, (~ spl15_40 | spl15_88 | ~ spl15_89), inference(avatar_split_clause, [], [f2301, f822, f818, f490])).
fof(f818, plain, (spl15_88 <=> (e3 = op(op(op(e2, e2), e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_88])])).
fof(f2301, plain, (~ (e3 = op(e1, e2)) | (spl15_88 | ~ spl15_89)), inference(forward_demodulation, [], [f820, f823])).
fof(f820, plain, (~ (e3 = op(op(op(e2, e2), e2), e2)) | spl15_88), inference(avatar_component_clause, [], [f818])).
fof(f2299, plain, (~ spl15_20 | ~ spl15_52), inference(avatar_split_clause, [], [f2298, f541, f405])).
fof(f2298, plain, (~ (e3 = op(e2, e3)) | ~ spl15_52), inference(forward_demodulation, [], [f152, f543])).
fof(f543, plain, ((e3 = op(e0, e3)) | ~ spl15_52), inference(avatar_component_clause, [], [f541])).
fof(f2293, plain, (~ spl15_26 | ~ spl15_28), inference(avatar_contradiction_clause, [], [f2292])).
fof(f2292, plain, ($false | (~ spl15_26 | ~ spl15_28)), inference(subsumption_resolution, [], [f2290, f185])).
fof(f185, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2290, plain, ((e1 = e3) | (~ spl15_26 | ~ spl15_28)), inference(backward_demodulation, [], [f441, f433])).
fof(f433, plain, ((e1 = op(e2, e1)) | ~ spl15_26), inference(avatar_component_clause, [], [f431])).
fof(f431, plain, (spl15_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_26])])).
fof(f441, plain, ((e3 = op(e2, e1)) | ~ spl15_28), inference(avatar_component_clause, [], [f439])).
fof(f2289, plain, (~ spl15_33 | ~ spl15_34), inference(avatar_contradiction_clause, [], [f2288])).
fof(f2288, plain, ($false | (~ spl15_33 | ~ spl15_34)), inference(subsumption_resolution, [], [f2287, f181])).
fof(f181, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f2287, plain, ((e0 = e1) | (~ spl15_33 | ~ spl15_34)), inference(forward_demodulation, [], [f467, f463])).
fof(f463, plain, ((e0 = op(e1, e3)) | ~ spl15_33), inference(avatar_component_clause, [], [f461])).
fof(f461, plain, (spl15_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_33])])).
fof(f2282, plain, (~ spl15_9 | ~ spl15_44 | ~ spl15_59 | spl15_86), inference(avatar_contradiction_clause, [], [f2281])).
fof(f2281, plain, ($false | (~ spl15_9 | ~ spl15_44 | ~ spl15_59 | spl15_86)), inference(subsumption_resolution, [], [f2280, f573])).
fof(f2280, plain, (~ (e2 = op(e0, e1)) | (~ spl15_9 | ~ spl15_44 | spl15_86)), inference(forward_demodulation, [], [f2277, f361])).
fof(f361, plain, ((e0 = op(e3, e1)) | ~ spl15_9), inference(avatar_component_clause, [], [f359])).
fof(f2277, plain, (~ (e2 = op(op(e3, e1), e1)) | (~ spl15_44 | spl15_86)), inference(backward_demodulation, [], [f811, f509])).
fof(f811, plain, (~ (e2 = op(op(op(e1, e1), e1), e1)) | spl15_86), inference(avatar_component_clause, [], [f809])).
fof(f809, plain, (spl15_86 <=> (e2 = op(op(op(e1, e1), e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_86])])).
fof(f2275, plain, (~ spl15_16 | ~ spl15_48), inference(avatar_split_clause, [], [f2273, f524, f388])).
fof(f388, plain, (spl15_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f2273, plain, (~ (e3 = op(e3, e0)) | ~ spl15_48), inference(backward_demodulation, [], [f137, f526])).
fof(f137, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2256, plain, (~ spl15_3 | ~ spl15_18 | spl15_85), inference(avatar_contradiction_clause, [], [f2255])).
fof(f2255, plain, ($false | (~ spl15_3 | ~ spl15_18 | spl15_85)), inference(subsumption_resolution, [], [f2254, f399])).
fof(f399, plain, ((e1 = op(e2, e3)) | ~ spl15_18), inference(avatar_component_clause, [], [f397])).
fof(f2254, plain, (~ (e1 = op(e2, e3)) | (~ spl15_3 | spl15_85)), inference(forward_demodulation, [], [f806, f335])).
fof(f806, plain, (~ (e1 = op(op(e3, e3), e3)) | spl15_85), inference(avatar_component_clause, [], [f804])).
fof(f804, plain, (spl15_85 <=> (e1 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_85])])).
fof(f2247, plain, (~ spl15_56 | ~ spl15_8), inference(avatar_split_clause, [], [f2246, f354, f558])).
fof(f2246, plain, (~ (e3 = op(e0, e2)) | ~ spl15_8), inference(forward_demodulation, [], [f147, f356])).
fof(f356, plain, ((e3 = op(e3, e2)) | ~ spl15_8), inference(avatar_component_clause, [], [f354])).
fof(f2227, plain, (~ spl15_16 | ~ spl15_8), inference(avatar_split_clause, [], [f2226, f354, f388])).
fof(f2226, plain, (~ (e3 = op(e3, e0)) | ~ spl15_8), inference(forward_demodulation, [], [f176, f356])).
fof(f2215, plain, (~ spl15_3 | ~ spl15_18 | ~ spl15_33 | spl15_92), inference(avatar_contradiction_clause, [], [f2214])).
fof(f2214, plain, ($false | (~ spl15_3 | ~ spl15_18 | ~ spl15_33 | spl15_92)), inference(subsumption_resolution, [], [f2213, f463])).
fof(f2213, plain, (~ (e0 = op(e1, e3)) | (~ spl15_3 | ~ spl15_18 | spl15_92)), inference(forward_demodulation, [], [f2207, f399])).
fof(f2207, plain, (~ (e0 = op(op(e2, e3), e3)) | (~ spl15_3 | spl15_92)), inference(backward_demodulation, [], [f838, f335])).
fof(f838, plain, (~ (e0 = op(op(op(e3, e3), e3), e3)) | spl15_92), inference(avatar_component_clause, [], [f836])).
fof(f836, plain, (spl15_92 <=> (e0 = op(op(op(e3, e3), e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_92])])).
fof(f2203, plain, (~ spl15_9 | ~ spl15_10), inference(avatar_contradiction_clause, [], [f2202])).
fof(f2202, plain, ($false | (~ spl15_9 | ~ spl15_10)), inference(subsumption_resolution, [], [f2201, f181])).
fof(f2201, plain, ((e0 = e1) | (~ spl15_9 | ~ spl15_10)), inference(backward_demodulation, [], [f365, f361])).
fof(f365, plain, ((e1 = op(e3, e1)) | ~ spl15_10), inference(avatar_component_clause, [], [f363])).
fof(f2196, plain, (~ spl15_20 | ~ spl15_28), inference(avatar_split_clause, [], [f2192, f439, f405])).
fof(f2192, plain, (~ (e3 = op(e2, e3)) | ~ spl15_28), inference(backward_demodulation, [], [f173, f441])).
fof(f173, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2188, plain, (~ spl15_33 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f2187])).
fof(f2187, plain, ($false | (~ spl15_33 | ~ spl15_36)), inference(subsumption_resolution, [], [f2186, f183])).
fof(f183, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f2186, plain, ((e0 = e3) | (~ spl15_33 | ~ spl15_36)), inference(backward_demodulation, [], [f475, f463])).
fof(f475, plain, ((e3 = op(e1, e3)) | ~ spl15_36), inference(avatar_component_clause, [], [f473])).
fof(f473, plain, (spl15_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_36])])).
fof(f2183, plain, (~ spl15_20 | ~ spl15_36), inference(avatar_split_clause, [], [f2181, f473, f405])).
fof(f2181, plain, (~ (e3 = op(e2, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f154, f475])).
fof(f2177, plain, (~ spl15_35 | ~ spl15_39), inference(avatar_split_clause, [], [f2175, f486, f469])).
fof(f469, plain, (spl15_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_35])])).
fof(f2175, plain, (~ (e2 = op(e1, e3)) | ~ spl15_39), inference(backward_demodulation, [], [f168, f488])).
fof(f2173, plain, (~ spl15_35 | ~ spl15_43), inference(avatar_split_clause, [], [f2167, f503, f469])).
fof(f2167, plain, (~ (e2 = op(e1, e3)) | ~ spl15_43), inference(backward_demodulation, [], [f167, f505])).
fof(f167, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2163, plain, (~ spl15_35 | ~ spl15_51), inference(avatar_split_clause, [], [f2159, f537, f469])).
fof(f2159, plain, (~ (e2 = op(e1, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f151, f539])).
fof(f151, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2151, plain, (~ spl15_50 | ~ spl15_58), inference(avatar_split_clause, [], [f2147, f567, f533])).
fof(f533, plain, (spl15_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_50])])).
fof(f2147, plain, (~ (e1 = op(e0, e3)) | ~ spl15_58), inference(backward_demodulation, [], [f161, f569])).
fof(f161, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2150, plain, (~ spl15_26 | ~ spl15_58), inference(avatar_split_clause, [], [f2145, f567, f431])).
fof(f2145, plain, (~ (e1 = op(e2, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f140, f569])).
fof(f140, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2141, plain, (~ spl15_56 | ~ spl15_21 | spl15_83), inference(avatar_split_clause, [], [f2122, f795, f410, f558])).
fof(f795, plain, (spl15_83 <=> (e3 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_83])])).
fof(f2122, plain, (~ (e3 = op(e0, e2)) | (~ spl15_21 | spl15_83)), inference(backward_demodulation, [], [f797, f412])).
fof(f412, plain, ((e0 = op(e2, e2)) | ~ spl15_21), inference(avatar_component_clause, [], [f410])).
fof(f797, plain, (~ (e3 = op(op(e2, e2), e2)) | spl15_83), inference(avatar_component_clause, [], [f795])).
fof(f2139, plain, (~ spl15_38 | ~ spl15_46), inference(avatar_split_clause, [], [f2079, f516, f482])).
fof(f2079, plain, (~ (e1 = op(e1, e2)) | ~ spl15_46), inference(backward_demodulation, [], [f164, f518])).
fof(f518, plain, ((e1 = op(e1, e0)) | ~ spl15_46), inference(avatar_component_clause, [], [f516])).
fof(f2134, plain, (~ spl15_10 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f2133])).
fof(f2133, plain, ($false | (~ spl15_10 | ~ spl15_11)), inference(subsumption_resolution, [], [f2132, f184])).
fof(f184, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2132, plain, ((e1 = e2) | (~ spl15_10 | ~ spl15_11)), inference(forward_demodulation, [], [f369, f365])).
fof(f2112, plain, (~ spl15_19 | ~ spl15_31), inference(avatar_split_clause, [], [f2109, f452, f401])).
fof(f2109, plain, (~ (e2 = op(e2, e3)) | ~ spl15_31), inference(backward_demodulation, [], [f171, f454])).
fof(f454, plain, ((e2 = op(e2, e0)) | ~ spl15_31), inference(avatar_component_clause, [], [f452])).
fof(f171, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2107, plain, (~ spl15_19 | ~ spl15_35), inference(avatar_split_clause, [], [f2105, f469, f401])).
fof(f2105, plain, (~ (e2 = op(e2, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f154, f471])).
fof(f471, plain, ((e2 = op(e1, e3)) | ~ spl15_35), inference(avatar_component_clause, [], [f469])).
fof(f2104, plain, (~ spl15_24 | ~ spl15_40), inference(avatar_split_clause, [], [f2101, f490, f422])).
fof(f2101, plain, (~ (e3 = op(e2, e2)) | ~ spl15_40), inference(backward_demodulation, [], [f148, f492])).
fof(f492, plain, ((e3 = op(e1, e2)) | ~ spl15_40), inference(avatar_component_clause, [], [f490])).
fof(f2094, plain, (~ spl15_25 | ~ spl15_41), inference(avatar_split_clause, [], [f2085, f495, f427])).
fof(f495, plain, (spl15_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_41])])).
fof(f2085, plain, (~ (e0 = op(e2, e1)) | ~ spl15_41), inference(backward_demodulation, [], [f142, f497])).
fof(f497, plain, ((e0 = op(e1, e1)) | ~ spl15_41), inference(avatar_component_clause, [], [f495])).
fof(f2084, plain, (~ spl15_34 | ~ spl15_46), inference(avatar_split_clause, [], [f2080, f516, f465])).
fof(f2080, plain, (~ (e1 = op(e1, e3)) | ~ spl15_46), inference(backward_demodulation, [], [f165, f518])).
fof(f165, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2076, plain, (~ spl15_35 | ~ spl15_1 | ~ spl15_50 | spl15_84), inference(avatar_split_clause, [], [f2074, f800, f533, f325, f469])).
fof(f325, plain, (spl15_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f800, plain, (spl15_84 <=> (e2 = op(op(op(e3, e3), e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_84])])).
fof(f2074, plain, (~ (e2 = op(e1, e3)) | (~ spl15_1 | ~ spl15_50 | spl15_84)), inference(backward_demodulation, [], [f2054, f535])).
fof(f535, plain, ((e1 = op(e0, e3)) | ~ spl15_50), inference(avatar_component_clause, [], [f533])).
fof(f2054, plain, (~ (e2 = op(op(e0, e3), e3)) | (~ spl15_1 | spl15_84)), inference(forward_demodulation, [], [f802, f327])).
fof(f327, plain, ((e0 = op(e3, e3)) | ~ spl15_1), inference(avatar_component_clause, [], [f325])).
fof(f802, plain, (~ (e2 = op(op(op(e3, e3), e3), e3)) | spl15_84), inference(avatar_component_clause, [], [f800])).
fof(f2075, plain, (~ spl15_34 | ~ spl15_50), inference(avatar_split_clause, [], [f2069, f533, f465])).
fof(f2069, plain, (~ (e1 = op(e1, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f151, f535])).
fof(f2068, plain, (~ spl15_7 | ~ spl15_55), inference(avatar_split_clause, [], [f2064, f554, f350])).
fof(f350, plain, (spl15_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f554, plain, (spl15_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_55])])).
fof(f2064, plain, (~ (e2 = op(e3, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f147, f556])).
fof(f556, plain, ((e2 = op(e0, e2)) | ~ spl15_55), inference(avatar_component_clause, [], [f554])).
fof(f2067, plain, (~ spl15_39 | ~ spl15_55), inference(avatar_split_clause, [], [f2062, f554, f486])).
fof(f2062, plain, (~ (e2 = op(e1, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f145, f556])).
fof(f145, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2061, plain, (~ spl15_44 | ~ spl15_60), inference(avatar_split_clause, [], [f2058, f575, f507])).
fof(f575, plain, (spl15_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_60])])).
fof(f2058, plain, (~ (e3 = op(e1, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f139, f577])).
fof(f577, plain, ((e3 = op(e0, e1)) | ~ spl15_60), inference(avatar_component_clause, [], [f575])).
fof(f2060, plain, (~ spl15_52 | ~ spl15_60), inference(avatar_split_clause, [], [f2056, f575, f541])).
fof(f2056, plain, (~ (e3 = op(e0, e3)) | ~ spl15_60), inference(backward_demodulation, [], [f161, f577])).
fof(f2051, plain, (~ spl15_50 | ~ spl15_1 | spl15_85), inference(avatar_split_clause, [], [f2050, f804, f325, f533])).
fof(f2050, plain, (~ (e1 = op(e0, e3)) | (~ spl15_1 | spl15_85)), inference(forward_demodulation, [], [f806, f327])).
fof(f2043, plain, (~ spl15_32 | ~ spl15_16), inference(avatar_split_clause, [], [f2042, f388, f456])).
fof(f2042, plain, (~ (e3 = op(e2, e0)) | ~ spl15_16), inference(forward_demodulation, [], [f138, f390])).
fof(f390, plain, ((e3 = op(e3, e0)) | ~ spl15_16), inference(avatar_component_clause, [], [f388])).
fof(f2037, plain, (~ spl15_6 | ~ spl15_10), inference(avatar_split_clause, [], [f2015, f363, f346])).
fof(f2015, plain, (~ (e1 = op(e3, e2)) | ~ spl15_10), inference(backward_demodulation, [], [f178, f365])).
fof(f178, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2022, plain, (~ spl15_2 | ~ spl15_10), inference(avatar_split_clause, [], [f2016, f363, f329])).
fof(f329, plain, (spl15_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_2])])).
fof(f2016, plain, (~ (e1 = op(e3, e3)) | ~ spl15_10), inference(backward_demodulation, [], [f179, f365])).
fof(f179, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2014, plain, (~ spl15_12 | ~ spl15_16), inference(avatar_split_clause, [], [f2011, f388, f371])).
fof(f371, plain, (spl15_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f2011, plain, (~ (e3 = op(e3, e1)) | ~ spl15_16), inference(backward_demodulation, [], [f175, f390])).
fof(f175, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2009, plain, (~ spl15_2 | ~ spl15_34), inference(avatar_split_clause, [], [f2008, f465, f329])).
fof(f2008, plain, (~ (e1 = op(e3, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f155, f467])).
fof(f155, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2005, plain, (~ spl15_37 | ~ spl15_39), inference(avatar_contradiction_clause, [], [f2004])).
fof(f2004, plain, ($false | (~ spl15_37 | ~ spl15_39)), inference(subsumption_resolution, [], [f2003, f182])).
fof(f2003, plain, ((e0 = e2) | (~ spl15_37 | ~ spl15_39)), inference(backward_demodulation, [], [f488, f480])).
fof(f2001, plain, (~ spl15_12 | ~ spl15_44), inference(avatar_split_clause, [], [f1996, f507, f371])).
fof(f1996, plain, (~ (e3 = op(e3, e1)) | ~ spl15_44), inference(backward_demodulation, [], [f143, f509])).
fof(f143, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1967, plain, (~ spl15_7 | ~ spl15_39), inference(avatar_split_clause, [], [f1966, f486, f350])).
fof(f1966, plain, (~ (e2 = op(e3, e2)) | ~ spl15_39), inference(forward_demodulation, [], [f149, f488])).
fof(f1959, plain, (~ spl15_5 | ~ spl15_24 | spl15_97), inference(avatar_split_clause, [], [f1943, f860, f422, f342])).
fof(f1943, plain, (~ (e0 = op(e3, e2)) | (~ spl15_24 | spl15_97)), inference(backward_demodulation, [], [f862, f424])).
fof(f1957, plain, (~ spl15_4 | ~ spl15_52), inference(avatar_split_clause, [], [f1956, f541, f337])).
fof(f1956, plain, (~ (e3 = op(e3, e3)) | ~ spl15_52), inference(forward_demodulation, [], [f153, f543])).
fof(f153, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1937, plain, (~ spl15_25 | ~ spl15_26), inference(avatar_contradiction_clause, [], [f1936])).
fof(f1936, plain, ($false | (~ spl15_25 | ~ spl15_26)), inference(subsumption_resolution, [], [f1935, f181])).
fof(f1935, plain, ((e0 = e1) | (~ spl15_25 | ~ spl15_26)), inference(backward_demodulation, [], [f433, f429])).
fof(f1933, plain, (~ spl15_22 | ~ spl15_30), inference(avatar_split_clause, [], [f1931, f448, f414])).
fof(f1931, plain, (~ (e1 = op(e2, e2)) | ~ spl15_30), inference(backward_demodulation, [], [f170, f450])).
fof(f170, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1928, plain, (~ spl15_17 | ~ spl15_33), inference(avatar_split_clause, [], [f1922, f461, f393])).
fof(f1922, plain, (~ (e0 = op(e2, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f154, f463])).
fof(f1918, plain, (~ spl15_36 | ~ spl15_52), inference(avatar_split_clause, [], [f1916, f541, f473])).
fof(f1916, plain, (~ (e3 = op(e1, e3)) | ~ spl15_52), inference(backward_demodulation, [], [f151, f543])).
fof(f1897, plain, (~ spl15_33 | ~ spl15_41), inference(avatar_split_clause, [], [f1855, f495, f461])).
fof(f1855, plain, (~ (e0 = op(e1, e3)) | ~ spl15_41), inference(backward_demodulation, [], [f167, f497])).
fof(f1891, plain, (~ spl15_22 | ~ spl15_26), inference(avatar_split_clause, [], [f1890, f431, f414])).
fof(f1890, plain, (~ (e1 = op(e2, e2)) | ~ spl15_26), inference(forward_demodulation, [], [f172, f433])).
fof(f172, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1888, plain, (~ spl15_30 | ~ spl15_26), inference(avatar_split_clause, [], [f1887, f431, f448])).
fof(f1887, plain, (~ (e1 = op(e2, e0)) | ~ spl15_26), inference(forward_demodulation, [], [f169, f433])).
fof(f1853, plain, (~ spl15_42 | ~ spl15_46), inference(avatar_split_clause, [], [f1852, f516, f499])).
fof(f499, plain, (spl15_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_42])])).
fof(f1852, plain, (~ (e1 = op(e1, e1)) | ~ spl15_46), inference(backward_demodulation, [], [f163, f518])).
fof(f163, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1844, plain, (~ spl15_52 | ~ spl15_56), inference(avatar_split_clause, [], [f1842, f558, f541])).
fof(f1842, plain, (~ (e3 = op(e0, e3)) | ~ spl15_56), inference(backward_demodulation, [], [f162, f560])).
fof(f162, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1840, plain, (~ spl15_42 | ~ spl15_58), inference(avatar_split_clause, [], [f1839, f567, f499])).
fof(f1839, plain, (~ (e1 = op(e1, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f139, f569])).
fof(f1829, plain, (~ spl15_57 | ~ spl15_61), inference(avatar_split_clause, [], [f1819, f580, f563])).
fof(f563, plain, (spl15_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_57])])).
fof(f580, plain, (spl15_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_61])])).
fof(f1819, plain, (~ (e0 = op(e0, e1)) | ~ spl15_61), inference(backward_demodulation, [], [f157, f582])).
fof(f582, plain, ((e0 = op(e0, e0)) | ~ spl15_61), inference(avatar_component_clause, [], [f580])).
fof(f1828, plain, (~ spl15_45 | ~ spl15_61), inference(avatar_split_clause, [], [f1818, f580, f512])).
fof(f512, plain, (spl15_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_45])])).
fof(f1818, plain, (~ (e0 = op(e1, e0)) | ~ spl15_61), inference(backward_demodulation, [], [f133, f582])).
fof(f133, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f1817, plain, (~ spl15_35 | ~ spl15_2 | spl15_81), inference(avatar_split_clause, [], [f1816, f786, f329, f469])).
fof(f786, plain, (spl15_81 <=> (e2 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_81])])).
fof(f1816, plain, (~ (e2 = op(e1, e3)) | (~ spl15_2 | spl15_81)), inference(forward_demodulation, [], [f788, f331])).
fof(f331, plain, ((e1 = op(e3, e3)) | ~ spl15_2), inference(avatar_component_clause, [], [f329])).
fof(f788, plain, (~ (e2 = op(op(e3, e3), e3)) | spl15_81), inference(avatar_component_clause, [], [f786])).
fof(f1805, plain, (~ spl15_59 | ~ spl15_27), inference(avatar_split_clause, [], [f1804, f435, f571])).
fof(f435, plain, (spl15_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_27])])).
fof(f1804, plain, (~ (e2 = op(e0, e1)) | ~ spl15_27), inference(forward_demodulation, [], [f140, f437])).
fof(f437, plain, ((e2 = op(e2, e1)) | ~ spl15_27), inference(avatar_component_clause, [], [f435])).
fof(f1784, plain, (~ spl15_20 | ~ spl15_32), inference(avatar_split_clause, [], [f1783, f456, f405])).
fof(f1783, plain, (~ (e3 = op(e2, e3)) | ~ spl15_32), inference(backward_demodulation, [], [f171, f458])).
fof(f458, plain, ((e3 = op(e2, e0)) | ~ spl15_32), inference(avatar_component_clause, [], [f456])).
fof(f1782, plain, (~ spl15_3 | ~ spl15_35), inference(avatar_split_clause, [], [f1780, f469, f333])).
fof(f1780, plain, (~ (e2 = op(e3, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f155, f471])).
fof(f1768, plain, (~ spl15_33 | ~ spl15_45), inference(avatar_split_clause, [], [f1760, f512, f461])).
fof(f1760, plain, (~ (e0 = op(e1, e3)) | ~ spl15_45), inference(backward_demodulation, [], [f165, f514])).
fof(f514, plain, ((e0 = op(e1, e0)) | ~ spl15_45), inference(avatar_component_clause, [], [f512])).
fof(f1767, plain, (~ spl15_29 | ~ spl15_45), inference(avatar_split_clause, [], [f1758, f512, f444])).
fof(f1758, plain, (~ (e0 = op(e2, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f136, f514])).
fof(f1752, plain, (~ spl15_17 | ~ spl15_81 | spl15_92), inference(avatar_split_clause, [], [f1750, f836, f786, f393])).
fof(f1750, plain, (~ (e0 = op(e2, e3)) | (~ spl15_81 | spl15_92)), inference(backward_demodulation, [], [f838, f787])).
fof(f787, plain, ((e2 = op(op(e3, e3), e3)) | ~ spl15_81), inference(avatar_component_clause, [], [f786])).
fof(f1736, plain, (~ spl15_31 | ~ spl15_27), inference(avatar_split_clause, [], [f1735, f435, f452])).
fof(f1735, plain, (~ (e2 = op(e2, e0)) | ~ spl15_27), inference(forward_demodulation, [], [f169, f437])).
fof(f1734, plain, (~ spl15_30 | ~ spl15_62), inference(avatar_split_clause, [], [f1733, f584, f448])).
fof(f1733, plain, (~ (e1 = op(e2, e0)) | ~ spl15_62), inference(forward_demodulation, [], [f134, f586])).
fof(f586, plain, ((op(e0, e0) = e1) | ~ spl15_62), inference(avatar_component_clause, [], [f584])).
fof(f1728, plain, (~ spl15_5 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1727])).
fof(f1727, plain, ($false | (~ spl15_5 | ~ spl15_8)), inference(subsumption_resolution, [], [f1726, f183])).
fof(f1726, plain, ((e0 = e3) | (~ spl15_5 | ~ spl15_8)), inference(forward_demodulation, [], [f356, f344])).
fof(f344, plain, ((e0 = op(e3, e2)) | ~ spl15_5), inference(avatar_component_clause, [], [f342])).
fof(f1725, plain, (~ spl15_14 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1724])).
fof(f1724, plain, ($false | (~ spl15_14 | ~ spl15_15)), inference(subsumption_resolution, [], [f1723, f184])).
fof(f1723, plain, ((e1 = e2) | (~ spl15_14 | ~ spl15_15)), inference(backward_demodulation, [], [f386, f382])).
fof(f1722, plain, (~ spl15_2 | ~ spl15_50), inference(avatar_split_clause, [], [f1721, f533, f329])).
fof(f1721, plain, (~ (e1 = op(e3, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f153, f535])).
fof(f1690, plain, (~ spl15_50 | spl15_80 | ~ spl15_94), inference(avatar_split_clause, [], [f1689, f846, f782, f533])).
fof(f782, plain, (spl15_80 <=> (e1 = op(op(op(e3, e3), e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_80])])).
fof(f1689, plain, (~ (e1 = op(e0, e3)) | (spl15_80 | ~ spl15_94)), inference(forward_demodulation, [], [f784, f847])).
fof(f784, plain, (~ (e1 = op(op(op(e3, e3), e3), e3)) | spl15_80), inference(avatar_component_clause, [], [f782])).
fof(f1685, plain, (~ spl15_50 | ~ spl15_62), inference(avatar_split_clause, [], [f1549, f584, f533])).
fof(f1549, plain, (~ (e1 = op(e0, e3)) | ~ spl15_62), inference(backward_demodulation, [], [f159, f586])).
fof(f1677, plain, (~ spl15_1 | ~ spl15_5), inference(avatar_split_clause, [], [f1673, f342, f325])).
fof(f1673, plain, (~ (e0 = op(e3, e3)) | ~ spl15_5), inference(backward_demodulation, [], [f180, f344])).
fof(f180, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1662, plain, (~ spl15_18 | ~ spl15_22), inference(avatar_split_clause, [], [f1657, f414, f397])).
fof(f1657, plain, (~ (e1 = op(e2, e3)) | ~ spl15_22), inference(backward_demodulation, [], [f174, f416])).
fof(f416, plain, ((e1 = op(e2, e2)) | ~ spl15_22), inference(avatar_component_clause, [], [f414])).
fof(f1661, plain, (~ spl15_6 | ~ spl15_22), inference(avatar_split_clause, [], [f1656, f414, f346])).
fof(f1656, plain, (~ (e1 = op(e3, e2)) | ~ spl15_22), inference(backward_demodulation, [], [f150, f416])).
fof(f1655, plain, (~ spl15_19 | ~ spl15_27), inference(avatar_split_clause, [], [f1653, f435, f401])).
fof(f1653, plain, (~ (e2 = op(e2, e3)) | ~ spl15_27), inference(backward_demodulation, [], [f173, f437])).
fof(f1654, plain, (~ spl15_11 | ~ spl15_27), inference(avatar_split_clause, [], [f1651, f435, f367])).
fof(f1651, plain, (~ (e2 = op(e3, e1)) | ~ spl15_27), inference(backward_demodulation, [], [f144, f437])).
fof(f1650, plain, (~ spl15_1 | ~ spl15_33), inference(avatar_split_clause, [], [f1646, f461, f325])).
fof(f1646, plain, (~ (e0 = op(e3, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f155, f463])).
fof(f1640, plain, (~ spl15_36 | ~ spl15_48), inference(avatar_split_clause, [], [f1638, f524, f473])).
fof(f1638, plain, (~ (e3 = op(e1, e3)) | ~ spl15_48), inference(backward_demodulation, [], [f165, f526])).
fof(f1635, plain, (~ spl15_19 | ~ spl15_51), inference(avatar_split_clause, [], [f1632, f537, f401])).
fof(f1632, plain, (~ (e2 = op(e2, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f152, f539])).
fof(f1623, plain, (~ spl15_51 | spl15_84 | ~ spl15_94), inference(avatar_split_clause, [], [f1622, f846, f800, f537])).
fof(f1622, plain, (~ (e2 = op(e0, e3)) | (spl15_84 | ~ spl15_94)), inference(forward_demodulation, [], [f802, f847])).
fof(f1609, plain, (~ spl15_49 | ~ spl15_57), inference(avatar_split_clause, [], [f1608, f563, f529])).
fof(f1608, plain, (~ (e0 = op(e0, e3)) | ~ spl15_57), inference(forward_demodulation, [], [f161, f565])).
fof(f565, plain, ((e0 = op(e0, e1)) | ~ spl15_57), inference(avatar_component_clause, [], [f563])).
fof(f1607, plain, (~ spl15_25 | ~ spl15_57), inference(avatar_split_clause, [], [f1606, f563, f427])).
fof(f1606, plain, (~ (e0 = op(e2, e1)) | ~ spl15_57), inference(forward_demodulation, [], [f140, f565])).
fof(f1599, plain, (~ spl15_38 | ~ spl15_42), inference(avatar_split_clause, [], [f1598, f499, f482])).
fof(f1598, plain, (~ (e1 = op(e1, e2)) | ~ spl15_42), inference(forward_demodulation, [], [f166, f501])).
fof(f501, plain, ((e1 = op(e1, e1)) | ~ spl15_42), inference(avatar_component_clause, [], [f499])).
fof(f1594, plain, (~ spl15_7 | ~ spl15_11), inference(avatar_split_clause, [], [f1593, f367, f350])).
fof(f1593, plain, (~ (e2 = op(e3, e2)) | ~ spl15_11), inference(backward_demodulation, [], [f178, f369])).
fof(f1591, plain, (~ spl15_18 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f1590])).
fof(f1590, plain, ($false | (~ spl15_18 | ~ spl15_19)), inference(subsumption_resolution, [], [f1589, f184])).
fof(f1589, plain, ((e1 = e2) | (~ spl15_18 | ~ spl15_19)), inference(backward_demodulation, [], [f403, f399])).
fof(f1568, plain, (~ spl15_34 | ~ spl15_42), inference(avatar_split_clause, [], [f1562, f499, f465])).
fof(f1562, plain, (~ (e1 = op(e1, e3)) | ~ spl15_42), inference(backward_demodulation, [], [f167, f501])).
fof(f1567, plain, (~ spl15_10 | ~ spl15_42), inference(avatar_split_clause, [], [f1561, f499, f363])).
fof(f1561, plain, (~ (e1 = op(e3, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f143, f501])).
fof(f1539, plain, (~ spl15_61 | ~ spl15_29), inference(avatar_split_clause, [], [f1538, f444, f580])).
fof(f1538, plain, (~ (e0 = op(e0, e0)) | ~ spl15_29), inference(forward_demodulation, [], [f134, f446])).
fof(f1532, plain, (~ spl15_51 | ~ spl15_55), inference(avatar_split_clause, [], [f1531, f554, f537])).
fof(f1531, plain, (~ (e2 = op(e0, e3)) | ~ spl15_55), inference(forward_demodulation, [], [f162, f556])).
fof(f1530, plain, (~ spl15_49 | ~ spl15_1), inference(avatar_split_clause, [], [f1529, f325, f529])).
fof(f1529, plain, (~ (e0 = op(e0, e3)) | ~ spl15_1), inference(forward_demodulation, [], [f153, f327])).
fof(f1519, plain, (~ spl15_23 | ~ spl15_55), inference(avatar_split_clause, [], [f1518, f554, f418])).
fof(f1518, plain, (~ (e2 = op(e2, e2)) | ~ spl15_55), inference(forward_demodulation, [], [f146, f556])).
fof(f1516, plain, (~ spl15_83 | ~ spl15_97), inference(avatar_contradiction_clause, [], [f1515])).
fof(f1515, plain, ($false | (~ spl15_83 | ~ spl15_97)), inference(subsumption_resolution, [], [f1514, f183])).
fof(f1514, plain, ((e0 = e3) | (~ spl15_83 | ~ spl15_97)), inference(backward_demodulation, [], [f796, f861])).
fof(f796, plain, ((e3 = op(op(e2, e2), e2)) | ~ spl15_83), inference(avatar_component_clause, [], [f795])).
fof(f1511, plain, (~ spl15_18 | spl15_80 | ~ spl15_81), inference(avatar_split_clause, [], [f1430, f786, f782, f397])).
fof(f1430, plain, (~ (e1 = op(e2, e3)) | (spl15_80 | ~ spl15_81)), inference(forward_demodulation, [], [f784, f787])).
fof(f1490, plain, (~ spl15_17 | ~ spl15_29), inference(avatar_split_clause, [], [f1486, f444, f393])).
fof(f1486, plain, (~ (e0 = op(e2, e3)) | ~ spl15_29), inference(backward_demodulation, [], [f171, f446])).
fof(f1464, plain, (~ spl15_46 | ~ spl15_47), inference(avatar_contradiction_clause, [], [f1463])).
fof(f1463, plain, ($false | (~ spl15_46 | ~ spl15_47)), inference(subsumption_resolution, [], [f1462, f184])).
fof(f1462, plain, ((e1 = e2) | (~ spl15_46 | ~ spl15_47)), inference(backward_demodulation, [], [f522, f518])).
fof(f1460, plain, (~ spl15_58 | ~ spl15_60), inference(avatar_contradiction_clause, [], [f1459])).
fof(f1459, plain, ($false | (~ spl15_58 | ~ spl15_60)), inference(subsumption_resolution, [], [f1457, f185])).
fof(f1457, plain, ((e1 = e3) | (~ spl15_58 | ~ spl15_60)), inference(backward_demodulation, [], [f577, f569])).
fof(f1455, plain, (~ spl15_12 | ~ spl15_60), inference(avatar_split_clause, [], [f1452, f575, f371])).
fof(f1452, plain, (~ (e3 = op(e3, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f141, f577])).
fof(f1442, plain, (~ spl15_13 | ~ spl15_61), inference(avatar_split_clause, [], [f1432, f580, f376])).
fof(f1432, plain, (~ (e0 = op(e3, e0)) | ~ spl15_61), inference(backward_demodulation, [], [f135, f582])).
fof(f1424, plain, (~ spl15_63 | ~ spl15_51), inference(avatar_split_clause, [], [f1391, f537, f588])).
fof(f1391, plain, (~ (op(e0, e0) = e2) | ~ spl15_51), inference(forward_demodulation, [], [f159, f539])).
fof(f1421, plain, (~ spl15_56 | ~ spl15_22 | ~ spl15_37 | spl15_88), inference(avatar_split_clause, [], [f1403, f818, f478, f414, f558])).
fof(f1403, plain, (~ (e3 = op(e0, e2)) | (~ spl15_22 | ~ spl15_37 | spl15_88)), inference(forward_demodulation, [], [f1402, f480])).
fof(f1402, plain, (~ (e3 = op(op(e1, e2), e2)) | (~ spl15_22 | spl15_88)), inference(forward_demodulation, [], [f820, f416])).
fof(f1414, plain, (~ spl15_43 | ~ spl15_47), inference(avatar_split_clause, [], [f1413, f520, f503])).
fof(f1413, plain, (~ (e2 = op(e1, e1)) | ~ spl15_47), inference(forward_demodulation, [], [f163, f522])).
fof(f1398, plain, (~ spl15_47 | ~ spl15_62 | spl15_99), inference(avatar_contradiction_clause, [], [f1397])).
fof(f1397, plain, ($false | (~ spl15_47 | ~ spl15_62 | spl15_99)), inference(subsumption_resolution, [], [f1396, f522])).
fof(f1396, plain, (~ (e2 = op(e1, e0)) | (~ spl15_62 | spl15_99)), inference(forward_demodulation, [], [f871, f586])).
fof(f871, plain, (~ (e2 = op(op(e0, e0), e0)) | spl15_99), inference(avatar_component_clause, [], [f869])).
fof(f869, plain, (spl15_99 <=> (e2 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_99])])).
fof(f1377, plain, (~ spl15_32 | ~ spl15_47 | ~ spl15_62 | spl15_98), inference(avatar_split_clause, [], [f1374, f865, f584, f520, f456])).
fof(f1374, plain, (~ (e3 = op(e2, e0)) | (~ spl15_47 | ~ spl15_62 | spl15_98)), inference(backward_demodulation, [], [f1359, f522])).
fof(f1359, plain, (~ (e3 = op(op(e1, e0), e0)) | (~ spl15_62 | spl15_98)), inference(backward_demodulation, [], [f867, f586])).
fof(f1376, plain, (~ spl15_35 | ~ spl15_47), inference(avatar_split_clause, [], [f1371, f520, f469])).
fof(f1371, plain, (~ (e2 = op(e1, e3)) | ~ spl15_47), inference(backward_demodulation, [], [f165, f522])).
fof(f1367, plain, (~ spl15_51 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f1366])).
fof(f1366, plain, ($false | (~ spl15_51 | ~ spl15_52)), inference(subsumption_resolution, [], [f1365, f186])).
fof(f186, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1365, plain, ((e2 = e3) | (~ spl15_51 | ~ spl15_52)), inference(backward_demodulation, [], [f543, f539])).
fof(f1362, plain, (~ spl15_54 | ~ spl15_62), inference(avatar_split_clause, [], [f1355, f584, f550])).
fof(f1355, plain, (~ (e1 = op(e0, e2)) | ~ spl15_62), inference(backward_demodulation, [], [f158, f586])).
fof(f1343, plain, (~ spl15_53 | ~ spl15_57), inference(avatar_split_clause, [], [f1342, f563, f546])).
fof(f546, plain, (spl15_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_53])])).
fof(f1342, plain, (~ (e0 = op(e0, e2)) | ~ spl15_57), inference(forward_demodulation, [], [f160, f565])).
fof(f1339, plain, (~ spl15_37 | ~ spl15_22 | spl15_97), inference(avatar_split_clause, [], [f1302, f860, f414, f478])).
fof(f1302, plain, (~ (e0 = op(e1, e2)) | (~ spl15_22 | spl15_97)), inference(backward_demodulation, [], [f862, f416])).
fof(f1314, plain, (~ spl15_17 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f1313])).
fof(f1313, plain, ($false | (~ spl15_17 | ~ spl15_20)), inference(subsumption_resolution, [], [f1312, f183])).
fof(f1312, plain, ((e0 = e3) | (~ spl15_17 | ~ spl15_20)), inference(backward_demodulation, [], [f407, f395])).
fof(f395, plain, ((e0 = op(e2, e3)) | ~ spl15_17), inference(avatar_component_clause, [], [f393])).
fof(f407, plain, ((e3 = op(e2, e3)) | ~ spl15_20), inference(avatar_component_clause, [], [f405])).
fof(f1282, plain, (~ spl15_53 | ~ spl15_54), inference(avatar_contradiction_clause, [], [f1281])).
fof(f1281, plain, ($false | (~ spl15_53 | ~ spl15_54)), inference(subsumption_resolution, [], [f1280, f181])).
fof(f1280, plain, ((e0 = e1) | (~ spl15_53 | ~ spl15_54)), inference(backward_demodulation, [], [f552, f548])).
fof(f548, plain, ((e0 = op(e0, e2)) | ~ spl15_53), inference(avatar_component_clause, [], [f546])).
fof(f1278, plain, (~ spl15_9 | ~ spl15_57), inference(avatar_split_clause, [], [f1275, f563, f359])).
fof(f1275, plain, (~ (e0 = op(e3, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f141, f565])).
fof(f1269, plain, (~ spl15_47 | ~ spl15_63), inference(avatar_split_clause, [], [f1263, f588, f520])).
fof(f1263, plain, (~ (e2 = op(e1, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f133, f590])).
fof(f1255, plain, (~ spl15_51 | ~ spl15_1 | spl15_81), inference(avatar_split_clause, [], [f1229, f786, f325, f537])).
fof(f1229, plain, (~ (e2 = op(e0, e3)) | (~ spl15_1 | spl15_81)), inference(backward_demodulation, [], [f788, f327])).
fof(f1251, plain, (~ spl15_36 | ~ spl15_40), inference(avatar_split_clause, [], [f1250, f490, f473])).
fof(f1250, plain, (~ (e3 = op(e1, e3)) | ~ spl15_40), inference(forward_demodulation, [], [f168, f492])).
fof(f1243, plain, (~ spl15_23 | ~ spl15_7), inference(avatar_split_clause, [], [f1242, f350, f418])).
fof(f1242, plain, (~ (e2 = op(e2, e2)) | ~ spl15_7), inference(forward_demodulation, [], [f150, f352])).
fof(f352, plain, ((e2 = op(e3, e2)) | ~ spl15_7), inference(avatar_component_clause, [], [f350])).
fof(f1235, plain, (~ spl15_9 | ~ spl15_1), inference(avatar_split_clause, [], [f1234, f325, f359])).
fof(f1234, plain, (~ (e0 = op(e3, e1)) | ~ spl15_1), inference(forward_demodulation, [], [f179, f327])).
fof(f1202, plain, (~ spl15_21 | ~ spl15_54 | spl15_89), inference(avatar_contradiction_clause, [], [f1201])).
fof(f1201, plain, ($false | (~ spl15_21 | ~ spl15_54 | spl15_89)), inference(subsumption_resolution, [], [f1200, f552])).
fof(f1200, plain, (~ (e1 = op(e0, e2)) | (~ spl15_21 | spl15_89)), inference(forward_demodulation, [], [f824, f412])).
fof(f824, plain, (~ (e1 = op(op(e2, e2), e2)) | spl15_89), inference(avatar_component_clause, [], [f822])).
fof(f1187, plain, (~ spl15_49 | ~ spl15_33), inference(avatar_split_clause, [], [f1186, f461, f529])).
fof(f1186, plain, (~ (e0 = op(e0, e3)) | ~ spl15_33), inference(forward_demodulation, [], [f151, f463])).
fof(f1179, plain, (~ spl15_29 | ~ spl15_21), inference(avatar_split_clause, [], [f1178, f410, f444])).
fof(f1178, plain, (~ (e0 = op(e2, e0)) | ~ spl15_21), inference(forward_demodulation, [], [f170, f412])).
fof(f1171, plain, (~ spl15_17 | ~ spl15_21), inference(avatar_split_clause, [], [f1170, f410, f393])).
fof(f1170, plain, (~ (e0 = op(e2, e3)) | ~ spl15_21), inference(forward_demodulation, [], [f174, f412])).
fof(f1165, plain, (~ spl15_13 | ~ spl15_9), inference(avatar_split_clause, [], [f1164, f359, f376])).
fof(f1164, plain, (~ (e0 = op(e3, e0)) | ~ spl15_9), inference(forward_demodulation, [], [f175, f361])).
fof(f1155, plain, (~ spl15_5 | ~ spl15_21), inference(avatar_split_clause, [], [f1154, f410, f342])).
fof(f1154, plain, (~ (e0 = op(e3, e2)) | ~ spl15_21), inference(forward_demodulation, [], [f150, f412])).
fof(f1151, plain, (~ spl15_2 | ~ spl15_33 | spl15_94), inference(avatar_contradiction_clause, [], [f1150])).
fof(f1150, plain, ($false | (~ spl15_2 | ~ spl15_33 | spl15_94)), inference(subsumption_resolution, [], [f1147, f463])).
fof(f1147, plain, (~ (e0 = op(e1, e3)) | (~ spl15_2 | spl15_94)), inference(backward_demodulation, [], [f848, f331])).
fof(f848, plain, (~ (e0 = op(op(e3, e3), e3)) | spl15_94), inference(avatar_component_clause, [], [f846])).
fof(f1140, plain, (~ spl15_9 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f1139])).
fof(f1139, plain, ($false | (~ spl15_9 | ~ spl15_12)), inference(subsumption_resolution, [], [f1138, f183])).
fof(f1138, plain, ((e0 = e3) | (~ spl15_9 | ~ spl15_12)), inference(backward_demodulation, [], [f373, f361])).
fof(f373, plain, ((e3 = op(e3, e1)) | ~ spl15_12), inference(avatar_component_clause, [], [f371])).
fof(f1127, plain, (~ spl15_21 | ~ spl15_40 | ~ spl15_54 | spl15_88), inference(avatar_contradiction_clause, [], [f1126])).
fof(f1126, plain, ($false | (~ spl15_21 | ~ spl15_40 | ~ spl15_54 | spl15_88)), inference(subsumption_resolution, [], [f1125, f492])).
fof(f1125, plain, (~ (e3 = op(e1, e2)) | (~ spl15_21 | ~ spl15_54 | spl15_88)), inference(forward_demodulation, [], [f1123, f552])).
fof(f1123, plain, (~ (e3 = op(op(e0, e2), e2)) | (~ spl15_21 | spl15_88)), inference(backward_demodulation, [], [f820, f412])).
fof(f1118, plain, (~ spl15_23 | ~ spl15_27), inference(avatar_split_clause, [], [f1116, f435, f418])).
fof(f1116, plain, (~ (e2 = op(e2, e2)) | ~ spl15_27), inference(backward_demodulation, [], [f172, f437])).
fof(f1113, plain, (~ spl15_30 | ~ spl15_31), inference(avatar_contradiction_clause, [], [f1112])).
fof(f1112, plain, ($false | (~ spl15_30 | ~ spl15_31)), inference(subsumption_resolution, [], [f1111, f184])).
fof(f1111, plain, ((e1 = e2) | (~ spl15_30 | ~ spl15_31)), inference(backward_demodulation, [], [f454, f450])).
fof(f1110, plain, (~ spl15_31 | ~ spl15_32), inference(avatar_contradiction_clause, [], [f1109])).
fof(f1109, plain, ($false | (~ spl15_31 | ~ spl15_32)), inference(subsumption_resolution, [], [f1108, f186])).
fof(f1108, plain, ((e2 = e3) | (~ spl15_31 | ~ spl15_32)), inference(backward_demodulation, [], [f458, f454])).
fof(f1107, plain, (~ spl15_33 | ~ spl15_35), inference(avatar_contradiction_clause, [], [f1106])).
fof(f1106, plain, ($false | (~ spl15_33 | ~ spl15_35)), inference(subsumption_resolution, [], [f1105, f182])).
fof(f1105, plain, ((e0 = e2) | (~ spl15_33 | ~ spl15_35)), inference(backward_demodulation, [], [f471, f463])).
fof(f1090, plain, (~ spl15_55 | ~ spl15_59), inference(avatar_split_clause, [], [f1086, f571, f554])).
fof(f1086, plain, (~ (e2 = op(e0, e2)) | ~ spl15_59), inference(backward_demodulation, [], [f160, f573])).
fof(f1079, plain, (~ spl15_49 | ~ spl15_61), inference(avatar_split_clause, [], [f1069, f580, f529])).
fof(f1069, plain, (~ (e0 = op(e0, e3)) | ~ spl15_61), inference(backward_demodulation, [], [f159, f582])).
fof(f1078, plain, (~ spl15_53 | ~ spl15_61), inference(avatar_split_clause, [], [f1068, f580, f546])).
fof(f1068, plain, (~ (e0 = op(e0, e2)) | ~ spl15_61), inference(backward_demodulation, [], [f158, f582])).
fof(f1029, plain, (~ spl15_5 | ~ spl15_7), inference(avatar_contradiction_clause, [], [f1028])).
fof(f1028, plain, ($false | (~ spl15_5 | ~ spl15_7)), inference(subsumption_resolution, [], [f1027, f182])).
fof(f1027, plain, ((e0 = e2) | (~ spl15_5 | ~ spl15_7)), inference(forward_demodulation, [], [f352, f344])).
fof(f1026, plain, (~ spl15_3 | ~ spl15_17 | spl15_94), inference(avatar_contradiction_clause, [], [f1025])).
fof(f1025, plain, ($false | (~ spl15_3 | ~ spl15_17 | spl15_94)), inference(subsumption_resolution, [], [f1023, f395])).
fof(f1023, plain, (~ (e0 = op(e2, e3)) | (~ spl15_3 | spl15_94)), inference(backward_demodulation, [], [f848, f335])).
fof(f1020, plain, (~ spl15_5 | ~ spl15_6), inference(avatar_contradiction_clause, [], [f1019])).
fof(f1019, plain, ($false | (~ spl15_5 | ~ spl15_6)), inference(subsumption_resolution, [], [f1018, f181])).
fof(f1018, plain, ((e0 = e1) | (~ spl15_5 | ~ spl15_6)), inference(backward_demodulation, [], [f348, f344])).
fof(f348, plain, ((e1 = op(e3, e2)) | ~ spl15_6), inference(avatar_component_clause, [], [f346])).
fof(f1017, plain, (~ spl15_6 | ~ spl15_7), inference(avatar_contradiction_clause, [], [f1016])).
fof(f1016, plain, ($false | (~ spl15_6 | ~ spl15_7)), inference(subsumption_resolution, [], [f1015, f184])).
fof(f1015, plain, ((e1 = e2) | (~ spl15_6 | ~ spl15_7)), inference(backward_demodulation, [], [f352, f348])).
fof(f1014, plain, (~ spl15_7 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1013])).
fof(f1013, plain, ($false | (~ spl15_7 | ~ spl15_8)), inference(subsumption_resolution, [], [f1012, f186])).
fof(f1012, plain, ((e2 = e3) | (~ spl15_7 | ~ spl15_8)), inference(backward_demodulation, [], [f356, f352])).
fof(f1011, plain, (~ spl15_4 | ~ spl15_8), inference(avatar_split_clause, [], [f1010, f354, f337])).
fof(f1010, plain, (~ (e3 = op(e3, e3)) | ~ spl15_8), inference(backward_demodulation, [], [f180, f356])).
fof(f1009, plain, (~ spl15_3 | ~ spl15_11), inference(avatar_split_clause, [], [f1007, f367, f333])).
fof(f1007, plain, (~ (e2 = op(e3, e3)) | ~ spl15_11), inference(backward_demodulation, [], [f179, f369])).
fof(f1003, plain, (~ spl15_10 | ~ spl15_14), inference(avatar_split_clause, [], [f1000, f380, f363])).
fof(f1000, plain, (~ (e1 = op(e3, e1)) | ~ spl15_14), inference(backward_demodulation, [], [f175, f382])).
fof(f999, plain, (~ spl15_17 | ~ spl15_18), inference(avatar_contradiction_clause, [], [f998])).
fof(f998, plain, ($false | (~ spl15_17 | ~ spl15_18)), inference(subsumption_resolution, [], [f997, f181])).
fof(f997, plain, ((e0 = e1) | (~ spl15_17 | ~ spl15_18)), inference(backward_demodulation, [], [f399, f395])).
fof(f996, plain, (~ spl15_2 | ~ spl15_18), inference(avatar_split_clause, [], [f995, f397, f329])).
fof(f995, plain, (~ (e1 = op(e3, e3)) | ~ spl15_18), inference(backward_demodulation, [], [f156, f399])).
fof(f988, plain, (~ spl15_21 | ~ spl15_25), inference(avatar_split_clause, [], [f985, f427, f410])).
fof(f985, plain, (~ (e0 = op(e2, e2)) | ~ spl15_25), inference(backward_demodulation, [], [f172, f429])).
fof(f982, plain, (~ spl15_24 | ~ spl15_32), inference(avatar_split_clause, [], [f977, f456, f422])).
fof(f977, plain, (~ (e3 = op(e2, e2)) | ~ spl15_32), inference(backward_demodulation, [], [f170, f458])).
fof(f973, plain, (~ spl15_35 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f972])).
fof(f972, plain, ($false | (~ spl15_35 | ~ spl15_36)), inference(subsumption_resolution, [], [f971, f186])).
fof(f971, plain, ((e2 = e3) | (~ spl15_35 | ~ spl15_36)), inference(backward_demodulation, [], [f475, f471])).
fof(f970, plain, (~ spl15_4 | ~ spl15_36), inference(avatar_split_clause, [], [f968, f473, f337])).
fof(f968, plain, (~ (e3 = op(e3, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f155, f475])).
fof(f966, plain, (~ spl15_33 | ~ spl15_37), inference(avatar_split_clause, [], [f963, f478, f461])).
fof(f963, plain, (~ (e0 = op(e1, e3)) | ~ spl15_37), inference(backward_demodulation, [], [f168, f480])).
fof(f964, plain, (~ spl15_21 | ~ spl15_37), inference(avatar_split_clause, [], [f961, f478, f410])).
fof(f961, plain, (~ (e0 = op(e2, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f148, f480])).
fof(f957, plain, (~ spl15_26 | ~ spl15_42), inference(avatar_split_clause, [], [f953, f499, f431])).
fof(f953, plain, (~ (e1 = op(e2, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f142, f501])).
fof(f949, plain, (~ spl15_37 | ~ spl15_45), inference(avatar_split_clause, [], [f944, f512, f478])).
fof(f944, plain, (~ (e0 = op(e1, e2)) | ~ spl15_45), inference(backward_demodulation, [], [f164, f514])).
fof(f948, plain, (~ spl15_41 | ~ spl15_45), inference(avatar_split_clause, [], [f943, f512, f495])).
fof(f943, plain, (~ (e0 = op(e1, e1)) | ~ spl15_45), inference(backward_demodulation, [], [f163, f514])).
fof(f947, plain, (~ spl15_13 | ~ spl15_45), inference(avatar_split_clause, [], [f942, f512, f376])).
fof(f942, plain, (~ (e0 = op(e3, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f137, f514])).
fof(f939, plain, (~ spl15_18 | ~ spl15_50), inference(avatar_split_clause, [], [f936, f533, f397])).
fof(f936, plain, (~ (e1 = op(e2, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f152, f535])).
fof(f934, plain, (~ spl15_49 | ~ spl15_53), inference(avatar_split_clause, [], [f930, f546, f529])).
fof(f930, plain, (~ (e0 = op(e0, e3)) | ~ spl15_53), inference(backward_demodulation, [], [f162, f548])).
fof(f931, plain, (~ spl15_37 | ~ spl15_53), inference(avatar_split_clause, [], [f927, f546, f478])).
fof(f927, plain, (~ (e0 = op(e1, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f145, f548])).
fof(f916, plain, (~ spl15_30 | ~ spl15_63 | spl15_103), inference(avatar_split_clause, [], [f909, f891, f588, f448])).
fof(f909, plain, (~ (e1 = op(e2, e0)) | (~ spl15_63 | spl15_103)), inference(backward_demodulation, [], [f893, f590])).
fof(f893, plain, (~ (e1 = op(op(e0, e0), e0)) | spl15_103), inference(avatar_component_clause, [], [f891])).
fof(f914, plain, (~ spl15_55 | ~ spl15_63), inference(avatar_split_clause, [], [f905, f588, f554])).
fof(f905, plain, (~ (e2 = op(e0, e2)) | ~ spl15_63), inference(backward_demodulation, [], [f158, f590])).
fof(f899, plain, (~ spl15_44 | ~ spl15_86 | ~ spl15_102), inference(avatar_split_clause, [], [f306, f886, f809, f507])).
fof(f306, plain, (~ (e0 = op(op(e1, e1), e1)) | ~ (e2 = op(op(op(e1, e1), e1), e1)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e0 = op(op(e1, e1), e1)) | ~ (e2 = op(op(op(e1, e1), e1), e1)) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e0 = op(op(e1, e1), e1)) & (e2 = op(op(op(e1, e1), e1), e1)) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax28)).
fof(f897, plain, (~ spl15_24 | ~ spl15_82 | ~ spl15_97), inference(avatar_split_clause, [], [f304, f860, f791, f422])).
fof(f304, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e1 = op(op(op(e2, e2), e2), e2)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e1 = op(op(op(e2, e2), e2), e2)) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(op(e2, e2), e2)) & (e1 = op(op(op(e2, e2), e2), e2)) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax26)).
fof(f896, plain, (~ spl15_44 | ~ spl15_100 | ~ spl15_91), inference(avatar_split_clause, [], [f303, f831, f875, f507])).
fof(f303, plain, (~ (e2 = op(op(e1, e1), e1)) | ~ (e0 = op(op(op(e1, e1), e1), e1)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e2 = op(op(e1, e1), e1)) | ~ (e0 = op(op(op(e1, e1), e1), e1)) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e2 = op(op(e1, e1), e1)) & (e0 = op(op(op(e1, e1), e1), e1)) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax25)).
fof(f894, plain, (~ spl15_63 | ~ spl15_98 | ~ spl15_103), inference(avatar_split_clause, [], [f301, f891, f865, f588])).
fof(f301, plain, (~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0)) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e1 = op(op(e0, e0), e0)) & (e3 = op(op(op(e0, e0), e0), e0)) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax23)).
fof(f879, plain, (~ spl15_3 | ~ spl15_80 | ~ spl15_94), inference(avatar_split_clause, [], [f298, f846, f782, f333])).
fof(f298, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3)) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e0 = op(op(e3, e3), e3)) & (e1 = op(op(op(e3, e3), e3), e3)) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax20)).
fof(f873, plain, (~ spl15_3 | ~ spl15_92 | ~ spl15_85), inference(avatar_split_clause, [], [f296, f804, f836, f333])).
fof(f296, plain, (~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3)) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e1 = op(op(e3, e3), e3)) & (e0 = op(op(op(e3, e3), e3), e3)) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax18)).
fof(f872, plain, (~ spl15_62 | ~ spl15_98 | ~ spl15_99), inference(avatar_split_clause, [], [f295, f869, f865, f584])).
fof(f295, plain, (~ (e2 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e2 = op(op(e0, e0), e0)) | ~ (e3 = op(op(op(e0, e0), e0), e0)) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e2 = op(op(e0, e0), e0)) & (e3 = op(op(op(e0, e0), e0), e0)) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax17)).
fof(f863, plain, (~ spl15_22 | ~ spl15_88 | ~ spl15_97), inference(avatar_split_clause, [], [f294, f860, f818, f414])).
fof(f294, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2)) | ~ (e1 = op(e2, e2))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e0 = op(op(e2, e2), e2)) & (e3 = op(op(op(e2, e2), e2), e2)) & (e1 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax16)).
fof(f849, plain, (~ spl15_2 | ~ spl15_84 | ~ spl15_94), inference(avatar_split_clause, [], [f292, f846, f800, f329])).
fof(f292, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3)) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e0 = op(op(e3, e3), e3)) & (e2 = op(op(op(e3, e3), e3), e3)) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax14)).
fof(f839, plain, (~ spl15_2 | ~ spl15_92 | ~ spl15_81), inference(avatar_split_clause, [], [f290, f786, f836, f329])).
fof(f290, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e0 = op(op(op(e3, e3), e3), e3)) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e2 = op(op(e3, e3), e3)) & (e0 = op(op(op(e3, e3), e3), e3)) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax12)).
fof(f825, plain, (~ spl15_21 | ~ spl15_88 | ~ spl15_89), inference(avatar_split_clause, [], [f288, f822, f818, f410])).
fof(f288, plain, (~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(op(op(e2, e2), e2), e2)) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e1 = op(op(e2, e2), e2)) & (e3 = op(op(op(e2, e2), e2), e2)) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax10)).
fof(f807, plain, (~ spl15_1 | ~ spl15_84 | ~ spl15_85), inference(avatar_split_clause, [], [f286, f804, f800, f325])).
fof(f286, plain, (~ (e1 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e1 = op(op(e3, e3), e3)) | ~ (e2 = op(op(op(e3, e3), e3), e3)) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e1 = op(op(e3, e3), e3)) & (e2 = op(op(op(e3, e3), e3), e3)) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax8)).
fof(f789, plain, (~ spl15_1 | ~ spl15_80 | ~ spl15_81), inference(avatar_split_clause, [], [f284, f786, f782, f325])).
fof(f284, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e1 = op(op(op(e3, e3), e3), e3)) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(op(e3, e3), e3)) & (e1 = op(op(op(e3, e3), e3), e3)) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax6)).
fof(f780, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | spl15_4), inference(avatar_split_clause, [], [f262, f337, f629, f638, f647, f656, f665, f674, f683, f692, f701, f710, f719, f728, f737, f746, f755])).
fof(f755, plain, (spl15_79 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl15_79])])).
fof(f746, plain, (spl15_78 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl15_78])])).
fof(f737, plain, (spl15_77 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl15_77])])).
fof(f728, plain, (spl15_76 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl15_76])])).
fof(f719, plain, (spl15_75 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_75])])).
fof(f710, plain, (spl15_74 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_74])])).
fof(f701, plain, (spl15_73 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_73])])).
fof(f692, plain, (spl15_72 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_72])])).
fof(f683, plain, (spl15_71 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_71])])).
fof(f674, plain, (spl15_70 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl15_70])])).
fof(f665, plain, (spl15_69 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl15_69])])).
fof(f656, plain, (spl15_68 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl15_68])])).
fof(f647, plain, (spl15_67 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl15_67])])).
fof(f638, plain, (spl15_66 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl15_66])])).
fof(f629, plain, (spl15_65 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl15_65])])).
fof(f262, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))) & (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f5, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e1, e0))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e1, e2))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e1, e3))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e2, e0))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e2, e1))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e2, e3))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e3, e0))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e3, e1))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e3, e2))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f5, plain, (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))) & (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e3 = op(e3, e3))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e3, e2))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e3, e1))) | ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e3, e0))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e2, e3))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e2, e1))) | ((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e2, e0))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e1, e3))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e1, e2))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e1, e0))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1))) | ((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax5)).
fof(f779, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_64 | ~ spl15_49), inference(avatar_split_clause, [], [f263, f529, f592, f629, f638, f647, f656, f665, f674, f683, f692, f701, f710, f719, f728, f737, f746, f755])).
fof(f263, plain, (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f776, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_4), inference(avatar_split_clause, [], [f308, f337, f629, f638, f647, f656, f665, f674, f683, f692, f701, f710, f719, f728, f737, f746, f755])).
fof(f308, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(duplicate_literal_removal, [], [f266])).
fof(f266, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f775, plain, (spl15_61 | spl15_42 | spl15_23 | spl15_4), inference(avatar_split_clause, [], [f267, f337, f418, f499, f580])).
fof(f267, plain, ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f69])).
fof(f774, plain, (~ spl15_62 | spl15_57), inference(avatar_split_clause, [], [f269, f563, f584])).
fof(f269, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f69])).
fof(f773, plain, (~ spl15_63 | spl15_53), inference(avatar_split_clause, [], [f270, f546, f588])).
fof(f270, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f69])).
fof(f772, plain, (~ spl15_64 | spl15_49), inference(avatar_split_clause, [], [f271, f529, f592])).
fof(f271, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f69])).
fof(f771, plain, (~ spl15_41 | spl15_46), inference(avatar_split_clause, [], [f272, f516, f495])).
fof(f272, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f69])).
fof(f770, plain, (~ spl15_43 | spl15_38), inference(avatar_split_clause, [], [f274, f482, f503])).
fof(f274, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f69])).
fof(f769, plain, (~ spl15_44 | spl15_34), inference(avatar_split_clause, [], [f275, f465, f507])).
fof(f275, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f69])).
fof(f768, plain, (~ spl15_21 | spl15_31), inference(avatar_split_clause, [], [f276, f452, f410])).
fof(f276, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f69])).
fof(f767, plain, (~ spl15_22 | spl15_27), inference(avatar_split_clause, [], [f277, f435, f414])).
fof(f277, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f69])).
fof(f766, plain, (~ spl15_24 | spl15_19), inference(avatar_split_clause, [], [f279, f401, f422])).
fof(f279, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f69])).
fof(f765, plain, (~ spl15_1 | spl15_16), inference(avatar_split_clause, [], [f280, f388, f325])).
fof(f280, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f69])).
fof(f764, plain, (~ spl15_2 | spl15_12), inference(avatar_split_clause, [], [f281, f371, f329])).
fof(f281, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f69])).
fof(f763, plain, (~ spl15_3 | spl15_8), inference(avatar_split_clause, [], [f282, f354, f333])).
fof(f282, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f69])).
fof(f762, plain, (~ spl15_79 | spl15_61), inference(avatar_split_clause, [], [f257, f580, f755])).
fof(f257, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f761, plain, (~ spl15_79 | ~ spl15_61), inference(avatar_split_clause, [], [f309, f580, f755])).
fof(f309, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(duplicate_literal_removal, [], [f258])).
fof(f258, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f753, plain, (~ spl15_78 | spl15_58), inference(avatar_split_clause, [], [f252, f567, f746])).
fof(f252, plain, ((e1 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f83, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e1 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f752, plain, (~ spl15_78 | ~ spl15_61), inference(avatar_split_clause, [], [f310, f580, f746])).
fof(f310, plain, (~ (e0 = op(e0, e0)) | ~ sP1), inference(duplicate_literal_removal, [], [f253])).
fof(f253, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f751, plain, (~ spl15_78 | ~ spl15_41 | ~ spl15_46), inference(avatar_split_clause, [], [f254, f516, f495, f746])).
fof(f254, plain, (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f750, plain, (~ spl15_78 | ~ spl15_21 | ~ spl15_31), inference(avatar_split_clause, [], [f255, f452, f410, f746])).
fof(f255, plain, (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f749, plain, (~ spl15_78 | ~ spl15_1 | ~ spl15_16), inference(avatar_split_clause, [], [f256, f388, f325, f746])).
fof(f256, plain, (~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f744, plain, (~ spl15_77 | spl15_55), inference(avatar_split_clause, [], [f247, f554, f737])).
fof(f247, plain, ((e2 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f82, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e2 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f743, plain, (~ spl15_77 | ~ spl15_61), inference(avatar_split_clause, [], [f311, f580, f737])).
fof(f311, plain, (~ (e0 = op(e0, e0)) | ~ sP2), inference(duplicate_literal_removal, [], [f248])).
fof(f248, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f741, plain, (~ spl15_77 | ~ spl15_21 | ~ spl15_31), inference(avatar_split_clause, [], [f250, f452, f410, f737])).
fof(f250, plain, (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f735, plain, (~ spl15_76 | spl15_52), inference(avatar_split_clause, [], [f242, f541, f728])).
fof(f242, plain, ((e3 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f81, plain, (((~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & (e3 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f734, plain, (~ spl15_76 | ~ spl15_61), inference(avatar_split_clause, [], [f312, f580, f728])).
fof(f312, plain, (~ (e0 = op(e0, e0)) | ~ sP3), inference(duplicate_literal_removal, [], [f243])).
fof(f243, plain, (~ (e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f733, plain, (~ spl15_76 | ~ spl15_41 | ~ spl15_46), inference(avatar_split_clause, [], [f244, f516, f495, f728])).
fof(f244, plain, (~ (e1 = op(e1, e0)) | ~ (e0 = op(e1, e1)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f732, plain, (~ spl15_76 | ~ spl15_21 | ~ spl15_31), inference(avatar_split_clause, [], [f245, f452, f410, f728])).
fof(f245, plain, (~ (e2 = op(e2, e0)) | ~ (e0 = op(e2, e2)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f731, plain, (~ spl15_76 | ~ spl15_1 | ~ spl15_16), inference(avatar_split_clause, [], [f246, f388, f325, f728])).
fof(f246, plain, (~ (e3 = op(e3, e0)) | ~ (e0 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f726, plain, (~ spl15_75 | spl15_45), inference(avatar_split_clause, [], [f237, f512, f719])).
fof(f237, plain, ((e0 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f80, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e0 = op(e1, e0))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f725, plain, (~ spl15_75 | ~ spl15_62 | ~ spl15_57), inference(avatar_split_clause, [], [f238, f563, f584, f719])).
fof(f238, plain, (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f724, plain, (~ spl15_75 | ~ spl15_42), inference(avatar_split_clause, [], [f313, f499, f719])).
fof(f313, plain, (~ (e1 = op(e1, e1)) | ~ sP4), inference(duplicate_literal_removal, [], [f239])).
fof(f239, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f717, plain, (~ spl15_74 | spl15_42), inference(avatar_split_clause, [], [f232, f499, f710])).
fof(f232, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f79, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f715, plain, (~ spl15_74 | ~ spl15_42), inference(avatar_split_clause, [], [f314, f499, f710])).
fof(f314, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(duplicate_literal_removal, [], [f234])).
fof(f234, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f708, plain, (~ spl15_73 | spl15_39), inference(avatar_split_clause, [], [f227, f486, f701])).
fof(f227, plain, ((e2 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e2 = op(e1, e2))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f707, plain, (~ spl15_73 | ~ spl15_62 | ~ spl15_57), inference(avatar_split_clause, [], [f228, f563, f584, f701])).
fof(f228, plain, (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f706, plain, (~ spl15_73 | ~ spl15_42), inference(avatar_split_clause, [], [f315, f499, f701])).
fof(f315, plain, (~ (e1 = op(e1, e1)) | ~ sP6), inference(duplicate_literal_removal, [], [f229])).
fof(f229, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f704, plain, (~ spl15_73 | ~ spl15_2 | ~ spl15_12), inference(avatar_split_clause, [], [f231, f371, f329, f701])).
fof(f231, plain, (~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f699, plain, (~ spl15_72 | spl15_36), inference(avatar_split_clause, [], [f222, f473, f692])).
fof(f222, plain, ((e3 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & (~ (e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & (e3 = op(e1, e3))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f698, plain, (~ spl15_72 | ~ spl15_62 | ~ spl15_57), inference(avatar_split_clause, [], [f223, f563, f584, f692])).
fof(f223, plain, (~ (e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f697, plain, (~ spl15_72 | ~ spl15_42), inference(avatar_split_clause, [], [f316, f499, f692])).
fof(f316, plain, (~ (e1 = op(e1, e1)) | ~ sP7), inference(duplicate_literal_removal, [], [f224])).
fof(f224, plain, (~ (e1 = op(e1, e1)) | ~ (e1 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f695, plain, (~ spl15_72 | ~ spl15_2 | ~ spl15_12), inference(avatar_split_clause, [], [f226, f371, f329, f692])).
fof(f226, plain, (~ (e3 = op(e3, e1)) | ~ (e1 = op(e3, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f690, plain, (~ spl15_71 | spl15_29), inference(avatar_split_clause, [], [f217, f444, f683])).
fof(f217, plain, ((e0 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e0 = op(e2, e0))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f689, plain, (~ spl15_71 | ~ spl15_63 | ~ spl15_53), inference(avatar_split_clause, [], [f218, f546, f588, f683])).
fof(f218, plain, (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f687, plain, (~ spl15_71 | ~ spl15_23), inference(avatar_split_clause, [], [f317, f418, f683])).
fof(f317, plain, (~ (e2 = op(e2, e2)) | ~ sP8), inference(duplicate_literal_removal, [], [f220])).
fof(f220, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f681, plain, (~ spl15_70 | spl15_26), inference(avatar_split_clause, [], [f212, f431, f674])).
fof(f212, plain, ((e1 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f75, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e1 = op(e2, e1))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f679, plain, (~ spl15_70 | ~ spl15_43 | ~ spl15_38), inference(avatar_split_clause, [], [f214, f482, f503, f674])).
fof(f214, plain, (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f678, plain, (~ spl15_70 | ~ spl15_23), inference(avatar_split_clause, [], [f318, f418, f674])).
fof(f318, plain, (~ (e2 = op(e2, e2)) | ~ sP9), inference(duplicate_literal_removal, [], [f215])).
fof(f215, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f672, plain, (~ spl15_69 | spl15_23), inference(avatar_split_clause, [], [f207, f418, f665])).
fof(f207, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f74, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f669, plain, (~ spl15_69 | ~ spl15_23), inference(avatar_split_clause, [], [f319, f418, f665])).
fof(f319, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(duplicate_literal_removal, [], [f210])).
fof(f210, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f663, plain, (~ spl15_68 | spl15_20), inference(avatar_split_clause, [], [f202, f405, f656])).
fof(f202, plain, ((e3 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f73, plain, (((~ (e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & (e3 = op(e2, e3))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f662, plain, (~ spl15_68 | ~ spl15_63 | ~ spl15_53), inference(avatar_split_clause, [], [f203, f546, f588, f656])).
fof(f203, plain, (~ (e0 = op(e0, e2)) | ~ (op(e0, e0) = e2) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f661, plain, (~ spl15_68 | ~ spl15_43 | ~ spl15_38), inference(avatar_split_clause, [], [f204, f482, f503, f656])).
fof(f204, plain, (~ (e1 = op(e1, e2)) | ~ (e2 = op(e1, e1)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f660, plain, (~ spl15_68 | ~ spl15_23), inference(avatar_split_clause, [], [f320, f418, f656])).
fof(f320, plain, (~ (e2 = op(e2, e2)) | ~ sP11), inference(duplicate_literal_removal, [], [f205])).
fof(f205, plain, (~ (e2 = op(e2, e2)) | ~ (e2 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f654, plain, (~ spl15_67 | spl15_13), inference(avatar_split_clause, [], [f197, f376, f647])).
fof(f197, plain, ((e0 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f72, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e0 = op(e3, e0))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f653, plain, (~ spl15_67 | ~ spl15_64 | ~ spl15_49), inference(avatar_split_clause, [], [f198, f529, f592, f647])).
fof(f198, plain, (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f650, plain, (~ spl15_67 | ~ spl15_4), inference(avatar_split_clause, [], [f321, f337, f647])).
fof(f321, plain, (~ (e3 = op(e3, e3)) | ~ sP12), inference(duplicate_literal_removal, [], [f201])).
fof(f201, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f645, plain, (~ spl15_66 | spl15_10), inference(avatar_split_clause, [], [f192, f363, f638])).
fof(f192, plain, ((e1 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f71, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e1 = op(e3, e1))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f644, plain, (~ spl15_66 | ~ spl15_64 | ~ spl15_49), inference(avatar_split_clause, [], [f193, f529, f592, f638])).
fof(f193, plain, (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f643, plain, (~ spl15_66 | ~ spl15_44 | ~ spl15_34), inference(avatar_split_clause, [], [f194, f465, f507, f638])).
fof(f194, plain, (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f641, plain, (~ spl15_66 | ~ spl15_4), inference(avatar_split_clause, [], [f322, f337, f638])).
fof(f322, plain, (~ (e3 = op(e3, e3)) | ~ sP13), inference(duplicate_literal_removal, [], [f196])).
fof(f196, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f636, plain, (~ spl15_65 | spl15_7), inference(avatar_split_clause, [], [f187, f350, f629])).
fof(f187, plain, ((e2 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & (~ (e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & (e2 = op(e3, e2))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f634, plain, (~ spl15_65 | ~ spl15_44 | ~ spl15_34), inference(avatar_split_clause, [], [f189, f465, f507, f629])).
fof(f189, plain, (~ (e1 = op(e1, e3)) | ~ (e3 = op(e1, e1)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f633, plain, (~ spl15_65 | ~ spl15_24 | ~ spl15_19), inference(avatar_split_clause, [], [f190, f401, f422, f629])).
fof(f190, plain, (~ (e2 = op(e2, e3)) | ~ (e3 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f632, plain, (~ spl15_65 | ~ spl15_4), inference(avatar_split_clause, [], [f323, f337, f629])).
fof(f323, plain, (~ (e3 = op(e3, e3)) | ~ sP14), inference(duplicate_literal_removal, [], [f191])).
fof(f191, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f627, plain, (spl15_61 | spl15_57 | spl15_53 | spl15_49), inference(avatar_split_clause, [], [f101, f529, f546, f563, f580])).
fof(f101, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax2)).
fof(f625, plain, (spl15_62 | spl15_58 | spl15_54 | spl15_50), inference(avatar_split_clause, [], [f103, f533, f550, f567, f584])).
fof(f103, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f624, plain, (spl15_62 | spl15_46 | spl15_30 | spl15_14), inference(avatar_split_clause, [], [f104, f380, f448, f516, f584])).
fof(f104, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f623, plain, (spl15_63 | spl15_59 | spl15_55 | spl15_51), inference(avatar_split_clause, [], [f105, f537, f554, f571, f588])).
fof(f105, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f622, plain, (spl15_63 | spl15_47 | spl15_31 | spl15_15), inference(avatar_split_clause, [], [f106, f384, f452, f520, f588])).
fof(f106, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f621, plain, (spl15_64 | spl15_60 | spl15_56 | spl15_52), inference(avatar_split_clause, [], [f107, f541, f558, f575, f592])).
fof(f107, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f620, plain, (spl15_64 | spl15_48 | spl15_32 | spl15_16), inference(avatar_split_clause, [], [f108, f388, f456, f524, f592])).
fof(f108, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f619, plain, (spl15_45 | spl15_41 | spl15_37 | spl15_33), inference(avatar_split_clause, [], [f109, f461, f478, f495, f512])).
fof(f109, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f618, plain, (spl15_57 | spl15_41 | spl15_25 | spl15_9), inference(avatar_split_clause, [], [f110, f359, f427, f495, f563])).
fof(f110, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f615, plain, (spl15_47 | spl15_43 | spl15_39 | spl15_35), inference(avatar_split_clause, [], [f113, f469, f486, f503, f520])).
fof(f113, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f614, plain, (spl15_59 | spl15_43 | spl15_27 | spl15_11), inference(avatar_split_clause, [], [f114, f367, f435, f503, f571])).
fof(f114, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f613, plain, (spl15_48 | spl15_44 | spl15_40 | spl15_36), inference(avatar_split_clause, [], [f115, f473, f490, f507, f524])).
fof(f115, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f611, plain, (spl15_29 | spl15_25 | spl15_21 | spl15_17), inference(avatar_split_clause, [], [f117, f393, f410, f427, f444])).
fof(f117, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f610, plain, (spl15_53 | spl15_37 | spl15_21 | spl15_5), inference(avatar_split_clause, [], [f118, f342, f410, f478, f546])).
fof(f118, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f609, plain, (spl15_30 | spl15_26 | spl15_22 | spl15_18), inference(avatar_split_clause, [], [f119, f397, f414, f431, f448])).
fof(f119, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f608, plain, (spl15_54 | spl15_38 | spl15_22 | spl15_6), inference(avatar_split_clause, [], [f120, f346, f414, f482, f550])).
fof(f120, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f605, plain, (spl15_32 | spl15_28 | spl15_24 | spl15_20), inference(avatar_split_clause, [], [f123, f405, f422, f439, f456])).
fof(f123, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f603, plain, (spl15_13 | spl15_9 | spl15_5 | spl15_1), inference(avatar_split_clause, [], [f125, f325, f342, f359, f376])).
fof(f125, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f602, plain, (spl15_49 | spl15_33 | spl15_17 | spl15_1), inference(avatar_split_clause, [], [f126, f325, f393, f461, f529])).
fof(f126, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f601, plain, (spl15_14 | spl15_10 | spl15_6 | spl15_2), inference(avatar_split_clause, [], [f127, f329, f346, f363, f380])).
fof(f127, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f600, plain, (spl15_50 | spl15_34 | spl15_18 | spl15_2), inference(avatar_split_clause, [], [f128, f329, f397, f465, f533])).
fof(f128, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f598, plain, (spl15_51 | spl15_35 | spl15_19 | spl15_3), inference(avatar_split_clause, [], [f130, f333, f401, f469, f537])).
fof(f130, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f597, plain, (spl15_16 | spl15_12 | spl15_8 | spl15_4), inference(avatar_split_clause, [], [f131, f337, f354, f371, f388])).
fof(f131, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f596, plain, (spl15_52 | spl15_36 | spl15_20 | spl15_4), inference(avatar_split_clause, [], [f132, f337, f405, f473, f541])).
fof(f132, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f595, plain, (spl15_61 | spl15_62 | spl15_63 | spl15_64), inference(avatar_split_clause, [], [f85, f592, f588, f584, f580])).
fof(f85, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG148+1.p', ax1)).
fof(f578, plain, (spl15_57 | spl15_58 | spl15_59 | spl15_60), inference(avatar_split_clause, [], [f86, f575, f571, f567, f563])).
fof(f86, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f561, plain, (spl15_53 | spl15_54 | spl15_55 | spl15_56), inference(avatar_split_clause, [], [f87, f558, f554, f550, f546])).
fof(f87, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f544, plain, (spl15_49 | spl15_50 | spl15_51 | spl15_52), inference(avatar_split_clause, [], [f88, f541, f537, f533, f529])).
fof(f88, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f527, plain, (spl15_45 | spl15_46 | spl15_47 | spl15_48), inference(avatar_split_clause, [], [f89, f524, f520, f516, f512])).
fof(f89, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f510, plain, (spl15_41 | spl15_42 | spl15_43 | spl15_44), inference(avatar_split_clause, [], [f90, f507, f503, f499, f495])).
fof(f90, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f493, plain, (spl15_37 | spl15_38 | spl15_39 | spl15_40), inference(avatar_split_clause, [], [f91, f490, f486, f482, f478])).
fof(f91, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f476, plain, (spl15_33 | spl15_34 | spl15_35 | spl15_36), inference(avatar_split_clause, [], [f92, f473, f469, f465, f461])).
fof(f92, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f459, plain, (spl15_29 | spl15_30 | spl15_31 | spl15_32), inference(avatar_split_clause, [], [f93, f456, f452, f448, f444])).
fof(f93, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f442, plain, (spl15_25 | spl15_26 | spl15_27 | spl15_28), inference(avatar_split_clause, [], [f94, f439, f435, f431, f427])).
fof(f94, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f425, plain, (spl15_21 | spl15_22 | spl15_23 | spl15_24), inference(avatar_split_clause, [], [f95, f422, f418, f414, f410])).
fof(f95, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f408, plain, (spl15_17 | spl15_18 | spl15_19 | spl15_20), inference(avatar_split_clause, [], [f96, f405, f401, f397, f393])).
fof(f96, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f374, plain, (spl15_9 | spl15_10 | spl15_11 | spl15_12), inference(avatar_split_clause, [], [f98, f371, f367, f363, f359])).
fof(f98, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f357, plain, (spl15_5 | spl15_6 | spl15_7 | spl15_8), inference(avatar_split_clause, [], [f99, f354, f350, f346, f342])).
fof(f99, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f340, plain, (spl15_1 | spl15_2 | spl15_3 | spl15_4), inference(avatar_split_clause, [], [f100, f337, f333, f329, f325])).
fof(f100, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).