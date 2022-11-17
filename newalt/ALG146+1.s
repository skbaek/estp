fof(f2641, plain, $false, inference(avatar_sat_refutation, [], [f292, f309, f326, f343, f360, f377, f394, f411, f428, f462, f479, f496, f513, f530, f547, f549, f550, f551, f552, f553, f554, f555, f556, f557, f560, f561, f562, f563, f564, f565, f566, f567, f568, f569, f570, f571, f572, f573, f574, f575, f577, f579, f584, f585, f586, f591, f592, f593, f598, f599, f600, f605, f606, f607, f612, f614, f619, f620, f621, f626, f627, f628, f633, f634, f635, f640, f641, f642, f647, f649, f654, f655, f656, f661, f662, f663, f668, f669, f670, f675, f676, f677, f682, f684, f685, f687, f688, f689, f690, f691, f692, f693, f694, f695, f696, f697, f698, f699, f700, f709, f727, f745, f754, f759, f778, f783, f793, f799, f814, f815, f816, f817, f818, f819, f832, f841, f852, f858, f862, f865, f872, f874, f884, f895, f898, f905, f917, f926, f935, f939, f942, f944, f950, f953, f963, f966, f978, f979, f983, f989, f1006, f1010, f1014, f1019, f1028, f1029, f1030, f1036, f1037, f1048, f1073, f1109, f1124, f1130, f1131, f1132, f1159, f1164, f1168, f1182, f1185, f1193, f1205, f1206, f1214, f1225, f1244, f1248, f1251, f1255, f1283, f1319, f1346, f1387, f1390, f1393, f1420, f1423, f1435, f1440, f1445, f1449, f1451, f1455, f1458, f1460, f1462, f1464, f1467, f1470, f1474, f1495, f1496, f1499, f1506, f1511, f1539, f1544, f1548, f1596, f1602, f1609, f1615, f1622, f1628, f1633, f1652, f1656, f1660, f1672, f1679, f1695, f1712, f1739, f1748, f1751, f1754, f1757, f1762, f1783, f1801, f1845, f1847, f1852, f1862, f1873, f1907, f1915, f1928, f1949, f1970, f1974, f1986, f1999, f2003, f2008, f2028, f2032, f2040, f2045, f2053, f2067, f2068, f2097, f2122, f2129, f2132, f2151, f2155, f2159, f2160, f2161, f2163, f2172, f2188, f2193, f2196, f2201, f2206, f2210, f2219, f2220, f2234, f2244, f2249, f2264, f2291, f2311, f2327, f2337, f2341, f2352, f2353, f2364, f2365, f2375, f2384, f2385, f2394, f2401, f2404, f2409, f2415, f2441, f2444, f2473, f2492, f2496, f2500, f2509, f2514, f2518, f2543, f2552, f2558, f2559, f2565, f2573, f2574, f2587, f2599, f2600, f2613, f2614, f2616, f2626, f2635, f2640])).
fof(f2640, plain, (~ spl15_21 | ~ spl15_64 | spl15_88), inference(avatar_contradiction_clause, [], [f2639])).
fof(f2639, plain, ($false | (~ spl15_21 | ~ spl15_64 | spl15_88)), inference(subsumption_resolution, [], [f2638, f546])).
fof(f546, plain, ((op(e0, e0) = e3) | ~ spl15_64), inference(avatar_component_clause, [], [f544])).
fof(f544, plain, (spl15_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl15_64])])).
fof(f2638, plain, (~ (op(e0, e0) = e3) | (~ spl15_21 | spl15_88)), inference(forward_demodulation, [], [f740, f364])).
fof(f364, plain, ((e0 = op(e2, e2)) | ~ spl15_21), inference(avatar_component_clause, [], [f362])).
fof(f362, plain, (spl15_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f740, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl15_88), inference(avatar_component_clause, [], [f738])).
fof(f738, plain, (spl15_88 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_88])])).
fof(f2635, plain, (~ spl15_41 | ~ spl15_64 | spl15_90), inference(avatar_contradiction_clause, [], [f2634])).
fof(f2634, plain, ($false | (~ spl15_41 | ~ spl15_64 | spl15_90)), inference(subsumption_resolution, [], [f2633, f546])).
fof(f2633, plain, (~ (op(e0, e0) = e3) | (~ spl15_41 | spl15_90)), inference(forward_demodulation, [], [f749, f449])).
fof(f449, plain, ((e0 = op(e1, e1)) | ~ spl15_41), inference(avatar_component_clause, [], [f447])).
fof(f447, plain, (spl15_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_41])])).
fof(f749, plain, (~ (e3 = op(op(e1, e1), op(e1, e1))) | spl15_90), inference(avatar_component_clause, [], [f747])).
fof(f747, plain, (spl15_90 <=> (e3 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_90])])).
fof(f2626, plain, (~ spl15_5 | ~ spl15_13), inference(avatar_split_clause, [], [f2622, f328, f294])).
fof(f294, plain, (spl15_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f328, plain, (spl15_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f2622, plain, (~ (e0 = op(e3, e2)) | ~ spl15_13), inference(backward_demodulation, [], [f176, f330])).
fof(f330, plain, ((e0 = op(e3, e0)) | ~ spl15_13), inference(avatar_component_clause, [], [f328])).
fof(f176, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax3)).
fof(f2616, plain, (~ spl15_6 | ~ spl15_21 | ~ spl15_64 | spl15_89), inference(avatar_split_clause, [], [f2615, f742, f544, f362, f298])).
fof(f298, plain, (spl15_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_6])])).
fof(f742, plain, (spl15_89 <=> (e1 = op(op(op(e2, e2), op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_89])])).
fof(f2615, plain, (~ (e1 = op(e3, e2)) | (~ spl15_21 | ~ spl15_64 | spl15_89)), inference(forward_demodulation, [], [f2610, f546])).
fof(f2610, plain, (~ (e1 = op(op(e0, e0), e2)) | (~ spl15_21 | spl15_89)), inference(backward_demodulation, [], [f744, f364])).
fof(f744, plain, (~ (e1 = op(op(op(e2, e2), op(e2, e2)), e2)) | spl15_89), inference(avatar_component_clause, [], [f742])).
fof(f2614, plain, (~ spl15_17 | ~ spl15_21), inference(avatar_split_clause, [], [f2605, f362, f345])).
fof(f345, plain, (spl15_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_17])])).
fof(f2605, plain, (~ (e0 = op(e2, e3)) | ~ spl15_21), inference(backward_demodulation, [], [f174, f364])).
fof(f174, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2613, plain, (~ spl15_5 | ~ spl15_21), inference(avatar_split_clause, [], [f2604, f362, f294])).
fof(f2604, plain, (~ (e0 = op(e3, e2)) | ~ spl15_21), inference(backward_demodulation, [], [f150, f364])).
fof(f150, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2600, plain, (~ spl15_27 | ~ spl15_31), inference(avatar_split_clause, [], [f2595, f404, f387])).
fof(f387, plain, (spl15_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_27])])).
fof(f404, plain, (spl15_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_31])])).
fof(f2595, plain, (~ (e2 = op(e2, e1)) | ~ spl15_31), inference(backward_demodulation, [], [f169, f406])).
fof(f406, plain, ((e2 = op(e2, e0)) | ~ spl15_31), inference(avatar_component_clause, [], [f404])).
fof(f169, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2599, plain, (~ spl15_15 | ~ spl15_31), inference(avatar_split_clause, [], [f2594, f404, f336])).
fof(f336, plain, (spl15_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f2594, plain, (~ (e2 = op(e3, e0)) | ~ spl15_31), inference(backward_demodulation, [], [f138, f406])).
fof(f138, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2587, plain, (~ spl15_11 | ~ spl15_41 | ~ spl15_64 | spl15_91), inference(avatar_split_clause, [], [f2586, f751, f544, f447, f319])).
fof(f319, plain, (spl15_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f751, plain, (spl15_91 <=> (e2 = op(op(op(e1, e1), op(e1, e1)), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_91])])).
fof(f2586, plain, (~ (e2 = op(e3, e1)) | (~ spl15_41 | ~ spl15_64 | spl15_91)), inference(forward_demodulation, [], [f2583, f546])).
fof(f2583, plain, (~ (e2 = op(op(e0, e0), e1)) | (~ spl15_41 | spl15_91)), inference(backward_demodulation, [], [f753, f449])).
fof(f753, plain, (~ (e2 = op(op(op(e1, e1), op(e1, e1)), e1)) | spl15_91), inference(avatar_component_clause, [], [f751])).
fof(f2574, plain, (~ spl15_34 | ~ spl15_46), inference(avatar_split_clause, [], [f2570, f468, f417])).
fof(f417, plain, (spl15_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_34])])).
fof(f468, plain, (spl15_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_46])])).
fof(f2570, plain, (~ (e1 = op(e1, e3)) | ~ spl15_46), inference(backward_demodulation, [], [f165, f470])).
fof(f470, plain, ((e1 = op(e1, e0)) | ~ spl15_46), inference(avatar_component_clause, [], [f468])).
fof(f165, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2573, plain, (~ spl15_30 | ~ spl15_46), inference(avatar_split_clause, [], [f2566, f468, f400])).
fof(f400, plain, (spl15_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_30])])).
fof(f2566, plain, (~ (e1 = op(e2, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f136, f470])).
fof(f136, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2565, plain, (~ spl15_17 | ~ spl15_49), inference(avatar_split_clause, [], [f2561, f481, f345])).
fof(f481, plain, (spl15_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_49])])).
fof(f2561, plain, (~ (e0 = op(e2, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f152, f483])).
fof(f483, plain, ((e0 = op(e0, e3)) | ~ spl15_49), inference(avatar_component_clause, [], [f481])).
fof(f152, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2559, plain, (~ spl15_51 | ~ spl15_55), inference(avatar_split_clause, [], [f2556, f506, f489])).
fof(f489, plain, (spl15_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_51])])).
fof(f506, plain, (spl15_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_55])])).
fof(f2556, plain, (~ (e2 = op(e0, e3)) | ~ spl15_55), inference(backward_demodulation, [], [f162, f508])).
fof(f508, plain, ((e2 = op(e0, e2)) | ~ spl15_55), inference(avatar_component_clause, [], [f506])).
fof(f162, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2558, plain, (~ spl15_39 | ~ spl15_55), inference(avatar_split_clause, [], [f2553, f506, f438])).
fof(f438, plain, (spl15_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_39])])).
fof(f2553, plain, (~ (e2 = op(e1, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f145, f508])).
fof(f145, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2552, plain, (~ spl15_10 | ~ spl15_58), inference(avatar_split_clause, [], [f2547, f519, f315])).
fof(f315, plain, (spl15_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f519, plain, (spl15_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_58])])).
fof(f2547, plain, (~ (e1 = op(e3, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f141, f521])).
fof(f521, plain, ((e1 = op(e0, e1)) | ~ spl15_58), inference(avatar_component_clause, [], [f519])).
fof(f141, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2543, plain, (~ spl15_56 | ~ spl15_64), inference(avatar_split_clause, [], [f2537, f544, f510])).
fof(f510, plain, (spl15_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_56])])).
fof(f2537, plain, (~ (e3 = op(e0, e2)) | ~ spl15_64), inference(backward_demodulation, [], [f158, f546])).
fof(f158, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2518, plain, (~ spl15_2 | ~ spl15_10), inference(avatar_split_clause, [], [f2515, f315, f281])).
fof(f281, plain, (spl15_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_2])])).
fof(f2515, plain, (~ (e1 = op(e3, e3)) | ~ spl15_10), inference(backward_demodulation, [], [f179, f317])).
fof(f317, plain, ((e1 = op(e3, e1)) | ~ spl15_10), inference(avatar_component_clause, [], [f315])).
fof(f179, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2514, plain, (~ spl15_5 | ~ spl15_22 | ~ spl15_44 | spl15_97), inference(avatar_contradiction_clause, [], [f2513])).
fof(f2513, plain, ($false | (~ spl15_5 | ~ spl15_22 | ~ spl15_44 | spl15_97)), inference(subsumption_resolution, [], [f2512, f296])).
fof(f296, plain, ((e0 = op(e3, e2)) | ~ spl15_5), inference(avatar_component_clause, [], [f294])).
fof(f2512, plain, (~ (e0 = op(e3, e2)) | (~ spl15_22 | ~ spl15_44 | spl15_97)), inference(forward_demodulation, [], [f2507, f461])).
fof(f461, plain, ((e3 = op(e1, e1)) | ~ spl15_44), inference(avatar_component_clause, [], [f459])).
fof(f459, plain, (spl15_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_44])])).
fof(f2507, plain, (~ (e0 = op(op(e1, e1), e2)) | (~ spl15_22 | spl15_97)), inference(backward_demodulation, [], [f782, f368])).
fof(f368, plain, ((e1 = op(e2, e2)) | ~ spl15_22), inference(avatar_component_clause, [], [f366])).
fof(f366, plain, (spl15_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_22])])).
fof(f782, plain, (~ (e0 = op(op(op(e2, e2), op(e2, e2)), e2)) | spl15_97), inference(avatar_component_clause, [], [f780])).
fof(f780, plain, (spl15_97 <=> (e0 = op(op(op(e2, e2), op(e2, e2)), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_97])])).
fof(f2509, plain, (~ spl15_22 | ~ spl15_44 | spl15_88), inference(avatar_contradiction_clause, [], [f2508])).
fof(f2508, plain, ($false | (~ spl15_22 | ~ spl15_44 | spl15_88)), inference(subsumption_resolution, [], [f2504, f461])).
fof(f2504, plain, (~ (e3 = op(e1, e1)) | (~ spl15_22 | spl15_88)), inference(backward_demodulation, [], [f740, f368])).
fof(f2500, plain, (~ spl15_23 | ~ spl15_27), inference(avatar_split_clause, [], [f2498, f387, f370])).
fof(f370, plain, (spl15_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_23])])).
fof(f2498, plain, (~ (e2 = op(e2, e2)) | ~ spl15_27), inference(backward_demodulation, [], [f172, f389])).
fof(f389, plain, ((e2 = op(e2, e1)) | ~ spl15_27), inference(avatar_component_clause, [], [f387])).
fof(f172, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2496, plain, (~ spl15_2 | ~ spl15_34), inference(avatar_split_clause, [], [f2493, f417, f281])).
fof(f2493, plain, (~ (e1 = op(e3, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f155, f419])).
fof(f419, plain, ((e1 = op(e1, e3)) | ~ spl15_34), inference(avatar_component_clause, [], [f417])).
fof(f155, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2492, plain, (~ spl15_23 | ~ spl15_39), inference(avatar_split_clause, [], [f2489, f438, f370])).
fof(f2489, plain, (~ (e2 = op(e2, e2)) | ~ spl15_39), inference(backward_demodulation, [], [f148, f440])).
fof(f440, plain, ((e2 = op(e1, e2)) | ~ spl15_39), inference(avatar_component_clause, [], [f438])).
fof(f148, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2473, plain, (~ spl15_43 | ~ spl15_62 | spl15_95), inference(avatar_split_clause, [], [f2472, f771, f536, f455])).
fof(f455, plain, (spl15_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_43])])).
fof(f536, plain, (spl15_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl15_62])])).
fof(f771, plain, (spl15_95 <=> (e2 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_95])])).
fof(f2472, plain, (~ (e2 = op(e1, e1)) | (~ spl15_62 | spl15_95)), inference(forward_demodulation, [], [f773, f538])).
fof(f538, plain, ((op(e0, e0) = e1) | ~ spl15_62), inference(avatar_component_clause, [], [f536])).
fof(f773, plain, (~ (e2 = op(op(e0, e0), op(e0, e0))) | spl15_95), inference(avatar_component_clause, [], [f771])).
fof(f2444, plain, (~ spl15_8 | ~ spl15_12), inference(avatar_split_clause, [], [f2442, f323, f306])).
fof(f306, plain, (spl15_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f323, plain, (spl15_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f2442, plain, (~ (e3 = op(e3, e2)) | ~ spl15_12), inference(backward_demodulation, [], [f178, f325])).
fof(f325, plain, ((e3 = op(e3, e1)) | ~ spl15_12), inference(avatar_component_clause, [], [f323])).
fof(f178, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2441, plain, (~ spl15_3 | ~ spl15_15), inference(avatar_split_clause, [], [f2439, f336, f285])).
fof(f285, plain, (spl15_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f2439, plain, (~ (e2 = op(e3, e3)) | ~ spl15_15), inference(backward_demodulation, [], [f177, f338])).
fof(f338, plain, ((e2 = op(e3, e0)) | ~ spl15_15), inference(avatar_component_clause, [], [f336])).
fof(f177, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2415, plain, (~ spl15_22 | ~ spl15_26), inference(avatar_split_clause, [], [f2411, f383, f366])).
fof(f383, plain, (spl15_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_26])])).
fof(f2411, plain, (~ (e1 = op(e2, e2)) | ~ spl15_26), inference(backward_demodulation, [], [f172, f385])).
fof(f385, plain, ((e1 = op(e2, e1)) | ~ spl15_26), inference(avatar_component_clause, [], [f383])).
fof(f2409, plain, (~ spl15_20 | ~ spl15_32), inference(avatar_split_clause, [], [f2408, f408, f357])).
fof(f357, plain, (spl15_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f408, plain, (spl15_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_32])])).
fof(f2408, plain, (~ (e3 = op(e2, e3)) | ~ spl15_32), inference(backward_demodulation, [], [f171, f410])).
fof(f410, plain, ((e3 = op(e2, e0)) | ~ spl15_32), inference(avatar_component_clause, [], [f408])).
fof(f171, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2404, plain, (~ spl15_20 | ~ spl15_36), inference(avatar_split_clause, [], [f2402, f425, f357])).
fof(f425, plain, (spl15_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_36])])).
fof(f2402, plain, (~ (e3 = op(e2, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f154, f427])).
fof(f427, plain, ((e3 = op(e1, e3)) | ~ spl15_36), inference(avatar_component_clause, [], [f425])).
fof(f154, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2401, plain, (~ spl15_22 | ~ spl15_38), inference(avatar_split_clause, [], [f2396, f434, f366])).
fof(f434, plain, (spl15_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_38])])).
fof(f2396, plain, (~ (e1 = op(e2, e2)) | ~ spl15_38), inference(backward_demodulation, [], [f148, f436])).
fof(f436, plain, ((e1 = op(e1, e2)) | ~ spl15_38), inference(avatar_component_clause, [], [f434])).
fof(f2394, plain, (~ spl15_27 | ~ spl15_43), inference(avatar_split_clause, [], [f2386, f455, f387])).
fof(f2386, plain, (~ (e2 = op(e2, e1)) | ~ spl15_43), inference(backward_demodulation, [], [f142, f457])).
fof(f457, plain, ((e2 = op(e1, e1)) | ~ spl15_43), inference(avatar_component_clause, [], [f455])).
fof(f142, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2385, plain, (~ spl15_33 | ~ spl15_45), inference(avatar_split_clause, [], [f2380, f464, f413])).
fof(f413, plain, (spl15_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_33])])).
fof(f464, plain, (spl15_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_45])])).
fof(f2380, plain, (~ (e0 = op(e1, e3)) | ~ spl15_45), inference(backward_demodulation, [], [f165, f466])).
fof(f466, plain, ((e0 = op(e1, e0)) | ~ spl15_45), inference(avatar_component_clause, [], [f464])).
fof(f2384, plain, (~ spl15_29 | ~ spl15_45), inference(avatar_split_clause, [], [f2376, f464, f396])).
fof(f396, plain, (spl15_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_29])])).
fof(f2376, plain, (~ (e0 = op(e2, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f136, f466])).
fof(f2375, plain, (~ spl15_3 | ~ spl15_51), inference(avatar_split_clause, [], [f2373, f489, f285])).
fof(f2373, plain, (~ (e2 = op(e3, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f153, f491])).
fof(f491, plain, ((e2 = op(e0, e3)) | ~ spl15_51), inference(avatar_component_clause, [], [f489])).
fof(f153, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2365, plain, (~ spl15_53 | ~ spl15_57), inference(avatar_split_clause, [], [f2359, f515, f498])).
fof(f498, plain, (spl15_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_53])])).
fof(f515, plain, (spl15_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_57])])).
fof(f2359, plain, (~ (e0 = op(e0, e2)) | ~ spl15_57), inference(backward_demodulation, [], [f160, f517])).
fof(f517, plain, ((e0 = op(e0, e1)) | ~ spl15_57), inference(avatar_component_clause, [], [f515])).
fof(f160, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2364, plain, (~ spl15_9 | ~ spl15_57), inference(avatar_split_clause, [], [f2358, f515, f311])).
fof(f311, plain, (spl15_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f2358, plain, (~ (e0 = op(e3, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f141, f517])).
fof(f2353, plain, (~ spl15_50 | ~ spl15_62), inference(avatar_split_clause, [], [f2347, f536, f485])).
fof(f485, plain, (spl15_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_50])])).
fof(f2347, plain, (~ (e1 = op(e0, e3)) | ~ spl15_62), inference(backward_demodulation, [], [f159, f538])).
fof(f159, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2352, plain, (~ spl15_14 | ~ spl15_62), inference(avatar_split_clause, [], [f2344, f536, f332])).
fof(f332, plain, (spl15_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f2344, plain, (~ (e1 = op(e3, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f135, f538])).
fof(f135, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2341, plain, (~ spl15_32 | ~ spl15_95 | spl15_96), inference(avatar_split_clause, [], [f2338, f775, f771, f408])).
fof(f775, plain, (spl15_96 <=> (e3 = op(op(op(e0, e0), op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_96])])).
fof(f2338, plain, (~ (e3 = op(e2, e0)) | (~ spl15_95 | spl15_96)), inference(backward_demodulation, [], [f777, f772])).
fof(f772, plain, ((e2 = op(op(e0, e0), op(e0, e0))) | ~ spl15_95), inference(avatar_component_clause, [], [f771])).
fof(f777, plain, (~ (e3 = op(op(op(e0, e0), op(e0, e0)), e0)) | spl15_96), inference(avatar_component_clause, [], [f775])).
fof(f2337, plain, (~ spl15_3 | ~ spl15_22 | ~ spl15_33 | spl15_94), inference(avatar_contradiction_clause, [], [f2336])).
fof(f2336, plain, ($false | (~ spl15_3 | ~ spl15_22 | ~ spl15_33 | spl15_94)), inference(subsumption_resolution, [], [f2335, f415])).
fof(f415, plain, ((e0 = op(e1, e3)) | ~ spl15_33), inference(avatar_component_clause, [], [f413])).
fof(f2335, plain, (~ (e0 = op(e1, e3)) | (~ spl15_3 | ~ spl15_22 | spl15_94)), inference(forward_demodulation, [], [f2325, f368])).
fof(f2325, plain, (~ (e0 = op(op(e2, e2), e3)) | (~ spl15_3 | spl15_94)), inference(backward_demodulation, [], [f768, f287])).
fof(f287, plain, ((e2 = op(e3, e3)) | ~ spl15_3), inference(avatar_component_clause, [], [f285])).
fof(f768, plain, (~ (e0 = op(op(op(e3, e3), op(e3, e3)), e3)) | spl15_94), inference(avatar_component_clause, [], [f766])).
fof(f766, plain, (spl15_94 <=> (e0 = op(op(op(e3, e3), op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_94])])).
fof(f2327, plain, (~ spl15_3 | ~ spl15_22 | spl15_80), inference(avatar_contradiction_clause, [], [f2326])).
fof(f2326, plain, ($false | (~ spl15_3 | ~ spl15_22 | spl15_80)), inference(subsumption_resolution, [], [f2320, f368])).
fof(f2320, plain, (~ (e1 = op(e2, e2)) | (~ spl15_3 | spl15_80)), inference(backward_demodulation, [], [f704, f287])).
fof(f704, plain, (~ (e1 = op(op(e3, e3), op(e3, e3))) | spl15_80), inference(avatar_component_clause, [], [f702])).
fof(f702, plain, (spl15_80 <=> (e1 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_80])])).
fof(f2311, plain, (~ spl15_40 | ~ spl15_48), inference(avatar_split_clause, [], [f2310, f476, f442])).
fof(f442, plain, (spl15_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_40])])).
fof(f476, plain, (spl15_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_48])])).
fof(f2310, plain, (~ (e3 = op(e1, e2)) | ~ spl15_48), inference(backward_demodulation, [], [f164, f478])).
fof(f478, plain, ((e3 = op(e1, e0)) | ~ spl15_48), inference(avatar_component_clause, [], [f476])).
fof(f164, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2291, plain, (~ spl15_47 | ~ spl15_63), inference(avatar_split_clause, [], [f2290, f540, f472])).
fof(f472, plain, (spl15_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_47])])).
fof(f540, plain, (spl15_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl15_63])])).
fof(f2290, plain, (~ (e2 = op(e1, e0)) | ~ spl15_63), inference(forward_demodulation, [], [f133, f542])).
fof(f542, plain, ((op(e0, e0) = e2) | ~ spl15_63), inference(avatar_component_clause, [], [f540])).
fof(f133, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2264, plain, (~ spl15_19 | ~ spl15_27), inference(avatar_split_clause, [], [f2263, f387, f353])).
fof(f353, plain, (spl15_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_19])])).
fof(f2263, plain, (~ (e2 = op(e2, e3)) | ~ spl15_27), inference(forward_demodulation, [], [f173, f389])).
fof(f173, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2249, plain, (~ spl15_1 | ~ spl15_33), inference(avatar_split_clause, [], [f2248, f413, f277])).
fof(f277, plain, (spl15_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f2248, plain, (~ (e0 = op(e3, e3)) | ~ spl15_33), inference(forward_demodulation, [], [f155, f415])).
fof(f2244, plain, (~ spl15_14 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f2243])).
fof(f2243, plain, ($false | (~ spl15_14 | ~ spl15_16)), inference(subsumption_resolution, [], [f2241, f185])).
fof(f185, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax4)).
fof(f2241, plain, ((e1 = e3) | (~ spl15_14 | ~ spl15_16)), inference(backward_demodulation, [], [f342, f334])).
fof(f334, plain, ((e1 = op(e3, e0)) | ~ spl15_14), inference(avatar_component_clause, [], [f332])).
fof(f342, plain, ((e3 = op(e3, e0)) | ~ spl15_16), inference(avatar_component_clause, [], [f340])).
fof(f340, plain, (spl15_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f2234, plain, (~ spl15_25 | ~ spl15_29), inference(avatar_split_clause, [], [f2231, f396, f379])).
fof(f379, plain, (spl15_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_25])])).
fof(f2231, plain, (~ (e0 = op(e2, e1)) | ~ spl15_29), inference(backward_demodulation, [], [f169, f398])).
fof(f398, plain, ((e0 = op(e2, e0)) | ~ spl15_29), inference(avatar_component_clause, [], [f396])).
fof(f2220, plain, (~ spl15_34 | ~ spl15_42), inference(avatar_split_clause, [], [f2214, f451, f417])).
fof(f451, plain, (spl15_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_42])])).
fof(f2214, plain, (~ (e1 = op(e1, e3)) | ~ spl15_42), inference(backward_demodulation, [], [f167, f453])).
fof(f453, plain, ((e1 = op(e1, e1)) | ~ spl15_42), inference(avatar_component_clause, [], [f451])).
fof(f167, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2219, plain, (~ spl15_10 | ~ spl15_42), inference(avatar_split_clause, [], [f2212, f451, f315])).
fof(f2212, plain, (~ (e1 = op(e3, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f143, f453])).
fof(f143, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2210, plain, (~ spl15_50 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f2209])).
fof(f2209, plain, ($false | (~ spl15_50 | ~ spl15_52)), inference(subsumption_resolution, [], [f2207, f185])).
fof(f2207, plain, ((e1 = e3) | (~ spl15_50 | ~ spl15_52)), inference(backward_demodulation, [], [f495, f487])).
fof(f487, plain, ((e1 = op(e0, e3)) | ~ spl15_50), inference(avatar_component_clause, [], [f485])).
fof(f495, plain, ((e3 = op(e0, e3)) | ~ spl15_52), inference(avatar_component_clause, [], [f493])).
fof(f493, plain, (spl15_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_52])])).
fof(f2206, plain, (~ spl15_37 | ~ spl15_53), inference(avatar_split_clause, [], [f2204, f498, f430])).
fof(f430, plain, (spl15_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_37])])).
fof(f2204, plain, (~ (e0 = op(e1, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f145, f500])).
fof(f500, plain, ((e0 = op(e0, e2)) | ~ spl15_53), inference(avatar_component_clause, [], [f498])).
fof(f2201, plain, (~ spl15_44 | ~ spl15_60), inference(avatar_split_clause, [], [f2197, f527, f459])).
fof(f527, plain, (spl15_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_60])])).
fof(f2197, plain, (~ (e3 = op(e1, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f139, f529])).
fof(f529, plain, ((e3 = op(e0, e1)) | ~ spl15_60), inference(avatar_component_clause, [], [f527])).
fof(f139, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2196, plain, (~ spl15_14 | ~ spl15_24 | ~ spl15_63 | spl15_103), inference(avatar_split_clause, [], [f2195, f811, f540, f374, f332])).
fof(f374, plain, (spl15_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_24])])).
fof(f811, plain, (spl15_103 <=> (e1 = op(op(op(e0, e0), op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_103])])).
fof(f2195, plain, (~ (e1 = op(e3, e0)) | (~ spl15_24 | ~ spl15_63 | spl15_103)), inference(forward_demodulation, [], [f2184, f376])).
fof(f376, plain, ((e3 = op(e2, e2)) | ~ spl15_24), inference(avatar_component_clause, [], [f374])).
fof(f2184, plain, (~ (e1 = op(op(e2, e2), e0)) | (~ spl15_63 | spl15_103)), inference(backward_demodulation, [], [f813, f542])).
fof(f813, plain, (~ (e1 = op(op(op(e0, e0), op(e0, e0)), e0)) | spl15_103), inference(avatar_component_clause, [], [f811])).
fof(f2193, plain, (~ spl15_24 | ~ spl15_63 | spl15_98), inference(avatar_contradiction_clause, [], [f2192])).
fof(f2192, plain, ($false | (~ spl15_24 | ~ spl15_63 | spl15_98)), inference(subsumption_resolution, [], [f2182, f376])).
fof(f2182, plain, (~ (e3 = op(e2, e2)) | (~ spl15_63 | spl15_98)), inference(backward_demodulation, [], [f787, f542])).
fof(f787, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl15_98), inference(avatar_component_clause, [], [f785])).
fof(f785, plain, (spl15_98 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_98])])).
fof(f2188, plain, (~ spl15_59 | ~ spl15_63), inference(avatar_split_clause, [], [f2177, f540, f523])).
fof(f523, plain, (spl15_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_59])])).
fof(f2177, plain, (~ (e2 = op(e0, e1)) | ~ spl15_63), inference(backward_demodulation, [], [f157, f542])).
fof(f157, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2172, plain, (~ spl15_54 | ~ spl15_1 | ~ spl15_24 | spl15_89), inference(avatar_split_clause, [], [f2171, f742, f374, f277, f502])).
fof(f502, plain, (spl15_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_54])])).
fof(f2171, plain, (~ (e1 = op(e0, e2)) | (~ spl15_1 | ~ spl15_24 | spl15_89)), inference(forward_demodulation, [], [f2170, f279])).
fof(f279, plain, ((e0 = op(e3, e3)) | ~ spl15_1), inference(avatar_component_clause, [], [f277])).
fof(f2170, plain, (~ (e1 = op(op(e3, e3), e2)) | (~ spl15_24 | spl15_89)), inference(forward_demodulation, [], [f744, f376])).
fof(f2163, plain, (~ spl15_64 | ~ spl15_16), inference(avatar_split_clause, [], [f2162, f340, f544])).
fof(f2162, plain, (~ (op(e0, e0) = e3) | ~ spl15_16), inference(forward_demodulation, [], [f135, f342])).
fof(f2161, plain, (~ spl15_62 | ~ spl15_1 | spl15_80), inference(avatar_split_clause, [], [f2105, f702, f277, f536])).
fof(f2105, plain, (~ (op(e0, e0) = e1) | (~ spl15_1 | spl15_80)), inference(backward_demodulation, [], [f704, f279])).
fof(f2160, plain, (~ spl15_63 | ~ spl15_1 | spl15_84), inference(avatar_split_clause, [], [f2106, f720, f277, f540])).
fof(f720, plain, (spl15_84 <=> (e2 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_84])])).
fof(f2106, plain, (~ (op(e0, e0) = e2) | (~ spl15_1 | spl15_84)), inference(backward_demodulation, [], [f722, f279])).
fof(f722, plain, (~ (e2 = op(op(e3, e3), op(e3, e3))) | spl15_84), inference(avatar_component_clause, [], [f720])).
fof(f2159, plain, (~ spl15_60 | ~ spl15_52), inference(avatar_split_clause, [], [f2158, f493, f527])).
fof(f2158, plain, (~ (e3 = op(e0, e1)) | ~ spl15_52), inference(forward_demodulation, [], [f161, f495])).
fof(f161, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2155, plain, (~ spl15_56 | ~ spl15_24), inference(avatar_split_clause, [], [f2154, f374, f510])).
fof(f2154, plain, (~ (e3 = op(e0, e2)) | ~ spl15_24), inference(forward_demodulation, [], [f146, f376])).
fof(f146, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2151, plain, (~ spl15_40 | ~ spl15_24), inference(avatar_split_clause, [], [f2150, f374, f442])).
fof(f2150, plain, (~ (e3 = op(e1, e2)) | ~ spl15_24), inference(forward_demodulation, [], [f148, f376])).
fof(f2132, plain, (~ spl15_1 | ~ spl15_24 | spl15_93), inference(avatar_contradiction_clause, [], [f2131])).
fof(f2131, plain, ($false | (~ spl15_1 | ~ spl15_24 | spl15_93)), inference(subsumption_resolution, [], [f2130, f279])).
fof(f2130, plain, (~ (e0 = op(e3, e3)) | (~ spl15_24 | spl15_93)), inference(forward_demodulation, [], [f763, f376])).
fof(f763, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl15_93), inference(avatar_component_clause, [], [f761])).
fof(f761, plain, (spl15_93 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_93])])).
fof(f2129, plain, (~ spl15_1 | ~ spl15_44 | spl15_100), inference(avatar_contradiction_clause, [], [f2128])).
fof(f2128, plain, ($false | (~ spl15_1 | ~ spl15_44 | spl15_100)), inference(subsumption_resolution, [], [f2127, f279])).
fof(f2127, plain, (~ (e0 = op(e3, e3)) | (~ spl15_44 | spl15_100)), inference(forward_demodulation, [], [f797, f461])).
fof(f797, plain, (~ (e0 = op(op(e1, e1), op(e1, e1))) | spl15_100), inference(avatar_component_clause, [], [f795])).
fof(f795, plain, (spl15_100 <=> (e0 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_100])])).
fof(f2122, plain, (~ spl15_1 | ~ spl15_44 | ~ spl15_59 | spl15_91), inference(avatar_contradiction_clause, [], [f2121])).
fof(f2121, plain, ($false | (~ spl15_1 | ~ spl15_44 | ~ spl15_59 | spl15_91)), inference(subsumption_resolution, [], [f2110, f525])).
fof(f525, plain, ((e2 = op(e0, e1)) | ~ spl15_59), inference(avatar_component_clause, [], [f523])).
fof(f2110, plain, (~ (e2 = op(e0, e1)) | (~ spl15_1 | ~ spl15_44 | spl15_91)), inference(backward_demodulation, [], [f2059, f279])).
fof(f2059, plain, (~ (e2 = op(op(e3, e3), e1)) | (~ spl15_44 | spl15_91)), inference(backward_demodulation, [], [f753, f461])).
fof(f2097, plain, (~ spl15_12 | ~ spl15_16), inference(avatar_split_clause, [], [f2095, f340, f323])).
fof(f2095, plain, (~ (e3 = op(e3, e1)) | ~ spl15_16), inference(backward_demodulation, [], [f175, f342])).
fof(f175, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2068, plain, (~ spl15_33 | ~ spl15_37), inference(avatar_split_clause, [], [f2063, f430, f413])).
fof(f2063, plain, (~ (e0 = op(e1, e3)) | ~ spl15_37), inference(backward_demodulation, [], [f168, f432])).
fof(f432, plain, ((e0 = op(e1, e2)) | ~ spl15_37), inference(avatar_component_clause, [], [f430])).
fof(f168, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2067, plain, (~ spl15_21 | ~ spl15_37), inference(avatar_split_clause, [], [f2062, f430, f362])).
fof(f2062, plain, (~ (e0 = op(e2, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f148, f432])).
fof(f2053, plain, (~ spl15_35 | ~ spl15_47), inference(avatar_split_clause, [], [f2050, f472, f421])).
fof(f421, plain, (spl15_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_35])])).
fof(f2050, plain, (~ (e2 = op(e1, e3)) | ~ spl15_47), inference(backward_demodulation, [], [f165, f474])).
fof(f474, plain, ((e2 = op(e1, e0)) | ~ spl15_47), inference(avatar_component_clause, [], [f472])).
fof(f2045, plain, (~ spl15_20 | ~ spl15_52), inference(avatar_split_clause, [], [f2042, f493, f357])).
fof(f2042, plain, (~ (e3 = op(e2, e3)) | ~ spl15_52), inference(backward_demodulation, [], [f152, f495])).
fof(f2040, plain, (~ spl15_38 | ~ spl15_54), inference(avatar_split_clause, [], [f2036, f502, f434])).
fof(f2036, plain, (~ (e1 = op(e1, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f145, f504])).
fof(f504, plain, ((e1 = op(e0, e2)) | ~ spl15_54), inference(avatar_component_clause, [], [f502])).
fof(f2032, plain, (~ spl15_13 | ~ spl15_61), inference(avatar_split_clause, [], [f2031, f532, f328])).
fof(f532, plain, (spl15_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_61])])).
fof(f2031, plain, (~ (e0 = op(e3, e0)) | ~ spl15_61), inference(forward_demodulation, [], [f135, f534])).
fof(f534, plain, ((e0 = op(e0, e0)) | ~ spl15_61), inference(avatar_component_clause, [], [f532])).
fof(f2028, plain, (~ spl15_3 | ~ spl15_7), inference(avatar_split_clause, [], [f2027, f302, f285])).
fof(f302, plain, (spl15_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f2027, plain, (~ (e2 = op(e3, e3)) | ~ spl15_7), inference(forward_demodulation, [], [f180, f304])).
fof(f304, plain, ((e2 = op(e3, e2)) | ~ spl15_7), inference(avatar_component_clause, [], [f302])).
fof(f180, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2008, plain, (~ spl15_19 | ~ spl15_31), inference(avatar_split_clause, [], [f2006, f404, f353])).
fof(f2006, plain, (~ (e2 = op(e2, e3)) | ~ spl15_31), inference(backward_demodulation, [], [f171, f406])).
fof(f2003, plain, (~ spl15_33 | ~ spl15_35), inference(avatar_contradiction_clause, [], [f2002])).
fof(f2002, plain, ($false | (~ spl15_33 | ~ spl15_35)), inference(subsumption_resolution, [], [f2000, f182])).
fof(f182, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2000, plain, ((e0 = e2) | (~ spl15_33 | ~ spl15_35)), inference(backward_demodulation, [], [f423, f415])).
fof(f423, plain, ((e2 = op(e1, e3)) | ~ spl15_35), inference(avatar_component_clause, [], [f421])).
fof(f1999, plain, (~ spl15_19 | ~ spl15_35), inference(avatar_split_clause, [], [f1998, f421, f353])).
fof(f1998, plain, (~ (e2 = op(e2, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f154, f423])).
fof(f1986, plain, (~ spl15_37 | ~ spl15_41), inference(avatar_split_clause, [], [f1976, f447, f430])).
fof(f1976, plain, (~ (e0 = op(e1, e2)) | ~ spl15_41), inference(backward_demodulation, [], [f166, f449])).
fof(f166, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1974, plain, (~ spl15_46 | ~ spl15_48), inference(avatar_contradiction_clause, [], [f1973])).
fof(f1973, plain, ($false | (~ spl15_46 | ~ spl15_48)), inference(subsumption_resolution, [], [f1972, f185])).
fof(f1972, plain, ((e1 = e3) | (~ spl15_46 | ~ spl15_48)), inference(backward_demodulation, [], [f478, f470])).
fof(f1970, plain, (~ spl15_36 | ~ spl15_48), inference(avatar_split_clause, [], [f1969, f476, f425])).
fof(f1969, plain, (~ (e3 = op(e1, e3)) | ~ spl15_48), inference(backward_demodulation, [], [f165, f478])).
fof(f1949, plain, (~ spl15_49 | ~ spl15_61), inference(avatar_split_clause, [], [f1940, f532, f481])).
fof(f1940, plain, (~ (e0 = op(e0, e3)) | ~ spl15_61), inference(backward_demodulation, [], [f159, f534])).
fof(f1928, plain, (~ spl15_50 | ~ spl15_2), inference(avatar_split_clause, [], [f1927, f281, f485])).
fof(f1927, plain, (~ (e1 = op(e0, e3)) | ~ spl15_2), inference(forward_demodulation, [], [f153, f283])).
fof(f283, plain, ((e1 = op(e3, e3)) | ~ spl15_2), inference(avatar_component_clause, [], [f281])).
fof(f1915, plain, (~ spl15_2 | ~ spl15_24 | ~ spl15_37 | spl15_97), inference(avatar_contradiction_clause, [], [f1914])).
fof(f1914, plain, ($false | (~ spl15_2 | ~ spl15_24 | ~ spl15_37 | spl15_97)), inference(subsumption_resolution, [], [f1913, f432])).
fof(f1913, plain, (~ (e0 = op(e1, e2)) | (~ spl15_2 | ~ spl15_24 | spl15_97)), inference(forward_demodulation, [], [f1904, f283])).
fof(f1904, plain, (~ (e0 = op(op(e3, e3), e2)) | (~ spl15_24 | spl15_97)), inference(backward_demodulation, [], [f782, f376])).
fof(f1907, plain, (~ spl15_2 | ~ spl15_24 | spl15_82), inference(avatar_contradiction_clause, [], [f1906])).
fof(f1906, plain, ($false | (~ spl15_2 | ~ spl15_24 | spl15_82)), inference(subsumption_resolution, [], [f1900, f283])).
fof(f1900, plain, (~ (e1 = op(e3, e3)) | (~ spl15_24 | spl15_82)), inference(backward_demodulation, [], [f713, f376])).
fof(f713, plain, (~ (e1 = op(op(e2, e2), op(e2, e2))) | spl15_82), inference(avatar_component_clause, [], [f711])).
fof(f711, plain, (spl15_82 <=> (e1 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_82])])).
fof(f1873, plain, (~ spl15_26 | ~ spl15_42), inference(avatar_split_clause, [], [f1864, f451, f383])).
fof(f1864, plain, (~ (e1 = op(e2, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f142, f453])).
fof(f1862, plain, (~ spl15_15 | ~ spl15_47), inference(avatar_split_clause, [], [f1857, f472, f336])).
fof(f1857, plain, (~ (e2 = op(e3, e0)) | ~ spl15_47), inference(backward_demodulation, [], [f137, f474])).
fof(f137, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f1852, plain, (~ spl15_47 | ~ spl15_2 | ~ spl15_64 | spl15_99), inference(avatar_split_clause, [], [f1821, f789, f544, f281, f472])).
fof(f789, plain, (spl15_99 <=> (e2 = op(op(op(e0, e0), op(e0, e0)), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_99])])).
fof(f1821, plain, (~ (e2 = op(e1, e0)) | (~ spl15_2 | ~ spl15_64 | spl15_99)), inference(backward_demodulation, [], [f1665, f283])).
fof(f1665, plain, (~ (e2 = op(op(e3, e3), e0)) | (~ spl15_64 | spl15_99)), inference(forward_demodulation, [], [f791, f546])).
fof(f791, plain, (~ (e2 = op(op(op(e0, e0), op(e0, e0)), e0)) | spl15_99), inference(avatar_component_clause, [], [f789])).
fof(f1847, plain, (~ spl15_41 | ~ spl15_2 | spl15_92), inference(avatar_split_clause, [], [f1819, f756, f281, f447])).
fof(f756, plain, (spl15_92 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_92])])).
fof(f1819, plain, (~ (e0 = op(e1, e1)) | (~ spl15_2 | spl15_92)), inference(backward_demodulation, [], [f758, f283])).
fof(f758, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl15_92), inference(avatar_component_clause, [], [f756])).
fof(f1845, plain, (~ spl15_28 | ~ spl15_12), inference(avatar_split_clause, [], [f1844, f323, f391])).
fof(f391, plain, (spl15_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_28])])).
fof(f1844, plain, (~ (e3 = op(e2, e1)) | ~ spl15_12), inference(forward_demodulation, [], [f144, f325])).
fof(f144, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1801, plain, (~ spl15_3 | ~ spl15_35), inference(avatar_split_clause, [], [f1799, f421, f285])).
fof(f1799, plain, (~ (e2 = op(e3, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f155, f423])).
fof(f1783, plain, (~ spl15_14 | ~ spl15_46), inference(avatar_split_clause, [], [f1777, f468, f332])).
fof(f1777, plain, (~ (e1 = op(e3, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f137, f470])).
fof(f1762, plain, (~ spl15_58 | ~ spl15_26), inference(avatar_split_clause, [], [f1761, f383, f519])).
fof(f1761, plain, (~ (e1 = op(e0, e1)) | ~ spl15_26), inference(forward_demodulation, [], [f140, f385])).
fof(f140, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1757, plain, (~ spl15_10 | ~ spl15_26), inference(avatar_split_clause, [], [f1756, f383, f315])).
fof(f1756, plain, (~ (e1 = op(e3, e1)) | ~ spl15_26), inference(forward_demodulation, [], [f144, f385])).
fof(f1754, plain, (~ spl15_13 | ~ spl15_29), inference(avatar_split_clause, [], [f1753, f396, f328])).
fof(f1753, plain, (~ (e0 = op(e3, e0)) | ~ spl15_29), inference(forward_demodulation, [], [f138, f398])).
fof(f1751, plain, (~ spl15_13 | ~ spl15_14), inference(avatar_contradiction_clause, [], [f1750])).
fof(f1750, plain, ($false | (~ spl15_13 | ~ spl15_14)), inference(subsumption_resolution, [], [f1749, f181])).
fof(f181, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1749, plain, ((e0 = e1) | (~ spl15_13 | ~ spl15_14)), inference(forward_demodulation, [], [f334, f330])).
fof(f1748, plain, (~ spl15_25 | ~ spl15_26), inference(avatar_contradiction_clause, [], [f1747])).
fof(f1747, plain, ($false | (~ spl15_25 | ~ spl15_26)), inference(subsumption_resolution, [], [f1746, f181])).
fof(f1746, plain, ((e0 = e1) | (~ spl15_25 | ~ spl15_26)), inference(forward_demodulation, [], [f385, f381])).
fof(f381, plain, ((e0 = op(e2, e1)) | ~ spl15_25), inference(avatar_component_clause, [], [f379])).
fof(f1739, plain, (~ spl15_3 | ~ spl15_44 | spl15_86), inference(avatar_contradiction_clause, [], [f1738])).
fof(f1738, plain, ($false | (~ spl15_3 | ~ spl15_44 | spl15_86)), inference(subsumption_resolution, [], [f1737, f287])).
fof(f1737, plain, (~ (e2 = op(e3, e3)) | (~ spl15_44 | spl15_86)), inference(forward_demodulation, [], [f731, f461])).
fof(f731, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl15_86), inference(avatar_component_clause, [], [f729])).
fof(f729, plain, (spl15_86 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_86])])).
fof(f1712, plain, (~ spl15_3 | ~ spl15_25 | ~ spl15_44 | spl15_102), inference(avatar_contradiction_clause, [], [f1711])).
fof(f1711, plain, ($false | (~ spl15_3 | ~ spl15_25 | ~ spl15_44 | spl15_102)), inference(subsumption_resolution, [], [f1704, f381])).
fof(f1704, plain, (~ (e0 = op(e2, e1)) | (~ spl15_3 | ~ spl15_44 | spl15_102)), inference(backward_demodulation, [], [f1523, f287])).
fof(f1523, plain, (~ (e0 = op(op(e3, e3), e1)) | (~ spl15_44 | spl15_102)), inference(backward_demodulation, [], [f808, f461])).
fof(f808, plain, (~ (e0 = op(op(op(e1, e1), op(e1, e1)), e1)) | spl15_102), inference(avatar_component_clause, [], [f806])).
fof(f806, plain, (spl15_102 <=> (e0 = op(op(op(e1, e1), op(e1, e1)), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_102])])).
fof(f1695, plain, (~ spl15_5 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1694])).
fof(f1694, plain, ($false | (~ spl15_5 | ~ spl15_8)), inference(subsumption_resolution, [], [f1693, f183])).
fof(f183, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f1693, plain, ((e0 = e3) | (~ spl15_5 | ~ spl15_8)), inference(forward_demodulation, [], [f308, f296])).
fof(f308, plain, ((e3 = op(e3, e2)) | ~ spl15_8), inference(avatar_component_clause, [], [f306])).
fof(f1679, plain, (~ spl15_46 | ~ spl15_47), inference(avatar_contradiction_clause, [], [f1678])).
fof(f1678, plain, ($false | (~ spl15_46 | ~ spl15_47)), inference(subsumption_resolution, [], [f1677, f184])).
fof(f184, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1677, plain, ((e1 = e2) | (~ spl15_46 | ~ spl15_47)), inference(backward_demodulation, [], [f474, f470])).
fof(f1672, plain, (~ spl15_2 | ~ spl15_64 | spl15_101), inference(avatar_split_clause, [], [f1671, f801, f544, f281])).
fof(f801, plain, (spl15_101 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_101])])).
fof(f1671, plain, (~ (e1 = op(e3, e3)) | (~ spl15_64 | spl15_101)), inference(forward_demodulation, [], [f803, f546])).
fof(f803, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl15_101), inference(avatar_component_clause, [], [f801])).
fof(f1660, plain, (~ spl15_48 | ~ spl15_44), inference(avatar_split_clause, [], [f1659, f459, f476])).
fof(f1659, plain, (~ (e3 = op(e1, e0)) | ~ spl15_44), inference(forward_demodulation, [], [f163, f461])).
fof(f163, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1656, plain, (~ spl15_48 | ~ spl15_64), inference(avatar_split_clause, [], [f1655, f544, f476])).
fof(f1655, plain, (~ (e3 = op(e1, e0)) | ~ spl15_64), inference(forward_demodulation, [], [f133, f546])).
fof(f1652, plain, (~ spl15_38 | ~ spl15_34), inference(avatar_split_clause, [], [f1570, f417, f434])).
fof(f1570, plain, (~ (e1 = op(e1, e2)) | ~ spl15_34), inference(forward_demodulation, [], [f168, f419])).
fof(f1633, plain, (~ spl15_1 | ~ spl15_5), inference(avatar_split_clause, [], [f1629, f294, f277])).
fof(f1629, plain, (~ (e0 = op(e3, e3)) | ~ spl15_5), inference(backward_demodulation, [], [f180, f296])).
fof(f1628, plain, (~ spl15_10 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f1627])).
fof(f1627, plain, ($false | (~ spl15_10 | ~ spl15_12)), inference(subsumption_resolution, [], [f1626, f185])).
fof(f1626, plain, ((e1 = e3) | (~ spl15_10 | ~ spl15_12)), inference(backward_demodulation, [], [f325, f317])).
fof(f1622, plain, (~ spl15_11 | ~ spl15_15), inference(avatar_split_clause, [], [f1621, f336, f319])).
fof(f1621, plain, (~ (e2 = op(e3, e1)) | ~ spl15_15), inference(backward_demodulation, [], [f175, f338])).
fof(f1615, plain, (~ spl15_34 | ~ spl15_35), inference(avatar_contradiction_clause, [], [f1614])).
fof(f1614, plain, ($false | (~ spl15_34 | ~ spl15_35)), inference(subsumption_resolution, [], [f1613, f184])).
fof(f1613, plain, ((e1 = e2) | (~ spl15_34 | ~ spl15_35)), inference(forward_demodulation, [], [f423, f419])).
fof(f1609, plain, (~ spl15_1 | ~ spl15_49), inference(avatar_split_clause, [], [f1605, f481, f277])).
fof(f1605, plain, (~ (e0 = op(e3, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f153, f483])).
fof(f1602, plain, (~ spl15_6 | ~ spl15_54), inference(avatar_split_clause, [], [f1598, f502, f298])).
fof(f1598, plain, (~ (e1 = op(e3, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f147, f504])).
fof(f147, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1596, plain, (~ spl15_11 | ~ spl15_59), inference(avatar_split_clause, [], [f1592, f523, f319])).
fof(f1592, plain, (~ (e2 = op(e3, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f141, f525])).
fof(f1548, plain, (~ spl15_7 | ~ spl15_11), inference(avatar_split_clause, [], [f1547, f319, f302])).
fof(f1547, plain, (~ (e2 = op(e3, e2)) | ~ spl15_11), inference(backward_demodulation, [], [f178, f321])).
fof(f321, plain, ((e2 = op(e3, e1)) | ~ spl15_11), inference(avatar_component_clause, [], [f319])).
fof(f1544, plain, (~ spl15_25 | ~ spl15_28), inference(avatar_contradiction_clause, [], [f1543])).
fof(f1543, plain, ($false | (~ spl15_25 | ~ spl15_28)), inference(subsumption_resolution, [], [f1541, f183])).
fof(f1541, plain, ((e0 = e3) | (~ spl15_25 | ~ spl15_28)), inference(backward_demodulation, [], [f393, f381])).
fof(f393, plain, ((e3 = op(e2, e1)) | ~ spl15_28), inference(avatar_component_clause, [], [f391])).
fof(f1539, plain, (~ spl15_18 | ~ spl15_30), inference(avatar_split_clause, [], [f1538, f400, f349])).
fof(f349, plain, (spl15_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f1538, plain, (~ (e1 = op(e2, e3)) | ~ spl15_30), inference(backward_demodulation, [], [f171, f402])).
fof(f402, plain, ((e1 = op(e2, e0)) | ~ spl15_30), inference(avatar_component_clause, [], [f400])).
fof(f1511, plain, (~ spl15_53 | ~ spl15_56), inference(avatar_contradiction_clause, [], [f1510])).
fof(f1510, plain, ($false | (~ spl15_53 | ~ spl15_56)), inference(subsumption_resolution, [], [f1508, f183])).
fof(f1508, plain, ((e0 = e3) | (~ spl15_53 | ~ spl15_56)), inference(backward_demodulation, [], [f512, f500])).
fof(f512, plain, ((e3 = op(e0, e2)) | ~ spl15_56), inference(avatar_component_clause, [], [f510])).
fof(f1506, plain, (~ spl15_50 | ~ spl15_58), inference(avatar_split_clause, [], [f1504, f519, f485])).
fof(f1504, plain, (~ (e1 = op(e0, e3)) | ~ spl15_58), inference(backward_demodulation, [], [f161, f521])).
fof(f1499, plain, (~ spl15_18 | ~ spl15_1 | ~ spl15_63 | spl15_85), inference(avatar_split_clause, [], [f1494, f724, f540, f277, f349])).
fof(f724, plain, (spl15_85 <=> (e1 = op(op(op(e3, e3), op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_85])])).
fof(f1494, plain, (~ (e1 = op(e2, e3)) | (~ spl15_1 | ~ spl15_63 | spl15_85)), inference(backward_demodulation, [], [f1352, f542])).
fof(f1352, plain, (~ (e1 = op(op(e0, e0), e3)) | (~ spl15_1 | spl15_85)), inference(forward_demodulation, [], [f726, f279])).
fof(f726, plain, (~ (e1 = op(op(op(e3, e3), op(e3, e3)), e3)) | spl15_85), inference(avatar_component_clause, [], [f724])).
fof(f1496, plain, (~ spl15_51 | ~ spl15_63), inference(avatar_split_clause, [], [f1488, f540, f489])).
fof(f1488, plain, (~ (e2 = op(e0, e3)) | ~ spl15_63), inference(backward_demodulation, [], [f159, f542])).
fof(f1495, plain, (~ spl15_31 | ~ spl15_63), inference(avatar_split_clause, [], [f1486, f540, f404])).
fof(f1486, plain, (~ (e2 = op(e2, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f134, f542])).
fof(f134, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1474, plain, (~ spl15_52 | ~ spl15_36), inference(avatar_split_clause, [], [f1473, f425, f493])).
fof(f1473, plain, (~ (e3 = op(e0, e3)) | ~ spl15_36), inference(forward_demodulation, [], [f151, f427])).
fof(f151, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1470, plain, (~ spl15_44 | ~ spl15_28), inference(avatar_split_clause, [], [f1180, f391, f459])).
fof(f1180, plain, (~ (e3 = op(e1, e1)) | ~ spl15_28), inference(forward_demodulation, [], [f142, f393])).
fof(f1467, plain, (~ spl15_40 | ~ spl15_36), inference(avatar_split_clause, [], [f1466, f425, f442])).
fof(f1466, plain, (~ (e3 = op(e1, e2)) | ~ spl15_36), inference(forward_demodulation, [], [f168, f427])).
fof(f1464, plain, (~ spl15_40 | ~ spl15_56), inference(avatar_split_clause, [], [f1463, f510, f442])).
fof(f1463, plain, (~ (e3 = op(e1, e2)) | ~ spl15_56), inference(forward_demodulation, [], [f145, f512])).
fof(f1462, plain, (~ spl15_32 | ~ spl15_28), inference(avatar_split_clause, [], [f1461, f391, f408])).
fof(f1461, plain, (~ (e3 = op(e2, e0)) | ~ spl15_28), inference(forward_demodulation, [], [f169, f393])).
fof(f1460, plain, (~ spl15_31 | ~ spl15_23), inference(avatar_split_clause, [], [f1459, f370, f404])).
fof(f1459, plain, (~ (e2 = op(e2, e0)) | ~ spl15_23), inference(forward_demodulation, [], [f170, f372])).
fof(f372, plain, ((e2 = op(e2, e2)) | ~ spl15_23), inference(avatar_component_clause, [], [f370])).
fof(f170, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1458, plain, (~ spl15_32 | ~ spl15_16), inference(avatar_split_clause, [], [f1457, f340, f408])).
fof(f1457, plain, (~ (e3 = op(e2, e0)) | ~ spl15_16), inference(forward_demodulation, [], [f138, f342])).
fof(f1455, plain, (~ spl15_17 | ~ spl15_1), inference(avatar_split_clause, [], [f1454, f277, f345])).
fof(f1454, plain, (~ (e0 = op(e2, e3)) | ~ spl15_1), inference(forward_demodulation, [], [f156, f279])).
fof(f156, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1451, plain, (~ spl15_19 | ~ spl15_23), inference(avatar_split_clause, [], [f1450, f370, f353])).
fof(f1450, plain, (~ (e2 = op(e2, e3)) | ~ spl15_23), inference(forward_demodulation, [], [f174, f372])).
fof(f1449, plain, (~ spl15_7 | ~ spl15_23), inference(avatar_split_clause, [], [f1448, f370, f302])).
fof(f1448, plain, (~ (e2 = op(e3, e2)) | ~ spl15_23), inference(forward_demodulation, [], [f150, f372])).
fof(f1445, plain, (~ spl15_5 | ~ spl15_7), inference(avatar_contradiction_clause, [], [f1444])).
fof(f1444, plain, ($false | (~ spl15_5 | ~ spl15_7)), inference(subsumption_resolution, [], [f1443, f182])).
fof(f1443, plain, ((e0 = e2) | (~ spl15_5 | ~ spl15_7)), inference(backward_demodulation, [], [f304, f296])).
fof(f1440, plain, (~ spl15_17 | ~ spl15_18), inference(avatar_contradiction_clause, [], [f1439])).
fof(f1439, plain, ($false | (~ spl15_17 | ~ spl15_18)), inference(subsumption_resolution, [], [f1438, f181])).
fof(f1438, plain, ((e0 = e1) | (~ spl15_17 | ~ spl15_18)), inference(backward_demodulation, [], [f351, f347])).
fof(f347, plain, ((e0 = op(e2, e3)) | ~ spl15_17), inference(avatar_component_clause, [], [f345])).
fof(f351, plain, ((e1 = op(e2, e3)) | ~ spl15_18), inference(avatar_component_clause, [], [f349])).
fof(f1435, plain, (~ spl15_55 | ~ spl15_7), inference(avatar_split_clause, [], [f1434, f302, f506])).
fof(f1434, plain, (~ (e2 = op(e0, e2)) | ~ spl15_7), inference(forward_demodulation, [], [f147, f304])).
fof(f1423, plain, (~ spl15_22 | ~ spl15_18), inference(avatar_split_clause, [], [f1422, f349, f366])).
fof(f1422, plain, (~ (e1 = op(e2, e2)) | ~ spl15_18), inference(forward_demodulation, [], [f174, f351])).
fof(f1420, plain, (~ spl15_55 | ~ spl15_23), inference(avatar_split_clause, [], [f1419, f370, f506])).
fof(f1419, plain, (~ (e2 = op(e0, e2)) | ~ spl15_23), inference(forward_demodulation, [], [f146, f372])).
fof(f1393, plain, (~ spl15_10 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1392])).
fof(f1392, plain, ($false | (~ spl15_10 | ~ spl15_11)), inference(subsumption_resolution, [], [f1391, f184])).
fof(f1391, plain, ((e1 = e2) | (~ spl15_10 | ~ spl15_11)), inference(backward_demodulation, [], [f321, f317])).
fof(f1390, plain, (~ spl15_21 | ~ spl15_23), inference(avatar_contradiction_clause, [], [f1389])).
fof(f1389, plain, ($false | (~ spl15_21 | ~ spl15_23)), inference(subsumption_resolution, [], [f1388, f182])).
fof(f1388, plain, ((e0 = e2) | (~ spl15_21 | ~ spl15_23)), inference(forward_demodulation, [], [f372, f364])).
fof(f1387, plain, (~ spl15_6 | ~ spl15_38), inference(avatar_split_clause, [], [f1385, f434, f298])).
fof(f1385, plain, (~ (e1 = op(e3, e2)) | ~ spl15_38), inference(backward_demodulation, [], [f149, f436])).
fof(f149, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1346, plain, (~ spl15_35 | ~ spl15_1 | ~ spl15_62 | spl15_81), inference(avatar_split_clause, [], [f1345, f706, f536, f277, f421])).
fof(f706, plain, (spl15_81 <=> (e2 = op(op(op(e3, e3), op(e3, e3)), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_81])])).
fof(f1345, plain, (~ (e2 = op(e1, e3)) | (~ spl15_1 | ~ spl15_62 | spl15_81)), inference(forward_demodulation, [], [f1344, f538])).
fof(f1344, plain, (~ (e2 = op(op(e0, e0), e3)) | (~ spl15_1 | spl15_81)), inference(forward_demodulation, [], [f708, f279])).
fof(f708, plain, (~ (e2 = op(op(op(e3, e3), op(e3, e3)), e3)) | spl15_81), inference(avatar_component_clause, [], [f706])).
fof(f1319, plain, (~ spl15_2 | ~ spl15_6), inference(avatar_split_clause, [], [f1317, f298, f281])).
fof(f1317, plain, (~ (e1 = op(e3, e3)) | ~ spl15_6), inference(backward_demodulation, [], [f180, f300])).
fof(f300, plain, ((e1 = op(e3, e2)) | ~ spl15_6), inference(avatar_component_clause, [], [f298])).
fof(f1283, plain, (~ spl15_46 | ~ spl15_42), inference(avatar_split_clause, [], [f1282, f451, f468])).
fof(f1282, plain, (~ (e1 = op(e1, e0)) | ~ spl15_42), inference(forward_demodulation, [], [f163, f453])).
fof(f1255, plain, (~ spl15_13 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1254])).
fof(f1254, plain, ($false | (~ spl15_13 | ~ spl15_15)), inference(subsumption_resolution, [], [f1252, f182])).
fof(f1252, plain, ((e0 = e2) | (~ spl15_13 | ~ spl15_15)), inference(backward_demodulation, [], [f338, f330])).
fof(f1251, plain, (~ spl15_15 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f1250])).
fof(f1250, plain, ($false | (~ spl15_15 | ~ spl15_16)), inference(subsumption_resolution, [], [f1249, f186])).
fof(f186, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1249, plain, ((e2 = e3) | (~ spl15_15 | ~ spl15_16)), inference(backward_demodulation, [], [f342, f338])).
fof(f1248, plain, (~ spl15_2 | ~ spl15_18), inference(avatar_split_clause, [], [f1246, f349, f281])).
fof(f1246, plain, (~ (e1 = op(e3, e3)) | ~ spl15_18), inference(backward_demodulation, [], [f156, f351])).
fof(f1244, plain, (~ spl15_39 | ~ spl15_40), inference(avatar_contradiction_clause, [], [f1243])).
fof(f1243, plain, ($false | (~ spl15_39 | ~ spl15_40)), inference(subsumption_resolution, [], [f1242, f186])).
fof(f1242, plain, ((e2 = e3) | (~ spl15_39 | ~ spl15_40)), inference(forward_demodulation, [], [f444, f440])).
fof(f444, plain, ((e3 = op(e1, e2)) | ~ spl15_40), inference(avatar_component_clause, [], [f442])).
fof(f1225, plain, (~ spl15_51 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f1224])).
fof(f1224, plain, ($false | (~ spl15_51 | ~ spl15_52)), inference(subsumption_resolution, [], [f1223, f186])).
fof(f1223, plain, ((e2 = e3) | (~ spl15_51 | ~ spl15_52)), inference(backward_demodulation, [], [f495, f491])).
fof(f1214, plain, (~ spl15_57 | ~ spl15_59), inference(avatar_contradiction_clause, [], [f1213])).
fof(f1213, plain, ($false | (~ spl15_57 | ~ spl15_59)), inference(subsumption_resolution, [], [f1212, f182])).
fof(f1212, plain, ((e0 = e2) | (~ spl15_57 | ~ spl15_59)), inference(backward_demodulation, [], [f525, f517])).
fof(f1206, plain, (~ spl15_54 | ~ spl15_62), inference(avatar_split_clause, [], [f1195, f536, f502])).
fof(f1195, plain, (~ (e1 = op(e0, e2)) | ~ spl15_62), inference(backward_demodulation, [], [f158, f538])).
fof(f1205, plain, (~ spl15_46 | ~ spl15_62), inference(avatar_split_clause, [], [f1194, f536, f468])).
fof(f1194, plain, (~ (e1 = op(e1, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f133, f538])).
fof(f1193, plain, (~ spl15_51 | spl15_81 | ~ spl15_92), inference(avatar_split_clause, [], [f1192, f756, f706, f489])).
fof(f1192, plain, (~ (e2 = op(e0, e3)) | (spl15_81 | ~ spl15_92)), inference(forward_demodulation, [], [f708, f757])).
fof(f757, plain, ((e0 = op(op(e3, e3), op(e3, e3))) | ~ spl15_92), inference(avatar_component_clause, [], [f756])).
fof(f1185, plain, (~ spl15_51 | ~ spl15_59), inference(avatar_split_clause, [], [f1128, f523, f489])).
fof(f1128, plain, (~ (e2 = op(e0, e3)) | ~ spl15_59), inference(backward_demodulation, [], [f161, f525])).
fof(f1182, plain, (~ spl15_48 | ~ spl15_16), inference(avatar_split_clause, [], [f1181, f340, f476])).
fof(f1181, plain, (~ (e3 = op(e1, e0)) | ~ spl15_16), inference(forward_demodulation, [], [f137, f342])).
fof(f1168, plain, (~ spl15_8 | ~ spl15_16), inference(avatar_split_clause, [], [f1167, f340, f306])).
fof(f1167, plain, (~ (e3 = op(e3, e2)) | ~ spl15_16), inference(forward_demodulation, [], [f176, f342])).
fof(f1164, plain, (~ spl15_4 | ~ spl15_16), inference(avatar_split_clause, [], [f1163, f340, f289])).
fof(f289, plain, (spl15_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f1163, plain, (~ (e3 = op(e3, e3)) | ~ spl15_16), inference(forward_demodulation, [], [f177, f342])).
fof(f1159, plain, (~ spl15_9 | ~ spl15_10), inference(avatar_contradiction_clause, [], [f1158])).
fof(f1158, plain, ($false | (~ spl15_9 | ~ spl15_10)), inference(subsumption_resolution, [], [f1157, f181])).
fof(f1157, plain, ((e0 = e1) | (~ spl15_9 | ~ spl15_10)), inference(forward_demodulation, [], [f317, f313])).
fof(f313, plain, ((e0 = op(e3, e1)) | ~ spl15_9), inference(avatar_component_clause, [], [f311])).
fof(f1132, plain, (~ spl15_43 | ~ spl15_59), inference(avatar_split_clause, [], [f1129, f523, f455])).
fof(f1129, plain, (~ (e2 = op(e1, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f139, f525])).
fof(f1131, plain, (~ spl15_55 | ~ spl15_59), inference(avatar_split_clause, [], [f1127, f523, f506])).
fof(f1127, plain, (~ (e2 = op(e0, e2)) | ~ spl15_59), inference(backward_demodulation, [], [f160, f525])).
fof(f1130, plain, (~ spl15_27 | ~ spl15_59), inference(avatar_split_clause, [], [f1126, f523, f387])).
fof(f1126, plain, (~ (e2 = op(e2, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f140, f525])).
fof(f1124, plain, (~ spl15_50 | ~ spl15_3 | ~ spl15_21 | spl15_85), inference(avatar_split_clause, [], [f1123, f724, f362, f285, f485])).
fof(f1123, plain, (~ (e1 = op(e0, e3)) | (~ spl15_3 | ~ spl15_21 | spl15_85)), inference(forward_demodulation, [], [f1122, f364])).
fof(f1122, plain, (~ (e1 = op(op(e2, e2), e3)) | (~ spl15_3 | spl15_85)), inference(forward_demodulation, [], [f726, f287])).
fof(f1109, plain, (~ spl15_47 | ~ spl15_39), inference(avatar_split_clause, [], [f1108, f438, f472])).
fof(f1108, plain, (~ (e2 = op(e1, e0)) | ~ spl15_39), inference(forward_demodulation, [], [f164, f440])).
fof(f1073, plain, (~ spl15_9 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1072])).
fof(f1072, plain, ($false | (~ spl15_9 | ~ spl15_11)), inference(subsumption_resolution, [], [f1071, f182])).
fof(f1071, plain, ((e0 = e2) | (~ spl15_9 | ~ spl15_11)), inference(backward_demodulation, [], [f321, f313])).
fof(f1048, plain, (~ spl15_45 | ~ spl15_48), inference(avatar_contradiction_clause, [], [f1047])).
fof(f1047, plain, ($false | (~ spl15_45 | ~ spl15_48)), inference(subsumption_resolution, [], [f1046, f183])).
fof(f1046, plain, ((e0 = e3) | (~ spl15_45 | ~ spl15_48)), inference(forward_demodulation, [], [f478, f466])).
fof(f1037, plain, (~ spl15_56 | ~ spl15_60), inference(avatar_split_clause, [], [f1035, f527, f510])).
fof(f1035, plain, (~ (e3 = op(e0, e2)) | ~ spl15_60), inference(backward_demodulation, [], [f160, f529])).
fof(f1036, plain, (~ spl15_28 | ~ spl15_60), inference(avatar_split_clause, [], [f1034, f527, f391])).
fof(f1034, plain, (~ (e3 = op(e2, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f140, f529])).
fof(f1030, plain, (~ spl15_53 | ~ spl15_61), inference(avatar_split_clause, [], [f1022, f532, f498])).
fof(f1022, plain, (~ (e0 = op(e0, e2)) | ~ spl15_61), inference(backward_demodulation, [], [f158, f534])).
fof(f1029, plain, (~ spl15_57 | ~ spl15_61), inference(avatar_split_clause, [], [f1021, f532, f515])).
fof(f1021, plain, (~ (e0 = op(e0, e1)) | ~ spl15_61), inference(backward_demodulation, [], [f157, f534])).
fof(f1028, plain, (~ spl15_29 | ~ spl15_61), inference(avatar_split_clause, [], [f1020, f532, f396])).
fof(f1020, plain, (~ (e0 = op(e2, e0)) | ~ spl15_61), inference(backward_demodulation, [], [f134, f534])).
fof(f1019, plain, (~ spl15_61 | ~ spl15_45), inference(avatar_split_clause, [], [f1018, f464, f532])).
fof(f1018, plain, (~ (e0 = op(e0, e0)) | ~ spl15_45), inference(forward_demodulation, [], [f133, f466])).
fof(f1014, plain, (~ spl15_58 | ~ spl15_42), inference(avatar_split_clause, [], [f1013, f451, f519])).
fof(f1013, plain, (~ (e1 = op(e0, e1)) | ~ spl15_42), inference(forward_demodulation, [], [f139, f453])).
fof(f1010, plain, (~ spl15_57 | ~ spl15_49), inference(avatar_split_clause, [], [f1009, f481, f515])).
fof(f1009, plain, (~ (e0 = op(e0, e1)) | ~ spl15_49), inference(forward_demodulation, [], [f161, f483])).
fof(f1006, plain, (~ spl15_53 | ~ spl15_49), inference(avatar_split_clause, [], [f1005, f481, f498])).
fof(f1005, plain, (~ (e0 = op(e0, e2)) | ~ spl15_49), inference(forward_demodulation, [], [f162, f483])).
fof(f989, plain, (~ spl15_30 | ~ spl15_14), inference(avatar_split_clause, [], [f988, f332, f400])).
fof(f988, plain, (~ (e1 = op(e2, e0)) | ~ spl15_14), inference(forward_demodulation, [], [f138, f334])).
fof(f983, plain, (~ spl15_26 | ~ spl15_18), inference(avatar_split_clause, [], [f982, f349, f383])).
fof(f982, plain, (~ (e1 = op(e2, e1)) | ~ spl15_18), inference(forward_demodulation, [], [f173, f351])).
fof(f979, plain, (~ spl15_21 | ~ spl15_3 | spl15_92), inference(avatar_split_clause, [], [f956, f756, f285, f362])).
fof(f956, plain, (~ (e0 = op(e2, e2)) | (~ spl15_3 | spl15_92)), inference(backward_demodulation, [], [f758, f287])).
fof(f978, plain, (~ spl15_18 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f977])).
fof(f977, plain, ($false | (~ spl15_18 | ~ spl15_20)), inference(subsumption_resolution, [], [f976, f185])).
fof(f976, plain, ((e1 = e3) | (~ spl15_18 | ~ spl15_20)), inference(forward_demodulation, [], [f359, f351])).
fof(f359, plain, ((e3 = op(e2, e3)) | ~ spl15_20), inference(avatar_component_clause, [], [f357])).
fof(f966, plain, (~ spl15_3 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f965])).
fof(f965, plain, ($false | (~ spl15_3 | ~ spl15_11)), inference(subsumption_resolution, [], [f964, f321])).
fof(f964, plain, (~ (e2 = op(e3, e1)) | ~ spl15_3), inference(forward_demodulation, [], [f179, f287])).
fof(f963, plain, (~ spl15_6 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f962])).
fof(f962, plain, ($false | (~ spl15_6 | ~ spl15_8)), inference(subsumption_resolution, [], [f961, f185])).
fof(f961, plain, ((e1 = e3) | (~ spl15_6 | ~ spl15_8)), inference(forward_demodulation, [], [f308, f300])).
fof(f953, plain, (~ spl15_5 | ~ spl15_6), inference(avatar_contradiction_clause, [], [f952])).
fof(f952, plain, ($false | (~ spl15_5 | ~ spl15_6)), inference(subsumption_resolution, [], [f951, f181])).
fof(f951, plain, ((e0 = e1) | (~ spl15_5 | ~ spl15_6)), inference(backward_demodulation, [], [f300, f296])).
fof(f950, plain, (~ spl15_6 | ~ spl15_7), inference(avatar_contradiction_clause, [], [f949])).
fof(f949, plain, ($false | (~ spl15_6 | ~ spl15_7)), inference(subsumption_resolution, [], [f948, f184])).
fof(f948, plain, ((e1 = e2) | (~ spl15_6 | ~ spl15_7)), inference(backward_demodulation, [], [f304, f300])).
fof(f944, plain, (~ spl15_4 | ~ spl15_8), inference(avatar_split_clause, [], [f943, f306, f289])).
fof(f943, plain, (~ (e3 = op(e3, e3)) | ~ spl15_8), inference(backward_demodulation, [], [f180, f308])).
fof(f942, plain, (~ spl15_11 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f941])).
fof(f941, plain, ($false | (~ spl15_11 | ~ spl15_12)), inference(subsumption_resolution, [], [f940, f186])).
fof(f940, plain, ((e2 = e3) | (~ spl15_11 | ~ spl15_12)), inference(backward_demodulation, [], [f325, f321])).
fof(f939, plain, (~ spl15_4 | ~ spl15_12), inference(avatar_split_clause, [], [f937, f323, f289])).
fof(f937, plain, (~ (e3 = op(e3, e3)) | ~ spl15_12), inference(backward_demodulation, [], [f179, f325])).
fof(f935, plain, (~ spl15_14 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f934])).
fof(f934, plain, ($false | (~ spl15_14 | ~ spl15_15)), inference(subsumption_resolution, [], [f933, f184])).
fof(f933, plain, ((e1 = e2) | (~ spl15_14 | ~ spl15_15)), inference(backward_demodulation, [], [f338, f334])).
fof(f926, plain, (~ spl15_18 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f925])).
fof(f925, plain, ($false | (~ spl15_18 | ~ spl15_19)), inference(subsumption_resolution, [], [f924, f184])).
fof(f924, plain, ((e1 = e2) | (~ spl15_18 | ~ spl15_19)), inference(backward_demodulation, [], [f355, f351])).
fof(f355, plain, ((e2 = op(e2, e3)) | ~ spl15_19), inference(avatar_component_clause, [], [f353])).
fof(f917, plain, (~ spl15_8 | ~ spl15_24), inference(avatar_split_clause, [], [f913, f374, f306])).
fof(f913, plain, (~ (e3 = op(e3, e2)) | ~ spl15_24), inference(backward_demodulation, [], [f150, f376])).
fof(f905, plain, (~ spl15_24 | ~ spl15_32), inference(avatar_split_clause, [], [f901, f408, f374])).
fof(f901, plain, (~ (e3 = op(e2, e2)) | ~ spl15_32), inference(backward_demodulation, [], [f170, f410])).
fof(f898, plain, (~ spl15_35 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f897])).
fof(f897, plain, ($false | (~ spl15_35 | ~ spl15_36)), inference(subsumption_resolution, [], [f896, f186])).
fof(f896, plain, ((e2 = e3) | (~ spl15_35 | ~ spl15_36)), inference(backward_demodulation, [], [f427, f423])).
fof(f895, plain, (~ spl15_4 | ~ spl15_36), inference(avatar_split_clause, [], [f893, f425, f289])).
fof(f893, plain, (~ (e3 = op(e3, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f155, f427])).
fof(f884, plain, (~ spl15_38 | ~ spl15_42), inference(avatar_split_clause, [], [f880, f451, f434])).
fof(f880, plain, (~ (e1 = op(e1, e2)) | ~ spl15_42), inference(backward_demodulation, [], [f166, f453])).
fof(f874, plain, (~ spl15_37 | ~ spl15_45), inference(avatar_split_clause, [], [f869, f464, f430])).
fof(f869, plain, (~ (e0 = op(e1, e2)) | ~ spl15_45), inference(backward_demodulation, [], [f164, f466])).
fof(f872, plain, (~ spl15_13 | ~ spl15_45), inference(avatar_split_clause, [], [f867, f464, f328])).
fof(f867, plain, (~ (e0 = op(e3, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f137, f466])).
fof(f865, plain, (~ spl15_49 | ~ spl15_50), inference(avatar_contradiction_clause, [], [f864])).
fof(f864, plain, ($false | (~ spl15_49 | ~ spl15_50)), inference(subsumption_resolution, [], [f863, f181])).
fof(f863, plain, ((e0 = e1) | (~ spl15_49 | ~ spl15_50)), inference(backward_demodulation, [], [f487, f483])).
fof(f862, plain, (~ spl15_50 | ~ spl15_51), inference(avatar_contradiction_clause, [], [f861])).
fof(f861, plain, ($false | (~ spl15_50 | ~ spl15_51)), inference(subsumption_resolution, [], [f860, f184])).
fof(f860, plain, ((e1 = e2) | (~ spl15_50 | ~ spl15_51)), inference(backward_demodulation, [], [f491, f487])).
fof(f858, plain, (~ spl15_19 | ~ spl15_51), inference(avatar_split_clause, [], [f855, f489, f353])).
fof(f855, plain, (~ (e2 = op(e2, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f152, f491])).
fof(f852, plain, (~ spl15_5 | ~ spl15_53), inference(avatar_split_clause, [], [f848, f498, f294])).
fof(f848, plain, (~ (e0 = op(e3, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f147, f500])).
fof(f841, plain, (~ spl15_41 | ~ spl15_57), inference(avatar_split_clause, [], [f836, f515, f447])).
fof(f836, plain, (~ (e0 = op(e1, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f139, f517])).
fof(f832, plain, (~ spl15_60 | ~ spl15_64), inference(avatar_split_clause, [], [f824, f544, f527])).
fof(f824, plain, (~ (e3 = op(e0, e1)) | ~ spl15_64), inference(backward_demodulation, [], [f157, f546])).
fof(f819, plain, (~ spl15_44 | ~ spl15_86 | ~ spl15_102), inference(avatar_split_clause, [], [f274, f806, f729, f459])).
fof(f274, plain, (~ (e0 = op(op(op(e1, e1), op(e1, e1)), e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e0 = op(op(op(e1, e1), op(e1, e1)), e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e0 = op(op(op(e1, e1), op(e1, e1)), e1)) & (e2 = op(op(e1, e1), op(e1, e1))) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax28)).
fof(f818, plain, (~ spl15_64 | ~ spl15_101 | ~ spl15_99), inference(avatar_split_clause, [], [f273, f789, f801, f544])).
fof(f273, plain, (~ (e2 = op(op(op(e0, e0), op(e0, e0)), e0)) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e2 = op(op(op(e0, e0), op(e0, e0)), e0)) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e3)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e2 = op(op(op(e0, e0), op(e0, e0)), e0)) & (e1 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e3)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax27)).
fof(f817, plain, (~ spl15_24 | ~ spl15_82 | ~ spl15_97), inference(avatar_split_clause, [], [f272, f780, f711, f374])).
fof(f272, plain, (~ (e0 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(op(op(e2, e2), op(e2, e2)), e2)) & (e1 = op(op(e2, e2), op(e2, e2))) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax26)).
fof(f816, plain, (~ spl15_44 | ~ spl15_100 | ~ spl15_91), inference(avatar_split_clause, [], [f271, f751, f795, f459])).
fof(f271, plain, (~ (e2 = op(op(op(e1, e1), op(e1, e1)), e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e2 = op(op(op(e1, e1), op(e1, e1)), e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e2 = op(op(op(e1, e1), op(e1, e1)), e1)) & (e0 = op(op(e1, e1), op(e1, e1))) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax25)).
fof(f815, plain, (~ spl15_24 | ~ spl15_93 | ~ spl15_89), inference(avatar_split_clause, [], [f270, f742, f761, f374])).
fof(f270, plain, (~ (e1 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e1 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e1 = op(op(op(e2, e2), op(e2, e2)), e2)) & (e0 = op(op(e2, e2), op(e2, e2))) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax24)).
fof(f814, plain, (~ spl15_63 | ~ spl15_98 | ~ spl15_103), inference(avatar_split_clause, [], [f269, f811, f785, f540])).
fof(f269, plain, (~ (e1 = op(op(op(e0, e0), op(e0, e0)), e0)) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e1 = op(op(op(e0, e0), op(e0, e0)), e0)) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e1 = op(op(op(e0, e0), op(e0, e0)), e0)) & (e3 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax23)).
fof(f799, plain, (~ spl15_3 | ~ spl15_80 | ~ spl15_94), inference(avatar_split_clause, [], [f266, f766, f702, f285])).
fof(f266, plain, (~ (e0 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e0 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e0 = op(op(op(e3, e3), op(e3, e3)), e3)) & (e1 = op(op(e3, e3), op(e3, e3))) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax20)).
fof(f793, plain, (~ spl15_3 | ~ spl15_92 | ~ spl15_85), inference(avatar_split_clause, [], [f264, f724, f756, f285])).
fof(f264, plain, (~ (e1 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e1 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e1 = op(op(op(e3, e3), op(e3, e3)), e3)) & (e0 = op(op(e3, e3), op(e3, e3))) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax18)).
fof(f783, plain, (~ spl15_22 | ~ spl15_88 | ~ spl15_97), inference(avatar_split_clause, [], [f262, f780, f738, f366])).
fof(f262, plain, (~ (e0 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e0 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(e2, e2))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e0 = op(op(op(e2, e2), op(e2, e2)), e2)) & (e3 = op(op(e2, e2), op(e2, e2))) & (e1 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax16)).
fof(f778, plain, (~ spl15_62 | ~ spl15_95 | ~ spl15_96), inference(avatar_split_clause, [], [f261, f775, f771, f536])).
fof(f261, plain, (~ (e3 = op(op(op(e0, e0), op(e0, e0)), e0)) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e3 = op(op(op(e0, e0), op(e0, e0)), e0)) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e3 = op(op(op(e0, e0), op(e0, e0)), e0)) & (e2 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax15)).
fof(f759, plain, (~ spl15_2 | ~ spl15_92 | ~ spl15_81), inference(avatar_split_clause, [], [f258, f706, f756, f281])).
fof(f258, plain, (~ (e2 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e2 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e2 = op(op(op(e3, e3), op(e3, e3)), e3)) & (e0 = op(op(e3, e3), op(e3, e3))) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax12)).
fof(f754, plain, (~ spl15_41 | ~ spl15_90 | ~ spl15_91), inference(avatar_split_clause, [], [f257, f751, f747, f447])).
fof(f257, plain, (~ (e2 = op(op(op(e1, e1), op(e1, e1)), e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e2 = op(op(op(e1, e1), op(e1, e1)), e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e2 = op(op(op(e1, e1), op(e1, e1)), e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e0 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax11)).
fof(f745, plain, (~ spl15_21 | ~ spl15_88 | ~ spl15_89), inference(avatar_split_clause, [], [f256, f742, f738, f362])).
fof(f256, plain, (~ (e1 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e1 = op(op(op(e2, e2), op(e2, e2)), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e1 = op(op(op(e2, e2), op(e2, e2)), e2)) & (e3 = op(op(e2, e2), op(e2, e2))) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax10)).
fof(f727, plain, (~ spl15_1 | ~ spl15_84 | ~ spl15_85), inference(avatar_split_clause, [], [f254, f724, f720, f277])).
fof(f254, plain, (~ (e1 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e1 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e1 = op(op(op(e3, e3), op(e3, e3)), e3)) & (e2 = op(op(e3, e3), op(e3, e3))) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax8)).
fof(f709, plain, (~ spl15_1 | ~ spl15_80 | ~ spl15_81), inference(avatar_split_clause, [], [f252, f706, f702, f277])).
fof(f252, plain, (~ (e2 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(op(op(e3, e3), op(e3, e3)), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(op(op(e3, e3), op(e3, e3)), e3)) & (e1 = op(op(e3, e3), op(e3, e3))) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax6)).
fof(f700, plain, (spl15_61 | spl15_42 | spl15_23 | spl15_4), inference(avatar_split_clause, [], [f232, f289, f370, f451, f532])).
fof(f232, plain, ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(definition_folding, [], [f5, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e1 = op(e1, e1)) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e1 = op(e1, e1)) & (e0 = op(e1, e0)) & (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e2 = op(e2, e2)) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e2 = op(e2, e2)) & (e0 = op(e2, e0)) & (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e3 = op(e3, e3)) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e3 = op(e3, e3)) & (e0 = op(e3, e0)) & (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, ((~ (e0 = op(e0, e0)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> (~ (e0 = op(e0, e0)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, ((~ (e2 = op(e2, e2)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> (~ (e2 = op(e2, e2)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, ((~ (e3 = op(e3, e3)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> (~ (e3 = op(e3, e3)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, ((~ (e0 = op(e0, e0)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> (~ (e0 = op(e0, e0)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, ((~ (e1 = op(e1, e1)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> (~ (e1 = op(e1, e1)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, ((~ (e3 = op(e3, e3)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> (~ (e3 = op(e3, e3)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, ((~ (e0 = op(e0, e0)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> (~ (e0 = op(e0, e0)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, ((~ (e1 = op(e1, e1)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> (~ (e1 = op(e1, e1)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, ((~ (e2 = op(e2, e2)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> (~ (e2 = op(e2, e2)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f5, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e2 = op(e2, e2)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | (~ (e1 = op(e1, e1)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | (~ (e0 = op(e0, e0)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | (~ (e3 = op(e3, e3)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e1 = op(e1, e1)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | (~ (e0 = op(e0, e0)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | (~ (e3 = op(e3, e3)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | (~ (e2 = op(e2, e2)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e0 = op(e0, e0)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | (~ (e3 = op(e3, e3)) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | (~ (e2 = op(e2, e2)) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | (~ (e1 = op(e1, e1)) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax5)).
fof(f699, plain, (~ spl15_62 | spl15_57), inference(avatar_split_clause, [], [f234, f515, f536])).
fof(f234, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f69])).
fof(f698, plain, (~ spl15_63 | spl15_53), inference(avatar_split_clause, [], [f235, f498, f540])).
fof(f235, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f69])).
fof(f697, plain, (~ spl15_64 | spl15_49), inference(avatar_split_clause, [], [f236, f481, f544])).
fof(f236, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f69])).
fof(f696, plain, (~ spl15_41 | spl15_46), inference(avatar_split_clause, [], [f237, f468, f447])).
fof(f237, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f69])).
fof(f695, plain, (~ spl15_43 | spl15_38), inference(avatar_split_clause, [], [f239, f434, f455])).
fof(f239, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f69])).
fof(f694, plain, (~ spl15_44 | spl15_34), inference(avatar_split_clause, [], [f240, f417, f459])).
fof(f240, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f69])).
fof(f693, plain, (~ spl15_21 | spl15_31), inference(avatar_split_clause, [], [f241, f404, f362])).
fof(f241, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f69])).
fof(f692, plain, (~ spl15_22 | spl15_27), inference(avatar_split_clause, [], [f242, f387, f366])).
fof(f242, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f69])).
fof(f691, plain, (~ spl15_24 | spl15_19), inference(avatar_split_clause, [], [f244, f353, f374])).
fof(f244, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f69])).
fof(f690, plain, (~ spl15_1 | spl15_16), inference(avatar_split_clause, [], [f245, f340, f277])).
fof(f245, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f69])).
fof(f689, plain, (~ spl15_2 | spl15_12), inference(avatar_split_clause, [], [f246, f323, f281])).
fof(f246, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f69])).
fof(f688, plain, (~ spl15_3 | spl15_8), inference(avatar_split_clause, [], [f247, f306, f285])).
fof(f247, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f69])).
fof(f687, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | spl15_4), inference(avatar_split_clause, [], [f249, f289, f581, f588, f595, f602, f609, f616, f623, f630, f637, f644, f651, f658, f665, f672, f679])).
fof(f679, plain, (spl15_79 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl15_79])])).
fof(f672, plain, (spl15_78 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl15_78])])).
fof(f665, plain, (spl15_77 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl15_77])])).
fof(f658, plain, (spl15_76 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl15_76])])).
fof(f651, plain, (spl15_75 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_75])])).
fof(f644, plain, (spl15_74 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_74])])).
fof(f637, plain, (spl15_73 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_73])])).
fof(f630, plain, (spl15_72 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_72])])).
fof(f623, plain, (spl15_71 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_71])])).
fof(f616, plain, (spl15_70 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl15_70])])).
fof(f609, plain, (spl15_69 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl15_69])])).
fof(f602, plain, (spl15_68 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl15_68])])).
fof(f595, plain, (spl15_67 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl15_67])])).
fof(f588, plain, (spl15_66 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl15_66])])).
fof(f581, plain, (spl15_65 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl15_65])])).
fof(f249, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f685, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_4), inference(avatar_split_clause, [], [f251, f289, f581, f588, f595, f602, f609, f616, f623, f630, f637, f644, f651, f658, f665, f672, f679])).
fof(f251, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f684, plain, (~ spl15_79 | spl15_61), inference(avatar_split_clause, [], [f229, f532, f679])).
fof(f229, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f682, plain, (~ spl15_79 | ~ spl15_61), inference(avatar_split_clause, [], [f231, f532, f679])).
fof(f231, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f677, plain, (~ spl15_78 | spl15_57), inference(avatar_split_clause, [], [f226, f515, f672])).
fof(f226, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e1 = op(e1, e1)) & (e0 = op(e1, e0)) & (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f676, plain, (~ spl15_78 | spl15_45), inference(avatar_split_clause, [], [f227, f464, f672])).
fof(f227, plain, ((e0 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f675, plain, (~ spl15_78 | ~ spl15_42), inference(avatar_split_clause, [], [f228, f451, f672])).
fof(f228, plain, (~ (e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f670, plain, (~ spl15_77 | spl15_53), inference(avatar_split_clause, [], [f223, f498, f665])).
fof(f223, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e2 = op(e2, e2)) & (e0 = op(e2, e0)) & (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f669, plain, (~ spl15_77 | spl15_29), inference(avatar_split_clause, [], [f224, f396, f665])).
fof(f224, plain, ((e0 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f668, plain, (~ spl15_77 | ~ spl15_23), inference(avatar_split_clause, [], [f225, f370, f665])).
fof(f225, plain, (~ (e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f663, plain, (~ spl15_76 | spl15_49), inference(avatar_split_clause, [], [f220, f481, f658])).
fof(f220, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e3 = op(e3, e3)) & (e0 = op(e3, e0)) & (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f662, plain, (~ spl15_76 | spl15_13), inference(avatar_split_clause, [], [f221, f328, f658])).
fof(f221, plain, ((e0 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f661, plain, (~ spl15_76 | ~ spl15_4), inference(avatar_split_clause, [], [f222, f289, f658])).
fof(f222, plain, (~ (e3 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f656, plain, (~ spl15_75 | spl15_46), inference(avatar_split_clause, [], [f217, f468, f651])).
fof(f217, plain, ((e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e0 = op(e0, e0)) & (e1 = op(e0, e1)) & (e1 = op(e1, e0))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f655, plain, (~ spl15_75 | spl15_58), inference(avatar_split_clause, [], [f218, f519, f651])).
fof(f218, plain, ((e1 = op(e0, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f654, plain, (~ spl15_75 | ~ spl15_61), inference(avatar_split_clause, [], [f219, f532, f651])).
fof(f219, plain, (~ (e0 = op(e0, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f649, plain, (~ spl15_74 | spl15_42), inference(avatar_split_clause, [], [f214, f451, f644])).
fof(f214, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f647, plain, (~ spl15_74 | ~ spl15_42), inference(avatar_split_clause, [], [f216, f451, f644])).
fof(f216, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f642, plain, (~ spl15_73 | spl15_38), inference(avatar_split_clause, [], [f211, f434, f637])).
fof(f211, plain, ((e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e2 = op(e2, e2)) & (e1 = op(e2, e1)) & (e1 = op(e1, e2))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f641, plain, (~ spl15_73 | spl15_26), inference(avatar_split_clause, [], [f212, f383, f637])).
fof(f212, plain, ((e1 = op(e2, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f640, plain, (~ spl15_73 | ~ spl15_23), inference(avatar_split_clause, [], [f213, f370, f637])).
fof(f213, plain, (~ (e2 = op(e2, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f635, plain, (~ spl15_72 | spl15_34), inference(avatar_split_clause, [], [f208, f417, f630])).
fof(f208, plain, ((e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e3 = op(e3, e3)) & (e1 = op(e3, e1)) & (e1 = op(e1, e3))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f634, plain, (~ spl15_72 | spl15_10), inference(avatar_split_clause, [], [f209, f315, f630])).
fof(f209, plain, ((e1 = op(e3, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f633, plain, (~ spl15_72 | ~ spl15_4), inference(avatar_split_clause, [], [f210, f289, f630])).
fof(f210, plain, (~ (e3 = op(e3, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f628, plain, (~ spl15_71 | spl15_31), inference(avatar_split_clause, [], [f205, f404, f623])).
fof(f205, plain, ((e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e0 = op(e0, e0)) & (e2 = op(e0, e2)) & (e2 = op(e2, e0))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f627, plain, (~ spl15_71 | spl15_55), inference(avatar_split_clause, [], [f206, f506, f623])).
fof(f206, plain, ((e2 = op(e0, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f626, plain, (~ spl15_71 | ~ spl15_61), inference(avatar_split_clause, [], [f207, f532, f623])).
fof(f207, plain, (~ (e0 = op(e0, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f621, plain, (~ spl15_70 | spl15_27), inference(avatar_split_clause, [], [f202, f387, f616])).
fof(f202, plain, ((e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e1 = op(e1, e1)) & (e2 = op(e1, e2)) & (e2 = op(e2, e1))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f620, plain, (~ spl15_70 | spl15_39), inference(avatar_split_clause, [], [f203, f438, f616])).
fof(f203, plain, ((e2 = op(e1, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f619, plain, (~ spl15_70 | ~ spl15_42), inference(avatar_split_clause, [], [f204, f451, f616])).
fof(f204, plain, (~ (e1 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f614, plain, (~ spl15_69 | spl15_23), inference(avatar_split_clause, [], [f199, f370, f609])).
fof(f199, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f612, plain, (~ spl15_69 | ~ spl15_23), inference(avatar_split_clause, [], [f201, f370, f609])).
fof(f201, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f607, plain, (~ spl15_68 | spl15_19), inference(avatar_split_clause, [], [f196, f353, f602])).
fof(f196, plain, ((e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e3 = op(e3, e3)) & (e2 = op(e3, e2)) & (e2 = op(e2, e3))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f606, plain, (~ spl15_68 | spl15_7), inference(avatar_split_clause, [], [f197, f302, f602])).
fof(f197, plain, ((e2 = op(e3, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f605, plain, (~ spl15_68 | ~ spl15_4), inference(avatar_split_clause, [], [f198, f289, f602])).
fof(f198, plain, (~ (e3 = op(e3, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f600, plain, (~ spl15_67 | spl15_16), inference(avatar_split_clause, [], [f193, f340, f595])).
fof(f193, plain, ((e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e0 = op(e0, e0)) & (e3 = op(e0, e3)) & (e3 = op(e3, e0))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f599, plain, (~ spl15_67 | spl15_52), inference(avatar_split_clause, [], [f194, f493, f595])).
fof(f194, plain, ((e3 = op(e0, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f598, plain, (~ spl15_67 | ~ spl15_61), inference(avatar_split_clause, [], [f195, f532, f595])).
fof(f195, plain, (~ (e0 = op(e0, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f593, plain, (~ spl15_66 | spl15_12), inference(avatar_split_clause, [], [f190, f323, f588])).
fof(f190, plain, ((e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e1 = op(e1, e1)) & (e3 = op(e1, e3)) & (e3 = op(e3, e1))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f592, plain, (~ spl15_66 | spl15_36), inference(avatar_split_clause, [], [f191, f425, f588])).
fof(f191, plain, ((e3 = op(e1, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f591, plain, (~ spl15_66 | ~ spl15_42), inference(avatar_split_clause, [], [f192, f451, f588])).
fof(f192, plain, (~ (e1 = op(e1, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f586, plain, (~ spl15_65 | spl15_8), inference(avatar_split_clause, [], [f187, f306, f581])).
fof(f187, plain, ((e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e2 = op(e2, e2)) & (e3 = op(e2, e3)) & (e3 = op(e3, e2))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f585, plain, (~ spl15_65 | spl15_20), inference(avatar_split_clause, [], [f188, f357, f581])).
fof(f188, plain, ((e3 = op(e2, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f584, plain, (~ spl15_65 | ~ spl15_23), inference(avatar_split_clause, [], [f189, f370, f581])).
fof(f189, plain, (~ (e2 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f579, plain, (spl15_61 | spl15_57 | spl15_53 | spl15_49), inference(avatar_split_clause, [], [f101, f481, f498, f515, f532])).
fof(f101, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax2)).
fof(f577, plain, (spl15_62 | spl15_58 | spl15_54 | spl15_50), inference(avatar_split_clause, [], [f103, f485, f502, f519, f536])).
fof(f103, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f575, plain, (spl15_63 | spl15_59 | spl15_55 | spl15_51), inference(avatar_split_clause, [], [f105, f489, f506, f523, f540])).
fof(f105, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f574, plain, (spl15_63 | spl15_47 | spl15_31 | spl15_15), inference(avatar_split_clause, [], [f106, f336, f404, f472, f540])).
fof(f106, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f573, plain, (spl15_64 | spl15_60 | spl15_56 | spl15_52), inference(avatar_split_clause, [], [f107, f493, f510, f527, f544])).
fof(f107, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f572, plain, (spl15_64 | spl15_48 | spl15_32 | spl15_16), inference(avatar_split_clause, [], [f108, f340, f408, f476, f544])).
fof(f108, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f571, plain, (spl15_45 | spl15_41 | spl15_37 | spl15_33), inference(avatar_split_clause, [], [f109, f413, f430, f447, f464])).
fof(f109, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f570, plain, (spl15_57 | spl15_41 | spl15_25 | spl15_9), inference(avatar_split_clause, [], [f110, f311, f379, f447, f515])).
fof(f110, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f569, plain, (spl15_46 | spl15_42 | spl15_38 | spl15_34), inference(avatar_split_clause, [], [f111, f417, f434, f451, f468])).
fof(f111, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f568, plain, (spl15_58 | spl15_42 | spl15_26 | spl15_10), inference(avatar_split_clause, [], [f112, f315, f383, f451, f519])).
fof(f112, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f567, plain, (spl15_47 | spl15_43 | spl15_39 | spl15_35), inference(avatar_split_clause, [], [f113, f421, f438, f455, f472])).
fof(f113, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f566, plain, (spl15_59 | spl15_43 | spl15_27 | spl15_11), inference(avatar_split_clause, [], [f114, f319, f387, f455, f523])).
fof(f114, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f565, plain, (spl15_48 | spl15_44 | spl15_40 | spl15_36), inference(avatar_split_clause, [], [f115, f425, f442, f459, f476])).
fof(f115, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f564, plain, (spl15_60 | spl15_44 | spl15_28 | spl15_12), inference(avatar_split_clause, [], [f116, f323, f391, f459, f527])).
fof(f116, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f563, plain, (spl15_29 | spl15_25 | spl15_21 | spl15_17), inference(avatar_split_clause, [], [f117, f345, f362, f379, f396])).
fof(f117, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f562, plain, (spl15_53 | spl15_37 | spl15_21 | spl15_5), inference(avatar_split_clause, [], [f118, f294, f362, f430, f498])).
fof(f118, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f561, plain, (spl15_30 | spl15_26 | spl15_22 | spl15_18), inference(avatar_split_clause, [], [f119, f349, f366, f383, f400])).
fof(f119, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f560, plain, (spl15_54 | spl15_38 | spl15_22 | spl15_6), inference(avatar_split_clause, [], [f120, f298, f366, f434, f502])).
fof(f120, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f557, plain, (spl15_32 | spl15_28 | spl15_24 | spl15_20), inference(avatar_split_clause, [], [f123, f357, f374, f391, f408])).
fof(f123, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f556, plain, (spl15_56 | spl15_40 | spl15_24 | spl15_8), inference(avatar_split_clause, [], [f124, f306, f374, f442, f510])).
fof(f124, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f555, plain, (spl15_13 | spl15_9 | spl15_5 | spl15_1), inference(avatar_split_clause, [], [f125, f277, f294, f311, f328])).
fof(f125, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f554, plain, (spl15_49 | spl15_33 | spl15_17 | spl15_1), inference(avatar_split_clause, [], [f126, f277, f345, f413, f481])).
fof(f126, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f553, plain, (spl15_14 | spl15_10 | spl15_6 | spl15_2), inference(avatar_split_clause, [], [f127, f281, f298, f315, f332])).
fof(f127, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f552, plain, (spl15_50 | spl15_34 | spl15_18 | spl15_2), inference(avatar_split_clause, [], [f128, f281, f349, f417, f485])).
fof(f128, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f551, plain, (spl15_15 | spl15_11 | spl15_7 | spl15_3), inference(avatar_split_clause, [], [f129, f285, f302, f319, f336])).
fof(f129, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f550, plain, (spl15_51 | spl15_35 | spl15_19 | spl15_3), inference(avatar_split_clause, [], [f130, f285, f353, f421, f489])).
fof(f130, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f549, plain, (spl15_16 | spl15_12 | spl15_8 | spl15_4), inference(avatar_split_clause, [], [f131, f289, f306, f323, f340])).
fof(f131, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f547, plain, (spl15_61 | spl15_62 | spl15_63 | spl15_64), inference(avatar_split_clause, [], [f85, f544, f540, f536, f532])).
fof(f85, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG146+1.p', ax1)).
fof(f530, plain, (spl15_57 | spl15_58 | spl15_59 | spl15_60), inference(avatar_split_clause, [], [f86, f527, f523, f519, f515])).
fof(f86, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f513, plain, (spl15_53 | spl15_54 | spl15_55 | spl15_56), inference(avatar_split_clause, [], [f87, f510, f506, f502, f498])).
fof(f87, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f496, plain, (spl15_49 | spl15_50 | spl15_51 | spl15_52), inference(avatar_split_clause, [], [f88, f493, f489, f485, f481])).
fof(f88, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f479, plain, (spl15_45 | spl15_46 | spl15_47 | spl15_48), inference(avatar_split_clause, [], [f89, f476, f472, f468, f464])).
fof(f89, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f462, plain, (spl15_41 | spl15_42 | spl15_43 | spl15_44), inference(avatar_split_clause, [], [f90, f459, f455, f451, f447])).
fof(f90, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f428, plain, (spl15_33 | spl15_34 | spl15_35 | spl15_36), inference(avatar_split_clause, [], [f92, f425, f421, f417, f413])).
fof(f92, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f411, plain, (spl15_29 | spl15_30 | spl15_31 | spl15_32), inference(avatar_split_clause, [], [f93, f408, f404, f400, f396])).
fof(f93, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f394, plain, (spl15_25 | spl15_26 | spl15_27 | spl15_28), inference(avatar_split_clause, [], [f94, f391, f387, f383, f379])).
fof(f94, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f377, plain, (spl15_21 | spl15_22 | spl15_23 | spl15_24), inference(avatar_split_clause, [], [f95, f374, f370, f366, f362])).
fof(f95, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f360, plain, (spl15_17 | spl15_18 | spl15_19 | spl15_20), inference(avatar_split_clause, [], [f96, f357, f353, f349, f345])).
fof(f96, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f343, plain, (spl15_13 | spl15_14 | spl15_15 | spl15_16), inference(avatar_split_clause, [], [f97, f340, f336, f332, f328])).
fof(f97, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f326, plain, (spl15_9 | spl15_10 | spl15_11 | spl15_12), inference(avatar_split_clause, [], [f98, f323, f319, f315, f311])).
fof(f98, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f309, plain, (spl15_5 | spl15_6 | spl15_7 | spl15_8), inference(avatar_split_clause, [], [f99, f306, f302, f298, f294])).
fof(f99, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f292, plain, (spl15_1 | spl15_2 | spl15_3 | spl15_4), inference(avatar_split_clause, [], [f100, f289, f285, f281, f277])).
fof(f100, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).