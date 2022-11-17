fof(f2162, plain, $false, inference(avatar_sat_refutation, [], [f695, f884, f935, f937, f939, f946, f952, f960, f967, f973, f979, f981, f988, f994, f1000, f1009, f1015, f1023, f1030, f1037, f1043, f1051, f1057, f1065, f1072, f1078, f1086, f1092, f1099, f1105, f1110, f1115, f1130, f1150, f1195, f1230, f1232, f1238, f1241, f1295, f1516, f1520, f1547, f1596, f1609, f1641, f1661, f1709, f1767, f1789, f1817, f1874, f1893, f1916, f1928, f1938, f1993, f2031, f2069, f2111, f2118, f2137])).
fof(f2137, plain, (~ spl24_94 | ~ spl24_95), inference(avatar_contradiction_clause, [], [f2136])).
fof(f2136, plain, ($false | (~ spl24_94 | ~ spl24_95)), inference(subsumption_resolution, [], [f2135, f254])).
fof(f254, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG063+1.p', ax5)).
fof(f2135, plain, ((e3 = e4) | (~ spl24_94 | ~ spl24_95)), inference(backward_demodulation, [], [f736, f732])).
fof(f732, plain, ((e3 = op(e1, e1)) | ~ spl24_94), inference(avatar_component_clause, [], [f730])).
fof(f730, plain, (spl24_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_94])])).
fof(f736, plain, ((e4 = op(e1, e1)) | ~ spl24_95), inference(avatar_component_clause, [], [f734])).
fof(f734, plain, (spl24_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_95])])).
fof(f2118, plain, (spl24_121 | ~ spl24_126), inference(avatar_split_clause, [], [f2011, f865, f844])).
fof(f844, plain, (spl24_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_121])])).
fof(f865, plain, (spl24_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_126])])).
fof(f2011, plain, ((e0 = op(e0, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f84, f867])).
fof(f867, plain, ((e0 = unit) | ~ spl24_126), inference(avatar_component_clause, [], [f865])).
fof(f84, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG063+1.p', ax2)).
fof(f2111, plain, (spl24_105 | ~ spl24_126), inference(avatar_split_clause, [], [f2019, f865, f776])).
fof(f776, plain, (spl24_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_105])])).
fof(f2019, plain, ((e4 = op(e0, e4)) | ~ spl24_126), inference(backward_demodulation, [], [f92, f867])).
fof(f92, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2069, plain, (~ spl24_62 | ~ spl24_64), inference(avatar_contradiction_clause, [], [f2068])).
fof(f2068, plain, ($false | (~ spl24_62 | ~ spl24_64)), inference(subsumption_resolution, [], [f2067, f250])).
fof(f250, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f2067, plain, ((e1 = e3) | (~ spl24_62 | ~ spl24_64)), inference(backward_demodulation, [], [f606, f598])).
fof(f598, plain, ((e1 = op(e2, e2)) | ~ spl24_62), inference(avatar_component_clause, [], [f596])).
fof(f596, plain, (spl24_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_62])])).
fof(f606, plain, ((e3 = op(e2, e2)) | ~ spl24_64), inference(avatar_component_clause, [], [f604])).
fof(f604, plain, (spl24_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_64])])).
fof(f2031, plain, (spl24_109 | ~ spl24_126), inference(avatar_split_clause, [], [f2017, f865, f793])).
fof(f793, plain, (spl24_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_109])])).
fof(f2017, plain, ((e3 = op(e0, e3)) | ~ spl24_126), inference(backward_demodulation, [], [f90, f867])).
fof(f90, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f1993, plain, (~ spl24_33 | ~ spl24_83), inference(avatar_split_clause, [], [f1992, f684, f474])).
fof(f474, plain, (spl24_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_33])])).
fof(f684, plain, (spl24_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_83])])).
fof(f1992, plain, (~ (e2 = op(e3, e3)) | ~ spl24_83), inference(forward_demodulation, [], [f180, f686])).
fof(f686, plain, ((e2 = op(e1, e3)) | ~ spl24_83), inference(avatar_component_clause, [], [f684])).
fof(f180, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG063+1.p', ax4)).
fof(f1938, plain, (~ spl24_64 | ~ spl24_65), inference(avatar_contradiction_clause, [], [f1937])).
fof(f1937, plain, ($false | (~ spl24_64 | ~ spl24_65)), inference(subsumption_resolution, [], [f1936, f254])).
fof(f1936, plain, ((e3 = e4) | (~ spl24_64 | ~ spl24_65)), inference(forward_demodulation, [], [f610, f606])).
fof(f610, plain, ((e4 = op(e2, e2)) | ~ spl24_65), inference(avatar_component_clause, [], [f608])).
fof(f608, plain, (spl24_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_65])])).
fof(f1928, plain, (~ spl24_91 | ~ spl24_95), inference(avatar_contradiction_clause, [], [f1927])).
fof(f1927, plain, ($false | (~ spl24_91 | ~ spl24_95)), inference(subsumption_resolution, [], [f1926, f248])).
fof(f248, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f1926, plain, ((e0 = e4) | (~ spl24_91 | ~ spl24_95)), inference(backward_demodulation, [], [f736, f720])).
fof(f720, plain, ((e0 = op(e1, e1)) | ~ spl24_91), inference(avatar_component_clause, [], [f718])).
fof(f718, plain, (spl24_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_91])])).
fof(f1916, plain, (~ spl24_31 | ~ spl24_106), inference(avatar_split_clause, [], [f1909, f781, f466])).
fof(f466, plain, (spl24_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_31])])).
fof(f781, plain, (spl24_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_106])])).
fof(f1909, plain, (~ (e0 = op(e3, e3)) | ~ spl24_106), inference(backward_demodulation, [], [f177, f783])).
fof(f783, plain, ((e0 = op(e0, e3)) | ~ spl24_106), inference(avatar_component_clause, [], [f781])).
fof(f177, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1893, plain, (spl24_106 | ~ spl24_129), inference(avatar_split_clause, [], [f1881, f877, f781])).
fof(f877, plain, (spl24_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_129])])).
fof(f1881, plain, ((e0 = op(e0, e3)) | ~ spl24_129), inference(backward_demodulation, [], [f85, f879])).
fof(f879, plain, ((e3 = unit) | ~ spl24_129), inference(avatar_component_clause, [], [f877])).
fof(f85, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f1874, plain, (~ spl24_124 | ~ spl24_49), inference(avatar_split_clause, [], [f1873, f541, f856])).
fof(f856, plain, (spl24_124 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl24_124])])).
fof(f541, plain, (spl24_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_49])])).
fof(f1873, plain, (~ (op(e0, e0) = e3) | ~ spl24_49), inference(forward_demodulation, [], [f147, f543])).
fof(f543, plain, ((e3 = op(e3, e0)) | ~ spl24_49), inference(avatar_component_clause, [], [f541])).
fof(f147, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1817, plain, (~ spl24_1 | ~ spl24_3), inference(avatar_contradiction_clause, [], [f1816])).
fof(f1816, plain, ($false | (~ spl24_1 | ~ spl24_3)), inference(subsumption_resolution, [], [f1815, f246])).
fof(f246, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f1815, plain, ((e0 = e2) | (~ spl24_1 | ~ spl24_3)), inference(backward_demodulation, [], [f350, f342])).
fof(f342, plain, ((e0 = op(e4, e4)) | ~ spl24_1), inference(avatar_component_clause, [], [f340])).
fof(f340, plain, (spl24_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_1])])).
fof(f350, plain, ((e2 = op(e4, e4)) | ~ spl24_3), inference(avatar_component_clause, [], [f348])).
fof(f348, plain, (spl24_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_3])])).
fof(f1789, plain, (~ spl24_92 | ~ spl24_95), inference(avatar_contradiction_clause, [], [f1788])).
fof(f1788, plain, ($false | (~ spl24_92 | ~ spl24_95)), inference(subsumption_resolution, [], [f1787, f251])).
fof(f251, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f1787, plain, ((e1 = e4) | (~ spl24_92 | ~ spl24_95)), inference(backward_demodulation, [], [f736, f724])).
fof(f724, plain, ((e1 = op(e1, e1)) | ~ spl24_92), inference(avatar_component_clause, [], [f722])).
fof(f722, plain, (spl24_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_92])])).
fof(f1767, plain, (~ spl24_86 | ~ spl24_128), inference(avatar_contradiction_clause, [], [f1766])).
fof(f1766, plain, ($false | (~ spl24_86 | ~ spl24_128)), inference(subsumption_resolution, [], [f1765, f245])).
fof(f245, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f1765, plain, ((e0 = e1) | (~ spl24_86 | ~ spl24_128)), inference(forward_demodulation, [], [f1750, f699])).
fof(f699, plain, ((e0 = op(e1, e2)) | ~ spl24_86), inference(avatar_component_clause, [], [f697])).
fof(f697, plain, (spl24_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_86])])).
fof(f1750, plain, ((e1 = op(e1, e2)) | ~ spl24_128), inference(backward_demodulation, [], [f87, f875])).
fof(f875, plain, ((e2 = unit) | ~ spl24_128), inference(avatar_component_clause, [], [f873])).
fof(f873, plain, (spl24_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_128])])).
fof(f87, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f1709, plain, (~ spl24_2 | ~ spl24_3), inference(avatar_contradiction_clause, [], [f1708])).
fof(f1708, plain, ($false | (~ spl24_2 | ~ spl24_3)), inference(subsumption_resolution, [], [f1707, f249])).
fof(f249, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f1707, plain, ((e1 = e2) | (~ spl24_2 | ~ spl24_3)), inference(backward_demodulation, [], [f350, f346])).
fof(f346, plain, ((e1 = op(e4, e4)) | ~ spl24_2), inference(avatar_component_clause, [], [f344])).
fof(f344, plain, (spl24_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_2])])).
fof(f1661, plain, (~ spl24_86 | ~ spl24_127), inference(avatar_contradiction_clause, [], [f1660])).
fof(f1660, plain, ($false | (~ spl24_86 | ~ spl24_127)), inference(subsumption_resolution, [], [f1659, f246])).
fof(f1659, plain, ((e0 = e2) | (~ spl24_86 | ~ spl24_127)), inference(forward_demodulation, [], [f1646, f699])).
fof(f1646, plain, ((e2 = op(e1, e2)) | ~ spl24_127), inference(backward_demodulation, [], [f88, f871])).
fof(f871, plain, ((e1 = unit) | ~ spl24_127), inference(avatar_component_clause, [], [f869])).
fof(f869, plain, (spl24_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_127])])).
fof(f88, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f1641, plain, (~ spl24_109 | ~ spl24_84), inference(avatar_split_clause, [], [f1640, f688, f793])).
fof(f688, plain, (spl24_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_84])])).
fof(f1640, plain, (~ (e3 = op(e0, e3)) | ~ spl24_84), inference(forward_demodulation, [], [f175, f690])).
fof(f690, plain, ((e3 = op(e1, e3)) | ~ spl24_84), inference(avatar_component_clause, [], [f688])).
fof(f175, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1609, plain, (~ spl24_13 | ~ spl24_3), inference(avatar_split_clause, [], [f1608, f348, f390])).
fof(f390, plain, (spl24_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_13])])).
fof(f1608, plain, (~ (e2 = op(e4, e2)) | ~ spl24_3), inference(forward_demodulation, [], [f243, f350])).
fof(f243, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1596, plain, (~ spl24_3 | ~ spl24_4), inference(avatar_contradiction_clause, [], [f1595])).
fof(f1595, plain, ($false | (~ spl24_3 | ~ spl24_4)), inference(subsumption_resolution, [], [f1594, f252])).
fof(f252, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f1594, plain, ((e2 = e3) | (~ spl24_3 | ~ spl24_4)), inference(backward_demodulation, [], [f354, f350])).
fof(f354, plain, ((e3 = op(e4, e4)) | ~ spl24_4), inference(avatar_component_clause, [], [f352])).
fof(f352, plain, (spl24_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_4])])).
fof(f1547, plain, (~ spl24_82 | ~ spl24_97), inference(avatar_split_clause, [], [f1544, f743, f680])).
fof(f680, plain, (spl24_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_82])])).
fof(f743, plain, (spl24_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_97])])).
fof(f1544, plain, (~ (e1 = op(e1, e3)) | ~ spl24_97), inference(backward_demodulation, [], [f207, f745])).
fof(f745, plain, ((e1 = op(e1, e0)) | ~ spl24_97), inference(avatar_component_clause, [], [f743])).
fof(f207, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1520, plain, (spl24_49 | ~ spl24_126), inference(avatar_split_clause, [], [f1511, f865, f541])).
fof(f1511, plain, ((e3 = op(e3, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f91, f867])).
fof(f91, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f1516, plain, (spl24_97 | ~ spl24_126), inference(avatar_split_clause, [], [f1507, f865, f743])).
fof(f1507, plain, ((e1 = op(e1, e0)) | ~ spl24_126), inference(backward_demodulation, [], [f87, f867])).
fof(f1295, plain, (~ spl24_63 | ~ spl24_64), inference(avatar_contradiction_clause, [], [f1294])).
fof(f1294, plain, ($false | (~ spl24_63 | ~ spl24_64)), inference(subsumption_resolution, [], [f1293, f252])).
fof(f1293, plain, ((e2 = e3) | (~ spl24_63 | ~ spl24_64)), inference(backward_demodulation, [], [f606, f602])).
fof(f602, plain, ((e2 = op(e2, e2)) | ~ spl24_63), inference(avatar_component_clause, [], [f600])).
fof(f600, plain, (spl24_63 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_63])])).
fof(f1241, plain, (~ spl24_81 | ~ spl24_86), inference(avatar_split_clause, [], [f1236, f697, f676])).
fof(f676, plain, (spl24_81 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_81])])).
fof(f1236, plain, (~ (e0 = op(e1, e3)) | ~ spl24_86), inference(backward_demodulation, [], [f212, f699])).
fof(f212, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1238, plain, (~ spl24_61 | ~ spl24_86), inference(avatar_split_clause, [], [f1233, f697, f592])).
fof(f592, plain, (spl24_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_61])])).
fof(f1233, plain, (~ (e0 = op(e2, e2)) | ~ spl24_86), inference(backward_demodulation, [], [f169, f699])).
fof(f169, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f1232, plain, (spl24_3 | ~ spl24_95), inference(avatar_split_clause, [], [f1225, f734, f348])).
fof(f1225, plain, ((e2 = op(e4, e4)) | ~ spl24_95), inference(backward_demodulation, [], [f256, f736])).
fof(f256, plain, (e2 = op(op(e1, e1), op(e1, e1))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(e1, e1)) & (e3 = op(op(op(e1, e1), op(e1, e1)), op(op(e1, e1), op(e1, e1)))) & (e2 = op(op(e1, e1), op(e1, e1))) & (e0 = op(e1, op(op(e1, e1), op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG063+1.p', ax6)).
fof(f1230, plain, (~ spl24_85 | ~ spl24_95), inference(avatar_split_clause, [], [f1223, f734, f692])).
fof(f692, plain, (spl24_85 <=> (e4 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_85])])).
fof(f1223, plain, (~ (e4 = op(e1, e3)) | ~ spl24_95), inference(backward_demodulation, [], [f210, f736])).
fof(f210, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1195, plain, (~ spl24_5 | ~ spl24_105), inference(avatar_split_clause, [], [f1191, f776, f356])).
fof(f356, plain, (spl24_5 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_5])])).
fof(f1191, plain, (~ (e4 = op(e4, e4)) | ~ spl24_105), inference(backward_demodulation, [], [f188, f778])).
fof(f778, plain, ((e4 = op(e0, e4)) | ~ spl24_105), inference(avatar_component_clause, [], [f776])).
fof(f188, plain, ~ (op(e0, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1150, plain, (~ spl24_106 | ~ spl24_121), inference(avatar_split_clause, [], [f1142, f844, f781])).
fof(f1142, plain, (~ (e0 = op(e0, e3)) | ~ spl24_121), inference(backward_demodulation, [], [f197, f846])).
fof(f846, plain, ((e0 = op(e0, e0)) | ~ spl24_121), inference(avatar_component_clause, [], [f844])).
fof(f197, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f1130, plain, (spl24_13 | ~ spl24_130), inference(avatar_split_clause, [], [f1120, f881, f390])).
fof(f881, plain, (spl24_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl24_130])])).
fof(f1120, plain, ((e2 = op(e4, e2)) | ~ spl24_130), inference(backward_demodulation, [], [f88, f883])).
fof(f883, plain, ((e4 = unit) | ~ spl24_130), inference(avatar_component_clause, [], [f881])).
fof(f1115, plain, (spl24_121 | spl24_91 | spl24_61 | spl24_31 | spl24_1), inference(avatar_split_clause, [], [f331, f340, f466, f592, f718, f844])).
fof(f331, plain, ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f18, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP8), inference(usedef, [], [e18])).
fof(e18, plain, (sP8 <=> (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f19, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP9), inference(usedef, [], [e19])).
fof(e19, plain, (sP9 <=> (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f20, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e20])).
fof(e20, plain, (sP10 <=> (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f21, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP11), inference(usedef, [], [e21])).
fof(e21, plain, (sP11 <=> (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f22, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP12), inference(usedef, [], [e22])).
fof(e22, plain, (sP12 <=> (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f23, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP13), inference(usedef, [], [e23])).
fof(e23, plain, (sP13 <=> (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f24, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP14), inference(usedef, [], [e24])).
fof(e24, plain, (sP14 <=> (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f25, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP15), inference(usedef, [], [e25])).
fof(e25, plain, (sP15 <=> (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f26, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP16), inference(usedef, [], [e26])).
fof(e26, plain, (sP16 <=> (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f27, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP17), inference(usedef, [], [e27])).
fof(e27, plain, (sP17 <=> (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f28, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP18), inference(usedef, [], [e28])).
fof(e28, plain, (sP18 <=> (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f29, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP19), inference(usedef, [], [e29])).
fof(e29, plain, (sP19 <=> (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f30, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP20), inference(usedef, [], [e30])).
fof(e30, plain, (sP20 <=> (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f31, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP21), inference(usedef, [], [e31])).
fof(e31, plain, (sP21 <=> (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f32, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP22), inference(usedef, [], [e32])).
fof(e32, plain, (sP22 <=> (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f33, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP23), inference(usedef, [], [e33])).
fof(e33, plain, (sP23 <=> (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f9, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG063+1.p', co1)).
fof(f1110, plain, (spl24_154 | spl24_153 | spl24_152 | spl24_151 | spl24_150 | spl24_149 | spl24_148 | spl24_147 | spl24_146 | spl24_145 | spl24_144 | spl24_143 | spl24_142 | spl24_141 | spl24_140 | spl24_139 | spl24_138 | spl24_137 | spl24_136 | spl24_135 | spl24_134 | spl24_133 | spl24_132 | spl24_131 | spl24_5), inference(avatar_split_clause, [], [f336, f356, f941, f948, f955, f962, f969, f976, f983, f990, f997, f1004, f1011, f1018, f1025, f1032, f1039, f1046, f1053, f1060, f1067, f1074, f1081, f1088, f1095, f1102])).
fof(f1102, plain, (spl24_154 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl24_154])])).
fof(f1095, plain, (spl24_153 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl24_153])])).
fof(f1088, plain, (spl24_152 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl24_152])])).
fof(f1081, plain, (spl24_151 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl24_151])])).
fof(f1074, plain, (spl24_150 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl24_150])])).
fof(f1067, plain, (spl24_149 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl24_149])])).
fof(f1060, plain, (spl24_148 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl24_148])])).
fof(f1053, plain, (spl24_147 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl24_147])])).
fof(f1046, plain, (spl24_146 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl24_146])])).
fof(f1039, plain, (spl24_145 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl24_145])])).
fof(f1032, plain, (spl24_144 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl24_144])])).
fof(f1025, plain, (spl24_143 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl24_143])])).
fof(f1018, plain, (spl24_142 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl24_142])])).
fof(f1011, plain, (spl24_141 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl24_141])])).
fof(f1004, plain, (spl24_140 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl24_140])])).
fof(f997, plain, (spl24_139 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl24_139])])).
fof(f990, plain, (spl24_138 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl24_138])])).
fof(f983, plain, (spl24_137 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl24_137])])).
fof(f976, plain, (spl24_136 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl24_136])])).
fof(f969, plain, (spl24_135 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl24_135])])).
fof(f962, plain, (spl24_134 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl24_134])])).
fof(f955, plain, (spl24_133 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl24_133])])).
fof(f948, plain, (spl24_132 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl24_132])])).
fof(f941, plain, (spl24_131 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl24_131])])).
fof(f336, plain, ((e4 = op(e4, e4)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f1105, plain, (~ spl24_154 | ~ spl24_121), inference(avatar_split_clause, [], [f330, f844, f1102])).
fof(f330, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1099, plain, (~ spl24_153 | spl24_91), inference(avatar_split_clause, [], [f326, f718, f1095])).
fof(f326, plain, ((e0 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1092, plain, (~ spl24_152 | spl24_61), inference(avatar_split_clause, [], [f323, f592, f1088])).
fof(f323, plain, ((e0 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f1086, plain, (~ spl24_151 | spl24_124), inference(avatar_split_clause, [], [f319, f856, f1081])).
fof(f319, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f13])).
fof(f1078, plain, (~ spl24_150 | spl24_1), inference(avatar_split_clause, [], [f317, f340, f1074])).
fof(f317, plain, ((e0 = op(e4, e4)) | ~ sP4), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f1072, plain, (~ spl24_149 | spl24_91), inference(avatar_split_clause, [], [f313, f718, f1067])).
fof(f313, plain, ((e0 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f1065, plain, (~ spl24_148 | spl24_92), inference(avatar_split_clause, [], [f310, f722, f1060])).
fof(f310, plain, ((e1 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f1057, plain, (~ spl24_147 | spl24_62), inference(avatar_split_clause, [], [f308, f596, f1053])).
fof(f308, plain, ((e1 = op(e2, e2)) | ~ sP7), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP7), inference(nnf_transformation, [], [f17])).
fof(f1051, plain, (~ spl24_146 | spl24_94), inference(avatar_split_clause, [], [f304, f730, f1046])).
fof(f304, plain, ((e3 = op(e1, e1)) | ~ sP8), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP8), inference(nnf_transformation, [], [f18])).
fof(f1043, plain, (~ spl24_145 | spl24_2), inference(avatar_split_clause, [], [f302, f344, f1039])).
fof(f302, plain, ((e1 = op(e4, e4)) | ~ sP9), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP9), inference(nnf_transformation, [], [f19])).
fof(f1037, plain, (~ spl24_144 | spl24_61), inference(avatar_split_clause, [], [f298, f592, f1032])).
fof(f298, plain, ((e0 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1030, plain, (~ spl24_143 | spl24_62), inference(avatar_split_clause, [], [f295, f596, f1025])).
fof(f295, plain, ((e1 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1023, plain, (~ spl24_142 | spl24_63), inference(avatar_split_clause, [], [f292, f600, f1018])).
fof(f292, plain, ((e2 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1015, plain, (~ spl24_141 | spl24_33), inference(avatar_split_clause, [], [f290, f474, f1011])).
fof(f290, plain, ((e2 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1009, plain, (~ spl24_140 | spl24_65), inference(avatar_split_clause, [], [f286, f608, f1004])).
fof(f286, plain, ((e4 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1000, plain, (~ spl24_139 | ~ spl24_49), inference(avatar_split_clause, [], [f285, f541, f997])).
fof(f285, plain, (~ (e3 = op(e3, e0)) | ~ sP15), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f994, plain, (~ spl24_138 | spl24_94), inference(avatar_split_clause, [], [f281, f730, f990])).
fof(f281, plain, ((e3 = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f988, plain, (~ spl24_137 | spl24_33), inference(avatar_split_clause, [], [f277, f474, f983])).
fof(f277, plain, ((e2 = op(e3, e3)) | ~ sP17), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f981, plain, (~ spl24_136 | spl24_34), inference(avatar_split_clause, [], [f274, f478, f976])).
fof(f478, plain, (spl24_34 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_34])])).
fof(f274, plain, ((e3 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f979, plain, (~ spl24_136 | ~ spl24_34), inference(avatar_split_clause, [], [f276, f478, f976])).
fof(f276, plain, (~ (e3 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f40])).
fof(f973, plain, (~ spl24_135 | spl24_4), inference(avatar_split_clause, [], [f272, f352, f969])).
fof(f272, plain, ((e3 = op(e4, e4)) | ~ sP19), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f967, plain, (~ spl24_134 | spl24_1), inference(avatar_split_clause, [], [f268, f340, f962])).
fof(f268, plain, ((e0 = op(e4, e4)) | ~ sP20), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f960, plain, (~ spl24_133 | spl24_2), inference(avatar_split_clause, [], [f265, f344, f955])).
fof(f265, plain, ((e1 = op(e4, e4)) | ~ sP21), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f952, plain, (~ spl24_132 | spl24_65), inference(avatar_split_clause, [], [f263, f608, f948])).
fof(f263, plain, ((e4 = op(e2, e2)) | ~ sP22), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f946, plain, (~ spl24_131 | spl24_4), inference(avatar_split_clause, [], [f259, f352, f941])).
fof(f259, plain, ((e3 = op(e4, e4)) | ~ sP23), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f939, plain, spl24_86, inference(avatar_split_clause, [], [f938, f697])).
fof(f938, plain, (e0 = op(e1, e2)), inference(forward_demodulation, [], [f255, f256])).
fof(f255, plain, (e0 = op(e1, op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f6])).
fof(f937, plain, spl24_64, inference(avatar_split_clause, [], [f936, f604])).
fof(f936, plain, (e3 = op(e2, e2)), inference(backward_demodulation, [], [f257, f256])).
fof(f257, plain, (e3 = op(op(op(e1, e1), op(e1, e1)), op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f6])).
fof(f935, plain, spl24_95, inference(avatar_split_clause, [], [f258, f734])).
fof(f258, plain, (e4 = op(e1, e1)), inference(cnf_transformation, [], [f6])).
fof(f884, plain, (spl24_126 | spl24_127 | spl24_128 | spl24_129 | spl24_130), inference(avatar_split_clause, [], [f94, f881, f877, f873, f869, f865])).
fof(f94, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f695, plain, (spl24_81 | spl24_82 | spl24_83 | spl24_84 | spl24_85), inference(avatar_split_clause, [], [f67, f692, f688, f684, f680, f676])).
fof(f67, plain, ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG063+1.p', ax1)).