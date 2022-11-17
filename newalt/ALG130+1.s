fof(f2171, plain, $false, inference(avatar_sat_refutation, [], [f235, f252, f269, f286, f337, f388, f439, f456, f473, f491, f493, f499, f500, f503, f504, f505, f507, f508, f509, f510, f513, f515, f517, f518, f519, f541, f546, f555, f570, f584, f589, f609, f614, f619, f634, f639, f649, f669, f679, f684, f694, f719, f729, f734, f758, f759, f760, f788, f789, f796, f849, f851, f856, f868, f920, f935, f937, f962, f981, f995, f1017, f1022, f1024, f1028, f1036, f1046, f1048, f1049, f1053, f1108, f1132, f1133, f1134, f1144, f1145, f1156, f1157, f1160, f1188, f1210, f1214, f1233, f1266, f1282, f1296, f1316, f1346, f1387, f1397, f1412, f1419, f1438, f1451, f1460, f1463, f1479, f1491, f1515, f1518, f1528, f1531, f1560, f1585, f1601, f1610, f1611, f1613, f1616, f1619, f1622, f1625, f1626, f1629, f1654, f1669, f1678, f1680, f1681, f1698, f1717, f1734, f1756, f1764, f1765, f1776, f1777, f1800, f1802, f1810, f1811, f1822, f1824, f1825, f1853, f1862, f1895, f1904, f1934, f1947, f1956, f1967, f1970, f1971, f2025, f2053, f2054, f2066, f2074, f2075, f2085, f2087, f2100, f2120, f2121, f2124])).
fof(f2124, plain, (~ spl3_32 | ~ spl3_51 | ~ spl3_77), inference(avatar_contradiction_clause, [], [f2123])).
fof(f2123, plain, ($false | (~ spl3_32 | ~ spl3_51 | ~ spl3_77)), inference(subsumption_resolution, [], [f2122, f162])).
fof(f162, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax4)).
fof(f2122, plain, ((e2 = e3) | (~ spl3_32 | ~ spl3_51 | ~ spl3_77)), inference(forward_demodulation, [], [f2117, f434])).
fof(f434, plain, ((e2 = op(e0, e3)) | ~ spl3_51), inference(avatar_component_clause, [], [f432])).
fof(f432, plain, (spl3_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_51])])).
fof(f2117, plain, ((e3 = op(e0, e3)) | (~ spl3_32 | ~ spl3_77)), inference(backward_demodulation, [], [f583, f353])).
fof(f353, plain, ((e3 = op(e2, e0)) | ~ spl3_32), inference(avatar_component_clause, [], [f351])).
fof(f351, plain, (spl3_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_32])])).
fof(f583, plain, ((op(e2, e0) = op(e0, op(e2, e0))) | ~ spl3_77), inference(avatar_component_clause, [], [f581])).
fof(f581, plain, (spl3_77 <=> (op(e2, e0) = op(e0, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_77])])).
fof(f2121, plain, (spl3_5 | ~ spl3_32), inference(avatar_split_clause, [], [f2116, f351, f237])).
fof(f237, plain, (spl3_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_5])])).
fof(f2116, plain, ((e0 = op(e3, e2)) | ~ spl3_32), inference(backward_demodulation, [], [f183, f353])).
fof(f183, plain, (e0 = op(op(e2, e0), e2)), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((((op(e3, e3) = op(e3, op(e3, e3))) & (op(e2, e3) = op(e3, op(e2, e3))) & (op(e1, e3) = op(e3, op(e1, e3))) & (op(e0, e3) = op(e3, op(e0, e3)))) | sP2 | sP1 | sP0) & (e3 = op(op(e3, e3), e3)) & (e2 = op(op(e3, e2), e3)) & (e1 = op(op(e3, e1), e3)) & (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e2), e2)) & (e1 = op(op(e2, e1), e2)) & (e0 = op(op(e2, e0), e2)) & (e3 = op(op(e1, e3), e1)) & (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e1), e1)) & (e0 = op(op(e1, e0), e1)) & (e3 = op(op(e0, e3), e0)) & (e2 = op(op(e0, e2), e0)) & (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e0), e0))), inference(definition_folding, [], [f5, e56, e55, e54])).
fof(f54, plain, (((op(e3, e0) = op(e0, op(e3, e0))) & (op(e2, e0) = op(e0, op(e2, e0))) & (op(e1, e0) = op(e0, op(e1, e0))) & (op(e0, e0) = op(e0, op(e0, e0)))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((op(e3, e0) = op(e0, op(e3, e0))) & (op(e2, e0) = op(e0, op(e2, e0))) & (op(e1, e0) = op(e0, op(e1, e0))) & (op(e0, e0) = op(e0, op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((op(e3, e1) = op(e1, op(e3, e1))) & (op(e2, e1) = op(e1, op(e2, e1))) & (op(e1, e1) = op(e1, op(e1, e1))) & (op(e0, e1) = op(e1, op(e0, e1)))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((op(e3, e1) = op(e1, op(e3, e1))) & (op(e2, e1) = op(e1, op(e2, e1))) & (op(e1, e1) = op(e1, op(e1, e1))) & (op(e0, e1) = op(e1, op(e0, e1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((op(e3, e2) = op(e2, op(e3, e2))) & (op(e2, e2) = op(e2, op(e2, e2))) & (op(e1, e2) = op(e2, op(e1, e2))) & (op(e0, e2) = op(e2, op(e0, e2)))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((op(e3, e2) = op(e2, op(e3, e2))) & (op(e2, e2) = op(e2, op(e2, e2))) & (op(e1, e2) = op(e2, op(e1, e2))) & (op(e0, e2) = op(e2, op(e0, e2))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f5, plain, ((((op(e3, e3) = op(e3, op(e3, e3))) & (op(e2, e3) = op(e3, op(e2, e3))) & (op(e1, e3) = op(e3, op(e1, e3))) & (op(e0, e3) = op(e3, op(e0, e3)))) | ((op(e3, e2) = op(e2, op(e3, e2))) & (op(e2, e2) = op(e2, op(e2, e2))) & (op(e1, e2) = op(e2, op(e1, e2))) & (op(e0, e2) = op(e2, op(e0, e2)))) | ((op(e3, e1) = op(e1, op(e3, e1))) & (op(e2, e1) = op(e1, op(e2, e1))) & (op(e1, e1) = op(e1, op(e1, e1))) & (op(e0, e1) = op(e1, op(e0, e1)))) | ((op(e3, e0) = op(e0, op(e3, e0))) & (op(e2, e0) = op(e0, op(e2, e0))) & (op(e1, e0) = op(e0, op(e1, e0))) & (op(e0, e0) = op(e0, op(e0, e0))))) & (e3 = op(op(e3, e3), e3)) & (e2 = op(op(e3, e2), e3)) & (e1 = op(op(e3, e1), e3)) & (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e2), e2)) & (e1 = op(op(e2, e1), e2)) & (e0 = op(op(e2, e0), e2)) & (e3 = op(op(e1, e3), e1)) & (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e1), e1)) & (e0 = op(op(e1, e0), e1)) & (e3 = op(op(e0, e3), e0)) & (e2 = op(op(e0, e2), e0)) & (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax5)).
fof(f2120, plain, (~ spl3_28 | ~ spl3_32), inference(avatar_split_clause, [], [f2114, f351, f334])).
fof(f334, plain, (spl3_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_28])])).
fof(f2114, plain, (~ (e3 = op(e2, e1)) | ~ spl3_32), inference(backward_demodulation, [], [f145, f353])).
fof(f145, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax3)).
fof(f2100, plain, (spl3_11 | ~ spl3_40), inference(avatar_split_clause, [], [f2096, f385, f262])).
fof(f262, plain, (spl3_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_11])])).
fof(f385, plain, (spl3_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_40])])).
fof(f2096, plain, ((e2 = op(e3, e1)) | ~ spl3_40), inference(backward_demodulation, [], [f181, f387])).
fof(f387, plain, ((e3 = op(e1, e2)) | ~ spl3_40), inference(avatar_component_clause, [], [f385])).
fof(f181, plain, (e2 = op(op(e1, e2), e1)), inference(cnf_transformation, [], [f57])).
fof(f2087, plain, (spl3_25 | ~ spl3_47), inference(avatar_split_clause, [], [f2080, f415, f322])).
fof(f322, plain, (spl3_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_25])])).
fof(f415, plain, (spl3_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_47])])).
fof(f2080, plain, ((e0 = op(e2, e1)) | ~ spl3_47), inference(backward_demodulation, [], [f179, f417])).
fof(f417, plain, ((e2 = op(e1, e0)) | ~ spl3_47), inference(avatar_component_clause, [], [f415])).
fof(f179, plain, (e0 = op(op(e1, e0), e1)), inference(cnf_transformation, [], [f57])).
fof(f2085, plain, (~ spl3_15 | ~ spl3_47), inference(avatar_split_clause, [], [f2077, f415, f279])).
fof(f279, plain, (spl3_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_15])])).
fof(f2077, plain, (~ (e2 = op(e3, e0)) | ~ spl3_47), inference(backward_demodulation, [], [f113, f417])).
fof(f113, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2075, plain, (spl3_32 | ~ spl3_51), inference(avatar_split_clause, [], [f2069, f432, f351])).
fof(f2069, plain, ((e3 = op(e2, e0)) | ~ spl3_51), inference(backward_demodulation, [], [f178, f434])).
fof(f178, plain, (e3 = op(op(e0, e3), e0)), inference(cnf_transformation, [], [f57])).
fof(f2074, plain, (~ spl3_35 | ~ spl3_51), inference(avatar_split_clause, [], [f2067, f432, f364])).
fof(f364, plain, (spl3_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_35])])).
fof(f2067, plain, (~ (e2 = op(e1, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f127, f434])).
fof(f127, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2066, plain, (spl3_47 | ~ spl3_54), inference(avatar_split_clause, [], [f2059, f445, f415])).
fof(f445, plain, (spl3_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_54])])).
fof(f2059, plain, ((e2 = op(e1, e0)) | ~ spl3_54), inference(backward_demodulation, [], [f177, f447])).
fof(f447, plain, ((e1 = op(e0, e2)) | ~ spl3_54), inference(avatar_component_clause, [], [f445])).
fof(f177, plain, (e2 = op(op(e0, e2), e0)), inference(cnf_transformation, [], [f57])).
fof(f2054, plain, (~ spl3_56 | ~ spl3_60), inference(avatar_split_clause, [], [f2047, f470, f453])).
fof(f453, plain, (spl3_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_56])])).
fof(f470, plain, (spl3_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_60])])).
fof(f2047, plain, (~ (e3 = op(e0, e2)) | ~ spl3_60), inference(backward_demodulation, [], [f136, f472])).
fof(f472, plain, ((e3 = op(e0, e1)) | ~ spl3_60), inference(avatar_component_clause, [], [f470])).
fof(f136, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2053, plain, (~ spl3_28 | ~ spl3_60), inference(avatar_split_clause, [], [f2045, f470, f334])).
fof(f2045, plain, (~ (e3 = op(e2, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f116, f472])).
fof(f116, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2025, plain, (~ spl3_1 | ~ spl3_9), inference(avatar_split_clause, [], [f2017, f254, f220])).
fof(f220, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f254, plain, (spl3_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_9])])).
fof(f2017, plain, (~ (e0 = op(e3, e3)) | ~ spl3_9), inference(backward_demodulation, [], [f155, f256])).
fof(f256, plain, ((e0 = op(e3, e1)) | ~ spl3_9), inference(avatar_component_clause, [], [f254])).
fof(f155, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1971, plain, (spl3_4 | ~ spl3_48 | ~ spl3_102), inference(avatar_split_clause, [], [f1964, f706, f419, f232])).
fof(f232, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f419, plain, (spl3_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_48])])).
fof(f706, plain, (spl3_102 <=> (e3 = op(op(e1, e0), op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_102])])).
fof(f1964, plain, ((e3 = op(e3, e3)) | (~ spl3_48 | ~ spl3_102)), inference(backward_demodulation, [], [f707, f421])).
fof(f421, plain, ((e3 = op(e1, e0)) | ~ spl3_48), inference(avatar_component_clause, [], [f419])).
fof(f707, plain, ((e3 = op(op(e1, e0), op(e1, e0))) | ~ spl3_102), inference(avatar_component_clause, [], [f706])).
fof(f1970, plain, (~ spl3_48 | ~ spl3_50 | ~ spl3_78), inference(avatar_contradiction_clause, [], [f1969])).
fof(f1969, plain, ($false | (~ spl3_48 | ~ spl3_50 | ~ spl3_78)), inference(subsumption_resolution, [], [f1968, f161])).
fof(f161, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f1968, plain, ((e1 = e3) | (~ spl3_48 | ~ spl3_50 | ~ spl3_78)), inference(forward_demodulation, [], [f1962, f430])).
fof(f430, plain, ((e1 = op(e0, e3)) | ~ spl3_50), inference(avatar_component_clause, [], [f428])).
fof(f428, plain, (spl3_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_50])])).
fof(f1962, plain, ((e3 = op(e0, e3)) | (~ spl3_48 | ~ spl3_78)), inference(backward_demodulation, [], [f588, f421])).
fof(f588, plain, ((op(e1, e0) = op(e0, op(e1, e0))) | ~ spl3_78), inference(avatar_component_clause, [], [f586])).
fof(f586, plain, (spl3_78 <=> (op(e1, e0) = op(e0, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_78])])).
fof(f1967, plain, (spl3_9 | ~ spl3_48), inference(avatar_split_clause, [], [f1961, f419, f254])).
fof(f1961, plain, ((e0 = op(e3, e1)) | ~ spl3_48), inference(backward_demodulation, [], [f179, f421])).
fof(f1956, plain, (spl3_48 | ~ spl3_50), inference(avatar_split_clause, [], [f1950, f428, f419])).
fof(f1950, plain, ((e3 = op(e1, e0)) | ~ spl3_50), inference(backward_demodulation, [], [f178, f430])).
fof(f1947, plain, (~ spl3_4 | ~ spl3_56 | spl3_106), inference(avatar_split_clause, [], [f1943, f726, f453, f232])).
fof(f726, plain, (spl3_106 <=> (e3 = op(op(e0, e2), op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_106])])).
fof(f1943, plain, (~ (e3 = op(e3, e3)) | (~ spl3_56 | spl3_106)), inference(backward_demodulation, [], [f728, f455])).
fof(f455, plain, ((e3 = op(e0, e2)) | ~ spl3_56), inference(avatar_component_clause, [], [f453])).
fof(f728, plain, (~ (e3 = op(op(e0, e2), op(e0, e2))) | spl3_106), inference(avatar_component_clause, [], [f726])).
fof(f1934, plain, (spl3_30 | ~ spl3_59), inference(avatar_split_clause, [], [f1928, f466, f343])).
fof(f343, plain, (spl3_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_30])])).
fof(f466, plain, (spl3_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_59])])).
fof(f1928, plain, ((e1 = op(e2, e0)) | ~ spl3_59), inference(backward_demodulation, [], [f176, f468])).
fof(f468, plain, ((e2 = op(e0, e1)) | ~ spl3_59), inference(avatar_component_clause, [], [f466])).
fof(f176, plain, (e1 = op(op(e0, e1), e0)), inference(cnf_transformation, [], [f57])).
fof(f1904, plain, (spl3_4 | ~ spl3_28 | ~ spl3_105), inference(avatar_split_clause, [], [f1903, f721, f334, f232])).
fof(f721, plain, (spl3_105 <=> (e3 = op(op(e2, e1), op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_105])])).
fof(f1903, plain, ((e3 = op(e3, e3)) | (~ spl3_28 | ~ spl3_105)), inference(forward_demodulation, [], [f722, f336])).
fof(f336, plain, ((e3 = op(e2, e1)) | ~ spl3_28), inference(avatar_component_clause, [], [f334])).
fof(f722, plain, ((e3 = op(op(e2, e1), op(e2, e1))) | ~ spl3_105), inference(avatar_component_clause, [], [f721])).
fof(f1895, plain, (~ spl3_41 | ~ spl3_6 | spl3_87), inference(avatar_split_clause, [], [f1872, f631, f241, f390])).
fof(f390, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f241, plain, (spl3_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_6])])).
fof(f631, plain, (spl3_87 <=> (e0 = op(op(e3, e2), op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_87])])).
fof(f1872, plain, (~ (e0 = op(e1, e1)) | (~ spl3_6 | spl3_87)), inference(backward_demodulation, [], [f633, f243])).
fof(f243, plain, ((e1 = op(e3, e2)) | ~ spl3_6), inference(avatar_component_clause, [], [f241])).
fof(f633, plain, (~ (e0 = op(op(e3, e2), op(e3, e2))) | spl3_87), inference(avatar_component_clause, [], [f631])).
fof(f1862, plain, (~ spl3_7 | ~ spl3_11), inference(avatar_split_clause, [], [f1855, f262, f245])).
fof(f245, plain, (spl3_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f1855, plain, (~ (e2 = op(e3, e2)) | ~ spl3_11), inference(backward_demodulation, [], [f154, f264])).
fof(f264, plain, ((e2 = op(e3, e1)) | ~ spl3_11), inference(avatar_component_clause, [], [f262])).
fof(f154, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1853, plain, (~ spl3_4 | ~ spl3_16), inference(avatar_split_clause, [], [f1849, f283, f232])).
fof(f283, plain, (spl3_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_16])])).
fof(f1849, plain, (~ (e3 = op(e3, e3)) | ~ spl3_16), inference(backward_demodulation, [], [f153, f285])).
fof(f285, plain, ((e3 = op(e3, e0)) | ~ spl3_16), inference(avatar_component_clause, [], [f283])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1825, plain, (~ spl3_4 | ~ spl3_28 | spl3_105), inference(avatar_split_clause, [], [f1820, f721, f334, f232])).
fof(f1820, plain, (~ (e3 = op(e3, e3)) | (~ spl3_28 | spl3_105)), inference(backward_demodulation, [], [f723, f336])).
fof(f723, plain, (~ (e3 = op(op(e2, e1), op(e2, e1))) | spl3_105), inference(avatar_component_clause, [], [f721])).
fof(f1824, plain, (~ spl3_1 | ~ spl3_28 | spl3_84), inference(avatar_split_clause, [], [f1819, f616, f334, f220])).
fof(f616, plain, (spl3_84 <=> (e0 = op(op(e2, e1), op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_84])])).
fof(f1819, plain, (~ (e0 = op(e3, e3)) | (~ spl3_28 | spl3_84)), inference(backward_demodulation, [], [f618, f336])).
fof(f618, plain, (~ (e0 = op(op(e2, e1), op(e2, e1))) | spl3_84), inference(avatar_component_clause, [], [f616])).
fof(f1822, plain, (spl3_6 | ~ spl3_28), inference(avatar_split_clause, [], [f1817, f334, f241])).
fof(f1817, plain, ((e1 = op(e3, e2)) | ~ spl3_28), inference(backward_demodulation, [], [f184, f336])).
fof(f184, plain, (e1 = op(op(e2, e1), e2)), inference(cnf_transformation, [], [f57])).
fof(f1811, plain, (spl3_28 | ~ spl3_35), inference(avatar_split_clause, [], [f1805, f364, f334])).
fof(f1805, plain, ((e3 = op(e2, e1)) | ~ spl3_35), inference(backward_demodulation, [], [f182, f366])).
fof(f366, plain, ((e2 = op(e1, e3)) | ~ spl3_35), inference(avatar_component_clause, [], [f364])).
fof(f182, plain, (e3 = op(op(e1, e3), e1)), inference(cnf_transformation, [], [f57])).
fof(f1810, plain, (~ spl3_19 | ~ spl3_35), inference(avatar_split_clause, [], [f1803, f364, f296])).
fof(f296, plain, (spl3_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_19])])).
fof(f1803, plain, (~ (e2 = op(e2, e3)) | ~ spl3_35), inference(backward_demodulation, [], [f130, f366])).
fof(f130, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1802, plain, (~ spl3_4 | ~ spl3_40 | spl3_107), inference(avatar_split_clause, [], [f1796, f731, f385, f232])).
fof(f731, plain, (spl3_107 <=> (e3 = op(op(e1, e2), op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_107])])).
fof(f1796, plain, (~ (e3 = op(e3, e3)) | (~ spl3_40 | spl3_107)), inference(backward_demodulation, [], [f733, f387])).
fof(f733, plain, (~ (e3 = op(op(e1, e2), op(e1, e2))) | spl3_107), inference(avatar_component_clause, [], [f731])).
fof(f1800, plain, (spl3_20 | ~ spl3_40 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f1799])).
fof(f1799, plain, ($false | (spl3_20 | ~ spl3_40 | ~ spl3_68)), inference(subsumption_resolution, [], [f1794, f301])).
fof(f301, plain, (~ (e3 = op(e2, e3)) | spl3_20), inference(avatar_component_clause, [], [f300])).
fof(f300, plain, (spl3_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_20])])).
fof(f1794, plain, ((e3 = op(e2, e3)) | (~ spl3_40 | ~ spl3_68)), inference(backward_demodulation, [], [f540, f387])).
fof(f540, plain, ((op(e1, e2) = op(e2, op(e1, e2))) | ~ spl3_68), inference(avatar_component_clause, [], [f538])).
fof(f538, plain, (spl3_68 <=> (op(e1, e2) = op(e2, op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_68])])).
fof(f1777, plain, (spl3_41 | ~ spl3_46), inference(avatar_split_clause, [], [f1771, f411, f390])).
fof(f411, plain, (spl3_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_46])])).
fof(f1771, plain, ((e0 = op(e1, e1)) | ~ spl3_46), inference(backward_demodulation, [], [f179, f413])).
fof(f413, plain, ((e1 = op(e1, e0)) | ~ spl3_46), inference(avatar_component_clause, [], [f411])).
fof(f1776, plain, (~ spl3_38 | ~ spl3_46), inference(avatar_split_clause, [], [f1769, f411, f377])).
fof(f377, plain, (spl3_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_38])])).
fof(f1769, plain, (~ (e1 = op(e1, e2)) | ~ spl3_46), inference(backward_demodulation, [], [f140, f413])).
fof(f140, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1765, plain, (spl3_16 | ~ spl3_52), inference(avatar_split_clause, [], [f1761, f436, f283])).
fof(f436, plain, (spl3_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_52])])).
fof(f1761, plain, ((e3 = op(e3, e0)) | ~ spl3_52), inference(backward_demodulation, [], [f178, f438])).
fof(f438, plain, ((e3 = op(e0, e3)) | ~ spl3_52), inference(avatar_component_clause, [], [f436])).
fof(f1764, plain, (~ spl3_4 | ~ spl3_52), inference(avatar_split_clause, [], [f1760, f436, f232])).
fof(f1760, plain, (~ (e3 = op(e3, e3)) | ~ spl3_52), inference(backward_demodulation, [], [f129, f438])).
fof(f129, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1756, plain, (spl3_46 | ~ spl3_58), inference(avatar_split_clause, [], [f1748, f462, f411])).
fof(f462, plain, (spl3_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_58])])).
fof(f1748, plain, ((e1 = op(e1, e0)) | ~ spl3_58), inference(backward_demodulation, [], [f176, f464])).
fof(f464, plain, ((e1 = op(e0, e1)) | ~ spl3_58), inference(avatar_component_clause, [], [f462])).
fof(f1734, plain, (~ spl3_3 | ~ spl3_7), inference(avatar_split_clause, [], [f1728, f245, f228])).
fof(f228, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f1728, plain, (~ (e2 = op(e3, e3)) | ~ spl3_7), inference(backward_demodulation, [], [f156, f247])).
fof(f247, plain, ((e2 = op(e3, e2)) | ~ spl3_7), inference(avatar_component_clause, [], [f245])).
fof(f156, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1717, plain, (spl3_7 | ~ spl3_24), inference(avatar_split_clause, [], [f1713, f317, f245])).
fof(f317, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f1713, plain, ((e2 = op(e3, e2)) | ~ spl3_24), inference(backward_demodulation, [], [f185, f319])).
fof(f319, plain, ((e3 = op(e2, e2)) | ~ spl3_24), inference(avatar_component_clause, [], [f317])).
fof(f185, plain, (e2 = op(op(e2, e2), e2)), inference(cnf_transformation, [], [f57])).
fof(f1698, plain, (spl3_13 | ~ spl3_33 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f1697])).
fof(f1697, plain, ($false | (spl3_13 | ~ spl3_33 | ~ spl3_82)), inference(subsumption_resolution, [], [f1694, f272])).
fof(f272, plain, (~ (e0 = op(e3, e0)) | spl3_13), inference(avatar_component_clause, [], [f271])).
fof(f271, plain, (spl3_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_13])])).
fof(f1694, plain, ((e0 = op(e3, e0)) | (~ spl3_33 | ~ spl3_82)), inference(backward_demodulation, [], [f608, f358])).
fof(f358, plain, ((e0 = op(e1, e3)) | ~ spl3_33), inference(avatar_component_clause, [], [f356])).
fof(f356, plain, (spl3_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_33])])).
fof(f608, plain, ((op(e1, e3) = op(e3, op(e1, e3))) | ~ spl3_82), inference(avatar_component_clause, [], [f606])).
fof(f606, plain, (spl3_82 <=> (op(e1, e3) = op(e3, op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_82])])).
fof(f1681, plain, (~ spl3_4 | ~ spl3_48 | spl3_102), inference(avatar_split_clause, [], [f1677, f706, f419, f232])).
fof(f1677, plain, (~ (e3 = op(e3, e3)) | (~ spl3_48 | spl3_102)), inference(backward_demodulation, [], [f708, f421])).
fof(f708, plain, (~ (e3 = op(op(e1, e0), op(e1, e0))) | spl3_102), inference(avatar_component_clause, [], [f706])).
fof(f1680, plain, (~ spl3_3 | ~ spl3_48 | spl3_96), inference(avatar_split_clause, [], [f1676, f676, f419, f228])).
fof(f676, plain, (spl3_96 <=> (e2 = op(op(e1, e0), op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_96])])).
fof(f1676, plain, (~ (e2 = op(e3, e3)) | (~ spl3_48 | spl3_96)), inference(backward_demodulation, [], [f678, f421])).
fof(f678, plain, (~ (e2 = op(op(e1, e0), op(e1, e0))) | spl3_96), inference(avatar_component_clause, [], [f676])).
fof(f1678, plain, (~ spl3_32 | ~ spl3_48), inference(avatar_split_clause, [], [f1672, f419, f351])).
fof(f1672, plain, (~ (e3 = op(e2, e0)) | ~ spl3_48), inference(backward_demodulation, [], [f112, f421])).
fof(f112, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1669, plain, (~ spl3_9 | ~ spl3_50 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1668])).
fof(f1668, plain, ($false | (~ spl3_9 | ~ spl3_50 | ~ spl3_83)), inference(subsumption_resolution, [], [f1667, f157])).
fof(f157, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1667, plain, ((e0 = e1) | (~ spl3_9 | ~ spl3_50 | ~ spl3_83)), inference(forward_demodulation, [], [f1663, f256])).
fof(f1663, plain, ((e1 = op(e3, e1)) | (~ spl3_50 | ~ spl3_83)), inference(backward_demodulation, [], [f613, f430])).
fof(f613, plain, ((op(e0, e3) = op(e3, op(e0, e3))) | ~ spl3_83), inference(avatar_component_clause, [], [f611])).
fof(f611, plain, (spl3_83 <=> (op(e0, e3) = op(e3, op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_83])])).
fof(f1654, plain, (~ spl3_49 | ~ spl3_53), inference(avatar_split_clause, [], [f1645, f441, f424])).
fof(f424, plain, (spl3_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_49])])).
fof(f441, plain, (spl3_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_53])])).
fof(f1645, plain, (~ (e0 = op(e0, e3)) | ~ spl3_53), inference(backward_demodulation, [], [f138, f443])).
fof(f443, plain, ((e0 = op(e0, e2)) | ~ spl3_53), inference(avatar_component_clause, [], [f441])).
fof(f138, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1629, plain, (~ spl3_44 | ~ spl3_38 | spl3_107), inference(avatar_split_clause, [], [f1501, f731, f377, f402])).
fof(f402, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f1501, plain, (~ (e3 = op(e1, e1)) | (~ spl3_38 | spl3_107)), inference(backward_demodulation, [], [f733, f379])).
fof(f379, plain, ((e1 = op(e1, e2)) | ~ spl3_38), inference(avatar_component_clause, [], [f377])).
fof(f1626, plain, (~ spl3_43 | ~ spl3_14 | spl3_97), inference(avatar_split_clause, [], [f1573, f681, f275, f398])).
fof(f398, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f275, plain, (spl3_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_14])])).
fof(f681, plain, (spl3_97 <=> (e2 = op(op(e3, e0), op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_97])])).
fof(f1573, plain, (~ (e2 = op(e1, e1)) | (~ spl3_14 | spl3_97)), inference(backward_demodulation, [], [f683, f277])).
fof(f277, plain, ((e1 = op(e3, e0)) | ~ spl3_14), inference(avatar_component_clause, [], [f275])).
fof(f683, plain, (~ (e2 = op(op(e3, e0), op(e3, e0))) | spl3_97), inference(avatar_component_clause, [], [f681])).
fof(f1625, plain, (spl3_33 | ~ spl3_14), inference(avatar_split_clause, [], [f1569, f275, f356])).
fof(f1569, plain, ((e0 = op(e1, e3)) | ~ spl3_14), inference(backward_demodulation, [], [f187, f277])).
fof(f187, plain, (e0 = op(op(e3, e0), e3)), inference(cnf_transformation, [], [f57])).
fof(f1622, plain, (spl3_29 | ~ spl3_63), inference(avatar_split_clause, [], [f1430, f483, f339])).
fof(f339, plain, (spl3_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_29])])).
fof(f483, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f1430, plain, ((e0 = op(e2, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f175, f485])).
fof(f485, plain, ((op(e0, e0) = e2) | ~ spl3_63), inference(avatar_component_clause, [], [f483])).
fof(f175, plain, (e0 = op(op(e0, e0), e0)), inference(cnf_transformation, [], [f57])).
fof(f1619, plain, (spl3_50 | ~ spl3_9), inference(avatar_split_clause, [], [f1618, f254, f428])).
fof(f1618, plain, ((e1 = op(e0, e3)) | ~ spl3_9), inference(forward_demodulation, [], [f188, f256])).
fof(f188, plain, (e1 = op(op(e3, e1), e3)), inference(cnf_transformation, [], [f57])).
fof(f1616, plain, (spl3_45 | ~ spl3_9 | ~ spl3_71), inference(avatar_split_clause, [], [f1615, f552, f254, f407])).
fof(f407, plain, (spl3_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_45])])).
fof(f552, plain, (spl3_71 <=> (op(e3, e1) = op(e1, op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f1615, plain, ((e0 = op(e1, e0)) | (~ spl3_9 | ~ spl3_71)), inference(forward_demodulation, [], [f554, f256])).
fof(f554, plain, ((op(e3, e1) = op(e1, op(e3, e1))) | ~ spl3_71), inference(avatar_component_clause, [], [f552])).
fof(f1613, plain, (~ spl3_63 | ~ spl3_9 | spl3_99), inference(avatar_split_clause, [], [f1612, f691, f254, f483])).
fof(f691, plain, (spl3_99 <=> (e2 = op(op(e3, e1), op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_99])])).
fof(f1612, plain, (~ (op(e0, e0) = e2) | (~ spl3_9 | spl3_99)), inference(forward_demodulation, [], [f693, f256])).
fof(f693, plain, (~ (e2 = op(op(e3, e1), op(e3, e1))) | spl3_99), inference(avatar_component_clause, [], [f691])).
fof(f1611, plain, (~ spl3_4 | ~ spl3_60 | spl3_104), inference(avatar_split_clause, [], [f1446, f716, f470, f232])).
fof(f716, plain, (spl3_104 <=> (e3 = op(op(e0, e1), op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_104])])).
fof(f1446, plain, (~ (e3 = op(e3, e3)) | (~ spl3_60 | spl3_104)), inference(backward_demodulation, [], [f718, f472])).
fof(f718, plain, (~ (e3 = op(op(e0, e1), op(e0, e1))) | spl3_104), inference(avatar_component_clause, [], [f716])).
fof(f1610, plain, (~ spl3_3 | ~ spl3_18), inference(avatar_contradiction_clause, [], [f1609])).
fof(f1609, plain, ($false | (~ spl3_3 | ~ spl3_18)), inference(subsumption_resolution, [], [f1608, f161])).
fof(f1608, plain, ((e1 = e3) | (~ spl3_3 | ~ spl3_18)), inference(forward_demodulation, [], [f1606, f294])).
fof(f294, plain, ((e1 = op(e2, e3)) | ~ spl3_18), inference(avatar_component_clause, [], [f292])).
fof(f292, plain, (spl3_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_18])])).
fof(f1606, plain, ((e3 = op(e2, e3)) | ~ spl3_3), inference(backward_demodulation, [], [f190, f230])).
fof(f230, plain, ((e2 = op(e3, e3)) | ~ spl3_3), inference(avatar_component_clause, [], [f228])).
fof(f190, plain, (e3 = op(op(e3, e3), e3)), inference(cnf_transformation, [], [f57])).
fof(f1601, plain, (spl3_3 | ~ spl3_8), inference(avatar_split_clause, [], [f1597, f249, f228])).
fof(f249, plain, (spl3_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_8])])).
fof(f1597, plain, ((e2 = op(e3, e3)) | ~ spl3_8), inference(backward_demodulation, [], [f189, f251])).
fof(f251, plain, ((e3 = op(e3, e2)) | ~ spl3_8), inference(avatar_component_clause, [], [f249])).
fof(f189, plain, (e2 = op(op(e3, e2), e3)), inference(cnf_transformation, [], [f57])).
fof(f1585, plain, (~ spl3_3 | ~ spl3_11), inference(avatar_split_clause, [], [f1579, f262, f228])).
fof(f1579, plain, (~ (e2 = op(e3, e3)) | ~ spl3_11), inference(backward_demodulation, [], [f155, f264])).
fof(f1560, plain, (spl3_8 | ~ spl3_20), inference(avatar_split_clause, [], [f1557, f300, f249])).
fof(f1557, plain, ((e3 = op(e3, e2)) | ~ spl3_20), inference(backward_demodulation, [], [f186, f302])).
fof(f302, plain, ((e3 = op(e2, e3)) | ~ spl3_20), inference(avatar_component_clause, [], [f300])).
fof(f186, plain, (e3 = op(op(e2, e3), e2)), inference(cnf_transformation, [], [f57])).
fof(f1531, plain, (~ spl3_2 | ~ spl3_32 | spl3_90), inference(avatar_split_clause, [], [f1525, f646, f351, f224])).
fof(f224, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f646, plain, (spl3_90 <=> (e1 = op(op(e2, e0), op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_90])])).
fof(f1525, plain, (~ (e1 = op(e3, e3)) | (~ spl3_32 | spl3_90)), inference(backward_demodulation, [], [f648, f353])).
fof(f648, plain, (~ (e1 = op(op(e2, e0), op(e2, e0))) | spl3_90), inference(avatar_component_clause, [], [f646])).
fof(f1528, plain, (~ spl3_20 | ~ spl3_32), inference(avatar_split_clause, [], [f1523, f351, f300])).
fof(f1523, plain, (~ (e3 = op(e2, e3)) | ~ spl3_32), inference(backward_demodulation, [], [f147, f353])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1518, plain, (~ spl3_21 | ~ spl3_35 | spl3_88), inference(avatar_split_clause, [], [f1512, f636, f364, f305])).
fof(f305, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f636, plain, (spl3_88 <=> (e0 = op(op(e1, e3), op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_88])])).
fof(f1512, plain, (~ (e0 = op(e2, e2)) | (~ spl3_35 | spl3_88)), inference(backward_demodulation, [], [f638, f366])).
fof(f638, plain, (~ (e0 = op(op(e1, e3), op(e1, e3))) | spl3_88), inference(avatar_component_clause, [], [f636])).
fof(f1515, plain, (~ spl3_3 | ~ spl3_35), inference(avatar_split_clause, [], [f1509, f364, f228])).
fof(f1509, plain, (~ (e2 = op(e3, e3)) | ~ spl3_35), inference(backward_demodulation, [], [f131, f366])).
fof(f131, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1491, plain, (spl3_10 | ~ spl3_44), inference(avatar_contradiction_clause, [], [f1490])).
fof(f1490, plain, ($false | (spl3_10 | ~ spl3_44)), inference(subsumption_resolution, [], [f1486, f259])).
fof(f259, plain, (~ (e1 = op(e3, e1)) | spl3_10), inference(avatar_component_clause, [], [f258])).
fof(f258, plain, (spl3_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_10])])).
fof(f1486, plain, ((e1 = op(e3, e1)) | ~ spl3_44), inference(backward_demodulation, [], [f180, f404])).
fof(f404, plain, ((e3 = op(e1, e1)) | ~ spl3_44), inference(avatar_component_clause, [], [f402])).
fof(f180, plain, (e1 = op(op(e1, e1), e1)), inference(cnf_transformation, [], [f57])).
fof(f1479, plain, (~ spl3_45 | spl3_57), inference(avatar_contradiction_clause, [], [f1478])).
fof(f1478, plain, ($false | (~ spl3_45 | spl3_57)), inference(subsumption_resolution, [], [f1470, f459])).
fof(f459, plain, (~ (e0 = op(e0, e1)) | spl3_57), inference(avatar_component_clause, [], [f458])).
fof(f458, plain, (spl3_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_57])])).
fof(f1470, plain, ((e0 = op(e0, e1)) | ~ spl3_45), inference(backward_demodulation, [], [f179, f409])).
fof(f409, plain, ((e0 = op(e1, e0)) | ~ spl3_45), inference(avatar_component_clause, [], [f407])).
fof(f1463, plain, (spl3_20 | ~ spl3_56 | ~ spl3_69), inference(avatar_split_clause, [], [f1457, f543, f453, f300])).
fof(f543, plain, (spl3_69 <=> (op(e0, e2) = op(e2, op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f1457, plain, ((e3 = op(e2, e3)) | (~ spl3_56 | ~ spl3_69)), inference(backward_demodulation, [], [f545, f455])).
fof(f545, plain, ((op(e0, e2) = op(e2, op(e0, e2))) | ~ spl3_69), inference(avatar_component_clause, [], [f543])).
fof(f1460, plain, (~ spl3_24 | ~ spl3_56), inference(avatar_split_clause, [], [f1454, f453, f317])).
fof(f1454, plain, (~ (e3 = op(e2, e2)) | ~ spl3_56), inference(backward_demodulation, [], [f122, f455])).
fof(f122, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1451, plain, (spl3_36 | ~ spl3_60 | ~ spl3_74), inference(avatar_split_clause, [], [f1444, f567, f470, f368])).
fof(f368, plain, (spl3_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_36])])).
fof(f567, plain, (spl3_74 <=> (op(e0, e1) = op(e1, op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_74])])).
fof(f1444, plain, ((e3 = op(e1, e3)) | (~ spl3_60 | ~ spl3_74)), inference(backward_demodulation, [], [f569, f472])).
fof(f569, plain, ((op(e0, e1) = op(e1, op(e0, e1))) | ~ spl3_74), inference(avatar_component_clause, [], [f567])).
fof(f1438, plain, (~ spl3_62 | ~ spl3_63), inference(avatar_contradiction_clause, [], [f1437])).
fof(f1437, plain, ($false | (~ spl3_62 | ~ spl3_63)), inference(subsumption_resolution, [], [f1436, f160])).
fof(f160, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1436, plain, ((e1 = e2) | (~ spl3_62 | ~ spl3_63)), inference(backward_demodulation, [], [f485, f481])).
fof(f481, plain, ((op(e0, e0) = e1) | ~ spl3_62), inference(avatar_component_clause, [], [f479])).
fof(f479, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f1419, plain, (spl3_64 | ~ spl3_49), inference(avatar_split_clause, [], [f1215, f424, f487])).
fof(f487, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f1215, plain, ((op(e0, e0) = e3) | ~ spl3_49), inference(backward_demodulation, [], [f178, f426])).
fof(f426, plain, ((e0 = op(e0, e3)) | ~ spl3_49), inference(avatar_component_clause, [], [f424])).
fof(f1412, plain, (~ spl3_6 | spl3_35), inference(avatar_contradiction_clause, [], [f1411])).
fof(f1411, plain, ($false | (~ spl3_6 | spl3_35)), inference(subsumption_resolution, [], [f1405, f365])).
fof(f365, plain, (~ (e2 = op(e1, e3)) | spl3_35), inference(avatar_component_clause, [], [f364])).
fof(f1405, plain, ((e2 = op(e1, e3)) | ~ spl3_6), inference(backward_demodulation, [], [f189, f243])).
fof(f1397, plain, (~ spl3_25 | spl3_54), inference(avatar_contradiction_clause, [], [f1396])).
fof(f1396, plain, ($false | (~ spl3_25 | spl3_54)), inference(subsumption_resolution, [], [f1389, f446])).
fof(f446, plain, (~ (e1 = op(e0, e2)) | spl3_54), inference(avatar_component_clause, [], [f445])).
fof(f1389, plain, ((e1 = op(e0, e2)) | ~ spl3_25), inference(backward_demodulation, [], [f184, f324])).
fof(f324, plain, ((e0 = op(e2, e1)) | ~ spl3_25), inference(avatar_component_clause, [], [f322])).
fof(f1387, plain, (~ spl3_37 | ~ spl3_64 | spl3_107), inference(avatar_contradiction_clause, [], [f1386])).
fof(f1386, plain, ($false | (~ spl3_37 | ~ spl3_64 | spl3_107)), inference(subsumption_resolution, [], [f1382, f489])).
fof(f489, plain, ((op(e0, e0) = e3) | ~ spl3_64), inference(avatar_component_clause, [], [f487])).
fof(f1382, plain, (~ (op(e0, e0) = e3) | (~ spl3_37 | spl3_107)), inference(backward_demodulation, [], [f733, f375])).
fof(f375, plain, ((e0 = op(e1, e2)) | ~ spl3_37), inference(avatar_component_clause, [], [f373])).
fof(f373, plain, (spl3_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_37])])).
fof(f1346, plain, (~ spl3_7 | ~ spl3_55), inference(avatar_split_clause, [], [f1340, f449, f245])).
fof(f449, plain, (spl3_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_55])])).
fof(f1340, plain, (~ (e2 = op(e3, e2)) | ~ spl3_55), inference(backward_demodulation, [], [f123, f451])).
fof(f451, plain, ((e2 = op(e0, e2)) | ~ spl3_55), inference(avatar_component_clause, [], [f449])).
fof(f123, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1316, plain, (spl3_37 | ~ spl3_30), inference(avatar_split_clause, [], [f1262, f343, f373])).
fof(f1262, plain, ((e0 = op(e1, e2)) | ~ spl3_30), inference(backward_demodulation, [], [f183, f345])).
fof(f345, plain, ((e1 = op(e2, e0)) | ~ spl3_30), inference(avatar_component_clause, [], [f343])).
fof(f1296, plain, (~ spl3_6 | ~ spl3_2), inference(avatar_split_clause, [], [f982, f224, f241])).
fof(f982, plain, (~ (e1 = op(e3, e2)) | ~ spl3_2), inference(forward_demodulation, [], [f156, f226])).
fof(f226, plain, ((e1 = op(e3, e3)) | ~ spl3_2), inference(avatar_component_clause, [], [f224])).
fof(f1282, plain, (~ spl3_21 | ~ spl3_22), inference(avatar_contradiction_clause, [], [f1281])).
fof(f1281, plain, ($false | (~ spl3_21 | ~ spl3_22)), inference(subsumption_resolution, [], [f1280, f157])).
fof(f1280, plain, ((e0 = e1) | (~ spl3_21 | ~ spl3_22)), inference(backward_demodulation, [], [f311, f307])).
fof(f307, plain, ((e0 = op(e2, e2)) | ~ spl3_21), inference(avatar_component_clause, [], [f305])).
fof(f311, plain, ((e1 = op(e2, e2)) | ~ spl3_22), inference(avatar_component_clause, [], [f309])).
fof(f309, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f1266, plain, (~ spl3_26 | ~ spl3_30), inference(avatar_split_clause, [], [f1261, f343, f326])).
fof(f326, plain, (spl3_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_26])])).
fof(f1261, plain, (~ (e1 = op(e2, e1)) | ~ spl3_30), inference(backward_demodulation, [], [f145, f345])).
fof(f1233, plain, (~ spl3_43 | ~ spl3_47), inference(avatar_split_clause, [], [f1225, f415, f398])).
fof(f1225, plain, (~ (e2 = op(e1, e1)) | ~ spl3_47), inference(backward_demodulation, [], [f139, f417])).
fof(f139, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1214, plain, (~ spl3_44 | ~ spl3_54 | spl3_106), inference(avatar_split_clause, [], [f1209, f726, f445, f402])).
fof(f1209, plain, (~ (e3 = op(e1, e1)) | (~ spl3_54 | spl3_106)), inference(backward_demodulation, [], [f728, f447])).
fof(f1210, plain, (~ spl3_38 | ~ spl3_54), inference(avatar_split_clause, [], [f1203, f445, f377])).
fof(f1203, plain, (~ (e1 = op(e1, e2)) | ~ spl3_54), inference(backward_demodulation, [], [f121, f447])).
fof(f121, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1188, plain, (~ spl3_56 | ~ spl3_64), inference(avatar_split_clause, [], [f1183, f487, f453])).
fof(f1183, plain, (~ (e3 = op(e0, e2)) | ~ spl3_64), inference(backward_demodulation, [], [f134, f489])).
fof(f134, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f1160, plain, (~ spl3_19 | ~ spl3_22), inference(avatar_contradiction_clause, [], [f1159])).
fof(f1159, plain, ($false | (~ spl3_19 | ~ spl3_22)), inference(subsumption_resolution, [], [f1158, f161])).
fof(f1158, plain, ((e1 = e3) | (~ spl3_19 | ~ spl3_22)), inference(forward_demodulation, [], [f1151, f311])).
fof(f1151, plain, ((e3 = op(e2, e2)) | ~ spl3_19), inference(backward_demodulation, [], [f186, f298])).
fof(f298, plain, ((e2 = op(e2, e3)) | ~ spl3_19), inference(avatar_component_clause, [], [f296])).
fof(f1157, plain, (~ spl3_27 | ~ spl3_19), inference(avatar_split_clause, [], [f1150, f296, f330])).
fof(f330, plain, (spl3_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_27])])).
fof(f1150, plain, (~ (e2 = op(e2, e1)) | ~ spl3_19), inference(backward_demodulation, [], [f149, f298])).
fof(f149, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1156, plain, (~ spl3_31 | ~ spl3_19), inference(avatar_split_clause, [], [f1149, f296, f347])).
fof(f347, plain, (spl3_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_31])])).
fof(f1149, plain, (~ (e2 = op(e2, e0)) | ~ spl3_19), inference(backward_demodulation, [], [f147, f298])).
fof(f1145, plain, (~ spl3_28 | ~ spl3_12), inference(avatar_split_clause, [], [f1142, f266, f334])).
fof(f266, plain, (spl3_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_12])])).
fof(f1142, plain, (~ (e3 = op(e2, e1)) | ~ spl3_12), inference(backward_demodulation, [], [f120, f268])).
fof(f268, plain, ((e3 = op(e3, e1)) | ~ spl3_12), inference(avatar_component_clause, [], [f266])).
fof(f120, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1144, plain, (~ spl3_16 | ~ spl3_12), inference(avatar_split_clause, [], [f1141, f266, f283])).
fof(f1141, plain, (~ (e3 = op(e3, e0)) | ~ spl3_12), inference(backward_demodulation, [], [f151, f268])).
fof(f151, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1134, plain, (spl3_19 | ~ spl3_7), inference(avatar_split_clause, [], [f1126, f245, f296])).
fof(f1126, plain, ((e2 = op(e2, e3)) | ~ spl3_7), inference(backward_demodulation, [], [f189, f247])).
fof(f1133, plain, (~ spl3_15 | ~ spl3_7), inference(avatar_split_clause, [], [f1125, f245, f279])).
fof(f1125, plain, (~ (e2 = op(e3, e0)) | ~ spl3_7), inference(backward_demodulation, [], [f152, f247])).
fof(f152, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1132, plain, (~ spl3_39 | ~ spl3_7), inference(avatar_split_clause, [], [f1124, f245, f381])).
fof(f381, plain, (spl3_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_39])])).
fof(f1124, plain, (~ (e2 = op(e1, e2)) | ~ spl3_7), inference(backward_demodulation, [], [f125, f247])).
fof(f125, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1108, plain, (spl3_22 | ~ spl3_27), inference(avatar_split_clause, [], [f1104, f330, f309])).
fof(f1104, plain, ((e1 = op(e2, e2)) | ~ spl3_27), inference(backward_demodulation, [], [f184, f332])).
fof(f332, plain, ((e2 = op(e2, e1)) | ~ spl3_27), inference(avatar_component_clause, [], [f330])).
fof(f1053, plain, (~ spl3_58 | ~ spl3_10), inference(avatar_split_clause, [], [f1052, f258, f462])).
fof(f1052, plain, (~ (e1 = op(e0, e1)) | ~ spl3_10), inference(forward_demodulation, [], [f117, f260])).
fof(f260, plain, ((e1 = op(e3, e1)) | ~ spl3_10), inference(avatar_component_clause, [], [f258])).
fof(f117, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1049, plain, (spl3_58 | ~ spl3_41), inference(avatar_split_clause, [], [f819, f390, f462])).
fof(f819, plain, ((e1 = op(e0, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f180, f392])).
fof(f392, plain, ((e0 = op(e1, e1)) | ~ spl3_41), inference(avatar_component_clause, [], [f390])).
fof(f1048, plain, (~ spl3_53 | ~ spl3_5), inference(avatar_split_clause, [], [f1047, f237, f441])).
fof(f1047, plain, (~ (e0 = op(e0, e2)) | ~ spl3_5), inference(forward_demodulation, [], [f123, f239])).
fof(f239, plain, ((e0 = op(e3, e2)) | ~ spl3_5), inference(avatar_component_clause, [], [f237])).
fof(f1046, plain, (~ spl3_55 | ~ spl3_51), inference(avatar_split_clause, [], [f1045, f432, f449])).
fof(f1045, plain, (~ (e2 = op(e0, e2)) | ~ spl3_51), inference(forward_demodulation, [], [f138, f434])).
fof(f1036, plain, (~ spl3_22 | ~ spl3_51 | spl3_94), inference(avatar_split_clause, [], [f1035, f666, f432, f309])).
fof(f666, plain, (spl3_94 <=> (e1 = op(op(e0, e3), op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_94])])).
fof(f1035, plain, (~ (e1 = op(e2, e2)) | (~ spl3_51 | spl3_94)), inference(forward_demodulation, [], [f668, f434])).
fof(f668, plain, (~ (e1 = op(op(e0, e3), op(e0, e3))) | spl3_94), inference(avatar_component_clause, [], [f666])).
fof(f1028, plain, (~ spl3_48 | ~ spl3_36), inference(avatar_split_clause, [], [f1027, f368, f419])).
fof(f1027, plain, (~ (e3 = op(e1, e0)) | ~ spl3_36), inference(forward_demodulation, [], [f141, f370])).
fof(f370, plain, ((e3 = op(e1, e3)) | ~ spl3_36), inference(avatar_component_clause, [], [f368])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1024, plain, (~ spl3_40 | ~ spl3_36), inference(avatar_split_clause, [], [f1023, f368, f385])).
fof(f1023, plain, (~ (e3 = op(e1, e2)) | ~ spl3_36), inference(forward_demodulation, [], [f144, f370])).
fof(f144, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1022, plain, (spl3_36 | ~ spl3_2), inference(avatar_split_clause, [], [f966, f224, f368])).
fof(f966, plain, ((e3 = op(e1, e3)) | ~ spl3_2), inference(backward_demodulation, [], [f190, f226])).
fof(f1017, plain, (~ spl3_26 | ~ spl3_10), inference(avatar_split_clause, [], [f1016, f258, f326])).
fof(f1016, plain, (~ (e1 = op(e2, e1)) | ~ spl3_10), inference(forward_demodulation, [], [f120, f260])).
fof(f995, plain, (~ spl3_13 | ~ spl3_5), inference(avatar_split_clause, [], [f994, f237, f271])).
fof(f994, plain, (~ (e0 = op(e3, e0)) | ~ spl3_5), inference(forward_demodulation, [], [f152, f239])).
fof(f981, plain, (spl3_51 | ~ spl3_5), inference(avatar_split_clause, [], [f980, f237, f432])).
fof(f980, plain, ((e2 = op(e0, e3)) | ~ spl3_5), inference(forward_demodulation, [], [f189, f239])).
fof(f962, plain, (~ spl3_6 | ~ spl3_7), inference(avatar_contradiction_clause, [], [f961])).
fof(f961, plain, ($false | (~ spl3_6 | ~ spl3_7)), inference(subsumption_resolution, [], [f960, f160])).
fof(f960, plain, ((e1 = e2) | (~ spl3_6 | ~ spl3_7)), inference(backward_demodulation, [], [f247, f243])).
fof(f937, plain, (spl3_2 | ~ spl3_12), inference(avatar_split_clause, [], [f933, f266, f224])).
fof(f933, plain, ((e1 = op(e3, e3)) | ~ spl3_12), inference(backward_demodulation, [], [f188, f268])).
fof(f935, plain, (~ spl3_8 | ~ spl3_12), inference(avatar_split_clause, [], [f931, f266, f249])).
fof(f931, plain, (~ (e3 = op(e3, e2)) | ~ spl3_12), inference(backward_demodulation, [], [f154, f268])).
fof(f920, plain, (~ spl3_8 | ~ spl3_16), inference(avatar_split_clause, [], [f914, f283, f249])).
fof(f914, plain, (~ (e3 = op(e3, e2)) | ~ spl3_16), inference(backward_demodulation, [], [f152, f285])).
fof(f868, plain, (~ spl3_25 | ~ spl3_29), inference(avatar_split_clause, [], [f861, f339, f322])).
fof(f861, plain, (~ (e0 = op(e2, e1)) | ~ spl3_29), inference(backward_demodulation, [], [f145, f341])).
fof(f341, plain, ((e0 = op(e2, e0)) | ~ spl3_29), inference(avatar_component_clause, [], [f339])).
fof(f856, plain, (~ spl3_35 | ~ spl3_36), inference(avatar_contradiction_clause, [], [f855])).
fof(f855, plain, ($false | (~ spl3_35 | ~ spl3_36)), inference(subsumption_resolution, [], [f854, f162])).
fof(f854, plain, ((e2 = e3) | (~ spl3_35 | ~ spl3_36)), inference(backward_demodulation, [], [f370, f366])).
fof(f851, plain, (spl3_12 | ~ spl3_36), inference(avatar_split_clause, [], [f846, f368, f266])).
fof(f846, plain, ((e3 = op(e3, e1)) | ~ spl3_36), inference(backward_demodulation, [], [f182, f370])).
fof(f849, plain, (~ spl3_20 | ~ spl3_36), inference(avatar_split_clause, [], [f844, f368, f300])).
fof(f844, plain, (~ (e3 = op(e2, e3)) | ~ spl3_36), inference(backward_demodulation, [], [f130, f370])).
fof(f796, plain, (~ spl3_51 | ~ spl3_52), inference(avatar_contradiction_clause, [], [f795])).
fof(f795, plain, ($false | (~ spl3_51 | ~ spl3_52)), inference(subsumption_resolution, [], [f794, f162])).
fof(f794, plain, ((e2 = e3) | (~ spl3_51 | ~ spl3_52)), inference(backward_demodulation, [], [f438, f434])).
fof(f789, plain, (~ spl3_20 | ~ spl3_52), inference(avatar_split_clause, [], [f783, f436, f300])).
fof(f783, plain, (~ (e3 = op(e2, e3)) | ~ spl3_52), inference(backward_demodulation, [], [f128, f438])).
fof(f128, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f788, plain, (~ spl3_36 | ~ spl3_52), inference(avatar_split_clause, [], [f782, f436, f368])).
fof(f782, plain, (~ (e3 = op(e1, e3)) | ~ spl3_52), inference(backward_demodulation, [], [f127, f438])).
fof(f760, plain, (~ spl3_49 | ~ spl3_57), inference(avatar_split_clause, [], [f752, f458, f424])).
fof(f752, plain, (~ (e0 = op(e0, e3)) | ~ spl3_57), inference(backward_demodulation, [], [f137, f460])).
fof(f460, plain, ((e0 = op(e0, e1)) | ~ spl3_57), inference(avatar_component_clause, [], [f458])).
fof(f137, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f759, plain, (~ spl3_53 | ~ spl3_57), inference(avatar_split_clause, [], [f751, f458, f441])).
fof(f751, plain, (~ (e0 = op(e0, e2)) | ~ spl3_57), inference(backward_demodulation, [], [f136, f460])).
fof(f758, plain, (~ spl3_9 | ~ spl3_57), inference(avatar_split_clause, [], [f750, f458, f254])).
fof(f750, plain, (~ (e0 = op(e3, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f117, f460])).
fof(f734, plain, (~ spl3_107 | ~ spl3_37), inference(avatar_split_clause, [], [f218, f373, f731])).
fof(f218, plain, (~ (e0 = op(e1, e2)) | ~ (e3 = op(op(e1, e2), op(e1, e2)))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(e1, e2)) | ~ (e3 = op(op(e1, e2), op(e1, e2)))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(e1, e2)) & (e3 = op(op(e1, e2), op(e1, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax29)).
fof(f729, plain, (~ spl3_106 | ~ spl3_54), inference(avatar_split_clause, [], [f217, f445, f726])).
fof(f217, plain, (~ (e1 = op(e0, e2)) | ~ (e3 = op(op(e0, e2), op(e0, e2)))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e1 = op(e0, e2)) | ~ (e3 = op(op(e0, e2), op(e0, e2)))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e1 = op(e0, e2)) & (e3 = op(op(e0, e2), op(e0, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax28)).
fof(f719, plain, (~ spl3_104 | ~ spl3_59), inference(avatar_split_clause, [], [f215, f466, f716])).
fof(f215, plain, (~ (e2 = op(e0, e1)) | ~ (e3 = op(op(e0, e1), op(e0, e1)))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e2 = op(e0, e1)) | ~ (e3 = op(op(e0, e1), op(e0, e1)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e2 = op(e0, e1)) & (e3 = op(op(e0, e1), op(e0, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax26)).
fof(f694, plain, (~ spl3_99 | ~ spl3_9), inference(avatar_split_clause, [], [f210, f254, f691])).
fof(f210, plain, (~ (e0 = op(e3, e1)) | ~ (e2 = op(op(e3, e1), op(e3, e1)))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e0 = op(e3, e1)) | ~ (e2 = op(op(e3, e1), op(e3, e1)))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e0 = op(e3, e1)) & (e2 = op(op(e3, e1), op(e3, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax21)).
fof(f684, plain, (~ spl3_97 | ~ spl3_14), inference(avatar_split_clause, [], [f208, f275, f681])).
fof(f208, plain, (~ (e1 = op(e3, e0)) | ~ (e2 = op(op(e3, e0), op(e3, e0)))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e3, e0)) | ~ (e2 = op(op(e3, e0), op(e3, e0)))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e3, e0)) & (e2 = op(op(e3, e0), op(e3, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax19)).
fof(f679, plain, (~ spl3_96 | ~ spl3_48), inference(avatar_split_clause, [], [f207, f419, f676])).
fof(f207, plain, (~ (e3 = op(e1, e0)) | ~ (e2 = op(op(e1, e0), op(e1, e0)))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e1, e0)) | ~ (e2 = op(op(e1, e0), op(e1, e0)))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e1, e0)) & (e2 = op(op(e1, e0), op(e1, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax18)).
fof(f669, plain, (~ spl3_94 | ~ spl3_51), inference(avatar_split_clause, [], [f205, f432, f666])).
fof(f205, plain, (~ (e2 = op(e0, e3)) | ~ (e1 = op(op(e0, e3), op(e0, e3)))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e2 = op(e0, e3)) | ~ (e1 = op(op(e0, e3), op(e0, e3)))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e2 = op(e0, e3)) & (e1 = op(op(e0, e3), op(e0, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax16)).
fof(f649, plain, (~ spl3_90 | ~ spl3_32), inference(avatar_split_clause, [], [f201, f351, f646])).
fof(f201, plain, (~ (e3 = op(e2, e0)) | ~ (e1 = op(op(e2, e0), op(e2, e0)))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e3 = op(e2, e0)) | ~ (e1 = op(op(e2, e0), op(e2, e0)))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e3 = op(e2, e0)) & (e1 = op(op(e2, e0), op(e2, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax12)).
fof(f639, plain, (~ spl3_88 | ~ spl3_35), inference(avatar_split_clause, [], [f199, f364, f636])).
fof(f199, plain, (~ (e2 = op(e1, e3)) | ~ (e0 = op(op(e1, e3), op(e1, e3)))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e2 = op(e1, e3)) | ~ (e0 = op(op(e1, e3), op(e1, e3)))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e2 = op(e1, e3)) & (e0 = op(op(e1, e3), op(e1, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax10)).
fof(f634, plain, (~ spl3_87 | ~ spl3_6), inference(avatar_split_clause, [], [f198, f241, f631])).
fof(f198, plain, (~ (e1 = op(e3, e2)) | ~ (e0 = op(op(e3, e2), op(e3, e2)))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(e3, e2)) | ~ (e0 = op(op(e3, e2), op(e3, e2)))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(e3, e2)) & (e0 = op(op(e3, e2), op(e3, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax9)).
fof(f619, plain, (~ spl3_84 | ~ spl3_28), inference(avatar_split_clause, [], [f195, f334, f616])).
fof(f195, plain, (~ (e3 = op(e2, e1)) | ~ (e0 = op(op(e2, e1), op(e2, e1)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e2, e1)) | ~ (e0 = op(op(e2, e1), op(e2, e1)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e2, e1)) & (e0 = op(op(e2, e1), op(e2, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax6)).
fof(f614, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_83), inference(avatar_split_clause, [], [f191, f611, f524, f548, f572])).
fof(f572, plain, (spl3_75 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_75])])).
fof(f548, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f524, plain, (spl3_65 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f191, plain, ((op(e0, e3) = op(e3, op(e0, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f609, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_82), inference(avatar_split_clause, [], [f192, f606, f524, f548, f572])).
fof(f192, plain, ((op(e1, e3) = op(e3, op(e1, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f589, plain, (~ spl3_75 | spl3_78), inference(avatar_split_clause, [], [f172, f586, f572])).
fof(f172, plain, ((op(e1, e0) = op(e0, op(e1, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (((op(e3, e0) = op(e0, op(e3, e0))) & (op(e2, e0) = op(e0, op(e2, e0))) & (op(e1, e0) = op(e0, op(e1, e0))) & (op(e0, e0) = op(e0, op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f584, plain, (~ spl3_75 | spl3_77), inference(avatar_split_clause, [], [f173, f581, f572])).
fof(f173, plain, ((op(e2, e0) = op(e0, op(e2, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f570, plain, (~ spl3_70 | spl3_74), inference(avatar_split_clause, [], [f167, f567, f548])).
fof(f167, plain, ((op(e0, e1) = op(e1, op(e0, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f59, plain, (((op(e3, e1) = op(e1, op(e3, e1))) & (op(e2, e1) = op(e1, op(e2, e1))) & (op(e1, e1) = op(e1, op(e1, e1))) & (op(e0, e1) = op(e1, op(e0, e1)))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f555, plain, (~ spl3_70 | spl3_71), inference(avatar_split_clause, [], [f170, f552, f548])).
fof(f170, plain, ((op(e3, e1) = op(e1, op(e3, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f546, plain, (~ spl3_65 | spl3_69), inference(avatar_split_clause, [], [f163, f543, f524])).
fof(f163, plain, ((op(e0, e2) = op(e2, op(e0, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((op(e3, e2) = op(e2, op(e3, e2))) & (op(e2, e2) = op(e2, op(e2, e2))) & (op(e1, e2) = op(e2, op(e1, e2))) & (op(e0, e2) = op(e2, op(e0, e2)))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f541, plain, (~ spl3_65 | spl3_68), inference(avatar_split_clause, [], [f164, f538, f524])).
fof(f164, plain, ((op(e1, e2) = op(e2, op(e1, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f519, plain, (spl3_62 | spl3_46 | spl3_30 | spl3_14), inference(avatar_split_clause, [], [f80, f275, f343, f411, f479])).
fof(f80, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax2)).
fof(f518, plain, (spl3_63 | spl3_59 | spl3_55 | spl3_51), inference(avatar_split_clause, [], [f81, f432, f449, f466, f483])).
fof(f81, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f517, plain, (spl3_63 | spl3_47 | spl3_31 | spl3_15), inference(avatar_split_clause, [], [f82, f279, f347, f415, f483])).
fof(f82, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f515, plain, (spl3_64 | spl3_48 | spl3_32 | spl3_16), inference(avatar_split_clause, [], [f84, f283, f351, f419, f487])).
fof(f84, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f513, plain, (spl3_57 | spl3_41 | spl3_25 | spl3_9), inference(avatar_split_clause, [], [f86, f254, f322, f390, f458])).
fof(f86, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f510, plain, (spl3_47 | spl3_43 | spl3_39 | spl3_35), inference(avatar_split_clause, [], [f89, f364, f381, f398, f415])).
fof(f89, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f509, plain, (spl3_59 | spl3_43 | spl3_27 | spl3_11), inference(avatar_split_clause, [], [f90, f262, f330, f398, f466])).
fof(f90, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f508, plain, (spl3_48 | spl3_44 | spl3_40 | spl3_36), inference(avatar_split_clause, [], [f91, f368, f385, f402, f419])).
fof(f91, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f507, plain, (spl3_60 | spl3_44 | spl3_28 | spl3_12), inference(avatar_split_clause, [], [f92, f266, f334, f402, f470])).
fof(f92, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f505, plain, (spl3_53 | spl3_37 | spl3_21 | spl3_5), inference(avatar_split_clause, [], [f94, f237, f305, f373, f441])).
fof(f94, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f504, plain, (spl3_30 | spl3_26 | spl3_22 | spl3_18), inference(avatar_split_clause, [], [f95, f292, f309, f326, f343])).
fof(f95, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f503, plain, (spl3_54 | spl3_38 | spl3_22 | spl3_6), inference(avatar_split_clause, [], [f96, f241, f309, f377, f445])).
fof(f96, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f500, plain, (spl3_32 | spl3_28 | spl3_24 | spl3_20), inference(avatar_split_clause, [], [f99, f300, f317, f334, f351])).
fof(f99, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f499, plain, (spl3_56 | spl3_40 | spl3_24 | spl3_8), inference(avatar_split_clause, [], [f100, f249, f317, f385, f453])).
fof(f100, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f493, plain, (spl3_51 | spl3_35 | spl3_19 | spl3_3), inference(avatar_split_clause, [], [f106, f228, f296, f364, f432])).
fof(f106, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f491, plain, (spl3_52 | spl3_36 | spl3_20 | spl3_4), inference(avatar_split_clause, [], [f108, f232, f300, f368, f436])).
fof(f108, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f473, plain, (spl3_57 | spl3_58 | spl3_59 | spl3_60), inference(avatar_split_clause, [], [f62, f470, f466, f462, f458])).
fof(f62, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG130+1.p', ax1)).
fof(f456, plain, (spl3_53 | spl3_54 | spl3_55 | spl3_56), inference(avatar_split_clause, [], [f63, f453, f449, f445, f441])).
fof(f63, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f439, plain, (spl3_49 | spl3_50 | spl3_51 | spl3_52), inference(avatar_split_clause, [], [f64, f436, f432, f428, f424])).
fof(f64, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f388, plain, (spl3_37 | spl3_38 | spl3_39 | spl3_40), inference(avatar_split_clause, [], [f67, f385, f381, f377, f373])).
fof(f67, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f337, plain, (spl3_25 | spl3_26 | spl3_27 | spl3_28), inference(avatar_split_clause, [], [f70, f334, f330, f326, f322])).
fof(f70, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f286, plain, (spl3_13 | spl3_14 | spl3_15 | spl3_16), inference(avatar_split_clause, [], [f73, f283, f279, f275, f271])).
fof(f73, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f269, plain, (spl3_9 | spl3_10 | spl3_11 | spl3_12), inference(avatar_split_clause, [], [f74, f266, f262, f258, f254])).
fof(f74, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f252, plain, (spl3_5 | spl3_6 | spl3_7 | spl3_8), inference(avatar_split_clause, [], [f75, f249, f245, f241, f237])).
fof(f75, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f235, plain, (spl3_1 | spl3_2 | spl3_3 | spl3_4), inference(avatar_split_clause, [], [f76, f232, f228, f224, f220])).
fof(f76, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).