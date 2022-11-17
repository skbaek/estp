fof(f1818, plain, $false, inference(avatar_sat_refutation, [], [f262, f279, f296, f313, f347, f364, f381, f398, f432, f466, f502, f505, f510, f512, f513, f518, f521, f522, f523, f527, f529, f532, f537, f538, f543, f544, f549, f550, f555, f556, f560, f562, f564, f566, f568, f569, f570, f571, f572, f575, f576, f578, f579, f580, f584, f586, f587, f588, f597, f606, f625, f630, f701, f706, f742, f774, f775, f776, f777, f788, f802, f809, f817, f821, f824, f855, f861, f874, f879, f881, f893, f900, f909, f916, f920, f924, f933, f936, f955, f965, f981, f993, f1003, f1023, f1037, f1038, f1059, f1084, f1102, f1104, f1119, f1133, f1144, f1188, f1199, f1203, f1209, f1221, f1227, f1231, f1237, f1242, f1290, f1330, f1332, f1352, f1372, f1373, f1381, f1383, f1387, f1391, f1392, f1405, f1414, f1415, f1427, f1435, f1438, f1459, f1489, f1494, f1511, f1515, f1521, f1530, f1560, f1564, f1586, f1600, f1677, f1698, f1707, f1709, f1717, f1720, f1723, f1726, f1741, f1750, f1751, f1755, f1796])).
fof(f1796, plain, (~ spl4_3 | ~ spl4_19), inference(avatar_split_clause, [], [f1795, f306, f238])).
fof(f238, plain, (spl4_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_3])])).
fof(f306, plain, (spl4_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_19])])).
fof(f1795, plain, (~ (e2 = op(e3, e3)) | ~ spl4_19), inference(backward_demodulation, [], [f134, f308])).
fof(f308, plain, ((e2 = op(e2, e3)) | ~ spl4_19), inference(avatar_component_clause, [], [f306])).
fof(f134, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax3)).
fof(f1755, plain, (~ spl4_30 | ~ spl4_63 | spl4_89), inference(avatar_contradiction_clause, [], [f1754])).
fof(f1754, plain, ($false | (~ spl4_30 | ~ spl4_63 | spl4_89)), inference(subsumption_resolution, [], [f1749, f355])).
fof(f355, plain, ((e1 = op(e2, e0)) | ~ spl4_30), inference(avatar_component_clause, [], [f353])).
fof(f353, plain, (spl4_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_30])])).
fof(f1749, plain, (~ (e1 = op(e2, e0)) | (~ spl4_63 | spl4_89)), inference(backward_demodulation, [], [f687, f495])).
fof(f495, plain, ((op(e0, e0) = e2) | ~ spl4_63), inference(avatar_component_clause, [], [f493])).
fof(f493, plain, (spl4_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl4_63])])).
fof(f687, plain, (~ (e1 = op(op(e0, e0), e0)) | spl4_89), inference(avatar_component_clause, [], [f685])).
fof(f685, plain, (spl4_89 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_89])])).
fof(f1751, plain, (spl4_54 | ~ spl4_63 | ~ spl4_82), inference(avatar_split_clause, [], [f1746, f651, f493, f455])).
fof(f455, plain, (spl4_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_54])])).
fof(f651, plain, (spl4_82 <=> (e1 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_82])])).
fof(f1746, plain, ((e1 = op(e0, e2)) | (~ spl4_63 | ~ spl4_82)), inference(backward_demodulation, [], [f652, f495])).
fof(f652, plain, ((e1 = op(e0, op(e0, e0))) | ~ spl4_82), inference(avatar_component_clause, [], [f651])).
fof(f1750, plain, (~ spl4_55 | ~ spl4_63), inference(avatar_split_clause, [], [f1743, f493, f459])).
fof(f459, plain, (spl4_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_55])])).
fof(f1743, plain, (~ (e2 = op(e0, e2)) | ~ spl4_63), inference(backward_demodulation, [], [f136, f495])).
fof(f136, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f1741, plain, (~ spl4_54 | ~ spl4_21 | spl4_72), inference(avatar_split_clause, [], [f1740, f603, f315, f455])).
fof(f315, plain, (spl4_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_21])])).
fof(f603, plain, (spl4_72 <=> (e1 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_72])])).
fof(f1740, plain, (~ (e1 = op(e0, e2)) | (~ spl4_21 | spl4_72)), inference(forward_demodulation, [], [f605, f317])).
fof(f317, plain, ((e0 = op(e2, e2)) | ~ spl4_21), inference(avatar_component_clause, [], [f315])).
fof(f605, plain, (~ (e1 = op(op(e2, e2), e2)) | spl4_72), inference(avatar_component_clause, [], [f603])).
fof(f1726, plain, (~ spl4_62 | ~ spl4_30), inference(avatar_split_clause, [], [f1725, f353, f489])).
fof(f489, plain, (spl4_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl4_62])])).
fof(f1725, plain, (~ (op(e0, e0) = e1) | ~ spl4_30), inference(forward_demodulation, [], [f112, f355])).
fof(f112, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1723, plain, (~ spl4_54 | ~ spl4_38), inference(avatar_split_clause, [], [f1722, f387, f455])).
fof(f387, plain, (spl4_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_38])])).
fof(f1722, plain, (~ (e1 = op(e0, e2)) | ~ spl4_38), inference(forward_demodulation, [], [f123, f389])).
fof(f389, plain, ((e1 = op(e1, e2)) | ~ spl4_38), inference(avatar_component_clause, [], [f387])).
fof(f123, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1720, plain, (~ spl4_42 | ~ spl4_38), inference(avatar_split_clause, [], [f1719, f387, f404])).
fof(f404, plain, (spl4_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_42])])).
fof(f1719, plain, (~ (e1 = op(e1, e1)) | ~ spl4_38), inference(forward_demodulation, [], [f144, f389])).
fof(f144, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1717, plain, (~ spl4_26 | ~ spl4_30), inference(avatar_split_clause, [], [f1716, f353, f336])).
fof(f336, plain, (spl4_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_26])])).
fof(f1716, plain, (~ (e1 = op(e2, e1)) | ~ spl4_30), inference(forward_demodulation, [], [f147, f355])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1709, plain, (~ spl4_9 | ~ spl4_13), inference(avatar_split_clause, [], [f1708, f281, f264])).
fof(f264, plain, (spl4_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_9])])).
fof(f281, plain, (spl4_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_13])])).
fof(f1708, plain, (~ (e0 = op(e3, e1)) | ~ spl4_13), inference(forward_demodulation, [], [f153, f283])).
fof(f283, plain, ((e0 = op(e3, e0)) | ~ spl4_13), inference(avatar_component_clause, [], [f281])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1707, plain, (~ spl4_6 | ~ spl4_38), inference(avatar_split_clause, [], [f1706, f387, f251])).
fof(f251, plain, (spl4_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_6])])).
fof(f1706, plain, (~ (e1 = op(e3, e2)) | ~ spl4_38), inference(forward_demodulation, [], [f127, f389])).
fof(f127, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1698, plain, (~ spl4_21 | ~ spl4_24), inference(avatar_contradiction_clause, [], [f1697])).
fof(f1697, plain, ($false | (~ spl4_21 | ~ spl4_24)), inference(subsumption_resolution, [], [f1696, f161])).
fof(f161, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax4)).
fof(f1696, plain, ((e0 = e3) | (~ spl4_21 | ~ spl4_24)), inference(forward_demodulation, [], [f329, f317])).
fof(f329, plain, ((e3 = op(e2, e2)) | ~ spl4_24), inference(avatar_component_clause, [], [f327])).
fof(f327, plain, (spl4_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_24])])).
fof(f1677, plain, (~ spl4_32 | ~ spl4_21 | spl4_90), inference(avatar_split_clause, [], [f1676, f692, f315, f361])).
fof(f361, plain, (spl4_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_32])])).
fof(f692, plain, (spl4_90 <=> (e3 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_90])])).
fof(f1676, plain, (~ (e3 = op(e2, e0)) | (~ spl4_21 | spl4_90)), inference(forward_demodulation, [], [f694, f317])).
fof(f694, plain, (~ (e3 = op(e2, op(e2, e2))) | spl4_90), inference(avatar_component_clause, [], [f692])).
fof(f1600, plain, (~ spl4_11 | ~ spl4_27), inference(avatar_split_clause, [], [f1596, f340, f272])).
fof(f272, plain, (spl4_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_11])])).
fof(f340, plain, (spl4_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_27])])).
fof(f1596, plain, (~ (e2 = op(e3, e1)) | ~ spl4_27), inference(backward_demodulation, [], [f122, f342])).
fof(f342, plain, ((e2 = op(e2, e1)) | ~ spl4_27), inference(avatar_component_clause, [], [f340])).
fof(f122, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1586, plain, (~ spl4_42 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1585])).
fof(f1585, plain, ($false | (~ spl4_42 | ~ spl4_44)), inference(subsumption_resolution, [], [f1584, f163])).
fof(f163, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f1584, plain, ((e1 = e3) | (~ spl4_42 | ~ spl4_44)), inference(backward_demodulation, [], [f414, f406])).
fof(f406, plain, ((e1 = op(e1, e1)) | ~ spl4_42), inference(avatar_component_clause, [], [f404])).
fof(f414, plain, ((e3 = op(e1, e1)) | ~ spl4_44), inference(avatar_component_clause, [], [f412])).
fof(f412, plain, (spl4_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_44])])).
fof(f1564, plain, (~ spl4_5 | ~ spl4_3 | spl4_69), inference(avatar_split_clause, [], [f1563, f590, f238, f247])).
fof(f247, plain, (spl4_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_5])])).
fof(f590, plain, (spl4_69 <=> (e0 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_69])])).
fof(f1563, plain, (~ (e0 = op(e3, e2)) | (~ spl4_3 | spl4_69)), inference(forward_demodulation, [], [f592, f240])).
fof(f240, plain, ((e2 = op(e3, e3)) | ~ spl4_3), inference(avatar_component_clause, [], [f238])).
fof(f592, plain, (~ (e0 = op(e3, op(e3, e3))) | spl4_69), inference(avatar_component_clause, [], [f590])).
fof(f1560, plain, (spl4_6 | ~ spl4_3 | ~ spl4_78), inference(avatar_split_clause, [], [f1559, f632, f238, f251])).
fof(f632, plain, (spl4_78 <=> (e1 = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_78])])).
fof(f1559, plain, ((e1 = op(e3, e2)) | (~ spl4_3 | ~ spl4_78)), inference(forward_demodulation, [], [f633, f240])).
fof(f633, plain, ((e1 = op(e3, op(e3, e3))) | ~ spl4_78), inference(avatar_component_clause, [], [f632])).
fof(f1530, plain, (~ spl4_35 | ~ spl4_3), inference(avatar_split_clause, [], [f1529, f238, f374])).
fof(f374, plain, (spl4_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_35])])).
fof(f1529, plain, (~ (e2 = op(e1, e3)) | ~ spl4_3), inference(forward_demodulation, [], [f133, f240])).
fof(f133, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1521, plain, (~ spl4_32 | ~ spl4_16), inference(avatar_split_clause, [], [f1520, f293, f361])).
fof(f293, plain, (spl4_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_16])])).
fof(f1520, plain, (~ (e3 = op(e2, e0)) | ~ spl4_16), inference(forward_demodulation, [], [f116, f295])).
fof(f295, plain, ((e3 = op(e3, e0)) | ~ spl4_16), inference(avatar_component_clause, [], [f293])).
fof(f116, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f1515, plain, (~ spl4_15 | ~ spl4_3), inference(avatar_split_clause, [], [f1514, f238, f289])).
fof(f289, plain, (spl4_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_15])])).
fof(f1514, plain, (~ (e2 = op(e3, e0)) | ~ spl4_3), inference(forward_demodulation, [], [f155, f240])).
fof(f155, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1511, plain, (~ spl4_7 | ~ spl4_3), inference(avatar_split_clause, [], [f1510, f238, f255])).
fof(f255, plain, (spl4_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_7])])).
fof(f1510, plain, (~ (e2 = op(e3, e2)) | ~ spl4_3), inference(forward_demodulation, [], [f158, f240])).
fof(f158, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1494, plain, (~ spl4_1 | ~ spl4_17), inference(avatar_split_clause, [], [f1492, f298, f230])).
fof(f230, plain, (spl4_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_1])])).
fof(f298, plain, (spl4_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_17])])).
fof(f1492, plain, (~ (e0 = op(e3, e3)) | ~ spl4_17), inference(backward_demodulation, [], [f134, f300])).
fof(f300, plain, ((e0 = op(e2, e3)) | ~ spl4_17), inference(avatar_component_clause, [], [f298])).
fof(f1489, plain, (~ spl4_6 | ~ spl4_22), inference(avatar_split_clause, [], [f1479, f319, f251])).
fof(f319, plain, (spl4_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_22])])).
fof(f1479, plain, (~ (e1 = op(e3, e2)) | ~ spl4_22), inference(backward_demodulation, [], [f128, f321])).
fof(f321, plain, ((e1 = op(e2, e2)) | ~ spl4_22), inference(avatar_component_clause, [], [f319])).
fof(f128, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1459, plain, (~ spl4_61 | ~ spl4_64), inference(avatar_contradiction_clause, [], [f1458])).
fof(f1458, plain, ($false | (~ spl4_61 | ~ spl4_64)), inference(subsumption_resolution, [], [f1457, f161])).
fof(f1457, plain, ((e0 = e3) | (~ spl4_61 | ~ spl4_64)), inference(backward_demodulation, [], [f499, f487])).
fof(f487, plain, ((e0 = op(e0, e0)) | ~ spl4_61), inference(avatar_component_clause, [], [f485])).
fof(f485, plain, (spl4_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_61])])).
fof(f499, plain, ((op(e0, e0) = e3) | ~ spl4_64), inference(avatar_component_clause, [], [f497])).
fof(f497, plain, (spl4_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl4_64])])).
fof(f1438, plain, (~ spl4_42 | ~ spl4_34), inference(avatar_split_clause, [], [f1437, f370, f404])).
fof(f370, plain, (spl4_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_34])])).
fof(f1437, plain, (~ (e1 = op(e1, e1)) | ~ spl4_34), inference(forward_demodulation, [], [f145, f372])).
fof(f372, plain, ((e1 = op(e1, e3)) | ~ spl4_34), inference(avatar_component_clause, [], [f370])).
fof(f145, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1435, plain, (~ spl4_31 | ~ spl4_47), inference(avatar_split_clause, [], [f1434, f425, f357])).
fof(f357, plain, (spl4_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_31])])).
fof(f425, plain, (spl4_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_47])])).
fof(f1434, plain, (~ (e2 = op(e2, e0)) | ~ spl4_47), inference(forward_demodulation, [], [f114, f427])).
fof(f427, plain, ((e2 = op(e1, e0)) | ~ spl4_47), inference(avatar_component_clause, [], [f425])).
fof(f114, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1427, plain, (~ spl4_1 | ~ spl4_4), inference(avatar_contradiction_clause, [], [f1426])).
fof(f1426, plain, ($false | (~ spl4_1 | ~ spl4_4)), inference(subsumption_resolution, [], [f1425, f161])).
fof(f1425, plain, ((e0 = e3) | (~ spl4_1 | ~ spl4_4)), inference(forward_demodulation, [], [f244, f232])).
fof(f232, plain, ((e0 = op(e3, e3)) | ~ spl4_1), inference(avatar_component_clause, [], [f230])).
fof(f244, plain, ((e3 = op(e3, e3)) | ~ spl4_4), inference(avatar_component_clause, [], [f242])).
fof(f242, plain, (spl4_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_4])])).
fof(f1415, plain, (~ spl4_6 | ~ spl4_24 | spl4_72), inference(avatar_split_clause, [], [f1409, f603, f327, f251])).
fof(f1409, plain, (~ (e1 = op(e3, e2)) | (~ spl4_24 | spl4_72)), inference(backward_demodulation, [], [f605, f329])).
fof(f1414, plain, (~ spl4_20 | ~ spl4_24), inference(avatar_split_clause, [], [f1408, f327, f310])).
fof(f310, plain, (spl4_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_20])])).
fof(f1408, plain, (~ (e3 = op(e2, e3)) | ~ spl4_24), inference(backward_demodulation, [], [f152, f329])).
fof(f152, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1405, plain, (~ spl4_24 | ~ spl4_28), inference(avatar_split_clause, [], [f1403, f344, f327])).
fof(f344, plain, (spl4_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_28])])).
fof(f1403, plain, (~ (e3 = op(e2, e2)) | ~ spl4_28), inference(backward_demodulation, [], [f150, f346])).
fof(f346, plain, ((e3 = op(e2, e1)) | ~ spl4_28), inference(avatar_component_clause, [], [f344])).
fof(f150, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1392, plain, (~ spl4_8 | ~ spl4_40), inference(avatar_split_clause, [], [f1389, f395, f259])).
fof(f259, plain, (spl4_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_8])])).
fof(f395, plain, (spl4_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_40])])).
fof(f1389, plain, (~ (e3 = op(e3, e2)) | ~ spl4_40), inference(backward_demodulation, [], [f127, f397])).
fof(f397, plain, ((e3 = op(e1, e2)) | ~ spl4_40), inference(avatar_component_clause, [], [f395])).
fof(f1391, plain, (~ spl4_24 | ~ spl4_40), inference(avatar_split_clause, [], [f1388, f395, f327])).
fof(f1388, plain, (~ (e3 = op(e2, e2)) | ~ spl4_40), inference(backward_demodulation, [], [f126, f397])).
fof(f126, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1387, plain, (~ spl4_42 | ~ spl4_43), inference(avatar_contradiction_clause, [], [f1386])).
fof(f1386, plain, ($false | (~ spl4_42 | ~ spl4_43)), inference(subsumption_resolution, [], [f1385, f162])).
fof(f162, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1385, plain, ((e1 = e2) | (~ spl4_42 | ~ spl4_43)), inference(backward_demodulation, [], [f410, f406])).
fof(f410, plain, ((e2 = op(e1, e1)) | ~ spl4_43), inference(avatar_component_clause, [], [f408])).
fof(f408, plain, (spl4_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_43])])).
fof(f1383, plain, (~ spl4_37 | ~ spl4_43 | spl4_74), inference(avatar_split_clause, [], [f1378, f613, f408, f383])).
fof(f383, plain, (spl4_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_37])])).
fof(f613, plain, (spl4_74 <=> (e0 = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl4_74])])).
fof(f1378, plain, (~ (e0 = op(e1, e2)) | (~ spl4_43 | spl4_74)), inference(backward_demodulation, [], [f615, f410])).
fof(f615, plain, (~ (e0 = op(e1, op(e1, e1))) | spl4_74), inference(avatar_component_clause, [], [f613])).
fof(f1381, plain, (~ spl4_39 | ~ spl4_43), inference(avatar_split_clause, [], [f1375, f408, f391])).
fof(f391, plain, (spl4_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_39])])).
fof(f1375, plain, (~ (e2 = op(e1, e2)) | ~ spl4_43), inference(backward_demodulation, [], [f144, f410])).
fof(f1373, plain, (~ spl4_39 | ~ spl4_47), inference(avatar_split_clause, [], [f1370, f425, f391])).
fof(f1370, plain, (~ (e2 = op(e1, e2)) | ~ spl4_47), inference(backward_demodulation, [], [f142, f427])).
fof(f142, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1372, plain, (~ spl4_43 | ~ spl4_47), inference(avatar_split_clause, [], [f1369, f425, f408])).
fof(f1369, plain, (~ (e2 = op(e1, e1)) | ~ spl4_47), inference(backward_demodulation, [], [f141, f427])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1352, plain, (~ spl4_62 | ~ spl4_64), inference(avatar_contradiction_clause, [], [f1351])).
fof(f1351, plain, ($false | (~ spl4_62 | ~ spl4_64)), inference(subsumption_resolution, [], [f1350, f163])).
fof(f1350, plain, ((e1 = e3) | (~ spl4_62 | ~ spl4_64)), inference(backward_demodulation, [], [f499, f491])).
fof(f491, plain, ((op(e0, e0) = e1) | ~ spl4_62), inference(avatar_component_clause, [], [f489])).
fof(f1332, plain, (~ spl4_48 | ~ spl4_64), inference(avatar_split_clause, [], [f1331, f497, f429])).
fof(f429, plain, (spl4_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_48])])).
fof(f1331, plain, (~ (e3 = op(e1, e0)) | ~ spl4_64), inference(forward_demodulation, [], [f111, f499])).
fof(f111, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f1330, plain, (~ spl4_62 | ~ spl4_14), inference(avatar_split_clause, [], [f1329, f285, f489])).
fof(f285, plain, (spl4_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_14])])).
fof(f1329, plain, (~ (op(e0, e0) = e1) | ~ spl4_14), inference(forward_demodulation, [], [f113, f287])).
fof(f287, plain, ((e1 = op(e3, e0)) | ~ spl4_14), inference(avatar_component_clause, [], [f285])).
fof(f113, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f1290, plain, (~ spl4_1 | ~ spl4_2), inference(avatar_contradiction_clause, [], [f1289])).
fof(f1289, plain, ($false | (~ spl4_1 | ~ spl4_2)), inference(subsumption_resolution, [], [f1288, f159])).
fof(f159, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1288, plain, ((e0 = e1) | (~ spl4_1 | ~ spl4_2)), inference(backward_demodulation, [], [f236, f232])).
fof(f236, plain, ((e1 = op(e3, e3)) | ~ spl4_2), inference(avatar_component_clause, [], [f234])).
fof(f234, plain, (spl4_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_2])])).
fof(f1242, plain, (~ spl4_63 | ~ spl4_64), inference(avatar_contradiction_clause, [], [f1241])).
fof(f1241, plain, ($false | (~ spl4_63 | ~ spl4_64)), inference(subsumption_resolution, [], [f1240, f164])).
fof(f164, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1240, plain, ((e2 = e3) | (~ spl4_63 | ~ spl4_64)), inference(forward_demodulation, [], [f499, f495])).
fof(f1237, plain, (~ spl4_56 | ~ spl4_63 | spl4_92), inference(avatar_split_clause, [], [f1236, f703, f493, f463])).
fof(f463, plain, (spl4_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_56])])).
fof(f703, plain, (spl4_92 <=> (e3 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_92])])).
fof(f1236, plain, (~ (e3 = op(e0, e2)) | (~ spl4_63 | spl4_92)), inference(forward_demodulation, [], [f705, f495])).
fof(f705, plain, (~ (e3 = op(e0, op(e0, e0))) | spl4_92), inference(avatar_component_clause, [], [f703])).
fof(f1231, plain, (~ spl4_54 | ~ spl4_63 | spl4_82), inference(avatar_split_clause, [], [f1230, f651, f493, f455])).
fof(f1230, plain, (~ (e1 = op(e0, e2)) | (~ spl4_63 | spl4_82)), inference(forward_demodulation, [], [f653, f495])).
fof(f653, plain, (~ (e1 = op(e0, op(e0, e0))) | spl4_82), inference(avatar_component_clause, [], [f651])).
fof(f1227, plain, (~ spl4_31 | ~ spl4_63), inference(avatar_contradiction_clause, [], [f1226])).
fof(f1226, plain, ($false | (~ spl4_31 | ~ spl4_63)), inference(subsumption_resolution, [], [f1225, f495])).
fof(f1225, plain, (~ (op(e0, e0) = e2) | ~ spl4_31), inference(forward_demodulation, [], [f112, f359])).
fof(f359, plain, ((e2 = op(e2, e0)) | ~ spl4_31), inference(avatar_component_clause, [], [f357])).
fof(f1221, plain, (~ spl4_56 | ~ spl4_8), inference(avatar_split_clause, [], [f1220, f259, f463])).
fof(f1220, plain, (~ (e3 = op(e0, e2)) | ~ spl4_8), inference(forward_demodulation, [], [f125, f261])).
fof(f261, plain, ((e3 = op(e3, e2)) | ~ spl4_8), inference(avatar_component_clause, [], [f259])).
fof(f125, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1209, plain, (~ spl4_23 | ~ spl4_31), inference(avatar_split_clause, [], [f1139, f357, f323])).
fof(f323, plain, (spl4_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_23])])).
fof(f1139, plain, (~ (e2 = op(e2, e2)) | ~ spl4_31), inference(backward_demodulation, [], [f148, f359])).
fof(f148, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1203, plain, (~ spl4_14 | ~ spl4_2), inference(avatar_split_clause, [], [f1202, f234, f285])).
fof(f1202, plain, (~ (e1 = op(e3, e0)) | ~ spl4_2), inference(forward_demodulation, [], [f155, f236])).
fof(f1199, plain, (~ spl4_10 | ~ spl4_2), inference(avatar_split_clause, [], [f1198, f234, f268])).
fof(f268, plain, (spl4_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_10])])).
fof(f1198, plain, (~ (e1 = op(e3, e1)) | ~ spl4_2), inference(forward_demodulation, [], [f157, f236])).
fof(f157, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1188, plain, (~ spl4_2 | ~ spl4_3), inference(avatar_contradiction_clause, [], [f1187])).
fof(f1187, plain, ($false | (~ spl4_2 | ~ spl4_3)), inference(subsumption_resolution, [], [f1186, f162])).
fof(f1186, plain, ((e1 = e2) | (~ spl4_2 | ~ spl4_3)), inference(backward_demodulation, [], [f240, f236])).
fof(f1144, plain, (~ spl4_12 | ~ spl4_28), inference(avatar_split_clause, [], [f1143, f344, f276])).
fof(f276, plain, (spl4_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_12])])).
fof(f1143, plain, (~ (e3 = op(e3, e1)) | ~ spl4_28), inference(backward_demodulation, [], [f122, f346])).
fof(f1133, plain, (~ spl4_41 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1132])).
fof(f1132, plain, ($false | (~ spl4_41 | ~ spl4_44)), inference(subsumption_resolution, [], [f1131, f161])).
fof(f1131, plain, ((e0 = e3) | (~ spl4_41 | ~ spl4_44)), inference(backward_demodulation, [], [f414, f402])).
fof(f402, plain, ((e0 = op(e1, e1)) | ~ spl4_41), inference(avatar_component_clause, [], [f400])).
fof(f400, plain, (spl4_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_41])])).
fof(f1119, plain, (~ spl4_62 | ~ spl4_63), inference(avatar_contradiction_clause, [], [f1118])).
fof(f1118, plain, ($false | (~ spl4_62 | ~ spl4_63)), inference(subsumption_resolution, [], [f1117, f162])).
fof(f1117, plain, ((e1 = e2) | (~ spl4_62 | ~ spl4_63)), inference(forward_demodulation, [], [f495, f491])).
fof(f1104, plain, (~ spl4_48 | ~ spl4_36), inference(avatar_split_clause, [], [f1103, f378, f429])).
fof(f378, plain, (spl4_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_36])])).
fof(f1103, plain, (~ (e3 = op(e1, e0)) | ~ spl4_36), inference(forward_demodulation, [], [f143, f380])).
fof(f380, plain, ((e3 = op(e1, e3)) | ~ spl4_36), inference(avatar_component_clause, [], [f378])).
fof(f143, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1102, plain, (~ spl4_46 | ~ spl4_62), inference(avatar_split_clause, [], [f1101, f489, f421])).
fof(f421, plain, (spl4_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_46])])).
fof(f1101, plain, (~ (e1 = op(e1, e0)) | ~ spl4_62), inference(forward_demodulation, [], [f111, f491])).
fof(f1084, plain, (~ spl4_12 | ~ spl4_44), inference(avatar_split_clause, [], [f1083, f412, f276])).
fof(f1083, plain, (~ (e3 = op(e3, e1)) | ~ spl4_44), inference(backward_demodulation, [], [f121, f414])).
fof(f121, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1059, plain, (~ spl4_43 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1058])).
fof(f1058, plain, ($false | (~ spl4_43 | ~ spl4_44)), inference(subsumption_resolution, [], [f1057, f164])).
fof(f1057, plain, ((e2 = e3) | (~ spl4_43 | ~ spl4_44)), inference(forward_demodulation, [], [f414, f410])).
fof(f1038, plain, (~ spl4_50 | ~ spl4_62), inference(avatar_split_clause, [], [f1032, f489, f438])).
fof(f438, plain, (spl4_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_50])])).
fof(f1032, plain, (~ (e1 = op(e0, e3)) | ~ spl4_62), inference(backward_demodulation, [], [f137, f491])).
fof(f137, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1037, plain, (~ spl4_54 | ~ spl4_62), inference(avatar_split_clause, [], [f1031, f489, f455])).
fof(f1031, plain, (~ (e1 = op(e0, e2)) | ~ spl4_62), inference(backward_demodulation, [], [f136, f491])).
fof(f1023, plain, (~ spl4_64 | ~ spl4_32), inference(avatar_split_clause, [], [f1022, f361, f497])).
fof(f1022, plain, (~ (op(e0, e0) = e3) | ~ spl4_32), inference(forward_demodulation, [], [f112, f363])).
fof(f363, plain, ((e3 = op(e2, e0)) | ~ spl4_32), inference(avatar_component_clause, [], [f361])).
fof(f1003, plain, (~ spl4_28 | ~ spl4_43 | spl4_77), inference(avatar_split_clause, [], [f1002, f627, f408, f344])).
fof(f627, plain, (spl4_77 <=> (e3 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_77])])).
fof(f1002, plain, (~ (e3 = op(e2, e1)) | (~ spl4_43 | spl4_77)), inference(forward_demodulation, [], [f629, f410])).
fof(f629, plain, (~ (e3 = op(op(e1, e1), e1)) | spl4_77), inference(avatar_component_clause, [], [f627])).
fof(f993, plain, (~ spl4_34 | ~ spl4_18), inference(avatar_split_clause, [], [f992, f302, f370])).
fof(f302, plain, (spl4_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_18])])).
fof(f992, plain, (~ (e1 = op(e1, e3)) | ~ spl4_18), inference(forward_demodulation, [], [f132, f304])).
fof(f304, plain, ((e1 = op(e2, e3)) | ~ spl4_18), inference(avatar_component_clause, [], [f302])).
fof(f132, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f981, plain, (~ spl4_25 | ~ spl4_22 | spl4_71), inference(avatar_split_clause, [], [f980, f599, f319, f332])).
fof(f332, plain, (spl4_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_25])])).
fof(f599, plain, (spl4_71 <=> (e0 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_71])])).
fof(f980, plain, (~ (e0 = op(e2, e1)) | (~ spl4_22 | spl4_71)), inference(forward_demodulation, [], [f601, f321])).
fof(f601, plain, (~ (e0 = op(e2, op(e2, e2))) | spl4_71), inference(avatar_component_clause, [], [f599])).
fof(f965, plain, (~ spl4_1 | ~ spl4_3), inference(avatar_contradiction_clause, [], [f964])).
fof(f964, plain, ($false | (~ spl4_1 | ~ spl4_3)), inference(subsumption_resolution, [], [f963, f160])).
fof(f160, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f963, plain, ((e0 = e2) | (~ spl4_1 | ~ spl4_3)), inference(backward_demodulation, [], [f240, f232])).
fof(f955, plain, (~ spl4_21 | ~ spl4_22), inference(avatar_contradiction_clause, [], [f954])).
fof(f954, plain, ($false | (~ spl4_21 | ~ spl4_22)), inference(subsumption_resolution, [], [f953, f159])).
fof(f953, plain, ((e0 = e1) | (~ spl4_21 | ~ spl4_22)), inference(backward_demodulation, [], [f321, f317])).
fof(f936, plain, (~ spl4_41 | ~ spl4_43), inference(avatar_contradiction_clause, [], [f935])).
fof(f935, plain, ($false | (~ spl4_41 | ~ spl4_43)), inference(subsumption_resolution, [], [f934, f160])).
fof(f934, plain, ((e0 = e2) | (~ spl4_41 | ~ spl4_43)), inference(forward_demodulation, [], [f410, f402])).
fof(f933, plain, (~ spl4_38 | ~ spl4_46), inference(avatar_split_clause, [], [f931, f421, f387])).
fof(f931, plain, (~ (e1 = op(e1, e2)) | ~ spl4_46), inference(backward_demodulation, [], [f142, f423])).
fof(f423, plain, ((e1 = op(e1, e0)) | ~ spl4_46), inference(avatar_component_clause, [], [f421])).
fof(f924, plain, (~ spl4_54 | ~ spl4_58), inference(avatar_split_clause, [], [f922, f472, f455])).
fof(f472, plain, (spl4_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_58])])).
fof(f922, plain, (~ (e1 = op(e0, e2)) | ~ spl4_58), inference(backward_demodulation, [], [f138, f474])).
fof(f474, plain, ((e1 = op(e0, e1)) | ~ spl4_58), inference(avatar_component_clause, [], [f472])).
fof(f138, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f920, plain, (~ spl4_40 | ~ spl4_22 | spl4_76), inference(avatar_split_clause, [], [f919, f622, f319, f395])).
fof(f622, plain, (spl4_76 <=> (e3 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_76])])).
fof(f919, plain, (~ (e3 = op(e1, e2)) | (~ spl4_22 | spl4_76)), inference(forward_demodulation, [], [f624, f321])).
fof(f624, plain, (~ (e3 = op(op(e2, e2), e2)) | spl4_76), inference(avatar_component_clause, [], [f622])).
fof(f916, plain, (~ spl4_57 | ~ spl4_41), inference(avatar_split_clause, [], [f915, f400, f468])).
fof(f468, plain, (spl4_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_57])])).
fof(f915, plain, (~ (e0 = op(e0, e1)) | ~ spl4_41), inference(forward_demodulation, [], [f117, f402])).
fof(f117, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f909, plain, (~ spl4_54 | ~ spl4_22), inference(avatar_split_clause, [], [f908, f319, f455])).
fof(f908, plain, (~ (e1 = op(e0, e2)) | ~ spl4_22), inference(forward_demodulation, [], [f124, f321])).
fof(f124, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f900, plain, (~ spl4_45 | ~ spl4_41), inference(avatar_split_clause, [], [f899, f400, f417])).
fof(f417, plain, (spl4_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_45])])).
fof(f899, plain, (~ (e0 = op(e1, e0)) | ~ spl4_41), inference(forward_demodulation, [], [f141, f402])).
fof(f893, plain, (~ spl4_38 | ~ spl4_34), inference(avatar_split_clause, [], [f892, f370, f387])).
fof(f892, plain, (~ (e1 = op(e1, e2)) | ~ spl4_34), inference(forward_demodulation, [], [f146, f372])).
fof(f146, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f881, plain, (~ spl4_26 | ~ spl4_22), inference(avatar_split_clause, [], [f880, f319, f336])).
fof(f880, plain, (~ (e1 = op(e2, e1)) | ~ spl4_22), inference(forward_demodulation, [], [f150, f321])).
fof(f879, plain, (~ spl4_22 | ~ spl4_24), inference(avatar_contradiction_clause, [], [f878])).
fof(f878, plain, ($false | (~ spl4_22 | ~ spl4_24)), inference(subsumption_resolution, [], [f877, f163])).
fof(f877, plain, ((e1 = e3) | (~ spl4_22 | ~ spl4_24)), inference(forward_demodulation, [], [f329, f321])).
fof(f874, plain, (~ spl4_18 | ~ spl4_3 | spl4_70), inference(avatar_split_clause, [], [f856, f594, f238, f302])).
fof(f594, plain, (spl4_70 <=> (e1 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_70])])).
fof(f856, plain, (~ (e1 = op(e2, e3)) | (~ spl4_3 | spl4_70)), inference(backward_demodulation, [], [f596, f240])).
fof(f596, plain, (~ (e1 = op(op(e3, e3), e3)) | spl4_70), inference(avatar_component_clause, [], [f594])).
fof(f861, plain, (~ spl4_6 | ~ spl4_3 | spl4_78), inference(avatar_split_clause, [], [f857, f632, f238, f251])).
fof(f857, plain, (~ (e1 = op(e3, e2)) | (~ spl4_3 | spl4_78)), inference(backward_demodulation, [], [f634, f240])).
fof(f634, plain, (~ (e1 = op(e3, op(e3, e3))) | spl4_78), inference(avatar_component_clause, [], [f632])).
fof(f855, plain, (~ spl4_2 | ~ spl4_6), inference(avatar_split_clause, [], [f854, f251, f234])).
fof(f854, plain, (~ (e1 = op(e3, e3)) | ~ spl4_6), inference(backward_demodulation, [], [f158, f253])).
fof(f253, plain, ((e1 = op(e3, e2)) | ~ spl4_6), inference(avatar_component_clause, [], [f251])).
fof(f824, plain, (~ spl4_22 | ~ spl4_23), inference(avatar_contradiction_clause, [], [f823])).
fof(f823, plain, ($false | (~ spl4_22 | ~ spl4_23)), inference(subsumption_resolution, [], [f822, f162])).
fof(f822, plain, ((e1 = e2) | (~ spl4_22 | ~ spl4_23)), inference(backward_demodulation, [], [f325, f321])).
fof(f325, plain, ((e2 = op(e2, e2)) | ~ spl4_23), inference(avatar_component_clause, [], [f323])).
fof(f821, plain, (~ spl4_23 | ~ spl4_24), inference(avatar_contradiction_clause, [], [f820])).
fof(f820, plain, ($false | (~ spl4_23 | ~ spl4_24)), inference(subsumption_resolution, [], [f819, f164])).
fof(f819, plain, ((e2 = e3) | (~ spl4_23 | ~ spl4_24)), inference(backward_demodulation, [], [f329, f325])).
fof(f817, plain, (~ spl4_17 | ~ spl4_24 | spl4_71), inference(avatar_split_clause, [], [f813, f599, f327, f298])).
fof(f813, plain, (~ (e0 = op(e2, e3)) | (~ spl4_24 | spl4_71)), inference(backward_demodulation, [], [f601, f329])).
fof(f809, plain, (~ spl4_21 | ~ spl4_25), inference(avatar_split_clause, [], [f806, f332, f315])).
fof(f806, plain, (~ (e0 = op(e2, e2)) | ~ spl4_25), inference(backward_demodulation, [], [f150, f334])).
fof(f334, plain, ((e0 = op(e2, e1)) | ~ spl4_25), inference(avatar_component_clause, [], [f332])).
fof(f802, plain, (~ spl4_21 | ~ spl4_29), inference(avatar_split_clause, [], [f798, f349, f315])).
fof(f349, plain, (spl4_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_29])])).
fof(f798, plain, (~ (e0 = op(e2, e2)) | ~ spl4_29), inference(backward_demodulation, [], [f148, f351])).
fof(f351, plain, ((e0 = op(e2, e0)) | ~ spl4_29), inference(avatar_component_clause, [], [f349])).
fof(f788, plain, (~ spl4_20 | ~ spl4_36), inference(avatar_split_clause, [], [f786, f378, f310])).
fof(f786, plain, (~ (e3 = op(e2, e3)) | ~ spl4_36), inference(backward_demodulation, [], [f132, f380])).
fof(f777, plain, (~ spl4_33 | ~ spl4_41), inference(avatar_split_clause, [], [f770, f400, f366])).
fof(f366, plain, (spl4_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_33])])).
fof(f770, plain, (~ (e0 = op(e1, e3)) | ~ spl4_41), inference(backward_demodulation, [], [f145, f402])).
fof(f776, plain, (~ spl4_37 | ~ spl4_41), inference(avatar_split_clause, [], [f769, f400, f383])).
fof(f769, plain, (~ (e0 = op(e1, e2)) | ~ spl4_41), inference(backward_demodulation, [], [f144, f402])).
fof(f775, plain, (~ spl4_9 | ~ spl4_41), inference(avatar_split_clause, [], [f768, f400, f264])).
fof(f768, plain, (~ (e0 = op(e3, e1)) | ~ spl4_41), inference(backward_demodulation, [], [f121, f402])).
fof(f774, plain, (~ spl4_25 | ~ spl4_41), inference(avatar_split_clause, [], [f767, f400, f332])).
fof(f767, plain, (~ (e0 = op(e2, e1)) | ~ spl4_41), inference(backward_demodulation, [], [f120, f402])).
fof(f120, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f742, plain, (~ spl4_5 | ~ spl4_53), inference(avatar_split_clause, [], [f738, f451, f247])).
fof(f451, plain, (spl4_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_53])])).
fof(f738, plain, (~ (e0 = op(e3, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f125, f453])).
fof(f453, plain, ((e0 = op(e0, e2)) | ~ spl4_53), inference(avatar_component_clause, [], [f451])).
fof(f706, plain, (~ spl4_92 | ~ spl4_89 | ~ spl4_63), inference(avatar_split_clause, [], [f226, f493, f685, f703])).
fof(f226, plain, (~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(e0, op(e0, e0)))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(e0, op(e0, e0)))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((op(e0, e0) = e2) & (e1 = op(op(e0, e0), e0)) & (e3 = op(e0, op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax27)).
fof(f701, plain, (~ spl4_90 | ~ spl4_72 | ~ spl4_21), inference(avatar_split_clause, [], [f225, f315, f603, f692])).
fof(f225, plain, (~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(e2, e2)) & (e1 = op(op(e2, e2), e2)) & (e3 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax26)).
fof(f630, plain, (~ spl4_74 | ~ spl4_77 | ~ spl4_43), inference(avatar_split_clause, [], [f210, f408, f627, f613])).
fof(f210, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1)) | ~ (e0 = op(e1, op(e1, e1)))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1)) | ~ (e0 = op(e1, op(e1, e1)))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e2 = op(e1, e1)) & (e3 = op(op(e1, e1), e1)) & (e0 = op(e1, op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax11)).
fof(f625, plain, (~ spl4_71 | ~ spl4_76 | ~ spl4_22), inference(avatar_split_clause, [], [f209, f319, f622, f599])).
fof(f209, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2)) | ~ (e0 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2)) | ~ (e0 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e1 = op(e2, e2)) & (e3 = op(op(e2, e2), e2)) & (e0 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax10)).
fof(f606, plain, (~ spl4_71 | ~ spl4_72 | ~ spl4_24), inference(avatar_split_clause, [], [f206, f327, f603, f599])).
fof(f206, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e0 = op(e2, op(e2, e2)))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2)) | ~ (e0 = op(e2, op(e2, e2)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e3 = op(e2, e2)) & (e1 = op(op(e2, e2), e2)) & (e0 = op(e2, op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax7)).
fof(f597, plain, (~ spl4_69 | ~ spl4_70 | ~ spl4_3), inference(avatar_split_clause, [], [f205, f238, f594, f590])).
fof(f205, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(e3, op(e3, e3)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(e3, op(e3, e3)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(e3, e3)) & (e1 = op(op(e3, e3), e3)) & (e0 = op(e3, op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax6)).
fof(f588, plain, (spl4_68 | spl4_41 | spl4_21 | spl4_1), inference(avatar_split_clause, [], [f173, f230, f315, f400, f552])).
fof(f552, plain, (spl4_68 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl4_68])])).
fof(f173, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | (~ (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | sP3) & ((~ (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | sP2) & ((~ (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | (~ (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | sP1) & ((~ (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | sP0)), inference(definition_folding, [], [f5, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e0 = op(e0, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e0 = op(e0, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e0 = op(e0, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f5, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | (~ (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | (~ (e0 = op(e0, e3)) & (op(e0, e0) = e3))) & ((~ (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | (~ (e0 = op(e0, e2)) & (op(e0, e0) = e2))) & ((~ (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | (~ (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e0 = op(e0, e1)) & (op(e0, e0) = e1))) & ((~ (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax5)).
fof(f587, plain, (spl4_68 | ~ spl4_46 | spl4_21 | spl4_1), inference(avatar_split_clause, [], [f174, f230, f315, f421, f552])).
fof(f174, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | ~ (e1 = op(e1, e0)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f586, plain, (spl4_68 | spl4_41 | ~ spl4_31 | spl4_1), inference(avatar_split_clause, [], [f175, f230, f357, f400, f552])).
fof(f175, plain, ((e0 = op(e3, e3)) | ~ (e2 = op(e2, e0)) | (e0 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f584, plain, (spl4_68 | spl4_41 | spl4_21 | ~ spl4_16), inference(avatar_split_clause, [], [f177, f293, f315, f400, f552])).
fof(f177, plain, (~ (e3 = op(e3, e0)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f580, plain, (spl4_67 | spl4_42 | spl4_22 | spl4_2), inference(avatar_split_clause, [], [f181, f234, f319, f404, f546])).
fof(f546, plain, (spl4_67 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl4_67])])).
fof(f181, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f579, plain, (spl4_67 | ~ spl4_42 | spl4_22 | spl4_2), inference(avatar_split_clause, [], [f182, f234, f319, f404, f546])).
fof(f182, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f578, plain, (spl4_67 | spl4_42 | ~ spl4_27 | spl4_2), inference(avatar_split_clause, [], [f183, f234, f340, f404, f546])).
fof(f183, plain, ((e1 = op(e3, e3)) | ~ (e2 = op(e2, e1)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f576, plain, (spl4_67 | spl4_42 | spl4_22 | ~ spl4_12), inference(avatar_split_clause, [], [f185, f276, f319, f404, f546])).
fof(f185, plain, (~ (e3 = op(e3, e1)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f575, plain, (spl4_67 | ~ spl4_42 | spl4_22 | ~ spl4_12), inference(avatar_split_clause, [], [f186, f276, f319, f404, f546])).
fof(f186, plain, (~ (e3 = op(e3, e1)) | (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f572, plain, (spl4_66 | spl4_43 | spl4_23 | spl4_3), inference(avatar_split_clause, [], [f189, f238, f323, f408, f540])).
fof(f540, plain, (spl4_66 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl4_66])])).
fof(f189, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f571, plain, (spl4_66 | ~ spl4_38 | spl4_23 | spl4_3), inference(avatar_split_clause, [], [f190, f238, f323, f387, f540])).
fof(f190, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | ~ (e1 = op(e1, e2)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f570, plain, (spl4_66 | spl4_43 | ~ spl4_23 | spl4_3), inference(avatar_split_clause, [], [f191, f238, f323, f408, f540])).
fof(f191, plain, ((e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f569, plain, (spl4_66 | ~ spl4_38 | ~ spl4_23 | spl4_3), inference(avatar_split_clause, [], [f192, f238, f323, f387, f540])).
fof(f192, plain, ((e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e1 = op(e1, e2)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f568, plain, (spl4_66 | spl4_43 | spl4_23 | ~ spl4_8), inference(avatar_split_clause, [], [f193, f259, f323, f408, f540])).
fof(f193, plain, (~ (e3 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f566, plain, (spl4_66 | spl4_43 | ~ spl4_23 | ~ spl4_8), inference(avatar_split_clause, [], [f195, f259, f323, f408, f540])).
fof(f195, plain, (~ (e3 = op(e3, e2)) | ~ (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f564, plain, (spl4_65 | spl4_44 | spl4_24 | spl4_4), inference(avatar_split_clause, [], [f197, f242, f327, f412, f534])).
fof(f534, plain, (spl4_65 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl4_65])])).
fof(f197, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f562, plain, (spl4_65 | spl4_44 | ~ spl4_19 | spl4_4), inference(avatar_split_clause, [], [f199, f242, f306, f412, f534])).
fof(f199, plain, ((e3 = op(e3, e3)) | ~ (e2 = op(e2, e3)) | (e3 = op(e1, e1)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f560, plain, (spl4_65 | spl4_44 | spl4_24 | ~ spl4_4), inference(avatar_split_clause, [], [f201, f242, f327, f412, f534])).
fof(f201, plain, (~ (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f556, plain, (~ spl4_68 | spl4_61), inference(avatar_split_clause, [], [f171, f485, f552])).
fof(f171, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f555, plain, (~ spl4_68 | ~ spl4_61), inference(avatar_split_clause, [], [f172, f485, f552])).
fof(f172, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f550, plain, (~ spl4_67 | spl4_62), inference(avatar_split_clause, [], [f169, f489, f546])).
fof(f169, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f549, plain, (~ spl4_67 | ~ spl4_57), inference(avatar_split_clause, [], [f170, f468, f546])).
fof(f170, plain, (~ (e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f544, plain, (~ spl4_66 | spl4_63), inference(avatar_split_clause, [], [f167, f493, f540])).
fof(f167, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f543, plain, (~ spl4_66 | ~ spl4_53), inference(avatar_split_clause, [], [f168, f451, f540])).
fof(f168, plain, (~ (e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f538, plain, (~ spl4_65 | spl4_64), inference(avatar_split_clause, [], [f165, f497, f534])).
fof(f165, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f537, plain, (~ spl4_65 | ~ spl4_49), inference(avatar_split_clause, [], [f166, f434, f534])).
fof(f434, plain, (spl4_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_49])])).
fof(f166, plain, (~ (e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f532, plain, (spl4_61 | spl4_57 | spl4_53 | spl4_49), inference(avatar_split_clause, [], [f79, f434, f451, f468, f485])).
fof(f79, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax2)).
fof(f529, plain, (spl4_62 | spl4_46 | spl4_30 | spl4_14), inference(avatar_split_clause, [], [f82, f285, f353, f421, f489])).
fof(f82, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f527, plain, (spl4_63 | spl4_47 | spl4_31 | spl4_15), inference(avatar_split_clause, [], [f84, f289, f357, f425, f493])).
fof(f84, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f523, plain, (spl4_57 | spl4_41 | spl4_25 | spl4_9), inference(avatar_split_clause, [], [f88, f264, f332, f400, f468])).
fof(f88, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f522, plain, (spl4_46 | spl4_42 | spl4_38 | spl4_34), inference(avatar_split_clause, [], [f89, f370, f387, f404, f421])).
fof(f89, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f521, plain, (spl4_58 | spl4_42 | spl4_26 | spl4_10), inference(avatar_split_clause, [], [f90, f268, f336, f404, f472])).
fof(f90, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f518, plain, (spl4_48 | spl4_44 | spl4_40 | spl4_36), inference(avatar_split_clause, [], [f93, f378, f395, f412, f429])).
fof(f93, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f513, plain, (spl4_54 | spl4_38 | spl4_22 | spl4_6), inference(avatar_split_clause, [], [f98, f251, f319, f387, f455])).
fof(f98, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f512, plain, (spl4_31 | spl4_27 | spl4_23 | spl4_19), inference(avatar_split_clause, [], [f99, f306, f323, f340, f357])).
fof(f99, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f510, plain, (spl4_32 | spl4_28 | spl4_24 | spl4_20), inference(avatar_split_clause, [], [f101, f310, f327, f344, f361])).
fof(f101, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f505, plain, (spl4_50 | spl4_34 | spl4_18 | spl4_2), inference(avatar_split_clause, [], [f106, f234, f302, f370, f438])).
fof(f106, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f502, plain, (spl4_16 | spl4_12 | spl4_8 | spl4_4), inference(avatar_split_clause, [], [f109, f242, f259, f276, f293])).
fof(f109, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f466, plain, (spl4_53 | spl4_54 | spl4_55 | spl4_56), inference(avatar_split_clause, [], [f65, f463, f459, f455, f451])).
fof(f65, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG142+1.p', ax1)).
fof(f432, plain, (spl4_45 | spl4_46 | spl4_47 | spl4_48), inference(avatar_split_clause, [], [f67, f429, f425, f421, f417])).
fof(f67, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f398, plain, (spl4_37 | spl4_38 | spl4_39 | spl4_40), inference(avatar_split_clause, [], [f69, f395, f391, f387, f383])).
fof(f69, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f381, plain, (spl4_33 | spl4_34 | spl4_35 | spl4_36), inference(avatar_split_clause, [], [f70, f378, f374, f370, f366])).
fof(f70, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f364, plain, (spl4_29 | spl4_30 | spl4_31 | spl4_32), inference(avatar_split_clause, [], [f71, f361, f357, f353, f349])).
fof(f71, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f347, plain, (spl4_25 | spl4_26 | spl4_27 | spl4_28), inference(avatar_split_clause, [], [f72, f344, f340, f336, f332])).
fof(f72, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f313, plain, (spl4_17 | spl4_18 | spl4_19 | spl4_20), inference(avatar_split_clause, [], [f74, f310, f306, f302, f298])).
fof(f74, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f296, plain, (spl4_13 | spl4_14 | spl4_15 | spl4_16), inference(avatar_split_clause, [], [f75, f293, f289, f285, f281])).
fof(f75, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f279, plain, (spl4_9 | spl4_10 | spl4_11 | spl4_12), inference(avatar_split_clause, [], [f76, f276, f272, f268, f264])).
fof(f76, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f262, plain, (spl4_5 | spl4_6 | spl4_7 | spl4_8), inference(avatar_split_clause, [], [f77, f259, f255, f251, f247])).
fof(f77, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).