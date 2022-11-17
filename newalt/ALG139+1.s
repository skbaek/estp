fof(f2034, plain, $false, inference(avatar_sat_refutation, [], [f245, f262, f279, f296, f313, f347, f364, f381, f398, f432, f449, f466, f483, f500, f502, f508, f510, f512, f513, f515, f516, f518, f520, f521, f524, f525, f527, f528, f529, f531, f537, f538, f544, f549, f550, f555, f556, f560, f563, f564, f568, f570, f572, f576, f579, f580, f583, f584, f586, f587, f588, f597, f615, f620, f625, f630, f639, f663, f678, f687, f689, f690, f700, f705, f721, f723, f739, f759, f760, f763, f775, f779, f788, f811, f817, f819, f828, f850, f857, f885, f887, f888, f893, f895, f914, f923, f933, f944, f946, f957, f974, f990, f1029, f1033, f1064, f1079, f1094, f1107, f1109, f1111, f1113, f1140, f1146, f1164, f1180, f1211, f1214, f1215, f1222, f1234, f1257, f1295, f1307, f1311, f1314, f1318, f1321, f1343, f1376, f1383, f1412, f1424, f1432, f1437, f1439, f1444, f1456, f1458, f1462, f1507, f1512, f1518, f1520, f1527, f1547, f1567, f1574, f1575, f1582, f1590, f1594, f1610, f1612, f1638, f1649, f1650, f1652, f1653, f1660, f1664, f1670, f1673, f1679, f1684, f1719, f1753, f1754, f1757, f1758, f1774, f1783, f1798, f1799, f1808, f1809, f1810, f1814, f1826, f1840, f1846, f1847, f1848, f1854, f1858, f1872, f1874, f1899, f1911, f1930, f1941, f1949, f1963, f1965, f1967, f1969, f1979, f1998, f2003])).
fof(f2003, plain, (~ spl4_16 | ~ spl4_48), inference(avatar_split_clause, [], [f2000, f429, f293])).
fof(f293, plain, (spl4_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_16])])).
fof(f429, plain, (spl4_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_48])])).
fof(f2000, plain, (~ (e3 = op(e3, e0)) | ~ spl4_48), inference(backward_demodulation, [], [f115, f431])).
fof(f431, plain, ((e3 = op(e1, e0)) | ~ spl4_48), inference(avatar_component_clause, [], [f429])).
fof(f115, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax3)).
fof(f1998, plain, (~ spl4_36 | ~ spl4_52), inference(avatar_split_clause, [], [f1996, f446, f378])).
fof(f378, plain, (spl4_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_36])])).
fof(f446, plain, (spl4_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_52])])).
fof(f1996, plain, (~ (e3 = op(e1, e3)) | ~ spl4_52), inference(backward_demodulation, [], [f129, f448])).
fof(f448, plain, ((e3 = op(e0, e3)) | ~ spl4_52), inference(avatar_component_clause, [], [f446])).
fof(f129, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1979, plain, (~ spl4_5 | ~ spl4_24 | spl4_69), inference(avatar_split_clause, [], [f1978, f590, f327, f247])).
fof(f247, plain, (spl4_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_5])])).
fof(f327, plain, (spl4_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_24])])).
fof(f590, plain, (spl4_69 <=> (e0 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_69])])).
fof(f1978, plain, (~ (e0 = op(e3, e2)) | (~ spl4_24 | spl4_69)), inference(forward_demodulation, [], [f592, f329])).
fof(f329, plain, ((e3 = op(e2, e2)) | ~ spl4_24), inference(avatar_component_clause, [], [f327])).
fof(f592, plain, (~ (e0 = op(op(e2, e2), e2)) | spl4_69), inference(avatar_component_clause, [], [f590])).
fof(f1969, plain, (~ spl4_35 | ~ spl4_2 | spl4_87), inference(avatar_split_clause, [], [f1955, f675, f234, f374])).
fof(f374, plain, (spl4_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_35])])).
fof(f234, plain, (spl4_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_2])])).
fof(f675, plain, (spl4_87 <=> (e2 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_87])])).
fof(f1955, plain, (~ (e2 = op(e1, e3)) | (~ spl4_2 | spl4_87)), inference(backward_demodulation, [], [f677, f236])).
fof(f236, plain, ((e1 = op(e3, e3)) | ~ spl4_2), inference(avatar_component_clause, [], [f234])).
fof(f677, plain, (~ (e2 = op(op(e3, e3), e3)) | spl4_87), inference(avatar_component_clause, [], [f675])).
fof(f1967, plain, (~ spl4_31 | ~ spl4_63), inference(avatar_split_clause, [], [f1966, f493, f357])).
fof(f357, plain, (spl4_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_31])])).
fof(f493, plain, (spl4_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl4_63])])).
fof(f1966, plain, (~ (e2 = op(e2, e0)) | ~ spl4_63), inference(forward_demodulation, [], [f112, f495])).
fof(f495, plain, ((op(e0, e0) = e2) | ~ spl4_63), inference(avatar_component_clause, [], [f493])).
fof(f112, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1965, plain, (~ spl4_30 | ~ spl4_63 | spl4_82), inference(avatar_split_clause, [], [f1883, f650, f493, f353])).
fof(f353, plain, (spl4_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_30])])).
fof(f650, plain, (spl4_82 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_82])])).
fof(f1883, plain, (~ (e1 = op(e2, e0)) | (~ spl4_63 | spl4_82)), inference(backward_demodulation, [], [f652, f495])).
fof(f652, plain, (~ (e1 = op(op(e0, e0), e0)) | spl4_82), inference(avatar_component_clause, [], [f650])).
fof(f1963, plain, (~ spl4_32 | ~ spl4_63 | spl4_92), inference(avatar_split_clause, [], [f1888, f702, f493, f361])).
fof(f361, plain, (spl4_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_32])])).
fof(f702, plain, (spl4_92 <=> (e3 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_92])])).
fof(f1888, plain, (~ (e3 = op(e2, e0)) | (~ spl4_63 | spl4_92)), inference(backward_demodulation, [], [f704, f495])).
fof(f704, plain, (~ (e3 = op(op(e0, e0), e0)) | spl4_92), inference(avatar_component_clause, [], [f702])).
fof(f1949, plain, (~ spl4_1 | ~ spl4_5), inference(avatar_split_clause, [], [f1945, f247, f230])).
fof(f230, plain, (spl4_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_1])])).
fof(f1945, plain, (~ (e0 = op(e3, e3)) | ~ spl4_5), inference(backward_demodulation, [], [f158, f249])).
fof(f249, plain, ((e0 = op(e3, e2)) | ~ spl4_5), inference(avatar_component_clause, [], [f247])).
fof(f158, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1941, plain, (~ spl4_12 | ~ spl4_16), inference(avatar_split_clause, [], [f1938, f293, f276])).
fof(f276, plain, (spl4_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_12])])).
fof(f1938, plain, (~ (e3 = op(e3, e1)) | ~ spl4_16), inference(backward_demodulation, [], [f153, f295])).
fof(f295, plain, ((e3 = op(e3, e0)) | ~ spl4_16), inference(avatar_component_clause, [], [f293])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1930, plain, (~ spl4_37 | ~ spl4_41), inference(avatar_split_clause, [], [f1920, f400, f383])).
fof(f383, plain, (spl4_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_37])])).
fof(f400, plain, (spl4_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_41])])).
fof(f1920, plain, (~ (e0 = op(e1, e2)) | ~ spl4_41), inference(backward_demodulation, [], [f144, f402])).
fof(f402, plain, ((e0 = op(e1, e1)) | ~ spl4_41), inference(avatar_component_clause, [], [f400])).
fof(f144, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1911, plain, (~ spl4_1 | ~ spl4_49), inference(avatar_split_clause, [], [f1907, f434, f230])).
fof(f434, plain, (spl4_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_49])])).
fof(f1907, plain, (~ (e0 = op(e3, e3)) | ~ spl4_49), inference(backward_demodulation, [], [f131, f436])).
fof(f436, plain, ((e0 = op(e0, e3)) | ~ spl4_49), inference(avatar_component_clause, [], [f434])).
fof(f131, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1899, plain, (~ spl4_52 | ~ spl4_60), inference(avatar_split_clause, [], [f1897, f480, f446])).
fof(f480, plain, (spl4_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_60])])).
fof(f1897, plain, (~ (e3 = op(e0, e3)) | ~ spl4_60), inference(backward_demodulation, [], [f139, f482])).
fof(f482, plain, ((e3 = op(e0, e1)) | ~ spl4_60), inference(avatar_component_clause, [], [f480])).
fof(f139, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1874, plain, (~ spl4_2 | ~ spl4_24 | spl4_70), inference(avatar_split_clause, [], [f1873, f594, f327, f234])).
fof(f594, plain, (spl4_70 <=> (e1 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_70])])).
fof(f1873, plain, (~ (e1 = op(e3, e3)) | (~ spl4_24 | spl4_70)), inference(forward_demodulation, [], [f596, f329])).
fof(f596, plain, (~ (e1 = op(op(e2, e2), op(e2, e2))) | spl4_70), inference(avatar_component_clause, [], [f594])).
fof(f1872, plain, (~ spl4_1 | ~ spl4_24 | spl4_79), inference(avatar_split_clause, [], [f1871, f636, f327, f230])).
fof(f636, plain, (spl4_79 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_79])])).
fof(f1871, plain, (~ (e0 = op(e3, e3)) | (~ spl4_24 | spl4_79)), inference(forward_demodulation, [], [f638, f329])).
fof(f638, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl4_79), inference(avatar_component_clause, [], [f636])).
fof(f1858, plain, (~ spl4_2 | ~ spl4_6), inference(avatar_split_clause, [], [f1855, f251, f234])).
fof(f251, plain, (spl4_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_6])])).
fof(f1855, plain, (~ (e1 = op(e3, e3)) | ~ spl4_6), inference(backward_demodulation, [], [f158, f253])).
fof(f253, plain, ((e1 = op(e3, e2)) | ~ spl4_6), inference(avatar_component_clause, [], [f251])).
fof(f1854, plain, (~ spl4_7 | ~ spl4_15), inference(avatar_split_clause, [], [f1851, f289, f255])).
fof(f255, plain, (spl4_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_7])])).
fof(f289, plain, (spl4_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_15])])).
fof(f1851, plain, (~ (e2 = op(e3, e2)) | ~ spl4_15), inference(backward_demodulation, [], [f154, f291])).
fof(f291, plain, ((e2 = op(e3, e0)) | ~ spl4_15), inference(avatar_component_clause, [], [f289])).
fof(f154, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1848, plain, (spl4_1 | ~ spl4_24 | ~ spl4_79), inference(avatar_split_clause, [], [f1845, f636, f327, f230])).
fof(f1845, plain, ((e0 = op(e3, e3)) | (~ spl4_24 | ~ spl4_79)), inference(backward_demodulation, [], [f637, f329])).
fof(f637, plain, ((e0 = op(op(e2, e2), op(e2, e2))) | ~ spl4_79), inference(avatar_component_clause, [], [f636])).
fof(f1847, plain, (~ spl4_6 | ~ spl4_24 | spl4_78), inference(avatar_split_clause, [], [f1844, f632, f327, f251])).
fof(f632, plain, (spl4_78 <=> (e1 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_78])])).
fof(f1844, plain, (~ (e1 = op(e3, e2)) | (~ spl4_24 | spl4_78)), inference(backward_demodulation, [], [f634, f329])).
fof(f634, plain, (~ (e1 = op(op(e2, e2), e2)) | spl4_78), inference(avatar_component_clause, [], [f632])).
fof(f1846, plain, (~ spl4_20 | ~ spl4_24), inference(avatar_split_clause, [], [f1842, f327, f310])).
fof(f310, plain, (spl4_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_20])])).
fof(f1842, plain, (~ (e3 = op(e2, e3)) | ~ spl4_24), inference(backward_demodulation, [], [f152, f329])).
fof(f152, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1840, plain, (~ spl4_21 | ~ spl4_29), inference(avatar_split_clause, [], [f1834, f349, f315])).
fof(f315, plain, (spl4_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_21])])).
fof(f349, plain, (spl4_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_29])])).
fof(f1834, plain, (~ (e0 = op(e2, e2)) | ~ spl4_29), inference(backward_demodulation, [], [f148, f351])).
fof(f351, plain, ((e0 = op(e2, e0)) | ~ spl4_29), inference(avatar_component_clause, [], [f349])).
fof(f148, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1826, plain, (~ spl4_21 | ~ spl4_37), inference(avatar_split_clause, [], [f1820, f383, f315])).
fof(f1820, plain, (~ (e0 = op(e2, e2)) | ~ spl4_37), inference(backward_demodulation, [], [f126, f385])).
fof(f385, plain, ((e0 = op(e1, e2)) | ~ spl4_37), inference(avatar_component_clause, [], [f383])).
fof(f126, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1814, plain, (~ spl4_20 | ~ spl4_52), inference(avatar_split_clause, [], [f1812, f446, f310])).
fof(f1812, plain, (~ (e3 = op(e2, e3)) | ~ spl4_52), inference(backward_demodulation, [], [f130, f448])).
fof(f130, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1810, plain, (~ spl4_51 | ~ spl4_55), inference(avatar_split_clause, [], [f1806, f459, f442])).
fof(f442, plain, (spl4_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_51])])).
fof(f459, plain, (spl4_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_55])])).
fof(f1806, plain, (~ (e2 = op(e0, e3)) | ~ spl4_55), inference(backward_demodulation, [], [f140, f461])).
fof(f461, plain, ((e2 = op(e0, e2)) | ~ spl4_55), inference(avatar_component_clause, [], [f459])).
fof(f140, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1809, plain, (~ spl4_7 | ~ spl4_55), inference(avatar_split_clause, [], [f1805, f459, f255])).
fof(f1805, plain, (~ (e2 = op(e3, e2)) | ~ spl4_55), inference(backward_demodulation, [], [f125, f461])).
fof(f125, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1808, plain, (~ spl4_23 | ~ spl4_55), inference(avatar_split_clause, [], [f1804, f459, f323])).
fof(f323, plain, (spl4_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_23])])).
fof(f1804, plain, (~ (e2 = op(e2, e2)) | ~ spl4_55), inference(backward_demodulation, [], [f124, f461])).
fof(f124, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1799, plain, (~ spl4_54 | ~ spl4_62), inference(avatar_split_clause, [], [f1790, f489, f455])).
fof(f455, plain, (spl4_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_54])])).
fof(f489, plain, (spl4_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl4_62])])).
fof(f1790, plain, (~ (e1 = op(e0, e2)) | ~ spl4_62), inference(backward_demodulation, [], [f136, f491])).
fof(f491, plain, ((op(e0, e0) = e1) | ~ spl4_62), inference(avatar_component_clause, [], [f489])).
fof(f136, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f1798, plain, (~ spl4_46 | ~ spl4_62), inference(avatar_split_clause, [], [f1787, f489, f421])).
fof(f421, plain, (spl4_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_46])])).
fof(f1787, plain, (~ (e1 = op(e1, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f111, f491])).
fof(f111, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f1783, plain, (~ spl4_47 | ~ spl4_43), inference(avatar_split_clause, [], [f1726, f408, f425])).
fof(f425, plain, (spl4_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_47])])).
fof(f408, plain, (spl4_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_43])])).
fof(f1726, plain, (~ (e2 = op(e1, e0)) | ~ spl4_43), inference(forward_demodulation, [], [f141, f410])).
fof(f410, plain, ((e2 = op(e1, e1)) | ~ spl4_43), inference(avatar_component_clause, [], [f408])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1774, plain, (~ spl4_2 | ~ spl4_43 | spl4_75), inference(avatar_contradiction_clause, [], [f1773])).
fof(f1773, plain, ($false | (~ spl4_2 | ~ spl4_43 | spl4_75)), inference(subsumption_resolution, [], [f1772, f410])).
fof(f1772, plain, (~ (e2 = op(e1, e1)) | (~ spl4_2 | spl4_75)), inference(backward_demodulation, [], [f619, f236])).
fof(f619, plain, (~ (e2 = op(op(e3, e3), op(e3, e3))) | spl4_75), inference(avatar_component_clause, [], [f617])).
fof(f617, plain, (spl4_75 <=> (e2 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_75])])).
fof(f1758, plain, (~ spl4_2 | ~ spl4_64 | spl4_89), inference(avatar_split_clause, [], [f1752, f684, f497, f234])).
fof(f497, plain, (spl4_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl4_64])])).
fof(f684, plain, (spl4_89 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_89])])).
fof(f1752, plain, (~ (e1 = op(e3, e3)) | (~ spl4_64 | spl4_89)), inference(backward_demodulation, [], [f686, f499])).
fof(f499, plain, ((op(e0, e0) = e3) | ~ spl4_64), inference(avatar_component_clause, [], [f497])).
fof(f686, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl4_89), inference(avatar_component_clause, [], [f684])).
fof(f1757, plain, (~ spl4_13 | ~ spl4_64 | ~ spl4_88), inference(avatar_contradiction_clause, [], [f1756])).
fof(f1756, plain, ($false | (~ spl4_13 | ~ spl4_64 | ~ spl4_88)), inference(subsumption_resolution, [], [f1755, f160])).
fof(f160, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax4)).
fof(f1755, plain, ((e0 = e2) | (~ spl4_13 | ~ spl4_64 | ~ spl4_88)), inference(forward_demodulation, [], [f1751, f283])).
fof(f283, plain, ((e0 = op(e3, e0)) | ~ spl4_13), inference(avatar_component_clause, [], [f281])).
fof(f281, plain, (spl4_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_13])])).
fof(f1751, plain, ((e2 = op(e3, e0)) | (~ spl4_64 | ~ spl4_88)), inference(backward_demodulation, [], [f681, f499])).
fof(f681, plain, ((e2 = op(op(e0, e0), e0)) | ~ spl4_88), inference(avatar_component_clause, [], [f680])).
fof(f680, plain, (spl4_88 <=> (e2 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_88])])).
fof(f1754, plain, (~ spl4_4 | ~ spl4_64 | spl4_84), inference(avatar_split_clause, [], [f1750, f660, f497, f242])).
fof(f242, plain, (spl4_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_4])])).
fof(f660, plain, (spl4_84 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_84])])).
fof(f1750, plain, (~ (e3 = op(e3, e3)) | (~ spl4_64 | spl4_84)), inference(backward_demodulation, [], [f662, f499])).
fof(f662, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl4_84), inference(avatar_component_clause, [], [f660])).
fof(f1753, plain, (~ spl4_56 | ~ spl4_64), inference(avatar_split_clause, [], [f1748, f497, f463])).
fof(f463, plain, (spl4_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_56])])).
fof(f1748, plain, (~ (e3 = op(e0, e2)) | ~ spl4_64), inference(backward_demodulation, [], [f136, f499])).
fof(f1719, plain, (~ spl4_39 | ~ spl4_7), inference(avatar_split_clause, [], [f1718, f255, f391])).
fof(f391, plain, (spl4_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_39])])).
fof(f1718, plain, (~ (e2 = op(e1, e2)) | ~ spl4_7), inference(forward_demodulation, [], [f127, f257])).
fof(f257, plain, ((e2 = op(e3, e2)) | ~ spl4_7), inference(avatar_component_clause, [], [f255])).
fof(f127, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1684, plain, (~ spl4_18 | ~ spl4_26), inference(avatar_split_clause, [], [f1681, f336, f302])).
fof(f302, plain, (spl4_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_18])])).
fof(f336, plain, (spl4_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_26])])).
fof(f1681, plain, (~ (e1 = op(e2, e3)) | ~ spl4_26), inference(backward_demodulation, [], [f151, f338])).
fof(f338, plain, ((e1 = op(e2, e1)) | ~ spl4_26), inference(avatar_component_clause, [], [f336])).
fof(f151, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1679, plain, (~ spl4_31 | ~ spl4_32), inference(avatar_contradiction_clause, [], [f1678])).
fof(f1678, plain, ($false | (~ spl4_31 | ~ spl4_32)), inference(subsumption_resolution, [], [f1677, f164])).
fof(f164, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f1677, plain, ((e2 = e3) | (~ spl4_31 | ~ spl4_32)), inference(backward_demodulation, [], [f363, f359])).
fof(f359, plain, ((e2 = op(e2, e0)) | ~ spl4_31), inference(avatar_component_clause, [], [f357])).
fof(f363, plain, ((e3 = op(e2, e0)) | ~ spl4_32), inference(avatar_component_clause, [], [f361])).
fof(f1673, plain, (~ spl4_43 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1672])).
fof(f1672, plain, ($false | (~ spl4_43 | ~ spl4_44)), inference(subsumption_resolution, [], [f1671, f164])).
fof(f1671, plain, ((e2 = e3) | (~ spl4_43 | ~ spl4_44)), inference(backward_demodulation, [], [f414, f410])).
fof(f414, plain, ((e3 = op(e1, e1)) | ~ spl4_44), inference(avatar_component_clause, [], [f412])).
fof(f412, plain, (spl4_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_44])])).
fof(f1670, plain, (~ spl4_38 | ~ spl4_46), inference(avatar_split_clause, [], [f1666, f421, f387])).
fof(f387, plain, (spl4_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_38])])).
fof(f1666, plain, (~ (e1 = op(e1, e2)) | ~ spl4_46), inference(backward_demodulation, [], [f142, f423])).
fof(f423, plain, ((e1 = op(e1, e0)) | ~ spl4_46), inference(avatar_component_clause, [], [f421])).
fof(f142, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1664, plain, (~ spl4_3 | ~ spl4_51), inference(avatar_split_clause, [], [f1662, f442, f238])).
fof(f238, plain, (spl4_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_3])])).
fof(f1662, plain, (~ (e2 = op(e3, e3)) | ~ spl4_51), inference(backward_demodulation, [], [f131, f444])).
fof(f444, plain, ((e2 = op(e0, e3)) | ~ spl4_51), inference(avatar_component_clause, [], [f442])).
fof(f1660, plain, (~ spl4_9 | ~ spl4_57), inference(avatar_split_clause, [], [f1655, f468, f264])).
fof(f264, plain, (spl4_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_9])])).
fof(f468, plain, (spl4_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_57])])).
fof(f1655, plain, (~ (e0 = op(e3, e1)) | ~ spl4_57), inference(backward_demodulation, [], [f119, f470])).
fof(f470, plain, ((e0 = op(e0, e1)) | ~ spl4_57), inference(avatar_component_clause, [], [f468])).
fof(f119, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f1653, plain, (~ spl4_47 | ~ spl4_62 | spl4_88), inference(avatar_split_clause, [], [f1646, f680, f489, f425])).
fof(f1646, plain, (~ (e2 = op(e1, e0)) | (~ spl4_62 | spl4_88)), inference(backward_demodulation, [], [f682, f491])).
fof(f682, plain, (~ (e2 = op(op(e0, e0), e0)) | spl4_88), inference(avatar_component_clause, [], [f680])).
fof(f1652, plain, (~ spl4_44 | ~ spl4_62 | spl4_84), inference(avatar_contradiction_clause, [], [f1651])).
fof(f1651, plain, ($false | (~ spl4_44 | ~ spl4_62 | spl4_84)), inference(subsumption_resolution, [], [f1645, f414])).
fof(f1645, plain, (~ (e3 = op(e1, e1)) | (~ spl4_62 | spl4_84)), inference(backward_demodulation, [], [f662, f491])).
fof(f1650, plain, (~ spl4_50 | ~ spl4_62), inference(avatar_split_clause, [], [f1642, f489, f438])).
fof(f438, plain, (spl4_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_50])])).
fof(f1642, plain, (~ (e1 = op(e0, e3)) | ~ spl4_62), inference(backward_demodulation, [], [f137, f491])).
fof(f137, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f1649, plain, (~ spl4_14 | ~ spl4_62), inference(avatar_split_clause, [], [f1640, f489, f285])).
fof(f285, plain, (spl4_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_14])])).
fof(f1640, plain, (~ (e1 = op(e3, e0)) | ~ spl4_62), inference(backward_demodulation, [], [f113, f491])).
fof(f113, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f1638, plain, (~ spl4_3 | ~ spl4_44 | spl4_74), inference(avatar_split_clause, [], [f1637, f612, f412, f238])).
fof(f612, plain, (spl4_74 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl4_74])])).
fof(f1637, plain, (~ (e2 = op(e3, e3)) | (~ spl4_44 | spl4_74)), inference(forward_demodulation, [], [f614, f414])).
fof(f614, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl4_74), inference(avatar_component_clause, [], [f612])).
fof(f1612, plain, (~ spl4_5 | ~ spl4_21), inference(avatar_split_clause, [], [f1611, f315, f247])).
fof(f1611, plain, (~ (e0 = op(e3, e2)) | ~ spl4_21), inference(forward_demodulation, [], [f128, f317])).
fof(f317, plain, ((e0 = op(e2, e2)) | ~ spl4_21), inference(avatar_component_clause, [], [f315])).
fof(f128, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1610, plain, (~ spl4_61 | ~ spl4_21 | spl4_79), inference(avatar_split_clause, [], [f1609, f636, f315, f485])).
fof(f485, plain, (spl4_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_61])])).
fof(f1609, plain, (~ (e0 = op(e0, e0)) | (~ spl4_21 | spl4_79)), inference(forward_demodulation, [], [f638, f317])).
fof(f1594, plain, (~ spl4_21 | ~ spl4_22), inference(avatar_contradiction_clause, [], [f1593])).
fof(f1593, plain, ($false | (~ spl4_21 | ~ spl4_22)), inference(subsumption_resolution, [], [f1592, f159])).
fof(f159, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f1592, plain, ((e0 = e1) | (~ spl4_21 | ~ spl4_22)), inference(backward_demodulation, [], [f321, f317])).
fof(f321, plain, ((e1 = op(e2, e2)) | ~ spl4_22), inference(avatar_component_clause, [], [f319])).
fof(f319, plain, (spl4_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_22])])).
fof(f1590, plain, (~ spl4_20 | ~ spl4_32), inference(avatar_split_clause, [], [f1588, f361, f310])).
fof(f1588, plain, (~ (e3 = op(e2, e3)) | ~ spl4_32), inference(backward_demodulation, [], [f149, f363])).
fof(f149, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f1582, plain, (~ spl4_42 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1581])).
fof(f1581, plain, ($false | (~ spl4_42 | ~ spl4_44)), inference(subsumption_resolution, [], [f1580, f163])).
fof(f163, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f1580, plain, ((e1 = e3) | (~ spl4_42 | ~ spl4_44)), inference(backward_demodulation, [], [f414, f406])).
fof(f406, plain, ((e1 = op(e1, e1)) | ~ spl4_42), inference(avatar_component_clause, [], [f404])).
fof(f404, plain, (spl4_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_42])])).
fof(f1575, plain, (~ spl4_9 | ~ spl4_44 | spl4_73), inference(avatar_split_clause, [], [f1570, f608, f412, f264])).
fof(f608, plain, (spl4_73 <=> (e0 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_73])])).
fof(f1570, plain, (~ (e0 = op(e3, e1)) | (~ spl4_44 | spl4_73)), inference(backward_demodulation, [], [f610, f414])).
fof(f610, plain, (~ (e0 = op(op(e1, e1), e1)) | spl4_73), inference(avatar_component_clause, [], [f608])).
fof(f1574, plain, (~ spl4_36 | ~ spl4_44), inference(avatar_split_clause, [], [f1569, f412, f378])).
fof(f1569, plain, (~ (e3 = op(e1, e3)) | ~ spl4_44), inference(backward_demodulation, [], [f145, f414])).
fof(f145, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f1567, plain, (~ spl4_31 | ~ spl4_47), inference(avatar_split_clause, [], [f1562, f425, f357])).
fof(f1562, plain, (~ (e2 = op(e2, e0)) | ~ spl4_47), inference(backward_demodulation, [], [f114, f427])).
fof(f427, plain, ((e2 = op(e1, e0)) | ~ spl4_47), inference(avatar_component_clause, [], [f425])).
fof(f114, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f1547, plain, (~ spl4_53 | ~ spl4_61), inference(avatar_split_clause, [], [f1538, f485, f451])).
fof(f451, plain, (spl4_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_53])])).
fof(f1538, plain, (~ (e0 = op(e0, e2)) | ~ spl4_61), inference(backward_demodulation, [], [f136, f487])).
fof(f487, plain, ((e0 = op(e0, e0)) | ~ spl4_61), inference(avatar_component_clause, [], [f485])).
fof(f1527, plain, (~ spl4_41 | ~ spl4_22 | spl4_79), inference(avatar_split_clause, [], [f1526, f636, f319, f400])).
fof(f1526, plain, (~ (e0 = op(e1, e1)) | (~ spl4_22 | spl4_79)), inference(forward_demodulation, [], [f638, f321])).
fof(f1520, plain, (~ spl4_43 | ~ spl4_27), inference(avatar_split_clause, [], [f1519, f340, f408])).
fof(f340, plain, (spl4_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_27])])).
fof(f1519, plain, (~ (e2 = op(e1, e1)) | ~ spl4_27), inference(forward_demodulation, [], [f120, f342])).
fof(f342, plain, ((e2 = op(e2, e1)) | ~ spl4_27), inference(avatar_component_clause, [], [f340])).
fof(f120, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1518, plain, (~ spl4_59 | ~ spl4_27), inference(avatar_split_clause, [], [f1517, f340, f476])).
fof(f476, plain, (spl4_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_59])])).
fof(f1517, plain, (~ (e2 = op(e0, e1)) | ~ spl4_27), inference(forward_demodulation, [], [f118, f342])).
fof(f118, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1512, plain, (spl4_21 | ~ spl4_3 | ~ spl4_81), inference(avatar_split_clause, [], [f1473, f645, f238, f315])).
fof(f645, plain, (spl4_81 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl4_81])])).
fof(f1473, plain, ((e0 = op(e2, e2)) | (~ spl4_3 | ~ spl4_81)), inference(forward_demodulation, [], [f646, f240])).
fof(f240, plain, ((e2 = op(e3, e3)) | ~ spl4_3), inference(avatar_component_clause, [], [f238])).
fof(f646, plain, ((e0 = op(op(e3, e3), op(e3, e3))) | ~ spl4_81), inference(avatar_component_clause, [], [f645])).
fof(f1507, plain, (~ spl4_2 | ~ spl4_3), inference(avatar_contradiction_clause, [], [f1506])).
fof(f1506, plain, ($false | (~ spl4_2 | ~ spl4_3)), inference(subsumption_resolution, [], [f1505, f162])).
fof(f162, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1505, plain, ((e1 = e2) | (~ spl4_2 | ~ spl4_3)), inference(backward_demodulation, [], [f240, f236])).
fof(f1462, plain, (~ spl4_19 | ~ spl4_51), inference(avatar_split_clause, [], [f1461, f442, f306])).
fof(f306, plain, (spl4_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_19])])).
fof(f1461, plain, (~ (e2 = op(e2, e3)) | ~ spl4_51), inference(backward_demodulation, [], [f130, f444])).
fof(f1458, plain, (~ spl4_48 | ~ spl4_64), inference(avatar_split_clause, [], [f1457, f497, f429])).
fof(f1457, plain, (~ (e3 = op(e1, e0)) | ~ spl4_64), inference(forward_demodulation, [], [f111, f499])).
fof(f1456, plain, (~ spl4_35 | ~ spl4_3), inference(avatar_split_clause, [], [f1455, f238, f374])).
fof(f1455, plain, (~ (e2 = op(e1, e3)) | ~ spl4_3), inference(forward_demodulation, [], [f133, f240])).
fof(f133, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1444, plain, (~ spl4_19 | ~ spl4_3), inference(avatar_split_clause, [], [f1443, f238, f306])).
fof(f1443, plain, (~ (e2 = op(e2, e3)) | ~ spl4_3), inference(forward_demodulation, [], [f134, f240])).
fof(f134, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1439, plain, (~ spl4_16 | ~ spl4_64), inference(avatar_split_clause, [], [f1438, f497, f293])).
fof(f1438, plain, (~ (e3 = op(e3, e0)) | ~ spl4_64), inference(forward_demodulation, [], [f113, f499])).
fof(f1437, plain, (~ spl4_15 | ~ spl4_64 | spl4_88), inference(avatar_split_clause, [], [f1400, f680, f497, f289])).
fof(f1400, plain, (~ (e2 = op(e3, e0)) | (~ spl4_64 | spl4_88)), inference(forward_demodulation, [], [f682, f499])).
fof(f1432, plain, (~ spl4_11 | ~ spl4_3), inference(avatar_split_clause, [], [f1431, f238, f272])).
fof(f272, plain, (spl4_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_11])])).
fof(f1431, plain, (~ (e2 = op(e3, e1)) | ~ spl4_3), inference(forward_demodulation, [], [f157, f240])).
fof(f157, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1424, plain, (~ spl4_3 | ~ spl4_22 | ~ spl4_75), inference(avatar_contradiction_clause, [], [f1423])).
fof(f1423, plain, ($false | (~ spl4_3 | ~ spl4_22 | ~ spl4_75)), inference(subsumption_resolution, [], [f1422, f162])).
fof(f1422, plain, ((e1 = e2) | (~ spl4_3 | ~ spl4_22 | ~ spl4_75)), inference(forward_demodulation, [], [f1421, f321])).
fof(f1421, plain, ((e2 = op(e2, e2)) | (~ spl4_3 | ~ spl4_75)), inference(forward_demodulation, [], [f618, f240])).
fof(f618, plain, ((e2 = op(op(e3, e3), op(e3, e3))) | ~ spl4_75), inference(avatar_component_clause, [], [f617])).
fof(f1412, plain, (~ spl4_29 | ~ spl4_31), inference(avatar_contradiction_clause, [], [f1411])).
fof(f1411, plain, ($false | (~ spl4_29 | ~ spl4_31)), inference(subsumption_resolution, [], [f1410, f160])).
fof(f1410, plain, ((e0 = e2) | (~ spl4_29 | ~ spl4_31)), inference(forward_demodulation, [], [f359, f351])).
fof(f1383, plain, (~ spl4_64 | ~ spl4_41 | spl4_76), inference(avatar_split_clause, [], [f1351, f622, f400, f497])).
fof(f622, plain, (spl4_76 <=> (e3 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl4_76])])).
fof(f1351, plain, (~ (op(e0, e0) = e3) | (~ spl4_41 | spl4_76)), inference(backward_demodulation, [], [f624, f402])).
fof(f624, plain, (~ (e3 = op(op(e1, e1), op(e1, e1))) | spl4_76), inference(avatar_component_clause, [], [f622])).
fof(f1376, plain, (~ spl4_1 | ~ spl4_4 | spl4_81), inference(avatar_split_clause, [], [f1371, f645, f242, f230])).
fof(f1371, plain, (~ (e0 = op(e3, e3)) | (~ spl4_4 | spl4_81)), inference(backward_demodulation, [], [f647, f244])).
fof(f244, plain, ((e3 = op(e3, e3)) | ~ spl4_4), inference(avatar_component_clause, [], [f242])).
fof(f647, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl4_81), inference(avatar_component_clause, [], [f645])).
fof(f1343, plain, (~ spl4_44 | ~ spl4_48), inference(avatar_split_clause, [], [f1340, f429, f412])).
fof(f1340, plain, (~ (e3 = op(e1, e1)) | ~ spl4_48), inference(backward_demodulation, [], [f141, f431])).
fof(f1321, plain, (~ spl4_63 | ~ spl4_64), inference(avatar_contradiction_clause, [], [f1320])).
fof(f1320, plain, ($false | (~ spl4_63 | ~ spl4_64)), inference(subsumption_resolution, [], [f1319, f164])).
fof(f1319, plain, ((e2 = e3) | (~ spl4_63 | ~ spl4_64)), inference(forward_demodulation, [], [f499, f495])).
fof(f1318, plain, (spl4_41 | ~ spl4_22 | ~ spl4_79), inference(avatar_split_clause, [], [f1317, f636, f319, f400])).
fof(f1317, plain, ((e0 = op(e1, e1)) | (~ spl4_22 | ~ spl4_79)), inference(forward_demodulation, [], [f637, f321])).
fof(f1314, plain, (~ spl4_40 | ~ spl4_22 | spl4_91), inference(avatar_split_clause, [], [f1313, f697, f319, f395])).
fof(f395, plain, (spl4_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_40])])).
fof(f697, plain, (spl4_91 <=> (e3 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_91])])).
fof(f1313, plain, (~ (e3 = op(e1, e2)) | (~ spl4_22 | spl4_91)), inference(forward_demodulation, [], [f699, f321])).
fof(f699, plain, (~ (e3 = op(op(e2, e2), e2)) | spl4_91), inference(avatar_component_clause, [], [f697])).
fof(f1311, plain, (~ spl4_60 | ~ spl4_28), inference(avatar_split_clause, [], [f1310, f344, f480])).
fof(f344, plain, (spl4_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_28])])).
fof(f1310, plain, (~ (e3 = op(e0, e1)) | ~ spl4_28), inference(forward_demodulation, [], [f118, f346])).
fof(f346, plain, ((e3 = op(e2, e1)) | ~ spl4_28), inference(avatar_component_clause, [], [f344])).
fof(f1307, plain, (~ spl4_44 | ~ spl4_28), inference(avatar_split_clause, [], [f1306, f344, f412])).
fof(f1306, plain, (~ (e3 = op(e1, e1)) | ~ spl4_28), inference(forward_demodulation, [], [f120, f346])).
fof(f1295, plain, (~ spl4_5 | ~ spl4_6), inference(avatar_contradiction_clause, [], [f1294])).
fof(f1294, plain, ($false | (~ spl4_5 | ~ spl4_6)), inference(subsumption_resolution, [], [f1293, f159])).
fof(f1293, plain, ((e0 = e1) | (~ spl4_5 | ~ spl4_6)), inference(backward_demodulation, [], [f253, f249])).
fof(f1257, plain, (~ spl4_62 | ~ spl4_63), inference(avatar_contradiction_clause, [], [f1256])).
fof(f1256, plain, ($false | (~ spl4_62 | ~ spl4_63)), inference(subsumption_resolution, [], [f1255, f162])).
fof(f1255, plain, ((e1 = e2) | (~ spl4_62 | ~ spl4_63)), inference(backward_demodulation, [], [f495, f491])).
fof(f1234, plain, (~ spl4_22 | ~ spl4_44 | spl4_77), inference(avatar_contradiction_clause, [], [f1233])).
fof(f1233, plain, ($false | (~ spl4_22 | ~ spl4_44 | spl4_77)), inference(subsumption_resolution, [], [f1100, f414])).
fof(f1100, plain, (~ (e3 = op(e1, e1)) | (~ spl4_22 | spl4_77)), inference(forward_demodulation, [], [f629, f321])).
fof(f629, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl4_77), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl4_77 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl4_77])])).
fof(f1222, plain, (~ spl4_33 | ~ spl4_1), inference(avatar_split_clause, [], [f1221, f230, f366])).
fof(f366, plain, (spl4_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_33])])).
fof(f1221, plain, (~ (e0 = op(e1, e3)) | ~ spl4_1), inference(forward_demodulation, [], [f133, f232])).
fof(f232, plain, ((e0 = op(e3, e3)) | ~ spl4_1), inference(avatar_component_clause, [], [f230])).
fof(f1215, plain, (~ spl4_18 | ~ spl4_22), inference(avatar_split_clause, [], [f1114, f319, f302])).
fof(f1114, plain, (~ (e1 = op(e2, e3)) | ~ spl4_22), inference(forward_demodulation, [], [f152, f321])).
fof(f1214, plain, (~ spl4_13 | ~ spl4_1), inference(avatar_split_clause, [], [f1084, f230, f281])).
fof(f1084, plain, (~ (e0 = op(e3, e0)) | ~ spl4_1), inference(forward_demodulation, [], [f155, f232])).
fof(f155, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1211, plain, (~ spl4_10 | ~ spl4_6), inference(avatar_split_clause, [], [f1210, f251, f268])).
fof(f268, plain, (spl4_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_10])])).
fof(f1210, plain, (~ (e1 = op(e3, e1)) | ~ spl4_6), inference(forward_demodulation, [], [f156, f253])).
fof(f156, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1180, plain, (~ spl4_17 | ~ spl4_25), inference(avatar_split_clause, [], [f1179, f332, f298])).
fof(f298, plain, (spl4_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_17])])).
fof(f332, plain, (spl4_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_25])])).
fof(f1179, plain, (~ (e0 = op(e2, e3)) | ~ spl4_25), inference(backward_demodulation, [], [f151, f334])).
fof(f334, plain, ((e0 = op(e2, e1)) | ~ spl4_25), inference(avatar_component_clause, [], [f332])).
fof(f1164, plain, (~ spl4_41 | ~ spl4_44), inference(avatar_contradiction_clause, [], [f1163])).
fof(f1163, plain, ($false | (~ spl4_41 | ~ spl4_44)), inference(subsumption_resolution, [], [f1162, f161])).
fof(f161, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f1162, plain, ((e0 = e3) | (~ spl4_41 | ~ spl4_44)), inference(forward_demodulation, [], [f414, f402])).
fof(f1146, plain, (~ spl4_22 | ~ spl4_63 | spl4_89), inference(avatar_contradiction_clause, [], [f1145])).
fof(f1145, plain, ($false | (~ spl4_22 | ~ spl4_63 | spl4_89)), inference(subsumption_resolution, [], [f1144, f321])).
fof(f1144, plain, (~ (e1 = op(e2, e2)) | (~ spl4_63 | spl4_89)), inference(forward_demodulation, [], [f686, f495])).
fof(f1140, plain, (~ spl4_59 | ~ spl4_41 | spl4_85), inference(avatar_split_clause, [], [f1045, f666, f400, f476])).
fof(f666, plain, (spl4_85 <=> (e2 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_85])])).
fof(f1045, plain, (~ (e2 = op(e0, e1)) | (~ spl4_41 | spl4_85)), inference(backward_demodulation, [], [f668, f402])).
fof(f668, plain, (~ (e2 = op(op(e1, e1), e1)) | spl4_85), inference(avatar_component_clause, [], [f666])).
fof(f1113, plain, (~ spl4_37 | ~ spl4_22 | spl4_69), inference(avatar_split_clause, [], [f1112, f590, f319, f383])).
fof(f1112, plain, (~ (e0 = op(e1, e2)) | (~ spl4_22 | spl4_69)), inference(forward_demodulation, [], [f592, f321])).
fof(f1111, plain, (~ spl4_26 | ~ spl4_22), inference(avatar_split_clause, [], [f1110, f319, f336])).
fof(f1110, plain, (~ (e1 = op(e2, e1)) | ~ spl4_22), inference(forward_demodulation, [], [f150, f321])).
fof(f150, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1109, plain, (~ spl4_30 | ~ spl4_22), inference(avatar_split_clause, [], [f1108, f319, f353])).
fof(f1108, plain, (~ (e1 = op(e2, e0)) | ~ spl4_22), inference(forward_demodulation, [], [f148, f321])).
fof(f1107, plain, (~ spl4_38 | ~ spl4_22), inference(avatar_split_clause, [], [f1106, f319, f387])).
fof(f1106, plain, (~ (e1 = op(e1, e2)) | ~ spl4_22), inference(forward_demodulation, [], [f126, f321])).
fof(f1094, plain, (~ spl4_17 | ~ spl4_1), inference(avatar_split_clause, [], [f1093, f230, f298])).
fof(f1093, plain, (~ (e0 = op(e2, e3)) | ~ spl4_1), inference(forward_demodulation, [], [f134, f232])).
fof(f1079, plain, (~ spl4_1 | ~ spl4_63 | spl4_75), inference(avatar_contradiction_clause, [], [f1078])).
fof(f1078, plain, ($false | (~ spl4_1 | ~ spl4_63 | spl4_75)), inference(subsumption_resolution, [], [f1073, f495])).
fof(f1073, plain, (~ (op(e0, e0) = e2) | (~ spl4_1 | spl4_75)), inference(backward_demodulation, [], [f619, f232])).
fof(f1064, plain, (~ spl4_22 | ~ spl4_24), inference(avatar_contradiction_clause, [], [f1063])).
fof(f1063, plain, ($false | (~ spl4_22 | ~ spl4_24)), inference(subsumption_resolution, [], [f1062, f163])).
fof(f1062, plain, ((e1 = e3) | (~ spl4_22 | ~ spl4_24)), inference(backward_demodulation, [], [f329, f321])).
fof(f1033, plain, (~ spl4_43 | ~ spl4_59), inference(avatar_split_clause, [], [f1032, f476, f408])).
fof(f1032, plain, (~ (e2 = op(e1, e1)) | ~ spl4_59), inference(backward_demodulation, [], [f117, f478])).
fof(f478, plain, ((e2 = op(e0, e1)) | ~ spl4_59), inference(avatar_component_clause, [], [f476])).
fof(f117, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f1029, plain, (~ spl4_24 | ~ spl4_63 | spl4_84), inference(avatar_contradiction_clause, [], [f1028])).
fof(f1028, plain, ($false | (~ spl4_24 | ~ spl4_63 | spl4_84)), inference(subsumption_resolution, [], [f1023, f329])).
fof(f1023, plain, (~ (e3 = op(e2, e2)) | (~ spl4_63 | spl4_84)), inference(backward_demodulation, [], [f662, f495])).
fof(f990, plain, (~ spl4_63 | ~ spl4_47), inference(avatar_split_clause, [], [f989, f425, f493])).
fof(f989, plain, (~ (op(e0, e0) = e2) | ~ spl4_47), inference(forward_demodulation, [], [f111, f427])).
fof(f974, plain, (~ spl4_2 | ~ spl4_14), inference(avatar_split_clause, [], [f973, f285, f234])).
fof(f973, plain, (~ (e1 = op(e3, e3)) | ~ spl4_14), inference(forward_demodulation, [], [f155, f287])).
fof(f287, plain, ((e1 = op(e3, e0)) | ~ spl4_14), inference(avatar_component_clause, [], [f285])).
fof(f957, plain, (~ spl4_21 | ~ spl4_24), inference(avatar_contradiction_clause, [], [f956])).
fof(f956, plain, ($false | (~ spl4_21 | ~ spl4_24)), inference(subsumption_resolution, [], [f954, f161])).
fof(f954, plain, ((e0 = e3) | (~ spl4_21 | ~ spl4_24)), inference(backward_demodulation, [], [f329, f317])).
fof(f946, plain, (~ spl4_8 | ~ spl4_40), inference(avatar_split_clause, [], [f945, f395, f259])).
fof(f259, plain, (spl4_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl4_8])])).
fof(f945, plain, (~ (e3 = op(e3, e2)) | ~ spl4_40), inference(backward_demodulation, [], [f127, f397])).
fof(f397, plain, ((e3 = op(e1, e2)) | ~ spl4_40), inference(avatar_component_clause, [], [f395])).
fof(f944, plain, (~ spl4_45 | ~ spl4_47), inference(avatar_contradiction_clause, [], [f943])).
fof(f943, plain, ($false | (~ spl4_45 | ~ spl4_47)), inference(subsumption_resolution, [], [f942, f160])).
fof(f942, plain, ((e0 = e2) | (~ spl4_45 | ~ spl4_47)), inference(forward_demodulation, [], [f427, f419])).
fof(f419, plain, ((e0 = op(e1, e0)) | ~ spl4_45), inference(avatar_component_clause, [], [f417])).
fof(f417, plain, (spl4_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl4_45])])).
fof(f933, plain, (~ spl4_39 | ~ spl4_55), inference(avatar_split_clause, [], [f931, f459, f391])).
fof(f931, plain, (~ (e2 = op(e1, e2)) | ~ spl4_55), inference(backward_demodulation, [], [f123, f461])).
fof(f123, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f923, plain, (~ spl4_58 | ~ spl4_62), inference(avatar_split_clause, [], [f916, f489, f472])).
fof(f472, plain, (spl4_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl4_58])])).
fof(f916, plain, (~ (e1 = op(e0, e1)) | ~ spl4_62), inference(backward_demodulation, [], [f135, f491])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f914, plain, (~ spl4_25 | ~ spl4_43 | spl4_73), inference(avatar_split_clause, [], [f913, f608, f408, f332])).
fof(f913, plain, (~ (e0 = op(e2, e1)) | (~ spl4_43 | spl4_73)), inference(forward_demodulation, [], [f610, f410])).
fof(f895, plain, (~ spl4_56 | ~ spl4_24), inference(avatar_split_clause, [], [f894, f327, f463])).
fof(f894, plain, (~ (e3 = op(e0, e2)) | ~ spl4_24), inference(forward_demodulation, [], [f124, f329])).
fof(f893, plain, (~ spl4_54 | ~ spl4_50), inference(avatar_split_clause, [], [f892, f438, f455])).
fof(f892, plain, (~ (e1 = op(e0, e2)) | ~ spl4_50), inference(forward_demodulation, [], [f140, f440])).
fof(f440, plain, ((e1 = op(e0, e3)) | ~ spl4_50), inference(avatar_component_clause, [], [f438])).
fof(f888, plain, (~ spl4_41 | ~ spl4_2 | spl4_81), inference(avatar_split_clause, [], [f849, f645, f234, f400])).
fof(f849, plain, (~ (e0 = op(e1, e1)) | (~ spl4_2 | spl4_81)), inference(backward_demodulation, [], [f647, f236])).
fof(f887, plain, (~ spl4_40 | ~ spl4_24), inference(avatar_split_clause, [], [f886, f327, f395])).
fof(f886, plain, (~ (e3 = op(e1, e2)) | ~ spl4_24), inference(forward_demodulation, [], [f126, f329])).
fof(f885, plain, (~ spl4_38 | ~ spl4_34), inference(avatar_split_clause, [], [f884, f370, f387])).
fof(f370, plain, (spl4_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_34])])).
fof(f884, plain, (~ (e1 = op(e1, e2)) | ~ spl4_34), inference(forward_demodulation, [], [f146, f372])).
fof(f372, plain, ((e1 = op(e1, e3)) | ~ spl4_34), inference(avatar_component_clause, [], [f370])).
fof(f146, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f857, plain, (~ spl4_9 | ~ spl4_12), inference(avatar_contradiction_clause, [], [f856])).
fof(f856, plain, ($false | (~ spl4_9 | ~ spl4_12)), inference(subsumption_resolution, [], [f855, f161])).
fof(f855, plain, ((e0 = e3) | (~ spl4_9 | ~ spl4_12)), inference(forward_demodulation, [], [f278, f266])).
fof(f266, plain, ((e0 = op(e3, e1)) | ~ spl4_9), inference(avatar_component_clause, [], [f264])).
fof(f278, plain, ((e3 = op(e3, e1)) | ~ spl4_12), inference(avatar_component_clause, [], [f276])).
fof(f850, plain, (~ spl4_33 | ~ spl4_2 | spl4_71), inference(avatar_split_clause, [], [f848, f599, f234, f366])).
fof(f599, plain, (spl4_71 <=> (e0 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl4_71])])).
fof(f848, plain, (~ (e0 = op(e1, e3)) | (~ spl4_2 | spl4_71)), inference(backward_demodulation, [], [f601, f236])).
fof(f601, plain, (~ (e0 = op(op(e3, e3), e3)) | spl4_71), inference(avatar_component_clause, [], [f599])).
fof(f828, plain, (~ spl4_4 | ~ spl4_16), inference(avatar_split_clause, [], [f825, f293, f242])).
fof(f825, plain, (~ (e3 = op(e3, e3)) | ~ spl4_16), inference(backward_demodulation, [], [f155, f295])).
fof(f819, plain, (~ spl4_2 | ~ spl4_18), inference(avatar_split_clause, [], [f818, f302, f234])).
fof(f818, plain, (~ (e1 = op(e3, e3)) | ~ spl4_18), inference(backward_demodulation, [], [f134, f304])).
fof(f304, plain, ((e1 = op(e2, e3)) | ~ spl4_18), inference(avatar_component_clause, [], [f302])).
fof(f817, plain, (~ spl4_23 | ~ spl4_24), inference(avatar_contradiction_clause, [], [f816])).
fof(f816, plain, ($false | (~ spl4_23 | ~ spl4_24)), inference(subsumption_resolution, [], [f815, f164])).
fof(f815, plain, ((e2 = e3) | (~ spl4_23 | ~ spl4_24)), inference(backward_demodulation, [], [f329, f325])).
fof(f325, plain, ((e2 = op(e2, e2)) | ~ spl4_23), inference(avatar_component_clause, [], [f323])).
fof(f811, plain, (~ spl4_8 | ~ spl4_24), inference(avatar_split_clause, [], [f807, f327, f259])).
fof(f807, plain, (~ (e3 = op(e3, e2)) | ~ spl4_24), inference(backward_demodulation, [], [f128, f329])).
fof(f788, plain, (~ spl4_20 | ~ spl4_36), inference(avatar_split_clause, [], [f786, f378, f310])).
fof(f786, plain, (~ (e3 = op(e2, e3)) | ~ spl4_36), inference(backward_demodulation, [], [f132, f380])).
fof(f380, plain, ((e3 = op(e1, e3)) | ~ spl4_36), inference(avatar_component_clause, [], [f378])).
fof(f132, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f779, plain, (~ spl4_41 | ~ spl4_43), inference(avatar_contradiction_clause, [], [f778])).
fof(f778, plain, ($false | (~ spl4_41 | ~ spl4_43)), inference(subsumption_resolution, [], [f777, f160])).
fof(f777, plain, ((e0 = e2) | (~ spl4_41 | ~ spl4_43)), inference(backward_demodulation, [], [f410, f402])).
fof(f775, plain, (~ spl4_24 | ~ spl4_43 | spl4_76), inference(avatar_split_clause, [], [f769, f622, f408, f327])).
fof(f769, plain, (~ (e3 = op(e2, e2)) | (~ spl4_43 | spl4_76)), inference(backward_demodulation, [], [f624, f410])).
fof(f763, plain, (~ spl4_33 | ~ spl4_45), inference(avatar_split_clause, [], [f758, f417, f366])).
fof(f758, plain, (~ (e0 = op(e1, e3)) | ~ spl4_45), inference(backward_demodulation, [], [f143, f419])).
fof(f143, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f760, plain, (~ spl4_13 | ~ spl4_45), inference(avatar_split_clause, [], [f755, f417, f281])).
fof(f755, plain, (~ (e0 = op(e3, e0)) | ~ spl4_45), inference(backward_demodulation, [], [f115, f419])).
fof(f759, plain, (~ spl4_29 | ~ spl4_45), inference(avatar_split_clause, [], [f754, f417, f349])).
fof(f754, plain, (~ (e0 = op(e2, e0)) | ~ spl4_45), inference(backward_demodulation, [], [f114, f419])).
fof(f739, plain, (~ spl4_21 | ~ spl4_53), inference(avatar_split_clause, [], [f735, f451, f315])).
fof(f735, plain, (~ (e0 = op(e2, e2)) | ~ spl4_53), inference(backward_demodulation, [], [f124, f453])).
fof(f453, plain, ((e0 = op(e0, e2)) | ~ spl4_53), inference(avatar_component_clause, [], [f451])).
fof(f723, plain, (~ spl4_49 | ~ spl4_61), inference(avatar_split_clause, [], [f714, f485, f434])).
fof(f714, plain, (~ (e0 = op(e0, e3)) | ~ spl4_61), inference(backward_demodulation, [], [f137, f487])).
fof(f721, plain, (~ spl4_57 | ~ spl4_61), inference(avatar_split_clause, [], [f712, f485, f468])).
fof(f712, plain, (~ (e0 = op(e0, e1)) | ~ spl4_61), inference(backward_demodulation, [], [f135, f487])).
fof(f705, plain, (~ spl4_92 | ~ spl4_89 | ~ spl4_63), inference(avatar_split_clause, [], [f225, f493, f684, f702])).
fof(f225, plain, (~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (e3 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (e3 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((op(e0, e0) = e2) & (e1 = op(op(e0, e0), op(e0, e0))) & (e3 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax26)).
fof(f700, plain, (~ spl4_91 | ~ spl4_79 | ~ spl4_22), inference(avatar_split_clause, [], [f224, f319, f636, f697])).
fof(f224, plain, (~ (e1 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e1 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e1 = op(e2, e2)) & (e0 = op(op(e2, e2), op(e2, e2))) & (e3 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax25)).
fof(f690, plain, (~ spl4_85 | ~ spl4_76 | ~ spl4_41), inference(avatar_split_clause, [], [f222, f400, f622, f666])).
fof(f222, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e0 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e2 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax23)).
fof(f689, plain, (~ spl4_88 | ~ spl4_84 | ~ spl4_62), inference(avatar_split_clause, [], [f221, f489, f660, f680])).
fof(f221, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((op(e0, e0) = e1) & (e3 = op(op(e0, e0), op(e0, e0))) & (e2 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax22)).
fof(f687, plain, (~ spl4_88 | ~ spl4_89 | ~ spl4_64), inference(avatar_split_clause, [], [f219, f497, f684, f680])).
fof(f219, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (e2 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((op(e0, e0) = e3) & (e1 = op(op(e0, e0), op(e0, e0))) & (e2 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax20)).
fof(f678, plain, (~ spl4_87 | ~ spl4_81 | ~ spl4_2), inference(avatar_split_clause, [], [f218, f234, f645, f675])).
fof(f218, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e1 = op(e3, e3)) & (e0 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax19)).
fof(f663, plain, (~ spl4_82 | ~ spl4_84 | ~ spl4_63), inference(avatar_split_clause, [], [f215, f493, f660, f650])).
fof(f215, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e1 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (e1 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((op(e0, e0) = e2) & (e3 = op(op(e0, e0), op(e0, e0))) & (e1 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax16)).
fof(f639, plain, (~ spl4_78 | ~ spl4_79 | ~ spl4_24), inference(avatar_split_clause, [], [f211, f327, f636, f632])).
fof(f211, plain, (~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e3 = op(e2, e2)) & (e0 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax12)).
fof(f630, plain, (~ spl4_69 | ~ spl4_77 | ~ spl4_22), inference(avatar_split_clause, [], [f210, f319, f627, f590])).
fof(f210, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e1 = op(e2, e2)) & (e3 = op(op(e2, e2), op(e2, e2))) & (e0 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax11)).
fof(f625, plain, (~ spl4_73 | ~ spl4_76 | ~ spl4_43), inference(avatar_split_clause, [], [f209, f408, f622, f608])).
fof(f209, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e2 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax10)).
fof(f620, plain, (~ spl4_71 | ~ spl4_75 | ~ spl4_2), inference(avatar_split_clause, [], [f208, f234, f617, f599])).
fof(f208, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e1 = op(e3, e3)) & (e2 = op(op(e3, e3), op(e3, e3))) & (e0 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax9)).
fof(f615, plain, (~ spl4_73 | ~ spl4_74 | ~ spl4_44), inference(avatar_split_clause, [], [f207, f412, f612, f608])).
fof(f207, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e3 = op(e1, e1)) & (e2 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax8)).
fof(f597, plain, (~ spl4_69 | ~ spl4_70 | ~ spl4_24), inference(avatar_split_clause, [], [f205, f327, f594, f590])).
fof(f205, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e2, e2)) & (e1 = op(op(e2, e2), op(e2, e2))) & (e0 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax6)).
fof(f588, plain, (spl4_68 | spl4_41 | spl4_21 | spl4_1), inference(avatar_split_clause, [], [f173, f230, f315, f400, f552])).
fof(f552, plain, (spl4_68 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl4_68])])).
fof(f173, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | sP3) & ((~ (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | sP2) & ((~ (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | sP1) & ((~ (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (op(e0, e0) = e1) & (e0 = op(e1, e1))) | sP0)), inference(definition_folding, [], [f5, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e0 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e0 = op(e2, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e0 = op(e3, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f5, plain, (((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e0 = op(e3, e3)) & (op(e0, e0) = e3))) & ((~ (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e0 = op(e2, e2)) & (op(e0, e0) = e2))) & ((~ (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e0 = op(e1, e1)) & (op(e0, e0) = e1))) & ((~ (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax5)).
fof(f587, plain, (spl4_68 | ~ spl4_62 | spl4_21 | spl4_1), inference(avatar_split_clause, [], [f174, f230, f315, f489, f552])).
fof(f174, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e2)) | ~ (op(e0, e0) = e1) | sP0), inference(cnf_transformation, [], [f58])).
fof(f586, plain, (spl4_68 | spl4_41 | ~ spl4_63 | spl4_1), inference(avatar_split_clause, [], [f175, f230, f493, f400, f552])).
fof(f175, plain, ((e0 = op(e3, e3)) | ~ (op(e0, e0) = e2) | (e0 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f584, plain, (spl4_68 | spl4_41 | spl4_21 | ~ spl4_64), inference(avatar_split_clause, [], [f177, f497, f315, f400, f552])).
fof(f177, plain, (~ (op(e0, e0) = e3) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | sP0), inference(cnf_transformation, [], [f58])).
fof(f583, plain, (spl4_68 | ~ spl4_62 | spl4_21 | ~ spl4_64), inference(avatar_split_clause, [], [f178, f497, f315, f489, f552])).
fof(f178, plain, (~ (op(e0, e0) = e3) | (e0 = op(e2, e2)) | ~ (op(e0, e0) = e1) | sP0), inference(cnf_transformation, [], [f58])).
fof(f580, plain, (spl4_67 | spl4_42 | spl4_22 | spl4_2), inference(avatar_split_clause, [], [f181, f234, f319, f404, f546])).
fof(f546, plain, (spl4_67 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl4_67])])).
fof(f181, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f579, plain, (spl4_67 | ~ spl4_42 | spl4_22 | spl4_2), inference(avatar_split_clause, [], [f182, f234, f319, f404, f546])).
fof(f182, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f576, plain, (spl4_67 | spl4_42 | spl4_22 | ~ spl4_44), inference(avatar_split_clause, [], [f185, f412, f319, f404, f546])).
fof(f185, plain, (~ (e3 = op(e1, e1)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | sP1), inference(cnf_transformation, [], [f58])).
fof(f572, plain, (spl4_66 | spl4_43 | spl4_23 | spl4_3), inference(avatar_split_clause, [], [f189, f238, f323, f408, f540])).
fof(f540, plain, (spl4_66 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl4_66])])).
fof(f189, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f570, plain, (spl4_66 | spl4_43 | ~ spl4_23 | spl4_3), inference(avatar_split_clause, [], [f191, f238, f323, f408, f540])).
fof(f191, plain, ((e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f568, plain, (spl4_66 | spl4_43 | spl4_23 | ~ spl4_24), inference(avatar_split_clause, [], [f193, f327, f323, f408, f540])).
fof(f193, plain, (~ (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | sP2), inference(cnf_transformation, [], [f58])).
fof(f564, plain, (spl4_65 | spl4_44 | spl4_24 | spl4_4), inference(avatar_split_clause, [], [f197, f242, f327, f412, f534])).
fof(f534, plain, (spl4_65 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl4_65])])).
fof(f197, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f563, plain, (spl4_65 | ~ spl4_2 | spl4_24 | spl4_4), inference(avatar_split_clause, [], [f198, f242, f327, f234, f534])).
fof(f198, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e2)) | ~ (e1 = op(e3, e3)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f560, plain, (spl4_65 | spl4_44 | spl4_24 | ~ spl4_4), inference(avatar_split_clause, [], [f201, f242, f327, f412, f534])).
fof(f201, plain, (~ (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | sP3), inference(cnf_transformation, [], [f58])).
fof(f556, plain, (~ spl4_68 | spl4_61), inference(avatar_split_clause, [], [f171, f485, f552])).
fof(f171, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f555, plain, (~ spl4_68 | ~ spl4_61), inference(avatar_split_clause, [], [f172, f485, f552])).
fof(f172, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f62])).
fof(f550, plain, (~ spl4_67 | spl4_62), inference(avatar_split_clause, [], [f169, f489, f546])).
fof(f169, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f549, plain, (~ spl4_67 | ~ spl4_41), inference(avatar_split_clause, [], [f170, f400, f546])).
fof(f170, plain, (~ (e0 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f61])).
fof(f544, plain, (~ spl4_66 | spl4_63), inference(avatar_split_clause, [], [f167, f493, f540])).
fof(f167, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f538, plain, (~ spl4_65 | spl4_64), inference(avatar_split_clause, [], [f165, f497, f534])).
fof(f165, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f537, plain, (~ spl4_65 | ~ spl4_1), inference(avatar_split_clause, [], [f166, f230, f534])).
fof(f166, plain, (~ (e0 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f59])).
fof(f531, plain, (spl4_61 | spl4_45 | spl4_29 | spl4_13), inference(avatar_split_clause, [], [f80, f281, f349, f417, f485])).
fof(f80, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax2)).
fof(f529, plain, (spl4_62 | spl4_46 | spl4_30 | spl4_14), inference(avatar_split_clause, [], [f82, f285, f353, f421, f489])).
fof(f82, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f528, plain, (spl4_63 | spl4_59 | spl4_55 | spl4_51), inference(avatar_split_clause, [], [f83, f442, f459, f476, f493])).
fof(f83, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f527, plain, (spl4_63 | spl4_47 | spl4_31 | spl4_15), inference(avatar_split_clause, [], [f84, f289, f357, f425, f493])).
fof(f84, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f525, plain, (spl4_64 | spl4_48 | spl4_32 | spl4_16), inference(avatar_split_clause, [], [f86, f293, f361, f429, f497])).
fof(f86, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f524, plain, (spl4_45 | spl4_41 | spl4_37 | spl4_33), inference(avatar_split_clause, [], [f87, f366, f383, f400, f417])).
fof(f87, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f521, plain, (spl4_58 | spl4_42 | spl4_26 | spl4_10), inference(avatar_split_clause, [], [f90, f268, f336, f404, f472])).
fof(f90, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f520, plain, (spl4_47 | spl4_43 | spl4_39 | spl4_35), inference(avatar_split_clause, [], [f91, f374, f391, f408, f425])).
fof(f91, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f518, plain, (spl4_48 | spl4_44 | spl4_40 | spl4_36), inference(avatar_split_clause, [], [f93, f378, f395, f412, f429])).
fof(f93, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f516, plain, (spl4_29 | spl4_25 | spl4_21 | spl4_17), inference(avatar_split_clause, [], [f95, f298, f315, f332, f349])).
fof(f95, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f515, plain, (spl4_53 | spl4_37 | spl4_21 | spl4_5), inference(avatar_split_clause, [], [f96, f247, f315, f383, f451])).
fof(f96, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f513, plain, (spl4_54 | spl4_38 | spl4_22 | spl4_6), inference(avatar_split_clause, [], [f98, f251, f319, f387, f455])).
fof(f98, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f512, plain, (spl4_31 | spl4_27 | spl4_23 | spl4_19), inference(avatar_split_clause, [], [f99, f306, f323, f340, f357])).
fof(f99, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f510, plain, (spl4_32 | spl4_28 | spl4_24 | spl4_20), inference(avatar_split_clause, [], [f101, f310, f327, f344, f361])).
fof(f101, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f508, plain, (spl4_13 | spl4_9 | spl4_5 | spl4_1), inference(avatar_split_clause, [], [f103, f230, f247, f264, f281])).
fof(f103, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f502, plain, (spl4_16 | spl4_12 | spl4_8 | spl4_4), inference(avatar_split_clause, [], [f109, f242, f259, f276, f293])).
fof(f109, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f500, plain, (spl4_61 | spl4_62 | spl4_63 | spl4_64), inference(avatar_split_clause, [], [f63, f497, f493, f489, f485])).
fof(f63, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG139+1.p', ax1)).
fof(f483, plain, (spl4_57 | spl4_58 | spl4_59 | spl4_60), inference(avatar_split_clause, [], [f64, f480, f476, f472, f468])).
fof(f64, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f466, plain, (spl4_53 | spl4_54 | spl4_55 | spl4_56), inference(avatar_split_clause, [], [f65, f463, f459, f455, f451])).
fof(f65, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f449, plain, (spl4_49 | spl4_50 | spl4_51 | spl4_52), inference(avatar_split_clause, [], [f66, f446, f442, f438, f434])).
fof(f66, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
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
fof(f245, plain, (spl4_1 | spl4_2 | spl4_3 | spl4_4), inference(avatar_split_clause, [], [f78, f242, f238, f234, f230])).
fof(f78, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).