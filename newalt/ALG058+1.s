fof(f4288, plain, $false, inference(avatar_sat_refutation, [], [f1515, f1544, f1549, f1566, f1568, f1575, f1581, f1588, f1594, f1602, f1610, f1617, f1623, f1631, f1638, f1644, f1652, f1658, f1666, f1673, f1680, f1687, f1692, f1694, f1699, f1708, f1714, f1721, f1729, f1734, f1775, f1919, f2073, f2252, f2351, f2392, f2393, f2444, f2498, f2561, f2595, f2609, f2610, f2633, f2634, f2679, f2698, f2733, f2812, f3035, f3062, f3116, f3197, f3222, f3314, f3459, f3492, f3595, f3596, f3623, f3637, f3662, f3683, f3686, f3732, f3841, f3879, f4034, f4036, f4038, f4084, f4135, f4141, f4169, f4174, f4189, f4264])).
fof(f4264, plain, (~ spl154_31 | ~ spl154_35), inference(avatar_contradiction_clause, [], [f4263])).
fof(f4263, plain, ($false | (~ spl154_31 | ~ spl154_35)), inference(subsumption_resolution, [], [f4261, f508])).
fof(f508, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG058+1.p', ax5)).
fof(f4261, plain, ((e0 = e4) | (~ spl154_31 | ~ spl154_35)), inference(backward_demodulation, [], [f1115, f1099])).
fof(f1099, plain, ((e0 = op(e3, e3)) | ~ spl154_31), inference(avatar_component_clause, [], [f1097])).
fof(f1097, plain, (spl154_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_31])])).
fof(f1115, plain, ((e4 = op(e3, e3)) | ~ spl154_35), inference(avatar_component_clause, [], [f1113])).
fof(f1113, plain, (spl154_35 <=> (e4 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_35])])).
fof(f4189, plain, (~ spl154_25 | ~ spl154_125), inference(avatar_split_clause, [], [f4185, f1491, f1071])).
fof(f1071, plain, (spl154_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_25])])).
fof(f1491, plain, (spl154_125 <=> (op(e0, e0) = e4)), introduced(avatar_definition, [new_symbols(naming, [spl154_125])])).
fof(f4185, plain, (~ (e4 = op(e4, e0)) | ~ spl154_125), inference(backward_demodulation, [], [f408, f1493])).
fof(f1493, plain, ((op(e0, e0) = e4) | ~ spl154_125), inference(avatar_component_clause, [], [f1491])).
fof(f408, plain, ~ (op(e0, e0) = op(e4, e0)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG058+1.p', ax4)).
fof(f4174, plain, (~ spl154_11 | ~ spl154_130), inference(avatar_contradiction_clause, [], [f4173])).
fof(f4173, plain, ($false | (~ spl154_11 | ~ spl154_130)), inference(subsumption_resolution, [], [f4172, f506])).
fof(f506, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f4172, plain, ((e0 = e2) | (~ spl154_11 | ~ spl154_130)), inference(forward_demodulation, [], [f4162, f1015])).
fof(f1015, plain, ((e0 = op(e4, e2)) | ~ spl154_11), inference(avatar_component_clause, [], [f1013])).
fof(f1013, plain, (spl154_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_11])])).
fof(f4162, plain, ((e2 = op(e4, e2)) | ~ spl154_130), inference(backward_demodulation, [], [f348, f1514])).
fof(f1514, plain, ((e4 = unit) | ~ spl154_130), inference(avatar_component_clause, [], [f1512])).
fof(f1512, plain, (spl154_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_130])])).
fof(f348, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG058+1.p', ax2)).
fof(f4169, plain, (spl154_101 | ~ spl154_130), inference(avatar_split_clause, [], [f4159, f1512, f1391])).
fof(f1391, plain, (spl154_101 <=> (e0 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_101])])).
fof(f4159, plain, ((e0 = op(e0, e4)) | ~ spl154_130), inference(backward_demodulation, [], [f345, f1514])).
fof(f345, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f4141, plain, (~ spl154_29 | ~ spl154_49), inference(avatar_contradiction_clause, [], [f4140])).
fof(f4140, plain, ($false | (~ spl154_29 | ~ spl154_49)), inference(subsumption_resolution, [], [f3976, f1174])).
fof(f1174, plain, ((e3 = op(e3, e0)) | ~ spl154_49), inference(avatar_component_clause, [], [f1172])).
fof(f1172, plain, (spl154_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_49])])).
fof(f3976, plain, (~ (e3 = op(e3, e0)) | ~ spl154_29), inference(forward_demodulation, [], [f488, f1090])).
fof(f1090, plain, ((e3 = op(e3, e4)) | ~ spl154_29), inference(avatar_component_clause, [], [f1088])).
fof(f1088, plain, (spl154_29 <=> (e3 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_29])])).
fof(f488, plain, ~ (op(e3, e0) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f4135, plain, (~ spl154_99 | ~ spl154_49), inference(avatar_split_clause, [], [f4134, f1172, f1382])).
fof(f1382, plain, (spl154_99 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_99])])).
fof(f4134, plain, (~ (e3 = op(e1, e0)) | ~ spl154_49), inference(forward_demodulation, [], [f410, f1174])).
fof(f410, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f4084, plain, (~ spl154_61 | ~ spl154_64), inference(avatar_contradiction_clause, [], [f4083])).
fof(f4083, plain, ($false | (~ spl154_61 | ~ spl154_64)), inference(subsumption_resolution, [], [f4081, f507])).
fof(f507, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f4081, plain, ((e0 = e3) | (~ spl154_61 | ~ spl154_64)), inference(backward_demodulation, [], [f1237, f1225])).
fof(f1225, plain, ((e0 = op(e2, e2)) | ~ spl154_61), inference(avatar_component_clause, [], [f1223])).
fof(f1223, plain, (spl154_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_61])])).
fof(f1237, plain, ((e3 = op(e2, e2)) | ~ spl154_64), inference(avatar_component_clause, [], [f1235])).
fof(f1235, plain, (spl154_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_64])])).
fof(f4038, plain, (spl154_73 | ~ spl154_126), inference(avatar_split_clause, [], [f4024, f1496, f1273])).
fof(f1273, plain, (spl154_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_73])])).
fof(f1496, plain, (spl154_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_126])])).
fof(f4024, plain, ((e2 = op(e2, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f349, f1498])).
fof(f1498, plain, ((e0 = unit) | ~ spl154_126), inference(avatar_component_clause, [], [f1496])).
fof(f349, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f4036, plain, (spl154_97 | ~ spl154_126), inference(avatar_split_clause, [], [f4022, f1496, f1374])).
fof(f1374, plain, (spl154_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_97])])).
fof(f4022, plain, ((e1 = op(e1, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f347, f1498])).
fof(f347, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f4034, plain, (spl154_121 | ~ spl154_126), inference(avatar_split_clause, [], [f4020, f1496, f1475])).
fof(f1475, plain, (spl154_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_121])])).
fof(f4020, plain, ((e0 = op(e0, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f345, f1498])).
fof(f3879, plain, (spl154_63 | ~ spl154_128), inference(avatar_contradiction_clause, [], [f3878])).
fof(f3878, plain, ($false | (spl154_63 | ~ spl154_128)), inference(subsumption_resolution, [], [f3862, f1232])).
fof(f1232, plain, (~ (e2 = op(e2, e2)) | spl154_63), inference(avatar_component_clause, [], [f1231])).
fof(f1231, plain, (spl154_63 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_63])])).
fof(f3862, plain, ((e2 = op(e2, e2)) | ~ spl154_128), inference(backward_demodulation, [], [f349, f1506])).
fof(f1506, plain, ((e2 = unit) | ~ spl154_128), inference(avatar_component_clause, [], [f1504])).
fof(f1504, plain, (spl154_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_128])])).
fof(f3841, plain, (~ spl154_89 | ~ spl154_64), inference(avatar_split_clause, [], [f3007, f1235, f1340])).
fof(f1340, plain, (spl154_89 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_89])])).
fof(f3007, plain, (~ (e3 = op(e1, e2)) | ~ spl154_64), inference(forward_demodulation, [], [f429, f1237])).
fof(f429, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f3732, plain, (~ spl154_37 | ~ spl154_129), inference(avatar_contradiction_clause, [], [f3731])).
fof(f3731, plain, ($false | (~ spl154_37 | ~ spl154_129)), inference(subsumption_resolution, [], [f3730, f509])).
fof(f509, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f3730, plain, ((e1 = e2) | (~ spl154_37 | ~ spl154_129)), inference(forward_demodulation, [], [f3719, f1124])).
fof(f1124, plain, ((e1 = op(e3, e2)) | ~ spl154_37), inference(avatar_component_clause, [], [f1122])).
fof(f1122, plain, (spl154_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_37])])).
fof(f3719, plain, ((e2 = op(e3, e2)) | ~ spl154_129), inference(backward_demodulation, [], [f348, f1510])).
fof(f1510, plain, ((e3 = unit) | ~ spl154_129), inference(avatar_component_clause, [], [f1508])).
fof(f1508, plain, (spl154_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_129])])).
fof(f3686, plain, (~ spl154_34 | ~ spl154_49), inference(avatar_split_clause, [], [f3685, f1172, f1109])).
fof(f1109, plain, (spl154_34 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_34])])).
fof(f3685, plain, (~ (e3 = op(e3, e3)) | ~ spl154_49), inference(forward_demodulation, [], [f487, f1174])).
fof(f487, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f3683, plain, (spl154_35 | ~ spl154_64), inference(avatar_split_clause, [], [f2974, f1235, f1113])).
fof(f2974, plain, ((e4 = op(e3, e3)) | ~ spl154_64), inference(forward_demodulation, [], [f518, f1237])).
fof(f518, plain, (e4 = op(op(e2, e2), op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(op(e2, e2), op(e2, e2))) & (e3 = op(e2, e2)) & (e1 = op(op(e2, e2), e2)) & (e0 = op(op(op(e2, e2), e2), op(op(e2, e2), e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG058+1.p', ax6)).
fof(f3662, plain, (~ spl154_36 | ~ spl154_37), inference(avatar_contradiction_clause, [], [f3661])).
fof(f3661, plain, ($false | (~ spl154_36 | ~ spl154_37)), inference(subsumption_resolution, [], [f3660, f505])).
fof(f505, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f3660, plain, ((e0 = e1) | (~ spl154_36 | ~ spl154_37)), inference(backward_demodulation, [], [f1124, f1120])).
fof(f1120, plain, ((e0 = op(e3, e2)) | ~ spl154_36), inference(avatar_component_clause, [], [f1118])).
fof(f1118, plain, (spl154_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_36])])).
fof(f3637, plain, (~ spl154_91 | ~ spl154_94), inference(avatar_contradiction_clause, [], [f3636])).
fof(f3636, plain, ($false | (~ spl154_91 | ~ spl154_94)), inference(subsumption_resolution, [], [f3635, f507])).
fof(f3635, plain, ((e0 = e3) | (~ spl154_91 | ~ spl154_94)), inference(forward_demodulation, [], [f1363, f1351])).
fof(f1351, plain, ((e0 = op(e1, e1)) | ~ spl154_91), inference(avatar_component_clause, [], [f1349])).
fof(f1349, plain, (spl154_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_91])])).
fof(f1363, plain, ((e3 = op(e1, e1)) | ~ spl154_94), inference(avatar_component_clause, [], [f1361])).
fof(f1361, plain, (spl154_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_94])])).
fof(f3623, plain, (~ spl154_84 | ~ spl154_109), inference(avatar_split_clause, [], [f3621, f1424, f1319])).
fof(f1319, plain, (spl154_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_84])])).
fof(f1424, plain, (spl154_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_109])])).
fof(f3621, plain, (~ (e3 = op(e1, e3)) | ~ spl154_109), inference(backward_demodulation, [], [f435, f1426])).
fof(f1426, plain, ((e3 = op(e0, e3)) | ~ spl154_109), inference(avatar_component_clause, [], [f1424])).
fof(f435, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f3596, plain, (spl154_49 | ~ spl154_126), inference(avatar_split_clause, [], [f3582, f1496, f1172])).
fof(f3582, plain, ((e3 = op(e3, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f351, f1498])).
fof(f351, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f3595, plain, (spl154_109 | ~ spl154_126), inference(avatar_split_clause, [], [f3581, f1496, f1424])).
fof(f3581, plain, ((e3 = op(e0, e3)) | ~ spl154_126), inference(backward_demodulation, [], [f350, f1498])).
fof(f350, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f3492, plain, (~ spl154_63 | ~ spl154_64), inference(avatar_contradiction_clause, [], [f3491])).
fof(f3491, plain, ($false | (~ spl154_63 | ~ spl154_64)), inference(subsumption_resolution, [], [f3490, f512])).
fof(f512, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f3490, plain, ((e2 = e3) | (~ spl154_63 | ~ spl154_64)), inference(backward_demodulation, [], [f1237, f1233])).
fof(f1233, plain, ((e2 = op(e2, e2)) | ~ spl154_63), inference(avatar_component_clause, [], [f1231])).
fof(f3459, plain, (~ spl154_91 | ~ spl154_93), inference(avatar_contradiction_clause, [], [f3458])).
fof(f3458, plain, ($false | (~ spl154_91 | ~ spl154_93)), inference(subsumption_resolution, [], [f3457, f506])).
fof(f3457, plain, ((e0 = e2) | (~ spl154_91 | ~ spl154_93)), inference(forward_demodulation, [], [f1359, f1351])).
fof(f1359, plain, ((e2 = op(e1, e1)) | ~ spl154_93), inference(avatar_component_clause, [], [f1357])).
fof(f1357, plain, (spl154_93 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_93])])).
fof(f3314, plain, (~ spl154_33 | ~ spl154_35), inference(avatar_contradiction_clause, [], [f3313])).
fof(f3313, plain, ($false | (~ spl154_33 | ~ spl154_35)), inference(subsumption_resolution, [], [f3312, f513])).
fof(f513, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f3312, plain, ((e2 = e4) | (~ spl154_33 | ~ spl154_35)), inference(backward_demodulation, [], [f1115, f1107])).
fof(f1107, plain, ((e2 = op(e3, e3)) | ~ spl154_33), inference(avatar_component_clause, [], [f1105])).
fof(f1105, plain, (spl154_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_33])])).
fof(f3222, plain, (~ spl154_79 | ~ spl154_4), inference(avatar_split_clause, [], [f2993, f983, f1298])).
fof(f1298, plain, (spl154_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_79])])).
fof(f983, plain, (spl154_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_4])])).
fof(f2993, plain, (~ (e3 = op(e1, e4)) | ~ spl154_4), inference(forward_demodulation, [], [f451, f985])).
fof(f985, plain, ((e3 = op(e4, e4)) | ~ spl154_4), inference(avatar_component_clause, [], [f983])).
fof(f451, plain, ~ (op(e1, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f3197, plain, (spl154_37 | ~ spl154_64), inference(avatar_split_clause, [], [f2975, f1235, f1122])).
fof(f2975, plain, ((e1 = op(e3, e2)) | ~ spl154_64), inference(forward_demodulation, [], [f516, f1237])).
fof(f516, plain, (e1 = op(op(e2, e2), e2)), inference(cnf_transformation, [], [f6])).
fof(f3116, plain, (~ spl154_91 | ~ spl154_95), inference(avatar_contradiction_clause, [], [f3115])).
fof(f3115, plain, ($false | (~ spl154_91 | ~ spl154_95)), inference(subsumption_resolution, [], [f3114, f508])).
fof(f3114, plain, ((e0 = e4) | (~ spl154_91 | ~ spl154_95)), inference(forward_demodulation, [], [f1367, f1351])).
fof(f1367, plain, ((e4 = op(e1, e1)) | ~ spl154_95), inference(avatar_component_clause, [], [f1365])).
fof(f1365, plain, (spl154_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_95])])).
fof(f3062, plain, (~ spl154_91 | ~ spl154_127), inference(avatar_contradiction_clause, [], [f3061])).
fof(f3061, plain, ($false | (~ spl154_91 | ~ spl154_127)), inference(subsumption_resolution, [], [f3060, f505])).
fof(f3060, plain, ((e0 = e1) | (~ spl154_91 | ~ spl154_127)), inference(forward_demodulation, [], [f3045, f1351])).
fof(f3045, plain, ((e1 = op(e1, e1)) | ~ spl154_127), inference(backward_demodulation, [], [f347, f1502])).
fof(f1502, plain, ((e1 = unit) | ~ spl154_127), inference(avatar_component_clause, [], [f1500])).
fof(f1500, plain, (spl154_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_127])])).
fof(f3035, plain, (~ spl154_116 | ~ spl154_91), inference(avatar_split_clause, [], [f3034, f1349, f1454])).
fof(f1454, plain, (spl154_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_116])])).
fof(f3034, plain, (~ (e0 = op(e0, e1)) | ~ spl154_91), inference(forward_demodulation, [], [f415, f1351])).
fof(f415, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f2812, plain, (~ spl154_64 | ~ spl154_65), inference(avatar_contradiction_clause, [], [f2811])).
fof(f2811, plain, ($false | (~ spl154_64 | ~ spl154_65)), inference(subsumption_resolution, [], [f2810, f514])).
fof(f514, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f2810, plain, ((e3 = e4) | (~ spl154_64 | ~ spl154_65)), inference(backward_demodulation, [], [f1241, f1237])).
fof(f1241, plain, ((e4 = op(e2, e2)) | ~ spl154_65), inference(avatar_component_clause, [], [f1239])).
fof(f1239, plain, (spl154_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_65])])).
fof(f2733, plain, (~ spl154_86 | ~ spl154_91), inference(avatar_split_clause, [], [f2726, f1349, f1328])).
fof(f1328, plain, (spl154_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_86])])).
fof(f2726, plain, (~ (e0 = op(e1, e2)) | ~ spl154_91), inference(backward_demodulation, [], [f469, f1351])).
fof(f469, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f2698, plain, (~ spl154_5 | ~ spl154_105), inference(avatar_split_clause, [], [f2694, f1407, f987])).
fof(f987, plain, (spl154_5 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_5])])).
fof(f1407, plain, (spl154_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_105])])).
fof(f2694, plain, (~ (e4 = op(e4, e4)) | ~ spl154_105), inference(backward_demodulation, [], [f448, f1409])).
fof(f1409, plain, ((e4 = op(e0, e4)) | ~ spl154_105), inference(avatar_component_clause, [], [f1407])).
fof(f448, plain, ~ (op(e0, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2679, plain, (~ spl154_101 | ~ spl154_111), inference(avatar_split_clause, [], [f2673, f1433, f1391])).
fof(f1433, plain, (spl154_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_111])])).
fof(f2673, plain, (~ (e0 = op(e0, e4)) | ~ spl154_111), inference(backward_demodulation, [], [f463, f1435])).
fof(f1435, plain, ((e0 = op(e0, e2)) | ~ spl154_111), inference(avatar_component_clause, [], [f1433])).
fof(f463, plain, ~ (op(e0, e2) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f2634, plain, (spl154_25 | ~ spl154_126), inference(avatar_split_clause, [], [f2620, f1496, f1071])).
fof(f2620, plain, ((e4 = op(e4, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f353, f1498])).
fof(f353, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2633, plain, (spl154_105 | ~ spl154_126), inference(avatar_split_clause, [], [f2619, f1496, f1407])).
fof(f2619, plain, ((e4 = op(e0, e4)) | ~ spl154_126), inference(backward_demodulation, [], [f352, f1498])).
fof(f352, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2610, plain, (spl154_259 | spl154_233 | spl154_207 | spl154_181 | spl154_155), inference(avatar_split_clause, [], [f966, f1738, f1867, f1996, f2125, f2254])).
fof(f2254, plain, (spl154_259 <=> sP125), introduced(avatar_definition, [new_symbols(naming, [spl154_259])])).
fof(f2125, plain, (spl154_233 <=> sP126), introduced(avatar_definition, [new_symbols(naming, [spl154_233])])).
fof(f1996, plain, (spl154_207 <=> sP127), introduced(avatar_definition, [new_symbols(naming, [spl154_207])])).
fof(f1867, plain, (spl154_181 <=> sP128), introduced(avatar_definition, [new_symbols(naming, [spl154_181])])).
fof(f1738, plain, (spl154_155 <=> sP129), introduced(avatar_definition, [new_symbols(naming, [spl154_155])])).
fof(f966, plain, (sP129 | sP128 | sP127 | sP126 | sP125), inference(cnf_transformation, [], [f164])).
fof(f164, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | sP153 | sP152 | sP151 | sP150 | sP149 | sP148 | sP147 | sP146 | sP145 | sP144 | sP143 | sP142 | sP141 | sP140 | sP139 | sP138 | sP137 | sP136 | sP135 | sP134 | sP133 | sP132 | sP131 | sP130) & (sP129 | sP128 | sP127 | sP126 | sP125)), inference(definition_folding, [], [f9, e163, e162, e161, e160, e159, e158, e157, e156, e155, e154, e153, e152, e151, e150, e149, e148, e147, e146, e145, e144, e143, e142, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88, e87, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1)) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2)) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3)) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4)) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0)) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2)) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f18, plain, ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3)) | ~ sP8), inference(usedef, [], [e18])).
fof(e18, plain, (sP8 <=> ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f19, plain, ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4)) | ~ sP9), inference(usedef, [], [e19])).
fof(e19, plain, (sP9 <=> ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f20, plain, ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0)) | ~ sP10), inference(usedef, [], [e20])).
fof(e20, plain, (sP10 <=> ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f21, plain, ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1)) | ~ sP11), inference(usedef, [], [e21])).
fof(e21, plain, (sP11 <=> ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f22, plain, ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2)) | ~ sP12), inference(usedef, [], [e22])).
fof(e22, plain, (sP12 <=> ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f23, plain, ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3)) | ~ sP13), inference(usedef, [], [e23])).
fof(e23, plain, (sP13 <=> ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f24, plain, ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4)) | ~ sP14), inference(usedef, [], [e24])).
fof(e24, plain, (sP14 <=> ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f25, plain, ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0)) | ~ sP15), inference(usedef, [], [e25])).
fof(e25, plain, (sP15 <=> ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f26, plain, ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1)) | ~ sP16), inference(usedef, [], [e26])).
fof(e26, plain, (sP16 <=> ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f27, plain, ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2)) | ~ sP17), inference(usedef, [], [e27])).
fof(e27, plain, (sP17 <=> ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f28, plain, ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3)) | ~ sP18), inference(usedef, [], [e28])).
fof(e28, plain, (sP18 <=> ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f29, plain, ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4)) | ~ sP19), inference(usedef, [], [e29])).
fof(e29, plain, (sP19 <=> ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f30, plain, ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0)) | ~ sP20), inference(usedef, [], [e30])).
fof(e30, plain, (sP20 <=> ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f31, plain, ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1)) | ~ sP21), inference(usedef, [], [e31])).
fof(e31, plain, (sP21 <=> ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f32, plain, ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2)) | ~ sP22), inference(usedef, [], [e32])).
fof(e32, plain, (sP22 <=> ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f33, plain, ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3)) | ~ sP23), inference(usedef, [], [e33])).
fof(e33, plain, (sP23 <=> ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f34, plain, ((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4)) | ~ sP24), inference(usedef, [], [e34])).
fof(e34, plain, (sP24 <=> ((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f35, plain, ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1) | ~ sP25), inference(usedef, [], [e35])).
fof(e35, plain, (sP25 <=> ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f36, plain, ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1)) | ~ sP26), inference(usedef, [], [e36])).
fof(e36, plain, (sP26 <=> ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f37, plain, ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2)) | ~ sP27), inference(usedef, [], [e37])).
fof(e37, plain, (sP27 <=> ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f38, plain, ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3)) | ~ sP28), inference(usedef, [], [e38])).
fof(e38, plain, (sP28 <=> ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f39, plain, ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4)) | ~ sP29), inference(usedef, [], [e39])).
fof(e39, plain, (sP29 <=> ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f40, plain, ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0)) | ~ sP30), inference(usedef, [], [e40])).
fof(e40, plain, (sP30 <=> ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f41, plain, ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1)) | ~ sP31), inference(usedef, [], [e41])).
fof(e41, plain, (sP31 <=> ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f42, plain, ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2)) | ~ sP32), inference(usedef, [], [e42])).
fof(e42, plain, (sP32 <=> ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f43, plain, ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3)) | ~ sP33), inference(usedef, [], [e43])).
fof(e43, plain, (sP33 <=> ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f44, plain, ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4)) | ~ sP34), inference(usedef, [], [e44])).
fof(e44, plain, (sP34 <=> ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f45, plain, ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0)) | ~ sP35), inference(usedef, [], [e45])).
fof(e45, plain, (sP35 <=> ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f46, plain, ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1)) | ~ sP36), inference(usedef, [], [e46])).
fof(e46, plain, (sP36 <=> ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f47, plain, ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2)) | ~ sP37), inference(usedef, [], [e47])).
fof(e47, plain, (sP37 <=> ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f48, plain, ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3)) | ~ sP38), inference(usedef, [], [e48])).
fof(e48, plain, (sP38 <=> ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f49, plain, ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4)) | ~ sP39), inference(usedef, [], [e49])).
fof(e49, plain, (sP39 <=> ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f50, plain, ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0)) | ~ sP40), inference(usedef, [], [e50])).
fof(e50, plain, (sP40 <=> ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f51, plain, ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1)) | ~ sP41), inference(usedef, [], [e51])).
fof(e51, plain, (sP41 <=> ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f52, plain, ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2)) | ~ sP42), inference(usedef, [], [e52])).
fof(e52, plain, (sP42 <=> ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f53, plain, ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3)) | ~ sP43), inference(usedef, [], [e53])).
fof(e53, plain, (sP43 <=> ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f54, plain, ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4)) | ~ sP44), inference(usedef, [], [e54])).
fof(e54, plain, (sP44 <=> ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f55, plain, ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0)) | ~ sP45), inference(usedef, [], [e55])).
fof(e55, plain, (sP45 <=> ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f56, plain, ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1)) | ~ sP46), inference(usedef, [], [e56])).
fof(e56, plain, (sP46 <=> ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f57, plain, ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2)) | ~ sP47), inference(usedef, [], [e57])).
fof(e57, plain, (sP47 <=> ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f58, plain, ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3)) | ~ sP48), inference(usedef, [], [e58])).
fof(e58, plain, (sP48 <=> ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f59, plain, ((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4)) | ~ sP49), inference(usedef, [], [e59])).
fof(e59, plain, (sP49 <=> ((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f60, plain, ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2) | ~ sP50), inference(usedef, [], [e60])).
fof(e60, plain, (sP50 <=> ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f61, plain, ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1)) | ~ sP51), inference(usedef, [], [e61])).
fof(e61, plain, (sP51 <=> ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f62, plain, ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2)) | ~ sP52), inference(usedef, [], [e62])).
fof(e62, plain, (sP52 <=> ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f63, plain, ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3)) | ~ sP53), inference(usedef, [], [e63])).
fof(e63, plain, (sP53 <=> ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f64, plain, ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4)) | ~ sP54), inference(usedef, [], [e64])).
fof(e64, plain, (sP54 <=> ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f65, plain, ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0)) | ~ sP55), inference(usedef, [], [e65])).
fof(e65, plain, (sP55 <=> ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f66, plain, ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1)) | ~ sP56), inference(usedef, [], [e66])).
fof(e66, plain, (sP56 <=> ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f67, plain, ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2)) | ~ sP57), inference(usedef, [], [e67])).
fof(e67, plain, (sP57 <=> ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f68, plain, ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3)) | ~ sP58), inference(usedef, [], [e68])).
fof(e68, plain, (sP58 <=> ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f69, plain, ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4)) | ~ sP59), inference(usedef, [], [e69])).
fof(e69, plain, (sP59 <=> ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f70, plain, ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0)) | ~ sP60), inference(usedef, [], [e70])).
fof(e70, plain, (sP60 <=> ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f71, plain, ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1)) | ~ sP61), inference(usedef, [], [e71])).
fof(e71, plain, (sP61 <=> ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f72, plain, ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2)) | ~ sP62), inference(usedef, [], [e72])).
fof(e72, plain, (sP62 <=> ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f73, plain, ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3)) | ~ sP63), inference(usedef, [], [e73])).
fof(e73, plain, (sP63 <=> ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f74, plain, ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4)) | ~ sP64), inference(usedef, [], [e74])).
fof(e74, plain, (sP64 <=> ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP64])])).
fof(f75, plain, ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0)) | ~ sP65), inference(usedef, [], [e75])).
fof(e75, plain, (sP65 <=> ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP65])])).
fof(f76, plain, ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1)) | ~ sP66), inference(usedef, [], [e76])).
fof(e76, plain, (sP66 <=> ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP66])])).
fof(f77, plain, ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2)) | ~ sP67), inference(usedef, [], [e77])).
fof(e77, plain, (sP67 <=> ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP67])])).
fof(f78, plain, ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3)) | ~ sP68), inference(usedef, [], [e78])).
fof(e78, plain, (sP68 <=> ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP68])])).
fof(f79, plain, ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4)) | ~ sP69), inference(usedef, [], [e79])).
fof(e79, plain, (sP69 <=> ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP69])])).
fof(f80, plain, ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0)) | ~ sP70), inference(usedef, [], [e80])).
fof(e80, plain, (sP70 <=> ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP70])])).
fof(f81, plain, ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1)) | ~ sP71), inference(usedef, [], [e81])).
fof(e81, plain, (sP71 <=> ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP71])])).
fof(f82, plain, ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2)) | ~ sP72), inference(usedef, [], [e82])).
fof(e82, plain, (sP72 <=> ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP72])])).
fof(f83, plain, ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3)) | ~ sP73), inference(usedef, [], [e83])).
fof(e83, plain, (sP73 <=> ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP73])])).
fof(f84, plain, ((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4)) | ~ sP74), inference(usedef, [], [e84])).
fof(e84, plain, (sP74 <=> ((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP74])])).
fof(f85, plain, ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3) | ~ sP75), inference(usedef, [], [e85])).
fof(e85, plain, (sP75 <=> ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP75])])).
fof(f86, plain, ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1)) | ~ sP76), inference(usedef, [], [e86])).
fof(e86, plain, (sP76 <=> ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP76])])).
fof(f87, plain, ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2)) | ~ sP77), inference(usedef, [], [e87])).
fof(e87, plain, (sP77 <=> ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP77])])).
fof(f88, plain, ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3)) | ~ sP78), inference(usedef, [], [e88])).
fof(e88, plain, (sP78 <=> ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP78])])).
fof(f89, plain, ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4)) | ~ sP79), inference(usedef, [], [e89])).
fof(e89, plain, (sP79 <=> ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP79])])).
fof(f90, plain, ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0)) | ~ sP80), inference(usedef, [], [e90])).
fof(e90, plain, (sP80 <=> ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP80])])).
fof(f91, plain, ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1)) | ~ sP81), inference(usedef, [], [e91])).
fof(e91, plain, (sP81 <=> ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP81])])).
fof(f92, plain, ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2)) | ~ sP82), inference(usedef, [], [e92])).
fof(e92, plain, (sP82 <=> ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP82])])).
fof(f93, plain, ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3)) | ~ sP83), inference(usedef, [], [e93])).
fof(e93, plain, (sP83 <=> ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP83])])).
fof(f94, plain, ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4)) | ~ sP84), inference(usedef, [], [e94])).
fof(e94, plain, (sP84 <=> ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP84])])).
fof(f95, plain, ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0)) | ~ sP85), inference(usedef, [], [e95])).
fof(e95, plain, (sP85 <=> ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP85])])).
fof(f96, plain, ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1)) | ~ sP86), inference(usedef, [], [e96])).
fof(e96, plain, (sP86 <=> ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP86])])).
fof(f97, plain, ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2)) | ~ sP87), inference(usedef, [], [e97])).
fof(e97, plain, (sP87 <=> ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP87])])).
fof(f98, plain, ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3)) | ~ sP88), inference(usedef, [], [e98])).
fof(e98, plain, (sP88 <=> ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP88])])).
fof(f99, plain, ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4)) | ~ sP89), inference(usedef, [], [e99])).
fof(e99, plain, (sP89 <=> ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP89])])).
fof(f100, plain, ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0)) | ~ sP90), inference(usedef, [], [e100])).
fof(e100, plain, (sP90 <=> ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP90])])).
fof(f101, plain, ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1)) | ~ sP91), inference(usedef, [], [e101])).
fof(e101, plain, (sP91 <=> ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP91])])).
fof(f102, plain, ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2)) | ~ sP92), inference(usedef, [], [e102])).
fof(e102, plain, (sP92 <=> ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP92])])).
fof(f103, plain, ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3)) | ~ sP93), inference(usedef, [], [e103])).
fof(e103, plain, (sP93 <=> ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP93])])).
fof(f104, plain, ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4)) | ~ sP94), inference(usedef, [], [e104])).
fof(e104, plain, (sP94 <=> ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP94])])).
fof(f105, plain, ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0)) | ~ sP95), inference(usedef, [], [e105])).
fof(e105, plain, (sP95 <=> ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP95])])).
fof(f106, plain, ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1)) | ~ sP96), inference(usedef, [], [e106])).
fof(e106, plain, (sP96 <=> ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP96])])).
fof(f107, plain, ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2)) | ~ sP97), inference(usedef, [], [e107])).
fof(e107, plain, (sP97 <=> ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP97])])).
fof(f108, plain, ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3)) | ~ sP98), inference(usedef, [], [e108])).
fof(e108, plain, (sP98 <=> ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP98])])).
fof(f109, plain, ((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4)) | ~ sP99), inference(usedef, [], [e109])).
fof(e109, plain, (sP99 <=> ((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP99])])).
fof(f110, plain, ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4) | ~ sP100), inference(usedef, [], [e110])).
fof(e110, plain, (sP100 <=> ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP100])])).
fof(f111, plain, ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1)) | ~ sP101), inference(usedef, [], [e111])).
fof(e111, plain, (sP101 <=> ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP101])])).
fof(f112, plain, ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2)) | ~ sP102), inference(usedef, [], [e112])).
fof(e112, plain, (sP102 <=> ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP102])])).
fof(f113, plain, ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3)) | ~ sP103), inference(usedef, [], [e113])).
fof(e113, plain, (sP103 <=> ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP103])])).
fof(f114, plain, ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4)) | ~ sP104), inference(usedef, [], [e114])).
fof(e114, plain, (sP104 <=> ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP104])])).
fof(f115, plain, ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0)) | ~ sP105), inference(usedef, [], [e115])).
fof(e115, plain, (sP105 <=> ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP105])])).
fof(f116, plain, ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1)) | ~ sP106), inference(usedef, [], [e116])).
fof(e116, plain, (sP106 <=> ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP106])])).
fof(f117, plain, ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2)) | ~ sP107), inference(usedef, [], [e117])).
fof(e117, plain, (sP107 <=> ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP107])])).
fof(f118, plain, ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3)) | ~ sP108), inference(usedef, [], [e118])).
fof(e118, plain, (sP108 <=> ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP108])])).
fof(f119, plain, ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4)) | ~ sP109), inference(usedef, [], [e119])).
fof(e119, plain, (sP109 <=> ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP109])])).
fof(f120, plain, ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0)) | ~ sP110), inference(usedef, [], [e120])).
fof(e120, plain, (sP110 <=> ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP110])])).
fof(f121, plain, ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1)) | ~ sP111), inference(usedef, [], [e121])).
fof(e121, plain, (sP111 <=> ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP111])])).
fof(f122, plain, ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2)) | ~ sP112), inference(usedef, [], [e122])).
fof(e122, plain, (sP112 <=> ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP112])])).
fof(f123, plain, ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3)) | ~ sP113), inference(usedef, [], [e123])).
fof(e123, plain, (sP113 <=> ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP113])])).
fof(f124, plain, ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4)) | ~ sP114), inference(usedef, [], [e124])).
fof(e124, plain, (sP114 <=> ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP114])])).
fof(f125, plain, ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0)) | ~ sP115), inference(usedef, [], [e125])).
fof(e125, plain, (sP115 <=> ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP115])])).
fof(f126, plain, ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1)) | ~ sP116), inference(usedef, [], [e126])).
fof(e126, plain, (sP116 <=> ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP116])])).
fof(f127, plain, ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2)) | ~ sP117), inference(usedef, [], [e127])).
fof(e127, plain, (sP117 <=> ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP117])])).
fof(f128, plain, ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(usedef, [], [e128])).
fof(e128, plain, (sP118 <=> ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP118])])).
fof(f129, plain, ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4)) | ~ sP119), inference(usedef, [], [e129])).
fof(e129, plain, (sP119 <=> ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP119])])).
fof(f130, plain, ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0)) | ~ sP120), inference(usedef, [], [e130])).
fof(e130, plain, (sP120 <=> ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP120])])).
fof(f131, plain, ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1)) | ~ sP121), inference(usedef, [], [e131])).
fof(e131, plain, (sP121 <=> ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP121])])).
fof(f132, plain, ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2)) | ~ sP122), inference(usedef, [], [e132])).
fof(e132, plain, (sP122 <=> ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP122])])).
fof(f133, plain, ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3)) | ~ sP123), inference(usedef, [], [e133])).
fof(e133, plain, (sP123 <=> ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP123])])).
fof(f134, plain, ((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4)) | ~ sP124), inference(usedef, [], [e134])).
fof(e134, plain, (sP124 <=> ((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP124])])).
fof(f135, plain, ((sP24 & sP23 & sP22 & sP21 & sP20 & sP19 & sP18 & sP17 & sP16 & sP15 & sP14 & sP13 & sP12 & sP11 & sP10 & sP9 & sP8 & sP7 & sP6 & sP5 & sP4 & sP3 & sP2 & sP1 & sP0) | ~ sP125), inference(usedef, [], [e135])).
fof(e135, plain, (sP125 <=> (sP24 & sP23 & sP22 & sP21 & sP20 & sP19 & sP18 & sP17 & sP16 & sP15 & sP14 & sP13 & sP12 & sP11 & sP10 & sP9 & sP8 & sP7 & sP6 & sP5 & sP4 & sP3 & sP2 & sP1 & sP0)), introduced(predicate_definition_introduction, [new_symbols(naming, [sP125])])).
fof(f136, plain, ((sP49 & sP48 & sP47 & sP46 & sP45 & sP44 & sP43 & sP42 & sP41 & sP40 & sP39 & sP38 & sP37 & sP36 & sP35 & sP34 & sP33 & sP32 & sP31 & sP30 & sP29 & sP28 & sP27 & sP26 & sP25) | ~ sP126), inference(usedef, [], [e136])).
fof(e136, plain, (sP126 <=> (sP49 & sP48 & sP47 & sP46 & sP45 & sP44 & sP43 & sP42 & sP41 & sP40 & sP39 & sP38 & sP37 & sP36 & sP35 & sP34 & sP33 & sP32 & sP31 & sP30 & sP29 & sP28 & sP27 & sP26 & sP25)), introduced(predicate_definition_introduction, [new_symbols(naming, [sP126])])).
fof(f137, plain, ((sP74 & sP73 & sP72 & sP71 & sP70 & sP69 & sP68 & sP67 & sP66 & sP65 & sP64 & sP63 & sP62 & sP61 & sP60 & sP59 & sP58 & sP57 & sP56 & sP55 & sP54 & sP53 & sP52 & sP51 & sP50) | ~ sP127), inference(usedef, [], [e137])).
fof(e137, plain, (sP127 <=> (sP74 & sP73 & sP72 & sP71 & sP70 & sP69 & sP68 & sP67 & sP66 & sP65 & sP64 & sP63 & sP62 & sP61 & sP60 & sP59 & sP58 & sP57 & sP56 & sP55 & sP54 & sP53 & sP52 & sP51 & sP50)), introduced(predicate_definition_introduction, [new_symbols(naming, [sP127])])).
fof(f138, plain, ((sP99 & sP98 & sP97 & sP96 & sP95 & sP94 & sP93 & sP92 & sP91 & sP90 & sP89 & sP88 & sP87 & sP86 & sP85 & sP84 & sP83 & sP82 & sP81 & sP80 & sP79 & sP78 & sP77 & sP76 & sP75) | ~ sP128), inference(usedef, [], [e138])).
fof(e138, plain, (sP128 <=> (sP99 & sP98 & sP97 & sP96 & sP95 & sP94 & sP93 & sP92 & sP91 & sP90 & sP89 & sP88 & sP87 & sP86 & sP85 & sP84 & sP83 & sP82 & sP81 & sP80 & sP79 & sP78 & sP77 & sP76 & sP75)), introduced(predicate_definition_introduction, [new_symbols(naming, [sP128])])).
fof(f139, plain, ((sP124 & sP123 & sP122 & sP121 & sP120 & sP119 & sP118 & sP117 & sP116 & sP115 & sP114 & sP113 & sP112 & sP111 & sP110 & sP109 & sP108 & sP107 & sP106 & sP105 & sP104 & sP103 & sP102 & sP101 & sP100) | ~ sP129), inference(usedef, [], [e139])).
fof(e139, plain, (sP129 <=> (sP124 & sP123 & sP122 & sP121 & sP120 & sP119 & sP118 & sP117 & sP116 & sP115 & sP114 & sP113 & sP112 & sP111 & sP110 & sP109 & sP108 & sP107 & sP106 & sP105 & sP104 & sP103 & sP102 & sP101 & sP100)), introduced(predicate_definition_introduction, [new_symbols(naming, [sP129])])).
fof(f140, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP130), inference(usedef, [], [e140])).
fof(e140, plain, (sP130 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP130])])).
fof(f141, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP131), inference(usedef, [], [e141])).
fof(e141, plain, (sP131 <=> (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP131])])).
fof(f142, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP132), inference(usedef, [], [e142])).
fof(e142, plain, (sP132 <=> (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP132])])).
fof(f143, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP133), inference(usedef, [], [e143])).
fof(e143, plain, (sP133 <=> (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP133])])).
fof(f144, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP134), inference(usedef, [], [e144])).
fof(e144, plain, (sP134 <=> (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP134])])).
fof(f145, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP135), inference(usedef, [], [e145])).
fof(e145, plain, (sP135 <=> (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP135])])).
fof(f146, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP136), inference(usedef, [], [e146])).
fof(e146, plain, (sP136 <=> (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP136])])).
fof(f147, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP137), inference(usedef, [], [e147])).
fof(e147, plain, (sP137 <=> (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP137])])).
fof(f148, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP138), inference(usedef, [], [e148])).
fof(e148, plain, (sP138 <=> (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP138])])).
fof(f149, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP139), inference(usedef, [], [e149])).
fof(e149, plain, (sP139 <=> (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP139])])).
fof(f150, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP140), inference(usedef, [], [e150])).
fof(e150, plain, (sP140 <=> (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP140])])).
fof(f151, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP141), inference(usedef, [], [e151])).
fof(e151, plain, (sP141 <=> (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP141])])).
fof(f152, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP142), inference(usedef, [], [e152])).
fof(e152, plain, (sP142 <=> (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP142])])).
fof(f153, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP143), inference(usedef, [], [e153])).
fof(e153, plain, (sP143 <=> (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP143])])).
fof(f154, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP144), inference(usedef, [], [e154])).
fof(e154, plain, (sP144 <=> (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP144])])).
fof(f155, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP145), inference(usedef, [], [e155])).
fof(e155, plain, (sP145 <=> (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP145])])).
fof(f156, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP146), inference(usedef, [], [e156])).
fof(e156, plain, (sP146 <=> (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP146])])).
fof(f157, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP147), inference(usedef, [], [e157])).
fof(e157, plain, (sP147 <=> (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP147])])).
fof(f158, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP148), inference(usedef, [], [e158])).
fof(e158, plain, (sP148 <=> (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP148])])).
fof(f159, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP149), inference(usedef, [], [e159])).
fof(e159, plain, (sP149 <=> (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP149])])).
fof(f160, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP150), inference(usedef, [], [e160])).
fof(e160, plain, (sP150 <=> (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP150])])).
fof(f161, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP151), inference(usedef, [], [e161])).
fof(e161, plain, (sP151 <=> (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP151])])).
fof(f162, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP152), inference(usedef, [], [e162])).
fof(e162, plain, (sP152 <=> (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP152])])).
fof(f163, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP153), inference(usedef, [], [e163])).
fof(e163, plain, (sP153 <=> (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP153])])).
fof(f9, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG058+1.p', co1)).
fof(f2609, plain, (spl154_154 | spl154_153 | spl154_152 | spl154_151 | spl154_150 | spl154_149 | spl154_148 | spl154_147 | spl154_146 | spl154_145 | spl154_144 | spl154_143 | spl154_142 | spl154_141 | spl154_140 | spl154_139 | spl154_138 | spl154_137 | spl154_136 | spl154_135 | spl154_134 | spl154_133 | spl154_132 | spl154_131 | spl154_5), inference(avatar_split_clause, [], [f967, f987, f1570, f1577, f1584, f1591, f1598, f1605, f1612, f1619, f1626, f1633, f1640, f1647, f1654, f1661, f1668, f1675, f1682, f1689, f1696, f1703, f1710, f1717, f1724, f1731])).
fof(f1731, plain, (spl154_154 <=> sP130), introduced(avatar_definition, [new_symbols(naming, [spl154_154])])).
fof(f1724, plain, (spl154_153 <=> sP131), introduced(avatar_definition, [new_symbols(naming, [spl154_153])])).
fof(f1717, plain, (spl154_152 <=> sP132), introduced(avatar_definition, [new_symbols(naming, [spl154_152])])).
fof(f1710, plain, (spl154_151 <=> sP133), introduced(avatar_definition, [new_symbols(naming, [spl154_151])])).
fof(f1703, plain, (spl154_150 <=> sP134), introduced(avatar_definition, [new_symbols(naming, [spl154_150])])).
fof(f1696, plain, (spl154_149 <=> sP135), introduced(avatar_definition, [new_symbols(naming, [spl154_149])])).
fof(f1689, plain, (spl154_148 <=> sP136), introduced(avatar_definition, [new_symbols(naming, [spl154_148])])).
fof(f1682, plain, (spl154_147 <=> sP137), introduced(avatar_definition, [new_symbols(naming, [spl154_147])])).
fof(f1675, plain, (spl154_146 <=> sP138), introduced(avatar_definition, [new_symbols(naming, [spl154_146])])).
fof(f1668, plain, (spl154_145 <=> sP139), introduced(avatar_definition, [new_symbols(naming, [spl154_145])])).
fof(f1661, plain, (spl154_144 <=> sP140), introduced(avatar_definition, [new_symbols(naming, [spl154_144])])).
fof(f1654, plain, (spl154_143 <=> sP141), introduced(avatar_definition, [new_symbols(naming, [spl154_143])])).
fof(f1647, plain, (spl154_142 <=> sP142), introduced(avatar_definition, [new_symbols(naming, [spl154_142])])).
fof(f1640, plain, (spl154_141 <=> sP143), introduced(avatar_definition, [new_symbols(naming, [spl154_141])])).
fof(f1633, plain, (spl154_140 <=> sP144), introduced(avatar_definition, [new_symbols(naming, [spl154_140])])).
fof(f1626, plain, (spl154_139 <=> sP145), introduced(avatar_definition, [new_symbols(naming, [spl154_139])])).
fof(f1619, plain, (spl154_138 <=> sP146), introduced(avatar_definition, [new_symbols(naming, [spl154_138])])).
fof(f1612, plain, (spl154_137 <=> sP147), introduced(avatar_definition, [new_symbols(naming, [spl154_137])])).
fof(f1605, plain, (spl154_136 <=> sP148), introduced(avatar_definition, [new_symbols(naming, [spl154_136])])).
fof(f1598, plain, (spl154_135 <=> sP149), introduced(avatar_definition, [new_symbols(naming, [spl154_135])])).
fof(f1591, plain, (spl154_134 <=> sP150), introduced(avatar_definition, [new_symbols(naming, [spl154_134])])).
fof(f1584, plain, (spl154_133 <=> sP151), introduced(avatar_definition, [new_symbols(naming, [spl154_133])])).
fof(f1577, plain, (spl154_132 <=> sP152), introduced(avatar_definition, [new_symbols(naming, [spl154_132])])).
fof(f1570, plain, (spl154_131 <=> sP153), introduced(avatar_definition, [new_symbols(naming, [spl154_131])])).
fof(f967, plain, ((e4 = op(e4, e4)) | sP153 | sP152 | sP151 | sP150 | sP149 | sP148 | sP147 | sP146 | sP145 | sP144 | sP143 | sP142 | sP141 | sP140 | sP139 | sP138 | sP137 | sP136 | sP135 | sP134 | sP133 | sP132 | sP131 | sP130), inference(cnf_transformation, [], [f164])).
fof(f2595, plain, (~ spl154_278 | ~ spl154_91 | ~ spl154_126), inference(avatar_split_clause, [], [f953, f1496, f1349, f2348])).
fof(f2348, plain, (spl154_278 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl154_278])])).
fof(f953, plain, (~ (e0 = unit) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f312])).
fof(f312, plain, ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f2561, plain, (~ spl154_258 | ~ spl154_122 | spl154_116), inference(avatar_split_clause, [], [f914, f1454, f1479, f2249])).
fof(f2249, plain, (spl154_258 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl154_258])])).
fof(f1479, plain, (spl154_122 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl154_122])])).
fof(f914, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1) | ~ sP25), inference(cnf_transformation, [], [f293])).
fof(f293, plain, ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1) | ~ sP25), inference(nnf_transformation, [], [f35])).
fof(f2498, plain, (~ spl154_222 | ~ spl154_73 | spl154_61), inference(avatar_split_clause, [], [f844, f1223, f1273, f2070])).
fof(f2070, plain, (spl154_222 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl154_222])])).
fof(f844, plain, ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0)) | ~ sP60), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0)) | ~ sP60), inference(nnf_transformation, [], [f70])).
fof(f2444, plain, (~ spl154_191 | ~ spl154_49 | spl154_31), inference(avatar_split_clause, [], [f784, f1097, f1172, f1916])).
fof(f1916, plain, (spl154_191 <=> sP90), introduced(avatar_definition, [new_symbols(naming, [spl154_191])])).
fof(f784, plain, ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP90), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0)) | ~ sP90), inference(nnf_transformation, [], [f100])).
fof(f2393, plain, (~ spl154_162 | ~ spl154_35 | spl154_29), inference(avatar_split_clause, [], [f728, f1088, f1113, f1772])).
fof(f1772, plain, (spl154_162 <=> sP118), introduced(avatar_definition, [new_symbols(naming, [spl154_162])])).
fof(f728, plain, ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(nnf_transformation, [], [f128])).
fof(f2392, plain, (~ spl154_162 | ~ spl154_35 | ~ spl154_130), inference(avatar_split_clause, [], [f729, f1512, f1113, f1772])).
fof(f729, plain, (~ (e4 = unit) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(cnf_transformation, [], [f200])).
fof(f2351, plain, (~ spl154_259 | spl154_278), inference(avatar_split_clause, [], [f697, f2348, f2254])).
fof(f697, plain, (sP6 | ~ sP125), inference(cnf_transformation, [], [f193])).
fof(f193, plain, ((sP24 & sP23 & sP22 & sP21 & sP20 & sP19 & sP18 & sP17 & sP16 & sP15 & sP14 & sP13 & sP12 & sP11 & sP10 & sP9 & sP8 & sP7 & sP6 & sP5 & sP4 & sP3 & sP2 & sP1 & sP0) | ~ sP125), inference(nnf_transformation, [], [f135])).
fof(f2252, plain, (~ spl154_233 | spl154_258), inference(avatar_split_clause, [], [f666, f2249, f2125])).
fof(f666, plain, (sP25 | ~ sP126), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ((sP49 & sP48 & sP47 & sP46 & sP45 & sP44 & sP43 & sP42 & sP41 & sP40 & sP39 & sP38 & sP37 & sP36 & sP35 & sP34 & sP33 & sP32 & sP31 & sP30 & sP29 & sP28 & sP27 & sP26 & sP25) | ~ sP126), inference(nnf_transformation, [], [f136])).
fof(f2073, plain, (~ spl154_207 | spl154_222), inference(avatar_split_clause, [], [f651, f2070, f1996])).
fof(f651, plain, (sP60 | ~ sP127), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ((sP74 & sP73 & sP72 & sP71 & sP70 & sP69 & sP68 & sP67 & sP66 & sP65 & sP64 & sP63 & sP62 & sP61 & sP60 & sP59 & sP58 & sP57 & sP56 & sP55 & sP54 & sP53 & sP52 & sP51 & sP50) | ~ sP127), inference(nnf_transformation, [], [f137])).
fof(f1919, plain, (~ spl154_181 | spl154_191), inference(avatar_split_clause, [], [f631, f1916, f1867])).
fof(f631, plain, (sP90 | ~ sP128), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ((sP99 & sP98 & sP97 & sP96 & sP95 & sP94 & sP93 & sP92 & sP91 & sP90 & sP89 & sP88 & sP87 & sP86 & sP85 & sP84 & sP83 & sP82 & sP81 & sP80 & sP79 & sP78 & sP77 & sP76 & sP75) | ~ sP128), inference(nnf_transformation, [], [f138])).
fof(f1775, plain, (~ spl154_155 | spl154_162), inference(avatar_split_clause, [], [f609, f1772, f1738])).
fof(f609, plain, (sP118 | ~ sP129), inference(cnf_transformation, [], [f189])).
fof(f189, plain, ((sP124 & sP123 & sP122 & sP121 & sP120 & sP119 & sP118 & sP117 & sP116 & sP115 & sP114 & sP113 & sP112 & sP111 & sP110 & sP109 & sP108 & sP107 & sP106 & sP105 & sP104 & sP103 & sP102 & sP101 & sP100) | ~ sP129), inference(nnf_transformation, [], [f139])).
fof(f1734, plain, (~ spl154_154 | ~ spl154_121), inference(avatar_split_clause, [], [f590, f1475, f1731])).
fof(f590, plain, (~ (e0 = op(e0, e0)) | ~ sP130), inference(cnf_transformation, [], [f188])).
fof(f188, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP130), inference(nnf_transformation, [], [f140])).
fof(f1729, plain, (~ spl154_153 | spl154_122), inference(avatar_split_clause, [], [f585, f1479, f1724])).
fof(f585, plain, ((op(e0, e0) = e1) | ~ sP131), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP131), inference(nnf_transformation, [], [f141])).
fof(f1721, plain, (~ spl154_152 | spl154_61), inference(avatar_split_clause, [], [f583, f1223, f1717])).
fof(f583, plain, ((e0 = op(e2, e2)) | ~ sP132), inference(cnf_transformation, [], [f186])).
fof(f186, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP132), inference(nnf_transformation, [], [f142])).
fof(f1714, plain, (~ spl154_151 | spl154_31), inference(avatar_split_clause, [], [f580, f1097, f1710])).
fof(f580, plain, ((e0 = op(e3, e3)) | ~ sP133), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP133), inference(nnf_transformation, [], [f143])).
fof(f1708, plain, (~ spl154_150 | spl154_125), inference(avatar_split_clause, [], [f576, f1491, f1703])).
fof(f576, plain, ((op(e0, e0) = e4) | ~ sP134), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP134), inference(nnf_transformation, [], [f144])).
fof(f1699, plain, (~ spl154_149 | ~ spl154_97), inference(avatar_split_clause, [], [f575, f1374, f1696])).
fof(f575, plain, (~ (e1 = op(e1, e0)) | ~ sP135), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP135), inference(nnf_transformation, [], [f145])).
fof(f1694, plain, (~ spl154_148 | spl154_92), inference(avatar_split_clause, [], [f570, f1353, f1689])).
fof(f1353, plain, (spl154_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_92])])).
fof(f570, plain, ((e1 = op(e1, e1)) | ~ sP136), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP136), inference(nnf_transformation, [], [f146])).
fof(f1692, plain, (~ spl154_148 | ~ spl154_92), inference(avatar_split_clause, [], [f572, f1353, f1689])).
fof(f572, plain, (~ (e1 = op(e1, e1)) | ~ sP136), inference(cnf_transformation, [], [f182])).
fof(f1687, plain, (~ spl154_147 | spl154_93), inference(avatar_split_clause, [], [f567, f1357, f1682])).
fof(f567, plain, ((e2 = op(e1, e1)) | ~ sP137), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP137), inference(nnf_transformation, [], [f147])).
fof(f1680, plain, (~ spl154_146 | spl154_94), inference(avatar_split_clause, [], [f564, f1361, f1675])).
fof(f564, plain, ((e3 = op(e1, e1)) | ~ sP138), inference(cnf_transformation, [], [f180])).
fof(f180, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP138), inference(nnf_transformation, [], [f148])).
fof(f1673, plain, (~ spl154_145 | spl154_95), inference(avatar_split_clause, [], [f561, f1365, f1668])).
fof(f561, plain, ((e4 = op(e1, e1)) | ~ sP139), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP139), inference(nnf_transformation, [], [f149])).
fof(f1666, plain, (~ spl154_144 | spl154_61), inference(avatar_split_clause, [], [f558, f1223, f1661])).
fof(f558, plain, ((e0 = op(e2, e2)) | ~ sP140), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP140), inference(nnf_transformation, [], [f150])).
fof(f1658, plain, (~ spl154_143 | spl154_93), inference(avatar_split_clause, [], [f556, f1357, f1654])).
fof(f556, plain, ((e2 = op(e1, e1)) | ~ sP141), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP141), inference(nnf_transformation, [], [f151])).
fof(f1652, plain, (~ spl154_142 | spl154_63), inference(avatar_split_clause, [], [f552, f1231, f1647])).
fof(f552, plain, ((e2 = op(e2, e2)) | ~ sP142), inference(cnf_transformation, [], [f176])).
fof(f176, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP142), inference(nnf_transformation, [], [f152])).
fof(f1644, plain, (~ spl154_141 | spl154_33), inference(avatar_split_clause, [], [f550, f1105, f1640])).
fof(f550, plain, ((e2 = op(e3, e3)) | ~ sP143), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP143), inference(nnf_transformation, [], [f153])).
fof(f1638, plain, (~ spl154_140 | spl154_65), inference(avatar_split_clause, [], [f546, f1239, f1633])).
fof(f546, plain, ((e4 = op(e2, e2)) | ~ sP144), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP144), inference(nnf_transformation, [], [f154])).
fof(f1631, plain, (~ spl154_139 | spl154_31), inference(avatar_split_clause, [], [f543, f1097, f1626])).
fof(f543, plain, ((e0 = op(e3, e3)) | ~ sP145), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP145), inference(nnf_transformation, [], [f155])).
fof(f1623, plain, (~ spl154_138 | spl154_94), inference(avatar_split_clause, [], [f541, f1361, f1619])).
fof(f541, plain, ((e3 = op(e1, e1)) | ~ sP146), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP146), inference(nnf_transformation, [], [f156])).
fof(f1617, plain, (~ spl154_137 | spl154_33), inference(avatar_split_clause, [], [f537, f1105, f1612])).
fof(f537, plain, ((e2 = op(e3, e3)) | ~ sP147), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP147), inference(nnf_transformation, [], [f157])).
fof(f1610, plain, (~ spl154_136 | spl154_34), inference(avatar_split_clause, [], [f534, f1109, f1605])).
fof(f534, plain, ((e3 = op(e3, e3)) | ~ sP148), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP148), inference(nnf_transformation, [], [f158])).
fof(f1602, plain, (~ spl154_135 | spl154_4), inference(avatar_split_clause, [], [f532, f983, f1598])).
fof(f532, plain, ((e3 = op(e4, e4)) | ~ sP149), inference(cnf_transformation, [], [f169])).
fof(f169, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP149), inference(nnf_transformation, [], [f159])).
fof(f1594, plain, (~ spl154_134 | ~ spl154_25), inference(avatar_split_clause, [], [f530, f1071, f1591])).
fof(f530, plain, (~ (e4 = op(e4, e0)) | ~ sP150), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP150), inference(nnf_transformation, [], [f160])).
fof(f1588, plain, (~ spl154_133 | spl154_95), inference(avatar_split_clause, [], [f526, f1365, f1584])).
fof(f526, plain, ((e4 = op(e1, e1)) | ~ sP151), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP151), inference(nnf_transformation, [], [f161])).
fof(f1581, plain, (~ spl154_132 | spl154_65), inference(avatar_split_clause, [], [f523, f1239, f1577])).
fof(f523, plain, ((e4 = op(e2, e2)) | ~ sP152), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP152), inference(nnf_transformation, [], [f162])).
fof(f1575, plain, (~ spl154_131 | spl154_4), inference(avatar_split_clause, [], [f519, f983, f1570])).
fof(f519, plain, ((e3 = op(e4, e4)) | ~ sP153), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP153), inference(nnf_transformation, [], [f163])).
fof(f1568, plain, spl154_91, inference(avatar_split_clause, [], [f1567, f1349])).
fof(f1567, plain, (e0 = op(e1, e1)), inference(forward_demodulation, [], [f515, f516])).
fof(f515, plain, (e0 = op(op(op(e2, e2), e2), op(op(e2, e2), e2))), inference(cnf_transformation, [], [f6])).
fof(f1566, plain, spl154_64, inference(avatar_split_clause, [], [f517, f1235])).
fof(f517, plain, (e3 = op(e2, e2)), inference(cnf_transformation, [], [f6])).
fof(f1549, plain, (spl154_99 | spl154_94 | spl154_89 | spl154_84 | spl154_79), inference(avatar_split_clause, [], [f371, f1298, f1319, f1340, f1361, f1382])).
fof(f371, plain, ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG058+1.p', ax3)).
fof(f1544, plain, (spl154_111 | spl154_86 | spl154_61 | spl154_36 | spl154_11), inference(avatar_split_clause, [], [f376, f1013, f1118, f1223, f1328, f1433])).
fof(f376, plain, ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f1515, plain, (spl154_126 | spl154_127 | spl154_128 | spl154_129 | spl154_130), inference(avatar_split_clause, [], [f354, f1512, f1508, f1504, f1500, f1496])).
fof(f354, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).