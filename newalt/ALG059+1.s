fof(f4082, plain, $false, inference(avatar_sat_refutation, [], [f1331, f1352, f1520, f1523, f1526, f1537, f1540, f1548, f1549, f1550, f1571, f1573, f1587, f1606, f1625, f1649, f1663, f1675, f1689, f1708, f1732, f1746, f1761, f1784, f1803, f1818, f1833, f1848, f1859, f1868, f1893, f1908, f1923, f1938, f1949, f1988, f2132, f2286, f2350, f2380, f2395, f2400, f2564, f2605, f2606, f2657, f2711, f2733, f2744, f2749, f2751, f2808, f2829, f2830, f2851, f2853, f2854, f2865, f2869, f2898, f2920, f2950, f2951, f2953, f2954, f3026, f3027, f3028, f3143, f3149, f3186, f3239, f3267, f3285, f3288, f3300, f3310, f3313, f3322, f3368, f3379, f3417, f3447, f3448, f3481, f3512, f3515, f3574, f3609, f3618, f3675, f3676, f3718, f3723, f3767, f3868, f3869, f3870, f3871, f3872, f3875, f3876, f3879, f3889, f3898, f3937, f3953, f3992, f3996, f4005, f4013, f4021, f4028, f4031, f4038, f4045, f4052, f4059, f4071, f4074, f4078])).
fof(f4078, plain, (~ spl154_91 | ~ spl154_97 | spl154_190), inference(avatar_contradiction_clause, [], [f4077])).
fof(f4077, plain, ($false | (~ spl154_91 | ~ spl154_97 | spl154_190)), inference(subsumption_resolution, [], [f4076, f1356])).
fof(f1356, plain, ((e0 = op(e1, e1)) | ~ spl154_91), inference(avatar_component_clause, [], [f1354])).
fof(f1354, plain, (spl154_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_91])])).
fof(f4076, plain, (~ (e0 = op(e1, e1)) | (~ spl154_97 | spl154_190)), inference(forward_demodulation, [], [f1867, f1381])).
fof(f1381, plain, ((e1 = op(e1, e0)) | ~ spl154_97), inference(avatar_component_clause, [], [f1379])).
fof(f1379, plain, (spl154_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_97])])).
fof(f1867, plain, (~ (e0 = op(op(e1, e0), e1)) | spl154_190), inference(avatar_component_clause, [], [f1865])).
fof(f1865, plain, (spl154_190 <=> (e0 = op(op(e1, e0), e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_190])])).
fof(f4074, plain, (~ spl154_97 | ~ spl154_117 | spl154_192), inference(avatar_contradiction_clause, [], [f4073])).
fof(f4073, plain, ($false | (~ spl154_97 | ~ spl154_117 | spl154_192)), inference(subsumption_resolution, [], [f4072, f1465])).
fof(f1465, plain, ((e1 = op(e0, e1)) | ~ spl154_117), inference(avatar_component_clause, [], [f1463])).
fof(f1463, plain, (spl154_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_117])])).
fof(f4072, plain, (~ (e1 = op(e0, e1)) | (~ spl154_97 | spl154_192)), inference(forward_demodulation, [], [f1877, f1381])).
fof(f1877, plain, (~ (op(e0, e1) = op(e1, e0)) | spl154_192), inference(avatar_component_clause, [], [f1875])).
fof(f1875, plain, (spl154_192 <=> (op(e0, e1) = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_192])])).
fof(f4071, plain, (~ spl154_70 | ~ spl154_90 | spl154_173), inference(avatar_contradiction_clause, [], [f4070])).
fof(f4070, plain, ($false | (~ spl154_70 | ~ spl154_90 | spl154_173)), inference(subsumption_resolution, [], [f4069, f1351])).
fof(f1351, plain, ((e4 = op(e1, e2)) | ~ spl154_90), inference(avatar_component_clause, [], [f1349])).
fof(f1349, plain, (spl154_90 <=> (e4 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_90])])).
fof(f4069, plain, (~ (e4 = op(e1, e2)) | (~ spl154_70 | spl154_173)), inference(forward_demodulation, [], [f1783, f1267])).
fof(f1267, plain, ((e4 = op(e2, e1)) | ~ spl154_70), inference(avatar_component_clause, [], [f1265])).
fof(f1265, plain, (spl154_70 <=> (e4 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_70])])).
fof(f1783, plain, (~ (op(e1, e2) = op(e2, e1)) | spl154_173), inference(avatar_component_clause, [], [f1781])).
fof(f1781, plain, (spl154_173 <=> (op(e1, e2) = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_173])])).
fof(f4059, plain, (~ spl154_73 | ~ spl154_113 | spl154_177), inference(avatar_contradiction_clause, [], [f4058])).
fof(f4058, plain, ($false | (~ spl154_73 | ~ spl154_113 | spl154_177)), inference(subsumption_resolution, [], [f4057, f1448])).
fof(f1448, plain, ((e2 = op(e0, e2)) | ~ spl154_113), inference(avatar_component_clause, [], [f1446])).
fof(f1446, plain, (spl154_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_113])])).
fof(f4057, plain, (~ (e2 = op(e0, e2)) | (~ spl154_73 | spl154_177)), inference(forward_demodulation, [], [f1802, f1280])).
fof(f1280, plain, ((e2 = op(e2, e0)) | ~ spl154_73), inference(avatar_component_clause, [], [f1278])).
fof(f1278, plain, (spl154_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_73])])).
fof(f1802, plain, (~ (op(e0, e2) = op(e2, e0)) | spl154_177), inference(avatar_component_clause, [], [f1800])).
fof(f1800, plain, (spl154_177 <=> (op(e0, e2) = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_177])])).
fof(f4052, plain, (~ spl154_56 | spl154_108 | ~ spl154_169), inference(avatar_contradiction_clause, [], [f4051])).
fof(f4051, plain, ($false | (~ spl154_56 | spl154_108 | ~ spl154_169)), inference(subsumption_resolution, [], [f4050, f1426])).
fof(f1426, plain, (~ (e2 = op(e0, e3)) | spl154_108), inference(avatar_component_clause, [], [f1425])).
fof(f1425, plain, (spl154_108 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_108])])).
fof(f4050, plain, ((e2 = op(e0, e3)) | (~ spl154_56 | ~ spl154_169)), inference(forward_demodulation, [], [f1760, f1209])).
fof(f1209, plain, ((e0 = op(e2, e3)) | ~ spl154_56), inference(avatar_component_clause, [], [f1207])).
fof(f1207, plain, (spl154_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_56])])).
fof(f1760, plain, ((e2 = op(op(e2, e3), e3)) | ~ spl154_169), inference(avatar_component_clause, [], [f1758])).
fof(f1758, plain, (spl154_169 <=> (e2 = op(op(e2, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_169])])).
fof(f4045, plain, (~ spl154_52 | spl154_78 | ~ spl154_166), inference(avatar_contradiction_clause, [], [f4044])).
fof(f4044, plain, ($false | (~ spl154_52 | spl154_78 | ~ spl154_166)), inference(subsumption_resolution, [], [f4043, f1300])).
fof(f1300, plain, (~ (e2 = op(e1, e4)) | spl154_78), inference(avatar_component_clause, [], [f1299])).
fof(f1299, plain, (spl154_78 <=> (e2 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_78])])).
fof(f4043, plain, ((e2 = op(e1, e4)) | (~ spl154_52 | ~ spl154_166)), inference(forward_demodulation, [], [f1745, f1192])).
fof(f1192, plain, ((e1 = op(e2, e4)) | ~ spl154_52), inference(avatar_component_clause, [], [f1190])).
fof(f1190, plain, (spl154_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_52])])).
fof(f1745, plain, ((e2 = op(op(e2, e4), e4)) | ~ spl154_166), inference(avatar_component_clause, [], [f1743])).
fof(f1743, plain, (spl154_166 <=> (e2 = op(op(e2, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_166])])).
fof(f4038, plain, (~ spl154_49 | ~ spl154_109 | spl154_163), inference(avatar_contradiction_clause, [], [f4037])).
fof(f4037, plain, ($false | (~ spl154_49 | ~ spl154_109 | spl154_163)), inference(subsumption_resolution, [], [f4036, f1431])).
fof(f1431, plain, ((e3 = op(e0, e3)) | ~ spl154_109), inference(avatar_component_clause, [], [f1429])).
fof(f1429, plain, (spl154_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_109])])).
fof(f4036, plain, (~ (e3 = op(e0, e3)) | (~ spl154_49 | spl154_163)), inference(forward_demodulation, [], [f1731, f1179])).
fof(f1179, plain, ((e3 = op(e3, e0)) | ~ spl154_49), inference(avatar_component_clause, [], [f1177])).
fof(f1177, plain, (spl154_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_49])])).
fof(f1731, plain, (~ (op(e0, e3) = op(e3, e0)) | spl154_163), inference(avatar_component_clause, [], [f1729])).
fof(f1729, plain, (spl154_163 <=> (op(e0, e3) = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_163])])).
fof(f4031, plain, (~ spl154_43 | spl154_69 | ~ spl154_158), inference(avatar_contradiction_clause, [], [f4030])).
fof(f4030, plain, ($false | (~ spl154_43 | spl154_69 | ~ spl154_158)), inference(subsumption_resolution, [], [f4029, f1262])).
fof(f1262, plain, (~ (e3 = op(e2, e1)) | spl154_69), inference(avatar_component_clause, [], [f1261])).
fof(f1261, plain, (spl154_69 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_69])])).
fof(f4029, plain, ((e3 = op(e2, e1)) | (~ spl154_43 | ~ spl154_158)), inference(forward_demodulation, [], [f1707, f1154])).
fof(f1154, plain, ((e2 = op(e3, e1)) | ~ spl154_43), inference(avatar_component_clause, [], [f1152])).
fof(f1152, plain, (spl154_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_43])])).
fof(f1707, plain, ((e3 = op(op(e3, e1), e1)) | ~ spl154_158), inference(avatar_component_clause, [], [f1705])).
fof(f1705, plain, (spl154_158 <=> (e3 = op(op(e3, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_158])])).
fof(f4028, plain, (~ spl154_43 | ~ spl154_83 | spl154_159), inference(avatar_contradiction_clause, [], [f4027])).
fof(f4027, plain, ($false | (~ spl154_43 | ~ spl154_83 | spl154_159)), inference(subsumption_resolution, [], [f4026, f1322])).
fof(f1322, plain, ((e2 = op(e1, e3)) | ~ spl154_83), inference(avatar_component_clause, [], [f1320])).
fof(f1320, plain, (spl154_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_83])])).
fof(f4026, plain, (~ (e2 = op(e1, e3)) | (~ spl154_43 | spl154_159)), inference(forward_demodulation, [], [f1712, f1154])).
fof(f1712, plain, (~ (op(e1, e3) = op(e3, e1)) | spl154_159), inference(avatar_component_clause, [], [f1710])).
fof(f1710, plain, (spl154_159 <=> (op(e1, e3) = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_159])])).
fof(f4021, plain, (~ spl154_37 | spl154_89 | ~ spl154_154), inference(avatar_contradiction_clause, [], [f4020])).
fof(f4020, plain, ($false | (~ spl154_37 | spl154_89 | ~ spl154_154)), inference(subsumption_resolution, [], [f4019, f1346])).
fof(f1346, plain, (~ (e3 = op(e1, e2)) | spl154_89), inference(avatar_component_clause, [], [f1345])).
fof(f1345, plain, (spl154_89 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_89])])).
fof(f4019, plain, ((e3 = op(e1, e2)) | (~ spl154_37 | ~ spl154_154)), inference(forward_demodulation, [], [f1688, f1129])).
fof(f1129, plain, ((e1 = op(e3, e2)) | ~ spl154_37), inference(avatar_component_clause, [], [f1127])).
fof(f1127, plain, (spl154_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_37])])).
fof(f1688, plain, ((e3 = op(op(e3, e2), e2)) | ~ spl154_154), inference(avatar_component_clause, [], [f1686])).
fof(f1686, plain, (spl154_154 <=> (e3 = op(op(e3, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_154])])).
fof(f4013, plain, (~ spl154_26 | spl154_104 | ~ spl154_149), inference(avatar_contradiction_clause, [], [f4012])).
fof(f4012, plain, ($false | (~ spl154_26 | spl154_104 | ~ spl154_149)), inference(subsumption_resolution, [], [f4011, f1409])).
fof(f1409, plain, (~ (e3 = op(e0, e4)) | spl154_104), inference(avatar_component_clause, [], [f1408])).
fof(f1408, plain, (spl154_104 <=> (e3 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_104])])).
fof(f4011, plain, ((e3 = op(e0, e4)) | (~ spl154_26 | ~ spl154_149)), inference(forward_demodulation, [], [f1662, f1083])).
fof(f1083, plain, ((e0 = op(e3, e4)) | ~ spl154_26), inference(avatar_component_clause, [], [f1081])).
fof(f1081, plain, (spl154_26 <=> (e0 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_26])])).
fof(f1662, plain, ((e3 = op(op(e3, e4), e4)) | ~ spl154_149), inference(avatar_component_clause, [], [f1660])).
fof(f1660, plain, (spl154_149 <=> (e3 = op(op(e3, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_149])])).
fof(f4005, plain, (~ spl154_25 | ~ spl154_105 | spl154_146), inference(avatar_contradiction_clause, [], [f4004])).
fof(f4004, plain, ($false | (~ spl154_25 | ~ spl154_105 | spl154_146)), inference(subsumption_resolution, [], [f4003, f1414])).
fof(f1414, plain, ((e4 = op(e0, e4)) | ~ spl154_105), inference(avatar_component_clause, [], [f1412])).
fof(f1412, plain, (spl154_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_105])])).
fof(f4003, plain, (~ (e4 = op(e0, e4)) | (~ spl154_25 | spl154_146)), inference(forward_demodulation, [], [f1648, f1078])).
fof(f1078, plain, ((e4 = op(e4, e0)) | ~ spl154_25), inference(avatar_component_clause, [], [f1076])).
fof(f1076, plain, (spl154_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_25])])).
fof(f1648, plain, (~ (op(e0, e4) = op(e4, e0)) | spl154_146), inference(avatar_component_clause, [], [f1646])).
fof(f1646, plain, (spl154_146 <=> (op(e0, e4) = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_146])])).
fof(f3996, plain, (~ spl154_19 | ~ spl154_43 | ~ spl154_141), inference(avatar_contradiction_clause, [], [f3995])).
fof(f3995, plain, ($false | (~ spl154_19 | ~ spl154_43 | ~ spl154_141)), inference(subsumption_resolution, [], [f3994, f513])).
fof(f513, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', ax5)).
fof(f3994, plain, ((e2 = e4) | (~ spl154_19 | ~ spl154_43 | ~ spl154_141)), inference(forward_demodulation, [], [f3993, f1154])).
fof(f3993, plain, ((e4 = op(e3, e1)) | (~ spl154_19 | ~ spl154_141)), inference(forward_demodulation, [], [f1624, f1053])).
fof(f1053, plain, ((e3 = op(e4, e1)) | ~ spl154_19), inference(avatar_component_clause, [], [f1051])).
fof(f1051, plain, (spl154_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_19])])).
fof(f1624, plain, ((e4 = op(op(e4, e1), e1)) | ~ spl154_141), inference(avatar_component_clause, [], [f1622])).
fof(f1622, plain, (spl154_141 <=> (e4 = op(op(e4, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_141])])).
fof(f3992, plain, (~ spl154_19 | ~ spl154_79 | spl154_142), inference(avatar_contradiction_clause, [], [f3991])).
fof(f3991, plain, ($false | (~ spl154_19 | ~ spl154_79 | spl154_142)), inference(subsumption_resolution, [], [f3990, f1305])).
fof(f1305, plain, ((e3 = op(e1, e4)) | ~ spl154_79), inference(avatar_component_clause, [], [f1303])).
fof(f1303, plain, (spl154_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_79])])).
fof(f3990, plain, (~ (e3 = op(e1, e4)) | (~ spl154_19 | spl154_142)), inference(forward_demodulation, [], [f1629, f1053])).
fof(f1629, plain, (~ (op(e1, e4) = op(e4, e1)) | spl154_142), inference(avatar_component_clause, [], [f1627])).
fof(f1627, plain, (spl154_142 <=> (op(e1, e4) = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_142])])).
fof(f3953, plain, (~ spl154_11 | ~ spl154_113 | ~ spl154_137), inference(avatar_contradiction_clause, [], [f3952])).
fof(f3952, plain, ($false | (~ spl154_11 | ~ spl154_113 | ~ spl154_137)), inference(subsumption_resolution, [], [f3951, f513])).
fof(f3951, plain, ((e2 = e4) | (~ spl154_11 | ~ spl154_113 | ~ spl154_137)), inference(forward_demodulation, [], [f3950, f1448])).
fof(f3950, plain, ((e4 = op(e0, e2)) | (~ spl154_11 | ~ spl154_137)), inference(backward_demodulation, [], [f1605, f1020])).
fof(f1020, plain, ((e0 = op(e4, e2)) | ~ spl154_11), inference(avatar_component_clause, [], [f1018])).
fof(f1018, plain, (spl154_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_11])])).
fof(f1605, plain, ((e4 = op(op(e4, e2), e2)) | ~ spl154_137), inference(avatar_component_clause, [], [f1603])).
fof(f1603, plain, (spl154_137 <=> (e4 = op(op(e4, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_137])])).
fof(f3937, plain, (~ spl154_29 | ~ spl154_49), inference(avatar_split_clause, [], [f3935, f1177, f1093])).
fof(f1093, plain, (spl154_29 <=> (e3 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_29])])).
fof(f3935, plain, (~ (e3 = op(e3, e4)) | ~ spl154_49), inference(backward_demodulation, [], [f488, f1179])).
fof(f488, plain, ~ (op(e3, e0) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', ax4)).
fof(f3898, plain, (~ spl154_88 | ~ spl154_113), inference(avatar_split_clause, [], [f3894, f1446, f1341])).
fof(f1341, plain, (spl154_88 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_88])])).
fof(f3894, plain, (~ (e2 = op(e1, e2)) | ~ spl154_113), inference(backward_demodulation, [], [f425, f1448])).
fof(f425, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f3889, plain, (~ spl154_46 | ~ spl154_121), inference(avatar_split_clause, [], [f3882, f1480, f1165])).
fof(f1165, plain, (spl154_46 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_46])])).
fof(f1480, plain, (spl154_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_121])])).
fof(f3882, plain, (~ (e0 = op(e3, e0)) | ~ spl154_121), inference(backward_demodulation, [], [f407, f1482])).
fof(f1482, plain, ((e0 = op(e0, e0)) | ~ spl154_121), inference(avatar_component_clause, [], [f1480])).
fof(f407, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f3879, plain, (~ spl154_23 | ~ spl154_126), inference(avatar_contradiction_clause, [], [f3878])).
fof(f3878, plain, ($false | (~ spl154_23 | ~ spl154_126)), inference(subsumption_resolution, [], [f3877, f513])).
fof(f3877, plain, ((e2 = e4) | (~ spl154_23 | ~ spl154_126)), inference(forward_demodulation, [], [f3862, f1070])).
fof(f1070, plain, ((e2 = op(e4, e0)) | ~ spl154_23), inference(avatar_component_clause, [], [f1068])).
fof(f1068, plain, (spl154_23 <=> (e2 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_23])])).
fof(f3862, plain, ((e4 = op(e4, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f353, f1503])).
fof(f1503, plain, ((e0 = unit) | ~ spl154_126), inference(avatar_component_clause, [], [f1501])).
fof(f1501, plain, (spl154_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_126])])).
fof(f353, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', ax2)).
fof(f3876, plain, (spl154_49 | ~ spl154_126), inference(avatar_split_clause, [], [f3860, f1501, f1177])).
fof(f3860, plain, ((e3 = op(e3, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f351, f1503])).
fof(f351, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f3875, plain, (~ spl154_108 | ~ spl154_126), inference(avatar_contradiction_clause, [], [f3874])).
fof(f3874, plain, ($false | (~ spl154_108 | ~ spl154_126)), inference(subsumption_resolution, [], [f3873, f512])).
fof(f512, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f3873, plain, ((e2 = e3) | (~ spl154_108 | ~ spl154_126)), inference(forward_demodulation, [], [f3859, f1427])).
fof(f1427, plain, ((e2 = op(e0, e3)) | ~ spl154_108), inference(avatar_component_clause, [], [f1425])).
fof(f3859, plain, ((e3 = op(e0, e3)) | ~ spl154_126), inference(backward_demodulation, [], [f350, f1503])).
fof(f350, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f3872, plain, (spl154_73 | ~ spl154_126), inference(avatar_split_clause, [], [f3858, f1501, f1278])).
fof(f3858, plain, ((e2 = op(e2, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f349, f1503])).
fof(f349, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f3871, plain, (spl154_113 | ~ spl154_126), inference(avatar_split_clause, [], [f3857, f1501, f1446])).
fof(f3857, plain, ((e2 = op(e0, e2)) | ~ spl154_126), inference(backward_demodulation, [], [f348, f1503])).
fof(f348, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f3870, plain, (spl154_97 | ~ spl154_126), inference(avatar_split_clause, [], [f3856, f1501, f1379])).
fof(f3856, plain, ((e1 = op(e1, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f347, f1503])).
fof(f347, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f3869, plain, (spl154_117 | ~ spl154_126), inference(avatar_split_clause, [], [f3855, f1501, f1463])).
fof(f3855, plain, ((e1 = op(e0, e1)) | ~ spl154_126), inference(backward_demodulation, [], [f346, f1503])).
fof(f346, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f3868, plain, (spl154_121 | ~ spl154_126), inference(avatar_split_clause, [], [f3854, f1501, f1480])).
fof(f3854, plain, ((e0 = op(e0, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f345, f1503])).
fof(f345, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f3767, plain, (~ spl154_61 | ~ spl154_64), inference(avatar_contradiction_clause, [], [f3766])).
fof(f3766, plain, ($false | (~ spl154_61 | ~ spl154_64)), inference(subsumption_resolution, [], [f3765, f507])).
fof(f507, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f3765, plain, ((e0 = e3) | (~ spl154_61 | ~ spl154_64)), inference(backward_demodulation, [], [f1242, f1230])).
fof(f1230, plain, ((e0 = op(e2, e2)) | ~ spl154_61), inference(avatar_component_clause, [], [f1228])).
fof(f1228, plain, (spl154_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_61])])).
fof(f1242, plain, ((e3 = op(e2, e2)) | ~ spl154_64), inference(avatar_component_clause, [], [f1240])).
fof(f1240, plain, (spl154_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_64])])).
fof(f3723, plain, (spl154_13 | ~ spl154_130), inference(avatar_contradiction_clause, [], [f3722])).
fof(f3722, plain, ($false | (spl154_13 | ~ spl154_130)), inference(subsumption_resolution, [], [f3711, f1027])).
fof(f1027, plain, (~ (e2 = op(e4, e2)) | spl154_13), inference(avatar_component_clause, [], [f1026])).
fof(f1026, plain, (spl154_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_13])])).
fof(f3711, plain, ((e2 = op(e4, e2)) | ~ spl154_130), inference(backward_demodulation, [], [f348, f1519])).
fof(f1519, plain, ((e4 = unit) | ~ spl154_130), inference(avatar_component_clause, [], [f1517])).
fof(f1517, plain, (spl154_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_130])])).
fof(f3718, plain, (spl154_101 | ~ spl154_130), inference(avatar_contradiction_clause, [], [f3717])).
fof(f3717, plain, ($false | (spl154_101 | ~ spl154_130)), inference(subsumption_resolution, [], [f3708, f1397])).
fof(f1397, plain, (~ (e0 = op(e0, e4)) | spl154_101), inference(avatar_component_clause, [], [f1396])).
fof(f1396, plain, (spl154_101 <=> (e0 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_101])])).
fof(f3708, plain, ((e0 = op(e0, e4)) | ~ spl154_130), inference(backward_demodulation, [], [f345, f1519])).
fof(f3676, plain, (spl154_85 | ~ spl154_7 | ~ spl154_133), inference(avatar_split_clause, [], [f3643, f1584, f1001, f1328])).
fof(f1328, plain, (spl154_85 <=> (e4 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_85])])).
fof(f1001, plain, (spl154_7 <=> (e1 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_7])])).
fof(f1584, plain, (spl154_133 <=> (e4 = op(op(e4, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_133])])).
fof(f3643, plain, ((e4 = op(e1, e3)) | (~ spl154_7 | ~ spl154_133)), inference(backward_demodulation, [], [f1586, f1003])).
fof(f1003, plain, ((e1 = op(e4, e3)) | ~ spl154_7), inference(avatar_component_clause, [], [f1001])).
fof(f1586, plain, ((e4 = op(op(e4, e3), e3)) | ~ spl154_133), inference(avatar_component_clause, [], [f1584])).
fof(f3675, plain, (~ spl154_69 | ~ spl154_64), inference(avatar_split_clause, [], [f3199, f1240, f1261])).
fof(f3199, plain, (~ (e3 = op(e2, e1)) | ~ spl154_64), inference(forward_demodulation, [], [f479, f1242])).
fof(f479, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f3618, plain, (~ spl154_36 | ~ spl154_37), inference(avatar_contradiction_clause, [], [f3617])).
fof(f3617, plain, ($false | (~ spl154_36 | ~ spl154_37)), inference(subsumption_resolution, [], [f3616, f505])).
fof(f505, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f3616, plain, ((e0 = e1) | (~ spl154_36 | ~ spl154_37)), inference(backward_demodulation, [], [f1129, f1125])).
fof(f1125, plain, ((e0 = op(e3, e2)) | ~ spl154_36), inference(avatar_component_clause, [], [f1123])).
fof(f1123, plain, (spl154_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_36])])).
fof(f3609, plain, (~ spl154_62 | ~ spl154_64), inference(avatar_contradiction_clause, [], [f3608])).
fof(f3608, plain, ($false | (~ spl154_62 | ~ spl154_64)), inference(subsumption_resolution, [], [f3607, f510])).
fof(f510, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f3607, plain, ((e1 = e3) | (~ spl154_62 | ~ spl154_64)), inference(backward_demodulation, [], [f1242, f1234])).
fof(f1234, plain, ((e1 = op(e2, e2)) | ~ spl154_62), inference(avatar_component_clause, [], [f1232])).
fof(f1232, plain, (spl154_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_62])])).
fof(f3574, plain, (~ spl154_37 | ~ spl154_129), inference(avatar_contradiction_clause, [], [f3573])).
fof(f3573, plain, ($false | (~ spl154_37 | ~ spl154_129)), inference(subsumption_resolution, [], [f3572, f509])).
fof(f509, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f3572, plain, ((e1 = e2) | (~ spl154_37 | ~ spl154_129)), inference(forward_demodulation, [], [f3559, f1129])).
fof(f3559, plain, ((e2 = op(e3, e2)) | ~ spl154_129), inference(backward_demodulation, [], [f348, f1515])).
fof(f1515, plain, ((e3 = unit) | ~ spl154_129), inference(avatar_component_clause, [], [f1513])).
fof(f1513, plain, (spl154_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_129])])).
fof(f3515, plain, (~ spl154_3 | ~ spl154_78), inference(avatar_split_clause, [], [f3514, f1299, f984])).
fof(f984, plain, (spl154_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_3])])).
fof(f3514, plain, (~ (e2 = op(e4, e4)) | ~ spl154_78), inference(forward_demodulation, [], [f451, f1301])).
fof(f1301, plain, ((e2 = op(e1, e4)) | ~ spl154_78), inference(avatar_component_clause, [], [f1299])).
fof(f451, plain, ~ (op(e1, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f3512, plain, (~ spl154_31 | ~ spl154_35), inference(avatar_contradiction_clause, [], [f3511])).
fof(f3511, plain, ($false | (~ spl154_31 | ~ spl154_35)), inference(subsumption_resolution, [], [f3510, f508])).
fof(f508, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f3510, plain, ((e0 = e4) | (~ spl154_31 | ~ spl154_35)), inference(backward_demodulation, [], [f1120, f1104])).
fof(f1104, plain, ((e0 = op(e3, e3)) | ~ spl154_31), inference(avatar_component_clause, [], [f1102])).
fof(f1102, plain, (spl154_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_31])])).
fof(f1120, plain, ((e4 = op(e3, e3)) | ~ spl154_35), inference(avatar_component_clause, [], [f1118])).
fof(f1118, plain, (spl154_35 <=> (e4 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_35])])).
fof(f3481, plain, (~ spl154_37 | ~ spl154_128), inference(avatar_contradiction_clause, [], [f3480])).
fof(f3480, plain, ($false | (~ spl154_37 | ~ spl154_128)), inference(subsumption_resolution, [], [f3479, f510])).
fof(f3479, plain, ((e1 = e3) | (~ spl154_37 | ~ spl154_128)), inference(forward_demodulation, [], [f3463, f1129])).
fof(f3463, plain, ((e3 = op(e3, e2)) | ~ spl154_128), inference(backward_demodulation, [], [f351, f1511])).
fof(f1511, plain, ((e2 = unit) | ~ spl154_128), inference(avatar_component_clause, [], [f1509])).
fof(f1509, plain, (spl154_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_128])])).
fof(f3448, plain, (~ spl154_89 | ~ spl154_64), inference(avatar_split_clause, [], [f3223, f1240, f1345])).
fof(f3223, plain, (~ (e3 = op(e1, e2)) | ~ spl154_64), inference(forward_demodulation, [], [f429, f1242])).
fof(f429, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f3447, plain, (~ spl154_87 | ~ spl154_37), inference(avatar_split_clause, [], [f3446, f1127, f1337])).
fof(f1337, plain, (spl154_87 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_87])])).
fof(f3446, plain, (~ (e1 = op(e1, e2)) | ~ spl154_37), inference(forward_demodulation, [], [f430, f1129])).
fof(f430, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f3417, plain, (~ spl154_26 | ~ spl154_51), inference(avatar_split_clause, [], [f3416, f1186, f1081])).
fof(f1186, plain, (spl154_51 <=> (e0 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_51])])).
fof(f3416, plain, (~ (e0 = op(e3, e4)) | ~ spl154_51), inference(forward_demodulation, [], [f452, f1188])).
fof(f1188, plain, ((e0 = op(e2, e4)) | ~ spl154_51), inference(avatar_component_clause, [], [f1186])).
fof(f452, plain, ~ (op(e2, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f3379, plain, (~ spl154_11 | ~ spl154_13), inference(avatar_contradiction_clause, [], [f3378])).
fof(f3378, plain, ($false | (~ spl154_11 | ~ spl154_13)), inference(subsumption_resolution, [], [f3377, f506])).
fof(f506, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f3377, plain, ((e0 = e2) | (~ spl154_11 | ~ spl154_13)), inference(backward_demodulation, [], [f1028, f1020])).
fof(f1028, plain, ((e2 = op(e4, e2)) | ~ spl154_13), inference(avatar_component_clause, [], [f1026])).
fof(f3368, plain, (~ spl154_32 | ~ spl154_37), inference(avatar_split_clause, [], [f3367, f1127, f1106])).
fof(f1106, plain, (spl154_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_32])])).
fof(f3367, plain, (~ (e1 = op(e3, e3)) | ~ spl154_37), inference(backward_demodulation, [], [f492, f1129])).
fof(f492, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f3322, plain, (~ spl154_35 | ~ spl154_85), inference(avatar_split_clause, [], [f3321, f1328, f1118])).
fof(f3321, plain, (~ (e4 = op(e3, e3)) | ~ spl154_85), inference(backward_demodulation, [], [f440, f1330])).
fof(f1330, plain, ((e4 = op(e1, e3)) | ~ spl154_85), inference(avatar_component_clause, [], [f1328])).
fof(f440, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f3313, plain, (~ spl154_82 | ~ spl154_97), inference(avatar_split_clause, [], [f3308, f1379, f1316])).
fof(f1316, plain, (spl154_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_82])])).
fof(f3308, plain, (~ (e1 = op(e1, e3)) | ~ spl154_97), inference(backward_demodulation, [], [f467, f1381])).
fof(f467, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f3310, plain, (~ spl154_72 | ~ spl154_97), inference(avatar_split_clause, [], [f3305, f1379, f1274])).
fof(f1274, plain, (spl154_72 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_72])])).
fof(f3305, plain, (~ (e1 = op(e2, e0)) | ~ spl154_97), inference(backward_demodulation, [], [f409, f1381])).
fof(f409, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f3300, plain, (~ spl154_84 | ~ spl154_109), inference(avatar_split_clause, [], [f3297, f1429, f1324])).
fof(f1324, plain, (spl154_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_84])])).
fof(f3297, plain, (~ (e3 = op(e1, e3)) | ~ spl154_109), inference(backward_demodulation, [], [f435, f1431])).
fof(f435, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f3288, plain, (~ spl154_107 | ~ spl154_117), inference(avatar_split_clause, [], [f3284, f1463, f1421])).
fof(f1421, plain, (spl154_107 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_107])])).
fof(f3284, plain, (~ (e1 = op(e0, e3)) | ~ spl154_117), inference(backward_demodulation, [], [f460, f1465])).
fof(f460, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f3285, plain, (~ spl154_67 | ~ spl154_117), inference(avatar_split_clause, [], [f3281, f1463, f1253])).
fof(f1253, plain, (spl154_67 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_67])])).
fof(f3281, plain, (~ (e1 = op(e2, e1)) | ~ spl154_117), inference(backward_demodulation, [], [f416, f1465])).
fof(f416, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f3267, plain, (~ spl154_91 | ~ spl154_127), inference(avatar_contradiction_clause, [], [f3266])).
fof(f3266, plain, ($false | (~ spl154_91 | ~ spl154_127)), inference(subsumption_resolution, [], [f3265, f505])).
fof(f3265, plain, ((e0 = e1) | (~ spl154_91 | ~ spl154_127)), inference(forward_demodulation, [], [f3250, f1356])).
fof(f3250, plain, ((e1 = op(e1, e1)) | ~ spl154_127), inference(backward_demodulation, [], [f347, f1507])).
fof(f1507, plain, ((e1 = unit) | ~ spl154_127), inference(avatar_component_clause, [], [f1505])).
fof(f1505, plain, (spl154_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl154_127])])).
fof(f3239, plain, (~ spl154_113 | ~ spl154_13), inference(avatar_split_clause, [], [f3238, f1026, f1446])).
fof(f3238, plain, (~ (e2 = op(e0, e2)) | ~ spl154_13), inference(forward_demodulation, [], [f428, f1028])).
fof(f428, plain, ~ (op(e0, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f3186, plain, (~ spl154_43 | ~ spl154_18), inference(avatar_split_clause, [], [f3185, f1047, f1152])).
fof(f1047, plain, (spl154_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_18])])).
fof(f3185, plain, (~ (e2 = op(e3, e1)) | ~ spl154_18), inference(forward_demodulation, [], [f424, f1049])).
fof(f1049, plain, ((e2 = op(e4, e1)) | ~ spl154_18), inference(avatar_component_clause, [], [f1047])).
fof(f424, plain, ~ (op(e3, e1) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f3149, plain, (~ spl154_3 | ~ spl154_4), inference(avatar_contradiction_clause, [], [f3148])).
fof(f3148, plain, ($false | (~ spl154_3 | ~ spl154_4)), inference(subsumption_resolution, [], [f3147, f512])).
fof(f3147, plain, ((e2 = e3) | (~ spl154_3 | ~ spl154_4)), inference(backward_demodulation, [], [f990, f986])).
fof(f986, plain, ((e2 = op(e4, e4)) | ~ spl154_3), inference(avatar_component_clause, [], [f984])).
fof(f990, plain, ((e3 = op(e4, e4)) | ~ spl154_4), inference(avatar_component_clause, [], [f988])).
fof(f988, plain, (spl154_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_4])])).
fof(f3143, plain, (~ spl154_7 | ~ spl154_8), inference(avatar_contradiction_clause, [], [f3142])).
fof(f3142, plain, ($false | (~ spl154_7 | ~ spl154_8)), inference(subsumption_resolution, [], [f3141, f509])).
fof(f3141, plain, ((e1 = e2) | (~ spl154_7 | ~ spl154_8)), inference(backward_demodulation, [], [f1007, f1003])).
fof(f1007, plain, ((e2 = op(e4, e3)) | ~ spl154_8), inference(avatar_component_clause, [], [f1005])).
fof(f1005, plain, (spl154_8 <=> (e2 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_8])])).
fof(f3028, plain, (spl154_35 | ~ spl154_64), inference(avatar_split_clause, [], [f3022, f1240, f1118])).
fof(f3022, plain, ((e4 = op(e3, e3)) | ~ spl154_64), inference(backward_demodulation, [], [f518, f1242])).
fof(f518, plain, (e4 = op(op(e2, e2), op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(op(e2, e2), op(e2, e2))) & (e3 = op(e2, e2)) & (e1 = op(op(e2, e2), e2)) & (e0 = op(op(op(e2, e2), e2), op(op(e2, e2), e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', ax6)).
fof(f3027, plain, (spl154_37 | ~ spl154_64), inference(avatar_split_clause, [], [f3021, f1240, f1127])).
fof(f3021, plain, ((e1 = op(e3, e2)) | ~ spl154_64), inference(backward_demodulation, [], [f516, f1242])).
fof(f516, plain, (e1 = op(op(e2, e2), e2)), inference(cnf_transformation, [], [f6])).
fof(f3026, plain, (~ spl154_54 | ~ spl154_64), inference(avatar_split_clause, [], [f3020, f1240, f1198])).
fof(f1198, plain, (spl154_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl154_54])])).
fof(f3020, plain, (~ (e3 = op(e2, e4)) | ~ spl154_64), inference(backward_demodulation, [], [f483, f1242])).
fof(f483, plain, ~ (op(e2, e2) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2954, plain, (~ spl154_81 | ~ spl154_91), inference(avatar_split_clause, [], [f2946, f1354, f1312])).
fof(f1312, plain, (spl154_81 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_81])])).
fof(f2946, plain, (~ (e0 = op(e1, e3)) | ~ spl154_91), inference(backward_demodulation, [], [f470, f1356])).
fof(f470, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2953, plain, (~ spl154_86 | ~ spl154_91), inference(avatar_split_clause, [], [f2945, f1354, f1333])).
fof(f1333, plain, (spl154_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_86])])).
fof(f2945, plain, (~ (e0 = op(e1, e2)) | ~ spl154_91), inference(backward_demodulation, [], [f469, f1356])).
fof(f469, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f2951, plain, (~ spl154_41 | ~ spl154_91), inference(avatar_split_clause, [], [f2943, f1354, f1144])).
fof(f1144, plain, (spl154_41 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_41])])).
fof(f2943, plain, (~ (e0 = op(e3, e1)) | ~ spl154_91), inference(backward_demodulation, [], [f420, f1356])).
fof(f420, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f2950, plain, (~ spl154_66 | ~ spl154_91), inference(avatar_split_clause, [], [f2942, f1354, f1249])).
fof(f1249, plain, (spl154_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl154_66])])).
fof(f2942, plain, (~ (e0 = op(e2, e1)) | ~ spl154_91), inference(backward_demodulation, [], [f419, f1356])).
fof(f419, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2920, plain, (~ spl154_104 | ~ spl154_105), inference(avatar_contradiction_clause, [], [f2919])).
fof(f2919, plain, ($false | (~ spl154_104 | ~ spl154_105)), inference(subsumption_resolution, [], [f2918, f514])).
fof(f514, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f2918, plain, ((e3 = e4) | (~ spl154_104 | ~ spl154_105)), inference(backward_demodulation, [], [f1414, f1410])).
fof(f1410, plain, ((e3 = op(e0, e4)) | ~ spl154_104), inference(avatar_component_clause, [], [f1408])).
fof(f2898, plain, (~ spl154_101 | ~ spl154_111), inference(avatar_split_clause, [], [f2891, f1438, f1396])).
fof(f1438, plain, (spl154_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl154_111])])).
fof(f2891, plain, (~ (e0 = op(e0, e4)) | ~ spl154_111), inference(backward_demodulation, [], [f463, f1440])).
fof(f1440, plain, ((e0 = op(e0, e2)) | ~ spl154_111), inference(avatar_component_clause, [], [f1438])).
fof(f463, plain, ~ (op(e0, e2) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f2869, plain, (~ spl154_111 | ~ spl154_121), inference(avatar_split_clause, [], [f2860, f1480, f1438])).
fof(f2860, plain, (~ (e0 = op(e0, e2)) | ~ spl154_121), inference(backward_demodulation, [], [f456, f1482])).
fof(f456, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f2865, plain, (~ spl154_71 | ~ spl154_121), inference(avatar_split_clause, [], [f2856, f1480, f1270])).
fof(f1270, plain, (spl154_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl154_71])])).
fof(f2856, plain, (~ (e0 = op(e2, e0)) | ~ spl154_121), inference(backward_demodulation, [], [f406, f1482])).
fof(f406, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f2854, plain, (spl154_25 | ~ spl154_126), inference(avatar_split_clause, [], [f2840, f1501, f1076])).
fof(f2840, plain, ((e4 = op(e4, e0)) | ~ spl154_126), inference(backward_demodulation, [], [f353, f1503])).
fof(f2853, plain, (spl154_105 | ~ spl154_126), inference(avatar_split_clause, [], [f2839, f1501, f1412])).
fof(f2839, plain, ((e4 = op(e0, e4)) | ~ spl154_126), inference(backward_demodulation, [], [f352, f1503])).
fof(f352, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2851, plain, (spl154_109 | ~ spl154_126), inference(avatar_split_clause, [], [f2837, f1501, f1429])).
fof(f2837, plain, ((e3 = op(e0, e3)) | ~ spl154_126), inference(backward_demodulation, [], [f350, f1503])).
fof(f2830, plain, (spl154_311 | spl154_285 | spl154_259 | spl154_233 | spl154_207), inference(avatar_split_clause, [], [f966, f1951, f2080, f2209, f2338, f2467])).
fof(f2467, plain, (spl154_311 <=> sP125), introduced(avatar_definition, [new_symbols(naming, [spl154_311])])).
fof(f2338, plain, (spl154_285 <=> sP126), introduced(avatar_definition, [new_symbols(naming, [spl154_285])])).
fof(f2209, plain, (spl154_259 <=> sP127), introduced(avatar_definition, [new_symbols(naming, [spl154_259])])).
fof(f2080, plain, (spl154_233 <=> sP128), introduced(avatar_definition, [new_symbols(naming, [spl154_233])])).
fof(f1951, plain, (spl154_207 <=> sP129), introduced(avatar_definition, [new_symbols(naming, [spl154_207])])).
fof(f966, plain, (sP129 | sP128 | sP127 | sP126 | sP125), inference(cnf_transformation, [], [f164])).
fof(f164, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | sP153 | sP152 | sP151 | sP150 | sP149 | sP148 | sP147 | sP146 | sP145 | sP144 | sP143 | sP142 | sP141 | sP140 | sP139 | sP138 | sP137 | sP136 | sP135 | sP134 | sP133 | sP132 | sP131 | sP130) & (sP129 | sP128 | sP127 | sP126 | sP125)), inference(definition_folding, [], [f9, e163, e162, e161, e160, e159, e158, e157, e156, e155, e154, e153, e152, e151, e150, e149, e148, e147, e146, e145, e144, e143, e142, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88, e87, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
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
fof(f140, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP130), inference(usedef, [], [e140])).
fof(e140, plain, (sP130 <=> (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP130])])).
fof(f141, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP131), inference(usedef, [], [e141])).
fof(e141, plain, (sP131 <=> (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP131])])).
fof(f142, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP132), inference(usedef, [], [e142])).
fof(e142, plain, (sP132 <=> (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP132])])).
fof(f143, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP133), inference(usedef, [], [e143])).
fof(e143, plain, (sP133 <=> (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP133])])).
fof(f144, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP134), inference(usedef, [], [e144])).
fof(e144, plain, (sP134 <=> (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP134])])).
fof(f145, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP135), inference(usedef, [], [e145])).
fof(e145, plain, (sP135 <=> (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP135])])).
fof(f146, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP136), inference(usedef, [], [e146])).
fof(e146, plain, (sP136 <=> (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP136])])).
fof(f147, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP137), inference(usedef, [], [e147])).
fof(e147, plain, (sP137 <=> (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP137])])).
fof(f148, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP138), inference(usedef, [], [e148])).
fof(e148, plain, (sP138 <=> (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP138])])).
fof(f149, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP139), inference(usedef, [], [e149])).
fof(e149, plain, (sP139 <=> (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP139])])).
fof(f150, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP140), inference(usedef, [], [e150])).
fof(e150, plain, (sP140 <=> (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP140])])).
fof(f151, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP141), inference(usedef, [], [e151])).
fof(e151, plain, (sP141 <=> (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP141])])).
fof(f152, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP142), inference(usedef, [], [e152])).
fof(e152, plain, (sP142 <=> (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP142])])).
fof(f153, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP143), inference(usedef, [], [e153])).
fof(e153, plain, (sP143 <=> (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP143])])).
fof(f154, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP144), inference(usedef, [], [e154])).
fof(e154, plain, (sP144 <=> (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP144])])).
fof(f155, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP145), inference(usedef, [], [e155])).
fof(e155, plain, (sP145 <=> (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP145])])).
fof(f156, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP146), inference(usedef, [], [e156])).
fof(e156, plain, (sP146 <=> (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP146])])).
fof(f157, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP147), inference(usedef, [], [e157])).
fof(e157, plain, (sP147 <=> (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP147])])).
fof(f158, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP148), inference(usedef, [], [e158])).
fof(e158, plain, (sP148 <=> (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP148])])).
fof(f159, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP149), inference(usedef, [], [e159])).
fof(e159, plain, (sP149 <=> (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP149])])).
fof(f160, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP150), inference(usedef, [], [e160])).
fof(e160, plain, (sP150 <=> (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP150])])).
fof(f161, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP151), inference(usedef, [], [e161])).
fof(e161, plain, (sP151 <=> (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP151])])).
fof(f162, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP152), inference(usedef, [], [e162])).
fof(e162, plain, (sP152 <=> (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP152])])).
fof(f163, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP153), inference(usedef, [], [e163])).
fof(e163, plain, (sP153 <=> (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP153])])).
fof(f9, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((((~ (e4 = unit) & (e4 = op(e4, e4))) | ~ (e4 = op(e4, e4))) & ((~ (e4 = unit) & (e3 = op(e4, e4))) | ~ (e4 = op(e4, e3))) & ((~ (e4 = unit) & (e2 = op(e4, e4))) | ~ (e4 = op(e4, e2))) & ((~ (e4 = unit) & (e1 = op(e4, e4))) | ~ (e4 = op(e4, e1))) & ((~ (e4 = unit) & (e0 = op(e4, e4))) | ~ (e4 = op(e4, e0))) & ((~ (e4 = unit) & (e4 = op(e3, e4))) | ~ (e4 = op(e3, e4))) & ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3))) & ((~ (e4 = unit) & (e2 = op(e3, e4))) | ~ (e4 = op(e3, e2))) & ((~ (e4 = unit) & (e1 = op(e3, e4))) | ~ (e4 = op(e3, e1))) & ((~ (e4 = unit) & (e0 = op(e3, e4))) | ~ (e4 = op(e3, e0))) & ((~ (e4 = unit) & (e4 = op(e2, e4))) | ~ (e4 = op(e2, e4))) & ((~ (e4 = unit) & (e3 = op(e2, e4))) | ~ (e4 = op(e2, e3))) & ((~ (e4 = unit) & (e2 = op(e2, e4))) | ~ (e4 = op(e2, e2))) & ((~ (e4 = unit) & (e1 = op(e2, e4))) | ~ (e4 = op(e2, e1))) & ((~ (e4 = unit) & (e0 = op(e2, e4))) | ~ (e4 = op(e2, e0))) & ((~ (e4 = unit) & (e4 = op(e1, e4))) | ~ (e4 = op(e1, e4))) & ((~ (e4 = unit) & (e3 = op(e1, e4))) | ~ (e4 = op(e1, e3))) & ((~ (e4 = unit) & (e2 = op(e1, e4))) | ~ (e4 = op(e1, e2))) & ((~ (e4 = unit) & (e1 = op(e1, e4))) | ~ (e4 = op(e1, e1))) & ((~ (e4 = unit) & (e0 = op(e1, e4))) | ~ (e4 = op(e1, e0))) & ((~ (e4 = unit) & (e4 = op(e0, e4))) | ~ (e4 = op(e0, e4))) & ((~ (e4 = unit) & (e3 = op(e0, e4))) | ~ (e4 = op(e0, e3))) & ((~ (e4 = unit) & (e2 = op(e0, e4))) | ~ (e4 = op(e0, e2))) & ((~ (e4 = unit) & (e1 = op(e0, e4))) | ~ (e4 = op(e0, e1))) & ((~ (e4 = unit) & (e0 = op(e0, e4))) | ~ (op(e0, e0) = e4))) | (((~ (e3 = unit) & (e4 = op(e4, e3))) | ~ (e3 = op(e4, e4))) & ((~ (e3 = unit) & (e3 = op(e4, e3))) | ~ (e3 = op(e4, e3))) & ((~ (e3 = unit) & (e2 = op(e4, e3))) | ~ (e3 = op(e4, e2))) & ((~ (e3 = unit) & (e1 = op(e4, e3))) | ~ (e3 = op(e4, e1))) & ((~ (e3 = unit) & (e0 = op(e4, e3))) | ~ (e3 = op(e4, e0))) & ((~ (e3 = unit) & (e4 = op(e3, e3))) | ~ (e3 = op(e3, e4))) & ((~ (e3 = unit) & (e3 = op(e3, e3))) | ~ (e3 = op(e3, e3))) & ((~ (e3 = unit) & (e2 = op(e3, e3))) | ~ (e3 = op(e3, e2))) & ((~ (e3 = unit) & (e1 = op(e3, e3))) | ~ (e3 = op(e3, e1))) & ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0))) & ((~ (e3 = unit) & (e4 = op(e2, e3))) | ~ (e3 = op(e2, e4))) & ((~ (e3 = unit) & (e3 = op(e2, e3))) | ~ (e3 = op(e2, e3))) & ((~ (e3 = unit) & (e2 = op(e2, e3))) | ~ (e3 = op(e2, e2))) & ((~ (e3 = unit) & (e1 = op(e2, e3))) | ~ (e3 = op(e2, e1))) & ((~ (e3 = unit) & (e0 = op(e2, e3))) | ~ (e3 = op(e2, e0))) & ((~ (e3 = unit) & (e4 = op(e1, e3))) | ~ (e3 = op(e1, e4))) & ((~ (e3 = unit) & (e3 = op(e1, e3))) | ~ (e3 = op(e1, e3))) & ((~ (e3 = unit) & (e2 = op(e1, e3))) | ~ (e3 = op(e1, e2))) & ((~ (e3 = unit) & (e1 = op(e1, e3))) | ~ (e3 = op(e1, e1))) & ((~ (e3 = unit) & (e0 = op(e1, e3))) | ~ (e3 = op(e1, e0))) & ((~ (e3 = unit) & (e4 = op(e0, e3))) | ~ (e3 = op(e0, e4))) & ((~ (e3 = unit) & (e3 = op(e0, e3))) | ~ (e3 = op(e0, e3))) & ((~ (e3 = unit) & (e2 = op(e0, e3))) | ~ (e3 = op(e0, e2))) & ((~ (e3 = unit) & (e1 = op(e0, e3))) | ~ (e3 = op(e0, e1))) & ((~ (e3 = unit) & (e0 = op(e0, e3))) | ~ (op(e0, e0) = e3))) | (((~ (e2 = unit) & (e4 = op(e4, e2))) | ~ (e2 = op(e4, e4))) & ((~ (e2 = unit) & (e3 = op(e4, e2))) | ~ (e2 = op(e4, e3))) & ((~ (e2 = unit) & (e2 = op(e4, e2))) | ~ (e2 = op(e4, e2))) & ((~ (e2 = unit) & (e1 = op(e4, e2))) | ~ (e2 = op(e4, e1))) & ((~ (e2 = unit) & (e0 = op(e4, e2))) | ~ (e2 = op(e4, e0))) & ((~ (e2 = unit) & (e4 = op(e3, e2))) | ~ (e2 = op(e3, e4))) & ((~ (e2 = unit) & (e3 = op(e3, e2))) | ~ (e2 = op(e3, e3))) & ((~ (e2 = unit) & (e2 = op(e3, e2))) | ~ (e2 = op(e3, e2))) & ((~ (e2 = unit) & (e1 = op(e3, e2))) | ~ (e2 = op(e3, e1))) & ((~ (e2 = unit) & (e0 = op(e3, e2))) | ~ (e2 = op(e3, e0))) & ((~ (e2 = unit) & (e4 = op(e2, e2))) | ~ (e2 = op(e2, e4))) & ((~ (e2 = unit) & (e3 = op(e2, e2))) | ~ (e2 = op(e2, e3))) & ((~ (e2 = unit) & (e2 = op(e2, e2))) | ~ (e2 = op(e2, e2))) & ((~ (e2 = unit) & (e1 = op(e2, e2))) | ~ (e2 = op(e2, e1))) & ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0))) & ((~ (e2 = unit) & (e4 = op(e1, e2))) | ~ (e2 = op(e1, e4))) & ((~ (e2 = unit) & (e3 = op(e1, e2))) | ~ (e2 = op(e1, e3))) & ((~ (e2 = unit) & (e2 = op(e1, e2))) | ~ (e2 = op(e1, e2))) & ((~ (e2 = unit) & (e1 = op(e1, e2))) | ~ (e2 = op(e1, e1))) & ((~ (e2 = unit) & (e0 = op(e1, e2))) | ~ (e2 = op(e1, e0))) & ((~ (e2 = unit) & (e4 = op(e0, e2))) | ~ (e2 = op(e0, e4))) & ((~ (e2 = unit) & (e3 = op(e0, e2))) | ~ (e2 = op(e0, e3))) & ((~ (e2 = unit) & (e2 = op(e0, e2))) | ~ (e2 = op(e0, e2))) & ((~ (e2 = unit) & (e1 = op(e0, e2))) | ~ (e2 = op(e0, e1))) & ((~ (e2 = unit) & (e0 = op(e0, e2))) | ~ (op(e0, e0) = e2))) | (((~ (e1 = unit) & (e4 = op(e4, e1))) | ~ (e1 = op(e4, e4))) & ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3))) & ((~ (e1 = unit) & (e2 = op(e4, e1))) | ~ (e1 = op(e4, e2))) & ((~ (e1 = unit) & (e1 = op(e4, e1))) | ~ (e1 = op(e4, e1))) & ((~ (e1 = unit) & (e0 = op(e4, e1))) | ~ (e1 = op(e4, e0))) & ((~ (e1 = unit) & (e4 = op(e3, e1))) | ~ (e1 = op(e3, e4))) & ((~ (e1 = unit) & (e3 = op(e3, e1))) | ~ (e1 = op(e3, e3))) & ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2))) & ((~ (e1 = unit) & (e1 = op(e3, e1))) | ~ (e1 = op(e3, e1))) & ((~ (e1 = unit) & (e0 = op(e3, e1))) | ~ (e1 = op(e3, e0))) & ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4))) & ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3))) & ((~ (e1 = unit) & (e2 = op(e2, e1))) | ~ (e1 = op(e2, e2))) & ((~ (e1 = unit) & (e1 = op(e2, e1))) | ~ (e1 = op(e2, e1))) & ((~ (e1 = unit) & (e0 = op(e2, e1))) | ~ (e1 = op(e2, e0))) & ((~ (e1 = unit) & (e4 = op(e1, e1))) | ~ (e1 = op(e1, e4))) & ((~ (e1 = unit) & (e3 = op(e1, e1))) | ~ (e1 = op(e1, e3))) & ((~ (e1 = unit) & (e2 = op(e1, e1))) | ~ (e1 = op(e1, e2))) & ((~ (e1 = unit) & (e1 = op(e1, e1))) | ~ (e1 = op(e1, e1))) & ((~ (e1 = unit) & (e0 = op(e1, e1))) | ~ (e1 = op(e1, e0))) & ((~ (e1 = unit) & (e4 = op(e0, e1))) | ~ (e1 = op(e0, e4))) & ((~ (e1 = unit) & (e3 = op(e0, e1))) | ~ (e1 = op(e0, e3))) & ((~ (e1 = unit) & (e2 = op(e0, e1))) | ~ (e1 = op(e0, e2))) & ((~ (e1 = unit) & (e1 = op(e0, e1))) | ~ (e1 = op(e0, e1))) & ((~ (e1 = unit) & (e0 = op(e0, e1))) | ~ (op(e0, e0) = e1))) | (((~ (e0 = unit) & (e4 = op(e4, e0))) | ~ (e0 = op(e4, e4))) & ((~ (e0 = unit) & (e3 = op(e4, e0))) | ~ (e0 = op(e4, e3))) & ((~ (e0 = unit) & (e2 = op(e4, e0))) | ~ (e0 = op(e4, e2))) & ((~ (e0 = unit) & (e1 = op(e4, e0))) | ~ (e0 = op(e4, e1))) & ((~ (e0 = unit) & (e0 = op(e4, e0))) | ~ (e0 = op(e4, e0))) & ((~ (e0 = unit) & (e4 = op(e3, e0))) | ~ (e0 = op(e3, e4))) & ((~ (e0 = unit) & (e3 = op(e3, e0))) | ~ (e0 = op(e3, e3))) & ((~ (e0 = unit) & (e2 = op(e3, e0))) | ~ (e0 = op(e3, e2))) & ((~ (e0 = unit) & (e1 = op(e3, e0))) | ~ (e0 = op(e3, e1))) & ((~ (e0 = unit) & (e0 = op(e3, e0))) | ~ (e0 = op(e3, e0))) & ((~ (e0 = unit) & (e4 = op(e2, e0))) | ~ (e0 = op(e2, e4))) & ((~ (e0 = unit) & (e3 = op(e2, e0))) | ~ (e0 = op(e2, e3))) & ((~ (e0 = unit) & (e2 = op(e2, e0))) | ~ (e0 = op(e2, e2))) & ((~ (e0 = unit) & (e1 = op(e2, e0))) | ~ (e0 = op(e2, e1))) & ((~ (e0 = unit) & (e0 = op(e2, e0))) | ~ (e0 = op(e2, e0))) & ((~ (e0 = unit) & (e4 = op(e1, e0))) | ~ (e0 = op(e1, e4))) & ((~ (e0 = unit) & (e3 = op(e1, e0))) | ~ (e0 = op(e1, e3))) & ((~ (e0 = unit) & (e2 = op(e1, e0))) | ~ (e0 = op(e1, e2))) & ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1))) & ((~ (e0 = unit) & (e0 = op(e1, e0))) | ~ (e0 = op(e1, e0))) & ((~ (e0 = unit) & (op(e0, e0) = e4)) | ~ (e0 = op(e0, e4))) & ((~ (e0 = unit) & (op(e0, e0) = e3)) | ~ (e0 = op(e0, e3))) & ((~ (e0 = unit) & (op(e0, e0) = e2)) | ~ (e0 = op(e0, e2))) & ((~ (e0 = unit) & (op(e0, e0) = e1)) | ~ (e0 = op(e0, e1))) & ((~ (e0 = unit) & (e0 = op(e0, e0))) | ~ (e0 = op(e0, e0)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', co1)).
fof(f2829, plain, (spl154_205 | spl154_202 | spl154_199 | spl154_196 | spl154_193 | spl154_189 | spl154_187 | spl154_184 | spl154_181 | spl154_178 | spl154_174 | spl154_170 | spl154_167 | spl154_164 | spl154_160 | spl154_156 | spl154_152 | spl154_150 | spl154_147 | spl154_143 | spl154_139 | spl154_135 | spl154_131), inference(avatar_split_clause, [], [f2828, f1575, f1594, f1613, f1632, f1651, f1666, f1677, f1696, f1715, f1734, f1749, f1767, f1786, f1805, f1820, f1835, f1850, f1861, f1880, f1895, f1910, f1925, f1940])).
fof(f1940, plain, (spl154_205 <=> sP130), introduced(avatar_definition, [new_symbols(naming, [spl154_205])])).
fof(f1925, plain, (spl154_202 <=> sP131), introduced(avatar_definition, [new_symbols(naming, [spl154_202])])).
fof(f1910, plain, (spl154_199 <=> sP132), introduced(avatar_definition, [new_symbols(naming, [spl154_199])])).
fof(f1895, plain, (spl154_196 <=> sP133), introduced(avatar_definition, [new_symbols(naming, [spl154_196])])).
fof(f1880, plain, (spl154_193 <=> sP134), introduced(avatar_definition, [new_symbols(naming, [spl154_193])])).
fof(f1861, plain, (spl154_189 <=> sP135), introduced(avatar_definition, [new_symbols(naming, [spl154_189])])).
fof(f1850, plain, (spl154_187 <=> sP136), introduced(avatar_definition, [new_symbols(naming, [spl154_187])])).
fof(f1835, plain, (spl154_184 <=> sP137), introduced(avatar_definition, [new_symbols(naming, [spl154_184])])).
fof(f1820, plain, (spl154_181 <=> sP138), introduced(avatar_definition, [new_symbols(naming, [spl154_181])])).
fof(f1805, plain, (spl154_178 <=> sP139), introduced(avatar_definition, [new_symbols(naming, [spl154_178])])).
fof(f1786, plain, (spl154_174 <=> sP140), introduced(avatar_definition, [new_symbols(naming, [spl154_174])])).
fof(f1767, plain, (spl154_170 <=> sP141), introduced(avatar_definition, [new_symbols(naming, [spl154_170])])).
fof(f1749, plain, (spl154_167 <=> sP143), introduced(avatar_definition, [new_symbols(naming, [spl154_167])])).
fof(f1734, plain, (spl154_164 <=> sP144), introduced(avatar_definition, [new_symbols(naming, [spl154_164])])).
fof(f1715, plain, (spl154_160 <=> sP145), introduced(avatar_definition, [new_symbols(naming, [spl154_160])])).
fof(f1696, plain, (spl154_156 <=> sP146), introduced(avatar_definition, [new_symbols(naming, [spl154_156])])).
fof(f1677, plain, (spl154_152 <=> sP147), introduced(avatar_definition, [new_symbols(naming, [spl154_152])])).
fof(f1666, plain, (spl154_150 <=> sP148), introduced(avatar_definition, [new_symbols(naming, [spl154_150])])).
fof(f1651, plain, (spl154_147 <=> sP149), introduced(avatar_definition, [new_symbols(naming, [spl154_147])])).
fof(f1632, plain, (spl154_143 <=> sP150), introduced(avatar_definition, [new_symbols(naming, [spl154_143])])).
fof(f1613, plain, (spl154_139 <=> sP151), introduced(avatar_definition, [new_symbols(naming, [spl154_139])])).
fof(f1594, plain, (spl154_135 <=> sP152), introduced(avatar_definition, [new_symbols(naming, [spl154_135])])).
fof(f1575, plain, (spl154_131 <=> sP153), introduced(avatar_definition, [new_symbols(naming, [spl154_131])])).
fof(f2828, plain, (sP153 | sP152 | sP151 | sP150 | sP149 | sP148 | sP147 | sP146 | sP145 | sP144 | sP143 | sP141 | sP140 | sP139 | sP138 | sP137 | sP136 | sP135 | sP134 | sP133 | sP132 | sP131 | sP130), inference(subsumption_resolution, [], [f970, f1765])).
fof(f1765, plain, ~ sP142, inference(subsumption_resolution, [], [f1764, f509])).
fof(f1764, plain, ((e1 = e2) | ~ sP142), inference(forward_demodulation, [], [f553, f516])).
fof(f553, plain, ((e2 = op(op(e2, e2), e2)) | ~ sP142), inference(cnf_transformation, [], [f176])).
fof(f176, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP142), inference(nnf_transformation, [], [f152])).
fof(f970, plain, (sP153 | sP152 | sP151 | sP150 | sP149 | sP148 | sP147 | sP146 | sP145 | sP144 | sP143 | sP142 | sP141 | sP140 | sP139 | sP138 | sP137 | sP136 | sP135 | sP134 | sP133 | sP132 | sP131 | sP130), inference(trivial_inequality_removal, [], [f967])).
fof(f967, plain, (~ (op(e4, e4) = op(e4, e4)) | sP153 | sP152 | sP151 | sP150 | sP149 | sP148 | sP147 | sP146 | sP145 | sP144 | sP143 | sP142 | sP141 | sP140 | sP139 | sP138 | sP137 | sP136 | sP135 | sP134 | sP133 | sP132 | sP131 | sP130), inference(cnf_transformation, [], [f164])).
fof(f2808, plain, (~ spl154_330 | ~ spl154_91 | ~ spl154_126), inference(avatar_split_clause, [], [f953, f1501, f1354, f2561])).
fof(f2561, plain, (spl154_330 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl154_330])])).
fof(f953, plain, (~ (e0 = unit) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f312])).
fof(f312, plain, ((~ (e0 = unit) & (e1 = op(e1, e0))) | ~ (e0 = op(e1, e1)) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f2751, plain, (~ spl154_297 | ~ spl154_57 | spl154_69), inference(avatar_split_clause, [], [f888, f1261, f1211, f2397])).
fof(f2397, plain, (spl154_297 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl154_297])])).
fof(f1211, plain, (spl154_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl154_57])])).
fof(f888, plain, ((e3 = op(e2, e1)) | ~ (e1 = op(e2, e3)) | ~ sP38), inference(cnf_transformation, [], [f280])).
fof(f280, plain, ((~ (e1 = unit) & (e3 = op(e2, e1))) | ~ (e1 = op(e2, e3)) | ~ sP38), inference(nnf_transformation, [], [f48])).
fof(f2749, plain, (~ spl154_296 | ~ spl154_52 | spl154_70), inference(avatar_split_clause, [], [f886, f1265, f1190, f2392])).
fof(f2392, plain, (spl154_296 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl154_296])])).
fof(f886, plain, ((e4 = op(e2, e1)) | ~ (e1 = op(e2, e4)) | ~ sP39), inference(cnf_transformation, [], [f279])).
fof(f279, plain, ((~ (e1 = unit) & (e4 = op(e2, e1))) | ~ (e1 = op(e2, e4)) | ~ sP39), inference(nnf_transformation, [], [f49])).
fof(f2744, plain, (~ spl154_293 | ~ spl154_37 | spl154_43), inference(avatar_split_clause, [], [f880, f1152, f1127, f2377])).
fof(f2377, plain, (spl154_293 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl154_293])])).
fof(f880, plain, ((e2 = op(e3, e1)) | ~ (e1 = op(e3, e2)) | ~ sP42), inference(cnf_transformation, [], [f276])).
fof(f276, plain, ((~ (e1 = unit) & (e2 = op(e3, e1))) | ~ (e1 = op(e3, e2)) | ~ sP42), inference(nnf_transformation, [], [f52])).
fof(f2733, plain, (~ spl154_287 | ~ spl154_7 | spl154_19), inference(avatar_split_clause, [], [f868, f1051, f1001, f2347])).
fof(f2347, plain, (spl154_287 <=> sP48), introduced(avatar_definition, [new_symbols(naming, [spl154_287])])).
fof(f868, plain, ((e3 = op(e4, e1)) | ~ (e1 = op(e4, e3)) | ~ sP48), inference(cnf_transformation, [], [f270])).
fof(f270, plain, ((~ (e1 = unit) & (e3 = op(e4, e1))) | ~ (e1 = op(e4, e3)) | ~ sP48), inference(nnf_transformation, [], [f58])).
fof(f2711, plain, (~ spl154_274 | ~ spl154_73 | spl154_61), inference(avatar_split_clause, [], [f844, f1228, f1278, f2283])).
fof(f2283, plain, (spl154_274 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl154_274])])).
fof(f844, plain, ((e0 = op(e2, e2)) | ~ (e2 = op(e2, e0)) | ~ sP60), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ((~ (e2 = unit) & (e0 = op(e2, e2))) | ~ (e2 = op(e2, e0)) | ~ sP60), inference(nnf_transformation, [], [f70])).
fof(f2657, plain, (~ spl154_243 | ~ spl154_49 | spl154_31), inference(avatar_split_clause, [], [f784, f1102, f1177, f2129])).
fof(f2129, plain, (spl154_243 <=> sP90), introduced(avatar_definition, [new_symbols(naming, [spl154_243])])).
fof(f784, plain, ((e0 = op(e3, e3)) | ~ (e3 = op(e3, e0)) | ~ sP90), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (e3 = unit) & (e0 = op(e3, e3))) | ~ (e3 = op(e3, e0)) | ~ sP90), inference(nnf_transformation, [], [f100])).
fof(f2606, plain, (~ spl154_214 | ~ spl154_35 | spl154_29), inference(avatar_split_clause, [], [f728, f1093, f1118, f1985])).
fof(f1985, plain, (spl154_214 <=> sP118), introduced(avatar_definition, [new_symbols(naming, [spl154_214])])).
fof(f728, plain, ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ((~ (e4 = unit) & (e3 = op(e3, e4))) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(nnf_transformation, [], [f128])).
fof(f2605, plain, (~ spl154_214 | ~ spl154_35 | ~ spl154_130), inference(avatar_split_clause, [], [f729, f1517, f1118, f1985])).
fof(f729, plain, (~ (e4 = unit) | ~ (e4 = op(e3, e3)) | ~ sP118), inference(cnf_transformation, [], [f200])).
fof(f2564, plain, (~ spl154_311 | spl154_330), inference(avatar_split_clause, [], [f697, f2561, f2467])).
fof(f697, plain, (sP6 | ~ sP125), inference(cnf_transformation, [], [f193])).
fof(f193, plain, ((sP24 & sP23 & sP22 & sP21 & sP20 & sP19 & sP18 & sP17 & sP16 & sP15 & sP14 & sP13 & sP12 & sP11 & sP10 & sP9 & sP8 & sP7 & sP6 & sP5 & sP4 & sP3 & sP2 & sP1 & sP0) | ~ sP125), inference(nnf_transformation, [], [f135])).
fof(f2400, plain, (~ spl154_285 | spl154_297), inference(avatar_split_clause, [], [f679, f2397, f2338])).
fof(f679, plain, (sP38 | ~ sP126), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ((sP49 & sP48 & sP47 & sP46 & sP45 & sP44 & sP43 & sP42 & sP41 & sP40 & sP39 & sP38 & sP37 & sP36 & sP35 & sP34 & sP33 & sP32 & sP31 & sP30 & sP29 & sP28 & sP27 & sP26 & sP25) | ~ sP126), inference(nnf_transformation, [], [f136])).
fof(f2395, plain, (~ spl154_285 | spl154_296), inference(avatar_split_clause, [], [f680, f2392, f2338])).
fof(f680, plain, (sP39 | ~ sP126), inference(cnf_transformation, [], [f192])).
fof(f2380, plain, (~ spl154_285 | spl154_293), inference(avatar_split_clause, [], [f683, f2377, f2338])).
fof(f683, plain, (sP42 | ~ sP126), inference(cnf_transformation, [], [f192])).
fof(f2350, plain, (~ spl154_285 | spl154_287), inference(avatar_split_clause, [], [f689, f2347, f2338])).
fof(f689, plain, (sP48 | ~ sP126), inference(cnf_transformation, [], [f192])).
fof(f2286, plain, (~ spl154_259 | spl154_274), inference(avatar_split_clause, [], [f651, f2283, f2209])).
fof(f651, plain, (sP60 | ~ sP127), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ((sP74 & sP73 & sP72 & sP71 & sP70 & sP69 & sP68 & sP67 & sP66 & sP65 & sP64 & sP63 & sP62 & sP61 & sP60 & sP59 & sP58 & sP57 & sP56 & sP55 & sP54 & sP53 & sP52 & sP51 & sP50) | ~ sP127), inference(nnf_transformation, [], [f137])).
fof(f2132, plain, (~ spl154_233 | spl154_243), inference(avatar_split_clause, [], [f631, f2129, f2080])).
fof(f631, plain, (sP90 | ~ sP128), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ((sP99 & sP98 & sP97 & sP96 & sP95 & sP94 & sP93 & sP92 & sP91 & sP90 & sP89 & sP88 & sP87 & sP86 & sP85 & sP84 & sP83 & sP82 & sP81 & sP80 & sP79 & sP78 & sP77 & sP76 & sP75) | ~ sP128), inference(nnf_transformation, [], [f138])).
fof(f1988, plain, (~ spl154_207 | spl154_214), inference(avatar_split_clause, [], [f609, f1985, f1951])).
fof(f609, plain, (sP118 | ~ sP129), inference(cnf_transformation, [], [f189])).
fof(f189, plain, ((sP124 & sP123 & sP122 & sP121 & sP120 & sP119 & sP118 & sP117 & sP116 & sP115 & sP114 & sP113 & sP112 & sP111 & sP110 & sP109 & sP108 & sP107 & sP106 & sP105 & sP104 & sP103 & sP102 & sP101 & sP100) | ~ sP129), inference(nnf_transformation, [], [f139])).
fof(f1949, plain, ~ spl154_205, inference(avatar_split_clause, [], [f971, f1940])).
fof(f971, plain, ~ sP130, inference(trivial_inequality_removal, [], [f588])).
fof(f588, plain, (~ (op(e0, e0) = op(e0, e0)) | ~ sP130), inference(cnf_transformation, [], [f188])).
fof(f188, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP130), inference(nnf_transformation, [], [f140])).
fof(f1938, plain, (~ spl154_202 | ~ spl154_192), inference(avatar_split_clause, [], [f585, f1875, f1925])).
fof(f585, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP131), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP131), inference(nnf_transformation, [], [f141])).
fof(f1923, plain, (~ spl154_199 | ~ spl154_177), inference(avatar_split_clause, [], [f582, f1800, f1910])).
fof(f582, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP132), inference(cnf_transformation, [], [f186])).
fof(f186, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP132), inference(nnf_transformation, [], [f142])).
fof(f1908, plain, (~ spl154_196 | ~ spl154_163), inference(avatar_split_clause, [], [f579, f1729, f1895])).
fof(f579, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP133), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP133), inference(nnf_transformation, [], [f143])).
fof(f1893, plain, (~ spl154_193 | ~ spl154_146), inference(avatar_split_clause, [], [f576, f1646, f1880])).
fof(f576, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP134), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP134), inference(nnf_transformation, [], [f144])).
fof(f1868, plain, (~ spl154_189 | ~ spl154_190), inference(avatar_split_clause, [], [f575, f1865, f1861])).
fof(f575, plain, (~ (e0 = op(op(e1, e0), e1)) | ~ sP135), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP135), inference(nnf_transformation, [], [f145])).
fof(f1859, plain, ~ spl154_187, inference(avatar_split_clause, [], [f972, f1850])).
fof(f972, plain, ~ sP136, inference(trivial_inequality_removal, [], [f570])).
fof(f570, plain, (~ (op(e1, e1) = op(e1, e1)) | ~ sP136), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP136), inference(nnf_transformation, [], [f146])).
fof(f1848, plain, (~ spl154_184 | ~ spl154_173), inference(avatar_split_clause, [], [f567, f1781, f1835])).
fof(f567, plain, (~ (op(e1, e2) = op(e2, e1)) | ~ sP137), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP137), inference(nnf_transformation, [], [f147])).
fof(f1833, plain, (~ spl154_181 | ~ spl154_159), inference(avatar_split_clause, [], [f564, f1710, f1820])).
fof(f564, plain, (~ (op(e1, e3) = op(e3, e1)) | ~ sP138), inference(cnf_transformation, [], [f180])).
fof(f180, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP138), inference(nnf_transformation, [], [f148])).
fof(f1818, plain, (~ spl154_178 | ~ spl154_142), inference(avatar_split_clause, [], [f561, f1627, f1805])).
fof(f561, plain, (~ (op(e1, e4) = op(e4, e1)) | ~ sP139), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP139), inference(nnf_transformation, [], [f149])).
fof(f1803, plain, (~ spl154_174 | ~ spl154_177), inference(avatar_split_clause, [], [f558, f1800, f1786])).
fof(f558, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP140), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP140), inference(nnf_transformation, [], [f150])).
fof(f1784, plain, (~ spl154_170 | ~ spl154_173), inference(avatar_split_clause, [], [f555, f1781, f1767])).
fof(f555, plain, (~ (op(e1, e2) = op(e2, e1)) | ~ sP141), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP141), inference(nnf_transformation, [], [f151])).
fof(f1761, plain, (~ spl154_167 | spl154_169), inference(avatar_split_clause, [], [f550, f1758, f1749])).
fof(f550, plain, ((e2 = op(op(e2, e3), e3)) | ~ sP143), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP143), inference(nnf_transformation, [], [f153])).
fof(f1746, plain, (~ spl154_164 | spl154_166), inference(avatar_split_clause, [], [f547, f1743, f1734])).
fof(f547, plain, ((e2 = op(op(e2, e4), e4)) | ~ sP144), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP144), inference(nnf_transformation, [], [f154])).
fof(f1732, plain, (~ spl154_160 | ~ spl154_163), inference(avatar_split_clause, [], [f543, f1729, f1715])).
fof(f543, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP145), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP145), inference(nnf_transformation, [], [f155])).
fof(f1708, plain, (~ spl154_156 | spl154_158), inference(avatar_split_clause, [], [f541, f1705, f1696])).
fof(f541, plain, ((e3 = op(op(e3, e1), e1)) | ~ sP146), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP146), inference(nnf_transformation, [], [f156])).
fof(f1689, plain, (~ spl154_152 | spl154_154), inference(avatar_split_clause, [], [f538, f1686, f1677])).
fof(f538, plain, ((e3 = op(op(e3, e2), e2)) | ~ sP147), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP147), inference(nnf_transformation, [], [f157])).
fof(f1675, plain, ~ spl154_150, inference(avatar_split_clause, [], [f974, f1666])).
fof(f974, plain, ~ sP148, inference(trivial_inequality_removal, [], [f534])).
fof(f534, plain, (~ (op(e3, e3) = op(e3, e3)) | ~ sP148), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP148), inference(nnf_transformation, [], [f158])).
fof(f1663, plain, (~ spl154_147 | spl154_149), inference(avatar_split_clause, [], [f532, f1660, f1651])).
fof(f532, plain, ((e3 = op(op(e3, e4), e4)) | ~ sP149), inference(cnf_transformation, [], [f169])).
fof(f169, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP149), inference(nnf_transformation, [], [f159])).
fof(f1649, plain, (~ spl154_143 | ~ spl154_146), inference(avatar_split_clause, [], [f528, f1646, f1632])).
fof(f528, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP150), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP150), inference(nnf_transformation, [], [f160])).
fof(f1625, plain, (~ spl154_139 | spl154_141), inference(avatar_split_clause, [], [f526, f1622, f1613])).
fof(f526, plain, ((e4 = op(op(e4, e1), e1)) | ~ sP151), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP151), inference(nnf_transformation, [], [f161])).
fof(f1606, plain, (~ spl154_135 | spl154_137), inference(avatar_split_clause, [], [f523, f1603, f1594])).
fof(f523, plain, ((e4 = op(op(e4, e2), e2)) | ~ sP152), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP152), inference(nnf_transformation, [], [f162])).
fof(f1587, plain, (~ spl154_131 | spl154_133), inference(avatar_split_clause, [], [f520, f1584, f1575])).
fof(f520, plain, ((e4 = op(op(e4, e3), e3)) | ~ sP153), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP153), inference(nnf_transformation, [], [f163])).
fof(f1573, plain, spl154_91, inference(avatar_split_clause, [], [f1572, f1354])).
fof(f1572, plain, (e0 = op(e1, e1)), inference(forward_demodulation, [], [f515, f516])).
fof(f515, plain, (e0 = op(op(op(e2, e2), e2), op(op(e2, e2), e2))), inference(cnf_transformation, [], [f6])).
fof(f1571, plain, spl154_64, inference(avatar_split_clause, [], [f517, f1240])).
fof(f517, plain, (e3 = op(e2, e2)), inference(cnf_transformation, [], [f6])).
fof(f1550, plain, (spl154_71 | spl154_66 | spl154_61 | spl154_56 | spl154_51), inference(avatar_split_clause, [], [f375, f1186, f1207, f1228, f1249, f1270])).
fof(f375, plain, ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', ax3)).
fof(f1549, plain, (spl154_111 | spl154_86 | spl154_61 | spl154_36 | spl154_11), inference(avatar_split_clause, [], [f376, f1018, f1123, f1228, f1333, f1438])).
fof(f376, plain, ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f1548, plain, (spl154_72 | spl154_67 | spl154_62 | spl154_57 | spl154_52), inference(avatar_split_clause, [], [f377, f1190, f1211, f1232, f1253, f1274])).
fof(f377, plain, ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f1540, plain, (spl154_46 | spl154_41 | spl154_36 | spl154_31 | spl154_26), inference(avatar_split_clause, [], [f385, f1081, f1102, f1123, f1144, f1165])).
fof(f385, plain, ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f1537, plain, (spl154_107 | spl154_82 | spl154_57 | spl154_32 | spl154_7), inference(avatar_split_clause, [], [f388, f1001, f1106, f1211, f1316, f1421])).
fof(f388, plain, ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f1526, plain, (spl154_23 | spl154_18 | spl154_13 | spl154_8 | spl154_3), inference(avatar_split_clause, [], [f399, f984, f1005, f1026, f1047, f1068])).
fof(f399, plain, ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f1523, plain, (spl154_104 | spl154_79 | spl154_54 | spl154_29 | spl154_4), inference(avatar_split_clause, [], [f402, f988, f1093, f1198, f1303, f1408])).
fof(f402, plain, ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f1520, plain, (spl154_126 | spl154_127 | spl154_128 | spl154_129 | spl154_130), inference(avatar_split_clause, [], [f354, f1517, f1513, f1509, f1505, f1501])).
fof(f354, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f1352, plain, (spl154_86 | spl154_87 | spl154_88 | spl154_89 | spl154_90), inference(avatar_split_clause, [], [f326, f1349, f1345, f1341, f1337, f1333])).
fof(f326, plain, ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG059+1.p', ax1)).
fof(f1331, plain, (spl154_81 | spl154_82 | spl154_83 | spl154_84 | spl154_85), inference(avatar_split_clause, [], [f327, f1328, f1324, f1320, f1316, f1312])).
fof(f327, plain, ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).