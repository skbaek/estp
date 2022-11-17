fof(f10064, plain, $false, inference(avatar_sat_refutation, [], [f948, f969, f1011, f1095, f1116, f1137, f1179, f1200, f1263, f1347, f1351, f1352, f1353, f1358, f1364, f1366, f1374, f1375, f1377, f1382, f1395, f1439, f1481, f1502, f1565, f1691, f1712, f1733, f1775, f1943, f1948, f1949, f1954, f1955, f1958, f1959, f1960, f1962, f1970, f1971, f1973, f1974, f2035, f2044, f2115, f2124, f2125, f2166, f2172, f2254, f2264, f2395, f2404, f2405, f2534, f2544, f2554, f2556, f2835, f2850, f2865, f2880, f3590, f3962, f4007, f4016, f4040, f4048, f4061, f4090, f4132, f4150, f4185, f4191, f4198, f4200, f4244, f4246, f4266, f4283, f4299, f4353, f4364, f4385, f4452, f4454, f4456, f4457, f4458, f4471, f4474, f4504, f4520, f4524, f4539, f4577, f4584, f4594, f4607, f4620, f4630, f4642, f4675, f4684, f4737, f4742, f4762, f4763, f4764, f4765, f4766, f4769, f4791, f4827, f4868, f4910, f4926, f4929, f4940, f4957, f4973, f4991, f5009, f5034, f5038, f5069, f5088, f5100, f5106, f5107, f5113, f5135, f5153, f5158, f5189, f5230, f5265, f5283, f5337, f5348, f5356, f5361, f5371, f5375, f5400, f5406, f5436, f5444, f5454, f5462, f5489, f5510, f5574, f5602, f5645, f5696, f5703, f5723, f5775, f5781, f5802, f5815, f5817, f5839, f5868, f5907, f5930, f5951, f5959, f5963, f6002, f6065, f6098, f6121, f6132, f6159, f6213, f6244, f6247, f6288, f6293, f6313, f6353, f6423, f6456, f6459, f6520, f6568, f6580, f6604, f6671, f6685, f6708, f6711, f6752, f6792, f6804, f6882, f6950, f6993, f7004, f7015, f7048, f7077, f7191, f7222, f7227, f7245, f7259, f7361, f7407, f7451, f7468, f7501, f7564, f7565, f7566, f7567, f7568, f7569, f7570, f7571, f7574, f7605, f7606, f7607, f7618, f7630, f7635, f7648, f7676, f7695, f7707, f7711, f7732, f7910, f7911, f7914, f7915, f7917, f7918, f7919, f7931, f8017, f8098, f8135, f8221, f8229, f8243, f8245, f8271, f8294, f8356, f8414, f8415, f8416, f8417, f8418, f8419, f8420, f8421, f8422, f8447, f8477, f8515, f8525, f8556, f8579, f8645, f8664, f8710, f8754, f8797, f8836, f8877, f8919, f8952, f8985, f9023, f9058, f9092, f9125, f9157, f9190, f9224, f9252, f9276, f9293, f9317, f9342, f9366, f9401, f9415, f9456, f9496, f9497, f9499, f9500, f9501, f9502, f9503, f9504, f9505, f9684, f9763, f9777, f9794, f9831, f10037])).
fof(f10037, plain, (~ spl40_24 | ~ spl40_107 | ~ spl40_312), inference(avatar_contradiction_clause, [], [f10036])).
fof(f10036, plain, ($false | (~ spl40_24 | ~ spl40_107 | ~ spl40_312)), inference(subsumption_resolution, [], [f10035, f485])).
fof(f485, plain, ~ (e11 = e14), inference(cnf_transformation, [], [f9])).
fof(f9, plain, (~ (e13 = e14) & ~ (e12 = e14) & ~ (e12 = e13) & ~ (e11 = e14) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e14) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax9)).
fof(f10035, plain, ((e11 = e14) | (~ spl40_24 | ~ spl40_107 | ~ spl40_312)), inference(forward_demodulation, [], [f10034, f1250])).
fof(f1250, plain, ((e11 = op1(e10, e13)) | ~ spl40_107), inference(avatar_component_clause, [], [f1248])).
fof(f1248, plain, (spl40_107 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_107])])).
fof(f10034, plain, ((e14 = op1(e10, e13)) | (~ spl40_24 | ~ spl40_312)), inference(forward_demodulation, [], [f2253, f901])).
fof(f901, plain, ((e13 = op1(e14, e10)) | ~ spl40_24), inference(avatar_component_clause, [], [f899])).
fof(f899, plain, (spl40_24 <=> (e13 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_24])])).
fof(f2253, plain, ((e14 = op1(e10, op1(e14, e10))) | ~ spl40_312), inference(avatar_component_clause, [], [f2251])).
fof(f2251, plain, (spl40_312 <=> (e14 = op1(e10, op1(e14, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_312])])).
fof(f9831, plain, (~ spl40_1 | ~ spl40_3), inference(avatar_contradiction_clause, [], [f9830])).
fof(f9830, plain, ($false | (~ spl40_1 | ~ spl40_3)), inference(subsumption_resolution, [], [f9829, f480])).
fof(f480, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f9])).
fof(f9829, plain, ((e10 = e12) | (~ spl40_1 | ~ spl40_3)), inference(forward_demodulation, [], [f813, f805])).
fof(f805, plain, ((e10 = op1(e14, e14)) | ~ spl40_1), inference(avatar_component_clause, [], [f803])).
fof(f803, plain, (spl40_1 <=> (e10 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_1])])).
fof(f813, plain, ((e12 = op1(e14, e14)) | ~ spl40_3), inference(avatar_component_clause, [], [f811])).
fof(f811, plain, (spl40_3 <=> (e12 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_3])])).
fof(f9794, plain, (~ spl40_19 | ~ spl40_44), inference(avatar_split_clause, [], [f9785, f983, f878])).
fof(f878, plain, (spl40_19 <=> (e13 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_19])])).
fof(f983, plain, (spl40_44 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_44])])).
fof(f9785, plain, (~ (e13 = op1(e14, e11)) | ~ spl40_44), inference(backward_demodulation, [], [f298, f985])).
fof(f985, plain, ((e13 = op1(e13, e11)) | ~ spl40_44), inference(avatar_component_clause, [], [f983])).
fof(f298, plain, ~ (op1(e13, e11) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (op1(e14, e13) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e14)) & ~ (op1(e14, e11) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e14)) & ~ (op1(e14, e10) = op1(e14, e13)) & ~ (op1(e14, e10) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e11)) & ~ (op1(e13, e13) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e14)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e14)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e13) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e14)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e14)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e13) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e14)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e14)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e13) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e14)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e14)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e13, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e14, e14)) & ~ (op1(e11, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e14, e14)) & ~ (op1(e10, e14) = op1(e13, e14)) & ~ (op1(e10, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e11, e14)) & ~ (op1(e13, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e14, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e14, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e13, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e14, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e14, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e13, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e14, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e14, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e13, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e14, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e14, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax7)).
fof(f9777, plain, (~ spl40_36 | ~ spl40_48 | spl40_314), inference(avatar_contradiction_clause, [], [f9776])).
fof(f9776, plain, ($false | (~ spl40_36 | ~ spl40_48 | spl40_314)), inference(subsumption_resolution, [], [f9769, f952])).
fof(f952, plain, ((e10 = op1(e13, e12)) | ~ spl40_36), inference(avatar_component_clause, [], [f950])).
fof(f950, plain, (spl40_36 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_36])])).
fof(f9769, plain, (~ (e10 = op1(e13, e12)) | (~ spl40_48 | spl40_314)), inference(backward_demodulation, [], [f2263, f1002])).
fof(f1002, plain, ((e12 = op1(e13, e10)) | ~ spl40_48), inference(avatar_component_clause, [], [f1000])).
fof(f1000, plain, (spl40_48 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_48])])).
fof(f2263, plain, (~ (e10 = op1(e13, op1(e13, e10))) | spl40_314), inference(avatar_component_clause, [], [f2261])).
fof(f2261, plain, (spl40_314 <=> (e10 = op1(e13, op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_314])])).
fof(f9763, plain, (~ spl40_56 | ~ spl40_57), inference(avatar_contradiction_clause, [], [f9762])).
fof(f9762, plain, ($false | (~ spl40_56 | ~ spl40_57)), inference(subsumption_resolution, [], [f9761, f479])).
fof(f479, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f9])).
fof(f9761, plain, ((e10 = e11) | (~ spl40_56 | ~ spl40_57)), inference(backward_demodulation, [], [f1040, f1036])).
fof(f1036, plain, ((e10 = op1(e12, e13)) | ~ spl40_56), inference(avatar_component_clause, [], [f1034])).
fof(f1034, plain, (spl40_56 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_56])])).
fof(f1040, plain, ((e11 = op1(e12, e13)) | ~ spl40_57), inference(avatar_component_clause, [], [f1038])).
fof(f1038, plain, (spl40_57 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_57])])).
fof(f9684, plain, (~ spl40_78 | ~ spl40_88), inference(avatar_split_clause, [], [f9677, f1168, f1126])).
fof(f1126, plain, (spl40_78 <=> (e12 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_78])])).
fof(f1168, plain, (spl40_88 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_88])])).
fof(f9677, plain, (~ (e12 = op1(e11, e14)) | ~ spl40_88), inference(backward_demodulation, [], [f347, f1170])).
fof(f1170, plain, ((e12 = op1(e11, e12)) | ~ spl40_88), inference(avatar_component_clause, [], [f1168])).
fof(f347, plain, ~ (op1(e11, e12) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f9505, plain, (spl40_20 | ~ spl40_127), inference(avatar_split_clause, [], [f9492, f1332, f882])).
fof(f882, plain, (spl40_20 <=> (e14 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_20])])).
fof(f1332, plain, (spl40_127 <=> (e11 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl40_127])])).
fof(f9492, plain, ((e14 = op1(e14, e11)) | ~ spl40_127), inference(backward_demodulation, [], [f141, f1334])).
fof(f1334, plain, ((e11 = unit1) | ~ spl40_127), inference(avatar_component_clause, [], [f1332])).
fof(f141, plain, (e14 = op1(e14, unit1)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e14 = unit1) | (e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)) & (e14 = op1(e14, unit1)) & (e14 = op1(unit1, e14)) & (e13 = op1(e13, unit1)) & (e13 = op1(unit1, e13)) & (e12 = op1(e12, unit1)) & (e12 = op1(unit1, e12)) & (e11 = op1(e11, unit1)) & (e11 = op1(unit1, e11)) & (e10 = op1(e10, unit1)) & (e10 = op1(unit1, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax2)).
fof(f9504, plain, (spl40_80 | ~ spl40_127), inference(avatar_split_clause, [], [f9491, f1332, f1134])).
fof(f1134, plain, (spl40_80 <=> (e14 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_80])])).
fof(f9491, plain, ((e14 = op1(e11, e14)) | ~ spl40_127), inference(backward_demodulation, [], [f140, f1334])).
fof(f140, plain, (e14 = op1(unit1, e14)), inference(cnf_transformation, [], [f2])).
fof(f9503, plain, (spl40_44 | ~ spl40_127), inference(avatar_split_clause, [], [f9490, f1332, f983])).
fof(f9490, plain, ((e13 = op1(e13, e11)) | ~ spl40_127), inference(backward_demodulation, [], [f139, f1334])).
fof(f139, plain, (e13 = op1(e13, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9502, plain, (spl40_84 | ~ spl40_127), inference(avatar_split_clause, [], [f9489, f1332, f1151])).
fof(f1151, plain, (spl40_84 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_84])])).
fof(f9489, plain, ((e13 = op1(e11, e13)) | ~ spl40_127), inference(backward_demodulation, [], [f138, f1334])).
fof(f138, plain, (e13 = op1(unit1, e13)), inference(cnf_transformation, [], [f2])).
fof(f9501, plain, (spl40_68 | ~ spl40_127), inference(avatar_split_clause, [], [f9488, f1332, f1084])).
fof(f1084, plain, (spl40_68 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_68])])).
fof(f9488, plain, ((e12 = op1(e12, e11)) | ~ spl40_127), inference(backward_demodulation, [], [f137, f1334])).
fof(f137, plain, (e12 = op1(e12, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9500, plain, (spl40_88 | ~ spl40_127), inference(avatar_split_clause, [], [f9487, f1332, f1168])).
fof(f9487, plain, ((e12 = op1(e11, e12)) | ~ spl40_127), inference(backward_demodulation, [], [f136, f1334])).
fof(f136, plain, (e12 = op1(unit1, e12)), inference(cnf_transformation, [], [f2])).
fof(f9499, plain, (spl40_92 | ~ spl40_127), inference(avatar_split_clause, [], [f9486, f1332, f1185])).
fof(f1185, plain, (spl40_92 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_92])])).
fof(f9486, plain, ((e11 = op1(e11, e11)) | ~ spl40_127), inference(backward_demodulation, [], [f135, f1334])).
fof(f135, plain, (e11 = op1(e11, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9497, plain, (spl40_116 | ~ spl40_127), inference(avatar_split_clause, [], [f9484, f1332, f1286])).
fof(f1286, plain, (spl40_116 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_116])])).
fof(f9484, plain, ((e10 = op1(e10, e11)) | ~ spl40_127), inference(backward_demodulation, [], [f133, f1334])).
fof(f133, plain, (e10 = op1(e10, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9496, plain, (spl40_96 | ~ spl40_127), inference(avatar_split_clause, [], [f9483, f1332, f1202])).
fof(f1202, plain, (spl40_96 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_96])])).
fof(f9483, plain, ((e10 = op1(e11, e10)) | ~ spl40_127), inference(backward_demodulation, [], [f132, f1334])).
fof(f132, plain, (e10 = op1(unit1, e10)), inference(cnf_transformation, [], [f2])).
fof(f9456, plain, (~ spl40_110 | ~ spl40_35), inference(avatar_split_clause, [], [f9455, f945, f1260])).
fof(f1260, plain, (spl40_110 <=> (e14 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_110])])).
fof(f945, plain, (spl40_35 <=> (e14 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_35])])).
fof(f9455, plain, (~ (e14 = op1(e10, e13)) | ~ spl40_35), inference(forward_demodulation, [], [f311, f947])).
fof(f947, plain, ((e14 = op1(e13, e13)) | ~ spl40_35), inference(avatar_component_clause, [], [f945])).
fof(f311, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f9415, plain, (~ spl40_72 | ~ spl40_57), inference(avatar_split_clause, [], [f9414, f1038, f1101])).
fof(f1101, plain, (spl40_72 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_72])])).
fof(f9414, plain, (~ (e11 = op1(e12, e10)) | ~ spl40_57), inference(forward_demodulation, [], [f351, f1040])).
fof(f351, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f9401, plain, (~ spl40_50 | ~ spl40_35), inference(avatar_split_clause, [], [f9400, f945, f1008])).
fof(f1008, plain, (spl40_50 <=> (e14 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_50])])).
fof(f9400, plain, (~ (e14 = op1(e13, e10)) | ~ spl40_35), inference(forward_demodulation, [], [f361, f947])).
fof(f361, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f9366, plain, (~ spl40_73 | ~ spl40_203 | spl40_532), inference(avatar_contradiction_clause, [], [f9365])).
fof(f9365, plain, ($false | (~ spl40_73 | ~ spl40_203 | spl40_532)), inference(subsumption_resolution, [], [f9364, f1703])).
fof(f1703, plain, ((e22 = op2(e22, e20)) | ~ spl40_203), inference(avatar_component_clause, [], [f1701])).
fof(f1701, plain, (spl40_203 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_203])])).
fof(f9364, plain, (~ (e22 = op2(e22, e20)) | (~ spl40_73 | spl40_532)), inference(forward_demodulation, [], [f9363, f662])).
fof(f662, plain, (e22 = h3(e12)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ((op2(e22, e22) = h3(e14)) & (op2(e22, op2(e22, e22)) = h3(e13)) & (op2(op2(e22, op2(e22, e22)), op2(e22, e22)) = h3(e11)) & (op2(op2(e22, e22), op2(e22, e22)) = h3(e10)) & (e22 = h3(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax18)).
fof(f9363, plain, (~ (op2(e22, e20) = h3(e12)) | (~ spl40_73 | spl40_532)), inference(forward_demodulation, [], [f3549, f1107])).
fof(f1107, plain, ((e12 = op1(e12, e10)) | ~ spl40_73), inference(avatar_component_clause, [], [f1105])).
fof(f1105, plain, (spl40_73 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_73])])).
fof(f3549, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | spl40_532), inference(avatar_component_clause, [], [f3547])).
fof(f3547, plain, (spl40_532 <=> (op2(e22, e20) = h3(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_532])])).
fof(f9342, plain, (~ spl40_121 | ~ spl40_455 | spl40_542), inference(avatar_contradiction_clause, [], [f9341])).
fof(f9341, plain, ($false | (~ spl40_121 | ~ spl40_455 | spl40_542)), inference(subsumption_resolution, [], [f9340, f3063])).
fof(f3063, plain, ((e20 = h1(e14)) | ~ spl40_455), inference(avatar_component_clause, [], [f3062])).
fof(f3062, plain, (spl40_455 <=> (e20 = h1(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_455])])).
fof(f9340, plain, (~ (e20 = h1(e14)) | (~ spl40_121 | spl40_542)), inference(forward_demodulation, [], [f9339, f2599])).
fof(f2599, plain, (e20 = h3(e10)), inference(forward_demodulation, [], [f2598, f2590])).
fof(f2590, plain, (e20 = op2(h3(e14), h3(e14))), inference(backward_demodulation, [], [f648, f666])).
fof(f666, plain, (op2(e22, e22) = h3(e14)), inference(cnf_transformation, [], [f18])).
fof(f648, plain, (e20 = op2(op2(e22, e22), op2(e22, e22))), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((e24 = op2(e22, e22)) & (e23 = op2(e22, op2(e22, e22))) & (e21 = op2(op2(e22, op2(e22, e22)), op2(e22, e22))) & (e20 = op2(op2(e22, e22), op2(e22, e22)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax15)).
fof(f2598, plain, (h3(e10) = op2(h3(e14), h3(e14))), inference(forward_demodulation, [], [f663, f666])).
fof(f663, plain, (op2(op2(e22, e22), op2(e22, e22)) = h3(e10)), inference(cnf_transformation, [], [f18])).
fof(f9339, plain, (~ (h1(e14) = h3(e10)) | (~ spl40_121 | spl40_542)), inference(forward_demodulation, [], [f3589, f1309])).
fof(f1309, plain, ((e10 = op1(e10, e10)) | ~ spl40_121), inference(avatar_component_clause, [], [f1307])).
fof(f1307, plain, (spl40_121 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_121])])).
fof(f3589, plain, (~ (h1(e14) = h3(op1(e10, e10))) | spl40_542), inference(avatar_component_clause, [], [f3587])).
fof(f3587, plain, (spl40_542 <=> (h1(e14) = h3(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_542])])).
fof(f9317, plain, (~ spl40_66 | ~ spl40_196 | spl40_531), inference(avatar_contradiction_clause, [], [f9316])).
fof(f9316, plain, ($false | (~ spl40_66 | ~ spl40_196 | spl40_531)), inference(subsumption_resolution, [], [f9315, f1674])).
fof(f1674, plain, ((e20 = op2(e22, e21)) | ~ spl40_196), inference(avatar_component_clause, [], [f1672])).
fof(f1672, plain, (spl40_196 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_196])])).
fof(f9315, plain, (~ (e20 = op2(e22, e21)) | (~ spl40_66 | spl40_531)), inference(forward_demodulation, [], [f9314, f2599])).
fof(f9314, plain, (~ (op2(e22, e21) = h3(e10)) | (~ spl40_66 | spl40_531)), inference(forward_demodulation, [], [f3545, f1078])).
fof(f1078, plain, ((e10 = op1(e12, e11)) | ~ spl40_66), inference(avatar_component_clause, [], [f1076])).
fof(f1076, plain, (spl40_66 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_66])])).
fof(f3545, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | spl40_531), inference(avatar_component_clause, [], [f3543])).
fof(f3543, plain, (spl40_531 <=> (op2(e22, e21) = h3(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl40_531])])).
fof(f9293, plain, (~ spl40_54 | spl40_528), inference(avatar_contradiction_clause, [], [f9292])).
fof(f9292, plain, ($false | (~ spl40_54 | spl40_528)), inference(subsumption_resolution, [], [f9291, f2594])).
fof(f2594, plain, (e23 = h3(e13)), inference(forward_demodulation, [], [f2593, f2591])).
fof(f2591, plain, (e23 = op2(e22, h3(e14))), inference(backward_demodulation, [], [f650, f666])).
fof(f650, plain, (e23 = op2(e22, op2(e22, e22))), inference(cnf_transformation, [], [f15])).
fof(f2593, plain, (h3(e13) = op2(e22, h3(e14))), inference(forward_demodulation, [], [f665, f666])).
fof(f665, plain, (op2(e22, op2(e22, e22)) = h3(e13)), inference(cnf_transformation, [], [f18])).
fof(f9291, plain, (~ (e23 = h3(e13)) | (~ spl40_54 | spl40_528)), inference(forward_demodulation, [], [f3533, f1027])).
fof(f1027, plain, ((e13 = op1(e12, e14)) | ~ spl40_54), inference(avatar_component_clause, [], [f1025])).
fof(f1025, plain, (spl40_54 <=> (e13 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_54])])).
fof(f3533, plain, (~ (e23 = h3(op1(e12, e14))) | spl40_528), inference(avatar_component_clause, [], [f3531])).
fof(f3531, plain, (spl40_528 <=> (e23 = h3(op1(e12, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl40_528])])).
fof(f9276, plain, (~ spl40_35 | ~ spl40_502 | ~ spl40_517 | spl40_524), inference(avatar_contradiction_clause, [], [f9275])).
fof(f9275, plain, ($false | (~ spl40_35 | ~ spl40_502 | ~ spl40_517 | spl40_524)), inference(subsumption_resolution, [], [f9274, f3488])).
fof(f3488, plain, ((e24 = h3(e14)) | ~ spl40_517), inference(avatar_component_clause, [], [f3487])).
fof(f3487, plain, (spl40_517 <=> (e24 = h3(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_517])])).
fof(f9274, plain, (~ (e24 = h3(e14)) | (~ spl40_35 | ~ spl40_502 | spl40_524)), inference(forward_demodulation, [], [f9273, f3341])).
fof(f3341, plain, ((e24 = h4(e14)) | ~ spl40_502), inference(avatar_component_clause, [], [f3340])).
fof(f3340, plain, (spl40_502 <=> (e24 = h4(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_502])])).
fof(f9273, plain, (~ (h3(e14) = h4(e14)) | (~ spl40_35 | spl40_524)), inference(forward_demodulation, [], [f3517, f947])).
fof(f3517, plain, (~ (h4(e14) = h3(op1(e13, e13))) | spl40_524), inference(avatar_component_clause, [], [f3515])).
fof(f3515, plain, (spl40_524 <=> (h4(e14) = h3(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_524])])).
fof(f9252, plain, (~ spl40_105 | ~ spl40_235 | ~ spl40_517 | spl40_538), inference(avatar_contradiction_clause, [], [f9251])).
fof(f9251, plain, ($false | (~ spl40_105 | ~ spl40_235 | ~ spl40_517 | spl40_538)), inference(subsumption_resolution, [], [f9250, f1837])).
fof(f1837, plain, ((e24 = op2(e20, e24)) | ~ spl40_235), inference(avatar_component_clause, [], [f1835])).
fof(f1835, plain, (spl40_235 <=> (e24 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_235])])).
fof(f9250, plain, (~ (e24 = op2(e20, e24)) | (~ spl40_105 | ~ spl40_517 | spl40_538)), inference(forward_demodulation, [], [f9249, f3488])).
fof(f9249, plain, (~ (op2(e20, e24) = h3(e14)) | (~ spl40_105 | ~ spl40_517 | spl40_538)), inference(forward_demodulation, [], [f9248, f1241])).
fof(f1241, plain, ((e14 = op1(e10, e14)) | ~ spl40_105), inference(avatar_component_clause, [], [f1239])).
fof(f1239, plain, (spl40_105 <=> (e14 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_105])])).
fof(f9248, plain, (~ (op2(e20, e24) = h3(op1(e10, e14))) | (~ spl40_517 | spl40_538)), inference(forward_demodulation, [], [f3573, f3488])).
fof(f3573, plain, (~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | spl40_538), inference(avatar_component_clause, [], [f3571])).
fof(f3571, plain, (spl40_538 <=> (h3(op1(e10, e14)) = op2(e20, h3(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl40_538])])).
fof(f9224, plain, (~ spl40_89 | ~ spl40_219 | spl40_535), inference(avatar_contradiction_clause, [], [f9223])).
fof(f9223, plain, ($false | (~ spl40_89 | ~ spl40_219 | spl40_535)), inference(subsumption_resolution, [], [f9222, f1770])).
fof(f1770, plain, ((e23 = op2(e21, e22)) | ~ spl40_219), inference(avatar_component_clause, [], [f1768])).
fof(f1768, plain, (spl40_219 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_219])])).
fof(f9222, plain, (~ (e23 = op2(e21, e22)) | (~ spl40_89 | spl40_535)), inference(forward_demodulation, [], [f9221, f2594])).
fof(f9221, plain, (~ (op2(e21, e22) = h3(e13)) | (~ spl40_89 | spl40_535)), inference(forward_demodulation, [], [f3561, f1174])).
fof(f1174, plain, ((e13 = op1(e11, e12)) | ~ spl40_89), inference(avatar_component_clause, [], [f1172])).
fof(f1172, plain, (spl40_89 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_89])])).
fof(f3561, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | spl40_535), inference(avatar_component_clause, [], [f3559])).
fof(f3559, plain, (spl40_535 <=> (op2(e21, e22) = h3(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl40_535])])).
fof(f9190, plain, (~ spl40_65 | spl40_530), inference(avatar_contradiction_clause, [], [f9189])).
fof(f9189, plain, ($false | (~ spl40_65 | spl40_530)), inference(trivial_inequality_removal, [], [f9188])).
fof(f9188, plain, (~ (h3(e14) = h3(e14)) | (~ spl40_65 | spl40_530)), inference(forward_demodulation, [], [f3541, f1073])).
fof(f1073, plain, ((e14 = op1(e12, e12)) | ~ spl40_65), inference(avatar_component_clause, [], [f1071])).
fof(f1071, plain, (spl40_65 <=> (e14 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_65])])).
fof(f3541, plain, (~ (h3(e14) = h3(op1(e12, e12))) | spl40_530), inference(avatar_component_clause, [], [f3539])).
fof(f3539, plain, (spl40_530 <=> (h3(e14) = h3(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl40_530])])).
fof(f9157, plain, (~ spl40_49 | ~ spl40_179 | spl40_527), inference(avatar_contradiction_clause, [], [f9156])).
fof(f9156, plain, ($false | (~ spl40_49 | ~ spl40_179 | spl40_527)), inference(subsumption_resolution, [], [f9155, f1602])).
fof(f1602, plain, ((e23 = op2(e23, e20)) | ~ spl40_179), inference(avatar_component_clause, [], [f1600])).
fof(f1600, plain, (spl40_179 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_179])])).
fof(f9155, plain, (~ (e23 = op2(e23, e20)) | (~ spl40_49 | spl40_527)), inference(forward_demodulation, [], [f9154, f2594])).
fof(f9154, plain, (~ (op2(e23, e20) = h3(e13)) | (~ spl40_49 | spl40_527)), inference(forward_demodulation, [], [f3529, f1006])).
fof(f1006, plain, ((e13 = op1(e13, e10)) | ~ spl40_49), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl40_49 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_49])])).
fof(f3529, plain, (~ (op2(e23, e20) = h3(op1(e13, e10))) | spl40_527), inference(avatar_component_clause, [], [f3527])).
fof(f3527, plain, (spl40_527 <=> (op2(e23, e20) = h3(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_527])])).
fof(f9125, plain, (~ spl40_36 | ~ spl40_166 | spl40_525), inference(avatar_contradiction_clause, [], [f9124])).
fof(f9124, plain, ($false | (~ spl40_36 | ~ spl40_166 | spl40_525)), inference(subsumption_resolution, [], [f9123, f1548])).
fof(f1548, plain, ((e20 = op2(e23, e22)) | ~ spl40_166), inference(avatar_component_clause, [], [f1546])).
fof(f1546, plain, (spl40_166 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_166])])).
fof(f9123, plain, (~ (e20 = op2(e23, e22)) | (~ spl40_36 | spl40_525)), inference(forward_demodulation, [], [f9122, f2599])).
fof(f9122, plain, (~ (op2(e23, e22) = h3(e10)) | (~ spl40_36 | spl40_525)), inference(forward_demodulation, [], [f3521, f952])).
fof(f3521, plain, (~ (op2(e23, e22) = h3(op1(e13, e12))) | spl40_525), inference(avatar_component_clause, [], [f3519])).
fof(f3519, plain, (spl40_525 <=> (op2(e23, e22) = h3(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl40_525])])).
fof(f9092, plain, (~ spl40_27 | spl40_523), inference(avatar_contradiction_clause, [], [f9091])).
fof(f9091, plain, ($false | (~ spl40_27 | spl40_523)), inference(subsumption_resolution, [], [f9090, f2597])).
fof(f2597, plain, (e21 = h3(e11)), inference(forward_demodulation, [], [f2596, f2592])).
fof(f2592, plain, (e21 = op2(e23, h3(e14))), inference(backward_demodulation, [], [f2557, f666])).
fof(f2557, plain, (e21 = op2(e23, op2(e22, e22))), inference(forward_demodulation, [], [f649, f650])).
fof(f649, plain, (e21 = op2(op2(e22, op2(e22, e22)), op2(e22, e22))), inference(cnf_transformation, [], [f15])).
fof(f2596, plain, (h3(e11) = op2(e23, h3(e14))), inference(forward_demodulation, [], [f2595, f2591])).
fof(f2595, plain, (h3(e11) = op2(op2(e22, h3(e14)), h3(e14))), inference(forward_demodulation, [], [f664, f666])).
fof(f664, plain, (op2(op2(e22, op2(e22, e22)), op2(e22, e22)) = h3(e11)), inference(cnf_transformation, [], [f18])).
fof(f9090, plain, (~ (e21 = h3(e11)) | (~ spl40_27 | spl40_523)), inference(forward_demodulation, [], [f3513, f914])).
fof(f914, plain, ((e11 = op1(e13, e14)) | ~ spl40_27), inference(avatar_component_clause, [], [f912])).
fof(f912, plain, (spl40_27 <=> (e11 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_27])])).
fof(f3513, plain, (~ (e21 = h3(op1(e13, e14))) | spl40_523), inference(avatar_component_clause, [], [f3511])).
fof(f3511, plain, (spl40_523 <=> (e21 = h3(op1(e13, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl40_523])])).
fof(f9058, plain, (~ spl40_25 | ~ spl40_155 | ~ spl40_517 | spl40_522), inference(avatar_contradiction_clause, [], [f9057])).
fof(f9057, plain, ($false | (~ spl40_25 | ~ spl40_155 | ~ spl40_517 | spl40_522)), inference(subsumption_resolution, [], [f9056, f1501])).
fof(f1501, plain, ((e24 = op2(e24, e20)) | ~ spl40_155), inference(avatar_component_clause, [], [f1499])).
fof(f1499, plain, (spl40_155 <=> (e24 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_155])])).
fof(f9056, plain, (~ (e24 = op2(e24, e20)) | (~ spl40_25 | ~ spl40_517 | spl40_522)), inference(forward_demodulation, [], [f9055, f3488])).
fof(f9055, plain, (~ (op2(e24, e20) = h3(e14)) | (~ spl40_25 | ~ spl40_517 | spl40_522)), inference(forward_demodulation, [], [f9054, f905])).
fof(f905, plain, ((e14 = op1(e14, e10)) | ~ spl40_25), inference(avatar_component_clause, [], [f903])).
fof(f903, plain, (spl40_25 <=> (e14 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_25])])).
fof(f9054, plain, (~ (op2(e24, e20) = h3(op1(e14, e10))) | (~ spl40_517 | spl40_522)), inference(forward_demodulation, [], [f3509, f3488])).
fof(f3509, plain, (~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | spl40_522), inference(avatar_component_clause, [], [f3507])).
fof(f3507, plain, (spl40_522 <=> (h3(op1(e14, e10)) = op2(h3(e14), e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_522])])).
fof(f9023, plain, (~ spl40_19 | ~ spl40_149 | ~ spl40_517 | spl40_521), inference(avatar_contradiction_clause, [], [f9022])).
fof(f9022, plain, ($false | (~ spl40_19 | ~ spl40_149 | ~ spl40_517 | spl40_521)), inference(subsumption_resolution, [], [f9021, f1476])).
fof(f1476, plain, ((e23 = op2(e24, e21)) | ~ spl40_149), inference(avatar_component_clause, [], [f1474])).
fof(f1474, plain, (spl40_149 <=> (e23 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_149])])).
fof(f9021, plain, (~ (e23 = op2(e24, e21)) | (~ spl40_19 | ~ spl40_517 | spl40_521)), inference(forward_demodulation, [], [f9020, f2594])).
fof(f9020, plain, (~ (op2(e24, e21) = h3(e13)) | (~ spl40_19 | ~ spl40_517 | spl40_521)), inference(forward_demodulation, [], [f9019, f880])).
fof(f880, plain, ((e13 = op1(e14, e11)) | ~ spl40_19), inference(avatar_component_clause, [], [f878])).
fof(f9019, plain, (~ (op2(e24, e21) = h3(op1(e14, e11))) | (~ spl40_517 | spl40_521)), inference(forward_demodulation, [], [f3505, f3488])).
fof(f3505, plain, (~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | spl40_521), inference(avatar_component_clause, [], [f3503])).
fof(f3503, plain, (spl40_521 <=> (h3(op1(e14, e11)) = op2(h3(e14), e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_521])])).
fof(f8985, plain, (~ spl40_109 | ~ spl40_239 | spl40_539), inference(avatar_contradiction_clause, [], [f8984])).
fof(f8984, plain, ($false | (~ spl40_109 | ~ spl40_239 | spl40_539)), inference(subsumption_resolution, [], [f8983, f1854])).
fof(f1854, plain, ((e23 = op2(e20, e23)) | ~ spl40_239), inference(avatar_component_clause, [], [f1852])).
fof(f1852, plain, (spl40_239 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_239])])).
fof(f8983, plain, (~ (e23 = op2(e20, e23)) | (~ spl40_109 | spl40_539)), inference(forward_demodulation, [], [f8982, f2594])).
fof(f8982, plain, (~ (op2(e20, e23) = h3(e13)) | (~ spl40_109 | spl40_539)), inference(forward_demodulation, [], [f3577, f1258])).
fof(f1258, plain, ((e13 = op1(e10, e13)) | ~ spl40_109), inference(avatar_component_clause, [], [f1256])).
fof(f1256, plain, (spl40_109 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_109])])).
fof(f3577, plain, (~ (op2(e20, e23) = h3(op1(e10, e13))) | spl40_539), inference(avatar_component_clause, [], [f3575])).
fof(f3575, plain, (spl40_539 <=> (op2(e20, e23) = h3(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_539])])).
fof(f8952, plain, (~ spl40_95 | ~ spl40_517 | spl40_536 | ~ spl40_557), inference(avatar_contradiction_clause, [], [f8951])).
fof(f8951, plain, ($false | (~ spl40_95 | ~ spl40_517 | spl40_536 | ~ spl40_557)), inference(subsumption_resolution, [], [f8950, f3666])).
fof(f3666, plain, ((e24 = h2(e14)) | ~ spl40_557), inference(avatar_component_clause, [], [f3665])).
fof(f3665, plain, (spl40_557 <=> (e24 = h2(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_557])])).
fof(f8950, plain, (~ (e24 = h2(e14)) | (~ spl40_95 | ~ spl40_517 | spl40_536)), inference(forward_demodulation, [], [f8949, f3488])).
fof(f8949, plain, (~ (h2(e14) = h3(e14)) | (~ spl40_95 | spl40_536)), inference(forward_demodulation, [], [f3565, f1199])).
fof(f1199, plain, ((e14 = op1(e11, e11)) | ~ spl40_95), inference(avatar_component_clause, [], [f1197])).
fof(f1197, plain, (spl40_95 <=> (e14 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_95])])).
fof(f3565, plain, (~ (h2(e14) = h3(op1(e11, e11))) | spl40_536), inference(avatar_component_clause, [], [f3563])).
fof(f3563, plain, (spl40_536 <=> (h2(e14) = h3(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl40_536])])).
fof(f8919, plain, (~ spl40_78 | ~ spl40_208 | ~ spl40_517 | spl40_533), inference(avatar_contradiction_clause, [], [f8918])).
fof(f8918, plain, ($false | (~ spl40_78 | ~ spl40_208 | ~ spl40_517 | spl40_533)), inference(subsumption_resolution, [], [f8917, f1724])).
fof(f1724, plain, ((e22 = op2(e21, e24)) | ~ spl40_208), inference(avatar_component_clause, [], [f1722])).
fof(f1722, plain, (spl40_208 <=> (e22 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_208])])).
fof(f8917, plain, (~ (e22 = op2(e21, e24)) | (~ spl40_78 | ~ spl40_517 | spl40_533)), inference(forward_demodulation, [], [f8916, f662])).
fof(f8916, plain, (~ (op2(e21, e24) = h3(e12)) | (~ spl40_78 | ~ spl40_517 | spl40_533)), inference(forward_demodulation, [], [f8915, f1128])).
fof(f1128, plain, ((e12 = op1(e11, e14)) | ~ spl40_78), inference(avatar_component_clause, [], [f1126])).
fof(f8915, plain, (~ (op2(e21, e24) = h3(op1(e11, e14))) | (~ spl40_517 | spl40_533)), inference(forward_demodulation, [], [f3553, f3488])).
fof(f3553, plain, (~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | spl40_533), inference(avatar_component_clause, [], [f3551])).
fof(f3551, plain, (spl40_533 <=> (h3(op1(e11, e14)) = op2(e21, h3(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl40_533])])).
fof(f8877, plain, (~ spl40_12 | ~ spl40_142 | ~ spl40_517 | spl40_520), inference(avatar_contradiction_clause, [], [f8876])).
fof(f8876, plain, ($false | (~ spl40_12 | ~ spl40_142 | ~ spl40_517 | spl40_520)), inference(subsumption_resolution, [], [f8875, f1447])).
fof(f1447, plain, ((e21 = op2(e24, e22)) | ~ spl40_142), inference(avatar_component_clause, [], [f1445])).
fof(f1445, plain, (spl40_142 <=> (e21 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_142])])).
fof(f8875, plain, (~ (e21 = op2(e24, e22)) | (~ spl40_12 | ~ spl40_517 | spl40_520)), inference(forward_demodulation, [], [f8874, f2597])).
fof(f8874, plain, (~ (op2(e24, e22) = h3(e11)) | (~ spl40_12 | ~ spl40_517 | spl40_520)), inference(forward_demodulation, [], [f8873, f851])).
fof(f851, plain, ((e11 = op1(e14, e12)) | ~ spl40_12), inference(avatar_component_clause, [], [f849])).
fof(f849, plain, (spl40_12 <=> (e11 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_12])])).
fof(f8873, plain, (~ (op2(e24, e22) = h3(op1(e14, e12))) | (~ spl40_517 | spl40_520)), inference(forward_demodulation, [], [f3501, f3488])).
fof(f3501, plain, (~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | spl40_520), inference(avatar_component_clause, [], [f3499])).
fof(f3499, plain, (spl40_520 <=> (h3(op1(e14, e12)) = op2(h3(e14), e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_520])])).
fof(f8836, plain, (~ spl40_8 | ~ spl40_138 | ~ spl40_517 | spl40_519), inference(avatar_contradiction_clause, [], [f8835])).
fof(f8835, plain, ($false | (~ spl40_8 | ~ spl40_138 | ~ spl40_517 | spl40_519)), inference(subsumption_resolution, [], [f8834, f1430])).
fof(f1430, plain, ((e22 = op2(e24, e23)) | ~ spl40_138), inference(avatar_component_clause, [], [f1428])).
fof(f1428, plain, (spl40_138 <=> (e22 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_138])])).
fof(f8834, plain, (~ (e22 = op2(e24, e23)) | (~ spl40_8 | ~ spl40_517 | spl40_519)), inference(forward_demodulation, [], [f8833, f662])).
fof(f8833, plain, (~ (op2(e24, e23) = h3(e12)) | (~ spl40_8 | ~ spl40_517 | spl40_519)), inference(forward_demodulation, [], [f8832, f834])).
fof(f834, plain, ((e12 = op1(e14, e13)) | ~ spl40_8), inference(avatar_component_clause, [], [f832])).
fof(f832, plain, (spl40_8 <=> (e12 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_8])])).
fof(f8832, plain, (~ (op2(e24, e23) = h3(op1(e14, e13))) | (~ spl40_517 | spl40_519)), inference(forward_demodulation, [], [f3497, f3488])).
fof(f3497, plain, (~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | spl40_519), inference(avatar_component_clause, [], [f3495])).
fof(f3495, plain, (spl40_519 <=> (h3(op1(e14, e13)) = op2(h3(e14), e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_519])])).
fof(f8797, plain, (~ spl40_1 | spl40_518), inference(avatar_contradiction_clause, [], [f8796])).
fof(f8796, plain, ($false | (~ spl40_1 | spl40_518)), inference(subsumption_resolution, [], [f8795, f2599])).
fof(f8795, plain, (~ (e20 = h3(e10)) | (~ spl40_1 | spl40_518)), inference(forward_demodulation, [], [f3493, f805])).
fof(f3493, plain, (~ (e20 = h3(op1(e14, e14))) | spl40_518), inference(avatar_component_clause, [], [f3491])).
fof(f3491, plain, (spl40_518 <=> (e20 = h3(op1(e14, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl40_518])])).
fof(f8754, plain, (~ spl40_113 | ~ spl40_243 | spl40_540), inference(avatar_contradiction_clause, [], [f8753])).
fof(f8753, plain, ($false | (~ spl40_113 | ~ spl40_243 | spl40_540)), inference(subsumption_resolution, [], [f8752, f1871])).
fof(f1871, plain, ((e22 = op2(e20, e22)) | ~ spl40_243), inference(avatar_component_clause, [], [f1869])).
fof(f1869, plain, (spl40_243 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_243])])).
fof(f8752, plain, (~ (e22 = op2(e20, e22)) | (~ spl40_113 | spl40_540)), inference(forward_demodulation, [], [f8751, f662])).
fof(f8751, plain, (~ (op2(e20, e22) = h3(e12)) | (~ spl40_113 | spl40_540)), inference(forward_demodulation, [], [f3581, f1275])).
fof(f1275, plain, ((e12 = op1(e10, e12)) | ~ spl40_113), inference(avatar_component_clause, [], [f1273])).
fof(f1273, plain, (spl40_113 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_113])])).
fof(f3581, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | spl40_540), inference(avatar_component_clause, [], [f3579])).
fof(f3579, plain, (spl40_540 <=> (op2(e20, e22) = h3(op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl40_540])])).
fof(f8710, plain, (~ spl40_97 | ~ spl40_227 | spl40_537), inference(avatar_contradiction_clause, [], [f8709])).
fof(f8709, plain, ($false | (~ spl40_97 | ~ spl40_227 | spl40_537)), inference(subsumption_resolution, [], [f8708, f1804])).
fof(f1804, plain, ((e21 = op2(e21, e20)) | ~ spl40_227), inference(avatar_component_clause, [], [f1802])).
fof(f1802, plain, (spl40_227 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_227])])).
fof(f8708, plain, (~ (e21 = op2(e21, e20)) | (~ spl40_97 | spl40_537)), inference(forward_demodulation, [], [f8707, f2597])).
fof(f8707, plain, (~ (op2(e21, e20) = h3(e11)) | (~ spl40_97 | spl40_537)), inference(forward_demodulation, [], [f3569, f1208])).
fof(f1208, plain, ((e11 = op1(e11, e10)) | ~ spl40_97), inference(avatar_component_clause, [], [f1206])).
fof(f1206, plain, (spl40_97 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_97])])).
fof(f3569, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | spl40_537), inference(avatar_component_clause, [], [f3567])).
fof(f3567, plain, (spl40_537 <=> (op2(e21, e20) = h3(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_537])])).
fof(f8664, plain, (~ spl40_81 | ~ spl40_211 | spl40_534), inference(avatar_contradiction_clause, [], [f8663])).
fof(f8663, plain, ($false | (~ spl40_81 | ~ spl40_211 | spl40_534)), inference(subsumption_resolution, [], [f8662, f1737])).
fof(f1737, plain, ((e20 = op2(e21, e23)) | ~ spl40_211), inference(avatar_component_clause, [], [f1735])).
fof(f1735, plain, (spl40_211 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_211])])).
fof(f8662, plain, (~ (e20 = op2(e21, e23)) | (~ spl40_81 | spl40_534)), inference(forward_demodulation, [], [f8661, f2599])).
fof(f8661, plain, (~ (op2(e21, e23) = h3(e10)) | (~ spl40_81 | spl40_534)), inference(forward_demodulation, [], [f3557, f1141])).
fof(f1141, plain, ((e10 = op1(e11, e13)) | ~ spl40_81), inference(avatar_component_clause, [], [f1139])).
fof(f1139, plain, (spl40_81 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_81])])).
fof(f3557, plain, (~ (op2(e21, e23) = h3(op1(e11, e13))) | spl40_534), inference(avatar_component_clause, [], [f3555])).
fof(f3555, plain, (spl40_534 <=> (op2(e21, e23) = h3(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_534])])).
fof(f8645, plain, (~ spl40_43 | ~ spl40_173 | spl40_526), inference(avatar_contradiction_clause, [], [f8644])).
fof(f8644, plain, ($false | (~ spl40_43 | ~ spl40_173 | spl40_526)), inference(subsumption_resolution, [], [f8643, f1577])).
fof(f1577, plain, ((e22 = op2(e23, e21)) | ~ spl40_173), inference(avatar_component_clause, [], [f1575])).
fof(f1575, plain, (spl40_173 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_173])])).
fof(f8643, plain, (~ (e22 = op2(e23, e21)) | (~ spl40_43 | spl40_526)), inference(forward_demodulation, [], [f8642, f662])).
fof(f8642, plain, (~ (op2(e23, e21) = h3(e12)) | (~ spl40_43 | spl40_526)), inference(forward_demodulation, [], [f3525, f981])).
fof(f981, plain, ((e12 = op1(e13, e11)) | ~ spl40_43), inference(avatar_component_clause, [], [f979])).
fof(f979, plain, (spl40_43 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_43])])).
fof(f3525, plain, (~ (op2(e23, e21) = h3(op1(e13, e11))) | spl40_526), inference(avatar_component_clause, [], [f3523])).
fof(f3523, plain, (spl40_526 <=> (op2(e23, e21) = h3(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl40_526])])).
fof(f8579, plain, (~ spl40_117 | ~ spl40_247 | spl40_541), inference(avatar_contradiction_clause, [], [f8578])).
fof(f8578, plain, ($false | (~ spl40_117 | ~ spl40_247 | spl40_541)), inference(subsumption_resolution, [], [f8577, f1888])).
fof(f1888, plain, ((e21 = op2(e20, e21)) | ~ spl40_247), inference(avatar_component_clause, [], [f1886])).
fof(f1886, plain, (spl40_247 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_247])])).
fof(f8577, plain, (~ (e21 = op2(e20, e21)) | (~ spl40_117 | spl40_541)), inference(forward_demodulation, [], [f8576, f2597])).
fof(f8576, plain, (~ (op2(e20, e21) = h3(e11)) | (~ spl40_117 | spl40_541)), inference(forward_demodulation, [], [f3585, f1292])).
fof(f1292, plain, ((e11 = op1(e10, e11)) | ~ spl40_117), inference(avatar_component_clause, [], [f1290])).
fof(f1290, plain, (spl40_117 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_117])])).
fof(f3585, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | spl40_541), inference(avatar_component_clause, [], [f3583])).
fof(f3583, plain, (spl40_541 <=> (op2(e20, e21) = h3(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl40_541])])).
fof(f8556, plain, (~ spl40_137 | ~ spl40_142), inference(avatar_split_clause, [], [f8550, f1445, f1424])).
fof(f1424, plain, (spl40_137 <=> (e21 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_137])])).
fof(f8550, plain, (~ (e21 = op2(e24, e23)) | ~ spl40_142), inference(backward_demodulation, [], [f476, f1447])).
fof(f476, plain, ~ (op2(e24, e22) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (op2(e24, e23) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e24)) & ~ (op2(e24, e21) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e24)) & ~ (op2(e24, e20) = op2(e24, e23)) & ~ (op2(e24, e20) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e21)) & ~ (op2(e23, e23) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e24)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e24)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e23) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e24)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e24)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e23) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e24)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e24)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e23) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e24)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e24)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e23, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e24, e24)) & ~ (op2(e21, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e24, e24)) & ~ (op2(e20, e24) = op2(e23, e24)) & ~ (op2(e20, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e21, e24)) & ~ (op2(e23, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e24, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e24, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e23, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e24, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e24, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e23, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e24, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e24, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e23, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e24, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e24, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax8)).
fof(f8525, plain, (~ spl40_186 | ~ spl40_196), inference(avatar_split_clause, [], [f8518, f1672, f1630])).
fof(f1630, plain, (spl40_186 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_186])])).
fof(f8518, plain, (~ (e20 = op2(e22, e23)) | ~ spl40_196), inference(backward_demodulation, [], [f454, f1674])).
fof(f454, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f8515, plain, (~ spl40_198 | ~ spl40_203), inference(avatar_split_clause, [], [f8509, f1701, f1680])).
fof(f1680, plain, (spl40_198 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_198])])).
fof(f8509, plain, (~ (e22 = op2(e22, e21)) | ~ spl40_203), inference(backward_demodulation, [], [f449, f1703])).
fof(f449, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f8477, plain, (~ spl40_202 | ~ spl40_227), inference(avatar_split_clause, [], [f8467, f1802, f1697])).
fof(f1697, plain, (spl40_202 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_202])])).
fof(f8467, plain, (~ (e21 = op2(e22, e20)) | ~ spl40_227), inference(backward_demodulation, [], [f383, f1804])).
fof(f383, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f8])).
fof(f8447, plain, (~ spl40_218 | ~ spl40_243), inference(avatar_split_clause, [], [f8439, f1869, f1764])).
fof(f1764, plain, (spl40_218 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_218])])).
fof(f8439, plain, (~ (e22 = op2(e21, e22)) | ~ spl40_243), inference(backward_demodulation, [], [f399, f1871])).
fof(f399, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f8])).
fof(f8422, plain, (spl40_155 | ~ spl40_256), inference(avatar_split_clause, [], [f8408, f1924, f1499])).
fof(f1924, plain, (spl40_256 <=> (e20 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl40_256])])).
fof(f8408, plain, ((e24 = op2(e24, e20)) | ~ spl40_256), inference(backward_demodulation, [], [f227, f1926])).
fof(f1926, plain, ((e20 = unit2) | ~ spl40_256), inference(avatar_component_clause, [], [f1924])).
fof(f227, plain, (e24 = op2(e24, unit2)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (((e24 = unit2) | (e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)) & (e24 = op2(e24, unit2)) & (e24 = op2(unit2, e24)) & (e23 = op2(e23, unit2)) & (e23 = op2(unit2, e23)) & (e22 = op2(e22, unit2)) & (e22 = op2(unit2, e22)) & (e21 = op2(e21, unit2)) & (e21 = op2(unit2, e21)) & (e20 = op2(e20, unit2)) & (e20 = op2(unit2, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax5)).
fof(f8421, plain, (spl40_235 | ~ spl40_256), inference(avatar_split_clause, [], [f8407, f1924, f1835])).
fof(f8407, plain, ((e24 = op2(e20, e24)) | ~ spl40_256), inference(backward_demodulation, [], [f226, f1926])).
fof(f226, plain, (e24 = op2(unit2, e24)), inference(cnf_transformation, [], [f5])).
fof(f8420, plain, (spl40_179 | ~ spl40_256), inference(avatar_split_clause, [], [f8406, f1924, f1600])).
fof(f8406, plain, ((e23 = op2(e23, e20)) | ~ spl40_256), inference(backward_demodulation, [], [f225, f1926])).
fof(f225, plain, (e23 = op2(e23, unit2)), inference(cnf_transformation, [], [f5])).
fof(f8419, plain, (spl40_239 | ~ spl40_256), inference(avatar_split_clause, [], [f8405, f1924, f1852])).
fof(f8405, plain, ((e23 = op2(e20, e23)) | ~ spl40_256), inference(backward_demodulation, [], [f224, f1926])).
fof(f224, plain, (e23 = op2(unit2, e23)), inference(cnf_transformation, [], [f5])).
fof(f8418, plain, (spl40_203 | ~ spl40_256), inference(avatar_split_clause, [], [f8404, f1924, f1701])).
fof(f8404, plain, ((e22 = op2(e22, e20)) | ~ spl40_256), inference(backward_demodulation, [], [f223, f1926])).
fof(f223, plain, (e22 = op2(e22, unit2)), inference(cnf_transformation, [], [f5])).
fof(f8417, plain, (spl40_243 | ~ spl40_256), inference(avatar_split_clause, [], [f8403, f1924, f1869])).
fof(f8403, plain, ((e22 = op2(e20, e22)) | ~ spl40_256), inference(backward_demodulation, [], [f222, f1926])).
fof(f222, plain, (e22 = op2(unit2, e22)), inference(cnf_transformation, [], [f5])).
fof(f8416, plain, (spl40_227 | ~ spl40_256), inference(avatar_split_clause, [], [f8402, f1924, f1802])).
fof(f8402, plain, ((e21 = op2(e21, e20)) | ~ spl40_256), inference(backward_demodulation, [], [f221, f1926])).
fof(f221, plain, (e21 = op2(e21, unit2)), inference(cnf_transformation, [], [f5])).
fof(f8415, plain, (spl40_247 | ~ spl40_256), inference(avatar_split_clause, [], [f8401, f1924, f1886])).
fof(f8401, plain, ((e21 = op2(e20, e21)) | ~ spl40_256), inference(backward_demodulation, [], [f220, f1926])).
fof(f220, plain, (e21 = op2(unit2, e21)), inference(cnf_transformation, [], [f5])).
fof(f8414, plain, (spl40_251 | ~ spl40_256), inference(avatar_split_clause, [], [f8400, f1924, f1903])).
fof(f1903, plain, (spl40_251 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_251])])).
fof(f8400, plain, ((e20 = op2(e20, e20)) | ~ spl40_256), inference(backward_demodulation, [], [f219, f1926])).
fof(f219, plain, (e20 = op2(e20, unit2)), inference(cnf_transformation, [], [f5])).
fof(f8356, plain, (~ spl40_180 | ~ spl40_387 | ~ spl40_485), inference(avatar_split_clause, [], [f8346, f3215, f2704, f1604])).
fof(f1604, plain, (spl40_180 <=> (e24 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_180])])).
fof(f2704, plain, (spl40_387 <=> (e20 = h5(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_387])])).
fof(f3215, plain, (spl40_485 <=> (e24 = h5(e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_485])])).
fof(f8346, plain, (~ (e24 = op2(e23, e20)) | (~ spl40_387 | ~ spl40_485)), inference(backward_demodulation, [], [f5171, f3216])).
fof(f3216, plain, ((e24 = h5(e13)) | ~ spl40_485), inference(avatar_component_clause, [], [f3215])).
fof(f5171, plain, (~ (op2(e23, e20) = h5(e13)) | ~ spl40_387), inference(backward_demodulation, [], [f388, f5163])).
fof(f5163, plain, ((op2(e24, e20) = h5(e13)) | ~ spl40_387), inference(backward_demodulation, [], [f2620, f2705])).
fof(f2705, plain, ((e20 = h5(e14)) | ~ spl40_387), inference(avatar_component_clause, [], [f2704])).
fof(f2620, plain, (h5(e13) = op2(e24, h5(e14))), inference(forward_demodulation, [], [f675, f676])).
fof(f676, plain, (op2(e24, e24) = h5(e14)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((op2(e24, e24) = h5(e14)) & (op2(e24, op2(e24, e24)) = h5(e13)) & (h5(e11) = op2(op2(e24, op2(e24, e24)), op2(e24, e24))) & (h5(e10) = op2(op2(e24, e24), op2(e24, e24))) & (e24 = h5(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax20)).
fof(f675, plain, (op2(e24, op2(e24, e24)) = h5(e13)), inference(cnf_transformation, [], [f20])).
fof(f388, plain, ~ (op2(e23, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f8294, plain, (~ spl40_187 | ~ spl40_57 | spl40_529), inference(avatar_split_clause, [], [f8293, f3535, f1038, f1634])).
fof(f1634, plain, (spl40_187 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_187])])).
fof(f3535, plain, (spl40_529 <=> (op2(e22, e23) = h3(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_529])])).
fof(f8293, plain, (~ (e21 = op2(e22, e23)) | (~ spl40_57 | spl40_529)), inference(forward_demodulation, [], [f8292, f2597])).
fof(f8292, plain, (~ (op2(e22, e23) = h3(e11)) | (~ spl40_57 | spl40_529)), inference(forward_demodulation, [], [f3537, f1040])).
fof(f3537, plain, (~ (op2(e22, e23) = h3(op1(e12, e13))) | spl40_529), inference(avatar_component_clause, [], [f3535])).
fof(f8271, plain, (~ spl40_180 | ~ spl40_233 | ~ spl40_366), inference(avatar_contradiction_clause, [], [f8270])).
fof(f8270, plain, ($false | (~ spl40_180 | ~ spl40_233 | ~ spl40_366)), inference(subsumption_resolution, [], [f8269, f496])).
fof(f496, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (~ (e23 = e24) & ~ (e22 = e24) & ~ (e22 = e23) & ~ (e21 = e24) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e24) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax10)).
fof(f8269, plain, ((e22 = e23) | (~ spl40_180 | ~ spl40_233 | ~ spl40_366)), inference(forward_demodulation, [], [f8267, f1829])).
fof(f1829, plain, ((e22 = op2(e20, e24)) | ~ spl40_233), inference(avatar_component_clause, [], [f1827])).
fof(f1827, plain, (spl40_233 <=> (e22 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_233])])).
fof(f8267, plain, ((e23 = op2(e20, e24)) | (~ spl40_180 | ~ spl40_366)), inference(backward_demodulation, [], [f2529, f1606])).
fof(f1606, plain, ((e24 = op2(e23, e20)) | ~ spl40_180), inference(avatar_component_clause, [], [f1604])).
fof(f2529, plain, ((e23 = op2(e20, op2(e23, e20))) | ~ spl40_366), inference(avatar_component_clause, [], [f2527])).
fof(f2527, plain, (spl40_366 <=> (e23 = op2(e20, op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl40_366])])).
fof(f8245, plain, (spl40_245 | ~ spl40_367 | ~ spl40_378 | ~ spl40_387), inference(avatar_contradiction_clause, [], [f8244])).
fof(f8244, plain, ($false | (spl40_245 | ~ spl40_367 | ~ spl40_378 | ~ spl40_387)), inference(subsumption_resolution, [], [f8236, f1878])).
fof(f1878, plain, (~ (e24 = op2(e20, e22)) | spl40_245), inference(avatar_component_clause, [], [f1877])).
fof(f1877, plain, (spl40_245 <=> (e24 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_245])])).
fof(f8236, plain, ((e24 = op2(e20, e22)) | (~ spl40_367 | ~ spl40_378 | ~ spl40_387)), inference(backward_demodulation, [], [f8148, f2660])).
fof(f2660, plain, ((e22 = h5(e13)) | ~ spl40_378), inference(avatar_component_clause, [], [f2659])).
fof(f2659, plain, (spl40_378 <=> (e22 = h5(e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_378])])).
fof(f8148, plain, ((e24 = op2(e20, h5(e13))) | (~ spl40_367 | ~ spl40_387)), inference(backward_demodulation, [], [f2533, f5163])).
fof(f2533, plain, ((e24 = op2(e20, op2(e24, e20))) | ~ spl40_367), inference(avatar_component_clause, [], [f2531])).
fof(f2531, plain, (spl40_367 <=> (e24 = op2(e20, op2(e24, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl40_367])])).
fof(f8243, plain, (~ spl40_138 | ~ spl40_378 | ~ spl40_387), inference(avatar_split_clause, [], [f8235, f2704, f2659, f1428])).
fof(f8235, plain, (~ (e22 = op2(e24, e23)) | (~ spl40_378 | ~ spl40_387)), inference(backward_demodulation, [], [f5173, f2660])).
fof(f5173, plain, (~ (op2(e24, e23) = h5(e13)) | ~ spl40_387), inference(backward_demodulation, [], [f471, f5163])).
fof(f471, plain, ~ (op2(e24, e20) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f8229, plain, (spl40_191 | ~ spl40_397 | ~ spl40_410), inference(avatar_contradiction_clause, [], [f8228])).
fof(f8228, plain, ($false | (spl40_191 | ~ spl40_397 | ~ spl40_410)), inference(subsumption_resolution, [], [f8219, f1652])).
fof(f1652, plain, (~ (e20 = op2(e22, e22)) | spl40_191), inference(avatar_component_clause, [], [f1651])).
fof(f1651, plain, (spl40_191 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_191])])).
fof(f8219, plain, ((e20 = op2(e22, e22)) | (~ spl40_397 | ~ spl40_410)), inference(backward_demodulation, [], [f8153, f2756])).
fof(f2756, plain, ((e22 = h4(e14)) | ~ spl40_397), inference(avatar_component_clause, [], [f2755])).
fof(f2755, plain, (spl40_397 <=> (e22 = h4(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_397])])).
fof(f8153, plain, ((e20 = op2(h4(e14), h4(e14))) | ~ spl40_410), inference(forward_demodulation, [], [f2611, f2822])).
fof(f2822, plain, ((e20 = h4(e10)) | ~ spl40_410), inference(avatar_component_clause, [], [f2821])).
fof(f2821, plain, (spl40_410 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_410])])).
fof(f2611, plain, (h4(e10) = op2(h4(e14), h4(e14))), inference(forward_demodulation, [], [f668, f671])).
fof(f671, plain, (op2(e23, e23) = h4(e14)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((op2(e23, e23) = h4(e14)) & (op2(e23, op2(e23, e23)) = h4(e13)) & (h4(e11) = op2(op2(e23, op2(e23, e23)), op2(e23, e23))) & (h4(e10) = op2(op2(e23, e23), op2(e23, e23))) & (e23 = h4(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax19)).
fof(f668, plain, (h4(e10) = op2(op2(e23, e23), op2(e23, e23))), inference(cnf_transformation, [], [f19])).
fof(f8221, plain, (~ spl40_138 | ~ spl40_397), inference(avatar_split_clause, [], [f8213, f2755, f1428])).
fof(f8213, plain, (~ (e22 = op2(e24, e23)) | ~ spl40_397), inference(backward_demodulation, [], [f2603, f2756])).
fof(f2603, plain, ~ (op2(e24, e23) = h4(e14)), inference(backward_demodulation, [], [f418, f671])).
fof(f418, plain, ~ (op2(e23, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f8135, plain, (~ spl40_143 | ~ spl40_218), inference(avatar_split_clause, [], [f8134, f1764, f1449])).
fof(f1449, plain, (spl40_143 <=> (e22 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_143])])).
fof(f8134, plain, (~ (e22 = op2(e24, e22)) | ~ spl40_218), inference(forward_demodulation, [], [f405, f1766])).
fof(f1766, plain, ((e22 = op2(e21, e22)) | ~ spl40_218), inference(avatar_component_clause, [], [f1764])).
fof(f405, plain, ~ (op2(e21, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f8098, plain, (~ spl40_154 | ~ spl40_237 | ~ spl40_367), inference(avatar_contradiction_clause, [], [f8097])).
fof(f8097, plain, ($false | (~ spl40_154 | ~ spl40_237 | ~ spl40_367)), inference(subsumption_resolution, [], [f8096, f495])).
fof(f495, plain, ~ (e21 = e24), inference(cnf_transformation, [], [f10])).
fof(f8096, plain, ((e21 = e24) | (~ spl40_154 | ~ spl40_237 | ~ spl40_367)), inference(forward_demodulation, [], [f8095, f1846])).
fof(f1846, plain, ((e21 = op2(e20, e23)) | ~ spl40_237), inference(avatar_component_clause, [], [f1844])).
fof(f1844, plain, (spl40_237 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_237])])).
fof(f8095, plain, ((e24 = op2(e20, e23)) | (~ spl40_154 | ~ spl40_367)), inference(forward_demodulation, [], [f2533, f1497])).
fof(f1497, plain, ((e23 = op2(e24, e20)) | ~ spl40_154), inference(avatar_component_clause, [], [f1495])).
fof(f1495, plain, (spl40_154 <=> (e23 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_154])])).
fof(f8017, plain, (~ spl40_166 | ~ spl40_178 | spl40_369), inference(avatar_contradiction_clause, [], [f8016])).
fof(f8016, plain, ($false | (~ spl40_166 | ~ spl40_178 | spl40_369)), inference(subsumption_resolution, [], [f8014, f1548])).
fof(f8014, plain, (~ (e20 = op2(e23, e22)) | (~ spl40_178 | spl40_369)), inference(backward_demodulation, [], [f2543, f1598])).
fof(f1598, plain, ((e22 = op2(e23, e20)) | ~ spl40_178), inference(avatar_component_clause, [], [f1596])).
fof(f1596, plain, (spl40_178 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_178])])).
fof(f2543, plain, (~ (e20 = op2(e23, op2(e23, e20))) | spl40_369), inference(avatar_component_clause, [], [f2541])).
fof(f2541, plain, (spl40_369 <=> (e20 = op2(e23, op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl40_369])])).
fof(f7931, plain, (~ spl40_196 | ~ spl40_246), inference(avatar_split_clause, [], [f7920, f1882, f1672])).
fof(f1882, plain, (spl40_246 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_246])])).
fof(f7920, plain, (~ (e20 = op2(e22, e21)) | ~ spl40_246), inference(backward_demodulation, [], [f390, f1884])).
fof(f1884, plain, ((e20 = op2(e20, e21)) | ~ spl40_246), inference(avatar_component_clause, [], [f1882])).
fof(f390, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f7919, plain, (spl40_150 | ~ spl40_257), inference(avatar_split_clause, [], [f7906, f1928, f1478])).
fof(f1478, plain, (spl40_150 <=> (e24 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_150])])).
fof(f1928, plain, (spl40_257 <=> (e21 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl40_257])])).
fof(f7906, plain, ((e24 = op2(e24, e21)) | ~ spl40_257), inference(backward_demodulation, [], [f227, f1930])).
fof(f1930, plain, ((e21 = unit2) | ~ spl40_257), inference(avatar_component_clause, [], [f1928])).
fof(f7918, plain, (spl40_210 | ~ spl40_257), inference(avatar_split_clause, [], [f7905, f1928, f1730])).
fof(f1730, plain, (spl40_210 <=> (e24 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_210])])).
fof(f7905, plain, ((e24 = op2(e21, e24)) | ~ spl40_257), inference(backward_demodulation, [], [f226, f1930])).
fof(f7917, plain, (spl40_174 | ~ spl40_257), inference(avatar_split_clause, [], [f7904, f1928, f1579])).
fof(f1579, plain, (spl40_174 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_174])])).
fof(f7904, plain, ((e23 = op2(e23, e21)) | ~ spl40_257), inference(backward_demodulation, [], [f225, f1930])).
fof(f7915, plain, (spl40_198 | ~ spl40_257), inference(avatar_split_clause, [], [f7902, f1928, f1680])).
fof(f7902, plain, ((e22 = op2(e22, e21)) | ~ spl40_257), inference(backward_demodulation, [], [f223, f1930])).
fof(f7914, plain, (spl40_218 | ~ spl40_257), inference(avatar_split_clause, [], [f7901, f1928, f1764])).
fof(f7901, plain, ((e22 = op2(e21, e22)) | ~ spl40_257), inference(backward_demodulation, [], [f222, f1930])).
fof(f7911, plain, (spl40_246 | ~ spl40_257), inference(avatar_split_clause, [], [f7898, f1928, f1882])).
fof(f7898, plain, ((e20 = op2(e20, e21)) | ~ spl40_257), inference(backward_demodulation, [], [f219, f1930])).
fof(f7910, plain, (spl40_226 | ~ spl40_257), inference(avatar_split_clause, [], [f7897, f1928, f1798])).
fof(f1798, plain, (spl40_226 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_226])])).
fof(f7897, plain, ((e20 = op2(e21, e20)) | ~ spl40_257), inference(backward_demodulation, [], [f218, f1930])).
fof(f218, plain, (e20 = op2(unit2, e20)), inference(cnf_transformation, [], [f5])).
fof(f7732, plain, (~ spl40_9 | ~ spl40_109), inference(avatar_split_clause, [], [f7731, f1256, f836])).
fof(f836, plain, (spl40_9 <=> (e13 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_9])])).
fof(f7731, plain, (~ (e13 = op1(e14, e13)) | ~ spl40_109), inference(forward_demodulation, [], [f312, f1258])).
fof(f312, plain, ~ (op1(e10, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f7711, plain, (~ spl40_20 | ~ spl40_45), inference(avatar_split_clause, [], [f7708, f987, f882])).
fof(f987, plain, (spl40_45 <=> (e14 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_45])])).
fof(f7708, plain, (~ (e14 = op1(e14, e11)) | ~ spl40_45), inference(backward_demodulation, [], [f298, f989])).
fof(f989, plain, ((e14 = op1(e13, e11)) | ~ spl40_45), inference(avatar_component_clause, [], [f987])).
fof(f7707, plain, (~ spl40_44 | ~ spl40_49), inference(avatar_split_clause, [], [f7703, f1004, f983])).
fof(f7703, plain, (~ (e13 = op1(e13, e11)) | ~ spl40_49), inference(backward_demodulation, [], [f359, f1006])).
fof(f359, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f7695, plain, (~ spl40_68 | ~ spl40_73), inference(avatar_split_clause, [], [f7690, f1105, f1084])).
fof(f7690, plain, (~ (e12 = op1(e12, e11)) | ~ spl40_73), inference(backward_demodulation, [], [f349, f1107])).
fof(f349, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f7676, plain, (~ spl40_80 | ~ spl40_85), inference(avatar_split_clause, [], [f7673, f1155, f1134])).
fof(f1155, plain, (spl40_85 <=> (e14 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_85])])).
fof(f7673, plain, (~ (e14 = op1(e11, e14)) | ~ spl40_85), inference(backward_demodulation, [], [f348, f1157])).
fof(f1157, plain, ((e14 = op1(e11, e13)) | ~ spl40_85), inference(avatar_component_clause, [], [f1155])).
fof(f348, plain, ~ (op1(e11, e13) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f7648, plain, (~ spl40_92 | ~ spl40_97), inference(avatar_split_clause, [], [f7638, f1206, f1185])).
fof(f7638, plain, (~ (e11 = op1(e11, e11)) | ~ spl40_97), inference(backward_demodulation, [], [f339, f1208])).
fof(f339, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f7])).
fof(f7635, plain, (~ spl40_80 | ~ spl40_105), inference(avatar_split_clause, [], [f7634, f1239, f1134])).
fof(f7634, plain, (~ (e14 = op1(e11, e14)) | ~ spl40_105), inference(backward_demodulation, [], [f319, f1241])).
fof(f319, plain, ~ (op1(e10, e14) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f7630, plain, (~ spl40_84 | ~ spl40_109), inference(avatar_split_clause, [], [f7624, f1256, f1151])).
fof(f7624, plain, (~ (e13 = op1(e11, e13)) | ~ spl40_109), inference(backward_demodulation, [], [f309, f1258])).
fof(f309, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f7])).
fof(f7618, plain, (~ spl40_88 | ~ spl40_113), inference(avatar_split_clause, [], [f7609, f1273, f1168])).
fof(f7609, plain, (~ (e12 = op1(e11, e12)) | ~ spl40_113), inference(backward_demodulation, [], [f299, f1275])).
fof(f299, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f7607, plain, (~ spl40_112 | ~ spl40_117), inference(avatar_split_clause, [], [f7597, f1290, f1269])).
fof(f1269, plain, (spl40_112 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_112])])).
fof(f7597, plain, (~ (e11 = op1(e10, e12)) | ~ spl40_117), inference(backward_demodulation, [], [f333, f1292])).
fof(f333, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f7])).
fof(f7606, plain, (~ spl40_67 | ~ spl40_117), inference(avatar_split_clause, [], [f7594, f1290, f1080])).
fof(f1080, plain, (spl40_67 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_67])])).
fof(f7594, plain, (~ (e11 = op1(e12, e11)) | ~ spl40_117), inference(backward_demodulation, [], [f290, f1292])).
fof(f290, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f7605, plain, (~ spl40_92 | ~ spl40_117), inference(avatar_split_clause, [], [f7593, f1290, f1185])).
fof(f7593, plain, (~ (e11 = op1(e11, e11)) | ~ spl40_117), inference(backward_demodulation, [], [f289, f1292])).
fof(f289, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f7])).
fof(f7574, plain, (~ spl40_23 | ~ spl40_126), inference(avatar_contradiction_clause, [], [f7573])).
fof(f7573, plain, ($false | (~ spl40_23 | ~ spl40_126)), inference(subsumption_resolution, [], [f7572, f487])).
fof(f487, plain, ~ (e12 = e14), inference(cnf_transformation, [], [f9])).
fof(f7572, plain, ((e12 = e14) | (~ spl40_23 | ~ spl40_126)), inference(forward_demodulation, [], [f7558, f897])).
fof(f897, plain, ((e12 = op1(e14, e10)) | ~ spl40_23), inference(avatar_component_clause, [], [f895])).
fof(f895, plain, (spl40_23 <=> (e12 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_23])])).
fof(f7558, plain, ((e14 = op1(e14, e10)) | ~ spl40_126), inference(backward_demodulation, [], [f141, f1330])).
fof(f1330, plain, ((e10 = unit1) | ~ spl40_126), inference(avatar_component_clause, [], [f1328])).
fof(f1328, plain, (spl40_126 <=> (e10 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl40_126])])).
fof(f7571, plain, (spl40_105 | ~ spl40_126), inference(avatar_split_clause, [], [f7557, f1328, f1239])).
fof(f7557, plain, ((e14 = op1(e10, e14)) | ~ spl40_126), inference(backward_demodulation, [], [f140, f1330])).
fof(f7570, plain, (spl40_49 | ~ spl40_126), inference(avatar_split_clause, [], [f7556, f1328, f1004])).
fof(f7556, plain, ((e13 = op1(e13, e10)) | ~ spl40_126), inference(backward_demodulation, [], [f139, f1330])).
fof(f7569, plain, (spl40_109 | ~ spl40_126), inference(avatar_split_clause, [], [f7555, f1328, f1256])).
fof(f7555, plain, ((e13 = op1(e10, e13)) | ~ spl40_126), inference(backward_demodulation, [], [f138, f1330])).
fof(f7568, plain, (spl40_73 | ~ spl40_126), inference(avatar_split_clause, [], [f7554, f1328, f1105])).
fof(f7554, plain, ((e12 = op1(e12, e10)) | ~ spl40_126), inference(backward_demodulation, [], [f137, f1330])).
fof(f7567, plain, (spl40_113 | ~ spl40_126), inference(avatar_split_clause, [], [f7553, f1328, f1273])).
fof(f7553, plain, ((e12 = op1(e10, e12)) | ~ spl40_126), inference(backward_demodulation, [], [f136, f1330])).
fof(f7566, plain, (spl40_97 | ~ spl40_126), inference(avatar_split_clause, [], [f7552, f1328, f1206])).
fof(f7552, plain, ((e11 = op1(e11, e10)) | ~ spl40_126), inference(backward_demodulation, [], [f135, f1330])).
fof(f7565, plain, (spl40_117 | ~ spl40_126), inference(avatar_split_clause, [], [f7551, f1328, f1290])).
fof(f7551, plain, ((e11 = op1(e10, e11)) | ~ spl40_126), inference(backward_demodulation, [], [f134, f1330])).
fof(f134, plain, (e11 = op1(unit1, e11)), inference(cnf_transformation, [], [f2])).
fof(f7564, plain, (spl40_121 | ~ spl40_126), inference(avatar_split_clause, [], [f7550, f1328, f1307])).
fof(f7550, plain, ((e10 = op1(e10, e10)) | ~ spl40_126), inference(backward_demodulation, [], [f133, f1330])).
fof(f7501, plain, (spl40_45 | ~ spl40_7 | ~ spl40_296), inference(avatar_split_clause, [], [f7500, f2163, f828, f987])).
fof(f828, plain, (spl40_7 <=> (e11 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_7])])).
fof(f2163, plain, (spl40_296 <=> (e14 = op1(e13, op1(e14, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_296])])).
fof(f7500, plain, ((e14 = op1(e13, e11)) | (~ spl40_7 | ~ spl40_296)), inference(forward_demodulation, [], [f2165, f830])).
fof(f830, plain, ((e11 = op1(e14, e13)) | ~ spl40_7), inference(avatar_component_clause, [], [f828])).
fof(f2165, plain, ((e14 = op1(e13, op1(e14, e13))) | ~ spl40_296), inference(avatar_component_clause, [], [f2163])).
fof(f7468, plain, (spl40_410 | ~ spl40_131 | ~ spl40_502), inference(avatar_split_clause, [], [f7467, f3340, f1399, f2821])).
fof(f1399, plain, (spl40_131 <=> (e20 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_131])])).
fof(f7467, plain, ((e20 = h4(e10)) | (~ spl40_131 | ~ spl40_502)), inference(forward_demodulation, [], [f7461, f1401])).
fof(f1401, plain, ((e20 = op2(e24, e24)) | ~ spl40_131), inference(avatar_component_clause, [], [f1399])).
fof(f7461, plain, ((op2(e24, e24) = h4(e10)) | ~ spl40_502), inference(backward_demodulation, [], [f2611, f3341])).
fof(f7451, plain, (~ spl40_215 | ~ spl40_557), inference(avatar_split_clause, [], [f7445, f3665, f1751])).
fof(f1751, plain, (spl40_215 <=> (e24 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_215])])).
fof(f7445, plain, (~ (e24 = op2(e21, e23)) | ~ spl40_557), inference(backward_demodulation, [], [f2576, f3666])).
fof(f2576, plain, ~ (op2(e21, e23) = h2(e14)), inference(backward_demodulation, [], [f444, f661])).
fof(f661, plain, (op2(e21, e21) = h2(e14)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e21, e21) = h2(e14)) & (op2(e21, op2(e21, e21)) = h2(e13)) & (h2(e11) = op2(op2(e21, op2(e21, e21)), op2(e21, e21))) & (h2(e10) = op2(op2(e21, e21), op2(e21, e21))) & (e21 = h2(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax17)).
fof(f444, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f7407, plain, (~ spl40_168 | ~ spl40_243), inference(avatar_split_clause, [], [f7406, f1869, f1554])).
fof(f1554, plain, (spl40_168 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_168])])).
fof(f7406, plain, (~ (e22 = op2(e23, e22)) | ~ spl40_243), inference(forward_demodulation, [], [f401, f1871])).
fof(f401, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f7361, plain, (~ spl40_34 | ~ spl40_44), inference(avatar_split_clause, [], [f7359, f983, f941])).
fof(f941, plain, (spl40_34 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_34])])).
fof(f7359, plain, (~ (e13 = op1(e13, e13)) | ~ spl40_44), inference(backward_demodulation, [], [f364, f985])).
fof(f364, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f7259, plain, (~ spl40_210 | ~ spl40_235), inference(avatar_split_clause, [], [f7256, f1835, f1730])).
fof(f7256, plain, (~ (e24 = op2(e21, e24)) | ~ spl40_235), inference(backward_demodulation, [], [f419, f1837])).
fof(f419, plain, ~ (op2(e20, e24) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f7245, plain, (~ spl40_147 | ~ spl40_247), inference(avatar_split_clause, [], [f7237, f1886, f1466])).
fof(f1466, plain, (spl40_147 <=> (e21 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_147])])).
fof(f7237, plain, (~ (e21 = op2(e24, e21)) | ~ spl40_247), inference(backward_demodulation, [], [f392, f1888])).
fof(f392, plain, ~ (op2(e20, e21) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f7227, plain, (~ spl40_178 | ~ spl40_256), inference(avatar_contradiction_clause, [], [f7226])).
fof(f7226, plain, ($false | (~ spl40_178 | ~ spl40_256)), inference(subsumption_resolution, [], [f7225, f496])).
fof(f7225, plain, ((e22 = e23) | (~ spl40_178 | ~ spl40_256)), inference(forward_demodulation, [], [f7209, f1598])).
fof(f7209, plain, ((e23 = op2(e23, e20)) | ~ spl40_256), inference(backward_demodulation, [], [f225, f1926])).
fof(f7222, plain, (~ spl40_242 | ~ spl40_256), inference(avatar_contradiction_clause, [], [f7221])).
fof(f7221, plain, ($false | (~ spl40_242 | ~ spl40_256)), inference(subsumption_resolution, [], [f7220, f493])).
fof(f493, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f10])).
fof(f7220, plain, ((e21 = e22) | (~ spl40_242 | ~ spl40_256)), inference(forward_demodulation, [], [f7206, f1867])).
fof(f1867, plain, ((e21 = op2(e20, e22)) | ~ spl40_242), inference(avatar_component_clause, [], [f1865])).
fof(f1865, plain, (spl40_242 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_242])])).
fof(f7206, plain, ((e22 = op2(e20, e22)) | ~ spl40_256), inference(backward_demodulation, [], [f222, f1926])).
fof(f7191, plain, (spl40_48 | ~ spl40_56 | ~ spl40_294), inference(avatar_contradiction_clause, [], [f7190])).
fof(f7190, plain, ($false | (spl40_48 | ~ spl40_56 | ~ spl40_294)), inference(subsumption_resolution, [], [f7189, f1001])).
fof(f1001, plain, (~ (e12 = op1(e13, e10)) | spl40_48), inference(avatar_component_clause, [], [f1000])).
fof(f7189, plain, ((e12 = op1(e13, e10)) | (~ spl40_56 | ~ spl40_294)), inference(forward_demodulation, [], [f2157, f1036])).
fof(f2157, plain, ((e12 = op1(e13, op1(e12, e13))) | ~ spl40_294), inference(avatar_component_clause, [], [f2155])).
fof(f2155, plain, (spl40_294 <=> (e12 = op1(e13, op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_294])])).
fof(f7077, plain, (~ spl40_150 | ~ spl40_175), inference(avatar_split_clause, [], [f7076, f1583, f1478])).
fof(f1583, plain, (spl40_175 <=> (e24 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_175])])).
fof(f7076, plain, (~ (e24 = op2(e24, e21)) | ~ spl40_175), inference(forward_demodulation, [], [f398, f1585])).
fof(f1585, plain, ((e24 = op2(e23, e21)) | ~ spl40_175), inference(avatar_component_clause, [], [f1583])).
fof(f398, plain, ~ (op2(e23, e21) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f7048, plain, (~ spl40_50 | ~ spl40_103 | ~ spl40_311), inference(avatar_contradiction_clause, [], [f7047])).
fof(f7047, plain, ($false | (~ spl40_50 | ~ spl40_103 | ~ spl40_311)), inference(subsumption_resolution, [], [f7046, f486])).
fof(f486, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f9])).
fof(f7046, plain, ((e12 = e13) | (~ spl40_50 | ~ spl40_103 | ~ spl40_311)), inference(forward_demodulation, [], [f7045, f1233])).
fof(f1233, plain, ((e12 = op1(e10, e14)) | ~ spl40_103), inference(avatar_component_clause, [], [f1231])).
fof(f1231, plain, (spl40_103 <=> (e12 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_103])])).
fof(f7045, plain, ((e13 = op1(e10, e14)) | (~ spl40_50 | ~ spl40_311)), inference(forward_demodulation, [], [f2249, f1010])).
fof(f1010, plain, ((e14 = op1(e13, e10)) | ~ spl40_50), inference(avatar_component_clause, [], [f1008])).
fof(f2249, plain, ((e13 = op1(e10, op1(e13, e10))) | ~ spl40_311), inference(avatar_component_clause, [], [f2247])).
fof(f2247, plain, (spl40_311 <=> (e13 = op1(e10, op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_311])])).
fof(f7015, plain, (~ spl40_62 | ~ spl40_65), inference(avatar_contradiction_clause, [], [f7014])).
fof(f7014, plain, ($false | (~ spl40_62 | ~ spl40_65)), inference(subsumption_resolution, [], [f7013, f485])).
fof(f7013, plain, ((e11 = e14) | (~ spl40_62 | ~ spl40_65)), inference(backward_demodulation, [], [f1073, f1061])).
fof(f1061, plain, ((e11 = op1(e12, e12)) | ~ spl40_62), inference(avatar_component_clause, [], [f1059])).
fof(f1059, plain, (spl40_62 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_62])])).
fof(f7004, plain, (spl40_26 | ~ spl40_110 | ~ spl40_271), inference(avatar_contradiction_clause, [], [f7003])).
fof(f7003, plain, ($false | (spl40_26 | ~ spl40_110 | ~ spl40_271)), inference(subsumption_resolution, [], [f6999, f909])).
fof(f909, plain, (~ (e10 = op1(e13, e14)) | spl40_26), inference(avatar_component_clause, [], [f908])).
fof(f908, plain, (spl40_26 <=> (e10 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_26])])).
fof(f6999, plain, ((e10 = op1(e13, e14)) | (~ spl40_110 | ~ spl40_271)), inference(backward_demodulation, [], [f2043, f1262])).
fof(f1262, plain, ((e14 = op1(e10, e13)) | ~ spl40_110), inference(avatar_component_clause, [], [f1260])).
fof(f2043, plain, ((e10 = op1(e13, op1(e10, e13))) | ~ spl40_271), inference(avatar_component_clause, [], [f2041])).
fof(f2041, plain, (spl40_271 <=> (e10 = op1(e13, op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_271])])).
fof(f6993, plain, (~ spl40_107 | ~ spl40_112), inference(avatar_split_clause, [], [f6985, f1269, f1248])).
fof(f6985, plain, (~ (e11 = op1(e10, e13)) | ~ spl40_112), inference(backward_demodulation, [], [f336, f1271])).
fof(f1271, plain, ((e11 = op1(e10, e12)) | ~ spl40_112), inference(avatar_component_clause, [], [f1269])).
fof(f336, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f6950, plain, (~ spl40_181 | ~ spl40_184), inference(avatar_contradiction_clause, [], [f6949])).
fof(f6949, plain, ($false | (~ spl40_181 | ~ spl40_184)), inference(subsumption_resolution, [], [f6947, f491])).
fof(f491, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f10])).
fof(f6947, plain, ((e20 = e23) | (~ spl40_181 | ~ spl40_184)), inference(backward_demodulation, [], [f1623, f1611])).
fof(f1611, plain, ((e20 = op2(e22, e24)) | ~ spl40_181), inference(avatar_component_clause, [], [f1609])).
fof(f1609, plain, (spl40_181 <=> (e20 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_181])])).
fof(f1623, plain, ((e23 = op2(e22, e24)) | ~ spl40_184), inference(avatar_component_clause, [], [f1621])).
fof(f1621, plain, (spl40_184 <=> (e23 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_184])])).
fof(f6882, plain, (~ spl40_166 | ~ spl40_407), inference(avatar_split_clause, [], [f6873, f2805, f1546])).
fof(f2805, plain, (spl40_407 <=> (e20 = h4(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_407])])).
fof(f6873, plain, (~ (e20 = op2(e23, e22)) | ~ spl40_407), inference(backward_demodulation, [], [f2606, f2806])).
fof(f2806, plain, ((e20 = h4(e14)) | ~ spl40_407), inference(avatar_component_clause, [], [f2805])).
fof(f2606, plain, ~ (op2(e23, e22) = h4(e14)), inference(backward_demodulation, [], [f466, f671])).
fof(f466, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f6804, plain, (~ spl40_122 | ~ spl40_72), inference(avatar_split_clause, [], [f6803, f1101, f1311])).
fof(f1311, plain, (spl40_122 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl40_122])])).
fof(f6803, plain, (~ (op1(e10, e10) = e11) | ~ spl40_72), inference(forward_demodulation, [], [f280, f1103])).
fof(f1103, plain, ((e11 = op1(e12, e10)) | ~ spl40_72), inference(avatar_component_clause, [], [f1101])).
fof(f280, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f6792, plain, (~ spl40_9 | ~ spl40_84), inference(avatar_split_clause, [], [f6791, f1151, f836])).
fof(f6791, plain, (~ (e13 = op1(e14, e13)) | ~ spl40_84), inference(forward_demodulation, [], [f315, f1153])).
fof(f1153, plain, ((e13 = op1(e11, e13)) | ~ spl40_84), inference(avatar_component_clause, [], [f1151])).
fof(f315, plain, ~ (op1(e11, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f6752, plain, (~ spl40_31 | ~ spl40_36), inference(avatar_split_clause, [], [f6751, f950, f929])).
fof(f929, plain, (spl40_31 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_31])])).
fof(f6751, plain, (~ (e10 = op1(e13, e13)) | ~ spl40_36), inference(forward_demodulation, [], [f366, f952])).
fof(f366, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f6711, plain, (~ spl40_61 | ~ spl40_65), inference(avatar_contradiction_clause, [], [f6710])).
fof(f6710, plain, ($false | (~ spl40_61 | ~ spl40_65)), inference(subsumption_resolution, [], [f6709, f482])).
fof(f482, plain, ~ (e10 = e14), inference(cnf_transformation, [], [f9])).
fof(f6709, plain, ((e10 = e14) | (~ spl40_61 | ~ spl40_65)), inference(backward_demodulation, [], [f1073, f1057])).
fof(f1057, plain, ((e10 = op1(e12, e12)) | ~ spl40_61), inference(avatar_component_clause, [], [f1055])).
fof(f1055, plain, (spl40_61 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_61])])).
fof(f6708, plain, (~ spl40_72 | ~ spl40_116 | ~ spl40_310), inference(avatar_contradiction_clause, [], [f6707])).
fof(f6707, plain, ($false | (~ spl40_72 | ~ spl40_116 | ~ spl40_310)), inference(subsumption_resolution, [], [f6706, f480])).
fof(f6706, plain, ((e10 = e12) | (~ spl40_72 | ~ spl40_116 | ~ spl40_310)), inference(forward_demodulation, [], [f6703, f1288])).
fof(f1288, plain, ((e10 = op1(e10, e11)) | ~ spl40_116), inference(avatar_component_clause, [], [f1286])).
fof(f6703, plain, ((e12 = op1(e10, e11)) | (~ spl40_72 | ~ spl40_310)), inference(backward_demodulation, [], [f2245, f1103])).
fof(f2245, plain, ((e12 = op1(e10, op1(e12, e10))) | ~ spl40_310), inference(avatar_component_clause, [], [f2243])).
fof(f2243, plain, (spl40_310 <=> (e12 = op1(e10, op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_310])])).
fof(f6685, plain, (~ spl40_38 | ~ spl40_88), inference(avatar_split_clause, [], [f6679, f1168, f958])).
fof(f958, plain, (spl40_38 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_38])])).
fof(f6679, plain, (~ (e12 = op1(e13, e12)) | ~ spl40_88), inference(backward_demodulation, [], [f304, f1170])).
fof(f304, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f6671, plain, (~ spl40_46 | ~ spl40_96), inference(avatar_split_clause, [], [f6660, f1202, f992])).
fof(f992, plain, (spl40_46 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_46])])).
fof(f6660, plain, (~ (e10 = op1(e13, e10)) | ~ spl40_96), inference(backward_demodulation, [], [f284, f1204])).
fof(f1204, plain, ((e10 = op1(e11, e10)) | ~ spl40_96), inference(avatar_component_clause, [], [f1202])).
fof(f284, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f7])).
fof(f6604, plain, (~ spl40_82 | ~ spl40_127), inference(avatar_contradiction_clause, [], [f6603])).
fof(f6603, plain, ($false | (~ spl40_82 | ~ spl40_127)), inference(subsumption_resolution, [], [f6602, f484])).
fof(f484, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f9])).
fof(f6602, plain, ((e11 = e13) | (~ spl40_82 | ~ spl40_127)), inference(forward_demodulation, [], [f6587, f1145])).
fof(f1145, plain, ((e11 = op1(e11, e13)) | ~ spl40_82), inference(avatar_component_clause, [], [f1143])).
fof(f1143, plain, (spl40_82 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_82])])).
fof(f6587, plain, ((e13 = op1(e11, e13)) | ~ spl40_127), inference(backward_demodulation, [], [f138, f1334])).
fof(f6580, plain, (~ spl40_148 | ~ spl40_150), inference(avatar_contradiction_clause, [], [f6579])).
fof(f6579, plain, ($false | (~ spl40_148 | ~ spl40_150)), inference(subsumption_resolution, [], [f6578, f497])).
fof(f497, plain, ~ (e22 = e24), inference(cnf_transformation, [], [f10])).
fof(f6578, plain, ((e22 = e24) | (~ spl40_148 | ~ spl40_150)), inference(backward_demodulation, [], [f1480, f1472])).
fof(f1472, plain, ((e22 = op2(e24, e21)) | ~ spl40_148), inference(avatar_component_clause, [], [f1470])).
fof(f1470, plain, (spl40_148 <=> (e22 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_148])])).
fof(f1480, plain, ((e24 = op2(e24, e21)) | ~ spl40_150), inference(avatar_component_clause, [], [f1478])).
fof(f6568, plain, (~ spl40_186 | ~ spl40_187), inference(avatar_contradiction_clause, [], [f6567])).
fof(f6567, plain, ($false | (~ spl40_186 | ~ spl40_187)), inference(subsumption_resolution, [], [f6566, f489])).
fof(f489, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f10])).
fof(f6566, plain, ((e20 = e21) | (~ spl40_186 | ~ spl40_187)), inference(backward_demodulation, [], [f1636, f1632])).
fof(f1632, plain, ((e20 = op2(e22, e23)) | ~ spl40_186), inference(avatar_component_clause, [], [f1630])).
fof(f1636, plain, ((e21 = op2(e22, e23)) | ~ spl40_187), inference(avatar_component_clause, [], [f1634])).
fof(f6520, plain, (~ spl40_202 | ~ spl40_383 | ~ spl40_387), inference(avatar_split_clause, [], [f6516, f2704, f2684, f1697])).
fof(f2684, plain, (spl40_383 <=> (e21 = h5(e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_383])])).
fof(f6516, plain, (~ (e21 = op2(e22, e20)) | (~ spl40_383 | ~ spl40_387)), inference(backward_demodulation, [], [f5828, f2685])).
fof(f2685, plain, ((e21 = h5(e13)) | ~ spl40_383), inference(avatar_component_clause, [], [f2684])).
fof(f5828, plain, (~ (op2(e22, e20) = h5(e13)) | ~ spl40_387), inference(forward_demodulation, [], [f387, f5163])).
fof(f387, plain, ~ (op2(e22, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f6459, plain, (~ spl40_205 | ~ spl40_517), inference(avatar_split_clause, [], [f5819, f3487, f1709])).
fof(f1709, plain, (spl40_205 <=> (e24 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_205])])).
fof(f5819, plain, (~ (e24 = op2(e22, e20)) | ~ spl40_517), inference(forward_demodulation, [], [f2586, f3488])).
fof(f2586, plain, ~ (op2(e22, e20) = h3(e14)), inference(backward_demodulation, [], [f450, f666])).
fof(f450, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f6456, plain, (~ spl40_204 | ~ spl40_184), inference(avatar_split_clause, [], [f5822, f1621, f1705])).
fof(f1705, plain, (spl40_204 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_204])])).
fof(f5822, plain, (~ (e23 = op2(e22, e20)) | ~ spl40_184), inference(forward_demodulation, [], [f452, f1623])).
fof(f452, plain, ~ (op2(e22, e20) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f6423, plain, (~ spl40_74 | ~ spl40_54), inference(avatar_split_clause, [], [f6422, f1025, f1109])).
fof(f1109, plain, (spl40_74 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_74])])).
fof(f6422, plain, (~ (e13 = op1(e12, e10)) | ~ spl40_54), inference(forward_demodulation, [], [f352, f1027])).
fof(f352, plain, ~ (op1(e12, e10) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f6353, plain, (~ spl40_27 | ~ spl40_130), inference(avatar_contradiction_clause, [], [f6352])).
fof(f6352, plain, ($false | (~ spl40_27 | ~ spl40_130)), inference(subsumption_resolution, [], [f6351, f484])).
fof(f6351, plain, ((e11 = e13) | (~ spl40_27 | ~ spl40_130)), inference(forward_demodulation, [], [f6335, f914])).
fof(f6335, plain, ((e13 = op1(e13, e14)) | ~ spl40_130), inference(backward_demodulation, [], [f139, f1346])).
fof(f1346, plain, ((e14 = unit1) | ~ spl40_130), inference(avatar_component_clause, [], [f1344])).
fof(f1344, plain, (spl40_130 <=> (e14 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl40_130])])).
fof(f6313, plain, (spl40_378 | ~ spl40_153 | ~ spl40_387), inference(avatar_split_clause, [], [f6312, f2704, f1491, f2659])).
fof(f1491, plain, (spl40_153 <=> (e22 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_153])])).
fof(f6312, plain, ((e22 = h5(e13)) | (~ spl40_153 | ~ spl40_387)), inference(backward_demodulation, [], [f5163, f1493])).
fof(f1493, plain, ((e22 = op2(e24, e20)) | ~ spl40_153), inference(avatar_component_clause, [], [f1491])).
fof(f6293, plain, (~ spl40_191 | ~ spl40_195), inference(avatar_contradiction_clause, [], [f6292])).
fof(f6292, plain, ($false | (~ spl40_191 | ~ spl40_195)), inference(subsumption_resolution, [], [f6290, f492])).
fof(f492, plain, ~ (e20 = e24), inference(cnf_transformation, [], [f10])).
fof(f6290, plain, ((e20 = e24) | (~ spl40_191 | ~ spl40_195)), inference(backward_demodulation, [], [f1669, f1653])).
fof(f1653, plain, ((e20 = op2(e22, e22)) | ~ spl40_191), inference(avatar_component_clause, [], [f1651])).
fof(f1669, plain, ((e24 = op2(e22, e22)) | ~ spl40_195), inference(avatar_component_clause, [], [f1667])).
fof(f1667, plain, (spl40_195 <=> (e24 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_195])])).
fof(f6288, plain, (~ spl40_188 | ~ spl40_198), inference(avatar_split_clause, [], [f6285, f1680, f1638])).
fof(f1638, plain, (spl40_188 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_188])])).
fof(f6285, plain, (~ (e22 = op2(e22, e23)) | ~ spl40_198), inference(backward_demodulation, [], [f454, f1682])).
fof(f1682, plain, ((e22 = op2(e22, e21)) | ~ spl40_198), inference(avatar_component_clause, [], [f1680])).
fof(f6247, plain, (~ spl40_208 | ~ spl40_257), inference(avatar_contradiction_clause, [], [f6246])).
fof(f6246, plain, ($false | (~ spl40_208 | ~ spl40_257)), inference(subsumption_resolution, [], [f6245, f497])).
fof(f6245, plain, ((e22 = e24) | (~ spl40_208 | ~ spl40_257)), inference(forward_demodulation, [], [f6231, f1724])).
fof(f6231, plain, ((e24 = op2(e21, e24)) | ~ spl40_257), inference(backward_demodulation, [], [f226, f1930])).
fof(f6244, plain, (~ spl40_212 | ~ spl40_257), inference(avatar_contradiction_clause, [], [f6243])).
fof(f6243, plain, ($false | (~ spl40_212 | ~ spl40_257)), inference(subsumption_resolution, [], [f6242, f494])).
fof(f494, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f10])).
fof(f6242, plain, ((e21 = e23) | (~ spl40_212 | ~ spl40_257)), inference(forward_demodulation, [], [f6229, f1741])).
fof(f1741, plain, ((e21 = op2(e21, e23)) | ~ spl40_212), inference(avatar_component_clause, [], [f1739])).
fof(f1739, plain, (spl40_212 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_212])])).
fof(f6229, plain, ((e23 = op2(e21, e23)) | ~ spl40_257), inference(backward_demodulation, [], [f224, f1930])).
fof(f6213, plain, (spl40_248 | ~ spl40_202 | ~ spl40_365), inference(avatar_split_clause, [], [f6212, f2523, f1697, f1890])).
fof(f1890, plain, (spl40_248 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_248])])).
fof(f2523, plain, (spl40_365 <=> (e22 = op2(e20, op2(e22, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl40_365])])).
fof(f6212, plain, ((e22 = op2(e20, e21)) | (~ spl40_202 | ~ spl40_365)), inference(forward_demodulation, [], [f2525, f1699])).
fof(f1699, plain, ((e21 = op2(e22, e20)) | ~ spl40_202), inference(avatar_component_clause, [], [f1697])).
fof(f2525, plain, ((e22 = op2(e20, op2(e22, e20))) | ~ spl40_365), inference(avatar_component_clause, [], [f2523])).
fof(f6159, plain, (~ spl40_218 | ~ spl40_168), inference(avatar_split_clause, [], [f6158, f1554, f1764])).
fof(f6158, plain, (~ (e22 = op2(e21, e22)) | ~ spl40_168), inference(forward_demodulation, [], [f404, f1556])).
fof(f1556, plain, ((e22 = op2(e23, e22)) | ~ spl40_168), inference(avatar_component_clause, [], [f1554])).
fof(f404, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f6132, plain, (~ spl40_136 | ~ spl40_387), inference(avatar_split_clause, [], [f5162, f2704, f1420])).
fof(f1420, plain, (spl40_136 <=> (e20 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_136])])).
fof(f5162, plain, (~ (e20 = op2(e24, e23)) | ~ spl40_387), inference(backward_demodulation, [], [f2619, f2705])).
fof(f2619, plain, ~ (op2(e24, e23) = h5(e14)), inference(backward_demodulation, [], [f478, f676])).
fof(f478, plain, ~ (op2(e24, e23) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f6121, plain, (~ spl40_102 | ~ spl40_27), inference(avatar_split_clause, [], [f6120, f912, f1227])).
fof(f1227, plain, (spl40_102 <=> (e11 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_102])])).
fof(f6120, plain, (~ (e11 = op1(e10, e14)) | ~ spl40_27), inference(forward_demodulation, [], [f321, f914])).
fof(f321, plain, ~ (op1(e10, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f6098, plain, (~ spl40_10 | ~ spl40_20), inference(avatar_split_clause, [], [f6097, f882, f840])).
fof(f840, plain, (spl40_10 <=> (e14 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_10])])).
fof(f6097, plain, (~ (e14 = op1(e14, e13)) | ~ spl40_20), inference(forward_demodulation, [], [f374, f884])).
fof(f884, plain, ((e14 = op1(e14, e11)) | ~ spl40_20), inference(avatar_component_clause, [], [f882])).
fof(f374, plain, ~ (op1(e14, e11) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f6065, plain, (~ spl40_84 | spl40_269), inference(avatar_contradiction_clause, [], [f6064])).
fof(f6064, plain, ($false | (~ spl40_84 | spl40_269)), inference(subsumption_resolution, [], [f6061, f1153])).
fof(f6061, plain, (~ (e13 = op1(e11, e13)) | (~ spl40_84 | spl40_269)), inference(backward_demodulation, [], [f2034, f1153])).
fof(f2034, plain, (~ (e13 = op1(e11, op1(e11, e13))) | spl40_269), inference(avatar_component_clause, [], [f2032])).
fof(f2032, plain, (spl40_269 <=> (e13 = op1(e11, op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_269])])).
fof(f6002, plain, (~ spl40_27 | ~ spl40_129), inference(avatar_contradiction_clause, [], [f6001])).
fof(f6001, plain, ($false | (~ spl40_27 | ~ spl40_129)), inference(subsumption_resolution, [], [f6000, f485])).
fof(f6000, plain, ((e11 = e14) | (~ spl40_27 | ~ spl40_129)), inference(forward_demodulation, [], [f5987, f914])).
fof(f5987, plain, ((e14 = op1(e13, e14)) | ~ spl40_129), inference(backward_demodulation, [], [f140, f1342])).
fof(f1342, plain, ((e13 = unit1) | ~ spl40_129), inference(avatar_component_clause, [], [f1340])).
fof(f1340, plain, (spl40_129 <=> (e13 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl40_129])])).
fof(f5963, plain, (~ spl40_155 | ~ spl40_387 | spl40_485), inference(avatar_contradiction_clause, [], [f5962])).
fof(f5962, plain, ($false | (~ spl40_155 | ~ spl40_387 | spl40_485)), inference(subsumption_resolution, [], [f5961, f3217])).
fof(f3217, plain, (~ (e24 = h5(e13)) | spl40_485), inference(avatar_component_clause, [], [f3215])).
fof(f5961, plain, ((e24 = h5(e13)) | (~ spl40_155 | ~ spl40_387)), inference(backward_demodulation, [], [f5163, f1501])).
fof(f5959, plain, (~ spl40_157 | ~ spl40_158), inference(avatar_contradiction_clause, [], [f5958])).
fof(f5958, plain, ($false | (~ spl40_157 | ~ spl40_158)), inference(subsumption_resolution, [], [f5957, f493])).
fof(f5957, plain, ((e21 = e22) | (~ spl40_157 | ~ spl40_158)), inference(backward_demodulation, [], [f1514, f1510])).
fof(f1510, plain, ((e21 = op2(e23, e24)) | ~ spl40_157), inference(avatar_component_clause, [], [f1508])).
fof(f1508, plain, (spl40_157 <=> (e21 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_157])])).
fof(f1514, plain, ((e22 = op2(e23, e24)) | ~ spl40_158), inference(avatar_component_clause, [], [f1512])).
fof(f1512, plain, (spl40_158 <=> (e22 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_158])])).
fof(f5951, plain, (~ spl40_169 | ~ spl40_174), inference(avatar_split_clause, [], [f5949, f1579, f1558])).
fof(f1558, plain, (spl40_169 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_169])])).
fof(f5949, plain, (~ (e23 = op2(e23, e22)) | ~ spl40_174), inference(backward_demodulation, [], [f463, f1581])).
fof(f1581, plain, ((e23 = op2(e23, e21)) | ~ spl40_174), inference(avatar_component_clause, [], [f1579])).
fof(f463, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f5930, plain, (~ spl40_193 | ~ spl40_195), inference(avatar_contradiction_clause, [], [f5929])).
fof(f5929, plain, ($false | (~ spl40_193 | ~ spl40_195)), inference(subsumption_resolution, [], [f5928, f497])).
fof(f5928, plain, ((e22 = e24) | (~ spl40_193 | ~ spl40_195)), inference(backward_demodulation, [], [f1669, f1661])).
fof(f1661, plain, ((e22 = op2(e22, e22)) | ~ spl40_193), inference(avatar_component_clause, [], [f1659])).
fof(f1659, plain, (spl40_193 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_193])])).
fof(f5907, plain, (~ spl40_225 | spl40_557), inference(avatar_contradiction_clause, [], [f5906])).
fof(f5906, plain, ($false | (~ spl40_225 | spl40_557)), inference(subsumption_resolution, [], [f5905, f3667])).
fof(f3667, plain, (~ (e24 = h2(e14)) | spl40_557), inference(avatar_component_clause, [], [f3665])).
fof(f5905, plain, ((e24 = h2(e14)) | ~ spl40_225), inference(backward_demodulation, [], [f661, f1795])).
fof(f1795, plain, ((e24 = op2(e21, e21)) | ~ spl40_225), inference(avatar_component_clause, [], [f1793])).
fof(f1793, plain, (spl40_225 <=> (e24 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_225])])).
fof(f5868, plain, (~ spl40_131 | ~ spl40_260), inference(avatar_contradiction_clause, [], [f5867])).
fof(f5867, plain, ($false | (~ spl40_131 | ~ spl40_260)), inference(subsumption_resolution, [], [f5866, f492])).
fof(f5866, plain, ((e20 = e24) | (~ spl40_131 | ~ spl40_260)), inference(forward_demodulation, [], [f5850, f1401])).
fof(f5850, plain, ((e24 = op2(e24, e24)) | ~ spl40_260), inference(backward_demodulation, [], [f227, f1942])).
fof(f1942, plain, ((e24 = unit2) | ~ spl40_260), inference(avatar_component_clause, [], [f1940])).
fof(f1940, plain, (spl40_260 <=> (e24 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl40_260])])).
fof(f5839, plain, (~ spl40_240 | ~ spl40_235), inference(avatar_split_clause, [], [f5838, f1835, f1856])).
fof(f1856, plain, (spl40_240 <=> (e24 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_240])])).
fof(f5838, plain, (~ (e24 = op2(e20, e23)) | ~ spl40_235), inference(forward_demodulation, [], [f438, f1837])).
fof(f438, plain, ~ (op2(e20, e23) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f5817, plain, (~ spl40_199 | ~ spl40_184), inference(avatar_split_clause, [], [f5816, f1621, f1684])).
fof(f1684, plain, (spl40_199 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_199])])).
fof(f5816, plain, (~ (e23 = op2(e22, e21)) | ~ spl40_184), inference(forward_demodulation, [], [f455, f1623])).
fof(f455, plain, ~ (op2(e22, e21) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f5815, plain, (~ spl40_198 | ~ spl40_248), inference(avatar_split_clause, [], [f5814, f1890, f1680])).
fof(f5814, plain, (~ (e22 = op2(e22, e21)) | ~ spl40_248), inference(forward_demodulation, [], [f390, f1892])).
fof(f1892, plain, ((e22 = op2(e20, e21)) | ~ spl40_248), inference(avatar_component_clause, [], [f1890])).
fof(f5802, plain, (~ spl40_163 | spl40_397), inference(avatar_contradiction_clause, [], [f5801])).
fof(f5801, plain, ($false | (~ spl40_163 | spl40_397)), inference(subsumption_resolution, [], [f5799, f2757])).
fof(f2757, plain, (~ (e22 = h4(e14)) | spl40_397), inference(avatar_component_clause, [], [f2755])).
fof(f5799, plain, ((e22 = h4(e14)) | ~ spl40_163), inference(backward_demodulation, [], [f671, f1535])).
fof(f1535, plain, ((e22 = op2(e23, e23)) | ~ spl40_163), inference(avatar_component_clause, [], [f1533])).
fof(f1533, plain, (spl40_163 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_163])])).
fof(f5781, plain, (~ spl40_110 | ~ spl40_105), inference(avatar_split_clause, [], [f5780, f1239, f1260])).
fof(f5780, plain, (~ (e14 = op1(e10, e13)) | ~ spl40_105), inference(forward_demodulation, [], [f338, f1241])).
fof(f338, plain, ~ (op1(e10, e13) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f5775, plain, (~ spl40_93 | ~ spl40_78), inference(avatar_split_clause, [], [f5774, f1126, f1189])).
fof(f1189, plain, (spl40_93 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_93])])).
fof(f5774, plain, (~ (e12 = op1(e11, e11)) | ~ spl40_78), inference(forward_demodulation, [], [f345, f1128])).
fof(f345, plain, ~ (op1(e11, e11) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f5723, plain, (~ spl40_12 | ~ spl40_14), inference(avatar_contradiction_clause, [], [f5722])).
fof(f5722, plain, ($false | (~ spl40_12 | ~ spl40_14)), inference(subsumption_resolution, [], [f5721, f484])).
fof(f5721, plain, ((e11 = e13) | (~ spl40_12 | ~ spl40_14)), inference(forward_demodulation, [], [f859, f851])).
fof(f859, plain, ((e13 = op1(e14, e12)) | ~ spl40_14), inference(avatar_component_clause, [], [f857])).
fof(f857, plain, (spl40_14 <=> (e13 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_14])])).
fof(f5703, plain, (~ spl40_51 | ~ spl40_54), inference(avatar_contradiction_clause, [], [f5702])).
fof(f5702, plain, ($false | (~ spl40_51 | ~ spl40_54)), inference(subsumption_resolution, [], [f5700, f481])).
fof(f481, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f9])).
fof(f5700, plain, ((e10 = e13) | (~ spl40_51 | ~ spl40_54)), inference(backward_demodulation, [], [f1027, f1015])).
fof(f1015, plain, ((e10 = op1(e12, e14)) | ~ spl40_51), inference(avatar_component_clause, [], [f1013])).
fof(f1013, plain, (spl40_51 <=> (e10 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_51])])).
fof(f5696, plain, (~ spl40_72 | ~ spl40_73), inference(avatar_contradiction_clause, [], [f5695])).
fof(f5695, plain, ($false | (~ spl40_72 | ~ spl40_73)), inference(subsumption_resolution, [], [f5694, f483])).
fof(f483, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f9])).
fof(f5694, plain, ((e11 = e12) | (~ spl40_72 | ~ spl40_73)), inference(backward_demodulation, [], [f1107, f1103])).
fof(f5645, plain, (~ spl40_54 | ~ spl40_128), inference(avatar_contradiction_clause, [], [f5644])).
fof(f5644, plain, ($false | (~ spl40_54 | ~ spl40_128)), inference(subsumption_resolution, [], [f5643, f488])).
fof(f488, plain, ~ (e13 = e14), inference(cnf_transformation, [], [f9])).
fof(f5643, plain, ((e13 = e14) | (~ spl40_54 | ~ spl40_128)), inference(forward_demodulation, [], [f5629, f1027])).
fof(f5629, plain, ((e14 = op1(e12, e14)) | ~ spl40_128), inference(backward_demodulation, [], [f140, f1338])).
fof(f1338, plain, ((e12 = unit1) | ~ spl40_128), inference(avatar_component_clause, [], [f1336])).
fof(f1336, plain, (spl40_128 <=> (e12 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl40_128])])).
fof(f5602, plain, (~ spl40_152 | spl40_383 | ~ spl40_387), inference(avatar_contradiction_clause, [], [f5601])).
fof(f5601, plain, ($false | (~ spl40_152 | spl40_383 | ~ spl40_387)), inference(subsumption_resolution, [], [f5600, f2686])).
fof(f2686, plain, (~ (e21 = h5(e13)) | spl40_383), inference(avatar_component_clause, [], [f2684])).
fof(f5600, plain, ((e21 = h5(e13)) | (~ spl40_152 | ~ spl40_387)), inference(backward_demodulation, [], [f5163, f1489])).
fof(f1489, plain, ((e21 = op2(e24, e20)) | ~ spl40_152), inference(avatar_component_clause, [], [f1487])).
fof(f1487, plain, (spl40_152 <=> (e21 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_152])])).
fof(f5574, plain, (~ spl40_157 | ~ spl40_207), inference(avatar_split_clause, [], [f5567, f1718, f1508])).
fof(f1718, plain, (spl40_207 <=> (e21 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_207])])).
fof(f5567, plain, (~ (e21 = op2(e23, e24)) | ~ spl40_207), inference(backward_demodulation, [], [f424, f1720])).
fof(f1720, plain, ((e21 = op2(e21, e24)) | ~ spl40_207), inference(avatar_component_clause, [], [f1718])).
fof(f424, plain, ~ (op2(e21, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f5510, plain, (spl40_172 | ~ spl40_259), inference(avatar_split_clause, [], [f5498, f1936, f1571])).
fof(f1571, plain, (spl40_172 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_172])])).
fof(f1936, plain, (spl40_259 <=> (e23 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl40_259])])).
fof(f5498, plain, ((e21 = op2(e23, e21)) | ~ spl40_259), inference(backward_demodulation, [], [f220, f1938])).
fof(f1938, plain, ((e23 = unit2) | ~ spl40_259), inference(avatar_component_clause, [], [f1936])).
fof(f5489, plain, (~ spl40_245 | ~ spl40_517), inference(avatar_split_clause, [], [f5488, f3487, f1877])).
fof(f5488, plain, (~ (e24 = op2(e20, e22)) | ~ spl40_517), inference(forward_demodulation, [], [f2582, f3488])).
fof(f2582, plain, ~ (op2(e20, e22) = h3(e14)), inference(backward_demodulation, [], [f400, f666])).
fof(f400, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f5462, plain, (~ spl40_206 | ~ spl40_387), inference(avatar_split_clause, [], [f5461, f2704, f1714])).
fof(f1714, plain, (spl40_206 <=> (e20 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_206])])).
fof(f5461, plain, (~ (e20 = op2(e21, e24)) | ~ spl40_387), inference(forward_demodulation, [], [f2613, f2705])).
fof(f2613, plain, ~ (op2(e21, e24) = h5(e14)), inference(backward_demodulation, [], [f425, f676])).
fof(f425, plain, ~ (op2(e21, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5454, plain, (~ spl40_161 | spl40_407), inference(avatar_contradiction_clause, [], [f5453])).
fof(f5453, plain, ($false | (~ spl40_161 | spl40_407)), inference(subsumption_resolution, [], [f5452, f2807])).
fof(f2807, plain, (~ (e20 = h4(e14)) | spl40_407), inference(avatar_component_clause, [], [f2805])).
fof(f5452, plain, ((e20 = h4(e14)) | ~ spl40_161), inference(forward_demodulation, [], [f671, f1527])).
fof(f1527, plain, ((e20 = op2(e23, e23)) | ~ spl40_161), inference(avatar_component_clause, [], [f1525])).
fof(f1525, plain, (spl40_161 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_161])])).
fof(f5444, plain, (spl40_157 | ~ spl40_517), inference(avatar_split_clause, [], [f4761, f3487, f1508])).
fof(f4761, plain, ((e21 = op2(e23, e24)) | ~ spl40_517), inference(backward_demodulation, [], [f2592, f3488])).
fof(f5436, plain, (~ spl40_146 | ~ spl40_387), inference(avatar_split_clause, [], [f5435, f2704, f1462])).
fof(f1462, plain, (spl40_146 <=> (e20 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_146])])).
fof(f5435, plain, (~ (e20 = op2(e24, e21)) | ~ spl40_387), inference(forward_demodulation, [], [f2617, f2705])).
fof(f2617, plain, ~ (op2(e24, e21) = h5(e14)), inference(backward_demodulation, [], [f475, f676])).
fof(f475, plain, ~ (op2(e24, e21) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5406, plain, (~ spl40_113 | ~ spl40_118), inference(avatar_split_clause, [], [f5405, f1294, f1273])).
fof(f1294, plain, (spl40_118 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_118])])).
fof(f5405, plain, (~ (e12 = op1(e10, e12)) | ~ spl40_118), inference(forward_demodulation, [], [f333, f1296])).
fof(f1296, plain, ((e12 = op1(e10, e11)) | ~ spl40_118), inference(avatar_component_clause, [], [f1294])).
fof(f5400, plain, (~ spl40_94 | ~ spl40_89), inference(avatar_split_clause, [], [f5399, f1172, f1193])).
fof(f1193, plain, (spl40_94 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_94])])).
fof(f5399, plain, (~ (e13 = op1(e11, e11)) | ~ spl40_89), inference(forward_demodulation, [], [f343, f1174])).
fof(f343, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f5375, plain, (~ spl40_70 | ~ spl40_65), inference(avatar_split_clause, [], [f5374, f1071, f1092])).
fof(f1092, plain, (spl40_70 <=> (e14 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_70])])).
fof(f5374, plain, (~ (e14 = op1(e12, e11)) | ~ spl40_65), inference(forward_demodulation, [], [f353, f1073])).
fof(f353, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f5371, plain, (~ spl40_69 | ~ spl40_54), inference(avatar_split_clause, [], [f5370, f1025, f1088])).
fof(f1088, plain, (spl40_69 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_69])])).
fof(f5370, plain, (~ (e13 = op1(e12, e11)) | ~ spl40_54), inference(forward_demodulation, [], [f355, f1027])).
fof(f355, plain, ~ (op1(e12, e11) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f5361, plain, (~ spl40_47 | ~ spl40_27), inference(avatar_split_clause, [], [f5024, f912, f996])).
fof(f996, plain, (spl40_47 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_47])])).
fof(f5024, plain, (~ (e11 = op1(e13, e10)) | ~ spl40_27), inference(forward_demodulation, [], [f362, f914])).
fof(f362, plain, ~ (op1(e13, e10) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f5356, plain, (~ spl40_39 | ~ spl40_44), inference(avatar_split_clause, [], [f5355, f983, f962])).
fof(f962, plain, (spl40_39 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_39])])).
fof(f5355, plain, (~ (e13 = op1(e13, e12)) | ~ spl40_44), inference(forward_demodulation, [], [f363, f985])).
fof(f363, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f5348, plain, (~ spl40_6 | ~ spl40_1), inference(avatar_split_clause, [], [f4989, f803, f824])).
fof(f824, plain, (spl40_6 <=> (e10 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_6])])).
fof(f4989, plain, (~ (e10 = op1(e14, e13)) | ~ spl40_1), inference(forward_demodulation, [], [f378, f805])).
fof(f378, plain, ~ (op1(e14, e13) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f5337, plain, (~ spl40_27 | ~ spl40_28), inference(avatar_contradiction_clause, [], [f5336])).
fof(f5336, plain, ($false | (~ spl40_27 | ~ spl40_28)), inference(subsumption_resolution, [], [f5335, f483])).
fof(f5335, plain, ((e11 = e12) | (~ spl40_27 | ~ spl40_28)), inference(forward_demodulation, [], [f918, f914])).
fof(f918, plain, ((e12 = op1(e13, e14)) | ~ spl40_28), inference(avatar_component_clause, [], [f916])).
fof(f916, plain, (spl40_28 <=> (e12 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_28])])).
fof(f5283, plain, (~ spl40_66 | ~ spl40_127), inference(avatar_contradiction_clause, [], [f5282])).
fof(f5282, plain, ($false | (~ spl40_66 | ~ spl40_127)), inference(subsumption_resolution, [], [f5281, f480])).
fof(f5281, plain, ((e10 = e12) | (~ spl40_66 | ~ spl40_127)), inference(forward_demodulation, [], [f5271, f1078])).
fof(f5271, plain, ((e12 = op1(e12, e11)) | ~ spl40_127), inference(backward_demodulation, [], [f137, f1334])).
fof(f5265, plain, (~ spl40_131 | ~ spl40_133), inference(avatar_contradiction_clause, [], [f5264])).
fof(f5264, plain, ($false | (~ spl40_131 | ~ spl40_133)), inference(subsumption_resolution, [], [f5263, f490])).
fof(f490, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f10])).
fof(f5263, plain, ((e20 = e22) | (~ spl40_131 | ~ spl40_133)), inference(backward_demodulation, [], [f1409, f1401])).
fof(f1409, plain, ((e22 = op2(e24, e24)) | ~ spl40_133), inference(avatar_component_clause, [], [f1407])).
fof(f1407, plain, (spl40_133 <=> (e22 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_133])])).
fof(f5230, plain, (~ spl40_192 | ~ spl40_195), inference(avatar_contradiction_clause, [], [f5229])).
fof(f5229, plain, ($false | (~ spl40_192 | ~ spl40_195)), inference(subsumption_resolution, [], [f5228, f495])).
fof(f5228, plain, ((e21 = e24) | (~ spl40_192 | ~ spl40_195)), inference(backward_demodulation, [], [f1669, f1657])).
fof(f1657, plain, ((e21 = op2(e22, e22)) | ~ spl40_192), inference(avatar_component_clause, [], [f1655])).
fof(f1655, plain, (spl40_192 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_192])])).
fof(f5189, plain, (spl40_193 | ~ spl40_258), inference(avatar_split_clause, [], [f5182, f1932, f1659])).
fof(f1932, plain, (spl40_258 <=> (e22 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl40_258])])).
fof(f5182, plain, ((e22 = op2(e22, e22)) | ~ spl40_258), inference(backward_demodulation, [], [f223, f1934])).
fof(f1934, plain, ((e22 = unit2) | ~ spl40_258), inference(avatar_component_clause, [], [f1932])).
fof(f5158, plain, (~ spl40_247 | ~ spl40_197), inference(avatar_split_clause, [], [f5157, f1676, f1886])).
fof(f1676, plain, (spl40_197 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_197])])).
fof(f5157, plain, (~ (e21 = op2(e20, e21)) | ~ spl40_197), inference(forward_demodulation, [], [f390, f1678])).
fof(f1678, plain, ((e21 = op2(e22, e21)) | ~ spl40_197), inference(avatar_component_clause, [], [f1676])).
fof(f5153, plain, (~ spl40_250 | ~ spl40_235), inference(avatar_split_clause, [], [f5152, f1835, f1898])).
fof(f1898, plain, (spl40_250 <=> (e24 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_250])])).
fof(f5152, plain, (~ (e24 = op2(e20, e21)) | ~ spl40_235), inference(backward_demodulation, [], [f435, f1837])).
fof(f435, plain, ~ (op2(e20, e21) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f5135, plain, (~ spl40_218 | ~ spl40_213), inference(avatar_split_clause, [], [f5134, f1743, f1764])).
fof(f1743, plain, (spl40_213 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_213])])).
fof(f5134, plain, (~ (e22 = op2(e21, e22)) | ~ spl40_213), inference(forward_demodulation, [], [f446, f1745])).
fof(f1745, plain, ((e22 = op2(e21, e23)) | ~ spl40_213), inference(avatar_component_clause, [], [f1743])).
fof(f446, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f5113, plain, (~ spl40_209 | ~ spl40_184), inference(avatar_split_clause, [], [f5111, f1621, f1726])).
fof(f1726, plain, (spl40_209 <=> (e23 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_209])])).
fof(f5111, plain, (~ (e23 = op2(e21, e24)) | ~ spl40_184), inference(backward_demodulation, [], [f423, f1623])).
fof(f423, plain, ~ (op2(e21, e24) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f5107, plain, (spl40_184 | ~ spl40_517), inference(avatar_split_clause, [], [f4760, f3487, f1621])).
fof(f4760, plain, ((e23 = op2(e22, e24)) | ~ spl40_517), inference(backward_demodulation, [], [f2591, f3488])).
fof(f5106, plain, (~ spl40_179 | ~ spl40_169), inference(avatar_split_clause, [], [f5105, f1558, f1600])).
fof(f5105, plain, (~ (e23 = op2(e23, e20)) | ~ spl40_169), inference(forward_demodulation, [], [f460, f1560])).
fof(f1560, plain, ((e23 = op2(e23, e22)) | ~ spl40_169), inference(avatar_component_clause, [], [f1558])).
fof(f460, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f5100, plain, (~ spl40_172 | ~ spl40_157), inference(avatar_split_clause, [], [f5099, f1508, f1571])).
fof(f5099, plain, (~ (e21 = op2(e23, e21)) | ~ spl40_157), inference(forward_demodulation, [], [f465, f1510])).
fof(f465, plain, ~ (op2(e23, e21) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f5088, plain, (~ spl40_162 | spl40_402), inference(avatar_contradiction_clause, [], [f5087])).
fof(f5087, plain, ($false | (~ spl40_162 | spl40_402)), inference(subsumption_resolution, [], [f5086, f2782])).
fof(f2782, plain, (~ (e21 = h4(e14)) | spl40_402), inference(avatar_component_clause, [], [f2780])).
fof(f2780, plain, (spl40_402 <=> (e21 = h4(e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_402])])).
fof(f5086, plain, ((e21 = h4(e14)) | ~ spl40_162), inference(forward_demodulation, [], [f671, f1531])).
fof(f1531, plain, ((e21 = op2(e23, e23)) | ~ spl40_162), inference(avatar_component_clause, [], [f1529])).
fof(f1529, plain, (spl40_162 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_162])])).
fof(f5069, plain, (~ spl40_139 | ~ spl40_239), inference(avatar_split_clause, [], [f5068, f1852, f1432])).
fof(f1432, plain, (spl40_139 <=> (e23 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_139])])).
fof(f5068, plain, (~ (e23 = op2(e24, e23)) | ~ spl40_239), inference(forward_demodulation, [], [f412, f1854])).
fof(f412, plain, ~ (op2(e20, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f5038, plain, (~ spl40_77 | ~ spl40_27), inference(avatar_split_clause, [], [f5037, f912, f1122])).
fof(f1122, plain, (spl40_77 <=> (e11 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_77])])).
fof(f5037, plain, (~ (e11 = op1(e11, e14)) | ~ spl40_27), inference(forward_demodulation, [], [f324, f914])).
fof(f324, plain, ~ (op1(e11, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f5034, plain, (~ spl40_79 | ~ spl40_54), inference(avatar_split_clause, [], [f5032, f1025, f1130])).
fof(f1130, plain, (spl40_79 <=> (e13 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_79])])).
fof(f5032, plain, (~ (e13 = op1(e11, e14)) | ~ spl40_54), inference(backward_demodulation, [], [f323, f1027])).
fof(f323, plain, ~ (op1(e11, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f5009, plain, (~ spl40_32 | ~ spl40_27), inference(avatar_split_clause, [], [f5008, f912, f933])).
fof(f933, plain, (spl40_32 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_32])])).
fof(f5008, plain, (~ (e11 = op1(e13, e13)) | ~ spl40_27), inference(forward_demodulation, [], [f368, f914])).
fof(f368, plain, ~ (op1(e13, e13) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4991, plain, (~ spl40_13 | ~ spl40_113), inference(avatar_split_clause, [], [f4990, f1273, f853])).
fof(f853, plain, (spl40_13 <=> (e12 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_13])])).
fof(f4990, plain, (~ (e12 = op1(e14, e12)) | ~ spl40_113), inference(forward_demodulation, [], [f302, f1275])).
fof(f302, plain, ~ (op1(e10, e12) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f4973, plain, (~ spl40_26 | ~ spl40_27), inference(avatar_contradiction_clause, [], [f4972])).
fof(f4972, plain, ($false | (~ spl40_26 | ~ spl40_27)), inference(subsumption_resolution, [], [f4971, f479])).
fof(f4971, plain, ((e10 = e11) | (~ spl40_26 | ~ spl40_27)), inference(backward_demodulation, [], [f914, f910])).
fof(f910, plain, ((e10 = op1(e13, e14)) | ~ spl40_26), inference(avatar_component_clause, [], [f908])).
fof(f4957, plain, (~ spl40_39 | ~ spl40_49), inference(avatar_split_clause, [], [f4955, f1004, f962])).
fof(f4955, plain, (~ (e13 = op1(e13, e12)) | ~ spl40_49), inference(backward_demodulation, [], [f360, f1006])).
fof(f360, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4940, plain, (~ spl40_81 | ~ spl40_85), inference(avatar_contradiction_clause, [], [f4939])).
fof(f4939, plain, ($false | (~ spl40_81 | ~ spl40_85)), inference(subsumption_resolution, [], [f4938, f482])).
fof(f4938, plain, ((e10 = e14) | (~ spl40_81 | ~ spl40_85)), inference(forward_demodulation, [], [f1157, f1141])).
fof(f4929, plain, (~ spl40_87 | ~ spl40_92), inference(avatar_split_clause, [], [f4928, f1185, f1164])).
fof(f1164, plain, (spl40_87 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_87])])).
fof(f4928, plain, (~ (e11 = op1(e11, e12)) | ~ spl40_92), inference(backward_demodulation, [], [f343, f1187])).
fof(f1187, plain, ((e11 = op1(e11, e11)) | ~ spl40_92), inference(avatar_component_clause, [], [f1185])).
fof(f4926, plain, (~ spl40_87 | ~ spl40_97), inference(avatar_split_clause, [], [f4920, f1206, f1164])).
fof(f4920, plain, (~ (e11 = op1(e11, e12)) | ~ spl40_97), inference(backward_demodulation, [], [f340, f1208])).
fof(f340, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f4910, plain, (~ spl40_38 | ~ spl40_113), inference(avatar_split_clause, [], [f4906, f1273, f958])).
fof(f4906, plain, (~ (e12 = op1(e13, e12)) | ~ spl40_113), inference(backward_demodulation, [], [f301, f1275])).
fof(f301, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4868, plain, (~ spl40_173 | ~ spl40_175), inference(avatar_contradiction_clause, [], [f4867])).
fof(f4867, plain, ($false | (~ spl40_173 | ~ spl40_175)), inference(subsumption_resolution, [], [f4866, f497])).
fof(f4866, plain, ((e22 = e24) | (~ spl40_173 | ~ spl40_175)), inference(forward_demodulation, [], [f1585, f1577])).
fof(f4827, plain, (~ spl40_217 | ~ spl40_227), inference(avatar_split_clause, [], [f4821, f1802, f1760])).
fof(f1760, plain, (spl40_217 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_217])])).
fof(f4821, plain, (~ (e21 = op2(e21, e22)) | ~ spl40_227), inference(backward_demodulation, [], [f440, f1804])).
fof(f440, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f8])).
fof(f4791, plain, (~ spl40_173 | ~ spl40_257), inference(avatar_contradiction_clause, [], [f4790])).
fof(f4790, plain, ($false | (~ spl40_173 | ~ spl40_257)), inference(subsumption_resolution, [], [f4789, f496])).
fof(f4789, plain, ((e22 = e23) | (~ spl40_173 | ~ spl40_257)), inference(forward_demodulation, [], [f4777, f1577])).
fof(f4777, plain, ((e23 = op2(e23, e21)) | ~ spl40_257), inference(backward_demodulation, [], [f225, f1930])).
fof(f4769, plain, (~ spl40_182 | ~ spl40_517), inference(avatar_contradiction_clause, [], [f4768])).
fof(f4768, plain, ($false | (~ spl40_182 | ~ spl40_517)), inference(subsumption_resolution, [], [f4767, f494])).
fof(f4767, plain, ((e21 = e23) | (~ spl40_182 | ~ spl40_517)), inference(forward_demodulation, [], [f4760, f1615])).
fof(f1615, plain, ((e21 = op2(e22, e24)) | ~ spl40_182), inference(avatar_component_clause, [], [f1613])).
fof(f1613, plain, (spl40_182 <=> (e21 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_182])])).
fof(f4766, plain, (spl40_131 | ~ spl40_517), inference(avatar_split_clause, [], [f4759, f3487, f1399])).
fof(f4759, plain, ((e20 = op2(e24, e24)) | ~ spl40_517), inference(backward_demodulation, [], [f2590, f3488])).
fof(f4765, plain, (~ spl40_190 | ~ spl40_517), inference(avatar_split_clause, [], [f4758, f3487, f1646])).
fof(f1646, plain, (spl40_190 <=> (e24 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_190])])).
fof(f4758, plain, (~ (e24 = op2(e22, e23)) | ~ spl40_517), inference(backward_demodulation, [], [f2588, f3488])).
fof(f2588, plain, ~ (op2(e22, e23) = h3(e14)), inference(backward_demodulation, [], [f456, f666])).
fof(f456, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f4764, plain, (~ spl40_200 | ~ spl40_517), inference(avatar_split_clause, [], [f4757, f3487, f1688])).
fof(f1688, plain, (spl40_200 <=> (e24 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl40_200])])).
fof(f4757, plain, (~ (e24 = op2(e22, e21)) | ~ spl40_517), inference(backward_demodulation, [], [f2587, f3488])).
fof(f2587, plain, ~ (op2(e22, e21) = h3(e14)), inference(backward_demodulation, [], [f453, f666])).
fof(f453, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f4763, plain, (~ spl40_170 | ~ spl40_517), inference(avatar_split_clause, [], [f4756, f3487, f1562])).
fof(f1562, plain, (spl40_170 <=> (e24 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_170])])).
fof(f4756, plain, (~ (e24 = op2(e23, e22)) | ~ spl40_517), inference(backward_demodulation, [], [f2584, f3488])).
fof(f2584, plain, ~ (op2(e23, e22) = h3(e14)), inference(backward_demodulation, [], [f406, f666])).
fof(f406, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4762, plain, (~ spl40_220 | ~ spl40_517), inference(avatar_split_clause, [], [f4755, f3487, f1772])).
fof(f1772, plain, (spl40_220 <=> (e24 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_220])])).
fof(f4755, plain, (~ (e24 = op2(e21, e22)) | ~ spl40_517), inference(backward_demodulation, [], [f2583, f3488])).
fof(f2583, plain, ~ (op2(e21, e22) = h3(e14)), inference(backward_demodulation, [], [f403, f666])).
fof(f403, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f4742, plain, (~ spl40_238 | ~ spl40_233), inference(avatar_split_clause, [], [f4741, f1827, f1848])).
fof(f1848, plain, (spl40_238 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_238])])).
fof(f4741, plain, (~ (e22 = op2(e20, e23)) | ~ spl40_233), inference(forward_demodulation, [], [f438, f1829])).
fof(f4737, plain, (~ spl40_226 | ~ spl40_201), inference(avatar_split_clause, [], [f4736, f1693, f1798])).
fof(f1693, plain, (spl40_201 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_201])])).
fof(f4736, plain, (~ (e20 = op2(e21, e20)) | ~ spl40_201), inference(forward_demodulation, [], [f383, f1695])).
fof(f1695, plain, ((e20 = op2(e22, e20)) | ~ spl40_201), inference(avatar_component_clause, [], [f1693])).
fof(f4684, plain, (~ spl40_167 | ~ spl40_157), inference(avatar_split_clause, [], [f4683, f1508, f1550])).
fof(f1550, plain, (spl40_167 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_167])])).
fof(f4683, plain, (~ (e21 = op2(e23, e22)) | ~ spl40_157), inference(forward_demodulation, [], [f467, f1510])).
fof(f467, plain, ~ (op2(e23, e22) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4675, plain, (~ spl40_148 | ~ spl40_138), inference(avatar_split_clause, [], [f4674, f1428, f1470])).
fof(f4674, plain, (~ (e22 = op2(e24, e21)) | ~ spl40_138), inference(forward_demodulation, [], [f474, f1430])).
fof(f474, plain, ~ (op2(e24, e21) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f4642, plain, (~ spl40_108 | ~ spl40_103), inference(avatar_split_clause, [], [f4641, f1231, f1252])).
fof(f1252, plain, (spl40_108 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_108])])).
fof(f4641, plain, (~ (e12 = op1(e10, e13)) | ~ spl40_103), inference(forward_demodulation, [], [f338, f1233])).
fof(f4630, plain, (~ spl40_91 | ~ spl40_66), inference(avatar_split_clause, [], [f4629, f1076, f1181])).
fof(f1181, plain, (spl40_91 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_91])])).
fof(f4629, plain, (~ (e10 = op1(e11, e11)) | ~ spl40_66), inference(forward_demodulation, [], [f293, f1078])).
fof(f293, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f4620, plain, (~ spl40_90 | ~ spl40_65), inference(avatar_split_clause, [], [f4619, f1071, f1176])).
fof(f1176, plain, (spl40_90 <=> (e14 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_90])])).
fof(f4619, plain, (~ (e14 = op1(e11, e12)) | ~ spl40_65), inference(forward_demodulation, [], [f303, f1073])).
fof(f303, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f4607, plain, (~ spl40_76 | ~ spl40_1), inference(avatar_split_clause, [], [f4606, f803, f1118])).
fof(f1118, plain, (spl40_76 <=> (e10 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_76])])).
fof(f4606, plain, (~ (e10 = op1(e11, e14)) | ~ spl40_1), inference(forward_demodulation, [], [f325, f805])).
fof(f325, plain, ~ (op1(e11, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4594, plain, (~ spl40_75 | ~ spl40_65), inference(avatar_split_clause, [], [f4593, f1071, f1113])).
fof(f1113, plain, (spl40_75 <=> (e14 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_75])])).
fof(f4593, plain, (~ (e14 = op1(e12, e10)) | ~ spl40_65), inference(forward_demodulation, [], [f350, f1073])).
fof(f350, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f4584, plain, (~ spl40_52 | ~ spl40_54), inference(avatar_contradiction_clause, [], [f4583])).
fof(f4583, plain, ($false | (~ spl40_52 | ~ spl40_54)), inference(subsumption_resolution, [], [f4582, f484])).
fof(f4582, plain, ((e11 = e13) | (~ spl40_52 | ~ spl40_54)), inference(forward_demodulation, [], [f1027, f1019])).
fof(f1019, plain, ((e11 = op1(e12, e14)) | ~ spl40_52), inference(avatar_component_clause, [], [f1017])).
fof(f1017, plain, (spl40_52 <=> (e11 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_52])])).
fof(f4577, plain, (~ spl40_48 | ~ spl40_33), inference(avatar_split_clause, [], [f4576, f937, f1000])).
fof(f937, plain, (spl40_33 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_33])])).
fof(f4576, plain, (~ (e12 = op1(e13, e10)) | ~ spl40_33), inference(forward_demodulation, [], [f361, f939])).
fof(f939, plain, ((e12 = op1(e13, e13)) | ~ spl40_33), inference(avatar_component_clause, [], [f937])).
fof(f4539, plain, (~ spl40_18 | ~ spl40_19), inference(avatar_contradiction_clause, [], [f4538])).
fof(f4538, plain, ($false | (~ spl40_18 | ~ spl40_19)), inference(subsumption_resolution, [], [f4537, f486])).
fof(f4537, plain, ((e12 = e13) | (~ spl40_18 | ~ spl40_19)), inference(backward_demodulation, [], [f880, f876])).
fof(f876, plain, ((e12 = op1(e14, e11)) | ~ spl40_18), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl40_18 <=> (e12 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl40_18])])).
fof(f4524, plain, (~ spl40_24 | ~ spl40_25), inference(avatar_contradiction_clause, [], [f4523])).
fof(f4523, plain, ($false | (~ spl40_24 | ~ spl40_25)), inference(subsumption_resolution, [], [f4522, f488])).
fof(f4522, plain, ((e13 = e14) | (~ spl40_24 | ~ spl40_25)), inference(backward_demodulation, [], [f905, f901])).
fof(f4520, plain, (~ spl40_10 | ~ spl40_25), inference(avatar_split_clause, [], [f4516, f903, f840])).
fof(f4516, plain, (~ (e14 = op1(e14, e13)) | ~ spl40_25), inference(backward_demodulation, [], [f371, f905])).
fof(f371, plain, ~ (op1(e14, e10) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f4504, plain, (~ spl40_27 | ~ spl40_37), inference(avatar_split_clause, [], [f4501, f954, f912])).
fof(f954, plain, (spl40_37 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_37])])).
fof(f4501, plain, (~ (e11 = op1(e13, e14)) | ~ spl40_37), inference(backward_demodulation, [], [f367, f956])).
fof(f956, plain, ((e11 = op1(e13, e12)) | ~ spl40_37), inference(avatar_component_clause, [], [f954])).
fof(f367, plain, ~ (op1(e13, e12) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4474, plain, (~ spl40_53 | ~ spl40_54), inference(avatar_contradiction_clause, [], [f4473])).
fof(f4473, plain, ($false | (~ spl40_53 | ~ spl40_54)), inference(subsumption_resolution, [], [f4472, f486])).
fof(f4472, plain, ((e12 = e13) | (~ spl40_53 | ~ spl40_54)), inference(backward_demodulation, [], [f1027, f1023])).
fof(f1023, plain, ((e12 = op1(e12, e14)) | ~ spl40_53), inference(avatar_component_clause, [], [f1021])).
fof(f1021, plain, (spl40_53 <=> (e12 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_53])])).
fof(f4471, plain, (~ spl40_4 | ~ spl40_54), inference(avatar_split_clause, [], [f4469, f1025, f815])).
fof(f815, plain, (spl40_4 <=> (e13 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl40_4])])).
fof(f4469, plain, (~ (e13 = op1(e14, e14)) | ~ spl40_54), inference(backward_demodulation, [], [f327, f1027])).
fof(f327, plain, ~ (op1(e12, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4458, plain, (spl40_27 | ~ spl40_65), inference(avatar_split_clause, [], [f4451, f1071, f912])).
fof(f4451, plain, ((e11 = op1(e13, e14)) | ~ spl40_65), inference(backward_demodulation, [], [f2555, f1073])).
fof(f2555, plain, (e11 = op1(e13, op1(e12, e12))), inference(forward_demodulation, [], [f645, f646])).
fof(f646, plain, (e13 = op1(e12, op1(e12, e12))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((e14 = op1(e12, e12)) & (e13 = op1(e12, op1(e12, e12))) & (e11 = op1(op1(e12, op1(e12, e12)), op1(e12, e12))) & (e10 = op1(op1(e12, e12), op1(e12, e12)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax14)).
fof(f645, plain, (e11 = op1(op1(e12, op1(e12, e12)), op1(e12, e12))), inference(cnf_transformation, [], [f14])).
fof(f4457, plain, (spl40_54 | ~ spl40_65), inference(avatar_split_clause, [], [f4450, f1071, f1025])).
fof(f4450, plain, ((e13 = op1(e12, e14)) | ~ spl40_65), inference(backward_demodulation, [], [f646, f1073])).
fof(f4456, plain, (spl40_1 | ~ spl40_65), inference(avatar_split_clause, [], [f4449, f1071, f803])).
fof(f4449, plain, ((e10 = op1(e14, e14)) | ~ spl40_65), inference(backward_demodulation, [], [f644, f1073])).
fof(f644, plain, (e10 = op1(op1(e12, e12), op1(e12, e12))), inference(cnf_transformation, [], [f14])).
fof(f4454, plain, (~ spl40_60 | ~ spl40_65), inference(avatar_split_clause, [], [f4447, f1071, f1050])).
fof(f1050, plain, (spl40_60 <=> (e14 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_60])])).
fof(f4447, plain, (~ (e14 = op1(e12, e13)) | ~ spl40_65), inference(backward_demodulation, [], [f356, f1073])).
fof(f356, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f4452, plain, (~ spl40_40 | ~ spl40_65), inference(avatar_split_clause, [], [f4445, f1071, f966])).
fof(f966, plain, (spl40_40 <=> (e14 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_40])])).
fof(f4445, plain, (~ (e14 = op1(e13, e12)) | ~ spl40_65), inference(backward_demodulation, [], [f306, f1073])).
fof(f306, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4385, plain, (~ spl40_36 | ~ spl40_86), inference(avatar_split_clause, [], [f4378, f1160, f950])).
fof(f1160, plain, (spl40_86 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl40_86])])).
fof(f4378, plain, (~ (e10 = op1(e13, e12)) | ~ spl40_86), inference(backward_demodulation, [], [f304, f1162])).
fof(f1162, plain, ((e10 = op1(e11, e12)) | ~ spl40_86), inference(avatar_component_clause, [], [f1160])).
fof(f4364, plain, (~ spl40_96 | spl40_286), inference(avatar_contradiction_clause, [], [f4363])).
fof(f4363, plain, ($false | (~ spl40_96 | spl40_286)), inference(subsumption_resolution, [], [f4352, f1204])).
fof(f4352, plain, (~ (e10 = op1(e11, e10)) | (~ spl40_96 | spl40_286)), inference(backward_demodulation, [], [f2114, f1204])).
fof(f2114, plain, (~ (e10 = op1(e11, op1(e11, e10))) | spl40_286), inference(avatar_component_clause, [], [f2112])).
fof(f2112, plain, (spl40_286 <=> (e10 = op1(e11, op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_286])])).
fof(f4353, plain, (~ spl40_71 | ~ spl40_96), inference(avatar_split_clause, [], [f4344, f1202, f1097])).
fof(f1097, plain, (spl40_71 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl40_71])])).
fof(f4344, plain, (~ (e10 = op1(e12, e10)) | ~ spl40_96), inference(backward_demodulation, [], [f283, f1204])).
fof(f283, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f4299, plain, (~ spl40_106 | ~ spl40_116), inference(avatar_split_clause, [], [f4290, f1286, f1244])).
fof(f1244, plain, (spl40_106 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl40_106])])).
fof(f4290, plain, (~ (e10 = op1(e10, e13)) | ~ spl40_116), inference(backward_demodulation, [], [f334, f1288])).
fof(f334, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f4283, plain, (~ spl40_106 | ~ spl40_121), inference(avatar_split_clause, [], [f4273, f1307, f1244])).
fof(f4273, plain, (~ (e10 = op1(e10, e13)) | ~ spl40_121), inference(backward_demodulation, [], [f331, f1309])).
fof(f331, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f4266, plain, (spl40_25 | ~ spl40_126), inference(avatar_split_clause, [], [f4256, f1328, f903])).
fof(f4256, plain, ((e14 = op1(e14, e10)) | ~ spl40_126), inference(backward_demodulation, [], [f141, f1330])).
fof(f4246, plain, (spl40_387 | ~ spl40_131), inference(avatar_split_clause, [], [f4245, f1399, f2704])).
fof(f4245, plain, ((e20 = h5(e14)) | ~ spl40_131), inference(backward_demodulation, [], [f676, f1401])).
fof(f4244, plain, (~ spl40_137 | ~ spl40_138), inference(avatar_contradiction_clause, [], [f4243])).
fof(f4243, plain, ($false | (~ spl40_137 | ~ spl40_138)), inference(subsumption_resolution, [], [f4242, f493])).
fof(f4242, plain, ((e21 = e22) | (~ spl40_137 | ~ spl40_138)), inference(backward_demodulation, [], [f1430, f1426])).
fof(f1426, plain, ((e21 = op2(e24, e23)) | ~ spl40_137), inference(avatar_component_clause, [], [f1424])).
fof(f4200, plain, (~ spl40_140 | ~ spl40_155), inference(avatar_split_clause, [], [f4195, f1499, f1436])).
fof(f1436, plain, (spl40_140 <=> (e24 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_140])])).
fof(f4195, plain, (~ (e24 = op2(e24, e23)) | ~ spl40_155), inference(backward_demodulation, [], [f471, f1501])).
fof(f4198, plain, (~ spl40_150 | ~ spl40_155), inference(avatar_split_clause, [], [f4193, f1499, f1478])).
fof(f4193, plain, (~ (e24 = op2(e24, e21)) | ~ spl40_155), inference(backward_demodulation, [], [f469, f1501])).
fof(f469, plain, ~ (op2(e24, e20) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f4191, plain, (~ spl40_402 | ~ spl40_157), inference(avatar_split_clause, [], [f4189, f1508, f2780])).
fof(f4189, plain, (~ (e21 = h4(e14)) | ~ spl40_157), inference(backward_demodulation, [], [f2607, f1510])).
fof(f2607, plain, ~ (op2(e23, e24) = h4(e14)), inference(backward_demodulation, [], [f468, f671])).
fof(f468, plain, ~ (op2(e23, e23) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4185, plain, (spl40_502 | ~ spl40_165), inference(avatar_split_clause, [], [f4184, f1541, f3340])).
fof(f1541, plain, (spl40_165 <=> (e24 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_165])])).
fof(f4184, plain, ((e24 = h4(e14)) | ~ spl40_165), inference(backward_demodulation, [], [f671, f1543])).
fof(f1543, plain, ((e24 = op2(e23, e23)) | ~ spl40_165), inference(avatar_component_clause, [], [f1541])).
fof(f4150, plain, (~ spl40_183 | ~ spl40_184), inference(avatar_contradiction_clause, [], [f4149])).
fof(f4149, plain, ($false | (~ spl40_183 | ~ spl40_184)), inference(subsumption_resolution, [], [f4148, f496])).
fof(f4148, plain, ((e22 = e23) | (~ spl40_183 | ~ spl40_184)), inference(backward_demodulation, [], [f1623, f1619])).
fof(f1619, plain, ((e22 = op2(e22, e24)) | ~ spl40_183), inference(avatar_component_clause, [], [f1617])).
fof(f1617, plain, (spl40_183 <=> (e22 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_183])])).
fof(f4132, plain, (spl40_517 | ~ spl40_195), inference(avatar_split_clause, [], [f4131, f1667, f3487])).
fof(f4131, plain, ((e24 = h3(e14)) | ~ spl40_195), inference(backward_demodulation, [], [f666, f1669])).
fof(f4090, plain, (~ spl40_160 | ~ spl40_210), inference(avatar_split_clause, [], [f4084, f1730, f1520])).
fof(f1520, plain, (spl40_160 <=> (e24 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl40_160])])).
fof(f4084, plain, (~ (e24 = op2(e23, e24)) | ~ spl40_210), inference(backward_demodulation, [], [f424, f1732])).
fof(f1732, plain, ((e24 = op2(e21, e24)) | ~ spl40_210), inference(avatar_component_clause, [], [f1730])).
fof(f4061, plain, (~ spl40_166 | ~ spl40_216), inference(avatar_split_clause, [], [f4053, f1756, f1546])).
fof(f1756, plain, (spl40_216 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl40_216])])).
fof(f4053, plain, (~ (e20 = op2(e23, e22)) | ~ spl40_216), inference(backward_demodulation, [], [f404, f1758])).
fof(f1758, plain, ((e20 = op2(e21, e22)) | ~ spl40_216), inference(avatar_component_clause, [], [f1756])).
fof(f4048, plain, (~ spl40_226 | spl40_341), inference(avatar_contradiction_clause, [], [f4047])).
fof(f4047, plain, ($false | (~ spl40_226 | spl40_341)), inference(subsumption_resolution, [], [f4035, f1800])).
fof(f1800, plain, ((e20 = op2(e21, e20)) | ~ spl40_226), inference(avatar_component_clause, [], [f1798])).
fof(f4035, plain, (~ (e20 = op2(e21, e20)) | (~ spl40_226 | spl40_341)), inference(backward_demodulation, [], [f2394, f1800])).
fof(f2394, plain, (~ (e20 = op2(e21, op2(e21, e20))) | spl40_341), inference(avatar_component_clause, [], [f2392])).
fof(f2392, plain, (spl40_341 <=> (e20 = op2(e21, op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl40_341])])).
fof(f4040, plain, (~ spl40_151 | ~ spl40_226), inference(avatar_split_clause, [], [f4030, f1798, f1483])).
fof(f1483, plain, (spl40_151 <=> (e20 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl40_151])])).
fof(f4030, plain, (~ (e20 = op2(e24, e20)) | ~ spl40_226), inference(backward_demodulation, [], [f385, f1800])).
fof(f385, plain, ~ (op2(e21, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f4016, plain, (~ spl40_160 | ~ spl40_235), inference(avatar_split_clause, [], [f4011, f1835, f1520])).
fof(f4011, plain, (~ (e24 = op2(e23, e24)) | ~ spl40_235), inference(backward_demodulation, [], [f421, f1837])).
fof(f421, plain, ~ (op2(e20, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4007, plain, (~ spl40_455 | ~ spl40_236), inference(avatar_split_clause, [], [f4001, f1840, f3062])).
fof(f1840, plain, (spl40_236 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl40_236])])).
fof(f4001, plain, (~ (e20 = h1(e14)) | ~ spl40_236), inference(backward_demodulation, [], [f2564, f1842])).
fof(f1842, plain, ((e20 = op2(e20, e23)) | ~ spl40_236), inference(avatar_component_clause, [], [f1840])).
fof(f2564, plain, ~ (op2(e20, e23) = h1(e14)), inference(backward_demodulation, [], [f431, f656])).
fof(f656, plain, (op2(e20, e20) = h1(e14)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e20, e20) = h1(e14)) & (op2(e20, op2(e20, e20)) = h1(e13)) & (h1(e11) = op2(op2(e20, op2(e20, e20)), op2(e20, e20))) & (h1(e10) = op2(op2(e20, e20), op2(e20, e20))) & (e20 = h1(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax16)).
fof(f431, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f8])).
fof(f3962, plain, (spl40_455 | ~ spl40_251), inference(avatar_split_clause, [], [f3961, f1903, f3062])).
fof(f3961, plain, ((e20 = h1(e14)) | ~ spl40_251), inference(backward_demodulation, [], [f656, f1905])).
fof(f1905, plain, ((e20 = op2(e20, e20)) | ~ spl40_251), inference(avatar_component_clause, [], [f1903])).
fof(f3590, plain, (spl40_417 | spl40_415 | spl40_413 | spl40_411 | ~ spl40_517 | ~ spl40_518 | ~ spl40_519 | ~ spl40_520 | ~ spl40_521 | ~ spl40_522 | ~ spl40_523 | ~ spl40_524 | ~ spl40_525 | ~ spl40_526 | ~ spl40_527 | ~ spl40_528 | ~ spl40_529 | ~ spl40_530 | ~ spl40_531 | ~ spl40_532 | ~ spl40_533 | ~ spl40_534 | ~ spl40_535 | ~ spl40_536 | ~ spl40_537 | ~ spl40_538 | ~ spl40_539 | ~ spl40_540 | ~ spl40_541 | ~ spl40_542), inference(avatar_split_clause, [], [f3485, f3587, f3583, f3579, f3575, f3571, f3567, f3563, f3559, f3555, f3551, f3547, f3543, f3539, f3535, f3531, f3527, f3523, f3519, f3515, f3511, f3507, f3503, f3499, f3495, f3491, f3487, f2826, f2840, f2854, f2868])).
fof(f2868, plain, (spl40_417 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl40_417])])).
fof(f2854, plain, (spl40_415 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl40_415])])).
fof(f2840, plain, (spl40_413 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl40_413])])).
fof(f2826, plain, (spl40_411 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl40_411])])).
fof(f3485, plain, (~ (h1(e14) = h3(op1(e10, e10))) | ~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28), inference(forward_demodulation, [], [f3484, f656])).
fof(f3484, plain, (~ (op2(e20, e20) = h3(op1(e10, e10))) | ~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28), inference(forward_demodulation, [], [f3483, f2599])).
fof(f3483, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3482, f2599])).
fof(f3482, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), e21)) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3481, f2597])).
fof(f3481, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3480, f2599])).
fof(f3480, plain, (~ (h3(op1(e10, e12)) = op2(h3(e10), e22)) | ~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3479, f662])).
fof(f3479, plain, (~ (op2(e20, e23) = h3(op1(e10, e13))) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3478, f2599])).
fof(f3478, plain, (~ (h3(op1(e10, e13)) = op2(h3(e10), e23)) | ~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3477, f2594])).
fof(f3477, plain, (~ (h3(op1(e10, e14)) = op2(e20, h3(e14))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3476, f2599])).
fof(f3476, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3475, f2597])).
fof(f3475, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3474, f2599])).
fof(f3474, plain, (~ (h2(e14) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3473, f661])).
fof(f3473, plain, (~ (op2(e21, e21) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3472, f2597])).
fof(f3472, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3471, f2597])).
fof(f3471, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3470, f662])).
fof(f3470, plain, (~ (op2(e21, e23) = h3(op1(e11, e13))) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3469, f2597])).
fof(f3469, plain, (~ (h3(op1(e11, e13)) = op2(h3(e11), e23)) | ~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3468, f2594])).
fof(f3468, plain, (~ (h3(op1(e11, e14)) = op2(e21, h3(e14))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3467, f2597])).
fof(f3467, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3466, f662])).
fof(f3466, plain, (~ (h3(op1(e12, e10)) = op2(h3(e12), e20)) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3465, f2599])).
fof(f3465, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3464, f662])).
fof(f3464, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), e21)) | ~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3463, f2597])).
fof(f3463, plain, (~ (h3(e14) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3462, f666])).
fof(f3462, plain, (~ (op2(e22, e22) = h3(op1(e12, e12))) | ~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3461, f662])).
fof(f3461, plain, (~ (op2(e22, e23) = h3(op1(e12, e13))) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3460, f662])).
fof(f3460, plain, (~ (h3(op1(e12, e13)) = op2(h3(e12), e23)) | ~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3459, f2594])).
fof(f3459, plain, (~ (e23 = h3(op1(e12, e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3458, f2591])).
fof(f3458, plain, (~ (h3(op1(e12, e14)) = op2(e22, h3(e14))) | ~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3457, f662])).
fof(f3457, plain, (~ (op2(e23, e20) = h3(op1(e13, e10))) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3456, f2594])).
fof(f3456, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3455, f2599])).
fof(f3455, plain, (~ (op2(e23, e21) = h3(op1(e13, e11))) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3454, f2594])).
fof(f3454, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3453, f2597])).
fof(f3453, plain, (~ (op2(e23, e22) = h3(op1(e13, e12))) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3452, f2594])).
fof(f3452, plain, (~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3451, f662])).
fof(f3451, plain, (~ (h4(e14) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3450, f671])).
fof(f3450, plain, (~ (op2(e23, e23) = h3(op1(e13, e13))) | ~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3449, f2594])).
fof(f3449, plain, (~ (e21 = h3(op1(e13, e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3448, f2592])).
fof(f3448, plain, (~ (h3(op1(e13, e14)) = op2(e23, h3(e14))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3447, f2594])).
fof(f3447, plain, (~ (h3(op1(e14, e10)) = op2(h3(e14), e20)) | ~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3446, f2599])).
fof(f3446, plain, (~ (h3(op1(e14, e11)) = op2(h3(e14), e21)) | ~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3445, f2597])).
fof(f3445, plain, (~ (h3(op1(e14, e12)) = op2(h3(e14), e22)) | ~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3444, f662])).
fof(f3444, plain, (~ (h3(op1(e14, e13)) = op2(h3(e14), e23)) | ~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f3443, f2594])).
fof(f3443, plain, (~ (e20 = h3(op1(e14, e14))) | ~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f791, f2590])).
fof(f791, plain, (~ (e24 = h3(e14)) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(cnf_transformation, [], [f66])).
fof(f66, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | sP39 | sP38 | sP37 | sP36 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | sP35 | sP34 | sP33 | sP32 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | sP31 | sP30 | sP29 | sP28 | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | sP27 | sP26 | sP25 | sP24 | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | sP23 | sP22 | sP21 | sP20 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f23, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46])).
fof(f46, plain, ((~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP20), inference(usedef, [], [e46])).
fof(e46, plain, (sP20 <=> (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f47, plain, ((~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP21), inference(usedef, [], [e47])).
fof(e47, plain, (sP21 <=> (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f48, plain, ((~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP22), inference(usedef, [], [e48])).
fof(e48, plain, (sP22 <=> (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f49, plain, ((~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | ~ sP23), inference(usedef, [], [e49])).
fof(e49, plain, (sP23 <=> (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f50, plain, ((~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP24), inference(usedef, [], [e50])).
fof(e50, plain, (sP24 <=> (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f51, plain, ((~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP25), inference(usedef, [], [e51])).
fof(e51, plain, (sP25 <=> (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f52, plain, ((~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP26), inference(usedef, [], [e52])).
fof(e52, plain, (sP26 <=> (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f53, plain, ((~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | ~ sP27), inference(usedef, [], [e53])).
fof(e53, plain, (sP27 <=> (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f54, plain, ((~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP28), inference(usedef, [], [e54])).
fof(e54, plain, (sP28 <=> (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f55, plain, ((~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP29), inference(usedef, [], [e55])).
fof(e55, plain, (sP29 <=> (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f56, plain, ((~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP30), inference(usedef, [], [e56])).
fof(e56, plain, (sP30 <=> (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f57, plain, ((~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | ~ sP31), inference(usedef, [], [e57])).
fof(e57, plain, (sP31 <=> (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f58, plain, ((~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP32), inference(usedef, [], [e58])).
fof(e58, plain, (sP32 <=> (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f59, plain, ((~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP33), inference(usedef, [], [e59])).
fof(e59, plain, (sP33 <=> (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f60, plain, ((~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP34), inference(usedef, [], [e60])).
fof(e60, plain, (sP34 <=> (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f61, plain, ((~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | ~ sP35), inference(usedef, [], [e61])).
fof(e61, plain, (sP35 <=> (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f62, plain, ((~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ sP36), inference(usedef, [], [e62])).
fof(e62, plain, (sP36 <=> (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f63, plain, ((~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | ~ sP37), inference(usedef, [], [e63])).
fof(e63, plain, (sP37 <=> (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f64, plain, ((~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | ~ sP38), inference(usedef, [], [e64])).
fof(e64, plain, (sP38 <=> (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f65, plain, ((~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | ~ sP39), inference(usedef, [], [e65])).
fof(e65, plain, (sP39 <=> (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f23, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f21])).
fof(f21, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', co1)).
fof(f2880, plain, ~ spl40_417, inference(avatar_split_clause, [], [f2879, f2868])).
fof(f2879, plain, ~ sP28, inference(subsumption_resolution, [], [f732, f2599])).
fof(f732, plain, (~ (e20 = h3(e10)) | ~ sP28), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP28), inference(nnf_transformation, [], [f54])).
fof(f2865, plain, ~ spl40_415, inference(avatar_split_clause, [], [f2864, f2854])).
fof(f2864, plain, ~ sP29, inference(subsumption_resolution, [], [f728, f2597])).
fof(f728, plain, (~ (e21 = h3(e11)) | ~ sP29), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP29), inference(nnf_transformation, [], [f55])).
fof(f2850, plain, ~ spl40_413, inference(avatar_split_clause, [], [f2849, f2840])).
fof(f2849, plain, ~ sP30, inference(subsumption_resolution, [], [f724, f662])).
fof(f724, plain, (~ (e22 = h3(e12)) | ~ sP30), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ((~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP30), inference(nnf_transformation, [], [f56])).
fof(f2835, plain, ~ spl40_411, inference(avatar_split_clause, [], [f2834, f2826])).
fof(f2834, plain, ~ sP31, inference(subsumption_resolution, [], [f720, f2594])).
fof(f720, plain, (~ (e23 = h3(e13)) | ~ sP31), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | ~ sP31), inference(nnf_transformation, [], [f57])).
fof(f2556, plain, spl40_195, inference(avatar_split_clause, [], [f651, f1667])).
fof(f651, plain, (e24 = op2(e22, e22)), inference(cnf_transformation, [], [f15])).
fof(f2554, plain, spl40_65, inference(avatar_split_clause, [], [f647, f1071])).
fof(f647, plain, (e14 = op1(e12, e12)), inference(cnf_transformation, [], [f14])).
fof(f2544, plain, (spl40_342 | spl40_339 | spl40_365 | ~ spl40_369 | spl40_367), inference(avatar_split_clause, [], [f609, f2531, f2541, f2523, f2383, f2397])).
fof(f2397, plain, (spl40_342 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl40_342])])).
fof(f2383, plain, (spl40_339 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl40_339])])).
fof(f609, plain, ((e24 = op2(e20, op2(e24, e20))) | ~ (e20 = op2(e23, op2(e23, e20))) | (e22 = op2(e20, op2(e22, e20))) | sP11 | sP10), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((((e24 = op2(e24, op2(e24, e24))) & ~ (e24 = op2(e24, op2(e24, e24)))) | ((e23 = op2(e24, op2(e23, e24))) & ~ (e24 = op2(e23, op2(e23, e24)))) | ((e22 = op2(e24, op2(e22, e24))) & ~ (e24 = op2(e22, op2(e22, e24)))) | sP19 | sP18) & (((e24 = op2(e23, op2(e24, e23))) & ~ (e23 = op2(e24, op2(e24, e23)))) | ((e23 = op2(e23, op2(e23, e23))) & ~ (e23 = op2(e23, op2(e23, e23)))) | ((e22 = op2(e23, op2(e22, e23))) & ~ (e23 = op2(e22, op2(e22, e23)))) | sP17 | sP16) & (((e24 = op2(e22, op2(e24, e22))) & ~ (e22 = op2(e24, op2(e24, e22)))) | ((e23 = op2(e22, op2(e23, e22))) & ~ (e22 = op2(e23, op2(e23, e22)))) | ((e22 = op2(e22, op2(e22, e22))) & ~ (e22 = op2(e22, op2(e22, e22)))) | sP15 | sP14) & (((e24 = op2(e21, op2(e24, e21))) & ~ (e21 = op2(e24, op2(e24, e21)))) | ((e23 = op2(e21, op2(e23, e21))) & ~ (e21 = op2(e23, op2(e23, e21)))) | ((e22 = op2(e21, op2(e22, e21))) & ~ (e21 = op2(e22, op2(e22, e21)))) | sP13 | sP12) & (((e24 = op2(e20, op2(e24, e20))) & ~ (e20 = op2(e24, op2(e24, e20)))) | ((e23 = op2(e20, op2(e23, e20))) & ~ (e20 = op2(e23, op2(e23, e20)))) | ((e22 = op2(e20, op2(e22, e20))) & ~ (e20 = op2(e22, op2(e22, e20)))) | sP11 | sP10)), inference(definition_folding, [], [f13, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35])).
fof(f35, plain, (((e20 = op2(e20, op2(e20, e20))) & ~ (e20 = op2(e20, op2(e20, e20)))) | ~ sP10), inference(usedef, [], [e35])).
fof(e35, plain, (sP10 <=> ((e20 = op2(e20, op2(e20, e20))) & ~ (e20 = op2(e20, op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f36, plain, (((e21 = op2(e20, op2(e21, e20))) & ~ (e20 = op2(e21, op2(e21, e20)))) | ~ sP11), inference(usedef, [], [e36])).
fof(e36, plain, (sP11 <=> ((e21 = op2(e20, op2(e21, e20))) & ~ (e20 = op2(e21, op2(e21, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f37, plain, (((e20 = op2(e21, op2(e20, e21))) & ~ (e21 = op2(e20, op2(e20, e21)))) | ~ sP12), inference(usedef, [], [e37])).
fof(e37, plain, (sP12 <=> ((e20 = op2(e21, op2(e20, e21))) & ~ (e21 = op2(e20, op2(e20, e21))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f38, plain, (((e21 = op2(e21, op2(e21, e21))) & ~ (e21 = op2(e21, op2(e21, e21)))) | ~ sP13), inference(usedef, [], [e38])).
fof(e38, plain, (sP13 <=> ((e21 = op2(e21, op2(e21, e21))) & ~ (e21 = op2(e21, op2(e21, e21))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f39, plain, (((e20 = op2(e22, op2(e20, e22))) & ~ (e22 = op2(e20, op2(e20, e22)))) | ~ sP14), inference(usedef, [], [e39])).
fof(e39, plain, (sP14 <=> ((e20 = op2(e22, op2(e20, e22))) & ~ (e22 = op2(e20, op2(e20, e22))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f40, plain, (((e21 = op2(e22, op2(e21, e22))) & ~ (e22 = op2(e21, op2(e21, e22)))) | ~ sP15), inference(usedef, [], [e40])).
fof(e40, plain, (sP15 <=> ((e21 = op2(e22, op2(e21, e22))) & ~ (e22 = op2(e21, op2(e21, e22))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f41, plain, (((e20 = op2(e23, op2(e20, e23))) & ~ (e23 = op2(e20, op2(e20, e23)))) | ~ sP16), inference(usedef, [], [e41])).
fof(e41, plain, (sP16 <=> ((e20 = op2(e23, op2(e20, e23))) & ~ (e23 = op2(e20, op2(e20, e23))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f42, plain, (((e21 = op2(e23, op2(e21, e23))) & ~ (e23 = op2(e21, op2(e21, e23)))) | ~ sP17), inference(usedef, [], [e42])).
fof(e42, plain, (sP17 <=> ((e21 = op2(e23, op2(e21, e23))) & ~ (e23 = op2(e21, op2(e21, e23))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f43, plain, (((e20 = op2(e24, op2(e20, e24))) & ~ (e24 = op2(e20, op2(e20, e24)))) | ~ sP18), inference(usedef, [], [e43])).
fof(e43, plain, (sP18 <=> ((e20 = op2(e24, op2(e20, e24))) & ~ (e24 = op2(e20, op2(e20, e24))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f44, plain, (((e21 = op2(e24, op2(e21, e24))) & ~ (e24 = op2(e21, op2(e21, e24)))) | ~ sP19), inference(usedef, [], [e44])).
fof(e44, plain, (sP19 <=> ((e21 = op2(e24, op2(e21, e24))) & ~ (e24 = op2(e21, op2(e21, e24))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f13, plain, ((((e24 = op2(e24, op2(e24, e24))) & ~ (e24 = op2(e24, op2(e24, e24)))) | ((e23 = op2(e24, op2(e23, e24))) & ~ (e24 = op2(e23, op2(e23, e24)))) | ((e22 = op2(e24, op2(e22, e24))) & ~ (e24 = op2(e22, op2(e22, e24)))) | ((e21 = op2(e24, op2(e21, e24))) & ~ (e24 = op2(e21, op2(e21, e24)))) | ((e20 = op2(e24, op2(e20, e24))) & ~ (e24 = op2(e20, op2(e20, e24))))) & (((e24 = op2(e23, op2(e24, e23))) & ~ (e23 = op2(e24, op2(e24, e23)))) | ((e23 = op2(e23, op2(e23, e23))) & ~ (e23 = op2(e23, op2(e23, e23)))) | ((e22 = op2(e23, op2(e22, e23))) & ~ (e23 = op2(e22, op2(e22, e23)))) | ((e21 = op2(e23, op2(e21, e23))) & ~ (e23 = op2(e21, op2(e21, e23)))) | ((e20 = op2(e23, op2(e20, e23))) & ~ (e23 = op2(e20, op2(e20, e23))))) & (((e24 = op2(e22, op2(e24, e22))) & ~ (e22 = op2(e24, op2(e24, e22)))) | ((e23 = op2(e22, op2(e23, e22))) & ~ (e22 = op2(e23, op2(e23, e22)))) | ((e22 = op2(e22, op2(e22, e22))) & ~ (e22 = op2(e22, op2(e22, e22)))) | ((e21 = op2(e22, op2(e21, e22))) & ~ (e22 = op2(e21, op2(e21, e22)))) | ((e20 = op2(e22, op2(e20, e22))) & ~ (e22 = op2(e20, op2(e20, e22))))) & (((e24 = op2(e21, op2(e24, e21))) & ~ (e21 = op2(e24, op2(e24, e21)))) | ((e23 = op2(e21, op2(e23, e21))) & ~ (e21 = op2(e23, op2(e23, e21)))) | ((e22 = op2(e21, op2(e22, e21))) & ~ (e21 = op2(e22, op2(e22, e21)))) | ((e21 = op2(e21, op2(e21, e21))) & ~ (e21 = op2(e21, op2(e21, e21)))) | ((e20 = op2(e21, op2(e20, e21))) & ~ (e21 = op2(e20, op2(e20, e21))))) & (((e24 = op2(e20, op2(e24, e20))) & ~ (e20 = op2(e24, op2(e24, e20)))) | ((e23 = op2(e20, op2(e23, e20))) & ~ (e20 = op2(e23, op2(e23, e20)))) | ((e22 = op2(e20, op2(e22, e20))) & ~ (e20 = op2(e22, op2(e22, e20)))) | ((e21 = op2(e20, op2(e21, e20))) & ~ (e20 = op2(e21, op2(e21, e20)))) | ((e20 = op2(e20, op2(e20, e20))) & ~ (e20 = op2(e20, op2(e20, e20)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax13)).
fof(f2534, plain, (spl40_342 | spl40_339 | spl40_365 | spl40_366 | spl40_367), inference(avatar_split_clause, [], [f611, f2531, f2527, f2523, f2383, f2397])).
fof(f611, plain, ((e24 = op2(e20, op2(e24, e20))) | (e23 = op2(e20, op2(e23, e20))) | (e22 = op2(e20, op2(e22, e20))) | sP11 | sP10), inference(cnf_transformation, [], [f45])).
fof(f2405, plain, (~ spl40_342 | ~ spl40_343), inference(avatar_split_clause, [], [f602, f2401, f2397])).
fof(f2401, plain, (spl40_343 <=> (e20 = op2(e20, op2(e20, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl40_343])])).
fof(f602, plain, (~ (e20 = op2(e20, op2(e20, e20))) | ~ sP10), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (((e20 = op2(e20, op2(e20, e20))) & ~ (e20 = op2(e20, op2(e20, e20)))) | ~ sP10), inference(nnf_transformation, [], [f35])).
fof(f2404, plain, (~ spl40_342 | spl40_343), inference(avatar_split_clause, [], [f603, f2401, f2397])).
fof(f603, plain, ((e20 = op2(e20, op2(e20, e20))) | ~ sP10), inference(cnf_transformation, [], [f86])).
fof(f2395, plain, (~ spl40_339 | ~ spl40_341), inference(avatar_split_clause, [], [f600, f2392, f2383])).
fof(f600, plain, (~ (e20 = op2(e21, op2(e21, e20))) | ~ sP11), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (((e21 = op2(e20, op2(e21, e20))) & ~ (e20 = op2(e21, op2(e21, e20)))) | ~ sP11), inference(nnf_transformation, [], [f36])).
fof(f2264, plain, (spl40_287 | spl40_284 | spl40_310 | ~ spl40_314 | spl40_312), inference(avatar_split_clause, [], [f549, f2251, f2261, f2243, f2103, f2117])).
fof(f2117, plain, (spl40_287 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl40_287])])).
fof(f2103, plain, (spl40_284 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl40_284])])).
fof(f549, plain, ((e14 = op1(e10, op1(e14, e10))) | ~ (e10 = op1(e13, op1(e13, e10))) | (e12 = op1(e10, op1(e12, e10))) | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ((((e14 = op1(e14, op1(e14, e14))) & ~ (e14 = op1(e14, op1(e14, e14)))) | ((e13 = op1(e14, op1(e13, e14))) & ~ (e14 = op1(e13, op1(e13, e14)))) | ((e12 = op1(e14, op1(e12, e14))) & ~ (e14 = op1(e12, op1(e12, e14)))) | sP9 | sP8) & (((e14 = op1(e13, op1(e14, e13))) & ~ (e13 = op1(e14, op1(e14, e13)))) | ((e13 = op1(e13, op1(e13, e13))) & ~ (e13 = op1(e13, op1(e13, e13)))) | ((e12 = op1(e13, op1(e12, e13))) & ~ (e13 = op1(e12, op1(e12, e13)))) | sP7 | sP6) & (((e14 = op1(e12, op1(e14, e12))) & ~ (e12 = op1(e14, op1(e14, e12)))) | ((e13 = op1(e12, op1(e13, e12))) & ~ (e12 = op1(e13, op1(e13, e12)))) | ((e12 = op1(e12, op1(e12, e12))) & ~ (e12 = op1(e12, op1(e12, e12)))) | sP5 | sP4) & (((e14 = op1(e11, op1(e14, e11))) & ~ (e11 = op1(e14, op1(e14, e11)))) | ((e13 = op1(e11, op1(e13, e11))) & ~ (e11 = op1(e13, op1(e13, e11)))) | ((e12 = op1(e11, op1(e12, e11))) & ~ (e11 = op1(e12, op1(e12, e11)))) | sP3 | sP2) & (((e14 = op1(e10, op1(e14, e10))) & ~ (e10 = op1(e14, op1(e14, e10)))) | ((e13 = op1(e10, op1(e13, e10))) & ~ (e10 = op1(e13, op1(e13, e10)))) | ((e12 = op1(e10, op1(e12, e10))) & ~ (e10 = op1(e12, op1(e12, e10)))) | sP1 | sP0)), inference(definition_folding, [], [f12, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24])).
fof(f24, plain, (((e10 = op1(e10, op1(e10, e10))) & ~ (e10 = op1(e10, op1(e10, e10)))) | ~ sP0), inference(usedef, [], [e24])).
fof(e24, plain, (sP0 <=> ((e10 = op1(e10, op1(e10, e10))) & ~ (e10 = op1(e10, op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f25, plain, (((e11 = op1(e10, op1(e11, e10))) & ~ (e10 = op1(e11, op1(e11, e10)))) | ~ sP1), inference(usedef, [], [e25])).
fof(e25, plain, (sP1 <=> ((e11 = op1(e10, op1(e11, e10))) & ~ (e10 = op1(e11, op1(e11, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f26, plain, (((e10 = op1(e11, op1(e10, e11))) & ~ (e11 = op1(e10, op1(e10, e11)))) | ~ sP2), inference(usedef, [], [e26])).
fof(e26, plain, (sP2 <=> ((e10 = op1(e11, op1(e10, e11))) & ~ (e11 = op1(e10, op1(e10, e11))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f27, plain, (((e11 = op1(e11, op1(e11, e11))) & ~ (e11 = op1(e11, op1(e11, e11)))) | ~ sP3), inference(usedef, [], [e27])).
fof(e27, plain, (sP3 <=> ((e11 = op1(e11, op1(e11, e11))) & ~ (e11 = op1(e11, op1(e11, e11))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f28, plain, (((e10 = op1(e12, op1(e10, e12))) & ~ (e12 = op1(e10, op1(e10, e12)))) | ~ sP4), inference(usedef, [], [e28])).
fof(e28, plain, (sP4 <=> ((e10 = op1(e12, op1(e10, e12))) & ~ (e12 = op1(e10, op1(e10, e12))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f29, plain, (((e11 = op1(e12, op1(e11, e12))) & ~ (e12 = op1(e11, op1(e11, e12)))) | ~ sP5), inference(usedef, [], [e29])).
fof(e29, plain, (sP5 <=> ((e11 = op1(e12, op1(e11, e12))) & ~ (e12 = op1(e11, op1(e11, e12))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f30, plain, (((e10 = op1(e13, op1(e10, e13))) & ~ (e13 = op1(e10, op1(e10, e13)))) | ~ sP6), inference(usedef, [], [e30])).
fof(e30, plain, (sP6 <=> ((e10 = op1(e13, op1(e10, e13))) & ~ (e13 = op1(e10, op1(e10, e13))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f31, plain, (((e11 = op1(e13, op1(e11, e13))) & ~ (e13 = op1(e11, op1(e11, e13)))) | ~ sP7), inference(usedef, [], [e31])).
fof(e31, plain, (sP7 <=> ((e11 = op1(e13, op1(e11, e13))) & ~ (e13 = op1(e11, op1(e11, e13))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f32, plain, (((e10 = op1(e14, op1(e10, e14))) & ~ (e14 = op1(e10, op1(e10, e14)))) | ~ sP8), inference(usedef, [], [e32])).
fof(e32, plain, (sP8 <=> ((e10 = op1(e14, op1(e10, e14))) & ~ (e14 = op1(e10, op1(e10, e14))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f33, plain, (((e11 = op1(e14, op1(e11, e14))) & ~ (e14 = op1(e11, op1(e11, e14)))) | ~ sP9), inference(usedef, [], [e33])).
fof(e33, plain, (sP9 <=> ((e11 = op1(e14, op1(e11, e14))) & ~ (e14 = op1(e11, op1(e11, e14))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f12, plain, ((((e14 = op1(e14, op1(e14, e14))) & ~ (e14 = op1(e14, op1(e14, e14)))) | ((e13 = op1(e14, op1(e13, e14))) & ~ (e14 = op1(e13, op1(e13, e14)))) | ((e12 = op1(e14, op1(e12, e14))) & ~ (e14 = op1(e12, op1(e12, e14)))) | ((e11 = op1(e14, op1(e11, e14))) & ~ (e14 = op1(e11, op1(e11, e14)))) | ((e10 = op1(e14, op1(e10, e14))) & ~ (e14 = op1(e10, op1(e10, e14))))) & (((e14 = op1(e13, op1(e14, e13))) & ~ (e13 = op1(e14, op1(e14, e13)))) | ((e13 = op1(e13, op1(e13, e13))) & ~ (e13 = op1(e13, op1(e13, e13)))) | ((e12 = op1(e13, op1(e12, e13))) & ~ (e13 = op1(e12, op1(e12, e13)))) | ((e11 = op1(e13, op1(e11, e13))) & ~ (e13 = op1(e11, op1(e11, e13)))) | ((e10 = op1(e13, op1(e10, e13))) & ~ (e13 = op1(e10, op1(e10, e13))))) & (((e14 = op1(e12, op1(e14, e12))) & ~ (e12 = op1(e14, op1(e14, e12)))) | ((e13 = op1(e12, op1(e13, e12))) & ~ (e12 = op1(e13, op1(e13, e12)))) | ((e12 = op1(e12, op1(e12, e12))) & ~ (e12 = op1(e12, op1(e12, e12)))) | ((e11 = op1(e12, op1(e11, e12))) & ~ (e12 = op1(e11, op1(e11, e12)))) | ((e10 = op1(e12, op1(e10, e12))) & ~ (e12 = op1(e10, op1(e10, e12))))) & (((e14 = op1(e11, op1(e14, e11))) & ~ (e11 = op1(e14, op1(e14, e11)))) | ((e13 = op1(e11, op1(e13, e11))) & ~ (e11 = op1(e13, op1(e13, e11)))) | ((e12 = op1(e11, op1(e12, e11))) & ~ (e11 = op1(e12, op1(e12, e11)))) | ((e11 = op1(e11, op1(e11, e11))) & ~ (e11 = op1(e11, op1(e11, e11)))) | ((e10 = op1(e11, op1(e10, e11))) & ~ (e11 = op1(e10, op1(e10, e11))))) & (((e14 = op1(e10, op1(e14, e10))) & ~ (e10 = op1(e14, op1(e14, e10)))) | ((e13 = op1(e10, op1(e13, e10))) & ~ (e10 = op1(e13, op1(e13, e10)))) | ((e12 = op1(e10, op1(e12, e10))) & ~ (e10 = op1(e12, op1(e12, e10)))) | ((e11 = op1(e10, op1(e11, e10))) & ~ (e10 = op1(e11, op1(e11, e10)))) | ((e10 = op1(e10, op1(e10, e10))) & ~ (e10 = op1(e10, op1(e10, e10)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax12)).
fof(f2254, plain, (spl40_287 | spl40_284 | spl40_310 | spl40_311 | spl40_312), inference(avatar_split_clause, [], [f551, f2251, f2247, f2243, f2103, f2117])).
fof(f551, plain, ((e14 = op1(e10, op1(e14, e10))) | (e13 = op1(e10, op1(e13, e10))) | (e12 = op1(e10, op1(e12, e10))) | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f2172, plain, (spl40_270 | spl40_267 | spl40_294 | ~ spl40_295 | spl40_296), inference(avatar_split_clause, [], [f573, f2163, f2159, f2155, f2023, f2037])).
fof(f2037, plain, (spl40_270 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl40_270])])).
fof(f2023, plain, (spl40_267 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl40_267])])).
fof(f2159, plain, (spl40_295 <=> (e13 = op1(e13, op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl40_295])])).
fof(f573, plain, ((e14 = op1(e13, op1(e14, e13))) | ~ (e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e13, op1(e12, e13))) | sP7 | sP6), inference(cnf_transformation, [], [f34])).
fof(f2166, plain, (spl40_270 | spl40_267 | spl40_294 | spl40_295 | spl40_296), inference(avatar_split_clause, [], [f575, f2163, f2159, f2155, f2023, f2037])).
fof(f575, plain, ((e14 = op1(e13, op1(e14, e13))) | (e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e13, op1(e12, e13))) | sP7 | sP6), inference(cnf_transformation, [], [f34])).
fof(f2125, plain, (~ spl40_287 | ~ spl40_288), inference(avatar_split_clause, [], [f542, f2121, f2117])).
fof(f2121, plain, (spl40_288 <=> (e10 = op1(e10, op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl40_288])])).
fof(f542, plain, (~ (e10 = op1(e10, op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (((e10 = op1(e10, op1(e10, e10))) & ~ (e10 = op1(e10, op1(e10, e10)))) | ~ sP0), inference(nnf_transformation, [], [f24])).
fof(f2124, plain, (~ spl40_287 | spl40_288), inference(avatar_split_clause, [], [f543, f2121, f2117])).
fof(f543, plain, ((e10 = op1(e10, op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f76])).
fof(f2115, plain, (~ spl40_284 | ~ spl40_286), inference(avatar_split_clause, [], [f540, f2112, f2103])).
fof(f540, plain, (~ (e10 = op1(e11, op1(e11, e10))) | ~ sP1), inference(cnf_transformation, [], [f75])).
fof(f75, plain, (((e11 = op1(e10, op1(e11, e10))) & ~ (e10 = op1(e11, op1(e11, e10)))) | ~ sP1), inference(nnf_transformation, [], [f25])).
fof(f2044, plain, (~ spl40_270 | spl40_271), inference(avatar_split_clause, [], [f531, f2041, f2037])).
fof(f531, plain, ((e10 = op1(e13, op1(e10, e13))) | ~ sP6), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (((e10 = op1(e13, op1(e10, e13))) & ~ (e13 = op1(e10, op1(e10, e13)))) | ~ sP6), inference(nnf_transformation, [], [f30])).
fof(f2035, plain, (~ spl40_267 | ~ spl40_269), inference(avatar_split_clause, [], [f528, f2032, f2023])).
fof(f528, plain, (~ (e13 = op1(e11, op1(e11, e13))) | ~ sP7), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((e11 = op1(e13, op1(e11, e13))) & ~ (e13 = op1(e11, op1(e11, e13)))) | ~ sP7), inference(nnf_transformation, [], [f31])).
fof(f1974, plain, (spl40_250 | spl40_225 | spl40_200 | spl40_175 | spl40_150), inference(avatar_split_clause, [], [f248, f1478, f1583, f1688, f1793, f1898])).
fof(f248, plain, ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (((e24 = op2(e24, e24)) | (e24 = op2(e23, e24)) | (e24 = op2(e22, e24)) | (e24 = op2(e21, e24)) | (e24 = op2(e20, e24))) & ((e24 = op2(e24, e24)) | (e24 = op2(e24, e23)) | (e24 = op2(e24, e22)) | (e24 = op2(e24, e21)) | (e24 = op2(e24, e20))) & ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))) & ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))) & ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))) & ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))) & ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))) & ((e21 = op2(e24, e24)) | (e21 = op2(e24, e23)) | (e21 = op2(e24, e22)) | (e21 = op2(e24, e21)) | (e21 = op2(e24, e20))) & ((e20 = op2(e24, e24)) | (e20 = op2(e23, e24)) | (e20 = op2(e22, e24)) | (e20 = op2(e21, e24)) | (e20 = op2(e20, e24))) & ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))) & ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))) & ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))) & ((e23 = op2(e24, e23)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e24, e23)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e24 = op2(e24, e22)) | (e24 = op2(e23, e22)) | (e24 = op2(e22, e22)) | (e24 = op2(e21, e22)) | (e24 = op2(e20, e22))) & ((e24 = op2(e22, e24)) | (e24 = op2(e22, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e22, e21)) | (e24 = op2(e22, e20))) & ((e23 = op2(e24, e22)) | (e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e24)) | (e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e24, e22)) | (e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e24)) | (e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e24)) | (e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e24, e22)) | (e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))) & ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))) & ((e23 = op2(e24, e21)) | (e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e24)) | (e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e24)) | (e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e24, e21)) | (e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e24, e21)) | (e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e24)) | (e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e24 = op2(e24, e20)) | (e24 = op2(e23, e20)) | (e24 = op2(e22, e20)) | (e24 = op2(e21, e20)) | (op2(e20, e20) = e24)) & ((e24 = op2(e20, e24)) | (e24 = op2(e20, e23)) | (e24 = op2(e20, e22)) | (e24 = op2(e20, e21)) | (op2(e20, e20) = e24)) & ((e23 = op2(e24, e20)) | (e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e24)) | (e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e24, e20)) | (e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e24)) | (e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e24, e20)) | (e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e24)) | (e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax6)).
fof(f1973, plain, (spl40_201 | spl40_196 | spl40_191 | spl40_186 | spl40_181), inference(avatar_split_clause, [], [f249, f1609, f1630, f1651, f1672, f1693])).
fof(f249, plain, ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f6])).
fof(f1971, plain, (spl40_202 | spl40_197 | spl40_192 | spl40_187 | spl40_182), inference(avatar_split_clause, [], [f251, f1613, f1634, f1655, f1676, f1697])).
fof(f251, plain, ((e21 = op2(e22, e24)) | (e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f6])).
fof(f1970, plain, (spl40_242 | spl40_217 | spl40_192 | spl40_167 | spl40_142), inference(avatar_split_clause, [], [f252, f1445, f1550, f1655, f1760, f1865])).
fof(f252, plain, ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f6])).
fof(f1962, plain, (spl40_236 | spl40_211 | spl40_186 | spl40_161 | spl40_136), inference(avatar_split_clause, [], [f260, f1420, f1525, f1630, f1735, f1840])).
fof(f260, plain, ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f1960, plain, (spl40_237 | spl40_212 | spl40_187 | spl40_162 | spl40_137), inference(avatar_split_clause, [], [f262, f1424, f1529, f1634, f1739, f1844])).
fof(f262, plain, ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f1959, plain, (spl40_178 | spl40_173 | spl40_168 | spl40_163 | spl40_158), inference(avatar_split_clause, [], [f263, f1512, f1533, f1554, f1575, f1596])).
fof(f263, plain, ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f1958, plain, (spl40_238 | spl40_213 | spl40_188 | spl40_163 | spl40_138), inference(avatar_split_clause, [], [f264, f1428, f1533, f1638, f1743, f1848])).
fof(f264, plain, ((e22 = op2(e24, e23)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f1955, plain, (spl40_180 | spl40_175 | spl40_170 | spl40_165 | spl40_160), inference(avatar_split_clause, [], [f267, f1520, f1541, f1562, f1583, f1604])).
fof(f267, plain, ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f1954, plain, (spl40_240 | spl40_215 | spl40_190 | spl40_165 | spl40_140), inference(avatar_split_clause, [], [f268, f1436, f1541, f1646, f1751, f1856])).
fof(f268, plain, ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f1949, plain, (spl40_153 | spl40_148 | spl40_143 | spl40_138 | spl40_133), inference(avatar_split_clause, [], [f273, f1407, f1428, f1449, f1470, f1491])).
fof(f273, plain, ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))), inference(cnf_transformation, [], [f6])).
fof(f1948, plain, (spl40_233 | spl40_208 | spl40_183 | spl40_158 | spl40_133), inference(avatar_split_clause, [], [f274, f1407, f1512, f1617, f1722, f1827])).
fof(f274, plain, ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))), inference(cnf_transformation, [], [f6])).
fof(f1943, plain, (spl40_256 | spl40_257 | spl40_258 | spl40_259 | spl40_260), inference(avatar_split_clause, [], [f228, f1940, f1936, f1932, f1928, f1924])).
fof(f228, plain, ((e24 = unit2) | (e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)), inference(cnf_transformation, [], [f5])).
fof(f1775, plain, (spl40_216 | spl40_217 | spl40_218 | spl40_219 | spl40_220), inference(avatar_split_clause, [], [f200, f1772, f1768, f1764, f1760, f1756])).
fof(f200, plain, ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e24 = op2(e24, e24)) | (e23 = op2(e24, e24)) | (e22 = op2(e24, e24)) | (e21 = op2(e24, e24)) | (e20 = op2(e24, e24))) & ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))) & ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))) & ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))) & ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))) & ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))) & ((e24 = op2(e23, e23)) | (e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e24 = op2(e23, e22)) | (e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))) & ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))) & ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e24 = op2(e21, e21)) | (e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e24 = op2(e21, e20)) | (e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e24 = op2(e20, e24)) | (e23 = op2(e20, e24)) | (e22 = op2(e20, e24)) | (e21 = op2(e20, e24)) | (e20 = op2(e20, e24))) & ((e24 = op2(e20, e23)) | (e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e24 = op2(e20, e22)) | (e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e24 = op2(e20, e21)) | (e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e24) | (op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax4)).
fof(f1733, plain, (spl40_206 | spl40_207 | spl40_208 | spl40_209 | spl40_210), inference(avatar_split_clause, [], [f202, f1730, f1726, f1722, f1718, f1714])).
fof(f202, plain, ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))), inference(cnf_transformation, [], [f4])).
fof(f1712, plain, (spl40_201 | spl40_202 | spl40_203 | spl40_204 | spl40_205), inference(avatar_split_clause, [], [f203, f1709, f1705, f1701, f1697, f1693])).
fof(f203, plain, ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1691, plain, (spl40_196 | spl40_197 | spl40_198 | spl40_199 | spl40_200), inference(avatar_split_clause, [], [f204, f1688, f1684, f1680, f1676, f1672])).
fof(f204, plain, ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f4])).
fof(f1565, plain, (spl40_166 | spl40_167 | spl40_168 | spl40_169 | spl40_170), inference(avatar_split_clause, [], [f210, f1562, f1558, f1554, f1550, f1546])).
fof(f210, plain, ((e24 = op2(e23, e22)) | (e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f4])).
fof(f1502, plain, (spl40_151 | spl40_152 | spl40_153 | spl40_154 | spl40_155), inference(avatar_split_clause, [], [f213, f1499, f1495, f1491, f1487, f1483])).
fof(f213, plain, ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))), inference(cnf_transformation, [], [f4])).
fof(f1481, plain, (spl40_146 | spl40_147 | spl40_148 | spl40_149 | spl40_150), inference(avatar_split_clause, [], [f214, f1478, f1474, f1470, f1466, f1462])).
fof(f214, plain, ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))), inference(cnf_transformation, [], [f4])).
fof(f1439, plain, (spl40_136 | spl40_137 | spl40_138 | spl40_139 | spl40_140), inference(avatar_split_clause, [], [f216, f1436, f1432, f1428, f1424, f1420])).
fof(f216, plain, ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))), inference(cnf_transformation, [], [f4])).
fof(f1395, plain, (spl40_122 | spl40_117 | spl40_112 | spl40_107 | spl40_102), inference(avatar_split_clause, [], [f145, f1227, f1248, f1269, f1290, f1311])).
fof(f145, plain, ((e11 = op1(e10, e14)) | (e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e14 = op1(e14, e14)) | (e14 = op1(e13, e14)) | (e14 = op1(e12, e14)) | (e14 = op1(e11, e14)) | (e14 = op1(e10, e14))) & ((e14 = op1(e14, e14)) | (e14 = op1(e14, e13)) | (e14 = op1(e14, e12)) | (e14 = op1(e14, e11)) | (e14 = op1(e14, e10))) & ((e13 = op1(e14, e14)) | (e13 = op1(e13, e14)) | (e13 = op1(e12, e14)) | (e13 = op1(e11, e14)) | (e13 = op1(e10, e14))) & ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))) & ((e12 = op1(e14, e14)) | (e12 = op1(e13, e14)) | (e12 = op1(e12, e14)) | (e12 = op1(e11, e14)) | (e12 = op1(e10, e14))) & ((e12 = op1(e14, e14)) | (e12 = op1(e14, e13)) | (e12 = op1(e14, e12)) | (e12 = op1(e14, e11)) | (e12 = op1(e14, e10))) & ((e11 = op1(e14, e14)) | (e11 = op1(e13, e14)) | (e11 = op1(e12, e14)) | (e11 = op1(e11, e14)) | (e11 = op1(e10, e14))) & ((e11 = op1(e14, e14)) | (e11 = op1(e14, e13)) | (e11 = op1(e14, e12)) | (e11 = op1(e14, e11)) | (e11 = op1(e14, e10))) & ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))) & ((e10 = op1(e14, e14)) | (e10 = op1(e14, e13)) | (e10 = op1(e14, e12)) | (e10 = op1(e14, e11)) | (e10 = op1(e14, e10))) & ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))) & ((e14 = op1(e13, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e13, e12)) | (e14 = op1(e13, e11)) | (e14 = op1(e13, e10))) & ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e14, e13)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e14 = op1(e14, e12)) | (e14 = op1(e13, e12)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e12)) | (e14 = op1(e10, e12))) & ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))) & ((e13 = op1(e14, e12)) | (e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e14)) | (e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e14, e12)) | (e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e14)) | (e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e14, e12)) | (e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e14 = op1(e14, e11)) | (e14 = op1(e13, e11)) | (e14 = op1(e12, e11)) | (e14 = op1(e11, e11)) | (e14 = op1(e10, e11))) & ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))) & ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e14)) | (e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e14)) | (e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e14, e11)) | (e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e14)) | (e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e14, e11)) | (e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e14)) | (e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e14 = op1(e14, e10)) | (e14 = op1(e13, e10)) | (e14 = op1(e12, e10)) | (e14 = op1(e11, e10)) | (op1(e10, e10) = e14)) & ((e14 = op1(e10, e14)) | (e14 = op1(e10, e13)) | (e14 = op1(e10, e12)) | (e14 = op1(e10, e11)) | (op1(e10, e10) = e14)) & ((e13 = op1(e14, e10)) | (e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e14)) | (e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e14, e10)) | (e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e14)) | (e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e14, e10)) | (e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e14)) | (e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e14, e10)) | (e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e14)) | (e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax3)).
fof(f1382, plain, (spl40_118 | spl40_93 | spl40_68 | spl40_43 | spl40_18), inference(avatar_split_clause, [], [f158, f874, f979, f1084, f1189, f1294])).
fof(f158, plain, ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f3])).
fof(f1377, plain, (spl40_71 | spl40_66 | spl40_61 | spl40_56 | spl40_51), inference(avatar_split_clause, [], [f163, f1013, f1034, f1055, f1076, f1097])).
fof(f163, plain, ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f1375, plain, (spl40_72 | spl40_67 | spl40_62 | spl40_57 | spl40_52), inference(avatar_split_clause, [], [f165, f1017, f1038, f1059, f1080, f1101])).
fof(f165, plain, ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f1374, plain, (spl40_112 | spl40_87 | spl40_62 | spl40_37 | spl40_12), inference(avatar_split_clause, [], [f166, f849, f954, f1059, f1164, f1269])).
fof(f166, plain, ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f1366, plain, (spl40_106 | spl40_81 | spl40_56 | spl40_31 | spl40_6), inference(avatar_split_clause, [], [f174, f824, f929, f1034, f1139, f1244])).
fof(f174, plain, ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f3])).
fof(f1364, plain, (spl40_107 | spl40_82 | spl40_57 | spl40_32 | spl40_7), inference(avatar_split_clause, [], [f176, f828, f933, f1038, f1143, f1248])).
fof(f176, plain, ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f3])).
fof(f1358, plain, (spl40_110 | spl40_85 | spl40_60 | spl40_35 | spl40_10), inference(avatar_split_clause, [], [f182, f840, f945, f1050, f1155, f1260])).
fof(f182, plain, ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))), inference(cnf_transformation, [], [f3])).
fof(f1353, plain, (spl40_23 | spl40_18 | spl40_13 | spl40_8 | spl40_3), inference(avatar_split_clause, [], [f187, f811, f832, f853, f874, f895])).
fof(f187, plain, ((e12 = op1(e14, e14)) | (e12 = op1(e14, e13)) | (e12 = op1(e14, e12)) | (e12 = op1(e14, e11)) | (e12 = op1(e14, e10))), inference(cnf_transformation, [], [f3])).
fof(f1352, plain, (spl40_103 | spl40_78 | spl40_53 | spl40_28 | spl40_3), inference(avatar_split_clause, [], [f188, f811, f916, f1021, f1126, f1231])).
fof(f188, plain, ((e12 = op1(e14, e14)) | (e12 = op1(e13, e14)) | (e12 = op1(e12, e14)) | (e12 = op1(e11, e14)) | (e12 = op1(e10, e14))), inference(cnf_transformation, [], [f3])).
fof(f1351, plain, (spl40_24 | spl40_19 | spl40_14 | spl40_9 | spl40_4), inference(avatar_split_clause, [], [f189, f815, f836, f857, f878, f899])).
fof(f189, plain, ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))), inference(cnf_transformation, [], [f3])).
fof(f1347, plain, (spl40_126 | spl40_127 | spl40_128 | spl40_129 | spl40_130), inference(avatar_split_clause, [], [f142, f1344, f1340, f1336, f1332, f1328])).
fof(f142, plain, ((e14 = unit1) | (e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)), inference(cnf_transformation, [], [f2])).
fof(f1263, plain, (spl40_106 | spl40_107 | spl40_108 | spl40_109 | spl40_110), inference(avatar_split_clause, [], [f110, f1260, f1256, f1252, f1248, f1244])).
fof(f110, plain, ((e14 = op1(e10, e13)) | (e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e14 = op1(e14, e14)) | (e13 = op1(e14, e14)) | (e12 = op1(e14, e14)) | (e11 = op1(e14, e14)) | (e10 = op1(e14, e14))) & ((e14 = op1(e14, e13)) | (e13 = op1(e14, e13)) | (e12 = op1(e14, e13)) | (e11 = op1(e14, e13)) | (e10 = op1(e14, e13))) & ((e14 = op1(e14, e12)) | (e13 = op1(e14, e12)) | (e12 = op1(e14, e12)) | (e11 = op1(e14, e12)) | (e10 = op1(e14, e12))) & ((e14 = op1(e14, e11)) | (e13 = op1(e14, e11)) | (e12 = op1(e14, e11)) | (e11 = op1(e14, e11)) | (e10 = op1(e14, e11))) & ((e14 = op1(e14, e10)) | (e13 = op1(e14, e10)) | (e12 = op1(e14, e10)) | (e11 = op1(e14, e10)) | (e10 = op1(e14, e10))) & ((e14 = op1(e13, e14)) | (e13 = op1(e13, e14)) | (e12 = op1(e13, e14)) | (e11 = op1(e13, e14)) | (e10 = op1(e13, e14))) & ((e14 = op1(e13, e13)) | (e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e14 = op1(e13, e12)) | (e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e14 = op1(e13, e11)) | (e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))) & ((e14 = op1(e12, e13)) | (e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e14 = op1(e12, e12)) | (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e14 = op1(e12, e10)) | (e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))) & ((e14 = op1(e11, e13)) | (e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e14 = op1(e11, e12)) | (e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e14 = op1(e11, e11)) | (e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e14 = op1(e11, e10)) | (e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e14 = op1(e10, e14)) | (e13 = op1(e10, e14)) | (e12 = op1(e10, e14)) | (e11 = op1(e10, e14)) | (e10 = op1(e10, e14))) & ((e14 = op1(e10, e13)) | (e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e14 = op1(e10, e12)) | (e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e14 = op1(e10, e11)) | (e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e14) | (op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG050+1.p', ax1)).
fof(f1200, plain, (spl40_91 | spl40_92 | spl40_93 | spl40_94 | spl40_95), inference(avatar_split_clause, [], [f113, f1197, f1193, f1189, f1185, f1181])).
fof(f113, plain, ((e14 = op1(e11, e11)) | (e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f1179, plain, (spl40_86 | spl40_87 | spl40_88 | spl40_89 | spl40_90), inference(avatar_split_clause, [], [f114, f1176, f1172, f1168, f1164, f1160])).
fof(f114, plain, ((e14 = op1(e11, e12)) | (e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f1137, plain, (spl40_76 | spl40_77 | spl40_78 | spl40_79 | spl40_80), inference(avatar_split_clause, [], [f116, f1134, f1130, f1126, f1122, f1118])).
fof(f116, plain, ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))), inference(cnf_transformation, [], [f1])).
fof(f1116, plain, (spl40_71 | spl40_72 | spl40_73 | spl40_74 | spl40_75), inference(avatar_split_clause, [], [f117, f1113, f1109, f1105, f1101, f1097])).
fof(f117, plain, ((e14 = op1(e12, e10)) | (e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f1095, plain, (spl40_66 | spl40_67 | spl40_68 | spl40_69 | spl40_70), inference(avatar_split_clause, [], [f118, f1092, f1088, f1084, f1080, f1076])).
fof(f118, plain, ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f1011, plain, (spl40_46 | spl40_47 | spl40_48 | spl40_49 | spl40_50), inference(avatar_split_clause, [], [f122, f1008, f1004, f1000, f996, f992])).
fof(f122, plain, ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).
fof(f969, plain, (spl40_36 | spl40_37 | spl40_38 | spl40_39 | spl40_40), inference(avatar_split_clause, [], [f124, f966, f962, f958, f954, f950])).
fof(f124, plain, ((e14 = op1(e13, e12)) | (e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))), inference(cnf_transformation, [], [f1])).
fof(f948, plain, (spl40_31 | spl40_32 | spl40_33 | spl40_34 | spl40_35), inference(avatar_split_clause, [], [f125, f945, f941, f937, f933, f929])).
fof(f125, plain, ((e14 = op1(e13, e13)) | (e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))), inference(cnf_transformation, [], [f1])).