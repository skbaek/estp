fof(f9785, plain, $false, inference(avatar_sat_refutation, [], [f960, f981, f1128, f1149, f1170, f1191, f1233, f1254, f1275, f1443, f1447, f1452, f1459, f1462, f1470, f1471, f1475, f1476, f1478, f1556, f1577, f1619, f1724, f1745, f1766, f1787, f1829, f1850, f1871, f2039, f2042, f2046, f2049, f2058, f2061, f2066, f2069, f2071, f2074, f2108, f2122, f2141, f2146, f2165, f2180, f2191, f2205, f2224, f2229, f2248, f2262, f2263, f2272, f2277, f2289, f2303, f2308, f2327, f2336, f2342, f2356, f2357, f2371, f2383, f2402, f2417, f2432, f2447, f2462, f2473, f2480, f2499, f2513, f2518, f2532, f2556, f2571, f2582, f2596, f2615, f2620, f2639, f2653, f2654, f2663, f2668, f2680, f2694, f2699, f2718, f2727, f2732, f2747, f2748, f2762, f2774, f2793, f2808, f2823, f2837, f2853, f2864, f2871, f2873, f2874, f2877, f2878, f3067, f3089, f3108, f3734, f4352, f4354, f4356, f4360, f4385, f4400, f4408, f4409, f4415, f4446, f4491, f4506, f4558, f4561, f4579, f4635, f4675, f4676, f4718, f4764, f4817, f4861, f4862, f4895, f4941, f4944, f4946, f4957, f4985, f4987, f4990, f5043, f5045, f5060, f5064, f5066, f5080, f5123, f5164, f5182, f5183, f5186, f5187, f5205, f5227, f5263, f5281, f5284, f5307, f5315, f5332, f5350, f5351, f5352, f5372, f5399, f5400, f5426, f5442, f5444, f5448, f5450, f5465, f5510, f5523, f5531, f5568, f5588, f5599, f5610, f5625, f5647, f5667, f5683, f5688, f5720, f5725, f5726, f5729, f5742, f5750, f5787, f5804, f5822, f5842, f5843, f5846, f5862, f5870, f5882, f5897, f5906, f5929, f5978, f5994, f6014, f6018, f6069, f6095, f6116, f6137, f6213, f6214, f6215, f6216, f6217, f6218, f6229, f6230, f6245, f6261, f6304, f6339, f6397, f6403, f6434, f6446, f6447, f6450, f6494, f6517, f6552, f6573, f6663, f6665, f6674, f6723, f6768, f6792, f6823, f6827, f6866, f6894, f6906, f6907, f6920, f6931, f6951, f7003, f7023, f7050, f7053, f7065, f7066, f7093, f7101, f7127, f7141, f7148, f7169, f7182, f7194, f7218, f7228, f7266, f7284, f7299, f7305, f7322, f7338, f7369, f7387, f7394, f7431, f7443, f7453, f7460, f7487, f7595, f7605, f7619, f7623, f7680, f7726, f7784, f7800, f7810, f7823, f7857, f7890, f7941, f7975, f8013, f8037, f8071, f8104, f8136, f8160, f8186, f8210, f8239, f8266, f8289, f8318, f8343, f8366, f8421, f8445, f8489, f8513, f8547, f8610, f8638, f8644, f8653, f8660, f8672, f8725, f8739, f8749, f8759, f8760, f8767, f8784, f8851, f8873, f8879, f8883, f8889, f8890, f8955, f9043, f9262, f9283, f9313, f9367, f9517, f9531, f9571, f9572, f9573, f9574, f9575, f9576, f9577, f9578, f9579, f9638, f9640, f9651, f9664, f9701, f9703, f9716, f9748])).
fof(f9748, plain, (~ spl68_56 | ~ spl68_66), inference(avatar_split_clause, [], [f9737, f1172, f1130])).
fof(f1130, plain, (spl68_56 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_56])])).
fof(f1172, plain, (spl68_66 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_66])])).
fof(f9737, plain, (~ (e10 = op1(e12, e13)) | ~ spl68_66), inference(backward_demodulation, [], [f410, f1174])).
fof(f1174, plain, ((e10 = op1(e12, e11)) | ~ spl68_66), inference(avatar_component_clause, [], [f1172])).
fof(f410, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (op1(e14, e13) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e14)) & ~ (op1(e14, e11) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e14)) & ~ (op1(e14, e10) = op1(e14, e13)) & ~ (op1(e14, e10) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e11)) & ~ (op1(e13, e13) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e14)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e14)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e13) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e14)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e14)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e13) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e14)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e14)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e13) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e14)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e14)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e13, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e14, e14)) & ~ (op1(e11, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e14, e14)) & ~ (op1(e10, e14) = op1(e13, e14)) & ~ (op1(e10, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e11, e14)) & ~ (op1(e13, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e14, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e14, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e13, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e14, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e14, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e13, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e14, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e14, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e13, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e14, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e14, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax7)).
fof(f9716, plain, (~ spl68_56 | ~ spl68_81), inference(avatar_split_clause, [], [f9706, f1235, f1130])).
fof(f1235, plain, (spl68_81 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_81])])).
fof(f9706, plain, (~ (e10 = op1(e12, e13)) | ~ spl68_81), inference(backward_demodulation, [], [f369, f1237])).
fof(f1237, plain, ((e10 = op1(e11, e13)) | ~ spl68_81), inference(avatar_component_clause, [], [f1235])).
fof(f369, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f9703, plain, (spl68_12 | ~ spl68_90 | ~ spl68_318), inference(avatar_split_clause, [], [f9696, f2368, f1272, f945])).
fof(f945, plain, (spl68_12 <=> (e11 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_12])])).
fof(f1272, plain, (spl68_90 <=> (e14 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_90])])).
fof(f2368, plain, (spl68_318 <=> (e11 = op1(op1(e11, e12), e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_318])])).
fof(f9696, plain, ((e11 = op1(e14, e12)) | (~ spl68_90 | ~ spl68_318)), inference(backward_demodulation, [], [f2370, f1274])).
fof(f1274, plain, ((e14 = op1(e11, e12)) | ~ spl68_90), inference(avatar_component_clause, [], [f1272])).
fof(f2370, plain, ((e11 = op1(op1(e11, e12), e12)) | ~ spl68_318), inference(avatar_component_clause, [], [f2368])).
fof(f9701, plain, (~ spl68_65 | ~ spl68_90), inference(avatar_split_clause, [], [f9690, f1272, f1167])).
fof(f1167, plain, (spl68_65 <=> (e14 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_65])])).
fof(f9690, plain, (~ (e14 = op1(e12, e12)) | ~ spl68_90), inference(backward_demodulation, [], [f359, f1274])).
fof(f359, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f9664, plain, (~ spl68_80 | ~ spl68_105), inference(avatar_split_clause, [], [f9654, f1335, f1230])).
fof(f1230, plain, (spl68_80 <=> (e14 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_80])])).
fof(f1335, plain, (spl68_105 <=> (e14 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_105])])).
fof(f9654, plain, (~ (e14 = op1(e11, e14)) | ~ spl68_105), inference(backward_demodulation, [], [f375, f1337])).
fof(f1337, plain, ((e14 = op1(e10, e14)) | ~ spl68_105), inference(avatar_component_clause, [], [f1335])).
fof(f375, plain, ~ (op1(e10, e14) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f9651, plain, (~ spl68_84 | ~ spl68_109), inference(avatar_split_clause, [], [f9642, f1352, f1247])).
fof(f1247, plain, (spl68_84 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_84])])).
fof(f1352, plain, (spl68_109 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_109])])).
fof(f9642, plain, (~ (e13 = op1(e11, e13)) | ~ spl68_109), inference(backward_demodulation, [], [f365, f1354])).
fof(f1354, plain, ((e13 = op1(e10, e13)) | ~ spl68_109), inference(avatar_component_clause, [], [f1352])).
fof(f365, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f7])).
fof(f9640, plain, (~ spl68_53 | ~ spl68_113 | ~ spl68_309), inference(avatar_split_clause, [], [f9633, f2324, f1369, f1117])).
fof(f1117, plain, (spl68_53 <=> (e12 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_53])])).
fof(f1369, plain, (spl68_113 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_113])])).
fof(f2324, plain, (spl68_309 <=> (op1(e10, e12) = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_309])])).
fof(f9633, plain, (~ (e12 = op1(e12, e14)) | (~ spl68_113 | ~ spl68_309)), inference(backward_demodulation, [], [f8946, f1371])).
fof(f1371, plain, ((e12 = op1(e10, e12)) | ~ spl68_113), inference(avatar_component_clause, [], [f1369])).
fof(f8946, plain, (~ (op1(e10, e12) = op1(e12, e14)) | ~ spl68_309), inference(forward_demodulation, [], [f408, f2325])).
fof(f2325, plain, ((op1(e10, e12) = op1(e12, e10)) | ~ spl68_309), inference(avatar_component_clause, [], [f2324])).
fof(f408, plain, ~ (op1(e12, e10) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f9638, plain, (~ spl68_88 | ~ spl68_113), inference(avatar_split_clause, [], [f9622, f1369, f1264])).
fof(f1264, plain, (spl68_88 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_88])])).
fof(f9622, plain, (~ (e12 = op1(e11, e12)) | ~ spl68_113), inference(backward_demodulation, [], [f355, f1371])).
fof(f355, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f9579, plain, (spl68_25 | ~ spl68_126), inference(avatar_split_clause, [], [f9565, f1424, f999])).
fof(f999, plain, (spl68_25 <=> (e14 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_25])])).
fof(f1424, plain, (spl68_126 <=> (e10 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl68_126])])).
fof(f9565, plain, ((e14 = op1(e14, e10)) | ~ spl68_126), inference(backward_demodulation, [], [f197, f1426])).
fof(f1426, plain, ((e10 = unit1) | ~ spl68_126), inference(avatar_component_clause, [], [f1424])).
fof(f197, plain, (e14 = op1(e14, unit1)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e14 = unit1) | (e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)) & (e14 = op1(e14, unit1)) & (e14 = op1(unit1, e14)) & (e13 = op1(e13, unit1)) & (e13 = op1(unit1, e13)) & (e12 = op1(e12, unit1)) & (e12 = op1(unit1, e12)) & (e11 = op1(e11, unit1)) & (e11 = op1(unit1, e11)) & (e10 = op1(e10, unit1)) & (e10 = op1(unit1, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax2)).
fof(f9578, plain, (spl68_105 | ~ spl68_126), inference(avatar_split_clause, [], [f9564, f1424, f1335])).
fof(f9564, plain, ((e14 = op1(e10, e14)) | ~ spl68_126), inference(backward_demodulation, [], [f196, f1426])).
fof(f196, plain, (e14 = op1(unit1, e14)), inference(cnf_transformation, [], [f2])).
fof(f9577, plain, (spl68_49 | ~ spl68_126), inference(avatar_split_clause, [], [f9563, f1424, f1100])).
fof(f1100, plain, (spl68_49 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_49])])).
fof(f9563, plain, ((e13 = op1(e13, e10)) | ~ spl68_126), inference(backward_demodulation, [], [f195, f1426])).
fof(f195, plain, (e13 = op1(e13, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9576, plain, (spl68_109 | ~ spl68_126), inference(avatar_split_clause, [], [f9562, f1424, f1352])).
fof(f9562, plain, ((e13 = op1(e10, e13)) | ~ spl68_126), inference(backward_demodulation, [], [f194, f1426])).
fof(f194, plain, (e13 = op1(unit1, e13)), inference(cnf_transformation, [], [f2])).
fof(f9575, plain, (spl68_73 | ~ spl68_126), inference(avatar_split_clause, [], [f9561, f1424, f1201])).
fof(f1201, plain, (spl68_73 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_73])])).
fof(f9561, plain, ((e12 = op1(e12, e10)) | ~ spl68_126), inference(backward_demodulation, [], [f193, f1426])).
fof(f193, plain, (e12 = op1(e12, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9574, plain, (spl68_113 | ~ spl68_126), inference(avatar_split_clause, [], [f9560, f1424, f1369])).
fof(f9560, plain, ((e12 = op1(e10, e12)) | ~ spl68_126), inference(backward_demodulation, [], [f192, f1426])).
fof(f192, plain, (e12 = op1(unit1, e12)), inference(cnf_transformation, [], [f2])).
fof(f9573, plain, (spl68_97 | ~ spl68_126), inference(avatar_split_clause, [], [f9559, f1424, f1302])).
fof(f1302, plain, (spl68_97 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_97])])).
fof(f9559, plain, ((e11 = op1(e11, e10)) | ~ spl68_126), inference(backward_demodulation, [], [f191, f1426])).
fof(f191, plain, (e11 = op1(e11, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9572, plain, (spl68_117 | ~ spl68_126), inference(avatar_split_clause, [], [f9558, f1424, f1386])).
fof(f1386, plain, (spl68_117 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_117])])).
fof(f9558, plain, ((e11 = op1(e10, e11)) | ~ spl68_126), inference(backward_demodulation, [], [f190, f1426])).
fof(f190, plain, (e11 = op1(unit1, e11)), inference(cnf_transformation, [], [f2])).
fof(f9571, plain, (spl68_121 | ~ spl68_126), inference(avatar_split_clause, [], [f9557, f1424, f1403])).
fof(f1403, plain, (spl68_121 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_121])])).
fof(f9557, plain, ((e10 = op1(e10, e10)) | ~ spl68_126), inference(backward_demodulation, [], [f189, f1426])).
fof(f189, plain, (e10 = op1(e10, unit1)), inference(cnf_transformation, [], [f2])).
fof(f9531, plain, (~ spl68_206 | spl68_232 | ~ spl68_391), inference(avatar_contradiction_clause, [], [f9530])).
fof(f9530, plain, ($false | (~ spl68_206 | spl68_232 | ~ spl68_391)), inference(subsumption_resolution, [], [f9525, f1920])).
fof(f1920, plain, (~ (e21 = op2(e20, e24)) | spl68_232), inference(avatar_component_clause, [], [f1919])).
fof(f1919, plain, (spl68_232 <=> (e21 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_232])])).
fof(f9525, plain, ((e21 = op2(e20, e24)) | (~ spl68_206 | ~ spl68_391)), inference(backward_demodulation, [], [f2731, f1812])).
fof(f1812, plain, ((e20 = op2(e21, e24)) | ~ spl68_206), inference(avatar_component_clause, [], [f1810])).
fof(f1810, plain, (spl68_206 <=> (e20 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_206])])).
fof(f2731, plain, ((e21 = op2(op2(e21, e24), e24)) | ~ spl68_391), inference(avatar_component_clause, [], [f2729])).
fof(f2729, plain, (spl68_391 <=> (e21 = op2(op2(e21, e24), e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_391])])).
fof(f9517, plain, (~ spl68_190 | ~ spl68_215), inference(avatar_split_clause, [], [f9512, f1847, f1742])).
fof(f1742, plain, (spl68_190 <=> (e24 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_190])])).
fof(f1847, plain, (spl68_215 <=> (e24 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_215])])).
fof(f9512, plain, (~ (e24 = op2(e22, e23)) | ~ spl68_215), inference(backward_demodulation, [], [f469, f1849])).
fof(f1849, plain, ((e24 = op2(e21, e23)) | ~ spl68_215), inference(avatar_component_clause, [], [f1847])).
fof(f469, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (op2(e24, e23) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e24)) & ~ (op2(e24, e21) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e24)) & ~ (op2(e24, e20) = op2(e24, e23)) & ~ (op2(e24, e20) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e21)) & ~ (op2(e23, e23) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e24)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e24)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e23) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e24)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e24)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e23) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e24)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e24)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e23) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e24)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e24)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e23, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e24, e24)) & ~ (op2(e21, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e24, e24)) & ~ (op2(e20, e24) = op2(e23, e24)) & ~ (op2(e20, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e21, e24)) & ~ (op2(e23, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e24, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e24, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e23, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e24, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e24, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e23, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e24, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e24, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e23, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e24, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e24, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax8)).
fof(f9367, plain, (~ spl68_190 | ~ spl68_591), inference(avatar_split_clause, [], [f9355, f3968, f1742])).
fof(f3968, plain, (spl68_591 <=> (e24 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_591])])).
fof(f9355, plain, (~ (e24 = op2(e22, e23)) | ~ spl68_591), inference(backward_demodulation, [], [f2913, f3969])).
fof(f3969, plain, ((e24 = h3(e11)) | ~ spl68_591), inference(avatar_component_clause, [], [f3968])).
fof(f2913, plain, ~ (op2(e22, e23) = h3(e11)), inference(backward_demodulation, [], [f512, f750])).
fof(f750, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ((op2(e22, op2(e22, e22)) = h3(e14)) & (op2(op2(e22, op2(e22, e22)), e22) = h3(e12)) & (op2(e22, e22) = h3(e11)) & (h3(e10) = op2(e22, op2(op2(e22, op2(e22, e22)), e22))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax18)).
fof(f512, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f9313, plain, (spl68_19 | ~ spl68_45 | ~ spl68_288), inference(avatar_split_clause, [], [f9016, f2221, f1083, f974])).
fof(f974, plain, (spl68_19 <=> (e13 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_19])])).
fof(f1083, plain, (spl68_45 <=> (e14 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_45])])).
fof(f2221, plain, (spl68_288 <=> (e13 = op1(op1(e13, e11), e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_288])])).
fof(f9016, plain, ((e13 = op1(e14, e11)) | (~ spl68_45 | ~ spl68_288)), inference(forward_demodulation, [], [f2223, f1085])).
fof(f1085, plain, ((e14 = op1(e13, e11)) | ~ spl68_45), inference(avatar_component_clause, [], [f1083])).
fof(f2223, plain, ((e13 = op1(op1(e13, e11), e11)) | ~ spl68_288), inference(avatar_component_clause, [], [f2221])).
fof(f9283, plain, (~ spl68_44 | ~ spl68_45), inference(avatar_contradiction_clause, [], [f9282])).
fof(f9282, plain, ($false | (~ spl68_44 | ~ spl68_45)), inference(subsumption_resolution, [], [f9281, f544])).
fof(f544, plain, ~ (e13 = e14), inference(cnf_transformation, [], [f9])).
fof(f9, plain, (~ (e13 = e14) & ~ (e12 = e14) & ~ (e12 = e13) & ~ (e11 = e14) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e14) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax9)).
fof(f9281, plain, ((e13 = e14) | (~ spl68_44 | ~ spl68_45)), inference(backward_demodulation, [], [f1085, f1081])).
fof(f1081, plain, ((e13 = op1(e13, e11)) | ~ spl68_44), inference(avatar_component_clause, [], [f1079])).
fof(f1079, plain, (spl68_44 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_44])])).
fof(f9262, plain, (~ spl68_51 | ~ spl68_56), inference(avatar_split_clause, [], [f9253, f1130, f1109])).
fof(f1109, plain, (spl68_51 <=> (e10 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_51])])).
fof(f9253, plain, (~ (e10 = op1(e12, e14)) | ~ spl68_56), inference(backward_demodulation, [], [f414, f1132])).
fof(f1132, plain, ((e10 = op1(e12, e13)) | ~ spl68_56), inference(avatar_component_clause, [], [f1130])).
fof(f414, plain, ~ (op1(e12, e13) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f9043, plain, (spl68_44 | ~ spl68_127), inference(avatar_split_clause, [], [f9030, f1428, f1079])).
fof(f1428, plain, (spl68_127 <=> (e11 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl68_127])])).
fof(f9030, plain, ((e13 = op1(e13, e11)) | ~ spl68_127), inference(backward_demodulation, [], [f195, f1430])).
fof(f1430, plain, ((e11 = unit1) | ~ spl68_127), inference(avatar_component_clause, [], [f1428])).
fof(f8955, plain, (~ spl68_85 | ~ spl68_45 | spl68_289), inference(avatar_split_clause, [], [f8856, f2226, f1083, f1251])).
fof(f1251, plain, (spl68_85 <=> (e14 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_85])])).
fof(f2226, plain, (spl68_289 <=> (op1(e11, e13) = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_289])])).
fof(f8856, plain, (~ (e14 = op1(e11, e13)) | (~ spl68_45 | spl68_289)), inference(forward_demodulation, [], [f2228, f1085])).
fof(f2228, plain, (~ (op1(e11, e13) = op1(e13, e11)) | spl68_289), inference(avatar_component_clause, [], [f2226])).
fof(f8890, plain, (~ spl68_20 | ~ spl68_45), inference(avatar_split_clause, [], [f5417, f1083, f978])).
fof(f978, plain, (spl68_20 <=> (e14 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_20])])).
fof(f5417, plain, (~ (e14 = op1(e14, e11)) | ~ spl68_45), inference(backward_demodulation, [], [f354, f1085])).
fof(f354, plain, ~ (op1(e13, e11) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f8889, plain, (~ spl68_19 | ~ spl68_69), inference(avatar_split_clause, [], [f8888, f1184, f974])).
fof(f1184, plain, (spl68_69 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_69])])).
fof(f8888, plain, (~ (e13 = op1(e14, e11)) | ~ spl68_69), inference(forward_demodulation, [], [f353, f1186])).
fof(f1186, plain, ((e13 = op1(e12, e11)) | ~ spl68_69), inference(avatar_component_clause, [], [f1184])).
fof(f353, plain, ~ (op1(e12, e11) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f8883, plain, (~ spl68_19 | ~ spl68_14), inference(avatar_split_clause, [], [f8882, f953, f974])).
fof(f953, plain, (spl68_14 <=> (e13 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_14])])).
fof(f8882, plain, (~ (e13 = op1(e14, e11)) | ~ spl68_14), inference(forward_demodulation, [], [f429, f955])).
fof(f955, plain, ((e13 = op1(e14, e12)) | ~ spl68_14), inference(avatar_component_clause, [], [f953])).
fof(f429, plain, ~ (op1(e14, e11) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f8879, plain, (spl68_43 | ~ spl68_69 | ~ spl68_304), inference(avatar_contradiction_clause, [], [f8878])).
fof(f8878, plain, ($false | (spl68_43 | ~ spl68_69 | ~ spl68_304)), inference(subsumption_resolution, [], [f8877, f1076])).
fof(f1076, plain, (~ (e12 = op1(e13, e11)) | spl68_43), inference(avatar_component_clause, [], [f1075])).
fof(f1075, plain, (spl68_43 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_43])])).
fof(f8877, plain, ((e12 = op1(e13, e11)) | (~ spl68_69 | ~ spl68_304)), inference(forward_demodulation, [], [f2302, f1186])).
fof(f2302, plain, ((e12 = op1(op1(e12, e11), e11)) | ~ spl68_304), inference(avatar_component_clause, [], [f2300])).
fof(f2300, plain, (spl68_304 <=> (e12 = op1(op1(e12, e11), e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_304])])).
fof(f8873, plain, (~ spl68_81 | spl68_107 | ~ spl68_315), inference(avatar_contradiction_clause, [], [f8872])).
fof(f8872, plain, ($false | (~ spl68_81 | spl68_107 | ~ spl68_315)), inference(subsumption_resolution, [], [f8871, f1345])).
fof(f1345, plain, (~ (e11 = op1(e10, e13)) | spl68_107), inference(avatar_component_clause, [], [f1344])).
fof(f1344, plain, (spl68_107 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_107])])).
fof(f8871, plain, ((e11 = op1(e10, e13)) | (~ spl68_81 | ~ spl68_315)), inference(forward_demodulation, [], [f2355, f1237])).
fof(f2355, plain, ((e11 = op1(op1(e11, e13), e13)) | ~ spl68_315), inference(avatar_component_clause, [], [f2353])).
fof(f2353, plain, (spl68_315 <=> (e11 = op1(op1(e11, e13), e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_315])])).
fof(f8851, plain, (~ spl68_45 | ~ spl68_79 | spl68_311), inference(avatar_contradiction_clause, [], [f8850])).
fof(f8850, plain, ($false | (~ spl68_45 | ~ spl68_79 | spl68_311)), inference(subsumption_resolution, [], [f8849, f1085])).
fof(f8849, plain, (~ (e14 = op1(e13, e11)) | (~ spl68_79 | spl68_311)), inference(forward_demodulation, [], [f2335, f1228])).
fof(f1228, plain, ((e13 = op1(e11, e14)) | ~ spl68_79), inference(avatar_component_clause, [], [f1226])).
fof(f1226, plain, (spl68_79 <=> (e13 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_79])])).
fof(f2335, plain, (~ (e14 = op1(op1(e11, e14), e11)) | spl68_311), inference(avatar_component_clause, [], [f2333])).
fof(f2333, plain, (spl68_311 <=> (e14 = op1(op1(e11, e14), e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_311])])).
fof(f8784, plain, (~ spl68_51 | spl68_103 | ~ spl68_296), inference(avatar_contradiction_clause, [], [f8783])).
fof(f8783, plain, ($false | (~ spl68_51 | spl68_103 | ~ spl68_296)), inference(subsumption_resolution, [], [f8777, f1328])).
fof(f1328, plain, (~ (e12 = op1(e10, e14)) | spl68_103), inference(avatar_component_clause, [], [f1327])).
fof(f1327, plain, (spl68_103 <=> (e12 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_103])])).
fof(f8777, plain, ((e12 = op1(e10, e14)) | (~ spl68_51 | ~ spl68_296)), inference(backward_demodulation, [], [f2261, f1111])).
fof(f1111, plain, ((e10 = op1(e12, e14)) | ~ spl68_51), inference(avatar_component_clause, [], [f1109])).
fof(f2261, plain, ((e12 = op1(op1(e12, e14), e14)) | ~ spl68_296), inference(avatar_component_clause, [], [f2259])).
fof(f2259, plain, (spl68_296 <=> (e12 = op1(op1(e12, e14), e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_296])])).
fof(f8767, plain, (~ spl68_14 | ~ spl68_60 | spl68_298), inference(avatar_split_clause, [], [f8763, f2269, f1146, f953])).
fof(f1146, plain, (spl68_60 <=> (e14 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_60])])).
fof(f2269, plain, (spl68_298 <=> (e13 = op1(op1(e12, e13), e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_298])])).
fof(f8763, plain, (~ (e13 = op1(e14, e12)) | (~ spl68_60 | spl68_298)), inference(backward_demodulation, [], [f2271, f1148])).
fof(f1148, plain, ((e14 = op1(e12, e13)) | ~ spl68_60), inference(avatar_component_clause, [], [f1146])).
fof(f2271, plain, (~ (e13 = op1(op1(e12, e13), e12)) | spl68_298), inference(avatar_component_clause, [], [f2269])).
fof(f8760, plain, (~ spl68_52 | ~ spl68_62), inference(avatar_split_clause, [], [f8752, f1155, f1113])).
fof(f1113, plain, (spl68_52 <=> (e11 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_52])])).
fof(f1155, plain, (spl68_62 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_62])])).
fof(f8752, plain, (~ (e11 = op1(e12, e14)) | ~ spl68_62), inference(backward_demodulation, [], [f413, f1157])).
fof(f1157, plain, ((e11 = op1(e12, e12)) | ~ spl68_62), inference(avatar_component_clause, [], [f1155])).
fof(f413, plain, ~ (op1(e12, e12) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f8759, plain, (~ spl68_12 | ~ spl68_62), inference(avatar_split_clause, [], [f8750, f1155, f945])).
fof(f8750, plain, (~ (e11 = op1(e14, e12)) | ~ spl68_62), inference(backward_demodulation, [], [f363, f1157])).
fof(f363, plain, ~ (op1(e12, e12) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f8749, plain, (~ spl68_4 | ~ spl68_79), inference(avatar_split_clause, [], [f8742, f1226, f911])).
fof(f911, plain, (spl68_4 <=> (e13 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_4])])).
fof(f8742, plain, (~ (e13 = op1(e14, e14)) | ~ spl68_79), inference(backward_demodulation, [], [f381, f1228])).
fof(f381, plain, ~ (op1(e11, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f8739, plain, (~ spl68_76 | ~ spl68_81), inference(avatar_split_clause, [], [f8729, f1235, f1214])).
fof(f1214, plain, (spl68_76 <=> (e10 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_76])])).
fof(f8729, plain, (~ (e10 = op1(e11, e14)) | ~ spl68_81), inference(backward_demodulation, [], [f404, f1237])).
fof(f404, plain, ~ (op1(e11, e13) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f8725, plain, (~ spl68_85 | ~ spl68_90), inference(avatar_split_clause, [], [f8716, f1272, f1251])).
fof(f8716, plain, (~ (e14 = op1(e11, e13)) | ~ spl68_90), inference(backward_demodulation, [], [f402, f1274])).
fof(f402, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f7])).
fof(f8672, plain, (~ spl68_54 | ~ spl68_69), inference(avatar_split_clause, [], [f8671, f1184, f1121])).
fof(f1121, plain, (spl68_54 <=> (e13 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_54])])).
fof(f8671, plain, (~ (e13 = op1(e12, e14)) | ~ spl68_69), inference(forward_demodulation, [], [f411, f1186])).
fof(f411, plain, ~ (op1(e12, e11) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f8660, plain, (~ spl68_49 | ~ spl68_109 | spl68_293), inference(avatar_contradiction_clause, [], [f8659])).
fof(f8659, plain, ($false | (~ spl68_49 | ~ spl68_109 | spl68_293)), inference(subsumption_resolution, [], [f8658, f1354])).
fof(f8658, plain, (~ (e13 = op1(e10, e13)) | (~ spl68_49 | spl68_293)), inference(forward_demodulation, [], [f2247, f1102])).
fof(f1102, plain, ((e13 = op1(e13, e10)) | ~ spl68_49), inference(avatar_component_clause, [], [f1100])).
fof(f2247, plain, (~ (op1(e10, e13) = op1(e13, e10)) | spl68_293), inference(avatar_component_clause, [], [f2245])).
fof(f2245, plain, (spl68_293 <=> (op1(e10, e13) = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_293])])).
fof(f8653, plain, (~ spl68_73 | ~ spl68_113 | spl68_309), inference(avatar_contradiction_clause, [], [f8652])).
fof(f8652, plain, ($false | (~ spl68_73 | ~ spl68_113 | spl68_309)), inference(subsumption_resolution, [], [f8651, f1371])).
fof(f8651, plain, (~ (e12 = op1(e10, e12)) | (~ spl68_73 | spl68_309)), inference(forward_demodulation, [], [f2326, f1203])).
fof(f1203, plain, ((e12 = op1(e12, e10)) | ~ spl68_73), inference(avatar_component_clause, [], [f1201])).
fof(f2326, plain, (~ (op1(e10, e12) = op1(e12, e10)) | spl68_309), inference(avatar_component_clause, [], [f2324])).
fof(f8644, plain, (~ spl68_69 | ~ spl68_89 | spl68_305), inference(avatar_contradiction_clause, [], [f8643])).
fof(f8643, plain, ($false | (~ spl68_69 | ~ spl68_89 | spl68_305)), inference(subsumption_resolution, [], [f8642, f1270])).
fof(f1270, plain, ((e13 = op1(e11, e12)) | ~ spl68_89), inference(avatar_component_clause, [], [f1268])).
fof(f1268, plain, (spl68_89 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_89])])).
fof(f8642, plain, (~ (e13 = op1(e11, e12)) | (~ spl68_69 | spl68_305)), inference(forward_demodulation, [], [f2307, f1186])).
fof(f2307, plain, (~ (op1(e11, e12) = op1(e12, e11)) | spl68_305), inference(avatar_component_clause, [], [f2305])).
fof(f2305, plain, (spl68_305 <=> (op1(e11, e12) = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_305])])).
fof(f8638, plain, (~ spl68_36 | ~ spl68_89 | ~ spl68_318), inference(avatar_contradiction_clause, [], [f8637])).
fof(f8637, plain, ($false | (~ spl68_36 | ~ spl68_89 | ~ spl68_318)), inference(subsumption_resolution, [], [f8636, f535])).
fof(f535, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f9])).
fof(f8636, plain, ((e10 = e11) | (~ spl68_36 | ~ spl68_89 | ~ spl68_318)), inference(forward_demodulation, [], [f8635, f1048])).
fof(f1048, plain, ((e10 = op1(e13, e12)) | ~ spl68_36), inference(avatar_component_clause, [], [f1046])).
fof(f1046, plain, (spl68_36 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_36])])).
fof(f8635, plain, ((e11 = op1(e13, e12)) | (~ spl68_89 | ~ spl68_318)), inference(forward_demodulation, [], [f2370, f1270])).
fof(f8610, plain, (~ spl68_16 | ~ spl68_76 | spl68_272), inference(avatar_contradiction_clause, [], [f8609])).
fof(f8609, plain, ($false | (~ spl68_16 | ~ spl68_76 | spl68_272)), inference(subsumption_resolution, [], [f8608, f1216])).
fof(f1216, plain, ((e10 = op1(e11, e14)) | ~ spl68_76), inference(avatar_component_clause, [], [f1214])).
fof(f8608, plain, (~ (e10 = op1(e11, e14)) | (~ spl68_16 | spl68_272)), inference(forward_demodulation, [], [f2145, f964])).
fof(f964, plain, ((e10 = op1(e14, e11)) | ~ spl68_16), inference(avatar_component_clause, [], [f962])).
fof(f962, plain, (spl68_16 <=> (e10 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_16])])).
fof(f2145, plain, (~ (op1(e11, e14) = op1(e14, e11)) | spl68_272), inference(avatar_component_clause, [], [f2143])).
fof(f2143, plain, (spl68_272 <=> (op1(e11, e14) = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_272])])).
fof(f8547, plain, (~ spl68_16 | ~ spl68_117 | ~ spl68_271), inference(avatar_contradiction_clause, [], [f8546])).
fof(f8546, plain, ($false | (~ spl68_16 | ~ spl68_117 | ~ spl68_271)), inference(subsumption_resolution, [], [f8545, f541])).
fof(f541, plain, ~ (e11 = e14), inference(cnf_transformation, [], [f9])).
fof(f8545, plain, ((e11 = e14) | (~ spl68_16 | ~ spl68_117 | ~ spl68_271)), inference(forward_demodulation, [], [f8544, f1388])).
fof(f1388, plain, ((e11 = op1(e10, e11)) | ~ spl68_117), inference(avatar_component_clause, [], [f1386])).
fof(f8544, plain, ((e14 = op1(e10, e11)) | (~ spl68_16 | ~ spl68_271)), inference(forward_demodulation, [], [f2140, f964])).
fof(f2140, plain, ((e14 = op1(op1(e14, e11), e11)) | ~ spl68_271), inference(avatar_component_clause, [], [f2138])).
fof(f2138, plain, (spl68_271 <=> (e14 = op1(op1(e14, e11), e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_271])])).
fof(f8513, plain, (~ spl68_56 | spl68_108 | ~ spl68_299), inference(avatar_contradiction_clause, [], [f8512])).
fof(f8512, plain, ($false | (~ spl68_56 | spl68_108 | ~ spl68_299)), inference(subsumption_resolution, [], [f8508, f1349])).
fof(f1349, plain, (~ (e12 = op1(e10, e13)) | spl68_108), inference(avatar_component_clause, [], [f1348])).
fof(f1348, plain, (spl68_108 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_108])])).
fof(f8508, plain, ((e12 = op1(e10, e13)) | (~ spl68_56 | ~ spl68_299)), inference(backward_demodulation, [], [f2276, f1132])).
fof(f2276, plain, ((e12 = op1(op1(e12, e13), e13)) | ~ spl68_299), inference(avatar_component_clause, [], [f2274])).
fof(f2274, plain, (spl68_299 <=> (e12 = op1(op1(e12, e13), e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_299])])).
fof(f8489, plain, (~ spl68_64 | ~ spl68_69), inference(avatar_split_clause, [], [f8484, f1184, f1163])).
fof(f1163, plain, (spl68_64 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_64])])).
fof(f8484, plain, (~ (e13 = op1(e12, e12)) | ~ spl68_69), inference(backward_demodulation, [], [f409, f1186])).
fof(f409, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f8445, plain, (~ spl68_79 | ~ spl68_89), inference(avatar_split_clause, [], [f8438, f1268, f1226])).
fof(f8438, plain, (~ (e13 = op1(e11, e14)) | ~ spl68_89), inference(backward_demodulation, [], [f403, f1270])).
fof(f403, plain, ~ (op1(e11, e12) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f8421, plain, (spl68_90 | ~ spl68_12 | ~ spl68_267), inference(avatar_split_clause, [], [f7059, f2119, f945, f1272])).
fof(f2119, plain, (spl68_267 <=> (e14 = op1(op1(e14, e12), e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_267])])).
fof(f7059, plain, ((e14 = op1(e11, e12)) | (~ spl68_12 | ~ spl68_267)), inference(backward_demodulation, [], [f2121, f947])).
fof(f947, plain, ((e11 = op1(e14, e12)) | ~ spl68_12), inference(avatar_component_clause, [], [f945])).
fof(f2121, plain, ((e14 = op1(op1(e14, e12), e12)) | ~ spl68_267), inference(avatar_component_clause, [], [f2119])).
fof(f8366, plain, (~ spl68_109 | ~ spl68_239 | spl68_558), inference(avatar_contradiction_clause, [], [f8365])).
fof(f8365, plain, ($false | (~ spl68_109 | ~ spl68_239 | spl68_558)), inference(subsumption_resolution, [], [f8364, f1950])).
fof(f1950, plain, ((e23 = op2(e20, e23)) | ~ spl68_239), inference(avatar_component_clause, [], [f1948])).
fof(f1948, plain, (spl68_239 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_239])])).
fof(f8364, plain, (~ (e23 = op2(e20, e23)) | (~ spl68_109 | spl68_558)), inference(forward_demodulation, [], [f8363, f753])).
fof(f753, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((op2(e23, op2(e23, e23)) = h4(e14)) & (op2(op2(e23, op2(e23, e23)), e23) = h4(e12)) & (op2(e23, e23) = h4(e11)) & (op2(e23, op2(op2(e23, op2(e23, e23)), e23)) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax19)).
fof(f8363, plain, (~ (op2(e20, e23) = h4(e13)) | (~ spl68_109 | spl68_558)), inference(forward_demodulation, [], [f3721, f1354])).
fof(f3721, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | spl68_558), inference(avatar_component_clause, [], [f3719])).
fof(f3719, plain, (spl68_558 <=> (op2(e20, e23) = h4(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl68_558])])).
fof(f8343, plain, (~ spl68_28 | ~ spl68_158 | ~ spl68_440 | spl68_546), inference(avatar_contradiction_clause, [], [f8342])).
fof(f8342, plain, ($false | (~ spl68_28 | ~ spl68_158 | ~ spl68_440 | spl68_546)), inference(subsumption_resolution, [], [f8341, f1610])).
fof(f1610, plain, ((e22 = op2(e23, e24)) | ~ spl68_158), inference(avatar_component_clause, [], [f1608])).
fof(f1608, plain, (spl68_158 <=> (e22 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_158])])).
fof(f8341, plain, (~ (e22 = op2(e23, e24)) | (~ spl68_28 | ~ spl68_440 | spl68_546)), inference(forward_demodulation, [], [f8340, f3065])).
fof(f3065, plain, ((e22 = h4(e12)) | ~ spl68_440), inference(avatar_component_clause, [], [f3064])).
fof(f3064, plain, (spl68_440 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_440])])).
fof(f8340, plain, (~ (op2(e23, e24) = h4(e12)) | (~ spl68_28 | spl68_546)), inference(forward_demodulation, [], [f3673, f1014])).
fof(f1014, plain, ((e12 = op1(e13, e14)) | ~ spl68_28), inference(avatar_component_clause, [], [f1012])).
fof(f1012, plain, (spl68_28 <=> (e12 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_28])])).
fof(f3673, plain, (~ (op2(e23, e24) = h4(op1(e13, e14))) | spl68_546), inference(avatar_component_clause, [], [f3671])).
fof(f3671, plain, (spl68_546 <=> (op2(e23, e24) = h4(op1(e13, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl68_546])])).
fof(f8318, plain, (~ spl68_52 | ~ spl68_182 | ~ spl68_440 | ~ spl68_444 | spl68_551), inference(avatar_contradiction_clause, [], [f8317])).
fof(f8317, plain, ($false | (~ spl68_52 | ~ spl68_182 | ~ spl68_440 | ~ spl68_444 | spl68_551)), inference(subsumption_resolution, [], [f8316, f1711])).
fof(f1711, plain, ((e21 = op2(e22, e24)) | ~ spl68_182), inference(avatar_component_clause, [], [f1709])).
fof(f1709, plain, (spl68_182 <=> (e21 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_182])])).
fof(f8316, plain, (~ (e21 = op2(e22, e24)) | (~ spl68_52 | ~ spl68_440 | ~ spl68_444 | spl68_551)), inference(forward_demodulation, [], [f8315, f3087])).
fof(f3087, plain, ((e21 = h4(e11)) | ~ spl68_444), inference(avatar_component_clause, [], [f3086])).
fof(f3086, plain, (spl68_444 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_444])])).
fof(f8315, plain, (~ (op2(e22, e24) = h4(e11)) | (~ spl68_52 | ~ spl68_440 | spl68_551)), inference(forward_demodulation, [], [f8314, f1115])).
fof(f1115, plain, ((e11 = op1(e12, e14)) | ~ spl68_52), inference(avatar_component_clause, [], [f1113])).
fof(f8314, plain, (~ (op2(e22, e24) = h4(op1(e12, e14))) | (~ spl68_440 | spl68_551)), inference(forward_demodulation, [], [f3693, f3065])).
fof(f3693, plain, (~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | spl68_551), inference(avatar_component_clause, [], [f3691])).
fof(f3691, plain, (spl68_551 <=> (h4(op1(e12, e14)) = op2(h4(e12), e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_551])])).
fof(f8289, plain, (~ spl68_73 | ~ spl68_203 | ~ spl68_440 | spl68_553), inference(avatar_contradiction_clause, [], [f8288])).
fof(f8288, plain, ($false | (~ spl68_73 | ~ spl68_203 | ~ spl68_440 | spl68_553)), inference(subsumption_resolution, [], [f8287, f1799])).
fof(f1799, plain, ((e22 = op2(e22, e20)) | ~ spl68_203), inference(avatar_component_clause, [], [f1797])).
fof(f1797, plain, (spl68_203 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_203])])).
fof(f8287, plain, (~ (e22 = op2(e22, e20)) | (~ spl68_73 | ~ spl68_440 | spl68_553)), inference(forward_demodulation, [], [f8286, f3065])).
fof(f8286, plain, (~ (op2(e22, e20) = h4(e12)) | (~ spl68_73 | ~ spl68_440 | spl68_553)), inference(forward_demodulation, [], [f8285, f1203])).
fof(f8285, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl68_440 | spl68_553)), inference(forward_demodulation, [], [f3701, f3065])).
fof(f3701, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | spl68_553), inference(avatar_component_clause, [], [f3699])).
fof(f3699, plain, (spl68_553 <=> (h4(op1(e12, e10)) = op2(h4(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_553])])).
fof(f8266, plain, (~ spl68_64 | ~ spl68_194 | ~ spl68_440 | spl68_540), inference(avatar_contradiction_clause, [], [f8265])).
fof(f8265, plain, ($false | (~ spl68_64 | ~ spl68_194 | ~ spl68_440 | spl68_540)), inference(subsumption_resolution, [], [f8264, f1761])).
fof(f1761, plain, ((e23 = op2(e22, e22)) | ~ spl68_194), inference(avatar_component_clause, [], [f1759])).
fof(f1759, plain, (spl68_194 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_194])])).
fof(f8264, plain, (~ (e23 = op2(e22, e22)) | (~ spl68_64 | ~ spl68_440 | spl68_540)), inference(forward_demodulation, [], [f8263, f753])).
fof(f8263, plain, (~ (op2(e22, e22) = h4(e13)) | (~ spl68_64 | ~ spl68_440 | spl68_540)), inference(forward_demodulation, [], [f8262, f1165])).
fof(f1165, plain, ((e13 = op1(e12, e12)) | ~ spl68_64), inference(avatar_component_clause, [], [f1163])).
fof(f8262, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | (~ spl68_440 | spl68_540)), inference(forward_demodulation, [], [f3649, f3065])).
fof(f3649, plain, (~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | spl68_540), inference(avatar_component_clause, [], [f3647])).
fof(f3647, plain, (spl68_540 <=> (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl68_540])])).
fof(f8239, plain, (~ spl68_60 | ~ spl68_190 | ~ spl68_440 | spl68_552), inference(avatar_contradiction_clause, [], [f8238])).
fof(f8238, plain, ($false | (~ spl68_60 | ~ spl68_190 | ~ spl68_440 | spl68_552)), inference(subsumption_resolution, [], [f8237, f1744])).
fof(f1744, plain, ((e24 = op2(e22, e23)) | ~ spl68_190), inference(avatar_component_clause, [], [f1742])).
fof(f8237, plain, (~ (e24 = op2(e22, e23)) | (~ spl68_60 | ~ spl68_440 | spl68_552)), inference(forward_demodulation, [], [f8236, f2919])).
fof(f2919, plain, (e24 = h4(e14)), inference(forward_demodulation, [], [f757, f737])).
fof(f737, plain, (e24 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((e24 = op2(e23, op2(e23, e23))) & (e22 = op2(op2(e23, op2(e23, e23)), e23)) & (e21 = op2(e23, e23)) & (e20 = op2(e23, op2(op2(e23, op2(e23, e23)), e23)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax15)).
fof(f757, plain, (op2(e23, op2(e23, e23)) = h4(e14)), inference(cnf_transformation, [], [f19])).
fof(f8236, plain, (~ (op2(e22, e23) = h4(e14)) | (~ spl68_60 | ~ spl68_440 | spl68_552)), inference(forward_demodulation, [], [f8235, f1148])).
fof(f8235, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | (~ spl68_440 | spl68_552)), inference(forward_demodulation, [], [f3697, f3065])).
fof(f3697, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | spl68_552), inference(avatar_component_clause, [], [f3695])).
fof(f3695, plain, (spl68_552 <=> (h4(op1(e12, e13)) = op2(h4(e12), e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_552])])).
fof(f8210, plain, (~ spl68_121 | ~ spl68_506 | spl68_561), inference(avatar_contradiction_clause, [], [f8209])).
fof(f8209, plain, ($false | (~ spl68_121 | ~ spl68_506 | spl68_561)), inference(subsumption_resolution, [], [f8208, f3404])).
fof(f3404, plain, ((e20 = h1(e11)) | ~ spl68_506), inference(avatar_component_clause, [], [f3403])).
fof(f3403, plain, (spl68_506 <=> (e20 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_506])])).
fof(f8208, plain, (~ (e20 = h1(e11)) | (~ spl68_121 | spl68_561)), inference(forward_demodulation, [], [f8207, f2942])).
fof(f2942, plain, (e20 = h4(e10)), inference(forward_demodulation, [], [f2941, f2929])).
fof(f2929, plain, (e20 = op2(e23, h4(e12))), inference(backward_demodulation, [], [f2879, f2920])).
fof(f2920, plain, (op2(e24, e23) = h4(e12)), inference(forward_demodulation, [], [f756, f737])).
fof(f756, plain, (op2(op2(e23, op2(e23, e23)), e23) = h4(e12)), inference(cnf_transformation, [], [f19])).
fof(f2879, plain, (e20 = op2(e23, op2(e24, e23))), inference(forward_demodulation, [], [f734, f737])).
fof(f734, plain, (e20 = op2(e23, op2(op2(e23, op2(e23, e23)), e23))), inference(cnf_transformation, [], [f15])).
fof(f2941, plain, (h4(e10) = op2(e23, h4(e12))), inference(forward_demodulation, [], [f2940, f2920])).
fof(f2940, plain, (h4(e10) = op2(e23, op2(e24, e23))), inference(forward_demodulation, [], [f2939, f2937])).
fof(f2937, plain, (e24 = op2(e23, h4(e11))), inference(backward_demodulation, [], [f737, f755])).
fof(f755, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f19])).
fof(f2939, plain, (h4(e10) = op2(e23, op2(op2(e23, h4(e11)), e23))), inference(forward_demodulation, [], [f754, f755])).
fof(f754, plain, (op2(e23, op2(op2(e23, op2(e23, e23)), e23)) = h4(e10)), inference(cnf_transformation, [], [f19])).
fof(f8207, plain, (~ (h1(e11) = h4(e10)) | (~ spl68_121 | spl68_561)), inference(forward_demodulation, [], [f3733, f1405])).
fof(f1405, plain, ((e10 = op1(e10, e10)) | ~ spl68_121), inference(avatar_component_clause, [], [f1403])).
fof(f3733, plain, (~ (h1(e11) = h4(op1(e10, e10))) | spl68_561), inference(avatar_component_clause, [], [f3731])).
fof(f3731, plain, (spl68_561 <=> (h1(e11) = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl68_561])])).
fof(f8186, plain, (~ spl68_25 | ~ spl68_155 | spl68_545), inference(avatar_contradiction_clause, [], [f8185])).
fof(f8185, plain, ($false | (~ spl68_25 | ~ spl68_155 | spl68_545)), inference(subsumption_resolution, [], [f8184, f1597])).
fof(f1597, plain, ((e24 = op2(e24, e20)) | ~ spl68_155), inference(avatar_component_clause, [], [f1595])).
fof(f1595, plain, (spl68_155 <=> (e24 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_155])])).
fof(f8184, plain, (~ (e24 = op2(e24, e20)) | (~ spl68_25 | spl68_545)), inference(forward_demodulation, [], [f8183, f2919])).
fof(f8183, plain, (~ (op2(e24, e20) = h4(e14)) | (~ spl68_25 | spl68_545)), inference(forward_demodulation, [], [f3669, f1001])).
fof(f1001, plain, ((e14 = op1(e14, e10)) | ~ spl68_25), inference(avatar_component_clause, [], [f999])).
fof(f3669, plain, (~ (op2(e24, e20) = h4(op1(e14, e10))) | spl68_545), inference(avatar_component_clause, [], [f3667])).
fof(f3667, plain, (spl68_545 <=> (op2(e24, e20) = h4(op1(e14, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl68_545])])).
fof(f8160, plain, (~ spl68_113 | ~ spl68_243 | ~ spl68_440 | spl68_559), inference(avatar_contradiction_clause, [], [f8159])).
fof(f8159, plain, ($false | (~ spl68_113 | ~ spl68_243 | ~ spl68_440 | spl68_559)), inference(subsumption_resolution, [], [f8158, f1967])).
fof(f1967, plain, ((e22 = op2(e20, e22)) | ~ spl68_243), inference(avatar_component_clause, [], [f1965])).
fof(f1965, plain, (spl68_243 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_243])])).
fof(f8158, plain, (~ (e22 = op2(e20, e22)) | (~ spl68_113 | ~ spl68_440 | spl68_559)), inference(forward_demodulation, [], [f8157, f3065])).
fof(f8157, plain, (~ (op2(e20, e22) = h4(e12)) | (~ spl68_113 | ~ spl68_440 | spl68_559)), inference(forward_demodulation, [], [f8156, f1371])).
fof(f8156, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl68_440 | spl68_559)), inference(forward_demodulation, [], [f3725, f3065])).
fof(f3725, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | spl68_559), inference(avatar_component_clause, [], [f3723])).
fof(f3723, plain, (spl68_559 <=> (h4(op1(e10, e12)) = op2(e20, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl68_559])])).
fof(f8136, plain, (~ spl68_32 | spl68_547), inference(avatar_contradiction_clause, [], [f8135])).
fof(f8135, plain, ($false | (~ spl68_32 | spl68_547)), inference(trivial_inequality_removal, [], [f8134])).
fof(f8134, plain, (~ (h4(e11) = h4(e11)) | (~ spl68_32 | spl68_547)), inference(forward_demodulation, [], [f3677, f1031])).
fof(f1031, plain, ((e11 = op1(e13, e13)) | ~ spl68_32), inference(avatar_component_clause, [], [f1029])).
fof(f1029, plain, (spl68_32 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_32])])).
fof(f3677, plain, (~ (h4(e11) = h4(op1(e13, e13))) | spl68_547), inference(avatar_component_clause, [], [f3675])).
fof(f3675, plain, (spl68_547 <=> (h4(e11) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl68_547])])).
fof(f8104, plain, (~ spl68_97 | ~ spl68_227 | ~ spl68_444 | spl68_556), inference(avatar_contradiction_clause, [], [f8103])).
fof(f8103, plain, ($false | (~ spl68_97 | ~ spl68_227 | ~ spl68_444 | spl68_556)), inference(subsumption_resolution, [], [f8102, f1900])).
fof(f1900, plain, ((e21 = op2(e21, e20)) | ~ spl68_227), inference(avatar_component_clause, [], [f1898])).
fof(f1898, plain, (spl68_227 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_227])])).
fof(f8102, plain, (~ (e21 = op2(e21, e20)) | (~ spl68_97 | ~ spl68_444 | spl68_556)), inference(forward_demodulation, [], [f8101, f3087])).
fof(f8101, plain, (~ (op2(e21, e20) = h4(e11)) | (~ spl68_97 | ~ spl68_444 | spl68_556)), inference(forward_demodulation, [], [f8100, f1304])).
fof(f1304, plain, ((e11 = op1(e11, e10)) | ~ spl68_97), inference(avatar_component_clause, [], [f1302])).
fof(f8100, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | (~ spl68_444 | spl68_556)), inference(forward_demodulation, [], [f3713, f3087])).
fof(f3713, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | spl68_556), inference(avatar_component_clause, [], [f3711])).
fof(f3711, plain, (spl68_556 <=> (h4(op1(e11, e10)) = op2(h4(e11), e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_556])])).
fof(f8071, plain, (~ spl68_8 | spl68_542), inference(avatar_contradiction_clause, [], [f8070])).
fof(f8070, plain, ($false | (~ spl68_8 | spl68_542)), inference(trivial_inequality_removal, [], [f8069])).
fof(f8069, plain, (~ (h4(e12) = h4(e12)) | (~ spl68_8 | spl68_542)), inference(forward_demodulation, [], [f3657, f930])).
fof(f930, plain, ((e12 = op1(e14, e13)) | ~ spl68_8), inference(avatar_component_clause, [], [f928])).
fof(f928, plain, (spl68_8 <=> (e12 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_8])])).
fof(f3657, plain, (~ (h4(e12) = h4(op1(e14, e13))) | spl68_542), inference(avatar_component_clause, [], [f3655])).
fof(f3655, plain, (spl68_542 <=> (h4(e12) = h4(op1(e14, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl68_542])])).
fof(f8037, plain, (~ spl68_36 | spl68_548), inference(avatar_contradiction_clause, [], [f8036])).
fof(f8036, plain, ($false | (~ spl68_36 | spl68_548)), inference(subsumption_resolution, [], [f8035, f2942])).
fof(f8035, plain, (~ (e20 = h4(e10)) | (~ spl68_36 | spl68_548)), inference(forward_demodulation, [], [f3681, f1048])).
fof(f3681, plain, (~ (e20 = h4(op1(e13, e12))) | spl68_548), inference(avatar_component_clause, [], [f3679])).
fof(f3679, plain, (spl68_548 <=> (e20 = h4(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl68_548])])).
fof(f8013, plain, (~ spl68_19 | ~ spl68_149 | ~ spl68_444 | spl68_544), inference(avatar_contradiction_clause, [], [f8012])).
fof(f8012, plain, ($false | (~ spl68_19 | ~ spl68_149 | ~ spl68_444 | spl68_544)), inference(subsumption_resolution, [], [f8011, f1572])).
fof(f1572, plain, ((e23 = op2(e24, e21)) | ~ spl68_149), inference(avatar_component_clause, [], [f1570])).
fof(f1570, plain, (spl68_149 <=> (e23 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_149])])).
fof(f8011, plain, (~ (e23 = op2(e24, e21)) | (~ spl68_19 | ~ spl68_444 | spl68_544)), inference(forward_demodulation, [], [f8010, f753])).
fof(f8010, plain, (~ (op2(e24, e21) = h4(e13)) | (~ spl68_19 | ~ spl68_444 | spl68_544)), inference(forward_demodulation, [], [f8009, f976])).
fof(f976, plain, ((e13 = op1(e14, e11)) | ~ spl68_19), inference(avatar_component_clause, [], [f974])).
fof(f8009, plain, (~ (op2(e24, e21) = h4(op1(e14, e11))) | (~ spl68_444 | spl68_544)), inference(forward_demodulation, [], [f3665, f3087])).
fof(f3665, plain, (~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | spl68_544), inference(avatar_component_clause, [], [f3663])).
fof(f3663, plain, (spl68_544 <=> (h4(op1(e14, e11)) = op2(e24, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl68_544])])).
fof(f7975, plain, (~ spl68_117 | ~ spl68_247 | ~ spl68_444 | spl68_560), inference(avatar_contradiction_clause, [], [f7974])).
fof(f7974, plain, ($false | (~ spl68_117 | ~ spl68_247 | ~ spl68_444 | spl68_560)), inference(subsumption_resolution, [], [f7973, f1984])).
fof(f1984, plain, ((e21 = op2(e20, e21)) | ~ spl68_247), inference(avatar_component_clause, [], [f1982])).
fof(f1982, plain, (spl68_247 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_247])])).
fof(f7973, plain, (~ (e21 = op2(e20, e21)) | (~ spl68_117 | ~ spl68_444 | spl68_560)), inference(forward_demodulation, [], [f7972, f3087])).
fof(f7972, plain, (~ (op2(e20, e21) = h4(e11)) | (~ spl68_117 | ~ spl68_444 | spl68_560)), inference(forward_demodulation, [], [f7971, f1388])).
fof(f7971, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | (~ spl68_444 | spl68_560)), inference(forward_demodulation, [], [f3729, f3087])).
fof(f3729, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | spl68_560), inference(avatar_component_clause, [], [f3727])).
fof(f3727, plain, (spl68_560 <=> (h4(op1(e10, e11)) = op2(e20, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl68_560])])).
fof(f7941, plain, (~ spl68_1 | ~ spl68_437 | spl68_541), inference(avatar_contradiction_clause, [], [f7940])).
fof(f7940, plain, ($false | (~ spl68_1 | ~ spl68_437 | spl68_541)), inference(subsumption_resolution, [], [f7939, f2942])).
fof(f7939, plain, (~ (e20 = h4(e10)) | (~ spl68_1 | ~ spl68_437 | spl68_541)), inference(forward_demodulation, [], [f7938, f3047])).
fof(f3047, plain, ((e20 = h5(e11)) | ~ spl68_437), inference(avatar_component_clause, [], [f3046])).
fof(f3046, plain, (spl68_437 <=> (e20 = h5(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_437])])).
fof(f7938, plain, (~ (h4(e10) = h5(e11)) | (~ spl68_1 | spl68_541)), inference(forward_demodulation, [], [f3653, f901])).
fof(f901, plain, ((e10 = op1(e14, e14)) | ~ spl68_1), inference(avatar_component_clause, [], [f899])).
fof(f899, plain, (spl68_1 <=> (e10 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_1])])).
fof(f3653, plain, (~ (h5(e11) = h4(op1(e14, e14))) | spl68_541), inference(avatar_component_clause, [], [f3651])).
fof(f3651, plain, (spl68_541 <=> (h5(e11) = h4(op1(e14, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl68_541])])).
fof(f7890, plain, (~ spl68_12 | ~ spl68_142 | ~ spl68_440 | ~ spl68_444 | spl68_543), inference(avatar_contradiction_clause, [], [f7889])).
fof(f7889, plain, ($false | (~ spl68_12 | ~ spl68_142 | ~ spl68_440 | ~ spl68_444 | spl68_543)), inference(subsumption_resolution, [], [f7888, f1543])).
fof(f1543, plain, ((e21 = op2(e24, e22)) | ~ spl68_142), inference(avatar_component_clause, [], [f1541])).
fof(f1541, plain, (spl68_142 <=> (e21 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_142])])).
fof(f7888, plain, (~ (e21 = op2(e24, e22)) | (~ spl68_12 | ~ spl68_440 | ~ spl68_444 | spl68_543)), inference(forward_demodulation, [], [f7887, f3087])).
fof(f7887, plain, (~ (op2(e24, e22) = h4(e11)) | (~ spl68_12 | ~ spl68_440 | spl68_543)), inference(forward_demodulation, [], [f7886, f947])).
fof(f7886, plain, (~ (op2(e24, e22) = h4(op1(e14, e12))) | (~ spl68_440 | spl68_543)), inference(forward_demodulation, [], [f3661, f3065])).
fof(f3661, plain, (~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | spl68_543), inference(avatar_component_clause, [], [f3659])).
fof(f3659, plain, (spl68_543 <=> (h4(op1(e14, e12)) = op2(e24, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl68_543])])).
fof(f7857, plain, (~ spl68_45 | spl68_549), inference(avatar_contradiction_clause, [], [f7856])).
fof(f7856, plain, ($false | (~ spl68_45 | spl68_549)), inference(subsumption_resolution, [], [f7855, f2919])).
fof(f7855, plain, (~ (e24 = h4(e14)) | (~ spl68_45 | spl68_549)), inference(forward_demodulation, [], [f3685, f1085])).
fof(f3685, plain, (~ (e24 = h4(op1(e13, e11))) | spl68_549), inference(avatar_component_clause, [], [f3683])).
fof(f3683, plain, (spl68_549 <=> (e24 = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl68_549])])).
fof(f7823, plain, (~ spl68_186 | ~ spl68_196), inference(avatar_split_clause, [], [f7815, f1768, f1726])).
fof(f1726, plain, (spl68_186 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_186])])).
fof(f1768, plain, (spl68_196 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_196])])).
fof(f7815, plain, (~ (e20 = op2(e22, e23)) | ~ spl68_196), inference(backward_demodulation, [], [f510, f1770])).
fof(f1770, plain, ((e20 = op2(e22, e21)) | ~ spl68_196), inference(avatar_component_clause, [], [f1768])).
fof(f510, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f7810, plain, (~ spl68_186 | ~ spl68_211), inference(avatar_split_clause, [], [f7802, f1831, f1726])).
fof(f1831, plain, (spl68_211 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_211])])).
fof(f7802, plain, (~ (e20 = op2(e22, e23)) | ~ spl68_211), inference(backward_demodulation, [], [f469, f1833])).
fof(f1833, plain, ((e20 = op2(e21, e23)) | ~ spl68_211), inference(avatar_component_clause, [], [f1831])).
fof(f7800, plain, (~ spl68_215 | ~ spl68_220), inference(avatar_split_clause, [], [f7795, f1868, f1847])).
fof(f1868, plain, (spl68_220 <=> (e24 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_220])])).
fof(f7795, plain, (~ (e24 = op2(e21, e23)) | ~ spl68_220), inference(backward_demodulation, [], [f502, f1870])).
fof(f1870, plain, ((e24 = op2(e21, e22)) | ~ spl68_220), inference(avatar_component_clause, [], [f1868])).
fof(f502, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f7784, plain, (spl68_149 | ~ spl68_175 | ~ spl68_367), inference(avatar_split_clause, [], [f7783, f2612, f1679, f1570])).
fof(f1679, plain, (spl68_175 <=> (e24 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_175])])).
fof(f2612, plain, (spl68_367 <=> (e23 = op2(op2(e23, e21), e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_367])])).
fof(f7783, plain, ((e23 = op2(e24, e21)) | (~ spl68_175 | ~ spl68_367)), inference(forward_demodulation, [], [f2614, f1681])).
fof(f1681, plain, ((e24 = op2(e23, e21)) | ~ spl68_175), inference(avatar_component_clause, [], [f1679])).
fof(f2614, plain, ((e23 = op2(op2(e23, e21), e21)) | ~ spl68_367), inference(avatar_component_clause, [], [f2612])).
fof(f7726, plain, (~ spl68_105 | ~ spl68_235 | spl68_557), inference(avatar_contradiction_clause, [], [f7725])).
fof(f7725, plain, ($false | (~ spl68_105 | ~ spl68_235 | spl68_557)), inference(subsumption_resolution, [], [f7724, f1933])).
fof(f1933, plain, ((e24 = op2(e20, e24)) | ~ spl68_235), inference(avatar_component_clause, [], [f1931])).
fof(f1931, plain, (spl68_235 <=> (e24 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_235])])).
fof(f7724, plain, (~ (e24 = op2(e20, e24)) | (~ spl68_105 | spl68_557)), inference(forward_demodulation, [], [f7723, f2919])).
fof(f7723, plain, (~ (op2(e20, e24) = h4(e14)) | (~ spl68_105 | spl68_557)), inference(forward_demodulation, [], [f3717, f1337])).
fof(f3717, plain, (~ (op2(e20, e24) = h4(op1(e10, e14))) | spl68_557), inference(avatar_component_clause, [], [f3715])).
fof(f3715, plain, (spl68_557 <=> (op2(e20, e24) = h4(op1(e10, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl68_557])])).
fof(f7680, plain, (~ spl68_247 | ~ spl68_248), inference(avatar_contradiction_clause, [], [f7679])).
fof(f7679, plain, ($false | (~ spl68_247 | ~ spl68_248)), inference(subsumption_resolution, [], [f7678, f549])).
fof(f549, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (~ (e23 = e24) & ~ (e22 = e24) & ~ (e22 = e23) & ~ (e21 = e24) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e24) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax10)).
fof(f7678, plain, ((e21 = e22) | (~ spl68_247 | ~ spl68_248)), inference(forward_demodulation, [], [f1988, f1984])).
fof(f1988, plain, ((e22 = op2(e20, e21)) | ~ spl68_248), inference(avatar_component_clause, [], [f1986])).
fof(f1986, plain, (spl68_248 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_248])])).
fof(f7623, plain, (~ spl68_199 | ~ spl68_219 | spl68_384), inference(avatar_contradiction_clause, [], [f7622])).
fof(f7622, plain, ($false | (~ spl68_199 | ~ spl68_219 | spl68_384)), inference(subsumption_resolution, [], [f7621, f1866])).
fof(f1866, plain, ((e23 = op2(e21, e22)) | ~ spl68_219), inference(avatar_component_clause, [], [f1864])).
fof(f1864, plain, (spl68_219 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_219])])).
fof(f7621, plain, (~ (e23 = op2(e21, e22)) | (~ spl68_199 | spl68_384)), inference(forward_demodulation, [], [f2698, f1782])).
fof(f1782, plain, ((e23 = op2(e22, e21)) | ~ spl68_199), inference(avatar_component_clause, [], [f1780])).
fof(f1780, plain, (spl68_199 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_199])])).
fof(f2698, plain, (~ (op2(e21, e22) = op2(e22, e21)) | spl68_384), inference(avatar_component_clause, [], [f2696])).
fof(f2696, plain, (spl68_384 <=> (op2(e21, e22) = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_384])])).
fof(f7619, plain, (~ spl68_166 | ~ spl68_219 | ~ spl68_397), inference(avatar_contradiction_clause, [], [f7618])).
fof(f7618, plain, ($false | (~ spl68_166 | ~ spl68_219 | ~ spl68_397)), inference(subsumption_resolution, [], [f7617, f545])).
fof(f545, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f10])).
fof(f7617, plain, ((e20 = e21) | (~ spl68_166 | ~ spl68_219 | ~ spl68_397)), inference(forward_demodulation, [], [f7616, f1644])).
fof(f1644, plain, ((e20 = op2(e23, e22)) | ~ spl68_166), inference(avatar_component_clause, [], [f1642])).
fof(f1642, plain, (spl68_166 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_166])])).
fof(f7616, plain, ((e21 = op2(e23, e22)) | (~ spl68_219 | ~ spl68_397)), inference(forward_demodulation, [], [f2761, f1866])).
fof(f2761, plain, ((e21 = op2(op2(e21, e22), e22)) | ~ spl68_397), inference(avatar_component_clause, [], [f2759])).
fof(f2759, plain, (spl68_397 <=> (e21 = op2(op2(e21, e22), e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_397])])).
fof(f7605, plain, (~ spl68_218 | ~ spl68_243), inference(avatar_split_clause, [], [f6232, f1965, f1860])).
fof(f1860, plain, (spl68_218 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_218])])).
fof(f6232, plain, (~ (e22 = op2(e21, e22)) | ~ spl68_243), inference(backward_demodulation, [], [f455, f1967])).
fof(f455, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f8])).
fof(f7595, plain, (~ spl68_186 | spl68_238 | ~ spl68_378), inference(avatar_contradiction_clause, [], [f7594])).
fof(f7594, plain, ($false | (~ spl68_186 | spl68_238 | ~ spl68_378)), inference(subsumption_resolution, [], [f7590, f1945])).
fof(f1945, plain, (~ (e22 = op2(e20, e23)) | spl68_238), inference(avatar_component_clause, [], [f1944])).
fof(f1944, plain, (spl68_238 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_238])])).
fof(f7590, plain, ((e22 = op2(e20, e23)) | (~ spl68_186 | ~ spl68_378)), inference(backward_demodulation, [], [f2667, f1728])).
fof(f1728, plain, ((e20 = op2(e22, e23)) | ~ spl68_186), inference(avatar_component_clause, [], [f1726])).
fof(f2667, plain, ((e22 = op2(op2(e22, e23), e23)) | ~ spl68_378), inference(avatar_component_clause, [], [f2665])).
fof(f2665, plain, (spl68_378 <=> (e22 = op2(op2(e22, e23), e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_378])])).
fof(f7487, plain, (~ spl68_215 | ~ spl68_175 | spl68_368), inference(avatar_split_clause, [], [f7240, f2617, f1679, f1847])).
fof(f2617, plain, (spl68_368 <=> (op2(e21, e23) = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_368])])).
fof(f7240, plain, (~ (e24 = op2(e21, e23)) | (~ spl68_175 | spl68_368)), inference(forward_demodulation, [], [f2619, f1681])).
fof(f2619, plain, (~ (op2(e21, e23) = op2(e23, e21)) | spl68_368), inference(avatar_component_clause, [], [f2617])).
fof(f7460, plain, (~ spl68_144 | ~ spl68_149), inference(avatar_split_clause, [], [f7456, f1570, f1549])).
fof(f1549, plain, (spl68_144 <=> (e23 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_144])])).
fof(f7456, plain, (~ (e23 = op2(e24, e22)) | ~ spl68_149), inference(backward_demodulation, [], [f529, f1572])).
fof(f529, plain, ~ (op2(e24, e21) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f7453, plain, (~ spl68_142 | ~ spl68_182 | spl68_347), inference(avatar_split_clause, [], [f7450, f2515, f1709, f1541])).
fof(f2515, plain, (spl68_347 <=> (op2(e22, e24) = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_347])])).
fof(f7450, plain, (~ (e21 = op2(e24, e22)) | (~ spl68_182 | spl68_347)), inference(backward_demodulation, [], [f2517, f1711])).
fof(f2517, plain, (~ (op2(e22, e24) = op2(e24, e22)) | spl68_347), inference(avatar_component_clause, [], [f2515])).
fof(f7443, plain, (~ spl68_146 | ~ spl68_196), inference(avatar_split_clause, [], [f7435, f1768, f1558])).
fof(f1558, plain, (spl68_146 <=> (e20 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_146])])).
fof(f7435, plain, (~ (e20 = op2(e24, e21)) | ~ spl68_196), inference(backward_demodulation, [], [f453, f1770])).
fof(f453, plain, ~ (op2(e22, e21) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f7431, plain, (spl68_142 | ~ spl68_220 | ~ spl68_397), inference(avatar_split_clause, [], [f7430, f2759, f1868, f1541])).
fof(f7430, plain, ((e21 = op2(e24, e22)) | (~ spl68_220 | ~ spl68_397)), inference(forward_demodulation, [], [f2761, f1870])).
fof(f7394, plain, (~ spl68_591 | ~ spl68_220), inference(avatar_split_clause, [], [f7393, f1868, f3968])).
fof(f7393, plain, (~ (e24 = h3(e11)) | ~ spl68_220), inference(forward_demodulation, [], [f2908, f1870])).
fof(f2908, plain, ~ (op2(e21, e22) = h3(e11)), inference(backward_demodulation, [], [f459, f750])).
fof(f459, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f7387, plain, (~ spl68_196 | ~ spl68_66 | ~ spl68_440 | ~ spl68_444 | spl68_539), inference(avatar_split_clause, [], [f7213, f3643, f3086, f3064, f1172, f1768])).
fof(f3643, plain, (spl68_539 <=> (h4(op1(e12, e11)) = op2(h4(e12), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl68_539])])).
fof(f7213, plain, (~ (e20 = op2(e22, e21)) | (~ spl68_66 | ~ spl68_440 | ~ spl68_444 | spl68_539)), inference(forward_demodulation, [], [f7212, f2942])).
fof(f7212, plain, (~ (op2(e22, e21) = h4(e10)) | (~ spl68_66 | ~ spl68_440 | ~ spl68_444 | spl68_539)), inference(forward_demodulation, [], [f7211, f1174])).
fof(f7211, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | (~ spl68_440 | ~ spl68_444 | spl68_539)), inference(forward_demodulation, [], [f7210, f3065])).
fof(f7210, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | (~ spl68_444 | spl68_539)), inference(forward_demodulation, [], [f3645, f3087])).
fof(f3645, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | spl68_539), inference(avatar_component_clause, [], [f3643])).
fof(f7369, plain, (~ spl68_211 | spl68_237 | ~ spl68_394), inference(avatar_contradiction_clause, [], [f7368])).
fof(f7368, plain, ($false | (~ spl68_211 | spl68_237 | ~ spl68_394)), inference(subsumption_resolution, [], [f7367, f1941])).
fof(f1941, plain, (~ (e21 = op2(e20, e23)) | spl68_237), inference(avatar_component_clause, [], [f1940])).
fof(f1940, plain, (spl68_237 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_237])])).
fof(f7367, plain, ((e21 = op2(e20, e23)) | (~ spl68_211 | ~ spl68_394)), inference(forward_demodulation, [], [f2746, f1833])).
fof(f2746, plain, ((e21 = op2(op2(e21, e23), e23)) | ~ spl68_394), inference(avatar_component_clause, [], [f2744])).
fof(f2744, plain, (spl68_394 <=> (e21 = op2(op2(e21, e23), e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_394])])).
fof(f7338, plain, (~ spl68_175 | ~ spl68_209 | spl68_390), inference(avatar_contradiction_clause, [], [f7337])).
fof(f7337, plain, ($false | (~ spl68_175 | ~ spl68_209 | spl68_390)), inference(subsumption_resolution, [], [f7336, f1681])).
fof(f7336, plain, (~ (e24 = op2(e23, e21)) | (~ spl68_209 | spl68_390)), inference(forward_demodulation, [], [f2726, f1824])).
fof(f1824, plain, ((e23 = op2(e21, e24)) | ~ spl68_209), inference(avatar_component_clause, [], [f1822])).
fof(f1822, plain, (spl68_209 <=> (e23 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_209])])).
fof(f2726, plain, (~ (e24 = op2(op2(e21, e24), e21)) | spl68_390), inference(avatar_component_clause, [], [f2724])).
fof(f2724, plain, (spl68_390 <=> (e24 = op2(op2(e21, e24), e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_390])])).
fof(f7322, plain, (~ spl68_144 | ~ spl68_190 | spl68_377), inference(avatar_contradiction_clause, [], [f7321])).
fof(f7321, plain, ($false | (~ spl68_144 | ~ spl68_190 | spl68_377)), inference(subsumption_resolution, [], [f7320, f1551])).
fof(f1551, plain, ((e23 = op2(e24, e22)) | ~ spl68_144), inference(avatar_component_clause, [], [f1549])).
fof(f7320, plain, (~ (e23 = op2(e24, e22)) | (~ spl68_190 | spl68_377)), inference(forward_demodulation, [], [f2662, f1744])).
fof(f2662, plain, (~ (e23 = op2(op2(e22, e23), e22)) | spl68_377), inference(avatar_component_clause, [], [f2660])).
fof(f2660, plain, (spl68_377 <=> (e23 = op2(op2(e22, e23), e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_377])])).
fof(f7305, plain, (~ spl68_203 | ~ spl68_243 | spl68_388), inference(avatar_contradiction_clause, [], [f7304])).
fof(f7304, plain, ($false | (~ spl68_203 | ~ spl68_243 | spl68_388)), inference(subsumption_resolution, [], [f7303, f1967])).
fof(f7303, plain, (~ (e22 = op2(e20, e22)) | (~ spl68_203 | spl68_388)), inference(forward_demodulation, [], [f2717, f1799])).
fof(f2717, plain, (~ (op2(e20, e22) = op2(e22, e20)) | spl68_388), inference(avatar_component_clause, [], [f2715])).
fof(f2715, plain, (spl68_388 <=> (op2(e20, e22) = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_388])])).
fof(f7299, plain, (spl68_191 | ~ spl68_243 | ~ spl68_412), inference(avatar_contradiction_clause, [], [f7298])).
fof(f7298, plain, ($false | (spl68_191 | ~ spl68_243 | ~ spl68_412)), inference(subsumption_resolution, [], [f7297, f1748])).
fof(f1748, plain, (~ (e20 = op2(e22, e22)) | spl68_191), inference(avatar_component_clause, [], [f1747])).
fof(f1747, plain, (spl68_191 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_191])])).
fof(f7297, plain, ((e20 = op2(e22, e22)) | (~ spl68_243 | ~ spl68_412)), inference(forward_demodulation, [], [f2836, f1967])).
fof(f2836, plain, ((e20 = op2(op2(e20, e22), e22)) | ~ spl68_412), inference(avatar_component_clause, [], [f2834])).
fof(f2834, plain, (spl68_412 <=> (e20 = op2(op2(e20, e22), e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_412])])).
fof(f7284, plain, (spl68_173 | ~ spl68_199 | ~ spl68_383), inference(avatar_contradiction_clause, [], [f7283])).
fof(f7283, plain, ($false | (spl68_173 | ~ spl68_199 | ~ spl68_383)), inference(subsumption_resolution, [], [f7282, f1672])).
fof(f1672, plain, (~ (e22 = op2(e23, e21)) | spl68_173), inference(avatar_component_clause, [], [f1671])).
fof(f1671, plain, (spl68_173 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_173])])).
fof(f7282, plain, ((e22 = op2(e23, e21)) | (~ spl68_199 | ~ spl68_383)), inference(forward_demodulation, [], [f2693, f1782])).
fof(f2693, plain, ((e22 = op2(op2(e22, e21), e21)) | ~ spl68_383), inference(avatar_component_clause, [], [f2691])).
fof(f2691, plain, (spl68_383 <=> (e22 = op2(op2(e22, e21), e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_383])])).
fof(f7266, plain, (~ spl68_181 | spl68_233 | ~ spl68_375), inference(avatar_contradiction_clause, [], [f7265])).
fof(f7265, plain, ($false | (~ spl68_181 | spl68_233 | ~ spl68_375)), inference(subsumption_resolution, [], [f7264, f1924])).
fof(f1924, plain, (~ (e22 = op2(e20, e24)) | spl68_233), inference(avatar_component_clause, [], [f1923])).
fof(f1923, plain, (spl68_233 <=> (e22 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_233])])).
fof(f7264, plain, ((e22 = op2(e20, e24)) | (~ spl68_181 | ~ spl68_375)), inference(forward_demodulation, [], [f2652, f1707])).
fof(f1707, plain, ((e20 = op2(e22, e24)) | ~ spl68_181), inference(avatar_component_clause, [], [f1705])).
fof(f1705, plain, (spl68_181 <=> (e20 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_181])])).
fof(f2652, plain, ((e22 = op2(op2(e22, e24), e24)) | ~ spl68_375), inference(avatar_component_clause, [], [f2650])).
fof(f2650, plain, (spl68_375 <=> (e22 = op2(op2(e22, e24), e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_375])])).
fof(f7228, plain, (~ spl68_36 | ~ spl68_113 | ~ spl68_284), inference(avatar_contradiction_clause, [], [f7227])).
fof(f7227, plain, ($false | (~ spl68_36 | ~ spl68_113 | ~ spl68_284)), inference(subsumption_resolution, [], [f7226, f542])).
fof(f542, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f9])).
fof(f7226, plain, ((e12 = e13) | (~ spl68_36 | ~ spl68_113 | ~ spl68_284)), inference(forward_demodulation, [], [f7225, f1371])).
fof(f7225, plain, ((e13 = op1(e10, e12)) | (~ spl68_36 | ~ spl68_284)), inference(forward_demodulation, [], [f2204, f1048])).
fof(f2204, plain, ((e13 = op1(op1(e13, e12), e12)) | ~ spl68_284), inference(avatar_component_clause, [], [f2202])).
fof(f2202, plain, (spl68_284 <=> (e13 = op1(op1(e13, e12), e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_284])])).
fof(f7218, plain, (~ spl68_155 | ~ spl68_235 | spl68_355), inference(avatar_contradiction_clause, [], [f7217])).
fof(f7217, plain, ($false | (~ spl68_155 | ~ spl68_235 | spl68_355)), inference(subsumption_resolution, [], [f7216, f1933])).
fof(f7216, plain, (~ (e24 = op2(e20, e24)) | (~ spl68_155 | spl68_355)), inference(forward_demodulation, [], [f2555, f1597])).
fof(f2555, plain, (~ (op2(e20, e24) = op2(e24, e20)) | spl68_355), inference(avatar_component_clause, [], [f2553])).
fof(f2553, plain, (spl68_355 <=> (op2(e20, e24) = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_355])])).
fof(f7194, plain, (~ spl68_97 | ~ spl68_117 | spl68_324), inference(avatar_contradiction_clause, [], [f7193])).
fof(f7193, plain, ($false | (~ spl68_97 | ~ spl68_117 | spl68_324)), inference(subsumption_resolution, [], [f7192, f1388])).
fof(f7192, plain, (~ (e11 = op1(e10, e11)) | (~ spl68_97 | spl68_324)), inference(forward_demodulation, [], [f2401, f1304])).
fof(f2401, plain, (~ (op1(e10, e11) = op1(e11, e10)) | spl68_324), inference(avatar_component_clause, [], [f2399])).
fof(f2399, plain, (spl68_324 <=> (op1(e10, e11) = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_324])])).
fof(f7182, plain, (~ spl68_227 | ~ spl68_247 | spl68_403), inference(avatar_contradiction_clause, [], [f7181])).
fof(f7181, plain, ($false | (~ spl68_227 | ~ spl68_247 | spl68_403)), inference(subsumption_resolution, [], [f7180, f1984])).
fof(f7180, plain, (~ (e21 = op2(e20, e21)) | (~ spl68_227 | spl68_403)), inference(forward_demodulation, [], [f2792, f1900])).
fof(f2792, plain, (~ (op2(e20, e21) = op2(e21, e20)) | spl68_403), inference(avatar_component_clause, [], [f2790])).
fof(f2790, plain, (spl68_403 <=> (op2(e20, e21) = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_403])])).
fof(f7169, plain, (~ spl68_79 | ~ spl68_209 | ~ spl68_444 | spl68_554), inference(avatar_contradiction_clause, [], [f7168])).
fof(f7168, plain, ($false | (~ spl68_79 | ~ spl68_209 | ~ spl68_444 | spl68_554)), inference(subsumption_resolution, [], [f7167, f1824])).
fof(f7167, plain, (~ (e23 = op2(e21, e24)) | (~ spl68_79 | ~ spl68_444 | spl68_554)), inference(forward_demodulation, [], [f7166, f753])).
fof(f7166, plain, (~ (op2(e21, e24) = h4(e13)) | (~ spl68_79 | ~ spl68_444 | spl68_554)), inference(forward_demodulation, [], [f7165, f1228])).
fof(f7165, plain, (~ (op2(e21, e24) = h4(op1(e11, e14))) | (~ spl68_444 | spl68_554)), inference(forward_demodulation, [], [f3705, f3087])).
fof(f3705, plain, (~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | spl68_554), inference(avatar_component_clause, [], [f3703])).
fof(f3703, plain, (spl68_554 <=> (h4(op1(e11, e14)) = op2(h4(e11), e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_554])])).
fof(f7148, plain, (~ spl68_25 | ~ spl68_105 | spl68_276), inference(avatar_contradiction_clause, [], [f7147])).
fof(f7147, plain, ($false | (~ spl68_25 | ~ spl68_105 | spl68_276)), inference(subsumption_resolution, [], [f7146, f1337])).
fof(f7146, plain, (~ (e14 = op1(e10, e14)) | (~ spl68_25 | spl68_276)), inference(forward_demodulation, [], [f2164, f1001])).
fof(f2164, plain, (~ (op1(e10, e14) = op1(e14, e10)) | spl68_276), inference(avatar_component_clause, [], [f2162])).
fof(f2162, plain, (spl68_276 <=> (op1(e10, e14) = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_276])])).
fof(f7141, plain, (~ spl68_179 | ~ spl68_239 | spl68_372), inference(avatar_contradiction_clause, [], [f7140])).
fof(f7140, plain, ($false | (~ spl68_179 | ~ spl68_239 | spl68_372)), inference(subsumption_resolution, [], [f7139, f1950])).
fof(f7139, plain, (~ (e23 = op2(e20, e23)) | (~ spl68_179 | spl68_372)), inference(forward_demodulation, [], [f2638, f1698])).
fof(f1698, plain, ((e23 = op2(e23, e20)) | ~ spl68_179), inference(avatar_component_clause, [], [f1696])).
fof(f1696, plain, (spl68_179 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_179])])).
fof(f2638, plain, (~ (op2(e20, e23) = op2(e23, e20)) | spl68_372), inference(avatar_component_clause, [], [f2636])).
fof(f2636, plain, (spl68_372 <=> (op2(e20, e23) = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_372])])).
fof(f7127, plain, (~ spl68_90 | ~ spl68_220 | ~ spl68_440 | ~ spl68_444 | spl68_538), inference(avatar_contradiction_clause, [], [f7126])).
fof(f7126, plain, ($false | (~ spl68_90 | ~ spl68_220 | ~ spl68_440 | ~ spl68_444 | spl68_538)), inference(subsumption_resolution, [], [f7125, f1870])).
fof(f7125, plain, (~ (e24 = op2(e21, e22)) | (~ spl68_90 | ~ spl68_440 | ~ spl68_444 | spl68_538)), inference(forward_demodulation, [], [f7124, f2919])).
fof(f7124, plain, (~ (op2(e21, e22) = h4(e14)) | (~ spl68_90 | ~ spl68_440 | ~ spl68_444 | spl68_538)), inference(forward_demodulation, [], [f7123, f1274])).
fof(f7123, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | (~ spl68_440 | ~ spl68_444 | spl68_538)), inference(forward_demodulation, [], [f7122, f3087])).
fof(f7122, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | (~ spl68_440 | spl68_538)), inference(forward_demodulation, [], [f3641, f3065])).
fof(f3641, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | spl68_538), inference(avatar_component_clause, [], [f3639])).
fof(f3639, plain, (spl68_538 <=> (h4(op1(e11, e12)) = op2(h4(e11), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl68_538])])).
fof(f7101, plain, (~ spl68_166 | ~ spl68_243 | ~ spl68_363), inference(avatar_contradiction_clause, [], [f7100])).
fof(f7100, plain, ($false | (~ spl68_166 | ~ spl68_243 | ~ spl68_363)), inference(subsumption_resolution, [], [f7099, f552])).
fof(f552, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f10])).
fof(f7099, plain, ((e22 = e23) | (~ spl68_166 | ~ spl68_243 | ~ spl68_363)), inference(forward_demodulation, [], [f7098, f1967])).
fof(f7098, plain, ((e23 = op2(e20, e22)) | (~ spl68_166 | ~ spl68_363)), inference(forward_demodulation, [], [f2595, f1644])).
fof(f2595, plain, ((e23 = op2(op2(e23, e22), e22)) | ~ spl68_363), inference(avatar_component_clause, [], [f2593])).
fof(f2593, plain, (spl68_363 <=> (e23 = op2(op2(e23, e22), e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_363])])).
fof(f7093, plain, (~ spl68_49 | ~ spl68_179 | spl68_550), inference(avatar_contradiction_clause, [], [f7092])).
fof(f7092, plain, ($false | (~ spl68_49 | ~ spl68_179 | spl68_550)), inference(subsumption_resolution, [], [f7091, f1698])).
fof(f7091, plain, (~ (e23 = op2(e23, e20)) | (~ spl68_49 | spl68_550)), inference(forward_demodulation, [], [f7090, f753])).
fof(f7090, plain, (~ (op2(e23, e20) = h4(e13)) | (~ spl68_49 | spl68_550)), inference(forward_demodulation, [], [f3689, f1102])).
fof(f3689, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | spl68_550), inference(avatar_component_clause, [], [f3687])).
fof(f3687, plain, (spl68_550 <=> (op2(e23, e20) = h4(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl68_550])])).
fof(f7066, plain, (~ spl68_219 | ~ spl68_209), inference(avatar_split_clause, [], [f6876, f1822, f1864])).
fof(f6876, plain, (~ (e23 = op2(e21, e22)) | ~ spl68_209), inference(forward_demodulation, [], [f503, f1824])).
fof(f503, plain, ~ (op2(e21, e22) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f7065, plain, (~ spl68_219 | ~ spl68_144), inference(avatar_split_clause, [], [f7064, f1549, f1864])).
fof(f7064, plain, (~ (e23 = op2(e21, e22)) | ~ spl68_144), inference(forward_demodulation, [], [f461, f1551])).
fof(f461, plain, ~ (op2(e21, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f7053, plain, (~ spl68_15 | ~ spl68_25), inference(avatar_split_clause, [], [f7052, f999, f957])).
fof(f957, plain, (spl68_15 <=> (e14 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_15])])).
fof(f7052, plain, (~ (e14 = op1(e14, e12)) | ~ spl68_25), inference(backward_demodulation, [], [f426, f1001])).
fof(f426, plain, ~ (op1(e14, e10) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f7050, plain, (~ spl68_12 | ~ spl68_52 | spl68_268), inference(avatar_split_clause, [], [f7049, f2124, f1113, f945])).
fof(f2124, plain, (spl68_268 <=> (op1(e12, e14) = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_268])])).
fof(f7049, plain, (~ (e11 = op1(e14, e12)) | (~ spl68_52 | spl68_268)), inference(backward_demodulation, [], [f2126, f1115])).
fof(f2126, plain, (~ (op1(e12, e14) = op1(e14, e12)) | spl68_268), inference(avatar_component_clause, [], [f2124])).
fof(f7023, plain, (~ spl68_87 | ~ spl68_97), inference(avatar_split_clause, [], [f7017, f1302, f1260])).
fof(f1260, plain, (spl68_87 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_87])])).
fof(f7017, plain, (~ (e11 = op1(e11, e12)) | ~ spl68_97), inference(backward_demodulation, [], [f396, f1304])).
fof(f396, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f7003, plain, (~ spl68_67 | ~ spl68_117), inference(avatar_split_clause, [], [f6998, f1386, f1176])).
fof(f1176, plain, (spl68_67 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_67])])).
fof(f6998, plain, (~ (e11 = op1(e12, e11)) | ~ spl68_117), inference(backward_demodulation, [], [f346, f1388])).
fof(f346, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f6951, plain, (~ spl68_146 | ~ spl68_247 | ~ spl68_350), inference(avatar_contradiction_clause, [], [f6950])).
fof(f6950, plain, ($false | (~ spl68_146 | ~ spl68_247 | ~ spl68_350)), inference(subsumption_resolution, [], [f6949, f551])).
fof(f551, plain, ~ (e21 = e24), inference(cnf_transformation, [], [f10])).
fof(f6949, plain, ((e21 = e24) | (~ spl68_146 | ~ spl68_247 | ~ spl68_350)), inference(forward_demodulation, [], [f6948, f1984])).
fof(f6948, plain, ((e24 = op2(e20, e21)) | (~ spl68_146 | ~ spl68_350)), inference(backward_demodulation, [], [f2531, f1560])).
fof(f1560, plain, ((e20 = op2(e24, e21)) | ~ spl68_146), inference(avatar_component_clause, [], [f1558])).
fof(f2531, plain, ((e24 = op2(op2(e24, e21), e21)) | ~ spl68_350), inference(avatar_component_clause, [], [f2529])).
fof(f2529, plain, (spl68_350 <=> (e24 = op2(op2(e24, e21), e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_350])])).
fof(f6931, plain, (~ spl68_227 | ~ spl68_230), inference(avatar_contradiction_clause, [], [f6930])).
fof(f6930, plain, ($false | (~ spl68_227 | ~ spl68_230)), inference(subsumption_resolution, [], [f6929, f551])).
fof(f6929, plain, ((e21 = e24) | (~ spl68_227 | ~ spl68_230)), inference(forward_demodulation, [], [f1912, f1900])).
fof(f1912, plain, ((e24 = op2(e21, e20)) | ~ spl68_230), inference(avatar_component_clause, [], [f1910])).
fof(f1910, plain, (spl68_230 <=> (e24 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_230])])).
fof(f6920, plain, (~ spl68_142 | ~ spl68_432), inference(avatar_split_clause, [], [f6914, f3021, f1541])).
fof(f3021, plain, (spl68_432 <=> (e21 = h5(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_432])])).
fof(f6914, plain, (~ (e21 = op2(e24, e22)) | ~ spl68_432), inference(backward_demodulation, [], [f2950, f3022])).
fof(f3022, plain, ((e21 = h5(e11)) | ~ spl68_432), inference(avatar_component_clause, [], [f3021])).
fof(f2950, plain, ~ (op2(e24, e22) = h5(e11)), inference(backward_demodulation, [], [f533, f760])).
fof(f760, plain, (op2(e24, e24) = h5(e11)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((op2(e24, op2(e24, e24)) = h5(e14)) & (op2(op2(e24, op2(e24, e24)), e24) = h5(e12)) & (op2(e24, e24) = h5(e11)) & (h5(e10) = op2(e24, op2(op2(e24, op2(e24, e24)), e24))) & (e24 = h5(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax20)).
fof(f533, plain, ~ (op2(e24, e22) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f6907, plain, (~ spl68_182 | ~ spl68_461), inference(avatar_split_clause, [], [f6901, f3176, f1709])).
fof(f3176, plain, (spl68_461 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_461])])).
fof(f6901, plain, (~ (e21 = op2(e22, e24)) | ~ spl68_461), inference(backward_demodulation, [], [f2914, f3177])).
fof(f3177, plain, ((e21 = h3(e11)) | ~ spl68_461), inference(avatar_component_clause, [], [f3176])).
fof(f2914, plain, ~ (op2(e22, e24) = h3(e11)), inference(backward_demodulation, [], [f513, f750])).
fof(f513, plain, ~ (op2(e22, e22) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f6906, plain, (~ spl68_142 | ~ spl68_461), inference(avatar_split_clause, [], [f6900, f3176, f1541])).
fof(f6900, plain, (~ (e21 = op2(e24, e22)) | ~ spl68_461), inference(backward_demodulation, [], [f2910, f3177])).
fof(f2910, plain, ~ (op2(e24, e22) = h3(e11)), inference(backward_demodulation, [], [f463, f750])).
fof(f463, plain, ~ (op2(e22, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f6894, plain, (~ spl68_81 | ~ spl68_211 | ~ spl68_444 | spl68_555), inference(avatar_contradiction_clause, [], [f6893])).
fof(f6893, plain, ($false | (~ spl68_81 | ~ spl68_211 | ~ spl68_444 | spl68_555)), inference(subsumption_resolution, [], [f6892, f1833])).
fof(f6892, plain, (~ (e20 = op2(e21, e23)) | (~ spl68_81 | ~ spl68_444 | spl68_555)), inference(forward_demodulation, [], [f6891, f2942])).
fof(f6891, plain, (~ (op2(e21, e23) = h4(e10)) | (~ spl68_81 | ~ spl68_444 | spl68_555)), inference(forward_demodulation, [], [f6890, f1237])).
fof(f6890, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | (~ spl68_444 | spl68_555)), inference(forward_demodulation, [], [f3709, f3087])).
fof(f3709, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | spl68_555), inference(avatar_component_clause, [], [f3707])).
fof(f3707, plain, (spl68_555 <=> (h4(op1(e11, e13)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_555])])).
fof(f6866, plain, (~ spl68_119 | ~ spl68_109), inference(avatar_split_clause, [], [f6865, f1352, f1394])).
fof(f1394, plain, (spl68_119 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_119])])).
fof(f6865, plain, (~ (e13 = op1(e10, e11)) | ~ spl68_109), inference(forward_demodulation, [], [f390, f1354])).
fof(f390, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f6827, plain, (~ spl68_79 | ~ spl68_94), inference(avatar_split_clause, [], [f6826, f1289, f1226])).
fof(f1289, plain, (spl68_94 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_94])])).
fof(f6826, plain, (~ (e13 = op1(e11, e14)) | ~ spl68_94), inference(backward_demodulation, [], [f401, f1291])).
fof(f1291, plain, ((e13 = op1(e11, e11)) | ~ spl68_94), inference(avatar_component_clause, [], [f1289])).
fof(f401, plain, ~ (op1(e11, e11) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f6823, plain, (~ spl68_59 | ~ spl68_109), inference(avatar_split_clause, [], [f6822, f1352, f1142])).
fof(f1142, plain, (spl68_59 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_59])])).
fof(f6822, plain, (~ (e13 = op1(e12, e13)) | ~ spl68_109), inference(backward_demodulation, [], [f366, f1354])).
fof(f366, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f6792, plain, (~ spl68_8 | ~ spl68_130), inference(avatar_contradiction_clause, [], [f6791])).
fof(f6791, plain, ($false | (~ spl68_8 | ~ spl68_130)), inference(subsumption_resolution, [], [f6790, f542])).
fof(f6790, plain, ((e12 = e13) | (~ spl68_8 | ~ spl68_130)), inference(forward_demodulation, [], [f6775, f930])).
fof(f6775, plain, ((e13 = op1(e14, e13)) | ~ spl68_130), inference(backward_demodulation, [], [f194, f1442])).
fof(f1442, plain, ((e14 = unit1) | ~ spl68_130), inference(avatar_component_clause, [], [f1440])).
fof(f1440, plain, (spl68_130 <=> (e14 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl68_130])])).
fof(f6768, plain, (~ spl68_131 | ~ spl68_134), inference(avatar_contradiction_clause, [], [f6767])).
fof(f6767, plain, ($false | (~ spl68_131 | ~ spl68_134)), inference(subsumption_resolution, [], [f6765, f547])).
fof(f547, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f10])).
fof(f6765, plain, ((e20 = e23) | (~ spl68_131 | ~ spl68_134)), inference(backward_demodulation, [], [f1509, f1497])).
fof(f1497, plain, ((e20 = op2(e24, e24)) | ~ spl68_131), inference(avatar_component_clause, [], [f1495])).
fof(f1495, plain, (spl68_131 <=> (e20 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_131])])).
fof(f1509, plain, ((e23 = op2(e24, e24)) | ~ spl68_134), inference(avatar_component_clause, [], [f1507])).
fof(f1507, plain, (spl68_134 <=> (e23 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_134])])).
fof(f6723, plain, (~ spl68_206 | ~ spl68_211), inference(avatar_split_clause, [], [f6718, f1831, f1810])).
fof(f6718, plain, (~ (e20 = op2(e21, e24)) | ~ spl68_211), inference(backward_demodulation, [], [f504, f1833])).
fof(f504, plain, ~ (op2(e21, e23) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f6674, plain, (~ spl68_456 | ~ spl68_243), inference(avatar_split_clause, [], [f6673, f1965, f3151])).
fof(f3151, plain, (spl68_456 <=> (e22 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_456])])).
fof(f6673, plain, (~ (e22 = h3(e11)) | ~ spl68_243), inference(forward_demodulation, [], [f2907, f1967])).
fof(f2907, plain, ~ (op2(e20, e22) = h3(e11)), inference(backward_demodulation, [], [f456, f750])).
fof(f456, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f6665, plain, (~ spl68_197 | ~ spl68_247), inference(avatar_split_clause, [], [f6664, f1982, f1772])).
fof(f1772, plain, (spl68_197 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_197])])).
fof(f6664, plain, (~ (e21 = op2(e22, e21)) | ~ spl68_247), inference(forward_demodulation, [], [f446, f1984])).
fof(f446, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f6663, plain, (~ spl68_198 | ~ spl68_203), inference(avatar_split_clause, [], [f6662, f1797, f1776])).
fof(f1776, plain, (spl68_198 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_198])])).
fof(f6662, plain, (~ (e22 = op2(e22, e21)) | ~ spl68_203), inference(forward_demodulation, [], [f505, f1799])).
fof(f505, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f6573, plain, (~ spl68_97 | ~ spl68_100), inference(avatar_contradiction_clause, [], [f6572])).
fof(f6572, plain, ($false | (~ spl68_97 | ~ spl68_100)), inference(subsumption_resolution, [], [f6571, f541])).
fof(f6571, plain, ((e11 = e14) | (~ spl68_97 | ~ spl68_100)), inference(forward_demodulation, [], [f1316, f1304])).
fof(f1316, plain, ((e14 = op1(e11, e10)) | ~ spl68_100), inference(avatar_component_clause, [], [f1314])).
fof(f1314, plain, (spl68_100 <=> (e14 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_100])])).
fof(f6552, plain, (~ spl68_32 | ~ spl68_129), inference(avatar_contradiction_clause, [], [f6551])).
fof(f6551, plain, ($false | (~ spl68_32 | ~ spl68_129)), inference(subsumption_resolution, [], [f6550, f540])).
fof(f540, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f9])).
fof(f6550, plain, ((e11 = e13) | (~ spl68_32 | ~ spl68_129)), inference(forward_demodulation, [], [f6530, f1031])).
fof(f6530, plain, ((e13 = op1(e13, e13)) | ~ spl68_129), inference(backward_demodulation, [], [f195, f1438])).
fof(f1438, plain, ((e13 = unit1) | ~ spl68_129), inference(avatar_component_clause, [], [f1436])).
fof(f1436, plain, (spl68_129 <=> (e13 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl68_129])])).
fof(f6517, plain, (~ spl68_144 | ~ spl68_166 | ~ spl68_346), inference(avatar_contradiction_clause, [], [f6516])).
fof(f6516, plain, ($false | (~ spl68_144 | ~ spl68_166 | ~ spl68_346)), inference(subsumption_resolution, [], [f6515, f548])).
fof(f548, plain, ~ (e20 = e24), inference(cnf_transformation, [], [f10])).
fof(f6515, plain, ((e20 = e24) | (~ spl68_144 | ~ spl68_166 | ~ spl68_346)), inference(forward_demodulation, [], [f6511, f1644])).
fof(f6511, plain, ((e24 = op2(e23, e22)) | (~ spl68_144 | ~ spl68_346)), inference(backward_demodulation, [], [f2512, f1551])).
fof(f2512, plain, ((e24 = op2(op2(e24, e22), e22)) | ~ spl68_346), inference(avatar_component_clause, [], [f2510])).
fof(f2510, plain, (spl68_346 <=> (e24 = op2(op2(e24, e22), e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_346])])).
fof(f6494, plain, (~ spl68_203 | ~ spl68_205), inference(avatar_contradiction_clause, [], [f6493])).
fof(f6493, plain, ($false | (~ spl68_203 | ~ spl68_205)), inference(subsumption_resolution, [], [f6492, f553])).
fof(f553, plain, ~ (e22 = e24), inference(cnf_transformation, [], [f10])).
fof(f6492, plain, ((e22 = e24) | (~ spl68_203 | ~ spl68_205)), inference(forward_demodulation, [], [f1807, f1799])).
fof(f1807, plain, ((e24 = op2(e22, e20)) | ~ spl68_205), inference(avatar_component_clause, [], [f1805])).
fof(f1805, plain, (spl68_205 <=> (e24 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_205])])).
fof(f6450, plain, (~ spl68_476 | ~ spl68_93 | ~ spl68_440 | ~ spl68_444 | spl68_537), inference(avatar_split_clause, [], [f5873, f3635, f3086, f3064, f1285, f3251])).
fof(f3251, plain, (spl68_476 <=> (e22 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_476])])).
fof(f1285, plain, (spl68_93 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_93])])).
fof(f3635, plain, (spl68_537 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl68_537])])).
fof(f5873, plain, (~ (e22 = h2(e11)) | (~ spl68_93 | ~ spl68_440 | ~ spl68_444 | spl68_537)), inference(forward_demodulation, [], [f5872, f3065])).
fof(f5872, plain, (~ (h2(e11) = h4(e12)) | (~ spl68_93 | ~ spl68_444 | spl68_537)), inference(backward_demodulation, [], [f5536, f1287])).
fof(f1287, plain, ((e12 = op1(e11, e11)) | ~ spl68_93), inference(avatar_component_clause, [], [f1285])).
fof(f5536, plain, (~ (h2(e11) = h4(op1(e11, e11))) | (~ spl68_444 | spl68_537)), inference(backward_demodulation, [], [f5461, f745])).
fof(f745, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e21, op2(e21, e21)) = h2(e14)) & (op2(op2(e21, op2(e21, e21)), e21) = h2(e12)) & (op2(e21, e21) = h2(e11)) & (h2(e10) = op2(e21, op2(op2(e21, op2(e21, e21)), e21))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax17)).
fof(f5461, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | (~ spl68_444 | spl68_537)), inference(forward_demodulation, [], [f3637, f3087])).
fof(f3637, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl68_537), inference(avatar_component_clause, [], [f3635])).
fof(f6447, plain, (~ spl68_217 | ~ spl68_227), inference(avatar_split_clause, [], [f6133, f1898, f1856])).
fof(f1856, plain, (spl68_217 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_217])])).
fof(f6133, plain, (~ (e21 = op2(e21, e22)) | ~ spl68_227), inference(forward_demodulation, [], [f496, f1900])).
fof(f496, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f8])).
fof(f6446, plain, (~ spl68_213 | ~ spl68_440), inference(avatar_split_clause, [], [f5945, f3064, f1839])).
fof(f1839, plain, (spl68_213 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_213])])).
fof(f5945, plain, (~ (e22 = op2(e21, e23)) | ~ spl68_440), inference(forward_demodulation, [], [f2922, f3065])).
fof(f2922, plain, ~ (op2(e21, e23) = h4(e12)), inference(backward_demodulation, [], [f471, f2920])).
fof(f471, plain, ~ (op2(e21, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f6434, plain, (~ spl68_208 | ~ spl68_158), inference(avatar_split_clause, [], [f6433, f1608, f1818])).
fof(f1818, plain, (spl68_208 <=> (e22 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_208])])).
fof(f6433, plain, (~ (e22 = op2(e21, e24)) | ~ spl68_158), inference(forward_demodulation, [], [f480, f1610])).
fof(f480, plain, ~ (op2(e21, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f6403, plain, (~ spl68_183 | ~ spl68_203), inference(avatar_split_clause, [], [f6402, f1797, f1713])).
fof(f1713, plain, (spl68_183 <=> (e22 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_183])])).
fof(f6402, plain, (~ (e22 = op2(e22, e24)) | ~ spl68_203), inference(forward_demodulation, [], [f508, f1799])).
fof(f508, plain, ~ (op2(e22, e20) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f6397, plain, (~ spl68_144 | ~ spl68_422), inference(avatar_split_clause, [], [f6184, f2971, f1549])).
fof(f2971, plain, (spl68_422 <=> (e23 = h5(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_422])])).
fof(f6184, plain, (~ (e23 = op2(e24, e22)) | ~ spl68_422), inference(backward_demodulation, [], [f2950, f2972])).
fof(f2972, plain, ((e23 = h5(e11)) | ~ spl68_422), inference(avatar_component_clause, [], [f2971])).
fof(f6339, plain, (~ spl68_14 | ~ spl68_36 | ~ spl68_267), inference(avatar_contradiction_clause, [], [f6338])).
fof(f6338, plain, ($false | (~ spl68_14 | ~ spl68_36 | ~ spl68_267)), inference(subsumption_resolution, [], [f6337, f538])).
fof(f538, plain, ~ (e10 = e14), inference(cnf_transformation, [], [f9])).
fof(f6337, plain, ((e10 = e14) | (~ spl68_14 | ~ spl68_36 | ~ spl68_267)), inference(forward_demodulation, [], [f6332, f1048])).
fof(f6332, plain, ((e14 = op1(e13, e12)) | (~ spl68_14 | ~ spl68_267)), inference(backward_demodulation, [], [f2121, f955])).
fof(f6304, plain, (~ spl68_36 | ~ spl68_128), inference(avatar_contradiction_clause, [], [f6303])).
fof(f6303, plain, ($false | (~ spl68_36 | ~ spl68_128)), inference(subsumption_resolution, [], [f6302, f537])).
fof(f537, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f9])).
fof(f6302, plain, ((e10 = e13) | (~ spl68_36 | ~ spl68_128)), inference(forward_demodulation, [], [f6284, f1048])).
fof(f6284, plain, ((e13 = op1(e13, e12)) | ~ spl68_128), inference(backward_demodulation, [], [f195, f1434])).
fof(f1434, plain, ((e12 = unit1) | ~ spl68_128), inference(avatar_component_clause, [], [f1432])).
fof(f1432, plain, (spl68_128 <=> (e12 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl68_128])])).
fof(f6261, plain, (~ spl68_145 | ~ spl68_155), inference(avatar_split_clause, [], [f6259, f1595, f1553])).
fof(f1553, plain, (spl68_145 <=> (e24 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_145])])).
fof(f6259, plain, (~ (e24 = op2(e24, e22)) | ~ spl68_155), inference(backward_demodulation, [], [f526, f1597])).
fof(f526, plain, ~ (op2(e24, e20) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f6245, plain, (~ spl68_232 | ~ spl68_235), inference(avatar_contradiction_clause, [], [f6244])).
fof(f6244, plain, ($false | (~ spl68_232 | ~ spl68_235)), inference(subsumption_resolution, [], [f6243, f551])).
fof(f6243, plain, ((e21 = e24) | (~ spl68_232 | ~ spl68_235)), inference(backward_demodulation, [], [f1933, f1921])).
fof(f1921, plain, ((e21 = op2(e20, e24)) | ~ spl68_232), inference(avatar_component_clause, [], [f1919])).
fof(f6230, plain, (~ spl68_242 | ~ spl68_247), inference(avatar_split_clause, [], [f6226, f1982, f1961])).
fof(f1961, plain, (spl68_242 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_242])])).
fof(f6226, plain, (~ (e21 = op2(e20, e22)) | ~ spl68_247), inference(backward_demodulation, [], [f489, f1984])).
fof(f489, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f8])).
fof(f6229, plain, (~ spl68_147 | ~ spl68_247), inference(avatar_split_clause, [], [f6225, f1982, f1562])).
fof(f1562, plain, (spl68_147 <=> (e21 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_147])])).
fof(f6225, plain, (~ (e21 = op2(e24, e21)) | ~ spl68_247), inference(backward_demodulation, [], [f448, f1984])).
fof(f448, plain, ~ (op2(e20, e21) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f6218, plain, (spl68_155 | ~ spl68_256), inference(avatar_split_clause, [], [f6207, f2020, f1595])).
fof(f2020, plain, (spl68_256 <=> (e20 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl68_256])])).
fof(f6207, plain, ((e24 = op2(e24, e20)) | ~ spl68_256), inference(backward_demodulation, [], [f283, f2022])).
fof(f2022, plain, ((e20 = unit2) | ~ spl68_256), inference(avatar_component_clause, [], [f2020])).
fof(f283, plain, (e24 = op2(e24, unit2)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (((e24 = unit2) | (e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)) & (e24 = op2(e24, unit2)) & (e24 = op2(unit2, e24)) & (e23 = op2(e23, unit2)) & (e23 = op2(unit2, e23)) & (e22 = op2(e22, unit2)) & (e22 = op2(unit2, e22)) & (e21 = op2(e21, unit2)) & (e21 = op2(unit2, e21)) & (e20 = op2(e20, unit2)) & (e20 = op2(unit2, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax5)).
fof(f6217, plain, (spl68_235 | ~ spl68_256), inference(avatar_split_clause, [], [f6206, f2020, f1931])).
fof(f6206, plain, ((e24 = op2(e20, e24)) | ~ spl68_256), inference(backward_demodulation, [], [f282, f2022])).
fof(f282, plain, (e24 = op2(unit2, e24)), inference(cnf_transformation, [], [f5])).
fof(f6216, plain, (spl68_239 | ~ spl68_256), inference(avatar_split_clause, [], [f6204, f2020, f1948])).
fof(f6204, plain, ((e23 = op2(e20, e23)) | ~ spl68_256), inference(backward_demodulation, [], [f280, f2022])).
fof(f280, plain, (e23 = op2(unit2, e23)), inference(cnf_transformation, [], [f5])).
fof(f6215, plain, (spl68_243 | ~ spl68_256), inference(avatar_split_clause, [], [f6202, f2020, f1965])).
fof(f6202, plain, ((e22 = op2(e20, e22)) | ~ spl68_256), inference(backward_demodulation, [], [f278, f2022])).
fof(f278, plain, (e22 = op2(unit2, e22)), inference(cnf_transformation, [], [f5])).
fof(f6214, plain, (spl68_247 | ~ spl68_256), inference(avatar_split_clause, [], [f6200, f2020, f1982])).
fof(f6200, plain, ((e21 = op2(e20, e21)) | ~ spl68_256), inference(backward_demodulation, [], [f276, f2022])).
fof(f276, plain, (e21 = op2(unit2, e21)), inference(cnf_transformation, [], [f5])).
fof(f6213, plain, (spl68_251 | ~ spl68_256), inference(avatar_split_clause, [], [f6199, f2020, f1999])).
fof(f1999, plain, (spl68_251 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_251])])).
fof(f6199, plain, ((e20 = op2(e20, e20)) | ~ spl68_256), inference(backward_demodulation, [], [f275, f2022])).
fof(f275, plain, (e20 = op2(e20, unit2)), inference(cnf_transformation, [], [f5])).
fof(f6137, plain, (~ spl68_238 | ~ spl68_440), inference(avatar_split_clause, [], [f5951, f3064, f1944])).
fof(f5951, plain, (~ (e22 = op2(e20, e23)) | ~ spl68_440), inference(forward_demodulation, [], [f2921, f3065])).
fof(f2921, plain, ~ (op2(e20, e23) = h4(e12)), inference(backward_demodulation, [], [f468, f2920])).
fof(f468, plain, ~ (op2(e20, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f6116, plain, (~ spl68_148 | ~ spl68_440), inference(avatar_split_clause, [], [f6115, f3064, f1566])).
fof(f1566, plain, (spl68_148 <=> (e22 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_148])])).
fof(f6115, plain, (~ (e22 = op2(e24, e21)) | ~ spl68_440), inference(forward_demodulation, [], [f2926, f3065])).
fof(f2926, plain, ~ (op2(e24, e21) = h4(e12)), inference(backward_demodulation, [], [f530, f2920])).
fof(f530, plain, ~ (op2(e24, e21) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f6095, plain, (~ spl68_36 | ~ spl68_38), inference(avatar_contradiction_clause, [], [f6094])).
fof(f6094, plain, ($false | (~ spl68_36 | ~ spl68_38)), inference(subsumption_resolution, [], [f6093, f536])).
fof(f536, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f9])).
fof(f6093, plain, ((e10 = e12) | (~ spl68_36 | ~ spl68_38)), inference(forward_demodulation, [], [f1056, f1048])).
fof(f1056, plain, ((e12 = op1(e13, e12)) | ~ spl68_38), inference(avatar_component_clause, [], [f1054])).
fof(f1054, plain, (spl68_38 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_38])])).
fof(f6069, plain, (~ spl68_117 | ~ spl68_118), inference(avatar_contradiction_clause, [], [f6068])).
fof(f6068, plain, ($false | (~ spl68_117 | ~ spl68_118)), inference(subsumption_resolution, [], [f6067, f539])).
fof(f539, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f9])).
fof(f6067, plain, ((e11 = e12) | (~ spl68_117 | ~ spl68_118)), inference(forward_demodulation, [], [f1392, f1388])).
fof(f1392, plain, ((e12 = op1(e10, e11)) | ~ spl68_118), inference(avatar_component_clause, [], [f1390])).
fof(f1390, plain, (spl68_118 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_118])])).
fof(f6018, plain, (~ spl68_159 | ~ spl68_179), inference(avatar_split_clause, [], [f6015, f1696, f1612])).
fof(f1612, plain, (spl68_159 <=> (e23 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_159])])).
fof(f6015, plain, (~ (e23 = op2(e23, e24)) | ~ spl68_179), inference(backward_demodulation, [], [f518, f1698])).
fof(f518, plain, ~ (op2(e23, e20) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f6014, plain, (~ spl68_181 | ~ spl68_182), inference(avatar_contradiction_clause, [], [f6013])).
fof(f6013, plain, ($false | (~ spl68_181 | ~ spl68_182)), inference(subsumption_resolution, [], [f6012, f545])).
fof(f6012, plain, ((e20 = e21) | (~ spl68_181 | ~ spl68_182)), inference(forward_demodulation, [], [f1711, f1707])).
fof(f5994, plain, (~ spl68_225 | spl68_620), inference(avatar_contradiction_clause, [], [f5993])).
fof(f5993, plain, ($false | (~ spl68_225 | spl68_620)), inference(subsumption_resolution, [], [f5992, f4143])).
fof(f4143, plain, (~ (e24 = h2(e11)) | spl68_620), inference(avatar_component_clause, [], [f4141])).
fof(f4141, plain, (spl68_620 <=> (e24 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_620])])).
fof(f5992, plain, ((e24 = h2(e11)) | ~ spl68_225), inference(backward_demodulation, [], [f745, f1891])).
fof(f1891, plain, ((e24 = op2(e21, e21)) | ~ spl68_225), inference(avatar_component_clause, [], [f1889])).
fof(f1889, plain, (spl68_225 <=> (e24 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_225])])).
fof(f5978, plain, (~ spl68_138 | ~ spl68_260), inference(avatar_contradiction_clause, [], [f5977])).
fof(f5977, plain, ($false | (~ spl68_138 | ~ spl68_260)), inference(subsumption_resolution, [], [f5976, f552])).
fof(f5976, plain, ((e22 = e23) | (~ spl68_138 | ~ spl68_260)), inference(forward_demodulation, [], [f5964, f1526])).
fof(f1526, plain, ((e22 = op2(e24, e23)) | ~ spl68_138), inference(avatar_component_clause, [], [f1524])).
fof(f1524, plain, (spl68_138 <=> (e22 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_138])])).
fof(f5964, plain, ((e23 = op2(e24, e23)) | ~ spl68_260), inference(backward_demodulation, [], [f280, f2038])).
fof(f2038, plain, ((e24 = unit2) | ~ spl68_260), inference(avatar_component_clause, [], [f2036])).
fof(f2036, plain, (spl68_260 <=> (e24 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl68_260])])).
fof(f5929, plain, (~ spl68_160 | ~ spl68_175), inference(avatar_split_clause, [], [f5732, f1679, f1616])).
fof(f1616, plain, (spl68_160 <=> (e24 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_160])])).
fof(f5732, plain, (~ (e24 = op2(e23, e24)) | ~ spl68_175), inference(backward_demodulation, [], [f521, f1681])).
fof(f521, plain, ~ (op2(e23, e21) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f5906, plain, (~ spl68_78 | ~ spl68_93), inference(avatar_split_clause, [], [f5905, f1285, f1222])).
fof(f1222, plain, (spl68_78 <=> (e12 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_78])])).
fof(f5905, plain, (~ (e12 = op1(e11, e14)) | ~ spl68_93), inference(forward_demodulation, [], [f401, f1287])).
fof(f5897, plain, (~ spl68_13 | ~ spl68_8), inference(avatar_split_clause, [], [f4961, f928, f949])).
fof(f949, plain, (spl68_13 <=> (e12 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_13])])).
fof(f4961, plain, (~ (e12 = op1(e14, e12)) | ~ spl68_8), inference(forward_demodulation, [], [f432, f930])).
fof(f432, plain, ~ (op1(e14, e12) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f5882, plain, (~ spl68_51 | ~ spl68_52), inference(avatar_contradiction_clause, [], [f5881])).
fof(f5881, plain, ($false | (~ spl68_51 | ~ spl68_52)), inference(subsumption_resolution, [], [f5880, f535])).
fof(f5880, plain, ((e10 = e11) | (~ spl68_51 | ~ spl68_52)), inference(forward_demodulation, [], [f1115, f1111])).
fof(f5870, plain, (~ spl68_134 | spl68_422), inference(avatar_contradiction_clause, [], [f5869])).
fof(f5869, plain, ($false | (~ spl68_134 | spl68_422)), inference(subsumption_resolution, [], [f5868, f2973])).
fof(f2973, plain, (~ (e23 = h5(e11)) | spl68_422), inference(avatar_component_clause, [], [f2971])).
fof(f5868, plain, ((e23 = h5(e11)) | ~ spl68_134), inference(backward_demodulation, [], [f760, f1509])).
fof(f5862, plain, (~ spl68_151 | ~ spl68_155), inference(avatar_contradiction_clause, [], [f5861])).
fof(f5861, plain, ($false | (~ spl68_151 | ~ spl68_155)), inference(subsumption_resolution, [], [f5860, f548])).
fof(f5860, plain, ((e20 = e24) | (~ spl68_151 | ~ spl68_155)), inference(backward_demodulation, [], [f1597, f1581])).
fof(f1581, plain, ((e20 = op2(e24, e20)) | ~ spl68_151), inference(avatar_component_clause, [], [f1579])).
fof(f1579, plain, (spl68_151 <=> (e20 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_151])])).
fof(f5846, plain, (~ spl68_161 | ~ spl68_162), inference(avatar_contradiction_clause, [], [f5845])).
fof(f5845, plain, ($false | (~ spl68_161 | ~ spl68_162)), inference(subsumption_resolution, [], [f5844, f545])).
fof(f5844, plain, ((e20 = e21) | (~ spl68_161 | ~ spl68_162)), inference(backward_demodulation, [], [f1627, f1623])).
fof(f1623, plain, ((e20 = op2(e23, e23)) | ~ spl68_161), inference(avatar_component_clause, [], [f1621])).
fof(f1621, plain, (spl68_161 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_161])])).
fof(f1627, plain, ((e21 = op2(e23, e23)) | ~ spl68_162), inference(avatar_component_clause, [], [f1625])).
fof(f1625, plain, (spl68_162 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_162])])).
fof(f5843, plain, (~ spl68_156 | ~ spl68_166), inference(avatar_split_clause, [], [f5836, f1642, f1600])).
fof(f1600, plain, (spl68_156 <=> (e20 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_156])])).
fof(f5836, plain, (~ (e20 = op2(e23, e24)) | ~ spl68_166), inference(backward_demodulation, [], [f523, f1644])).
fof(f523, plain, ~ (op2(e23, e22) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f5842, plain, (~ spl68_141 | ~ spl68_166), inference(avatar_split_clause, [], [f5835, f1642, f1537])).
fof(f1537, plain, (spl68_141 <=> (e20 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_141])])).
fof(f5835, plain, (~ (e20 = op2(e24, e22)) | ~ spl68_166), inference(backward_demodulation, [], [f464, f1644])).
fof(f464, plain, ~ (op2(e23, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f5822, plain, (~ spl68_195 | spl68_591), inference(avatar_contradiction_clause, [], [f5821])).
fof(f5821, plain, ($false | (~ spl68_195 | spl68_591)), inference(subsumption_resolution, [], [f5820, f3970])).
fof(f3970, plain, (~ (e24 = h3(e11)) | spl68_591), inference(avatar_component_clause, [], [f3968])).
fof(f5820, plain, ((e24 = h3(e11)) | ~ spl68_195), inference(backward_demodulation, [], [f750, f1765])).
fof(f1765, plain, ((e24 = op2(e22, e22)) | ~ spl68_195), inference(avatar_component_clause, [], [f1763])).
fof(f1763, plain, (spl68_195 <=> (e24 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_195])])).
fof(f5804, plain, (~ spl68_207 | ~ spl68_227), inference(avatar_split_clause, [], [f5801, f1898, f1814])).
fof(f1814, plain, (spl68_207 <=> (e21 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_207])])).
fof(f5801, plain, (~ (e21 = op2(e21, e24)) | ~ spl68_227), inference(backward_demodulation, [], [f498, f1900])).
fof(f498, plain, ~ (op2(e21, e20) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f5787, plain, (~ spl68_162 | ~ spl68_259), inference(avatar_contradiction_clause, [], [f5786])).
fof(f5786, plain, ($false | (~ spl68_162 | ~ spl68_259)), inference(subsumption_resolution, [], [f5785, f550])).
fof(f550, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f10])).
fof(f5785, plain, ((e21 = e23) | (~ spl68_162 | ~ spl68_259)), inference(forward_demodulation, [], [f5769, f1627])).
fof(f5769, plain, ((e23 = op2(e23, e23)) | ~ spl68_259), inference(backward_demodulation, [], [f281, f2034])).
fof(f2034, plain, ((e23 = unit2) | ~ spl68_259), inference(avatar_component_clause, [], [f2032])).
fof(f2032, plain, (spl68_259 <=> (e23 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl68_259])])).
fof(f281, plain, (e23 = op2(e23, unit2)), inference(cnf_transformation, [], [f5])).
fof(f5750, plain, (~ spl68_189 | ~ spl68_239), inference(avatar_split_clause, [], [f5749, f1948, f1738])).
fof(f1738, plain, (spl68_189 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_189])])).
fof(f5749, plain, (~ (e23 = op2(e22, e23)) | ~ spl68_239), inference(forward_demodulation, [], [f466, f1950])).
fof(f466, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f5742, plain, (~ spl68_188 | ~ spl68_440), inference(avatar_split_clause, [], [f5741, f3064, f1734])).
fof(f1734, plain, (spl68_188 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_188])])).
fof(f5741, plain, (~ (e22 = op2(e22, e23)) | ~ spl68_440), inference(forward_demodulation, [], [f2923, f3065])).
fof(f2923, plain, ~ (op2(e22, e23) = h4(e12)), inference(backward_demodulation, [], [f473, f2920])).
fof(f473, plain, ~ (op2(e22, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f5729, plain, (spl68_166 | ~ spl68_440), inference(avatar_split_clause, [], [f5541, f3064, f1642])).
fof(f5541, plain, ((e20 = op2(e23, e22)) | ~ spl68_440), inference(backward_demodulation, [], [f2929, f3065])).
fof(f5726, plain, (~ spl68_157 | ~ spl68_444), inference(avatar_split_clause, [], [f5489, f3086, f1604])).
fof(f1604, plain, (spl68_157 <=> (e21 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_157])])).
fof(f5489, plain, (~ (e21 = op2(e23, e24)) | ~ spl68_444), inference(forward_demodulation, [], [f2936, f3087])).
fof(f2936, plain, ~ (op2(e23, e24) = h4(e11)), inference(backward_demodulation, [], [f524, f755])).
fof(f524, plain, ~ (op2(e23, e23) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f5725, plain, (~ spl68_158 | spl68_343 | ~ spl68_440), inference(avatar_split_clause, [], [f5724, f3064, f2496, f1608])).
fof(f2496, plain, (spl68_343 <=> (op2(e23, e24) = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_343])])).
fof(f5724, plain, (~ (e22 = op2(e23, e24)) | (spl68_343 | ~ spl68_440)), inference(forward_demodulation, [], [f4336, f3065])).
fof(f4336, plain, (~ (op2(e23, e24) = h4(e12)) | spl68_343), inference(forward_demodulation, [], [f2498, f2920])).
fof(f2498, plain, (~ (op2(e23, e24) = op2(e24, e23)) | spl68_343), inference(avatar_component_clause, [], [f2496])).
fof(f5720, plain, (~ spl68_143 | ~ spl68_440), inference(avatar_split_clause, [], [f5719, f3064, f1545])).
fof(f1545, plain, (spl68_143 <=> (e22 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_143])])).
fof(f5719, plain, (~ (e22 = op2(e24, e22)) | ~ spl68_440), inference(forward_demodulation, [], [f2927, f3065])).
fof(f2927, plain, ~ (op2(e24, e22) = h4(e12)), inference(backward_demodulation, [], [f532, f2920])).
fof(f532, plain, ~ (op2(e24, e22) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f5688, plain, (~ spl68_28 | ~ spl68_8 | spl68_264), inference(avatar_split_clause, [], [f4980, f2105, f928, f1012])).
fof(f2105, plain, (spl68_264 <=> (op1(e13, e14) = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_264])])).
fof(f4980, plain, (~ (e12 = op1(e13, e14)) | (~ spl68_8 | spl68_264)), inference(forward_demodulation, [], [f2107, f930])).
fof(f2107, plain, (~ (op1(e13, e14) = op1(e14, e13)) | spl68_264), inference(avatar_component_clause, [], [f2105])).
fof(f5683, plain, (~ spl68_14 | ~ spl68_89), inference(avatar_split_clause, [], [f5682, f1268, f953])).
fof(f5682, plain, (~ (e13 = op1(e14, e12)) | ~ spl68_89), inference(forward_demodulation, [], [f361, f1270])).
fof(f361, plain, ~ (op1(e11, e12) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f5667, plain, (~ spl68_32 | ~ spl68_33), inference(avatar_contradiction_clause, [], [f5666])).
fof(f5666, plain, ($false | (~ spl68_32 | ~ spl68_33)), inference(subsumption_resolution, [], [f5665, f539])).
fof(f5665, plain, ((e11 = e12) | (~ spl68_32 | ~ spl68_33)), inference(forward_demodulation, [], [f1035, f1031])).
fof(f1035, plain, ((e12 = op1(e13, e13)) | ~ spl68_33), inference(avatar_component_clause, [], [f1033])).
fof(f1033, plain, (spl68_33 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_33])])).
fof(f5647, plain, (~ spl68_81 | ~ spl68_85), inference(avatar_contradiction_clause, [], [f5646])).
fof(f5646, plain, ($false | (~ spl68_81 | ~ spl68_85)), inference(subsumption_resolution, [], [f5645, f538])).
fof(f5645, plain, ((e10 = e14) | (~ spl68_81 | ~ spl68_85)), inference(backward_demodulation, [], [f1253, f1237])).
fof(f1253, plain, ((e14 = op1(e11, e13)) | ~ spl68_85), inference(avatar_component_clause, [], [f1251])).
fof(f5625, plain, (~ spl68_136 | ~ spl68_138), inference(avatar_contradiction_clause, [], [f5624])).
fof(f5624, plain, ($false | (~ spl68_136 | ~ spl68_138)), inference(subsumption_resolution, [], [f5623, f546])).
fof(f546, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f10])).
fof(f5623, plain, ((e20 = e22) | (~ spl68_136 | ~ spl68_138)), inference(backward_demodulation, [], [f1526, f1518])).
fof(f1518, plain, ((e20 = op2(e24, e23)) | ~ spl68_136), inference(avatar_component_clause, [], [f1516])).
fof(f1516, plain, (spl68_136 <=> (e20 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_136])])).
fof(f5610, plain, (~ spl68_174 | ~ spl68_175), inference(avatar_contradiction_clause, [], [f5609])).
fof(f5609, plain, ($false | (~ spl68_174 | ~ spl68_175)), inference(subsumption_resolution, [], [f5608, f554])).
fof(f554, plain, ~ (e23 = e24), inference(cnf_transformation, [], [f10])).
fof(f5608, plain, ((e23 = e24) | (~ spl68_174 | ~ spl68_175)), inference(backward_demodulation, [], [f1681, f1677])).
fof(f1677, plain, ((e23 = op2(e23, e21)) | ~ spl68_174), inference(avatar_component_clause, [], [f1675])).
fof(f1675, plain, (spl68_174 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_174])])).
fof(f5599, plain, (spl68_456 | ~ spl68_193), inference(avatar_split_clause, [], [f5598, f1755, f3151])).
fof(f1755, plain, (spl68_193 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_193])])).
fof(f5598, plain, ((e22 = h3(e11)) | ~ spl68_193), inference(backward_demodulation, [], [f750, f1757])).
fof(f1757, plain, ((e22 = op2(e22, e22)) | ~ spl68_193), inference(avatar_component_clause, [], [f1755])).
fof(f5588, plain, (spl68_476 | ~ spl68_223), inference(avatar_split_clause, [], [f5587, f1881, f3251])).
fof(f1881, plain, (spl68_223 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_223])])).
fof(f5587, plain, ((e22 = h2(e11)) | ~ spl68_223), inference(backward_demodulation, [], [f745, f1883])).
fof(f1883, plain, ((e22 = op2(e21, e21)) | ~ spl68_223), inference(avatar_component_clause, [], [f1881])).
fof(f5568, plain, (spl68_241 | ~ spl68_258), inference(avatar_contradiction_clause, [], [f5567])).
fof(f5567, plain, ($false | (spl68_241 | ~ spl68_258)), inference(subsumption_resolution, [], [f5556, f1958])).
fof(f1958, plain, (~ (e20 = op2(e20, e22)) | spl68_241), inference(avatar_component_clause, [], [f1957])).
fof(f1957, plain, (spl68_241 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_241])])).
fof(f5556, plain, ((e20 = op2(e20, e22)) | ~ spl68_258), inference(backward_demodulation, [], [f275, f2030])).
fof(f2030, plain, ((e22 = unit2) | ~ spl68_258), inference(avatar_component_clause, [], [f2028])).
fof(f2028, plain, (spl68_258 <=> (e22 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl68_258])])).
fof(f5531, plain, (~ spl68_200 | ~ spl68_175), inference(avatar_split_clause, [], [f5530, f1679, f1784])).
fof(f1784, plain, (spl68_200 <=> (e24 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_200])])).
fof(f5530, plain, (~ (e24 = op2(e22, e21)) | ~ spl68_175), inference(forward_demodulation, [], [f452, f1681])).
fof(f452, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f5523, plain, (~ spl68_187 | ~ spl68_444), inference(avatar_split_clause, [], [f5522, f3086, f1730])).
fof(f1730, plain, (spl68_187 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_187])])).
fof(f5522, plain, (~ (e21 = op2(e22, e23)) | ~ spl68_444), inference(forward_demodulation, [], [f2932, f3087])).
fof(f2932, plain, ~ (op2(e22, e23) = h4(e11)), inference(backward_demodulation, [], [f472, f755])).
fof(f472, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f5510, plain, (~ spl68_199 | ~ spl68_184), inference(avatar_split_clause, [], [f5506, f1717, f1780])).
fof(f1717, plain, (spl68_184 <=> (e23 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_184])])).
fof(f5506, plain, (~ (e23 = op2(e22, e21)) | ~ spl68_184), inference(backward_demodulation, [], [f511, f1719])).
fof(f1719, plain, ((e23 = op2(e22, e24)) | ~ spl68_184), inference(avatar_component_clause, [], [f1717])).
fof(f511, plain, ~ (op2(e22, e21) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f5465, plain, (~ spl68_95 | ~ spl68_45), inference(avatar_split_clause, [], [f5464, f1083, f1293])).
fof(f1293, plain, (spl68_95 <=> (e14 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_95])])).
fof(f5464, plain, (~ (e14 = op1(e11, e11)) | ~ spl68_45), inference(forward_demodulation, [], [f350, f1085])).
fof(f350, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f5450, plain, (~ spl68_77 | ~ spl68_97), inference(avatar_split_clause, [], [f5449, f1302, f1218])).
fof(f1218, plain, (spl68_77 <=> (e11 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_77])])).
fof(f5449, plain, (~ (e11 = op1(e11, e14)) | ~ spl68_97), inference(forward_demodulation, [], [f398, f1304])).
fof(f398, plain, ~ (op1(e11, e10) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f5448, plain, (~ spl68_70 | ~ spl68_45), inference(avatar_split_clause, [], [f5447, f1083, f1188])).
fof(f1188, plain, (spl68_70 <=> (e14 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_70])])).
fof(f5447, plain, (~ (e14 = op1(e12, e11)) | ~ spl68_45), inference(forward_demodulation, [], [f352, f1085])).
fof(f352, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f5444, plain, (~ spl68_57 | ~ spl68_32), inference(avatar_split_clause, [], [f5443, f1029, f1134])).
fof(f1134, plain, (spl68_57 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_57])])).
fof(f5443, plain, (~ (e11 = op1(e12, e13)) | ~ spl68_32), inference(forward_demodulation, [], [f372, f1031])).
fof(f372, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f5442, plain, (~ spl68_58 | ~ spl68_8), inference(avatar_split_clause, [], [f5441, f928, f1138])).
fof(f1138, plain, (spl68_58 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_58])])).
fof(f5441, plain, (~ (e12 = op1(e12, e13)) | ~ spl68_8), inference(forward_demodulation, [], [f373, f930])).
fof(f373, plain, ~ (op1(e12, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f5426, plain, (~ spl68_79 | ~ spl68_54), inference(avatar_split_clause, [], [f5423, f1121, f1226])).
fof(f5423, plain, (~ (e13 = op1(e11, e14)) | ~ spl68_54), inference(backward_demodulation, [], [f379, f1123])).
fof(f1123, plain, ((e13 = op1(e12, e14)) | ~ spl68_54), inference(avatar_component_clause, [], [f1121])).
fof(f379, plain, ~ (op1(e11, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f5400, plain, (~ spl68_18 | ~ spl68_8), inference(avatar_split_clause, [], [f4969, f928, f970])).
fof(f970, plain, (spl68_18 <=> (e12 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_18])])).
fof(f4969, plain, (~ (e12 = op1(e14, e11)) | ~ spl68_8), inference(forward_demodulation, [], [f430, f930])).
fof(f430, plain, ~ (op1(e14, e11) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f5399, plain, (~ spl68_17 | ~ spl68_117), inference(avatar_split_clause, [], [f5398, f1386, f966])).
fof(f966, plain, (spl68_17 <=> (e11 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_17])])).
fof(f5398, plain, (~ (e11 = op1(e14, e11)) | ~ spl68_117), inference(forward_demodulation, [], [f348, f1388])).
fof(f348, plain, ~ (op1(e10, e11) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f5372, plain, (~ spl68_43 | ~ spl68_45), inference(avatar_contradiction_clause, [], [f5371])).
fof(f5371, plain, ($false | (~ spl68_43 | ~ spl68_45)), inference(subsumption_resolution, [], [f5370, f543])).
fof(f543, plain, ~ (e12 = e14), inference(cnf_transformation, [], [f9])).
fof(f5370, plain, ((e12 = e14) | (~ spl68_43 | ~ spl68_45)), inference(backward_demodulation, [], [f1085, f1077])).
fof(f1077, plain, ((e12 = op1(e13, e11)) | ~ spl68_43), inference(avatar_component_clause, [], [f1075])).
fof(f5352, plain, (~ spl68_63 | ~ spl68_73), inference(avatar_split_clause, [], [f5349, f1201, f1159])).
fof(f1159, plain, (spl68_63 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_63])])).
fof(f5349, plain, (~ (e12 = op1(e12, e12)) | ~ spl68_73), inference(backward_demodulation, [], [f406, f1203])).
fof(f406, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f5351, plain, (~ spl68_68 | ~ spl68_73), inference(avatar_split_clause, [], [f5348, f1201, f1180])).
fof(f1180, plain, (spl68_68 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_68])])).
fof(f5348, plain, (~ (e12 = op1(e12, e11)) | ~ spl68_73), inference(backward_demodulation, [], [f405, f1203])).
fof(f405, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f5350, plain, (~ spl68_48 | ~ spl68_73), inference(avatar_split_clause, [], [f5347, f1201, f1096])).
fof(f1096, plain, (spl68_48 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_48])])).
fof(f5347, plain, (~ (e12 = op1(e13, e10)) | ~ spl68_73), inference(backward_demodulation, [], [f342, f1203])).
fof(f342, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f7])).
fof(f5332, plain, (~ spl68_72 | ~ spl68_97), inference(avatar_split_clause, [], [f5328, f1302, f1197])).
fof(f1197, plain, (spl68_72 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_72])])).
fof(f5328, plain, (~ (e11 = op1(e12, e10)) | ~ spl68_97), inference(backward_demodulation, [], [f339, f1304])).
fof(f339, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f5315, plain, (~ spl68_112 | ~ spl68_117), inference(avatar_split_clause, [], [f5311, f1386, f1365])).
fof(f1365, plain, (spl68_112 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_112])])).
fof(f5311, plain, (~ (e11 = op1(e10, e12)) | ~ spl68_117), inference(backward_demodulation, [], [f389, f1388])).
fof(f389, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f7])).
fof(f5307, plain, (spl68_432 | ~ spl68_132), inference(avatar_split_clause, [], [f5305, f1499, f3021])).
fof(f1499, plain, (spl68_132 <=> (e21 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_132])])).
fof(f5305, plain, ((e21 = h5(e11)) | ~ spl68_132), inference(backward_demodulation, [], [f760, f1501])).
fof(f1501, plain, ((e21 = op2(e24, e24)) | ~ spl68_132), inference(avatar_component_clause, [], [f1499])).
fof(f5284, plain, (~ spl68_173 | ~ spl68_175), inference(avatar_contradiction_clause, [], [f5283])).
fof(f5283, plain, ($false | (~ spl68_173 | ~ spl68_175)), inference(subsumption_resolution, [], [f5282, f553])).
fof(f5282, plain, ((e22 = e24) | (~ spl68_173 | ~ spl68_175)), inference(backward_demodulation, [], [f1681, f1673])).
fof(f1673, plain, ((e22 = op2(e23, e21)) | ~ spl68_173), inference(avatar_component_clause, [], [f1671])).
fof(f5281, plain, (~ spl68_620 | ~ spl68_175), inference(avatar_split_clause, [], [f5279, f1679, f4141])).
fof(f5279, plain, (~ (e24 = h2(e11)) | ~ spl68_175), inference(backward_demodulation, [], [f2896, f1681])).
fof(f2896, plain, ~ (op2(e23, e21) = h2(e11)), inference(backward_demodulation, [], [f450, f745])).
fof(f450, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f5263, plain, (spl68_461 | ~ spl68_192), inference(avatar_split_clause, [], [f5262, f1751, f3176])).
fof(f1751, plain, (spl68_192 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_192])])).
fof(f5262, plain, ((e21 = h3(e11)) | ~ spl68_192), inference(backward_demodulation, [], [f750, f1753])).
fof(f1753, plain, ((e21 = op2(e22, e22)) | ~ spl68_192), inference(avatar_component_clause, [], [f1751])).
fof(f5227, plain, (~ spl68_214 | ~ spl68_239), inference(avatar_split_clause, [], [f5224, f1948, f1843])).
fof(f1843, plain, (spl68_214 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_214])])).
fof(f5224, plain, (~ (e23 = op2(e21, e23)) | ~ spl68_239), inference(backward_demodulation, [], [f465, f1950])).
fof(f465, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f5205, plain, (spl68_174 | ~ spl68_257), inference(avatar_split_clause, [], [f5197, f2024, f1675])).
fof(f2024, plain, (spl68_257 <=> (e21 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl68_257])])).
fof(f5197, plain, ((e23 = op2(e23, e21)) | ~ spl68_257), inference(backward_demodulation, [], [f281, f2026])).
fof(f2026, plain, ((e21 = unit2) | ~ spl68_257), inference(avatar_component_clause, [], [f2024])).
fof(f5187, plain, (spl68_175 | ~ spl68_444), inference(avatar_split_clause, [], [f5178, f3086, f1679])).
fof(f5178, plain, ((e24 = op2(e23, e21)) | ~ spl68_444), inference(backward_demodulation, [], [f2937, f3087])).
fof(f5186, plain, (~ spl68_167 | ~ spl68_444), inference(avatar_split_clause, [], [f5177, f3086, f1646])).
fof(f1646, plain, (spl68_167 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_167])])).
fof(f5177, plain, (~ (e21 = op2(e23, e22)) | ~ spl68_444), inference(backward_demodulation, [], [f2935, f3087])).
fof(f2935, plain, ~ (op2(e23, e22) = h4(e11)), inference(backward_demodulation, [], [f522, f755])).
fof(f522, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f5183, plain, (~ spl68_212 | ~ spl68_444), inference(avatar_split_clause, [], [f5174, f3086, f1835])).
fof(f1835, plain, (spl68_212 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_212])])).
fof(f5174, plain, (~ (e21 = op2(e21, e23)) | ~ spl68_444), inference(backward_demodulation, [], [f2931, f3087])).
fof(f2931, plain, ~ (op2(e21, e23) = h4(e11)), inference(backward_demodulation, [], [f470, f755])).
fof(f470, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f5182, plain, (~ spl68_237 | ~ spl68_444), inference(avatar_split_clause, [], [f5173, f3086, f1940])).
fof(f5173, plain, (~ (e21 = op2(e20, e23)) | ~ spl68_444), inference(backward_demodulation, [], [f2930, f3087])).
fof(f2930, plain, ~ (op2(e20, e23) = h4(e11)), inference(backward_demodulation, [], [f467, f755])).
fof(f467, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f5164, plain, (~ spl68_233 | ~ spl68_235), inference(avatar_contradiction_clause, [], [f5163])).
fof(f5163, plain, ($false | (~ spl68_233 | ~ spl68_235)), inference(subsumption_resolution, [], [f5162, f553])).
fof(f5162, plain, ((e22 = e24) | (~ spl68_233 | ~ spl68_235)), inference(forward_demodulation, [], [f1933, f1925])).
fof(f1925, plain, ((e22 = op2(e20, e24)) | ~ spl68_233), inference(avatar_component_clause, [], [f1923])).
fof(f5123, plain, (~ spl68_182 | ~ spl68_184), inference(avatar_contradiction_clause, [], [f5122])).
fof(f5122, plain, ($false | (~ spl68_182 | ~ spl68_184)), inference(subsumption_resolution, [], [f5121, f550])).
fof(f5121, plain, ((e21 = e23) | (~ spl68_182 | ~ spl68_184)), inference(forward_demodulation, [], [f1719, f1711])).
fof(f5080, plain, (spl68_440 | ~ spl68_138), inference(avatar_split_clause, [], [f5079, f1524, f3064])).
fof(f5079, plain, ((e22 = h4(e12)) | ~ spl68_138), inference(forward_demodulation, [], [f2920, f1526])).
fof(f5066, plain, (~ spl68_107 | ~ spl68_32), inference(avatar_split_clause, [], [f5065, f1029, f1344])).
fof(f5065, plain, (~ (e11 = op1(e10, e13)) | ~ spl68_32), inference(forward_demodulation, [], [f367, f1031])).
fof(f367, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f5064, plain, (~ spl68_108 | ~ spl68_8), inference(avatar_split_clause, [], [f5063, f928, f1348])).
fof(f5063, plain, (~ (e12 = op1(e10, e13)) | ~ spl68_8), inference(forward_demodulation, [], [f368, f930])).
fof(f368, plain, ~ (op1(e10, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f5060, plain, (~ spl68_103 | ~ spl68_105), inference(avatar_contradiction_clause, [], [f5059])).
fof(f5059, plain, ($false | (~ spl68_103 | ~ spl68_105)), inference(subsumption_resolution, [], [f5058, f543])).
fof(f5058, plain, ((e12 = e14) | (~ spl68_103 | ~ spl68_105)), inference(forward_demodulation, [], [f1337, f1329])).
fof(f1329, plain, ((e12 = op1(e10, e14)) | ~ spl68_103), inference(avatar_component_clause, [], [f1327])).
fof(f5045, plain, (~ spl68_82 | ~ spl68_32), inference(avatar_split_clause, [], [f5044, f1029, f1239])).
fof(f1239, plain, (spl68_82 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_82])])).
fof(f5044, plain, (~ (e11 = op1(e11, e13)) | ~ spl68_32), inference(forward_demodulation, [], [f370, f1031])).
fof(f370, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f5043, plain, (~ spl68_83 | ~ spl68_8), inference(avatar_split_clause, [], [f5042, f928, f1243])).
fof(f1243, plain, (spl68_83 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_83])])).
fof(f5042, plain, (~ (e12 = op1(e11, e13)) | ~ spl68_8), inference(forward_demodulation, [], [f371, f930])).
fof(f371, plain, ~ (op1(e11, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f4990, plain, (spl68_45 | ~ spl68_32), inference(avatar_split_clause, [], [f4866, f1029, f1083])).
fof(f4866, plain, ((e14 = op1(e13, e11)) | ~ spl68_32), inference(backward_demodulation, [], [f733, f1031])).
fof(f733, plain, (e14 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((e14 = op1(e13, op1(e13, e13))) & (e12 = op1(op1(e13, op1(e13, e13)), e13)) & (e11 = op1(e13, e13)) & (e10 = op1(e13, op1(op1(e13, op1(e13, e13)), e13)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax14)).
fof(f4987, plain, (~ spl68_37 | ~ spl68_32), inference(avatar_split_clause, [], [f4986, f1029, f1050])).
fof(f1050, plain, (spl68_37 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_37])])).
fof(f4986, plain, (~ (e11 = op1(e13, e12)) | ~ spl68_32), inference(forward_demodulation, [], [f422, f1031])).
fof(f422, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f4985, plain, (~ spl68_36 | ~ spl68_26), inference(avatar_split_clause, [], [f4984, f1004, f1046])).
fof(f1004, plain, (spl68_26 <=> (e10 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_26])])).
fof(f4984, plain, (~ (e10 = op1(e13, e12)) | ~ spl68_26), inference(forward_demodulation, [], [f423, f1006])).
fof(f1006, plain, ((e10 = op1(e13, e14)) | ~ spl68_26), inference(avatar_component_clause, [], [f1004])).
fof(f423, plain, ~ (op1(e13, e12) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4957, plain, (~ spl68_12 | ~ spl68_14), inference(avatar_contradiction_clause, [], [f4956])).
fof(f4956, plain, ($false | (~ spl68_12 | ~ spl68_14)), inference(subsumption_resolution, [], [f4955, f540])).
fof(f4955, plain, ((e11 = e13) | (~ spl68_12 | ~ spl68_14)), inference(forward_demodulation, [], [f955, f947])).
fof(f4946, plain, (spl68_36 | ~ spl68_8), inference(avatar_split_clause, [], [f4945, f928, f1046])).
fof(f4945, plain, ((e10 = op1(e13, e12)) | ~ spl68_8), inference(forward_demodulation, [], [f2875, f930])).
fof(f2875, plain, (e10 = op1(e13, op1(e14, e13))), inference(forward_demodulation, [], [f730, f733])).
fof(f730, plain, (e10 = op1(e13, op1(op1(e13, op1(e13, e13)), e13))), inference(cnf_transformation, [], [f14])).
fof(f4944, plain, (~ spl68_6 | ~ spl68_8), inference(avatar_contradiction_clause, [], [f4943])).
fof(f4943, plain, ($false | (~ spl68_6 | ~ spl68_8)), inference(subsumption_resolution, [], [f4942, f536])).
fof(f4942, plain, ((e10 = e12) | (~ spl68_6 | ~ spl68_8)), inference(backward_demodulation, [], [f930, f922])).
fof(f922, plain, ((e10 = op1(e14, e13)) | ~ spl68_6), inference(avatar_component_clause, [], [f920])).
fof(f920, plain, (spl68_6 <=> (e10 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_6])])).
fof(f4941, plain, (~ spl68_8 | ~ spl68_9), inference(avatar_contradiction_clause, [], [f4940])).
fof(f4940, plain, ($false | (~ spl68_8 | ~ spl68_9)), inference(subsumption_resolution, [], [f4939, f542])).
fof(f4939, plain, ((e12 = e13) | (~ spl68_8 | ~ spl68_9)), inference(backward_demodulation, [], [f934, f930])).
fof(f934, plain, ((e13 = op1(e14, e13)) | ~ spl68_9), inference(avatar_component_clause, [], [f932])).
fof(f932, plain, (spl68_9 <=> (e13 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_9])])).
fof(f4895, plain, (~ spl68_24 | ~ spl68_25), inference(avatar_contradiction_clause, [], [f4894])).
fof(f4894, plain, ($false | (~ spl68_24 | ~ spl68_25)), inference(subsumption_resolution, [], [f4893, f544])).
fof(f4893, plain, ((e13 = e14) | (~ spl68_24 | ~ spl68_25)), inference(backward_demodulation, [], [f1001, f997])).
fof(f997, plain, ((e13 = op1(e14, e10)) | ~ spl68_24), inference(avatar_component_clause, [], [f995])).
fof(f995, plain, (spl68_24 <=> (e13 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl68_24])])).
fof(f4862, plain, (~ spl68_31 | ~ spl68_36), inference(avatar_split_clause, [], [f4859, f1046, f1025])).
fof(f1025, plain, (spl68_31 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_31])])).
fof(f4859, plain, (~ (e10 = op1(e13, e13)) | ~ spl68_36), inference(backward_demodulation, [], [f422, f1048])).
fof(f4861, plain, (~ spl68_11 | ~ spl68_36), inference(avatar_split_clause, [], [f4858, f1046, f941])).
fof(f941, plain, (spl68_11 <=> (e10 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_11])])).
fof(f4858, plain, (~ (e10 = op1(e14, e12)) | ~ spl68_36), inference(backward_demodulation, [], [f364, f1048])).
fof(f364, plain, ~ (op1(e13, e12) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f4817, plain, (~ spl68_36 | ~ spl68_61), inference(avatar_split_clause, [], [f4813, f1151, f1046])).
fof(f1151, plain, (spl68_61 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_61])])).
fof(f4813, plain, (~ (e10 = op1(e13, e12)) | ~ spl68_61), inference(backward_demodulation, [], [f362, f1153])).
fof(f1153, plain, ((e10 = op1(e12, e12)) | ~ spl68_61), inference(avatar_component_clause, [], [f1151])).
fof(f362, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4764, plain, (~ spl68_36 | ~ spl68_86), inference(avatar_split_clause, [], [f4759, f1256, f1046])).
fof(f1256, plain, (spl68_86 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl68_86])])).
fof(f4759, plain, (~ (e10 = op1(e13, e12)) | ~ spl68_86), inference(backward_demodulation, [], [f360, f1258])).
fof(f1258, plain, ((e10 = op1(e11, e12)) | ~ spl68_86), inference(avatar_component_clause, [], [f1256])).
fof(f360, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4718, plain, (~ spl68_55 | ~ spl68_105), inference(avatar_split_clause, [], [f4714, f1335, f1125])).
fof(f1125, plain, (spl68_55 <=> (e14 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_55])])).
fof(f4714, plain, (~ (e14 = op1(e12, e14)) | ~ spl68_105), inference(backward_demodulation, [], [f376, f1337])).
fof(f376, plain, ~ (op1(e10, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f4676, plain, (~ spl68_101 | ~ spl68_121), inference(avatar_split_clause, [], [f4664, f1403, f1319])).
fof(f1319, plain, (spl68_101 <=> (e10 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl68_101])])).
fof(f4664, plain, (~ (e10 = op1(e10, e14)) | ~ spl68_121), inference(backward_demodulation, [], [f388, f1405])).
fof(f388, plain, ~ (op1(e10, e10) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f4675, plain, (~ spl68_106 | ~ spl68_121), inference(avatar_split_clause, [], [f4663, f1403, f1340])).
fof(f1340, plain, (spl68_106 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl68_106])])).
fof(f4663, plain, (~ (e10 = op1(e10, e13)) | ~ spl68_121), inference(backward_demodulation, [], [f387, f1405])).
fof(f387, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f4635, plain, (spl68_437 | ~ spl68_131), inference(avatar_split_clause, [], [f4633, f1495, f3046])).
fof(f4633, plain, ((e20 = h5(e11)) | ~ spl68_131), inference(backward_demodulation, [], [f760, f1497])).
fof(f4579, plain, (~ spl68_150 | ~ spl68_155), inference(avatar_split_clause, [], [f4574, f1595, f1574])).
fof(f1574, plain, (spl68_150 <=> (e24 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl68_150])])).
fof(f4574, plain, (~ (e24 = op2(e24, e21)) | ~ spl68_155), inference(backward_demodulation, [], [f525, f1597])).
fof(f525, plain, ~ (op2(e24, e20) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f4561, plain, (spl68_444 | ~ spl68_162), inference(avatar_split_clause, [], [f4560, f1625, f3086])).
fof(f4560, plain, ((e21 = h4(e11)) | ~ spl68_162), inference(backward_demodulation, [], [f755, f1627])).
fof(f4558, plain, (~ spl68_466 | ~ spl68_166), inference(avatar_split_clause, [], [f4554, f1642, f3201])).
fof(f3201, plain, (spl68_466 <=> (e20 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl68_466])])).
fof(f4554, plain, (~ (e20 = h3(e11)) | ~ spl68_166), inference(backward_demodulation, [], [f2909, f1644])).
fof(f2909, plain, ~ (op2(e23, e22) = h3(e11)), inference(backward_demodulation, [], [f462, f750])).
fof(f462, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4506, plain, (spl68_466 | ~ spl68_191), inference(avatar_split_clause, [], [f4505, f1747, f3201])).
fof(f4505, plain, ((e20 = h3(e11)) | ~ spl68_191), inference(backward_demodulation, [], [f750, f1749])).
fof(f1749, plain, ((e20 = op2(e22, e22)) | ~ spl68_191), inference(avatar_component_clause, [], [f1747])).
fof(f4491, plain, (~ spl68_506 | ~ spl68_201), inference(avatar_split_clause, [], [f4484, f1789, f3403])).
fof(f1789, plain, (spl68_201 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl68_201])])).
fof(f4484, plain, (~ (e20 = h1(e11)) | ~ spl68_201), inference(backward_demodulation, [], [f2882, f1791])).
fof(f1791, plain, ((e20 = op2(e22, e20)) | ~ spl68_201), inference(avatar_component_clause, [], [f1789])).
fof(f2882, plain, ~ (op2(e22, e20) = h1(e11)), inference(backward_demodulation, [], [f436, f740])).
fof(f740, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e20, op2(e20, e20)) = h1(e14)) & (op2(op2(e20, op2(e20, e20)), e20) = h1(e12)) & (op2(e20, e20) = h1(e11)) & (h1(e10) = op2(e20, op2(op2(e20, op2(e20, e20)), e20))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax16)).
fof(f436, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f8])).
fof(f4446, plain, (~ spl68_166 | ~ spl68_216), inference(avatar_split_clause, [], [f4440, f1852, f1642])).
fof(f1852, plain, (spl68_216 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl68_216])])).
fof(f4440, plain, (~ (e20 = op2(e23, e22)) | ~ spl68_216), inference(backward_demodulation, [], [f460, f1854])).
fof(f1854, plain, ((e20 = op2(e21, e22)) | ~ spl68_216), inference(avatar_component_clause, [], [f1852])).
fof(f460, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4415, plain, (~ spl68_234 | ~ spl68_235), inference(avatar_contradiction_clause, [], [f4414])).
fof(f4414, plain, ($false | (~ spl68_234 | ~ spl68_235)), inference(subsumption_resolution, [], [f4413, f554])).
fof(f4413, plain, ((e23 = e24) | (~ spl68_234 | ~ spl68_235)), inference(backward_demodulation, [], [f1933, f1929])).
fof(f1929, plain, ((e23 = op2(e20, e24)) | ~ spl68_234), inference(avatar_component_clause, [], [f1927])).
fof(f1927, plain, (spl68_234 <=> (e23 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_234])])).
fof(f4409, plain, (~ spl68_185 | ~ spl68_235), inference(avatar_split_clause, [], [f4404, f1931, f1721])).
fof(f1721, plain, (spl68_185 <=> (e24 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_185])])).
fof(f4404, plain, (~ (e24 = op2(e22, e24)) | ~ spl68_235), inference(backward_demodulation, [], [f476, f1933])).
fof(f476, plain, ~ (op2(e20, e24) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f4408, plain, (~ spl68_210 | ~ spl68_235), inference(avatar_split_clause, [], [f4403, f1931, f1826])).
fof(f1826, plain, (spl68_210 <=> (e24 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl68_210])])).
fof(f4403, plain, (~ (e24 = op2(e21, e24)) | ~ spl68_235), inference(backward_demodulation, [], [f475, f1933])).
fof(f475, plain, ~ (op2(e20, e24) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f4400, plain, (~ spl68_506 | ~ spl68_236), inference(avatar_split_clause, [], [f4394, f1936, f3403])).
fof(f1936, plain, (spl68_236 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl68_236])])).
fof(f4394, plain, (~ (e20 = h1(e11)) | ~ spl68_236), inference(backward_demodulation, [], [f2887, f1938])).
fof(f1938, plain, ((e20 = op2(e20, e23)) | ~ spl68_236), inference(avatar_component_clause, [], [f1936])).
fof(f2887, plain, ~ (op2(e20, e23) = h1(e11)), inference(backward_demodulation, [], [f487, f740])).
fof(f487, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f8])).
fof(f4385, plain, (~ spl68_166 | ~ spl68_241), inference(avatar_split_clause, [], [f4378, f1957, f1642])).
fof(f4378, plain, (~ (e20 = op2(e23, e22)) | ~ spl68_241), inference(backward_demodulation, [], [f457, f1959])).
fof(f1959, plain, ((e20 = op2(e20, e22)) | ~ spl68_241), inference(avatar_component_clause, [], [f1957])).
fof(f457, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4360, plain, (spl68_506 | ~ spl68_251), inference(avatar_split_clause, [], [f4359, f1999, f3403])).
fof(f4359, plain, ((e20 = h1(e11)) | ~ spl68_251), inference(backward_demodulation, [], [f740, f2001])).
fof(f2001, plain, ((e20 = op2(e20, e20)) | ~ spl68_251), inference(avatar_component_clause, [], [f1999])).
fof(f4356, plain, (spl68_179 | ~ spl68_256), inference(avatar_split_clause, [], [f4346, f2020, f1696])).
fof(f4346, plain, ((e23 = op2(e23, e20)) | ~ spl68_256), inference(backward_demodulation, [], [f281, f2022])).
fof(f4354, plain, (spl68_203 | ~ spl68_256), inference(avatar_split_clause, [], [f4344, f2020, f1797])).
fof(f4344, plain, ((e22 = op2(e22, e20)) | ~ spl68_256), inference(backward_demodulation, [], [f279, f2022])).
fof(f279, plain, (e22 = op2(e22, unit2)), inference(cnf_transformation, [], [f5])).
fof(f4352, plain, (spl68_227 | ~ spl68_256), inference(avatar_split_clause, [], [f4342, f2020, f1898])).
fof(f4342, plain, ((e21 = op2(e21, e20)) | ~ spl68_256), inference(backward_demodulation, [], [f277, f2022])).
fof(f277, plain, (e21 = op2(e21, unit2)), inference(cnf_transformation, [], [f5])).
fof(f3734, plain, (~ spl68_537 | ~ spl68_538 | ~ spl68_539 | ~ spl68_540 | spl68_445 | spl68_442 | spl68_439 | ~ spl68_541 | ~ spl68_542 | ~ spl68_543 | ~ spl68_544 | ~ spl68_545 | ~ spl68_546 | ~ spl68_547 | ~ spl68_548 | ~ spl68_549 | ~ spl68_550 | ~ spl68_551 | ~ spl68_552 | ~ spl68_553 | ~ spl68_554 | ~ spl68_555 | ~ spl68_556 | ~ spl68_557 | ~ spl68_558 | ~ spl68_559 | ~ spl68_560 | ~ spl68_561), inference(avatar_split_clause, [], [f3633, f3731, f3727, f3723, f3719, f3715, f3711, f3707, f3703, f3699, f3695, f3691, f3687, f3683, f3679, f3675, f3671, f3667, f3663, f3659, f3655, f3651, f3060, f3077, f3094, f3647, f3643, f3639, f3635])).
fof(f3094, plain, (spl68_445 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl68_445])])).
fof(f3077, plain, (spl68_442 <=> sP61), introduced(avatar_definition, [new_symbols(naming, [spl68_442])])).
fof(f3060, plain, (spl68_439 <=> sP62), introduced(avatar_definition, [new_symbols(naming, [spl68_439])])).
fof(f3633, plain, (~ (h1(e11) = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), inference(forward_demodulation, [], [f3632, f740])).
fof(f3632, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), inference(forward_demodulation, [], [f3631, f2942])).
fof(f3631, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3630, f2942])).
fof(f3630, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3629, f2942])).
fof(f3629, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3628, f2942])).
fof(f3628, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3627, f753])).
fof(f3627, plain, (~ (op2(e20, e24) = h4(op1(e10, e14))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3626, f2942])).
fof(f3626, plain, (~ (h4(op1(e10, e14)) = op2(h4(e10), e24)) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3625, f2919])).
fof(f3625, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3624, f2942])).
fof(f3624, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3623, f753])).
fof(f3623, plain, (~ (h4(op1(e11, e14)) = op2(h4(e11), e24)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3622, f2919])).
fof(f3622, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3621, f2942])).
fof(f3621, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3620, f753])).
fof(f3620, plain, (~ (h4(op1(e12, e14)) = op2(h4(e12), e24)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3619, f2919])).
fof(f3619, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3618, f753])).
fof(f3618, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e20)) | ~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3617, f2942])).
fof(f3617, plain, (~ (e24 = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3616, f2937])).
fof(f3616, plain, (~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3615, f753])).
fof(f3615, plain, (~ (e20 = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3614, f2929])).
fof(f3614, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3613, f753])).
fof(f3613, plain, (~ (h4(e11) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3612, f755])).
fof(f3612, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | ~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3611, f753])).
fof(f3611, plain, (~ (op2(e23, e24) = h4(op1(e13, e14))) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3610, f753])).
fof(f3610, plain, (~ (h4(op1(e13, e14)) = op2(h4(e13), e24)) | ~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3609, f2919])).
fof(f3609, plain, (~ (op2(e24, e20) = h4(op1(e14, e10))) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3608, f2919])).
fof(f3608, plain, (~ (h4(op1(e14, e10)) = op2(h4(e14), e20)) | ~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3607, f2942])).
fof(f3607, plain, (~ (h4(op1(e14, e11)) = op2(e24, h4(e11))) | ~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3606, f2919])).
fof(f3606, plain, (~ (h4(op1(e14, e12)) = op2(e24, h4(e12))) | ~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3605, f2919])).
fof(f3605, plain, (~ (h4(e12) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3604, f2920])).
fof(f3604, plain, (~ (op2(e24, e23) = h4(op1(e14, e13))) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3603, f2919])).
fof(f3603, plain, (~ (h4(op1(e14, e13)) = op2(h4(e14), e23)) | ~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3602, f753])).
fof(f3602, plain, (~ (h5(e11) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3601, f760])).
fof(f3601, plain, (~ (op2(e24, e24) = h4(op1(e14, e14))) | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3600, f2919])).
fof(f3600, plain, (sP62 | sP61 | sP60 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f3599, f3056])).
fof(f3056, plain, ~ sP63, inference(subsumption_resolution, [], [f786, f753])).
fof(f786, plain, (~ (e23 = h4(e13)) | ~ sP63), inference(cnf_transformation, [], [f147])).
fof(f147, plain, ((~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | ~ sP63), inference(nnf_transformation, [], [f89])).
fof(f89, plain, ((~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | ~ sP63), inference(usedef, [], [e89])).
fof(e89, plain, (sP63 <=> (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f3599, plain, (sP63 | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f882, f2919])).
fof(f882, plain, (~ (e24 = h4(e14)) | sP63 | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f94])).
fof(f94, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | sP67 | sP66 | sP65 | sP64 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | sP63 | sP62 | sP61 | sP60 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | sP59 | sP58 | sP57 | sP56 | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | sP55 | sP54 | sP53 | sP52 | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | sP51 | sP50 | sP49 | sP48 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f23, e93, e92, e91, e90, e89, e88, e87, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74])).
fof(f74, plain, ((~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP48), inference(usedef, [], [e74])).
fof(e74, plain, (sP48 <=> (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f75, plain, ((~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP49), inference(usedef, [], [e75])).
fof(e75, plain, (sP49 <=> (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f76, plain, ((~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP50), inference(usedef, [], [e76])).
fof(e76, plain, (sP50 <=> (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f77, plain, ((~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | ~ sP51), inference(usedef, [], [e77])).
fof(e77, plain, (sP51 <=> (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f78, plain, ((~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP52), inference(usedef, [], [e78])).
fof(e78, plain, (sP52 <=> (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f79, plain, ((~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP53), inference(usedef, [], [e79])).
fof(e79, plain, (sP53 <=> (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f80, plain, ((~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP54), inference(usedef, [], [e80])).
fof(e80, plain, (sP54 <=> (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f81, plain, ((~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | ~ sP55), inference(usedef, [], [e81])).
fof(e81, plain, (sP55 <=> (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f82, plain, ((~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP56), inference(usedef, [], [e82])).
fof(e82, plain, (sP56 <=> (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f83, plain, ((~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP57), inference(usedef, [], [e83])).
fof(e83, plain, (sP57 <=> (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f84, plain, ((~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP58), inference(usedef, [], [e84])).
fof(e84, plain, (sP58 <=> (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f85, plain, ((~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | ~ sP59), inference(usedef, [], [e85])).
fof(e85, plain, (sP59 <=> (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f86, plain, ((~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP60), inference(usedef, [], [e86])).
fof(e86, plain, (sP60 <=> (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f87, plain, ((~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP61), inference(usedef, [], [e87])).
fof(e87, plain, (sP61 <=> (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f88, plain, ((~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP62), inference(usedef, [], [e88])).
fof(e88, plain, (sP62 <=> (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f90, plain, ((~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ sP64), inference(usedef, [], [e90])).
fof(e90, plain, (sP64 <=> (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP64])])).
fof(f91, plain, ((~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | ~ sP65), inference(usedef, [], [e91])).
fof(e91, plain, (sP65 <=> (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP65])])).
fof(f92, plain, ((~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | ~ sP66), inference(usedef, [], [e92])).
fof(e92, plain, (sP66 <=> (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP66])])).
fof(f93, plain, ((~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | ~ sP67), inference(usedef, [], [e93])).
fof(e93, plain, (sP67 <=> (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP67])])).
fof(f23, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f21])).
fof(f21, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', co1)).
fof(f3108, plain, ~ spl68_445, inference(avatar_split_clause, [], [f3107, f3094])).
fof(f3107, plain, ~ sP60, inference(subsumption_resolution, [], [f798, f2942])).
fof(f798, plain, (~ (e20 = h4(e10)) | ~ sP60), inference(cnf_transformation, [], [f150])).
fof(f150, plain, ((~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP60), inference(nnf_transformation, [], [f86])).
fof(f3089, plain, (~ spl68_442 | ~ spl68_444), inference(avatar_split_clause, [], [f794, f3086, f3077])).
fof(f794, plain, (~ (e21 = h4(e11)) | ~ sP61), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ((~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP61), inference(nnf_transformation, [], [f87])).
fof(f3067, plain, (~ spl68_439 | ~ spl68_440), inference(avatar_split_clause, [], [f790, f3064, f3060])).
fof(f790, plain, (~ (e22 = h4(e12)) | ~ sP62), inference(cnf_transformation, [], [f148])).
fof(f148, plain, ((~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP62), inference(nnf_transformation, [], [f88])).
fof(f2878, plain, spl68_162, inference(avatar_split_clause, [], [f735, f1625])).
fof(f735, plain, (e21 = op2(e23, e23)), inference(cnf_transformation, [], [f15])).
fof(f2877, plain, spl68_138, inference(avatar_split_clause, [], [f2876, f1524])).
fof(f2876, plain, (e22 = op2(e24, e23)), inference(forward_demodulation, [], [f736, f737])).
fof(f736, plain, (e22 = op2(op2(e23, op2(e23, e23)), e23)), inference(cnf_transformation, [], [f15])).
fof(f2874, plain, spl68_32, inference(avatar_split_clause, [], [f731, f1029])).
fof(f731, plain, (e11 = op1(e13, e13)), inference(cnf_transformation, [], [f14])).
fof(f2873, plain, spl68_8, inference(avatar_split_clause, [], [f2872, f928])).
fof(f2872, plain, (e12 = op1(e14, e13)), inference(forward_demodulation, [], [f732, f733])).
fof(f732, plain, (e12 = op1(op1(e13, op1(e13, e13)), e13)), inference(cnf_transformation, [], [f14])).
fof(f2871, plain, (spl68_416 | spl68_413 | spl68_410 | spl68_407 | spl68_404 | spl68_400 | spl68_398 | spl68_395 | spl68_392 | spl68_389 | spl68_385 | spl68_381 | spl68_379 | spl68_376 | spl68_373 | spl68_369 | spl68_365 | spl68_361 | spl68_359 | spl68_356 | spl68_352 | spl68_348 | spl68_344 | spl68_340), inference(avatar_split_clause, [], [f888, f2482, f2501, f2520, f2539, f2558, f2573, f2584, f2603, f2622, f2641, f2656, f2671, f2682, f2701, f2720, f2735, f2750, f2765, f2776, f2795, f2810, f2825, f2840, f2855])).
fof(f2855, plain, (spl68_416 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl68_416])])).
fof(f2840, plain, (spl68_413 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl68_413])])).
fof(f2825, plain, (spl68_410 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl68_410])])).
fof(f2810, plain, (spl68_407 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl68_407])])).
fof(f2795, plain, (spl68_404 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl68_404])])).
fof(f2776, plain, (spl68_400 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl68_400])])).
fof(f2765, plain, (spl68_398 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl68_398])])).
fof(f2750, plain, (spl68_395 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl68_395])])).
fof(f2735, plain, (spl68_392 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl68_392])])).
fof(f2720, plain, (spl68_389 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl68_389])])).
fof(f2701, plain, (spl68_385 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl68_385])])).
fof(f2682, plain, (spl68_381 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl68_381])])).
fof(f2671, plain, (spl68_379 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl68_379])])).
fof(f2656, plain, (spl68_376 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl68_376])])).
fof(f2641, plain, (spl68_373 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl68_373])])).
fof(f2622, plain, (spl68_369 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl68_369])])).
fof(f2603, plain, (spl68_365 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl68_365])])).
fof(f2584, plain, (spl68_361 <=> sP41), introduced(avatar_definition, [new_symbols(naming, [spl68_361])])).
fof(f2573, plain, (spl68_359 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl68_359])])).
fof(f2558, plain, (spl68_356 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl68_356])])).
fof(f2539, plain, (spl68_352 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl68_352])])).
fof(f2520, plain, (spl68_348 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl68_348])])).
fof(f2501, plain, (spl68_344 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl68_344])])).
fof(f2482, plain, (spl68_340 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl68_340])])).
fof(f888, plain, (sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24), inference(trivial_inequality_removal, [], [f727])).
fof(f727, plain, (~ (op2(e24, e24) = op2(e24, e24)) | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e24 = op2(op2(e24, e24), e24)) & (e24 = op2(op2(e24, e24), e24)) & ~ (op2(e24, e24) = op2(e24, e24))) | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24), inference(definition_folding, [], [f13, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49])).
fof(f49, plain, ((~ (e20 = op2(op2(e20, e20), e20)) & (e20 = op2(op2(e20, e20), e20)) & ~ (op2(e20, e20) = op2(e20, e20))) | ~ sP24), inference(usedef, [], [e49])).
fof(e49, plain, (sP24 <=> (~ (e20 = op2(op2(e20, e20), e20)) & (e20 = op2(op2(e20, e20), e20)) & ~ (op2(e20, e20) = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f50, plain, ((~ (e21 = op2(op2(e20, e21), e20)) & (e20 = op2(op2(e20, e21), e21)) & ~ (op2(e20, e21) = op2(e21, e20))) | ~ sP25), inference(usedef, [], [e50])).
fof(e50, plain, (sP25 <=> (~ (e21 = op2(op2(e20, e21), e20)) & (e20 = op2(op2(e20, e21), e21)) & ~ (op2(e20, e21) = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f51, plain, ((~ (e22 = op2(op2(e20, e22), e20)) & (e20 = op2(op2(e20, e22), e22)) & ~ (op2(e20, e22) = op2(e22, e20))) | ~ sP26), inference(usedef, [], [e51])).
fof(e51, plain, (sP26 <=> (~ (e22 = op2(op2(e20, e22), e20)) & (e20 = op2(op2(e20, e22), e22)) & ~ (op2(e20, e22) = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f52, plain, ((~ (e23 = op2(op2(e20, e23), e20)) & (e20 = op2(op2(e20, e23), e23)) & ~ (op2(e20, e23) = op2(e23, e20))) | ~ sP27), inference(usedef, [], [e52])).
fof(e52, plain, (sP27 <=> (~ (e23 = op2(op2(e20, e23), e20)) & (e20 = op2(op2(e20, e23), e23)) & ~ (op2(e20, e23) = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f53, plain, ((~ (e24 = op2(op2(e20, e24), e20)) & (e20 = op2(op2(e20, e24), e24)) & ~ (op2(e20, e24) = op2(e24, e20))) | ~ sP28), inference(usedef, [], [e53])).
fof(e53, plain, (sP28 <=> (~ (e24 = op2(op2(e20, e24), e20)) & (e20 = op2(op2(e20, e24), e24)) & ~ (op2(e20, e24) = op2(e24, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f54, plain, ((~ (e20 = op2(op2(e21, e20), e21)) & (e21 = op2(op2(e21, e20), e20)) & ~ (op2(e20, e21) = op2(e21, e20))) | ~ sP29), inference(usedef, [], [e54])).
fof(e54, plain, (sP29 <=> (~ (e20 = op2(op2(e21, e20), e21)) & (e21 = op2(op2(e21, e20), e20)) & ~ (op2(e20, e21) = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f55, plain, ((~ (e21 = op2(op2(e21, e21), e21)) & (e21 = op2(op2(e21, e21), e21)) & ~ (op2(e21, e21) = op2(e21, e21))) | ~ sP30), inference(usedef, [], [e55])).
fof(e55, plain, (sP30 <=> (~ (e21 = op2(op2(e21, e21), e21)) & (e21 = op2(op2(e21, e21), e21)) & ~ (op2(e21, e21) = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f56, plain, ((~ (e22 = op2(op2(e21, e22), e21)) & (e21 = op2(op2(e21, e22), e22)) & ~ (op2(e21, e22) = op2(e22, e21))) | ~ sP31), inference(usedef, [], [e56])).
fof(e56, plain, (sP31 <=> (~ (e22 = op2(op2(e21, e22), e21)) & (e21 = op2(op2(e21, e22), e22)) & ~ (op2(e21, e22) = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f57, plain, ((~ (e23 = op2(op2(e21, e23), e21)) & (e21 = op2(op2(e21, e23), e23)) & ~ (op2(e21, e23) = op2(e23, e21))) | ~ sP32), inference(usedef, [], [e57])).
fof(e57, plain, (sP32 <=> (~ (e23 = op2(op2(e21, e23), e21)) & (e21 = op2(op2(e21, e23), e23)) & ~ (op2(e21, e23) = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f58, plain, ((~ (e24 = op2(op2(e21, e24), e21)) & (e21 = op2(op2(e21, e24), e24)) & ~ (op2(e21, e24) = op2(e24, e21))) | ~ sP33), inference(usedef, [], [e58])).
fof(e58, plain, (sP33 <=> (~ (e24 = op2(op2(e21, e24), e21)) & (e21 = op2(op2(e21, e24), e24)) & ~ (op2(e21, e24) = op2(e24, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f59, plain, ((~ (e20 = op2(op2(e22, e20), e22)) & (e22 = op2(op2(e22, e20), e20)) & ~ (op2(e20, e22) = op2(e22, e20))) | ~ sP34), inference(usedef, [], [e59])).
fof(e59, plain, (sP34 <=> (~ (e20 = op2(op2(e22, e20), e22)) & (e22 = op2(op2(e22, e20), e20)) & ~ (op2(e20, e22) = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f60, plain, ((~ (e21 = op2(op2(e22, e21), e22)) & (e22 = op2(op2(e22, e21), e21)) & ~ (op2(e21, e22) = op2(e22, e21))) | ~ sP35), inference(usedef, [], [e60])).
fof(e60, plain, (sP35 <=> (~ (e21 = op2(op2(e22, e21), e22)) & (e22 = op2(op2(e22, e21), e21)) & ~ (op2(e21, e22) = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f61, plain, ((~ (e22 = op2(op2(e22, e22), e22)) & (e22 = op2(op2(e22, e22), e22)) & ~ (op2(e22, e22) = op2(e22, e22))) | ~ sP36), inference(usedef, [], [e61])).
fof(e61, plain, (sP36 <=> (~ (e22 = op2(op2(e22, e22), e22)) & (e22 = op2(op2(e22, e22), e22)) & ~ (op2(e22, e22) = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f62, plain, ((~ (e23 = op2(op2(e22, e23), e22)) & (e22 = op2(op2(e22, e23), e23)) & ~ (op2(e22, e23) = op2(e23, e22))) | ~ sP37), inference(usedef, [], [e62])).
fof(e62, plain, (sP37 <=> (~ (e23 = op2(op2(e22, e23), e22)) & (e22 = op2(op2(e22, e23), e23)) & ~ (op2(e22, e23) = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f63, plain, ((~ (e24 = op2(op2(e22, e24), e22)) & (e22 = op2(op2(e22, e24), e24)) & ~ (op2(e22, e24) = op2(e24, e22))) | ~ sP38), inference(usedef, [], [e63])).
fof(e63, plain, (sP38 <=> (~ (e24 = op2(op2(e22, e24), e22)) & (e22 = op2(op2(e22, e24), e24)) & ~ (op2(e22, e24) = op2(e24, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f64, plain, ((~ (e20 = op2(op2(e23, e20), e23)) & (e23 = op2(op2(e23, e20), e20)) & ~ (op2(e20, e23) = op2(e23, e20))) | ~ sP39), inference(usedef, [], [e64])).
fof(e64, plain, (sP39 <=> (~ (e20 = op2(op2(e23, e20), e23)) & (e23 = op2(op2(e23, e20), e20)) & ~ (op2(e20, e23) = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f65, plain, ((~ (e21 = op2(op2(e23, e21), e23)) & (e23 = op2(op2(e23, e21), e21)) & ~ (op2(e21, e23) = op2(e23, e21))) | ~ sP40), inference(usedef, [], [e65])).
fof(e65, plain, (sP40 <=> (~ (e21 = op2(op2(e23, e21), e23)) & (e23 = op2(op2(e23, e21), e21)) & ~ (op2(e21, e23) = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f66, plain, ((~ (e22 = op2(op2(e23, e22), e23)) & (e23 = op2(op2(e23, e22), e22)) & ~ (op2(e22, e23) = op2(e23, e22))) | ~ sP41), inference(usedef, [], [e66])).
fof(e66, plain, (sP41 <=> (~ (e22 = op2(op2(e23, e22), e23)) & (e23 = op2(op2(e23, e22), e22)) & ~ (op2(e22, e23) = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f67, plain, ((~ (e23 = op2(op2(e23, e23), e23)) & (e23 = op2(op2(e23, e23), e23)) & ~ (op2(e23, e23) = op2(e23, e23))) | ~ sP42), inference(usedef, [], [e67])).
fof(e67, plain, (sP42 <=> (~ (e23 = op2(op2(e23, e23), e23)) & (e23 = op2(op2(e23, e23), e23)) & ~ (op2(e23, e23) = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f68, plain, ((~ (e24 = op2(op2(e23, e24), e23)) & (e23 = op2(op2(e23, e24), e24)) & ~ (op2(e23, e24) = op2(e24, e23))) | ~ sP43), inference(usedef, [], [e68])).
fof(e68, plain, (sP43 <=> (~ (e24 = op2(op2(e23, e24), e23)) & (e23 = op2(op2(e23, e24), e24)) & ~ (op2(e23, e24) = op2(e24, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f69, plain, ((~ (e20 = op2(op2(e24, e20), e24)) & (e24 = op2(op2(e24, e20), e20)) & ~ (op2(e20, e24) = op2(e24, e20))) | ~ sP44), inference(usedef, [], [e69])).
fof(e69, plain, (sP44 <=> (~ (e20 = op2(op2(e24, e20), e24)) & (e24 = op2(op2(e24, e20), e20)) & ~ (op2(e20, e24) = op2(e24, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f70, plain, ((~ (e21 = op2(op2(e24, e21), e24)) & (e24 = op2(op2(e24, e21), e21)) & ~ (op2(e21, e24) = op2(e24, e21))) | ~ sP45), inference(usedef, [], [e70])).
fof(e70, plain, (sP45 <=> (~ (e21 = op2(op2(e24, e21), e24)) & (e24 = op2(op2(e24, e21), e21)) & ~ (op2(e21, e24) = op2(e24, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f71, plain, ((~ (e22 = op2(op2(e24, e22), e24)) & (e24 = op2(op2(e24, e22), e22)) & ~ (op2(e22, e24) = op2(e24, e22))) | ~ sP46), inference(usedef, [], [e71])).
fof(e71, plain, (sP46 <=> (~ (e22 = op2(op2(e24, e22), e24)) & (e24 = op2(op2(e24, e22), e22)) & ~ (op2(e22, e24) = op2(e24, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f72, plain, ((~ (e23 = op2(op2(e24, e23), e24)) & (e24 = op2(op2(e24, e23), e23)) & ~ (op2(e23, e24) = op2(e24, e23))) | ~ sP47), inference(usedef, [], [e72])).
fof(e72, plain, (sP47 <=> (~ (e23 = op2(op2(e24, e23), e24)) & (e24 = op2(op2(e24, e23), e23)) & ~ (op2(e23, e24) = op2(e24, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f13, plain, ((~ (e24 = op2(op2(e24, e24), e24)) & (e24 = op2(op2(e24, e24), e24)) & ~ (op2(e24, e24) = op2(e24, e24))) | (~ (e23 = op2(op2(e24, e23), e24)) & (e24 = op2(op2(e24, e23), e23)) & ~ (op2(e23, e24) = op2(e24, e23))) | (~ (e22 = op2(op2(e24, e22), e24)) & (e24 = op2(op2(e24, e22), e22)) & ~ (op2(e22, e24) = op2(e24, e22))) | (~ (e21 = op2(op2(e24, e21), e24)) & (e24 = op2(op2(e24, e21), e21)) & ~ (op2(e21, e24) = op2(e24, e21))) | (~ (e20 = op2(op2(e24, e20), e24)) & (e24 = op2(op2(e24, e20), e20)) & ~ (op2(e20, e24) = op2(e24, e20))) | (~ (e24 = op2(op2(e23, e24), e23)) & (e23 = op2(op2(e23, e24), e24)) & ~ (op2(e23, e24) = op2(e24, e23))) | (~ (e23 = op2(op2(e23, e23), e23)) & (e23 = op2(op2(e23, e23), e23)) & ~ (op2(e23, e23) = op2(e23, e23))) | (~ (e22 = op2(op2(e23, e22), e23)) & (e23 = op2(op2(e23, e22), e22)) & ~ (op2(e22, e23) = op2(e23, e22))) | (~ (e21 = op2(op2(e23, e21), e23)) & (e23 = op2(op2(e23, e21), e21)) & ~ (op2(e21, e23) = op2(e23, e21))) | (~ (e20 = op2(op2(e23, e20), e23)) & (e23 = op2(op2(e23, e20), e20)) & ~ (op2(e20, e23) = op2(e23, e20))) | (~ (e24 = op2(op2(e22, e24), e22)) & (e22 = op2(op2(e22, e24), e24)) & ~ (op2(e22, e24) = op2(e24, e22))) | (~ (e23 = op2(op2(e22, e23), e22)) & (e22 = op2(op2(e22, e23), e23)) & ~ (op2(e22, e23) = op2(e23, e22))) | (~ (e22 = op2(op2(e22, e22), e22)) & (e22 = op2(op2(e22, e22), e22)) & ~ (op2(e22, e22) = op2(e22, e22))) | (~ (e21 = op2(op2(e22, e21), e22)) & (e22 = op2(op2(e22, e21), e21)) & ~ (op2(e21, e22) = op2(e22, e21))) | (~ (e20 = op2(op2(e22, e20), e22)) & (e22 = op2(op2(e22, e20), e20)) & ~ (op2(e20, e22) = op2(e22, e20))) | (~ (e24 = op2(op2(e21, e24), e21)) & (e21 = op2(op2(e21, e24), e24)) & ~ (op2(e21, e24) = op2(e24, e21))) | (~ (e23 = op2(op2(e21, e23), e21)) & (e21 = op2(op2(e21, e23), e23)) & ~ (op2(e21, e23) = op2(e23, e21))) | (~ (e22 = op2(op2(e21, e22), e21)) & (e21 = op2(op2(e21, e22), e22)) & ~ (op2(e21, e22) = op2(e22, e21))) | (~ (e21 = op2(op2(e21, e21), e21)) & (e21 = op2(op2(e21, e21), e21)) & ~ (op2(e21, e21) = op2(e21, e21))) | (~ (e20 = op2(op2(e21, e20), e21)) & (e21 = op2(op2(e21, e20), e20)) & ~ (op2(e20, e21) = op2(e21, e20))) | (~ (e24 = op2(op2(e20, e24), e20)) & (e20 = op2(op2(e20, e24), e24)) & ~ (op2(e20, e24) = op2(e24, e20))) | (~ (e23 = op2(op2(e20, e23), e20)) & (e20 = op2(op2(e20, e23), e23)) & ~ (op2(e20, e23) = op2(e23, e20))) | (~ (e22 = op2(op2(e20, e22), e20)) & (e20 = op2(op2(e20, e22), e22)) & ~ (op2(e20, e22) = op2(e22, e20))) | (~ (e21 = op2(op2(e20, e21), e20)) & (e20 = op2(op2(e20, e21), e21)) & ~ (op2(e20, e21) = op2(e21, e20))) | (~ (e20 = op2(op2(e20, e20), e20)) & (e20 = op2(op2(e20, e20), e20)) & ~ (op2(e20, e20) = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax13)).
fof(f2864, plain, ~ spl68_416, inference(avatar_split_clause, [], [f889, f2855])).
fof(f889, plain, ~ sP24, inference(trivial_inequality_removal, [], [f724])).
fof(f724, plain, (~ (op2(e20, e20) = op2(e20, e20)) | ~ sP24), inference(cnf_transformation, [], [f142])).
fof(f142, plain, ((~ (e20 = op2(op2(e20, e20), e20)) & (e20 = op2(op2(e20, e20), e20)) & ~ (op2(e20, e20) = op2(e20, e20))) | ~ sP24), inference(nnf_transformation, [], [f49])).
fof(f2853, plain, (~ spl68_413 | ~ spl68_403), inference(avatar_split_clause, [], [f721, f2790, f2840])).
fof(f721, plain, (~ (op2(e20, e21) = op2(e21, e20)) | ~ sP25), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ((~ (e21 = op2(op2(e20, e21), e20)) & (e20 = op2(op2(e20, e21), e21)) & ~ (op2(e20, e21) = op2(e21, e20))) | ~ sP25), inference(nnf_transformation, [], [f50])).
fof(f2837, plain, (~ spl68_410 | spl68_412), inference(avatar_split_clause, [], [f719, f2834, f2825])).
fof(f719, plain, ((e20 = op2(op2(e20, e22), e22)) | ~ sP26), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ((~ (e22 = op2(op2(e20, e22), e20)) & (e20 = op2(op2(e20, e22), e22)) & ~ (op2(e20, e22) = op2(e22, e20))) | ~ sP26), inference(nnf_transformation, [], [f51])).
fof(f2823, plain, (~ spl68_407 | ~ spl68_372), inference(avatar_split_clause, [], [f715, f2636, f2810])).
fof(f715, plain, (~ (op2(e20, e23) = op2(e23, e20)) | ~ sP27), inference(cnf_transformation, [], [f139])).
fof(f139, plain, ((~ (e23 = op2(op2(e20, e23), e20)) & (e20 = op2(op2(e20, e23), e23)) & ~ (op2(e20, e23) = op2(e23, e20))) | ~ sP27), inference(nnf_transformation, [], [f52])).
fof(f2808, plain, (~ spl68_404 | ~ spl68_355), inference(avatar_split_clause, [], [f712, f2553, f2795])).
fof(f712, plain, (~ (op2(e20, e24) = op2(e24, e20)) | ~ sP28), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ((~ (e24 = op2(op2(e20, e24), e20)) & (e20 = op2(op2(e20, e24), e24)) & ~ (op2(e20, e24) = op2(e24, e20))) | ~ sP28), inference(nnf_transformation, [], [f53])).
fof(f2793, plain, (~ spl68_400 | ~ spl68_403), inference(avatar_split_clause, [], [f709, f2790, f2776])).
fof(f709, plain, (~ (op2(e20, e21) = op2(e21, e20)) | ~ sP29), inference(cnf_transformation, [], [f137])).
fof(f137, plain, ((~ (e20 = op2(op2(e21, e20), e21)) & (e21 = op2(op2(e21, e20), e20)) & ~ (op2(e20, e21) = op2(e21, e20))) | ~ sP29), inference(nnf_transformation, [], [f54])).
fof(f2774, plain, ~ spl68_398, inference(avatar_split_clause, [], [f890, f2765])).
fof(f890, plain, ~ sP30, inference(trivial_inequality_removal, [], [f706])).
fof(f706, plain, (~ (op2(e21, e21) = op2(e21, e21)) | ~ sP30), inference(cnf_transformation, [], [f136])).
fof(f136, plain, ((~ (e21 = op2(op2(e21, e21), e21)) & (e21 = op2(op2(e21, e21), e21)) & ~ (op2(e21, e21) = op2(e21, e21))) | ~ sP30), inference(nnf_transformation, [], [f55])).
fof(f2762, plain, (~ spl68_395 | spl68_397), inference(avatar_split_clause, [], [f704, f2759, f2750])).
fof(f704, plain, ((e21 = op2(op2(e21, e22), e22)) | ~ sP31), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ((~ (e22 = op2(op2(e21, e22), e21)) & (e21 = op2(op2(e21, e22), e22)) & ~ (op2(e21, e22) = op2(e22, e21))) | ~ sP31), inference(nnf_transformation, [], [f56])).
fof(f2748, plain, (~ spl68_392 | ~ spl68_368), inference(avatar_split_clause, [], [f700, f2617, f2735])).
fof(f700, plain, (~ (op2(e21, e23) = op2(e23, e21)) | ~ sP32), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ((~ (e23 = op2(op2(e21, e23), e21)) & (e21 = op2(op2(e21, e23), e23)) & ~ (op2(e21, e23) = op2(e23, e21))) | ~ sP32), inference(nnf_transformation, [], [f57])).
fof(f2747, plain, (~ spl68_392 | spl68_394), inference(avatar_split_clause, [], [f701, f2744, f2735])).
fof(f701, plain, ((e21 = op2(op2(e21, e23), e23)) | ~ sP32), inference(cnf_transformation, [], [f134])).
fof(f2732, plain, (~ spl68_389 | spl68_391), inference(avatar_split_clause, [], [f698, f2729, f2720])).
fof(f698, plain, ((e21 = op2(op2(e21, e24), e24)) | ~ sP33), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ((~ (e24 = op2(op2(e21, e24), e21)) & (e21 = op2(op2(e21, e24), e24)) & ~ (op2(e21, e24) = op2(e24, e21))) | ~ sP33), inference(nnf_transformation, [], [f58])).
fof(f2727, plain, (~ spl68_389 | ~ spl68_390), inference(avatar_split_clause, [], [f699, f2724, f2720])).
fof(f699, plain, (~ (e24 = op2(op2(e21, e24), e21)) | ~ sP33), inference(cnf_transformation, [], [f133])).
fof(f2718, plain, (~ spl68_385 | ~ spl68_388), inference(avatar_split_clause, [], [f694, f2715, f2701])).
fof(f694, plain, (~ (op2(e20, e22) = op2(e22, e20)) | ~ sP34), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ((~ (e20 = op2(op2(e22, e20), e22)) & (e22 = op2(op2(e22, e20), e20)) & ~ (op2(e20, e22) = op2(e22, e20))) | ~ sP34), inference(nnf_transformation, [], [f59])).
fof(f2699, plain, (~ spl68_381 | ~ spl68_384), inference(avatar_split_clause, [], [f691, f2696, f2682])).
fof(f691, plain, (~ (op2(e21, e22) = op2(e22, e21)) | ~ sP35), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ((~ (e21 = op2(op2(e22, e21), e22)) & (e22 = op2(op2(e22, e21), e21)) & ~ (op2(e21, e22) = op2(e22, e21))) | ~ sP35), inference(nnf_transformation, [], [f60])).
fof(f2694, plain, (~ spl68_381 | spl68_383), inference(avatar_split_clause, [], [f692, f2691, f2682])).
fof(f692, plain, ((e22 = op2(op2(e22, e21), e21)) | ~ sP35), inference(cnf_transformation, [], [f131])).
fof(f2680, plain, ~ spl68_379, inference(avatar_split_clause, [], [f891, f2671])).
fof(f891, plain, ~ sP36, inference(trivial_inequality_removal, [], [f688])).
fof(f688, plain, (~ (op2(e22, e22) = op2(e22, e22)) | ~ sP36), inference(cnf_transformation, [], [f130])).
fof(f130, plain, ((~ (e22 = op2(op2(e22, e22), e22)) & (e22 = op2(op2(e22, e22), e22)) & ~ (op2(e22, e22) = op2(e22, e22))) | ~ sP36), inference(nnf_transformation, [], [f61])).
fof(f2668, plain, (~ spl68_376 | spl68_378), inference(avatar_split_clause, [], [f686, f2665, f2656])).
fof(f686, plain, ((e22 = op2(op2(e22, e23), e23)) | ~ sP37), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ((~ (e23 = op2(op2(e22, e23), e22)) & (e22 = op2(op2(e22, e23), e23)) & ~ (op2(e22, e23) = op2(e23, e22))) | ~ sP37), inference(nnf_transformation, [], [f62])).
fof(f2663, plain, (~ spl68_376 | ~ spl68_377), inference(avatar_split_clause, [], [f687, f2660, f2656])).
fof(f687, plain, (~ (e23 = op2(op2(e22, e23), e22)) | ~ sP37), inference(cnf_transformation, [], [f129])).
fof(f2654, plain, (~ spl68_373 | ~ spl68_347), inference(avatar_split_clause, [], [f682, f2515, f2641])).
fof(f682, plain, (~ (op2(e22, e24) = op2(e24, e22)) | ~ sP38), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ((~ (e24 = op2(op2(e22, e24), e22)) & (e22 = op2(op2(e22, e24), e24)) & ~ (op2(e22, e24) = op2(e24, e22))) | ~ sP38), inference(nnf_transformation, [], [f63])).
fof(f2653, plain, (~ spl68_373 | spl68_375), inference(avatar_split_clause, [], [f683, f2650, f2641])).
fof(f683, plain, ((e22 = op2(op2(e22, e24), e24)) | ~ sP38), inference(cnf_transformation, [], [f128])).
fof(f2639, plain, (~ spl68_369 | ~ spl68_372), inference(avatar_split_clause, [], [f679, f2636, f2622])).
fof(f679, plain, (~ (op2(e20, e23) = op2(e23, e20)) | ~ sP39), inference(cnf_transformation, [], [f127])).
fof(f127, plain, ((~ (e20 = op2(op2(e23, e20), e23)) & (e23 = op2(op2(e23, e20), e20)) & ~ (op2(e20, e23) = op2(e23, e20))) | ~ sP39), inference(nnf_transformation, [], [f64])).
fof(f2620, plain, (~ spl68_365 | ~ spl68_368), inference(avatar_split_clause, [], [f676, f2617, f2603])).
fof(f676, plain, (~ (op2(e21, e23) = op2(e23, e21)) | ~ sP40), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ((~ (e21 = op2(op2(e23, e21), e23)) & (e23 = op2(op2(e23, e21), e21)) & ~ (op2(e21, e23) = op2(e23, e21))) | ~ sP40), inference(nnf_transformation, [], [f65])).
fof(f2615, plain, (~ spl68_365 | spl68_367), inference(avatar_split_clause, [], [f677, f2612, f2603])).
fof(f677, plain, ((e23 = op2(op2(e23, e21), e21)) | ~ sP40), inference(cnf_transformation, [], [f126])).
fof(f2596, plain, (~ spl68_361 | spl68_363), inference(avatar_split_clause, [], [f674, f2593, f2584])).
fof(f674, plain, ((e23 = op2(op2(e23, e22), e22)) | ~ sP41), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ((~ (e22 = op2(op2(e23, e22), e23)) & (e23 = op2(op2(e23, e22), e22)) & ~ (op2(e22, e23) = op2(e23, e22))) | ~ sP41), inference(nnf_transformation, [], [f66])).
fof(f2582, plain, ~ spl68_359, inference(avatar_split_clause, [], [f892, f2573])).
fof(f892, plain, ~ sP42, inference(trivial_inequality_removal, [], [f670])).
fof(f670, plain, (~ (op2(e23, e23) = op2(e23, e23)) | ~ sP42), inference(cnf_transformation, [], [f124])).
fof(f124, plain, ((~ (e23 = op2(op2(e23, e23), e23)) & (e23 = op2(op2(e23, e23), e23)) & ~ (op2(e23, e23) = op2(e23, e23))) | ~ sP42), inference(nnf_transformation, [], [f67])).
fof(f2571, plain, (~ spl68_356 | ~ spl68_343), inference(avatar_split_clause, [], [f667, f2496, f2558])).
fof(f667, plain, (~ (op2(e23, e24) = op2(e24, e23)) | ~ sP43), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ((~ (e24 = op2(op2(e23, e24), e23)) & (e23 = op2(op2(e23, e24), e24)) & ~ (op2(e23, e24) = op2(e24, e23))) | ~ sP43), inference(nnf_transformation, [], [f68])).
fof(f2556, plain, (~ spl68_352 | ~ spl68_355), inference(avatar_split_clause, [], [f664, f2553, f2539])).
fof(f664, plain, (~ (op2(e20, e24) = op2(e24, e20)) | ~ sP44), inference(cnf_transformation, [], [f122])).
fof(f122, plain, ((~ (e20 = op2(op2(e24, e20), e24)) & (e24 = op2(op2(e24, e20), e20)) & ~ (op2(e20, e24) = op2(e24, e20))) | ~ sP44), inference(nnf_transformation, [], [f69])).
fof(f2532, plain, (~ spl68_348 | spl68_350), inference(avatar_split_clause, [], [f662, f2529, f2520])).
fof(f662, plain, ((e24 = op2(op2(e24, e21), e21)) | ~ sP45), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ((~ (e21 = op2(op2(e24, e21), e24)) & (e24 = op2(op2(e24, e21), e21)) & ~ (op2(e21, e24) = op2(e24, e21))) | ~ sP45), inference(nnf_transformation, [], [f70])).
fof(f2518, plain, (~ spl68_344 | ~ spl68_347), inference(avatar_split_clause, [], [f658, f2515, f2501])).
fof(f658, plain, (~ (op2(e22, e24) = op2(e24, e22)) | ~ sP46), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ((~ (e22 = op2(op2(e24, e22), e24)) & (e24 = op2(op2(e24, e22), e22)) & ~ (op2(e22, e24) = op2(e24, e22))) | ~ sP46), inference(nnf_transformation, [], [f71])).
fof(f2513, plain, (~ spl68_344 | spl68_346), inference(avatar_split_clause, [], [f659, f2510, f2501])).
fof(f659, plain, ((e24 = op2(op2(e24, e22), e22)) | ~ sP46), inference(cnf_transformation, [], [f120])).
fof(f2499, plain, (~ spl68_340 | ~ spl68_343), inference(avatar_split_clause, [], [f655, f2496, f2482])).
fof(f655, plain, (~ (op2(e23, e24) = op2(e24, e23)) | ~ sP47), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ((~ (e23 = op2(op2(e24, e23), e24)) & (e24 = op2(op2(e24, e23), e23)) & ~ (op2(e23, e24) = op2(e24, e23))) | ~ sP47), inference(nnf_transformation, [], [f72])).
fof(f2480, plain, (spl68_337 | spl68_334 | spl68_331 | spl68_328 | spl68_325 | spl68_321 | spl68_319 | spl68_316 | spl68_313 | spl68_310 | spl68_306 | spl68_302 | spl68_300 | spl68_297 | spl68_294 | spl68_290 | spl68_286 | spl68_282 | spl68_280 | spl68_277 | spl68_273 | spl68_269 | spl68_265 | spl68_261), inference(avatar_split_clause, [], [f893, f2091, f2110, f2129, f2148, f2167, f2182, f2193, f2212, f2231, f2250, f2265, f2280, f2291, f2310, f2329, f2344, f2359, f2374, f2385, f2404, f2419, f2434, f2449, f2464])).
fof(f2464, plain, (spl68_337 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl68_337])])).
fof(f2449, plain, (spl68_334 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl68_334])])).
fof(f2434, plain, (spl68_331 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl68_331])])).
fof(f2419, plain, (spl68_328 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl68_328])])).
fof(f2404, plain, (spl68_325 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl68_325])])).
fof(f2385, plain, (spl68_321 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl68_321])])).
fof(f2374, plain, (spl68_319 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl68_319])])).
fof(f2359, plain, (spl68_316 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl68_316])])).
fof(f2344, plain, (spl68_313 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl68_313])])).
fof(f2329, plain, (spl68_310 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl68_310])])).
fof(f2310, plain, (spl68_306 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl68_306])])).
fof(f2291, plain, (spl68_302 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl68_302])])).
fof(f2280, plain, (spl68_300 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl68_300])])).
fof(f2265, plain, (spl68_297 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl68_297])])).
fof(f2250, plain, (spl68_294 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl68_294])])).
fof(f2231, plain, (spl68_290 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl68_290])])).
fof(f2212, plain, (spl68_286 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl68_286])])).
fof(f2193, plain, (spl68_282 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl68_282])])).
fof(f2182, plain, (spl68_280 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl68_280])])).
fof(f2167, plain, (spl68_277 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl68_277])])).
fof(f2148, plain, (spl68_273 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl68_273])])).
fof(f2129, plain, (spl68_269 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl68_269])])).
fof(f2110, plain, (spl68_265 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl68_265])])).
fof(f2091, plain, (spl68_261 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl68_261])])).
fof(f893, plain, (sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(trivial_inequality_removal, [], [f652])).
fof(f652, plain, (~ (op1(e14, e14) = op1(e14, e14)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e14 = op1(op1(e14, e14), e14)) & (e14 = op1(op1(e14, e14), e14)) & ~ (op1(e14, e14) = op1(e14, e14))) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(definition_folding, [], [f12, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24])).
fof(f24, plain, ((~ (e10 = op1(op1(e10, e10), e10)) & (e10 = op1(op1(e10, e10), e10)) & ~ (op1(e10, e10) = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e24])).
fof(e24, plain, (sP0 <=> (~ (e10 = op1(op1(e10, e10), e10)) & (e10 = op1(op1(e10, e10), e10)) & ~ (op1(e10, e10) = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f25, plain, ((~ (e11 = op1(op1(e10, e11), e10)) & (e10 = op1(op1(e10, e11), e11)) & ~ (op1(e10, e11) = op1(e11, e10))) | ~ sP1), inference(usedef, [], [e25])).
fof(e25, plain, (sP1 <=> (~ (e11 = op1(op1(e10, e11), e10)) & (e10 = op1(op1(e10, e11), e11)) & ~ (op1(e10, e11) = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f26, plain, ((~ (e12 = op1(op1(e10, e12), e10)) & (e10 = op1(op1(e10, e12), e12)) & ~ (op1(e10, e12) = op1(e12, e10))) | ~ sP2), inference(usedef, [], [e26])).
fof(e26, plain, (sP2 <=> (~ (e12 = op1(op1(e10, e12), e10)) & (e10 = op1(op1(e10, e12), e12)) & ~ (op1(e10, e12) = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f27, plain, ((~ (e13 = op1(op1(e10, e13), e10)) & (e10 = op1(op1(e10, e13), e13)) & ~ (op1(e10, e13) = op1(e13, e10))) | ~ sP3), inference(usedef, [], [e27])).
fof(e27, plain, (sP3 <=> (~ (e13 = op1(op1(e10, e13), e10)) & (e10 = op1(op1(e10, e13), e13)) & ~ (op1(e10, e13) = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f28, plain, ((~ (e14 = op1(op1(e10, e14), e10)) & (e10 = op1(op1(e10, e14), e14)) & ~ (op1(e10, e14) = op1(e14, e10))) | ~ sP4), inference(usedef, [], [e28])).
fof(e28, plain, (sP4 <=> (~ (e14 = op1(op1(e10, e14), e10)) & (e10 = op1(op1(e10, e14), e14)) & ~ (op1(e10, e14) = op1(e14, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f29, plain, ((~ (e10 = op1(op1(e11, e10), e11)) & (e11 = op1(op1(e11, e10), e10)) & ~ (op1(e10, e11) = op1(e11, e10))) | ~ sP5), inference(usedef, [], [e29])).
fof(e29, plain, (sP5 <=> (~ (e10 = op1(op1(e11, e10), e11)) & (e11 = op1(op1(e11, e10), e10)) & ~ (op1(e10, e11) = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f30, plain, ((~ (e11 = op1(op1(e11, e11), e11)) & (e11 = op1(op1(e11, e11), e11)) & ~ (op1(e11, e11) = op1(e11, e11))) | ~ sP6), inference(usedef, [], [e30])).
fof(e30, plain, (sP6 <=> (~ (e11 = op1(op1(e11, e11), e11)) & (e11 = op1(op1(e11, e11), e11)) & ~ (op1(e11, e11) = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f31, plain, ((~ (e12 = op1(op1(e11, e12), e11)) & (e11 = op1(op1(e11, e12), e12)) & ~ (op1(e11, e12) = op1(e12, e11))) | ~ sP7), inference(usedef, [], [e31])).
fof(e31, plain, (sP7 <=> (~ (e12 = op1(op1(e11, e12), e11)) & (e11 = op1(op1(e11, e12), e12)) & ~ (op1(e11, e12) = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f32, plain, ((~ (e13 = op1(op1(e11, e13), e11)) & (e11 = op1(op1(e11, e13), e13)) & ~ (op1(e11, e13) = op1(e13, e11))) | ~ sP8), inference(usedef, [], [e32])).
fof(e32, plain, (sP8 <=> (~ (e13 = op1(op1(e11, e13), e11)) & (e11 = op1(op1(e11, e13), e13)) & ~ (op1(e11, e13) = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f33, plain, ((~ (e14 = op1(op1(e11, e14), e11)) & (e11 = op1(op1(e11, e14), e14)) & ~ (op1(e11, e14) = op1(e14, e11))) | ~ sP9), inference(usedef, [], [e33])).
fof(e33, plain, (sP9 <=> (~ (e14 = op1(op1(e11, e14), e11)) & (e11 = op1(op1(e11, e14), e14)) & ~ (op1(e11, e14) = op1(e14, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f34, plain, ((~ (e10 = op1(op1(e12, e10), e12)) & (e12 = op1(op1(e12, e10), e10)) & ~ (op1(e10, e12) = op1(e12, e10))) | ~ sP10), inference(usedef, [], [e34])).
fof(e34, plain, (sP10 <=> (~ (e10 = op1(op1(e12, e10), e12)) & (e12 = op1(op1(e12, e10), e10)) & ~ (op1(e10, e12) = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f35, plain, ((~ (e11 = op1(op1(e12, e11), e12)) & (e12 = op1(op1(e12, e11), e11)) & ~ (op1(e11, e12) = op1(e12, e11))) | ~ sP11), inference(usedef, [], [e35])).
fof(e35, plain, (sP11 <=> (~ (e11 = op1(op1(e12, e11), e12)) & (e12 = op1(op1(e12, e11), e11)) & ~ (op1(e11, e12) = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f36, plain, ((~ (e12 = op1(op1(e12, e12), e12)) & (e12 = op1(op1(e12, e12), e12)) & ~ (op1(e12, e12) = op1(e12, e12))) | ~ sP12), inference(usedef, [], [e36])).
fof(e36, plain, (sP12 <=> (~ (e12 = op1(op1(e12, e12), e12)) & (e12 = op1(op1(e12, e12), e12)) & ~ (op1(e12, e12) = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f37, plain, ((~ (e13 = op1(op1(e12, e13), e12)) & (e12 = op1(op1(e12, e13), e13)) & ~ (op1(e12, e13) = op1(e13, e12))) | ~ sP13), inference(usedef, [], [e37])).
fof(e37, plain, (sP13 <=> (~ (e13 = op1(op1(e12, e13), e12)) & (e12 = op1(op1(e12, e13), e13)) & ~ (op1(e12, e13) = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f38, plain, ((~ (e14 = op1(op1(e12, e14), e12)) & (e12 = op1(op1(e12, e14), e14)) & ~ (op1(e12, e14) = op1(e14, e12))) | ~ sP14), inference(usedef, [], [e38])).
fof(e38, plain, (sP14 <=> (~ (e14 = op1(op1(e12, e14), e12)) & (e12 = op1(op1(e12, e14), e14)) & ~ (op1(e12, e14) = op1(e14, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f39, plain, ((~ (e10 = op1(op1(e13, e10), e13)) & (e13 = op1(op1(e13, e10), e10)) & ~ (op1(e10, e13) = op1(e13, e10))) | ~ sP15), inference(usedef, [], [e39])).
fof(e39, plain, (sP15 <=> (~ (e10 = op1(op1(e13, e10), e13)) & (e13 = op1(op1(e13, e10), e10)) & ~ (op1(e10, e13) = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f40, plain, ((~ (e11 = op1(op1(e13, e11), e13)) & (e13 = op1(op1(e13, e11), e11)) & ~ (op1(e11, e13) = op1(e13, e11))) | ~ sP16), inference(usedef, [], [e40])).
fof(e40, plain, (sP16 <=> (~ (e11 = op1(op1(e13, e11), e13)) & (e13 = op1(op1(e13, e11), e11)) & ~ (op1(e11, e13) = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f41, plain, ((~ (e12 = op1(op1(e13, e12), e13)) & (e13 = op1(op1(e13, e12), e12)) & ~ (op1(e12, e13) = op1(e13, e12))) | ~ sP17), inference(usedef, [], [e41])).
fof(e41, plain, (sP17 <=> (~ (e12 = op1(op1(e13, e12), e13)) & (e13 = op1(op1(e13, e12), e12)) & ~ (op1(e12, e13) = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f42, plain, ((~ (e13 = op1(op1(e13, e13), e13)) & (e13 = op1(op1(e13, e13), e13)) & ~ (op1(e13, e13) = op1(e13, e13))) | ~ sP18), inference(usedef, [], [e42])).
fof(e42, plain, (sP18 <=> (~ (e13 = op1(op1(e13, e13), e13)) & (e13 = op1(op1(e13, e13), e13)) & ~ (op1(e13, e13) = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f43, plain, ((~ (e14 = op1(op1(e13, e14), e13)) & (e13 = op1(op1(e13, e14), e14)) & ~ (op1(e13, e14) = op1(e14, e13))) | ~ sP19), inference(usedef, [], [e43])).
fof(e43, plain, (sP19 <=> (~ (e14 = op1(op1(e13, e14), e13)) & (e13 = op1(op1(e13, e14), e14)) & ~ (op1(e13, e14) = op1(e14, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f44, plain, ((~ (e10 = op1(op1(e14, e10), e14)) & (e14 = op1(op1(e14, e10), e10)) & ~ (op1(e10, e14) = op1(e14, e10))) | ~ sP20), inference(usedef, [], [e44])).
fof(e44, plain, (sP20 <=> (~ (e10 = op1(op1(e14, e10), e14)) & (e14 = op1(op1(e14, e10), e10)) & ~ (op1(e10, e14) = op1(e14, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f45, plain, ((~ (e11 = op1(op1(e14, e11), e14)) & (e14 = op1(op1(e14, e11), e11)) & ~ (op1(e11, e14) = op1(e14, e11))) | ~ sP21), inference(usedef, [], [e45])).
fof(e45, plain, (sP21 <=> (~ (e11 = op1(op1(e14, e11), e14)) & (e14 = op1(op1(e14, e11), e11)) & ~ (op1(e11, e14) = op1(e14, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f46, plain, ((~ (e12 = op1(op1(e14, e12), e14)) & (e14 = op1(op1(e14, e12), e12)) & ~ (op1(e12, e14) = op1(e14, e12))) | ~ sP22), inference(usedef, [], [e46])).
fof(e46, plain, (sP22 <=> (~ (e12 = op1(op1(e14, e12), e14)) & (e14 = op1(op1(e14, e12), e12)) & ~ (op1(e12, e14) = op1(e14, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f47, plain, ((~ (e13 = op1(op1(e14, e13), e14)) & (e14 = op1(op1(e14, e13), e13)) & ~ (op1(e13, e14) = op1(e14, e13))) | ~ sP23), inference(usedef, [], [e47])).
fof(e47, plain, (sP23 <=> (~ (e13 = op1(op1(e14, e13), e14)) & (e14 = op1(op1(e14, e13), e13)) & ~ (op1(e13, e14) = op1(e14, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f12, plain, ((~ (e14 = op1(op1(e14, e14), e14)) & (e14 = op1(op1(e14, e14), e14)) & ~ (op1(e14, e14) = op1(e14, e14))) | (~ (e13 = op1(op1(e14, e13), e14)) & (e14 = op1(op1(e14, e13), e13)) & ~ (op1(e13, e14) = op1(e14, e13))) | (~ (e12 = op1(op1(e14, e12), e14)) & (e14 = op1(op1(e14, e12), e12)) & ~ (op1(e12, e14) = op1(e14, e12))) | (~ (e11 = op1(op1(e14, e11), e14)) & (e14 = op1(op1(e14, e11), e11)) & ~ (op1(e11, e14) = op1(e14, e11))) | (~ (e10 = op1(op1(e14, e10), e14)) & (e14 = op1(op1(e14, e10), e10)) & ~ (op1(e10, e14) = op1(e14, e10))) | (~ (e14 = op1(op1(e13, e14), e13)) & (e13 = op1(op1(e13, e14), e14)) & ~ (op1(e13, e14) = op1(e14, e13))) | (~ (e13 = op1(op1(e13, e13), e13)) & (e13 = op1(op1(e13, e13), e13)) & ~ (op1(e13, e13) = op1(e13, e13))) | (~ (e12 = op1(op1(e13, e12), e13)) & (e13 = op1(op1(e13, e12), e12)) & ~ (op1(e12, e13) = op1(e13, e12))) | (~ (e11 = op1(op1(e13, e11), e13)) & (e13 = op1(op1(e13, e11), e11)) & ~ (op1(e11, e13) = op1(e13, e11))) | (~ (e10 = op1(op1(e13, e10), e13)) & (e13 = op1(op1(e13, e10), e10)) & ~ (op1(e10, e13) = op1(e13, e10))) | (~ (e14 = op1(op1(e12, e14), e12)) & (e12 = op1(op1(e12, e14), e14)) & ~ (op1(e12, e14) = op1(e14, e12))) | (~ (e13 = op1(op1(e12, e13), e12)) & (e12 = op1(op1(e12, e13), e13)) & ~ (op1(e12, e13) = op1(e13, e12))) | (~ (e12 = op1(op1(e12, e12), e12)) & (e12 = op1(op1(e12, e12), e12)) & ~ (op1(e12, e12) = op1(e12, e12))) | (~ (e11 = op1(op1(e12, e11), e12)) & (e12 = op1(op1(e12, e11), e11)) & ~ (op1(e11, e12) = op1(e12, e11))) | (~ (e10 = op1(op1(e12, e10), e12)) & (e12 = op1(op1(e12, e10), e10)) & ~ (op1(e10, e12) = op1(e12, e10))) | (~ (e14 = op1(op1(e11, e14), e11)) & (e11 = op1(op1(e11, e14), e14)) & ~ (op1(e11, e14) = op1(e14, e11))) | (~ (e13 = op1(op1(e11, e13), e11)) & (e11 = op1(op1(e11, e13), e13)) & ~ (op1(e11, e13) = op1(e13, e11))) | (~ (e12 = op1(op1(e11, e12), e11)) & (e11 = op1(op1(e11, e12), e12)) & ~ (op1(e11, e12) = op1(e12, e11))) | (~ (e11 = op1(op1(e11, e11), e11)) & (e11 = op1(op1(e11, e11), e11)) & ~ (op1(e11, e11) = op1(e11, e11))) | (~ (e10 = op1(op1(e11, e10), e11)) & (e11 = op1(op1(e11, e10), e10)) & ~ (op1(e10, e11) = op1(e11, e10))) | (~ (e14 = op1(op1(e10, e14), e10)) & (e10 = op1(op1(e10, e14), e14)) & ~ (op1(e10, e14) = op1(e14, e10))) | (~ (e13 = op1(op1(e10, e13), e10)) & (e10 = op1(op1(e10, e13), e13)) & ~ (op1(e10, e13) = op1(e13, e10))) | (~ (e12 = op1(op1(e10, e12), e10)) & (e10 = op1(op1(e10, e12), e12)) & ~ (op1(e10, e12) = op1(e12, e10))) | (~ (e11 = op1(op1(e10, e11), e10)) & (e10 = op1(op1(e10, e11), e11)) & ~ (op1(e10, e11) = op1(e11, e10))) | (~ (e10 = op1(op1(e10, e10), e10)) & (e10 = op1(op1(e10, e10), e10)) & ~ (op1(e10, e10) = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax12)).
fof(f2473, plain, ~ spl68_337, inference(avatar_split_clause, [], [f894, f2464])).
fof(f894, plain, ~ sP0, inference(trivial_inequality_removal, [], [f649])).
fof(f649, plain, (~ (op1(e10, e10) = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ((~ (e10 = op1(op1(e10, e10), e10)) & (e10 = op1(op1(e10, e10), e10)) & ~ (op1(e10, e10) = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f24])).
fof(f2462, plain, (~ spl68_334 | ~ spl68_324), inference(avatar_split_clause, [], [f646, f2399, f2449])).
fof(f646, plain, (~ (op1(e10, e11) = op1(e11, e10)) | ~ sP1), inference(cnf_transformation, [], [f117])).
fof(f117, plain, ((~ (e11 = op1(op1(e10, e11), e10)) & (e10 = op1(op1(e10, e11), e11)) & ~ (op1(e10, e11) = op1(e11, e10))) | ~ sP1), inference(nnf_transformation, [], [f25])).
fof(f2447, plain, (~ spl68_331 | ~ spl68_309), inference(avatar_split_clause, [], [f643, f2324, f2434])).
fof(f643, plain, (~ (op1(e10, e12) = op1(e12, e10)) | ~ sP2), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ((~ (e12 = op1(op1(e10, e12), e10)) & (e10 = op1(op1(e10, e12), e12)) & ~ (op1(e10, e12) = op1(e12, e10))) | ~ sP2), inference(nnf_transformation, [], [f26])).
fof(f2432, plain, (~ spl68_328 | ~ spl68_293), inference(avatar_split_clause, [], [f640, f2245, f2419])).
fof(f640, plain, (~ (op1(e10, e13) = op1(e13, e10)) | ~ sP3), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ((~ (e13 = op1(op1(e10, e13), e10)) & (e10 = op1(op1(e10, e13), e13)) & ~ (op1(e10, e13) = op1(e13, e10))) | ~ sP3), inference(nnf_transformation, [], [f27])).
fof(f2417, plain, (~ spl68_325 | ~ spl68_276), inference(avatar_split_clause, [], [f637, f2162, f2404])).
fof(f637, plain, (~ (op1(e10, e14) = op1(e14, e10)) | ~ sP4), inference(cnf_transformation, [], [f114])).
fof(f114, plain, ((~ (e14 = op1(op1(e10, e14), e10)) & (e10 = op1(op1(e10, e14), e14)) & ~ (op1(e10, e14) = op1(e14, e10))) | ~ sP4), inference(nnf_transformation, [], [f28])).
fof(f2402, plain, (~ spl68_321 | ~ spl68_324), inference(avatar_split_clause, [], [f634, f2399, f2385])).
fof(f634, plain, (~ (op1(e10, e11) = op1(e11, e10)) | ~ sP5), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ((~ (e10 = op1(op1(e11, e10), e11)) & (e11 = op1(op1(e11, e10), e10)) & ~ (op1(e10, e11) = op1(e11, e10))) | ~ sP5), inference(nnf_transformation, [], [f29])).
fof(f2383, plain, ~ spl68_319, inference(avatar_split_clause, [], [f895, f2374])).
fof(f895, plain, ~ sP6, inference(trivial_inequality_removal, [], [f631])).
fof(f631, plain, (~ (op1(e11, e11) = op1(e11, e11)) | ~ sP6), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ((~ (e11 = op1(op1(e11, e11), e11)) & (e11 = op1(op1(e11, e11), e11)) & ~ (op1(e11, e11) = op1(e11, e11))) | ~ sP6), inference(nnf_transformation, [], [f30])).
fof(f2371, plain, (~ spl68_316 | spl68_318), inference(avatar_split_clause, [], [f629, f2368, f2359])).
fof(f629, plain, ((e11 = op1(op1(e11, e12), e12)) | ~ sP7), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ((~ (e12 = op1(op1(e11, e12), e11)) & (e11 = op1(op1(e11, e12), e12)) & ~ (op1(e11, e12) = op1(e12, e11))) | ~ sP7), inference(nnf_transformation, [], [f31])).
fof(f2357, plain, (~ spl68_313 | ~ spl68_289), inference(avatar_split_clause, [], [f625, f2226, f2344])).
fof(f625, plain, (~ (op1(e11, e13) = op1(e13, e11)) | ~ sP8), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ((~ (e13 = op1(op1(e11, e13), e11)) & (e11 = op1(op1(e11, e13), e13)) & ~ (op1(e11, e13) = op1(e13, e11))) | ~ sP8), inference(nnf_transformation, [], [f32])).
fof(f2356, plain, (~ spl68_313 | spl68_315), inference(avatar_split_clause, [], [f626, f2353, f2344])).
fof(f626, plain, ((e11 = op1(op1(e11, e13), e13)) | ~ sP8), inference(cnf_transformation, [], [f110])).
fof(f2342, plain, (~ spl68_310 | ~ spl68_272), inference(avatar_split_clause, [], [f622, f2143, f2329])).
fof(f622, plain, (~ (op1(e11, e14) = op1(e14, e11)) | ~ sP9), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ((~ (e14 = op1(op1(e11, e14), e11)) & (e11 = op1(op1(e11, e14), e14)) & ~ (op1(e11, e14) = op1(e14, e11))) | ~ sP9), inference(nnf_transformation, [], [f33])).
fof(f2336, plain, (~ spl68_310 | ~ spl68_311), inference(avatar_split_clause, [], [f624, f2333, f2329])).
fof(f624, plain, (~ (e14 = op1(op1(e11, e14), e11)) | ~ sP9), inference(cnf_transformation, [], [f109])).
fof(f2327, plain, (~ spl68_306 | ~ spl68_309), inference(avatar_split_clause, [], [f619, f2324, f2310])).
fof(f619, plain, (~ (op1(e10, e12) = op1(e12, e10)) | ~ sP10), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ((~ (e10 = op1(op1(e12, e10), e12)) & (e12 = op1(op1(e12, e10), e10)) & ~ (op1(e10, e12) = op1(e12, e10))) | ~ sP10), inference(nnf_transformation, [], [f34])).
fof(f2308, plain, (~ spl68_302 | ~ spl68_305), inference(avatar_split_clause, [], [f616, f2305, f2291])).
fof(f616, plain, (~ (op1(e11, e12) = op1(e12, e11)) | ~ sP11), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ((~ (e11 = op1(op1(e12, e11), e12)) & (e12 = op1(op1(e12, e11), e11)) & ~ (op1(e11, e12) = op1(e12, e11))) | ~ sP11), inference(nnf_transformation, [], [f35])).
fof(f2303, plain, (~ spl68_302 | spl68_304), inference(avatar_split_clause, [], [f617, f2300, f2291])).
fof(f617, plain, ((e12 = op1(op1(e12, e11), e11)) | ~ sP11), inference(cnf_transformation, [], [f107])).
fof(f2289, plain, ~ spl68_300, inference(avatar_split_clause, [], [f896, f2280])).
fof(f896, plain, ~ sP12, inference(trivial_inequality_removal, [], [f613])).
fof(f613, plain, (~ (op1(e12, e12) = op1(e12, e12)) | ~ sP12), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ((~ (e12 = op1(op1(e12, e12), e12)) & (e12 = op1(op1(e12, e12), e12)) & ~ (op1(e12, e12) = op1(e12, e12))) | ~ sP12), inference(nnf_transformation, [], [f36])).
fof(f2277, plain, (~ spl68_297 | spl68_299), inference(avatar_split_clause, [], [f611, f2274, f2265])).
fof(f611, plain, ((e12 = op1(op1(e12, e13), e13)) | ~ sP13), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ((~ (e13 = op1(op1(e12, e13), e12)) & (e12 = op1(op1(e12, e13), e13)) & ~ (op1(e12, e13) = op1(e13, e12))) | ~ sP13), inference(nnf_transformation, [], [f37])).
fof(f2272, plain, (~ spl68_297 | ~ spl68_298), inference(avatar_split_clause, [], [f612, f2269, f2265])).
fof(f612, plain, (~ (e13 = op1(op1(e12, e13), e12)) | ~ sP13), inference(cnf_transformation, [], [f105])).
fof(f2263, plain, (~ spl68_294 | ~ spl68_268), inference(avatar_split_clause, [], [f607, f2124, f2250])).
fof(f607, plain, (~ (op1(e12, e14) = op1(e14, e12)) | ~ sP14), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ((~ (e14 = op1(op1(e12, e14), e12)) & (e12 = op1(op1(e12, e14), e14)) & ~ (op1(e12, e14) = op1(e14, e12))) | ~ sP14), inference(nnf_transformation, [], [f38])).
fof(f2262, plain, (~ spl68_294 | spl68_296), inference(avatar_split_clause, [], [f608, f2259, f2250])).
fof(f608, plain, ((e12 = op1(op1(e12, e14), e14)) | ~ sP14), inference(cnf_transformation, [], [f104])).
fof(f2248, plain, (~ spl68_290 | ~ spl68_293), inference(avatar_split_clause, [], [f604, f2245, f2231])).
fof(f604, plain, (~ (op1(e10, e13) = op1(e13, e10)) | ~ sP15), inference(cnf_transformation, [], [f103])).
fof(f103, plain, ((~ (e10 = op1(op1(e13, e10), e13)) & (e13 = op1(op1(e13, e10), e10)) & ~ (op1(e10, e13) = op1(e13, e10))) | ~ sP15), inference(nnf_transformation, [], [f39])).
fof(f2229, plain, (~ spl68_286 | ~ spl68_289), inference(avatar_split_clause, [], [f601, f2226, f2212])).
fof(f601, plain, (~ (op1(e11, e13) = op1(e13, e11)) | ~ sP16), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ((~ (e11 = op1(op1(e13, e11), e13)) & (e13 = op1(op1(e13, e11), e11)) & ~ (op1(e11, e13) = op1(e13, e11))) | ~ sP16), inference(nnf_transformation, [], [f40])).
fof(f2224, plain, (~ spl68_286 | spl68_288), inference(avatar_split_clause, [], [f602, f2221, f2212])).
fof(f602, plain, ((e13 = op1(op1(e13, e11), e11)) | ~ sP16), inference(cnf_transformation, [], [f102])).
fof(f2205, plain, (~ spl68_282 | spl68_284), inference(avatar_split_clause, [], [f599, f2202, f2193])).
fof(f599, plain, ((e13 = op1(op1(e13, e12), e12)) | ~ sP17), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ((~ (e12 = op1(op1(e13, e12), e13)) & (e13 = op1(op1(e13, e12), e12)) & ~ (op1(e12, e13) = op1(e13, e12))) | ~ sP17), inference(nnf_transformation, [], [f41])).
fof(f2191, plain, ~ spl68_280, inference(avatar_split_clause, [], [f897, f2182])).
fof(f897, plain, ~ sP18, inference(trivial_inequality_removal, [], [f595])).
fof(f595, plain, (~ (op1(e13, e13) = op1(e13, e13)) | ~ sP18), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ((~ (e13 = op1(op1(e13, e13), e13)) & (e13 = op1(op1(e13, e13), e13)) & ~ (op1(e13, e13) = op1(e13, e13))) | ~ sP18), inference(nnf_transformation, [], [f42])).
fof(f2180, plain, (~ spl68_277 | ~ spl68_264), inference(avatar_split_clause, [], [f592, f2105, f2167])).
fof(f592, plain, (~ (op1(e13, e14) = op1(e14, e13)) | ~ sP19), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ((~ (e14 = op1(op1(e13, e14), e13)) & (e13 = op1(op1(e13, e14), e14)) & ~ (op1(e13, e14) = op1(e14, e13))) | ~ sP19), inference(nnf_transformation, [], [f43])).
fof(f2165, plain, (~ spl68_273 | ~ spl68_276), inference(avatar_split_clause, [], [f589, f2162, f2148])).
fof(f589, plain, (~ (op1(e10, e14) = op1(e14, e10)) | ~ sP20), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((~ (e10 = op1(op1(e14, e10), e14)) & (e14 = op1(op1(e14, e10), e10)) & ~ (op1(e10, e14) = op1(e14, e10))) | ~ sP20), inference(nnf_transformation, [], [f44])).
fof(f2146, plain, (~ spl68_269 | ~ spl68_272), inference(avatar_split_clause, [], [f586, f2143, f2129])).
fof(f586, plain, (~ (op1(e11, e14) = op1(e14, e11)) | ~ sP21), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ (e11 = op1(op1(e14, e11), e14)) & (e14 = op1(op1(e14, e11), e11)) & ~ (op1(e11, e14) = op1(e14, e11))) | ~ sP21), inference(nnf_transformation, [], [f45])).
fof(f2141, plain, (~ spl68_269 | spl68_271), inference(avatar_split_clause, [], [f587, f2138, f2129])).
fof(f587, plain, ((e14 = op1(op1(e14, e11), e11)) | ~ sP21), inference(cnf_transformation, [], [f97])).
fof(f2122, plain, (~ spl68_265 | spl68_267), inference(avatar_split_clause, [], [f584, f2119, f2110])).
fof(f584, plain, ((e14 = op1(op1(e14, e12), e12)) | ~ sP22), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ((~ (e12 = op1(op1(e14, e12), e14)) & (e14 = op1(op1(e14, e12), e12)) & ~ (op1(e12, e14) = op1(e14, e12))) | ~ sP22), inference(nnf_transformation, [], [f46])).
fof(f2108, plain, (~ spl68_261 | ~ spl68_264), inference(avatar_split_clause, [], [f580, f2105, f2091])).
fof(f580, plain, (~ (op1(e13, e14) = op1(e14, e13)) | ~ sP23), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ (e13 = op1(op1(e14, e13), e14)) & (e14 = op1(op1(e14, e13), e13)) & ~ (op1(e13, e14) = op1(e14, e13))) | ~ sP23), inference(nnf_transformation, [], [f47])).
fof(f2074, plain, (spl68_248 | spl68_223 | spl68_198 | spl68_173 | spl68_148), inference(avatar_split_clause, [], [f300, f1566, f1671, f1776, f1881, f1986])).
fof(f300, plain, ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (((e24 = op2(e24, e24)) | (e24 = op2(e23, e24)) | (e24 = op2(e22, e24)) | (e24 = op2(e21, e24)) | (e24 = op2(e20, e24))) & ((e24 = op2(e24, e24)) | (e24 = op2(e24, e23)) | (e24 = op2(e24, e22)) | (e24 = op2(e24, e21)) | (e24 = op2(e24, e20))) & ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))) & ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))) & ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))) & ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))) & ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))) & ((e21 = op2(e24, e24)) | (e21 = op2(e24, e23)) | (e21 = op2(e24, e22)) | (e21 = op2(e24, e21)) | (e21 = op2(e24, e20))) & ((e20 = op2(e24, e24)) | (e20 = op2(e23, e24)) | (e20 = op2(e22, e24)) | (e20 = op2(e21, e24)) | (e20 = op2(e20, e24))) & ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))) & ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))) & ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))) & ((e23 = op2(e24, e23)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e24, e23)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e24 = op2(e24, e22)) | (e24 = op2(e23, e22)) | (e24 = op2(e22, e22)) | (e24 = op2(e21, e22)) | (e24 = op2(e20, e22))) & ((e24 = op2(e22, e24)) | (e24 = op2(e22, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e22, e21)) | (e24 = op2(e22, e20))) & ((e23 = op2(e24, e22)) | (e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e24)) | (e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e24, e22)) | (e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e24)) | (e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e24)) | (e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e24, e22)) | (e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))) & ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))) & ((e23 = op2(e24, e21)) | (e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e24)) | (e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e24)) | (e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e24, e21)) | (e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e24, e21)) | (e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e24)) | (e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e24 = op2(e24, e20)) | (e24 = op2(e23, e20)) | (e24 = op2(e22, e20)) | (e24 = op2(e21, e20)) | (op2(e20, e20) = e24)) & ((e24 = op2(e20, e24)) | (e24 = op2(e20, e23)) | (e24 = op2(e20, e22)) | (e24 = op2(e20, e21)) | (op2(e20, e20) = e24)) & ((e23 = op2(e24, e20)) | (e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e24)) | (e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e24, e20)) | (e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e24)) | (e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e24, e20)) | (e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e24)) | (e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax6)).
fof(f2071, plain, (spl68_230 | spl68_225 | spl68_220 | spl68_215 | spl68_210), inference(avatar_split_clause, [], [f303, f1826, f1847, f1868, f1889, f1910])).
fof(f303, plain, ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))), inference(cnf_transformation, [], [f6])).
fof(f2069, plain, (spl68_201 | spl68_196 | spl68_191 | spl68_186 | spl68_181), inference(avatar_split_clause, [], [f305, f1705, f1726, f1747, f1768, f1789])).
fof(f305, plain, ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f6])).
fof(f2066, plain, (spl68_242 | spl68_217 | spl68_192 | spl68_167 | spl68_142), inference(avatar_split_clause, [], [f308, f1541, f1646, f1751, f1856, f1961])).
fof(f308, plain, ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f6])).
fof(f2061, plain, (spl68_205 | spl68_200 | spl68_195 | spl68_190 | spl68_185), inference(avatar_split_clause, [], [f313, f1721, f1742, f1763, f1784, f1805])).
fof(f313, plain, ((e24 = op2(e22, e24)) | (e24 = op2(e22, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e22, e21)) | (e24 = op2(e22, e20))), inference(cnf_transformation, [], [f6])).
fof(f2058, plain, (spl68_236 | spl68_211 | spl68_186 | spl68_161 | spl68_136), inference(avatar_split_clause, [], [f316, f1516, f1621, f1726, f1831, f1936])).
fof(f316, plain, ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f2049, plain, (spl68_151 | spl68_146 | spl68_141 | spl68_136 | spl68_131), inference(avatar_split_clause, [], [f325, f1495, f1516, f1537, f1558, f1579])).
fof(f325, plain, ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))), inference(cnf_transformation, [], [f6])).
fof(f2046, plain, (spl68_232 | spl68_207 | spl68_182 | spl68_157 | spl68_132), inference(avatar_split_clause, [], [f328, f1499, f1604, f1709, f1814, f1919])).
fof(f328, plain, ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))), inference(cnf_transformation, [], [f6])).
fof(f2042, plain, (spl68_234 | spl68_209 | spl68_184 | spl68_159 | spl68_134), inference(avatar_split_clause, [], [f332, f1507, f1612, f1717, f1822, f1927])).
fof(f332, plain, ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))), inference(cnf_transformation, [], [f6])).
fof(f2039, plain, (spl68_256 | spl68_257 | spl68_258 | spl68_259 | spl68_260), inference(avatar_split_clause, [], [f284, f2036, f2032, f2028, f2024, f2020])).
fof(f284, plain, ((e24 = unit2) | (e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)), inference(cnf_transformation, [], [f5])).
fof(f1871, plain, (spl68_216 | spl68_217 | spl68_218 | spl68_219 | spl68_220), inference(avatar_split_clause, [], [f256, f1868, f1864, f1860, f1856, f1852])).
fof(f256, plain, ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e24 = op2(e24, e24)) | (e23 = op2(e24, e24)) | (e22 = op2(e24, e24)) | (e21 = op2(e24, e24)) | (e20 = op2(e24, e24))) & ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))) & ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))) & ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))) & ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))) & ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))) & ((e24 = op2(e23, e23)) | (e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e24 = op2(e23, e22)) | (e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))) & ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))) & ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e24 = op2(e21, e21)) | (e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e24 = op2(e21, e20)) | (e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e24 = op2(e20, e24)) | (e23 = op2(e20, e24)) | (e22 = op2(e20, e24)) | (e21 = op2(e20, e24)) | (e20 = op2(e20, e24))) & ((e24 = op2(e20, e23)) | (e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e24 = op2(e20, e22)) | (e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e24 = op2(e20, e21)) | (e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e24) | (op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax4)).
fof(f1850, plain, (spl68_211 | spl68_212 | spl68_213 | spl68_214 | spl68_215), inference(avatar_split_clause, [], [f257, f1847, f1843, f1839, f1835, f1831])).
fof(f257, plain, ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f4])).
fof(f1829, plain, (spl68_206 | spl68_207 | spl68_208 | spl68_209 | spl68_210), inference(avatar_split_clause, [], [f258, f1826, f1822, f1818, f1814, f1810])).
fof(f258, plain, ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))), inference(cnf_transformation, [], [f4])).
fof(f1787, plain, (spl68_196 | spl68_197 | spl68_198 | spl68_199 | spl68_200), inference(avatar_split_clause, [], [f260, f1784, f1780, f1776, f1772, f1768])).
fof(f260, plain, ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f4])).
fof(f1766, plain, (spl68_191 | spl68_192 | spl68_193 | spl68_194 | spl68_195), inference(avatar_split_clause, [], [f261, f1763, f1759, f1755, f1751, f1747])).
fof(f261, plain, ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f4])).
fof(f1745, plain, (spl68_186 | spl68_187 | spl68_188 | spl68_189 | spl68_190), inference(avatar_split_clause, [], [f262, f1742, f1738, f1734, f1730, f1726])).
fof(f262, plain, ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f4])).
fof(f1724, plain, (spl68_181 | spl68_182 | spl68_183 | spl68_184 | spl68_185), inference(avatar_split_clause, [], [f263, f1721, f1717, f1713, f1709, f1705])).
fof(f263, plain, ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))), inference(cnf_transformation, [], [f4])).
fof(f1619, plain, (spl68_156 | spl68_157 | spl68_158 | spl68_159 | spl68_160), inference(avatar_split_clause, [], [f268, f1616, f1612, f1608, f1604, f1600])).
fof(f268, plain, ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))), inference(cnf_transformation, [], [f4])).
fof(f1577, plain, (spl68_146 | spl68_147 | spl68_148 | spl68_149 | spl68_150), inference(avatar_split_clause, [], [f270, f1574, f1570, f1566, f1562, f1558])).
fof(f270, plain, ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))), inference(cnf_transformation, [], [f4])).
fof(f1556, plain, (spl68_141 | spl68_142 | spl68_143 | spl68_144 | spl68_145), inference(avatar_split_clause, [], [f271, f1553, f1549, f1545, f1541, f1537])).
fof(f271, plain, ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))), inference(cnf_transformation, [], [f4])).
fof(f1478, plain, (spl68_118 | spl68_93 | spl68_68 | spl68_43 | spl68_18), inference(avatar_split_clause, [], [f214, f970, f1075, f1180, f1285, f1390])).
fof(f214, plain, ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e14 = op1(e14, e14)) | (e14 = op1(e13, e14)) | (e14 = op1(e12, e14)) | (e14 = op1(e11, e14)) | (e14 = op1(e10, e14))) & ((e14 = op1(e14, e14)) | (e14 = op1(e14, e13)) | (e14 = op1(e14, e12)) | (e14 = op1(e14, e11)) | (e14 = op1(e14, e10))) & ((e13 = op1(e14, e14)) | (e13 = op1(e13, e14)) | (e13 = op1(e12, e14)) | (e13 = op1(e11, e14)) | (e13 = op1(e10, e14))) & ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))) & ((e12 = op1(e14, e14)) | (e12 = op1(e13, e14)) | (e12 = op1(e12, e14)) | (e12 = op1(e11, e14)) | (e12 = op1(e10, e14))) & ((e12 = op1(e14, e14)) | (e12 = op1(e14, e13)) | (e12 = op1(e14, e12)) | (e12 = op1(e14, e11)) | (e12 = op1(e14, e10))) & ((e11 = op1(e14, e14)) | (e11 = op1(e13, e14)) | (e11 = op1(e12, e14)) | (e11 = op1(e11, e14)) | (e11 = op1(e10, e14))) & ((e11 = op1(e14, e14)) | (e11 = op1(e14, e13)) | (e11 = op1(e14, e12)) | (e11 = op1(e14, e11)) | (e11 = op1(e14, e10))) & ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))) & ((e10 = op1(e14, e14)) | (e10 = op1(e14, e13)) | (e10 = op1(e14, e12)) | (e10 = op1(e14, e11)) | (e10 = op1(e14, e10))) & ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))) & ((e14 = op1(e13, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e13, e12)) | (e14 = op1(e13, e11)) | (e14 = op1(e13, e10))) & ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e14, e13)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e14 = op1(e14, e12)) | (e14 = op1(e13, e12)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e12)) | (e14 = op1(e10, e12))) & ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))) & ((e13 = op1(e14, e12)) | (e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e14)) | (e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e14, e12)) | (e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e14)) | (e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e14, e12)) | (e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e14 = op1(e14, e11)) | (e14 = op1(e13, e11)) | (e14 = op1(e12, e11)) | (e14 = op1(e11, e11)) | (e14 = op1(e10, e11))) & ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))) & ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e14)) | (e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e14)) | (e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e14, e11)) | (e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e14)) | (e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e14, e11)) | (e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e14)) | (e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e14 = op1(e14, e10)) | (e14 = op1(e13, e10)) | (e14 = op1(e12, e10)) | (e14 = op1(e11, e10)) | (op1(e10, e10) = e14)) & ((e14 = op1(e10, e14)) | (e14 = op1(e10, e13)) | (e14 = op1(e10, e12)) | (e14 = op1(e10, e11)) | (op1(e10, e10) = e14)) & ((e13 = op1(e14, e10)) | (e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e14)) | (e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e14, e10)) | (e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e14)) | (e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e14, e10)) | (e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e14)) | (e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e14, e10)) | (e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e14)) | (e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax3)).
fof(f1476, plain, (spl68_119 | spl68_94 | spl68_69 | spl68_44 | spl68_19), inference(avatar_split_clause, [], [f216, f974, f1079, f1184, f1289, f1394])).
fof(f216, plain, ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f3])).
fof(f1475, plain, (spl68_100 | spl68_95 | spl68_90 | spl68_85 | spl68_80), inference(avatar_split_clause, [], [f217, f1230, f1251, f1272, f1293, f1314])).
fof(f217, plain, ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))), inference(cnf_transformation, [], [f3])).
fof(f1471, plain, (spl68_72 | spl68_67 | spl68_62 | spl68_57 | spl68_52), inference(avatar_split_clause, [], [f221, f1113, f1134, f1155, f1176, f1197])).
fof(f221, plain, ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f1470, plain, (spl68_112 | spl68_87 | spl68_62 | spl68_37 | spl68_12), inference(avatar_split_clause, [], [f222, f945, f1050, f1155, f1260, f1365])).
fof(f222, plain, ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f1462, plain, (spl68_106 | spl68_81 | spl68_56 | spl68_31 | spl68_6), inference(avatar_split_clause, [], [f230, f920, f1025, f1130, f1235, f1340])).
fof(f230, plain, ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f3])).
fof(f1459, plain, (spl68_48 | spl68_43 | spl68_38 | spl68_33 | spl68_28), inference(avatar_split_clause, [], [f233, f1012, f1033, f1054, f1075, f1096])).
fof(f233, plain, ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f3])).
fof(f1452, plain, (spl68_101 | spl68_76 | spl68_51 | spl68_26 | spl68_1), inference(avatar_split_clause, [], [f240, f899, f1004, f1109, f1214, f1319])).
fof(f240, plain, ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))), inference(cnf_transformation, [], [f3])).
fof(f1447, plain, (spl68_24 | spl68_19 | spl68_14 | spl68_9 | spl68_4), inference(avatar_split_clause, [], [f245, f911, f932, f953, f974, f995])).
fof(f245, plain, ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))), inference(cnf_transformation, [], [f3])).
fof(f1443, plain, (spl68_126 | spl68_127 | spl68_128 | spl68_129 | spl68_130), inference(avatar_split_clause, [], [f198, f1440, f1436, f1432, f1428, f1424])).
fof(f198, plain, ((e14 = unit1) | (e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)), inference(cnf_transformation, [], [f2])).
fof(f1275, plain, (spl68_86 | spl68_87 | spl68_88 | spl68_89 | spl68_90), inference(avatar_split_clause, [], [f170, f1272, f1268, f1264, f1260, f1256])).
fof(f170, plain, ((e14 = op1(e11, e12)) | (e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e14 = op1(e14, e14)) | (e13 = op1(e14, e14)) | (e12 = op1(e14, e14)) | (e11 = op1(e14, e14)) | (e10 = op1(e14, e14))) & ((e14 = op1(e14, e13)) | (e13 = op1(e14, e13)) | (e12 = op1(e14, e13)) | (e11 = op1(e14, e13)) | (e10 = op1(e14, e13))) & ((e14 = op1(e14, e12)) | (e13 = op1(e14, e12)) | (e12 = op1(e14, e12)) | (e11 = op1(e14, e12)) | (e10 = op1(e14, e12))) & ((e14 = op1(e14, e11)) | (e13 = op1(e14, e11)) | (e12 = op1(e14, e11)) | (e11 = op1(e14, e11)) | (e10 = op1(e14, e11))) & ((e14 = op1(e14, e10)) | (e13 = op1(e14, e10)) | (e12 = op1(e14, e10)) | (e11 = op1(e14, e10)) | (e10 = op1(e14, e10))) & ((e14 = op1(e13, e14)) | (e13 = op1(e13, e14)) | (e12 = op1(e13, e14)) | (e11 = op1(e13, e14)) | (e10 = op1(e13, e14))) & ((e14 = op1(e13, e13)) | (e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e14 = op1(e13, e12)) | (e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e14 = op1(e13, e11)) | (e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))) & ((e14 = op1(e12, e13)) | (e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e14 = op1(e12, e12)) | (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e14 = op1(e12, e10)) | (e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))) & ((e14 = op1(e11, e13)) | (e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e14 = op1(e11, e12)) | (e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e14 = op1(e11, e11)) | (e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e14 = op1(e11, e10)) | (e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e14 = op1(e10, e14)) | (e13 = op1(e10, e14)) | (e12 = op1(e10, e14)) | (e11 = op1(e10, e14)) | (e10 = op1(e10, e14))) & ((e14 = op1(e10, e13)) | (e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e14 = op1(e10, e12)) | (e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e14 = op1(e10, e11)) | (e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e14) | (op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG053+1.p', ax1)).
fof(f1254, plain, (spl68_81 | spl68_82 | spl68_83 | spl68_84 | spl68_85), inference(avatar_split_clause, [], [f171, f1251, f1247, f1243, f1239, f1235])).
fof(f171, plain, ((e14 = op1(e11, e13)) | (e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f1233, plain, (spl68_76 | spl68_77 | spl68_78 | spl68_79 | spl68_80), inference(avatar_split_clause, [], [f172, f1230, f1226, f1222, f1218, f1214])).
fof(f172, plain, ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))), inference(cnf_transformation, [], [f1])).
fof(f1191, plain, (spl68_66 | spl68_67 | spl68_68 | spl68_69 | spl68_70), inference(avatar_split_clause, [], [f174, f1188, f1184, f1180, f1176, f1172])).
fof(f174, plain, ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f1170, plain, (spl68_61 | spl68_62 | spl68_63 | spl68_64 | spl68_65), inference(avatar_split_clause, [], [f175, f1167, f1163, f1159, f1155, f1151])).
fof(f175, plain, ((e14 = op1(e12, e12)) | (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f1])).
fof(f1149, plain, (spl68_56 | spl68_57 | spl68_58 | spl68_59 | spl68_60), inference(avatar_split_clause, [], [f176, f1146, f1142, f1138, f1134, f1130])).
fof(f176, plain, ((e14 = op1(e12, e13)) | (e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f1128, plain, (spl68_51 | spl68_52 | spl68_53 | spl68_54 | spl68_55), inference(avatar_split_clause, [], [f177, f1125, f1121, f1117, f1113, f1109])).
fof(f177, plain, ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))), inference(cnf_transformation, [], [f1])).
fof(f981, plain, (spl68_16 | spl68_17 | spl68_18 | spl68_19 | spl68_20), inference(avatar_split_clause, [], [f184, f978, f974, f970, f966, f962])).
fof(f184, plain, ((e14 = op1(e14, e11)) | (e13 = op1(e14, e11)) | (e12 = op1(e14, e11)) | (e11 = op1(e14, e11)) | (e10 = op1(e14, e11))), inference(cnf_transformation, [], [f1])).
fof(f960, plain, (spl68_11 | spl68_12 | spl68_13 | spl68_14 | spl68_15), inference(avatar_split_clause, [], [f185, f957, f953, f949, f945, f941])).
fof(f185, plain, ((e14 = op1(e14, e12)) | (e13 = op1(e14, e12)) | (e12 = op1(e14, e12)) | (e11 = op1(e14, e12)) | (e10 = op1(e14, e12))), inference(cnf_transformation, [], [f1])).