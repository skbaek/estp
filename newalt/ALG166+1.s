fof(f8064, plain, $false, inference(avatar_sat_refutation, [], [f840, f861, f924, f945, f966, f1008, f1029, f1050, f1071, f1155, f1176, f1197, f1271, f1272, f1273, f1275, f1276, f1282, f1283, f1289, f1298, f1309, f1310, f1373, f1415, f1436, f1499, f1520, f1583, f1604, f1730, f1751, f1793, f1839, f1840, f1844, f1846, f1853, f1858, f1860, f1873, f1882, f1883, f1884, f1885, f1890, f1901, f1909, f1920, f1926, f1931, f1942, f1950, f1961, f1967, f1968, f1971, f1972, f1975, f2366, f2386, f2413, f3329, f3387, f3435, f3469, f3471, f3475, f3489, f3516, f3532, f3544, f3552, f3590, f3603, f3665, f3721, f3724, f3734, f3735, f3736, f3737, f3738, f3740, f3741, f3742, f3770, f3786, f3848, f3884, f3887, f3897, f3944, f3955, f3969, f4073, f4076, f4098, f4137, f4151, f4165, f4179, f4184, f4203, f4208, f4257, f4320, f4338, f4340, f4343, f4353, f4354, f4355, f4356, f4369, f4378, f4399, f4404, f4413, f4462, f4506, f4534, f4552, f4595, f4625, f4628, f4634, f4637, f4643, f4679, f4696, f4712, f4731, f4752, f4816, f4818, f4827, f4852, f4871, f4880, f4911, f4915, f4942, f4996, f4997, f5009, f5012, f5047, f5141, f5174, f5179, f5185, f5191, f5222, f5311, f5355, f5394, f5400, f5425, f5440, f5451, f5515, f5537, f5550, f5575, f5576, f5581, f5601, f5631, f5639, f5642, f5651, f5652, f5672, f5687, f5755, f5783, f5855, f5868, f5872, f5887, f5913, f5938, f5951, f5962, f5991, f5995, f6021, f6052, f6055, f6071, f6104, f6110, f6112, f6113, f6115, f6123, f6131, f6148, f6155, f6159, f6164, f6173, f6236, f6239, f6276, f6311, f6322, f6324, f6348, f6351, f6420, f6423, f6438, f6496, f6501, f6518, f6534, f6569, f6574, f6601, f6602, f6647, f6719, f6749, f6751, f6774, f6775, f6782, f6796, f6810, f6817, f6832, f6834, f6857, f6893, f6910, f6939, f6940, f6962, f7022, f7061, f7076, f7085, f7086, f7087, f7100, f7119, f7128, f7130, f7132, f7141, f7147, f7224, f7263, f7296, f7342, f7376, f7414, f7449, f7492, f7537, f7575, f7608, f7653, f7692, f7735, f7780, f7817, f7859, f7895, f7927, f7961, f7995, f8034])).
fof(f8034, plain, (~ spl28_42 | ~ spl28_134 | ~ spl28_260 | spl28_452 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f8033])).
fof(f8033, plain, ($false | (~ spl28_42 | ~ spl28_134 | ~ spl28_260 | spl28_452 | ~ spl28_455)), inference(subsumption_resolution, [], [f8032, f1347])).
fof(f1347, plain, ((e23 = op2(e24, e23)) | ~ spl28_134), inference(avatar_component_clause, [], [f1345])).
fof(f1345, plain, (spl28_134 <=> (e23 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_134])])).
fof(f8032, plain, (~ (e23 = op2(e24, e23)) | (~ spl28_42 | ~ spl28_260 | spl28_452 | ~ spl28_455)), inference(forward_demodulation, [], [f8031, f2057])).
fof(f2057, plain, ((e23 = h1(e11)) | ~ spl28_260), inference(avatar_component_clause, [], [f2056])).
fof(f2056, plain, (spl28_260 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_260])])).
fof(f8031, plain, (~ (op2(e24, e23) = h1(e11)) | (~ spl28_42 | ~ spl28_260 | spl28_452 | ~ spl28_455)), inference(forward_demodulation, [], [f8030, f911])).
fof(f911, plain, ((e11 = op1(e13, e11)) | ~ spl28_42), inference(avatar_component_clause, [], [f909])).
fof(f909, plain, (spl28_42 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_42])])).
fof(f8030, plain, (~ (op2(e24, e23) = h1(op1(e13, e11))) | (~ spl28_260 | spl28_452 | ~ spl28_455)), inference(forward_demodulation, [], [f8029, f3279])).
fof(f3279, plain, ((e24 = h1(e13)) | ~ spl28_455), inference(avatar_component_clause, [], [f3278])).
fof(f3278, plain, (spl28_455 <=> (e24 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_455])])).
fof(f8029, plain, (~ (h1(op1(e13, e11)) = op2(h1(e13), e23)) | (~ spl28_260 | spl28_452)), inference(forward_demodulation, [], [f3268, f2057])).
fof(f3268, plain, (~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | spl28_452), inference(avatar_component_clause, [], [f3266])).
fof(f3266, plain, (spl28_452 <=> (h1(op1(e13, e11)) = op2(h1(e13), h1(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_452])])).
fof(f7995, plain, (~ spl28_31 | ~ spl28_127 | ~ spl28_327 | spl28_454 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7994])).
fof(f7994, plain, ($false | (~ spl28_31 | ~ spl28_127 | ~ spl28_327 | spl28_454 | ~ spl28_455)), inference(subsumption_resolution, [], [f7993, f1318])).
fof(f1318, plain, ((e21 = op2(e24, e24)) | ~ spl28_127), inference(avatar_component_clause, [], [f1316])).
fof(f1316, plain, (spl28_127 <=> (e21 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_127])])).
fof(f7993, plain, (~ (e21 = op2(e24, e24)) | (~ spl28_31 | ~ spl28_327 | spl28_454 | ~ spl28_455)), inference(forward_demodulation, [], [f7992, f2411])).
fof(f2411, plain, ((e21 = h1(e10)) | ~ spl28_327), inference(avatar_component_clause, [], [f2410])).
fof(f2410, plain, (spl28_327 <=> (e21 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_327])])).
fof(f7992, plain, (~ (op2(e24, e24) = h1(e10)) | (~ spl28_31 | spl28_454 | ~ spl28_455)), inference(forward_demodulation, [], [f7991, f865])).
fof(f865, plain, ((e10 = op1(e13, e13)) | ~ spl28_31), inference(avatar_component_clause, [], [f863])).
fof(f863, plain, (spl28_31 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_31])])).
fof(f7991, plain, (~ (op2(e24, e24) = h1(op1(e13, e13))) | (spl28_454 | ~ spl28_455)), inference(forward_demodulation, [], [f3276, f3279])).
fof(f3276, plain, (~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | spl28_454), inference(avatar_component_clause, [], [f3274])).
fof(f3274, plain, (spl28_454 <=> (h1(op1(e13, e13)) = op2(h1(e13), h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl28_454])])).
fof(f7961, plain, (~ spl28_2 | spl28_456), inference(avatar_contradiction_clause, [], [f7960])).
fof(f7960, plain, ($false | (~ spl28_2 | spl28_456)), inference(trivial_inequality_removal, [], [f7959])).
fof(f7959, plain, (~ (h1(e11) = h1(e11)) | (~ spl28_2 | spl28_456)), inference(forward_demodulation, [], [f3284, f743])).
fof(f743, plain, ((e11 = op1(e14, e14)) | ~ spl28_2), inference(avatar_component_clause, [], [f741])).
fof(f741, plain, (spl28_2 <=> (e11 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_2])])).
fof(f3284, plain, (~ (h1(e11) = h1(op1(e14, e14))) | spl28_456), inference(avatar_component_clause, [], [f3282])).
fof(f3282, plain, (spl28_456 <=> (h1(e11) = h1(op1(e14, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl28_456])])).
fof(f7927, plain, (~ spl28_50 | ~ spl28_141 | ~ spl28_327 | spl28_451 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7926])).
fof(f7926, plain, ($false | (~ spl28_50 | ~ spl28_141 | ~ spl28_327 | spl28_451 | ~ spl28_455)), inference(subsumption_resolution, [], [f7925, f1377])).
fof(f1377, plain, ((e20 = op2(e24, e21)) | ~ spl28_141), inference(avatar_component_clause, [], [f1375])).
fof(f1375, plain, (spl28_141 <=> (e20 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_141])])).
fof(f7925, plain, (~ (e20 = op2(e24, e21)) | (~ spl28_50 | ~ spl28_327 | spl28_451 | ~ spl28_455)), inference(forward_demodulation, [], [f7924, f586])).
fof(f586, plain, (e20 = h1(e14)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((h1(e13) = op2(op2(e20, op2(e20, e20)), op2(e20, op2(e20, e20)))) & (h1(e12) = op2(op2(e20, op2(e20, e20)), op2(e20, e20))) & (op2(e20, e20) = h1(e11)) & (h1(e10) = op2(e20, op2(e20, e20))) & (e20 = h1(e14))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax16)).
fof(f7924, plain, (~ (op2(e24, e21) = h1(e14)) | (~ spl28_50 | ~ spl28_327 | spl28_451 | ~ spl28_455)), inference(forward_demodulation, [], [f7923, f944])).
fof(f944, plain, ((e14 = op1(e13, e10)) | ~ spl28_50), inference(avatar_component_clause, [], [f942])).
fof(f942, plain, (spl28_50 <=> (e14 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_50])])).
fof(f7923, plain, (~ (op2(e24, e21) = h1(op1(e13, e10))) | (~ spl28_327 | spl28_451 | ~ spl28_455)), inference(forward_demodulation, [], [f7922, f3279])).
fof(f7922, plain, (~ (h1(op1(e13, e10)) = op2(h1(e13), e21)) | (~ spl28_327 | spl28_451)), inference(forward_demodulation, [], [f3264, f2411])).
fof(f3264, plain, (~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | spl28_451), inference(avatar_component_clause, [], [f3262])).
fof(f3262, plain, (spl28_451 <=> (h1(op1(e13, e10)) = op2(h1(e13), h1(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_451])])).
fof(f7895, plain, (~ spl28_63 | ~ spl28_188 | ~ spl28_322 | spl28_449), inference(avatar_contradiction_clause, [], [f7894])).
fof(f7894, plain, ($false | (~ spl28_63 | ~ spl28_188 | ~ spl28_322 | spl28_449)), inference(subsumption_resolution, [], [f7893, f1574])).
fof(f1574, plain, ((e22 = op2(e22, e22)) | ~ spl28_188), inference(avatar_component_clause, [], [f1572])).
fof(f1572, plain, (spl28_188 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_188])])).
fof(f7893, plain, (~ (e22 = op2(e22, e22)) | (~ spl28_63 | ~ spl28_322 | spl28_449)), inference(forward_demodulation, [], [f7892, f2384])).
fof(f2384, plain, ((e22 = h1(e12)) | ~ spl28_322), inference(avatar_component_clause, [], [f2383])).
fof(f2383, plain, (spl28_322 <=> (e22 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_322])])).
fof(f7892, plain, (~ (op2(e22, e22) = h1(e12)) | (~ spl28_63 | ~ spl28_322 | spl28_449)), inference(forward_demodulation, [], [f7891, f999])).
fof(f999, plain, ((e12 = op1(e12, e12)) | ~ spl28_63), inference(avatar_component_clause, [], [f997])).
fof(f997, plain, (spl28_63 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_63])])).
fof(f7891, plain, (~ (op2(e22, e22) = h1(op1(e12, e12))) | (~ spl28_322 | spl28_449)), inference(forward_demodulation, [], [f3256, f2384])).
fof(f3256, plain, (~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | spl28_449), inference(avatar_component_clause, [], [f3254])).
fof(f3254, plain, (spl28_449 <=> (h1(op1(e12, e12)) = op2(h1(e12), h1(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_449])])).
fof(f7859, plain, (~ spl28_9 | ~ spl28_230 | ~ spl28_455 | spl28_457), inference(avatar_contradiction_clause, [], [f7858])).
fof(f7858, plain, ($false | (~ spl28_9 | ~ spl28_230 | ~ spl28_455 | spl28_457)), inference(subsumption_resolution, [], [f7857, f1750])).
fof(f1750, plain, ((e24 = op2(e20, e24)) | ~ spl28_230), inference(avatar_component_clause, [], [f1748])).
fof(f1748, plain, (spl28_230 <=> (e24 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_230])])).
fof(f7857, plain, (~ (e24 = op2(e20, e24)) | (~ spl28_9 | ~ spl28_455 | spl28_457)), inference(forward_demodulation, [], [f7856, f3279])).
fof(f7856, plain, (~ (op2(e20, e24) = h1(e13)) | (~ spl28_9 | ~ spl28_455 | spl28_457)), inference(forward_demodulation, [], [f7855, f772])).
fof(f772, plain, ((e13 = op1(e14, e13)) | ~ spl28_9), inference(avatar_component_clause, [], [f770])).
fof(f770, plain, (spl28_9 <=> (e13 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_9])])).
fof(f7855, plain, (~ (op2(e20, e24) = h1(op1(e14, e13))) | (~ spl28_455 | spl28_457)), inference(forward_demodulation, [], [f3288, f3279])).
fof(f3288, plain, (~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | spl28_457), inference(avatar_component_clause, [], [f3286])).
fof(f3286, plain, (spl28_457 <=> (h1(op1(e14, e13)) = op2(e20, h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl28_457])])).
fof(f7817, plain, (~ spl28_15 | ~ spl28_236 | ~ spl28_322 | spl28_458), inference(avatar_contradiction_clause, [], [f7816])).
fof(f7816, plain, ($false | (~ spl28_15 | ~ spl28_236 | ~ spl28_322 | spl28_458)), inference(subsumption_resolution, [], [f7815, f1776])).
fof(f1776, plain, ((e20 = op2(e20, e22)) | ~ spl28_236), inference(avatar_component_clause, [], [f1774])).
fof(f1774, plain, (spl28_236 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_236])])).
fof(f7815, plain, (~ (e20 = op2(e20, e22)) | (~ spl28_15 | ~ spl28_322 | spl28_458)), inference(forward_demodulation, [], [f7814, f586])).
fof(f7814, plain, (~ (op2(e20, e22) = h1(e14)) | (~ spl28_15 | ~ spl28_322 | spl28_458)), inference(forward_demodulation, [], [f7813, f797])).
fof(f797, plain, ((e14 = op1(e14, e12)) | ~ spl28_15), inference(avatar_component_clause, [], [f795])).
fof(f795, plain, (spl28_15 <=> (e14 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_15])])).
fof(f7813, plain, (~ (op2(e20, e22) = h1(op1(e14, e12))) | (~ spl28_322 | spl28_458)), inference(forward_demodulation, [], [f3292, f2384])).
fof(f3292, plain, (~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | spl28_458), inference(avatar_component_clause, [], [f3290])).
fof(f3290, plain, (spl28_458 <=> (h1(op1(e14, e12)) = op2(e20, h1(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_458])])).
fof(f7780, plain, (~ spl28_72 | ~ spl28_194 | ~ spl28_260 | ~ spl28_322 | ~ spl28_327 | spl28_447), inference(avatar_contradiction_clause, [], [f7779])).
fof(f7779, plain, ($false | (~ spl28_72 | ~ spl28_194 | ~ spl28_260 | ~ spl28_322 | ~ spl28_327 | spl28_447)), inference(subsumption_resolution, [], [f7778, f1599])).
fof(f1599, plain, ((e23 = op2(e22, e21)) | ~ spl28_194), inference(avatar_component_clause, [], [f1597])).
fof(f1597, plain, (spl28_194 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_194])])).
fof(f7778, plain, (~ (e23 = op2(e22, e21)) | (~ spl28_72 | ~ spl28_260 | ~ spl28_322 | ~ spl28_327 | spl28_447)), inference(forward_demodulation, [], [f7777, f2057])).
fof(f7777, plain, (~ (op2(e22, e21) = h1(e11)) | (~ spl28_72 | ~ spl28_322 | ~ spl28_327 | spl28_447)), inference(forward_demodulation, [], [f7776, f1037])).
fof(f1037, plain, ((e11 = op1(e12, e10)) | ~ spl28_72), inference(avatar_component_clause, [], [f1035])).
fof(f1035, plain, (spl28_72 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_72])])).
fof(f7776, plain, (~ (op2(e22, e21) = h1(op1(e12, e10))) | (~ spl28_322 | ~ spl28_327 | spl28_447)), inference(forward_demodulation, [], [f7775, f2384])).
fof(f7775, plain, (~ (h1(op1(e12, e10)) = op2(h1(e12), e21)) | (~ spl28_327 | spl28_447)), inference(forward_demodulation, [], [f3248, f2411])).
fof(f3248, plain, (~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | spl28_447), inference(avatar_component_clause, [], [f3246])).
fof(f3246, plain, (spl28_447 <=> (h1(op1(e12, e10)) = op2(h1(e12), h1(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_447])])).
fof(f7735, plain, (~ spl28_28 | ~ spl28_148 | ~ spl28_322 | ~ spl28_455 | spl28_461), inference(avatar_contradiction_clause, [], [f7734])).
fof(f7734, plain, ($false | (~ spl28_28 | ~ spl28_148 | ~ spl28_322 | ~ spl28_455 | spl28_461)), inference(subsumption_resolution, [], [f7733, f1406])).
fof(f1406, plain, ((e22 = op2(e24, e20)) | ~ spl28_148), inference(avatar_component_clause, [], [f1404])).
fof(f1404, plain, (spl28_148 <=> (e22 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_148])])).
fof(f7733, plain, (~ (e22 = op2(e24, e20)) | (~ spl28_28 | ~ spl28_322 | ~ spl28_455 | spl28_461)), inference(forward_demodulation, [], [f7732, f2384])).
fof(f7732, plain, (~ (op2(e24, e20) = h1(e12)) | (~ spl28_28 | ~ spl28_455 | spl28_461)), inference(forward_demodulation, [], [f7731, f852])).
fof(f852, plain, ((e12 = op1(e13, e14)) | ~ spl28_28), inference(avatar_component_clause, [], [f850])).
fof(f850, plain, (spl28_28 <=> (e12 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_28])])).
fof(f7731, plain, (~ (op2(e24, e20) = h1(op1(e13, e14))) | (~ spl28_455 | spl28_461)), inference(forward_demodulation, [], [f3304, f3279])).
fof(f3304, plain, (~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | spl28_461), inference(avatar_component_clause, [], [f3302])).
fof(f3302, plain, (spl28_461 <=> (h1(op1(e13, e14)) = op2(h1(e13), e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_461])])).
fof(f7692, plain, (~ spl28_39 | ~ spl28_140 | ~ spl28_322 | spl28_453 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7691])).
fof(f7691, plain, ($false | (~ spl28_39 | ~ spl28_140 | ~ spl28_322 | spl28_453 | ~ spl28_455)), inference(subsumption_resolution, [], [f7690, f1372])).
fof(f1372, plain, ((e24 = op2(e24, e22)) | ~ spl28_140), inference(avatar_component_clause, [], [f1370])).
fof(f1370, plain, (spl28_140 <=> (e24 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_140])])).
fof(f7690, plain, (~ (e24 = op2(e24, e22)) | (~ spl28_39 | ~ spl28_322 | spl28_453 | ~ spl28_455)), inference(forward_demodulation, [], [f7689, f3279])).
fof(f7689, plain, (~ (op2(e24, e22) = h1(e13)) | (~ spl28_39 | ~ spl28_322 | spl28_453 | ~ spl28_455)), inference(forward_demodulation, [], [f7688, f898])).
fof(f898, plain, ((e13 = op1(e13, e12)) | ~ spl28_39), inference(avatar_component_clause, [], [f896])).
fof(f896, plain, (spl28_39 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_39])])).
fof(f7688, plain, (~ (op2(e24, e22) = h1(op1(e13, e12))) | (~ spl28_322 | spl28_453 | ~ spl28_455)), inference(forward_demodulation, [], [f7687, f3279])).
fof(f7687, plain, (~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | (~ spl28_322 | spl28_453)), inference(forward_demodulation, [], [f3272, f2384])).
fof(f3272, plain, (~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | spl28_453), inference(avatar_component_clause, [], [f3270])).
fof(f3270, plain, (spl28_453 <=> (h1(op1(e13, e12)) = op2(h1(e13), h1(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_453])])).
fof(f7653, plain, (~ spl28_60 | ~ spl28_176 | ~ spl28_322 | spl28_450 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7652])).
fof(f7652, plain, ($false | (~ spl28_60 | ~ spl28_176 | ~ spl28_322 | spl28_450 | ~ spl28_455)), inference(subsumption_resolution, [], [f7651, f1524])).
fof(f1524, plain, ((e20 = op2(e22, e24)) | ~ spl28_176), inference(avatar_component_clause, [], [f1522])).
fof(f1522, plain, (spl28_176 <=> (e20 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_176])])).
fof(f7651, plain, (~ (e20 = op2(e22, e24)) | (~ spl28_60 | ~ spl28_322 | spl28_450 | ~ spl28_455)), inference(forward_demodulation, [], [f7650, f586])).
fof(f7650, plain, (~ (op2(e22, e24) = h1(e14)) | (~ spl28_60 | ~ spl28_322 | spl28_450 | ~ spl28_455)), inference(forward_demodulation, [], [f7649, f986])).
fof(f986, plain, ((e14 = op1(e12, e13)) | ~ spl28_60), inference(avatar_component_clause, [], [f984])).
fof(f984, plain, (spl28_60 <=> (e14 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_60])])).
fof(f7649, plain, (~ (op2(e22, e24) = h1(op1(e12, e13))) | (~ spl28_322 | spl28_450 | ~ spl28_455)), inference(forward_demodulation, [], [f7648, f2384])).
fof(f7648, plain, (~ (h1(op1(e12, e13)) = op2(h1(e12), e24)) | (spl28_450 | ~ spl28_455)), inference(forward_demodulation, [], [f3260, f3279])).
fof(f3260, plain, (~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | spl28_450), inference(avatar_component_clause, [], [f3258])).
fof(f3258, plain, (spl28_450 <=> (h1(op1(e12, e13)) = op2(h1(e12), h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl28_450])])).
fof(f7608, plain, (~ spl28_23 | ~ spl28_243 | ~ spl28_322 | ~ spl28_327 | spl28_460), inference(avatar_contradiction_clause, [], [f7607])).
fof(f7607, plain, ($false | (~ spl28_23 | ~ spl28_243 | ~ spl28_322 | ~ spl28_327 | spl28_460)), inference(subsumption_resolution, [], [f7606, f1805])).
fof(f1805, plain, ((e22 = op2(e20, e21)) | ~ spl28_243), inference(avatar_component_clause, [], [f1803])).
fof(f1803, plain, (spl28_243 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_243])])).
fof(f7606, plain, (~ (e22 = op2(e20, e21)) | (~ spl28_23 | ~ spl28_322 | ~ spl28_327 | spl28_460)), inference(forward_demodulation, [], [f7605, f2384])).
fof(f7605, plain, (~ (op2(e20, e21) = h1(e12)) | (~ spl28_23 | ~ spl28_327 | spl28_460)), inference(forward_demodulation, [], [f7604, f831])).
fof(f831, plain, ((e12 = op1(e14, e10)) | ~ spl28_23), inference(avatar_component_clause, [], [f829])).
fof(f829, plain, (spl28_23 <=> (e12 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_23])])).
fof(f7604, plain, (~ (op2(e20, e21) = h1(op1(e14, e10))) | (~ spl28_327 | spl28_460)), inference(forward_demodulation, [], [f3300, f2411])).
fof(f3300, plain, (~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | spl28_460), inference(avatar_component_clause, [], [f3298])).
fof(f3298, plain, (spl28_460 <=> (h1(op1(e14, e10)) = op2(e20, h1(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_460])])).
fof(f7575, plain, (~ spl28_83 | ~ spl28_153 | ~ spl28_260 | ~ spl28_322 | spl28_446 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7574])).
fof(f7574, plain, ($false | (~ spl28_83 | ~ spl28_153 | ~ spl28_260 | ~ spl28_322 | spl28_446 | ~ spl28_455)), inference(subsumption_resolution, [], [f7573, f1427])).
fof(f1427, plain, ((e22 = op2(e23, e24)) | ~ spl28_153), inference(avatar_component_clause, [], [f1425])).
fof(f1425, plain, (spl28_153 <=> (e22 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_153])])).
fof(f7573, plain, (~ (e22 = op2(e23, e24)) | (~ spl28_83 | ~ spl28_260 | ~ spl28_322 | spl28_446 | ~ spl28_455)), inference(forward_demodulation, [], [f7572, f2384])).
fof(f7572, plain, (~ (op2(e23, e24) = h1(e12)) | (~ spl28_83 | ~ spl28_260 | spl28_446 | ~ spl28_455)), inference(forward_demodulation, [], [f7571, f1083])).
fof(f1083, plain, ((e12 = op1(e11, e13)) | ~ spl28_83), inference(avatar_component_clause, [], [f1081])).
fof(f1081, plain, (spl28_83 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_83])])).
fof(f7571, plain, (~ (op2(e23, e24) = h1(op1(e11, e13))) | (~ spl28_260 | spl28_446 | ~ spl28_455)), inference(forward_demodulation, [], [f7570, f2057])).
fof(f7570, plain, (~ (h1(op1(e11, e13)) = op2(h1(e11), e24)) | (spl28_446 | ~ spl28_455)), inference(forward_demodulation, [], [f3244, f3279])).
fof(f3244, plain, (~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | spl28_446), inference(avatar_component_clause, [], [f3242])).
fof(f3242, plain, (spl28_446 <=> (h1(op1(e11, e13)) = op2(h1(e11), h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl28_446])])).
fof(f7537, plain, (~ spl28_51 | ~ spl28_197 | ~ spl28_322 | ~ spl28_327 | spl28_462), inference(avatar_contradiction_clause, [], [f7536])).
fof(f7536, plain, ($false | (~ spl28_51 | ~ spl28_197 | ~ spl28_322 | ~ spl28_327 | spl28_462)), inference(subsumption_resolution, [], [f7535, f1612])).
fof(f1612, plain, ((e21 = op2(e22, e20)) | ~ spl28_197), inference(avatar_component_clause, [], [f1610])).
fof(f1610, plain, (spl28_197 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_197])])).
fof(f7535, plain, (~ (e21 = op2(e22, e20)) | (~ spl28_51 | ~ spl28_322 | ~ spl28_327 | spl28_462)), inference(forward_demodulation, [], [f7534, f2411])).
fof(f7534, plain, (~ (op2(e22, e20) = h1(e10)) | (~ spl28_51 | ~ spl28_322 | spl28_462)), inference(forward_demodulation, [], [f7533, f949])).
fof(f949, plain, ((e10 = op1(e12, e14)) | ~ spl28_51), inference(avatar_component_clause, [], [f947])).
fof(f947, plain, (spl28_51 <=> (e10 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_51])])).
fof(f7533, plain, (~ (op2(e22, e20) = h1(op1(e12, e14))) | (~ spl28_322 | spl28_462)), inference(forward_demodulation, [], [f3308, f2384])).
fof(f3308, plain, (~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | spl28_462), inference(avatar_component_clause, [], [f3306])).
fof(f3306, plain, (spl28_462 <=> (h1(op1(e12, e14)) = op2(h1(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_462])])).
fof(f7492, plain, (~ spl28_105 | ~ spl28_221 | ~ spl28_327 | spl28_465), inference(avatar_contradiction_clause, [], [f7491])).
fof(f7491, plain, ($false | (~ spl28_105 | ~ spl28_221 | ~ spl28_327 | spl28_465)), inference(subsumption_resolution, [], [f7490, f1713])).
fof(f1713, plain, ((e20 = op2(e21, e20)) | ~ spl28_221), inference(avatar_component_clause, [], [f1711])).
fof(f1711, plain, (spl28_221 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_221])])).
fof(f7490, plain, (~ (e20 = op2(e21, e20)) | (~ spl28_105 | ~ spl28_327 | spl28_465)), inference(forward_demodulation, [], [f7489, f586])).
fof(f7489, plain, (~ (op2(e21, e20) = h1(e14)) | (~ spl28_105 | ~ spl28_327 | spl28_465)), inference(forward_demodulation, [], [f7488, f1175])).
fof(f1175, plain, ((e14 = op1(e10, e14)) | ~ spl28_105), inference(avatar_component_clause, [], [f1173])).
fof(f1173, plain, (spl28_105 <=> (e14 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_105])])).
fof(f7488, plain, (~ (op2(e21, e20) = h1(op1(e10, e14))) | (~ spl28_327 | spl28_465)), inference(forward_demodulation, [], [f3320, f2411])).
fof(f3320, plain, (~ (h1(op1(e10, e14)) = op2(h1(e10), e20)) | spl28_465), inference(avatar_component_clause, [], [f3318])).
fof(f3318, plain, (spl28_465 <=> (h1(op1(e10, e14)) = op2(h1(e10), e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_465])])).
fof(f7449, plain, (~ spl28_87 | ~ spl28_164 | ~ spl28_260 | ~ spl28_322 | spl28_445), inference(avatar_contradiction_clause, [], [f7448])).
fof(f7448, plain, ($false | (~ spl28_87 | ~ spl28_164 | ~ spl28_260 | ~ spl28_322 | spl28_445)), inference(subsumption_resolution, [], [f7447, f1473])).
fof(f1473, plain, ((e23 = op2(e23, e22)) | ~ spl28_164), inference(avatar_component_clause, [], [f1471])).
fof(f1471, plain, (spl28_164 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_164])])).
fof(f7447, plain, (~ (e23 = op2(e23, e22)) | (~ spl28_87 | ~ spl28_260 | ~ spl28_322 | spl28_445)), inference(forward_demodulation, [], [f7446, f2057])).
fof(f7446, plain, (~ (op2(e23, e22) = h1(e11)) | (~ spl28_87 | ~ spl28_260 | ~ spl28_322 | spl28_445)), inference(forward_demodulation, [], [f7445, f1100])).
fof(f1100, plain, ((e11 = op1(e11, e12)) | ~ spl28_87), inference(avatar_component_clause, [], [f1098])).
fof(f1098, plain, (spl28_87 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_87])])).
fof(f7445, plain, (~ (op2(e23, e22) = h1(op1(e11, e12))) | (~ spl28_260 | ~ spl28_322 | spl28_445)), inference(forward_demodulation, [], [f7444, f2057])).
fof(f7444, plain, (~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | (~ spl28_322 | spl28_445)), inference(forward_demodulation, [], [f3240, f2384])).
fof(f3240, plain, (~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | spl28_445), inference(avatar_component_clause, [], [f3238])).
fof(f3238, plain, (spl28_445 <=> (h1(op1(e11, e12)) = op2(h1(e11), h1(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_445])])).
fof(f7414, plain, (~ spl28_96 | ~ spl28_167 | ~ spl28_260 | ~ spl28_327 | spl28_444), inference(avatar_contradiction_clause, [], [f7413])).
fof(f7413, plain, ($false | (~ spl28_96 | ~ spl28_167 | ~ spl28_260 | ~ spl28_327 | spl28_444)), inference(subsumption_resolution, [], [f7412, f1486])).
fof(f1486, plain, ((e21 = op2(e23, e21)) | ~ spl28_167), inference(avatar_component_clause, [], [f1484])).
fof(f1484, plain, (spl28_167 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_167])])).
fof(f7412, plain, (~ (e21 = op2(e23, e21)) | (~ spl28_96 | ~ spl28_260 | ~ spl28_327 | spl28_444)), inference(forward_demodulation, [], [f7411, f2411])).
fof(f7411, plain, (~ (op2(e23, e21) = h1(e10)) | (~ spl28_96 | ~ spl28_260 | ~ spl28_327 | spl28_444)), inference(forward_demodulation, [], [f7410, f1138])).
fof(f1138, plain, ((e10 = op1(e11, e10)) | ~ spl28_96), inference(avatar_component_clause, [], [f1136])).
fof(f1136, plain, (spl28_96 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_96])])).
fof(f7410, plain, (~ (op2(e23, e21) = h1(op1(e11, e10))) | (~ spl28_260 | ~ spl28_327 | spl28_444)), inference(forward_demodulation, [], [f7409, f2057])).
fof(f7409, plain, (~ (h1(op1(e11, e10)) = op2(h1(e11), e21)) | (~ spl28_327 | spl28_444)), inference(forward_demodulation, [], [f3236, f2411])).
fof(f3236, plain, (~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | spl28_444), inference(avatar_component_clause, [], [f3234])).
fof(f3234, plain, (spl28_444 <=> (h1(op1(e11, e10)) = op2(h1(e11), h1(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_444])])).
fof(f7376, plain, (~ spl28_95 | spl28_464), inference(avatar_contradiction_clause, [], [f7375])).
fof(f7375, plain, ($false | (~ spl28_95 | spl28_464)), inference(subsumption_resolution, [], [f7374, f586])).
fof(f7374, plain, (~ (e20 = h1(e14)) | (~ spl28_95 | spl28_464)), inference(forward_demodulation, [], [f3316, f1133])).
fof(f1133, plain, ((e14 = op1(e11, e11)) | ~ spl28_95), inference(avatar_component_clause, [], [f1131])).
fof(f1131, plain, (spl28_95 <=> (e14 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_95])])).
fof(f3316, plain, (~ (e20 = h1(op1(e11, e11))) | spl28_464), inference(avatar_component_clause, [], [f3314])).
fof(f3314, plain, (spl28_464 <=> (e20 = h1(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_464])])).
fof(f7342, plain, (~ spl28_107 | ~ spl28_204 | ~ spl28_260 | ~ spl28_327 | spl28_443 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7341])).
fof(f7341, plain, ($false | (~ spl28_107 | ~ spl28_204 | ~ spl28_260 | ~ spl28_327 | spl28_443 | ~ spl28_455)), inference(subsumption_resolution, [], [f7340, f1641])).
fof(f1641, plain, ((e23 = op2(e21, e24)) | ~ spl28_204), inference(avatar_component_clause, [], [f1639])).
fof(f1639, plain, (spl28_204 <=> (e23 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_204])])).
fof(f7340, plain, (~ (e23 = op2(e21, e24)) | (~ spl28_107 | ~ spl28_260 | ~ spl28_327 | spl28_443 | ~ spl28_455)), inference(forward_demodulation, [], [f7339, f2057])).
fof(f7339, plain, (~ (op2(e21, e24) = h1(e11)) | (~ spl28_107 | ~ spl28_327 | spl28_443 | ~ spl28_455)), inference(forward_demodulation, [], [f7338, f1184])).
fof(f1184, plain, ((e11 = op1(e10, e13)) | ~ spl28_107), inference(avatar_component_clause, [], [f1182])).
fof(f1182, plain, (spl28_107 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_107])])).
fof(f7338, plain, (~ (op2(e21, e24) = h1(op1(e10, e13))) | (~ spl28_327 | spl28_443 | ~ spl28_455)), inference(forward_demodulation, [], [f7337, f2411])).
fof(f7337, plain, (~ (h1(op1(e10, e13)) = op2(h1(e10), e24)) | (spl28_443 | ~ spl28_455)), inference(forward_demodulation, [], [f3232, f3279])).
fof(f3232, plain, (~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | spl28_443), inference(avatar_component_clause, [], [f3230])).
fof(f3230, plain, (spl28_443 <=> (h1(op1(e10, e13)) = op2(h1(e10), h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl28_443])])).
fof(f7296, plain, (~ spl28_124 | spl28_467), inference(avatar_contradiction_clause, [], [f7295])).
fof(f7295, plain, ($false | (~ spl28_124 | spl28_467)), inference(trivial_inequality_removal, [], [f7294])).
fof(f7294, plain, (~ (h1(e13) = h1(e13)) | (~ spl28_124 | spl28_467)), inference(forward_demodulation, [], [f3328, f1255])).
fof(f1255, plain, ((op1(e10, e10) = e13) | ~ spl28_124), inference(avatar_component_clause, [], [f1253])).
fof(f1253, plain, (spl28_124 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl28_124])])).
fof(f3328, plain, (~ (h1(e13) = h1(op1(e10, e10))) | spl28_467), inference(avatar_component_clause, [], [f3326])).
fof(f3326, plain, (spl28_467 <=> (h1(e13) = h1(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_467])])).
fof(f7263, plain, (~ spl28_111 | ~ spl28_212 | ~ spl28_322 | ~ spl28_327 | spl28_442), inference(avatar_contradiction_clause, [], [f7262])).
fof(f7262, plain, ($false | (~ spl28_111 | ~ spl28_212 | ~ spl28_322 | ~ spl28_327 | spl28_442)), inference(subsumption_resolution, [], [f7261, f1675])).
fof(f1675, plain, ((e21 = op2(e21, e22)) | ~ spl28_212), inference(avatar_component_clause, [], [f1673])).
fof(f1673, plain, (spl28_212 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_212])])).
fof(f7261, plain, (~ (e21 = op2(e21, e22)) | (~ spl28_111 | ~ spl28_322 | ~ spl28_327 | spl28_442)), inference(forward_demodulation, [], [f7260, f2411])).
fof(f7260, plain, (~ (op2(e21, e22) = h1(e10)) | (~ spl28_111 | ~ spl28_322 | ~ spl28_327 | spl28_442)), inference(forward_demodulation, [], [f7259, f1201])).
fof(f1201, plain, ((e10 = op1(e10, e12)) | ~ spl28_111), inference(avatar_component_clause, [], [f1199])).
fof(f1199, plain, (spl28_111 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_111])])).
fof(f7259, plain, (~ (op2(e21, e22) = h1(op1(e10, e12))) | (~ spl28_322 | ~ spl28_327 | spl28_442)), inference(forward_demodulation, [], [f7258, f2411])).
fof(f7258, plain, (~ (h1(op1(e10, e12)) = op2(h1(e10), e22)) | (~ spl28_322 | spl28_442)), inference(forward_demodulation, [], [f3228, f2384])).
fof(f3228, plain, (~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | spl28_442), inference(avatar_component_clause, [], [f3226])).
fof(f3226, plain, (spl28_442 <=> (h1(op1(e10, e12)) = op2(h1(e10), h1(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_442])])).
fof(f7224, plain, (~ spl28_118 | spl28_466), inference(avatar_contradiction_clause, [], [f7223])).
fof(f7223, plain, ($false | (~ spl28_118 | spl28_466)), inference(trivial_inequality_removal, [], [f7222])).
fof(f7222, plain, (~ (h1(e12) = h1(e12)) | (~ spl28_118 | spl28_466)), inference(forward_demodulation, [], [f3324, f1230])).
fof(f1230, plain, ((e12 = op1(e10, e11)) | ~ spl28_118), inference(avatar_component_clause, [], [f1228])).
fof(f1228, plain, (spl28_118 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_118])])).
fof(f3324, plain, (~ (h1(e12) = h1(op1(e10, e11))) | spl28_466), inference(avatar_component_clause, [], [f3322])).
fof(f3322, plain, (spl28_466 <=> (h1(e12) = h1(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_466])])).
fof(f7147, plain, (~ spl28_55 | ~ spl28_60), inference(avatar_split_clause, [], [f7145, f984, f963])).
fof(f963, plain, (spl28_55 <=> (e14 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_55])])).
fof(f7145, plain, (~ (e14 = op1(e12, e14)) | ~ spl28_60), inference(backward_demodulation, [], [f362, f986])).
fof(f362, plain, ~ (op1(e12, e13) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (op1(e14, e13) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e14)) & ~ (op1(e14, e11) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e14)) & ~ (op1(e14, e10) = op1(e14, e13)) & ~ (op1(e14, e10) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e11)) & ~ (op1(e13, e13) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e14)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e14)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e13) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e14)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e14)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e13) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e14)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e14)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e13) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e14)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e14)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e13, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e14, e14)) & ~ (op1(e11, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e14, e14)) & ~ (op1(e10, e14) = op1(e13, e14)) & ~ (op1(e10, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e11, e14)) & ~ (op1(e13, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e14, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e14, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e13, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e14, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e14, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e13, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e14, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e14, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e13, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e14, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e14, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax7)).
fof(f7141, plain, (~ spl28_53 | ~ spl28_63), inference(avatar_split_clause, [], [f7138, f997, f955])).
fof(f955, plain, (spl28_53 <=> (e12 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_53])])).
fof(f7138, plain, (~ (e12 = op1(e12, e14)) | ~ spl28_63), inference(backward_demodulation, [], [f361, f999])).
fof(f361, plain, ~ (op1(e12, e12) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f7132, plain, (~ spl28_69 | ~ spl28_185 | ~ spl28_260 | ~ spl28_322 | spl28_448 | ~ spl28_455), inference(avatar_contradiction_clause, [], [f7131])).
fof(f7131, plain, ($false | (~ spl28_69 | ~ spl28_185 | ~ spl28_260 | ~ spl28_322 | spl28_448 | ~ spl28_455)), inference(subsumption_resolution, [], [f7125, f3279])).
fof(f7125, plain, (~ (e24 = h1(e13)) | (~ spl28_69 | ~ spl28_185 | ~ spl28_260 | ~ spl28_322 | spl28_448)), inference(backward_demodulation, [], [f7019, f1024])).
fof(f1024, plain, ((e13 = op1(e12, e11)) | ~ spl28_69), inference(avatar_component_clause, [], [f1022])).
fof(f1022, plain, (spl28_69 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_69])])).
fof(f7019, plain, (~ (e24 = h1(op1(e12, e11))) | (~ spl28_185 | ~ spl28_260 | ~ spl28_322 | spl28_448)), inference(backward_demodulation, [], [f6906, f1561])).
fof(f1561, plain, ((e24 = op2(e22, e23)) | ~ spl28_185), inference(avatar_component_clause, [], [f1559])).
fof(f1559, plain, (spl28_185 <=> (e24 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_185])])).
fof(f6906, plain, (~ (op2(e22, e23) = h1(op1(e12, e11))) | (~ spl28_260 | ~ spl28_322 | spl28_448)), inference(backward_demodulation, [], [f6871, f2384])).
fof(f6871, plain, (~ (h1(op1(e12, e11)) = op2(h1(e12), e23)) | (~ spl28_260 | spl28_448)), inference(forward_demodulation, [], [f3252, f2057])).
fof(f3252, plain, (~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | spl28_448), inference(avatar_component_clause, [], [f3250])).
fof(f3250, plain, (spl28_448 <=> (h1(op1(e12, e11)) = op2(h1(e12), h1(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_448])])).
fof(f7130, plain, (~ spl28_59 | ~ spl28_69), inference(avatar_split_clause, [], [f7122, f1022, f980])).
fof(f980, plain, (spl28_59 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_59])])).
fof(f7122, plain, (~ (e13 = op1(e12, e13)) | ~ spl28_69), inference(backward_demodulation, [], [f358, f1024])).
fof(f358, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f7128, plain, (~ spl28_44 | ~ spl28_69), inference(avatar_split_clause, [], [f7120, f1022, f917])).
fof(f917, plain, (spl28_44 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_44])])).
fof(f7120, plain, (~ (e13 = op1(e13, e11)) | ~ spl28_69), inference(backward_demodulation, [], [f300, f1024])).
fof(f300, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f7119, plain, (~ spl28_67 | ~ spl28_72), inference(avatar_split_clause, [], [f7110, f1035, f1014])).
fof(f1014, plain, (spl28_67 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_67])])).
fof(f7110, plain, (~ (e11 = op1(e12, e11)) | ~ spl28_72), inference(backward_demodulation, [], [f353, f1037])).
fof(f353, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f7100, plain, (spl28_72 | ~ spl28_96 | ~ spl28_118), inference(avatar_split_clause, [], [f7096, f1228, f1136, f1035])).
fof(f7096, plain, ((e11 = op1(e12, e10)) | (~ spl28_96 | ~ spl28_118)), inference(backward_demodulation, [], [f5180, f1138])).
fof(f5180, plain, ((e11 = op1(e12, op1(e11, e10))) | ~ spl28_118), inference(forward_demodulation, [], [f163, f1230])).
fof(f163, plain, (e11 = op1(op1(e10, e11), op1(e11, e10))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ((e14 = op1(op1(e14, e14), op1(e14, e14))) & (e14 = op1(op1(e13, e14), op1(e14, e13))) & (e14 = op1(op1(e12, e14), op1(e14, e12))) & (e14 = op1(op1(e11, e14), op1(e14, e11))) & (e14 = op1(op1(e10, e14), op1(e14, e10))) & (e13 = op1(op1(e14, e13), op1(e13, e14))) & (e13 = op1(op1(e13, e13), op1(e13, e13))) & (e13 = op1(op1(e12, e13), op1(e13, e12))) & (e13 = op1(op1(e11, e13), op1(e13, e11))) & (e13 = op1(op1(e10, e13), op1(e13, e10))) & (e12 = op1(op1(e14, e12), op1(e12, e14))) & (e12 = op1(op1(e13, e12), op1(e12, e13))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e12 = op1(op1(e11, e12), op1(e12, e11))) & (e12 = op1(op1(e10, e12), op1(e12, e10))) & (e11 = op1(op1(e14, e11), op1(e11, e14))) & (e11 = op1(op1(e13, e11), op1(e11, e13))) & (e11 = op1(op1(e12, e11), op1(e11, e12))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e11 = op1(op1(e10, e11), op1(e11, e10))) & (e10 = op1(op1(e14, e10), op1(e10, e14))) & (e10 = op1(op1(e13, e10), op1(e10, e13))) & (e10 = op1(op1(e12, e10), op1(e10, e12))) & (e10 = op1(op1(e11, e10), op1(e10, e11))) & (e10 = op1(op1(e10, e10), op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax3)).
fof(f7087, plain, (spl28_51 | ~ spl28_23 | ~ spl28_105), inference(avatar_split_clause, [], [f7083, f1173, f829, f947])).
fof(f7083, plain, ((e10 = op1(e12, e14)) | (~ spl28_23 | ~ spl28_105)), inference(backward_demodulation, [], [f6845, f1175])).
fof(f6845, plain, ((e10 = op1(e12, op1(e10, e14))) | ~ spl28_23), inference(forward_demodulation, [], [f162, f831])).
fof(f162, plain, (e10 = op1(op1(e14, e10), op1(e10, e14))), inference(cnf_transformation, [], [f3])).
fof(f7086, plain, (spl28_15 | ~ spl28_23 | ~ spl28_105), inference(avatar_split_clause, [], [f7082, f1173, f829, f795])).
fof(f7082, plain, ((e14 = op1(e14, e12)) | (~ spl28_23 | ~ spl28_105)), inference(backward_demodulation, [], [f6844, f1175])).
fof(f6844, plain, ((e14 = op1(op1(e10, e14), e12)) | ~ spl28_23), inference(forward_demodulation, [], [f178, f831])).
fof(f178, plain, (e14 = op1(op1(e10, e14), op1(e14, e10))), inference(cnf_transformation, [], [f3])).
fof(f7085, plain, (~ spl28_55 | ~ spl28_105), inference(avatar_split_clause, [], [f7080, f1173, f963])).
fof(f7080, plain, (~ (e14 = op1(e12, e14)) | ~ spl28_105), inference(backward_demodulation, [], [f324, f1175])).
fof(f324, plain, ~ (op1(e10, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f7076, plain, (~ spl28_101 | ~ spl28_111), inference(avatar_split_clause, [], [f7068, f1199, f1157])).
fof(f1157, plain, (spl28_101 <=> (e10 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_101])])).
fof(f7068, plain, (~ (e10 = op1(e10, e14)) | ~ spl28_111), inference(backward_demodulation, [], [f341, f1201])).
fof(f341, plain, ~ (op1(e10, e12) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f7061, plain, (~ spl28_134 | ~ spl28_154 | ~ spl28_156), inference(avatar_contradiction_clause, [], [f7060])).
fof(f7060, plain, ($false | (~ spl28_134 | ~ spl28_154 | ~ spl28_156)), inference(subsumption_resolution, [], [f7059, f495])).
fof(f495, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (~ (e23 = e24) & ~ (e22 = e24) & ~ (e22 = e23) & ~ (e21 = e24) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e24) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax10)).
fof(f7059, plain, ((e20 = e23) | (~ spl28_134 | ~ spl28_154 | ~ spl28_156)), inference(forward_demodulation, [], [f7054, f1440])).
fof(f1440, plain, ((e20 = op2(e23, e23)) | ~ spl28_156), inference(avatar_component_clause, [], [f1438])).
fof(f1438, plain, (spl28_156 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_156])])).
fof(f7054, plain, ((e23 = op2(e23, e23)) | (~ spl28_134 | ~ spl28_154)), inference(backward_demodulation, [], [f6850, f1347])).
fof(f6850, plain, ((e23 = op2(op2(e24, e23), e23)) | ~ spl28_154), inference(forward_demodulation, [], [f277, f1431])).
fof(f1431, plain, ((e23 = op2(e23, e24)) | ~ spl28_154), inference(avatar_component_clause, [], [f1429])).
fof(f1429, plain, (spl28_154 <=> (e23 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_154])])).
fof(f277, plain, (e23 = op2(op2(e24, e23), op2(e23, e24))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e24 = op2(op2(e24, e24), op2(e24, e24))) & (e24 = op2(op2(e23, e24), op2(e24, e23))) & (e24 = op2(op2(e22, e24), op2(e24, e22))) & (e24 = op2(op2(e21, e24), op2(e24, e21))) & (e24 = op2(op2(e20, e24), op2(e24, e20))) & (e23 = op2(op2(e24, e23), op2(e23, e24))) & (e23 = op2(op2(e23, e23), op2(e23, e23))) & (e23 = op2(op2(e22, e23), op2(e23, e22))) & (e23 = op2(op2(e21, e23), op2(e23, e21))) & (e23 = op2(op2(e20, e23), op2(e23, e20))) & (e22 = op2(op2(e24, e22), op2(e22, e24))) & (e22 = op2(op2(e23, e22), op2(e22, e23))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e22 = op2(op2(e21, e22), op2(e22, e21))) & (e22 = op2(op2(e20, e22), op2(e22, e20))) & (e21 = op2(op2(e24, e21), op2(e21, e24))) & (e21 = op2(op2(e23, e21), op2(e21, e23))) & (e21 = op2(op2(e22, e21), op2(e21, e22))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e21 = op2(op2(e20, e21), op2(e21, e20))) & (e20 = op2(op2(e24, e20), op2(e20, e24))) & (e20 = op2(op2(e23, e20), op2(e20, e23))) & (e20 = op2(op2(e22, e20), op2(e20, e22))) & (e20 = op2(op2(e21, e20), op2(e20, e21))) & (e20 = op2(op2(e20, e20), op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax6)).
fof(f7022, plain, (~ spl28_141 | ~ spl28_162 | ~ spl28_185), inference(avatar_contradiction_clause, [], [f7021])).
fof(f7021, plain, ($false | (~ spl28_141 | ~ spl28_162 | ~ spl28_185)), inference(subsumption_resolution, [], [f7020, f495])).
fof(f7020, plain, ((e20 = e23) | (~ spl28_141 | ~ spl28_162 | ~ spl28_185)), inference(forward_demodulation, [], [f7017, f1377])).
fof(f7017, plain, ((e23 = op2(e24, e21)) | (~ spl28_162 | ~ spl28_185)), inference(backward_demodulation, [], [f6863, f1561])).
fof(f6863, plain, ((e23 = op2(op2(e22, e23), e21)) | ~ spl28_162), inference(forward_demodulation, [], [f275, f1465])).
fof(f1465, plain, ((e21 = op2(e23, e22)) | ~ spl28_162), inference(avatar_component_clause, [], [f1463])).
fof(f1463, plain, (spl28_162 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_162])])).
fof(f275, plain, (e23 = op2(op2(e22, e23), op2(e23, e22))), inference(cnf_transformation, [], [f6])).
fof(f6962, plain, (spl28_197 | ~ spl28_221 | ~ spl28_243), inference(avatar_split_clause, [], [f6958, f1803, f1711, f1610])).
fof(f6958, plain, ((e21 = op2(e22, e20)) | (~ spl28_221 | ~ spl28_243)), inference(backward_demodulation, [], [f6139, f1713])).
fof(f6139, plain, ((e21 = op2(e22, op2(e21, e20))) | ~ spl28_243), inference(forward_demodulation, [], [f263, f1805])).
fof(f263, plain, (e21 = op2(op2(e20, e21), op2(e21, e20))), inference(cnf_transformation, [], [f6])).
fof(f6940, plain, (~ spl28_226 | ~ spl28_236), inference(avatar_split_clause, [], [f6933, f1774, f1732])).
fof(f1732, plain, (spl28_226 <=> (e20 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_226])])).
fof(f6933, plain, (~ (e20 = op2(e20, e24)) | ~ spl28_236), inference(backward_demodulation, [], [f441, f1776])).
fof(f441, plain, ~ (op2(e20, e22) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (op2(e24, e23) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e24)) & ~ (op2(e24, e21) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e24)) & ~ (op2(e24, e20) = op2(e24, e23)) & ~ (op2(e24, e20) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e21)) & ~ (op2(e23, e23) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e24)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e24)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e23) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e24)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e24)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e23) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e24)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e24)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e23) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e24)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e24)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e23, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e24, e24)) & ~ (op2(e21, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e24, e24)) & ~ (op2(e20, e24) = op2(e23, e24)) & ~ (op2(e20, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e21, e24)) & ~ (op2(e23, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e24, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e24, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e23, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e24, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e24, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e23, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e24, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e24, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e23, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e24, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e24, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax8)).
fof(f6939, plain, (~ spl28_211 | ~ spl28_236), inference(avatar_split_clause, [], [f6931, f1774, f1669])).
fof(f1669, plain, (spl28_211 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_211])])).
fof(f6931, plain, (~ (e20 = op2(e21, e22)) | ~ spl28_236), inference(backward_demodulation, [], [f403, f1776])).
fof(f403, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f8])).
fof(f6910, plain, (~ spl28_213 | ~ spl28_260 | ~ spl28_322 | ~ spl28_327), inference(avatar_split_clause, [], [f6904, f2410, f2383, f2056, f1677])).
fof(f1677, plain, (spl28_213 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_213])])).
fof(f6904, plain, (~ (e22 = op2(e21, e22)) | (~ spl28_260 | ~ spl28_322 | ~ spl28_327)), inference(backward_demodulation, [], [f6505, f2384])).
fof(f6505, plain, (~ (op2(e21, e22) = h1(e12)) | (~ spl28_260 | ~ spl28_327)), inference(forward_demodulation, [], [f450, f5965])).
fof(f5965, plain, ((op2(e21, e23) = h1(e12)) | (~ spl28_260 | ~ spl28_327)), inference(backward_demodulation, [], [f4350, f2411])).
fof(f4350, plain, ((h1(e12) = op2(h1(e10), e23)) | ~ spl28_260), inference(backward_demodulation, [], [f1989, f2057])).
fof(f1989, plain, (h1(e12) = op2(h1(e10), h1(e11))), inference(backward_demodulation, [], [f1985, f1987])).
fof(f1987, plain, (h1(e10) = op2(e20, h1(e11))), inference(forward_demodulation, [], [f587, f588])).
fof(f588, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f16])).
fof(f587, plain, (h1(e10) = op2(e20, op2(e20, e20))), inference(cnf_transformation, [], [f16])).
fof(f1985, plain, (h1(e12) = op2(op2(e20, h1(e11)), h1(e11))), inference(backward_demodulation, [], [f589, f588])).
fof(f589, plain, (h1(e12) = op2(op2(e20, op2(e20, e20)), op2(e20, e20))), inference(cnf_transformation, [], [f16])).
fof(f450, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f6893, plain, (spl28_204 | ~ spl28_260 | ~ spl28_284 | ~ spl28_327 | ~ spl28_383), inference(avatar_split_clause, [], [f6880, f2840, f2410, f2190, f2056, f1639])).
fof(f2190, plain, (spl28_284 <=> (e20 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_284])])).
fof(f2840, plain, (spl28_383 <=> (e24 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_383])])).
fof(f6880, plain, ((e23 = op2(e21, e24)) | (~ spl28_260 | ~ spl28_284 | ~ spl28_327 | ~ spl28_383)), inference(backward_demodulation, [], [f5968, f2841])).
fof(f2841, plain, ((e24 = h4(e10)) | ~ spl28_383), inference(avatar_component_clause, [], [f2840])).
fof(f5968, plain, ((e23 = op2(e21, h4(e10))) | (~ spl28_260 | ~ spl28_284 | ~ spl28_327)), inference(backward_demodulation, [], [f5892, f2411])).
fof(f5892, plain, ((e23 = op2(h1(e10), h4(e10))) | (~ spl28_260 | ~ spl28_284)), inference(forward_demodulation, [], [f4360, f5664])).
fof(f5664, plain, ((op2(e23, e20) = h4(e10)) | ~ spl28_284), inference(backward_demodulation, [], [f2029, f2191])).
fof(f2191, plain, ((e20 = h4(e11)) | ~ spl28_284), inference(avatar_component_clause, [], [f2190])).
fof(f2029, plain, (h4(e10) = op2(e23, h4(e11))), inference(forward_demodulation, [], [f602, f603])).
fof(f603, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((h4(e13) = op2(op2(e23, op2(e23, e23)), op2(e23, op2(e23, e23)))) & (h4(e12) = op2(op2(e23, op2(e23, e23)), op2(e23, e23))) & (op2(e23, e23) = h4(e11)) & (h4(e10) = op2(e23, op2(e23, e23))) & (e23 = h4(e14))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax19)).
fof(f602, plain, (h4(e10) = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f19])).
fof(f4360, plain, ((e23 = op2(h1(e10), op2(e23, e20))) | ~ spl28_260), inference(backward_demodulation, [], [f273, f4349])).
fof(f4349, plain, ((op2(e20, e23) = h1(e10)) | ~ spl28_260), inference(backward_demodulation, [], [f1987, f2057])).
fof(f273, plain, (e23 = op2(op2(e20, e23), op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f6857, plain, (~ spl28_212 | ~ spl28_162), inference(avatar_split_clause, [], [f6856, f1463, f1673])).
fof(f6856, plain, (~ (e21 = op2(e21, e22)) | ~ spl28_162), inference(forward_demodulation, [], [f408, f1465])).
fof(f408, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f6834, plain, (~ spl28_54 | ~ spl28_79), inference(avatar_split_clause, [], [f6833, f1064, f959])).
fof(f959, plain, (spl28_54 <=> (e13 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_54])])).
fof(f1064, plain, (spl28_79 <=> (e13 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_79])])).
fof(f6833, plain, (~ (e13 = op1(e12, e14)) | ~ spl28_79), inference(forward_demodulation, [], [f327, f1066])).
fof(f1066, plain, ((e13 = op1(e11, e14)) | ~ spl28_79), inference(avatar_component_clause, [], [f1064])).
fof(f327, plain, ~ (op1(e11, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f6832, plain, (~ spl28_29 | ~ spl28_79), inference(avatar_split_clause, [], [f6831, f1064, f854])).
fof(f854, plain, (spl28_29 <=> (e13 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_29])])).
fof(f6831, plain, (~ (e13 = op1(e13, e14)) | ~ spl28_79), inference(forward_demodulation, [], [f328, f1066])).
fof(f328, plain, ~ (op1(e11, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f6817, plain, (~ spl28_8 | ~ spl28_23), inference(avatar_split_clause, [], [f6811, f829, f766])).
fof(f766, plain, (spl28_8 <=> (e12 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_8])])).
fof(f6811, plain, (~ (e12 = op1(e14, e13)) | ~ spl28_23), inference(backward_demodulation, [], [f375, f831])).
fof(f375, plain, ~ (op1(e14, e10) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f6810, plain, (~ spl28_31 | ~ spl28_33), inference(avatar_contradiction_clause, [], [f6809])).
fof(f6809, plain, ($false | (~ spl28_31 | ~ spl28_33)), inference(subsumption_resolution, [], [f6808, f484])).
fof(f484, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f9])).
fof(f9, plain, (~ (e13 = e14) & ~ (e12 = e14) & ~ (e12 = e13) & ~ (e11 = e14) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e14) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax9)).
fof(f6808, plain, ((e10 = e12) | (~ spl28_31 | ~ spl28_33)), inference(forward_demodulation, [], [f873, f865])).
fof(f873, plain, ((e12 = op1(e13, e13)) | ~ spl28_33), inference(avatar_component_clause, [], [f871])).
fof(f871, plain, (spl28_33 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_33])])).
fof(f6796, plain, (~ spl28_25 | ~ spl28_50), inference(avatar_split_clause, [], [f6791, f942, f837])).
fof(f837, plain, (spl28_25 <=> (e14 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_25])])).
fof(f6791, plain, (~ (e14 = op1(e14, e10)) | ~ spl28_50), inference(backward_demodulation, [], [f292, f944])).
fof(f292, plain, ~ (op1(e13, e10) = op1(e14, e10)), inference(cnf_transformation, [], [f7])).
fof(f6782, plain, (~ spl28_31 | ~ spl28_64), inference(avatar_contradiction_clause, [], [f6781])).
fof(f6781, plain, ($false | (~ spl28_31 | ~ spl28_64)), inference(subsumption_resolution, [], [f6780, f484])).
fof(f6780, plain, ((e10 = e12) | (~ spl28_31 | ~ spl28_64)), inference(forward_demodulation, [], [f6776, f865])).
fof(f6776, plain, ((e12 = op1(e13, e13)) | ~ spl28_64), inference(backward_demodulation, [], [f170, f1003])).
fof(f1003, plain, ((e13 = op1(e12, e12)) | ~ spl28_64), inference(avatar_component_clause, [], [f1001])).
fof(f1001, plain, (spl28_64 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_64])])).
fof(f170, plain, (e12 = op1(op1(e12, e12), op1(e12, e12))), inference(cnf_transformation, [], [f3])).
fof(f6775, plain, (~ spl28_62 | ~ spl28_67), inference(avatar_split_clause, [], [f6767, f1014, f993])).
fof(f993, plain, (spl28_62 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_62])])).
fof(f6767, plain, (~ (e11 = op1(e12, e12)) | ~ spl28_67), inference(backward_demodulation, [], [f357, f1016])).
fof(f1016, plain, ((e11 = op1(e12, e11)) | ~ spl28_67), inference(avatar_component_clause, [], [f1014])).
fof(f357, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f6774, plain, (~ spl28_42 | ~ spl28_67), inference(avatar_split_clause, [], [f6766, f1014, f909])).
fof(f6766, plain, (~ (e11 = op1(e13, e11)) | ~ spl28_67), inference(backward_demodulation, [], [f300, f1016])).
fof(f6751, plain, (spl28_67 | ~ spl28_97 | ~ spl28_118), inference(avatar_split_clause, [], [f6747, f1228, f1140, f1014])).
fof(f1140, plain, (spl28_97 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_97])])).
fof(f6747, plain, ((e11 = op1(e12, e11)) | (~ spl28_97 | ~ spl28_118)), inference(backward_demodulation, [], [f5180, f1142])).
fof(f1142, plain, ((e11 = op1(e11, e10)) | ~ spl28_97), inference(avatar_component_clause, [], [f1140])).
fof(f6749, plain, (~ spl28_47 | ~ spl28_97), inference(avatar_split_clause, [], [f6740, f1140, f930])).
fof(f930, plain, (spl28_47 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_47])])).
fof(f6740, plain, (~ (e11 = op1(e13, e10)) | ~ spl28_97), inference(backward_demodulation, [], [f288, f1142])).
fof(f288, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f7])).
fof(f6719, plain, (~ spl28_139 | ~ spl28_162 | ~ spl28_178), inference(avatar_contradiction_clause, [], [f6718])).
fof(f6718, plain, ($false | (~ spl28_139 | ~ spl28_162 | ~ spl28_178)), inference(subsumption_resolution, [], [f6717, f497])).
fof(f497, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f10])).
fof(f6717, plain, ((e21 = e22) | (~ spl28_139 | ~ spl28_162 | ~ spl28_178)), inference(forward_demodulation, [], [f6714, f1465])).
fof(f6714, plain, ((e22 = op2(e23, e22)) | (~ spl28_139 | ~ spl28_178)), inference(backward_demodulation, [], [f6665, f1368])).
fof(f1368, plain, ((e23 = op2(e24, e22)) | ~ spl28_139), inference(avatar_component_clause, [], [f1366])).
fof(f1366, plain, (spl28_139 <=> (e23 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_139])])).
fof(f6665, plain, ((e22 = op2(op2(e24, e22), e22)) | ~ spl28_178), inference(backward_demodulation, [], [f272, f1532])).
fof(f1532, plain, ((e22 = op2(e22, e24)) | ~ spl28_178), inference(avatar_component_clause, [], [f1530])).
fof(f1530, plain, (spl28_178 <=> (e22 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_178])])).
fof(f272, plain, (e22 = op2(op2(e24, e22), op2(e22, e24))), inference(cnf_transformation, [], [f6])).
fof(f6647, plain, (~ spl28_167 | ~ spl28_192), inference(avatar_split_clause, [], [f6638, f1589, f1484])).
fof(f1589, plain, (spl28_192 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_192])])).
fof(f6638, plain, (~ (e21 = op2(e23, e21)) | ~ spl28_192), inference(backward_demodulation, [], [f400, f1591])).
fof(f1591, plain, ((e21 = op2(e22, e21)) | ~ spl28_192), inference(avatar_component_clause, [], [f1589])).
fof(f400, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f6602, plain, (spl28_192 | ~ spl28_222 | ~ spl28_243), inference(avatar_split_clause, [], [f6597, f1803, f1715, f1589])).
fof(f1715, plain, (spl28_222 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_222])])).
fof(f6597, plain, ((e21 = op2(e22, e21)) | (~ spl28_222 | ~ spl28_243)), inference(backward_demodulation, [], [f6139, f1717])).
fof(f1717, plain, ((e21 = op2(e21, e20)) | ~ spl28_222), inference(avatar_component_clause, [], [f1715])).
fof(f6601, plain, (spl28_211 | ~ spl28_222 | ~ spl28_243), inference(avatar_split_clause, [], [f6595, f1803, f1715, f1669])).
fof(f6595, plain, ((e20 = op2(e21, e22)) | (~ spl28_222 | ~ spl28_243)), inference(backward_demodulation, [], [f5636, f1717])).
fof(f5636, plain, ((e20 = op2(op2(e21, e20), e22)) | ~ spl28_243), inference(forward_demodulation, [], [f259, f1805])).
fof(f259, plain, (e20 = op2(op2(e21, e20), op2(e20, e21))), inference(cnf_transformation, [], [f6])).
fof(f6574, plain, (~ spl28_247 | ~ spl28_249), inference(avatar_contradiction_clause, [], [f6573])).
fof(f6573, plain, ($false | (~ spl28_247 | ~ spl28_249)), inference(subsumption_resolution, [], [f6572, f498])).
fof(f498, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f10])).
fof(f6572, plain, ((e21 = e23) | (~ spl28_247 | ~ spl28_249)), inference(backward_demodulation, [], [f1830, f1822])).
fof(f1822, plain, ((op2(e20, e20) = e21) | ~ spl28_247), inference(avatar_component_clause, [], [f1820])).
fof(f1820, plain, (spl28_247 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl28_247])])).
fof(f1830, plain, ((op2(e20, e20) = e23) | ~ spl28_249), inference(avatar_component_clause, [], [f1828])).
fof(f1828, plain, (spl28_249 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl28_249])])).
fof(f6569, plain, (spl28_191 | ~ spl28_260 | ~ spl28_275 | ~ spl28_284 | ~ spl28_327), inference(avatar_contradiction_clause, [], [f6568])).
fof(f6568, plain, ($false | (spl28_191 | ~ spl28_260 | ~ spl28_275 | ~ spl28_284 | ~ spl28_327)), inference(subsumption_resolution, [], [f6553, f1586])).
fof(f1586, plain, (~ (e20 = op2(e22, e21)) | spl28_191), inference(avatar_component_clause, [], [f1585])).
fof(f1585, plain, (spl28_191 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_191])])).
fof(f6553, plain, ((e20 = op2(e22, e21)) | (~ spl28_260 | ~ spl28_275 | ~ spl28_284 | ~ spl28_327)), inference(backward_demodulation, [], [f5967, f2146])).
fof(f2146, plain, ((e22 = h4(e10)) | ~ spl28_275), inference(avatar_component_clause, [], [f2145])).
fof(f2145, plain, (spl28_275 <=> (e22 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_275])])).
fof(f5967, plain, ((e20 = op2(h4(e10), e21)) | (~ spl28_260 | ~ spl28_284 | ~ spl28_327)), inference(backward_demodulation, [], [f5891, f2411])).
fof(f5891, plain, ((e20 = op2(h4(e10), h1(e10))) | (~ spl28_260 | ~ spl28_284)), inference(forward_demodulation, [], [f4359, f5664])).
fof(f4359, plain, ((e20 = op2(op2(e23, e20), h1(e10))) | ~ spl28_260), inference(backward_demodulation, [], [f261, f4349])).
fof(f261, plain, (e20 = op2(op2(e23, e20), op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f6534, plain, (~ spl28_156 | ~ spl28_289), inference(avatar_contradiction_clause, [], [f6533])).
fof(f6533, plain, ($false | (~ spl28_156 | ~ spl28_289)), inference(subsumption_resolution, [], [f6532, f494])).
fof(f494, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f10])).
fof(f6532, plain, ((e20 = e22) | (~ spl28_156 | ~ spl28_289)), inference(forward_demodulation, [], [f6520, f1440])).
fof(f6520, plain, ((e22 = op2(e23, e23)) | ~ spl28_289), inference(backward_demodulation, [], [f2004, f2216])).
fof(f2216, plain, ((e23 = h3(e11)) | ~ spl28_289), inference(avatar_component_clause, [], [f2215])).
fof(f2215, plain, (spl28_289 <=> (e23 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_289])])).
fof(f2004, plain, (e22 = op2(h3(e11), h3(e11))), inference(backward_demodulation, [], [f270, f598])).
fof(f598, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ((h3(e13) = op2(op2(e22, op2(e22, e22)), op2(e22, op2(e22, e22)))) & (h3(e12) = op2(op2(e22, op2(e22, e22)), op2(e22, e22))) & (op2(e22, e22) = h3(e11)) & (h3(e10) = op2(e22, op2(e22, e22))) & (e22 = h3(e14))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax18)).
fof(f270, plain, (e22 = op2(op2(e22, e22), op2(e22, e22))), inference(cnf_transformation, [], [f6])).
fof(f6518, plain, (~ spl28_327 | ~ spl28_469), inference(avatar_contradiction_clause, [], [f6517])).
fof(f6517, plain, ($false | (~ spl28_327 | ~ spl28_469)), inference(subsumption_resolution, [], [f6516, f499])).
fof(f499, plain, ~ (e21 = e24), inference(cnf_transformation, [], [f10])).
fof(f6516, plain, ((e21 = e24) | (~ spl28_327 | ~ spl28_469)), inference(forward_demodulation, [], [f3383, f2411])).
fof(f3383, plain, ((e24 = h1(e10)) | ~ spl28_469), inference(avatar_component_clause, [], [f3382])).
fof(f3382, plain, (spl28_469 <=> (e24 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_469])])).
fof(f6501, plain, (~ spl28_202 | ~ spl28_267), inference(avatar_split_clause, [], [f5933, f2099, f1631])).
fof(f1631, plain, (spl28_202 <=> (e21 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_202])])).
fof(f2099, plain, (spl28_267 <=> (e21 = h5(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_267])])).
fof(f5933, plain, (~ (e21 = op2(e21, e24)) | ~ spl28_267), inference(forward_demodulation, [], [f2038, f2100])).
fof(f2100, plain, ((e21 = h5(e11)) | ~ spl28_267), inference(avatar_component_clause, [], [f2099])).
fof(f2038, plain, ~ (op2(e21, e24) = h5(e11)), inference(backward_demodulation, [], [f429, f608])).
fof(f608, plain, (op2(e24, e24) = h5(e11)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((op2(op2(e24, op2(e24, e24)), op2(e24, op2(e24, e24))) = h5(e13)) & (op2(op2(e24, op2(e24, e24)), op2(e24, e24)) = h5(e12)) & (op2(e24, e24) = h5(e11)) & (op2(e24, op2(e24, e24)) = h5(e10)) & (e24 = h5(e14))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax20)).
fof(f429, plain, ~ (op2(e21, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f6496, plain, (~ spl28_411 | ~ spl28_240), inference(avatar_split_clause, [], [f6495, f1790, f3002])).
fof(f3002, plain, (spl28_411 <=> (e24 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_411])])).
fof(f1790, plain, (spl28_240 <=> (e24 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_240])])).
fof(f6495, plain, (~ (e24 = h3(e11)) | ~ spl28_240), inference(forward_demodulation, [], [f2005, f1792])).
fof(f1792, plain, ((e24 = op2(e20, e22)) | ~ spl28_240), inference(avatar_component_clause, [], [f1790])).
fof(f2005, plain, ~ (op2(e20, e22) = h3(e11)), inference(backward_demodulation, [], [f404, f598])).
fof(f404, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f6438, plain, (~ spl28_25 | ~ spl28_101), inference(avatar_contradiction_clause, [], [f6437])).
fof(f6437, plain, ($false | (~ spl28_25 | ~ spl28_101)), inference(subsumption_resolution, [], [f6436, f486])).
fof(f486, plain, ~ (e10 = e14), inference(cnf_transformation, [], [f9])).
fof(f6436, plain, ((e10 = e14) | (~ spl28_25 | ~ spl28_101)), inference(forward_demodulation, [], [f6432, f1159])).
fof(f1159, plain, ((e10 = op1(e10, e14)) | ~ spl28_101), inference(avatar_component_clause, [], [f1157])).
fof(f6432, plain, ((e14 = op1(e10, e14)) | (~ spl28_25 | ~ spl28_101)), inference(backward_demodulation, [], [f6304, f839])).
fof(f839, plain, ((e14 = op1(e14, e10)) | ~ spl28_25), inference(avatar_component_clause, [], [f837])).
fof(f6304, plain, ((e14 = op1(e10, op1(e14, e10))) | ~ spl28_101), inference(backward_demodulation, [], [f178, f1159])).
fof(f6423, plain, (~ spl28_31 | ~ spl28_35), inference(avatar_contradiction_clause, [], [f6422])).
fof(f6422, plain, ($false | (~ spl28_31 | ~ spl28_35)), inference(subsumption_resolution, [], [f6421, f486])).
fof(f6421, plain, ((e10 = e14) | (~ spl28_31 | ~ spl28_35)), inference(forward_demodulation, [], [f881, f865])).
fof(f881, plain, ((e14 = op1(e13, e13)) | ~ spl28_35), inference(avatar_component_clause, [], [f879])).
fof(f879, plain, (spl28_35 <=> (e14 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_35])])).
fof(f6420, plain, (spl28_14 | ~ spl28_38 | ~ spl28_60), inference(avatar_split_clause, [], [f6418, f984, f892, f791])).
fof(f791, plain, (spl28_14 <=> (e13 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_14])])).
fof(f892, plain, (spl28_38 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_38])])).
fof(f6418, plain, ((e13 = op1(e14, e12)) | (~ spl28_38 | ~ spl28_60)), inference(backward_demodulation, [], [f6394, f894])).
fof(f894, plain, ((e12 = op1(e13, e12)) | ~ spl28_38), inference(avatar_component_clause, [], [f892])).
fof(f6394, plain, ((e13 = op1(e14, op1(e13, e12))) | ~ spl28_60), inference(backward_demodulation, [], [f175, f986])).
fof(f175, plain, (e13 = op1(op1(e12, e13), op1(e13, e12))), inference(cnf_transformation, [], [f3])).
fof(f6351, plain, (~ spl28_42 | ~ spl28_84), inference(avatar_contradiction_clause, [], [f6350])).
fof(f6350, plain, ($false | (~ spl28_42 | ~ spl28_84)), inference(subsumption_resolution, [], [f6349, f488])).
fof(f488, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f9])).
fof(f6349, plain, ((e11 = e13) | (~ spl28_42 | ~ spl28_84)), inference(forward_demodulation, [], [f6345, f911])).
fof(f6345, plain, ((e13 = op1(e13, e11)) | (~ spl28_42 | ~ spl28_84)), inference(backward_demodulation, [], [f6116, f1087])).
fof(f1087, plain, ((e13 = op1(e11, e13)) | ~ spl28_84), inference(avatar_component_clause, [], [f1085])).
fof(f1085, plain, (spl28_84 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_84])])).
fof(f6116, plain, ((e13 = op1(op1(e11, e13), e11)) | ~ spl28_42), inference(forward_demodulation, [], [f174, f911])).
fof(f174, plain, (e13 = op1(op1(e11, e13), op1(e13, e11))), inference(cnf_transformation, [], [f3])).
fof(f6348, plain, (~ spl28_79 | ~ spl28_84), inference(avatar_split_clause, [], [f6343, f1085, f1064])).
fof(f6343, plain, (~ (e13 = op1(e11, e14)) | ~ spl28_84), inference(backward_demodulation, [], [f352, f1087])).
fof(f352, plain, ~ (op1(e11, e13) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f6324, plain, (spl28_62 | ~ spl28_98 | ~ spl28_118), inference(avatar_split_clause, [], [f6320, f1228, f1144, f993])).
fof(f1144, plain, (spl28_98 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_98])])).
fof(f6320, plain, ((e11 = op1(e12, e12)) | (~ spl28_98 | ~ spl28_118)), inference(backward_demodulation, [], [f5180, f1146])).
fof(f1146, plain, ((e12 = op1(e11, e10)) | ~ spl28_98), inference(avatar_component_clause, [], [f1144])).
fof(f6322, plain, (~ spl28_23 | ~ spl28_98), inference(avatar_split_clause, [], [f6314, f1144, f829])).
fof(f6314, plain, (~ (e12 = op1(e14, e10)) | ~ spl28_98), inference(backward_demodulation, [], [f289, f1146])).
fof(f289, plain, ~ (op1(e11, e10) = op1(e14, e10)), inference(cnf_transformation, [], [f7])).
fof(f6311, plain, (~ spl28_51 | ~ spl28_101), inference(avatar_split_clause, [], [f6306, f1157, f947])).
fof(f6306, plain, (~ (e10 = op1(e12, e14)) | ~ spl28_101), inference(backward_demodulation, [], [f324, f1159])).
fof(f6276, plain, (~ spl28_134 | ~ spl28_135), inference(avatar_contradiction_clause, [], [f6275])).
fof(f6275, plain, ($false | (~ spl28_134 | ~ spl28_135)), inference(subsumption_resolution, [], [f6274, f502])).
fof(f502, plain, ~ (e23 = e24), inference(cnf_transformation, [], [f10])).
fof(f6274, plain, ((e23 = e24) | (~ spl28_134 | ~ spl28_135)), inference(forward_demodulation, [], [f1351, f1347])).
fof(f1351, plain, ((e24 = op2(e24, e23)) | ~ spl28_135), inference(avatar_component_clause, [], [f1349])).
fof(f1349, plain, (spl28_135 <=> (e24 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_135])])).
fof(f6239, plain, (~ spl28_156 | ~ spl28_160), inference(avatar_contradiction_clause, [], [f6238])).
fof(f6238, plain, ($false | (~ spl28_156 | ~ spl28_160)), inference(subsumption_resolution, [], [f6237, f496])).
fof(f496, plain, ~ (e20 = e24), inference(cnf_transformation, [], [f10])).
fof(f6237, plain, ((e20 = e24) | (~ spl28_156 | ~ spl28_160)), inference(forward_demodulation, [], [f1456, f1440])).
fof(f1456, plain, ((e24 = op2(e23, e23)) | ~ spl28_160), inference(avatar_component_clause, [], [f1454])).
fof(f1454, plain, (spl28_160 <=> (e24 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_160])])).
fof(f6236, plain, (~ spl28_172 | spl28_280 | ~ spl28_284), inference(avatar_contradiction_clause, [], [f6235])).
fof(f6235, plain, ($false | (~ spl28_172 | spl28_280 | ~ spl28_284)), inference(subsumption_resolution, [], [f6233, f2172])).
fof(f2172, plain, (~ (e21 = h4(e10)) | spl28_280), inference(avatar_component_clause, [], [f2170])).
fof(f2170, plain, (spl28_280 <=> (e21 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_280])])).
fof(f6233, plain, ((e21 = h4(e10)) | (~ spl28_172 | ~ spl28_284)), inference(backward_demodulation, [], [f5664, f1507])).
fof(f1507, plain, ((e21 = op2(e23, e20)) | ~ spl28_172), inference(avatar_component_clause, [], [f1505])).
fof(f1505, plain, (spl28_172 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_172])])).
fof(f6173, plain, (spl28_136 | ~ spl28_225 | ~ spl28_243), inference(avatar_split_clause, [], [f6167, f1803, f1727, f1354])).
fof(f1354, plain, (spl28_136 <=> (e20 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_136])])).
fof(f1727, plain, (spl28_225 <=> (e24 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_225])])).
fof(f6167, plain, ((e20 = op2(e24, e22)) | (~ spl28_225 | ~ spl28_243)), inference(backward_demodulation, [], [f5636, f1729])).
fof(f1729, plain, ((e24 = op2(e21, e20)) | ~ spl28_225), inference(avatar_component_clause, [], [f1727])).
fof(f6164, plain, (~ spl28_155 | ~ spl28_230), inference(avatar_split_clause, [], [f6162, f1748, f1433])).
fof(f1433, plain, (spl28_155 <=> (e24 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_155])])).
fof(f6162, plain, (~ (e24 = op2(e23, e24)) | ~ spl28_230), inference(backward_demodulation, [], [f425, f1750])).
fof(f425, plain, ~ (op2(e20, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f6159, plain, (~ spl28_246 | ~ spl28_249), inference(avatar_contradiction_clause, [], [f6158])).
fof(f6158, plain, ($false | (~ spl28_246 | ~ spl28_249)), inference(subsumption_resolution, [], [f6157, f495])).
fof(f6157, plain, ((e20 = e23) | (~ spl28_246 | ~ spl28_249)), inference(backward_demodulation, [], [f1830, f1818])).
fof(f1818, plain, ((e20 = op2(e20, e20)) | ~ spl28_246), inference(avatar_component_clause, [], [f1816])).
fof(f1816, plain, (spl28_246 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_246])])).
fof(f6155, plain, (spl28_455 | ~ spl28_220 | ~ spl28_327), inference(avatar_split_clause, [], [f5969, f2410, f1706, f3278])).
fof(f1706, plain, (spl28_220 <=> (e24 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_220])])).
fof(f5969, plain, ((e24 = h1(e13)) | (~ spl28_220 | ~ spl28_327)), inference(forward_demodulation, [], [f5963, f1708])).
fof(f1708, plain, ((e24 = op2(e21, e21)) | ~ spl28_220), inference(avatar_component_clause, [], [f1706])).
fof(f5963, plain, ((op2(e21, e21) = h1(e13)) | ~ spl28_327), inference(backward_demodulation, [], [f1988, f2411])).
fof(f1988, plain, (h1(e13) = op2(h1(e10), h1(e10))), inference(backward_demodulation, [], [f1986, f1987])).
fof(f1986, plain, (h1(e13) = op2(op2(e20, h1(e11)), op2(e20, h1(e11)))), inference(backward_demodulation, [], [f590, f588])).
fof(f590, plain, (h1(e13) = op2(op2(e20, op2(e20, e20)), op2(e20, op2(e20, e20)))), inference(cnf_transformation, [], [f16])).
fof(f6148, plain, (~ spl28_141 | ~ spl28_197 | ~ spl28_240), inference(avatar_contradiction_clause, [], [f6147])).
fof(f6147, plain, ($false | (~ spl28_141 | ~ spl28_197 | ~ spl28_240)), inference(subsumption_resolution, [], [f6146, f494])).
fof(f6146, plain, ((e20 = e22) | (~ spl28_141 | ~ spl28_197 | ~ spl28_240)), inference(forward_demodulation, [], [f6145, f1377])).
fof(f6145, plain, ((e22 = op2(e24, e21)) | (~ spl28_197 | ~ spl28_240)), inference(forward_demodulation, [], [f5300, f1792])).
fof(f5300, plain, ((e22 = op2(op2(e20, e22), e21)) | ~ spl28_197), inference(forward_demodulation, [], [f268, f1612])).
fof(f268, plain, (e22 = op2(op2(e20, e22), op2(e22, e20))), inference(cnf_transformation, [], [f6])).
fof(f6131, plain, (~ spl28_195 | ~ spl28_440), inference(avatar_split_clause, [], [f5916, f3184, f1601])).
fof(f1601, plain, (spl28_195 <=> (e24 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_195])])).
fof(f3184, plain, (spl28_440 <=> (e24 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_440])])).
fof(f5916, plain, (~ (e24 = op2(e22, e21)) | ~ spl28_440), inference(forward_demodulation, [], [f1992, f3185])).
fof(f3185, plain, ((e24 = h2(e11)) | ~ spl28_440), inference(avatar_component_clause, [], [f3184])).
fof(f1992, plain, ~ (op2(e22, e21) = h2(e11)), inference(backward_demodulation, [], [f397, f593])).
fof(f593, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((h2(e13) = op2(op2(e21, op2(e21, e21)), op2(e21, op2(e21, e21)))) & (h2(e12) = op2(op2(e21, op2(e21, e21)), op2(e21, e21))) & (op2(e21, e21) = h2(e11)) & (h2(e10) = op2(e21, op2(e21, e21))) & (e21 = h2(e14))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax17)).
fof(f397, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f6123, plain, (~ spl28_136 | ~ spl28_141), inference(avatar_split_clause, [], [f5204, f1375, f1354])).
fof(f5204, plain, (~ (e20 = op2(e24, e22)) | ~ spl28_141), inference(forward_demodulation, [], [f477, f1377])).
fof(f477, plain, ~ (op2(e24, e21) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f6115, plain, (~ spl28_80 | ~ spl28_95), inference(avatar_split_clause, [], [f4995, f1131, f1068])).
fof(f1068, plain, (spl28_80 <=> (e14 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_80])])).
fof(f4995, plain, (~ (e14 = op1(e11, e14)) | ~ spl28_95), inference(backward_demodulation, [], [f349, f1133])).
fof(f349, plain, ~ (op1(e11, e11) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f6113, plain, (~ spl28_77 | ~ spl28_2), inference(avatar_split_clause, [], [f4166, f741, f1056])).
fof(f1056, plain, (spl28_77 <=> (e11 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_77])])).
fof(f4166, plain, (~ (e11 = op1(e11, e14)) | ~ spl28_2), inference(forward_demodulation, [], [f329, f743])).
fof(f329, plain, ~ (op1(e11, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f6112, plain, (~ spl28_53 | ~ spl28_28), inference(avatar_split_clause, [], [f6111, f850, f955])).
fof(f6111, plain, (~ (e12 = op1(e12, e14)) | ~ spl28_28), inference(forward_demodulation, [], [f330, f852])).
fof(f330, plain, ~ (op1(e12, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f6110, plain, (~ spl28_47 | ~ spl28_42), inference(avatar_split_clause, [], [f6109, f909, f930])).
fof(f6109, plain, (~ (e11 = op1(e13, e10)) | ~ spl28_42), inference(forward_demodulation, [], [f363, f911])).
fof(f363, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f6104, plain, (~ spl28_9 | ~ spl28_10), inference(avatar_contradiction_clause, [], [f6103])).
fof(f6103, plain, ($false | (~ spl28_9 | ~ spl28_10)), inference(subsumption_resolution, [], [f6102, f492])).
fof(f492, plain, ~ (e13 = e14), inference(cnf_transformation, [], [f9])).
fof(f6102, plain, ((e13 = e14) | (~ spl28_9 | ~ spl28_10)), inference(forward_demodulation, [], [f776, f772])).
fof(f776, plain, ((e14 = op1(e14, e13)) | ~ spl28_10), inference(avatar_component_clause, [], [f774])).
fof(f774, plain, (spl28_10 <=> (e14 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_10])])).
fof(f6071, plain, (~ spl28_2 | ~ spl28_65), inference(avatar_contradiction_clause, [], [f6070])).
fof(f6070, plain, ($false | (~ spl28_2 | ~ spl28_65)), inference(subsumption_resolution, [], [f6069, f487])).
fof(f487, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f9])).
fof(f6069, plain, ((e11 = e12) | (~ spl28_2 | ~ spl28_65)), inference(forward_demodulation, [], [f6066, f743])).
fof(f6066, plain, ((e12 = op1(e14, e14)) | ~ spl28_65), inference(backward_demodulation, [], [f170, f1007])).
fof(f1007, plain, ((e14 = op1(e12, e12)) | ~ spl28_65), inference(avatar_component_clause, [], [f1005])).
fof(f1005, plain, (spl28_65 <=> (e14 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_65])])).
fof(f6055, plain, (~ spl28_127 | ~ spl28_129), inference(avatar_contradiction_clause, [], [f6054])).
fof(f6054, plain, ($false | (~ spl28_127 | ~ spl28_129)), inference(subsumption_resolution, [], [f6053, f498])).
fof(f6053, plain, ((e21 = e23) | (~ spl28_127 | ~ spl28_129)), inference(forward_demodulation, [], [f1326, f1318])).
fof(f1326, plain, ((e23 = op2(e24, e24)) | ~ spl28_129), inference(avatar_component_clause, [], [f1324])).
fof(f1324, plain, (spl28_129 <=> (e23 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_129])])).
fof(f6052, plain, (~ spl28_139 | ~ spl28_176 | ~ spl28_232), inference(avatar_contradiction_clause, [], [f6051])).
fof(f6051, plain, ($false | (~ spl28_139 | ~ spl28_176 | ~ spl28_232)), inference(subsumption_resolution, [], [f6050, f499])).
fof(f6050, plain, ((e21 = e24) | (~ spl28_139 | ~ spl28_176 | ~ spl28_232)), inference(forward_demodulation, [], [f6049, f1759])).
fof(f1759, plain, ((e21 = op2(e20, e23)) | ~ spl28_232), inference(avatar_component_clause, [], [f1757])).
fof(f1757, plain, (spl28_232 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_232])])).
fof(f6049, plain, ((e24 = op2(e20, e23)) | (~ spl28_139 | ~ spl28_176)), inference(backward_demodulation, [], [f4888, f1368])).
fof(f4888, plain, ((e24 = op2(e20, op2(e24, e22))) | ~ spl28_176), inference(backward_demodulation, [], [f280, f1524])).
fof(f280, plain, (e24 = op2(op2(e22, e24), op2(e24, e22))), inference(cnf_transformation, [], [f6])).
fof(f6021, plain, (spl28_275 | ~ spl28_173 | ~ spl28_284), inference(avatar_split_clause, [], [f6018, f2190, f1509, f2145])).
fof(f1509, plain, (spl28_173 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_173])])).
fof(f6018, plain, ((e22 = h4(e10)) | (~ spl28_173 | ~ spl28_284)), inference(backward_demodulation, [], [f5664, f1511])).
fof(f1511, plain, ((e22 = op2(e23, e20)) | ~ spl28_173), inference(avatar_component_clause, [], [f1509])).
fof(f5995, plain, (spl28_322 | ~ spl28_208 | ~ spl28_260 | ~ spl28_327), inference(avatar_split_clause, [], [f5994, f2410, f2056, f1656, f2383])).
fof(f1656, plain, (spl28_208 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_208])])).
fof(f5994, plain, ((e22 = h1(e12)) | (~ spl28_208 | ~ spl28_260 | ~ spl28_327)), inference(backward_demodulation, [], [f5965, f1658])).
fof(f1658, plain, ((e22 = op2(e21, e23)) | ~ spl28_208), inference(avatar_component_clause, [], [f1656])).
fof(f5991, plain, (~ spl28_212 | ~ spl28_214), inference(avatar_contradiction_clause, [], [f5990])).
fof(f5990, plain, ($false | (~ spl28_212 | ~ spl28_214)), inference(subsumption_resolution, [], [f5989, f498])).
fof(f5989, plain, ((e21 = e23) | (~ spl28_212 | ~ spl28_214)), inference(forward_demodulation, [], [f1683, f1675])).
fof(f1683, plain, ((e23 = op2(e21, e22)) | ~ spl28_214), inference(avatar_component_clause, [], [f1681])).
fof(f1681, plain, (spl28_214 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_214])])).
fof(f5962, plain, (spl28_327 | ~ spl28_232 | ~ spl28_260), inference(avatar_split_clause, [], [f5679, f2056, f1757, f2410])).
fof(f5679, plain, ((e21 = h1(e10)) | (~ spl28_232 | ~ spl28_260)), inference(backward_demodulation, [], [f4349, f1759])).
fof(f5951, plain, (~ spl28_228 | ~ spl28_243), inference(avatar_split_clause, [], [f5950, f1803, f1740])).
fof(f1740, plain, (spl28_228 <=> (e22 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_228])])).
fof(f5950, plain, (~ (e22 = op2(e20, e24)) | ~ spl28_243), inference(forward_demodulation, [], [f439, f1805])).
fof(f439, plain, ~ (op2(e20, e21) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f5938, plain, (~ spl28_210 | ~ spl28_440), inference(avatar_split_clause, [], [f5937, f3184, f1664])).
fof(f1664, plain, (spl28_210 <=> (e24 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_210])])).
fof(f5937, plain, (~ (e24 = op2(e21, e23)) | ~ spl28_440), inference(forward_demodulation, [], [f1997, f3185])).
fof(f1997, plain, ~ (op2(e21, e23) = h2(e11)), inference(backward_demodulation, [], [f448, f593])).
fof(f448, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f5913, plain, (spl28_208 | ~ spl28_194 | ~ spl28_212), inference(avatar_split_clause, [], [f5912, f1673, f1597, f1656])).
fof(f5912, plain, ((e22 = op2(e21, e23)) | (~ spl28_194 | ~ spl28_212)), inference(forward_demodulation, [], [f5911, f1675])).
fof(f5911, plain, ((e22 = op2(op2(e21, e22), e23)) | ~ spl28_194), inference(forward_demodulation, [], [f269, f1599])).
fof(f269, plain, (e22 = op2(op2(e21, e22), op2(e22, e21))), inference(cnf_transformation, [], [f6])).
fof(f5887, plain, (~ spl28_280 | ~ spl28_197 | ~ spl28_284), inference(avatar_split_clause, [], [f5886, f2190, f1610, f2170])).
fof(f5886, plain, (~ (e21 = h4(e10)) | (~ spl28_197 | ~ spl28_284)), inference(forward_demodulation, [], [f5885, f1612])).
fof(f5885, plain, (~ (op2(e22, e20) = h4(e10)) | ~ spl28_284), inference(forward_demodulation, [], [f390, f5664])).
fof(f390, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f5872, plain, (~ spl28_2 | ~ spl28_50 | ~ spl28_110), inference(avatar_contradiction_clause, [], [f5871])).
fof(f5871, plain, ($false | (~ spl28_2 | ~ spl28_50 | ~ spl28_110)), inference(subsumption_resolution, [], [f5870, f488])).
fof(f5870, plain, ((e11 = e13) | (~ spl28_2 | ~ spl28_50 | ~ spl28_110)), inference(forward_demodulation, [], [f5866, f743])).
fof(f5866, plain, ((e13 = op1(e14, e14)) | (~ spl28_50 | ~ spl28_110)), inference(backward_demodulation, [], [f5860, f1196])).
fof(f1196, plain, ((e14 = op1(e10, e13)) | ~ spl28_110), inference(avatar_component_clause, [], [f1194])).
fof(f1194, plain, (spl28_110 <=> (e14 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_110])])).
fof(f5860, plain, ((e13 = op1(op1(e10, e13), e14)) | ~ spl28_50), inference(forward_demodulation, [], [f173, f944])).
fof(f173, plain, (e13 = op1(op1(e10, e13), op1(e13, e10))), inference(cnf_transformation, [], [f3])).
fof(f5868, plain, (~ spl28_105 | ~ spl28_110), inference(avatar_split_clause, [], [f5864, f1194, f1173])).
fof(f5864, plain, (~ (e14 = op1(e10, e14)) | ~ spl28_110), inference(backward_demodulation, [], [f342, f1196])).
fof(f342, plain, ~ (op1(e10, e13) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f5855, plain, (~ spl28_102 | ~ spl28_2), inference(avatar_split_clause, [], [f5177, f741, f1161])).
fof(f1161, plain, (spl28_102 <=> (e11 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_102])])).
fof(f5177, plain, (~ (e11 = op1(e10, e14)) | ~ spl28_2), inference(forward_demodulation, [], [f326, f743])).
fof(f326, plain, ~ (op1(e10, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f5783, plain, (~ spl28_30 | ~ spl28_50), inference(avatar_split_clause, [], [f5778, f942, f858])).
fof(f858, plain, (spl28_30 <=> (e14 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_30])])).
fof(f5778, plain, (~ (e14 = op1(e13, e14)) | ~ spl28_50), inference(backward_demodulation, [], [f366, f944])).
fof(f366, plain, ~ (op1(e13, e10) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f5755, plain, (~ spl28_30 | ~ spl28_105), inference(avatar_split_clause, [], [f5754, f1173, f858])).
fof(f5754, plain, (~ (e14 = op1(e13, e14)) | ~ spl28_105), inference(backward_demodulation, [], [f325, f1175])).
fof(f325, plain, ~ (op1(e10, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f5687, plain, (~ spl28_221 | ~ spl28_223), inference(avatar_contradiction_clause, [], [f5686])).
fof(f5686, plain, ($false | (~ spl28_221 | ~ spl28_223)), inference(subsumption_resolution, [], [f5685, f494])).
fof(f5685, plain, ((e20 = e22) | (~ spl28_221 | ~ spl28_223)), inference(backward_demodulation, [], [f1721, f1713])).
fof(f1721, plain, ((e22 = op2(e21, e20)) | ~ spl28_223), inference(avatar_component_clause, [], [f1719])).
fof(f1719, plain, (spl28_223 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_223])])).
fof(f5672, plain, (spl28_383 | ~ spl28_175 | ~ spl28_284), inference(avatar_split_clause, [], [f5671, f2190, f1517, f2840])).
fof(f1517, plain, (spl28_175 <=> (e24 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_175])])).
fof(f5671, plain, ((e24 = h4(e10)) | (~ spl28_175 | ~ spl28_284)), inference(forward_demodulation, [], [f5664, f1519])).
fof(f1519, plain, ((e24 = op2(e23, e20)) | ~ spl28_175), inference(avatar_component_clause, [], [f1517])).
fof(f5652, plain, (spl28_440 | ~ spl28_220), inference(avatar_split_clause, [], [f5276, f1706, f3184])).
fof(f5276, plain, ((e24 = h2(e11)) | ~ spl28_220), inference(forward_demodulation, [], [f593, f1708])).
fof(f5651, plain, (spl28_284 | ~ spl28_156), inference(avatar_split_clause, [], [f5232, f1438, f2190])).
fof(f5232, plain, ((e20 = h4(e11)) | ~ spl28_156), inference(forward_demodulation, [], [f603, f1440])).
fof(f5642, plain, (~ spl28_193 | ~ spl28_243), inference(avatar_contradiction_clause, [], [f5641])).
fof(f5641, plain, ($false | (~ spl28_193 | ~ spl28_243)), inference(subsumption_resolution, [], [f5640, f1805])).
fof(f5640, plain, (~ (e22 = op2(e20, e21)) | ~ spl28_193), inference(forward_demodulation, [], [f394, f1595])).
fof(f1595, plain, ((e22 = op2(e22, e21)) | ~ spl28_193), inference(avatar_component_clause, [], [f1593])).
fof(f1593, plain, (spl28_193 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_193])])).
fof(f394, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f5639, plain, (spl28_186 | ~ spl28_223 | ~ spl28_243), inference(avatar_contradiction_clause, [], [f5638])).
fof(f5638, plain, ($false | (spl28_186 | ~ spl28_223 | ~ spl28_243)), inference(subsumption_resolution, [], [f5637, f1565])).
fof(f1565, plain, (~ (e20 = op2(e22, e22)) | spl28_186), inference(avatar_component_clause, [], [f1564])).
fof(f1564, plain, (spl28_186 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_186])])).
fof(f5637, plain, ((e20 = op2(e22, e22)) | (~ spl28_223 | ~ spl28_243)), inference(forward_demodulation, [], [f5636, f1721])).
fof(f5631, plain, (~ spl28_238 | ~ spl28_243), inference(avatar_contradiction_clause, [], [f5630])).
fof(f5630, plain, ($false | (~ spl28_238 | ~ spl28_243)), inference(subsumption_resolution, [], [f5629, f1805])).
fof(f5629, plain, (~ (e22 = op2(e20, e21)) | ~ spl28_238), inference(forward_demodulation, [], [f437, f1784])).
fof(f1784, plain, ((e22 = op2(e20, e22)) | ~ spl28_238), inference(avatar_component_clause, [], [f1782])).
fof(f1782, plain, (spl28_238 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_238])])).
fof(f437, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f8])).
fof(f5601, plain, (spl28_289 | ~ spl28_189), inference(avatar_split_clause, [], [f5600, f1576, f2215])).
fof(f1576, plain, (spl28_189 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_189])])).
fof(f5600, plain, ((e23 = h3(e11)) | ~ spl28_189), inference(forward_demodulation, [], [f598, f1578])).
fof(f1578, plain, ((e23 = op2(e22, e22)) | ~ spl28_189), inference(avatar_component_clause, [], [f1576])).
fof(f5581, plain, (~ spl28_146 | ~ spl28_141), inference(avatar_split_clause, [], [f5208, f1375, f1396])).
fof(f1396, plain, (spl28_146 <=> (e20 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_146])])).
fof(f5208, plain, (~ (e20 = op2(e24, e20)) | ~ spl28_141), inference(forward_demodulation, [], [f473, f1377])).
fof(f473, plain, ~ (op2(e24, e20) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f5576, plain, (~ spl28_149 | ~ spl28_260), inference(avatar_split_clause, [], [f5218, f2056, f1408])).
fof(f1408, plain, (spl28_149 <=> (e23 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_149])])).
fof(f5218, plain, (~ (e23 = op2(e24, e20)) | ~ spl28_260), inference(forward_demodulation, [], [f1980, f2057])).
fof(f1980, plain, ~ (op2(e24, e20) = h1(e11)), inference(backward_demodulation, [], [f386, f588])).
fof(f386, plain, ~ (op2(e20, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f5575, plain, (~ spl28_147 | ~ spl28_267), inference(avatar_split_clause, [], [f5220, f2099, f1400])).
fof(f1400, plain, (spl28_147 <=> (e21 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_147])])).
fof(f5220, plain, (~ (e21 = op2(e24, e20)) | ~ spl28_267), inference(forward_demodulation, [], [f2041, f2100])).
fof(f2041, plain, ~ (op2(e24, e20) = h5(e11)), inference(backward_demodulation, [], [f476, f608])).
fof(f476, plain, ~ (op2(e24, e20) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5550, plain, (~ spl28_85 | ~ spl28_95), inference(avatar_split_clause, [], [f5549, f1131, f1089])).
fof(f1089, plain, (spl28_85 <=> (e14 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_85])])).
fof(f5549, plain, (~ (e14 = op1(e11, e13)) | ~ spl28_95), inference(forward_demodulation, [], [f348, f1133])).
fof(f348, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f7])).
fof(f5537, plain, (~ spl28_43 | ~ spl28_118), inference(avatar_split_clause, [], [f5536, f1228, f913])).
fof(f913, plain, (spl28_43 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_43])])).
fof(f5536, plain, (~ (e12 = op1(e13, e11)) | ~ spl28_118), inference(forward_demodulation, [], [f295, f1230])).
fof(f295, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f5515, plain, (~ spl28_14 | ~ spl28_51 | ~ spl28_107), inference(avatar_contradiction_clause, [], [f5514])).
fof(f5514, plain, ($false | (~ spl28_14 | ~ spl28_51 | ~ spl28_107)), inference(subsumption_resolution, [], [f5513, f489])).
fof(f489, plain, ~ (e11 = e14), inference(cnf_transformation, [], [f9])).
fof(f5513, plain, ((e11 = e14) | (~ spl28_14 | ~ spl28_51 | ~ spl28_107)), inference(forward_demodulation, [], [f5509, f1184])).
fof(f5509, plain, ((e14 = op1(e10, e13)) | (~ spl28_14 | ~ spl28_51)), inference(backward_demodulation, [], [f5468, f793])).
fof(f793, plain, ((e13 = op1(e14, e12)) | ~ spl28_14), inference(avatar_component_clause, [], [f791])).
fof(f5468, plain, ((e14 = op1(e10, op1(e14, e12))) | ~ spl28_51), inference(backward_demodulation, [], [f180, f949])).
fof(f180, plain, (e14 = op1(op1(e12, e14), op1(e14, e12))), inference(cnf_transformation, [], [f3])).
fof(f5451, plain, (~ spl28_54 | ~ spl28_69), inference(avatar_split_clause, [], [f5446, f1022, f959])).
fof(f5446, plain, (~ (e13 = op1(e12, e14)) | ~ spl28_69), inference(backward_demodulation, [], [f359, f1024])).
fof(f359, plain, ~ (op1(e12, e11) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f5440, plain, (~ spl28_455 | ~ spl28_79 | ~ spl28_175 | ~ spl28_260 | spl28_463), inference(avatar_split_clause, [], [f5436, f3310, f2056, f1517, f1064, f3278])).
fof(f3310, plain, (spl28_463 <=> (h1(op1(e11, e14)) = op2(h1(e11), e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_463])])).
fof(f5436, plain, (~ (e24 = h1(e13)) | (~ spl28_79 | ~ spl28_175 | ~ spl28_260 | spl28_463)), inference(backward_demodulation, [], [f5306, f1066])).
fof(f5306, plain, (~ (e24 = h1(op1(e11, e14))) | (~ spl28_175 | ~ spl28_260 | spl28_463)), inference(forward_demodulation, [], [f5305, f1519])).
fof(f5305, plain, (~ (op2(e23, e20) = h1(op1(e11, e14))) | (~ spl28_260 | spl28_463)), inference(forward_demodulation, [], [f3312, f2057])).
fof(f3312, plain, (~ (h1(op1(e11, e14)) = op2(h1(e11), e20)) | spl28_463), inference(avatar_component_clause, [], [f3310])).
fof(f5425, plain, (~ spl28_76 | ~ spl28_96), inference(avatar_split_clause, [], [f5419, f1136, f1052])).
fof(f1052, plain, (spl28_76 <=> (e10 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_76])])).
fof(f5419, plain, (~ (e10 = op1(e11, e14)) | ~ spl28_96), inference(backward_demodulation, [], [f346, f1138])).
fof(f346, plain, ~ (op1(e11, e10) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f5400, plain, (~ spl28_127 | ~ spl28_128), inference(avatar_contradiction_clause, [], [f5399])).
fof(f5399, plain, ($false | (~ spl28_127 | ~ spl28_128)), inference(subsumption_resolution, [], [f5398, f497])).
fof(f5398, plain, ((e21 = e22) | (~ spl28_127 | ~ spl28_128)), inference(forward_demodulation, [], [f1322, f1318])).
fof(f1322, plain, ((e22 = op2(e24, e24)) | ~ spl28_128), inference(avatar_component_clause, [], [f1320])).
fof(f1320, plain, (spl28_128 <=> (e22 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_128])])).
fof(f5394, plain, (~ spl28_138 | ~ spl28_176 | ~ spl28_197), inference(avatar_contradiction_clause, [], [f5393])).
fof(f5393, plain, ($false | (~ spl28_138 | ~ spl28_176 | ~ spl28_197)), inference(subsumption_resolution, [], [f5392, f497])).
fof(f5392, plain, ((e21 = e22) | (~ spl28_138 | ~ spl28_176 | ~ spl28_197)), inference(forward_demodulation, [], [f5390, f1612])).
fof(f5390, plain, ((e22 = op2(e22, e20)) | (~ spl28_138 | ~ spl28_176)), inference(backward_demodulation, [], [f4887, f1364])).
fof(f1364, plain, ((e22 = op2(e24, e22)) | ~ spl28_138), inference(avatar_component_clause, [], [f1362])).
fof(f1362, plain, (spl28_138 <=> (e22 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_138])])).
fof(f4887, plain, ((e22 = op2(op2(e24, e22), e20)) | ~ spl28_176), inference(backward_demodulation, [], [f272, f1524])).
fof(f5355, plain, (~ spl28_190 | spl28_411), inference(avatar_contradiction_clause, [], [f5354])).
fof(f5354, plain, ($false | (~ spl28_190 | spl28_411)), inference(subsumption_resolution, [], [f5353, f3004])).
fof(f3004, plain, (~ (e24 = h3(e11)) | spl28_411), inference(avatar_component_clause, [], [f3002])).
fof(f5353, plain, ((e24 = h3(e11)) | ~ spl28_190), inference(backward_demodulation, [], [f598, f1582])).
fof(f1582, plain, ((e24 = op2(e22, e22)) | ~ spl28_190), inference(avatar_component_clause, [], [f1580])).
fof(f1580, plain, (spl28_190 <=> (e24 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_190])])).
fof(f5311, plain, (~ spl28_242 | ~ spl28_243), inference(avatar_contradiction_clause, [], [f5310])).
fof(f5310, plain, ($false | (~ spl28_242 | ~ spl28_243)), inference(subsumption_resolution, [], [f5309, f497])).
fof(f5309, plain, ((e21 = e22) | (~ spl28_242 | ~ spl28_243)), inference(forward_demodulation, [], [f1805, f1801])).
fof(f1801, plain, ((e21 = op2(e20, e21)) | ~ spl28_242), inference(avatar_component_clause, [], [f1799])).
fof(f1799, plain, (spl28_242 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_242])])).
fof(f5222, plain, (~ spl28_152 | ~ spl28_267), inference(avatar_split_clause, [], [f4745, f2099, f1421])).
fof(f1421, plain, (spl28_152 <=> (e21 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_152])])).
fof(f4745, plain, (~ (e21 = op2(e23, e24)) | ~ spl28_267), inference(forward_demodulation, [], [f2040, f2100])).
fof(f2040, plain, ~ (op2(e23, e24) = h5(e11)), inference(backward_demodulation, [], [f432, f608])).
fof(f432, plain, ~ (op2(e23, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5191, plain, (~ spl28_68 | ~ spl28_118), inference(avatar_split_clause, [], [f5190, f1228, f1018])).
fof(f1018, plain, (spl28_68 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_68])])).
fof(f5190, plain, (~ (e12 = op1(e12, e11)) | ~ spl28_118), inference(forward_demodulation, [], [f294, f1230])).
fof(f294, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f5185, plain, (~ spl28_108 | ~ spl28_118), inference(avatar_contradiction_clause, [], [f5184])).
fof(f5184, plain, ($false | (~ spl28_108 | ~ spl28_118)), inference(subsumption_resolution, [], [f5183, f1230])).
fof(f5183, plain, (~ (e12 = op1(e10, e11)) | ~ spl28_108), inference(forward_demodulation, [], [f338, f1188])).
fof(f1188, plain, ((e12 = op1(e10, e13)) | ~ spl28_108), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl28_108 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_108])])).
fof(f338, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f5179, plain, (~ spl28_103 | ~ spl28_118), inference(avatar_split_clause, [], [f5178, f1228, f1165])).
fof(f1165, plain, (spl28_103 <=> (e12 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_103])])).
fof(f5178, plain, (~ (e12 = op1(e10, e14)) | ~ spl28_118), inference(forward_demodulation, [], [f339, f1230])).
fof(f339, plain, ~ (op1(e10, e11) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f5174, plain, (~ spl28_100 | ~ spl28_95), inference(avatar_split_clause, [], [f5173, f1131, f1152])).
fof(f1152, plain, (spl28_100 <=> (e14 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_100])])).
fof(f5173, plain, (~ (e14 = op1(e11, e10)) | ~ spl28_95), inference(forward_demodulation, [], [f343, f1133])).
fof(f343, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f7])).
fof(f5141, plain, (spl28_103 | ~ spl28_75 | ~ spl28_111), inference(avatar_split_clause, [], [f5140, f1199, f1047, f1165])).
fof(f1047, plain, (spl28_75 <=> (e14 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_75])])).
fof(f5140, plain, ((e12 = op1(e10, e14)) | (~ spl28_75 | ~ spl28_111)), inference(forward_demodulation, [], [f5139, f1201])).
fof(f5139, plain, ((e12 = op1(op1(e10, e12), e14)) | ~ spl28_75), inference(forward_demodulation, [], [f168, f1049])).
fof(f1049, plain, ((e14 = op1(e12, e10)) | ~ spl28_75), inference(avatar_component_clause, [], [f1047])).
fof(f168, plain, (e12 = op1(op1(e10, e12), op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f5047, plain, (~ spl28_23 | ~ spl28_48), inference(avatar_split_clause, [], [f5040, f934, f829])).
fof(f934, plain, (spl28_48 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_48])])).
fof(f5040, plain, (~ (e12 = op1(e14, e10)) | ~ spl28_48), inference(backward_demodulation, [], [f292, f936])).
fof(f936, plain, ((e12 = op1(e13, e10)) | ~ spl28_48), inference(avatar_component_clause, [], [f934])).
fof(f5012, plain, (~ spl28_16 | ~ spl28_78 | ~ spl28_111), inference(avatar_contradiction_clause, [], [f5011])).
fof(f5011, plain, ($false | (~ spl28_16 | ~ spl28_78 | ~ spl28_111)), inference(subsumption_resolution, [], [f5010, f483])).
fof(f483, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f9])).
fof(f5010, plain, ((e10 = e11) | (~ spl28_16 | ~ spl28_78 | ~ spl28_111)), inference(forward_demodulation, [], [f5005, f1201])).
fof(f5005, plain, ((e11 = op1(e10, e12)) | (~ spl28_16 | ~ spl28_78)), inference(backward_demodulation, [], [f4683, f1062])).
fof(f1062, plain, ((e12 = op1(e11, e14)) | ~ spl28_78), inference(avatar_component_clause, [], [f1060])).
fof(f1060, plain, (spl28_78 <=> (e12 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_78])])).
fof(f4683, plain, ((e11 = op1(e10, op1(e11, e14))) | ~ spl28_16), inference(forward_demodulation, [], [f167, f802])).
fof(f802, plain, ((e10 = op1(e14, e11)) | ~ spl28_16), inference(avatar_component_clause, [], [f800])).
fof(f800, plain, (spl28_16 <=> (e10 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_16])])).
fof(f167, plain, (e11 = op1(op1(e14, e11), op1(e11, e14))), inference(cnf_transformation, [], [f3])).
fof(f5009, plain, (~ spl28_16 | ~ spl28_72 | ~ spl28_78), inference(avatar_contradiction_clause, [], [f5008])).
fof(f5008, plain, ($false | (~ spl28_16 | ~ spl28_72 | ~ spl28_78)), inference(subsumption_resolution, [], [f5007, f489])).
fof(f5007, plain, ((e11 = e14) | (~ spl28_16 | ~ spl28_72 | ~ spl28_78)), inference(forward_demodulation, [], [f5004, f1037])).
fof(f5004, plain, ((e14 = op1(e12, e10)) | (~ spl28_16 | ~ spl28_78)), inference(backward_demodulation, [], [f4621, f1062])).
fof(f4621, plain, ((e14 = op1(op1(e11, e14), e10)) | ~ spl28_16), inference(forward_demodulation, [], [f179, f802])).
fof(f179, plain, (e14 = op1(op1(e11, e14), op1(e14, e11))), inference(cnf_transformation, [], [f3])).
fof(f4997, plain, (~ spl28_45 | ~ spl28_95), inference(avatar_split_clause, [], [f4994, f1131, f921])).
fof(f921, plain, (spl28_45 <=> (e14 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_45])])).
fof(f4994, plain, (~ (e14 = op1(e13, e11)) | ~ spl28_95), inference(backward_demodulation, [], [f298, f1133])).
fof(f298, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f4996, plain, (~ spl28_70 | ~ spl28_95), inference(avatar_split_clause, [], [f4993, f1131, f1026])).
fof(f1026, plain, (spl28_70 <=> (e14 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_70])])).
fof(f4993, plain, (~ (e14 = op1(e12, e11)) | ~ spl28_95), inference(backward_demodulation, [], [f297, f1133])).
fof(f297, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f4942, plain, (~ spl28_141 | ~ spl28_144), inference(avatar_contradiction_clause, [], [f4941])).
fof(f4941, plain, ($false | (~ spl28_141 | ~ spl28_144)), inference(subsumption_resolution, [], [f4940, f495])).
fof(f4940, plain, ((e20 = e23) | (~ spl28_141 | ~ spl28_144)), inference(backward_demodulation, [], [f1389, f1377])).
fof(f1389, plain, ((e23 = op2(e24, e21)) | ~ spl28_144), inference(avatar_component_clause, [], [f1387])).
fof(f1387, plain, (spl28_144 <=> (e23 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_144])])).
fof(f4915, plain, (~ spl28_156 | ~ spl28_157), inference(avatar_contradiction_clause, [], [f4914])).
fof(f4914, plain, ($false | (~ spl28_156 | ~ spl28_157)), inference(subsumption_resolution, [], [f4913, f493])).
fof(f493, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f10])).
fof(f4913, plain, ((e20 = e21) | (~ spl28_156 | ~ spl28_157)), inference(backward_demodulation, [], [f1444, f1440])).
fof(f1444, plain, ((e21 = op2(e23, e23)) | ~ spl28_157), inference(avatar_component_clause, [], [f1442])).
fof(f1442, plain, (spl28_157 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_157])])).
fof(f4911, plain, (~ spl28_163 | ~ spl28_164), inference(avatar_contradiction_clause, [], [f4910])).
fof(f4910, plain, ($false | (~ spl28_163 | ~ spl28_164)), inference(subsumption_resolution, [], [f4909, f500])).
fof(f500, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f10])).
fof(f4909, plain, ((e22 = e23) | (~ spl28_163 | ~ spl28_164)), inference(forward_demodulation, [], [f1473, f1469])).
fof(f1469, plain, ((e22 = op2(e23, e22)) | ~ spl28_163), inference(avatar_component_clause, [], [f1467])).
fof(f1467, plain, (spl28_163 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_163])])).
fof(f4880, plain, (~ spl28_197 | spl28_216 | ~ spl28_237), inference(avatar_contradiction_clause, [], [f4879])).
fof(f4879, plain, ($false | (~ spl28_197 | spl28_216 | ~ spl28_237)), inference(subsumption_resolution, [], [f4875, f1691])).
fof(f1691, plain, (~ (e20 = op2(e21, e21)) | spl28_216), inference(avatar_component_clause, [], [f1690])).
fof(f1690, plain, (spl28_216 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_216])])).
fof(f4875, plain, ((e20 = op2(e21, e21)) | (~ spl28_197 | ~ spl28_237)), inference(backward_demodulation, [], [f4780, f1612])).
fof(f4780, plain, ((e20 = op2(op2(e22, e20), e21)) | ~ spl28_237), inference(forward_demodulation, [], [f260, f1780])).
fof(f1780, plain, ((e21 = op2(e20, e22)) | ~ spl28_237), inference(avatar_component_clause, [], [f1778])).
fof(f1778, plain, (spl28_237 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_237])])).
fof(f260, plain, (e20 = op2(op2(e22, e20), op2(e20, e22))), inference(cnf_transformation, [], [f6])).
fof(f4871, plain, (~ spl28_217 | ~ spl28_220), inference(avatar_contradiction_clause, [], [f4870])).
fof(f4870, plain, ($false | (~ spl28_217 | ~ spl28_220)), inference(subsumption_resolution, [], [f4869, f499])).
fof(f4869, plain, ((e21 = e24) | (~ spl28_217 | ~ spl28_220)), inference(forward_demodulation, [], [f1708, f1696])).
fof(f1696, plain, ((e21 = op2(e21, e21)) | ~ spl28_217), inference(avatar_component_clause, [], [f1694])).
fof(f1694, plain, (spl28_217 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_217])])).
fof(f4852, plain, (spl28_469 | ~ spl28_235 | ~ spl28_260), inference(avatar_split_clause, [], [f4851, f2056, f1769, f3382])).
fof(f1769, plain, (spl28_235 <=> (e24 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_235])])).
fof(f4851, plain, ((e24 = h1(e10)) | (~ spl28_235 | ~ spl28_260)), inference(backward_demodulation, [], [f4349, f1771])).
fof(f1771, plain, ((e24 = op2(e20, e23)) | ~ spl28_235), inference(avatar_component_clause, [], [f1769])).
fof(f4827, plain, (spl28_243 | ~ spl28_267), inference(avatar_split_clause, [], [f4336, f2099, f1803])).
fof(f4336, plain, ((e22 = op2(e20, e21)) | ~ spl28_267), inference(backward_demodulation, [], [f2046, f2100])).
fof(f2046, plain, (e22 = op2(e20, h5(e11))), inference(backward_demodulation, [], [f1973, f608])).
fof(f1973, plain, (e22 = op2(e20, op2(e24, e24))), inference(backward_demodulation, [], [f584, f582])).
fof(f582, plain, (e20 = op2(e24, op2(e24, e24))), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((e23 = op2(op2(e24, op2(e24, e24)), op2(e24, op2(e24, e24)))) & (e22 = op2(op2(e24, op2(e24, e24)), op2(e24, e24))) & (e21 = op2(e24, e24)) & (e20 = op2(e24, op2(e24, e24)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax15)).
fof(f584, plain, (e22 = op2(op2(e24, op2(e24, e24)), op2(e24, e24))), inference(cnf_transformation, [], [f15])).
fof(f4818, plain, (~ spl28_229 | ~ spl28_260), inference(avatar_split_clause, [], [f4817, f2056, f1744])).
fof(f1744, plain, (spl28_229 <=> (e23 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_229])])).
fof(f4817, plain, (~ (e23 = op2(e20, e24)) | ~ spl28_260), inference(forward_demodulation, [], [f1984, f2057])).
fof(f1984, plain, ~ (op2(e20, e24) = h1(e11)), inference(backward_demodulation, [], [f436, f588])).
fof(f436, plain, ~ (op2(e20, e20) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f4816, plain, (~ spl28_227 | ~ spl28_267), inference(avatar_split_clause, [], [f4815, f2099, f1736])).
fof(f1736, plain, (spl28_227 <=> (e21 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_227])])).
fof(f4815, plain, (~ (e21 = op2(e20, e24)) | ~ spl28_267), inference(forward_demodulation, [], [f2037, f2100])).
fof(f2037, plain, ~ (op2(e20, e24) = h5(e11)), inference(backward_demodulation, [], [f426, f608])).
fof(f426, plain, ~ (op2(e20, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f4752, plain, (~ spl28_284 | ~ spl28_151), inference(avatar_split_clause, [], [f4751, f1417, f2190])).
fof(f1417, plain, (spl28_151 <=> (e20 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_151])])).
fof(f4751, plain, (~ (e20 = h4(e11)) | ~ spl28_151), inference(forward_demodulation, [], [f2026, f1419])).
fof(f1419, plain, ((e20 = op2(e23, e24)) | ~ spl28_151), inference(avatar_component_clause, [], [f1417])).
fof(f2026, plain, ~ (op2(e23, e24) = h4(e11)), inference(backward_demodulation, [], [f472, f603])).
fof(f472, plain, ~ (op2(e23, e23) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4731, plain, (~ spl28_137 | ~ spl28_267), inference(avatar_split_clause, [], [f4730, f2099, f1358])).
fof(f1358, plain, (spl28_137 <=> (e21 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_137])])).
fof(f4730, plain, (~ (e21 = op2(e24, e22)) | ~ spl28_267), inference(forward_demodulation, [], [f2043, f2100])).
fof(f2043, plain, ~ (op2(e24, e22) = h5(e11)), inference(backward_demodulation, [], [f481, f608])).
fof(f481, plain, ~ (op2(e24, e22) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f4712, plain, (~ spl28_97 | ~ spl28_72), inference(avatar_split_clause, [], [f4711, f1035, f1140])).
fof(f4711, plain, (~ (e11 = op1(e11, e10)) | ~ spl28_72), inference(forward_demodulation, [], [f287, f1037])).
fof(f287, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f4696, plain, (~ spl28_78 | ~ spl28_83), inference(avatar_split_clause, [], [f4695, f1081, f1060])).
fof(f4695, plain, (~ (e12 = op1(e11, e14)) | ~ spl28_83), inference(forward_demodulation, [], [f352, f1083])).
fof(f4679, plain, (~ spl28_2 | ~ spl28_40 | ~ spl28_60), inference(avatar_contradiction_clause, [], [f4678])).
fof(f4678, plain, ($false | (~ spl28_2 | ~ spl28_40 | ~ spl28_60)), inference(subsumption_resolution, [], [f4677, f487])).
fof(f4677, plain, ((e11 = e12) | (~ spl28_2 | ~ spl28_40 | ~ spl28_60)), inference(forward_demodulation, [], [f4676, f743])).
fof(f4676, plain, ((e12 = op1(e14, e14)) | (~ spl28_40 | ~ spl28_60)), inference(forward_demodulation, [], [f4675, f902])).
fof(f902, plain, ((e14 = op1(e13, e12)) | ~ spl28_40), inference(avatar_component_clause, [], [f900])).
fof(f900, plain, (spl28_40 <=> (e14 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_40])])).
fof(f4675, plain, ((e12 = op1(op1(e13, e12), e14)) | ~ spl28_60), inference(forward_demodulation, [], [f171, f986])).
fof(f171, plain, (e12 = op1(op1(e13, e12), op1(e12, e13))), inference(cnf_transformation, [], [f3])).
fof(f4643, plain, (~ spl28_26 | ~ spl28_31), inference(avatar_split_clause, [], [f4642, f863, f842])).
fof(f842, plain, (spl28_26 <=> (e10 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_26])])).
fof(f4642, plain, (~ (e10 = op1(e13, e14)) | ~ spl28_31), inference(forward_demodulation, [], [f372, f865])).
fof(f372, plain, ~ (op1(e13, e13) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4637, plain, (~ spl28_27 | ~ spl28_2), inference(avatar_split_clause, [], [f4109, f741, f846])).
fof(f846, plain, (spl28_27 <=> (e11 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_27])])).
fof(f4109, plain, (~ (e11 = op1(e13, e14)) | ~ spl28_2), inference(forward_demodulation, [], [f332, f743])).
fof(f332, plain, ~ (op1(e13, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4634, plain, (~ spl28_11 | ~ spl28_16), inference(avatar_split_clause, [], [f4633, f800, f779])).
fof(f779, plain, (spl28_11 <=> (e10 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_11])])).
fof(f4633, plain, (~ (e10 = op1(e14, e12)) | ~ spl28_16), inference(forward_demodulation, [], [f377, f802])).
fof(f377, plain, ~ (op1(e14, e11) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f4628, plain, (~ spl28_16 | spl28_459), inference(avatar_contradiction_clause, [], [f4627])).
fof(f4627, plain, ($false | (~ spl28_16 | spl28_459)), inference(trivial_inequality_removal, [], [f4626])).
fof(f4626, plain, (~ (h1(e10) = h1(e10)) | (~ spl28_16 | spl28_459)), inference(forward_demodulation, [], [f3296, f802])).
fof(f3296, plain, (~ (h1(e10) = h1(op1(e14, e11))) | spl28_459), inference(avatar_component_clause, [], [f3294])).
fof(f3294, plain, (spl28_459 <=> (h1(e10) = h1(op1(e14, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_459])])).
fof(f4625, plain, (~ spl28_21 | ~ spl28_16), inference(avatar_split_clause, [], [f4624, f800, f821])).
fof(f821, plain, (spl28_21 <=> (e10 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_21])])).
fof(f4624, plain, (~ (e10 = op1(e14, e10)) | ~ spl28_16), inference(forward_demodulation, [], [f373, f802])).
fof(f373, plain, ~ (op1(e14, e10) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f4595, plain, (~ spl28_31 | ~ spl28_34), inference(avatar_contradiction_clause, [], [f4594])).
fof(f4594, plain, ($false | (~ spl28_31 | ~ spl28_34)), inference(subsumption_resolution, [], [f4593, f485])).
fof(f485, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f9])).
fof(f4593, plain, ((e10 = e13) | (~ spl28_31 | ~ spl28_34)), inference(backward_demodulation, [], [f877, f865])).
fof(f877, plain, ((e13 = op1(e13, e13)) | ~ spl28_34), inference(avatar_component_clause, [], [f875])).
fof(f875, plain, (spl28_34 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_34])])).
fof(f4552, plain, (~ spl28_58 | ~ spl28_60), inference(avatar_contradiction_clause, [], [f4551])).
fof(f4551, plain, ($false | (~ spl28_58 | ~ spl28_60)), inference(subsumption_resolution, [], [f4550, f491])).
fof(f491, plain, ~ (e12 = e14), inference(cnf_transformation, [], [f9])).
fof(f4550, plain, ((e12 = e14) | (~ spl28_58 | ~ spl28_60)), inference(forward_demodulation, [], [f986, f978])).
fof(f978, plain, ((e12 = op1(e12, e13)) | ~ spl28_58), inference(avatar_component_clause, [], [f976])).
fof(f976, plain, (spl28_58 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_58])])).
fof(f4534, plain, (~ spl28_62 | ~ spl28_72), inference(avatar_split_clause, [], [f4526, f1035, f993])).
fof(f4526, plain, (~ (e11 = op1(e12, e12)) | ~ spl28_72), inference(backward_demodulation, [], [f354, f1037])).
fof(f354, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f4506, plain, (~ spl28_42 | ~ spl28_92), inference(avatar_split_clause, [], [f4503, f1119, f909])).
fof(f1119, plain, (spl28_92 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_92])])).
fof(f4503, plain, (~ (e11 = op1(e13, e11)) | ~ spl28_92), inference(backward_demodulation, [], [f298, f1121])).
fof(f1121, plain, ((e11 = op1(e11, e11)) | ~ spl28_92), inference(avatar_component_clause, [], [f1119])).
fof(f4462, plain, (~ spl28_121 | ~ spl28_124), inference(avatar_contradiction_clause, [], [f4461])).
fof(f4461, plain, ($false | (~ spl28_121 | ~ spl28_124)), inference(subsumption_resolution, [], [f4460, f485])).
fof(f4460, plain, ((e10 = e13) | (~ spl28_121 | ~ spl28_124)), inference(backward_demodulation, [], [f1255, f1243])).
fof(f1243, plain, ((e10 = op1(e10, e10)) | ~ spl28_121), inference(avatar_component_clause, [], [f1241])).
fof(f1241, plain, (spl28_121 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_121])])).
fof(f4413, plain, (~ spl28_170 | ~ spl28_175), inference(avatar_split_clause, [], [f4411, f1517, f1496])).
fof(f1496, plain, (spl28_170 <=> (e24 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_170])])).
fof(f4411, plain, (~ (e24 = op2(e23, e21)) | ~ spl28_175), inference(backward_demodulation, [], [f463, f1519])).
fof(f463, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f4404, plain, (spl28_294 | ~ spl28_187), inference(avatar_split_clause, [], [f4403, f1568, f2241])).
fof(f2241, plain, (spl28_294 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_294])])).
fof(f1568, plain, (spl28_187 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_187])])).
fof(f4403, plain, ((e21 = h3(e11)) | ~ spl28_187), inference(backward_demodulation, [], [f598, f1570])).
fof(f1570, plain, ((e21 = op2(e22, e22)) | ~ spl28_187), inference(avatar_component_clause, [], [f1568])).
fof(f4399, plain, (~ spl28_294 | ~ spl28_192), inference(avatar_split_clause, [], [f4395, f1589, f2241])).
fof(f4395, plain, (~ (e21 = h3(e11)) | ~ spl28_192), inference(backward_demodulation, [], [f2010, f1591])).
fof(f2010, plain, ~ (op2(e22, e21) = h3(e11)), inference(backward_demodulation, [], [f457, f598])).
fof(f457, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f4378, plain, (~ spl28_172 | ~ spl28_222), inference(avatar_split_clause, [], [f4374, f1715, f1505])).
fof(f4374, plain, (~ (e21 = op2(e23, e20)) | ~ spl28_222), inference(backward_demodulation, [], [f388, f1717])).
fof(f388, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f4369, plain, (~ spl28_162 | ~ spl28_237), inference(avatar_split_clause, [], [f4365, f1778, f1463])).
fof(f4365, plain, (~ (e21 = op2(e23, e22)) | ~ spl28_237), inference(backward_demodulation, [], [f405, f1780])).
fof(f405, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4356, plain, (~ spl28_239 | ~ spl28_260), inference(avatar_split_clause, [], [f4347, f2056, f1786])).
fof(f1786, plain, (spl28_239 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_239])])).
fof(f4347, plain, (~ (e23 = op2(e20, e22)) | ~ spl28_260), inference(backward_demodulation, [], [f1982, f2057])).
fof(f1982, plain, ~ (op2(e20, e22) = h1(e11)), inference(backward_demodulation, [], [f434, f588])).
fof(f434, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f8])).
fof(f4355, plain, (~ spl28_174 | ~ spl28_260), inference(avatar_split_clause, [], [f4346, f2056, f1513])).
fof(f1513, plain, (spl28_174 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_174])])).
fof(f4346, plain, (~ (e23 = op2(e23, e20)) | ~ spl28_260), inference(backward_demodulation, [], [f1979, f2057])).
fof(f1979, plain, ~ (op2(e23, e20) = h1(e11)), inference(backward_demodulation, [], [f385, f588])).
fof(f385, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f4354, plain, (~ spl28_224 | ~ spl28_260), inference(avatar_split_clause, [], [f4345, f2056, f1723])).
fof(f1723, plain, (spl28_224 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_224])])).
fof(f4345, plain, (~ (e23 = op2(e21, e20)) | ~ spl28_260), inference(backward_demodulation, [], [f1977, f2057])).
fof(f1977, plain, ~ (op2(e21, e20) = h1(e11)), inference(backward_demodulation, [], [f383, f588])).
fof(f383, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f8])).
fof(f4353, plain, (spl28_156 | ~ spl28_260), inference(avatar_split_clause, [], [f4344, f2056, f1438])).
fof(f4344, plain, ((e20 = op2(e23, e23)) | ~ spl28_260), inference(backward_demodulation, [], [f1976, f2057])).
fof(f1976, plain, (e20 = op2(h1(e11), h1(e11))), inference(backward_demodulation, [], [f258, f588])).
fof(f258, plain, (e20 = op2(op2(e20, e20), op2(e20, e20))), inference(cnf_transformation, [], [f6])).
fof(f4343, plain, (~ spl28_241 | ~ spl28_267), inference(avatar_contradiction_clause, [], [f4342])).
fof(f4342, plain, ($false | (~ spl28_241 | ~ spl28_267)), inference(subsumption_resolution, [], [f4341, f494])).
fof(f4341, plain, ((e20 = e22) | (~ spl28_241 | ~ spl28_267)), inference(forward_demodulation, [], [f4336, f1797])).
fof(f1797, plain, ((e20 = op2(e20, e21)) | ~ spl28_241), inference(avatar_component_clause, [], [f1795])).
fof(f1795, plain, (spl28_241 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_241])])).
fof(f4340, plain, (spl28_141 | ~ spl28_267), inference(avatar_split_clause, [], [f4335, f2099, f1375])).
fof(f4335, plain, ((e20 = op2(e24, e21)) | ~ spl28_267), inference(backward_demodulation, [], [f2045, f2100])).
fof(f2045, plain, (e20 = op2(e24, h5(e11))), inference(backward_demodulation, [], [f582, f608])).
fof(f4338, plain, (spl28_220 | ~ spl28_267), inference(avatar_split_clause, [], [f4333, f2099, f1706])).
fof(f4333, plain, ((e24 = op2(e21, e21)) | ~ spl28_267), inference(backward_demodulation, [], [f2036, f2100])).
fof(f2036, plain, (e24 = op2(h5(e11), h5(e11))), inference(backward_demodulation, [], [f282, f608])).
fof(f282, plain, (e24 = op2(op2(e24, e24), op2(e24, e24))), inference(cnf_transformation, [], [f6])).
fof(f4320, plain, (~ spl28_232 | ~ spl28_207), inference(avatar_split_clause, [], [f4319, f1652, f1757])).
fof(f1652, plain, (spl28_207 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_207])])).
fof(f4319, plain, (~ (e21 = op2(e20, e23)) | ~ spl28_207), inference(forward_demodulation, [], [f413, f1654])).
fof(f1654, plain, ((e21 = op2(e21, e23)) | ~ spl28_207), inference(avatar_component_clause, [], [f1652])).
fof(f413, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f4257, plain, (~ spl28_168 | ~ spl28_153), inference(avatar_split_clause, [], [f4256, f1425, f1488])).
fof(f1488, plain, (spl28_168 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_168])])).
fof(f4256, plain, (~ (e22 = op2(e23, e21)) | ~ spl28_153), inference(forward_demodulation, [], [f469, f1427])).
fof(f469, plain, ~ (op2(e23, e21) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4208, plain, (spl28_118 | ~ spl28_2), inference(avatar_split_clause, [], [f4066, f741, f1228])).
fof(f4066, plain, ((e12 = op1(e10, e11)) | ~ spl28_2), inference(backward_demodulation, [], [f1969, f743])).
fof(f1969, plain, (e12 = op1(e10, op1(e14, e14))), inference(backward_demodulation, [], [f580, f578])).
fof(f578, plain, (e10 = op1(e14, op1(e14, e14))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((e13 = op1(op1(e14, op1(e14, e14)), op1(e14, op1(e14, e14)))) & (e12 = op1(op1(e14, op1(e14, e14)), op1(e14, e14))) & (e11 = op1(e14, e14)) & (e10 = op1(e14, op1(e14, e14)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax14)).
fof(f580, plain, (e12 = op1(op1(e14, op1(e14, e14)), op1(e14, e14))), inference(cnf_transformation, [], [f14])).
fof(f4203, plain, (~ spl28_107 | ~ spl28_82), inference(avatar_split_clause, [], [f4202, f1077, f1182])).
fof(f1077, plain, (spl28_82 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_82])])).
fof(f4202, plain, (~ (e11 = op1(e10, e13)) | ~ spl28_82), inference(forward_demodulation, [], [f313, f1079])).
fof(f1079, plain, ((e11 = op1(e11, e13)) | ~ spl28_82), inference(avatar_component_clause, [], [f1077])).
fof(f313, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f7])).
fof(f4184, plain, (spl28_95 | ~ spl28_2), inference(avatar_split_clause, [], [f4063, f741, f1131])).
fof(f4063, plain, ((e14 = op1(e11, e11)) | ~ spl28_2), inference(backward_demodulation, [], [f182, f743])).
fof(f182, plain, (e14 = op1(op1(e14, e14), op1(e14, e14))), inference(cnf_transformation, [], [f3])).
fof(f4179, plain, (~ spl28_89 | ~ spl28_79), inference(avatar_split_clause, [], [f4178, f1064, f1106])).
fof(f1106, plain, (spl28_89 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_89])])).
fof(f4178, plain, (~ (e13 = op1(e11, e12)) | ~ spl28_79), inference(forward_demodulation, [], [f351, f1066])).
fof(f351, plain, ~ (op1(e11, e12) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f4165, plain, (~ spl28_73 | ~ spl28_23), inference(avatar_split_clause, [], [f4164, f829, f1039])).
fof(f1039, plain, (spl28_73 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_73])])).
fof(f4164, plain, (~ (e12 = op1(e12, e10)) | ~ spl28_23), inference(forward_demodulation, [], [f291, f831])).
fof(f291, plain, ~ (op1(e12, e10) = op1(e14, e10)), inference(cnf_transformation, [], [f7])).
fof(f4151, plain, (~ spl28_63 | ~ spl28_58), inference(avatar_split_clause, [], [f4150, f976, f997])).
fof(f4150, plain, (~ (e12 = op1(e12, e12)) | ~ spl28_58), inference(forward_demodulation, [], [f360, f978])).
fof(f360, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f4137, plain, (~ spl28_52 | ~ spl28_2), inference(avatar_split_clause, [], [f4136, f741, f951])).
fof(f951, plain, (spl28_52 <=> (e11 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_52])])).
fof(f4136, plain, (~ (e11 = op1(e12, e14)) | ~ spl28_2), inference(forward_demodulation, [], [f331, f743])).
fof(f331, plain, ~ (op1(e12, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4098, plain, (~ spl28_22 | ~ spl28_2), inference(avatar_split_clause, [], [f4097, f741, f825])).
fof(f825, plain, (spl28_22 <=> (e11 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_22])])).
fof(f4097, plain, (~ (e11 = op1(e14, e10)) | ~ spl28_2), inference(forward_demodulation, [], [f376, f743])).
fof(f376, plain, ~ (op1(e14, e10) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4076, plain, (~ spl28_2 | ~ spl28_116), inference(avatar_contradiction_clause, [], [f4075])).
fof(f4075, plain, ($false | (~ spl28_2 | ~ spl28_116)), inference(subsumption_resolution, [], [f4074, f484])).
fof(f4074, plain, ((e10 = e12) | (~ spl28_2 | ~ spl28_116)), inference(forward_demodulation, [], [f4066, f1222])).
fof(f1222, plain, ((e10 = op1(e10, e11)) | ~ spl28_116), inference(avatar_component_clause, [], [f1220])).
fof(f1220, plain, (spl28_116 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_116])])).
fof(f4073, plain, (spl28_16 | ~ spl28_2), inference(avatar_split_clause, [], [f4064, f741, f800])).
fof(f4064, plain, ((e10 = op1(e14, e11)) | ~ spl28_2), inference(backward_demodulation, [], [f578, f743])).
fof(f3969, plain, (~ spl28_31 | ~ spl28_36), inference(avatar_split_clause, [], [f3964, f884, f863])).
fof(f884, plain, (spl28_36 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_36])])).
fof(f3964, plain, (~ (e10 = op1(e13, e13)) | ~ spl28_36), inference(backward_demodulation, [], [f370, f886])).
fof(f886, plain, ((e10 = op1(e13, e12)) | ~ spl28_36), inference(avatar_component_clause, [], [f884])).
fof(f370, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f3955, plain, (~ spl28_31 | ~ spl28_41), inference(avatar_split_clause, [], [f3949, f905, f863])).
fof(f905, plain, (spl28_41 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_41])])).
fof(f3949, plain, (~ (e10 = op1(e13, e13)) | ~ spl28_41), inference(backward_demodulation, [], [f368, f907])).
fof(f907, plain, ((e10 = op1(e13, e11)) | ~ spl28_41), inference(avatar_component_clause, [], [f905])).
fof(f368, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f3944, plain, (~ spl28_31 | ~ spl28_46), inference(avatar_split_clause, [], [f3937, f926, f863])).
fof(f926, plain, (spl28_46 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_46])])).
fof(f3937, plain, (~ (e10 = op1(e13, e13)) | ~ spl28_46), inference(backward_demodulation, [], [f365, f928])).
fof(f928, plain, ((e10 = op1(e13, e10)) | ~ spl28_46), inference(avatar_component_clause, [], [f926])).
fof(f365, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f3897, plain, (~ spl28_16 | ~ spl28_66), inference(avatar_split_clause, [], [f3890, f1010, f800])).
fof(f1010, plain, (spl28_66 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_66])])).
fof(f3890, plain, (~ (e10 = op1(e14, e11)) | ~ spl28_66), inference(backward_demodulation, [], [f301, f1012])).
fof(f1012, plain, ((e10 = op1(e12, e11)) | ~ spl28_66), inference(avatar_component_clause, [], [f1010])).
fof(f301, plain, ~ (op1(e12, e11) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f3887, plain, (spl28_121 | ~ spl28_71 | ~ spl28_111), inference(avatar_split_clause, [], [f3879, f1199, f1031, f1241])).
fof(f1031, plain, (spl28_71 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_71])])).
fof(f3879, plain, ((e10 = op1(e10, e10)) | (~ spl28_71 | ~ spl28_111)), inference(backward_demodulation, [], [f3759, f1033])).
fof(f1033, plain, ((e10 = op1(e12, e10)) | ~ spl28_71), inference(avatar_component_clause, [], [f1031])).
fof(f3759, plain, ((e10 = op1(op1(e12, e10), e10)) | ~ spl28_111), inference(backward_demodulation, [], [f160, f1201])).
fof(f160, plain, (e10 = op1(op1(e12, e10), op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f3884, plain, (~ spl28_61 | ~ spl28_71), inference(avatar_split_clause, [], [f3876, f1031, f989])).
fof(f989, plain, (spl28_61 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_61])])).
fof(f3876, plain, (~ (e10 = op1(e12, e12)) | ~ spl28_71), inference(backward_demodulation, [], [f354, f1033])).
fof(f3848, plain, (~ spl28_76 | ~ spl28_86), inference(avatar_split_clause, [], [f3843, f1094, f1052])).
fof(f1094, plain, (spl28_86 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_86])])).
fof(f3843, plain, (~ (e10 = op1(e11, e14)) | ~ spl28_86), inference(backward_demodulation, [], [f351, f1096])).
fof(f1096, plain, ((e10 = op1(e11, e12)) | ~ spl28_86), inference(avatar_component_clause, [], [f1094])).
fof(f3786, plain, (~ spl28_31 | ~ spl28_106), inference(avatar_split_clause, [], [f3781, f1178, f863])).
fof(f1178, plain, (spl28_106 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_106])])).
fof(f3781, plain, (~ (e10 = op1(e13, e13)) | ~ spl28_106), inference(backward_demodulation, [], [f315, f1180])).
fof(f1180, plain, ((e10 = op1(e10, e13)) | ~ spl28_106), inference(avatar_component_clause, [], [f1178])).
fof(f315, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f3770, plain, (~ spl28_61 | ~ spl28_111), inference(avatar_split_clause, [], [f3762, f1199, f989])).
fof(f3762, plain, (~ (e10 = op1(e12, e12)) | ~ spl28_111), inference(backward_demodulation, [], [f304, f1201])).
fof(f304, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f3742, plain, (~ spl28_104 | ~ spl28_124), inference(avatar_split_clause, [], [f3733, f1253, f1169])).
fof(f1169, plain, (spl28_104 <=> (e13 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_104])])).
fof(f3733, plain, (~ (e13 = op1(e10, e14)) | ~ spl28_124), inference(backward_demodulation, [], [f336, f1255])).
fof(f336, plain, ~ (op1(e10, e10) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f3741, plain, (~ spl28_109 | ~ spl28_124), inference(avatar_split_clause, [], [f3732, f1253, f1190])).
fof(f1190, plain, (spl28_109 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_109])])).
fof(f3732, plain, (~ (e13 = op1(e10, e13)) | ~ spl28_124), inference(backward_demodulation, [], [f335, f1255])).
fof(f335, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f3740, plain, (~ spl28_114 | ~ spl28_124), inference(avatar_split_clause, [], [f3731, f1253, f1211])).
fof(f1211, plain, (spl28_114 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_114])])).
fof(f3731, plain, (~ (e13 = op1(e10, e12)) | ~ spl28_124), inference(backward_demodulation, [], [f334, f1255])).
fof(f334, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f7])).
fof(f3738, plain, (~ spl28_24 | ~ spl28_124), inference(avatar_split_clause, [], [f3729, f1253, f833])).
fof(f833, plain, (spl28_24 <=> (e13 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_24])])).
fof(f3729, plain, (~ (e13 = op1(e14, e10)) | ~ spl28_124), inference(backward_demodulation, [], [f286, f1255])).
fof(f286, plain, ~ (op1(e10, e10) = op1(e14, e10)), inference(cnf_transformation, [], [f7])).
fof(f3737, plain, (~ spl28_49 | ~ spl28_124), inference(avatar_split_clause, [], [f3728, f1253, f938])).
fof(f938, plain, (spl28_49 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_49])])).
fof(f3728, plain, (~ (e13 = op1(e13, e10)) | ~ spl28_124), inference(backward_demodulation, [], [f285, f1255])).
fof(f285, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f7])).
fof(f3736, plain, (~ spl28_74 | ~ spl28_124), inference(avatar_split_clause, [], [f3727, f1253, f1043])).
fof(f1043, plain, (spl28_74 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_74])])).
fof(f3727, plain, (~ (e13 = op1(e12, e10)) | ~ spl28_124), inference(backward_demodulation, [], [f284, f1255])).
fof(f284, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f3735, plain, (~ spl28_99 | ~ spl28_124), inference(avatar_split_clause, [], [f3726, f1253, f1148])).
fof(f1148, plain, (spl28_99 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_99])])).
fof(f3726, plain, (~ (e13 = op1(e11, e10)) | ~ spl28_124), inference(backward_demodulation, [], [f283, f1255])).
fof(f283, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f7])).
fof(f3734, plain, (spl28_31 | ~ spl28_124), inference(avatar_split_clause, [], [f3725, f1253, f863])).
fof(f3725, plain, ((e10 = op1(e13, e13)) | ~ spl28_124), inference(backward_demodulation, [], [f158, f1255])).
fof(f158, plain, (e10 = op1(op1(e10, e10), op1(e10, e10))), inference(cnf_transformation, [], [f3])).
fof(f3724, plain, (~ spl28_126 | ~ spl28_127), inference(avatar_contradiction_clause, [], [f3723])).
fof(f3723, plain, ($false | (~ spl28_126 | ~ spl28_127)), inference(subsumption_resolution, [], [f3722, f493])).
fof(f3722, plain, ((e20 = e21) | (~ spl28_126 | ~ spl28_127)), inference(backward_demodulation, [], [f1318, f1314])).
fof(f1314, plain, ((e20 = op2(e24, e24)) | ~ spl28_126), inference(avatar_component_clause, [], [f1312])).
fof(f1312, plain, (spl28_126 <=> (e20 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_126])])).
fof(f3721, plain, (spl28_267 | ~ spl28_127), inference(avatar_split_clause, [], [f3720, f1316, f2099])).
fof(f3720, plain, ((e21 = h5(e11)) | ~ spl28_127), inference(backward_demodulation, [], [f608, f1318])).
fof(f3665, plain, (spl28_126 | ~ spl28_150 | ~ spl28_230), inference(avatar_split_clause, [], [f3658, f1748, f1412, f1312])).
fof(f1412, plain, (spl28_150 <=> (e24 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_150])])).
fof(f3658, plain, ((e20 = op2(e24, e24)) | (~ spl28_150 | ~ spl28_230)), inference(backward_demodulation, [], [f3436, f1414])).
fof(f1414, plain, ((e24 = op2(e24, e20)) | ~ spl28_150), inference(avatar_component_clause, [], [f1412])).
fof(f3436, plain, ((e20 = op2(op2(e24, e20), e24)) | ~ spl28_230), inference(backward_demodulation, [], [f262, f1750])).
fof(f262, plain, (e20 = op2(op2(e24, e20), op2(e20, e24))), inference(cnf_transformation, [], [f6])).
fof(f3603, plain, (~ spl28_284 | ~ spl28_166), inference(avatar_split_clause, [], [f3596, f1480, f2190])).
fof(f1480, plain, (spl28_166 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_166])])).
fof(f3596, plain, (~ (e20 = h4(e11)) | ~ spl28_166), inference(backward_demodulation, [], [f2024, f1482])).
fof(f1482, plain, ((e20 = op2(e23, e21)) | ~ spl28_166), inference(avatar_component_clause, [], [f1480])).
fof(f2024, plain, ~ (op2(e23, e21) = h4(e11)), inference(backward_demodulation, [], [f468, f603])).
fof(f468, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f3590, plain, (~ spl28_284 | ~ spl28_171), inference(avatar_split_clause, [], [f3582, f1501, f2190])).
fof(f1501, plain, (spl28_171 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_171])])).
fof(f3582, plain, (~ (e20 = h4(e11)) | ~ spl28_171), inference(backward_demodulation, [], [f2023, f1503])).
fof(f1503, plain, ((e20 = op2(e23, e20)) | ~ spl28_171), inference(avatar_component_clause, [], [f1501])).
fof(f2023, plain, ~ (op2(e23, e20) = h4(e11)), inference(backward_demodulation, [], [f465, f603])).
fof(f465, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f3552, plain, (spl28_299 | ~ spl28_186), inference(avatar_split_clause, [], [f3551, f1564, f2266])).
fof(f2266, plain, (spl28_299 <=> (e20 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_299])])).
fof(f3551, plain, ((e20 = h3(e11)) | ~ spl28_186), inference(backward_demodulation, [], [f598, f1566])).
fof(f1566, plain, ((e20 = op2(e22, e22)) | ~ spl28_186), inference(avatar_component_clause, [], [f1564])).
fof(f3544, plain, (~ spl28_141 | ~ spl28_191), inference(avatar_split_clause, [], [f3536, f1585, f1375])).
fof(f3536, plain, (~ (e20 = op2(e24, e21)) | ~ spl28_191), inference(backward_demodulation, [], [f401, f1587])).
fof(f1587, plain, ((e20 = op2(e22, e21)) | ~ spl28_191), inference(avatar_component_clause, [], [f1585])).
fof(f401, plain, ~ (op2(e22, e21) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f3532, plain, (~ spl28_299 | ~ spl28_196), inference(avatar_split_clause, [], [f3523, f1606, f2266])).
fof(f1606, plain, (spl28_196 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_196])])).
fof(f3523, plain, (~ (e20 = h3(e11)) | ~ spl28_196), inference(backward_demodulation, [], [f2009, f1608])).
fof(f1608, plain, ((e20 = op2(e22, e20)) | ~ spl28_196), inference(avatar_component_clause, [], [f1606])).
fof(f2009, plain, ~ (op2(e22, e20) = h3(e11)), inference(backward_demodulation, [], [f454, f598])).
fof(f454, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f3516, plain, (~ spl28_203 | ~ spl28_204), inference(avatar_contradiction_clause, [], [f3515])).
fof(f3515, plain, ($false | (~ spl28_203 | ~ spl28_204)), inference(subsumption_resolution, [], [f3514, f500])).
fof(f3514, plain, ((e22 = e23) | (~ spl28_203 | ~ spl28_204)), inference(backward_demodulation, [], [f1641, f1637])).
fof(f1637, plain, ((e22 = op2(e21, e24)) | ~ spl28_203), inference(avatar_component_clause, [], [f1635])).
fof(f1635, plain, (spl28_203 <=> (e22 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_203])])).
fof(f3489, plain, (~ spl28_299 | ~ spl28_211), inference(avatar_split_clause, [], [f3483, f1669, f2266])).
fof(f3483, plain, (~ (e20 = h3(e11)) | ~ spl28_211), inference(backward_demodulation, [], [f2006, f1671])).
fof(f1671, plain, ((e20 = op2(e21, e22)) | ~ spl28_211), inference(avatar_component_clause, [], [f1669])).
fof(f2006, plain, ~ (op2(e21, e22) = h3(e11)), inference(backward_demodulation, [], [f407, f598])).
fof(f407, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f3475, plain, (spl28_314 | ~ spl28_216), inference(avatar_split_clause, [], [f3474, f1690, f2342])).
fof(f2342, plain, (spl28_314 <=> (e20 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_314])])).
fof(f3474, plain, ((e20 = h2(e11)) | ~ spl28_216), inference(backward_demodulation, [], [f593, f1692])).
fof(f1692, plain, ((e20 = op2(e21, e21)) | ~ spl28_216), inference(avatar_component_clause, [], [f1690])).
fof(f3471, plain, (~ spl28_314 | ~ spl28_221), inference(avatar_split_clause, [], [f3461, f1711, f2342])).
fof(f3461, plain, (~ (e20 = h2(e11)) | ~ spl28_221), inference(backward_demodulation, [], [f1995, f1713])).
fof(f1995, plain, ~ (op2(e21, e20) = h2(e11)), inference(backward_demodulation, [], [f443, f593])).
fof(f443, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f8])).
fof(f3469, plain, (~ spl28_201 | ~ spl28_221), inference(avatar_split_clause, [], [f3459, f1711, f1627])).
fof(f1627, plain, (spl28_201 <=> (e20 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_201])])).
fof(f3459, plain, (~ (e20 = op2(e21, e24)) | ~ spl28_221), inference(backward_demodulation, [], [f446, f1713])).
fof(f446, plain, ~ (op2(e21, e20) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f3435, plain, (~ spl28_284 | ~ spl28_231), inference(avatar_split_clause, [], [f3429, f1753, f2190])).
fof(f1753, plain, (spl28_231 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_231])])).
fof(f3429, plain, (~ (e20 = h4(e11)) | ~ spl28_231), inference(backward_demodulation, [], [f2019, f1755])).
fof(f1755, plain, ((e20 = op2(e20, e23)) | ~ spl28_231), inference(avatar_component_clause, [], [f1753])).
fof(f2019, plain, ~ (op2(e20, e23) = h4(e11)), inference(backward_demodulation, [], [f415, f603])).
fof(f415, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f3387, plain, (spl28_260 | ~ spl28_249), inference(avatar_split_clause, [], [f3386, f1828, f2056])).
fof(f3386, plain, ((e23 = h1(e11)) | ~ spl28_249), inference(backward_demodulation, [], [f588, f1830])).
fof(f3329, plain, (~ spl28_442 | ~ spl28_443 | ~ spl28_444 | ~ spl28_445 | ~ spl28_446 | ~ spl28_447 | ~ spl28_448 | ~ spl28_449 | ~ spl28_450 | ~ spl28_451 | ~ spl28_452 | ~ spl28_453 | ~ spl28_454 | spl28_324 | spl28_320 | spl28_316 | ~ spl28_455 | ~ spl28_456 | ~ spl28_457 | ~ spl28_458 | ~ spl28_459 | ~ spl28_460 | ~ spl28_461 | ~ spl28_462 | ~ spl28_463 | ~ spl28_464 | ~ spl28_465 | ~ spl28_466 | ~ spl28_467), inference(avatar_split_clause, [], [f3224, f3326, f3322, f3318, f3314, f3310, f3306, f3302, f3298, f3294, f3290, f3286, f3282, f3278, f2353, f2374, f2395, f3274, f3270, f3266, f3262, f3258, f3254, f3250, f3246, f3242, f3238, f3234, f3230, f3226])).
fof(f2395, plain, (spl28_324 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl28_324])])).
fof(f2374, plain, (spl28_320 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl28_320])])).
fof(f2353, plain, (spl28_316 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl28_316])])).
fof(f3224, plain, (~ (h1(e13) = h1(op1(e10, e10))) | ~ (h1(e12) = h1(op1(e10, e11))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), e20)) | ~ (e20 = h1(op1(e11, e11))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), e20)) | ~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | ~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12)))), inference(forward_demodulation, [], [f3223, f1988])).
fof(f3223, plain, (~ (h1(e12) = h1(op1(e10, e11))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), e20)) | ~ (e20 = h1(op1(e11, e11))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), e20)) | ~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | ~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3222, f1989])).
fof(f3222, plain, (~ (h1(op1(e10, e14)) = op2(h1(e10), e20)) | ~ (e20 = h1(op1(e11, e11))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), e20)) | ~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | ~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3221, f586])).
fof(f3221, plain, (~ (e20 = h1(op1(e11, e11))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), e20)) | ~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | ~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3220, f1976])).
fof(f3220, plain, (~ (h1(op1(e11, e14)) = op2(h1(e11), e20)) | ~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | ~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3219, f586])).
fof(f3219, plain, (~ (h1(op1(e12, e14)) = op2(h1(e12), e20)) | ~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3218, f586])).
fof(f3218, plain, (~ (h1(op1(e13, e14)) = op2(h1(e13), e20)) | ~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3217, f586])).
fof(f3217, plain, (~ (h1(op1(e14, e10)) = op2(e20, h1(e10))) | ~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3216, f586])).
fof(f3216, plain, (~ (h1(e10) = h1(op1(e14, e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3215, f1987])).
fof(f3215, plain, (~ (h1(op1(e14, e11)) = op2(e20, h1(e11))) | ~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3214, f586])).
fof(f3214, plain, (~ (h1(op1(e14, e12)) = op2(e20, h1(e12))) | ~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3213, f586])).
fof(f3213, plain, (~ (h1(op1(e14, e13)) = op2(e20, h1(e13))) | ~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3212, f586])).
fof(f3212, plain, (~ (h1(e11) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3211, f588])).
fof(f3211, plain, (~ (op2(e20, e20) = h1(op1(e14, e14))) | ~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3210, f586])).
fof(f3210, plain, (~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(subsumption_resolution, [], [f714, f2414])).
fof(f2414, plain, ~ sP8, inference(subsumption_resolution, [], [f710, f586])).
fof(f710, plain, (~ (e20 = h1(e14)) | ~ sP8), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP8), inference(nnf_transformation, [], [f34])).
fof(f34, plain, ((~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP8), inference(usedef, [], [e34])).
fof(e34, plain, (sP8 <=> (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f714, plain, (~ (e24 = h1(e13)) | sP11 | sP10 | sP9 | sP8 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(cnf_transformation, [], [f54])).
fof(f54, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | sP23 | sP22 | sP21 | sP20 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | sP19 | sP18 | sP17 | sP16 | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | sP15 | sP14 | sP13 | sP12 | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | sP11 | sP10 | sP9 | sP8 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f23, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34])).
fof(f35, plain, ((~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP9), inference(usedef, [], [e35])).
fof(e35, plain, (sP9 <=> (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f36, plain, ((~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP10), inference(usedef, [], [e36])).
fof(e36, plain, (sP10 <=> (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f37, plain, ((~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | ~ sP11), inference(usedef, [], [e37])).
fof(e37, plain, (sP11 <=> (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f38, plain, ((~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP12), inference(usedef, [], [e38])).
fof(e38, plain, (sP12 <=> (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f39, plain, ((~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP13), inference(usedef, [], [e39])).
fof(e39, plain, (sP13 <=> (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f40, plain, ((~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP14), inference(usedef, [], [e40])).
fof(e40, plain, (sP14 <=> (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f41, plain, ((~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | ~ sP15), inference(usedef, [], [e41])).
fof(e41, plain, (sP15 <=> (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f42, plain, ((~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP16), inference(usedef, [], [e42])).
fof(e42, plain, (sP16 <=> (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f43, plain, ((~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP17), inference(usedef, [], [e43])).
fof(e43, plain, (sP17 <=> (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f44, plain, ((~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP18), inference(usedef, [], [e44])).
fof(e44, plain, (sP18 <=> (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f45, plain, ((~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | ~ sP19), inference(usedef, [], [e45])).
fof(e45, plain, (sP19 <=> (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f46, plain, ((~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP20), inference(usedef, [], [e46])).
fof(e46, plain, (sP20 <=> (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f47, plain, ((~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP21), inference(usedef, [], [e47])).
fof(e47, plain, (sP21 <=> (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f48, plain, ((~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP22), inference(usedef, [], [e48])).
fof(e48, plain, (sP22 <=> (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f49, plain, ((~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | ~ sP23), inference(usedef, [], [e49])).
fof(e49, plain, (sP23 <=> (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f50, plain, ((~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ sP24), inference(usedef, [], [e50])).
fof(e50, plain, (sP24 <=> (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f51, plain, ((~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | ~ sP25), inference(usedef, [], [e51])).
fof(e51, plain, (sP25 <=> (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f52, plain, ((~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | ~ sP26), inference(usedef, [], [e52])).
fof(e52, plain, (sP26 <=> (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f53, plain, ((~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | ~ sP27), inference(usedef, [], [e53])).
fof(e53, plain, (sP27 <=> (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f23, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f21])).
fof(f21, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', co1)).
fof(f2413, plain, (~ spl28_324 | ~ spl28_327), inference(avatar_split_clause, [], [f701, f2410, f2395])).
fof(f701, plain, (~ (e21 = h1(e10)) | ~ sP9), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP9), inference(nnf_transformation, [], [f35])).
fof(f2386, plain, (~ spl28_320 | ~ spl28_322), inference(avatar_split_clause, [], [f698, f2383, f2374])).
fof(f698, plain, (~ (e22 = h1(e12)) | ~ sP10), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP10), inference(nnf_transformation, [], [f36])).
fof(f2366, plain, (~ spl28_316 | ~ spl28_260), inference(avatar_split_clause, [], [f692, f2056, f2353])).
fof(f692, plain, (~ (e23 = h1(e11)) | ~ sP11), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | ~ sP11), inference(nnf_transformation, [], [f37])).
fof(f1975, plain, spl28_249, inference(avatar_split_clause, [], [f1974, f1828])).
fof(f1974, plain, (op2(e20, e20) = e23), inference(backward_demodulation, [], [f585, f582])).
fof(f585, plain, (e23 = op2(op2(e24, op2(e24, e24)), op2(e24, op2(e24, e24)))), inference(cnf_transformation, [], [f15])).
fof(f1972, plain, spl28_127, inference(avatar_split_clause, [], [f583, f1316])).
fof(f583, plain, (e21 = op2(e24, e24)), inference(cnf_transformation, [], [f15])).
fof(f1971, plain, spl28_124, inference(avatar_split_clause, [], [f1970, f1253])).
fof(f1970, plain, (op1(e10, e10) = e13), inference(backward_demodulation, [], [f581, f578])).
fof(f581, plain, (e13 = op1(op1(e14, op1(e14, e14)), op1(e14, op1(e14, e14)))), inference(cnf_transformation, [], [f14])).
fof(f1968, plain, spl28_2, inference(avatar_split_clause, [], [f579, f741])).
fof(f579, plain, (e11 = op1(e14, e14)), inference(cnf_transformation, [], [f14])).
fof(f1967, plain, (spl28_258 | spl28_257 | spl28_256 | spl28_255 | ~ spl28_226), inference(avatar_split_clause, [], [f573, f1732, f1928, f1937, f1946, f1955])).
fof(f1955, plain, (spl28_258 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl28_258])])).
fof(f1946, plain, (spl28_257 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl28_257])])).
fof(f1937, plain, (spl28_256 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl28_256])])).
fof(f1928, plain, (spl28_255 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl28_255])])).
fof(f573, plain, (~ (e20 = op2(e20, e24)) | sP7 | sP6 | sP5 | sP4), inference(cnf_transformation, [], [f33])).
fof(f33, plain, ((~ (e24 = op2(e24, e24)) & ~ (e23 = op2(e23, e24)) & ~ (e22 = op2(e22, e24)) & ~ (e21 = op2(e21, e24)) & ~ (e20 = op2(e20, e24))) | sP7 | sP6 | sP5 | sP4), inference(definition_folding, [], [f13, e32, e31, e30, e29])).
fof(f29, plain, ((~ (e24 = op2(e24, e20)) & ~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20))) | ~ sP4), inference(usedef, [], [e29])).
fof(e29, plain, (sP4 <=> (~ (e24 = op2(e24, e20)) & ~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f30, plain, ((~ (e24 = op2(e24, e21)) & ~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21))) | ~ sP5), inference(usedef, [], [e30])).
fof(e30, plain, (sP5 <=> (~ (e24 = op2(e24, e21)) & ~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f31, plain, ((~ (e24 = op2(e24, e22)) & ~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22))) | ~ sP6), inference(usedef, [], [e31])).
fof(e31, plain, (sP6 <=> (~ (e24 = op2(e24, e22)) & ~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f32, plain, ((~ (e24 = op2(e24, e23)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e22, e23)) & ~ (e21 = op2(e21, e23)) & ~ (e20 = op2(e20, e23))) | ~ sP7), inference(usedef, [], [e32])).
fof(e32, plain, (sP7 <=> (~ (e24 = op2(e24, e23)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e22, e23)) & ~ (e21 = op2(e21, e23)) & ~ (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f13, plain, ((~ (e24 = op2(e24, e24)) & ~ (e23 = op2(e23, e24)) & ~ (e22 = op2(e22, e24)) & ~ (e21 = op2(e21, e24)) & ~ (e20 = op2(e20, e24))) | (~ (e24 = op2(e24, e23)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e22, e23)) & ~ (e21 = op2(e21, e23)) & ~ (e20 = op2(e20, e23))) | (~ (e24 = op2(e24, e22)) & ~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22))) | (~ (e24 = op2(e24, e21)) & ~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21))) | (~ (e24 = op2(e24, e20)) & ~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax13)).
fof(f1961, plain, (~ spl28_258 | ~ spl28_222), inference(avatar_split_clause, [], [f569, f1715, f1955])).
fof(f569, plain, (~ (e21 = op2(e21, e20)) | ~ sP4), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e24 = op2(e24, e20)) & ~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20))) | ~ sP4), inference(nnf_transformation, [], [f29])).
fof(f1950, plain, (~ spl28_257 | ~ spl28_169), inference(avatar_split_clause, [], [f566, f1492, f1946])).
fof(f1492, plain, (spl28_169 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_169])])).
fof(f566, plain, (~ (e23 = op2(e23, e21)) | ~ sP5), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e24 = op2(e24, e21)) & ~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21))) | ~ sP5), inference(nnf_transformation, [], [f30])).
fof(f1942, plain, (~ spl28_256 | ~ spl28_188), inference(avatar_split_clause, [], [f560, f1572, f1937])).
fof(f560, plain, (~ (e22 = op2(e22, e22)) | ~ sP6), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e24 = op2(e24, e22)) & ~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22))) | ~ sP6), inference(nnf_transformation, [], [f31])).
fof(f1931, plain, (~ spl28_255 | ~ spl28_135), inference(avatar_split_clause, [], [f557, f1349, f1928])).
fof(f557, plain, (~ (e24 = op2(e24, e23)) | ~ sP7), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e24 = op2(e24, e23)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e22, e23)) & ~ (e21 = op2(e21, e23)) & ~ (e20 = op2(e20, e23))) | ~ sP7), inference(nnf_transformation, [], [f32])).
fof(f1926, plain, (spl28_254 | spl28_253 | spl28_252 | spl28_251 | ~ spl28_101), inference(avatar_split_clause, [], [f548, f1157, f1887, f1896, f1905, f1914])).
fof(f1914, plain, (spl28_254 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl28_254])])).
fof(f1905, plain, (spl28_253 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl28_253])])).
fof(f1896, plain, (spl28_252 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl28_252])])).
fof(f1887, plain, (spl28_251 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl28_251])])).
fof(f548, plain, (~ (e10 = op1(e10, e14)) | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ((~ (e14 = op1(e14, e14)) & ~ (e13 = op1(e13, e14)) & ~ (e12 = op1(e12, e14)) & ~ (e11 = op1(e11, e14)) & ~ (e10 = op1(e10, e14))) | sP3 | sP2 | sP1 | sP0), inference(definition_folding, [], [f12, e27, e26, e25, e24])).
fof(f24, plain, ((~ (e14 = op1(e14, e10)) & ~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e24])).
fof(e24, plain, (sP0 <=> (~ (e14 = op1(e14, e10)) & ~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f25, plain, ((~ (e14 = op1(e14, e11)) & ~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e25])).
fof(e25, plain, (sP1 <=> (~ (e14 = op1(e14, e11)) & ~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f26, plain, ((~ (e14 = op1(e14, e12)) & ~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e26])).
fof(e26, plain, (sP2 <=> (~ (e14 = op1(e14, e12)) & ~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f27, plain, ((~ (e14 = op1(e14, e13)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e12, e13)) & ~ (e11 = op1(e11, e13)) & ~ (e10 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e27])).
fof(e27, plain, (sP3 <=> (~ (e14 = op1(e14, e13)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e12, e13)) & ~ (e11 = op1(e11, e13)) & ~ (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f12, plain, ((~ (e14 = op1(e14, e14)) & ~ (e13 = op1(e13, e14)) & ~ (e12 = op1(e12, e14)) & ~ (e11 = op1(e11, e14)) & ~ (e10 = op1(e10, e14))) | (~ (e14 = op1(e14, e13)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e12, e13)) & ~ (e11 = op1(e11, e13)) & ~ (e10 = op1(e10, e13))) | (~ (e14 = op1(e14, e12)) & ~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12))) | (~ (e14 = op1(e14, e11)) & ~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11))) | (~ (e14 = op1(e14, e10)) & ~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax12)).
fof(f1920, plain, (~ spl28_254 | ~ spl28_97), inference(avatar_split_clause, [], [f544, f1140, f1914])).
fof(f544, plain, (~ (e11 = op1(e11, e10)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e14 = op1(e14, e10)) & ~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f24])).
fof(f1909, plain, (~ spl28_253 | ~ spl28_44), inference(avatar_split_clause, [], [f541, f917, f1905])).
fof(f541, plain, (~ (e13 = op1(e13, e11)) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e14 = op1(e14, e11)) & ~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f25])).
fof(f1901, plain, (~ spl28_252 | ~ spl28_63), inference(avatar_split_clause, [], [f535, f997, f1896])).
fof(f535, plain, (~ (e12 = op1(e12, e12)) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e14 = op1(e14, e12)) & ~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f26])).
fof(f1890, plain, (~ spl28_251 | ~ spl28_10), inference(avatar_split_clause, [], [f532, f774, f1887])).
fof(f532, plain, (~ (e14 = op1(e14, e13)) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e14 = op1(e14, e13)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e12, e13)) & ~ (e11 = op1(e11, e13)) & ~ (e10 = op1(e10, e13))) | ~ sP3), inference(nnf_transformation, [], [f27])).
fof(f1885, plain, (spl28_246 | spl28_241 | spl28_236 | spl28_231 | spl28_226), inference(avatar_split_clause, [], [f208, f1732, f1753, f1774, f1795, f1816])).
fof(f208, plain, ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (((e24 = op2(e24, e24)) | (e24 = op2(e23, e24)) | (e24 = op2(e22, e24)) | (e24 = op2(e21, e24)) | (e24 = op2(e20, e24))) & ((e24 = op2(e24, e24)) | (e24 = op2(e24, e23)) | (e24 = op2(e24, e22)) | (e24 = op2(e24, e21)) | (e24 = op2(e24, e20))) & ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))) & ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))) & ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))) & ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))) & ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))) & ((e21 = op2(e24, e24)) | (e21 = op2(e24, e23)) | (e21 = op2(e24, e22)) | (e21 = op2(e24, e21)) | (e21 = op2(e24, e20))) & ((e20 = op2(e24, e24)) | (e20 = op2(e23, e24)) | (e20 = op2(e22, e24)) | (e20 = op2(e21, e24)) | (e20 = op2(e20, e24))) & ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))) & ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))) & ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))) & ((e23 = op2(e24, e23)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e24, e23)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e24 = op2(e24, e22)) | (e24 = op2(e23, e22)) | (e24 = op2(e22, e22)) | (e24 = op2(e21, e22)) | (e24 = op2(e20, e22))) & ((e24 = op2(e22, e24)) | (e24 = op2(e22, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e22, e21)) | (e24 = op2(e22, e20))) & ((e23 = op2(e24, e22)) | (e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e24)) | (e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e24, e22)) | (e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e24)) | (e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e24)) | (e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e24, e22)) | (e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))) & ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))) & ((e23 = op2(e24, e21)) | (e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e24)) | (e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e24)) | (e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e24, e21)) | (e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e24, e21)) | (e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e24)) | (e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e24 = op2(e24, e20)) | (e24 = op2(e23, e20)) | (e24 = op2(e22, e20)) | (e24 = op2(e21, e20)) | (op2(e20, e20) = e24)) & ((e24 = op2(e20, e24)) | (e24 = op2(e20, e23)) | (e24 = op2(e20, e22)) | (e24 = op2(e20, e21)) | (op2(e20, e20) = e24)) & ((e23 = op2(e24, e20)) | (e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e24)) | (e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e24, e20)) | (e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e24)) | (e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e24, e20)) | (e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e24)) | (e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax5)).
fof(f1884, plain, (spl28_246 | spl28_221 | spl28_196 | spl28_171 | spl28_146), inference(avatar_split_clause, [], [f209, f1396, f1501, f1606, f1711, f1816])).
fof(f209, plain, ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f5])).
fof(f1883, plain, (spl28_247 | spl28_242 | spl28_237 | spl28_232 | spl28_227), inference(avatar_split_clause, [], [f210, f1736, f1757, f1778, f1799, f1820])).
fof(f210, plain, ((e21 = op2(e20, e24)) | (e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f5])).
fof(f1882, plain, (spl28_247 | spl28_222 | spl28_197 | spl28_172 | spl28_147), inference(avatar_split_clause, [], [f211, f1400, f1505, f1610, f1715, f1820])).
fof(f211, plain, ((e21 = op2(e24, e20)) | (e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f5])).
fof(f1873, plain, (spl28_222 | spl28_217 | spl28_212 | spl28_207 | spl28_202), inference(avatar_split_clause, [], [f220, f1631, f1652, f1673, f1694, f1715])).
fof(f220, plain, ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f5])).
fof(f1860, plain, (spl28_238 | spl28_213 | spl28_188 | spl28_163 | spl28_138), inference(avatar_split_clause, [], [f233, f1362, f1467, f1572, f1677, f1782])).
fof(f233, plain, ((e22 = op2(e24, e22)) | (e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f5])).
fof(f1858, plain, (spl28_239 | spl28_214 | spl28_189 | spl28_164 | spl28_139), inference(avatar_split_clause, [], [f235, f1366, f1471, f1576, f1681, f1786])).
fof(f235, plain, ((e23 = op2(e24, e22)) | (e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f5])).
fof(f1853, plain, (spl28_172 | spl28_167 | spl28_162 | spl28_157 | spl28_152), inference(avatar_split_clause, [], [f240, f1421, f1442, f1463, f1484, f1505])).
fof(f240, plain, ((e21 = op2(e23, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f5])).
fof(f1846, plain, (spl28_235 | spl28_210 | spl28_185 | spl28_160 | spl28_135), inference(avatar_split_clause, [], [f247, f1349, f1454, f1559, f1664, f1769])).
fof(f247, plain, ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))), inference(cnf_transformation, [], [f5])).
fof(f1844, plain, (spl28_226 | spl28_201 | spl28_176 | spl28_151 | spl28_126), inference(avatar_split_clause, [], [f249, f1312, f1417, f1522, f1627, f1732])).
fof(f249, plain, ((e20 = op2(e24, e24)) | (e20 = op2(e23, e24)) | (e20 = op2(e22, e24)) | (e20 = op2(e21, e24)) | (e20 = op2(e20, e24))), inference(cnf_transformation, [], [f5])).
fof(f1840, plain, (spl28_228 | spl28_203 | spl28_178 | spl28_153 | spl28_128), inference(avatar_split_clause, [], [f253, f1320, f1425, f1530, f1635, f1740])).
fof(f253, plain, ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))), inference(cnf_transformation, [], [f5])).
fof(f1839, plain, (spl28_149 | spl28_144 | spl28_139 | spl28_134 | spl28_129), inference(avatar_split_clause, [], [f254, f1324, f1345, f1366, f1387, f1408])).
fof(f254, plain, ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))), inference(cnf_transformation, [], [f5])).
fof(f1793, plain, (spl28_236 | spl28_237 | spl28_238 | spl28_239 | spl28_240), inference(avatar_split_clause, [], [f185, f1790, f1786, f1782, f1778, f1774])).
fof(f185, plain, ((e24 = op2(e20, e22)) | (e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e24 = op2(e24, e24)) | (e23 = op2(e24, e24)) | (e22 = op2(e24, e24)) | (e21 = op2(e24, e24)) | (e20 = op2(e24, e24))) & ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))) & ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))) & ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))) & ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))) & ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))) & ((e24 = op2(e23, e23)) | (e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e24 = op2(e23, e22)) | (e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))) & ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))) & ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e24 = op2(e21, e21)) | (e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e24 = op2(e21, e20)) | (e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e24 = op2(e20, e24)) | (e23 = op2(e20, e24)) | (e22 = op2(e20, e24)) | (e21 = op2(e20, e24)) | (e20 = op2(e20, e24))) & ((e24 = op2(e20, e23)) | (e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e24 = op2(e20, e22)) | (e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e24 = op2(e20, e21)) | (e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e24) | (op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax4)).
fof(f1751, plain, (spl28_226 | spl28_227 | spl28_228 | spl28_229 | spl28_230), inference(avatar_split_clause, [], [f187, f1748, f1744, f1740, f1736, f1732])).
fof(f187, plain, ((e24 = op2(e20, e24)) | (e23 = op2(e20, e24)) | (e22 = op2(e20, e24)) | (e21 = op2(e20, e24)) | (e20 = op2(e20, e24))), inference(cnf_transformation, [], [f4])).
fof(f1730, plain, (spl28_221 | spl28_222 | spl28_223 | spl28_224 | spl28_225), inference(avatar_split_clause, [], [f188, f1727, f1723, f1719, f1715, f1711])).
fof(f188, plain, ((e24 = op2(e21, e20)) | (e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1604, plain, (spl28_191 | spl28_192 | spl28_193 | spl28_194 | spl28_195), inference(avatar_split_clause, [], [f194, f1601, f1597, f1593, f1589, f1585])).
fof(f194, plain, ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f4])).
fof(f1583, plain, (spl28_186 | spl28_187 | spl28_188 | spl28_189 | spl28_190), inference(avatar_split_clause, [], [f195, f1580, f1576, f1572, f1568, f1564])).
fof(f195, plain, ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f4])).
fof(f1520, plain, (spl28_171 | spl28_172 | spl28_173 | spl28_174 | spl28_175), inference(avatar_split_clause, [], [f198, f1517, f1513, f1509, f1505, f1501])).
fof(f198, plain, ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1499, plain, (spl28_166 | spl28_167 | spl28_168 | spl28_169 | spl28_170), inference(avatar_split_clause, [], [f199, f1496, f1492, f1488, f1484, f1480])).
fof(f199, plain, ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))), inference(cnf_transformation, [], [f4])).
fof(f1436, plain, (spl28_151 | spl28_152 | spl28_153 | spl28_154 | spl28_155), inference(avatar_split_clause, [], [f202, f1433, f1429, f1425, f1421, f1417])).
fof(f202, plain, ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))), inference(cnf_transformation, [], [f4])).
fof(f1415, plain, (spl28_146 | spl28_147 | spl28_148 | spl28_149 | spl28_150), inference(avatar_split_clause, [], [f203, f1412, f1408, f1404, f1400, f1396])).
fof(f203, plain, ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))), inference(cnf_transformation, [], [f4])).
fof(f1373, plain, (spl28_136 | spl28_137 | spl28_138 | spl28_139 | spl28_140), inference(avatar_split_clause, [], [f205, f1370, f1366, f1362, f1358, f1354])).
fof(f205, plain, ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))), inference(cnf_transformation, [], [f4])).
fof(f1310, plain, (spl28_121 | spl28_116 | spl28_111 | spl28_106 | spl28_101), inference(avatar_split_clause, [], [f108, f1157, f1178, f1199, f1220, f1241])).
fof(f108, plain, ((e10 = op1(e10, e14)) | (e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e14 = op1(e14, e14)) | (e14 = op1(e13, e14)) | (e14 = op1(e12, e14)) | (e14 = op1(e11, e14)) | (e14 = op1(e10, e14))) & ((e14 = op1(e14, e14)) | (e14 = op1(e14, e13)) | (e14 = op1(e14, e12)) | (e14 = op1(e14, e11)) | (e14 = op1(e14, e10))) & ((e13 = op1(e14, e14)) | (e13 = op1(e13, e14)) | (e13 = op1(e12, e14)) | (e13 = op1(e11, e14)) | (e13 = op1(e10, e14))) & ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))) & ((e12 = op1(e14, e14)) | (e12 = op1(e13, e14)) | (e12 = op1(e12, e14)) | (e12 = op1(e11, e14)) | (e12 = op1(e10, e14))) & ((e12 = op1(e14, e14)) | (e12 = op1(e14, e13)) | (e12 = op1(e14, e12)) | (e12 = op1(e14, e11)) | (e12 = op1(e14, e10))) & ((e11 = op1(e14, e14)) | (e11 = op1(e13, e14)) | (e11 = op1(e12, e14)) | (e11 = op1(e11, e14)) | (e11 = op1(e10, e14))) & ((e11 = op1(e14, e14)) | (e11 = op1(e14, e13)) | (e11 = op1(e14, e12)) | (e11 = op1(e14, e11)) | (e11 = op1(e14, e10))) & ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))) & ((e10 = op1(e14, e14)) | (e10 = op1(e14, e13)) | (e10 = op1(e14, e12)) | (e10 = op1(e14, e11)) | (e10 = op1(e14, e10))) & ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))) & ((e14 = op1(e13, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e13, e12)) | (e14 = op1(e13, e11)) | (e14 = op1(e13, e10))) & ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e14, e13)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e14 = op1(e14, e12)) | (e14 = op1(e13, e12)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e12)) | (e14 = op1(e10, e12))) & ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))) & ((e13 = op1(e14, e12)) | (e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e14)) | (e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e14, e12)) | (e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e14)) | (e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e14, e12)) | (e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e14 = op1(e14, e11)) | (e14 = op1(e13, e11)) | (e14 = op1(e12, e11)) | (e14 = op1(e11, e11)) | (e14 = op1(e10, e11))) & ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))) & ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e14)) | (e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e14)) | (e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e14, e11)) | (e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e14)) | (e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e14, e11)) | (e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e14)) | (e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e14 = op1(e14, e10)) | (e14 = op1(e13, e10)) | (e14 = op1(e12, e10)) | (e14 = op1(e11, e10)) | (op1(e10, e10) = e14)) & ((e14 = op1(e10, e14)) | (e14 = op1(e10, e13)) | (e14 = op1(e10, e12)) | (e14 = op1(e10, e11)) | (op1(e10, e10) = e14)) & ((e13 = op1(e14, e10)) | (e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e14)) | (e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e14, e10)) | (e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e14)) | (e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e14, e10)) | (e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e14)) | (e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e14, e10)) | (e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e14)) | (e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax2)).
fof(f1309, plain, (spl28_121 | spl28_96 | spl28_71 | spl28_46 | spl28_21), inference(avatar_split_clause, [], [f109, f821, f926, f1031, f1136, f1241])).
fof(f109, plain, ((e10 = op1(e14, e10)) | (e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f1298, plain, (spl28_97 | spl28_92 | spl28_87 | spl28_82 | spl28_77), inference(avatar_split_clause, [], [f120, f1056, f1077, f1098, f1119, f1140])).
fof(f120, plain, ((e11 = op1(e11, e14)) | (e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1289, plain, (spl28_111 | spl28_86 | spl28_61 | spl28_36 | spl28_11), inference(avatar_split_clause, [], [f129, f779, f884, f989, f1094, f1199])).
fof(f129, plain, ((e10 = op1(e14, e12)) | (e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1283, plain, (spl28_114 | spl28_89 | spl28_64 | spl28_39 | spl28_14), inference(avatar_split_clause, [], [f135, f791, f896, f1001, f1106, f1211])).
fof(f135, plain, ((e13 = op1(e14, e12)) | (e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1282, plain, (spl28_75 | spl28_70 | spl28_65 | spl28_60 | spl28_55), inference(avatar_split_clause, [], [f136, f963, f984, f1005, f1026, f1047])).
fof(f136, plain, ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1276, plain, (spl28_48 | spl28_43 | spl28_38 | spl28_33 | spl28_28), inference(avatar_split_clause, [], [f142, f850, f871, f892, f913, f934])).
fof(f142, plain, ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1275, plain, (spl28_108 | spl28_83 | spl28_58 | spl28_33 | spl28_8), inference(avatar_split_clause, [], [f143, f766, f871, f976, f1081, f1186])).
fof(f143, plain, ((e12 = op1(e14, e13)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1273, plain, (spl28_109 | spl28_84 | spl28_59 | spl28_34 | spl28_9), inference(avatar_split_clause, [], [f145, f770, f875, f980, f1085, f1190])).
fof(f145, plain, ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1272, plain, (spl28_50 | spl28_45 | spl28_40 | spl28_35 | spl28_30), inference(avatar_split_clause, [], [f146, f858, f879, f900, f921, f942])).
fof(f146, plain, ((e14 = op1(e13, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e13, e12)) | (e14 = op1(e13, e11)) | (e14 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1271, plain, (spl28_110 | spl28_85 | spl28_60 | spl28_35 | spl28_10), inference(avatar_split_clause, [], [f147, f774, f879, f984, f1089, f1194])).
fof(f147, plain, ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1197, plain, (spl28_106 | spl28_107 | spl28_108 | spl28_109 | spl28_110), inference(avatar_split_clause, [], [f86, f1194, f1190, f1186, f1182, f1178])).
fof(f86, plain, ((e14 = op1(e10, e13)) | (e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e14 = op1(e14, e14)) | (e13 = op1(e14, e14)) | (e12 = op1(e14, e14)) | (e11 = op1(e14, e14)) | (e10 = op1(e14, e14))) & ((e14 = op1(e14, e13)) | (e13 = op1(e14, e13)) | (e12 = op1(e14, e13)) | (e11 = op1(e14, e13)) | (e10 = op1(e14, e13))) & ((e14 = op1(e14, e12)) | (e13 = op1(e14, e12)) | (e12 = op1(e14, e12)) | (e11 = op1(e14, e12)) | (e10 = op1(e14, e12))) & ((e14 = op1(e14, e11)) | (e13 = op1(e14, e11)) | (e12 = op1(e14, e11)) | (e11 = op1(e14, e11)) | (e10 = op1(e14, e11))) & ((e14 = op1(e14, e10)) | (e13 = op1(e14, e10)) | (e12 = op1(e14, e10)) | (e11 = op1(e14, e10)) | (e10 = op1(e14, e10))) & ((e14 = op1(e13, e14)) | (e13 = op1(e13, e14)) | (e12 = op1(e13, e14)) | (e11 = op1(e13, e14)) | (e10 = op1(e13, e14))) & ((e14 = op1(e13, e13)) | (e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e14 = op1(e13, e12)) | (e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e14 = op1(e13, e11)) | (e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))) & ((e14 = op1(e12, e13)) | (e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e14 = op1(e12, e12)) | (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e14 = op1(e12, e10)) | (e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))) & ((e14 = op1(e11, e13)) | (e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e14 = op1(e11, e12)) | (e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e14 = op1(e11, e11)) | (e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e14 = op1(e11, e10)) | (e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e14 = op1(e10, e14)) | (e13 = op1(e10, e14)) | (e12 = op1(e10, e14)) | (e11 = op1(e10, e14)) | (e10 = op1(e10, e14))) & ((e14 = op1(e10, e13)) | (e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e14 = op1(e10, e12)) | (e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e14 = op1(e10, e11)) | (e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e14) | (op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG166+1.p', ax1)).
fof(f1176, plain, (spl28_101 | spl28_102 | spl28_103 | spl28_104 | spl28_105), inference(avatar_split_clause, [], [f87, f1173, f1169, f1165, f1161, f1157])).
fof(f87, plain, ((e14 = op1(e10, e14)) | (e13 = op1(e10, e14)) | (e12 = op1(e10, e14)) | (e11 = op1(e10, e14)) | (e10 = op1(e10, e14))), inference(cnf_transformation, [], [f1])).
fof(f1155, plain, (spl28_96 | spl28_97 | spl28_98 | spl28_99 | spl28_100), inference(avatar_split_clause, [], [f88, f1152, f1148, f1144, f1140, f1136])).
fof(f88, plain, ((e14 = op1(e11, e10)) | (e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1071, plain, (spl28_76 | spl28_77 | spl28_78 | spl28_79 | spl28_80), inference(avatar_split_clause, [], [f92, f1068, f1064, f1060, f1056, f1052])).
fof(f92, plain, ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))), inference(cnf_transformation, [], [f1])).
fof(f1050, plain, (spl28_71 | spl28_72 | spl28_73 | spl28_74 | spl28_75), inference(avatar_split_clause, [], [f93, f1047, f1043, f1039, f1035, f1031])).
fof(f93, plain, ((e14 = op1(e12, e10)) | (e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f1029, plain, (spl28_66 | spl28_67 | spl28_68 | spl28_69 | spl28_70), inference(avatar_split_clause, [], [f94, f1026, f1022, f1018, f1014, f1010])).
fof(f94, plain, ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f1008, plain, (spl28_61 | spl28_62 | spl28_63 | spl28_64 | spl28_65), inference(avatar_split_clause, [], [f95, f1005, f1001, f997, f993, f989])).
fof(f95, plain, ((e14 = op1(e12, e12)) | (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f1])).
fof(f966, plain, (spl28_51 | spl28_52 | spl28_53 | spl28_54 | spl28_55), inference(avatar_split_clause, [], [f97, f963, f959, f955, f951, f947])).
fof(f97, plain, ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))), inference(cnf_transformation, [], [f1])).
fof(f945, plain, (spl28_46 | spl28_47 | spl28_48 | spl28_49 | spl28_50), inference(avatar_split_clause, [], [f98, f942, f938, f934, f930, f926])).
fof(f98, plain, ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).
fof(f924, plain, (spl28_41 | spl28_42 | spl28_43 | spl28_44 | spl28_45), inference(avatar_split_clause, [], [f99, f921, f917, f913, f909, f905])).
fof(f99, plain, ((e14 = op1(e13, e11)) | (e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).
fof(f861, plain, (spl28_26 | spl28_27 | spl28_28 | spl28_29 | spl28_30), inference(avatar_split_clause, [], [f102, f858, f854, f850, f846, f842])).
fof(f102, plain, ((e14 = op1(e13, e14)) | (e13 = op1(e13, e14)) | (e12 = op1(e13, e14)) | (e11 = op1(e13, e14)) | (e10 = op1(e13, e14))), inference(cnf_transformation, [], [f1])).
fof(f840, plain, (spl28_21 | spl28_22 | spl28_23 | spl28_24 | spl28_25), inference(avatar_split_clause, [], [f103, f837, f833, f829, f825, f821])).
fof(f103, plain, ((e14 = op1(e14, e10)) | (e13 = op1(e14, e10)) | (e12 = op1(e14, e10)) | (e11 = op1(e14, e10)) | (e10 = op1(e14, e10))), inference(cnf_transformation, [], [f1])).