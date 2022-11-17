fof(f5904, plain, $false, inference(avatar_sat_refutation, [], [f1203, f1220, f1254, f1305, f1322, f1339, f1373, f1443, f1446, f1449, f1450, f1451, f1466, f1469, f1490, f1507, f1575, f1609, f1626, f1643, f1677, f1745, f1750, f1752, f1753, f1755, f1756, f1766, f1773, f1783, f1790, f1798, f1806, f1815, f1824, f1830, f1841, f1849, f1857, f1865, f1873, f1881, f1889, f1897, f1905, f1913, f1921, f1929, f1937, f1945, f1953, f1961, f1966, f1974, f1983, f1991, f2001, f2009, f2017, f2025, f2033, f2041, f2046, f2054, f2062, f2071, f2081, f2086, f2097, f2105, f2113, f2121, f2128, f2136, f2144, f2152, f2159, f2167, f2174, f2182, f2190, f2199, f2209, f2214, f2225, f2233, f2241, f2249, f2257, f2265, f2271, f2281, f2290, f2319, f2353, f2357, f2372, f2378, f2379, f2380, f2381, f2894, f2923, f2957, f2976, f2982, f2983, f2984, f2985, f2987, f2989, f3165, f3551, f3703, f3722, f3739, f3753, f3780, f3793, f3800, f3803, f3816, f3840, f3848, f3851, f3861, f3863, f3882, f3894, f3912, f3913, f3914, f3918, f3929, f3956, f3976, f4044, f4050, f4054, f4056, f4058, f4068, f4069, f4075, f4077, f4094, f4096, f4122, f4124, f4149, f4151, f4152, f4153, f4155, f4166, f4180, f4182, f4200, f4201, f4219, f4248, f4252, f4260, f4271, f4277, f4286, f4296, f4299, f4314, f4323, f4336, f4339, f4342, f4363, f4391, f4395, f4397, f4401, f4409, f4418, f4427, f4430, f4437, f4453, f4460, f4470, f4483, f4494, f4500, f4501, f4543, f4556, f4577, f4593, f4600, f4620, f4622, f4628, f4663, f4684, f4694, f4697, f4719, f4762, f4769, f4789, f4790, f4822, f4841, f4884, f4908, f4972, f5004, f5037, f5073, f5103, f5135, f5192, f5220, f5291, f5304, f5306, f5459, f5464, f5477, f5487, f5491, f5493, f5498, f5520, f5572, f5573, f5620, f5622, f5624, f5645, f5648, f5651, f5654, f5687, f5711, f5734, f5736, f5836, f5898])).
fof(f5898, plain, (~ spl144_125 | spl144_288), inference(avatar_contradiction_clause, [], [f5897])).
fof(f5897, plain, ($false | (~ spl144_125 | spl144_288)), inference(subsumption_resolution, [], [f5896, f1732])).
fof(f1732, plain, ((e20 = op2(e20, e20)) | ~ spl144_125), inference(avatar_component_clause, [], [f1730])).
fof(f1730, plain, (spl144_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_125])])).
fof(f5896, plain, (~ (e20 = op2(e20, e20)) | (~ spl144_125 | spl144_288)), inference(forward_demodulation, [], [f2956, f1732])).
fof(f2956, plain, (~ (e20 = op2(e20, op2(e20, e20))) | spl144_288), inference(avatar_component_clause, [], [f2954])).
fof(f2954, plain, (spl144_288 <=> (e20 = op2(e20, op2(e20, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_288])])).
fof(f5836, plain, (~ spl144_84 | spl144_275), inference(avatar_contradiction_clause, [], [f5835])).
fof(f5835, plain, ($false | (~ spl144_84 | spl144_275)), inference(subsumption_resolution, [], [f5834, f1557])).
fof(f1557, plain, ((e23 = op2(e22, e23)) | ~ spl144_84), inference(avatar_component_clause, [], [f1555])).
fof(f1555, plain, (spl144_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_84])])).
fof(f5834, plain, (~ (e23 = op2(e22, e23)) | (~ spl144_84 | spl144_275)), inference(forward_demodulation, [], [f2893, f1557])).
fof(f2893, plain, (~ (e23 = op2(e22, op2(e22, e23))) | spl144_275), inference(avatar_component_clause, [], [f2891])).
fof(f2891, plain, (spl144_275 <=> (e23 = op2(e22, op2(e22, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_275])])).
fof(f5736, plain, (~ spl144_103 | spl144_281), inference(avatar_contradiction_clause, [], [f5735])).
fof(f5735, plain, ($false | (~ spl144_103 | spl144_281)), inference(subsumption_resolution, [], [f5732, f1638])).
fof(f1638, plain, ((e22 = op2(e21, e22)) | ~ spl144_103), inference(avatar_component_clause, [], [f1636])).
fof(f1636, plain, (spl144_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_103])])).
fof(f5732, plain, (~ (e22 = op2(e21, e22)) | (~ spl144_103 | spl144_281)), inference(backward_demodulation, [], [f2922, f1638])).
fof(f2922, plain, (~ (e22 = op2(e21, op2(e21, e22))) | spl144_281), inference(avatar_component_clause, [], [f2920])).
fof(f2920, plain, (spl144_281 <=> (e22 = op2(e21, op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_281])])).
fof(f5734, plain, (~ spl144_99 | ~ spl144_103), inference(avatar_split_clause, [], [f5730, f1636, f1619])).
fof(f1619, plain, (spl144_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_99])])).
fof(f5730, plain, (~ (e22 = op2(e21, e23)) | ~ spl144_103), inference(backward_demodulation, [], [f491, f1638])).
fof(f491, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax6)).
fof(f5711, plain, (~ spl144_69 | ~ spl144_304), inference(avatar_split_clause, [], [f5698, f3084, f1492])).
fof(f1492, plain, (spl144_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_69])])).
fof(f3084, plain, (spl144_304 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_304])])).
fof(f5698, plain, (~ (e20 = op2(e23, e22)) | ~ spl144_304), inference(backward_demodulation, [], [f3025, f3085])).
fof(f3085, plain, ((e20 = h4(e10)) | ~ spl144_304), inference(avatar_component_clause, [], [f3084])).
fof(f3025, plain, ~ (op2(e23, e22) = h4(e10)), inference(backward_demodulation, [], [f503, f1103])).
fof(f1103, plain, (op2(e23, e23) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(op2(e23, e23), e23) = h4(e13)) & (h4(e12) = op2(op2(op2(e23, e23), e23), op2(e23, e23))) & (op2(e23, e23) = h4(e10)) & (e23 = h4(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax17)).
fof(f503, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f5687, plain, (~ spl144_69 | ~ spl144_316), inference(avatar_split_clause, [], [f5674, f3145, f1492])).
fof(f3145, plain, (spl144_316 <=> (e20 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_316])])).
fof(f5674, plain, (~ (e20 = op2(e23, e22)) | ~ spl144_316), inference(backward_demodulation, [], [f3013, f3146])).
fof(f3146, plain, ((e20 = h3(e10)) | ~ spl144_316), inference(avatar_component_clause, [], [f3145])).
fof(f3013, plain, ~ (op2(e23, e22) = h3(e10)), inference(backward_demodulation, [], [f473, f1099])).
fof(f1099, plain, (op2(e22, e22) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(op2(e22, e22), e22) = h3(e13)) & (h3(e12) = op2(op2(op2(e22, e22), e22), op2(e22, e22))) & (op2(e22, e22) = h3(e10)) & (e22 = h3(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax16)).
fof(f473, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f5654, plain, (~ spl144_349 | ~ spl144_4 | spl144_370), inference(avatar_split_clause, [], [f5653, f3492, f1183, f3344])).
fof(f3344, plain, (spl144_349 <=> (e23 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_349])])).
fof(f1183, plain, (spl144_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_4])])).
fof(f3492, plain, (spl144_370 <=> (h4(e10) = h2(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_370])])).
fof(f5653, plain, (~ (e23 = h4(e10)) | (~ spl144_4 | spl144_370)), inference(forward_demodulation, [], [f5652, f2999])).
fof(f2999, plain, (e23 = h2(e13)), inference(forward_demodulation, [], [f1097, f1089])).
fof(f1089, plain, (e23 = op2(op2(e21, e21), e21)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(op2(e21, e21), e21)) & (e22 = op2(op2(op2(e21, e21), e21), op2(e21, e21))) & (e20 = op2(e21, e21))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax13)).
fof(f1097, plain, (op2(op2(e21, e21), e21) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(op2(e21, e21), e21) = h2(e13)) & (op2(op2(op2(e21, e21), e21), op2(e21, e21)) = h2(e12)) & (op2(e21, e21) = h2(e10)) & (e21 = h2(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax15)).
fof(f5652, plain, (~ (h2(e13) = h4(e10)) | (~ spl144_4 | spl144_370)), inference(forward_demodulation, [], [f3494, f1185])).
fof(f1185, plain, ((e13 = op1(e13, e13)) | ~ spl144_4), inference(avatar_component_clause, [], [f1183])).
fof(f3494, plain, (~ (h4(e10) = h2(op1(e13, e13))) | spl144_370), inference(avatar_component_clause, [], [f3492])).
fof(f5651, plain, (~ spl144_368 | ~ spl144_24 | spl144_375), inference(avatar_split_clause, [], [f5650, f3512, f1268, f3454])).
fof(f3454, plain, (spl144_368 <=> (e23 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_368])])).
fof(f1268, plain, (spl144_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_24])])).
fof(f3512, plain, (spl144_375 <=> (h3(e10) = h2(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_375])])).
fof(f5650, plain, (~ (e23 = h3(e10)) | (~ spl144_24 | spl144_375)), inference(forward_demodulation, [], [f5649, f2999])).
fof(f5649, plain, (~ (h2(e13) = h3(e10)) | (~ spl144_24 | spl144_375)), inference(forward_demodulation, [], [f3514, f1270])).
fof(f1270, plain, ((e13 = op1(e12, e12)) | ~ spl144_24), inference(avatar_component_clause, [], [f1268])).
fof(f3514, plain, (~ (h3(e10) = h2(op1(e12, e12))) | spl144_375), inference(avatar_component_clause, [], [f3512])).
fof(f5648, plain, (~ spl144_102 | ~ spl144_38 | spl144_379), inference(avatar_split_clause, [], [f5647, f3528, f1328, f1632])).
fof(f1632, plain, (spl144_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_102])])).
fof(f1328, plain, (spl144_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_38])])).
fof(f3528, plain, (spl144_379 <=> (op2(e21, e22) = h2(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_379])])).
fof(f5647, plain, (~ (e21 = op2(e21, e22)) | (~ spl144_38 | spl144_379)), inference(forward_demodulation, [], [f5646, f1094])).
fof(f1094, plain, (e21 = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f5646, plain, (~ (op2(e21, e22) = h2(e11)) | (~ spl144_38 | spl144_379)), inference(forward_demodulation, [], [f3530, f1330])).
fof(f1330, plain, ((e11 = op1(e11, e12)) | ~ spl144_38), inference(avatar_component_clause, [], [f1328])).
fof(f3530, plain, (~ (op2(e21, e22) = h2(op1(e11, e12))) | spl144_379), inference(avatar_component_clause, [], [f3528])).
fof(f5645, plain, (~ spl144_119 | ~ spl144_55 | ~ spl144_318 | spl144_383), inference(avatar_split_clause, [], [f5644, f3544, f3162, f1400, f1704])).
fof(f1704, plain, (spl144_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_119])])).
fof(f1400, plain, (spl144_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_55])])).
fof(f3162, plain, (spl144_318 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_318])])).
fof(f3544, plain, (spl144_383 <=> (h2(op1(e10, e12)) = op2(h2(e10), e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_383])])).
fof(f5644, plain, (~ (e22 = op2(e20, e22)) | (~ spl144_55 | ~ spl144_318 | spl144_383)), inference(forward_demodulation, [], [f5643, f3001])).
fof(f3001, plain, (e22 = h2(e12)), inference(forward_demodulation, [], [f3000, f2988])).
fof(f2988, plain, (e22 = op2(e23, op2(e21, e21))), inference(forward_demodulation, [], [f1088, f1089])).
fof(f1088, plain, (e22 = op2(op2(op2(e21, e21), e21), op2(e21, e21))), inference(cnf_transformation, [], [f13])).
fof(f3000, plain, (h2(e12) = op2(e23, op2(e21, e21))), inference(forward_demodulation, [], [f1096, f1089])).
fof(f1096, plain, (op2(op2(op2(e21, e21), e21), op2(e21, e21)) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f5643, plain, (~ (op2(e20, e22) = h2(e12)) | (~ spl144_55 | ~ spl144_318 | spl144_383)), inference(forward_demodulation, [], [f5642, f1402])).
fof(f1402, plain, ((e12 = op1(e10, e12)) | ~ spl144_55), inference(avatar_component_clause, [], [f1400])).
fof(f5642, plain, (~ (op2(e20, e22) = h2(op1(e10, e12))) | (~ spl144_318 | spl144_383)), inference(forward_demodulation, [], [f3546, f3163])).
fof(f3163, plain, ((e20 = h2(e10)) | ~ spl144_318), inference(avatar_component_clause, [], [f3162])).
fof(f3546, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | spl144_383), inference(avatar_component_clause, [], [f3544])).
fof(f5624, plain, (~ spl144_100 | ~ spl144_112), inference(avatar_split_clause, [], [f5623, f1674, f1623])).
fof(f1623, plain, (spl144_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_100])])).
fof(f1674, plain, (spl144_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_112])])).
fof(f5623, plain, (~ (e23 = op2(e21, e23)) | ~ spl144_112), inference(forward_demodulation, [], [f488, f1676])).
fof(f1676, plain, ((e23 = op2(e21, e20)) | ~ spl144_112), inference(avatar_component_clause, [], [f1674])).
fof(f488, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f5622, plain, (~ spl144_82 | ~ spl144_94), inference(avatar_split_clause, [], [f5621, f1598, f1547])).
fof(f1547, plain, (spl144_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_82])])).
fof(f1598, plain, (spl144_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_94])])).
fof(f5621, plain, (~ (e21 = op2(e22, e23)) | ~ spl144_94), inference(forward_demodulation, [], [f494, f1600])).
fof(f1600, plain, ((e21 = op2(e22, e20)) | ~ spl144_94), inference(avatar_component_clause, [], [f1598])).
fof(f494, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f5620, plain, (~ spl144_1 | ~ spl144_4), inference(avatar_contradiction_clause, [], [f5619])).
fof(f5619, plain, ($false | (~ spl144_1 | ~ spl144_4)), inference(subsumption_resolution, [], [f5617, f506])).
fof(f506, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax7)).
fof(f5617, plain, ((e10 = e13) | (~ spl144_1 | ~ spl144_4)), inference(backward_demodulation, [], [f1185, f1173])).
fof(f1173, plain, ((e10 = op1(e13, e13)) | ~ spl144_1), inference(avatar_component_clause, [], [f1171])).
fof(f1171, plain, (spl144_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_1])])).
fof(f5573, plain, (~ spl144_104 | ~ spl144_112), inference(avatar_split_clause, [], [f5570, f1674, f1640])).
fof(f1640, plain, (spl144_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_104])])).
fof(f5570, plain, (~ (e23 = op2(e21, e22)) | ~ spl144_112), inference(backward_demodulation, [], [f487, f1676])).
fof(f487, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f5572, plain, (~ spl144_96 | ~ spl144_112), inference(avatar_split_clause, [], [f5569, f1674, f1606])).
fof(f1606, plain, (spl144_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_96])])).
fof(f5569, plain, (~ (e23 = op2(e22, e20)) | ~ spl144_112), inference(backward_demodulation, [], [f459, f1676])).
fof(f459, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f5520, plain, (~ spl144_72 | ~ spl144_368), inference(avatar_split_clause, [], [f5509, f3454, f1504])).
fof(f1504, plain, (spl144_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_72])])).
fof(f5509, plain, (~ (e23 = op2(e23, e22)) | ~ spl144_368), inference(backward_demodulation, [], [f3013, f3455])).
fof(f3455, plain, ((e23 = h3(e10)) | ~ spl144_368), inference(avatar_component_clause, [], [f3454])).
fof(f5498, plain, (~ spl144_19 | ~ spl144_27), inference(avatar_split_clause, [], [f4826, f1281, f1247])).
fof(f1247, plain, (spl144_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_19])])).
fof(f1281, plain, (spl144_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_27])])).
fof(f4826, plain, (~ (e12 = op1(e12, e13)) | ~ spl144_27), inference(backward_demodulation, [], [f448, f1283])).
fof(f1283, plain, ((e12 = op1(e12, e11)) | ~ spl144_27), inference(avatar_component_clause, [], [f1281])).
fof(f448, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax5)).
fof(f5493, plain, (~ spl144_8 | ~ spl144_4), inference(avatar_split_clause, [], [f5492, f1183, f1200])).
fof(f1200, plain, (spl144_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_8])])).
fof(f5492, plain, (~ (e13 = op1(e13, e12)) | ~ spl144_4), inference(forward_demodulation, [], [f455, f1185])).
fof(f455, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f5491, plain, (~ spl144_6 | ~ spl144_10), inference(avatar_split_clause, [], [f5490, f1209, f1192])).
fof(f1192, plain, (spl144_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_6])])).
fof(f1209, plain, (spl144_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_10])])).
fof(f5490, plain, (~ (e11 = op1(e13, e12)) | ~ spl144_10), inference(forward_demodulation, [], [f453, f1211])).
fof(f1211, plain, ((e11 = op1(e13, e11)) | ~ spl144_10), inference(avatar_component_clause, [], [f1209])).
fof(f453, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f5487, plain, (~ spl144_7 | ~ spl144_55), inference(avatar_split_clause, [], [f5486, f1400, f1196])).
fof(f1196, plain, (spl144_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_7])])).
fof(f5486, plain, (~ (e12 = op1(e13, e12)) | ~ spl144_55), inference(forward_demodulation, [], [f422, f1402])).
fof(f422, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f5477, plain, (spl144_67 | ~ spl144_72 | ~ spl144_290), inference(avatar_contradiction_clause, [], [f5476])).
fof(f5476, plain, ($false | (spl144_67 | ~ spl144_72 | ~ spl144_290)), inference(subsumption_resolution, [], [f5475, f1484])).
fof(f1484, plain, (~ (e22 = op2(e23, e23)) | spl144_67), inference(avatar_component_clause, [], [f1483])).
fof(f1483, plain, (spl144_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_67])])).
fof(f5475, plain, ((e22 = op2(e23, e23)) | (~ spl144_72 | ~ spl144_290)), inference(backward_demodulation, [], [f2969, f1506])).
fof(f1506, plain, ((e23 = op2(e23, e22)) | ~ spl144_72), inference(avatar_component_clause, [], [f1504])).
fof(f2969, plain, ((e22 = op2(e23, op2(e23, e22))) | ~ spl144_290), inference(avatar_component_clause, [], [f2968])).
fof(f2968, plain, (spl144_290 <=> (e22 = op2(e23, op2(e23, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_290])])).
fof(f5464, plain, (~ spl144_72 | ~ spl144_104), inference(avatar_split_clause, [], [f5462, f1640, f1504])).
fof(f5462, plain, (~ (e23 = op2(e23, e22)) | ~ spl144_104), inference(backward_demodulation, [], [f472, f1642])).
fof(f1642, plain, ((e23 = op2(e21, e22)) | ~ spl144_104), inference(avatar_component_clause, [], [f1640])).
fof(f472, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f5459, plain, (~ spl144_102 | ~ spl144_110), inference(avatar_split_clause, [], [f5456, f1666, f1632])).
fof(f1666, plain, (spl144_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_110])])).
fof(f5456, plain, (~ (e21 = op2(e21, e22)) | ~ spl144_110), inference(backward_demodulation, [], [f487, f1668])).
fof(f1668, plain, ((e21 = op2(e21, e20)) | ~ spl144_110), inference(avatar_component_clause, [], [f1666])).
fof(f5306, plain, (~ spl144_70 | ~ spl144_74), inference(avatar_split_clause, [], [f5305, f1513, f1496])).
fof(f1496, plain, (spl144_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_70])])).
fof(f1513, plain, (spl144_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_74])])).
fof(f5305, plain, (~ (e21 = op2(e23, e22)) | ~ spl144_74), inference(forward_demodulation, [], [f501, f1515])).
fof(f1515, plain, ((e21 = op2(e23, e21)) | ~ spl144_74), inference(avatar_component_clause, [], [f1513])).
fof(f501, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f5304, plain, (~ spl144_71 | ~ spl144_79), inference(avatar_split_clause, [], [f5303, f1534, f1500])).
fof(f1500, plain, (spl144_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_71])])).
fof(f1534, plain, (spl144_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_79])])).
fof(f5303, plain, (~ (e22 = op2(e23, e22)) | ~ spl144_79), inference(forward_demodulation, [], [f499, f1536])).
fof(f1536, plain, ((e22 = op2(e23, e20)) | ~ spl144_79), inference(avatar_component_clause, [], [f1534])).
fof(f499, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f5291, plain, (~ spl144_123 | ~ spl144_124), inference(avatar_contradiction_clause, [], [f5290])).
fof(f5290, plain, ($false | (~ spl144_123 | ~ spl144_124)), inference(subsumption_resolution, [], [f5289, f515])).
fof(f515, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax8)).
fof(f5289, plain, ((e22 = e23) | (~ spl144_123 | ~ spl144_124)), inference(backward_demodulation, [], [f1727, f1723])).
fof(f1723, plain, ((e22 = op2(e20, e21)) | ~ spl144_123), inference(avatar_component_clause, [], [f1721])).
fof(f1721, plain, (spl144_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_123])])).
fof(f1727, plain, ((e23 = op2(e20, e21)) | ~ spl144_124), inference(avatar_component_clause, [], [f1725])).
fof(f1725, plain, (spl144_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_124])])).
fof(f5220, plain, (~ spl144_41 | spl144_380), inference(avatar_contradiction_clause, [], [f5219])).
fof(f5219, plain, ($false | (~ spl144_41 | spl144_380)), inference(trivial_inequality_removal, [], [f5218])).
fof(f5218, plain, (~ (h2(e10) = h2(e10)) | (~ spl144_41 | spl144_380)), inference(forward_demodulation, [], [f3534, f1343])).
fof(f1343, plain, ((e10 = op1(e11, e11)) | ~ spl144_41), inference(avatar_component_clause, [], [f1341])).
fof(f1341, plain, (spl144_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_41])])).
fof(f3534, plain, (~ (h2(e10) = h2(op1(e11, e11))) | spl144_380), inference(avatar_component_clause, [], [f3532])).
fof(f3532, plain, (spl144_380 <=> (h2(e10) = h2(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_380])])).
fof(f5192, plain, (~ spl144_50 | ~ spl144_114 | ~ spl144_318 | spl144_382), inference(avatar_contradiction_clause, [], [f5191])).
fof(f5191, plain, ($false | (~ spl144_50 | ~ spl144_114 | ~ spl144_318 | spl144_382)), inference(subsumption_resolution, [], [f5190, f1685])).
fof(f1685, plain, ((e21 = op2(e20, e23)) | ~ spl144_114), inference(avatar_component_clause, [], [f1683])).
fof(f1683, plain, (spl144_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_114])])).
fof(f5190, plain, (~ (e21 = op2(e20, e23)) | (~ spl144_50 | ~ spl144_318 | spl144_382)), inference(forward_demodulation, [], [f5189, f1094])).
fof(f5189, plain, (~ (op2(e20, e23) = h2(e11)) | (~ spl144_50 | ~ spl144_318 | spl144_382)), inference(forward_demodulation, [], [f5188, f1381])).
fof(f1381, plain, ((e11 = op1(e10, e13)) | ~ spl144_50), inference(avatar_component_clause, [], [f1379])).
fof(f1379, plain, (spl144_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_50])])).
fof(f5188, plain, (~ (op2(e20, e23) = h2(op1(e10, e13))) | (~ spl144_318 | spl144_382)), inference(forward_demodulation, [], [f3542, f3163])).
fof(f3542, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | spl144_382), inference(avatar_component_clause, [], [f3540])).
fof(f3540, plain, (spl144_382 <=> (h2(op1(e10, e13)) = op2(h2(e10), e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_382])])).
fof(f5135, plain, (~ spl144_30 | ~ spl144_94 | ~ spl144_318 | spl144_377), inference(avatar_contradiction_clause, [], [f5134])).
fof(f5134, plain, ($false | (~ spl144_30 | ~ spl144_94 | ~ spl144_318 | spl144_377)), inference(subsumption_resolution, [], [f5133, f1600])).
fof(f5133, plain, (~ (e21 = op2(e22, e20)) | (~ spl144_30 | ~ spl144_318 | spl144_377)), inference(forward_demodulation, [], [f5132, f1094])).
fof(f5132, plain, (~ (op2(e22, e20) = h2(e11)) | (~ spl144_30 | ~ spl144_318 | spl144_377)), inference(forward_demodulation, [], [f5131, f1296])).
fof(f1296, plain, ((e11 = op1(e12, e10)) | ~ spl144_30), inference(avatar_component_clause, [], [f1294])).
fof(f1294, plain, (spl144_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_30])])).
fof(f5131, plain, (~ (op2(e22, e20) = h2(op1(e12, e10))) | (~ spl144_318 | spl144_377)), inference(forward_demodulation, [], [f3522, f3163])).
fof(f3522, plain, (~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | spl144_377), inference(avatar_component_clause, [], [f3520])).
fof(f3520, plain, (spl144_377 <=> (h2(op1(e12, e10)) = op2(e22, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_377])])).
fof(f5103, plain, (~ spl144_27 | ~ spl144_91 | spl144_376), inference(avatar_contradiction_clause, [], [f5102])).
fof(f5102, plain, ($false | (~ spl144_27 | ~ spl144_91 | spl144_376)), inference(subsumption_resolution, [], [f5101, f1587])).
fof(f1587, plain, ((e22 = op2(e22, e21)) | ~ spl144_91), inference(avatar_component_clause, [], [f1585])).
fof(f1585, plain, (spl144_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_91])])).
fof(f5101, plain, (~ (e22 = op2(e22, e21)) | (~ spl144_27 | spl144_376)), inference(forward_demodulation, [], [f5100, f3001])).
fof(f5100, plain, (~ (op2(e22, e21) = h2(e12)) | (~ spl144_27 | spl144_376)), inference(forward_demodulation, [], [f3518, f1283])).
fof(f3518, plain, (~ (op2(e22, e21) = h2(op1(e12, e11))) | spl144_376), inference(avatar_component_clause, [], [f3516])).
fof(f3516, plain, (spl144_376 <=> (op2(e22, e21) = h2(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_376])])).
fof(f5073, plain, (~ spl144_17 | ~ spl144_81 | ~ spl144_318 | spl144_374), inference(avatar_contradiction_clause, [], [f5072])).
fof(f5072, plain, ($false | (~ spl144_17 | ~ spl144_81 | ~ spl144_318 | spl144_374)), inference(subsumption_resolution, [], [f5071, f1545])).
fof(f1545, plain, ((e20 = op2(e22, e23)) | ~ spl144_81), inference(avatar_component_clause, [], [f1543])).
fof(f1543, plain, (spl144_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_81])])).
fof(f5071, plain, (~ (e20 = op2(e22, e23)) | (~ spl144_17 | ~ spl144_318 | spl144_374)), inference(forward_demodulation, [], [f5070, f3163])).
fof(f5070, plain, (~ (op2(e22, e23) = h2(e10)) | (~ spl144_17 | spl144_374)), inference(forward_demodulation, [], [f3510, f1241])).
fof(f1241, plain, ((e10 = op1(e12, e13)) | ~ spl144_17), inference(avatar_component_clause, [], [f1239])).
fof(f1239, plain, (spl144_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_17])])).
fof(f3510, plain, (~ (op2(e22, e23) = h2(op1(e12, e13))) | spl144_374), inference(avatar_component_clause, [], [f3508])).
fof(f3508, plain, (spl144_374 <=> (op2(e22, e23) = h2(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_374])])).
fof(f5037, plain, (~ spl144_60 | spl144_384), inference(avatar_contradiction_clause, [], [f5036])).
fof(f5036, plain, ($false | (~ spl144_60 | spl144_384)), inference(subsumption_resolution, [], [f5035, f2999])).
fof(f5035, plain, (~ (e23 = h2(e13)) | (~ spl144_60 | spl144_384)), inference(forward_demodulation, [], [f3550, f1423])).
fof(f1423, plain, ((e13 = op1(e10, e11)) | ~ spl144_60), inference(avatar_component_clause, [], [f1421])).
fof(f1421, plain, (spl144_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_60])])).
fof(f3550, plain, (~ (e23 = h2(op1(e10, e11))) | spl144_384), inference(avatar_component_clause, [], [f3548])).
fof(f3548, plain, (spl144_384 <=> (e23 = h2(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_384])])).
fof(f5004, plain, (~ spl144_48 | ~ spl144_112 | ~ spl144_318 | spl144_381), inference(avatar_contradiction_clause, [], [f5003])).
fof(f5003, plain, ($false | (~ spl144_48 | ~ spl144_112 | ~ spl144_318 | spl144_381)), inference(subsumption_resolution, [], [f5002, f1676])).
fof(f5002, plain, (~ (e23 = op2(e21, e20)) | (~ spl144_48 | ~ spl144_318 | spl144_381)), inference(forward_demodulation, [], [f5001, f2999])).
fof(f5001, plain, (~ (op2(e21, e20) = h2(e13)) | (~ spl144_48 | ~ spl144_318 | spl144_381)), inference(forward_demodulation, [], [f5000, f1372])).
fof(f1372, plain, ((e13 = op1(e11, e10)) | ~ spl144_48), inference(avatar_component_clause, [], [f1370])).
fof(f1370, plain, (spl144_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_48])])).
fof(f5000, plain, (~ (op2(e21, e20) = h2(op1(e11, e10))) | (~ spl144_318 | spl144_381)), inference(forward_demodulation, [], [f3538, f3163])).
fof(f3538, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | spl144_381), inference(avatar_component_clause, [], [f3536])).
fof(f3536, plain, (spl144_381 <=> (h2(op1(e11, e10)) = op2(e21, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_381])])).
fof(f4972, plain, (~ spl144_35 | ~ spl144_99 | spl144_378), inference(avatar_contradiction_clause, [], [f4971])).
fof(f4971, plain, ($false | (~ spl144_35 | ~ spl144_99 | spl144_378)), inference(subsumption_resolution, [], [f4970, f1621])).
fof(f1621, plain, ((e22 = op2(e21, e23)) | ~ spl144_99), inference(avatar_component_clause, [], [f1619])).
fof(f4970, plain, (~ (e22 = op2(e21, e23)) | (~ spl144_35 | spl144_378)), inference(forward_demodulation, [], [f4969, f3001])).
fof(f4969, plain, (~ (op2(e21, e23) = h2(e12)) | (~ spl144_35 | spl144_378)), inference(forward_demodulation, [], [f3526, f1317])).
fof(f1317, plain, ((e12 = op1(e11, e13)) | ~ spl144_35), inference(avatar_component_clause, [], [f1315])).
fof(f1315, plain, (spl144_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_35])])).
fof(f3526, plain, (~ (op2(e21, e23) = h2(op1(e11, e13))) | spl144_378), inference(avatar_component_clause, [], [f3524])).
fof(f3524, plain, (spl144_378 <=> (op2(e21, e23) = h2(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_378])])).
fof(f4908, plain, (~ spl144_5 | ~ spl144_69 | ~ spl144_318 | spl144_371), inference(avatar_contradiction_clause, [], [f4907])).
fof(f4907, plain, ($false | (~ spl144_5 | ~ spl144_69 | ~ spl144_318 | spl144_371)), inference(subsumption_resolution, [], [f4906, f1494])).
fof(f1494, plain, ((e20 = op2(e23, e22)) | ~ spl144_69), inference(avatar_component_clause, [], [f1492])).
fof(f4906, plain, (~ (e20 = op2(e23, e22)) | (~ spl144_5 | ~ spl144_318 | spl144_371)), inference(forward_demodulation, [], [f4905, f3163])).
fof(f4905, plain, (~ (op2(e23, e22) = h2(e10)) | (~ spl144_5 | spl144_371)), inference(forward_demodulation, [], [f3498, f1190])).
fof(f1190, plain, ((e10 = op1(e13, e12)) | ~ spl144_5), inference(avatar_component_clause, [], [f1188])).
fof(f1188, plain, (spl144_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_5])])).
fof(f3498, plain, (~ (op2(e23, e22) = h2(op1(e13, e12))) | spl144_371), inference(avatar_component_clause, [], [f3496])).
fof(f3496, plain, (spl144_371 <=> (op2(e23, e22) = h2(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_371])])).
fof(f4884, plain, (~ spl144_61 | ~ spl144_125 | ~ spl144_318 | spl144_369), inference(avatar_contradiction_clause, [], [f4883])).
fof(f4883, plain, ($false | (~ spl144_61 | ~ spl144_125 | ~ spl144_318 | spl144_369)), inference(subsumption_resolution, [], [f4882, f1732])).
fof(f4882, plain, (~ (e20 = op2(e20, e20)) | (~ spl144_61 | ~ spl144_318 | spl144_369)), inference(forward_demodulation, [], [f4881, f3163])).
fof(f4881, plain, (~ (op2(e20, e20) = h2(e10)) | (~ spl144_61 | ~ spl144_318 | spl144_369)), inference(forward_demodulation, [], [f4880, f1428])).
fof(f1428, plain, ((e10 = op1(e10, e10)) | ~ spl144_61), inference(avatar_component_clause, [], [f1426])).
fof(f1426, plain, (spl144_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_61])])).
fof(f4880, plain, (~ (op2(e20, e20) = h2(op1(e10, e10))) | (~ spl144_318 | spl144_369)), inference(forward_demodulation, [], [f3490, f3163])).
fof(f3490, plain, (~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10))) | spl144_369), inference(avatar_component_clause, [], [f3488])).
fof(f3488, plain, (spl144_369 <=> (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_369])])).
fof(f4841, plain, (~ spl144_2 | ~ spl144_10), inference(avatar_split_clause, [], [f4839, f1209, f1175])).
fof(f1175, plain, (spl144_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_2])])).
fof(f4839, plain, (~ (e11 = op1(e13, e13)) | ~ spl144_10), inference(backward_demodulation, [], [f454, f1211])).
fof(f454, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4822, plain, (~ spl144_18 | ~ spl144_30), inference(avatar_split_clause, [], [f4821, f1294, f1243])).
fof(f1243, plain, (spl144_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_18])])).
fof(f4821, plain, (~ (e11 = op1(e12, e13)) | ~ spl144_30), inference(backward_demodulation, [], [f446, f1296])).
fof(f446, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4790, plain, (~ spl144_49 | ~ spl144_61), inference(avatar_split_clause, [], [f4784, f1426, f1375])).
fof(f1375, plain, (spl144_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_49])])).
fof(f4784, plain, (~ (e10 = op1(e10, e13)) | ~ spl144_61), inference(backward_demodulation, [], [f434, f1428])).
fof(f434, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f4789, plain, (~ spl144_29 | ~ spl144_61), inference(avatar_split_clause, [], [f4782, f1426, f1290])).
fof(f1290, plain, (spl144_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_29])])).
fof(f4782, plain, (~ (e10 = op1(e12, e10)) | ~ spl144_61), inference(backward_demodulation, [], [f409, f1428])).
fof(f409, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f4769, plain, (~ spl144_15 | spl144_373), inference(avatar_contradiction_clause, [], [f4768])).
fof(f4768, plain, ($false | (~ spl144_15 | spl144_373)), inference(subsumption_resolution, [], [f4767, f3001])).
fof(f4767, plain, (~ (e22 = h2(e12)) | (~ spl144_15 | spl144_373)), inference(forward_demodulation, [], [f3506, f1232])).
fof(f1232, plain, ((e12 = op1(e13, e10)) | ~ spl144_15), inference(avatar_component_clause, [], [f1230])).
fof(f1230, plain, (spl144_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_15])])).
fof(f3506, plain, (~ (e22 = h2(op1(e13, e10))) | spl144_373), inference(avatar_component_clause, [], [f3504])).
fof(f3504, plain, (spl144_373 <=> (e22 = h2(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_373])])).
fof(f4762, plain, (~ spl144_51 | ~ spl144_35), inference(avatar_split_clause, [], [f4761, f1315, f1383])).
fof(f1383, plain, (spl144_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_51])])).
fof(f4761, plain, (~ (e12 = op1(e10, e13)) | ~ spl144_35), inference(forward_demodulation, [], [f426, f1317])).
fof(f426, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4719, plain, (~ spl144_8 | ~ spl144_24), inference(avatar_split_clause, [], [f4718, f1268, f1200])).
fof(f4718, plain, (~ (e13 = op1(e13, e12)) | ~ spl144_24), inference(backward_demodulation, [], [f425, f1270])).
fof(f425, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4697, plain, (~ spl144_41 | ~ spl144_42), inference(avatar_contradiction_clause, [], [f4696])).
fof(f4696, plain, ($false | (~ spl144_41 | ~ spl144_42)), inference(subsumption_resolution, [], [f4695, f504])).
fof(f504, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f4695, plain, ((e10 = e11) | (~ spl144_41 | ~ spl144_42)), inference(forward_demodulation, [], [f1347, f1343])).
fof(f1347, plain, ((e11 = op1(e11, e11)) | ~ spl144_42), inference(avatar_component_clause, [], [f1345])).
fof(f1345, plain, (spl144_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_42])])).
fof(f4694, plain, (~ spl144_46 | ~ spl144_48), inference(avatar_contradiction_clause, [], [f4693])).
fof(f4693, plain, ($false | (~ spl144_46 | ~ spl144_48)), inference(subsumption_resolution, [], [f4692, f508])).
fof(f508, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f4692, plain, ((e11 = e13) | (~ spl144_46 | ~ spl144_48)), inference(backward_demodulation, [], [f1372, f1364])).
fof(f1364, plain, ((e11 = op1(e11, e10)) | ~ spl144_46), inference(avatar_component_clause, [], [f1362])).
fof(f1362, plain, (spl144_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_46])])).
fof(f4684, plain, (~ spl144_59 | ~ spl144_60), inference(avatar_contradiction_clause, [], [f4683])).
fof(f4683, plain, ($false | (~ spl144_59 | ~ spl144_60)), inference(subsumption_resolution, [], [f4682, f509])).
fof(f509, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f4682, plain, ((e12 = e13) | (~ spl144_59 | ~ spl144_60)), inference(backward_demodulation, [], [f1423, f1419])).
fof(f1419, plain, ((e12 = op1(e10, e11)) | ~ spl144_59), inference(avatar_component_clause, [], [f1417])).
fof(f1417, plain, (spl144_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_59])])).
fof(f4663, plain, (~ spl144_39 | spl144_199), inference(avatar_contradiction_clause, [], [f4662])).
fof(f4662, plain, ($false | (~ spl144_39 | spl144_199)), inference(subsumption_resolution, [], [f4661, f1334])).
fof(f1334, plain, ((e12 = op1(e11, e12)) | ~ spl144_39), inference(avatar_component_clause, [], [f1332])).
fof(f1332, plain, (spl144_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_39])])).
fof(f4661, plain, (~ (e12 = op1(e11, e12)) | (~ spl144_39 | spl144_199)), inference(forward_demodulation, [], [f2318, f1334])).
fof(f2318, plain, (~ (e12 = op1(e11, op1(e11, e12))) | spl144_199), inference(avatar_component_clause, [], [f2316])).
fof(f2316, plain, (spl144_199 <=> (e12 = op1(e11, op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_199])])).
fof(f4628, plain, (~ spl144_113 | ~ spl144_330), inference(avatar_split_clause, [], [f4627, f3223, f1679])).
fof(f1679, plain, (spl144_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_113])])).
fof(f3223, plain, (spl144_330 <=> (e20 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_330])])).
fof(f4627, plain, (~ (e20 = op2(e20, e23)) | ~ spl144_330), inference(forward_demodulation, [], [f2996, f3224])).
fof(f3224, plain, ((e20 = h1(e10)) | ~ spl144_330), inference(avatar_component_clause, [], [f3223])).
fof(f2996, plain, ~ (op2(e20, e23) = h1(e10)), inference(backward_demodulation, [], [f482, f1091])).
fof(f1091, plain, (op2(e20, e20) = h1(e10)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(op2(e20, e20), e20) = h1(e13)) & (h1(e12) = op2(op2(op2(e20, e20), e20), op2(e20, e20))) & (op2(e20, e20) = h1(e10)) & (e20 = h1(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax14)).
fof(f482, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f4622, plain, (~ spl144_63 | ~ spl144_15), inference(avatar_split_clause, [], [f4621, f1230, f1434])).
fof(f1434, plain, (spl144_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl144_63])])).
fof(f4621, plain, (~ (op1(e10, e10) = e12) | ~ spl144_15), inference(forward_demodulation, [], [f410, f1232])).
fof(f410, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f4620, plain, (~ spl144_64 | ~ spl144_60), inference(avatar_split_clause, [], [f4619, f1421, f1438])).
fof(f1438, plain, (spl144_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl144_64])])).
fof(f4619, plain, (~ (op1(e10, e10) = e13) | ~ spl144_60), inference(forward_demodulation, [], [f432, f1423])).
fof(f432, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f4600, plain, (~ spl144_28 | ~ spl144_60), inference(avatar_split_clause, [], [f4599, f1421, f1285])).
fof(f1285, plain, (spl144_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_28])])).
fof(f4599, plain, (~ (e13 = op1(e12, e11)) | ~ spl144_60), inference(forward_demodulation, [], [f415, f1423])).
fof(f415, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f4593, plain, (~ spl144_12 | ~ spl144_60), inference(avatar_split_clause, [], [f4592, f1421, f1217])).
fof(f1217, plain, (spl144_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_12])])).
fof(f4592, plain, (~ (e13 = op1(e13, e11)) | ~ spl144_60), inference(forward_demodulation, [], [f416, f1423])).
fof(f416, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4577, plain, (~ spl144_90 | ~ spl144_91), inference(avatar_contradiction_clause, [], [f4576])).
fof(f4576, plain, ($false | (~ spl144_90 | ~ spl144_91)), inference(subsumption_resolution, [], [f4575, f513])).
fof(f513, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f4575, plain, ((e21 = e22) | (~ spl144_90 | ~ spl144_91)), inference(backward_demodulation, [], [f1587, f1583])).
fof(f1583, plain, ((e21 = op2(e22, e21)) | ~ spl144_90), inference(avatar_component_clause, [], [f1581])).
fof(f1581, plain, (spl144_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_90])])).
fof(f4556, plain, (~ spl144_61 | spl144_206), inference(avatar_contradiction_clause, [], [f4555])).
fof(f4555, plain, ($false | (~ spl144_61 | spl144_206)), inference(subsumption_resolution, [], [f4554, f1428])).
fof(f4554, plain, (~ (e10 = op1(e10, e10)) | (~ spl144_61 | spl144_206)), inference(forward_demodulation, [], [f2352, f1428])).
fof(f2352, plain, (~ (e10 = op1(e10, op1(e10, e10))) | spl144_206), inference(avatar_component_clause, [], [f2350])).
fof(f2350, plain, (spl144_206 <=> (e10 = op1(e10, op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_206])])).
fof(f4543, plain, (~ spl144_74 | ~ spl144_300), inference(avatar_split_clause, [], [f4535, f3064, f1513])).
fof(f3064, plain, (spl144_300 <=> (e21 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_300])])).
fof(f4535, plain, (~ (e21 = op2(e23, e21)) | ~ spl144_300), inference(backward_demodulation, [], [f3024, f3065])).
fof(f3065, plain, ((e21 = h4(e10)) | ~ spl144_300), inference(avatar_component_clause, [], [f3064])).
fof(f3024, plain, ~ (op2(e23, e21) = h4(e10)), inference(backward_demodulation, [], [f502, f1103])).
fof(f502, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4501, plain, (~ spl144_79 | ~ spl144_69 | spl144_290), inference(avatar_split_clause, [], [f4487, f2968, f1492, f1534])).
fof(f4487, plain, (~ (e22 = op2(e23, e20)) | (~ spl144_69 | spl144_290)), inference(backward_demodulation, [], [f2970, f1494])).
fof(f2970, plain, (~ (e22 = op2(e23, op2(e23, e22))) | spl144_290), inference(avatar_component_clause, [], [f2968])).
fof(f4500, plain, (~ spl144_76 | ~ spl144_124), inference(avatar_split_clause, [], [f4415, f1725, f1521])).
fof(f1521, plain, (spl144_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_76])])).
fof(f4415, plain, (~ (e23 = op2(e23, e21)) | ~ spl144_124), inference(forward_demodulation, [], [f464, f1727])).
fof(f464, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4494, plain, (~ spl144_67 | spl144_296), inference(avatar_contradiction_clause, [], [f4493])).
fof(f4493, plain, ($false | (~ spl144_67 | spl144_296)), inference(subsumption_resolution, [], [f4491, f3046])).
fof(f3046, plain, (~ (e22 = h4(e10)) | spl144_296), inference(avatar_component_clause, [], [f3044])).
fof(f3044, plain, (spl144_296 <=> (e22 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_296])])).
fof(f4491, plain, ((e22 = h4(e10)) | ~ spl144_67), inference(backward_demodulation, [], [f1103, f1485])).
fof(f1485, plain, ((e22 = op2(e23, e23)) | ~ spl144_67), inference(avatar_component_clause, [], [f1483])).
fof(f4483, plain, (~ spl144_74 | spl144_291), inference(avatar_contradiction_clause, [], [f4482])).
fof(f4482, plain, ($false | (~ spl144_74 | spl144_291)), inference(subsumption_resolution, [], [f4479, f1515])).
fof(f4479, plain, (~ (e21 = op2(e23, e21)) | (~ spl144_74 | spl144_291)), inference(backward_demodulation, [], [f2975, f1515])).
fof(f2975, plain, (~ (e21 = op2(e23, op2(e23, e21))) | spl144_291), inference(avatar_component_clause, [], [f2973])).
fof(f2973, plain, (spl144_291 <=> (e21 = op2(e23, op2(e23, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_291])])).
fof(f4470, plain, (~ spl144_88 | spl144_368), inference(avatar_contradiction_clause, [], [f4469])).
fof(f4469, plain, ($false | (~ spl144_88 | spl144_368)), inference(subsumption_resolution, [], [f4468, f3456])).
fof(f3456, plain, (~ (e23 = h3(e10)) | spl144_368), inference(avatar_component_clause, [], [f3454])).
fof(f4468, plain, ((e23 = h3(e10)) | ~ spl144_88), inference(backward_demodulation, [], [f1099, f1574])).
fof(f1574, plain, ((e23 = op2(e22, e22)) | ~ spl144_88), inference(avatar_component_clause, [], [f1572])).
fof(f1572, plain, (spl144_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_88])])).
fof(f4460, plain, (~ spl144_98 | ~ spl144_102), inference(avatar_split_clause, [], [f4458, f1632, f1615])).
fof(f1615, plain, (spl144_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_98])])).
fof(f4458, plain, (~ (e21 = op2(e21, e23)) | ~ spl144_102), inference(backward_demodulation, [], [f491, f1634])).
fof(f1634, plain, ((e21 = op2(e21, e22)) | ~ spl144_102), inference(avatar_component_clause, [], [f1632])).
fof(f4453, plain, (~ spl144_105 | ~ spl144_106), inference(avatar_contradiction_clause, [], [f4452])).
fof(f4452, plain, ($false | (~ spl144_105 | ~ spl144_106)), inference(subsumption_resolution, [], [f4451, f510])).
fof(f510, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f4451, plain, ((e20 = e21) | (~ spl144_105 | ~ spl144_106)), inference(forward_demodulation, [], [f1651, f1647])).
fof(f1647, plain, ((e20 = op2(e21, e21)) | ~ spl144_105), inference(avatar_component_clause, [], [f1645])).
fof(f1645, plain, (spl144_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_105])])).
fof(f1651, plain, ((e21 = op2(e21, e21)) | ~ spl144_106), inference(avatar_component_clause, [], [f1649])).
fof(f1649, plain, (spl144_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_106])])).
fof(f4437, plain, (~ spl144_103 | ~ spl144_119), inference(avatar_split_clause, [], [f4431, f1704, f1636])).
fof(f4431, plain, (~ (e22 = op2(e21, e22)) | ~ spl144_119), inference(backward_demodulation, [], [f468, f1706])).
fof(f1706, plain, ((e22 = op2(e20, e22)) | ~ spl144_119), inference(avatar_component_clause, [], [f1704])).
fof(f468, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4430, plain, (~ spl144_128 | spl144_404), inference(avatar_contradiction_clause, [], [f4429])).
fof(f4429, plain, ($false | (~ spl144_128 | spl144_404)), inference(subsumption_resolution, [], [f4428, f3699])).
fof(f3699, plain, (~ (e23 = h1(e10)) | spl144_404), inference(avatar_component_clause, [], [f3697])).
fof(f3697, plain, (spl144_404 <=> (e23 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_404])])).
fof(f4428, plain, ((e23 = h1(e10)) | ~ spl144_128), inference(backward_demodulation, [], [f1091, f1744])).
fof(f1744, plain, ((op2(e20, e20) = e23) | ~ spl144_128), inference(avatar_component_clause, [], [f1742])).
fof(f1742, plain, (spl144_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl144_128])])).
fof(f4427, plain, (~ spl144_20 | spl144_193), inference(avatar_contradiction_clause, [], [f4426])).
fof(f4426, plain, ($false | (~ spl144_20 | spl144_193)), inference(subsumption_resolution, [], [f4425, f1253])).
fof(f1253, plain, ((e13 = op1(e12, e13)) | ~ spl144_20), inference(avatar_component_clause, [], [f1251])).
fof(f1251, plain, (spl144_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_20])])).
fof(f4425, plain, (~ (e13 = op1(e12, e13)) | (~ spl144_20 | spl144_193)), inference(forward_demodulation, [], [f2289, f1253])).
fof(f2289, plain, (~ (e13 = op1(e12, op1(e12, e13))) | spl144_193), inference(avatar_component_clause, [], [f2287])).
fof(f2287, plain, (spl144_193 <=> (e13 = op1(e12, op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_193])])).
fof(f4418, plain, (~ spl144_404 | ~ spl144_124), inference(avatar_split_clause, [], [f4417, f1725, f3697])).
fof(f4417, plain, (~ (e23 = h1(e10)) | ~ spl144_124), inference(forward_demodulation, [], [f2994, f1727])).
fof(f2994, plain, ~ (op2(e20, e21) = h1(e10)), inference(backward_demodulation, [], [f480, f1091])).
fof(f480, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f4409, plain, (~ spl144_97 | ~ spl144_318), inference(avatar_split_clause, [], [f4408, f3162, f1611])).
fof(f1611, plain, (spl144_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_97])])).
fof(f4408, plain, (~ (e20 = op2(e21, e23)) | ~ spl144_318), inference(forward_demodulation, [], [f3007, f3163])).
fof(f3007, plain, ~ (op2(e21, e23) = h2(e10)), inference(backward_demodulation, [], [f490, f1095])).
fof(f1095, plain, (op2(e21, e21) = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f490, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4401, plain, (~ spl144_312 | ~ spl144_94), inference(avatar_split_clause, [], [f4400, f1598, f3125])).
fof(f3125, plain, (spl144_312 <=> (e21 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_312])])).
fof(f4400, plain, (~ (e21 = h3(e10)) | ~ spl144_94), inference(forward_demodulation, [], [f3014, f1600])).
fof(f3014, plain, ~ (op2(e22, e20) = h3(e10)), inference(backward_demodulation, [], [f493, f1099])).
fof(f493, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f4397, plain, (spl144_79 | ~ spl144_318), inference(avatar_split_clause, [], [f4146, f3162, f1534])).
fof(f4146, plain, ((e22 = op2(e23, e20)) | ~ spl144_318), inference(backward_demodulation, [], [f3009, f3163])).
fof(f3009, plain, (e22 = op2(e23, h2(e10))), inference(backward_demodulation, [], [f2988, f1095])).
fof(f4395, plain, (~ spl144_74 | ~ spl144_10 | spl144_372), inference(avatar_split_clause, [], [f4394, f3500, f1209, f1513])).
fof(f3500, plain, (spl144_372 <=> (op2(e23, e21) = h2(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_372])])).
fof(f4394, plain, (~ (e21 = op2(e23, e21)) | (~ spl144_10 | spl144_372)), inference(forward_demodulation, [], [f4393, f1094])).
fof(f4393, plain, (~ (op2(e23, e21) = h2(e11)) | (~ spl144_10 | spl144_372)), inference(forward_demodulation, [], [f3502, f1211])).
fof(f3502, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | spl144_372), inference(avatar_component_clause, [], [f3500])).
fof(f4391, plain, (~ spl144_56 | ~ spl144_60), inference(avatar_split_clause, [], [f4390, f1421, f1404])).
fof(f1404, plain, (spl144_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_56])])).
fof(f4390, plain, (~ (e13 = op1(e10, e12)) | ~ spl144_60), inference(forward_demodulation, [], [f435, f1423])).
fof(f435, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f4363, plain, (~ spl144_10 | spl144_209), inference(avatar_contradiction_clause, [], [f4362])).
fof(f4362, plain, ($false | (~ spl144_10 | spl144_209)), inference(subsumption_resolution, [], [f4359, f1211])).
fof(f4359, plain, (~ (e11 = op1(e13, e11)) | (~ spl144_10 | spl144_209)), inference(backward_demodulation, [], [f2371, f1211])).
fof(f2371, plain, (~ (e11 = op1(e13, op1(e13, e11))) | spl144_209), inference(avatar_component_clause, [], [f2369])).
fof(f2369, plain, (spl144_209 <=> (e11 = op1(e13, op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_209])])).
fof(f4342, plain, (~ spl144_40 | ~ spl144_48), inference(avatar_split_clause, [], [f4341, f1370, f1336])).
fof(f1336, plain, (spl144_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_40])])).
fof(f4341, plain, (~ (e13 = op1(e11, e12)) | ~ spl144_48), inference(backward_demodulation, [], [f439, f1372])).
fof(f439, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4339, plain, (~ spl144_58 | ~ spl144_60), inference(avatar_contradiction_clause, [], [f4338])).
fof(f4338, plain, ($false | (~ spl144_58 | ~ spl144_60)), inference(subsumption_resolution, [], [f4337, f508])).
fof(f4337, plain, ((e11 = e13) | (~ spl144_58 | ~ spl144_60)), inference(forward_demodulation, [], [f1423, f1415])).
fof(f1415, plain, ((e11 = op1(e10, e11)) | ~ spl144_58), inference(avatar_component_clause, [], [f1413])).
fof(f1413, plain, (spl144_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_58])])).
fof(f4336, plain, (~ spl144_66 | spl144_300), inference(avatar_contradiction_clause, [], [f4335])).
fof(f4335, plain, ($false | (~ spl144_66 | spl144_300)), inference(subsumption_resolution, [], [f4332, f3066])).
fof(f3066, plain, (~ (e21 = h4(e10)) | spl144_300), inference(avatar_component_clause, [], [f3064])).
fof(f4332, plain, ((e21 = h4(e10)) | ~ spl144_66), inference(backward_demodulation, [], [f1103, f1481])).
fof(f1481, plain, ((e21 = op2(e23, e23)) | ~ spl144_66), inference(avatar_component_clause, [], [f1479])).
fof(f1479, plain, (spl144_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_66])])).
fof(f4323, plain, (~ spl144_77 | ~ spl144_79), inference(avatar_contradiction_clause, [], [f4322])).
fof(f4322, plain, ($false | (~ spl144_77 | ~ spl144_79)), inference(subsumption_resolution, [], [f4321, f511])).
fof(f511, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f4321, plain, ((e20 = e22) | (~ spl144_77 | ~ spl144_79)), inference(backward_demodulation, [], [f1536, f1528])).
fof(f1528, plain, ((e20 = op2(e23, e20)) | ~ spl144_77), inference(avatar_component_clause, [], [f1526])).
fof(f1526, plain, (spl144_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_77])])).
fof(f4314, plain, (spl144_312 | ~ spl144_86), inference(avatar_split_clause, [], [f4313, f1564, f3125])).
fof(f1564, plain, (spl144_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_86])])).
fof(f4313, plain, ((e21 = h3(e10)) | ~ spl144_86), inference(backward_demodulation, [], [f1099, f1566])).
fof(f1566, plain, ((e21 = op2(e22, e22)) | ~ spl144_86), inference(avatar_component_clause, [], [f1564])).
fof(f4299, plain, (~ spl144_122 | ~ spl144_124), inference(avatar_contradiction_clause, [], [f4298])).
fof(f4298, plain, ($false | (~ spl144_122 | ~ spl144_124)), inference(subsumption_resolution, [], [f4297, f514])).
fof(f514, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f4297, plain, ((e21 = e23) | (~ spl144_122 | ~ spl144_124)), inference(forward_demodulation, [], [f1727, f1719])).
fof(f1719, plain, ((e21 = op2(e20, e21)) | ~ spl144_122), inference(avatar_component_clause, [], [f1717])).
fof(f1717, plain, (spl144_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_122])])).
fof(f4296, plain, (~ spl144_127 | spl144_322), inference(avatar_contradiction_clause, [], [f4295])).
fof(f4295, plain, ($false | (~ spl144_127 | spl144_322)), inference(subsumption_resolution, [], [f4294, f3184])).
fof(f3184, plain, (~ (e22 = h1(e10)) | spl144_322), inference(avatar_component_clause, [], [f3182])).
fof(f3182, plain, (spl144_322 <=> (e22 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_322])])).
fof(f4294, plain, ((e22 = h1(e10)) | ~ spl144_127), inference(backward_demodulation, [], [f1091, f1740])).
fof(f1740, plain, ((op2(e20, e20) = e22) | ~ spl144_127), inference(avatar_component_clause, [], [f1738])).
fof(f1738, plain, (spl144_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl144_127])])).
fof(f4286, plain, (~ spl144_95 | ~ spl144_79), inference(avatar_split_clause, [], [f4285, f1534, f1602])).
fof(f1602, plain, (spl144_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_95])])).
fof(f4285, plain, (~ (e22 = op2(e22, e20)) | ~ spl144_79), inference(forward_demodulation, [], [f461, f1536])).
fof(f461, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f4277, plain, (~ spl144_308 | ~ spl144_91), inference(avatar_split_clause, [], [f4276, f1585, f3105])).
fof(f3105, plain, (spl144_308 <=> (e22 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_308])])).
fof(f4276, plain, (~ (e22 = h3(e10)) | ~ spl144_91), inference(forward_demodulation, [], [f3015, f1587])).
fof(f3015, plain, ~ (op2(e22, e21) = h3(e10)), inference(backward_demodulation, [], [f495, f1099])).
fof(f495, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f4271, plain, (~ spl144_73 | ~ spl144_318), inference(avatar_split_clause, [], [f4270, f3162, f1509])).
fof(f1509, plain, (spl144_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_73])])).
fof(f4270, plain, (~ (e20 = op2(e23, e21)) | ~ spl144_318), inference(forward_demodulation, [], [f3004, f3163])).
fof(f3004, plain, ~ (op2(e23, e21) = h2(e10)), inference(backward_demodulation, [], [f466, f1095])).
fof(f466, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4260, plain, (~ spl144_48 | ~ spl144_32), inference(avatar_split_clause, [], [f4259, f1302, f1370])).
fof(f1302, plain, (spl144_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_32])])).
fof(f4259, plain, (~ (e13 = op1(e11, e10)) | ~ spl144_32), inference(forward_demodulation, [], [f411, f1304])).
fof(f1304, plain, ((e13 = op1(e12, e10)) | ~ spl144_32), inference(avatar_component_clause, [], [f1302])).
fof(f411, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f4252, plain, (~ spl144_20 | ~ spl144_32), inference(avatar_split_clause, [], [f4251, f1302, f1251])).
fof(f4251, plain, (~ (e13 = op1(e12, e13)) | ~ spl144_32), inference(forward_demodulation, [], [f446, f1304])).
fof(f4248, plain, (~ spl144_11 | ~ spl144_15), inference(avatar_split_clause, [], [f4247, f1230, f1213])).
fof(f1213, plain, (spl144_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_11])])).
fof(f4247, plain, (~ (e12 = op1(e13, e11)) | ~ spl144_15), inference(forward_demodulation, [], [f450, f1232])).
fof(f450, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4219, plain, (~ spl144_23 | ~ spl144_27), inference(avatar_split_clause, [], [f4218, f1281, f1264])).
fof(f1264, plain, (spl144_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_23])])).
fof(f4218, plain, (~ (e12 = op1(e12, e12)) | ~ spl144_27), inference(backward_demodulation, [], [f447, f1283])).
fof(f447, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4201, plain, (~ spl144_38 | ~ spl144_46), inference(avatar_split_clause, [], [f4199, f1362, f1328])).
fof(f4199, plain, (~ (e11 = op1(e11, e12)) | ~ spl144_46), inference(backward_demodulation, [], [f439, f1364])).
fof(f4200, plain, (~ spl144_30 | ~ spl144_46), inference(avatar_split_clause, [], [f4198, f1362, f1294])).
fof(f4198, plain, (~ (e11 = op1(e12, e10)) | ~ spl144_46), inference(backward_demodulation, [], [f411, f1364])).
fof(f4182, plain, (spl144_308 | ~ spl144_87), inference(avatar_split_clause, [], [f4181, f1568, f3105])).
fof(f1568, plain, (spl144_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_87])])).
fof(f4181, plain, ((e22 = h3(e10)) | ~ spl144_87), inference(backward_demodulation, [], [f1099, f1570])).
fof(f1570, plain, ((e22 = op2(e22, e22)) | ~ spl144_87), inference(avatar_component_clause, [], [f1568])).
fof(f4180, plain, (~ spl144_91 | ~ spl144_92), inference(avatar_contradiction_clause, [], [f4179])).
fof(f4179, plain, ($false | (~ spl144_91 | ~ spl144_92)), inference(subsumption_resolution, [], [f4178, f515])).
fof(f4178, plain, ((e22 = e23) | (~ spl144_91 | ~ spl144_92)), inference(backward_demodulation, [], [f1591, f1587])).
fof(f1591, plain, ((e23 = op2(e22, e21)) | ~ spl144_92), inference(avatar_component_clause, [], [f1589])).
fof(f1589, plain, (spl144_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_92])])).
fof(f4166, plain, (~ spl144_326 | ~ spl144_110), inference(avatar_split_clause, [], [f4164, f1666, f3202])).
fof(f3202, plain, (spl144_326 <=> (e21 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_326])])).
fof(f4164, plain, (~ (e21 = h1(e10)) | ~ spl144_110), inference(backward_demodulation, [], [f2991, f1668])).
fof(f2991, plain, ~ (op2(e21, e20) = h1(e10)), inference(backward_demodulation, [], [f456, f1091])).
fof(f456, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f4155, plain, (spl144_326 | ~ spl144_126), inference(avatar_split_clause, [], [f4154, f1734, f3202])).
fof(f1734, plain, (spl144_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl144_126])])).
fof(f4154, plain, ((e21 = h1(e10)) | ~ spl144_126), inference(backward_demodulation, [], [f1091, f1736])).
fof(f1736, plain, ((op2(e20, e20) = e21) | ~ spl144_126), inference(avatar_component_clause, [], [f1734])).
fof(f4153, plain, (spl144_124 | ~ spl144_318), inference(avatar_split_clause, [], [f4145, f3162, f1725])).
fof(f4145, plain, ((e23 = op2(e20, e21)) | ~ spl144_318), inference(backward_demodulation, [], [f3008, f3163])).
fof(f3008, plain, (e23 = op2(h2(e10), e21)), inference(backward_demodulation, [], [f1089, f1095])).
fof(f4152, plain, (~ spl144_101 | ~ spl144_318), inference(avatar_split_clause, [], [f4144, f3162, f1628])).
fof(f1628, plain, (spl144_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_101])])).
fof(f4144, plain, (~ (e20 = op2(e21, e22)) | ~ spl144_318), inference(backward_demodulation, [], [f3006, f3163])).
fof(f3006, plain, ~ (op2(e21, e22) = h2(e10)), inference(backward_demodulation, [], [f489, f1095])).
fof(f489, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4151, plain, (~ spl144_109 | ~ spl144_318), inference(avatar_split_clause, [], [f4143, f3162, f1662])).
fof(f1662, plain, (spl144_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_109])])).
fof(f4143, plain, (~ (e20 = op2(e21, e20)) | ~ spl144_318), inference(backward_demodulation, [], [f3005, f3163])).
fof(f3005, plain, ~ (op2(e21, e20) = h2(e10)), inference(backward_demodulation, [], [f486, f1095])).
fof(f486, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4149, plain, (~ spl144_121 | ~ spl144_318), inference(avatar_split_clause, [], [f4141, f3162, f1713])).
fof(f1713, plain, (spl144_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_121])])).
fof(f4141, plain, (~ (e20 = op2(e20, e21)) | ~ spl144_318), inference(backward_demodulation, [], [f3002, f3163])).
fof(f3002, plain, ~ (op2(e20, e21) = h2(e10)), inference(backward_demodulation, [], [f462, f1095])).
fof(f462, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4124, plain, (~ spl144_111 | ~ spl144_79), inference(avatar_split_clause, [], [f4123, f1534, f1670])).
fof(f1670, plain, (spl144_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_111])])).
fof(f4123, plain, (~ (e22 = op2(e21, e20)) | ~ spl144_79), inference(forward_demodulation, [], [f460, f1536])).
fof(f460, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f4122, plain, (~ spl144_110 | ~ spl144_98), inference(avatar_split_clause, [], [f4121, f1615, f1666])).
fof(f4121, plain, (~ (e21 = op2(e21, e20)) | ~ spl144_98), inference(forward_demodulation, [], [f488, f1617])).
fof(f1617, plain, ((e21 = op2(e21, e23)) | ~ spl144_98), inference(avatar_component_clause, [], [f1615])).
fof(f4096, plain, (~ spl144_322 | ~ spl144_79), inference(avatar_split_clause, [], [f4095, f1534, f3182])).
fof(f4095, plain, (~ (e22 = h1(e10)) | ~ spl144_79), inference(forward_demodulation, [], [f2993, f1536])).
fof(f2993, plain, ~ (op2(e23, e20) = h1(e10)), inference(backward_demodulation, [], [f458, f1091])).
fof(f458, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f4094, plain, (~ spl144_296 | ~ spl144_79), inference(avatar_split_clause, [], [f4093, f1534, f3044])).
fof(f4093, plain, (~ (e22 = h4(e10)) | ~ spl144_79), inference(forward_demodulation, [], [f3023, f1536])).
fof(f3023, plain, ~ (op2(e23, e20) = h4(e10)), inference(backward_demodulation, [], [f500, f1103])).
fof(f500, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4077, plain, (spl144_304 | ~ spl144_65), inference(avatar_split_clause, [], [f4076, f1475, f3084])).
fof(f1475, plain, (spl144_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_65])])).
fof(f4076, plain, ((e20 = h4(e10)) | ~ spl144_65), inference(forward_demodulation, [], [f1103, f1477])).
fof(f1477, plain, ((e20 = op2(e23, e23)) | ~ spl144_65), inference(avatar_component_clause, [], [f1475])).
fof(f4075, plain, (~ spl144_57 | ~ spl144_41), inference(avatar_split_clause, [], [f4074, f1341, f1409])).
fof(f1409, plain, (spl144_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_57])])).
fof(f4074, plain, (~ (e10 = op1(e10, e11)) | ~ spl144_41), inference(forward_demodulation, [], [f414, f1343])).
fof(f414, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4069, plain, (spl144_60 | ~ spl144_41), inference(avatar_split_clause, [], [f3909, f1341, f1421])).
fof(f3909, plain, ((e13 = op1(e10, e11)) | ~ spl144_41), inference(backward_demodulation, [], [f1086, f1343])).
fof(f1086, plain, (e13 = op1(op1(e11, e11), e11)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(op1(e11, e11), e11)) & (e12 = op1(op1(op1(e11, e11), e11), op1(e11, e11))) & (e10 = op1(e11, e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax12)).
fof(f4068, plain, (~ spl144_53 | ~ spl144_5), inference(avatar_split_clause, [], [f4067, f1188, f1392])).
fof(f1392, plain, (spl144_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_53])])).
fof(f4067, plain, (~ (e10 = op1(e10, e12)) | ~ spl144_5), inference(forward_demodulation, [], [f422, f1190])).
fof(f4058, plain, (~ spl144_47 | ~ spl144_15), inference(avatar_split_clause, [], [f4057, f1230, f1366])).
fof(f1366, plain, (spl144_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_47])])).
fof(f4057, plain, (~ (e12 = op1(e11, e10)) | ~ spl144_15), inference(forward_demodulation, [], [f412, f1232])).
fof(f412, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f4056, plain, (~ spl144_45 | ~ spl144_41), inference(avatar_split_clause, [], [f4055, f1341, f1358])).
fof(f1358, plain, (spl144_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_45])])).
fof(f4055, plain, (~ (e10 = op1(e11, e10)) | ~ spl144_41), inference(forward_demodulation, [], [f438, f1343])).
fof(f438, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4054, plain, (~ spl144_46 | ~ spl144_34), inference(avatar_split_clause, [], [f4053, f1311, f1362])).
fof(f1311, plain, (spl144_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_34])])).
fof(f4053, plain, (~ (e11 = op1(e11, e10)) | ~ spl144_34), inference(forward_demodulation, [], [f440, f1313])).
fof(f1313, plain, ((e11 = op1(e11, e13)) | ~ spl144_34), inference(avatar_component_clause, [], [f1311])).
fof(f440, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4050, plain, (~ spl144_38 | ~ spl144_34), inference(avatar_split_clause, [], [f4049, f1311, f1328])).
fof(f4049, plain, (~ (e11 = op1(e11, e12)) | ~ spl144_34), inference(forward_demodulation, [], [f443, f1313])).
fof(f443, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4044, plain, (~ spl144_31 | ~ spl144_15), inference(avatar_split_clause, [], [f4043, f1230, f1298])).
fof(f1298, plain, (spl144_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_31])])).
fof(f4043, plain, (~ (e12 = op1(e12, e10)) | ~ spl144_15), inference(forward_demodulation, [], [f413, f1232])).
fof(f413, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3976, plain, (~ spl144_15 | ~ spl144_16), inference(avatar_contradiction_clause, [], [f3975])).
fof(f3975, plain, ($false | (~ spl144_15 | ~ spl144_16)), inference(subsumption_resolution, [], [f3974, f509])).
fof(f3974, plain, ((e12 = e13) | (~ spl144_15 | ~ spl144_16)), inference(backward_demodulation, [], [f1236, f1232])).
fof(f1236, plain, ((e13 = op1(e13, e10)) | ~ spl144_16), inference(avatar_component_clause, [], [f1234])).
fof(f1234, plain, (spl144_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_16])])).
fof(f3956, plain, (~ spl144_4 | ~ spl144_20), inference(avatar_split_clause, [], [f3955, f1251, f1183])).
fof(f3955, plain, (~ (e13 = op1(e13, e13)) | ~ spl144_20), inference(backward_demodulation, [], [f431, f1253])).
fof(f431, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3929, plain, (~ spl144_4 | ~ spl144_36), inference(avatar_split_clause, [], [f3927, f1319, f1183])).
fof(f1319, plain, (spl144_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_36])])).
fof(f3927, plain, (~ (e13 = op1(e13, e13)) | ~ spl144_36), inference(backward_demodulation, [], [f430, f1321])).
fof(f1321, plain, ((e13 = op1(e11, e13)) | ~ spl144_36), inference(avatar_component_clause, [], [f1319])).
fof(f430, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3918, plain, (spl144_15 | ~ spl144_41), inference(avatar_split_clause, [], [f3910, f1341, f1230])).
fof(f3910, plain, ((e12 = op1(e13, e10)) | ~ spl144_41), inference(backward_demodulation, [], [f2986, f1343])).
fof(f2986, plain, (e12 = op1(e13, op1(e11, e11))), inference(forward_demodulation, [], [f1085, f1086])).
fof(f1085, plain, (e12 = op1(op1(op1(e11, e11), e11), op1(e11, e11))), inference(cnf_transformation, [], [f12])).
fof(f3914, plain, (~ spl144_33 | ~ spl144_41), inference(avatar_split_clause, [], [f3908, f1341, f1307])).
fof(f1307, plain, (spl144_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_33])])).
fof(f3908, plain, (~ (e10 = op1(e11, e13)) | ~ spl144_41), inference(backward_demodulation, [], [f442, f1343])).
fof(f442, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3913, plain, (~ spl144_37 | ~ spl144_41), inference(avatar_split_clause, [], [f3907, f1341, f1324])).
fof(f1324, plain, (spl144_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_37])])).
fof(f3907, plain, (~ (e10 = op1(e11, e12)) | ~ spl144_41), inference(backward_demodulation, [], [f441, f1343])).
fof(f441, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f3912, plain, (~ spl144_9 | ~ spl144_41), inference(avatar_split_clause, [], [f3906, f1341, f1205])).
fof(f1205, plain, (spl144_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_9])])).
fof(f3906, plain, (~ (e10 = op1(e13, e11)) | ~ spl144_41), inference(backward_demodulation, [], [f418, f1343])).
fof(f418, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3894, plain, (~ spl144_50 | ~ spl144_51), inference(avatar_contradiction_clause, [], [f3893])).
fof(f3893, plain, ($false | (~ spl144_50 | ~ spl144_51)), inference(subsumption_resolution, [], [f3892, f507])).
fof(f507, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f3892, plain, ((e11 = e12) | (~ spl144_50 | ~ spl144_51)), inference(backward_demodulation, [], [f1385, f1381])).
fof(f1385, plain, ((e12 = op1(e10, e13)) | ~ spl144_51), inference(avatar_component_clause, [], [f1383])).
fof(f3882, plain, (~ spl144_49 | ~ spl144_53), inference(avatar_split_clause, [], [f3878, f1392, f1375])).
fof(f3878, plain, (~ (e10 = op1(e10, e13)) | ~ spl144_53), inference(backward_demodulation, [], [f437, f1394])).
fof(f1394, plain, ((e10 = op1(e10, e12)) | ~ spl144_53), inference(avatar_component_clause, [], [f1392])).
fof(f437, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3863, plain, (~ spl144_53 | ~ spl144_61), inference(avatar_split_clause, [], [f3856, f1426, f1392])).
fof(f3856, plain, (~ (e10 = op1(e10, e12)) | ~ spl144_61), inference(backward_demodulation, [], [f433, f1428])).
fof(f433, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3861, plain, (~ spl144_13 | ~ spl144_61), inference(avatar_split_clause, [], [f3854, f1426, f1222])).
fof(f1222, plain, (spl144_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_13])])).
fof(f3854, plain, (~ (e10 = op1(e13, e10)) | ~ spl144_61), inference(backward_demodulation, [], [f410, f1428])).
fof(f3851, plain, (~ spl144_65 | ~ spl144_68), inference(avatar_contradiction_clause, [], [f3850])).
fof(f3850, plain, ($false | (~ spl144_65 | ~ spl144_68)), inference(subsumption_resolution, [], [f3849, f512])).
fof(f512, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f3849, plain, ((e20 = e23) | (~ spl144_65 | ~ spl144_68)), inference(backward_demodulation, [], [f1489, f1477])).
fof(f1489, plain, ((e23 = op2(e23, e23)) | ~ spl144_68), inference(avatar_component_clause, [], [f1487])).
fof(f1487, plain, (spl144_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_68])])).
fof(f3848, plain, (spl144_349 | ~ spl144_68), inference(avatar_split_clause, [], [f3847, f1487, f3344])).
fof(f3847, plain, ((e23 = h4(e10)) | ~ spl144_68), inference(backward_demodulation, [], [f1103, f1489])).
fof(f3840, plain, (~ spl144_349 | ~ spl144_72), inference(avatar_split_clause, [], [f3837, f1504, f3344])).
fof(f3837, plain, (~ (e23 = h4(e10)) | ~ spl144_72), inference(backward_demodulation, [], [f3025, f1506])).
fof(f3816, plain, (~ spl144_79 | ~ spl144_80), inference(avatar_contradiction_clause, [], [f3815])).
fof(f3815, plain, ($false | (~ spl144_79 | ~ spl144_80)), inference(subsumption_resolution, [], [f3814, f515])).
fof(f3814, plain, ((e22 = e23) | (~ spl144_79 | ~ spl144_80)), inference(backward_demodulation, [], [f1540, f1536])).
fof(f1540, plain, ((e23 = op2(e23, e20)) | ~ spl144_80), inference(avatar_component_clause, [], [f1538])).
fof(f1538, plain, (spl144_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_80])])).
fof(f3803, plain, (~ spl144_82 | ~ spl144_83), inference(avatar_contradiction_clause, [], [f3802])).
fof(f3802, plain, ($false | (~ spl144_82 | ~ spl144_83)), inference(subsumption_resolution, [], [f3801, f513])).
fof(f3801, plain, ((e21 = e22) | (~ spl144_82 | ~ spl144_83)), inference(backward_demodulation, [], [f1553, f1549])).
fof(f1549, plain, ((e21 = op2(e22, e23)) | ~ spl144_82), inference(avatar_component_clause, [], [f1547])).
fof(f1553, plain, ((e22 = op2(e22, e23)) | ~ spl144_83), inference(avatar_component_clause, [], [f1551])).
fof(f1551, plain, (spl144_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_83])])).
fof(f3800, plain, (~ spl144_83 | ~ spl144_84), inference(avatar_contradiction_clause, [], [f3799])).
fof(f3799, plain, ($false | (~ spl144_83 | ~ spl144_84)), inference(subsumption_resolution, [], [f3798, f515])).
fof(f3798, plain, ((e22 = e23) | (~ spl144_83 | ~ spl144_84)), inference(backward_demodulation, [], [f1557, f1553])).
fof(f3793, plain, (spl144_316 | ~ spl144_85), inference(avatar_split_clause, [], [f3792, f1560, f3145])).
fof(f1560, plain, (spl144_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_85])])).
fof(f3792, plain, ((e20 = h3(e10)) | ~ spl144_85), inference(backward_demodulation, [], [f1099, f1562])).
fof(f1562, plain, ((e20 = op2(e22, e22)) | ~ spl144_85), inference(avatar_component_clause, [], [f1560])).
fof(f3780, plain, (~ spl144_330 | ~ spl144_93), inference(avatar_split_clause, [], [f3775, f1594, f3223])).
fof(f1594, plain, (spl144_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_93])])).
fof(f3775, plain, (~ (e20 = h1(e10)) | ~ spl144_93), inference(backward_demodulation, [], [f2992, f1596])).
fof(f1596, plain, ((e20 = op2(e22, e20)) | ~ spl144_93), inference(avatar_component_clause, [], [f1594])).
fof(f2992, plain, ~ (op2(e22, e20) = h1(e10)), inference(backward_demodulation, [], [f457, f1091])).
fof(f457, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f3753, plain, (spl144_318 | ~ spl144_105), inference(avatar_split_clause, [], [f3752, f1645, f3162])).
fof(f3752, plain, ((e20 = h2(e10)) | ~ spl144_105), inference(backward_demodulation, [], [f1095, f1647])).
fof(f3739, plain, (~ spl144_114 | ~ spl144_115), inference(avatar_contradiction_clause, [], [f3738])).
fof(f3738, plain, ($false | (~ spl144_114 | ~ spl144_115)), inference(subsumption_resolution, [], [f3737, f513])).
fof(f3737, plain, ((e21 = e22) | (~ spl144_114 | ~ spl144_115)), inference(backward_demodulation, [], [f1689, f1685])).
fof(f1689, plain, ((e22 = op2(e20, e23)) | ~ spl144_115), inference(avatar_component_clause, [], [f1687])).
fof(f1687, plain, (spl144_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_115])])).
fof(f3722, plain, (~ spl144_330 | ~ spl144_117), inference(avatar_split_clause, [], [f3717, f1696, f3223])).
fof(f1696, plain, (spl144_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_117])])).
fof(f3717, plain, (~ (e20 = h1(e10)) | ~ spl144_117), inference(backward_demodulation, [], [f2995, f1698])).
fof(f1698, plain, ((e20 = op2(e20, e22)) | ~ spl144_117), inference(avatar_component_clause, [], [f1696])).
fof(f2995, plain, ~ (op2(e20, e22) = h1(e10)), inference(backward_demodulation, [], [f481, f1091])).
fof(f481, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3703, plain, (spl144_330 | ~ spl144_125), inference(avatar_split_clause, [], [f3702, f1730, f3223])).
fof(f3702, plain, ((e20 = h1(e10)) | ~ spl144_125), inference(backward_demodulation, [], [f1091, f1732])).
fof(f3551, plain, (~ spl144_369 | spl144_317 | ~ spl144_370 | ~ spl144_371 | ~ spl144_372 | ~ spl144_373 | ~ spl144_374 | ~ spl144_375 | ~ spl144_376 | ~ spl144_377 | ~ spl144_378 | ~ spl144_379 | ~ spl144_380 | ~ spl144_381 | ~ spl144_382 | ~ spl144_383 | ~ spl144_384), inference(avatar_split_clause, [], [f3486, f3548, f3544, f3540, f3536, f3532, f3528, f3524, f3520, f3516, f3512, f3508, f3504, f3500, f3496, f3492, f3158, f3488])).
fof(f3158, plain, (spl144_317 <=> sP135), introduced(avatar_definition, [new_symbols(naming, [spl144_317])])).
fof(f3486, plain, (~ (e23 = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e10) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3485, f3008])).
fof(f3485, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | ~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e10) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3484, f1094])).
fof(f3484, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e10) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3483, f3001])).
fof(f3483, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e10) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3482, f2999])).
fof(f3482, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e10) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3481, f1094])).
fof(f3481, plain, (~ (h2(e10) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3480, f1095])).
fof(f3480, plain, (~ (op2(e21, e21) = h2(op1(e11, e11))) | ~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3479, f1094])).
fof(f3479, plain, (~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3478, f1094])).
fof(f3478, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), e22)) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3477, f3001])).
fof(f3477, plain, (~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3476, f1094])).
fof(f3476, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), e23)) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3475, f2999])).
fof(f3475, plain, (~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3474, f3001])).
fof(f3474, plain, (~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3473, f3001])).
fof(f3473, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), e21)) | ~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3472, f1094])).
fof(f3472, plain, (~ (h3(e10) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3471, f1099])).
fof(f3471, plain, (~ (op2(e22, e22) = h2(op1(e12, e12))) | ~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3470, f3001])).
fof(f3470, plain, (~ (op2(e22, e23) = h2(op1(e12, e13))) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3469, f3001])).
fof(f3469, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3468, f2999])).
fof(f3468, plain, (~ (e22 = h2(op1(e13, e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3467, f3009])).
fof(f3467, plain, (~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3466, f2999])).
fof(f3466, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3465, f2999])).
fof(f3465, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3464, f1094])).
fof(f3464, plain, (~ (op2(e23, e22) = h2(op1(e13, e12))) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3463, f2999])).
fof(f3463, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3462, f3001])).
fof(f3462, plain, (~ (h4(e10) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3461, f1103])).
fof(f3461, plain, (~ (op2(e23, e23) = h2(op1(e13, e13))) | sP135 | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3460, f2999])).
fof(f3460, plain, (sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(subsumption_resolution, [], [f3459, f3153])).
fof(f3153, plain, ~ sP136, inference(subsumption_resolution, [], [f1135, f1094])).
fof(f1135, plain, (~ (e21 = h2(e11)) | ~ sP136), inference(cnf_transformation, [], [f307])).
fof(f307, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP136), inference(nnf_transformation, [], [f159])).
fof(f159, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP136), inference(usedef, [], [e159])).
fof(e159, plain, (sP136 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP136])])).
fof(f3459, plain, (sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(subsumption_resolution, [], [f3458, f3150])).
fof(f3150, plain, ~ sP137, inference(subsumption_resolution, [], [f1132, f3001])).
fof(f1132, plain, (~ (e22 = h2(e12)) | ~ sP137), inference(cnf_transformation, [], [f306])).
fof(f306, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP137), inference(nnf_transformation, [], [f160])).
fof(f160, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP137), inference(usedef, [], [e160])).
fof(e160, plain, (sP137 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP137])])).
fof(f3458, plain, (sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(subsumption_resolution, [], [f1161, f2999])).
fof(f1161, plain, (~ (e23 = h2(e13)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f167])).
fof(f167, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP140 | sP139 | sP138 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e166, e165, e164, e163, e162, e161, e160, e159, e158, e157, e156, e155])).
fof(f155, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP132), inference(usedef, [], [e155])).
fof(e155, plain, (sP132 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP132])])).
fof(f156, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP133), inference(usedef, [], [e156])).
fof(e156, plain, (sP133 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP133])])).
fof(f157, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP134), inference(usedef, [], [e157])).
fof(e157, plain, (sP134 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP134])])).
fof(f158, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP135), inference(usedef, [], [e158])).
fof(e158, plain, (sP135 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP135])])).
fof(f161, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP138), inference(usedef, [], [e161])).
fof(e161, plain, (sP138 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP138])])).
fof(f162, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP139), inference(usedef, [], [e162])).
fof(e162, plain, (sP139 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP139])])).
fof(f163, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP140), inference(usedef, [], [e163])).
fof(e163, plain, (sP140 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP140])])).
fof(f164, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP141), inference(usedef, [], [e164])).
fof(e164, plain, (sP141 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP141])])).
fof(f165, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP142), inference(usedef, [], [e165])).
fof(e165, plain, (sP142 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP142])])).
fof(f166, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP143), inference(usedef, [], [e166])).
fof(e166, plain, (sP143 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP143])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', co1)).
fof(f3165, plain, (~ spl144_317 | ~ spl144_318), inference(avatar_split_clause, [], [f1138, f3162, f3158])).
fof(f1138, plain, (~ (e20 = h2(e10)) | ~ sP135), inference(cnf_transformation, [], [f308])).
fof(f308, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP135), inference(nnf_transformation, [], [f158])).
fof(f2989, plain, spl144_105, inference(avatar_split_clause, [], [f1087, f1645])).
fof(f1087, plain, (e20 = op2(e21, e21)), inference(cnf_transformation, [], [f13])).
fof(f2987, plain, spl144_41, inference(avatar_split_clause, [], [f1084, f1341])).
fof(f1084, plain, (e10 = op1(e11, e11)), inference(cnf_transformation, [], [f12])).
fof(f2985, plain, (spl144_125 | spl144_110 | spl144_95 | spl144_80), inference(avatar_split_clause, [], [f1072, f1538, f1602, f1666, f1730])).
fof(f1072, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ((((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP131 | sP130 | sP129 | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69) & ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20)))) | sP68 | sP67 | sP66) & ((e23 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e20, e20)))), inference(definition_folding, [], [f11, e153, e152, e151, e150, e149, e148, e147, e146, e145, e144, e143, e142, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88])).
fof(f88, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20)))) | ~ sP66), inference(usedef, [], [e88])).
fof(e88, plain, (sP66 <=> (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP66])])).
fof(f89, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20)))) | ~ sP67), inference(usedef, [], [e89])).
fof(e89, plain, (sP67 <=> (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP67])])).
fof(f90, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20)))) | ~ sP68), inference(usedef, [], [e90])).
fof(e90, plain, (sP68 <=> (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP68])])).
fof(f91, plain, (((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP69), inference(usedef, [], [e91])).
fof(e91, plain, (sP69 <=> ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP69])])).
fof(f92, plain, (((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP70), inference(usedef, [], [e92])).
fof(e92, plain, (sP70 <=> ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP70])])).
fof(f93, plain, (((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP71), inference(usedef, [], [e93])).
fof(e93, plain, (sP71 <=> ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP71])])).
fof(f94, plain, (((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP72), inference(usedef, [], [e94])).
fof(e94, plain, (sP72 <=> ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP72])])).
fof(f95, plain, (((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP73), inference(usedef, [], [e95])).
fof(e95, plain, (sP73 <=> ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP73])])).
fof(f96, plain, (((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP74), inference(usedef, [], [e96])).
fof(e96, plain, (sP74 <=> ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP74])])).
fof(f97, plain, (((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP75), inference(usedef, [], [e97])).
fof(e97, plain, (sP75 <=> ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP75])])).
fof(f98, plain, (((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP76), inference(usedef, [], [e98])).
fof(e98, plain, (sP76 <=> ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP76])])).
fof(f99, plain, (((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP77), inference(usedef, [], [e99])).
fof(e99, plain, (sP77 <=> ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP77])])).
fof(f100, plain, (((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP78), inference(usedef, [], [e100])).
fof(e100, plain, (sP78 <=> ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP78])])).
fof(f101, plain, (((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP79), inference(usedef, [], [e101])).
fof(e101, plain, (sP79 <=> ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP79])])).
fof(f102, plain, (((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP80), inference(usedef, [], [e102])).
fof(e102, plain, (sP80 <=> ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP80])])).
fof(f103, plain, (((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP81), inference(usedef, [], [e103])).
fof(e103, plain, (sP81 <=> ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP81])])).
fof(f104, plain, (((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP82), inference(usedef, [], [e104])).
fof(e104, plain, (sP82 <=> ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP82])])).
fof(f105, plain, (((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP83), inference(usedef, [], [e105])).
fof(e105, plain, (sP83 <=> ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP83])])).
fof(f106, plain, (((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP84), inference(usedef, [], [e106])).
fof(e106, plain, (sP84 <=> ((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP84])])).
fof(f107, plain, (((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP85), inference(usedef, [], [e107])).
fof(e107, plain, (sP85 <=> ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP85])])).
fof(f108, plain, (((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP86), inference(usedef, [], [e108])).
fof(e108, plain, (sP86 <=> ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP86])])).
fof(f109, plain, (((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP87), inference(usedef, [], [e109])).
fof(e109, plain, (sP87 <=> ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP87])])).
fof(f110, plain, (((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP88), inference(usedef, [], [e110])).
fof(e110, plain, (sP88 <=> ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP88])])).
fof(f111, plain, (((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP89), inference(usedef, [], [e111])).
fof(e111, plain, (sP89 <=> ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP89])])).
fof(f112, plain, (((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP90), inference(usedef, [], [e112])).
fof(e112, plain, (sP90 <=> ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP90])])).
fof(f113, plain, (((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP91), inference(usedef, [], [e113])).
fof(e113, plain, (sP91 <=> ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP91])])).
fof(f114, plain, (((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP92), inference(usedef, [], [e114])).
fof(e114, plain, (sP92 <=> ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP92])])).
fof(f115, plain, (((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP93), inference(usedef, [], [e115])).
fof(e115, plain, (sP93 <=> ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP93])])).
fof(f116, plain, (((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP94), inference(usedef, [], [e116])).
fof(e116, plain, (sP94 <=> ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP94])])).
fof(f117, plain, (((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP95), inference(usedef, [], [e117])).
fof(e117, plain, (sP95 <=> ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP95])])).
fof(f118, plain, (((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP96), inference(usedef, [], [e118])).
fof(e118, plain, (sP96 <=> ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP96])])).
fof(f119, plain, (((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP97), inference(usedef, [], [e119])).
fof(e119, plain, (sP97 <=> ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP97])])).
fof(f120, plain, (((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP98), inference(usedef, [], [e120])).
fof(e120, plain, (sP98 <=> ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP98])])).
fof(f121, plain, (((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP99), inference(usedef, [], [e121])).
fof(e121, plain, (sP99 <=> ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP99])])).
fof(f122, plain, (((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP100), inference(usedef, [], [e122])).
fof(e122, plain, (sP100 <=> ((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP100])])).
fof(f123, plain, (((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP101), inference(usedef, [], [e123])).
fof(e123, plain, (sP101 <=> ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP101])])).
fof(f124, plain, (((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP102), inference(usedef, [], [e124])).
fof(e124, plain, (sP102 <=> ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP102])])).
fof(f125, plain, (((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP103), inference(usedef, [], [e125])).
fof(e125, plain, (sP103 <=> ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP103])])).
fof(f126, plain, (((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP104), inference(usedef, [], [e126])).
fof(e126, plain, (sP104 <=> ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP104])])).
fof(f127, plain, (((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP105), inference(usedef, [], [e127])).
fof(e127, plain, (sP105 <=> ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP105])])).
fof(f128, plain, (((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP106), inference(usedef, [], [e128])).
fof(e128, plain, (sP106 <=> ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP106])])).
fof(f129, plain, (((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP107), inference(usedef, [], [e129])).
fof(e129, plain, (sP107 <=> ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP107])])).
fof(f130, plain, (((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP108), inference(usedef, [], [e130])).
fof(e130, plain, (sP108 <=> ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP108])])).
fof(f131, plain, (((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP109), inference(usedef, [], [e131])).
fof(e131, plain, (sP109 <=> ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP109])])).
fof(f132, plain, (((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP110), inference(usedef, [], [e132])).
fof(e132, plain, (sP110 <=> ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP110])])).
fof(f133, plain, (((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP111), inference(usedef, [], [e133])).
fof(e133, plain, (sP111 <=> ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP111])])).
fof(f134, plain, (((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP112), inference(usedef, [], [e134])).
fof(e134, plain, (sP112 <=> ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP112])])).
fof(f135, plain, (((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP113), inference(usedef, [], [e135])).
fof(e135, plain, (sP113 <=> ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP113])])).
fof(f136, plain, (((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP114), inference(usedef, [], [e136])).
fof(e136, plain, (sP114 <=> ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP114])])).
fof(f137, plain, (((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP115), inference(usedef, [], [e137])).
fof(e137, plain, (sP115 <=> ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP115])])).
fof(f138, plain, (((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP116), inference(usedef, [], [e138])).
fof(e138, plain, (sP116 <=> ((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP116])])).
fof(f139, plain, (((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP117), inference(usedef, [], [e139])).
fof(e139, plain, (sP117 <=> ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP117])])).
fof(f140, plain, (((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP118), inference(usedef, [], [e140])).
fof(e140, plain, (sP118 <=> ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP118])])).
fof(f141, plain, (((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP119), inference(usedef, [], [e141])).
fof(e141, plain, (sP119 <=> ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP119])])).
fof(f142, plain, (((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP120), inference(usedef, [], [e142])).
fof(e142, plain, (sP120 <=> ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP120])])).
fof(f143, plain, (((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP121), inference(usedef, [], [e143])).
fof(e143, plain, (sP121 <=> ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP121])])).
fof(f144, plain, (((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP122), inference(usedef, [], [e144])).
fof(e144, plain, (sP122 <=> ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP122])])).
fof(f145, plain, (((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP123), inference(usedef, [], [e145])).
fof(e145, plain, (sP123 <=> ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP123])])).
fof(f146, plain, (((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP124), inference(usedef, [], [e146])).
fof(e146, plain, (sP124 <=> ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP124])])).
fof(f147, plain, (((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP125), inference(usedef, [], [e147])).
fof(e147, plain, (sP125 <=> ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP125])])).
fof(f148, plain, (((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP126), inference(usedef, [], [e148])).
fof(e148, plain, (sP126 <=> ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP126])])).
fof(f149, plain, (((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP127), inference(usedef, [], [e149])).
fof(e149, plain, (sP127 <=> ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP127])])).
fof(f150, plain, (((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP128), inference(usedef, [], [e150])).
fof(e150, plain, (sP128 <=> ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP128])])).
fof(f151, plain, (((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP129), inference(usedef, [], [e151])).
fof(e151, plain, (sP129 <=> ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP129])])).
fof(f152, plain, (((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP130), inference(usedef, [], [e152])).
fof(e152, plain, (sP130 <=> ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP130])])).
fof(f153, plain, (((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP131), inference(usedef, [], [e153])).
fof(e153, plain, (sP131 <=> ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP131])])).
fof(f11, plain, ((((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ((e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ((e23 = op2(e23, e22)) & (e23 = op2(e22, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ((e23 = op2(e23, e21)) & (e23 = op2(e21, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ((e23 = op2(e23, e20)) & (e23 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ((e22 = op2(e22, e23)) & (e22 = op2(e23, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ((e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ((e22 = op2(e22, e21)) & (e22 = op2(e21, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ((e22 = op2(e22, e20)) & (e22 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ((e21 = op2(e21, e23)) & (e21 = op2(e23, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ((e21 = op2(e21, e22)) & (e21 = op2(e22, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ((e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ((e21 = op2(e21, e20)) & (e21 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ((e20 = op2(e20, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ((e20 = op2(e20, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ((e20 = op2(e20, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ((e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20)))) | (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20)))) | (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20)))) | (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))))) & ((e23 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax11)).
fof(f2984, plain, (spl144_121 | spl144_106 | spl144_91 | spl144_76), inference(avatar_split_clause, [], [f1073, f1521, f1585, f1649, f1713])).
fof(f1073, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f154])).
fof(f2983, plain, (spl144_117 | spl144_102 | spl144_87 | spl144_72), inference(avatar_split_clause, [], [f1074, f1504, f1568, f1632, f1696])).
fof(f1074, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f154])).
fof(f2982, plain, (spl144_113 | spl144_98 | spl144_83 | spl144_68), inference(avatar_split_clause, [], [f1075, f1487, f1551, f1615, f1679])).
fof(f1075, plain, ((e23 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f154])).
fof(f2976, plain, (spl144_284 | spl144_279 | spl144_274 | ~ spl144_291), inference(avatar_split_clause, [], [f1077, f2973, f2887, f2911, f2935])).
fof(f2935, plain, (spl144_284 <=> sP66), introduced(avatar_definition, [new_symbols(naming, [spl144_284])])).
fof(f2911, plain, (spl144_279 <=> sP67), introduced(avatar_definition, [new_symbols(naming, [spl144_279])])).
fof(f2887, plain, (spl144_274 <=> sP68), introduced(avatar_definition, [new_symbols(naming, [spl144_274])])).
fof(f1077, plain, (~ (e21 = op2(e23, op2(e23, e21))) | sP68 | sP67 | sP66), inference(cnf_transformation, [], [f154])).
fof(f2957, plain, (~ spl144_284 | ~ spl144_288), inference(avatar_split_clause, [], [f1068, f2954, f2935])).
fof(f1068, plain, (~ (e20 = op2(e20, op2(e20, e20))) | ~ sP66), inference(cnf_transformation, [], [f299])).
fof(f299, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20)))) | ~ sP66), inference(nnf_transformation, [], [f88])).
fof(f2923, plain, (~ spl144_279 | ~ spl144_281), inference(avatar_split_clause, [], [f1066, f2920, f2911])).
fof(f1066, plain, (~ (e22 = op2(e21, op2(e21, e22))) | ~ sP67), inference(cnf_transformation, [], [f298])).
fof(f298, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20)))) | ~ sP67), inference(nnf_transformation, [], [f89])).
fof(f2894, plain, (~ spl144_274 | ~ spl144_275), inference(avatar_split_clause, [], [f1063, f2891, f2887])).
fof(f1063, plain, (~ (e23 = op2(e22, op2(e22, e23))) | ~ sP68), inference(cnf_transformation, [], [f297])).
fof(f297, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20)))) | ~ sP68), inference(nnf_transformation, [], [f90])).
fof(f2381, plain, (spl144_61 | spl144_46 | spl144_31 | spl144_16), inference(avatar_split_clause, [], [f796, f1234, f1298, f1362, f1426])).
fof(f796, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP65 | sP64 | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3) & ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10)))) | sP2 | sP1 | sP0) & ((e13 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e10, e10)))), inference(definition_folding, [], [f10, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10)))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10)))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10)))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f36, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP15), inference(usedef, [], [e36])).
fof(e36, plain, (sP15 <=> ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f37, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP16), inference(usedef, [], [e37])).
fof(e37, plain, (sP16 <=> ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f38, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP17), inference(usedef, [], [e38])).
fof(e38, plain, (sP17 <=> ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f39, plain, (((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP18), inference(usedef, [], [e39])).
fof(e39, plain, (sP18 <=> ((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f40, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(usedef, [], [e40])).
fof(e40, plain, (sP19 <=> ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f41, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP20), inference(usedef, [], [e41])).
fof(e41, plain, (sP20 <=> ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f42, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP21), inference(usedef, [], [e42])).
fof(e42, plain, (sP21 <=> ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f43, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP22), inference(usedef, [], [e43])).
fof(e43, plain, (sP22 <=> ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f44, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(usedef, [], [e44])).
fof(e44, plain, (sP23 <=> ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f45, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP24), inference(usedef, [], [e45])).
fof(e45, plain, (sP24 <=> ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f46, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP25), inference(usedef, [], [e46])).
fof(e46, plain, (sP25 <=> ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f47, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP26), inference(usedef, [], [e47])).
fof(e47, plain, (sP26 <=> ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f48, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(usedef, [], [e48])).
fof(e48, plain, (sP27 <=> ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f49, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP28), inference(usedef, [], [e49])).
fof(e49, plain, (sP28 <=> ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f50, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP29), inference(usedef, [], [e50])).
fof(e50, plain, (sP29 <=> ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f51, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP30), inference(usedef, [], [e51])).
fof(e51, plain, (sP30 <=> ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f52, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(usedef, [], [e52])).
fof(e52, plain, (sP31 <=> ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f53, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP32), inference(usedef, [], [e53])).
fof(e53, plain, (sP32 <=> ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f54, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP33), inference(usedef, [], [e54])).
fof(e54, plain, (sP33 <=> ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f55, plain, (((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP34), inference(usedef, [], [e55])).
fof(e55, plain, (sP34 <=> ((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f56, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(usedef, [], [e56])).
fof(e56, plain, (sP35 <=> ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f57, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP36), inference(usedef, [], [e57])).
fof(e57, plain, (sP36 <=> ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f58, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP37), inference(usedef, [], [e58])).
fof(e58, plain, (sP37 <=> ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f59, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP38), inference(usedef, [], [e59])).
fof(e59, plain, (sP38 <=> ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f60, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(usedef, [], [e60])).
fof(e60, plain, (sP39 <=> ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f61, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP40), inference(usedef, [], [e61])).
fof(e61, plain, (sP40 <=> ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f62, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP41), inference(usedef, [], [e62])).
fof(e62, plain, (sP41 <=> ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f63, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP42), inference(usedef, [], [e63])).
fof(e63, plain, (sP42 <=> ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f64, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(usedef, [], [e64])).
fof(e64, plain, (sP43 <=> ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f65, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP44), inference(usedef, [], [e65])).
fof(e65, plain, (sP44 <=> ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f66, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP45), inference(usedef, [], [e66])).
fof(e66, plain, (sP45 <=> ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f67, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP46), inference(usedef, [], [e67])).
fof(e67, plain, (sP46 <=> ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f68, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(usedef, [], [e68])).
fof(e68, plain, (sP47 <=> ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f69, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP48), inference(usedef, [], [e69])).
fof(e69, plain, (sP48 <=> ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f70, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP49), inference(usedef, [], [e70])).
fof(e70, plain, (sP49 <=> ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f71, plain, (((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP50), inference(usedef, [], [e71])).
fof(e71, plain, (sP50 <=> ((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f72, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(usedef, [], [e72])).
fof(e72, plain, (sP51 <=> ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f73, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP52), inference(usedef, [], [e73])).
fof(e73, plain, (sP52 <=> ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f74, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP53), inference(usedef, [], [e74])).
fof(e74, plain, (sP53 <=> ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f75, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP54), inference(usedef, [], [e75])).
fof(e75, plain, (sP54 <=> ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f76, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(usedef, [], [e76])).
fof(e76, plain, (sP55 <=> ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f77, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP56), inference(usedef, [], [e77])).
fof(e77, plain, (sP56 <=> ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f78, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP57), inference(usedef, [], [e78])).
fof(e78, plain, (sP57 <=> ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f79, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP58), inference(usedef, [], [e79])).
fof(e79, plain, (sP58 <=> ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f80, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(usedef, [], [e80])).
fof(e80, plain, (sP59 <=> ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f81, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP60), inference(usedef, [], [e81])).
fof(e81, plain, (sP60 <=> ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f82, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP61), inference(usedef, [], [e82])).
fof(e82, plain, (sP61 <=> ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f83, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP62), inference(usedef, [], [e83])).
fof(e83, plain, (sP62 <=> ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f84, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP63), inference(usedef, [], [e84])).
fof(e84, plain, (sP63 <=> ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f85, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP64), inference(usedef, [], [e85])).
fof(e85, plain, (sP64 <=> ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP64])])).
fof(f86, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP65), inference(usedef, [], [e86])).
fof(e86, plain, (sP65 <=> ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP65])])).
fof(f10, plain, ((((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10)))) | (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10)))) | (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10)))) | (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))))) & ((e13 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax10)).
fof(f2380, plain, (spl144_57 | spl144_42 | spl144_27 | spl144_12), inference(avatar_split_clause, [], [f797, f1217, f1281, f1345, f1409])).
fof(f797, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f87])).
fof(f2379, plain, (spl144_53 | spl144_38 | spl144_23 | spl144_8), inference(avatar_split_clause, [], [f798, f1200, f1264, f1328, f1392])).
fof(f798, plain, ((e13 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f87])).
fof(f2378, plain, (spl144_49 | spl144_34 | spl144_19 | spl144_4), inference(avatar_split_clause, [], [f799, f1183, f1247, f1311, f1375])).
fof(f799, plain, ((e13 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f87])).
fof(f2372, plain, (spl144_202 | spl144_197 | spl144_192 | ~ spl144_209), inference(avatar_split_clause, [], [f801, f2369, f2283, f2307, f2331])).
fof(f2331, plain, (spl144_202 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl144_202])])).
fof(f2307, plain, (spl144_197 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl144_197])])).
fof(f2283, plain, (spl144_192 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl144_192])])).
fof(f801, plain, (~ (e11 = op1(e13, op1(e13, e11))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f87])).
fof(f2357, plain, (spl144_191 | spl144_190 | spl144_189 | spl144_188 | spl144_187 | spl144_186 | spl144_185 | spl144_184 | spl144_183 | spl144_182 | spl144_181 | spl144_180 | spl144_179 | spl144_178 | spl144_177 | spl144_176 | spl144_175 | spl144_174 | spl144_173 | spl144_172 | spl144_171 | spl144_170 | spl144_169 | spl144_168 | spl144_167 | spl144_166 | spl144_165 | spl144_164 | spl144_163 | spl144_162 | spl144_161 | spl144_160 | spl144_159 | spl144_158 | spl144_157 | spl144_156 | spl144_155 | spl144_154 | spl144_153 | spl144_152 | spl144_151 | spl144_150 | spl144_149 | spl144_148 | spl144_147 | spl144_146 | spl144_145 | spl144_144 | spl144_143 | spl144_142 | spl144_141 | spl144_140 | spl144_139 | spl144_138 | spl144_137 | spl144_136 | spl144_135 | spl144_134 | spl144_133 | spl144_132 | spl144_131 | spl144_130 | spl144_129 | spl144_4), inference(avatar_split_clause, [], [f804, f1183, f1779, f1787, f1795, f1803, f1811, f1819, f1827, f1835, f1843, f1851, f1859, f1867, f1875, f1883, f1891, f1899, f1907, f1915, f1923, f1931, f1939, f1947, f1955, f1963, f1971, f1979, f1987, f1995, f2003, f2011, f2019, f2027, f2035, f2043, f2051, f2059, f2067, f2075, f2083, f2091, f2099, f2107, f2115, f2123, f2131, f2139, f2147, f2155, f2163, f2171, f2179, f2187, f2195, f2203, f2211, f2219, f2227, f2235, f2243, f2251, f2259, f2267, f2275])).
fof(f2275, plain, (spl144_191 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl144_191])])).
fof(f2267, plain, (spl144_190 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl144_190])])).
fof(f2259, plain, (spl144_189 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl144_189])])).
fof(f2251, plain, (spl144_188 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl144_188])])).
fof(f2243, plain, (spl144_187 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl144_187])])).
fof(f2235, plain, (spl144_186 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl144_186])])).
fof(f2227, plain, (spl144_185 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl144_185])])).
fof(f2219, plain, (spl144_184 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl144_184])])).
fof(f2211, plain, (spl144_183 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl144_183])])).
fof(f2203, plain, (spl144_182 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl144_182])])).
fof(f2195, plain, (spl144_181 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl144_181])])).
fof(f2187, plain, (spl144_180 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl144_180])])).
fof(f2179, plain, (spl144_179 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl144_179])])).
fof(f2171, plain, (spl144_178 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl144_178])])).
fof(f2163, plain, (spl144_177 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl144_177])])).
fof(f2155, plain, (spl144_176 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl144_176])])).
fof(f2147, plain, (spl144_175 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl144_175])])).
fof(f2139, plain, (spl144_174 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl144_174])])).
fof(f2131, plain, (spl144_173 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl144_173])])).
fof(f2123, plain, (spl144_172 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl144_172])])).
fof(f2115, plain, (spl144_171 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl144_171])])).
fof(f2107, plain, (spl144_170 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl144_170])])).
fof(f2099, plain, (spl144_169 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl144_169])])).
fof(f2091, plain, (spl144_168 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl144_168])])).
fof(f2083, plain, (spl144_167 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl144_167])])).
fof(f2075, plain, (spl144_166 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl144_166])])).
fof(f2067, plain, (spl144_165 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl144_165])])).
fof(f2059, plain, (spl144_164 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl144_164])])).
fof(f2051, plain, (spl144_163 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl144_163])])).
fof(f2043, plain, (spl144_162 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl144_162])])).
fof(f2035, plain, (spl144_161 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl144_161])])).
fof(f2027, plain, (spl144_160 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl144_160])])).
fof(f2019, plain, (spl144_159 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl144_159])])).
fof(f2011, plain, (spl144_158 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl144_158])])).
fof(f2003, plain, (spl144_157 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl144_157])])).
fof(f1995, plain, (spl144_156 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl144_156])])).
fof(f1987, plain, (spl144_155 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl144_155])])).
fof(f1979, plain, (spl144_154 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl144_154])])).
fof(f1971, plain, (spl144_153 <=> sP41), introduced(avatar_definition, [new_symbols(naming, [spl144_153])])).
fof(f1963, plain, (spl144_152 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl144_152])])).
fof(f1955, plain, (spl144_151 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl144_151])])).
fof(f1947, plain, (spl144_150 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl144_150])])).
fof(f1939, plain, (spl144_149 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl144_149])])).
fof(f1931, plain, (spl144_148 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl144_148])])).
fof(f1923, plain, (spl144_147 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl144_147])])).
fof(f1915, plain, (spl144_146 <=> sP48), introduced(avatar_definition, [new_symbols(naming, [spl144_146])])).
fof(f1907, plain, (spl144_145 <=> sP49), introduced(avatar_definition, [new_symbols(naming, [spl144_145])])).
fof(f1899, plain, (spl144_144 <=> sP50), introduced(avatar_definition, [new_symbols(naming, [spl144_144])])).
fof(f1891, plain, (spl144_143 <=> sP51), introduced(avatar_definition, [new_symbols(naming, [spl144_143])])).
fof(f1883, plain, (spl144_142 <=> sP52), introduced(avatar_definition, [new_symbols(naming, [spl144_142])])).
fof(f1875, plain, (spl144_141 <=> sP53), introduced(avatar_definition, [new_symbols(naming, [spl144_141])])).
fof(f1867, plain, (spl144_140 <=> sP54), introduced(avatar_definition, [new_symbols(naming, [spl144_140])])).
fof(f1859, plain, (spl144_139 <=> sP55), introduced(avatar_definition, [new_symbols(naming, [spl144_139])])).
fof(f1851, plain, (spl144_138 <=> sP56), introduced(avatar_definition, [new_symbols(naming, [spl144_138])])).
fof(f1843, plain, (spl144_137 <=> sP57), introduced(avatar_definition, [new_symbols(naming, [spl144_137])])).
fof(f1835, plain, (spl144_136 <=> sP58), introduced(avatar_definition, [new_symbols(naming, [spl144_136])])).
fof(f1827, plain, (spl144_135 <=> sP59), introduced(avatar_definition, [new_symbols(naming, [spl144_135])])).
fof(f1819, plain, (spl144_134 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl144_134])])).
fof(f1811, plain, (spl144_133 <=> sP61), introduced(avatar_definition, [new_symbols(naming, [spl144_133])])).
fof(f1803, plain, (spl144_132 <=> sP62), introduced(avatar_definition, [new_symbols(naming, [spl144_132])])).
fof(f1795, plain, (spl144_131 <=> sP63), introduced(avatar_definition, [new_symbols(naming, [spl144_131])])).
fof(f1787, plain, (spl144_130 <=> sP64), introduced(avatar_definition, [new_symbols(naming, [spl144_130])])).
fof(f1779, plain, (spl144_129 <=> sP65), introduced(avatar_definition, [new_symbols(naming, [spl144_129])])).
fof(f804, plain, ((e13 = op1(e13, e13)) | sP65 | sP64 | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f87])).
fof(f2353, plain, (~ spl144_202 | ~ spl144_206), inference(avatar_split_clause, [], [f792, f2350, f2331])).
fof(f792, plain, (~ (e10 = op1(e10, op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f233])).
fof(f233, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10)))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f2319, plain, (~ spl144_197 | ~ spl144_199), inference(avatar_split_clause, [], [f790, f2316, f2307])).
fof(f790, plain, (~ (e12 = op1(e11, op1(e11, e12))) | ~ sP1), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10)))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f2290, plain, (~ spl144_192 | ~ spl144_193), inference(avatar_split_clause, [], [f787, f2287, f2283])).
fof(f787, plain, (~ (e13 = op1(e12, op1(e12, e13))) | ~ sP2), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10)))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f2281, plain, (~ spl144_191 | spl144_61), inference(avatar_split_clause, [], [f780, f1426, f2275])).
fof(f780, plain, ((e10 = op1(e10, e10)) | ~ sP3), inference(cnf_transformation, [], [f230])).
fof(f230, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f2271, plain, (~ spl144_190 | spl144_45), inference(avatar_split_clause, [], [f778, f1358, f2267])).
fof(f778, plain, ((e10 = op1(e11, e10)) | ~ sP4), inference(cnf_transformation, [], [f229])).
fof(f229, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f2265, plain, (~ spl144_189 | spl144_61), inference(avatar_split_clause, [], [f772, f1426, f2259])).
fof(f772, plain, ((e10 = op1(e10, e10)) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f228, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f2257, plain, (~ spl144_188 | spl144_61), inference(avatar_split_clause, [], [f768, f1426, f2251])).
fof(f768, plain, ((e10 = op1(e10, e10)) | ~ sP6), inference(cnf_transformation, [], [f227])).
fof(f227, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f2249, plain, (~ spl144_187 | spl144_57), inference(avatar_split_clause, [], [f764, f1409, f2243])).
fof(f764, plain, ((e10 = op1(e10, e11)) | ~ sP7), inference(cnf_transformation, [], [f226])).
fof(f226, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f2241, plain, (~ spl144_186 | spl144_57), inference(avatar_split_clause, [], [f760, f1409, f2235])).
fof(f760, plain, ((e10 = op1(e10, e11)) | ~ sP8), inference(cnf_transformation, [], [f225])).
fof(f225, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f2233, plain, (~ spl144_185 | spl144_57), inference(avatar_split_clause, [], [f756, f1409, f2227])).
fof(f756, plain, ((e10 = op1(e10, e11)) | ~ sP9), inference(cnf_transformation, [], [f224])).
fof(f224, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f2225, plain, (~ spl144_184 | spl144_57), inference(avatar_split_clause, [], [f752, f1409, f2219])).
fof(f752, plain, ((e10 = op1(e10, e11)) | ~ sP10), inference(cnf_transformation, [], [f223])).
fof(f223, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f2214, plain, (~ spl144_183 | spl144_31), inference(avatar_split_clause, [], [f751, f1298, f2211])).
fof(f751, plain, ((e12 = op1(e12, e10)) | ~ sP11), inference(cnf_transformation, [], [f222])).
fof(f222, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f2209, plain, (~ spl144_182 | spl144_53), inference(avatar_split_clause, [], [f744, f1392, f2203])).
fof(f744, plain, ((e10 = op1(e10, e12)) | ~ sP12), inference(cnf_transformation, [], [f221])).
fof(f221, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f2199, plain, (~ spl144_181 | spl144_23), inference(avatar_split_clause, [], [f742, f1264, f2195])).
fof(f742, plain, ((e12 = op1(e12, e12)) | ~ sP13), inference(cnf_transformation, [], [f220])).
fof(f220, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f2190, plain, (~ spl144_180 | spl144_19), inference(avatar_split_clause, [], [f739, f1247, f2187])).
fof(f739, plain, ((e12 = op1(e12, e13)) | ~ sP14), inference(cnf_transformation, [], [f219])).
fof(f219, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f2182, plain, (~ spl144_179 | spl144_16), inference(avatar_split_clause, [], [f735, f1234, f2179])).
fof(f735, plain, ((e13 = op1(e13, e10)) | ~ sP15), inference(cnf_transformation, [], [f218])).
fof(f218, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP15), inference(nnf_transformation, [], [f36])).
fof(f2174, plain, (~ spl144_178 | spl144_12), inference(avatar_split_clause, [], [f731, f1217, f2171])).
fof(f731, plain, ((e13 = op1(e13, e11)) | ~ sP16), inference(cnf_transformation, [], [f217])).
fof(f217, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP16), inference(nnf_transformation, [], [f37])).
fof(f2167, plain, (~ spl144_177 | spl144_20), inference(avatar_split_clause, [], [f726, f1251, f2163])).
fof(f726, plain, ((e13 = op1(e12, e13)) | ~ sP17), inference(cnf_transformation, [], [f216])).
fof(f216, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP17), inference(nnf_transformation, [], [f38])).
fof(f2159, plain, (~ spl144_176 | spl144_4), inference(avatar_split_clause, [], [f722, f1183, f2155])).
fof(f722, plain, ((e13 = op1(e13, e13)) | ~ sP18), inference(cnf_transformation, [], [f215])).
fof(f215, plain, (((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP18), inference(nnf_transformation, [], [f39])).
fof(f2152, plain, (~ spl144_175 | spl144_58), inference(avatar_split_clause, [], [f717, f1413, f2147])).
fof(f717, plain, ((e11 = op1(e10, e11)) | ~ sP19), inference(cnf_transformation, [], [f214])).
fof(f214, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(nnf_transformation, [], [f40])).
fof(f2144, plain, (~ spl144_174 | spl144_58), inference(avatar_split_clause, [], [f713, f1413, f2139])).
fof(f713, plain, ((e11 = op1(e10, e11)) | ~ sP20), inference(cnf_transformation, [], [f213])).
fof(f213, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP20), inference(nnf_transformation, [], [f41])).
fof(f2136, plain, (~ spl144_173 | spl144_58), inference(avatar_split_clause, [], [f709, f1413, f2131])).
fof(f709, plain, ((e11 = op1(e10, e11)) | ~ sP21), inference(cnf_transformation, [], [f212])).
fof(f212, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP21), inference(nnf_transformation, [], [f42])).
fof(f2128, plain, (~ spl144_172 | spl144_58), inference(avatar_split_clause, [], [f705, f1413, f2123])).
fof(f705, plain, ((e11 = op1(e10, e11)) | ~ sP22), inference(cnf_transformation, [], [f211])).
fof(f211, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP22), inference(nnf_transformation, [], [f43])).
fof(f2121, plain, (~ spl144_171 | spl144_42), inference(avatar_split_clause, [], [f700, f1345, f2115])).
fof(f700, plain, ((e11 = op1(e11, e11)) | ~ sP23), inference(cnf_transformation, [], [f210])).
fof(f210, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(nnf_transformation, [], [f44])).
fof(f2113, plain, (~ spl144_170 | spl144_42), inference(avatar_split_clause, [], [f696, f1345, f2107])).
fof(f696, plain, ((e11 = op1(e11, e11)) | ~ sP24), inference(cnf_transformation, [], [f209])).
fof(f209, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP24), inference(nnf_transformation, [], [f45])).
fof(f2105, plain, (~ spl144_169 | spl144_42), inference(avatar_split_clause, [], [f692, f1345, f2099])).
fof(f692, plain, ((e11 = op1(e11, e11)) | ~ sP25), inference(cnf_transformation, [], [f208])).
fof(f208, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP25), inference(nnf_transformation, [], [f46])).
fof(f2097, plain, (~ spl144_168 | spl144_42), inference(avatar_split_clause, [], [f688, f1345, f2091])).
fof(f688, plain, ((e11 = op1(e11, e11)) | ~ sP26), inference(cnf_transformation, [], [f207])).
fof(f207, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP26), inference(nnf_transformation, [], [f47])).
fof(f2086, plain, (~ spl144_167 | spl144_31), inference(avatar_split_clause, [], [f687, f1298, f2083])).
fof(f687, plain, ((e12 = op1(e12, e10)) | ~ sP27), inference(cnf_transformation, [], [f206])).
fof(f206, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(nnf_transformation, [], [f48])).
fof(f2081, plain, (~ spl144_166 | spl144_38), inference(avatar_split_clause, [], [f680, f1328, f2075])).
fof(f680, plain, ((e11 = op1(e11, e12)) | ~ sP28), inference(cnf_transformation, [], [f205])).
fof(f205, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP28), inference(nnf_transformation, [], [f49])).
fof(f2071, plain, (~ spl144_165 | spl144_23), inference(avatar_split_clause, [], [f678, f1264, f2067])).
fof(f678, plain, ((e12 = op1(e12, e12)) | ~ sP29), inference(cnf_transformation, [], [f204])).
fof(f204, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP29), inference(nnf_transformation, [], [f50])).
fof(f2062, plain, (~ spl144_164 | spl144_19), inference(avatar_split_clause, [], [f675, f1247, f2059])).
fof(f675, plain, ((e12 = op1(e12, e13)) | ~ sP30), inference(cnf_transformation, [], [f203])).
fof(f203, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP30), inference(nnf_transformation, [], [f51])).
fof(f2054, plain, (~ spl144_163 | spl144_16), inference(avatar_split_clause, [], [f671, f1234, f2051])).
fof(f671, plain, ((e13 = op1(e13, e10)) | ~ sP31), inference(cnf_transformation, [], [f202])).
fof(f202, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(nnf_transformation, [], [f52])).
fof(f2046, plain, (~ spl144_162 | spl144_12), inference(avatar_split_clause, [], [f667, f1217, f2043])).
fof(f667, plain, ((e13 = op1(e13, e11)) | ~ sP32), inference(cnf_transformation, [], [f201])).
fof(f201, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP32), inference(nnf_transformation, [], [f53])).
fof(f2041, plain, (~ spl144_161 | spl144_34), inference(avatar_split_clause, [], [f660, f1311, f2035])).
fof(f660, plain, ((e11 = op1(e11, e13)) | ~ sP33), inference(cnf_transformation, [], [f200])).
fof(f200, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP33), inference(nnf_transformation, [], [f54])).
fof(f2033, plain, (~ spl144_160 | spl144_34), inference(avatar_split_clause, [], [f656, f1311, f2027])).
fof(f656, plain, ((e11 = op1(e11, e13)) | ~ sP34), inference(cnf_transformation, [], [f199])).
fof(f199, plain, (((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP34), inference(nnf_transformation, [], [f55])).
fof(f2025, plain, (~ spl144_159 | spl144_31), inference(avatar_split_clause, [], [f652, f1298, f2019])).
fof(f652, plain, ((e12 = op1(e12, e10)) | ~ sP35), inference(cnf_transformation, [], [f198])).
fof(f198, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(nnf_transformation, [], [f56])).
fof(f2017, plain, (~ spl144_158 | spl144_31), inference(avatar_split_clause, [], [f648, f1298, f2011])).
fof(f648, plain, ((e12 = op1(e12, e10)) | ~ sP36), inference(cnf_transformation, [], [f197])).
fof(f197, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP36), inference(nnf_transformation, [], [f57])).
fof(f2009, plain, (~ spl144_157 | spl144_31), inference(avatar_split_clause, [], [f644, f1298, f2003])).
fof(f644, plain, ((e12 = op1(e12, e10)) | ~ sP37), inference(cnf_transformation, [], [f196])).
fof(f196, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP37), inference(nnf_transformation, [], [f58])).
fof(f2001, plain, (~ spl144_156 | spl144_31), inference(avatar_split_clause, [], [f640, f1298, f1995])).
fof(f640, plain, ((e12 = op1(e12, e10)) | ~ sP38), inference(cnf_transformation, [], [f195])).
fof(f195, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP38), inference(nnf_transformation, [], [f59])).
fof(f1991, plain, (~ spl144_155 | spl144_58), inference(avatar_split_clause, [], [f638, f1413, f1987])).
fof(f638, plain, ((e11 = op1(e10, e11)) | ~ sP39), inference(cnf_transformation, [], [f194])).
fof(f194, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(nnf_transformation, [], [f60])).
fof(f1983, plain, (~ spl144_154 | spl144_42), inference(avatar_split_clause, [], [f634, f1345, f1979])).
fof(f634, plain, ((e11 = op1(e11, e11)) | ~ sP40), inference(cnf_transformation, [], [f193])).
fof(f193, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP40), inference(nnf_transformation, [], [f61])).
fof(f1974, plain, (~ spl144_153 | spl144_38), inference(avatar_split_clause, [], [f631, f1328, f1971])).
fof(f631, plain, ((e11 = op1(e11, e12)) | ~ sP41), inference(cnf_transformation, [], [f192])).
fof(f192, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP41), inference(nnf_transformation, [], [f62])).
fof(f1966, plain, (~ spl144_152 | spl144_34), inference(avatar_split_clause, [], [f627, f1311, f1963])).
fof(f627, plain, ((e11 = op1(e11, e13)) | ~ sP42), inference(cnf_transformation, [], [f191])).
fof(f191, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP42), inference(nnf_transformation, [], [f63])).
fof(f1961, plain, (~ spl144_151 | spl144_23), inference(avatar_split_clause, [], [f620, f1264, f1955])).
fof(f620, plain, ((e12 = op1(e12, e12)) | ~ sP43), inference(cnf_transformation, [], [f190])).
fof(f190, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(nnf_transformation, [], [f64])).
fof(f1953, plain, (~ spl144_150 | spl144_23), inference(avatar_split_clause, [], [f616, f1264, f1947])).
fof(f616, plain, ((e12 = op1(e12, e12)) | ~ sP44), inference(cnf_transformation, [], [f189])).
fof(f189, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP44), inference(nnf_transformation, [], [f65])).
fof(f1945, plain, (~ spl144_149 | spl144_23), inference(avatar_split_clause, [], [f612, f1264, f1939])).
fof(f612, plain, ((e12 = op1(e12, e12)) | ~ sP45), inference(cnf_transformation, [], [f188])).
fof(f188, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP45), inference(nnf_transformation, [], [f66])).
fof(f1937, plain, (~ spl144_148 | spl144_23), inference(avatar_split_clause, [], [f608, f1264, f1931])).
fof(f608, plain, ((e12 = op1(e12, e12)) | ~ sP46), inference(cnf_transformation, [], [f187])).
fof(f187, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP46), inference(nnf_transformation, [], [f67])).
fof(f1929, plain, (~ spl144_147 | spl144_19), inference(avatar_split_clause, [], [f604, f1247, f1923])).
fof(f604, plain, ((e12 = op1(e12, e13)) | ~ sP47), inference(cnf_transformation, [], [f186])).
fof(f186, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(nnf_transformation, [], [f68])).
fof(f1921, plain, (~ spl144_146 | spl144_19), inference(avatar_split_clause, [], [f600, f1247, f1915])).
fof(f600, plain, ((e12 = op1(e12, e13)) | ~ sP48), inference(cnf_transformation, [], [f185])).
fof(f185, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP48), inference(nnf_transformation, [], [f69])).
fof(f1913, plain, (~ spl144_145 | spl144_19), inference(avatar_split_clause, [], [f596, f1247, f1907])).
fof(f596, plain, ((e12 = op1(e12, e13)) | ~ sP49), inference(cnf_transformation, [], [f184])).
fof(f184, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP49), inference(nnf_transformation, [], [f70])).
fof(f1905, plain, (~ spl144_144 | spl144_19), inference(avatar_split_clause, [], [f592, f1247, f1899])).
fof(f592, plain, ((e12 = op1(e12, e13)) | ~ sP50), inference(cnf_transformation, [], [f183])).
fof(f183, plain, (((e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP50), inference(nnf_transformation, [], [f71])).
fof(f1897, plain, (~ spl144_143 | spl144_16), inference(avatar_split_clause, [], [f588, f1234, f1891])).
fof(f588, plain, ((e13 = op1(e13, e10)) | ~ sP51), inference(cnf_transformation, [], [f182])).
fof(f182, plain, (((e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(nnf_transformation, [], [f72])).
fof(f1889, plain, (~ spl144_142 | spl144_16), inference(avatar_split_clause, [], [f584, f1234, f1883])).
fof(f584, plain, ((e13 = op1(e13, e10)) | ~ sP52), inference(cnf_transformation, [], [f181])).
fof(f181, plain, (((e10 = op1(e10, e11)) & (e10 = op1(e11, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP52), inference(nnf_transformation, [], [f73])).
fof(f1881, plain, (~ spl144_141 | spl144_16), inference(avatar_split_clause, [], [f580, f1234, f1875])).
fof(f580, plain, ((e13 = op1(e13, e10)) | ~ sP53), inference(cnf_transformation, [], [f180])).
fof(f180, plain, (((e10 = op1(e10, e12)) & (e10 = op1(e12, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP53), inference(nnf_transformation, [], [f74])).
fof(f1873, plain, (~ spl144_140 | spl144_16), inference(avatar_split_clause, [], [f576, f1234, f1867])).
fof(f576, plain, ((e13 = op1(e13, e10)) | ~ sP54), inference(cnf_transformation, [], [f179])).
fof(f179, plain, (((e10 = op1(e10, e13)) & (e10 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP54), inference(nnf_transformation, [], [f75])).
fof(f1865, plain, (~ spl144_139 | spl144_12), inference(avatar_split_clause, [], [f572, f1217, f1859])).
fof(f572, plain, ((e13 = op1(e13, e11)) | ~ sP55), inference(cnf_transformation, [], [f178])).
fof(f178, plain, (((e11 = op1(e11, e10)) & (e11 = op1(e10, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(nnf_transformation, [], [f76])).
fof(f1857, plain, (~ spl144_138 | spl144_12), inference(avatar_split_clause, [], [f568, f1217, f1851])).
fof(f568, plain, ((e13 = op1(e13, e11)) | ~ sP56), inference(cnf_transformation, [], [f177])).
fof(f177, plain, (((e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP56), inference(nnf_transformation, [], [f77])).
fof(f1849, plain, (~ spl144_137 | spl144_12), inference(avatar_split_clause, [], [f564, f1217, f1843])).
fof(f564, plain, ((e13 = op1(e13, e11)) | ~ sP57), inference(cnf_transformation, [], [f176])).
fof(f176, plain, (((e11 = op1(e11, e12)) & (e11 = op1(e12, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP57), inference(nnf_transformation, [], [f78])).
fof(f1841, plain, (~ spl144_136 | spl144_12), inference(avatar_split_clause, [], [f560, f1217, f1835])).
fof(f560, plain, ((e13 = op1(e13, e11)) | ~ sP58), inference(cnf_transformation, [], [f175])).
fof(f175, plain, (((e11 = op1(e11, e13)) & (e11 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP58), inference(nnf_transformation, [], [f79])).
fof(f1830, plain, (~ spl144_135 | spl144_31), inference(avatar_split_clause, [], [f559, f1298, f1827])).
fof(f559, plain, ((e12 = op1(e12, e10)) | ~ sP59), inference(cnf_transformation, [], [f174])).
fof(f174, plain, (((e12 = op1(e12, e10)) & (e12 = op1(e10, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(nnf_transformation, [], [f80])).
fof(f1824, plain, (~ spl144_134 | spl144_20), inference(avatar_split_clause, [], [f553, f1251, f1819])).
fof(f553, plain, ((e13 = op1(e12, e13)) | ~ sP60), inference(cnf_transformation, [], [f173])).
fof(f173, plain, (((e12 = op1(e12, e11)) & (e12 = op1(e11, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP60), inference(nnf_transformation, [], [f81])).
fof(f1815, plain, (~ spl144_133 | spl144_23), inference(avatar_split_clause, [], [f550, f1264, f1811])).
fof(f550, plain, ((e12 = op1(e12, e12)) | ~ sP61), inference(cnf_transformation, [], [f172])).
fof(f172, plain, (((e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP61), inference(nnf_transformation, [], [f82])).
fof(f1806, plain, (~ spl144_132 | spl144_19), inference(avatar_split_clause, [], [f547, f1247, f1803])).
fof(f547, plain, ((e12 = op1(e12, e13)) | ~ sP62), inference(cnf_transformation, [], [f171])).
fof(f171, plain, (((e12 = op1(e12, e13)) & (e12 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP62), inference(nnf_transformation, [], [f83])).
fof(f1798, plain, (~ spl144_131 | spl144_16), inference(avatar_split_clause, [], [f543, f1234, f1795])).
fof(f543, plain, ((e13 = op1(e13, e10)) | ~ sP63), inference(cnf_transformation, [], [f170])).
fof(f170, plain, (((e13 = op1(e13, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP63), inference(nnf_transformation, [], [f84])).
fof(f1790, plain, (~ spl144_130 | spl144_12), inference(avatar_split_clause, [], [f539, f1217, f1787])).
fof(f539, plain, ((e13 = op1(e13, e11)) | ~ sP64), inference(cnf_transformation, [], [f169])).
fof(f169, plain, (((e13 = op1(e13, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP64), inference(nnf_transformation, [], [f85])).
fof(f1783, plain, (~ spl144_129 | spl144_20), inference(avatar_split_clause, [], [f534, f1251, f1779])).
fof(f534, plain, ((e13 = op1(e12, e13)) | ~ sP65), inference(cnf_transformation, [], [f168])).
fof(f168, plain, (((e13 = op1(e13, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP65), inference(nnf_transformation, [], [f86])).
fof(f1773, plain, (spl144_127 | spl144_123 | spl144_119 | spl144_115), inference(avatar_split_clause, [], [f380, f1687, f1704, f1721, f1738])).
fof(f380, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax4)).
fof(f1766, plain, (spl144_122 | spl144_106 | spl144_90 | spl144_74), inference(avatar_split_clause, [], [f387, f1513, f1581, f1649, f1717])).
fof(f387, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1756, plain, (spl144_119 | spl144_103 | spl144_87 | spl144_71), inference(avatar_split_clause, [], [f397, f1500, f1568, f1636, f1704])).
fof(f397, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1755, plain, (spl144_96 | spl144_92 | spl144_88 | spl144_84), inference(avatar_split_clause, [], [f398, f1555, f1572, f1589, f1606])).
fof(f398, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1753, plain, (spl144_77 | spl144_73 | spl144_69 | spl144_65), inference(avatar_split_clause, [], [f400, f1475, f1492, f1509, f1526])).
fof(f400, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1752, plain, (spl144_113 | spl144_97 | spl144_81 | spl144_65), inference(avatar_split_clause, [], [f401, f1475, f1543, f1611, f1679])).
fof(f401, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1750, plain, (spl144_114 | spl144_98 | spl144_82 | spl144_66), inference(avatar_split_clause, [], [f403, f1479, f1547, f1615, f1683])).
fof(f403, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1745, plain, (spl144_125 | spl144_126 | spl144_127 | spl144_128), inference(avatar_split_clause, [], [f360, f1742, f1738, f1734, f1730])).
fof(f360, plain, ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax3)).
fof(f1677, plain, (spl144_109 | spl144_110 | spl144_111 | spl144_112), inference(avatar_split_clause, [], [f364, f1674, f1670, f1666, f1662])).
fof(f364, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1643, plain, (spl144_101 | spl144_102 | spl144_103 | spl144_104), inference(avatar_split_clause, [], [f366, f1640, f1636, f1632, f1628])).
fof(f366, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f1626, plain, (spl144_97 | spl144_98 | spl144_99 | spl144_100), inference(avatar_split_clause, [], [f367, f1623, f1619, f1615, f1611])).
fof(f367, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1609, plain, (spl144_93 | spl144_94 | spl144_95 | spl144_96), inference(avatar_split_clause, [], [f368, f1606, f1602, f1598, f1594])).
fof(f368, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f1575, plain, (spl144_85 | spl144_86 | spl144_87 | spl144_88), inference(avatar_split_clause, [], [f370, f1572, f1568, f1564, f1560])).
fof(f370, plain, ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f3])).
fof(f1507, plain, (spl144_69 | spl144_70 | spl144_71 | spl144_72), inference(avatar_split_clause, [], [f374, f1504, f1500, f1496, f1492])).
fof(f374, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f1490, plain, (spl144_65 | spl144_66 | spl144_67 | spl144_68), inference(avatar_split_clause, [], [f375, f1487, f1483, f1479, f1475])).
fof(f375, plain, ((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))), inference(cnf_transformation, [], [f3])).
fof(f1469, plain, (spl144_63 | spl144_59 | spl144_55 | spl144_51), inference(avatar_split_clause, [], [f332, f1383, f1400, f1417, f1434])).
fof(f332, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax2)).
fof(f1466, plain, (spl144_64 | spl144_48 | spl144_32 | spl144_16), inference(avatar_split_clause, [], [f335, f1234, f1302, f1370, f1438])).
fof(f335, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f1451, plain, (spl144_32 | spl144_28 | spl144_24 | spl144_20), inference(avatar_split_clause, [], [f350, f1251, f1268, f1285, f1302])).
fof(f350, plain, ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1450, plain, (spl144_56 | spl144_40 | spl144_24 | spl144_8), inference(avatar_split_clause, [], [f351, f1200, f1268, f1336, f1404])).
fof(f351, plain, ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1449, plain, (spl144_13 | spl144_9 | spl144_5 | spl144_1), inference(avatar_split_clause, [], [f352, f1171, f1188, f1205, f1222])).
fof(f352, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1446, plain, (spl144_50 | spl144_34 | spl144_18 | spl144_2), inference(avatar_split_clause, [], [f355, f1175, f1243, f1311, f1379])).
fof(f355, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1443, plain, (spl144_16 | spl144_12 | spl144_8 | spl144_4), inference(avatar_split_clause, [], [f358, f1183, f1200, f1217, f1234])).
fof(f358, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1373, plain, (spl144_45 | spl144_46 | spl144_47 | spl144_48), inference(avatar_split_clause, [], [f316, f1370, f1366, f1362, f1358])).
fof(f316, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG118+1.p', ax1)).
fof(f1339, plain, (spl144_37 | spl144_38 | spl144_39 | spl144_40), inference(avatar_split_clause, [], [f318, f1336, f1332, f1328, f1324])).
fof(f318, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f1322, plain, (spl144_33 | spl144_34 | spl144_35 | spl144_36), inference(avatar_split_clause, [], [f319, f1319, f1315, f1311, f1307])).
fof(f319, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f1305, plain, (spl144_29 | spl144_30 | spl144_31 | spl144_32), inference(avatar_split_clause, [], [f320, f1302, f1298, f1294, f1290])).
fof(f320, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f1254, plain, (spl144_17 | spl144_18 | spl144_19 | spl144_20), inference(avatar_split_clause, [], [f323, f1251, f1247, f1243, f1239])).
fof(f323, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f1220, plain, (spl144_9 | spl144_10 | spl144_11 | spl144_12), inference(avatar_split_clause, [], [f325, f1217, f1213, f1209, f1205])).
fof(f325, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).
fof(f1203, plain, (spl144_5 | spl144_6 | spl144_7 | spl144_8), inference(avatar_split_clause, [], [f326, f1200, f1196, f1192, f1188])).
fof(f326, plain, ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))), inference(cnf_transformation, [], [f1])).