fof(f8486, plain, $false, inference(avatar_sat_refutation, [], [f777, f945, f966, f987, f1092, f1134, f1155, f1218, f1239, f1261, f1273, f1274, f1277, f1279, f1282, f1287, f1288, f1292, f1293, f1352, f1373, f1499, f1520, f1541, f1562, f1625, f1836, f1837, f1838, f1841, f1846, f1848, f1849, f1852, f1866, f1867, f1870, f1873, f1878, f1881, f1884, f1885, f1894, f1899, f1911, f1919, f1923, f1935, f1940, f1952, f1960, f1964, f1968, f1970, f1972, f1973, f1975, f1977, f2085, f2094, f2132, f2153, f2672, f3521, f3575, f3635, f3640, f3694, f3743, f3774, f3782, f3793, f3802, f3817, f3833, f3851, f3883, f3885, f3986, f3996, f4001, f4132, f4142, f4209, f4211, f4240, f4245, f4247, f4255, f4267, f4269, f4276, f4291, f4327, f4353, f4372, f4377, f4399, f4403, f4416, f4439, f4448, f4482, f4498, f4644, f4646, f4647, f4672, f4721, f4738, f4748, f4749, f4799, f4807, f4815, f4824, f4856, f4893, f4903, f4952, f4980, f4986, f5007, f5045, f5052, f5055, f5149, f5158, f5195, f5200, f5213, f5220, f5232, f5253, f5270, f5276, f5280, f5312, f5316, f5331, f5338, f5339, f5340, f5368, f5388, f5406, f5418, f5438, f5473, f5476, f5480, f5538, f5547, f5560, f5575, f5600, f5615, f5620, f5623, f5639, f5645, f5688, f5689, f5706, f5722, f5724, f5761, f5809, f5822, f5861, f5886, f5934, f5946, f5955, f5966, f5970, f6046, f6063, f6091, f6092, f6095, f6131, f6161, f6200, f6212, f6263, f6273, f6306, f6378, f6386, f6407, f6420, f6471, f6488, f6503, f6601, f6617, f6647, f6654, f6675, f6695, f6734, f6740, f6741, f6756, f6788, f6793, f6804, f6809, f6818, f6835, f6838, f6839, f6840, f6856, f6865, f6921, f6923, f6930, f6982, f6999, f7011, f7029, f7062, f7067, f7101, f7104, f7146, f7186, f7252, f7262, f7306, f7338, f7383, f7422, f7472, f7512, f7556, f7596, f7632, f7676, f7725, f7766, f7796, f7830, f7867, f7906, f7986, f8026, f8062, f8095, f8190, f8247, f8262, f8294, f8344, f8346, f8356, f8423, f8440, f8447, f8461])).
fof(f8461, plain, (~ spl28_178 | ~ spl28_193), inference(avatar_split_clause, [], [f8456, f1593, f1530])).
fof(f1530, plain, (spl28_178 <=> (e22 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_178])])).
fof(f1593, plain, (spl28_193 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_193])])).
fof(f8456, plain, (~ (e22 = op2(e22, e24)) | ~ spl28_193), inference(backward_demodulation, [], [f459, f1595])).
fof(f1595, plain, ((e22 = op2(e22, e21)) | ~ spl28_193), inference(avatar_component_clause, [], [f1593])).
fof(f459, plain, ~ (op2(e22, e21) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (op2(e24, e23) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e24)) & ~ (op2(e24, e21) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e24)) & ~ (op2(e24, e20) = op2(e24, e23)) & ~ (op2(e24, e20) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e21)) & ~ (op2(e23, e23) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e24)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e24)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e23) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e24)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e24)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e23) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e24)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e24)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e23) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e24)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e24)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e23, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e24, e24)) & ~ (op2(e21, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e24, e24)) & ~ (op2(e20, e24) = op2(e23, e24)) & ~ (op2(e20, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e21, e24)) & ~ (op2(e23, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e24, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e24, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e23, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e24, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e24, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e23, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e24, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e24, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e23, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e24, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e24, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax8)).
fof(f8447, plain, (~ spl28_205 | ~ spl28_210), inference(avatar_split_clause, [], [f8446, f1664, f1643])).
fof(f1643, plain, (spl28_205 <=> (e24 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_205])])).
fof(f1664, plain, (spl28_210 <=> (e24 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_210])])).
fof(f8446, plain, (~ (e24 = op2(e21, e24)) | ~ spl28_210), inference(backward_demodulation, [], [f452, f1666])).
fof(f1666, plain, ((e24 = op2(e21, e23)) | ~ spl28_210), inference(avatar_component_clause, [], [f1664])).
fof(f452, plain, ~ (op2(e21, e23) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f8440, plain, (~ spl28_181 | ~ spl28_224 | ~ spl28_243), inference(avatar_contradiction_clause, [], [f8439])).
fof(f8439, plain, ($false | (~ spl28_181 | ~ spl28_224 | ~ spl28_243)), inference(subsumption_resolution, [], [f8438, f493])).
fof(f493, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (~ (e23 = e24) & ~ (e22 = e24) & ~ (e22 = e23) & ~ (e21 = e24) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e24) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax10)).
fof(f8438, plain, ((e20 = e21) | (~ spl28_181 | ~ spl28_224 | ~ spl28_243)), inference(forward_demodulation, [], [f8433, f1545])).
fof(f1545, plain, ((e20 = op2(e22, e23)) | ~ spl28_181), inference(avatar_component_clause, [], [f1543])).
fof(f1543, plain, (spl28_181 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_181])])).
fof(f8433, plain, ((e21 = op2(e22, e23)) | (~ spl28_224 | ~ spl28_243)), inference(backward_demodulation, [], [f8400, f1725])).
fof(f1725, plain, ((e23 = op2(e21, e20)) | ~ spl28_224), inference(avatar_component_clause, [], [f1723])).
fof(f1723, plain, (spl28_224 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_224])])).
fof(f8400, plain, ((e21 = op2(e22, op2(e21, e20))) | ~ spl28_243), inference(forward_demodulation, [], [f263, f1805])).
fof(f1805, plain, ((e22 = op2(e20, e21)) | ~ spl28_243), inference(avatar_component_clause, [], [f1803])).
fof(f1803, plain, (spl28_243 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_243])])).
fof(f263, plain, (e21 = op2(op2(e20, e21), op2(e21, e20))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e24 = op2(op2(e24, e24), op2(e24, e24))) & (e24 = op2(op2(e23, e24), op2(e24, e23))) & (e24 = op2(op2(e22, e24), op2(e24, e22))) & (e24 = op2(op2(e21, e24), op2(e24, e21))) & (e24 = op2(op2(e20, e24), op2(e24, e20))) & (e23 = op2(op2(e24, e23), op2(e23, e24))) & (e23 = op2(op2(e23, e23), op2(e23, e23))) & (e23 = op2(op2(e22, e23), op2(e23, e22))) & (e23 = op2(op2(e21, e23), op2(e23, e21))) & (e23 = op2(op2(e20, e23), op2(e23, e20))) & (e22 = op2(op2(e24, e22), op2(e22, e24))) & (e22 = op2(op2(e23, e22), op2(e22, e23))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e22 = op2(op2(e21, e22), op2(e22, e21))) & (e22 = op2(op2(e20, e22), op2(e22, e20))) & (e21 = op2(op2(e24, e21), op2(e21, e24))) & (e21 = op2(op2(e23, e21), op2(e21, e23))) & (e21 = op2(op2(e22, e21), op2(e21, e22))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e21 = op2(op2(e20, e21), op2(e21, e20))) & (e20 = op2(op2(e24, e20), op2(e20, e24))) & (e20 = op2(op2(e23, e20), op2(e20, e23))) & (e20 = op2(op2(e22, e20), op2(e20, e22))) & (e20 = op2(op2(e21, e20), op2(e20, e21))) & (e20 = op2(op2(e20, e20), op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax6)).
fof(f8423, plain, (~ spl28_174 | ~ spl28_111 | ~ spl28_263 | ~ spl28_276 | spl28_373), inference(avatar_split_clause, [], [f8422, f2644, f2150, f2082, f1199, f1513])).
fof(f1513, plain, (spl28_174 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_174])])).
fof(f1199, plain, (spl28_111 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_111])])).
fof(f2082, plain, (spl28_263 <=> (e23 = h5(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_263])])).
fof(f2150, plain, (spl28_276 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_276])])).
fof(f2644, plain, (spl28_373 <=> (h5(op1(e10, e12)) = op2(h5(e10), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_373])])).
fof(f8422, plain, (~ (e23 = op2(e23, e20)) | (~ spl28_111 | ~ spl28_263 | ~ spl28_276 | spl28_373)), inference(forward_demodulation, [], [f8421, f2083])).
fof(f2083, plain, ((e23 = h5(e10)) | ~ spl28_263), inference(avatar_component_clause, [], [f2082])).
fof(f8421, plain, (~ (op2(e23, e20) = h5(e10)) | (~ spl28_111 | ~ spl28_263 | ~ spl28_276 | spl28_373)), inference(forward_demodulation, [], [f8420, f1201])).
fof(f1201, plain, ((e10 = op1(e10, e12)) | ~ spl28_111), inference(avatar_component_clause, [], [f1199])).
fof(f8420, plain, (~ (op2(e23, e20) = h5(op1(e10, e12))) | (~ spl28_263 | ~ spl28_276 | spl28_373)), inference(forward_demodulation, [], [f8419, f2083])).
fof(f8419, plain, (~ (h5(op1(e10, e12)) = op2(h5(e10), e20)) | (~ spl28_276 | spl28_373)), inference(forward_demodulation, [], [f2646, f2151])).
fof(f2151, plain, ((e20 = h4(e10)) | ~ spl28_276), inference(avatar_component_clause, [], [f2150])).
fof(f2646, plain, (~ (h5(op1(e10, e12)) = op2(h5(e10), h4(e10))) | spl28_373), inference(avatar_component_clause, [], [f2644])).
fof(f8356, plain, (~ spl28_170 | ~ spl28_165), inference(avatar_split_clause, [], [f7050, f1475, f1496])).
fof(f1496, plain, (spl28_170 <=> (e24 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_170])])).
fof(f1475, plain, (spl28_165 <=> (e24 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_165])])).
fof(f7050, plain, (~ (e24 = op2(e23, e21)) | ~ spl28_165), inference(forward_demodulation, [], [f467, f1477])).
fof(f1477, plain, ((e24 = op2(e23, e22)) | ~ spl28_165), inference(avatar_component_clause, [], [f1475])).
fof(f467, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f8346, plain, (spl28_210 | ~ spl28_134 | ~ spl28_284), inference(avatar_split_clause, [], [f8345, f2195, f1345, f1664])).
fof(f1345, plain, (spl28_134 <=> (e23 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_134])])).
fof(f2195, plain, (spl28_284 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_284])])).
fof(f8345, plain, ((e24 = op2(e21, e23)) | (~ spl28_134 | ~ spl28_284)), inference(forward_demodulation, [], [f5278, f1347])).
fof(f1347, plain, ((e23 = op2(e24, e23)) | ~ spl28_134), inference(avatar_component_clause, [], [f1345])).
fof(f5278, plain, ((e24 = op2(e21, op2(e24, e23))) | ~ spl28_284), inference(forward_demodulation, [], [f2034, f2196])).
fof(f2196, plain, ((e21 = h4(e11)) | ~ spl28_284), inference(avatar_component_clause, [], [f2195])).
fof(f2034, plain, (e24 = op2(h4(e11), op2(e24, e23))), inference(backward_demodulation, [], [f281, f2032])).
fof(f2032, plain, (op2(e23, e24) = h4(e11)), inference(forward_demodulation, [], [f2031, f2030])).
fof(f2030, plain, (e24 = op2(e23, h4(e12))), inference(backward_demodulation, [], [f585, f604])).
fof(f604, plain, (op2(e23, e23) = h4(e12)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((op2(e23, op2(e23, e23)) = h4(e14)) & (op2(e23, e23) = h4(e12)) & (op2(e23, op2(e23, op2(e23, e23))) = h4(e11)) & (op2(op2(e23, op2(e23, e23)), op2(e23, op2(e23, e23))) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax19)).
fof(f585, plain, (e24 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((e24 = op2(e23, op2(e23, e23))) & (e22 = op2(e23, e23)) & (e21 = op2(e23, op2(e23, op2(e23, e23)))) & (e20 = op2(op2(e23, op2(e23, e23)), op2(e23, op2(e23, e23))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax15)).
fof(f2031, plain, (h4(e11) = op2(e23, op2(e23, h4(e12)))), inference(forward_demodulation, [], [f603, f604])).
fof(f603, plain, (op2(e23, op2(e23, op2(e23, e23))) = h4(e11)), inference(cnf_transformation, [], [f19])).
fof(f281, plain, (e24 = op2(op2(e23, e24), op2(e24, e23))), inference(cnf_transformation, [], [f6])).
fof(f8344, plain, (spl28_169 | ~ spl28_134 | ~ spl28_284), inference(avatar_split_clause, [], [f8343, f2195, f1345, f1492])).
fof(f1492, plain, (spl28_169 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_169])])).
fof(f8343, plain, ((e23 = op2(e23, e21)) | (~ spl28_134 | ~ spl28_284)), inference(forward_demodulation, [], [f5277, f1347])).
fof(f5277, plain, ((e23 = op2(op2(e24, e23), e21)) | ~ spl28_284), inference(forward_demodulation, [], [f2033, f2196])).
fof(f2033, plain, (e23 = op2(op2(e24, e23), h4(e11))), inference(backward_demodulation, [], [f277, f2032])).
fof(f277, plain, (e23 = op2(op2(e24, e23), op2(e23, e24))), inference(cnf_transformation, [], [f6])).
fof(f8294, plain, (~ spl28_144 | ~ spl28_41 | ~ spl28_263 | ~ spl28_272 | spl28_363), inference(avatar_split_clause, [], [f8293, f2604, f2129, f2082, f905, f1387])).
fof(f1387, plain, (spl28_144 <=> (e23 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_144])])).
fof(f905, plain, (spl28_41 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_41])])).
fof(f2129, plain, (spl28_272 <=> (e21 = h5(e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_272])])).
fof(f2604, plain, (spl28_363 <=> (h5(op1(e13, e11)) = op2(e24, h5(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_363])])).
fof(f8293, plain, (~ (e23 = op2(e24, e21)) | (~ spl28_41 | ~ spl28_263 | ~ spl28_272 | spl28_363)), inference(forward_demodulation, [], [f8292, f2083])).
fof(f8292, plain, (~ (op2(e24, e21) = h5(e10)) | (~ spl28_41 | ~ spl28_272 | spl28_363)), inference(forward_demodulation, [], [f8291, f907])).
fof(f907, plain, ((e10 = op1(e13, e11)) | ~ spl28_41), inference(avatar_component_clause, [], [f905])).
fof(f8291, plain, (~ (op2(e24, e21) = h5(op1(e13, e11))) | (~ spl28_272 | spl28_363)), inference(forward_demodulation, [], [f2606, f2130])).
fof(f2130, plain, ((e21 = h5(e11)) | ~ spl28_272), inference(avatar_component_clause, [], [f2129])).
fof(f2606, plain, (~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | spl28_363), inference(avatar_component_clause, [], [f2604])).
fof(f8262, plain, (spl28_144 | ~ spl28_135 | ~ spl28_284), inference(avatar_split_clause, [], [f6489, f2195, f1349, f1387])).
fof(f1349, plain, (spl28_135 <=> (e24 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_135])])).
fof(f6489, plain, ((e23 = op2(e24, e21)) | (~ spl28_135 | ~ spl28_284)), inference(forward_demodulation, [], [f5277, f1351])).
fof(f1351, plain, ((e24 = op2(e24, e23)) | ~ spl28_135), inference(avatar_component_clause, [], [f1349])).
fof(f8247, plain, (spl28_62 | ~ spl28_93), inference(avatar_contradiction_clause, [], [f8246])).
fof(f8246, plain, ($false | (spl28_62 | ~ spl28_93)), inference(subsumption_resolution, [], [f8238, f994])).
fof(f994, plain, (~ (e11 = op1(e12, e12)) | spl28_62), inference(avatar_component_clause, [], [f993])).
fof(f993, plain, (spl28_62 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_62])])).
fof(f8238, plain, ((e11 = op1(e12, e12)) | ~ spl28_93), inference(backward_demodulation, [], [f164, f1125])).
fof(f1125, plain, ((e12 = op1(e11, e11)) | ~ spl28_93), inference(avatar_component_clause, [], [f1123])).
fof(f1123, plain, (spl28_93 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_93])])).
fof(f164, plain, (e11 = op1(op1(e11, e11), op1(e11, e11))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ((e14 = op1(op1(e14, e14), op1(e14, e14))) & (e14 = op1(op1(e13, e14), op1(e14, e13))) & (e14 = op1(op1(e12, e14), op1(e14, e12))) & (e14 = op1(op1(e11, e14), op1(e14, e11))) & (e14 = op1(op1(e10, e14), op1(e14, e10))) & (e13 = op1(op1(e14, e13), op1(e13, e14))) & (e13 = op1(op1(e13, e13), op1(e13, e13))) & (e13 = op1(op1(e12, e13), op1(e13, e12))) & (e13 = op1(op1(e11, e13), op1(e13, e11))) & (e13 = op1(op1(e10, e13), op1(e13, e10))) & (e12 = op1(op1(e14, e12), op1(e12, e14))) & (e12 = op1(op1(e13, e12), op1(e12, e13))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e12 = op1(op1(e11, e12), op1(e12, e11))) & (e12 = op1(op1(e10, e12), op1(e12, e10))) & (e11 = op1(op1(e14, e11), op1(e11, e14))) & (e11 = op1(op1(e13, e11), op1(e11, e13))) & (e11 = op1(op1(e12, e11), op1(e11, e12))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e11 = op1(op1(e10, e11), op1(e11, e10))) & (e10 = op1(op1(e14, e10), op1(e10, e14))) & (e10 = op1(op1(e13, e10), op1(e10, e13))) & (e10 = op1(op1(e12, e10), op1(e10, e12))) & (e10 = op1(op1(e11, e10), op1(e10, e11))) & (e10 = op1(op1(e10, e10), op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax3)).
fof(f8190, plain, (spl28_32 | ~ spl28_94), inference(avatar_contradiction_clause, [], [f8189])).
fof(f8189, plain, ($false | (spl28_32 | ~ spl28_94)), inference(subsumption_resolution, [], [f8182, f868])).
fof(f868, plain, (~ (e11 = op1(e13, e13)) | spl28_32), inference(avatar_component_clause, [], [f867])).
fof(f867, plain, (spl28_32 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_32])])).
fof(f8182, plain, ((e11 = op1(e13, e13)) | ~ spl28_94), inference(backward_demodulation, [], [f164, f1129])).
fof(f1129, plain, ((e13 = op1(e11, e11)) | ~ spl28_94), inference(avatar_component_clause, [], [f1127])).
fof(f1127, plain, (spl28_94 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_94])])).
fof(f8095, plain, (~ spl28_70 | ~ spl28_243 | ~ spl28_265 | ~ spl28_272 | ~ spl28_276 | spl28_368), inference(avatar_contradiction_clause, [], [f8094])).
fof(f8094, plain, ($false | (~ spl28_70 | ~ spl28_243 | ~ spl28_265 | ~ spl28_272 | ~ spl28_276 | spl28_368)), inference(subsumption_resolution, [], [f8093, f1805])).
fof(f8093, plain, (~ (e22 = op2(e20, e21)) | (~ spl28_70 | ~ spl28_265 | ~ spl28_272 | ~ spl28_276 | spl28_368)), inference(forward_demodulation, [], [f8092, f2092])).
fof(f2092, plain, ((e22 = h5(e14)) | ~ spl28_265), inference(avatar_component_clause, [], [f2091])).
fof(f2091, plain, (spl28_265 <=> (e22 = h5(e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_265])])).
fof(f8092, plain, (~ (op2(e20, e21) = h5(e14)) | (~ spl28_70 | ~ spl28_272 | ~ spl28_276 | spl28_368)), inference(forward_demodulation, [], [f8091, f1028])).
fof(f1028, plain, ((e14 = op1(e12, e11)) | ~ spl28_70), inference(avatar_component_clause, [], [f1026])).
fof(f1026, plain, (spl28_70 <=> (e14 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_70])])).
fof(f8091, plain, (~ (op2(e20, e21) = h5(op1(e12, e11))) | (~ spl28_272 | ~ spl28_276 | spl28_368)), inference(forward_demodulation, [], [f8090, f2151])).
fof(f8090, plain, (~ (h5(op1(e12, e11)) = op2(h4(e10), e21)) | (~ spl28_272 | spl28_368)), inference(forward_demodulation, [], [f2626, f2130])).
fof(f2626, plain, (~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | spl28_368), inference(avatar_component_clause, [], [f2624])).
fof(f2624, plain, (spl28_368 <=> (h5(op1(e12, e11)) = op2(h4(e10), h5(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_368])])).
fof(f8062, plain, (~ spl28_84 | ~ spl28_205 | ~ spl28_272 | spl28_370), inference(avatar_contradiction_clause, [], [f8061])).
fof(f8061, plain, ($false | (~ spl28_84 | ~ spl28_205 | ~ spl28_272 | spl28_370)), inference(subsumption_resolution, [], [f8060, f1645])).
fof(f1645, plain, ((e24 = op2(e21, e24)) | ~ spl28_205), inference(avatar_component_clause, [], [f1643])).
fof(f8060, plain, (~ (e24 = op2(e21, e24)) | (~ spl28_84 | ~ spl28_272 | spl28_370)), inference(forward_demodulation, [], [f8059, f606])).
fof(f606, plain, (e24 = h5(e13)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((op2(e24, op2(e24, e24)) = h5(e14)) & (op2(e24, e24) = h5(e12)) & (h5(e11) = op2(e24, op2(e24, op2(e24, e24)))) & (h5(e10) = op2(op2(e24, op2(e24, e24)), op2(e24, op2(e24, e24)))) & (e24 = h5(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax20)).
fof(f8059, plain, (~ (op2(e21, e24) = h5(e13)) | (~ spl28_84 | ~ spl28_272 | spl28_370)), inference(forward_demodulation, [], [f8058, f1087])).
fof(f1087, plain, ((e13 = op1(e11, e13)) | ~ spl28_84), inference(avatar_component_clause, [], [f1085])).
fof(f1085, plain, (spl28_84 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_84])])).
fof(f8058, plain, (~ (op2(e21, e24) = h5(op1(e11, e13))) | (~ spl28_272 | spl28_370)), inference(forward_demodulation, [], [f2634, f2130])).
fof(f2634, plain, (~ (h5(op1(e11, e13)) = op2(h5(e11), e24)) | spl28_370), inference(avatar_component_clause, [], [f2632])).
fof(f2632, plain, (spl28_370 <=> (h5(op1(e11, e13)) = op2(h5(e11), e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_370])])).
fof(f8026, plain, (~ spl28_88 | ~ spl28_221 | ~ spl28_272 | ~ spl28_276 | spl28_371), inference(avatar_contradiction_clause, [], [f8025])).
fof(f8025, plain, ($false | (~ spl28_88 | ~ spl28_221 | ~ spl28_272 | ~ spl28_276 | spl28_371)), inference(subsumption_resolution, [], [f8024, f1713])).
fof(f1713, plain, ((e20 = op2(e21, e20)) | ~ spl28_221), inference(avatar_component_clause, [], [f1711])).
fof(f1711, plain, (spl28_221 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_221])])).
fof(f8024, plain, (~ (e20 = op2(e21, e20)) | (~ spl28_88 | ~ spl28_272 | ~ spl28_276 | spl28_371)), inference(forward_demodulation, [], [f8023, f4495])).
fof(f4495, plain, ((e20 = h5(e12)) | ~ spl28_276), inference(backward_demodulation, [], [f2055, f2151])).
fof(f2055, plain, (h4(e10) = h5(e12)), inference(forward_demodulation, [], [f609, f2044])).
fof(f2044, plain, (op2(e24, e24) = h4(e10)), inference(forward_demodulation, [], [f2043, f2030])).
fof(f2043, plain, (h4(e10) = op2(op2(e23, h4(e12)), op2(e23, h4(e12)))), inference(forward_demodulation, [], [f602, f604])).
fof(f602, plain, (op2(op2(e23, op2(e23, e23)), op2(e23, op2(e23, e23))) = h4(e10)), inference(cnf_transformation, [], [f19])).
fof(f609, plain, (op2(e24, e24) = h5(e12)), inference(cnf_transformation, [], [f20])).
fof(f8023, plain, (~ (op2(e21, e20) = h5(e12)) | (~ spl28_88 | ~ spl28_272 | ~ spl28_276 | spl28_371)), inference(forward_demodulation, [], [f8022, f1104])).
fof(f1104, plain, ((e12 = op1(e11, e12)) | ~ spl28_88), inference(avatar_component_clause, [], [f1102])).
fof(f1102, plain, (spl28_88 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_88])])).
fof(f8022, plain, (~ (op2(e21, e20) = h5(op1(e11, e12))) | (~ spl28_272 | ~ spl28_276 | spl28_371)), inference(forward_demodulation, [], [f8021, f2130])).
fof(f8021, plain, (~ (h5(op1(e11, e12)) = op2(h5(e11), e20)) | (~ spl28_276 | spl28_371)), inference(forward_demodulation, [], [f2638, f2151])).
fof(f2638, plain, (~ (h5(op1(e11, e12)) = op2(h5(e11), h4(e10))) | spl28_371), inference(avatar_component_clause, [], [f2636])).
fof(f2636, plain, (spl28_371 <=> (h5(op1(e11, e12)) = op2(h5(e11), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_371])])).
fof(f7986, plain, (~ spl28_49 | ~ spl28_135 | ~ spl28_263 | spl28_364), inference(avatar_contradiction_clause, [], [f7985])).
fof(f7985, plain, ($false | (~ spl28_49 | ~ spl28_135 | ~ spl28_263 | spl28_364)), inference(subsumption_resolution, [], [f7984, f1351])).
fof(f7984, plain, (~ (e24 = op2(e24, e23)) | (~ spl28_49 | ~ spl28_263 | spl28_364)), inference(forward_demodulation, [], [f7983, f606])).
fof(f7983, plain, (~ (op2(e24, e23) = h5(e13)) | (~ spl28_49 | ~ spl28_263 | spl28_364)), inference(forward_demodulation, [], [f7982, f940])).
fof(f940, plain, ((e13 = op1(e13, e10)) | ~ spl28_49), inference(avatar_component_clause, [], [f938])).
fof(f938, plain, (spl28_49 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_49])])).
fof(f7982, plain, (~ (op2(e24, e23) = h5(op1(e13, e10))) | (~ spl28_263 | spl28_364)), inference(forward_demodulation, [], [f2610, f2083])).
fof(f2610, plain, (~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | spl28_364), inference(avatar_component_clause, [], [f2608])).
fof(f2608, plain, (spl28_364 <=> (h5(op1(e13, e10)) = op2(e24, h5(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_364])])).
fof(f7906, plain, (~ spl28_72 | ~ spl28_232 | ~ spl28_263 | ~ spl28_272 | ~ spl28_276 | spl28_369), inference(avatar_contradiction_clause, [], [f7905])).
fof(f7905, plain, ($false | (~ spl28_72 | ~ spl28_232 | ~ spl28_263 | ~ spl28_272 | ~ spl28_276 | spl28_369)), inference(subsumption_resolution, [], [f7904, f1759])).
fof(f1759, plain, ((e21 = op2(e20, e23)) | ~ spl28_232), inference(avatar_component_clause, [], [f1757])).
fof(f1757, plain, (spl28_232 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_232])])).
fof(f7904, plain, (~ (e21 = op2(e20, e23)) | (~ spl28_72 | ~ spl28_263 | ~ spl28_272 | ~ spl28_276 | spl28_369)), inference(forward_demodulation, [], [f7903, f2130])).
fof(f7903, plain, (~ (op2(e20, e23) = h5(e11)) | (~ spl28_72 | ~ spl28_263 | ~ spl28_276 | spl28_369)), inference(forward_demodulation, [], [f7902, f1037])).
fof(f1037, plain, ((e11 = op1(e12, e10)) | ~ spl28_72), inference(avatar_component_clause, [], [f1035])).
fof(f1035, plain, (spl28_72 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_72])])).
fof(f7902, plain, (~ (op2(e20, e23) = h5(op1(e12, e10))) | (~ spl28_263 | ~ spl28_276 | spl28_369)), inference(forward_demodulation, [], [f7901, f2151])).
fof(f7901, plain, (~ (h5(op1(e12, e10)) = op2(h4(e10), e23)) | (~ spl28_263 | spl28_369)), inference(forward_demodulation, [], [f2630, f2083])).
fof(f2630, plain, (~ (h5(op1(e12, e10)) = op2(h4(e10), h5(e10))) | spl28_369), inference(avatar_component_clause, [], [f2628])).
fof(f2628, plain, (spl28_369 <=> (h5(op1(e12, e10)) = op2(h4(e10), h5(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_369])])).
fof(f7867, plain, (~ spl28_33 | ~ spl28_276 | spl28_361), inference(avatar_contradiction_clause, [], [f7866])).
fof(f7866, plain, ($false | (~ spl28_33 | ~ spl28_276 | spl28_361)), inference(subsumption_resolution, [], [f7865, f2151])).
fof(f7865, plain, (~ (e20 = h4(e10)) | (~ spl28_33 | ~ spl28_276 | spl28_361)), inference(forward_demodulation, [], [f7864, f4495])).
fof(f7864, plain, (~ (h4(e10) = h5(e12)) | (~ spl28_33 | spl28_361)), inference(forward_demodulation, [], [f2598, f873])).
fof(f873, plain, ((e12 = op1(e13, e13)) | ~ spl28_33), inference(avatar_component_clause, [], [f871])).
fof(f871, plain, (spl28_33 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_33])])).
fof(f2598, plain, (~ (h4(e10) = h5(op1(e13, e13))) | spl28_361), inference(avatar_component_clause, [], [f2596])).
fof(f2596, plain, (spl28_361 <=> (h4(e10) = h5(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl28_361])])).
fof(f7830, plain, (~ spl28_64 | spl28_367), inference(avatar_contradiction_clause, [], [f7829])).
fof(f7829, plain, ($false | (~ spl28_64 | spl28_367)), inference(subsumption_resolution, [], [f7828, f606])).
fof(f7828, plain, (~ (e24 = h5(e13)) | (~ spl28_64 | spl28_367)), inference(forward_demodulation, [], [f2622, f1003])).
fof(f1003, plain, ((e13 = op1(e12, e12)) | ~ spl28_64), inference(avatar_component_clause, [], [f1001])).
fof(f1001, plain, (spl28_64 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_64])])).
fof(f2622, plain, (~ (e24 = h5(op1(e12, e12))) | spl28_367), inference(avatar_component_clause, [], [f2620])).
fof(f2620, plain, (spl28_367 <=> (e24 = h5(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_367])])).
fof(f7796, plain, (~ spl28_27 | spl28_360), inference(avatar_contradiction_clause, [], [f7795])).
fof(f7795, plain, ($false | (~ spl28_27 | spl28_360)), inference(trivial_inequality_removal, [], [f7794])).
fof(f7794, plain, (~ (h5(e11) = h5(e11)) | (~ spl28_27 | spl28_360)), inference(forward_demodulation, [], [f2594, f848])).
fof(f848, plain, ((e11 = op1(e13, e14)) | ~ spl28_27), inference(avatar_component_clause, [], [f846])).
fof(f846, plain, (spl28_27 <=> (e11 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_27])])).
fof(f2594, plain, (~ (h5(e11) = h5(op1(e13, e14))) | spl28_360), inference(avatar_component_clause, [], [f2592])).
fof(f2592, plain, (spl28_360 <=> (h5(e11) = h5(op1(e13, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl28_360])])).
fof(f7766, plain, (~ spl28_12 | ~ spl28_197 | ~ spl28_265 | ~ spl28_272 | ~ spl28_276 | spl28_359), inference(avatar_contradiction_clause, [], [f7765])).
fof(f7765, plain, ($false | (~ spl28_12 | ~ spl28_197 | ~ spl28_265 | ~ spl28_272 | ~ spl28_276 | spl28_359)), inference(subsumption_resolution, [], [f7764, f1612])).
fof(f1612, plain, ((e21 = op2(e22, e20)) | ~ spl28_197), inference(avatar_component_clause, [], [f1610])).
fof(f1610, plain, (spl28_197 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_197])])).
fof(f7764, plain, (~ (e21 = op2(e22, e20)) | (~ spl28_12 | ~ spl28_265 | ~ spl28_272 | ~ spl28_276 | spl28_359)), inference(forward_demodulation, [], [f7763, f2130])).
fof(f7763, plain, (~ (op2(e22, e20) = h5(e11)) | (~ spl28_12 | ~ spl28_265 | ~ spl28_276 | spl28_359)), inference(forward_demodulation, [], [f7762, f785])).
fof(f785, plain, ((e11 = op1(e14, e12)) | ~ spl28_12), inference(avatar_component_clause, [], [f783])).
fof(f783, plain, (spl28_12 <=> (e11 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_12])])).
fof(f7762, plain, (~ (op2(e22, e20) = h5(op1(e14, e12))) | (~ spl28_265 | ~ spl28_276 | spl28_359)), inference(forward_demodulation, [], [f7761, f2092])).
fof(f7761, plain, (~ (h5(op1(e14, e12)) = op2(h5(e14), e20)) | (~ spl28_276 | spl28_359)), inference(forward_demodulation, [], [f2590, f2151])).
fof(f2590, plain, (~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | spl28_359), inference(avatar_component_clause, [], [f2588])).
fof(f2588, plain, (spl28_359 <=> (h5(op1(e14, e12)) = op2(h5(e14), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_359])])).
fof(f7725, plain, (~ spl28_1 | spl28_357), inference(avatar_contradiction_clause, [], [f7724])).
fof(f7724, plain, ($false | (~ spl28_1 | spl28_357)), inference(trivial_inequality_removal, [], [f7723])).
fof(f7723, plain, (~ (h5(e10) = h5(e10)) | (~ spl28_1 | spl28_357)), inference(forward_demodulation, [], [f2582, f739])).
fof(f739, plain, ((e10 = op1(e14, e14)) | ~ spl28_1), inference(avatar_component_clause, [], [f737])).
fof(f737, plain, (spl28_1 <=> (e10 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_1])])).
fof(f2582, plain, (~ (h5(e10) = h5(op1(e14, e14))) | spl28_357), inference(avatar_component_clause, [], [f2580])).
fof(f2580, plain, (spl28_357 <=> (h5(e10) = h5(op1(e14, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl28_357])])).
fof(f7676, plain, (~ spl28_56 | ~ spl28_229 | ~ spl28_263 | ~ spl28_276 | spl28_366), inference(avatar_contradiction_clause, [], [f7675])).
fof(f7675, plain, ($false | (~ spl28_56 | ~ spl28_229 | ~ spl28_263 | ~ spl28_276 | spl28_366)), inference(subsumption_resolution, [], [f7674, f1746])).
fof(f1746, plain, ((e23 = op2(e20, e24)) | ~ spl28_229), inference(avatar_component_clause, [], [f1744])).
fof(f1744, plain, (spl28_229 <=> (e23 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_229])])).
fof(f7674, plain, (~ (e23 = op2(e20, e24)) | (~ spl28_56 | ~ spl28_263 | ~ spl28_276 | spl28_366)), inference(forward_demodulation, [], [f7673, f2083])).
fof(f7673, plain, (~ (op2(e20, e24) = h5(e10)) | (~ spl28_56 | ~ spl28_276 | spl28_366)), inference(forward_demodulation, [], [f7672, f970])).
fof(f970, plain, ((e10 = op1(e12, e13)) | ~ spl28_56), inference(avatar_component_clause, [], [f968])).
fof(f968, plain, (spl28_56 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_56])])).
fof(f7672, plain, (~ (op2(e20, e24) = h5(op1(e12, e13))) | (~ spl28_276 | spl28_366)), inference(forward_demodulation, [], [f2618, f2151])).
fof(f2618, plain, (~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | spl28_366), inference(avatar_component_clause, [], [f2616])).
fof(f2616, plain, (spl28_366 <=> (h5(op1(e12, e13)) = op2(h4(e10), e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_366])])).
fof(f7632, plain, (~ spl28_53 | ~ spl28_236 | ~ spl28_265 | ~ spl28_276 | spl28_365), inference(avatar_contradiction_clause, [], [f7631])).
fof(f7631, plain, ($false | (~ spl28_53 | ~ spl28_236 | ~ spl28_265 | ~ spl28_276 | spl28_365)), inference(subsumption_resolution, [], [f7630, f1776])).
fof(f1776, plain, ((e20 = op2(e20, e22)) | ~ spl28_236), inference(avatar_component_clause, [], [f1774])).
fof(f1774, plain, (spl28_236 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_236])])).
fof(f7630, plain, (~ (e20 = op2(e20, e22)) | (~ spl28_53 | ~ spl28_265 | ~ spl28_276 | spl28_365)), inference(forward_demodulation, [], [f7629, f4495])).
fof(f7629, plain, (~ (op2(e20, e22) = h5(e12)) | (~ spl28_53 | ~ spl28_265 | ~ spl28_276 | spl28_365)), inference(forward_demodulation, [], [f7628, f957])).
fof(f957, plain, ((e12 = op1(e12, e14)) | ~ spl28_53), inference(avatar_component_clause, [], [f955])).
fof(f955, plain, (spl28_53 <=> (e12 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_53])])).
fof(f7628, plain, (~ (op2(e20, e22) = h5(op1(e12, e14))) | (~ spl28_265 | ~ spl28_276 | spl28_365)), inference(forward_demodulation, [], [f7627, f2151])).
fof(f7627, plain, (~ (h5(op1(e12, e14)) = op2(h4(e10), e22)) | (~ spl28_265 | spl28_365)), inference(forward_demodulation, [], [f2614, f2092])).
fof(f2614, plain, (~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | spl28_365), inference(avatar_component_clause, [], [f2612])).
fof(f2612, plain, (spl28_365 <=> (h5(op1(e12, e14)) = op2(h4(e10), h5(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl28_365])])).
fof(f7596, plain, (~ spl28_80 | ~ spl28_213 | ~ spl28_265 | ~ spl28_272 | spl28_353), inference(avatar_contradiction_clause, [], [f7595])).
fof(f7595, plain, ($false | (~ spl28_80 | ~ spl28_213 | ~ spl28_265 | ~ spl28_272 | spl28_353)), inference(subsumption_resolution, [], [f7594, f1679])).
fof(f1679, plain, ((e22 = op2(e21, e22)) | ~ spl28_213), inference(avatar_component_clause, [], [f1677])).
fof(f1677, plain, (spl28_213 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_213])])).
fof(f7594, plain, (~ (e22 = op2(e21, e22)) | (~ spl28_80 | ~ spl28_265 | ~ spl28_272 | spl28_353)), inference(forward_demodulation, [], [f7593, f2092])).
fof(f7593, plain, (~ (op2(e21, e22) = h5(e14)) | (~ spl28_80 | ~ spl28_265 | ~ spl28_272 | spl28_353)), inference(forward_demodulation, [], [f7592, f1070])).
fof(f1070, plain, ((e14 = op1(e11, e14)) | ~ spl28_80), inference(avatar_component_clause, [], [f1068])).
fof(f1068, plain, (spl28_80 <=> (e14 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_80])])).
fof(f7592, plain, (~ (op2(e21, e22) = h5(op1(e11, e14))) | (~ spl28_265 | ~ spl28_272 | spl28_353)), inference(forward_demodulation, [], [f7591, f2130])).
fof(f7591, plain, (~ (h5(op1(e11, e14)) = op2(h5(e11), e22)) | (~ spl28_265 | spl28_353)), inference(forward_demodulation, [], [f2566, f2092])).
fof(f2566, plain, (~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | spl28_353), inference(avatar_component_clause, [], [f2564])).
fof(f2564, plain, (spl28_353 <=> (h5(op1(e11, e14)) = op2(h5(e11), h5(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl28_353])])).
fof(f7556, plain, (~ spl28_19 | ~ spl28_195 | ~ spl28_265 | ~ spl28_272 | spl28_355), inference(avatar_contradiction_clause, [], [f7555])).
fof(f7555, plain, ($false | (~ spl28_19 | ~ spl28_195 | ~ spl28_265 | ~ spl28_272 | spl28_355)), inference(subsumption_resolution, [], [f7554, f1603])).
fof(f1603, plain, ((e24 = op2(e22, e21)) | ~ spl28_195), inference(avatar_component_clause, [], [f1601])).
fof(f1601, plain, (spl28_195 <=> (e24 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_195])])).
fof(f7554, plain, (~ (e24 = op2(e22, e21)) | (~ spl28_19 | ~ spl28_265 | ~ spl28_272 | spl28_355)), inference(forward_demodulation, [], [f7553, f606])).
fof(f7553, plain, (~ (op2(e22, e21) = h5(e13)) | (~ spl28_19 | ~ spl28_265 | ~ spl28_272 | spl28_355)), inference(forward_demodulation, [], [f7552, f814])).
fof(f814, plain, ((e13 = op1(e14, e11)) | ~ spl28_19), inference(avatar_component_clause, [], [f812])).
fof(f812, plain, (spl28_19 <=> (e13 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_19])])).
fof(f7552, plain, (~ (op2(e22, e21) = h5(op1(e14, e11))) | (~ spl28_265 | ~ spl28_272 | spl28_355)), inference(forward_demodulation, [], [f7551, f2092])).
fof(f7551, plain, (~ (h5(op1(e14, e11)) = op2(h5(e14), e21)) | (~ spl28_272 | spl28_355)), inference(forward_demodulation, [], [f2574, f2130])).
fof(f2574, plain, (~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | spl28_355), inference(avatar_component_clause, [], [f2572])).
fof(f2572, plain, (spl28_355 <=> (h5(op1(e14, e11)) = op2(h5(e14), h5(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_355])])).
fof(f7512, plain, (~ spl28_40 | spl28_362), inference(avatar_contradiction_clause, [], [f7511])).
fof(f7511, plain, ($false | (~ spl28_40 | spl28_362)), inference(trivial_inequality_removal, [], [f7510])).
fof(f7510, plain, (~ (h5(e14) = h5(e14)) | (~ spl28_40 | spl28_362)), inference(forward_demodulation, [], [f2602, f902])).
fof(f902, plain, ((e14 = op1(e13, e12)) | ~ spl28_40), inference(avatar_component_clause, [], [f900])).
fof(f900, plain, (spl28_40 <=> (e14 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_40])])).
fof(f2602, plain, (~ (h5(e14) = h5(op1(e13, e12))) | spl28_362), inference(avatar_component_clause, [], [f2600])).
fof(f2600, plain, (spl28_362 <=> (h5(e14) = h5(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl28_362])])).
fof(f7472, plain, (~ spl28_118 | ~ spl28_166 | ~ spl28_263 | ~ spl28_272 | ~ spl28_276 | spl28_349), inference(avatar_contradiction_clause, [], [f7471])).
fof(f7471, plain, ($false | (~ spl28_118 | ~ spl28_166 | ~ spl28_263 | ~ spl28_272 | ~ spl28_276 | spl28_349)), inference(subsumption_resolution, [], [f7470, f1482])).
fof(f1482, plain, ((e20 = op2(e23, e21)) | ~ spl28_166), inference(avatar_component_clause, [], [f1480])).
fof(f1480, plain, (spl28_166 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_166])])).
fof(f7470, plain, (~ (e20 = op2(e23, e21)) | (~ spl28_118 | ~ spl28_263 | ~ spl28_272 | ~ spl28_276 | spl28_349)), inference(forward_demodulation, [], [f7469, f4495])).
fof(f7469, plain, (~ (op2(e23, e21) = h5(e12)) | (~ spl28_118 | ~ spl28_263 | ~ spl28_272 | spl28_349)), inference(forward_demodulation, [], [f7468, f1230])).
fof(f1230, plain, ((e12 = op1(e10, e11)) | ~ spl28_118), inference(avatar_component_clause, [], [f1228])).
fof(f1228, plain, (spl28_118 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_118])])).
fof(f7468, plain, (~ (op2(e23, e21) = h5(op1(e10, e11))) | (~ spl28_263 | ~ spl28_272 | spl28_349)), inference(forward_demodulation, [], [f7467, f2083])).
fof(f7467, plain, (~ (h5(op1(e10, e11)) = op2(h5(e10), e21)) | (~ spl28_272 | spl28_349)), inference(forward_demodulation, [], [f2550, f2130])).
fof(f2550, plain, (~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | spl28_349), inference(avatar_component_clause, [], [f2548])).
fof(f2548, plain, (spl28_349 <=> (h5(op1(e10, e11)) = op2(h5(e10), h5(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_349])])).
fof(f7422, plain, (~ spl28_23 | ~ spl28_181 | ~ spl28_263 | ~ spl28_265 | ~ spl28_276 | spl28_354), inference(avatar_contradiction_clause, [], [f7421])).
fof(f7421, plain, ($false | (~ spl28_23 | ~ spl28_181 | ~ spl28_263 | ~ spl28_265 | ~ spl28_276 | spl28_354)), inference(subsumption_resolution, [], [f7420, f1545])).
fof(f7420, plain, (~ (e20 = op2(e22, e23)) | (~ spl28_23 | ~ spl28_263 | ~ spl28_265 | ~ spl28_276 | spl28_354)), inference(forward_demodulation, [], [f7419, f4495])).
fof(f7419, plain, (~ (op2(e22, e23) = h5(e12)) | (~ spl28_23 | ~ spl28_263 | ~ spl28_265 | spl28_354)), inference(forward_demodulation, [], [f7418, f831])).
fof(f831, plain, ((e12 = op1(e14, e10)) | ~ spl28_23), inference(avatar_component_clause, [], [f829])).
fof(f829, plain, (spl28_23 <=> (e12 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_23])])).
fof(f7418, plain, (~ (op2(e22, e23) = h5(op1(e14, e10))) | (~ spl28_263 | ~ spl28_265 | spl28_354)), inference(forward_demodulation, [], [f7417, f2092])).
fof(f7417, plain, (~ (h5(op1(e14, e10)) = op2(h5(e14), e23)) | (~ spl28_263 | spl28_354)), inference(forward_demodulation, [], [f2570, f2083])).
fof(f2570, plain, (~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | spl28_354), inference(avatar_component_clause, [], [f2568])).
fof(f2568, plain, (spl28_354 <=> (h5(op1(e14, e10)) = op2(h5(e14), h5(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_354])])).
fof(f7383, plain, (~ spl28_104 | ~ spl28_165 | ~ spl28_263 | ~ spl28_265 | spl28_350), inference(avatar_contradiction_clause, [], [f7382])).
fof(f7382, plain, ($false | (~ spl28_104 | ~ spl28_165 | ~ spl28_263 | ~ spl28_265 | spl28_350)), inference(subsumption_resolution, [], [f7381, f1477])).
fof(f7381, plain, (~ (e24 = op2(e23, e22)) | (~ spl28_104 | ~ spl28_263 | ~ spl28_265 | spl28_350)), inference(forward_demodulation, [], [f7380, f606])).
fof(f7380, plain, (~ (op2(e23, e22) = h5(e13)) | (~ spl28_104 | ~ spl28_263 | ~ spl28_265 | spl28_350)), inference(forward_demodulation, [], [f7379, f1171])).
fof(f1171, plain, ((e13 = op1(e10, e14)) | ~ spl28_104), inference(avatar_component_clause, [], [f1169])).
fof(f1169, plain, (spl28_104 <=> (e13 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_104])])).
fof(f7379, plain, (~ (op2(e23, e22) = h5(op1(e10, e14))) | (~ spl28_263 | ~ spl28_265 | spl28_350)), inference(forward_demodulation, [], [f7378, f2083])).
fof(f7378, plain, (~ (h5(op1(e10, e14)) = op2(h5(e10), e22)) | (~ spl28_265 | spl28_350)), inference(forward_demodulation, [], [f2554, f2092])).
fof(f2554, plain, (~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | spl28_350), inference(avatar_component_clause, [], [f2552])).
fof(f2552, plain, (spl28_350 <=> (h5(op1(e10, e14)) = op2(h5(e10), h5(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl28_350])])).
fof(f7338, plain, (~ spl28_10 | ~ spl28_178 | ~ spl28_265 | spl28_358), inference(avatar_contradiction_clause, [], [f7337])).
fof(f7337, plain, ($false | (~ spl28_10 | ~ spl28_178 | ~ spl28_265 | spl28_358)), inference(subsumption_resolution, [], [f7336, f1532])).
fof(f1532, plain, ((e22 = op2(e22, e24)) | ~ spl28_178), inference(avatar_component_clause, [], [f1530])).
fof(f7336, plain, (~ (e22 = op2(e22, e24)) | (~ spl28_10 | ~ spl28_265 | spl28_358)), inference(forward_demodulation, [], [f7335, f2092])).
fof(f7335, plain, (~ (op2(e22, e24) = h5(e14)) | (~ spl28_10 | ~ spl28_265 | spl28_358)), inference(forward_demodulation, [], [f7334, f776])).
fof(f776, plain, ((e14 = op1(e14, e13)) | ~ spl28_10), inference(avatar_component_clause, [], [f774])).
fof(f774, plain, (spl28_10 <=> (e14 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_10])])).
fof(f7334, plain, (~ (op2(e22, e24) = h5(op1(e14, e13))) | (~ spl28_265 | spl28_358)), inference(forward_demodulation, [], [f2586, f2092])).
fof(f2586, plain, (~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | spl28_358), inference(avatar_component_clause, [], [f2584])).
fof(f2584, plain, (spl28_358 <=> (h5(op1(e14, e13)) = op2(h5(e14), e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_358])])).
fof(f7306, plain, (~ spl28_96 | ~ spl28_209 | ~ spl28_263 | ~ spl28_272 | spl28_351), inference(avatar_contradiction_clause, [], [f7305])).
fof(f7305, plain, ($false | (~ spl28_96 | ~ spl28_209 | ~ spl28_263 | ~ spl28_272 | spl28_351)), inference(subsumption_resolution, [], [f7304, f1662])).
fof(f1662, plain, ((e23 = op2(e21, e23)) | ~ spl28_209), inference(avatar_component_clause, [], [f1660])).
fof(f1660, plain, (spl28_209 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_209])])).
fof(f7304, plain, (~ (e23 = op2(e21, e23)) | (~ spl28_96 | ~ spl28_263 | ~ spl28_272 | spl28_351)), inference(forward_demodulation, [], [f7303, f2083])).
fof(f7303, plain, (~ (op2(e21, e23) = h5(e10)) | (~ spl28_96 | ~ spl28_263 | ~ spl28_272 | spl28_351)), inference(forward_demodulation, [], [f7302, f1138])).
fof(f1138, plain, ((e10 = op1(e11, e10)) | ~ spl28_96), inference(avatar_component_clause, [], [f1136])).
fof(f1136, plain, (spl28_96 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_96])])).
fof(f7302, plain, (~ (op2(e21, e23) = h5(op1(e11, e10))) | (~ spl28_263 | ~ spl28_272 | spl28_351)), inference(forward_demodulation, [], [f7301, f2130])).
fof(f7301, plain, (~ (h5(op1(e11, e10)) = op2(h5(e11), e23)) | (~ spl28_263 | spl28_351)), inference(forward_demodulation, [], [f2558, f2083])).
fof(f2558, plain, (~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | spl28_351), inference(avatar_component_clause, [], [f2556])).
fof(f2556, plain, (spl28_351 <=> (h5(op1(e11, e10)) = op2(h5(e11), h5(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_351])])).
fof(f7262, plain, (~ spl28_248 | ~ spl28_250), inference(avatar_contradiction_clause, [], [f7261])).
fof(f7261, plain, ($false | (~ spl28_248 | ~ spl28_250)), inference(subsumption_resolution, [], [f7260, f501])).
fof(f501, plain, ~ (e22 = e24), inference(cnf_transformation, [], [f10])).
fof(f7260, plain, ((e22 = e24) | (~ spl28_248 | ~ spl28_250)), inference(backward_demodulation, [], [f1834, f1826])).
fof(f1826, plain, ((op2(e20, e20) = e22) | ~ spl28_248), inference(avatar_component_clause, [], [f1824])).
fof(f1824, plain, (spl28_248 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl28_248])])).
fof(f1834, plain, ((op2(e20, e20) = e24) | ~ spl28_250), inference(avatar_component_clause, [], [f1832])).
fof(f1832, plain, (spl28_250 <=> (op2(e20, e20) = e24)), introduced(avatar_definition, [new_symbols(naming, [spl28_250])])).
fof(f7252, plain, (~ spl28_92 | ~ spl28_217 | ~ spl28_272 | spl28_352), inference(avatar_contradiction_clause, [], [f7251])).
fof(f7251, plain, ($false | (~ spl28_92 | ~ spl28_217 | ~ spl28_272 | spl28_352)), inference(subsumption_resolution, [], [f7250, f1696])).
fof(f1696, plain, ((e21 = op2(e21, e21)) | ~ spl28_217), inference(avatar_component_clause, [], [f1694])).
fof(f1694, plain, (spl28_217 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_217])])).
fof(f7250, plain, (~ (e21 = op2(e21, e21)) | (~ spl28_92 | ~ spl28_272 | spl28_352)), inference(forward_demodulation, [], [f7249, f2130])).
fof(f7249, plain, (~ (op2(e21, e21) = h5(e11)) | (~ spl28_92 | ~ spl28_272 | spl28_352)), inference(forward_demodulation, [], [f7248, f1121])).
fof(f1121, plain, ((e11 = op1(e11, e11)) | ~ spl28_92), inference(avatar_component_clause, [], [f1119])).
fof(f1119, plain, (spl28_92 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_92])])).
fof(f7248, plain, (~ (op2(e21, e21) = h5(op1(e11, e11))) | (~ spl28_272 | spl28_352)), inference(forward_demodulation, [], [f2562, f2130])).
fof(f2562, plain, (~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | spl28_352), inference(avatar_component_clause, [], [f2560])).
fof(f2560, plain, (spl28_352 <=> (h5(op1(e11, e11)) = op2(h5(e11), h5(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl28_352])])).
fof(f7186, plain, (~ spl28_107 | ~ spl28_152 | ~ spl28_263 | ~ spl28_272 | spl28_372), inference(avatar_contradiction_clause, [], [f7185])).
fof(f7185, plain, ($false | (~ spl28_107 | ~ spl28_152 | ~ spl28_263 | ~ spl28_272 | spl28_372)), inference(subsumption_resolution, [], [f7184, f1423])).
fof(f1423, plain, ((e21 = op2(e23, e24)) | ~ spl28_152), inference(avatar_component_clause, [], [f1421])).
fof(f1421, plain, (spl28_152 <=> (e21 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_152])])).
fof(f7184, plain, (~ (e21 = op2(e23, e24)) | (~ spl28_107 | ~ spl28_263 | ~ spl28_272 | spl28_372)), inference(forward_demodulation, [], [f7183, f2130])).
fof(f7183, plain, (~ (op2(e23, e24) = h5(e11)) | (~ spl28_107 | ~ spl28_263 | spl28_372)), inference(forward_demodulation, [], [f7182, f1184])).
fof(f1184, plain, ((e11 = op1(e10, e13)) | ~ spl28_107), inference(avatar_component_clause, [], [f1182])).
fof(f1182, plain, (spl28_107 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_107])])).
fof(f7182, plain, (~ (op2(e23, e24) = h5(op1(e10, e13))) | (~ spl28_263 | spl28_372)), inference(forward_demodulation, [], [f2642, f2083])).
fof(f2642, plain, (~ (h5(op1(e10, e13)) = op2(h5(e10), e24)) | spl28_372), inference(avatar_component_clause, [], [f2640])).
fof(f2640, plain, (spl28_372 <=> (h5(op1(e10, e13)) = op2(h5(e10), e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_372])])).
fof(f7146, plain, (~ spl28_207 | ~ spl28_209), inference(avatar_contradiction_clause, [], [f7145])).
fof(f7145, plain, ($false | (~ spl28_207 | ~ spl28_209)), inference(subsumption_resolution, [], [f7144, f498])).
fof(f498, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f10])).
fof(f7144, plain, ((e21 = e23) | (~ spl28_207 | ~ spl28_209)), inference(backward_demodulation, [], [f1662, f1654])).
fof(f1654, plain, ((e21 = op2(e21, e23)) | ~ spl28_207), inference(avatar_component_clause, [], [f1652])).
fof(f1652, plain, (spl28_207 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_207])])).
fof(f7104, plain, (~ spl28_125 | ~ spl28_158 | ~ spl28_263 | ~ spl28_265 | spl28_348), inference(avatar_contradiction_clause, [], [f7103])).
fof(f7103, plain, ($false | (~ spl28_125 | ~ spl28_158 | ~ spl28_263 | ~ spl28_265 | spl28_348)), inference(subsumption_resolution, [], [f7102, f1448])).
fof(f1448, plain, ((e22 = op2(e23, e23)) | ~ spl28_158), inference(avatar_component_clause, [], [f1446])).
fof(f1446, plain, (spl28_158 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_158])])).
fof(f7102, plain, (~ (e22 = op2(e23, e23)) | (~ spl28_125 | ~ spl28_263 | ~ spl28_265 | spl28_348)), inference(backward_demodulation, [], [f6912, f2083])).
fof(f6912, plain, (~ (e22 = op2(h5(e10), h5(e10))) | (~ spl28_125 | ~ spl28_265 | spl28_348)), inference(backward_demodulation, [], [f5263, f2092])).
fof(f5263, plain, (~ (h5(e14) = op2(h5(e10), h5(e10))) | (~ spl28_125 | spl28_348)), inference(forward_demodulation, [], [f2546, f1259])).
fof(f1259, plain, ((op1(e10, e10) = e14) | ~ spl28_125), inference(avatar_component_clause, [], [f1257])).
fof(f1257, plain, (spl28_125 <=> (op1(e10, e10) = e14)), introduced(avatar_definition, [new_symbols(naming, [spl28_125])])).
fof(f2546, plain, (~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10))) | spl28_348), inference(avatar_component_clause, [], [f2544])).
fof(f2544, plain, (spl28_348 <=> (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl28_348])])).
fof(f7101, plain, (spl28_213 | ~ spl28_178 | ~ spl28_265 | ~ spl28_272), inference(avatar_split_clause, [], [f7100, f2129, f2091, f1530, f1677])).
fof(f7100, plain, ((e22 = op2(e21, e22)) | (~ spl28_178 | ~ spl28_265 | ~ spl28_272)), inference(backward_demodulation, [], [f6918, f2130])).
fof(f6918, plain, ((e22 = op2(h5(e11), e22)) | (~ spl28_178 | ~ spl28_265)), inference(backward_demodulation, [], [f6815, f6905])).
fof(f6905, plain, ((op2(e24, e22) = h5(e11)) | ~ spl28_265), inference(backward_demodulation, [], [f2057, f2092])).
fof(f2057, plain, (h5(e11) = op2(e24, h5(e14))), inference(forward_demodulation, [], [f2056, f2054])).
fof(f2054, plain, (h5(e14) = op2(e24, h4(e10))), inference(forward_demodulation, [], [f610, f2044])).
fof(f610, plain, (op2(e24, op2(e24, e24)) = h5(e14)), inference(cnf_transformation, [], [f20])).
fof(f2056, plain, (h5(e11) = op2(e24, op2(e24, h4(e10)))), inference(forward_demodulation, [], [f608, f2044])).
fof(f608, plain, (h5(e11) = op2(e24, op2(e24, op2(e24, e24)))), inference(cnf_transformation, [], [f20])).
fof(f6815, plain, ((e22 = op2(op2(e24, e22), e22)) | ~ spl28_178), inference(forward_demodulation, [], [f272, f1532])).
fof(f272, plain, (e22 = op2(op2(e24, e22), op2(e22, e24))), inference(cnf_transformation, [], [f6])).
fof(f7067, plain, (~ spl28_418 | ~ spl28_195 | ~ spl28_290), inference(avatar_split_clause, [], [f7066, f2228, f1601, f3048])).
fof(f3048, plain, (spl28_418 <=> (e24 = h3(e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_418])])).
fof(f2228, plain, (spl28_290 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_290])])).
fof(f7066, plain, (~ (e24 = h3(e14)) | (~ spl28_195 | ~ spl28_290)), inference(forward_demodulation, [], [f5727, f1603])).
fof(f5727, plain, (~ (op2(e22, e21) = h3(e14)) | ~ spl28_290), inference(backward_demodulation, [], [f458, f5719])).
fof(f5719, plain, ((op2(e22, e23) = h3(e14)) | ~ spl28_290), inference(backward_demodulation, [], [f2015, f2229])).
fof(f2229, plain, ((e23 = h3(e12)) | ~ spl28_290), inference(avatar_component_clause, [], [f2228])).
fof(f2015, plain, (h3(e14) = op2(e22, h3(e12))), inference(backward_demodulation, [], [f600, f599])).
fof(f599, plain, (op2(e22, e22) = h3(e12)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ((op2(e22, op2(e22, e22)) = h3(e14)) & (op2(e22, e22) = h3(e12)) & (h3(e11) = op2(e22, op2(e22, op2(e22, e22)))) & (h3(e10) = op2(op2(e22, op2(e22, e22)), op2(e22, op2(e22, e22)))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax18)).
fof(f600, plain, (op2(e22, op2(e22, e22)) = h3(e14)), inference(cnf_transformation, [], [f18])).
fof(f458, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f7062, plain, (~ spl28_233 | ~ spl28_280), inference(avatar_split_clause, [], [f6472, f2173, f1761])).
fof(f1761, plain, (spl28_233 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_233])])).
fof(f2173, plain, (spl28_280 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_280])])).
fof(f6472, plain, (~ (e22 = op2(e20, e23)) | ~ spl28_280), inference(forward_demodulation, [], [f2022, f2174])).
fof(f2174, plain, ((e22 = h4(e12)) | ~ spl28_280), inference(avatar_component_clause, [], [f2173])).
fof(f2022, plain, ~ (op2(e20, e23) = h4(e12)), inference(backward_demodulation, [], [f415, f604])).
fof(f415, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f7029, plain, (~ spl28_9 | ~ spl28_84), inference(avatar_split_clause, [], [f7025, f1085, f770])).
fof(f770, plain, (spl28_9 <=> (e13 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_9])])).
fof(f7025, plain, (~ (e13 = op1(e14, e13)) | ~ spl28_84), inference(backward_demodulation, [], [f319, f1087])).
fof(f319, plain, ~ (op1(e11, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (op1(e14, e13) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e14)) & ~ (op1(e14, e11) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e14)) & ~ (op1(e14, e10) = op1(e14, e13)) & ~ (op1(e14, e10) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e11)) & ~ (op1(e13, e13) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e14)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e14)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e13) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e14)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e14)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e13) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e14)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e14)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e13) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e14)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e14)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e13, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e14, e14)) & ~ (op1(e11, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e14, e14)) & ~ (op1(e10, e14) = op1(e13, e14)) & ~ (op1(e10, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e11, e14)) & ~ (op1(e13, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e14, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e14, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e13, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e14, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e14, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e13, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e14, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e14, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e13, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e14, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e14, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax7)).
fof(f7011, plain, (spl28_272 | ~ spl28_137 | ~ spl28_265), inference(avatar_split_clause, [], [f7009, f2091, f1358, f2129])).
fof(f1358, plain, (spl28_137 <=> (e21 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_137])])).
fof(f7009, plain, ((e21 = h5(e11)) | (~ spl28_137 | ~ spl28_265)), inference(backward_demodulation, [], [f6905, f1360])).
fof(f1360, plain, ((e21 = op2(e24, e22)) | ~ spl28_137), inference(avatar_component_clause, [], [f1358])).
fof(f6999, plain, (~ spl28_165 | spl28_204 | ~ spl28_290 | ~ spl28_299), inference(avatar_contradiction_clause, [], [f6998])).
fof(f6998, plain, ($false | (~ spl28_165 | spl28_204 | ~ spl28_290 | ~ spl28_299)), inference(subsumption_resolution, [], [f6993, f1640])).
fof(f1640, plain, (~ (e23 = op2(e21, e24)) | spl28_204), inference(avatar_component_clause, [], [f1639])).
fof(f1639, plain, (spl28_204 <=> (e23 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_204])])).
fof(f6993, plain, ((e23 = op2(e21, e24)) | (~ spl28_165 | ~ spl28_290 | ~ spl28_299)), inference(backward_demodulation, [], [f6546, f1477])).
fof(f6546, plain, ((e23 = op2(e21, op2(e23, e22))) | (~ spl28_290 | ~ spl28_299)), inference(backward_demodulation, [], [f6445, f2274])).
fof(f2274, plain, ((e21 = h3(e14)) | ~ spl28_299), inference(avatar_component_clause, [], [f2273])).
fof(f2273, plain, (spl28_299 <=> (e21 = h3(e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_299])])).
fof(f6445, plain, ((e23 = op2(h3(e14), op2(e23, e22))) | ~ spl28_290), inference(backward_demodulation, [], [f275, f5719])).
fof(f275, plain, (e23 = op2(op2(e22, e23), op2(e23, e22))), inference(cnf_transformation, [], [f6])).
fof(f6982, plain, (~ spl28_187 | ~ spl28_189), inference(avatar_contradiction_clause, [], [f6981])).
fof(f6981, plain, ($false | (~ spl28_187 | ~ spl28_189)), inference(subsumption_resolution, [], [f6980, f498])).
fof(f6980, plain, ((e21 = e23) | (~ spl28_187 | ~ spl28_189)), inference(backward_demodulation, [], [f1578, f1570])).
fof(f1570, plain, ((e21 = op2(e22, e22)) | ~ spl28_187), inference(avatar_component_clause, [], [f1568])).
fof(f1568, plain, (spl28_187 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_187])])).
fof(f1578, plain, ((e23 = op2(e22, e22)) | ~ spl28_189), inference(avatar_component_clause, [], [f1576])).
fof(f1576, plain, (spl28_189 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_189])])).
fof(f6930, plain, (~ spl28_249 | ~ spl28_250), inference(avatar_contradiction_clause, [], [f6929])).
fof(f6929, plain, ($false | (~ spl28_249 | ~ spl28_250)), inference(subsumption_resolution, [], [f6928, f502])).
fof(f502, plain, ~ (e23 = e24), inference(cnf_transformation, [], [f10])).
fof(f6928, plain, ((e23 = e24) | (~ spl28_249 | ~ spl28_250)), inference(backward_demodulation, [], [f1834, f1830])).
fof(f1830, plain, ((op2(e20, e20) = e23) | ~ spl28_249), inference(avatar_component_clause, [], [f1828])).
fof(f1828, plain, (spl28_249 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl28_249])])).
fof(f6923, plain, (~ spl28_198 | ~ spl28_265 | ~ spl28_276), inference(avatar_split_clause, [], [f6910, f2150, f2091, f1614])).
fof(f1614, plain, (spl28_198 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_198])])).
fof(f6910, plain, (~ (e22 = op2(e22, e20)) | (~ spl28_265 | ~ spl28_276)), inference(backward_demodulation, [], [f4838, f2092])).
fof(f4838, plain, (~ (op2(e22, e20) = h5(e14)) | ~ spl28_276), inference(forward_demodulation, [], [f391, f4494])).
fof(f4494, plain, ((op2(e24, e20) = h5(e14)) | ~ spl28_276), inference(backward_demodulation, [], [f2054, f2151])).
fof(f391, plain, ~ (op2(e22, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f6921, plain, (spl28_263 | ~ spl28_189 | ~ spl28_265), inference(avatar_split_clause, [], [f6920, f2091, f1576, f2082])).
fof(f6920, plain, ((e23 = h5(e10)) | (~ spl28_189 | ~ spl28_265)), inference(forward_demodulation, [], [f6906, f1578])).
fof(f6906, plain, ((op2(e22, e22) = h5(e10)) | ~ spl28_265), inference(backward_demodulation, [], [f2059, f2092])).
fof(f2059, plain, (h5(e10) = op2(h5(e14), h5(e14))), inference(forward_demodulation, [], [f2058, f2054])).
fof(f2058, plain, (h5(e10) = op2(op2(e24, h4(e10)), op2(e24, h4(e10)))), inference(forward_demodulation, [], [f607, f2044])).
fof(f607, plain, (h5(e10) = op2(op2(e24, op2(e24, e24)), op2(e24, op2(e24, e24)))), inference(cnf_transformation, [], [f20])).
fof(f6865, plain, (~ spl28_158 | ~ spl28_169 | ~ spl28_209), inference(avatar_contradiction_clause, [], [f6864])).
fof(f6864, plain, ($false | (~ spl28_158 | ~ spl28_169 | ~ spl28_209)), inference(subsumption_resolution, [], [f6863, f500])).
fof(f500, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f10])).
fof(f6863, plain, ((e22 = e23) | (~ spl28_158 | ~ spl28_169 | ~ spl28_209)), inference(forward_demodulation, [], [f6862, f1448])).
fof(f6862, plain, ((e23 = op2(e23, e23)) | (~ spl28_169 | ~ spl28_209)), inference(forward_demodulation, [], [f6861, f1662])).
fof(f6861, plain, ((e23 = op2(op2(e21, e23), e23)) | ~ spl28_169), inference(forward_demodulation, [], [f274, f1494])).
fof(f1494, plain, ((e23 = op2(e23, e21)) | ~ spl28_169), inference(avatar_component_clause, [], [f1492])).
fof(f274, plain, (e23 = op2(op2(e21, e23), op2(e23, e21))), inference(cnf_transformation, [], [f6])).
fof(f6856, plain, (~ spl28_198 | ~ spl28_178), inference(avatar_split_clause, [], [f6855, f1530, f1614])).
fof(f6855, plain, (~ (e22 = op2(e22, e20)) | ~ spl28_178), inference(forward_demodulation, [], [f456, f1532])).
fof(f456, plain, ~ (op2(e22, e20) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f6840, plain, (spl28_187 | ~ spl28_315), inference(avatar_split_clause, [], [f6522, f2354, f1568])).
fof(f2354, plain, (spl28_315 <=> (e22 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_315])])).
fof(f6522, plain, ((e21 = op2(e22, e22)) | ~ spl28_315), inference(backward_demodulation, [], [f1992, f2355])).
fof(f2355, plain, ((e22 = h2(e12)) | ~ spl28_315), inference(avatar_component_clause, [], [f2354])).
fof(f1992, plain, (e21 = op2(h2(e12), h2(e12))), inference(backward_demodulation, [], [f264, f594])).
fof(f594, plain, (op2(e21, e21) = h2(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e21, op2(e21, e21)) = h2(e14)) & (op2(e21, e21) = h2(e12)) & (h2(e11) = op2(e21, op2(e21, op2(e21, e21)))) & (h2(e10) = op2(op2(e21, op2(e21, e21)), op2(e21, op2(e21, e21)))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax17)).
fof(f264, plain, (e21 = op2(op2(e21, e21), op2(e21, e21))), inference(cnf_transformation, [], [f6])).
fof(f6839, plain, (~ spl28_175 | ~ spl28_488), inference(avatar_split_clause, [], [f5660, f3466, f1517])).
fof(f1517, plain, (spl28_175 <=> (e24 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_175])])).
fof(f3466, plain, (spl28_488 <=> (e24 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_488])])).
fof(f5660, plain, (~ (e24 = op2(e23, e20)) | ~ spl28_488), inference(forward_demodulation, [], [f1981, f3467])).
fof(f3467, plain, ((e24 = h1(e12)) | ~ spl28_488), inference(avatar_component_clause, [], [f3466])).
fof(f1981, plain, ~ (op2(e23, e20) = h1(e12)), inference(backward_demodulation, [], [f385, f589])).
fof(f589, plain, (op2(e20, e20) = h1(e12)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e20, op2(e20, e20)) = h1(e14)) & (op2(e20, e20) = h1(e12)) & (h1(e11) = op2(e20, op2(e20, op2(e20, e20)))) & (h1(e10) = op2(op2(e20, op2(e20, e20)), op2(e20, op2(e20, e20)))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax16)).
fof(f385, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f6838, plain, (~ spl28_174 | ~ spl28_169), inference(avatar_split_clause, [], [f6837, f1492, f1513])).
fof(f6837, plain, (~ (e23 = op2(e23, e20)) | ~ spl28_169), inference(forward_demodulation, [], [f463, f1494])).
fof(f463, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f6835, plain, (spl28_165 | ~ spl28_280), inference(avatar_split_clause, [], [f4983, f2173, f1475])).
fof(f4983, plain, ((e24 = op2(e23, e22)) | ~ spl28_280), inference(backward_demodulation, [], [f2030, f2174])).
fof(f6818, plain, (~ spl28_140 | ~ spl28_135), inference(avatar_split_clause, [], [f6817, f1349, f1370])).
fof(f1370, plain, (spl28_140 <=> (e24 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_140])])).
fof(f6817, plain, (~ (e24 = op2(e24, e22)) | ~ spl28_135), inference(forward_demodulation, [], [f480, f1351])).
fof(f480, plain, ~ (op2(e24, e22) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f6809, plain, (~ spl28_120 | ~ spl28_70), inference(avatar_split_clause, [], [f6396, f1026, f1236])).
fof(f1236, plain, (spl28_120 <=> (e14 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_120])])).
fof(f6396, plain, (~ (e14 = op1(e10, e11)) | ~ spl28_70), inference(forward_demodulation, [], [f294, f1028])).
fof(f294, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f6804, plain, (spl28_104 | ~ spl28_40 | ~ spl28_56), inference(avatar_split_clause, [], [f6729, f968, f900, f1169])).
fof(f6729, plain, ((e13 = op1(e10, e14)) | (~ spl28_40 | ~ spl28_56)), inference(backward_demodulation, [], [f4761, f970])).
fof(f4761, plain, ((e13 = op1(op1(e12, e13), e14)) | ~ spl28_40), inference(forward_demodulation, [], [f175, f902])).
fof(f175, plain, (e13 = op1(op1(e12, e13), op1(e13, e12))), inference(cnf_transformation, [], [f3])).
fof(f6793, plain, (~ spl28_85 | ~ spl28_80), inference(avatar_split_clause, [], [f6792, f1068, f1089])).
fof(f1089, plain, (spl28_85 <=> (e14 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_85])])).
fof(f6792, plain, (~ (e14 = op1(e11, e13)) | ~ spl28_80), inference(forward_demodulation, [], [f352, f1070])).
fof(f352, plain, ~ (op1(e11, e13) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f6788, plain, (spl28_84 | ~ spl28_49 | ~ spl28_107), inference(avatar_split_clause, [], [f6739, f1182, f938, f1085])).
fof(f6739, plain, ((e13 = op1(e11, e13)) | (~ spl28_49 | ~ spl28_107)), inference(backward_demodulation, [], [f6391, f940])).
fof(f6391, plain, ((e13 = op1(e11, op1(e13, e10))) | ~ spl28_107), inference(forward_demodulation, [], [f173, f1184])).
fof(f173, plain, (e13 = op1(op1(e10, e13), op1(e13, e10))), inference(cnf_transformation, [], [f3])).
fof(f6756, plain, (~ spl28_41 | ~ spl28_85 | spl28_102), inference(avatar_contradiction_clause, [], [f6755])).
fof(f6755, plain, ($false | (~ spl28_41 | ~ spl28_85 | spl28_102)), inference(subsumption_resolution, [], [f6751, f1162])).
fof(f1162, plain, (~ (e11 = op1(e10, e14)) | spl28_102), inference(avatar_component_clause, [], [f1161])).
fof(f1161, plain, (spl28_102 <=> (e11 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_102])])).
fof(f6751, plain, ((e11 = op1(e10, e14)) | (~ spl28_41 | ~ spl28_85)), inference(backward_demodulation, [], [f6415, f907])).
fof(f6415, plain, ((e11 = op1(op1(e13, e11), e14)) | ~ spl28_85), inference(backward_demodulation, [], [f166, f1091])).
fof(f1091, plain, ((e14 = op1(e11, e13)) | ~ spl28_85), inference(avatar_component_clause, [], [f1089])).
fof(f166, plain, (e11 = op1(op1(e13, e11), op1(e11, e13))), inference(cnf_transformation, [], [f3])).
fof(f6741, plain, (spl28_41 | ~ spl28_49 | ~ spl28_107), inference(avatar_split_clause, [], [f6738, f1182, f938, f905])).
fof(f6738, plain, ((e10 = op1(e13, e11)) | (~ spl28_49 | ~ spl28_107)), inference(backward_demodulation, [], [f6390, f940])).
fof(f6390, plain, ((e10 = op1(op1(e13, e10), e11)) | ~ spl28_107), inference(forward_demodulation, [], [f161, f1184])).
fof(f161, plain, (e10 = op1(op1(e13, e10), op1(e10, e13))), inference(cnf_transformation, [], [f3])).
fof(f6740, plain, (~ spl28_44 | ~ spl28_49), inference(avatar_split_clause, [], [f6736, f938, f917])).
fof(f917, plain, (spl28_44 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_44])])).
fof(f6736, plain, (~ (e13 = op1(e13, e11)) | ~ spl28_49), inference(backward_demodulation, [], [f363, f940])).
fof(f363, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f6734, plain, (spl28_23 | ~ spl28_40 | ~ spl28_56), inference(avatar_split_clause, [], [f6730, f968, f900, f829])).
fof(f6730, plain, ((e12 = op1(e14, e10)) | (~ spl28_40 | ~ spl28_56)), inference(backward_demodulation, [], [f4762, f970])).
fof(f4762, plain, ((e12 = op1(e14, op1(e12, e13))) | ~ spl28_40), inference(forward_demodulation, [], [f171, f902])).
fof(f171, plain, (e12 = op1(op1(e13, e12), op1(e12, e13))), inference(cnf_transformation, [], [f3])).
fof(f6695, plain, (~ spl28_49 | ~ spl28_99), inference(avatar_split_clause, [], [f6688, f1148, f938])).
fof(f1148, plain, (spl28_99 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_99])])).
fof(f6688, plain, (~ (e13 = op1(e13, e10)) | ~ spl28_99), inference(backward_demodulation, [], [f288, f1150])).
fof(f1150, plain, ((e13 = op1(e11, e10)) | ~ spl28_99), inference(avatar_component_clause, [], [f1148])).
fof(f288, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f7])).
fof(f6675, plain, (~ spl28_158 | ~ spl28_160), inference(avatar_contradiction_clause, [], [f6674])).
fof(f6674, plain, ($false | (~ spl28_158 | ~ spl28_160)), inference(subsumption_resolution, [], [f6673, f501])).
fof(f6673, plain, ((e22 = e24) | (~ spl28_158 | ~ spl28_160)), inference(forward_demodulation, [], [f1456, f1448])).
fof(f1456, plain, ((e24 = op2(e23, e23)) | ~ spl28_160), inference(avatar_component_clause, [], [f1454])).
fof(f1454, plain, (spl28_160 <=> (e24 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_160])])).
fof(f6654, plain, (~ spl28_171 | ~ spl28_174), inference(avatar_contradiction_clause, [], [f6653])).
fof(f6653, plain, ($false | (~ spl28_171 | ~ spl28_174)), inference(subsumption_resolution, [], [f6652, f495])).
fof(f495, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f10])).
fof(f6652, plain, ((e20 = e23) | (~ spl28_171 | ~ spl28_174)), inference(backward_demodulation, [], [f1515, f1503])).
fof(f1503, plain, ((e20 = op2(e23, e20)) | ~ spl28_171), inference(avatar_component_clause, [], [f1501])).
fof(f1501, plain, (spl28_171 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_171])])).
fof(f1515, plain, ((e23 = op2(e23, e20)) | ~ spl28_174), inference(avatar_component_clause, [], [f1513])).
fof(f6647, plain, (~ spl28_138 | ~ spl28_178 | ~ spl28_189), inference(avatar_contradiction_clause, [], [f6646])).
fof(f6646, plain, ($false | (~ spl28_138 | ~ spl28_178 | ~ spl28_189)), inference(subsumption_resolution, [], [f6645, f502])).
fof(f6645, plain, ((e23 = e24) | (~ spl28_138 | ~ spl28_178 | ~ spl28_189)), inference(forward_demodulation, [], [f6643, f1578])).
fof(f6643, plain, ((e24 = op2(e22, e22)) | (~ spl28_138 | ~ spl28_178)), inference(backward_demodulation, [], [f6441, f1532])).
fof(f6441, plain, ((e24 = op2(op2(e22, e24), e22)) | ~ spl28_138), inference(forward_demodulation, [], [f280, f1364])).
fof(f1364, plain, ((e22 = op2(e24, e22)) | ~ spl28_138), inference(avatar_component_clause, [], [f1362])).
fof(f1362, plain, (spl28_138 <=> (e22 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_138])])).
fof(f280, plain, (e24 = op2(op2(e22, e24), op2(e24, e22))), inference(cnf_transformation, [], [f6])).
fof(f6617, plain, (~ spl28_180 | ~ spl28_205), inference(avatar_split_clause, [], [f6614, f1643, f1538])).
fof(f1538, plain, (spl28_180 <=> (e24 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_180])])).
fof(f6614, plain, (~ (e24 = op2(e22, e24)) | ~ spl28_205), inference(backward_demodulation, [], [f427, f1645])).
fof(f427, plain, ~ (op2(e21, e24) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f6601, plain, (~ spl28_197 | ~ spl28_222), inference(avatar_split_clause, [], [f6594, f1715, f1610])).
fof(f1715, plain, (spl28_222 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_222])])).
fof(f6594, plain, (~ (e21 = op2(e22, e20)) | ~ spl28_222), inference(backward_demodulation, [], [f387, f1717])).
fof(f1717, plain, ((e21 = op2(e21, e20)) | ~ spl28_222), inference(avatar_component_clause, [], [f1715])).
fof(f387, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f8])).
fof(f6503, plain, (~ spl28_235 | ~ spl28_488), inference(avatar_split_clause, [], [f6502, f3466, f1769])).
fof(f1769, plain, (spl28_235 <=> (e24 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_235])])).
fof(f6502, plain, (~ (e24 = op2(e20, e23)) | ~ spl28_488), inference(backward_demodulation, [], [f1985, f3467])).
fof(f1985, plain, ~ (op2(e20, e23) = h1(e12)), inference(backward_demodulation, [], [f435, f589])).
fof(f435, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f8])).
fof(f6488, plain, (spl28_205 | ~ spl28_135 | ~ spl28_284), inference(avatar_split_clause, [], [f6487, f2195, f1349, f1643])).
fof(f6487, plain, ((e24 = op2(e21, e24)) | (~ spl28_135 | ~ spl28_284)), inference(forward_demodulation, [], [f5278, f1351])).
fof(f6471, plain, (~ spl28_234 | ~ spl28_229), inference(avatar_split_clause, [], [f6470, f1744, f1765])).
fof(f1765, plain, (spl28_234 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_234])])).
fof(f6470, plain, (~ (e23 = op2(e20, e23)) | ~ spl28_229), inference(forward_demodulation, [], [f442, f1746])).
fof(f442, plain, ~ (op2(e20, e23) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f6420, plain, (~ spl28_104 | ~ spl28_119), inference(avatar_split_clause, [], [f6419, f1232, f1169])).
fof(f1232, plain, (spl28_119 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_119])])).
fof(f6419, plain, (~ (e13 = op1(e10, e14)) | ~ spl28_119), inference(forward_demodulation, [], [f339, f1234])).
fof(f1234, plain, ((e13 = op1(e10, e11)) | ~ spl28_119), inference(avatar_component_clause, [], [f1232])).
fof(f339, plain, ~ (op1(e10, e11) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f6407, plain, (~ spl28_69 | ~ spl28_64), inference(avatar_split_clause, [], [f6025, f1001, f1022])).
fof(f1022, plain, (spl28_69 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_69])])).
fof(f6025, plain, (~ (e13 = op1(e12, e11)) | ~ spl28_64), inference(forward_demodulation, [], [f357, f1003])).
fof(f357, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f6386, plain, (spl28_44 | ~ spl28_9 | ~ spl28_27), inference(avatar_split_clause, [], [f6371, f846, f770, f917])).
fof(f6371, plain, ((e13 = op1(e13, e11)) | (~ spl28_9 | ~ spl28_27)), inference(backward_demodulation, [], [f4241, f772])).
fof(f772, plain, ((e13 = op1(e14, e13)) | ~ spl28_9), inference(avatar_component_clause, [], [f770])).
fof(f4241, plain, ((e13 = op1(op1(e14, e13), e11)) | ~ spl28_27), inference(forward_demodulation, [], [f177, f848])).
fof(f177, plain, (e13 = op1(op1(e14, e13), op1(e13, e14))), inference(cnf_transformation, [], [f3])).
fof(f6378, plain, (~ spl28_1 | ~ spl28_5), inference(avatar_contradiction_clause, [], [f6377])).
fof(f6377, plain, ($false | (~ spl28_1 | ~ spl28_5)), inference(subsumption_resolution, [], [f6376, f486])).
fof(f486, plain, ~ (e10 = e14), inference(cnf_transformation, [], [f9])).
fof(f9, plain, (~ (e13 = e14) & ~ (e12 = e14) & ~ (e12 = e13) & ~ (e11 = e14) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e14) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax9)).
fof(f6376, plain, ((e10 = e14) | (~ spl28_1 | ~ spl28_5)), inference(forward_demodulation, [], [f755, f739])).
fof(f755, plain, ((e14 = op1(e14, e14)) | ~ spl28_5), inference(avatar_component_clause, [], [f753])).
fof(f753, plain, (spl28_5 <=> (e14 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_5])])).
fof(f6306, plain, (~ spl28_81 | ~ spl28_85), inference(avatar_contradiction_clause, [], [f6305])).
fof(f6305, plain, ($false | (~ spl28_81 | ~ spl28_85)), inference(subsumption_resolution, [], [f6304, f486])).
fof(f6304, plain, ((e10 = e14) | (~ spl28_81 | ~ spl28_85)), inference(backward_demodulation, [], [f1091, f1075])).
fof(f1075, plain, ((e10 = op1(e11, e13)) | ~ spl28_81), inference(avatar_component_clause, [], [f1073])).
fof(f1073, plain, (spl28_81 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_81])])).
fof(f6273, plain, (~ spl28_23 | ~ spl28_98), inference(avatar_split_clause, [], [f6265, f1144, f829])).
fof(f1144, plain, (spl28_98 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_98])])).
fof(f6265, plain, (~ (e12 = op1(e14, e10)) | ~ spl28_98), inference(backward_demodulation, [], [f289, f1146])).
fof(f1146, plain, ((e12 = op1(e11, e10)) | ~ spl28_98), inference(avatar_component_clause, [], [f1144])).
fof(f289, plain, ~ (op1(e11, e10) = op1(e14, e10)), inference(cnf_transformation, [], [f7])).
fof(f6263, plain, (~ spl28_46 | ~ spl28_107 | spl28_116), inference(avatar_contradiction_clause, [], [f6262])).
fof(f6262, plain, ($false | (~ spl28_46 | ~ spl28_107 | spl28_116)), inference(subsumption_resolution, [], [f6258, f1221])).
fof(f1221, plain, (~ (e10 = op1(e10, e11)) | spl28_116), inference(avatar_component_clause, [], [f1220])).
fof(f1220, plain, (spl28_116 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_116])])).
fof(f6258, plain, ((e10 = op1(e10, e11)) | (~ spl28_46 | ~ spl28_107)), inference(backward_demodulation, [], [f5628, f1184])).
fof(f5628, plain, ((e10 = op1(e10, op1(e10, e13))) | ~ spl28_46), inference(forward_demodulation, [], [f161, f928])).
fof(f928, plain, ((e10 = op1(e13, e10)) | ~ spl28_46), inference(avatar_component_clause, [], [f926])).
fof(f926, plain, (spl28_46 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_46])])).
fof(f6212, plain, (~ spl28_157 | ~ spl28_158), inference(avatar_contradiction_clause, [], [f6211])).
fof(f6211, plain, ($false | (~ spl28_157 | ~ spl28_158)), inference(subsumption_resolution, [], [f6210, f497])).
fof(f497, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f10])).
fof(f6210, plain, ((e21 = e22) | (~ spl28_157 | ~ spl28_158)), inference(backward_demodulation, [], [f1448, f1444])).
fof(f1444, plain, ((e21 = op2(e23, e23)) | ~ spl28_157), inference(avatar_component_clause, [], [f1442])).
fof(f1442, plain, (spl28_157 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_157])])).
fof(f6200, plain, (spl28_418 | ~ spl28_185 | ~ spl28_290), inference(avatar_split_clause, [], [f6199, f2228, f1559, f3048])).
fof(f1559, plain, (spl28_185 <=> (e24 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_185])])).
fof(f6199, plain, ((e24 = h3(e14)) | (~ spl28_185 | ~ spl28_290)), inference(backward_demodulation, [], [f5719, f1561])).
fof(f1561, plain, ((e24 = op2(e22, e23)) | ~ spl28_185), inference(avatar_component_clause, [], [f1559])).
fof(f6161, plain, (~ spl28_246 | ~ spl28_250), inference(avatar_contradiction_clause, [], [f6160])).
fof(f6160, plain, ($false | (~ spl28_246 | ~ spl28_250)), inference(subsumption_resolution, [], [f6159, f496])).
fof(f496, plain, ~ (e20 = e24), inference(cnf_transformation, [], [f10])).
fof(f6159, plain, ((e20 = e24) | (~ spl28_246 | ~ spl28_250)), inference(backward_demodulation, [], [f1834, f1818])).
fof(f1818, plain, ((e20 = op2(e20, e20)) | ~ spl28_246), inference(avatar_component_clause, [], [f1816])).
fof(f1816, plain, (spl28_246 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_246])])).
fof(f6131, plain, (spl28_157 | ~ spl28_310), inference(avatar_split_clause, [], [f6125, f2329, f1442])).
fof(f2329, plain, (spl28_310 <=> (e23 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_310])])).
fof(f6125, plain, ((e21 = op2(e23, e23)) | ~ spl28_310), inference(backward_demodulation, [], [f1992, f2330])).
fof(f2330, plain, ((e23 = h2(e12)) | ~ spl28_310), inference(avatar_component_clause, [], [f2329])).
fof(f6095, plain, (~ spl28_185 | ~ spl28_180), inference(avatar_split_clause, [], [f5669, f1538, f1559])).
fof(f5669, plain, (~ (e24 = op2(e22, e23)) | ~ spl28_180), inference(forward_demodulation, [], [f462, f1540])).
fof(f1540, plain, ((e24 = op2(e22, e24)) | ~ spl28_180), inference(avatar_component_clause, [], [f1538])).
fof(f462, plain, ~ (op2(e22, e23) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f6092, plain, (~ spl28_172 | ~ spl28_284), inference(avatar_split_clause, [], [f5303, f2195, f1505])).
fof(f1505, plain, (spl28_172 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_172])])).
fof(f5303, plain, (~ (e21 = op2(e23, e20)) | ~ spl28_284), inference(forward_demodulation, [], [f2039, f2196])).
fof(f2039, plain, ~ (op2(e23, e20) = h4(e11)), inference(backward_demodulation, [], [f466, f2032])).
fof(f466, plain, ~ (op2(e23, e20) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f6091, plain, (~ spl28_173 | ~ spl28_280), inference(avatar_split_clause, [], [f5301, f2173, f1509])).
fof(f1509, plain, (spl28_173 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_173])])).
fof(f5301, plain, (~ (e22 = op2(e23, e20)) | ~ spl28_280), inference(forward_demodulation, [], [f2026, f2174])).
fof(f2026, plain, ~ (op2(e23, e20) = h4(e12)), inference(backward_demodulation, [], [f465, f604])).
fof(f465, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f6063, plain, (~ spl28_138 | ~ spl28_180), inference(avatar_contradiction_clause, [], [f6062])).
fof(f6062, plain, ($false | (~ spl28_138 | ~ spl28_180)), inference(subsumption_resolution, [], [f6061, f501])).
fof(f6061, plain, ((e22 = e24) | (~ spl28_138 | ~ spl28_180)), inference(forward_demodulation, [], [f6060, f1364])).
fof(f6060, plain, ((e24 = op2(e24, e22)) | (~ spl28_138 | ~ spl28_180)), inference(forward_demodulation, [], [f5420, f1364])).
fof(f5420, plain, ((e24 = op2(e24, op2(e24, e22))) | ~ spl28_180), inference(backward_demodulation, [], [f280, f1540])).
fof(f6046, plain, (~ spl28_114 | ~ spl28_64), inference(avatar_split_clause, [], [f6045, f1001, f1211])).
fof(f1211, plain, (spl28_114 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_114])])).
fof(f6045, plain, (~ (e13 = op1(e10, e12)) | ~ spl28_64), inference(forward_demodulation, [], [f304, f1003])).
fof(f304, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f5970, plain, (~ spl28_62 | ~ spl28_64), inference(avatar_contradiction_clause, [], [f5969])).
fof(f5969, plain, ($false | (~ spl28_62 | ~ spl28_64)), inference(subsumption_resolution, [], [f5968, f488])).
fof(f488, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f9])).
fof(f5968, plain, ((e11 = e13) | (~ spl28_62 | ~ spl28_64)), inference(backward_demodulation, [], [f1003, f995])).
fof(f995, plain, ((e11 = op1(e12, e12)) | ~ spl28_62), inference(avatar_component_clause, [], [f993])).
fof(f5966, plain, (~ spl28_59 | ~ spl28_64), inference(avatar_split_clause, [], [f5962, f1001, f980])).
fof(f980, plain, (spl28_59 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_59])])).
fof(f5962, plain, (~ (e13 = op1(e12, e13)) | ~ spl28_64), inference(backward_demodulation, [], [f360, f1003])).
fof(f360, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f5955, plain, (~ spl28_53 | ~ spl28_68), inference(avatar_split_clause, [], [f5950, f1018, f955])).
fof(f1018, plain, (spl28_68 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_68])])).
fof(f5950, plain, (~ (e12 = op1(e12, e14)) | ~ spl28_68), inference(backward_demodulation, [], [f359, f1020])).
fof(f1020, plain, ((e12 = op1(e12, e11)) | ~ spl28_68), inference(avatar_component_clause, [], [f1018])).
fof(f359, plain, ~ (op1(e12, e11) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f5946, plain, (spl28_68 | ~ spl28_72 | ~ spl28_113), inference(avatar_split_clause, [], [f5943, f1207, f1035, f1018])).
fof(f1207, plain, (spl28_113 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_113])])).
fof(f5943, plain, ((e12 = op1(e12, e11)) | (~ spl28_72 | ~ spl28_113)), inference(backward_demodulation, [], [f5066, f1037])).
fof(f5066, plain, ((e12 = op1(e12, op1(e12, e10))) | ~ spl28_113), inference(backward_demodulation, [], [f168, f1209])).
fof(f1209, plain, ((e12 = op1(e10, e12)) | ~ spl28_113), inference(avatar_component_clause, [], [f1207])).
fof(f168, plain, (e12 = op1(op1(e10, e12), op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f5934, plain, (spl28_32 | ~ spl28_44 | ~ spl28_84), inference(avatar_contradiction_clause, [], [f5933])).
fof(f5933, plain, ($false | (spl28_32 | ~ spl28_44 | ~ spl28_84)), inference(subsumption_resolution, [], [f5928, f868])).
fof(f5928, plain, ((e11 = op1(e13, e13)) | (~ spl28_44 | ~ spl28_84)), inference(backward_demodulation, [], [f5595, f1087])).
fof(f5595, plain, ((e11 = op1(e13, op1(e11, e13))) | ~ spl28_44), inference(forward_demodulation, [], [f166, f919])).
fof(f919, plain, ((e13 = op1(e13, e11)) | ~ spl28_44), inference(avatar_component_clause, [], [f917])).
fof(f5886, plain, (~ spl28_46 | ~ spl28_106 | spl28_121), inference(avatar_contradiction_clause, [], [f5885])).
fof(f5885, plain, ($false | (~ spl28_46 | ~ spl28_106 | spl28_121)), inference(subsumption_resolution, [], [f5882, f1242])).
fof(f1242, plain, (~ (e10 = op1(e10, e10)) | spl28_121), inference(avatar_component_clause, [], [f1241])).
fof(f1241, plain, (spl28_121 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_121])])).
fof(f5882, plain, ((e10 = op1(e10, e10)) | (~ spl28_46 | ~ spl28_106)), inference(backward_demodulation, [], [f5628, f1180])).
fof(f1180, plain, ((e10 = op1(e10, e13)) | ~ spl28_106), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl28_106 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_106])])).
fof(f5861, plain, (~ spl28_126 | ~ spl28_130), inference(avatar_contradiction_clause, [], [f5860])).
fof(f5860, plain, ($false | (~ spl28_126 | ~ spl28_130)), inference(subsumption_resolution, [], [f5859, f496])).
fof(f5859, plain, ((e20 = e24) | (~ spl28_126 | ~ spl28_130)), inference(forward_demodulation, [], [f1330, f1314])).
fof(f1314, plain, ((e20 = op2(e24, e24)) | ~ spl28_126), inference(avatar_component_clause, [], [f1312])).
fof(f1312, plain, (spl28_126 <=> (e20 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_126])])).
fof(f1330, plain, ((e24 = op2(e24, e24)) | ~ spl28_130), inference(avatar_component_clause, [], [f1328])).
fof(f1328, plain, (spl28_130 <=> (e24 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_130])])).
fof(f5822, plain, (~ spl28_152 | ~ spl28_154), inference(avatar_contradiction_clause, [], [f5821])).
fof(f5821, plain, ($false | (~ spl28_152 | ~ spl28_154)), inference(subsumption_resolution, [], [f5820, f498])).
fof(f5820, plain, ((e21 = e23) | (~ spl28_152 | ~ spl28_154)), inference(forward_demodulation, [], [f1431, f1423])).
fof(f1431, plain, ((e23 = op2(e23, e24)) | ~ spl28_154), inference(avatar_component_clause, [], [f1429])).
fof(f1429, plain, (spl28_154 <=> (e23 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_154])])).
fof(f5809, plain, (spl28_299 | ~ spl28_182 | ~ spl28_290), inference(avatar_split_clause, [], [f5808, f2228, f1547, f2273])).
fof(f1547, plain, (spl28_182 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_182])])).
fof(f5808, plain, ((e21 = h3(e14)) | (~ spl28_182 | ~ spl28_290)), inference(backward_demodulation, [], [f5719, f1549])).
fof(f1549, plain, ((e21 = op2(e22, e23)) | ~ spl28_182), inference(avatar_component_clause, [], [f1547])).
fof(f5761, plain, (~ spl28_220 | spl28_459), inference(avatar_contradiction_clause, [], [f5760])).
fof(f5760, plain, ($false | (~ spl28_220 | spl28_459)), inference(subsumption_resolution, [], [f5759, f3291])).
fof(f3291, plain, (~ (e24 = h2(e12)) | spl28_459), inference(avatar_component_clause, [], [f3289])).
fof(f3289, plain, (spl28_459 <=> (e24 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_459])])).
fof(f5759, plain, ((e24 = h2(e12)) | ~ spl28_220), inference(backward_demodulation, [], [f594, f1708])).
fof(f1708, plain, ((e24 = op2(e21, e21)) | ~ spl28_220), inference(avatar_component_clause, [], [f1706])).
fof(f1706, plain, (spl28_220 <=> (e24 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_220])])).
fof(f5724, plain, (~ spl28_184 | ~ spl28_290), inference(avatar_split_clause, [], [f5718, f2228, f1555])).
fof(f1555, plain, (spl28_184 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_184])])).
fof(f5718, plain, (~ (e23 = op2(e22, e23)) | ~ spl28_290), inference(backward_demodulation, [], [f2013, f2229])).
fof(f2013, plain, ~ (op2(e22, e23) = h3(e12)), inference(backward_demodulation, [], [f460, f599])).
fof(f460, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f5722, plain, (~ spl28_199 | ~ spl28_290), inference(avatar_split_clause, [], [f5716, f2228, f1618])).
fof(f1618, plain, (spl28_199 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_199])])).
fof(f5716, plain, (~ (e23 = op2(e22, e20)) | ~ spl28_290), inference(backward_demodulation, [], [f2011, f2229])).
fof(f2011, plain, ~ (op2(e22, e20) = h3(e12)), inference(backward_demodulation, [], [f454, f599])).
fof(f454, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f5706, plain, (~ spl28_245 | ~ spl28_488), inference(avatar_split_clause, [], [f5705, f3466, f1811])).
fof(f1811, plain, (spl28_245 <=> (e24 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_245])])).
fof(f5705, plain, (~ (e24 = op2(e20, e21)) | ~ spl28_488), inference(forward_demodulation, [], [f1983, f3467])).
fof(f1983, plain, ~ (op2(e20, e21) = h1(e12)), inference(backward_demodulation, [], [f433, f589])).
fof(f433, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f8])).
fof(f5689, plain, (~ spl28_226 | ~ spl28_276), inference(avatar_split_clause, [], [f5322, f2150, f1732])).
fof(f1732, plain, (spl28_226 <=> (e20 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_226])])).
fof(f5322, plain, (~ (e20 = op2(e20, e24)) | ~ spl28_276), inference(forward_demodulation, [], [f2046, f2151])).
fof(f2046, plain, ~ (op2(e20, e24) = h4(e10)), inference(backward_demodulation, [], [f426, f2044])).
fof(f426, plain, ~ (op2(e20, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5688, plain, (~ spl28_212 | ~ spl28_137), inference(avatar_split_clause, [], [f5687, f1358, f1673])).
fof(f1673, plain, (spl28_212 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_212])])).
fof(f5687, plain, (~ (e21 = op2(e21, e22)) | ~ spl28_137), inference(forward_demodulation, [], [f409, f1360])).
fof(f409, plain, ~ (op2(e21, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f5645, plain, (~ spl28_168 | ~ spl28_280), inference(avatar_split_clause, [], [f5644, f2173, f1488])).
fof(f1488, plain, (spl28_168 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_168])])).
fof(f5644, plain, (~ (e22 = op2(e23, e21)) | ~ spl28_280), inference(forward_demodulation, [], [f2027, f2174])).
fof(f2027, plain, ~ (op2(e23, e21) = h4(e12)), inference(backward_demodulation, [], [f468, f604])).
fof(f468, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f5639, plain, (~ spl28_146 | ~ spl28_276), inference(avatar_split_clause, [], [f4843, f2150, f1396])).
fof(f1396, plain, (spl28_146 <=> (e20 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_146])])).
fof(f4843, plain, (~ (e20 = op2(e24, e20)) | ~ spl28_276), inference(forward_demodulation, [], [f2049, f2151])).
fof(f2049, plain, ~ (op2(e24, e20) = h4(e10)), inference(backward_demodulation, [], [f476, f2044])).
fof(f476, plain, ~ (op2(e24, e20) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5623, plain, (~ spl28_102 | ~ spl28_27), inference(avatar_split_clause, [], [f5622, f846, f1161])).
fof(f5622, plain, (~ (e11 = op1(e10, e14)) | ~ spl28_27), inference(forward_demodulation, [], [f325, f848])).
fof(f325, plain, ~ (op1(e10, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f5620, plain, (~ spl28_105 | ~ spl28_125), inference(avatar_split_clause, [], [f5619, f1257, f1173])).
fof(f1173, plain, (spl28_105 <=> (e14 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_105])])).
fof(f5619, plain, (~ (e14 = op1(e10, e14)) | ~ spl28_125), inference(forward_demodulation, [], [f336, f1259])).
fof(f336, plain, ~ (op1(e10, e10) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f5615, plain, (~ spl28_100 | ~ spl28_125), inference(avatar_split_clause, [], [f5614, f1257, f1152])).
fof(f1152, plain, (spl28_100 <=> (e14 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_100])])).
fof(f5614, plain, (~ (e14 = op1(e11, e10)) | ~ spl28_125), inference(forward_demodulation, [], [f283, f1259])).
fof(f283, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f7])).
fof(f5600, plain, (~ spl28_1 | ~ spl28_95), inference(avatar_contradiction_clause, [], [f5599])).
fof(f5599, plain, ($false | (~ spl28_1 | ~ spl28_95)), inference(subsumption_resolution, [], [f5598, f483])).
fof(f483, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f9])).
fof(f5598, plain, ((e10 = e11) | (~ spl28_1 | ~ spl28_95)), inference(forward_demodulation, [], [f5597, f739])).
fof(f5597, plain, ((e11 = op1(e14, e14)) | ~ spl28_95), inference(forward_demodulation, [], [f164, f1133])).
fof(f1133, plain, ((e14 = op1(e11, e11)) | ~ spl28_95), inference(avatar_component_clause, [], [f1131])).
fof(f1131, plain, (spl28_95 <=> (e14 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_95])])).
fof(f5575, plain, (~ spl28_27 | ~ spl28_29), inference(avatar_contradiction_clause, [], [f5574])).
fof(f5574, plain, ($false | (~ spl28_27 | ~ spl28_29)), inference(subsumption_resolution, [], [f5573, f488])).
fof(f5573, plain, ((e11 = e13) | (~ spl28_27 | ~ spl28_29)), inference(forward_demodulation, [], [f856, f848])).
fof(f856, plain, ((e13 = op1(e13, e14)) | ~ spl28_29), inference(avatar_component_clause, [], [f854])).
fof(f854, plain, (spl28_29 <=> (e13 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_29])])).
fof(f5560, plain, (~ spl28_46 | ~ spl28_109), inference(avatar_contradiction_clause, [], [f5559])).
fof(f5559, plain, ($false | (~ spl28_46 | ~ spl28_109)), inference(subsumption_resolution, [], [f5558, f485])).
fof(f485, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f9])).
fof(f5558, plain, ((e10 = e13) | (~ spl28_46 | ~ spl28_109)), inference(forward_demodulation, [], [f5555, f928])).
fof(f5555, plain, ((e13 = op1(e13, e10)) | (~ spl28_46 | ~ spl28_109)), inference(backward_demodulation, [], [f4633, f928])).
fof(f4633, plain, ((e13 = op1(e13, op1(e13, e10))) | ~ spl28_109), inference(backward_demodulation, [], [f173, f1192])).
fof(f1192, plain, ((e13 = op1(e10, e13)) | ~ spl28_109), inference(avatar_component_clause, [], [f1190])).
fof(f1190, plain, (spl28_109 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_109])])).
fof(f5547, plain, (~ spl28_12 | ~ spl28_53 | spl28_88), inference(avatar_contradiction_clause, [], [f5546])).
fof(f5546, plain, ($false | (~ spl28_12 | ~ spl28_53 | spl28_88)), inference(subsumption_resolution, [], [f5542, f1103])).
fof(f1103, plain, (~ (e12 = op1(e11, e12)) | spl28_88), inference(avatar_component_clause, [], [f1102])).
fof(f5542, plain, ((e12 = op1(e11, e12)) | (~ spl28_12 | ~ spl28_53)), inference(backward_demodulation, [], [f5208, f957])).
fof(f5208, plain, ((e12 = op1(e11, op1(e12, e14))) | ~ spl28_12), inference(forward_demodulation, [], [f172, f785])).
fof(f172, plain, (e12 = op1(op1(e14, e12), op1(e12, e14))), inference(cnf_transformation, [], [f3])).
fof(f5538, plain, (~ spl28_56 | ~ spl28_57), inference(avatar_contradiction_clause, [], [f5537])).
fof(f5537, plain, ($false | (~ spl28_56 | ~ spl28_57)), inference(subsumption_resolution, [], [f5536, f483])).
fof(f5536, plain, ((e10 = e11) | (~ spl28_56 | ~ spl28_57)), inference(backward_demodulation, [], [f974, f970])).
fof(f974, plain, ((e11 = op1(e12, e13)) | ~ spl28_57), inference(avatar_component_clause, [], [f972])).
fof(f972, plain, (spl28_57 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_57])])).
fof(f5480, plain, (~ spl28_126 | ~ spl28_129), inference(avatar_contradiction_clause, [], [f5479])).
fof(f5479, plain, ($false | (~ spl28_126 | ~ spl28_129)), inference(subsumption_resolution, [], [f5478, f495])).
fof(f5478, plain, ((e20 = e23) | (~ spl28_126 | ~ spl28_129)), inference(forward_demodulation, [], [f1326, f1314])).
fof(f1326, plain, ((e23 = op2(e24, e24)) | ~ spl28_129), inference(avatar_component_clause, [], [f1324])).
fof(f1324, plain, (spl28_129 <=> (e23 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_129])])).
fof(f5476, plain, (~ spl28_135 | ~ spl28_204 | ~ spl28_284), inference(avatar_contradiction_clause, [], [f5475])).
fof(f5475, plain, ($false | (~ spl28_135 | ~ spl28_204 | ~ spl28_284)), inference(subsumption_resolution, [], [f5474, f502])).
fof(f5474, plain, ((e23 = e24) | (~ spl28_135 | ~ spl28_204 | ~ spl28_284)), inference(forward_demodulation, [], [f5469, f1641])).
fof(f1641, plain, ((e23 = op2(e21, e24)) | ~ spl28_204), inference(avatar_component_clause, [], [f1639])).
fof(f5469, plain, ((e24 = op2(e21, e24)) | (~ spl28_135 | ~ spl28_284)), inference(backward_demodulation, [], [f5278, f1351])).
fof(f5473, plain, (~ spl28_135 | ~ spl28_143 | ~ spl28_284), inference(avatar_contradiction_clause, [], [f5472])).
fof(f5472, plain, ($false | (~ spl28_135 | ~ spl28_143 | ~ spl28_284)), inference(subsumption_resolution, [], [f5471, f500])).
fof(f5471, plain, ((e22 = e23) | (~ spl28_135 | ~ spl28_143 | ~ spl28_284)), inference(forward_demodulation, [], [f5468, f1385])).
fof(f1385, plain, ((e22 = op2(e24, e21)) | ~ spl28_143), inference(avatar_component_clause, [], [f1383])).
fof(f1383, plain, (spl28_143 <=> (e22 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_143])])).
fof(f5468, plain, ((e23 = op2(e24, e21)) | (~ spl28_135 | ~ spl28_284)), inference(backward_demodulation, [], [f5277, f1351])).
fof(f5438, plain, (spl28_265 | ~ spl28_148 | ~ spl28_276), inference(avatar_split_clause, [], [f5437, f2150, f1404, f2091])).
fof(f1404, plain, (spl28_148 <=> (e22 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_148])])).
fof(f5437, plain, ((e22 = h5(e14)) | (~ spl28_148 | ~ spl28_276)), inference(backward_demodulation, [], [f4494, f1406])).
fof(f1406, plain, ((e22 = op2(e24, e20)) | ~ spl28_148), inference(avatar_component_clause, [], [f1404])).
fof(f5418, plain, (spl28_148 | ~ spl28_165 | ~ spl28_181), inference(avatar_split_clause, [], [f5413, f1543, f1475, f1404])).
fof(f5413, plain, ((e22 = op2(e24, e20)) | (~ spl28_165 | ~ spl28_181)), inference(backward_demodulation, [], [f5310, f1545])).
fof(f5310, plain, ((e22 = op2(e24, op2(e22, e23))) | ~ spl28_165), inference(forward_demodulation, [], [f271, f1477])).
fof(f271, plain, (e22 = op2(op2(e23, e22), op2(e22, e23))), inference(cnf_transformation, [], [f6])).
fof(f5406, plain, (spl28_290 | ~ spl28_189), inference(avatar_split_clause, [], [f5405, f1576, f2228])).
fof(f5405, plain, ((e23 = h3(e12)) | ~ spl28_189), inference(backward_demodulation, [], [f599, f1578])).
fof(f5388, plain, (~ spl28_207 | ~ spl28_210), inference(avatar_contradiction_clause, [], [f5387])).
fof(f5387, plain, ($false | (~ spl28_207 | ~ spl28_210)), inference(subsumption_resolution, [], [f5386, f499])).
fof(f499, plain, ~ (e21 = e24), inference(cnf_transformation, [], [f10])).
fof(f5386, plain, ((e21 = e24) | (~ spl28_207 | ~ spl28_210)), inference(backward_demodulation, [], [f1666, f1654])).
fof(f5368, plain, (~ spl28_219 | spl28_310), inference(avatar_contradiction_clause, [], [f5367])).
fof(f5367, plain, ($false | (~ spl28_219 | spl28_310)), inference(subsumption_resolution, [], [f5366, f2331])).
fof(f2331, plain, (~ (e23 = h2(e12)) | spl28_310), inference(avatar_component_clause, [], [f2329])).
fof(f5366, plain, ((e23 = h2(e12)) | ~ spl28_219), inference(backward_demodulation, [], [f594, f1704])).
fof(f1704, plain, ((e23 = op2(e21, e21)) | ~ spl28_219), inference(avatar_component_clause, [], [f1702])).
fof(f1702, plain, (spl28_219 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_219])])).
fof(f5340, plain, (~ spl28_230 | ~ spl28_488), inference(avatar_split_clause, [], [f5336, f3466, f1748])).
fof(f1748, plain, (spl28_230 <=> (e24 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_230])])).
fof(f5336, plain, (~ (e24 = op2(e20, e24)) | ~ spl28_488), inference(backward_demodulation, [], [f1986, f3467])).
fof(f1986, plain, ~ (op2(e20, e24) = h1(e12)), inference(backward_demodulation, [], [f436, f589])).
fof(f436, plain, ~ (op2(e20, e20) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f5339, plain, (~ spl28_200 | ~ spl28_488), inference(avatar_split_clause, [], [f5334, f3466, f1622])).
fof(f1622, plain, (spl28_200 <=> (e24 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_200])])).
fof(f5334, plain, (~ (e24 = op2(e22, e20)) | ~ spl28_488), inference(backward_demodulation, [], [f1980, f3467])).
fof(f1980, plain, ~ (op2(e22, e20) = h1(e12)), inference(backward_demodulation, [], [f384, f589])).
fof(f384, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f8])).
fof(f5338, plain, (~ spl28_225 | ~ spl28_488), inference(avatar_split_clause, [], [f5333, f3466, f1727])).
fof(f1727, plain, (spl28_225 <=> (e24 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_225])])).
fof(f5333, plain, (~ (e24 = op2(e21, e20)) | ~ spl28_488), inference(backward_demodulation, [], [f1979, f3467])).
fof(f1979, plain, ~ (op2(e21, e20) = h1(e12)), inference(backward_demodulation, [], [f383, f589])).
fof(f383, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f8])).
fof(f5331, plain, (spl28_488 | ~ spl28_250), inference(avatar_split_clause, [], [f4988, f1832, f3466])).
fof(f4988, plain, ((e24 = h1(e12)) | ~ spl28_250), inference(backward_demodulation, [], [f589, f1834])).
fof(f5316, plain, (~ spl28_185 | ~ spl28_210), inference(avatar_split_clause, [], [f5315, f1664, f1559])).
fof(f5315, plain, (~ (e24 = op2(e22, e23)) | ~ spl28_210), inference(forward_demodulation, [], [f417, f1666])).
fof(f417, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f5312, plain, (~ spl28_183 | ~ spl28_280), inference(avatar_split_clause, [], [f5311, f2173, f1551])).
fof(f1551, plain, (spl28_183 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_183])])).
fof(f5311, plain, (~ (e22 = op2(e22, e23)) | ~ spl28_280), inference(forward_demodulation, [], [f2024, f2174])).
fof(f2024, plain, ~ (op2(e22, e23) = h4(e12)), inference(backward_demodulation, [], [f420, f604])).
fof(f420, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f5280, plain, (~ spl28_136 | ~ spl28_276), inference(avatar_split_clause, [], [f4831, f2150, f1354])).
fof(f1354, plain, (spl28_136 <=> (e20 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_136])])).
fof(f4831, plain, (~ (e20 = op2(e24, e22)) | ~ spl28_276), inference(forward_demodulation, [], [f2051, f2151])).
fof(f2051, plain, ~ (op2(e24, e22) = h4(e10)), inference(backward_demodulation, [], [f481, f2044])).
fof(f481, plain, ~ (op2(e24, e22) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5276, plain, (~ spl28_133 | ~ spl28_280), inference(avatar_split_clause, [], [f5275, f2173, f1341])).
fof(f1341, plain, (spl28_133 <=> (e22 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_133])])).
fof(f5275, plain, (~ (e22 = op2(e24, e23)) | ~ spl28_280), inference(forward_demodulation, [], [f2025, f2174])).
fof(f2025, plain, ~ (op2(e24, e23) = h4(e12)), inference(backward_demodulation, [], [f422, f604])).
fof(f422, plain, ~ (op2(e23, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f5270, plain, (~ spl28_75 | ~ spl28_125), inference(avatar_split_clause, [], [f5269, f1257, f1047])).
fof(f1047, plain, (spl28_75 <=> (e14 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_75])])).
fof(f5269, plain, (~ (e14 = op1(e12, e10)) | ~ spl28_125), inference(forward_demodulation, [], [f284, f1259])).
fof(f284, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f5253, plain, (spl28_122 | ~ spl28_91), inference(avatar_split_clause, [], [f5252, f1115, f1245])).
fof(f1245, plain, (spl28_122 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl28_122])])).
fof(f1115, plain, (spl28_91 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_91])])).
fof(f5252, plain, ((op1(e10, e10) = e11) | ~ spl28_91), inference(forward_demodulation, [], [f164, f1117])).
fof(f1117, plain, ((e10 = op1(e11, e11)) | ~ spl28_91), inference(avatar_component_clause, [], [f1115])).
fof(f5232, plain, (spl28_79 | ~ spl28_40 | ~ spl28_57), inference(avatar_split_clause, [], [f5121, f972, f900, f1064])).
fof(f1064, plain, (spl28_79 <=> (e13 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_79])])).
fof(f5121, plain, ((e13 = op1(e11, e14)) | (~ spl28_40 | ~ spl28_57)), inference(backward_demodulation, [], [f4761, f974])).
fof(f5220, plain, (spl28_64 | ~ spl28_33), inference(avatar_split_clause, [], [f4250, f871, f1001])).
fof(f4250, plain, ((e13 = op1(e12, e12)) | ~ spl28_33), inference(forward_demodulation, [], [f176, f873])).
fof(f176, plain, (e13 = op1(op1(e13, e13), op1(e13, e13))), inference(cnf_transformation, [], [f3])).
fof(f5213, plain, (~ spl28_1 | ~ spl28_65), inference(avatar_contradiction_clause, [], [f5212])).
fof(f5212, plain, ($false | (~ spl28_1 | ~ spl28_65)), inference(subsumption_resolution, [], [f5211, f484])).
fof(f484, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f9])).
fof(f5211, plain, ((e10 = e12) | (~ spl28_1 | ~ spl28_65)), inference(forward_demodulation, [], [f5210, f739])).
fof(f5210, plain, ((e12 = op1(e14, e14)) | ~ spl28_65), inference(forward_demodulation, [], [f170, f1007])).
fof(f1007, plain, ((e14 = op1(e12, e12)) | ~ spl28_65), inference(avatar_component_clause, [], [f1005])).
fof(f1005, plain, (spl28_65 <=> (e14 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_65])])).
fof(f170, plain, (e12 = op1(op1(e12, e12), op1(e12, e12))), inference(cnf_transformation, [], [f3])).
fof(f5200, plain, (~ spl28_10 | ~ spl28_27 | ~ spl28_79), inference(avatar_contradiction_clause, [], [f5199])).
fof(f5199, plain, ($false | (~ spl28_10 | ~ spl28_27 | ~ spl28_79)), inference(subsumption_resolution, [], [f5198, f492])).
fof(f492, plain, ~ (e13 = e14), inference(cnf_transformation, [], [f9])).
fof(f5198, plain, ((e13 = e14) | (~ spl28_10 | ~ spl28_27 | ~ spl28_79)), inference(forward_demodulation, [], [f5197, f1066])).
fof(f1066, plain, ((e13 = op1(e11, e14)) | ~ spl28_79), inference(avatar_component_clause, [], [f1064])).
fof(f5197, plain, ((e14 = op1(e11, e14)) | (~ spl28_10 | ~ spl28_27)), inference(backward_demodulation, [], [f4744, f776])).
fof(f4744, plain, ((e14 = op1(e11, op1(e14, e13))) | ~ spl28_27), inference(backward_demodulation, [], [f181, f848])).
fof(f181, plain, (e14 = op1(op1(e13, e14), op1(e14, e13))), inference(cnf_transformation, [], [f3])).
fof(f5195, plain, (~ spl28_12 | ~ spl28_54 | spl28_83), inference(avatar_contradiction_clause, [], [f5194])).
fof(f5194, plain, ($false | (~ spl28_12 | ~ spl28_54 | spl28_83)), inference(subsumption_resolution, [], [f5191, f1082])).
fof(f1082, plain, (~ (e12 = op1(e11, e13)) | spl28_83), inference(avatar_component_clause, [], [f1081])).
fof(f1081, plain, (spl28_83 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_83])])).
fof(f5191, plain, ((e12 = op1(e11, e13)) | (~ spl28_12 | ~ spl28_54)), inference(backward_demodulation, [], [f4750, f785])).
fof(f4750, plain, ((e12 = op1(op1(e14, e12), e13)) | ~ spl28_54), inference(backward_demodulation, [], [f172, f961])).
fof(f961, plain, ((e13 = op1(e12, e14)) | ~ spl28_54), inference(avatar_component_clause, [], [f959])).
fof(f959, plain, (spl28_54 <=> (e13 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_54])])).
fof(f5158, plain, (~ spl28_31 | ~ spl28_33), inference(avatar_contradiction_clause, [], [f5157])).
fof(f5157, plain, ($false | (~ spl28_31 | ~ spl28_33)), inference(subsumption_resolution, [], [f5156, f484])).
fof(f5156, plain, ((e10 = e12) | (~ spl28_31 | ~ spl28_33)), inference(backward_demodulation, [], [f873, f865])).
fof(f865, plain, ((e10 = op1(e13, e13)) | ~ spl28_31), inference(avatar_component_clause, [], [f863])).
fof(f863, plain, (spl28_31 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_31])])).
fof(f5149, plain, (~ spl28_39 | ~ spl28_40), inference(avatar_contradiction_clause, [], [f5148])).
fof(f5148, plain, ($false | (~ spl28_39 | ~ spl28_40)), inference(subsumption_resolution, [], [f5147, f492])).
fof(f5147, plain, ((e13 = e14) | (~ spl28_39 | ~ spl28_40)), inference(backward_demodulation, [], [f902, f898])).
fof(f898, plain, ((e13 = op1(e13, e12)) | ~ spl28_39), inference(avatar_component_clause, [], [f896])).
fof(f896, plain, (spl28_39 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_39])])).
fof(f5055, plain, (~ spl28_122 | ~ spl28_125), inference(avatar_contradiction_clause, [], [f5054])).
fof(f5054, plain, ($false | (~ spl28_122 | ~ spl28_125)), inference(subsumption_resolution, [], [f5053, f489])).
fof(f489, plain, ~ (e11 = e14), inference(cnf_transformation, [], [f9])).
fof(f5053, plain, ((e11 = e14) | (~ spl28_122 | ~ spl28_125)), inference(forward_demodulation, [], [f1259, f1247])).
fof(f1247, plain, ((op1(e10, e10) = e11) | ~ spl28_122), inference(avatar_component_clause, [], [f1245])).
fof(f5052, plain, (~ spl28_126 | ~ spl28_128), inference(avatar_contradiction_clause, [], [f5051])).
fof(f5051, plain, ($false | (~ spl28_126 | ~ spl28_128)), inference(subsumption_resolution, [], [f5050, f494])).
fof(f494, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f10])).
fof(f5050, plain, ((e20 = e22) | (~ spl28_126 | ~ spl28_128)), inference(forward_demodulation, [], [f1322, f1314])).
fof(f1322, plain, ((e22 = op2(e24, e24)) | ~ spl28_128), inference(avatar_component_clause, [], [f1320])).
fof(f1320, plain, (spl28_128 <=> (e22 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_128])])).
fof(f5045, plain, (~ spl28_164 | ~ spl28_165), inference(avatar_contradiction_clause, [], [f5044])).
fof(f5044, plain, ($false | (~ spl28_164 | ~ spl28_165)), inference(subsumption_resolution, [], [f5043, f502])).
fof(f5043, plain, ((e23 = e24) | (~ spl28_164 | ~ spl28_165)), inference(backward_demodulation, [], [f1477, f1473])).
fof(f1473, plain, ((e23 = op2(e23, e22)) | ~ spl28_164), inference(avatar_component_clause, [], [f1471])).
fof(f1471, plain, (spl28_164 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_164])])).
fof(f5007, plain, (spl28_315 | ~ spl28_218), inference(avatar_split_clause, [], [f5006, f1698, f2354])).
fof(f1698, plain, (spl28_218 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_218])])).
fof(f5006, plain, ((e22 = h2(e12)) | ~ spl28_218), inference(backward_demodulation, [], [f594, f1700])).
fof(f1700, plain, ((e22 = op2(e21, e21)) | ~ spl28_218), inference(avatar_component_clause, [], [f1698])).
fof(f4986, plain, (spl28_189 | ~ spl28_280), inference(avatar_split_clause, [], [f4982, f2173, f1576])).
fof(f4982, plain, ((e23 = op2(e22, e22)) | ~ spl28_280), inference(backward_demodulation, [], [f2021, f2174])).
fof(f2021, plain, (e23 = op2(h4(e12), h4(e12))), inference(backward_demodulation, [], [f276, f604])).
fof(f276, plain, (e23 = op2(op2(e23, e23), op2(e23, e23))), inference(cnf_transformation, [], [f6])).
fof(f4980, plain, (spl28_219 | ~ spl28_132 | ~ spl28_284), inference(avatar_split_clause, [], [f4976, f2195, f1337, f1702])).
fof(f1337, plain, (spl28_132 <=> (e21 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_132])])).
fof(f4976, plain, ((e23 = op2(e21, e21)) | (~ spl28_132 | ~ spl28_284)), inference(backward_demodulation, [], [f4368, f2196])).
fof(f4368, plain, ((e23 = op2(e21, h4(e11))) | ~ spl28_132), inference(forward_demodulation, [], [f2033, f1339])).
fof(f1339, plain, ((e21 = op2(e24, e23)) | ~ spl28_132), inference(avatar_component_clause, [], [f1337])).
fof(f4952, plain, (~ spl28_145 | ~ spl28_152 | ~ spl28_204), inference(avatar_contradiction_clause, [], [f4951])).
fof(f4951, plain, ($false | (~ spl28_145 | ~ spl28_152 | ~ spl28_204)), inference(subsumption_resolution, [], [f4950, f499])).
fof(f4950, plain, ((e21 = e24) | (~ spl28_145 | ~ spl28_152 | ~ spl28_204)), inference(forward_demodulation, [], [f4945, f1423])).
fof(f4945, plain, ((e24 = op2(e23, e24)) | (~ spl28_145 | ~ spl28_204)), inference(backward_demodulation, [], [f4836, f1641])).
fof(f4836, plain, ((e24 = op2(op2(e21, e24), e24)) | ~ spl28_145), inference(backward_demodulation, [], [f279, f1393])).
fof(f1393, plain, ((e24 = op2(e24, e21)) | ~ spl28_145), inference(avatar_component_clause, [], [f1391])).
fof(f1391, plain, (spl28_145 <=> (e24 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_145])])).
fof(f279, plain, (e24 = op2(op2(e21, e24), op2(e24, e21))), inference(cnf_transformation, [], [f6])).
fof(f4903, plain, (~ spl28_290 | ~ spl28_179), inference(avatar_split_clause, [], [f4897, f1534, f2228])).
fof(f1534, plain, (spl28_179 <=> (e23 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_179])])).
fof(f4897, plain, (~ (e23 = h3(e12)) | ~ spl28_179), inference(backward_demodulation, [], [f2014, f1536])).
fof(f1536, plain, ((e23 = op2(e22, e24)) | ~ spl28_179), inference(avatar_component_clause, [], [f1534])).
fof(f2014, plain, ~ (op2(e22, e24) = h3(e12)), inference(backward_demodulation, [], [f461, f599])).
fof(f461, plain, ~ (op2(e22, e22) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f4893, plain, (~ spl28_176 | ~ spl28_276), inference(avatar_split_clause, [], [f4892, f2150, f1522])).
fof(f1522, plain, (spl28_176 <=> (e20 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_176])])).
fof(f4892, plain, (~ (e20 = op2(e22, e24)) | ~ spl28_276), inference(forward_demodulation, [], [f2048, f2151])).
fof(f2048, plain, ~ (op2(e22, e24) = h4(e10)), inference(backward_demodulation, [], [f431, f2044])).
fof(f431, plain, ~ (op2(e22, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f4856, plain, (~ spl28_215 | ~ spl28_165), inference(avatar_split_clause, [], [f4855, f1475, f1685])).
fof(f1685, plain, (spl28_215 <=> (e24 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_215])])).
fof(f4855, plain, (~ (e24 = op2(e21, e22)) | ~ spl28_165), inference(forward_demodulation, [], [f408, f1477])).
fof(f408, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4824, plain, (~ spl28_117 | ~ spl28_92), inference(avatar_split_clause, [], [f4823, f1119, f1224])).
fof(f1224, plain, (spl28_117 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_117])])).
fof(f4823, plain, (~ (e11 = op1(e10, e11)) | ~ spl28_92), inference(forward_demodulation, [], [f293, f1121])).
fof(f293, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f7])).
fof(f4815, plain, (~ spl28_115 | ~ spl28_40), inference(avatar_split_clause, [], [f4814, f900, f1215])).
fof(f1215, plain, (spl28_115 <=> (e14 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_115])])).
fof(f4814, plain, (~ (e14 = op1(e10, e12)) | ~ spl28_40), inference(forward_demodulation, [], [f305, f902])).
fof(f305, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4807, plain, (~ spl28_97 | ~ spl28_92), inference(avatar_split_clause, [], [f4806, f1119, f1140])).
fof(f1140, plain, (spl28_97 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_97])])).
fof(f4806, plain, (~ (e11 = op1(e11, e10)) | ~ spl28_92), inference(forward_demodulation, [], [f343, f1121])).
fof(f343, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f7])).
fof(f4799, plain, (~ spl28_90 | ~ spl28_40), inference(avatar_split_clause, [], [f4798, f900, f1110])).
fof(f1110, plain, (spl28_90 <=> (e14 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_90])])).
fof(f4798, plain, (~ (e14 = op1(e11, e12)) | ~ spl28_40), inference(forward_demodulation, [], [f308, f902])).
fof(f308, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4749, plain, (~ spl28_52 | ~ spl28_27), inference(avatar_split_clause, [], [f4277, f846, f951])).
fof(f951, plain, (spl28_52 <=> (e11 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_52])])).
fof(f4277, plain, (~ (e11 = op1(e12, e14)) | ~ spl28_27), inference(forward_demodulation, [], [f330, f848])).
fof(f330, plain, ~ (op1(e12, e14) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4748, plain, (~ spl28_50 | ~ spl28_40), inference(avatar_split_clause, [], [f4747, f900, f942])).
fof(f942, plain, (spl28_50 <=> (e14 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_50])])).
fof(f4747, plain, (~ (e14 = op1(e13, e10)) | ~ spl28_40), inference(forward_demodulation, [], [f364, f902])).
fof(f364, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f4738, plain, (~ spl28_8 | ~ spl28_33), inference(avatar_split_clause, [], [f4737, f871, f766])).
fof(f766, plain, (spl28_8 <=> (e12 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_8])])).
fof(f4737, plain, (~ (e12 = op1(e14, e13)) | ~ spl28_33), inference(forward_demodulation, [], [f322, f873])).
fof(f322, plain, ~ (op1(e13, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f4721, plain, (~ spl28_1 | ~ spl28_40 | ~ spl28_60), inference(avatar_contradiction_clause, [], [f4720])).
fof(f4720, plain, ($false | (~ spl28_1 | ~ spl28_40 | ~ spl28_60)), inference(subsumption_resolution, [], [f4719, f484])).
fof(f4719, plain, ((e10 = e12) | (~ spl28_1 | ~ spl28_40 | ~ spl28_60)), inference(forward_demodulation, [], [f4715, f739])).
fof(f4715, plain, ((e12 = op1(e14, e14)) | (~ spl28_40 | ~ spl28_60)), inference(backward_demodulation, [], [f4688, f902])).
fof(f4688, plain, ((e12 = op1(op1(e13, e12), e14)) | ~ spl28_60), inference(backward_demodulation, [], [f171, f986])).
fof(f986, plain, ((e14 = op1(e12, e13)) | ~ spl28_60), inference(avatar_component_clause, [], [f984])).
fof(f984, plain, (spl28_60 <=> (e14 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_60])])).
fof(f4672, plain, (~ spl28_72 | spl28_91 | ~ spl28_112), inference(avatar_contradiction_clause, [], [f4671])).
fof(f4671, plain, ($false | (~ spl28_72 | spl28_91 | ~ spl28_112)), inference(subsumption_resolution, [], [f4664, f1116])).
fof(f1116, plain, (~ (e10 = op1(e11, e11)) | spl28_91), inference(avatar_component_clause, [], [f1115])).
fof(f4664, plain, ((e10 = op1(e11, e11)) | (~ spl28_72 | ~ spl28_112)), inference(backward_demodulation, [], [f4624, f1037])).
fof(f4624, plain, ((e10 = op1(op1(e12, e10), e11)) | ~ spl28_112), inference(backward_demodulation, [], [f160, f1205])).
fof(f1205, plain, ((e11 = op1(e10, e12)) | ~ spl28_112), inference(avatar_component_clause, [], [f1203])).
fof(f1203, plain, (spl28_112 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_112])])).
fof(f160, plain, (e10 = op1(op1(e12, e10), op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f4647, plain, (~ spl28_82 | ~ spl28_92), inference(avatar_split_clause, [], [f4643, f1119, f1077])).
fof(f1077, plain, (spl28_82 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_82])])).
fof(f4643, plain, (~ (e11 = op1(e11, e13)) | ~ spl28_92), inference(backward_demodulation, [], [f348, f1121])).
fof(f348, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f7])).
fof(f4646, plain, (~ spl28_87 | ~ spl28_92), inference(avatar_split_clause, [], [f4642, f1119, f1098])).
fof(f1098, plain, (spl28_87 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_87])])).
fof(f4642, plain, (~ (e11 = op1(e11, e12)) | ~ spl28_92), inference(backward_demodulation, [], [f347, f1121])).
fof(f347, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f4644, plain, (~ spl28_67 | ~ spl28_92), inference(avatar_split_clause, [], [f4640, f1119, f1014])).
fof(f1014, plain, (spl28_67 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl28_67])])).
fof(f4640, plain, (~ (e11 = op1(e12, e11)) | ~ spl28_92), inference(backward_demodulation, [], [f297, f1121])).
fof(f297, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f4498, plain, (spl28_250 | ~ spl28_276), inference(avatar_split_clause, [], [f4492, f2150, f1832])).
fof(f4492, plain, ((op2(e20, e20) = e24) | ~ spl28_276), inference(backward_demodulation, [], [f2045, f2151])).
fof(f2045, plain, (e24 = op2(h4(e10), h4(e10))), inference(backward_demodulation, [], [f282, f2044])).
fof(f282, plain, (e24 = op2(op2(e24, e24), op2(e24, e24))), inference(cnf_transformation, [], [f6])).
fof(f4482, plain, (~ spl28_231 | ~ spl28_181), inference(avatar_split_clause, [], [f4481, f1543, f1753])).
fof(f1753, plain, (spl28_231 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_231])])).
fof(f4481, plain, (~ (e20 = op2(e20, e23)) | ~ spl28_181), inference(forward_demodulation, [], [f414, f1545])).
fof(f414, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f4448, plain, (~ spl28_284 | ~ spl28_202), inference(avatar_split_clause, [], [f4447, f1631, f2195])).
fof(f1631, plain, (spl28_202 <=> (e21 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_202])])).
fof(f4447, plain, (~ (e21 = h4(e11)) | ~ spl28_202), inference(forward_demodulation, [], [f2036, f1633])).
fof(f1633, plain, ((e21 = op2(e21, e24)) | ~ spl28_202), inference(avatar_component_clause, [], [f1631])).
fof(f2036, plain, ~ (op2(e21, e24) = h4(e11)), inference(backward_demodulation, [], [f428, f2032])).
fof(f428, plain, ~ (op2(e21, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4439, plain, (~ spl28_196 | ~ spl28_181), inference(avatar_split_clause, [], [f4438, f1543, f1606])).
fof(f1606, plain, (spl28_196 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_196])])).
fof(f4438, plain, (~ (e20 = op2(e22, e20)) | ~ spl28_181), inference(forward_demodulation, [], [f455, f1545])).
fof(f455, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f4416, plain, (~ spl28_284 | ~ spl28_177), inference(avatar_split_clause, [], [f4415, f1526, f2195])).
fof(f1526, plain, (spl28_177 <=> (e21 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_177])])).
fof(f4415, plain, (~ (e21 = h4(e11)) | ~ spl28_177), inference(forward_demodulation, [], [f2037, f1528])).
fof(f1528, plain, ((e21 = op2(e22, e24)) | ~ spl28_177), inference(avatar_component_clause, [], [f1526])).
fof(f2037, plain, ~ (op2(e22, e24) = h4(e11)), inference(backward_demodulation, [], [f430, f2032])).
fof(f430, plain, ~ (op2(e22, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f4403, plain, (spl28_280 | ~ spl28_158), inference(avatar_split_clause, [], [f4402, f1446, f2173])).
fof(f4402, plain, ((e22 = h4(e12)) | ~ spl28_158), inference(forward_demodulation, [], [f604, f1448])).
fof(f4399, plain, (spl28_284 | ~ spl28_152), inference(avatar_split_clause, [], [f4398, f1421, f2195])).
fof(f4398, plain, ((e21 = h4(e11)) | ~ spl28_152), inference(forward_demodulation, [], [f2032, f1423])).
fof(f4377, plain, (~ spl28_143 | ~ spl28_145), inference(avatar_contradiction_clause, [], [f4376])).
fof(f4376, plain, ($false | (~ spl28_143 | ~ spl28_145)), inference(subsumption_resolution, [], [f4375, f501])).
fof(f4375, plain, ((e22 = e24) | (~ spl28_143 | ~ spl28_145)), inference(forward_demodulation, [], [f1393, f1385])).
fof(f4372, plain, (~ spl28_290 | ~ spl28_139), inference(avatar_split_clause, [], [f4371, f1366, f2228])).
fof(f1366, plain, (spl28_139 <=> (e23 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_139])])).
fof(f4371, plain, (~ (e23 = h3(e12)) | ~ spl28_139), inference(forward_demodulation, [], [f2010, f1368])).
fof(f1368, plain, ((e23 = op2(e24, e22)) | ~ spl28_139), inference(avatar_component_clause, [], [f1366])).
fof(f2010, plain, ~ (op2(e24, e22) = h3(e12)), inference(backward_demodulation, [], [f411, f599])).
fof(f411, plain, ~ (op2(e22, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f4353, plain, (spl28_125 | ~ spl28_1), inference(avatar_split_clause, [], [f4205, f737, f1257])).
fof(f4205, plain, ((op1(e10, e10) = e14) | ~ spl28_1), inference(backward_demodulation, [], [f182, f739])).
fof(f182, plain, (e14 = op1(op1(e14, e14), op1(e14, e14))), inference(cnf_transformation, [], [f3])).
fof(f4327, plain, (~ spl28_83 | ~ spl28_33), inference(avatar_split_clause, [], [f4326, f871, f1081])).
fof(f4326, plain, (~ (e12 = op1(e11, e13)) | ~ spl28_33), inference(forward_demodulation, [], [f318, f873])).
fof(f318, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f4291, plain, (~ spl28_58 | ~ spl28_33), inference(avatar_split_clause, [], [f4290, f871, f976])).
fof(f976, plain, (spl28_58 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_58])])).
fof(f4290, plain, (~ (e12 = op1(e12, e13)) | ~ spl28_33), inference(forward_demodulation, [], [f320, f873])).
fof(f320, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f4276, plain, (~ spl28_51 | ~ spl28_1), inference(avatar_split_clause, [], [f4275, f737, f947])).
fof(f947, plain, (spl28_51 <=> (e10 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_51])])).
fof(f4275, plain, (~ (e10 = op1(e12, e14)) | ~ spl28_1), inference(forward_demodulation, [], [f331, f739])).
fof(f331, plain, ~ (op1(e12, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4269, plain, (~ spl28_48 | ~ spl28_33), inference(avatar_split_clause, [], [f4268, f871, f934])).
fof(f934, plain, (spl28_48 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_48])])).
fof(f4268, plain, (~ (e12 = op1(e13, e10)) | ~ spl28_33), inference(forward_demodulation, [], [f365, f873])).
fof(f365, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f4267, plain, (~ spl28_47 | ~ spl28_27), inference(avatar_split_clause, [], [f4266, f846, f930])).
fof(f930, plain, (spl28_47 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl28_47])])).
fof(f4266, plain, (~ (e11 = op1(e13, e10)) | ~ spl28_27), inference(forward_demodulation, [], [f366, f848])).
fof(f366, plain, ~ (op1(e13, e10) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4255, plain, (~ spl28_37 | ~ spl28_27), inference(avatar_split_clause, [], [f4254, f846, f888])).
fof(f888, plain, (spl28_37 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl28_37])])).
fof(f4254, plain, (~ (e11 = op1(e13, e12)) | ~ spl28_27), inference(forward_demodulation, [], [f371, f848])).
fof(f371, plain, ~ (op1(e13, e12) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4247, plain, (~ spl28_32 | ~ spl28_27), inference(avatar_split_clause, [], [f4246, f846, f867])).
fof(f4246, plain, (~ (e11 = op1(e13, e13)) | ~ spl28_27), inference(forward_demodulation, [], [f372, f848])).
fof(f372, plain, ~ (op1(e13, e13) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f4245, plain, (spl28_40 | ~ spl28_33), inference(avatar_split_clause, [], [f4244, f871, f900])).
fof(f4244, plain, ((e14 = op1(e13, e12)) | ~ spl28_33), inference(forward_demodulation, [], [f581, f873])).
fof(f581, plain, (e14 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((e14 = op1(e13, op1(e13, e13))) & (e12 = op1(e13, e13)) & (e11 = op1(e13, op1(e13, op1(e13, e13)))) & (e10 = op1(op1(e13, op1(e13, e13)), op1(e13, op1(e13, e13))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax14)).
fof(f4240, plain, (spl28_95 | ~ spl28_7 | ~ spl28_27), inference(avatar_split_clause, [], [f4239, f846, f762, f1131])).
fof(f762, plain, (spl28_7 <=> (e11 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_7])])).
fof(f4239, plain, ((e14 = op1(e11, e11)) | (~ spl28_7 | ~ spl28_27)), inference(forward_demodulation, [], [f4238, f848])).
fof(f4238, plain, ((e14 = op1(op1(e13, e14), e11)) | ~ spl28_7), inference(forward_demodulation, [], [f181, f764])).
fof(f764, plain, ((e11 = op1(e14, e13)) | ~ spl28_7), inference(avatar_component_clause, [], [f762])).
fof(f4211, plain, (~ spl28_6 | ~ spl28_1), inference(avatar_split_clause, [], [f4210, f737, f758])).
fof(f758, plain, (spl28_6 <=> (e10 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_6])])).
fof(f4210, plain, (~ (e10 = op1(e14, e13)) | ~ spl28_1), inference(forward_demodulation, [], [f382, f739])).
fof(f382, plain, ~ (op1(e14, e13) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4209, plain, (~ spl28_1 | ~ spl28_121), inference(avatar_contradiction_clause, [], [f4208])).
fof(f4208, plain, ($false | (~ spl28_1 | ~ spl28_121)), inference(subsumption_resolution, [], [f4207, f486])).
fof(f4207, plain, ((e10 = e14) | (~ spl28_1 | ~ spl28_121)), inference(forward_demodulation, [], [f4205, f1243])).
fof(f1243, plain, ((e10 = op1(e10, e10)) | ~ spl28_121), inference(avatar_component_clause, [], [f1241])).
fof(f4142, plain, (~ spl28_27 | ~ spl28_30), inference(avatar_contradiction_clause, [], [f4141])).
fof(f4141, plain, ($false | (~ spl28_27 | ~ spl28_30)), inference(subsumption_resolution, [], [f4140, f489])).
fof(f4140, plain, ((e11 = e14) | (~ spl28_27 | ~ spl28_30)), inference(backward_demodulation, [], [f860, f848])).
fof(f860, plain, ((e14 = op1(e13, e14)) | ~ spl28_30), inference(avatar_component_clause, [], [f858])).
fof(f858, plain, (spl28_30 <=> (e14 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_30])])).
fof(f4132, plain, ~ spl28_34, inference(avatar_contradiction_clause, [], [f4131])).
fof(f4131, plain, ($false | ~ spl28_34), inference(subsumption_resolution, [], [f4130, f492])).
fof(f4130, plain, ((e13 = e14) | ~ spl28_34), inference(forward_demodulation, [], [f4127, f877])).
fof(f877, plain, ((e13 = op1(e13, e13)) | ~ spl28_34), inference(avatar_component_clause, [], [f875])).
fof(f875, plain, (spl28_34 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl28_34])])).
fof(f4127, plain, ((e14 = op1(e13, e13)) | ~ spl28_34), inference(backward_demodulation, [], [f581, f877])).
fof(f4001, plain, (~ spl28_79 | ~ spl28_80), inference(avatar_contradiction_clause, [], [f4000])).
fof(f4000, plain, ($false | (~ spl28_79 | ~ spl28_80)), inference(subsumption_resolution, [], [f3999, f492])).
fof(f3999, plain, ((e13 = e14) | (~ spl28_79 | ~ spl28_80)), inference(backward_demodulation, [], [f1070, f1066])).
fof(f3996, plain, (~ spl28_55 | ~ spl28_80), inference(avatar_split_clause, [], [f3992, f1068, f963])).
fof(f963, plain, (spl28_55 <=> (e14 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl28_55])])).
fof(f3992, plain, (~ (e14 = op1(e12, e14)) | ~ spl28_80), inference(backward_demodulation, [], [f327, f1070])).
fof(f327, plain, ~ (op1(e11, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f3986, plain, (~ spl28_56 | ~ spl28_81), inference(avatar_split_clause, [], [f3982, f1073, f968])).
fof(f3982, plain, (~ (e10 = op1(e12, e13)) | ~ spl28_81), inference(backward_demodulation, [], [f317, f1075])).
fof(f317, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f7])).
fof(f3885, plain, (~ spl28_111 | ~ spl28_116), inference(avatar_split_clause, [], [f3877, f1220, f1199])).
fof(f3877, plain, (~ (e10 = op1(e10, e12)) | ~ spl28_116), inference(backward_demodulation, [], [f337, f1222])).
fof(f1222, plain, ((e10 = op1(e10, e11)) | ~ spl28_116), inference(avatar_component_clause, [], [f1220])).
fof(f337, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f7])).
fof(f3883, plain, (~ spl28_41 | ~ spl28_116), inference(avatar_split_clause, [], [f3875, f1220, f905])).
fof(f3875, plain, (~ (e10 = op1(e13, e11)) | ~ spl28_116), inference(backward_demodulation, [], [f295, f1222])).
fof(f295, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f3851, plain, (spl28_276 | ~ spl28_126), inference(avatar_split_clause, [], [f3850, f1312, f2150])).
fof(f3850, plain, ((e20 = h4(e10)) | ~ spl28_126), inference(backward_demodulation, [], [f2044, f1314])).
fof(f3833, plain, (spl28_130 | ~ spl28_140 | ~ spl28_180), inference(avatar_split_clause, [], [f3828, f1538, f1370, f1328])).
fof(f3828, plain, ((e24 = op2(e24, e24)) | (~ spl28_140 | ~ spl28_180)), inference(backward_demodulation, [], [f3699, f1372])).
fof(f1372, plain, ((e24 = op2(e24, e22)) | ~ spl28_140), inference(avatar_component_clause, [], [f1370])).
fof(f3699, plain, ((e24 = op2(e24, op2(e24, e22))) | ~ spl28_180), inference(backward_demodulation, [], [f280, f1540])).
fof(f3817, plain, (spl28_130 | ~ spl28_145 | ~ spl28_205), inference(avatar_split_clause, [], [f3811, f1643, f1391, f1328])).
fof(f3811, plain, ((e24 = op2(e24, e24)) | (~ spl28_145 | ~ spl28_205)), inference(backward_demodulation, [], [f3629, f1393])).
fof(f3629, plain, ((e24 = op2(e24, op2(e24, e21))) | ~ spl28_205), inference(backward_demodulation, [], [f279, f1645])).
fof(f3802, plain, (~ spl28_148 | ~ spl28_149), inference(avatar_contradiction_clause, [], [f3801])).
fof(f3801, plain, ($false | (~ spl28_148 | ~ spl28_149)), inference(subsumption_resolution, [], [f3800, f500])).
fof(f3800, plain, ((e22 = e23) | (~ spl28_148 | ~ spl28_149)), inference(backward_demodulation, [], [f1410, f1406])).
fof(f1410, plain, ((e23 = op2(e24, e20)) | ~ spl28_149), inference(avatar_component_clause, [], [f1408])).
fof(f1408, plain, (spl28_149 <=> (e23 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_149])])).
fof(f3793, plain, (~ spl28_488 | ~ spl28_150), inference(avatar_split_clause, [], [f3786, f1412, f3466])).
fof(f1412, plain, (spl28_150 <=> (e24 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl28_150])])).
fof(f3786, plain, (~ (e24 = h1(e12)) | ~ spl28_150), inference(backward_demodulation, [], [f1982, f1414])).
fof(f1414, plain, ((e24 = op2(e24, e20)) | ~ spl28_150), inference(avatar_component_clause, [], [f1412])).
fof(f1982, plain, ~ (op2(e24, e20) = h1(e12)), inference(backward_demodulation, [], [f386, f589])).
fof(f386, plain, ~ (op2(e20, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f3782, plain, (~ spl28_152 | ~ spl28_155), inference(avatar_contradiction_clause, [], [f3781])).
fof(f3781, plain, ($false | (~ spl28_152 | ~ spl28_155)), inference(subsumption_resolution, [], [f3780, f499])).
fof(f3780, plain, ((e21 = e24) | (~ spl28_152 | ~ spl28_155)), inference(backward_demodulation, [], [f1435, f1423])).
fof(f1435, plain, ((e24 = op2(e23, e24)) | ~ spl28_155), inference(avatar_component_clause, [], [f1433])).
fof(f1433, plain, (spl28_155 <=> (e24 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_155])])).
fof(f3774, plain, ~ spl28_159, inference(avatar_contradiction_clause, [], [f3773])).
fof(f3773, plain, ($false | ~ spl28_159), inference(subsumption_resolution, [], [f3772, f502])).
fof(f3772, plain, ((e23 = e24) | ~ spl28_159), inference(forward_demodulation, [], [f3769, f1452])).
fof(f1452, plain, ((e23 = op2(e23, e23)) | ~ spl28_159), inference(avatar_component_clause, [], [f1450])).
fof(f1450, plain, (spl28_159 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_159])])).
fof(f3769, plain, ((e24 = op2(e23, e23)) | ~ spl28_159), inference(backward_demodulation, [], [f2030, f3766])).
fof(f3766, plain, ((e23 = h4(e12)) | ~ spl28_159), inference(backward_demodulation, [], [f604, f1452])).
fof(f3743, plain, (~ spl28_284 | ~ spl28_167), inference(avatar_split_clause, [], [f3736, f1484, f2195])).
fof(f1484, plain, (spl28_167 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_167])])).
fof(f3736, plain, (~ (e21 = h4(e11)) | ~ spl28_167), inference(backward_demodulation, [], [f2040, f1486])).
fof(f1486, plain, ((e21 = op2(e23, e21)) | ~ spl28_167), inference(avatar_component_clause, [], [f1484])).
fof(f2040, plain, ~ (op2(e23, e21) = h4(e11)), inference(backward_demodulation, [], [f469, f2032])).
fof(f469, plain, ~ (op2(e23, e21) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f3694, plain, (~ spl28_131 | ~ spl28_181), inference(avatar_split_clause, [], [f3690, f1543, f1333])).
fof(f1333, plain, (spl28_131 <=> (e20 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl28_131])])).
fof(f3690, plain, (~ (e20 = op2(e24, e23)) | ~ spl28_181), inference(backward_demodulation, [], [f421, f1545])).
fof(f421, plain, ~ (op2(e22, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f3640, plain, (~ spl28_204 | ~ spl28_205), inference(avatar_contradiction_clause, [], [f3639])).
fof(f3639, plain, ($false | (~ spl28_204 | ~ spl28_205)), inference(subsumption_resolution, [], [f3638, f502])).
fof(f3638, plain, ((e23 = e24) | (~ spl28_204 | ~ spl28_205)), inference(backward_demodulation, [], [f1645, f1641])).
fof(f3635, plain, (~ spl28_459 | ~ spl28_205), inference(avatar_split_clause, [], [f3631, f1643, f3289])).
fof(f3631, plain, (~ (e24 = h2(e12)) | ~ spl28_205), inference(backward_demodulation, [], [f2000, f1645])).
fof(f2000, plain, ~ (op2(e21, e24) = h2(e12)), inference(backward_demodulation, [], [f449, f594])).
fof(f449, plain, ~ (op2(e21, e21) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f3575, plain, (~ spl28_228 | ~ spl28_229), inference(avatar_contradiction_clause, [], [f3574])).
fof(f3574, plain, ($false | (~ spl28_228 | ~ spl28_229)), inference(subsumption_resolution, [], [f3573, f500])).
fof(f3573, plain, ((e22 = e23) | (~ spl28_228 | ~ spl28_229)), inference(backward_demodulation, [], [f1746, f1742])).
fof(f1742, plain, ((e22 = op2(e20, e24)) | ~ spl28_228), inference(avatar_component_clause, [], [f1740])).
fof(f1740, plain, (spl28_228 <=> (e22 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl28_228])])).
fof(f3521, plain, (~ spl28_166 | ~ spl28_241), inference(avatar_split_clause, [], [f3513, f1795, f1480])).
fof(f1795, plain, (spl28_241 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl28_241])])).
fof(f3513, plain, (~ (e20 = op2(e23, e21)) | ~ spl28_241), inference(backward_demodulation, [], [f395, f1797])).
fof(f1797, plain, ((e20 = op2(e20, e21)) | ~ spl28_241), inference(avatar_component_clause, [], [f1795])).
fof(f395, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f2672, plain, (~ spl28_348 | ~ spl28_349 | ~ spl28_350 | ~ spl28_351 | ~ spl28_352 | ~ spl28_353 | ~ spl28_354 | ~ spl28_355 | spl28_274 | spl28_269 | spl28_264 | spl28_259 | ~ spl28_357 | ~ spl28_358 | ~ spl28_359 | ~ spl28_360 | ~ spl28_361 | ~ spl28_362 | ~ spl28_363 | ~ spl28_364 | ~ spl28_365 | ~ spl28_366 | ~ spl28_367 | ~ spl28_368 | ~ spl28_369 | ~ spl28_370 | ~ spl28_371 | ~ spl28_372 | ~ spl28_373), inference(avatar_split_clause, [], [f2671, f2644, f2640, f2636, f2632, f2628, f2624, f2620, f2616, f2612, f2608, f2604, f2600, f2596, f2592, f2588, f2584, f2580, f2061, f2087, f2113, f2139, f2572, f2568, f2564, f2560, f2556, f2552, f2548, f2544])).
fof(f2139, plain, (spl28_274 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl28_274])])).
fof(f2113, plain, (spl28_269 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl28_269])])).
fof(f2087, plain, (spl28_264 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl28_264])])).
fof(f2061, plain, (spl28_259 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl28_259])])).
fof(f2671, plain, (~ (h5(op1(e10, e12)) = op2(h5(e10), h4(e10))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), e24)) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h4(e10))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), e24)) | ~ (h5(op1(e12, e10)) = op2(h4(e10), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | ~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2670, f2055])).
fof(f2670, plain, (~ (h5(op1(e10, e13)) = op2(h5(e10), e24)) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h4(e10))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), e24)) | ~ (h5(op1(e12, e10)) = op2(h4(e10), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | ~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2669, f606])).
fof(f2669, plain, (~ (h5(op1(e11, e12)) = op2(h5(e11), h4(e10))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), e24)) | ~ (h5(op1(e12, e10)) = op2(h4(e10), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | ~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2668, f2055])).
fof(f2668, plain, (~ (h5(op1(e11, e13)) = op2(h5(e11), e24)) | ~ (h5(op1(e12, e10)) = op2(h4(e10), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | ~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2667, f606])).
fof(f2667, plain, (~ (h5(op1(e12, e10)) = op2(h4(e10), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | ~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2666, f2055])).
fof(f2666, plain, (~ (h5(op1(e12, e11)) = op2(h4(e10), h5(e11))) | ~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2665, f2055])).
fof(f2665, plain, (~ (e24 = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2664, f2045])).
fof(f2664, plain, (~ (op2(h4(e10), h4(e10)) = h5(op1(e12, e12))) | ~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2663, f2055])).
fof(f2663, plain, (~ (h5(op1(e12, e13)) = op2(h4(e10), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2662, f2055])).
fof(f2662, plain, (~ (h5(op1(e12, e13)) = op2(h5(e12), e24)) | ~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2661, f606])).
fof(f2661, plain, (~ (h5(op1(e12, e14)) = op2(h4(e10), h5(e14))) | ~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2660, f2055])).
fof(f2660, plain, (~ (h5(op1(e13, e10)) = op2(e24, h5(e10))) | ~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2659, f606])).
fof(f2659, plain, (~ (h5(op1(e13, e11)) = op2(e24, h5(e11))) | ~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2658, f606])).
fof(f2658, plain, (~ (h5(e14) = h5(op1(e13, e12))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2657, f2054])).
fof(f2657, plain, (~ (h5(op1(e13, e12)) = op2(e24, h4(e10))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2656, f606])).
fof(f2656, plain, (~ (h5(op1(e13, e12)) = op2(h5(e13), h4(e10))) | ~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2655, f2055])).
fof(f2655, plain, (~ (h4(e10) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2654, f2044])).
fof(f2654, plain, (~ (op2(e24, e24) = h5(op1(e13, e13))) | ~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2653, f606])).
fof(f2653, plain, (~ (h5(e11) = h5(op1(e13, e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2652, f2057])).
fof(f2652, plain, (~ (h5(op1(e13, e14)) = op2(e24, h5(e14))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2651, f606])).
fof(f2651, plain, (~ (h5(op1(e14, e12)) = op2(h5(e14), h4(e10))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2650, f2055])).
fof(f2650, plain, (~ (h5(op1(e14, e13)) = op2(h5(e14), e24)) | ~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2649, f606])).
fof(f2649, plain, (~ (h5(e10) = h5(op1(e14, e14))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(forward_demodulation, [], [f2648, f2059])).
fof(f2648, plain, (sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(subsumption_resolution, [], [f734, f606])).
fof(f734, plain, (~ (e24 = h5(e13)) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))), inference(cnf_transformation, [], [f54])).
fof(f54, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | sP27 | sP26 | sP25 | sP24 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | sP23 | sP22 | sP21 | sP20 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | sP19 | sP18 | sP17 | sP16 | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | sP15 | sP14 | sP13 | sP12 | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | sP11 | sP10 | sP9 | sP8 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f23, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34])).
fof(f34, plain, ((~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP8), inference(usedef, [], [e34])).
fof(e34, plain, (sP8 <=> (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
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
fof(f21, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', co1)).
fof(f2153, plain, (~ spl28_274 | ~ spl28_276), inference(avatar_split_clause, [], [f2148, f2150, f2139])).
fof(f2148, plain, (~ (e20 = h4(e10)) | ~ sP24), inference(forward_demodulation, [], [f628, f2055])).
fof(f628, plain, (~ (e20 = h5(e12)) | ~ sP24), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ sP24), inference(nnf_transformation, [], [f50])).
fof(f2132, plain, (~ spl28_269 | ~ spl28_272), inference(avatar_split_clause, [], [f622, f2129, f2113])).
fof(f622, plain, (~ (e21 = h5(e11)) | ~ sP25), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ((~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | ~ sP25), inference(nnf_transformation, [], [f51])).
fof(f2094, plain, (~ spl28_264 | ~ spl28_265), inference(avatar_split_clause, [], [f620, f2091, f2087])).
fof(f620, plain, (~ (e22 = h5(e14)) | ~ sP26), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ((~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | ~ sP26), inference(nnf_transformation, [], [f52])).
fof(f2085, plain, (~ spl28_259 | ~ spl28_263), inference(avatar_split_clause, [], [f611, f2082, f2061])).
fof(f611, plain, (~ (e23 = h5(e10)) | ~ sP27), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ((~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | ~ sP27), inference(nnf_transformation, [], [f53])).
fof(f1977, plain, spl28_126, inference(avatar_split_clause, [], [f1976, f1312])).
fof(f1976, plain, (e20 = op2(e24, e24)), inference(forward_demodulation, [], [f582, f585])).
fof(f582, plain, (e20 = op2(op2(e23, op2(e23, e23)), op2(e23, op2(e23, e23)))), inference(cnf_transformation, [], [f15])).
fof(f1975, plain, spl28_152, inference(avatar_split_clause, [], [f1974, f1421])).
fof(f1974, plain, (e21 = op2(e23, e24)), inference(forward_demodulation, [], [f583, f585])).
fof(f583, plain, (e21 = op2(e23, op2(e23, op2(e23, e23)))), inference(cnf_transformation, [], [f15])).
fof(f1973, plain, spl28_158, inference(avatar_split_clause, [], [f584, f1446])).
fof(f584, plain, (e22 = op2(e23, e23)), inference(cnf_transformation, [], [f15])).
fof(f1972, plain, spl28_1, inference(avatar_split_clause, [], [f1971, f737])).
fof(f1971, plain, (e10 = op1(e14, e14)), inference(forward_demodulation, [], [f578, f581])).
fof(f578, plain, (e10 = op1(op1(e13, op1(e13, e13)), op1(e13, op1(e13, e13)))), inference(cnf_transformation, [], [f14])).
fof(f1970, plain, spl28_27, inference(avatar_split_clause, [], [f1969, f846])).
fof(f1969, plain, (e11 = op1(e13, e14)), inference(forward_demodulation, [], [f579, f581])).
fof(f579, plain, (e11 = op1(e13, op1(e13, op1(e13, e13)))), inference(cnf_transformation, [], [f14])).
fof(f1968, plain, spl28_33, inference(avatar_split_clause, [], [f580, f871])).
fof(f580, plain, (e12 = op1(e13, e13)), inference(cnf_transformation, [], [f14])).
fof(f1964, plain, (spl28_258 | spl28_257 | spl28_256 | spl28_255 | ~ spl28_134), inference(avatar_split_clause, [], [f576, f1345, f1928, f1937, f1946, f1955])).
fof(f1955, plain, (spl28_258 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl28_258])])).
fof(f1946, plain, (spl28_257 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl28_257])])).
fof(f1937, plain, (spl28_256 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl28_256])])).
fof(f1928, plain, (spl28_255 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl28_255])])).
fof(f576, plain, (~ (e23 = op2(e24, e23)) | sP7 | sP6 | sP5 | sP4), inference(cnf_transformation, [], [f33])).
fof(f33, plain, ((~ (e24 = op2(e24, e24)) & ~ (e23 = op2(e24, e23)) & ~ (e22 = op2(e24, e22)) & ~ (e21 = op2(e24, e21)) & ~ (e20 = op2(e24, e20))) | sP7 | sP6 | sP5 | sP4), inference(definition_folding, [], [f13, e32, e31, e30, e29])).
fof(f29, plain, ((~ (e24 = op2(e20, e24)) & ~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20))) | ~ sP4), inference(usedef, [], [e29])).
fof(e29, plain, (sP4 <=> (~ (e24 = op2(e20, e24)) & ~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f30, plain, ((~ (e24 = op2(e21, e24)) & ~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20))) | ~ sP5), inference(usedef, [], [e30])).
fof(e30, plain, (sP5 <=> (~ (e24 = op2(e21, e24)) & ~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f31, plain, ((~ (e24 = op2(e22, e24)) & ~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20))) | ~ sP6), inference(usedef, [], [e31])).
fof(e31, plain, (sP6 <=> (~ (e24 = op2(e22, e24)) & ~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f32, plain, ((~ (e24 = op2(e23, e24)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20))) | ~ sP7), inference(usedef, [], [e32])).
fof(e32, plain, (sP7 <=> (~ (e24 = op2(e23, e24)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f13, plain, ((~ (e24 = op2(e24, e24)) & ~ (e23 = op2(e24, e23)) & ~ (e22 = op2(e24, e22)) & ~ (e21 = op2(e24, e21)) & ~ (e20 = op2(e24, e20))) | (~ (e24 = op2(e23, e24)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20))) | (~ (e24 = op2(e22, e24)) & ~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20))) | (~ (e24 = op2(e21, e24)) & ~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20))) | (~ (e24 = op2(e20, e24)) & ~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax13)).
fof(f1960, plain, (~ spl28_258 | ~ spl28_238), inference(avatar_split_clause, [], [f570, f1782, f1955])).
fof(f1782, plain, (spl28_238 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl28_238])])).
fof(f570, plain, (~ (e22 = op2(e20, e22)) | ~ sP4), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e24 = op2(e20, e24)) & ~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20))) | ~ sP4), inference(nnf_transformation, [], [f29])).
fof(f1952, plain, (~ spl28_257 | ~ spl28_217), inference(avatar_split_clause, [], [f564, f1694, f1946])).
fof(f564, plain, (~ (e21 = op2(e21, e21)) | ~ sP5), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e24 = op2(e21, e24)) & ~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20))) | ~ sP5), inference(nnf_transformation, [], [f30])).
fof(f1940, plain, (~ spl28_256 | ~ spl28_180), inference(avatar_split_clause, [], [f562, f1538, f1937])).
fof(f562, plain, (~ (e24 = op2(e22, e24)) | ~ sP6), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e24 = op2(e22, e24)) & ~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20))) | ~ sP6), inference(nnf_transformation, [], [f31])).
fof(f1935, plain, (~ spl28_255 | ~ spl28_171), inference(avatar_split_clause, [], [f553, f1501, f1928])).
fof(f553, plain, (~ (e20 = op2(e23, e20)) | ~ sP7), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e24 = op2(e23, e24)) & ~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20))) | ~ sP7), inference(nnf_transformation, [], [f32])).
fof(f1923, plain, (spl28_254 | spl28_253 | spl28_252 | spl28_251 | ~ spl28_9), inference(avatar_split_clause, [], [f551, f770, f1887, f1896, f1905, f1914])).
fof(f1914, plain, (spl28_254 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl28_254])])).
fof(f1905, plain, (spl28_253 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl28_253])])).
fof(f1896, plain, (spl28_252 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl28_252])])).
fof(f1887, plain, (spl28_251 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl28_251])])).
fof(f551, plain, (~ (e13 = op1(e14, e13)) | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ((~ (e14 = op1(e14, e14)) & ~ (e13 = op1(e14, e13)) & ~ (e12 = op1(e14, e12)) & ~ (e11 = op1(e14, e11)) & ~ (e10 = op1(e14, e10))) | sP3 | sP2 | sP1 | sP0), inference(definition_folding, [], [f12, e27, e26, e25, e24])).
fof(f24, plain, ((~ (e14 = op1(e10, e14)) & ~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e24])).
fof(e24, plain, (sP0 <=> (~ (e14 = op1(e10, e14)) & ~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f25, plain, ((~ (e14 = op1(e11, e14)) & ~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10))) | ~ sP1), inference(usedef, [], [e25])).
fof(e25, plain, (sP1 <=> (~ (e14 = op1(e11, e14)) & ~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f26, plain, ((~ (e14 = op1(e12, e14)) & ~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10))) | ~ sP2), inference(usedef, [], [e26])).
fof(e26, plain, (sP2 <=> (~ (e14 = op1(e12, e14)) & ~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f27, plain, ((~ (e14 = op1(e13, e14)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10))) | ~ sP3), inference(usedef, [], [e27])).
fof(e27, plain, (sP3 <=> (~ (e14 = op1(e13, e14)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f12, plain, ((~ (e14 = op1(e14, e14)) & ~ (e13 = op1(e14, e13)) & ~ (e12 = op1(e14, e12)) & ~ (e11 = op1(e14, e11)) & ~ (e10 = op1(e14, e10))) | (~ (e14 = op1(e13, e14)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10))) | (~ (e14 = op1(e12, e14)) & ~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10))) | (~ (e14 = op1(e11, e14)) & ~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10))) | (~ (e14 = op1(e10, e14)) & ~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax12)).
fof(f1919, plain, (~ spl28_254 | ~ spl28_113), inference(avatar_split_clause, [], [f545, f1207, f1914])).
fof(f545, plain, (~ (e12 = op1(e10, e12)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e14 = op1(e10, e14)) & ~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f24])).
fof(f1911, plain, (~ spl28_253 | ~ spl28_92), inference(avatar_split_clause, [], [f539, f1119, f1905])).
fof(f539, plain, (~ (e11 = op1(e11, e11)) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e14 = op1(e11, e14)) & ~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10))) | ~ sP1), inference(nnf_transformation, [], [f25])).
fof(f1899, plain, (~ spl28_252 | ~ spl28_55), inference(avatar_split_clause, [], [f537, f963, f1896])).
fof(f537, plain, (~ (e14 = op1(e12, e14)) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e14 = op1(e12, e14)) & ~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10))) | ~ sP2), inference(nnf_transformation, [], [f26])).
fof(f1894, plain, (~ spl28_251 | ~ spl28_46), inference(avatar_split_clause, [], [f528, f926, f1887])).
fof(f528, plain, (~ (e10 = op1(e13, e10)) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e14 = op1(e13, e14)) & ~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10))) | ~ sP3), inference(nnf_transformation, [], [f27])).
fof(f1885, plain, (spl28_246 | spl28_241 | spl28_236 | spl28_231 | spl28_226), inference(avatar_split_clause, [], [f208, f1732, f1753, f1774, f1795, f1816])).
fof(f208, plain, ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (((e24 = op2(e24, e24)) | (e24 = op2(e23, e24)) | (e24 = op2(e22, e24)) | (e24 = op2(e21, e24)) | (e24 = op2(e20, e24))) & ((e24 = op2(e24, e24)) | (e24 = op2(e24, e23)) | (e24 = op2(e24, e22)) | (e24 = op2(e24, e21)) | (e24 = op2(e24, e20))) & ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))) & ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))) & ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))) & ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))) & ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))) & ((e21 = op2(e24, e24)) | (e21 = op2(e24, e23)) | (e21 = op2(e24, e22)) | (e21 = op2(e24, e21)) | (e21 = op2(e24, e20))) & ((e20 = op2(e24, e24)) | (e20 = op2(e23, e24)) | (e20 = op2(e22, e24)) | (e20 = op2(e21, e24)) | (e20 = op2(e20, e24))) & ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))) & ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))) & ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))) & ((e23 = op2(e24, e23)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e24, e23)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e24 = op2(e24, e22)) | (e24 = op2(e23, e22)) | (e24 = op2(e22, e22)) | (e24 = op2(e21, e22)) | (e24 = op2(e20, e22))) & ((e24 = op2(e22, e24)) | (e24 = op2(e22, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e22, e21)) | (e24 = op2(e22, e20))) & ((e23 = op2(e24, e22)) | (e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e24)) | (e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e24, e22)) | (e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e24)) | (e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e24)) | (e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e24, e22)) | (e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))) & ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))) & ((e23 = op2(e24, e21)) | (e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e24)) | (e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e24)) | (e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e24, e21)) | (e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e24, e21)) | (e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e24)) | (e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e24 = op2(e24, e20)) | (e24 = op2(e23, e20)) | (e24 = op2(e22, e20)) | (e24 = op2(e21, e20)) | (op2(e20, e20) = e24)) & ((e24 = op2(e20, e24)) | (e24 = op2(e20, e23)) | (e24 = op2(e20, e22)) | (e24 = op2(e20, e21)) | (op2(e20, e20) = e24)) & ((e23 = op2(e24, e20)) | (e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e24)) | (e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e24, e20)) | (e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e24)) | (e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e24, e20)) | (e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e24)) | (e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax5)).
fof(f1884, plain, (spl28_246 | spl28_221 | spl28_196 | spl28_171 | spl28_146), inference(avatar_split_clause, [], [f209, f1396, f1501, f1606, f1711, f1816])).
fof(f209, plain, ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f5])).
fof(f1881, plain, (spl28_248 | spl28_243 | spl28_238 | spl28_233 | spl28_228), inference(avatar_split_clause, [], [f212, f1740, f1761, f1782, f1803, f1824])).
fof(f212, plain, ((e22 = op2(e20, e24)) | (e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f5])).
fof(f1878, plain, (spl28_249 | spl28_224 | spl28_199 | spl28_174 | spl28_149), inference(avatar_split_clause, [], [f215, f1408, f1513, f1618, f1723, f1828])).
fof(f215, plain, ((e23 = op2(e24, e20)) | (e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f5])).
fof(f1873, plain, (spl28_222 | spl28_217 | spl28_212 | spl28_207 | spl28_202), inference(avatar_split_clause, [], [f220, f1631, f1652, f1673, f1694, f1715])).
fof(f220, plain, ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f5])).
fof(f1870, plain, (spl28_243 | spl28_218 | spl28_193 | spl28_168 | spl28_143), inference(avatar_split_clause, [], [f223, f1383, f1488, f1593, f1698, f1803])).
fof(f223, plain, ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f5])).
fof(f1867, plain, (spl28_225 | spl28_220 | spl28_215 | spl28_210 | spl28_205), inference(avatar_split_clause, [], [f226, f1643, f1664, f1685, f1706, f1727])).
fof(f226, plain, ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))), inference(cnf_transformation, [], [f5])).
fof(f1866, plain, (spl28_245 | spl28_220 | spl28_195 | spl28_170 | spl28_145), inference(avatar_split_clause, [], [f227, f1391, f1496, f1601, f1706, f1811])).
fof(f227, plain, ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))), inference(cnf_transformation, [], [f5])).
fof(f1852, plain, (spl28_232 | spl28_207 | spl28_182 | spl28_157 | spl28_132), inference(avatar_split_clause, [], [f241, f1337, f1442, f1547, f1652, f1757])).
fof(f241, plain, ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f5])).
fof(f1849, plain, (spl28_174 | spl28_169 | spl28_164 | spl28_159 | spl28_154), inference(avatar_split_clause, [], [f244, f1429, f1450, f1471, f1492, f1513])).
fof(f244, plain, ((e23 = op2(e23, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f5])).
fof(f1848, plain, (spl28_234 | spl28_209 | spl28_184 | spl28_159 | spl28_134), inference(avatar_split_clause, [], [f245, f1345, f1450, f1555, f1660, f1765])).
fof(f245, plain, ((e23 = op2(e24, e23)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f5])).
fof(f1846, plain, (spl28_235 | spl28_210 | spl28_185 | spl28_160 | spl28_135), inference(avatar_split_clause, [], [f247, f1349, f1454, f1559, f1664, f1769])).
fof(f247, plain, ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))), inference(cnf_transformation, [], [f5])).
fof(f1841, plain, (spl28_148 | spl28_143 | spl28_138 | spl28_133 | spl28_128), inference(avatar_split_clause, [], [f252, f1320, f1341, f1362, f1383, f1404])).
fof(f252, plain, ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))), inference(cnf_transformation, [], [f5])).
fof(f1838, plain, (spl28_229 | spl28_204 | spl28_179 | spl28_154 | spl28_129), inference(avatar_split_clause, [], [f255, f1324, f1429, f1534, f1639, f1744])).
fof(f255, plain, ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))), inference(cnf_transformation, [], [f5])).
fof(f1837, plain, (spl28_150 | spl28_145 | spl28_140 | spl28_135 | spl28_130), inference(avatar_split_clause, [], [f256, f1328, f1349, f1370, f1391, f1412])).
fof(f256, plain, ((e24 = op2(e24, e24)) | (e24 = op2(e24, e23)) | (e24 = op2(e24, e22)) | (e24 = op2(e24, e21)) | (e24 = op2(e24, e20))), inference(cnf_transformation, [], [f5])).
fof(f1836, plain, (spl28_230 | spl28_205 | spl28_180 | spl28_155 | spl28_130), inference(avatar_split_clause, [], [f257, f1328, f1433, f1538, f1643, f1748])).
fof(f257, plain, ((e24 = op2(e24, e24)) | (e24 = op2(e23, e24)) | (e24 = op2(e22, e24)) | (e24 = op2(e21, e24)) | (e24 = op2(e20, e24))), inference(cnf_transformation, [], [f5])).
fof(f1625, plain, (spl28_196 | spl28_197 | spl28_198 | spl28_199 | spl28_200), inference(avatar_split_clause, [], [f193, f1622, f1618, f1614, f1610, f1606])).
fof(f193, plain, ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e24 = op2(e24, e24)) | (e23 = op2(e24, e24)) | (e22 = op2(e24, e24)) | (e21 = op2(e24, e24)) | (e20 = op2(e24, e24))) & ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))) & ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))) & ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))) & ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))) & ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))) & ((e24 = op2(e23, e23)) | (e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e24 = op2(e23, e22)) | (e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))) & ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))) & ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e24 = op2(e21, e21)) | (e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e24 = op2(e21, e20)) | (e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e24 = op2(e20, e24)) | (e23 = op2(e20, e24)) | (e22 = op2(e20, e24)) | (e21 = op2(e20, e24)) | (e20 = op2(e20, e24))) & ((e24 = op2(e20, e23)) | (e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e24 = op2(e20, e22)) | (e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e24 = op2(e20, e21)) | (e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e24) | (op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax4)).
fof(f1562, plain, (spl28_181 | spl28_182 | spl28_183 | spl28_184 | spl28_185), inference(avatar_split_clause, [], [f196, f1559, f1555, f1551, f1547, f1543])).
fof(f196, plain, ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f4])).
fof(f1541, plain, (spl28_176 | spl28_177 | spl28_178 | spl28_179 | spl28_180), inference(avatar_split_clause, [], [f197, f1538, f1534, f1530, f1526, f1522])).
fof(f197, plain, ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))), inference(cnf_transformation, [], [f4])).
fof(f1520, plain, (spl28_171 | spl28_172 | spl28_173 | spl28_174 | spl28_175), inference(avatar_split_clause, [], [f198, f1517, f1513, f1509, f1505, f1501])).
fof(f198, plain, ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1499, plain, (spl28_166 | spl28_167 | spl28_168 | spl28_169 | spl28_170), inference(avatar_split_clause, [], [f199, f1496, f1492, f1488, f1484, f1480])).
fof(f199, plain, ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))), inference(cnf_transformation, [], [f4])).
fof(f1373, plain, (spl28_136 | spl28_137 | spl28_138 | spl28_139 | spl28_140), inference(avatar_split_clause, [], [f205, f1370, f1366, f1362, f1358, f1354])).
fof(f205, plain, ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))), inference(cnf_transformation, [], [f4])).
fof(f1352, plain, (spl28_131 | spl28_132 | spl28_133 | spl28_134 | spl28_135), inference(avatar_split_clause, [], [f206, f1349, f1345, f1341, f1337, f1333])).
fof(f206, plain, ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))), inference(cnf_transformation, [], [f4])).
fof(f1293, plain, (spl28_119 | spl28_94 | spl28_69 | spl28_44 | spl28_19), inference(avatar_split_clause, [], [f125, f812, f917, f1022, f1127, f1232])).
fof(f125, plain, ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e14 = op1(e14, e14)) | (e14 = op1(e13, e14)) | (e14 = op1(e12, e14)) | (e14 = op1(e11, e14)) | (e14 = op1(e10, e14))) & ((e14 = op1(e14, e14)) | (e14 = op1(e14, e13)) | (e14 = op1(e14, e12)) | (e14 = op1(e14, e11)) | (e14 = op1(e14, e10))) & ((e13 = op1(e14, e14)) | (e13 = op1(e13, e14)) | (e13 = op1(e12, e14)) | (e13 = op1(e11, e14)) | (e13 = op1(e10, e14))) & ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))) & ((e12 = op1(e14, e14)) | (e12 = op1(e13, e14)) | (e12 = op1(e12, e14)) | (e12 = op1(e11, e14)) | (e12 = op1(e10, e14))) & ((e12 = op1(e14, e14)) | (e12 = op1(e14, e13)) | (e12 = op1(e14, e12)) | (e12 = op1(e14, e11)) | (e12 = op1(e14, e10))) & ((e11 = op1(e14, e14)) | (e11 = op1(e13, e14)) | (e11 = op1(e12, e14)) | (e11 = op1(e11, e14)) | (e11 = op1(e10, e14))) & ((e11 = op1(e14, e14)) | (e11 = op1(e14, e13)) | (e11 = op1(e14, e12)) | (e11 = op1(e14, e11)) | (e11 = op1(e14, e10))) & ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))) & ((e10 = op1(e14, e14)) | (e10 = op1(e14, e13)) | (e10 = op1(e14, e12)) | (e10 = op1(e14, e11)) | (e10 = op1(e14, e10))) & ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))) & ((e14 = op1(e13, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e13, e12)) | (e14 = op1(e13, e11)) | (e14 = op1(e13, e10))) & ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e14, e13)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e14 = op1(e14, e12)) | (e14 = op1(e13, e12)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e12)) | (e14 = op1(e10, e12))) & ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))) & ((e13 = op1(e14, e12)) | (e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e14)) | (e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e14, e12)) | (e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e14)) | (e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e14, e12)) | (e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e14 = op1(e14, e11)) | (e14 = op1(e13, e11)) | (e14 = op1(e12, e11)) | (e14 = op1(e11, e11)) | (e14 = op1(e10, e11))) & ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))) & ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e14)) | (e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e14)) | (e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e14, e11)) | (e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e14)) | (e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e14, e11)) | (e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e14)) | (e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e14 = op1(e14, e10)) | (e14 = op1(e13, e10)) | (e14 = op1(e12, e10)) | (e14 = op1(e11, e10)) | (op1(e10, e10) = e14)) & ((e14 = op1(e10, e14)) | (e14 = op1(e10, e13)) | (e14 = op1(e10, e12)) | (e14 = op1(e10, e11)) | (op1(e10, e10) = e14)) & ((e13 = op1(e14, e10)) | (e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e14)) | (e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e14, e10)) | (e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e14)) | (e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e14, e10)) | (e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e14)) | (e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e14, e10)) | (e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e14)) | (e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax2)).
fof(f1292, plain, (spl28_100 | spl28_95 | spl28_90 | spl28_85 | spl28_80), inference(avatar_split_clause, [], [f126, f1068, f1089, f1110, f1131, f1152])).
fof(f126, plain, ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1288, plain, (spl28_72 | spl28_67 | spl28_62 | spl28_57 | spl28_52), inference(avatar_split_clause, [], [f130, f951, f972, f993, f1014, f1035])).
fof(f130, plain, ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1287, plain, (spl28_112 | spl28_87 | spl28_62 | spl28_37 | spl28_12), inference(avatar_split_clause, [], [f131, f783, f888, f993, f1098, f1203])).
fof(f131, plain, ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1282, plain, (spl28_75 | spl28_70 | spl28_65 | spl28_60 | spl28_55), inference(avatar_split_clause, [], [f136, f963, f984, f1005, f1026, f1047])).
fof(f136, plain, ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1279, plain, (spl28_106 | spl28_81 | spl28_56 | spl28_31 | spl28_6), inference(avatar_split_clause, [], [f139, f758, f863, f968, f1073, f1178])).
fof(f139, plain, ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1277, plain, (spl28_107 | spl28_82 | spl28_57 | spl28_32 | spl28_7), inference(avatar_split_clause, [], [f141, f762, f867, f972, f1077, f1182])).
fof(f141, plain, ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1274, plain, (spl28_49 | spl28_44 | spl28_39 | spl28_34 | spl28_29), inference(avatar_split_clause, [], [f144, f854, f875, f896, f917, f938])).
fof(f144, plain, ((e13 = op1(e13, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1273, plain, (spl28_109 | spl28_84 | spl28_59 | spl28_34 | spl28_9), inference(avatar_split_clause, [], [f145, f770, f875, f980, f1085, f1190])).
fof(f145, plain, ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1261, plain, (spl28_105 | spl28_80 | spl28_55 | spl28_30 | spl28_5), inference(avatar_split_clause, [], [f157, f753, f858, f963, f1068, f1173])).
fof(f157, plain, ((e14 = op1(e14, e14)) | (e14 = op1(e13, e14)) | (e14 = op1(e12, e14)) | (e14 = op1(e11, e14)) | (e14 = op1(e10, e14))), inference(cnf_transformation, [], [f2])).
fof(f1239, plain, (spl28_116 | spl28_117 | spl28_118 | spl28_119 | spl28_120), inference(avatar_split_clause, [], [f84, f1236, f1232, f1228, f1224, f1220])).
fof(f84, plain, ((e14 = op1(e10, e11)) | (e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e14 = op1(e14, e14)) | (e13 = op1(e14, e14)) | (e12 = op1(e14, e14)) | (e11 = op1(e14, e14)) | (e10 = op1(e14, e14))) & ((e14 = op1(e14, e13)) | (e13 = op1(e14, e13)) | (e12 = op1(e14, e13)) | (e11 = op1(e14, e13)) | (e10 = op1(e14, e13))) & ((e14 = op1(e14, e12)) | (e13 = op1(e14, e12)) | (e12 = op1(e14, e12)) | (e11 = op1(e14, e12)) | (e10 = op1(e14, e12))) & ((e14 = op1(e14, e11)) | (e13 = op1(e14, e11)) | (e12 = op1(e14, e11)) | (e11 = op1(e14, e11)) | (e10 = op1(e14, e11))) & ((e14 = op1(e14, e10)) | (e13 = op1(e14, e10)) | (e12 = op1(e14, e10)) | (e11 = op1(e14, e10)) | (e10 = op1(e14, e10))) & ((e14 = op1(e13, e14)) | (e13 = op1(e13, e14)) | (e12 = op1(e13, e14)) | (e11 = op1(e13, e14)) | (e10 = op1(e13, e14))) & ((e14 = op1(e13, e13)) | (e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e14 = op1(e13, e12)) | (e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e14 = op1(e13, e11)) | (e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))) & ((e14 = op1(e12, e13)) | (e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e14 = op1(e12, e12)) | (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e14 = op1(e12, e11)) | (e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e14 = op1(e12, e10)) | (e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e14 = op1(e11, e14)) | (e13 = op1(e11, e14)) | (e12 = op1(e11, e14)) | (e11 = op1(e11, e14)) | (e10 = op1(e11, e14))) & ((e14 = op1(e11, e13)) | (e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e14 = op1(e11, e12)) | (e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e14 = op1(e11, e11)) | (e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e14 = op1(e11, e10)) | (e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e14 = op1(e10, e14)) | (e13 = op1(e10, e14)) | (e12 = op1(e10, e14)) | (e11 = op1(e10, e14)) | (e10 = op1(e10, e14))) & ((e14 = op1(e10, e13)) | (e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e14 = op1(e10, e12)) | (e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e14 = op1(e10, e11)) | (e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e14) | (op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG167+1.p', ax1)).
fof(f1218, plain, (spl28_111 | spl28_112 | spl28_113 | spl28_114 | spl28_115), inference(avatar_split_clause, [], [f85, f1215, f1211, f1207, f1203, f1199])).
fof(f85, plain, ((e14 = op1(e10, e12)) | (e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f1155, plain, (spl28_96 | spl28_97 | spl28_98 | spl28_99 | spl28_100), inference(avatar_split_clause, [], [f88, f1152, f1148, f1144, f1140, f1136])).
fof(f88, plain, ((e14 = op1(e11, e10)) | (e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1134, plain, (spl28_91 | spl28_92 | spl28_93 | spl28_94 | spl28_95), inference(avatar_split_clause, [], [f89, f1131, f1127, f1123, f1119, f1115])).
fof(f89, plain, ((e14 = op1(e11, e11)) | (e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f1092, plain, (spl28_81 | spl28_82 | spl28_83 | spl28_84 | spl28_85), inference(avatar_split_clause, [], [f91, f1089, f1085, f1081, f1077, f1073])).
fof(f91, plain, ((e14 = op1(e11, e13)) | (e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f987, plain, (spl28_56 | spl28_57 | spl28_58 | spl28_59 | spl28_60), inference(avatar_split_clause, [], [f96, f984, f980, f976, f972, f968])).
fof(f96, plain, ((e14 = op1(e12, e13)) | (e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f966, plain, (spl28_51 | spl28_52 | spl28_53 | spl28_54 | spl28_55), inference(avatar_split_clause, [], [f97, f963, f959, f955, f951, f947])).
fof(f97, plain, ((e14 = op1(e12, e14)) | (e13 = op1(e12, e14)) | (e12 = op1(e12, e14)) | (e11 = op1(e12, e14)) | (e10 = op1(e12, e14))), inference(cnf_transformation, [], [f1])).
fof(f945, plain, (spl28_46 | spl28_47 | spl28_48 | spl28_49 | spl28_50), inference(avatar_split_clause, [], [f98, f942, f938, f934, f930, f926])).
fof(f98, plain, ((e14 = op1(e13, e10)) | (e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).
fof(f777, plain, (spl28_6 | spl28_7 | spl28_8 | spl28_9 | spl28_10), inference(avatar_split_clause, [], [f106, f774, f770, f766, f762, f758])).
fof(f106, plain, ((e14 = op1(e14, e13)) | (e13 = op1(e14, e13)) | (e12 = op1(e14, e13)) | (e11 = op1(e14, e13)) | (e10 = op1(e14, e13))), inference(cnf_transformation, [], [f1])).