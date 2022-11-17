fof(f7584, plain, $false, inference(avatar_sat_refutation, [], [f1195, f1198, f1203, f1204, f1205, f1216, f1217, f1223, f1225, f1228, f1230, f1231, f1308, f1539, f1581, f1602, f1791, f1795, f1798, f1801, f1802, f1803, f1807, f1811, f1845, f1846, f1850, f1851, f1852, f1854, f1856, f1857, f1859, f1861, f2253, f2272, f2291, f2313, f3112, f3353, f3359, f3361, f3402, f3403, f3440, f3442, f3464, f3492, f3498, f3529, f3552, f3572, f3573, f3578, f3627, f3630, f3667, f3720, f3748, f3751, f3754, f3760, f3762, f3764, f3814, f3878, f3895, f3904, f3929, f3976, f4045, f4106, f4107, f4109, f4112, f4121, f4123, f4135, f4148, f4164, f4175, f4187, f4195, f4196, f4229, f4290, f4296, f4342, f4368, f4371, f4395, f4416, f4418, f4439, f4462, f4476, f4490, f4502, f4534, f4535, f4541, f4556, f4558, f4572, f4595, f4604, f4615, f4624, f4652, f4679, f4693, f4702, f4717, f4738, f4769, f4828, f4840, f4861, f4867, f4879, f4903, f4930, f4942, f4945, f4987, f4989, f4999, f5066, f5067, f5068, f5069, f5070, f5071, f5072, f5166, f5228, f5288, f5304, f5307, f5334, f5370, f5404, f5426, f5428, f5438, f5488, f5528, f5576, f5626, f5628, f5630, f5650, f5668, f5684, f5703, f5760, f5806, f5845, f5888, f5927, f5963, f6002, f6036, f6077, f6119, f6160, f6240, f6275, f6315, f6349, f6386, f6426, f6513, f6515, f6555, f6568, f6634, f6726, f7030, f7169, f7233, f7249, f7250, f7251, f7252, f7253, f7254, f7255, f7256, f7257, f7276, f7290, f7499, f7502, f7534, f7568, f7578])).
fof(f7578, plain, (~ spl20_171 | ~ spl20_173), inference(avatar_contradiction_clause, [], [f7577])).
fof(f7577, plain, ($false | (~ spl20_171 | ~ spl20_173)), inference(subsumption_resolution, [], [f7576, f448])).
fof(f448, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (~ (e23 = e24) & ~ (e22 = e24) & ~ (e22 = e23) & ~ (e21 = e24) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e24) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax10)).
fof(f7576, plain, ((e20 = e22) | (~ spl20_171 | ~ spl20_173)), inference(backward_demodulation, [], [f1425, f1417])).
fof(f1417, plain, ((e20 = op2(e23, e21)) | ~ spl20_171), inference(avatar_component_clause, [], [f1415])).
fof(f1415, plain, (spl20_171 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_171])])).
fof(f1425, plain, ((e22 = op2(e23, e21)) | ~ spl20_173), inference(avatar_component_clause, [], [f1423])).
fof(f1423, plain, (spl20_173 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_173])])).
fof(f7568, plain, (~ spl20_182 | ~ spl20_197), inference(avatar_split_clause, [], [f7567, f1524, f1461])).
fof(f1461, plain, (spl20_182 <=> (e21 = op2(e22, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_182])])).
fof(f1524, plain, (spl20_197 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_197])])).
fof(f7567, plain, (~ (e21 = op2(e22, e24)) | ~ spl20_197), inference(backward_demodulation, [], [f413, f1526])).
fof(f1526, plain, ((e21 = op2(e22, e21)) | ~ spl20_197), inference(avatar_component_clause, [], [f1524])).
fof(f413, plain, ~ (op2(e22, e21) = op2(e22, e24)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (op2(e24, e23) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e24)) & ~ (op2(e24, e22) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e24)) & ~ (op2(e24, e21) = op2(e24, e23)) & ~ (op2(e24, e21) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e24)) & ~ (op2(e24, e20) = op2(e24, e23)) & ~ (op2(e24, e20) = op2(e24, e22)) & ~ (op2(e24, e20) = op2(e24, e21)) & ~ (op2(e23, e23) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e24)) & ~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e24)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e24)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e23) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e24)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e24)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e24)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e23) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e24)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e24)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e24)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e23) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e24)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e24)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e24)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e23, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e24, e24)) & ~ (op2(e22, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e24, e24)) & ~ (op2(e21, e24) = op2(e23, e24)) & ~ (op2(e21, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e24, e24)) & ~ (op2(e20, e24) = op2(e23, e24)) & ~ (op2(e20, e24) = op2(e22, e24)) & ~ (op2(e20, e24) = op2(e21, e24)) & ~ (op2(e23, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e24, e23)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e24, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e24, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e23, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e24, e22)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e24, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e24, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e23, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e24, e21)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e24, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e24, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e23, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e24, e20)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e24, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e24, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax8)).
fof(f7534, plain, (~ spl20_196 | ~ spl20_66 | ~ spl20_332 | spl20_457), inference(avatar_split_clause, [], [f7533, f3077, f2310, f924, f1520])).
fof(f1520, plain, (spl20_196 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_196])])).
fof(f924, plain, (spl20_66 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_66])])).
fof(f2310, plain, (spl20_332 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_332])])).
fof(f3077, plain, (spl20_457 <=> (op2(e22, e21) = h2(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_457])])).
fof(f7533, plain, (~ (e20 = op2(e22, e21)) | (~ spl20_66 | ~ spl20_332 | spl20_457)), inference(forward_demodulation, [], [f7532, f2311])).
fof(f2311, plain, ((e20 = h2(e10)) | ~ spl20_332), inference(avatar_component_clause, [], [f2310])).
fof(f7532, plain, (~ (op2(e22, e21) = h2(e10)) | (~ spl20_66 | spl20_457)), inference(forward_demodulation, [], [f3079, f926])).
fof(f926, plain, ((e10 = op1(e12, e11)) | ~ spl20_66), inference(avatar_component_clause, [], [f924])).
fof(f3079, plain, (~ (op2(e22, e21) = h2(op1(e12, e11))) | spl20_457), inference(avatar_component_clause, [], [f3077])).
fof(f7502, plain, (~ spl20_157 | ~ spl20_302 | ~ spl20_314), inference(avatar_split_clause, [], [f7501, f2208, f2145, f1356])).
fof(f1356, plain, (spl20_157 <=> (e21 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_157])])).
fof(f2145, plain, (spl20_302 <=> (e23 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_302])])).
fof(f2208, plain, (spl20_314 <=> (e21 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_314])])).
fof(f7501, plain, (~ (e21 = op2(e23, e24)) | (~ spl20_302 | ~ spl20_314)), inference(forward_demodulation, [], [f4410, f2209])).
fof(f2209, plain, ((e21 = h3(e12)) | ~ spl20_314), inference(avatar_component_clause, [], [f2208])).
fof(f4410, plain, (~ (op2(e23, e24) = h3(e12)) | ~ spl20_302), inference(forward_demodulation, [], [f1919, f4137])).
fof(f4137, plain, ((h3(e12) = h4(e14)) | ~ spl20_302), inference(backward_demodulation, [], [f519, f4131])).
fof(f4131, plain, ((op2(e23, e23) = h3(e12)) | ~ spl20_302), inference(backward_demodulation, [], [f1908, f2146])).
fof(f2146, plain, ((e23 = h2(e13)) | ~ spl20_302), inference(avatar_component_clause, [], [f2145])).
fof(f1908, plain, (h3(e12) = op2(h2(e13), h2(e13))), inference(forward_demodulation, [], [f512, f1885])).
fof(f1885, plain, (op2(e22, e22) = h2(e13)), inference(forward_demodulation, [], [f1884, f1883])).
fof(f1883, plain, (e22 = op2(h2(e14), h2(e14))), inference(backward_demodulation, [], [f497, f509])).
fof(f509, plain, (op2(e21, e21) = h2(e14)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e21, e21) = h2(e14)) & (op2(op2(op2(e21, e21), op2(e21, e21)), op2(op2(e21, e21), op2(e21, e21))) = h2(e13)) & (op2(op2(e21, e21), op2(e21, e21)) = h2(e12)) & (op2(e21, op2(op2(e21, e21), op2(e21, e21))) = h2(e10)) & (e21 = h2(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax17)).
fof(f497, plain, (e22 = op2(op2(e21, e21), op2(e21, e21))), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((e24 = op2(e21, e21)) & (e23 = op2(op2(op2(e21, e21), op2(e21, e21)), op2(op2(e21, e21), op2(e21, e21)))) & (e22 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(e21, op2(op2(e21, e21), op2(e21, e21))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax15)).
fof(f1884, plain, (h2(e13) = op2(op2(h2(e14), h2(e14)), op2(h2(e14), h2(e14)))), inference(forward_demodulation, [], [f508, f509])).
fof(f508, plain, (op2(op2(op2(e21, e21), op2(e21, e21)), op2(op2(e21, e21), op2(e21, e21))) = h2(e13)), inference(cnf_transformation, [], [f17])).
fof(f512, plain, (op2(op2(e22, e22), op2(e22, e22)) = h3(e12)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ((op2(e22, e22) = h3(e14)) & (h3(e13) = op2(op2(op2(e22, e22), op2(e22, e22)), op2(op2(e22, e22), op2(e22, e22)))) & (op2(op2(e22, e22), op2(e22, e22)) = h3(e12)) & (h3(e10) = op2(e22, op2(op2(e22, e22), op2(e22, e22)))) & (e22 = h3(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax18)).
fof(f519, plain, (op2(e23, e23) = h4(e14)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((op2(e23, e23) = h4(e14)) & (h4(e13) = op2(op2(op2(e23, e23), op2(e23, e23)), op2(op2(e23, e23), op2(e23, e23)))) & (op2(op2(e23, e23), op2(e23, e23)) = h4(e12)) & (h4(e10) = op2(e23, op2(op2(e23, e23), op2(e23, e23)))) & (e23 = h4(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax19)).
fof(f1919, plain, ~ (op2(e23, e24) = h4(e14)), inference(backward_demodulation, [], [f426, f519])).
fof(f426, plain, ~ (op2(e23, e23) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f7499, plain, (~ spl20_160 | ~ spl20_235), inference(avatar_split_clause, [], [f7498, f1683, f1368])).
fof(f1368, plain, (spl20_160 <=> (e24 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_160])])).
fof(f1683, plain, (spl20_235 <=> (e24 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_235])])).
fof(f7498, plain, (~ (e24 = op2(e23, e24)) | ~ spl20_235), inference(forward_demodulation, [], [f379, f1685])).
fof(f1685, plain, ((e24 = op2(e20, e24)) | ~ spl20_235), inference(avatar_component_clause, [], [f1683])).
fof(f379, plain, ~ (op2(e20, e24) = op2(e23, e24)), inference(cnf_transformation, [], [f8])).
fof(f7290, plain, (~ spl20_67 | ~ spl20_117), inference(avatar_split_clause, [], [f7277, f1138, f928])).
fof(f928, plain, (spl20_67 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_67])])).
fof(f1138, plain, (spl20_117 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_117])])).
fof(f7277, plain, (~ (e11 = op1(e12, e11)) | ~ spl20_117), inference(backward_demodulation, [], [f248, f1140])).
fof(f1140, plain, ((e11 = op1(e10, e11)) | ~ spl20_117), inference(avatar_component_clause, [], [f1138])).
fof(f248, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (op1(e14, e13) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e14)) & ~ (op1(e14, e12) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e14)) & ~ (op1(e14, e11) = op1(e14, e13)) & ~ (op1(e14, e11) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e14)) & ~ (op1(e14, e10) = op1(e14, e13)) & ~ (op1(e14, e10) = op1(e14, e12)) & ~ (op1(e14, e10) = op1(e14, e11)) & ~ (op1(e13, e13) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e14)) & ~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e14)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e14)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e13) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e14)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e14)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e14)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e13) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e14)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e14)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e14)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e13) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e14)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e14)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e14)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e13, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e14, e14)) & ~ (op1(e12, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e14, e14)) & ~ (op1(e11, e14) = op1(e13, e14)) & ~ (op1(e11, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e14, e14)) & ~ (op1(e10, e14) = op1(e13, e14)) & ~ (op1(e10, e14) = op1(e12, e14)) & ~ (op1(e10, e14) = op1(e11, e14)) & ~ (op1(e13, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e14, e13)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e14, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e14, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e13, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e14, e12)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e14, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e14, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e13, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e14, e11)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e14, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e14, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e13, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e14, e10)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e14, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e14, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax7)).
fof(f7276, plain, (~ spl20_106 | ~ spl20_121), inference(avatar_split_clause, [], [f7264, f1155, f1092])).
fof(f1092, plain, (spl20_106 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_106])])).
fof(f1155, plain, (spl20_121 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_121])])).
fof(f7264, plain, (~ (e10 = op1(e10, e13)) | ~ spl20_121), inference(backward_demodulation, [], [f289, f1157])).
fof(f1157, plain, ((e10 = op1(e10, e10)) | ~ spl20_121), inference(avatar_component_clause, [], [f1155])).
fof(f289, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f7257, plain, (spl20_25 | ~ spl20_126), inference(avatar_split_clause, [], [f7243, f1176, f751])).
fof(f751, plain, (spl20_25 <=> (e14 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_25])])).
fof(f1176, plain, (spl20_126 <=> (e10 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl20_126])])).
fof(f7243, plain, ((e14 = op1(e14, e10)) | ~ spl20_126), inference(backward_demodulation, [], [f99, f1178])).
fof(f1178, plain, ((e10 = unit1) | ~ spl20_126), inference(avatar_component_clause, [], [f1176])).
fof(f99, plain, (e14 = op1(e14, unit1)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e14 = unit1) | (e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)) & (e14 = op1(e14, unit1)) & (e14 = op1(unit1, e14)) & (e13 = op1(e13, unit1)) & (e13 = op1(unit1, e13)) & (e12 = op1(e12, unit1)) & (e12 = op1(unit1, e12)) & (e11 = op1(e11, unit1)) & (e11 = op1(unit1, e11)) & (e10 = op1(e10, unit1)) & (e10 = op1(unit1, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax2)).
fof(f7256, plain, (spl20_105 | ~ spl20_126), inference(avatar_split_clause, [], [f7242, f1176, f1087])).
fof(f1087, plain, (spl20_105 <=> (e14 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_105])])).
fof(f7242, plain, ((e14 = op1(e10, e14)) | ~ spl20_126), inference(backward_demodulation, [], [f98, f1178])).
fof(f98, plain, (e14 = op1(unit1, e14)), inference(cnf_transformation, [], [f2])).
fof(f7255, plain, (spl20_49 | ~ spl20_126), inference(avatar_split_clause, [], [f7241, f1176, f852])).
fof(f852, plain, (spl20_49 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_49])])).
fof(f7241, plain, ((e13 = op1(e13, e10)) | ~ spl20_126), inference(backward_demodulation, [], [f97, f1178])).
fof(f97, plain, (e13 = op1(e13, unit1)), inference(cnf_transformation, [], [f2])).
fof(f7254, plain, (spl20_109 | ~ spl20_126), inference(avatar_split_clause, [], [f7240, f1176, f1104])).
fof(f1104, plain, (spl20_109 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_109])])).
fof(f7240, plain, ((e13 = op1(e10, e13)) | ~ spl20_126), inference(backward_demodulation, [], [f96, f1178])).
fof(f96, plain, (e13 = op1(unit1, e13)), inference(cnf_transformation, [], [f2])).
fof(f7253, plain, (spl20_73 | ~ spl20_126), inference(avatar_split_clause, [], [f7239, f1176, f953])).
fof(f953, plain, (spl20_73 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_73])])).
fof(f7239, plain, ((e12 = op1(e12, e10)) | ~ spl20_126), inference(backward_demodulation, [], [f95, f1178])).
fof(f95, plain, (e12 = op1(e12, unit1)), inference(cnf_transformation, [], [f2])).
fof(f7252, plain, (spl20_113 | ~ spl20_126), inference(avatar_split_clause, [], [f7238, f1176, f1121])).
fof(f1121, plain, (spl20_113 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_113])])).
fof(f7238, plain, ((e12 = op1(e10, e12)) | ~ spl20_126), inference(backward_demodulation, [], [f94, f1178])).
fof(f94, plain, (e12 = op1(unit1, e12)), inference(cnf_transformation, [], [f2])).
fof(f7251, plain, (spl20_97 | ~ spl20_126), inference(avatar_split_clause, [], [f7237, f1176, f1054])).
fof(f1054, plain, (spl20_97 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_97])])).
fof(f7237, plain, ((e11 = op1(e11, e10)) | ~ spl20_126), inference(backward_demodulation, [], [f93, f1178])).
fof(f93, plain, (e11 = op1(e11, unit1)), inference(cnf_transformation, [], [f2])).
fof(f7250, plain, (spl20_117 | ~ spl20_126), inference(avatar_split_clause, [], [f7236, f1176, f1138])).
fof(f7236, plain, ((e11 = op1(e10, e11)) | ~ spl20_126), inference(backward_demodulation, [], [f92, f1178])).
fof(f92, plain, (e11 = op1(unit1, e11)), inference(cnf_transformation, [], [f2])).
fof(f7249, plain, (spl20_121 | ~ spl20_126), inference(avatar_split_clause, [], [f7235, f1176, f1155])).
fof(f7235, plain, ((e10 = op1(e10, e10)) | ~ spl20_126), inference(backward_demodulation, [], [f91, f1178])).
fof(f91, plain, (e10 = op1(e10, unit1)), inference(cnf_transformation, [], [f2])).
fof(f7233, plain, (~ spl20_162 | ~ spl20_165), inference(avatar_contradiction_clause, [], [f7232])).
fof(f7232, plain, ($false | (~ spl20_162 | ~ spl20_165)), inference(subsumption_resolution, [], [f7231, f453])).
fof(f453, plain, ~ (e21 = e24), inference(cnf_transformation, [], [f10])).
fof(f7231, plain, ((e21 = e24) | (~ spl20_162 | ~ spl20_165)), inference(forward_demodulation, [], [f1391, f1379])).
fof(f1379, plain, ((e21 = op2(e23, e23)) | ~ spl20_162), inference(avatar_component_clause, [], [f1377])).
fof(f1377, plain, (spl20_162 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_162])])).
fof(f1391, plain, ((e24 = op2(e23, e23)) | ~ spl20_165), inference(avatar_component_clause, [], [f1389])).
fof(f1389, plain, (spl20_165 <=> (e24 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_165])])).
fof(f7169, plain, (~ spl20_91 | ~ spl20_95), inference(avatar_contradiction_clause, [], [f7168])).
fof(f7168, plain, ($false | (~ spl20_91 | ~ spl20_95)), inference(subsumption_resolution, [], [f7167, f440])).
fof(f440, plain, ~ (e10 = e14), inference(cnf_transformation, [], [f9])).
fof(f9, plain, (~ (e13 = e14) & ~ (e12 = e14) & ~ (e12 = e13) & ~ (e11 = e14) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e14) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax9)).
fof(f7167, plain, ((e10 = e14) | (~ spl20_91 | ~ spl20_95)), inference(forward_demodulation, [], [f1047, f1031])).
fof(f1031, plain, ((e10 = op1(e11, e11)) | ~ spl20_91), inference(avatar_component_clause, [], [f1029])).
fof(f1029, plain, (spl20_91 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_91])])).
fof(f1047, plain, ((e14 = op1(e11, e11)) | ~ spl20_95), inference(avatar_component_clause, [], [f1045])).
fof(f1045, plain, (spl20_95 <=> (e14 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_95])])).
fof(f7030, plain, (~ spl20_93 | ~ spl20_95), inference(avatar_contradiction_clause, [], [f7029])).
fof(f7029, plain, ($false | (~ spl20_93 | ~ spl20_95)), inference(subsumption_resolution, [], [f7027, f445])).
fof(f445, plain, ~ (e12 = e14), inference(cnf_transformation, [], [f9])).
fof(f7027, plain, ((e12 = e14) | (~ spl20_93 | ~ spl20_95)), inference(backward_demodulation, [], [f1047, f1039])).
fof(f1039, plain, ((e12 = op1(e11, e11)) | ~ spl20_93), inference(avatar_component_clause, [], [f1037])).
fof(f1037, plain, (spl20_93 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_93])])).
fof(f6726, plain, (~ spl20_1 | ~ spl20_3), inference(avatar_contradiction_clause, [], [f6725])).
fof(f6725, plain, ($false | (~ spl20_1 | ~ spl20_3)), inference(subsumption_resolution, [], [f6724, f438])).
fof(f438, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f9])).
fof(f6724, plain, ((e10 = e12) | (~ spl20_1 | ~ spl20_3)), inference(backward_demodulation, [], [f661, f653])).
fof(f653, plain, ((e10 = op1(e14, e14)) | ~ spl20_1), inference(avatar_component_clause, [], [f651])).
fof(f651, plain, (spl20_1 <=> (e10 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_1])])).
fof(f661, plain, ((e12 = op1(e14, e14)) | ~ spl20_3), inference(avatar_component_clause, [], [f659])).
fof(f659, plain, (spl20_3 <=> (e12 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_3])])).
fof(f6634, plain, (~ spl20_73 | ~ spl20_98), inference(avatar_split_clause, [], [f6625, f1058, f953])).
fof(f1058, plain, (spl20_98 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_98])])).
fof(f6625, plain, (~ (e12 = op1(e12, e10)) | ~ spl20_98), inference(backward_demodulation, [], [f241, f1060])).
fof(f1060, plain, ((e12 = op1(e11, e10)) | ~ spl20_98), inference(avatar_component_clause, [], [f1058])).
fof(f241, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f6568, plain, (spl20_10 | ~ spl20_129), inference(avatar_split_clause, [], [f6552, f1188, f688])).
fof(f688, plain, (spl20_10 <=> (e14 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_10])])).
fof(f1188, plain, (spl20_129 <=> (e13 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl20_129])])).
fof(f6552, plain, ((e14 = op1(e14, e13)) | ~ spl20_129), inference(backward_demodulation, [], [f99, f1190])).
fof(f1190, plain, ((e13 = unit1) | ~ spl20_129), inference(avatar_component_clause, [], [f1188])).
fof(f6555, plain, (spl20_106 | ~ spl20_129), inference(avatar_split_clause, [], [f6544, f1188, f1092])).
fof(f6544, plain, ((e10 = op1(e10, e13)) | ~ spl20_129), inference(backward_demodulation, [], [f91, f1190])).
fof(f6515, plain, (~ spl20_118 | ~ spl20_113), inference(avatar_split_clause, [], [f6514, f1121, f1142])).
fof(f1142, plain, (spl20_118 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_118])])).
fof(f6514, plain, (~ (e12 = op1(e10, e11)) | ~ spl20_113), inference(forward_demodulation, [], [f291, f1123])).
fof(f1123, plain, ((e12 = op1(e10, e12)) | ~ spl20_113), inference(avatar_component_clause, [], [f1121])).
fof(f291, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f7])).
fof(f6513, plain, (~ spl20_119 | ~ spl20_109), inference(avatar_split_clause, [], [f6512, f1104, f1146])).
fof(f1146, plain, (spl20_119 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_119])])).
fof(f6512, plain, (~ (e13 = op1(e10, e11)) | ~ spl20_109), inference(forward_demodulation, [], [f292, f1106])).
fof(f1106, plain, ((e13 = op1(e10, e13)) | ~ spl20_109), inference(avatar_component_clause, [], [f1104])).
fof(f292, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f7])).
fof(f6426, plain, (~ spl20_113 | ~ spl20_243 | ~ spl20_332 | spl20_464), inference(avatar_contradiction_clause, [], [f6425])).
fof(f6425, plain, ($false | (~ spl20_113 | ~ spl20_243 | ~ spl20_332 | spl20_464)), inference(subsumption_resolution, [], [f6424, f1719])).
fof(f1719, plain, ((e22 = op2(e20, e22)) | ~ spl20_243), inference(avatar_component_clause, [], [f1717])).
fof(f1717, plain, (spl20_243 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_243])])).
fof(f6424, plain, (~ (e22 = op2(e20, e22)) | (~ spl20_113 | ~ spl20_332 | spl20_464)), inference(forward_demodulation, [], [f6423, f1895])).
fof(f1895, plain, (e22 = h2(e12)), inference(forward_demodulation, [], [f1894, f1883])).
fof(f1894, plain, (h2(e12) = op2(h2(e14), h2(e14))), inference(forward_demodulation, [], [f507, f509])).
fof(f507, plain, (op2(op2(e21, e21), op2(e21, e21)) = h2(e12)), inference(cnf_transformation, [], [f17])).
fof(f6423, plain, (~ (op2(e20, e22) = h2(e12)) | (~ spl20_113 | ~ spl20_332 | spl20_464)), inference(forward_demodulation, [], [f6422, f1123])).
fof(f6422, plain, (~ (op2(e20, e22) = h2(op1(e10, e12))) | (~ spl20_332 | spl20_464)), inference(forward_demodulation, [], [f3107, f2311])).
fof(f3107, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | spl20_464), inference(avatar_component_clause, [], [f3105])).
fof(f3105, plain, (spl20_464 <=> (h2(op1(e10, e12)) = op2(h2(e10), e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_464])])).
fof(f6386, plain, (~ spl20_95 | spl20_462), inference(avatar_contradiction_clause, [], [f6385])).
fof(f6385, plain, ($false | (~ spl20_95 | spl20_462)), inference(trivial_inequality_removal, [], [f6384])).
fof(f6384, plain, (~ (h2(e14) = h2(e14)) | (~ spl20_95 | spl20_462)), inference(forward_demodulation, [], [f3099, f1047])).
fof(f3099, plain, (~ (h2(e14) = h2(op1(e11, e11))) | spl20_462), inference(avatar_component_clause, [], [f3097])).
fof(f3097, plain, (spl20_462 <=> (h2(e14) = h2(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_462])])).
fof(f6349, plain, (~ spl20_83 | ~ spl20_213 | ~ spl20_302 | spl20_460), inference(avatar_contradiction_clause, [], [f6348])).
fof(f6348, plain, ($false | (~ spl20_83 | ~ spl20_213 | ~ spl20_302 | spl20_460)), inference(subsumption_resolution, [], [f6347, f1593])).
fof(f1593, plain, ((e22 = op2(e21, e23)) | ~ spl20_213), inference(avatar_component_clause, [], [f1591])).
fof(f1591, plain, (spl20_213 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_213])])).
fof(f6347, plain, (~ (e22 = op2(e21, e23)) | (~ spl20_83 | ~ spl20_302 | spl20_460)), inference(forward_demodulation, [], [f6346, f1895])).
fof(f6346, plain, (~ (op2(e21, e23) = h2(e12)) | (~ spl20_83 | ~ spl20_302 | spl20_460)), inference(forward_demodulation, [], [f6345, f997])).
fof(f997, plain, ((e12 = op1(e11, e13)) | ~ spl20_83), inference(avatar_component_clause, [], [f995])).
fof(f995, plain, (spl20_83 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_83])])).
fof(f6345, plain, (~ (op2(e21, e23) = h2(op1(e11, e13))) | (~ spl20_302 | spl20_460)), inference(forward_demodulation, [], [f3091, f2146])).
fof(f3091, plain, (~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | spl20_460), inference(avatar_component_clause, [], [f3089])).
fof(f3089, plain, (spl20_460 <=> (h2(op1(e11, e13)) = op2(e21, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_460])])).
fof(f6315, plain, (~ spl20_79 | ~ spl20_209 | ~ spl20_302 | ~ spl20_447 | spl20_459), inference(avatar_contradiction_clause, [], [f6314])).
fof(f6314, plain, ($false | (~ spl20_79 | ~ spl20_209 | ~ spl20_302 | ~ spl20_447 | spl20_459)), inference(subsumption_resolution, [], [f6313, f1576])).
fof(f1576, plain, ((e23 = op2(e21, e24)) | ~ spl20_209), inference(avatar_component_clause, [], [f1574])).
fof(f1574, plain, (spl20_209 <=> (e23 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_209])])).
fof(f6313, plain, (~ (e23 = op2(e21, e24)) | (~ spl20_79 | ~ spl20_302 | ~ spl20_447 | spl20_459)), inference(forward_demodulation, [], [f6312, f2146])).
fof(f6312, plain, (~ (op2(e21, e24) = h2(e13)) | (~ spl20_79 | ~ spl20_447 | spl20_459)), inference(forward_demodulation, [], [f6311, f980])).
fof(f980, plain, ((e13 = op1(e11, e14)) | ~ spl20_79), inference(avatar_component_clause, [], [f978])).
fof(f978, plain, (spl20_79 <=> (e13 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_79])])).
fof(f6311, plain, (~ (op2(e21, e24) = h2(op1(e11, e14))) | (~ spl20_447 | spl20_459)), inference(forward_demodulation, [], [f3087, f3038])).
fof(f3038, plain, ((e24 = h2(e14)) | ~ spl20_447), inference(avatar_component_clause, [], [f3037])).
fof(f3037, plain, (spl20_447 <=> (e24 = h2(e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_447])])).
fof(f3087, plain, (~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | spl20_459), inference(avatar_component_clause, [], [f3085])).
fof(f3085, plain, (spl20_459 <=> (h2(op1(e11, e14)) = op2(e21, h2(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl20_459])])).
fof(f6275, plain, (~ spl20_97 | ~ spl20_227 | ~ spl20_332 | spl20_463), inference(avatar_contradiction_clause, [], [f6274])).
fof(f6274, plain, ($false | (~ spl20_97 | ~ spl20_227 | ~ spl20_332 | spl20_463)), inference(subsumption_resolution, [], [f6273, f1652])).
fof(f1652, plain, ((e21 = op2(e21, e20)) | ~ spl20_227), inference(avatar_component_clause, [], [f1650])).
fof(f1650, plain, (spl20_227 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_227])])).
fof(f6273, plain, (~ (e21 = op2(e21, e20)) | (~ spl20_97 | ~ spl20_332 | spl20_463)), inference(forward_demodulation, [], [f6272, f505])).
fof(f505, plain, (e21 = h2(e11)), inference(cnf_transformation, [], [f17])).
fof(f6272, plain, (~ (op2(e21, e20) = h2(e11)) | (~ spl20_97 | ~ spl20_332 | spl20_463)), inference(forward_demodulation, [], [f6271, f1056])).
fof(f1056, plain, ((e11 = op1(e11, e10)) | ~ spl20_97), inference(avatar_component_clause, [], [f1054])).
fof(f6271, plain, (~ (op2(e21, e20) = h2(op1(e11, e10))) | (~ spl20_332 | spl20_463)), inference(forward_demodulation, [], [f3103, f2311])).
fof(f3103, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | spl20_463), inference(avatar_component_clause, [], [f3101])).
fof(f3101, plain, (spl20_463 <=> (h2(op1(e11, e10)) = op2(e21, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_463])])).
fof(f6240, plain, (~ spl20_73 | ~ spl20_203 | ~ spl20_332 | spl20_458), inference(avatar_contradiction_clause, [], [f6239])).
fof(f6239, plain, ($false | (~ spl20_73 | ~ spl20_203 | ~ spl20_332 | spl20_458)), inference(subsumption_resolution, [], [f6238, f1551])).
fof(f1551, plain, ((e22 = op2(e22, e20)) | ~ spl20_203), inference(avatar_component_clause, [], [f1549])).
fof(f1549, plain, (spl20_203 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_203])])).
fof(f6238, plain, (~ (e22 = op2(e22, e20)) | (~ spl20_73 | ~ spl20_332 | spl20_458)), inference(forward_demodulation, [], [f6237, f1895])).
fof(f6237, plain, (~ (op2(e22, e20) = h2(e12)) | (~ spl20_73 | ~ spl20_332 | spl20_458)), inference(forward_demodulation, [], [f6236, f955])).
fof(f955, plain, ((e12 = op1(e12, e10)) | ~ spl20_73), inference(avatar_component_clause, [], [f953])).
fof(f6236, plain, (~ (op2(e22, e20) = h2(op1(e12, e10))) | (~ spl20_332 | spl20_458)), inference(forward_demodulation, [], [f3083, f2311])).
fof(f3083, plain, (~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | spl20_458), inference(avatar_component_clause, [], [f3081])).
fof(f3081, plain, (spl20_458 <=> (h2(op1(e12, e10)) = op2(e22, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_458])])).
fof(f6160, plain, (~ spl20_64 | spl20_456), inference(avatar_contradiction_clause, [], [f6159])).
fof(f6159, plain, ($false | (~ spl20_64 | spl20_456)), inference(trivial_inequality_removal, [], [f6158])).
fof(f6158, plain, (~ (h2(e13) = h2(e13)) | (~ spl20_64 | spl20_456)), inference(forward_demodulation, [], [f3075, f917])).
fof(f917, plain, ((e13 = op1(e12, e12)) | ~ spl20_64), inference(avatar_component_clause, [], [f915])).
fof(f915, plain, (spl20_64 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_64])])).
fof(f3075, plain, (~ (h2(e13) = h2(op1(e12, e12))) | spl20_456), inference(avatar_component_clause, [], [f3073])).
fof(f3073, plain, (spl20_456 <=> (h2(e13) = h2(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_456])])).
fof(f6119, plain, (~ spl20_52 | ~ spl20_182 | ~ spl20_447 | spl20_454), inference(avatar_contradiction_clause, [], [f6118])).
fof(f6118, plain, ($false | (~ spl20_52 | ~ spl20_182 | ~ spl20_447 | spl20_454)), inference(subsumption_resolution, [], [f6117, f1463])).
fof(f1463, plain, ((e21 = op2(e22, e24)) | ~ spl20_182), inference(avatar_component_clause, [], [f1461])).
fof(f6117, plain, (~ (e21 = op2(e22, e24)) | (~ spl20_52 | ~ spl20_447 | spl20_454)), inference(forward_demodulation, [], [f6116, f505])).
fof(f6116, plain, (~ (op2(e22, e24) = h2(e11)) | (~ spl20_52 | ~ spl20_447 | spl20_454)), inference(forward_demodulation, [], [f6115, f867])).
fof(f867, plain, ((e11 = op1(e12, e14)) | ~ spl20_52), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl20_52 <=> (e11 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_52])])).
fof(f6115, plain, (~ (op2(e22, e24) = h2(op1(e12, e14))) | (~ spl20_447 | spl20_454)), inference(forward_demodulation, [], [f3067, f3038])).
fof(f3067, plain, (~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | spl20_454), inference(avatar_component_clause, [], [f3065])).
fof(f3065, plain, (spl20_454 <=> (h2(op1(e12, e14)) = op2(e22, h2(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl20_454])])).
fof(f6077, plain, (~ spl20_43 | ~ spl20_173 | ~ spl20_302 | spl20_453), inference(avatar_contradiction_clause, [], [f6076])).
fof(f6076, plain, ($false | (~ spl20_43 | ~ spl20_173 | ~ spl20_302 | spl20_453)), inference(subsumption_resolution, [], [f6075, f1425])).
fof(f6075, plain, (~ (e22 = op2(e23, e21)) | (~ spl20_43 | ~ spl20_302 | spl20_453)), inference(forward_demodulation, [], [f6074, f1895])).
fof(f6074, plain, (~ (op2(e23, e21) = h2(e12)) | (~ spl20_43 | ~ spl20_302 | spl20_453)), inference(forward_demodulation, [], [f6073, f829])).
fof(f829, plain, ((e12 = op1(e13, e11)) | ~ spl20_43), inference(avatar_component_clause, [], [f827])).
fof(f827, plain, (spl20_43 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_43])])).
fof(f6073, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | (~ spl20_302 | spl20_453)), inference(forward_demodulation, [], [f3063, f2146])).
fof(f3063, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | spl20_453), inference(avatar_component_clause, [], [f3061])).
fof(f3061, plain, (spl20_453 <=> (h2(op1(e13, e11)) = op2(h2(e13), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_453])])).
fof(f6036, plain, (~ spl20_32 | ~ spl20_314 | spl20_451), inference(avatar_contradiction_clause, [], [f6035])).
fof(f6035, plain, ($false | (~ spl20_32 | ~ spl20_314 | spl20_451)), inference(subsumption_resolution, [], [f6034, f505])).
fof(f6034, plain, (~ (e21 = h2(e11)) | (~ spl20_32 | ~ spl20_314 | spl20_451)), inference(forward_demodulation, [], [f6033, f2209])).
fof(f6033, plain, (~ (h2(e11) = h3(e12)) | (~ spl20_32 | spl20_451)), inference(forward_demodulation, [], [f3055, f783])).
fof(f783, plain, ((e11 = op1(e13, e13)) | ~ spl20_32), inference(avatar_component_clause, [], [f781])).
fof(f781, plain, (spl20_32 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_32])])).
fof(f3055, plain, (~ (h3(e12) = h2(op1(e13, e13))) | spl20_451), inference(avatar_component_clause, [], [f3053])).
fof(f3053, plain, (spl20_451 <=> (h3(e12) = h2(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_451])])).
fof(f6002, plain, (~ spl20_19 | ~ spl20_149 | ~ spl20_302 | ~ spl20_447 | spl20_450), inference(avatar_contradiction_clause, [], [f6001])).
fof(f6001, plain, ($false | (~ spl20_19 | ~ spl20_149 | ~ spl20_302 | ~ spl20_447 | spl20_450)), inference(subsumption_resolution, [], [f6000, f1324])).
fof(f1324, plain, ((e23 = op2(e24, e21)) | ~ spl20_149), inference(avatar_component_clause, [], [f1322])).
fof(f1322, plain, (spl20_149 <=> (e23 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_149])])).
fof(f6000, plain, (~ (e23 = op2(e24, e21)) | (~ spl20_19 | ~ spl20_302 | ~ spl20_447 | spl20_450)), inference(forward_demodulation, [], [f5999, f2146])).
fof(f5999, plain, (~ (op2(e24, e21) = h2(e13)) | (~ spl20_19 | ~ spl20_447 | spl20_450)), inference(forward_demodulation, [], [f5998, f728])).
fof(f728, plain, ((e13 = op1(e14, e11)) | ~ spl20_19), inference(avatar_component_clause, [], [f726])).
fof(f726, plain, (spl20_19 <=> (e13 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_19])])).
fof(f5998, plain, (~ (op2(e24, e21) = h2(op1(e14, e11))) | (~ spl20_447 | spl20_450)), inference(forward_demodulation, [], [f3051, f3038])).
fof(f3051, plain, (~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | spl20_450), inference(avatar_component_clause, [], [f3049])).
fof(f3049, plain, (spl20_450 <=> (h2(op1(e14, e11)) = op2(h2(e14), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_450])])).
fof(f5963, plain, (~ spl20_117 | ~ spl20_247 | ~ spl20_332 | spl20_465), inference(avatar_contradiction_clause, [], [f5962])).
fof(f5962, plain, ($false | (~ spl20_117 | ~ spl20_247 | ~ spl20_332 | spl20_465)), inference(subsumption_resolution, [], [f5961, f1736])).
fof(f1736, plain, ((e21 = op2(e20, e21)) | ~ spl20_247), inference(avatar_component_clause, [], [f1734])).
fof(f1734, plain, (spl20_247 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_247])])).
fof(f5961, plain, (~ (e21 = op2(e20, e21)) | (~ spl20_117 | ~ spl20_332 | spl20_465)), inference(forward_demodulation, [], [f5960, f505])).
fof(f5960, plain, (~ (op2(e20, e21) = h2(e11)) | (~ spl20_117 | ~ spl20_332 | spl20_465)), inference(forward_demodulation, [], [f5959, f1140])).
fof(f5959, plain, (~ (op2(e20, e21) = h2(op1(e10, e11))) | (~ spl20_332 | spl20_465)), inference(forward_demodulation, [], [f3111, f2311])).
fof(f3111, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | spl20_465), inference(avatar_component_clause, [], [f3109])).
fof(f3109, plain, (spl20_465 <=> (h2(op1(e10, e11)) = op2(h2(e10), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_465])])).
fof(f5927, plain, (~ spl20_86 | spl20_461), inference(avatar_contradiction_clause, [], [f5926])).
fof(f5926, plain, ($false | (~ spl20_86 | spl20_461)), inference(trivial_inequality_removal, [], [f5925])).
fof(f5925, plain, (~ (h2(e10) = h2(e10)) | (~ spl20_86 | spl20_461)), inference(forward_demodulation, [], [f3095, f1010])).
fof(f1010, plain, ((e10 = op1(e11, e12)) | ~ spl20_86), inference(avatar_component_clause, [], [f1008])).
fof(f1008, plain, (spl20_86 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_86])])).
fof(f3095, plain, (~ (h2(e10) = h2(op1(e11, e12))) | spl20_461), inference(avatar_component_clause, [], [f3093])).
fof(f3093, plain, (spl20_461 <=> (h2(e10) = h2(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_461])])).
fof(f5888, plain, (~ spl20_60 | ~ spl20_190 | ~ spl20_302 | ~ spl20_447 | spl20_455), inference(avatar_contradiction_clause, [], [f5887])).
fof(f5887, plain, ($false | (~ spl20_60 | ~ spl20_190 | ~ spl20_302 | ~ spl20_447 | spl20_455)), inference(subsumption_resolution, [], [f5886, f1496])).
fof(f1496, plain, ((e24 = op2(e22, e23)) | ~ spl20_190), inference(avatar_component_clause, [], [f1494])).
fof(f1494, plain, (spl20_190 <=> (e24 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_190])])).
fof(f5886, plain, (~ (e24 = op2(e22, e23)) | (~ spl20_60 | ~ spl20_302 | ~ spl20_447 | spl20_455)), inference(forward_demodulation, [], [f5885, f3038])).
fof(f5885, plain, (~ (op2(e22, e23) = h2(e14)) | (~ spl20_60 | ~ spl20_302 | spl20_455)), inference(forward_demodulation, [], [f5884, f900])).
fof(f900, plain, ((e14 = op1(e12, e13)) | ~ spl20_60), inference(avatar_component_clause, [], [f898])).
fof(f898, plain, (spl20_60 <=> (e14 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_60])])).
fof(f5884, plain, (~ (op2(e22, e23) = h2(op1(e12, e13))) | (~ spl20_302 | spl20_455)), inference(forward_demodulation, [], [f3071, f2146])).
fof(f3071, plain, (~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | spl20_455), inference(avatar_component_clause, [], [f3069])).
fof(f3069, plain, (spl20_455 <=> (h2(op1(e12, e13)) = op2(e22, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_455])])).
fof(f5845, plain, (~ spl20_40 | ~ spl20_170 | ~ spl20_302 | ~ spl20_447 | spl20_452), inference(avatar_contradiction_clause, [], [f5844])).
fof(f5844, plain, ($false | (~ spl20_40 | ~ spl20_170 | ~ spl20_302 | ~ spl20_447 | spl20_452)), inference(subsumption_resolution, [], [f5843, f1412])).
fof(f1412, plain, ((e24 = op2(e23, e22)) | ~ spl20_170), inference(avatar_component_clause, [], [f1410])).
fof(f1410, plain, (spl20_170 <=> (e24 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_170])])).
fof(f5843, plain, (~ (e24 = op2(e23, e22)) | (~ spl20_40 | ~ spl20_302 | ~ spl20_447 | spl20_452)), inference(forward_demodulation, [], [f5842, f3038])).
fof(f5842, plain, (~ (op2(e23, e22) = h2(e14)) | (~ spl20_40 | ~ spl20_302 | spl20_452)), inference(forward_demodulation, [], [f5841, f816])).
fof(f816, plain, ((e14 = op1(e13, e12)) | ~ spl20_40), inference(avatar_component_clause, [], [f814])).
fof(f814, plain, (spl20_40 <=> (e14 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_40])])).
fof(f5841, plain, (~ (op2(e23, e22) = h2(op1(e13, e12))) | (~ spl20_302 | spl20_452)), inference(forward_demodulation, [], [f3059, f2146])).
fof(f3059, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | spl20_452), inference(avatar_component_clause, [], [f3057])).
fof(f3057, plain, (spl20_452 <=> (h2(op1(e13, e12)) = op2(h2(e13), e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_452])])).
fof(f5806, plain, (~ spl20_12 | ~ spl20_142 | ~ spl20_447 | spl20_449), inference(avatar_contradiction_clause, [], [f5805])).
fof(f5805, plain, ($false | (~ spl20_12 | ~ spl20_142 | ~ spl20_447 | spl20_449)), inference(subsumption_resolution, [], [f5804, f1295])).
fof(f1295, plain, ((e21 = op2(e24, e22)) | ~ spl20_142), inference(avatar_component_clause, [], [f1293])).
fof(f1293, plain, (spl20_142 <=> (e21 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_142])])).
fof(f5804, plain, (~ (e21 = op2(e24, e22)) | (~ spl20_12 | ~ spl20_447 | spl20_449)), inference(forward_demodulation, [], [f5803, f505])).
fof(f5803, plain, (~ (op2(e24, e22) = h2(e11)) | (~ spl20_12 | ~ spl20_447 | spl20_449)), inference(forward_demodulation, [], [f5802, f699])).
fof(f699, plain, ((e11 = op1(e14, e12)) | ~ spl20_12), inference(avatar_component_clause, [], [f697])).
fof(f697, plain, (spl20_12 <=> (e11 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_12])])).
fof(f5802, plain, (~ (op2(e24, e22) = h2(op1(e14, e12))) | (~ spl20_447 | spl20_449)), inference(forward_demodulation, [], [f3047, f3038])).
fof(f3047, plain, (~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | spl20_449), inference(avatar_component_clause, [], [f3045])).
fof(f3045, plain, (spl20_449 <=> (h2(op1(e14, e12)) = op2(h2(e14), e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_449])])).
fof(f5760, plain, (~ spl20_3 | spl20_448), inference(avatar_contradiction_clause, [], [f5759])).
fof(f5759, plain, ($false | (~ spl20_3 | spl20_448)), inference(subsumption_resolution, [], [f5758, f1895])).
fof(f5758, plain, (~ (e22 = h2(e12)) | (~ spl20_3 | spl20_448)), inference(forward_demodulation, [], [f3043, f661])).
fof(f3043, plain, (~ (e22 = h2(op1(e14, e14))) | spl20_448), inference(avatar_component_clause, [], [f3041])).
fof(f3041, plain, (spl20_448 <=> (e22 = h2(op1(e14, e14)))), introduced(avatar_definition, [new_symbols(naming, [spl20_448])])).
fof(f5703, plain, (~ spl20_25 | ~ spl20_155 | ~ spl20_332 | spl20_445 | ~ spl20_447), inference(avatar_contradiction_clause, [], [f5702])).
fof(f5702, plain, ($false | (~ spl20_25 | ~ spl20_155 | ~ spl20_332 | spl20_445 | ~ spl20_447)), inference(subsumption_resolution, [], [f5701, f1349])).
fof(f1349, plain, ((e24 = op2(e24, e20)) | ~ spl20_155), inference(avatar_component_clause, [], [f1347])).
fof(f1347, plain, (spl20_155 <=> (e24 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_155])])).
fof(f5701, plain, (~ (e24 = op2(e24, e20)) | (~ spl20_25 | ~ spl20_332 | spl20_445 | ~ spl20_447)), inference(forward_demodulation, [], [f5700, f3038])).
fof(f5700, plain, (~ (op2(e24, e20) = h2(e14)) | (~ spl20_25 | ~ spl20_332 | spl20_445 | ~ spl20_447)), inference(forward_demodulation, [], [f5699, f753])).
fof(f753, plain, ((e14 = op1(e14, e10)) | ~ spl20_25), inference(avatar_component_clause, [], [f751])).
fof(f5699, plain, (~ (op2(e24, e20) = h2(op1(e14, e10))) | (~ spl20_332 | spl20_445 | ~ spl20_447)), inference(forward_demodulation, [], [f5698, f3038])).
fof(f5698, plain, (~ (h2(op1(e14, e10)) = op2(h2(e14), e20)) | (~ spl20_332 | spl20_445)), inference(forward_demodulation, [], [f3031, f2311])).
fof(f3031, plain, (~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | spl20_445), inference(avatar_component_clause, [], [f3029])).
fof(f3029, plain, (spl20_445 <=> (h2(op1(e14, e10)) = op2(h2(e14), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_445])])).
fof(f5684, plain, (~ spl20_73 | ~ spl20_75), inference(avatar_contradiction_clause, [], [f5683])).
fof(f5683, plain, ($false | (~ spl20_73 | ~ spl20_75)), inference(subsumption_resolution, [], [f5682, f445])).
fof(f5682, plain, ((e12 = e14) | (~ spl20_73 | ~ spl20_75)), inference(forward_demodulation, [], [f963, f955])).
fof(f963, plain, ((e14 = op1(e12, e10)) | ~ spl20_75), inference(avatar_component_clause, [], [f961])).
fof(f961, plain, (spl20_75 <=> (e14 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_75])])).
fof(f5668, plain, (~ spl20_232 | ~ spl20_235), inference(avatar_contradiction_clause, [], [f5667])).
fof(f5667, plain, ($false | (~ spl20_232 | ~ spl20_235)), inference(subsumption_resolution, [], [f5665, f453])).
fof(f5665, plain, ((e21 = e24) | (~ spl20_232 | ~ spl20_235)), inference(backward_demodulation, [], [f1685, f1673])).
fof(f1673, plain, ((e21 = op2(e20, e24)) | ~ spl20_232), inference(avatar_component_clause, [], [f1671])).
fof(f1671, plain, (spl20_232 <=> (e21 = op2(e20, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_232])])).
fof(f5650, plain, (~ spl20_26 | ~ spl20_156 | ~ spl20_302 | ~ spl20_332 | spl20_444 | ~ spl20_447), inference(avatar_contradiction_clause, [], [f5649])).
fof(f5649, plain, ($false | (~ spl20_26 | ~ spl20_156 | ~ spl20_302 | ~ spl20_332 | spl20_444 | ~ spl20_447)), inference(subsumption_resolution, [], [f5648, f1354])).
fof(f1354, plain, ((e20 = op2(e23, e24)) | ~ spl20_156), inference(avatar_component_clause, [], [f1352])).
fof(f1352, plain, (spl20_156 <=> (e20 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_156])])).
fof(f5648, plain, (~ (e20 = op2(e23, e24)) | (~ spl20_26 | ~ spl20_302 | ~ spl20_332 | spl20_444 | ~ spl20_447)), inference(forward_demodulation, [], [f5647, f2311])).
fof(f5647, plain, (~ (op2(e23, e24) = h2(e10)) | (~ spl20_26 | ~ spl20_302 | spl20_444 | ~ spl20_447)), inference(forward_demodulation, [], [f5646, f758])).
fof(f758, plain, ((e10 = op1(e13, e14)) | ~ spl20_26), inference(avatar_component_clause, [], [f756])).
fof(f756, plain, (spl20_26 <=> (e10 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_26])])).
fof(f5646, plain, (~ (op2(e23, e24) = h2(op1(e13, e14))) | (~ spl20_302 | spl20_444 | ~ spl20_447)), inference(forward_demodulation, [], [f5645, f2146])).
fof(f5645, plain, (~ (h2(op1(e13, e14)) = op2(h2(e13), e24)) | (spl20_444 | ~ spl20_447)), inference(forward_demodulation, [], [f3027, f3038])).
fof(f3027, plain, (~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | spl20_444), inference(avatar_component_clause, [], [f3025])).
fof(f3025, plain, (spl20_444 <=> (h2(op1(e13, e14)) = op2(h2(e13), h2(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl20_444])])).
fof(f5630, plain, (~ spl20_206 | ~ spl20_332), inference(avatar_split_clause, [], [f5629, f2310, f1562])).
fof(f1562, plain, (spl20_206 <=> (e20 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_206])])).
fof(f5629, plain, (~ (e20 = op2(e21, e24)) | ~ spl20_332), inference(forward_demodulation, [], [f1903, f2311])).
fof(f1903, plain, ~ (op2(e21, e24) = h2(e10)), inference(backward_demodulation, [], [f405, f1897])).
fof(f1897, plain, (op2(e21, e22) = h2(e10)), inference(forward_demodulation, [], [f1896, f1883])).
fof(f1896, plain, (h2(e10) = op2(e21, op2(h2(e14), h2(e14)))), inference(forward_demodulation, [], [f506, f509])).
fof(f506, plain, (op2(e21, op2(op2(e21, e21), op2(e21, e21))) = h2(e10)), inference(cnf_transformation, [], [f17])).
fof(f405, plain, ~ (op2(e21, e22) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f5628, plain, (~ spl20_208 | ~ spl20_267), inference(avatar_split_clause, [], [f5627, f1968, f1570])).
fof(f1570, plain, (spl20_208 <=> (e22 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_208])])).
fof(f1968, plain, (spl20_267 <=> (e22 = h5(e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_267])])).
fof(f5627, plain, (~ (e22 = op2(e21, e24)) | ~ spl20_267), inference(forward_demodulation, [], [f1926, f1969])).
fof(f1969, plain, ((e22 = h5(e14)) | ~ spl20_267), inference(avatar_component_clause, [], [f1968])).
fof(f1926, plain, ~ (op2(e21, e24) = h5(e14)), inference(backward_demodulation, [], [f383, f524])).
fof(f524, plain, (op2(e24, e24) = h5(e14)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((op2(e24, e24) = h5(e14)) & (h5(e13) = op2(op2(op2(e24, e24), op2(e24, e24)), op2(op2(e24, e24), op2(e24, e24)))) & (op2(op2(e24, e24), op2(e24, e24)) = h5(e12)) & (h5(e10) = op2(e24, op2(op2(e24, e24), op2(e24, e24)))) & (e24 = h5(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax20)).
fof(f383, plain, ~ (op2(e21, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f5626, plain, (~ spl20_210 | ~ spl20_235), inference(avatar_split_clause, [], [f5625, f1683, f1578])).
fof(f1578, plain, (spl20_210 <=> (e24 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_210])])).
fof(f5625, plain, (~ (e24 = op2(e21, e24)) | ~ spl20_235), inference(forward_demodulation, [], [f377, f1685])).
fof(f377, plain, ~ (op2(e20, e24) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f5576, plain, (~ spl20_105 | ~ spl20_235 | ~ spl20_332 | spl20_442 | ~ spl20_447), inference(avatar_contradiction_clause, [], [f5575])).
fof(f5575, plain, ($false | (~ spl20_105 | ~ spl20_235 | ~ spl20_332 | spl20_442 | ~ spl20_447)), inference(subsumption_resolution, [], [f5574, f1685])).
fof(f5574, plain, (~ (e24 = op2(e20, e24)) | (~ spl20_105 | ~ spl20_332 | spl20_442 | ~ spl20_447)), inference(forward_demodulation, [], [f5573, f3038])).
fof(f5573, plain, (~ (op2(e20, e24) = h2(e14)) | (~ spl20_105 | ~ spl20_332 | spl20_442 | ~ spl20_447)), inference(forward_demodulation, [], [f5572, f1089])).
fof(f1089, plain, ((e14 = op1(e10, e14)) | ~ spl20_105), inference(avatar_component_clause, [], [f1087])).
fof(f5572, plain, (~ (op2(e20, e24) = h2(op1(e10, e14))) | (~ spl20_332 | spl20_442 | ~ spl20_447)), inference(forward_demodulation, [], [f5571, f2311])).
fof(f5571, plain, (~ (h2(op1(e10, e14)) = op2(h2(e10), e24)) | (spl20_442 | ~ spl20_447)), inference(forward_demodulation, [], [f3019, f3038])).
fof(f3019, plain, (~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | spl20_442), inference(avatar_component_clause, [], [f3017])).
fof(f3017, plain, (spl20_442 <=> (h2(op1(e10, e14)) = op2(h2(e10), h2(e14)))), introduced(avatar_definition, [new_symbols(naming, [spl20_442])])).
fof(f5528, plain, (~ spl20_109 | ~ spl20_239 | ~ spl20_302 | ~ spl20_332 | spl20_441), inference(avatar_contradiction_clause, [], [f5527])).
fof(f5527, plain, ($false | (~ spl20_109 | ~ spl20_239 | ~ spl20_302 | ~ spl20_332 | spl20_441)), inference(subsumption_resolution, [], [f5526, f1702])).
fof(f1702, plain, ((e23 = op2(e20, e23)) | ~ spl20_239), inference(avatar_component_clause, [], [f1700])).
fof(f1700, plain, (spl20_239 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_239])])).
fof(f5526, plain, (~ (e23 = op2(e20, e23)) | (~ spl20_109 | ~ spl20_302 | ~ spl20_332 | spl20_441)), inference(forward_demodulation, [], [f5525, f2146])).
fof(f5525, plain, (~ (op2(e20, e23) = h2(e13)) | (~ spl20_109 | ~ spl20_302 | ~ spl20_332 | spl20_441)), inference(forward_demodulation, [], [f5524, f1106])).
fof(f5524, plain, (~ (op2(e20, e23) = h2(op1(e10, e13))) | (~ spl20_302 | ~ spl20_332 | spl20_441)), inference(forward_demodulation, [], [f5523, f2311])).
fof(f5523, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | (~ spl20_302 | spl20_441)), inference(forward_demodulation, [], [f3015, f2146])).
fof(f3015, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | spl20_441), inference(avatar_component_clause, [], [f3013])).
fof(f3013, plain, (spl20_441 <=> (h2(op1(e10, e13)) = op2(h2(e10), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_441])])).
fof(f5488, plain, (~ spl20_121 | ~ spl20_251 | ~ spl20_332 | spl20_440), inference(avatar_contradiction_clause, [], [f5487])).
fof(f5487, plain, ($false | (~ spl20_121 | ~ spl20_251 | ~ spl20_332 | spl20_440)), inference(subsumption_resolution, [], [f5486, f1753])).
fof(f1753, plain, ((e20 = op2(e20, e20)) | ~ spl20_251), inference(avatar_component_clause, [], [f1751])).
fof(f1751, plain, (spl20_251 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_251])])).
fof(f5486, plain, (~ (e20 = op2(e20, e20)) | (~ spl20_121 | ~ spl20_332 | spl20_440)), inference(forward_demodulation, [], [f5485, f2311])).
fof(f5485, plain, (~ (op2(e20, e20) = h2(e10)) | (~ spl20_121 | ~ spl20_332 | spl20_440)), inference(forward_demodulation, [], [f5484, f1157])).
fof(f5484, plain, (~ (op2(e20, e20) = h2(op1(e10, e10))) | (~ spl20_332 | spl20_440)), inference(forward_demodulation, [], [f3011, f2311])).
fof(f3011, plain, (~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10))) | spl20_440), inference(avatar_component_clause, [], [f3009])).
fof(f3009, plain, (spl20_440 <=> (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_440])])).
fof(f5438, plain, (~ spl20_7 | ~ spl20_32), inference(avatar_split_clause, [], [f5433, f781, f676])).
fof(f676, plain, (spl20_7 <=> (e11 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_7])])).
fof(f5433, plain, (~ (e11 = op1(e14, e13)) | ~ spl20_32), inference(backward_demodulation, [], [f276, f783])).
fof(f276, plain, ~ (op1(e13, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f5428, plain, (~ spl20_49 | ~ spl20_179 | ~ spl20_302 | ~ spl20_332 | spl20_443), inference(avatar_contradiction_clause, [], [f5427])).
fof(f5427, plain, ($false | (~ spl20_49 | ~ spl20_179 | ~ spl20_302 | ~ spl20_332 | spl20_443)), inference(subsumption_resolution, [], [f5424, f2146])).
fof(f5424, plain, (~ (e23 = h2(e13)) | (~ spl20_49 | ~ spl20_179 | ~ spl20_302 | ~ spl20_332 | spl20_443)), inference(backward_demodulation, [], [f5342, f854])).
fof(f854, plain, ((e13 = op1(e13, e10)) | ~ spl20_49), inference(avatar_component_clause, [], [f852])).
fof(f5342, plain, (~ (e23 = h2(op1(e13, e10))) | (~ spl20_179 | ~ spl20_302 | ~ spl20_332 | spl20_443)), inference(forward_demodulation, [], [f5341, f1450])).
fof(f1450, plain, ((e23 = op2(e23, e20)) | ~ spl20_179), inference(avatar_component_clause, [], [f1448])).
fof(f1448, plain, (spl20_179 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_179])])).
fof(f5341, plain, (~ (op2(e23, e20) = h2(op1(e13, e10))) | (~ spl20_302 | ~ spl20_332 | spl20_443)), inference(forward_demodulation, [], [f5340, f2146])).
fof(f5340, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | (~ spl20_332 | spl20_443)), inference(forward_demodulation, [], [f3023, f2311])).
fof(f3023, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | spl20_443), inference(avatar_component_clause, [], [f3021])).
fof(f3021, plain, (spl20_443 <=> (h2(op1(e13, e10)) = op2(h2(e13), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_443])])).
fof(f5426, plain, (~ spl20_29 | ~ spl20_49), inference(avatar_split_clause, [], [f5422, f852, f768])).
fof(f768, plain, (spl20_29 <=> (e13 = op1(e13, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_29])])).
fof(f5422, plain, (~ (e13 = op1(e13, e14)) | ~ spl20_49), inference(backward_demodulation, [], [f320, f854])).
fof(f320, plain, ~ (op1(e13, e10) = op1(e13, e14)), inference(cnf_transformation, [], [f7])).
fof(f5404, plain, (~ spl20_55 | ~ spl20_105), inference(avatar_split_clause, [], [f5402, f1087, f877])).
fof(f877, plain, (spl20_55 <=> (e14 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_55])])).
fof(f5402, plain, (~ (e14 = op1(e12, e14)) | ~ spl20_105), inference(backward_demodulation, [], [f278, f1089])).
fof(f278, plain, ~ (op1(e10, e14) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f5370, plain, (~ spl20_22 | ~ spl20_126), inference(avatar_contradiction_clause, [], [f5369])).
fof(f5369, plain, ($false | (~ spl20_22 | ~ spl20_126)), inference(subsumption_resolution, [], [f5368, f443])).
fof(f443, plain, ~ (e11 = e14), inference(cnf_transformation, [], [f9])).
fof(f5368, plain, ((e11 = e14) | (~ spl20_22 | ~ spl20_126)), inference(forward_demodulation, [], [f5356, f741])).
fof(f741, plain, ((e11 = op1(e14, e10)) | ~ spl20_22), inference(avatar_component_clause, [], [f739])).
fof(f739, plain, (spl20_22 <=> (e11 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_22])])).
fof(f5356, plain, ((e14 = op1(e14, e10)) | ~ spl20_126), inference(backward_demodulation, [], [f99, f1178])).
fof(f5334, plain, (~ spl20_105 | ~ spl20_115), inference(avatar_split_clause, [], [f5333, f1129, f1087])).
fof(f1129, plain, (spl20_115 <=> (e14 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_115])])).
fof(f5333, plain, (~ (e14 = op1(e10, e14)) | ~ spl20_115), inference(forward_demodulation, [], [f295, f1131])).
fof(f1131, plain, ((e14 = op1(e10, e12)) | ~ spl20_115), inference(avatar_component_clause, [], [f1129])).
fof(f295, plain, ~ (op1(e10, e12) = op1(e10, e14)), inference(cnf_transformation, [], [f7])).
fof(f5307, plain, (~ spl20_62 | ~ spl20_64), inference(avatar_contradiction_clause, [], [f5306])).
fof(f5306, plain, ($false | (~ spl20_62 | ~ spl20_64)), inference(subsumption_resolution, [], [f5305, f442])).
fof(f442, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f9])).
fof(f5305, plain, ((e11 = e13) | (~ spl20_62 | ~ spl20_64)), inference(backward_demodulation, [], [f917, f909])).
fof(f909, plain, ((e11 = op1(e12, e12)) | ~ spl20_62), inference(avatar_component_clause, [], [f907])).
fof(f907, plain, (spl20_62 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_62])])).
fof(f5304, plain, (~ spl20_86 | ~ spl20_88), inference(avatar_contradiction_clause, [], [f5303])).
fof(f5303, plain, ($false | (~ spl20_86 | ~ spl20_88)), inference(subsumption_resolution, [], [f5302, f438])).
fof(f5302, plain, ((e10 = e12) | (~ spl20_86 | ~ spl20_88)), inference(forward_demodulation, [], [f1018, f1010])).
fof(f1018, plain, ((e12 = op1(e11, e12)) | ~ spl20_88), inference(avatar_component_clause, [], [f1016])).
fof(f1016, plain, (spl20_88 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_88])])).
fof(f5288, plain, (~ spl20_3 | ~ spl20_130), inference(avatar_contradiction_clause, [], [f5287])).
fof(f5287, plain, ($false | (~ spl20_3 | ~ spl20_130)), inference(subsumption_resolution, [], [f5286, f445])).
fof(f5286, plain, ((e12 = e14) | (~ spl20_3 | ~ spl20_130)), inference(forward_demodulation, [], [f5273, f661])).
fof(f5273, plain, ((e14 = op1(e14, e14)) | ~ spl20_130), inference(backward_demodulation, [], [f99, f1194])).
fof(f1194, plain, ((e14 = unit1) | ~ spl20_130), inference(avatar_component_clause, [], [f1192])).
fof(f1192, plain, (spl20_130 <=> (e14 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl20_130])])).
fof(f5228, plain, (~ spl20_265 | ~ spl20_239 | ~ spl20_264), inference(avatar_split_clause, [], [f5227, f1953, f1700, f1959])).
fof(f1959, plain, (spl20_265 <=> (e23 = h5(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_265])])).
fof(f1953, plain, (spl20_264 <=> (e23 = h5(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_264])])).
fof(f5227, plain, (~ (e23 = h5(e10)) | (~ spl20_239 | ~ spl20_264)), inference(forward_demodulation, [], [f5000, f1702])).
fof(f5000, plain, (~ (op2(e20, e23) = h5(e10)) | ~ spl20_264), inference(forward_demodulation, [], [f370, f4791])).
fof(f4791, plain, ((op2(e24, e23) = h5(e10)) | ~ spl20_264), inference(backward_demodulation, [], [f1937, f1954])).
fof(f1954, plain, ((e23 = h5(e12)) | ~ spl20_264), inference(avatar_component_clause, [], [f1953])).
fof(f1937, plain, (h5(e10) = op2(e24, h5(e12))), inference(forward_demodulation, [], [f1936, f1934])).
fof(f1934, plain, (h5(e12) = op2(h5(e14), h5(e14))), inference(forward_demodulation, [], [f522, f524])).
fof(f522, plain, (op2(op2(e24, e24), op2(e24, e24)) = h5(e12)), inference(cnf_transformation, [], [f20])).
fof(f1936, plain, (h5(e10) = op2(e24, op2(h5(e14), h5(e14)))), inference(forward_demodulation, [], [f521, f524])).
fof(f521, plain, (h5(e10) = op2(e24, op2(op2(e24, e24), op2(e24, e24)))), inference(cnf_transformation, [], [f20])).
fof(f370, plain, ~ (op2(e20, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f5166, plain, (~ spl20_31 | ~ spl20_129), inference(avatar_contradiction_clause, [], [f5165])).
fof(f5165, plain, ($false | (~ spl20_31 | ~ spl20_129)), inference(subsumption_resolution, [], [f5164, f439])).
fof(f439, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f9])).
fof(f5164, plain, ((e10 = e13) | (~ spl20_31 | ~ spl20_129)), inference(forward_demodulation, [], [f5146, f779])).
fof(f779, plain, ((e10 = op1(e13, e13)) | ~ spl20_31), inference(avatar_component_clause, [], [f777])).
fof(f777, plain, (spl20_31 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_31])])).
fof(f5146, plain, ((e13 = op1(e13, e13)) | ~ spl20_129), inference(backward_demodulation, [], [f97, f1190])).
fof(f5072, plain, (spl20_235 | ~ spl20_256), inference(avatar_split_clause, [], [f5059, f1772, f1683])).
fof(f1772, plain, (spl20_256 <=> (e20 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl20_256])])).
fof(f5059, plain, ((e24 = op2(e20, e24)) | ~ spl20_256), inference(backward_demodulation, [], [f184, f1774])).
fof(f1774, plain, ((e20 = unit2) | ~ spl20_256), inference(avatar_component_clause, [], [f1772])).
fof(f184, plain, (e24 = op2(unit2, e24)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (((e24 = unit2) | (e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)) & (e24 = op2(e24, unit2)) & (e24 = op2(unit2, e24)) & (e23 = op2(e23, unit2)) & (e23 = op2(unit2, e23)) & (e22 = op2(e22, unit2)) & (e22 = op2(unit2, e22)) & (e21 = op2(e21, unit2)) & (e21 = op2(unit2, e21)) & (e20 = op2(e20, unit2)) & (e20 = op2(unit2, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax5)).
fof(f5071, plain, (spl20_179 | ~ spl20_256), inference(avatar_split_clause, [], [f5058, f1772, f1448])).
fof(f5058, plain, ((e23 = op2(e23, e20)) | ~ spl20_256), inference(backward_demodulation, [], [f183, f1774])).
fof(f183, plain, (e23 = op2(e23, unit2)), inference(cnf_transformation, [], [f5])).
fof(f5070, plain, (spl20_239 | ~ spl20_256), inference(avatar_split_clause, [], [f5057, f1772, f1700])).
fof(f5057, plain, ((e23 = op2(e20, e23)) | ~ spl20_256), inference(backward_demodulation, [], [f182, f1774])).
fof(f182, plain, (e23 = op2(unit2, e23)), inference(cnf_transformation, [], [f5])).
fof(f5069, plain, (spl20_203 | ~ spl20_256), inference(avatar_split_clause, [], [f5056, f1772, f1549])).
fof(f5056, plain, ((e22 = op2(e22, e20)) | ~ spl20_256), inference(backward_demodulation, [], [f181, f1774])).
fof(f181, plain, (e22 = op2(e22, unit2)), inference(cnf_transformation, [], [f5])).
fof(f5068, plain, (spl20_243 | ~ spl20_256), inference(avatar_split_clause, [], [f5055, f1772, f1717])).
fof(f5055, plain, ((e22 = op2(e20, e22)) | ~ spl20_256), inference(backward_demodulation, [], [f180, f1774])).
fof(f180, plain, (e22 = op2(unit2, e22)), inference(cnf_transformation, [], [f5])).
fof(f5067, plain, (spl20_247 | ~ spl20_256), inference(avatar_split_clause, [], [f5053, f1772, f1734])).
fof(f5053, plain, ((e21 = op2(e20, e21)) | ~ spl20_256), inference(backward_demodulation, [], [f178, f1774])).
fof(f178, plain, (e21 = op2(unit2, e21)), inference(cnf_transformation, [], [f5])).
fof(f5066, plain, (spl20_251 | ~ spl20_256), inference(avatar_split_clause, [], [f5052, f1772, f1751])).
fof(f5052, plain, ((e20 = op2(e20, e20)) | ~ spl20_256), inference(backward_demodulation, [], [f177, f1774])).
fof(f177, plain, (e20 = op2(e20, unit2)), inference(cnf_transformation, [], [f5])).
fof(f4999, plain, (~ spl20_235 | ~ spl20_240), inference(avatar_split_clause, [], [f4998, f1704, f1683])).
fof(f1704, plain, (spl20_240 <=> (e24 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_240])])).
fof(f4998, plain, (~ (e24 = op2(e20, e24)) | ~ spl20_240), inference(forward_demodulation, [], [f396, f1706])).
fof(f1706, plain, ((e24 = op2(e20, e23)) | ~ spl20_240), inference(avatar_component_clause, [], [f1704])).
fof(f396, plain, ~ (op2(e20, e23) = op2(e20, e24)), inference(cnf_transformation, [], [f8])).
fof(f4989, plain, (~ spl20_180 | ~ spl20_155), inference(avatar_split_clause, [], [f4745, f1347, f1452])).
fof(f1452, plain, (spl20_180 <=> (e24 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_180])])).
fof(f4745, plain, (~ (e24 = op2(e23, e20)) | ~ spl20_155), inference(forward_demodulation, [], [f346, f1349])).
fof(f346, plain, ~ (op2(e23, e20) = op2(e24, e20)), inference(cnf_transformation, [], [f8])).
fof(f4987, plain, (~ spl20_92 | ~ spl20_95), inference(avatar_contradiction_clause, [], [f4986])).
fof(f4986, plain, ($false | (~ spl20_92 | ~ spl20_95)), inference(subsumption_resolution, [], [f4985, f443])).
fof(f4985, plain, ((e11 = e14) | (~ spl20_92 | ~ spl20_95)), inference(forward_demodulation, [], [f1047, f1035])).
fof(f1035, plain, ((e11 = op1(e11, e11)) | ~ spl20_92), inference(avatar_component_clause, [], [f1033])).
fof(f1033, plain, (spl20_92 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_92])])).
fof(f4945, plain, (~ spl20_64 | ~ spl20_65), inference(avatar_contradiction_clause, [], [f4944])).
fof(f4944, plain, ($false | (~ spl20_64 | ~ spl20_65)), inference(subsumption_resolution, [], [f4943, f446])).
fof(f446, plain, ~ (e13 = e14), inference(cnf_transformation, [], [f9])).
fof(f4943, plain, ((e13 = e14) | (~ spl20_64 | ~ spl20_65)), inference(forward_demodulation, [], [f921, f917])).
fof(f921, plain, ((e14 = op1(e12, e12)) | ~ spl20_65), inference(avatar_component_clause, [], [f919])).
fof(f919, plain, (spl20_65 <=> (e14 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_65])])).
fof(f4942, plain, (~ spl20_72 | ~ spl20_73), inference(avatar_contradiction_clause, [], [f4941])).
fof(f4941, plain, ($false | (~ spl20_72 | ~ spl20_73)), inference(subsumption_resolution, [], [f4940, f441])).
fof(f441, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f9])).
fof(f4940, plain, ((e11 = e12) | (~ spl20_72 | ~ spl20_73)), inference(backward_demodulation, [], [f955, f951])).
fof(f951, plain, ((e11 = op1(e12, e10)) | ~ spl20_72), inference(avatar_component_clause, [], [f949])).
fof(f949, plain, (spl20_72 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_72])])).
fof(f4930, plain, (~ spl20_94 | ~ spl20_95), inference(avatar_contradiction_clause, [], [f4929])).
fof(f4929, plain, ($false | (~ spl20_94 | ~ spl20_95)), inference(subsumption_resolution, [], [f4928, f446])).
fof(f4928, plain, ((e13 = e14) | (~ spl20_94 | ~ spl20_95)), inference(backward_demodulation, [], [f1047, f1043])).
fof(f1043, plain, ((e13 = op1(e11, e11)) | ~ spl20_94), inference(avatar_component_clause, [], [f1041])).
fof(f1041, plain, (spl20_94 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_94])])).
fof(f4903, plain, (~ spl20_86 | ~ spl20_128), inference(avatar_contradiction_clause, [], [f4902])).
fof(f4902, plain, ($false | (~ spl20_86 | ~ spl20_128)), inference(subsumption_resolution, [], [f4901, f437])).
fof(f437, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f9])).
fof(f4901, plain, ((e10 = e11) | (~ spl20_86 | ~ spl20_128)), inference(forward_demodulation, [], [f4887, f1010])).
fof(f4887, plain, ((e11 = op1(e11, e12)) | ~ spl20_128), inference(backward_demodulation, [], [f93, f1186])).
fof(f1186, plain, ((e12 = unit1) | ~ spl20_128), inference(avatar_component_clause, [], [f1184])).
fof(f1184, plain, (spl20_128 <=> (e12 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl20_128])])).
fof(f4879, plain, (spl20_265 | ~ spl20_139 | ~ spl20_264), inference(avatar_split_clause, [], [f4876, f1953, f1280, f1959])).
fof(f1280, plain, (spl20_139 <=> (e23 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_139])])).
fof(f4876, plain, ((e23 = h5(e10)) | (~ spl20_139 | ~ spl20_264)), inference(backward_demodulation, [], [f4791, f1282])).
fof(f1282, plain, ((e23 = op2(e24, e23)) | ~ spl20_139), inference(avatar_component_clause, [], [f1280])).
fof(f4867, plain, (~ spl20_151 | ~ spl20_155), inference(avatar_contradiction_clause, [], [f4866])).
fof(f4866, plain, ($false | (~ spl20_151 | ~ spl20_155)), inference(subsumption_resolution, [], [f4863, f450])).
fof(f450, plain, ~ (e20 = e24), inference(cnf_transformation, [], [f10])).
fof(f4863, plain, ((e20 = e24) | (~ spl20_151 | ~ spl20_155)), inference(backward_demodulation, [], [f1349, f1333])).
fof(f1333, plain, ((e20 = op2(e24, e20)) | ~ spl20_151), inference(avatar_component_clause, [], [f1331])).
fof(f1331, plain, (spl20_151 <=> (e20 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_151])])).
fof(f4861, plain, (~ spl20_161 | ~ spl20_302 | spl20_319), inference(avatar_contradiction_clause, [], [f4860])).
fof(f4860, plain, ($false | (~ spl20_161 | ~ spl20_302 | spl20_319)), inference(subsumption_resolution, [], [f4857, f2236])).
fof(f2236, plain, (~ (e20 = h3(e12)) | spl20_319), inference(avatar_component_clause, [], [f2234])).
fof(f2234, plain, (spl20_319 <=> (e20 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_319])])).
fof(f4857, plain, ((e20 = h3(e12)) | (~ spl20_161 | ~ spl20_302)), inference(backward_demodulation, [], [f4131, f1375])).
fof(f1375, plain, ((e20 = op2(e23, e23)) | ~ spl20_161), inference(avatar_component_clause, [], [f1373])).
fof(f1373, plain, (spl20_161 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_161])])).
fof(f4840, plain, (~ spl20_222 | ~ spl20_225), inference(avatar_contradiction_clause, [], [f4839])).
fof(f4839, plain, ($false | (~ spl20_222 | ~ spl20_225)), inference(subsumption_resolution, [], [f4838, f453])).
fof(f4838, plain, ((e21 = e24) | (~ spl20_222 | ~ spl20_225)), inference(backward_demodulation, [], [f1643, f1631])).
fof(f1631, plain, ((e21 = op2(e21, e21)) | ~ spl20_222), inference(avatar_component_clause, [], [f1629])).
fof(f1629, plain, (spl20_222 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_222])])).
fof(f1643, plain, ((e24 = op2(e21, e21)) | ~ spl20_225), inference(avatar_component_clause, [], [f1641])).
fof(f1641, plain, (spl20_225 <=> (e24 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_225])])).
fof(f4828, plain, (~ spl20_133 | ~ spl20_260), inference(avatar_contradiction_clause, [], [f4827])).
fof(f4827, plain, ($false | (~ spl20_133 | ~ spl20_260)), inference(subsumption_resolution, [], [f4826, f455])).
fof(f455, plain, ~ (e22 = e24), inference(cnf_transformation, [], [f10])).
fof(f4826, plain, ((e22 = e24) | (~ spl20_133 | ~ spl20_260)), inference(forward_demodulation, [], [f4806, f1257])).
fof(f1257, plain, ((e22 = op2(e24, e24)) | ~ spl20_133), inference(avatar_component_clause, [], [f1255])).
fof(f1255, plain, (spl20_133 <=> (e22 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_133])])).
fof(f4806, plain, ((e24 = op2(e24, e24)) | ~ spl20_260), inference(backward_demodulation, [], [f185, f1790])).
fof(f1790, plain, ((e24 = unit2) | ~ spl20_260), inference(avatar_component_clause, [], [f1788])).
fof(f1788, plain, (spl20_260 <=> (e24 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl20_260])])).
fof(f185, plain, (e24 = op2(e24, unit2)), inference(cnf_transformation, [], [f5])).
fof(f4769, plain, (~ spl20_203 | ~ spl20_198), inference(avatar_split_clause, [], [f4768, f1528, f1549])).
fof(f1528, plain, (spl20_198 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_198])])).
fof(f4768, plain, (~ (e22 = op2(e22, e20)) | ~ spl20_198), inference(forward_demodulation, [], [f407, f1530])).
fof(f1530, plain, ((e22 = op2(e22, e21)) | ~ spl20_198), inference(avatar_component_clause, [], [f1528])).
fof(f407, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f4738, plain, (~ spl20_144 | ~ spl20_302), inference(avatar_split_clause, [], [f4539, f2145, f1301])).
fof(f1301, plain, (spl20_144 <=> (e23 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_144])])).
fof(f4539, plain, (~ (e23 = op2(e24, e22)) | ~ spl20_302), inference(forward_demodulation, [], [f1889, f2146])).
fof(f1889, plain, ~ (op2(e24, e22) = h2(e13)), inference(backward_demodulation, [], [f365, f1885])).
fof(f365, plain, ~ (op2(e22, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f4717, plain, (~ spl20_78 | ~ spl20_3), inference(avatar_split_clause, [], [f4716, f659, f974])).
fof(f974, plain, (spl20_78 <=> (e12 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_78])])).
fof(f4716, plain, (~ (e12 = op1(e11, e14)) | ~ spl20_3), inference(forward_demodulation, [], [f283, f661])).
fof(f283, plain, ~ (op1(e11, e14) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4702, plain, (~ spl20_18 | ~ spl20_3), inference(avatar_split_clause, [], [f4701, f659, f722])).
fof(f722, plain, (spl20_18 <=> (e12 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_18])])).
fof(f4701, plain, (~ (e12 = op1(e14, e11)) | ~ spl20_3), inference(forward_demodulation, [], [f333, f661])).
fof(f333, plain, ~ (op1(e14, e11) = op1(e14, e14)), inference(cnf_transformation, [], [f7])).
fof(f4693, plain, (~ spl20_21 | ~ spl20_25), inference(avatar_contradiction_clause, [], [f4692])).
fof(f4692, plain, ($false | (~ spl20_21 | ~ spl20_25)), inference(subsumption_resolution, [], [f4689, f440])).
fof(f4689, plain, ((e10 = e14) | (~ spl20_21 | ~ spl20_25)), inference(backward_demodulation, [], [f753, f737])).
fof(f737, plain, ((e10 = op1(e14, e10)) | ~ spl20_21), inference(avatar_component_clause, [], [f735])).
fof(f735, plain, (spl20_21 <=> (e10 = op1(e14, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_21])])).
fof(f4679, plain, (~ spl20_101 | ~ spl20_105), inference(avatar_contradiction_clause, [], [f4678])).
fof(f4678, plain, ($false | (~ spl20_101 | ~ spl20_105)), inference(subsumption_resolution, [], [f4675, f440])).
fof(f4675, plain, ((e10 = e14) | (~ spl20_101 | ~ spl20_105)), inference(backward_demodulation, [], [f1089, f1073])).
fof(f1073, plain, ((e10 = op1(e10, e14)) | ~ spl20_101), inference(avatar_component_clause, [], [f1071])).
fof(f1071, plain, (spl20_101 <=> (e10 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_101])])).
fof(f4652, plain, (~ spl20_86 | ~ spl20_127), inference(avatar_contradiction_clause, [], [f4651])).
fof(f4651, plain, ($false | (~ spl20_86 | ~ spl20_127)), inference(subsumption_resolution, [], [f4650, f438])).
fof(f4650, plain, ((e10 = e12) | (~ spl20_86 | ~ spl20_127)), inference(forward_demodulation, [], [f4638, f1010])).
fof(f4638, plain, ((e12 = op1(e11, e12)) | ~ spl20_127), inference(backward_demodulation, [], [f94, f1182])).
fof(f1182, plain, ((e11 = unit1) | ~ spl20_127), inference(avatar_component_clause, [], [f1180])).
fof(f1180, plain, (spl20_127 <=> (e11 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl20_127])])).
fof(f4624, plain, (~ spl20_168 | ~ spl20_170), inference(avatar_contradiction_clause, [], [f4623])).
fof(f4623, plain, ($false | (~ spl20_168 | ~ spl20_170)), inference(subsumption_resolution, [], [f4622, f455])).
fof(f4622, plain, ((e22 = e24) | (~ spl20_168 | ~ spl20_170)), inference(backward_demodulation, [], [f1412, f1404])).
fof(f1404, plain, ((e22 = op2(e23, e22)) | ~ spl20_168), inference(avatar_component_clause, [], [f1402])).
fof(f1402, plain, (spl20_168 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_168])])).
fof(f4615, plain, (~ spl20_178 | ~ spl20_179), inference(avatar_contradiction_clause, [], [f4614])).
fof(f4614, plain, ($false | (~ spl20_178 | ~ spl20_179)), inference(subsumption_resolution, [], [f4613, f454])).
fof(f454, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f10])).
fof(f4613, plain, ((e22 = e23) | (~ spl20_178 | ~ spl20_179)), inference(backward_demodulation, [], [f1450, f1446])).
fof(f1446, plain, ((e22 = op2(e23, e20)) | ~ spl20_178), inference(avatar_component_clause, [], [f1444])).
fof(f1444, plain, (spl20_178 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_178])])).
fof(f4604, plain, (~ spl20_192 | ~ spl20_194), inference(avatar_contradiction_clause, [], [f4603])).
fof(f4603, plain, ($false | (~ spl20_192 | ~ spl20_194)), inference(subsumption_resolution, [], [f4602, f452])).
fof(f452, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f10])).
fof(f4602, plain, ((e21 = e23) | (~ spl20_192 | ~ spl20_194)), inference(backward_demodulation, [], [f1513, f1505])).
fof(f1505, plain, ((e21 = op2(e22, e22)) | ~ spl20_192), inference(avatar_component_clause, [], [f1503])).
fof(f1503, plain, (spl20_192 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_192])])).
fof(f1513, plain, ((e23 = op2(e22, e22)) | ~ spl20_194), inference(avatar_component_clause, [], [f1511])).
fof(f1511, plain, (spl20_194 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_194])])).
fof(f4595, plain, (~ spl20_221 | ~ spl20_225), inference(avatar_contradiction_clause, [], [f4594])).
fof(f4594, plain, ($false | (~ spl20_221 | ~ spl20_225)), inference(subsumption_resolution, [], [f4593, f450])).
fof(f4593, plain, ((e20 = e24) | (~ spl20_221 | ~ spl20_225)), inference(backward_demodulation, [], [f1643, f1627])).
fof(f1627, plain, ((e20 = op2(e21, e21)) | ~ spl20_221), inference(avatar_component_clause, [], [f1625])).
fof(f1625, plain, (spl20_221 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_221])])).
fof(f4572, plain, (spl20_236 | ~ spl20_259), inference(avatar_contradiction_clause, [], [f4571])).
fof(f4571, plain, ($false | (spl20_236 | ~ spl20_259)), inference(subsumption_resolution, [], [f4560, f1689])).
fof(f1689, plain, (~ (e20 = op2(e20, e23)) | spl20_236), inference(avatar_component_clause, [], [f1688])).
fof(f1688, plain, (spl20_236 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_236])])).
fof(f4560, plain, ((e20 = op2(e20, e23)) | ~ spl20_259), inference(backward_demodulation, [], [f177, f1786])).
fof(f1786, plain, ((e23 = unit2) | ~ spl20_259), inference(avatar_component_clause, [], [f1784])).
fof(f1784, plain, (spl20_259 <=> (e23 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl20_259])])).
fof(f4558, plain, (spl20_264 | ~ spl20_194 | ~ spl20_267), inference(avatar_split_clause, [], [f4557, f1968, f1511, f1953])).
fof(f4557, plain, ((e23 = h5(e12)) | (~ spl20_194 | ~ spl20_267)), inference(forward_demodulation, [], [f4551, f1513])).
fof(f4551, plain, ((op2(e22, e22) = h5(e12)) | ~ spl20_267), inference(backward_demodulation, [], [f1934, f1969])).
fof(f4556, plain, (~ spl20_143 | ~ spl20_267), inference(avatar_split_clause, [], [f4550, f1968, f1297])).
fof(f1297, plain, (spl20_143 <=> (e22 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_143])])).
fof(f4550, plain, (~ (e22 = op2(e24, e22)) | ~ spl20_267), inference(backward_demodulation, [], [f1931, f1969])).
fof(f1931, plain, ~ (op2(e24, e22) = h5(e14)), inference(backward_demodulation, [], [f435, f524])).
fof(f435, plain, ~ (op2(e24, e22) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f4541, plain, (~ spl20_141 | ~ spl20_332), inference(avatar_split_clause, [], [f4540, f2310, f1289])).
fof(f1289, plain, (spl20_141 <=> (e20 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_141])])).
fof(f4540, plain, (~ (e20 = op2(e24, e22)) | ~ spl20_332), inference(forward_demodulation, [], [f1900, f2311])).
fof(f1900, plain, ~ (op2(e24, e22) = h2(e10)), inference(backward_demodulation, [], [f363, f1897])).
fof(f363, plain, ~ (op2(e21, e22) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f4535, plain, (spl20_133 | ~ spl20_447), inference(avatar_split_clause, [], [f4103, f3037, f1255])).
fof(f4103, plain, ((e22 = op2(e24, e24)) | ~ spl20_447), inference(backward_demodulation, [], [f1883, f3038])).
fof(f4534, plain, (~ spl20_122 | ~ spl20_97), inference(avatar_split_clause, [], [f4533, f1054, f1159])).
fof(f1159, plain, (spl20_122 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl20_122])])).
fof(f4533, plain, (~ (op1(e10, e10) = e11) | ~ spl20_97), inference(forward_demodulation, [], [f237, f1056])).
fof(f237, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f7])).
fof(f4502, plain, (~ spl20_16 | ~ spl20_19), inference(avatar_contradiction_clause, [], [f4501])).
fof(f4501, plain, ($false | (~ spl20_16 | ~ spl20_19)), inference(subsumption_resolution, [], [f4499, f439])).
fof(f4499, plain, ((e10 = e13) | (~ spl20_16 | ~ spl20_19)), inference(backward_demodulation, [], [f728, f716])).
fof(f716, plain, ((e10 = op1(e14, e11)) | ~ spl20_16), inference(avatar_component_clause, [], [f714])).
fof(f714, plain, (spl20_16 <=> (e10 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_16])])).
fof(f4490, plain, (~ spl20_51 | ~ spl20_52), inference(avatar_contradiction_clause, [], [f4489])).
fof(f4489, plain, ($false | (~ spl20_51 | ~ spl20_52)), inference(subsumption_resolution, [], [f4488, f437])).
fof(f4488, plain, ((e10 = e11) | (~ spl20_51 | ~ spl20_52)), inference(backward_demodulation, [], [f867, f863])).
fof(f863, plain, ((e10 = op1(e12, e14)) | ~ spl20_51), inference(avatar_component_clause, [], [f861])).
fof(f861, plain, (spl20_51 <=> (e10 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_51])])).
fof(f4476, plain, (~ spl20_134 | spl20_262), inference(avatar_contradiction_clause, [], [f4475])).
fof(f4475, plain, ($false | (~ spl20_134 | spl20_262)), inference(subsumption_resolution, [], [f4474, f1945])).
fof(f1945, plain, (~ (e23 = h5(e14)) | spl20_262), inference(avatar_component_clause, [], [f1943])).
fof(f1943, plain, (spl20_262 <=> (e23 = h5(e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_262])])).
fof(f4474, plain, ((e23 = h5(e14)) | ~ spl20_134), inference(backward_demodulation, [], [f524, f1261])).
fof(f1261, plain, ((e23 = op2(e24, e24)) | ~ spl20_134), inference(avatar_component_clause, [], [f1259])).
fof(f1259, plain, (spl20_134 <=> (e23 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_134])])).
fof(f4462, plain, (~ spl20_191 | ~ spl20_194), inference(avatar_contradiction_clause, [], [f4461])).
fof(f4461, plain, ($false | (~ spl20_191 | ~ spl20_194)), inference(subsumption_resolution, [], [f4460, f449])).
fof(f449, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f10])).
fof(f4460, plain, ((e20 = e23) | (~ spl20_191 | ~ spl20_194)), inference(backward_demodulation, [], [f1513, f1501])).
fof(f1501, plain, ((e20 = op2(e22, e22)) | ~ spl20_191), inference(avatar_component_clause, [], [f1499])).
fof(f1499, plain, (spl20_191 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_191])])).
fof(f4439, plain, (~ spl20_216 | ~ spl20_258), inference(avatar_contradiction_clause, [], [f4438])).
fof(f4438, plain, ($false | (~ spl20_216 | ~ spl20_258)), inference(subsumption_resolution, [], [f4437, f447])).
fof(f447, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f10])).
fof(f4437, plain, ((e20 = e21) | (~ spl20_216 | ~ spl20_258)), inference(forward_demodulation, [], [f4428, f1606])).
fof(f1606, plain, ((e20 = op2(e21, e22)) | ~ spl20_216), inference(avatar_component_clause, [], [f1604])).
fof(f1604, plain, (spl20_216 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_216])])).
fof(f4428, plain, ((e21 = op2(e21, e22)) | ~ spl20_258), inference(backward_demodulation, [], [f179, f1782])).
fof(f1782, plain, ((e22 = unit2) | ~ spl20_258), inference(avatar_component_clause, [], [f1780])).
fof(f1780, plain, (spl20_258 <=> (e22 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl20_258])])).
fof(f179, plain, (e21 = op2(e21, unit2)), inference(cnf_transformation, [], [f5])).
fof(f4418, plain, (~ spl20_207 | ~ spl20_227), inference(avatar_split_clause, [], [f4417, f1650, f1566])).
fof(f1566, plain, (spl20_207 <=> (e21 = op2(e21, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_207])])).
fof(f4417, plain, (~ (e21 = op2(e21, e24)) | ~ spl20_227), inference(forward_demodulation, [], [f400, f1652])).
fof(f400, plain, ~ (op2(e21, e20) = op2(e21, e24)), inference(cnf_transformation, [], [f8])).
fof(f4416, plain, (spl20_314 | ~ spl20_162 | ~ spl20_302), inference(avatar_split_clause, [], [f4415, f2145, f1377, f2208])).
fof(f4415, plain, ((e21 = h3(e12)) | (~ spl20_162 | ~ spl20_302)), inference(forward_demodulation, [], [f4131, f1379])).
fof(f4395, plain, (~ spl20_319 | ~ spl20_136 | ~ spl20_302), inference(avatar_split_clause, [], [f4394, f2145, f1268, f2234])).
fof(f1268, plain, (spl20_136 <=> (e20 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_136])])).
fof(f4394, plain, (~ (e20 = h3(e12)) | (~ spl20_136 | ~ spl20_302)), inference(forward_demodulation, [], [f4393, f1270])).
fof(f1270, plain, ((e20 = op2(e24, e23)) | ~ spl20_136), inference(avatar_component_clause, [], [f1268])).
fof(f4393, plain, (~ (op2(e24, e23) = h3(e12)) | ~ spl20_302), inference(forward_demodulation, [], [f1915, f4137])).
fof(f1915, plain, ~ (op2(e24, e23) = h4(e14)), inference(backward_demodulation, [], [f376, f519])).
fof(f376, plain, ~ (op2(e23, e23) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f4371, plain, (~ spl20_69 | ~ spl20_64), inference(avatar_split_clause, [], [f4370, f915, f936])).
fof(f936, plain, (spl20_69 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_69])])).
fof(f4370, plain, (~ (e13 = op1(e12, e11)) | ~ spl20_64), inference(forward_demodulation, [], [f311, f917])).
fof(f311, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f4368, plain, (~ spl20_68 | ~ spl20_73), inference(avatar_split_clause, [], [f4367, f953, f932])).
fof(f932, plain, (spl20_68 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_68])])).
fof(f4367, plain, (~ (e12 = op1(e12, e11)) | ~ spl20_73), inference(forward_demodulation, [], [f307, f955])).
fof(f307, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f4342, plain, (~ spl20_17 | ~ spl20_117), inference(avatar_split_clause, [], [f4341, f1138, f718])).
fof(f718, plain, (spl20_17 <=> (e11 = op1(e14, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_17])])).
fof(f4341, plain, (~ (e11 = op1(e14, e11)) | ~ spl20_117), inference(forward_demodulation, [], [f250, f1140])).
fof(f250, plain, ~ (op1(e10, e11) = op1(e14, e11)), inference(cnf_transformation, [], [f7])).
fof(f4296, plain, (~ spl20_44 | ~ spl20_49), inference(avatar_split_clause, [], [f4293, f852, f831])).
fof(f831, plain, (spl20_44 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_44])])).
fof(f4293, plain, (~ (e13 = op1(e13, e11)) | ~ spl20_49), inference(backward_demodulation, [], [f317, f854])).
fof(f317, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f7])).
fof(f4290, plain, (~ spl20_57 | ~ spl20_60), inference(avatar_contradiction_clause, [], [f4289])).
fof(f4289, plain, ($false | (~ spl20_57 | ~ spl20_60)), inference(subsumption_resolution, [], [f4288, f443])).
fof(f4288, plain, ((e11 = e14) | (~ spl20_57 | ~ spl20_60)), inference(backward_demodulation, [], [f900, f888])).
fof(f888, plain, ((e11 = op1(e12, e13)) | ~ spl20_57), inference(avatar_component_clause, [], [f886])).
fof(f886, plain, (spl20_57 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_57])])).
fof(f4229, plain, (~ spl20_162 | ~ spl20_163), inference(avatar_contradiction_clause, [], [f4228])).
fof(f4228, plain, ($false | (~ spl20_162 | ~ spl20_163)), inference(subsumption_resolution, [], [f4227, f451])).
fof(f451, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f10])).
fof(f4227, plain, ((e21 = e22) | (~ spl20_162 | ~ spl20_163)), inference(backward_demodulation, [], [f1383, f1379])).
fof(f1383, plain, ((e22 = op2(e23, e23)) | ~ spl20_163), inference(avatar_component_clause, [], [f1381])).
fof(f1381, plain, (spl20_163 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_163])])).
fof(f4196, plain, (~ spl20_344 | ~ spl20_227), inference(avatar_split_clause, [], [f4193, f1650, f2369])).
fof(f2369, plain, (spl20_344 <=> (e21 = h1(e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_344])])).
fof(f4193, plain, (~ (e21 = h1(e14)) | ~ spl20_227), inference(backward_demodulation, [], [f1862, f1652])).
fof(f1862, plain, ~ (op2(e21, e20) = h1(e14)), inference(backward_demodulation, [], [f337, f504])).
fof(f504, plain, (op2(e20, e20) = h1(e14)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e20, e20) = h1(e14)) & (h1(e13) = op2(op2(op2(e20, e20), op2(e20, e20)), op2(op2(e20, e20), op2(e20, e20)))) & (op2(op2(e20, e20), op2(e20, e20)) = h1(e12)) & (h1(e10) = op2(e20, op2(op2(e20, e20), op2(e20, e20)))) & (e20 = h1(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax16)).
fof(f337, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f8])).
fof(f4195, plain, (~ spl20_212 | ~ spl20_227), inference(avatar_split_clause, [], [f4192, f1650, f1587])).
fof(f1587, plain, (spl20_212 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_212])])).
fof(f4192, plain, (~ (e21 = op2(e21, e23)) | ~ spl20_227), inference(backward_demodulation, [], [f399, f1652])).
fof(f399, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f4187, plain, (~ spl20_214 | ~ spl20_239), inference(avatar_split_clause, [], [f4183, f1700, f1595])).
fof(f1595, plain, (spl20_214 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_214])])).
fof(f4183, plain, (~ (e23 = op2(e21, e23)) | ~ spl20_239), inference(backward_demodulation, [], [f367, f1702])).
fof(f367, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f4175, plain, (spl20_344 | ~ spl20_252), inference(avatar_split_clause, [], [f4174, f1755, f2369])).
fof(f1755, plain, (spl20_252 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl20_252])])).
fof(f4174, plain, ((e21 = h1(e14)) | ~ spl20_252), inference(backward_demodulation, [], [f504, f1757])).
fof(f1757, plain, ((op2(e20, e20) = e21) | ~ spl20_252), inference(avatar_component_clause, [], [f1755])).
fof(f4164, plain, (~ spl20_216 | ~ spl20_257), inference(avatar_contradiction_clause, [], [f4163])).
fof(f4163, plain, ($false | (~ spl20_216 | ~ spl20_257)), inference(subsumption_resolution, [], [f4162, f448])).
fof(f4162, plain, ((e20 = e22) | (~ spl20_216 | ~ spl20_257)), inference(forward_demodulation, [], [f4153, f1606])).
fof(f4153, plain, ((e22 = op2(e21, e22)) | ~ spl20_257), inference(backward_demodulation, [], [f180, f1778])).
fof(f1778, plain, ((e21 = unit2) | ~ spl20_257), inference(avatar_component_clause, [], [f1776])).
fof(f1776, plain, (spl20_257 <=> (e21 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl20_257])])).
fof(f4148, plain, (~ spl20_136 | ~ spl20_6 | ~ spl20_302 | ~ spl20_332 | spl20_446 | ~ spl20_447), inference(avatar_split_clause, [], [f4132, f3037, f3033, f2310, f2145, f672, f1268])).
fof(f672, plain, (spl20_6 <=> (e10 = op1(e14, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_6])])).
fof(f3033, plain, (spl20_446 <=> (h2(op1(e14, e13)) = op2(h2(e14), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_446])])).
fof(f4132, plain, (~ (e20 = op2(e24, e23)) | (~ spl20_6 | ~ spl20_302 | ~ spl20_332 | spl20_446 | ~ spl20_447)), inference(backward_demodulation, [], [f4119, f2146])).
fof(f4119, plain, (~ (e20 = op2(e24, h2(e13))) | (~ spl20_6 | ~ spl20_332 | spl20_446 | ~ spl20_447)), inference(backward_demodulation, [], [f4105, f2311])).
fof(f4105, plain, (~ (h2(e10) = op2(e24, h2(e13))) | (~ spl20_6 | spl20_446 | ~ spl20_447)), inference(backward_demodulation, [], [f3933, f3038])).
fof(f3933, plain, (~ (h2(e10) = op2(h2(e14), h2(e13))) | (~ spl20_6 | spl20_446)), inference(forward_demodulation, [], [f3035, f674])).
fof(f674, plain, ((e10 = op1(e14, e13)) | ~ spl20_6), inference(avatar_component_clause, [], [f672])).
fof(f3035, plain, (~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | spl20_446), inference(avatar_component_clause, [], [f3033])).
fof(f4135, plain, (~ spl20_199 | ~ spl20_302), inference(avatar_split_clause, [], [f4128, f2145, f1532])).
fof(f1532, plain, (spl20_199 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_199])])).
fof(f4128, plain, (~ (e23 = op2(e22, e21)) | ~ spl20_302), inference(backward_demodulation, [], [f1891, f2146])).
fof(f1891, plain, ~ (op2(e22, e21) = h2(e13)), inference(backward_demodulation, [], [f411, f1885])).
fof(f411, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f4123, plain, (~ spl20_211 | ~ spl20_332), inference(avatar_split_clause, [], [f4117, f2310, f1583])).
fof(f1583, plain, (spl20_211 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_211])])).
fof(f4117, plain, (~ (e20 = op2(e21, e23)) | ~ spl20_332), inference(backward_demodulation, [], [f1902, f2311])).
fof(f1902, plain, ~ (op2(e21, e23) = h2(e10)), inference(backward_demodulation, [], [f404, f1897])).
fof(f404, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f4121, plain, (~ spl20_166 | ~ spl20_332), inference(avatar_split_clause, [], [f4115, f2310, f1394])).
fof(f1394, plain, (spl20_166 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_166])])).
fof(f4115, plain, (~ (e20 = op2(e23, e22)) | ~ spl20_332), inference(backward_demodulation, [], [f1899, f2311])).
fof(f1899, plain, ~ (op2(e23, e22) = h2(e10)), inference(backward_demodulation, [], [f362, f1897])).
fof(f362, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f4112, plain, (~ spl20_131 | ~ spl20_447), inference(avatar_contradiction_clause, [], [f4111])).
fof(f4111, plain, ($false | (~ spl20_131 | ~ spl20_447)), inference(subsumption_resolution, [], [f4110, f448])).
fof(f4110, plain, ((e20 = e22) | (~ spl20_131 | ~ spl20_447)), inference(forward_demodulation, [], [f4103, f1249])).
fof(f1249, plain, ((e20 = op2(e24, e24)) | ~ spl20_131), inference(avatar_component_clause, [], [f1247])).
fof(f1247, plain, (spl20_131 <=> (e20 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_131])])).
fof(f4109, plain, (~ spl20_215 | ~ spl20_447), inference(avatar_split_clause, [], [f4102, f3037, f1599])).
fof(f1599, plain, (spl20_215 <=> (e24 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_215])])).
fof(f4102, plain, (~ (e24 = op2(e21, e23)) | ~ spl20_447), inference(backward_demodulation, [], [f1881, f3038])).
fof(f1881, plain, ~ (op2(e21, e23) = h2(e14)), inference(backward_demodulation, [], [f402, f509])).
fof(f402, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f8])).
fof(f4107, plain, (~ spl20_175 | ~ spl20_447), inference(avatar_split_clause, [], [f4100, f3037, f1431])).
fof(f1431, plain, (spl20_175 <=> (e24 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_175])])).
fof(f4100, plain, (~ (e24 = op2(e23, e21)) | ~ spl20_447), inference(backward_demodulation, [], [f1877, f3038])).
fof(f1877, plain, ~ (op2(e23, e21) = h2(e14)), inference(backward_demodulation, [], [f352, f509])).
fof(f352, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f4106, plain, (~ spl20_200 | ~ spl20_447), inference(avatar_split_clause, [], [f4099, f3037, f1536])).
fof(f1536, plain, (spl20_200 <=> (e24 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_200])])).
fof(f4099, plain, (~ (e24 = op2(e22, e21)) | ~ spl20_447), inference(backward_demodulation, [], [f1876, f3038])).
fof(f1876, plain, ~ (op2(e22, e21) = h2(e14)), inference(backward_demodulation, [], [f351, f509])).
fof(f351, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f4045, plain, (~ spl20_267 | ~ spl20_158), inference(avatar_split_clause, [], [f4044, f1360, f1968])).
fof(f1360, plain, (spl20_158 <=> (e22 = op2(e23, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_158])])).
fof(f4044, plain, (~ (e22 = h5(e14)) | ~ spl20_158), inference(forward_demodulation, [], [f1928, f1362])).
fof(f1362, plain, ((e22 = op2(e23, e24)) | ~ spl20_158), inference(avatar_component_clause, [], [f1360])).
fof(f1928, plain, ~ (op2(e23, e24) = h5(e14)), inference(backward_demodulation, [], [f386, f524])).
fof(f386, plain, ~ (op2(e23, e24) = op2(e24, e24)), inference(cnf_transformation, [], [f8])).
fof(f3976, plain, (~ spl20_56 | ~ spl20_6), inference(avatar_split_clause, [], [f3975, f672, f882])).
fof(f882, plain, (spl20_56 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_56])])).
fof(f3975, plain, (~ (e10 = op1(e12, e13)) | ~ spl20_6), inference(forward_demodulation, [], [f275, f674])).
fof(f275, plain, ~ (op1(e12, e13) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f3929, plain, (~ spl20_2 | ~ spl20_3), inference(avatar_contradiction_clause, [], [f3928])).
fof(f3928, plain, ($false | (~ spl20_2 | ~ spl20_3)), inference(subsumption_resolution, [], [f3927, f441])).
fof(f3927, plain, ((e11 = e12) | (~ spl20_2 | ~ spl20_3)), inference(backward_demodulation, [], [f661, f657])).
fof(f657, plain, ((e11 = op1(e14, e14)) | ~ spl20_2), inference(avatar_component_clause, [], [f655])).
fof(f655, plain, (spl20_2 <=> (e11 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_2])])).
fof(f3904, plain, (~ spl20_10 | ~ spl20_15), inference(avatar_split_clause, [], [f3902, f709, f688])).
fof(f709, plain, (spl20_15 <=> (e14 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_15])])).
fof(f3902, plain, (~ (e14 = op1(e14, e13)) | ~ spl20_15), inference(backward_demodulation, [], [f334, f711])).
fof(f711, plain, ((e14 = op1(e14, e12)) | ~ spl20_15), inference(avatar_component_clause, [], [f709])).
fof(f334, plain, ~ (op1(e14, e12) = op1(e14, e13)), inference(cnf_transformation, [], [f7])).
fof(f3895, plain, (~ spl20_4 | ~ spl20_19), inference(avatar_split_clause, [], [f3892, f726, f663])).
fof(f663, plain, (spl20_4 <=> (e13 = op1(e14, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_4])])).
fof(f3892, plain, (~ (e13 = op1(e14, e14)) | ~ spl20_19), inference(backward_demodulation, [], [f333, f728])).
fof(f3878, plain, (~ spl20_15 | ~ spl20_25), inference(avatar_split_clause, [], [f3874, f751, f709])).
fof(f3874, plain, (~ (e14 = op1(e14, e12)) | ~ spl20_25), inference(backward_demodulation, [], [f328, f753])).
fof(f328, plain, ~ (op1(e14, e10) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f3814, plain, (~ spl20_54 | ~ spl20_64), inference(avatar_split_clause, [], [f3810, f915, f873])).
fof(f873, plain, (spl20_54 <=> (e13 = op1(e12, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_54])])).
fof(f3810, plain, (~ (e13 = op1(e12, e14)) | ~ spl20_64), inference(backward_demodulation, [], [f315, f917])).
fof(f315, plain, ~ (op1(e12, e12) = op1(e12, e14)), inference(cnf_transformation, [], [f7])).
fof(f3764, plain, (~ spl20_76 | ~ spl20_86), inference(avatar_split_clause, [], [f3759, f1008, f966])).
fof(f966, plain, (spl20_76 <=> (e10 = op1(e11, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_76])])).
fof(f3759, plain, (~ (e10 = op1(e11, e14)) | ~ spl20_86), inference(backward_demodulation, [], [f305, f1010])).
fof(f305, plain, ~ (op1(e11, e12) = op1(e11, e14)), inference(cnf_transformation, [], [f7])).
fof(f3762, plain, (~ spl20_11 | ~ spl20_86), inference(avatar_split_clause, [], [f3757, f1008, f693])).
fof(f693, plain, (spl20_11 <=> (e10 = op1(e14, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_11])])).
fof(f3757, plain, (~ (e10 = op1(e14, e12)) | ~ spl20_86), inference(backward_demodulation, [], [f263, f1010])).
fof(f263, plain, ~ (op1(e11, e12) = op1(e14, e12)), inference(cnf_transformation, [], [f7])).
fof(f3760, plain, (~ spl20_61 | ~ spl20_86), inference(avatar_split_clause, [], [f3755, f1008, f903])).
fof(f903, plain, (spl20_61 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_61])])).
fof(f3755, plain, (~ (e10 = op1(e12, e12)) | ~ spl20_86), inference(backward_demodulation, [], [f261, f1010])).
fof(f261, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f3754, plain, (spl20_3 | ~ spl20_95), inference(avatar_split_clause, [], [f3747, f1045, f659])).
fof(f3747, plain, ((e12 = op1(e14, e14)) | ~ spl20_95), inference(backward_demodulation, [], [f493, f1047])).
fof(f493, plain, (e12 = op1(op1(e11, e11), op1(e11, e11))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((e14 = op1(e11, e11)) & (e13 = op1(op1(op1(e11, e11), op1(e11, e11)), op1(op1(e11, e11), op1(e11, e11)))) & (e12 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(e11, op1(op1(e11, e11), op1(e11, e11))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax14)).
fof(f3751, plain, (~ spl20_90 | ~ spl20_95), inference(avatar_split_clause, [], [f3744, f1045, f1024])).
fof(f1024, plain, (spl20_90 <=> (e14 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_90])])).
fof(f3744, plain, (~ (e14 = op1(e11, e12)) | ~ spl20_95), inference(backward_demodulation, [], [f301, f1047])).
fof(f301, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f3748, plain, (~ spl20_70 | ~ spl20_95), inference(avatar_split_clause, [], [f3741, f1045, f940])).
fof(f940, plain, (spl20_70 <=> (e14 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_70])])).
fof(f3741, plain, (~ (e14 = op1(e12, e11)) | ~ spl20_95), inference(backward_demodulation, [], [f251, f1047])).
fof(f251, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f3720, plain, (~ spl20_104 | ~ spl20_105), inference(avatar_contradiction_clause, [], [f3719])).
fof(f3719, plain, ($false | (~ spl20_104 | ~ spl20_105)), inference(subsumption_resolution, [], [f3718, f446])).
fof(f3718, plain, ((e13 = e14) | (~ spl20_104 | ~ spl20_105)), inference(backward_demodulation, [], [f1089, f1085])).
fof(f1085, plain, ((e13 = op1(e10, e14)) | ~ spl20_104), inference(avatar_component_clause, [], [f1083])).
fof(f1083, plain, (spl20_104 <=> (e13 = op1(e10, e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_104])])).
fof(f3667, plain, (~ spl20_71 | ~ spl20_121), inference(avatar_split_clause, [], [f3655, f1155, f945])).
fof(f945, plain, (spl20_71 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_71])])).
fof(f3655, plain, (~ (e10 = op1(e12, e10)) | ~ spl20_121), inference(backward_demodulation, [], [f238, f1157])).
fof(f238, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f3630, plain, (~ spl20_132 | ~ spl20_133), inference(avatar_contradiction_clause, [], [f3629])).
fof(f3629, plain, ($false | (~ spl20_132 | ~ spl20_133)), inference(subsumption_resolution, [], [f3628, f451])).
fof(f3628, plain, ((e21 = e22) | (~ spl20_132 | ~ spl20_133)), inference(backward_demodulation, [], [f1257, f1253])).
fof(f1253, plain, ((e21 = op2(e24, e24)) | ~ spl20_132), inference(avatar_component_clause, [], [f1251])).
fof(f1251, plain, (spl20_132 <=> (e21 = op2(e24, e24))), introduced(avatar_definition, [new_symbols(naming, [spl20_132])])).
fof(f3627, plain, (spl20_267 | ~ spl20_133), inference(avatar_split_clause, [], [f3626, f1255, f1968])).
fof(f3626, plain, ((e22 = h5(e14)) | ~ spl20_133), inference(backward_demodulation, [], [f524, f1257])).
fof(f3578, plain, (~ spl20_154 | ~ spl20_155), inference(avatar_contradiction_clause, [], [f3577])).
fof(f3577, plain, ($false | (~ spl20_154 | ~ spl20_155)), inference(subsumption_resolution, [], [f3576, f456])).
fof(f456, plain, ~ (e23 = e24), inference(cnf_transformation, [], [f10])).
fof(f3576, plain, ((e23 = e24) | (~ spl20_154 | ~ spl20_155)), inference(backward_demodulation, [], [f1349, f1345])).
fof(f1345, plain, ((e23 = op2(e24, e20)) | ~ spl20_154), inference(avatar_component_clause, [], [f1343])).
fof(f1343, plain, (spl20_154 <=> (e23 = op2(e24, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_154])])).
fof(f3573, plain, (~ spl20_140 | ~ spl20_155), inference(avatar_split_clause, [], [f3568, f1347, f1284])).
fof(f1284, plain, (spl20_140 <=> (e24 = op2(e24, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_140])])).
fof(f3568, plain, (~ (e24 = op2(e24, e23)) | ~ spl20_155), inference(backward_demodulation, [], [f429, f1349])).
fof(f429, plain, ~ (op2(e24, e20) = op2(e24, e23)), inference(cnf_transformation, [], [f8])).
fof(f3572, plain, (~ spl20_145 | ~ spl20_155), inference(avatar_split_clause, [], [f3567, f1347, f1305])).
fof(f1305, plain, (spl20_145 <=> (e24 = op2(e24, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_145])])).
fof(f3567, plain, (~ (e24 = op2(e24, e22)) | ~ spl20_155), inference(backward_demodulation, [], [f428, f1349])).
fof(f428, plain, ~ (op2(e24, e20) = op2(e24, e22)), inference(cnf_transformation, [], [f8])).
fof(f3552, plain, (spl20_297 | ~ spl20_161), inference(avatar_split_clause, [], [f3551, f1373, f2119])).
fof(f2119, plain, (spl20_297 <=> (e20 = h4(e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_297])])).
fof(f3551, plain, ((e20 = h4(e14)) | ~ spl20_161), inference(backward_demodulation, [], [f519, f1375])).
fof(f3529, plain, (~ spl20_349 | ~ spl20_176), inference(avatar_split_clause, [], [f3523, f1436, f2394])).
fof(f2394, plain, (spl20_349 <=> (e20 = h1(e14))), introduced(avatar_definition, [new_symbols(naming, [spl20_349])])).
fof(f1436, plain, (spl20_176 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_176])])).
fof(f3523, plain, (~ (e20 = h1(e14)) | ~ spl20_176), inference(backward_demodulation, [], [f1864, f1438])).
fof(f1438, plain, ((e20 = op2(e23, e20)) | ~ spl20_176), inference(avatar_component_clause, [], [f1436])).
fof(f1864, plain, ~ (op2(e23, e20) = h1(e14)), inference(backward_demodulation, [], [f339, f504])).
fof(f339, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f3498, plain, (spl20_302 | ~ spl20_194), inference(avatar_split_clause, [], [f3497, f1511, f2145])).
fof(f3497, plain, ((e23 = h2(e13)) | ~ spl20_194), inference(backward_demodulation, [], [f1885, f1513])).
fof(f3492, plain, (~ spl20_146 | ~ spl20_196), inference(avatar_split_clause, [], [f3486, f1520, f1310])).
fof(f1310, plain, (spl20_146 <=> (e20 = op2(e24, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_146])])).
fof(f3486, plain, (~ (e20 = op2(e24, e21)) | ~ spl20_196), inference(backward_demodulation, [], [f355, f1522])).
fof(f1522, plain, ((e20 = op2(e22, e21)) | ~ spl20_196), inference(avatar_component_clause, [], [f1520])).
fof(f355, plain, ~ (op2(e22, e21) = op2(e24, e21)), inference(cnf_transformation, [], [f8])).
fof(f3464, plain, (~ spl20_262 | ~ spl20_209), inference(avatar_split_clause, [], [f3459, f1574, f1943])).
fof(f3459, plain, (~ (e23 = h5(e14)) | ~ spl20_209), inference(backward_demodulation, [], [f1926, f1576])).
fof(f3442, plain, (spl20_332 | ~ spl20_216), inference(avatar_split_clause, [], [f3441, f1604, f2310])).
fof(f3441, plain, ((e20 = h2(e10)) | ~ spl20_216), inference(backward_demodulation, [], [f1897, f1606])).
fof(f3440, plain, (spl20_447 | ~ spl20_225), inference(avatar_split_clause, [], [f3439, f1641, f3037])).
fof(f3439, plain, ((e24 = h2(e14)) | ~ spl20_225), inference(backward_demodulation, [], [f509, f1643])).
fof(f3403, plain, (~ spl20_297 | ~ spl20_236), inference(avatar_split_clause, [], [f3397, f1688, f2119])).
fof(f3397, plain, (~ (e20 = h4(e14)) | ~ spl20_236), inference(backward_demodulation, [], [f1912, f1690])).
fof(f1690, plain, ((e20 = op2(e20, e23)) | ~ spl20_236), inference(avatar_component_clause, [], [f1688])).
fof(f1912, plain, ~ (op2(e20, e23) = h4(e14)), inference(backward_demodulation, [], [f369, f519])).
fof(f369, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f3402, plain, (~ spl20_349 | ~ spl20_236), inference(avatar_split_clause, [], [f3396, f1688, f2394])).
fof(f3396, plain, (~ (e20 = h1(e14)) | ~ spl20_236), inference(backward_demodulation, [], [f1868, f1690])).
fof(f1868, plain, ~ (op2(e20, e23) = h1(e14)), inference(backward_demodulation, [], [f389, f504])).
fof(f389, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f8])).
fof(f3361, plain, (spl20_349 | ~ spl20_251), inference(avatar_split_clause, [], [f3360, f1751, f2394])).
fof(f3360, plain, ((e20 = h1(e14)) | ~ spl20_251), inference(backward_demodulation, [], [f504, f1753])).
fof(f3359, plain, (spl20_155 | ~ spl20_256), inference(avatar_split_clause, [], [f3349, f1772, f1347])).
fof(f3349, plain, ((e24 = op2(e24, e20)) | ~ spl20_256), inference(backward_demodulation, [], [f185, f1774])).
fof(f3353, plain, (spl20_227 | ~ spl20_256), inference(avatar_split_clause, [], [f3343, f1772, f1650])).
fof(f3343, plain, ((e21 = op2(e21, e20)) | ~ spl20_256), inference(backward_demodulation, [], [f179, f1774])).
fof(f3112, plain, (~ spl20_440 | ~ spl20_441 | ~ spl20_442 | ~ spl20_443 | ~ spl20_444 | ~ spl20_445 | ~ spl20_446 | spl20_330 | spl20_327 | spl20_324 | spl20_321 | ~ spl20_447 | ~ spl20_448 | ~ spl20_449 | ~ spl20_450 | ~ spl20_451 | ~ spl20_452 | ~ spl20_453 | ~ spl20_454 | ~ spl20_455 | ~ spl20_456 | ~ spl20_457 | ~ spl20_458 | ~ spl20_459 | ~ spl20_460 | ~ spl20_461 | ~ spl20_462 | ~ spl20_463 | ~ spl20_464 | ~ spl20_465), inference(avatar_split_clause, [], [f3007, f3109, f3105, f3101, f3097, f3093, f3089, f3085, f3081, f3077, f3073, f3069, f3065, f3061, f3057, f3053, f3049, f3045, f3041, f3037, f2245, f2262, f2280, f2298, f3033, f3029, f3025, f3021, f3017, f3013, f3009])).
fof(f2298, plain, (spl20_330 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl20_330])])).
fof(f2280, plain, (spl20_327 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl20_327])])).
fof(f2262, plain, (spl20_324 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl20_324])])).
fof(f2245, plain, (spl20_321 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl20_321])])).
fof(f3007, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | ~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e14) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3006, f505])).
fof(f3006, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e14) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3005, f1895])).
fof(f3005, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e14) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3004, f505])).
fof(f3004, plain, (~ (h2(e14) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3003, f509])).
fof(f3003, plain, (~ (op2(e21, e21) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3002, f505])).
fof(f3002, plain, (~ (h2(e10) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3001, f1897])).
fof(f3001, plain, (~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3000, f505])).
fof(f3000, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), e22)) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2999, f1895])).
fof(f2999, plain, (~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2998, f505])).
fof(f2998, plain, (~ (h2(op1(e11, e14)) = op2(e21, h2(e14))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2997, f505])).
fof(f2997, plain, (~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2996, f1895])).
fof(f2996, plain, (~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2995, f1895])).
fof(f2995, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), e21)) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2994, f505])).
fof(f2994, plain, (~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2993, f1885])).
fof(f2993, plain, (~ (op2(e22, e22) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2992, f1895])).
fof(f2992, plain, (~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2991, f1895])).
fof(f2991, plain, (~ (h2(op1(e12, e14)) = op2(e22, h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2990, f1895])).
fof(f2990, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2989, f505])).
fof(f2989, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2988, f1895])).
fof(f2988, plain, (~ (h3(e12) = h2(op1(e13, e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2987, f1908])).
fof(f2987, plain, (~ (h2(op1(e14, e11)) = op2(h2(e14), e21)) | ~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2986, f505])).
fof(f2986, plain, (~ (h2(op1(e14, e12)) = op2(h2(e14), e22)) | ~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f2985, f1895])).
fof(f2985, plain, (~ (e22 = h2(op1(e14, e14))) | ~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f634, f1883])).
fof(f634, plain, (~ (e24 = h2(e14)) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | sP19 | sP18 | sP17 | sP16 | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | sP15 | sP14 | sP13 | sP12 | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | sP11 | sP10 | sP9 | sP8 | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | sP7 | sP6 | sP5 | sP4 | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | sP3 | sP2 | sP1 | sP0 | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f23, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24])).
fof(f24, plain, ((~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP0), inference(usedef, [], [e24])).
fof(e24, plain, (sP0 <=> (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f25, plain, ((~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP1), inference(usedef, [], [e25])).
fof(e25, plain, (sP1 <=> (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f26, plain, ((~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP2), inference(usedef, [], [e26])).
fof(e26, plain, (sP2 <=> (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f27, plain, ((~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | ~ sP3), inference(usedef, [], [e27])).
fof(e27, plain, (sP3 <=> (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f28, plain, ((~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP4), inference(usedef, [], [e28])).
fof(e28, plain, (sP4 <=> (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f29, plain, ((~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP5), inference(usedef, [], [e29])).
fof(e29, plain, (sP5 <=> (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f30, plain, ((~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP6), inference(usedef, [], [e30])).
fof(e30, plain, (sP6 <=> (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f31, plain, ((~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | ~ sP7), inference(usedef, [], [e31])).
fof(e31, plain, (sP7 <=> (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f32, plain, ((~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP8), inference(usedef, [], [e32])).
fof(e32, plain, (sP8 <=> (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f33, plain, ((~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP9), inference(usedef, [], [e33])).
fof(e33, plain, (sP9 <=> (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f34, plain, ((~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP10), inference(usedef, [], [e34])).
fof(e34, plain, (sP10 <=> (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f35, plain, ((~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | ~ sP11), inference(usedef, [], [e35])).
fof(e35, plain, (sP11 <=> (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f36, plain, ((~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP12), inference(usedef, [], [e36])).
fof(e36, plain, (sP12 <=> (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f37, plain, ((~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP13), inference(usedef, [], [e37])).
fof(e37, plain, (sP13 <=> (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f38, plain, ((~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP14), inference(usedef, [], [e38])).
fof(e38, plain, (sP14 <=> (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f39, plain, ((~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | ~ sP15), inference(usedef, [], [e39])).
fof(e39, plain, (sP15 <=> (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f40, plain, ((~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ sP16), inference(usedef, [], [e40])).
fof(e40, plain, (sP16 <=> (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f41, plain, ((~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | ~ sP17), inference(usedef, [], [e41])).
fof(e41, plain, (sP17 <=> (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f42, plain, ((~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | ~ sP18), inference(usedef, [], [e42])).
fof(e42, plain, (sP18 <=> (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f43, plain, ((~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | ~ sP19), inference(usedef, [], [e43])).
fof(e43, plain, (sP19 <=> (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f23, plain, (((~ (e24 = h5(e14)) & ~ (e24 = h5(e13)) & ~ (e24 = h5(e12)) & ~ (e24 = h5(e11)) & ~ (e24 = h5(e10))) | (~ (e23 = h5(e14)) & ~ (e23 = h5(e13)) & ~ (e23 = h5(e12)) & ~ (e23 = h5(e11)) & ~ (e23 = h5(e10))) | (~ (e22 = h5(e14)) & ~ (e22 = h5(e13)) & ~ (e22 = h5(e12)) & ~ (e22 = h5(e11)) & ~ (e22 = h5(e10))) | (~ (e21 = h5(e14)) & ~ (e21 = h5(e13)) & ~ (e21 = h5(e12)) & ~ (e21 = h5(e11)) & ~ (e21 = h5(e10))) | (~ (e20 = h5(e14)) & ~ (e20 = h5(e13)) & ~ (e20 = h5(e12)) & ~ (e20 = h5(e11)) & ~ (e20 = h5(e10))) | ~ (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) | ~ (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) | ~ (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) | ~ (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) | ~ (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) | ~ (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) | ~ (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) | ~ (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) | ~ (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) | ~ (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) | ~ (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) | ~ (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) | ~ (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) | ~ (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) | ~ (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) | ~ (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) | ~ (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) | ~ (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) | ~ (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) | ~ (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) | ~ (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) | ~ (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) | ~ (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) | ~ (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) | ~ (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) & ((~ (e24 = h4(e14)) & ~ (e24 = h4(e13)) & ~ (e24 = h4(e12)) & ~ (e24 = h4(e11)) & ~ (e24 = h4(e10))) | (~ (e23 = h4(e14)) & ~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e14)) & ~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e14)) & ~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e14)) & ~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) | ~ (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) | ~ (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) | ~ (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) | ~ (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) | ~ (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e24 = h3(e14)) & ~ (e24 = h3(e13)) & ~ (e24 = h3(e12)) & ~ (e24 = h3(e11)) & ~ (e24 = h3(e10))) | (~ (e23 = h3(e14)) & ~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e14)) & ~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e14)) & ~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e14)) & ~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) | ~ (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) | ~ (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) | ~ (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) | ~ (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) | ~ (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e24 = h2(e14)) & ~ (e24 = h2(e13)) & ~ (e24 = h2(e12)) & ~ (e24 = h2(e11)) & ~ (e24 = h2(e10))) | (~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) | ~ (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) | ~ (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) | ~ (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) | ~ (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) | ~ (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e24 = h1(e14)) & ~ (e24 = h1(e13)) & ~ (e24 = h1(e12)) & ~ (e24 = h1(e11)) & ~ (e24 = h1(e10))) | (~ (e23 = h1(e14)) & ~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e14)) & ~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e14)) & ~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e14)) & ~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) | ~ (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) | ~ (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) | ~ (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) | ~ (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) | ~ (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f21])).
fof(f21, plain, ~ ((((e24 = h5(e14)) | (e24 = h5(e13)) | (e24 = h5(e12)) | (e24 = h5(e11)) | (e24 = h5(e10))) & ((e23 = h5(e14)) | (e23 = h5(e13)) | (e23 = h5(e12)) | (e23 = h5(e11)) | (e23 = h5(e10))) & ((e22 = h5(e14)) | (e22 = h5(e13)) | (e22 = h5(e12)) | (e22 = h5(e11)) | (e22 = h5(e10))) & ((e21 = h5(e14)) | (e21 = h5(e13)) | (e21 = h5(e12)) | (e21 = h5(e11)) | (e21 = h5(e10))) & ((e20 = h5(e14)) | (e20 = h5(e13)) | (e20 = h5(e12)) | (e20 = h5(e11)) | (e20 = h5(e10))) & (h5(op1(e14, e14)) = op2(h5(e14), h5(e14))) & (h5(op1(e14, e13)) = op2(h5(e14), h5(e13))) & (h5(op1(e14, e12)) = op2(h5(e14), h5(e12))) & (h5(op1(e14, e11)) = op2(h5(e14), h5(e11))) & (h5(op1(e14, e10)) = op2(h5(e14), h5(e10))) & (h5(op1(e13, e14)) = op2(h5(e13), h5(e14))) & (h5(op1(e13, e13)) = op2(h5(e13), h5(e13))) & (h5(op1(e13, e12)) = op2(h5(e13), h5(e12))) & (h5(op1(e13, e11)) = op2(h5(e13), h5(e11))) & (h5(op1(e13, e10)) = op2(h5(e13), h5(e10))) & (h5(op1(e12, e14)) = op2(h5(e12), h5(e14))) & (h5(op1(e12, e13)) = op2(h5(e12), h5(e13))) & (h5(op1(e12, e12)) = op2(h5(e12), h5(e12))) & (h5(op1(e12, e11)) = op2(h5(e12), h5(e11))) & (h5(op1(e12, e10)) = op2(h5(e12), h5(e10))) & (h5(op1(e11, e14)) = op2(h5(e11), h5(e14))) & (h5(op1(e11, e13)) = op2(h5(e11), h5(e13))) & (h5(op1(e11, e12)) = op2(h5(e11), h5(e12))) & (h5(op1(e11, e11)) = op2(h5(e11), h5(e11))) & (h5(op1(e11, e10)) = op2(h5(e11), h5(e10))) & (h5(op1(e10, e14)) = op2(h5(e10), h5(e14))) & (h5(op1(e10, e13)) = op2(h5(e10), h5(e13))) & (h5(op1(e10, e12)) = op2(h5(e10), h5(e12))) & (h5(op1(e10, e11)) = op2(h5(e10), h5(e11))) & (h5(op1(e10, e10)) = op2(h5(e10), h5(e10)))) | (((e24 = h4(e14)) | (e24 = h4(e13)) | (e24 = h4(e12)) | (e24 = h4(e11)) | (e24 = h4(e10))) & ((e23 = h4(e14)) | (e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e14)) | (e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e14)) | (e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e14)) | (e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e14, e14)) = op2(h4(e14), h4(e14))) & (h4(op1(e14, e13)) = op2(h4(e14), h4(e13))) & (h4(op1(e14, e12)) = op2(h4(e14), h4(e12))) & (h4(op1(e14, e11)) = op2(h4(e14), h4(e11))) & (h4(op1(e14, e10)) = op2(h4(e14), h4(e10))) & (h4(op1(e13, e14)) = op2(h4(e13), h4(e14))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e14)) = op2(h4(e12), h4(e14))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e14)) = op2(h4(e11), h4(e14))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e14)) = op2(h4(e10), h4(e14))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e24 = h3(e14)) | (e24 = h3(e13)) | (e24 = h3(e12)) | (e24 = h3(e11)) | (e24 = h3(e10))) & ((e23 = h3(e14)) | (e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e14)) | (e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e14)) | (e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e14)) | (e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e14, e14)) = op2(h3(e14), h3(e14))) & (h3(op1(e14, e13)) = op2(h3(e14), h3(e13))) & (h3(op1(e14, e12)) = op2(h3(e14), h3(e12))) & (h3(op1(e14, e11)) = op2(h3(e14), h3(e11))) & (h3(op1(e14, e10)) = op2(h3(e14), h3(e10))) & (h3(op1(e13, e14)) = op2(h3(e13), h3(e14))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e14)) = op2(h3(e12), h3(e14))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e14)) = op2(h3(e11), h3(e14))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e14)) = op2(h3(e10), h3(e14))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e24 = h2(e14)) | (e24 = h2(e13)) | (e24 = h2(e12)) | (e24 = h2(e11)) | (e24 = h2(e10))) & ((e23 = h2(e14)) | (e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e14)) | (e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e14)) | (e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e14)) | (e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e14, e14)) = op2(h2(e14), h2(e14))) & (h2(op1(e14, e13)) = op2(h2(e14), h2(e13))) & (h2(op1(e14, e12)) = op2(h2(e14), h2(e12))) & (h2(op1(e14, e11)) = op2(h2(e14), h2(e11))) & (h2(op1(e14, e10)) = op2(h2(e14), h2(e10))) & (h2(op1(e13, e14)) = op2(h2(e13), h2(e14))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e14)) = op2(h2(e12), h2(e14))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e14)) = op2(h2(e11), h2(e14))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e14)) = op2(h2(e10), h2(e14))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e24 = h1(e14)) | (e24 = h1(e13)) | (e24 = h1(e12)) | (e24 = h1(e11)) | (e24 = h1(e10))) & ((e23 = h1(e14)) | (e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e14)) | (e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e14)) | (e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e14)) | (e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e14, e14)) = op2(h1(e14), h1(e14))) & (h1(op1(e14, e13)) = op2(h1(e14), h1(e13))) & (h1(op1(e14, e12)) = op2(h1(e14), h1(e12))) & (h1(op1(e14, e11)) = op2(h1(e14), h1(e11))) & (h1(op1(e14, e10)) = op2(h1(e14), h1(e10))) & (h1(op1(e13, e14)) = op2(h1(e13), h1(e14))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e14)) = op2(h1(e12), h1(e14))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e14)) = op2(h1(e11), h1(e14))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e14)) = op2(h1(e10), h1(e14))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', co1)).
fof(f2313, plain, (~ spl20_330 | ~ spl20_332), inference(avatar_split_clause, [], [f600, f2310, f2298])).
fof(f600, plain, (~ (e20 = h2(e10)) | ~ sP4), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e20 = h2(e14)) & ~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP4), inference(nnf_transformation, [], [f28])).
fof(f2291, plain, ~ spl20_327, inference(avatar_split_clause, [], [f2290, f2280])).
fof(f2290, plain, ~ sP5, inference(subsumption_resolution, [], [f596, f505])).
fof(f596, plain, (~ (e21 = h2(e11)) | ~ sP5), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e21 = h2(e14)) & ~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP5), inference(nnf_transformation, [], [f29])).
fof(f2272, plain, ~ spl20_324, inference(avatar_split_clause, [], [f2271, f2262])).
fof(f2271, plain, ~ sP6, inference(subsumption_resolution, [], [f592, f1895])).
fof(f592, plain, (~ (e22 = h2(e12)) | ~ sP6), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e22 = h2(e14)) & ~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP6), inference(nnf_transformation, [], [f30])).
fof(f2253, plain, (~ spl20_321 | ~ spl20_302), inference(avatar_split_clause, [], [f588, f2145, f2245])).
fof(f588, plain, (~ (e23 = h2(e13)) | ~ sP7), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e23 = h2(e14)) & ~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | ~ sP7), inference(nnf_transformation, [], [f31])).
fof(f1861, plain, spl20_216, inference(avatar_split_clause, [], [f1860, f1604])).
fof(f1860, plain, (e20 = op2(e21, e22)), inference(forward_demodulation, [], [f496, f497])).
fof(f496, plain, (e20 = op2(e21, op2(op2(e21, e21), op2(e21, e21)))), inference(cnf_transformation, [], [f15])).
fof(f1859, plain, spl20_194, inference(avatar_split_clause, [], [f1858, f1511])).
fof(f1858, plain, (e23 = op2(e22, e22)), inference(backward_demodulation, [], [f498, f497])).
fof(f498, plain, (e23 = op2(op2(op2(e21, e21), op2(e21, e21)), op2(op2(e21, e21), op2(e21, e21)))), inference(cnf_transformation, [], [f15])).
fof(f1857, plain, spl20_225, inference(avatar_split_clause, [], [f499, f1641])).
fof(f499, plain, (e24 = op2(e21, e21)), inference(cnf_transformation, [], [f15])).
fof(f1856, plain, spl20_86, inference(avatar_split_clause, [], [f1855, f1008])).
fof(f1855, plain, (e10 = op1(e11, e12)), inference(forward_demodulation, [], [f492, f493])).
fof(f492, plain, (e10 = op1(e11, op1(op1(e11, e11), op1(e11, e11)))), inference(cnf_transformation, [], [f14])).
fof(f1854, plain, spl20_64, inference(avatar_split_clause, [], [f1853, f915])).
fof(f1853, plain, (e13 = op1(e12, e12)), inference(backward_demodulation, [], [f494, f493])).
fof(f494, plain, (e13 = op1(op1(op1(e11, e11), op1(e11, e11)), op1(op1(e11, e11), op1(e11, e11)))), inference(cnf_transformation, [], [f14])).
fof(f1852, plain, spl20_95, inference(avatar_split_clause, [], [f495, f1045])).
fof(f495, plain, (e14 = op1(e11, e11)), inference(cnf_transformation, [], [f14])).
fof(f1851, plain, (spl20_251 | spl20_221 | spl20_191 | spl20_161 | spl20_131), inference(avatar_split_clause, [], [f487, f1247, f1373, f1499, f1625, f1751])).
fof(f487, plain, ((e20 = op2(e24, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, (((e24 = op2(e24, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e21, e21)) | (op2(e20, e20) = e24)) & ((e23 = op2(e24, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e24, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e24, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e24, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax13)).
fof(f1850, plain, (spl20_252 | spl20_222 | spl20_192 | spl20_162 | spl20_132), inference(avatar_split_clause, [], [f488, f1251, f1377, f1503, f1629, f1755])).
fof(f488, plain, ((e21 = op2(e24, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f13])).
fof(f1846, plain, (spl20_121 | spl20_91 | spl20_61 | spl20_31 | spl20_1), inference(avatar_split_clause, [], [f482, f651, f777, f903, f1029, f1155])).
fof(f482, plain, ((e10 = op1(e14, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, (((e14 = op1(e14, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e11)) | (op1(e10, e10) = e14)) & ((e13 = op1(e14, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e14, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e14, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e14, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax12)).
fof(f1845, plain, (spl20_122 | spl20_92 | spl20_62 | spl20_32 | spl20_2), inference(avatar_split_clause, [], [f483, f655, f781, f907, f1033, f1159])).
fof(f483, plain, ((e11 = op1(e14, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f12])).
fof(f1811, plain, (spl20_176 | spl20_171 | spl20_166 | spl20_161 | spl20_156), inference(avatar_split_clause, [], [f217, f1352, f1373, f1394, f1415, f1436])).
fof(f217, plain, ((e20 = op2(e23, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (((e24 = op2(e24, e24)) | (e24 = op2(e23, e24)) | (e24 = op2(e22, e24)) | (e24 = op2(e21, e24)) | (e24 = op2(e20, e24))) & ((e24 = op2(e24, e24)) | (e24 = op2(e24, e23)) | (e24 = op2(e24, e22)) | (e24 = op2(e24, e21)) | (e24 = op2(e24, e20))) & ((e23 = op2(e24, e24)) | (e23 = op2(e23, e24)) | (e23 = op2(e22, e24)) | (e23 = op2(e21, e24)) | (e23 = op2(e20, e24))) & ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))) & ((e22 = op2(e24, e24)) | (e22 = op2(e23, e24)) | (e22 = op2(e22, e24)) | (e22 = op2(e21, e24)) | (e22 = op2(e20, e24))) & ((e22 = op2(e24, e24)) | (e22 = op2(e24, e23)) | (e22 = op2(e24, e22)) | (e22 = op2(e24, e21)) | (e22 = op2(e24, e20))) & ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))) & ((e21 = op2(e24, e24)) | (e21 = op2(e24, e23)) | (e21 = op2(e24, e22)) | (e21 = op2(e24, e21)) | (e21 = op2(e24, e20))) & ((e20 = op2(e24, e24)) | (e20 = op2(e23, e24)) | (e20 = op2(e22, e24)) | (e20 = op2(e21, e24)) | (e20 = op2(e20, e24))) & ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))) & ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))) & ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))) & ((e23 = op2(e24, e23)) | (e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e24)) | (e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e24, e23)) | (e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e24, e23)) | (e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e24)) | (e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e24, e23)) | (e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e24)) | (e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e24 = op2(e24, e22)) | (e24 = op2(e23, e22)) | (e24 = op2(e22, e22)) | (e24 = op2(e21, e22)) | (e24 = op2(e20, e22))) & ((e24 = op2(e22, e24)) | (e24 = op2(e22, e23)) | (e24 = op2(e22, e22)) | (e24 = op2(e22, e21)) | (e24 = op2(e22, e20))) & ((e23 = op2(e24, e22)) | (e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e24)) | (e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e24, e22)) | (e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e24)) | (e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e24, e22)) | (e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e24)) | (e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e24, e22)) | (e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e24)) | (e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e24 = op2(e24, e21)) | (e24 = op2(e23, e21)) | (e24 = op2(e22, e21)) | (e24 = op2(e21, e21)) | (e24 = op2(e20, e21))) & ((e24 = op2(e21, e24)) | (e24 = op2(e21, e23)) | (e24 = op2(e21, e22)) | (e24 = op2(e21, e21)) | (e24 = op2(e21, e20))) & ((e23 = op2(e24, e21)) | (e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e24)) | (e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e24, e21)) | (e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e24)) | (e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e24, e21)) | (e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e24)) | (e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e24, e21)) | (e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e24)) | (e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e24 = op2(e24, e20)) | (e24 = op2(e23, e20)) | (e24 = op2(e22, e20)) | (e24 = op2(e21, e20)) | (op2(e20, e20) = e24)) & ((e24 = op2(e20, e24)) | (e24 = op2(e20, e23)) | (e24 = op2(e20, e22)) | (e24 = op2(e20, e21)) | (op2(e20, e20) = e24)) & ((e23 = op2(e24, e20)) | (e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e24)) | (e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e24, e20)) | (e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e24)) | (e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e24, e20)) | (e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e24)) | (e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e24, e20)) | (e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e24)) | (e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax6)).
fof(f1807, plain, (spl20_178 | spl20_173 | spl20_168 | spl20_163 | spl20_158), inference(avatar_split_clause, [], [f221, f1360, f1381, f1402, f1423, f1444])).
fof(f221, plain, ((e22 = op2(e23, e24)) | (e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f1803, plain, (spl20_180 | spl20_175 | spl20_170 | spl20_165 | spl20_160), inference(avatar_split_clause, [], [f225, f1368, f1389, f1410, f1431, f1452])).
fof(f225, plain, ((e24 = op2(e23, e24)) | (e24 = op2(e23, e23)) | (e24 = op2(e23, e22)) | (e24 = op2(e23, e21)) | (e24 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f1802, plain, (spl20_240 | spl20_215 | spl20_190 | spl20_165 | spl20_140), inference(avatar_split_clause, [], [f226, f1284, f1389, f1494, f1599, f1704])).
fof(f226, plain, ((e24 = op2(e24, e23)) | (e24 = op2(e23, e23)) | (e24 = op2(e22, e23)) | (e24 = op2(e21, e23)) | (e24 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f1801, plain, (spl20_151 | spl20_146 | spl20_141 | spl20_136 | spl20_131), inference(avatar_split_clause, [], [f227, f1247, f1268, f1289, f1310, f1331])).
fof(f227, plain, ((e20 = op2(e24, e24)) | (e20 = op2(e24, e23)) | (e20 = op2(e24, e22)) | (e20 = op2(e24, e21)) | (e20 = op2(e24, e20))), inference(cnf_transformation, [], [f6])).
fof(f1798, plain, (spl20_232 | spl20_207 | spl20_182 | spl20_157 | spl20_132), inference(avatar_split_clause, [], [f230, f1251, f1356, f1461, f1566, f1671])).
fof(f230, plain, ((e21 = op2(e24, e24)) | (e21 = op2(e23, e24)) | (e21 = op2(e22, e24)) | (e21 = op2(e21, e24)) | (e21 = op2(e20, e24))), inference(cnf_transformation, [], [f6])).
fof(f1795, plain, (spl20_154 | spl20_149 | spl20_144 | spl20_139 | spl20_134), inference(avatar_split_clause, [], [f233, f1259, f1280, f1301, f1322, f1343])).
fof(f233, plain, ((e23 = op2(e24, e24)) | (e23 = op2(e24, e23)) | (e23 = op2(e24, e22)) | (e23 = op2(e24, e21)) | (e23 = op2(e24, e20))), inference(cnf_transformation, [], [f6])).
fof(f1791, plain, (spl20_256 | spl20_257 | spl20_258 | spl20_259 | spl20_260), inference(avatar_split_clause, [], [f186, f1788, f1784, f1780, f1776, f1772])).
fof(f186, plain, ((e24 = unit2) | (e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)), inference(cnf_transformation, [], [f5])).
fof(f1602, plain, (spl20_211 | spl20_212 | spl20_213 | spl20_214 | spl20_215), inference(avatar_split_clause, [], [f159, f1599, f1595, f1591, f1587, f1583])).
fof(f159, plain, ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e24 = op2(e24, e24)) | (e23 = op2(e24, e24)) | (e22 = op2(e24, e24)) | (e21 = op2(e24, e24)) | (e20 = op2(e24, e24))) & ((e24 = op2(e24, e23)) | (e23 = op2(e24, e23)) | (e22 = op2(e24, e23)) | (e21 = op2(e24, e23)) | (e20 = op2(e24, e23))) & ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))) & ((e24 = op2(e24, e21)) | (e23 = op2(e24, e21)) | (e22 = op2(e24, e21)) | (e21 = op2(e24, e21)) | (e20 = op2(e24, e21))) & ((e24 = op2(e24, e20)) | (e23 = op2(e24, e20)) | (e22 = op2(e24, e20)) | (e21 = op2(e24, e20)) | (e20 = op2(e24, e20))) & ((e24 = op2(e23, e24)) | (e23 = op2(e23, e24)) | (e22 = op2(e23, e24)) | (e21 = op2(e23, e24)) | (e20 = op2(e23, e24))) & ((e24 = op2(e23, e23)) | (e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e24 = op2(e23, e22)) | (e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e24 = op2(e23, e21)) | (e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e24 = op2(e23, e20)) | (e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e24 = op2(e22, e24)) | (e23 = op2(e22, e24)) | (e22 = op2(e22, e24)) | (e21 = op2(e22, e24)) | (e20 = op2(e22, e24))) & ((e24 = op2(e22, e23)) | (e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e24 = op2(e22, e22)) | (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e24 = op2(e22, e20)) | (e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))) & ((e24 = op2(e21, e23)) | (e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e24 = op2(e21, e22)) | (e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e24 = op2(e21, e21)) | (e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e24 = op2(e21, e20)) | (e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e24 = op2(e20, e24)) | (e23 = op2(e20, e24)) | (e22 = op2(e20, e24)) | (e21 = op2(e20, e24)) | (e20 = op2(e20, e24))) & ((e24 = op2(e20, e23)) | (e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e24 = op2(e20, e22)) | (e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e24 = op2(e20, e21)) | (e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e24) | (op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax4)).
fof(f1581, plain, (spl20_206 | spl20_207 | spl20_208 | spl20_209 | spl20_210), inference(avatar_split_clause, [], [f160, f1578, f1574, f1570, f1566, f1562])).
fof(f160, plain, ((e24 = op2(e21, e24)) | (e23 = op2(e21, e24)) | (e22 = op2(e21, e24)) | (e21 = op2(e21, e24)) | (e20 = op2(e21, e24))), inference(cnf_transformation, [], [f4])).
fof(f1539, plain, (spl20_196 | spl20_197 | spl20_198 | spl20_199 | spl20_200), inference(avatar_split_clause, [], [f162, f1536, f1532, f1528, f1524, f1520])).
fof(f162, plain, ((e24 = op2(e22, e21)) | (e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f4])).
fof(f1308, plain, (spl20_141 | spl20_142 | spl20_143 | spl20_144 | spl20_145), inference(avatar_split_clause, [], [f173, f1305, f1301, f1297, f1293, f1289])).
fof(f173, plain, ((e24 = op2(e24, e22)) | (e23 = op2(e24, e22)) | (e22 = op2(e24, e22)) | (e21 = op2(e24, e22)) | (e20 = op2(e24, e22))), inference(cnf_transformation, [], [f4])).
fof(f1231, plain, (spl20_98 | spl20_93 | spl20_88 | spl20_83 | spl20_78), inference(avatar_split_clause, [], [f115, f974, f995, f1016, f1037, f1058])).
fof(f115, plain, ((e12 = op1(e11, e14)) | (e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e14 = op1(e14, e14)) | (e14 = op1(e13, e14)) | (e14 = op1(e12, e14)) | (e14 = op1(e11, e14)) | (e14 = op1(e10, e14))) & ((e14 = op1(e14, e14)) | (e14 = op1(e14, e13)) | (e14 = op1(e14, e12)) | (e14 = op1(e14, e11)) | (e14 = op1(e14, e10))) & ((e13 = op1(e14, e14)) | (e13 = op1(e13, e14)) | (e13 = op1(e12, e14)) | (e13 = op1(e11, e14)) | (e13 = op1(e10, e14))) & ((e13 = op1(e14, e14)) | (e13 = op1(e14, e13)) | (e13 = op1(e14, e12)) | (e13 = op1(e14, e11)) | (e13 = op1(e14, e10))) & ((e12 = op1(e14, e14)) | (e12 = op1(e13, e14)) | (e12 = op1(e12, e14)) | (e12 = op1(e11, e14)) | (e12 = op1(e10, e14))) & ((e12 = op1(e14, e14)) | (e12 = op1(e14, e13)) | (e12 = op1(e14, e12)) | (e12 = op1(e14, e11)) | (e12 = op1(e14, e10))) & ((e11 = op1(e14, e14)) | (e11 = op1(e13, e14)) | (e11 = op1(e12, e14)) | (e11 = op1(e11, e14)) | (e11 = op1(e10, e14))) & ((e11 = op1(e14, e14)) | (e11 = op1(e14, e13)) | (e11 = op1(e14, e12)) | (e11 = op1(e14, e11)) | (e11 = op1(e14, e10))) & ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))) & ((e10 = op1(e14, e14)) | (e10 = op1(e14, e13)) | (e10 = op1(e14, e12)) | (e10 = op1(e14, e11)) | (e10 = op1(e14, e10))) & ((e14 = op1(e14, e13)) | (e14 = op1(e13, e13)) | (e14 = op1(e12, e13)) | (e14 = op1(e11, e13)) | (e14 = op1(e10, e13))) & ((e14 = op1(e13, e14)) | (e14 = op1(e13, e13)) | (e14 = op1(e13, e12)) | (e14 = op1(e13, e11)) | (e14 = op1(e13, e10))) & ((e13 = op1(e14, e13)) | (e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e14)) | (e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e14, e13)) | (e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e14)) | (e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e14, e13)) | (e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e14)) | (e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e14, e13)) | (e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e14)) | (e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e14 = op1(e14, e12)) | (e14 = op1(e13, e12)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e12)) | (e14 = op1(e10, e12))) & ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))) & ((e13 = op1(e14, e12)) | (e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e14)) | (e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e14, e12)) | (e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e14)) | (e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e14, e12)) | (e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e14, e12)) | (e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e14 = op1(e14, e11)) | (e14 = op1(e13, e11)) | (e14 = op1(e12, e11)) | (e14 = op1(e11, e11)) | (e14 = op1(e10, e11))) & ((e14 = op1(e11, e14)) | (e14 = op1(e11, e13)) | (e14 = op1(e11, e12)) | (e14 = op1(e11, e11)) | (e14 = op1(e11, e10))) & ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e14)) | (e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e14)) | (e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e14, e11)) | (e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e14)) | (e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e14, e11)) | (e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e14)) | (e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e14 = op1(e14, e10)) | (e14 = op1(e13, e10)) | (e14 = op1(e12, e10)) | (e14 = op1(e11, e10)) | (op1(e10, e10) = e14)) & ((e14 = op1(e10, e14)) | (e14 = op1(e10, e13)) | (e14 = op1(e10, e12)) | (e14 = op1(e10, e11)) | (op1(e10, e10) = e14)) & ((e13 = op1(e14, e10)) | (e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e14)) | (e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e14, e10)) | (e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e14)) | (e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e14, e10)) | (e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e14)) | (e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e14, e10)) | (e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e14)) | (e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG048+1.p', ax3)).
fof(f1230, plain, (spl20_118 | spl20_93 | spl20_68 | spl20_43 | spl20_18), inference(avatar_split_clause, [], [f116, f722, f827, f932, f1037, f1142])).
fof(f116, plain, ((e12 = op1(e14, e11)) | (e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f3])).
fof(f1228, plain, (spl20_119 | spl20_94 | spl20_69 | spl20_44 | spl20_19), inference(avatar_split_clause, [], [f118, f726, f831, f936, f1041, f1146])).
fof(f118, plain, ((e13 = op1(e14, e11)) | (e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f3])).
fof(f1225, plain, (spl20_71 | spl20_66 | spl20_61 | spl20_56 | spl20_51), inference(avatar_split_clause, [], [f121, f861, f882, f903, f924, f945])).
fof(f121, plain, ((e10 = op1(e12, e14)) | (e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f1223, plain, (spl20_72 | spl20_67 | spl20_62 | spl20_57 | spl20_52), inference(avatar_split_clause, [], [f123, f865, f886, f907, f928, f949])).
fof(f123, plain, ((e11 = op1(e12, e14)) | (e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f1217, plain, (spl20_75 | spl20_70 | spl20_65 | spl20_60 | spl20_55), inference(avatar_split_clause, [], [f129, f877, f898, f919, f940, f961])).
fof(f129, plain, ((e14 = op1(e12, e14)) | (e14 = op1(e12, e13)) | (e14 = op1(e12, e12)) | (e14 = op1(e12, e11)) | (e14 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f1216, plain, (spl20_115 | spl20_90 | spl20_65 | spl20_40 | spl20_15), inference(avatar_split_clause, [], [f130, f709, f814, f919, f1024, f1129])).
fof(f130, plain, ((e14 = op1(e14, e12)) | (e14 = op1(e13, e12)) | (e14 = op1(e12, e12)) | (e14 = op1(e11, e12)) | (e14 = op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f1205, plain, (spl20_21 | spl20_16 | spl20_11 | spl20_6 | spl20_1), inference(avatar_split_clause, [], [f141, f651, f672, f693, f714, f735])).
fof(f141, plain, ((e10 = op1(e14, e14)) | (e10 = op1(e14, e13)) | (e10 = op1(e14, e12)) | (e10 = op1(e14, e11)) | (e10 = op1(e14, e10))), inference(cnf_transformation, [], [f3])).
fof(f1204, plain, (spl20_101 | spl20_76 | spl20_51 | spl20_26 | spl20_1), inference(avatar_split_clause, [], [f142, f651, f756, f861, f966, f1071])).
fof(f142, plain, ((e10 = op1(e14, e14)) | (e10 = op1(e13, e14)) | (e10 = op1(e12, e14)) | (e10 = op1(e11, e14)) | (e10 = op1(e10, e14))), inference(cnf_transformation, [], [f3])).
fof(f1203, plain, (spl20_22 | spl20_17 | spl20_12 | spl20_7 | spl20_2), inference(avatar_split_clause, [], [f143, f655, f676, f697, f718, f739])).
fof(f143, plain, ((e11 = op1(e14, e14)) | (e11 = op1(e14, e13)) | (e11 = op1(e14, e12)) | (e11 = op1(e14, e11)) | (e11 = op1(e14, e10))), inference(cnf_transformation, [], [f3])).
fof(f1198, plain, (spl20_104 | spl20_79 | spl20_54 | spl20_29 | spl20_4), inference(avatar_split_clause, [], [f148, f663, f768, f873, f978, f1083])).
fof(f148, plain, ((e13 = op1(e14, e14)) | (e13 = op1(e13, e14)) | (e13 = op1(e12, e14)) | (e13 = op1(e11, e14)) | (e13 = op1(e10, e14))), inference(cnf_transformation, [], [f3])).
fof(f1195, plain, (spl20_126 | spl20_127 | spl20_128 | spl20_129 | spl20_130), inference(avatar_split_clause, [], [f100, f1192, f1188, f1184, f1180, f1176])).
fof(f100, plain, ((e14 = unit1) | (e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)), inference(cnf_transformation, [], [f2])).